# -*- perl -*-

# Copyright (c) 2002 by Jeff Weisberg
# Author: Jeff Weisberg <jaw+chart-strip @ tcp4me.com>
# Date: 2002-Nov-01 16:11 (EST)
# Function: draw strip charts
#
# $Id: Strip.pm,v 1.7 2004/02/12 23:05:08 jaw Exp jaw $

$Chart::Strip::VERSION = "1.0";

=head1 NAME

Chart::Strip - Draw strip chart type graphs.

=head1 SYNOPSIS

    use Chart::Strip;

    my $ch = Chart::Strip->new(
	         title   => 'Happiness of our Group',
		 # other options ...
             );

    $ch->add_data( $davey_data, { style => 'line',
				  color => 'FF0000',
				  label => 'Davey' } );

    $ch->add_data( $jenna_data, { style => 'line',
				  color => '00FF88',
				  label => 'Jenna' } );

    print $ch->png();

=head1 DESCRIPTION

The Chart::Strip package plots data values versus time graphs, such as
used for seismographs, EKGs, or network usage reports.

It can plot multiple data sets on one graph. It offers several
styles of plots. It automatically determines the proper ranges
and labels for both axii.

=head1 USAGE

=head2 Create the Chart

    $chart = Chart::Strip->new();
    $chart = Chart::Strip->new(
			       option1 => value,
			       option2 => value,
			       );

If no options are specified, sensible default values will be used.
The following options are recognized:

=over 4

=item C<width>

The width of the image

=item C<height>

The height of the image.

=item C<title>

The title of the graph. Will be placed centered at the top.
    
=item C<x_label>

The label for the x axis. Will be placed centered at the bottom.
    
=item C<y_label>

The label for the y axis. Will be placed vertically along the left side.
    
=item C<draw_grid>

Should a grid be drawn on the graph?
    
=item C<draw_border>

Should a border be drawn around the edge of the image?
    
=item C<draw_tic_labels>

Should value labels be shown?
    
=item C<draw_data_labels>

Should each data set be labeled?
    
=item C<transparent>

Should the background be transparent?
    
=item C<grid_on_top>

Should the grid be drawn over the data (1) or below the data (0)?

=item C<binary>

Use powers of 2 instead of powers of 10 for the y axis labels.    
    
=back

=head2 Adding Data

    $chart->add_data( $data, $options );

The data should be an array ref of data points. Each data point
should be a hash ref containing:
    
    {
	time  => $time_t,  # must be a unix time_t 
	value => $value,   # the data value
	color => $color,   # optional
    }
or, range style graphs should contain:
    
    {
	time  => $time_t,  # must be a unix time_t 
	min   => $low,     # the minimum data value
	max   => $high,    # the maximum data value
	color => $color,   # optional, used for this one point
    }

and the options may contain:
    
    {
	color => 'FF00FF',           # color used for the graph
	label => 'New England',      # name of the data set
    }

=head2 Outputing The Image

=item $chart->png()

Will return the PNG image

=item $chart->jpeg()

Will return the jpeg image

=item $chart->gd()

Will return the underlying GD object.
    

=cut
    ;


package Chart::Strip;
use GD;
use Carp;
use POSIX;

use strict;

sub new {
    my $class = shift;
    my %param = @_;

    my $me = bless {
	width	      => 640,
	height        => 192,
	margin_left   => 8,
	margin_bottom => 8,
	margin_right  => 8,
	margin_top    => 8,
	n_y_tics      => 4, # aprox.
	
	transparent     => 1,
	grid_on_top     => 1,
	draw_grid       => 1,
	draw_border     => 1,
	draw_tic_labels => 1,
	draw_data_labels=> 1,
	
	# title
	# x_label
	# y_label
	
	# user specified params override defaults
	%param
	    
    }, $class;

    $me->adjust();

    my $im = GD::Image->new( $me->{width}, $me->{height} );
    $me->{img} = $im;
    
    # Nor long the sun his daily course withheld, 
    # But added colors to the world reveal'd: 
    # When early Turnus, wak'ning with the light, 
    #   -- Virgil, Aeneid
    # allocate some useful colors
    $me->{color}{white} = $im->colorAllocate(255,255,255);
    $me->{color}{black} = $im->colorAllocate(0,0,0);
    $me->{color}{blue}  = $im->colorAllocate(0, 0, 255);
    $me->{color}{red}   = $im->colorAllocate(255, 0, 0);
    $me->{color}{grn}   = $im->colorAllocate(0, 255, 0);
    $me->{color}{gray}  = $im->colorAllocate(128, 128, 128);

    # style for grid lines
    $im->setStyle(gdTransparent, $me->{color}{gray}, gdTransparent, gdTransparent);

    $im->interlaced('true');
    $me->{img}->transparent($me->{color}{white})
	if $me->{transparent};

    $me->{img}->rectangle(0, 0, $me->{width}-1, $me->{height}-1, $me->{color}{black})
	if $me->{draw_border};

    $me;
}


sub add_data {
    my $me   = shift;
    my $data = shift;
    my $opts = shift;

    $me->analyze( $data );
    
    unless( $opts->{style} ){
	$opts->{style} = defined $data->[0]{min} ? 'range' : 'line';
    }
    
    push @{$me->{data}}, {data => $data, opts => $opts};
    
    $me;
}

# A plot shall show us all a merry day.
#   -- Shakespeare, King Richard II
sub plot {
    my $me = shift;

    return unless $me->{data};
    return if $me->{all_done};
    
    $me->clabels();
    $me->xlabel();
    $me->ylabel();
    $me->title();

    if( $me->{draw_tic_labels} ){
	# move margin for xtics before we do ytics
	$me->{margin_bottom} += 12;
	$me->adjust();
    }

    $me->ytics();
    $me->xtics();
    $me->axii();
    $me->drawgrid() unless $me->{grid_on_top};
    
    # plot
    foreach my $d ( @{$me->{data}} ){
	$me->plot_data( $d->{data}, $d->{opts} );
    }
    
    $me->drawgrid() if $me->{grid_on_top};

    $me->{all_done} = 1;
    $me;
}



# The axis of the earth sticks out visibly through the centre of each and every town or city.
#   -- Oliver Wendell Holmes, The Autocrat of the Breakfast-Table    
sub axii {
    my $me = shift;
    my $im = $me->{img};
    # draw axii

    $im->line( $me->xpt(-1), $me->ypt(-1), $me->xpt(-1), $me->ypt($me->{ymax}), $me->{color}{black});
    $im->line( $me->xpt(-1), $me->ypt(-1), $me->xpt($me->{xmax}), $me->ypt(-1), $me->{color}{black});
    
    # 'Talking of axes,' said the Duchess, 'chop off her head!'
    #   -- Alice in Wonderland
    $me;
}

sub gd {
    my $me = shift;

    $me->plot();
    $me->{img};
}

sub png {
    my $me = shift;

    $me->plot();
    $me->{img}->png( @_ );
}

sub jpeg {
    my $me = shift;

    $me->plot();
    $me->{img}->jpeg( @_ );
}
    

# xpt, ypt - convert graph space => image space
sub xpt {
    my $me = shift;
    my $pt = shift;

    $pt + $me->{margin_left};
}

sub ypt {
    my $me = shift;
    my $pt = shift;

    # make 0 bottom
    $me->{height} - $pt - $me->{margin_bottom};
}

# xdatapt, ydatapt - convert data space => image space
sub xdatapt {
    my $me = shift;
    my $pt = shift;

    $me->xpt( ($pt - $me->{xd_min}) * $me->{xd_scale} );
}

sub ydatapt {
    my $me = shift;
    my $pt = shift;

    $pt = $pt < $me->{yd_min} ? $me->{yd_min} : $pt;
    
    $me->ypt( ($pt - $me->{yd_min}) * $me->{yd_scale} );
}

sub adjust {
    my $me = shift;
    
    # I have touched the highest point of all my greatness;
    #   -- Shakespeare, King Henry VIII
    $me->{xmax} = $me->{width}  - $me->{margin_right}  - $me->{margin_left};
    $me->{ymax} = $me->{height} - $me->{margin_bottom} - $me->{margin_top} ;

    if( $me->{data} ){
	$me->{xd_scale} = ($me->{xd_min} == $me->{xd_max}) ? 1
	    : $me->{xmax} / ($me->{xd_max} - $me->{xd_min});
	
	$me->{yd_scale} = ($me->{yd_min} == $me->{yd_max}) ? 1
	    : $me->{ymax} / ($me->{yd_max} - $me->{yd_min});
    }
    
    $me;
}

sub analyze {
    my $me   = shift;
    my $data = shift;
    my( $st, $et, $min, $max );

    $st = $data->[0]{time};	# start time
    $et = $data->[-1]{time};	# end time

    foreach my $s (@$data){
	my $a = defined $s->{min} ? $s->{min} : $s->{value};
	my $b = defined $s->{max} ? $s->{max} : $s->{value};

	$min = $a if !defined($min) || $a < $min;
	$max = $b if !defined($max) || $b > $max;
    }

    $me->{xd_min} = $st  if $st && (!defined($me->{xd_min}) || $st  < $me->{xd_min});
    $me->{xd_max} = $et  if $et && (!defined($me->{xd_max}) || $et  > $me->{xd_max});
    $me->{yd_min} = $min if         !defined($me->{yd_min}) || $min < $me->{yd_min};
    $me->{yd_max} = $max if         !defined($me->{yd_max}) || $max > $me->{yd_max};

    $me->adjust();
}

# I hear beyond the range of sound,
# I see beyond the range of sight,
# New earths and skies and seas around,
# And in my day the sun doth pale his light.
#   -- Thoreau, Inspiration
sub set_y_range {
    my $me = shift;
    my $l  = shift;
    my $h  = shift;

    $me->{yd_min} = $l if defined($l) && $l ne '';
    $me->{yd_max} = $h if defined($h) && $h ne '';
    $me->adjust();
}

sub set_x_range {
    my $me = shift;
    my $l  = shift;
    my $h  = shift;

    $me->{xd_min} = $l if defined($l) && $l ne '';
    $me->{xd_max} = $h if defined($h) && $h ne '';
    $me->adjust();
}

# choose proper color for plot
sub color {
    my $me   = shift;
    my $data = shift;
    my $opts = shift;

    # What is your favorite color?
    # Blue.  No yel--  Auuuuuuuugh!
    #   -- Monty Python, Holy Grail
    my $c = $data->{color} || $opts->{color};
    if( $c ){
	return $me->{color}{$c} if $me->{color}{$c};
	my($r,$g,$b) = map {hex} unpack('a2 a2 a2', $c);
	my $i = $me->{img}->colorAllocate( $r, $g, $b );

	$me->{color}{$c} = $i;
	return $i;
    }
    
    return $me->{color}{grn};
}

# Titles are marks of honest men, and wise;
# The fool or knave that wears a title lies.
#   -- Edward Young, Love of Fame
sub title {
    my $me = shift;
    my( $loc );

    return unless $me->{title};
    $me->{margin_top} += 16;
    $me->adjust();
    # center title
    $loc = ($me->{width} - length($me->{title}) * 7) / 2;
    $me->{img}->string(gdMediumBoldFont, $loc, 2, $me->{title}, $me->{color}{black});
}

# when I waked, I found This label on my bosom
#   -- Shakespeare, Cymbeline
sub xlabel {
    my $me = shift;
    my( $loc, $y );

    return unless $me->{x_label};
    $me->{margin_bottom} += 16;
    $me->adjust();
    $loc = ($me->{width} - length($me->{x_label}) * 6) / 2;
    $y = $me->{height} - $me->{margin_bottom} + 8;
    $me->{img}->string(gdSmallFont, $loc, $y, $me->{x_label}, $me->{color}{black});
}

sub ylabel {
    my $me = shift;
    my( $loc );

    return unless $me->{y_label};
    $me->{margin_left} += 12;
    $me->adjust();
    $loc = ($me->{height} + length($me->{y_label}) * 6) / 2;
    $me->{img}->stringUp(gdSmallFont, 2, $loc, $me->{y_label}, $me->{color}{black});
    # small => 12, 6; tiny => 10,5
}

# It must be a very pretty dance
#   -- Alice in Wonderland
# make tic numbers pretty
sub pretty {
    my $me = shift;
    my $y  = shift;
    my $st = shift;
    my( $ay, $sc, $b, $prec );

    $sc = '';
    $ay = abs($y);
    $b = $me->{binary} ? 1024 : 1000;
    
    if( $ay < 1 ){
	if( $ay < 1/$b**3 ){
	    return "0";
	}
	elsif( $ay < 1/$b**2 ){
	    $y *= $b ** 3; $st *= $b ** 3;
	    $sc = 'p';
	}
	elsif( $ay < 1/$b ){
	    $y *= $b**2; $st *= $b**2;
	    $sc = 'u';
	}
	elsif( $ay < 100/$b ){
	    $y *= $b; $st *= $b;
	    $sc = 'm';
	}
    }else{
	if( $ay >= $b**3 ){
	    $y /= $b**3;  $st /= $b**3;
	    $sc = 'G';
	}
	elsif( $ay >= $b**2 ){
	    $y /= $b**2; $st /= $b**2;
	    $sc = 'M';
	}
	elsif( $ay >= $b ){
	    $y /= $b;   $st /= $b;
	    $sc = 'k';
	}
    }
    $sc .= 'i' if $sc && $me->{binary}; # as per IEC 60027-2
    if( $st > 1 ){
	$prec = 0;
    }else{
	$prec = abs(floor(log($st)/log(10)));
    }

    sprintf "%.${prec}f$sc", $y;
}

sub ytics {
    my $me  = shift;
    my( $min, $max, $tp, $st, $is, $low, $maxw, @tics );

    $min = $me->{yd_min};
    $max = $me->{yd_max};
    $maxw = 0;
    
    if( $min == $max ){
	# not a very interesting graph...
	my $lb = $me->pretty($min, 1);	# QQQ
	my $w = length($lb) * 5 + 6;
	push @tics, [$me->ydatapt($min), $lb, $w];
	$maxw = $w;
    }else{
	$tp = ($max - $min) / $me->{n_y_tics};	# approx # of tics
	if( $me->{binary} ){
	    $is =  2 ** floor( log($tp)/log(2) );
	}else{
	    $is = 10 ** floor( log($tp)/log(10) );
	}
	$st  = floor( $tp / $is ) * $is; # -> 4 - 8, ceil -> 2 - 4
	$low = int( $min / $st ) * $st;
	for my $i ( 0 .. 10 ){
	    my $y = $low + $i * $st;
	    next if $y < $min;
	    last if $y > $max;
	    my $yy = $me->ydatapt($y);
	    my $label = $me->pretty($y, $st);
	    my $w = 5 * length($label) + 6;
	    $maxw = $w if $w > $maxw;
	    
	    push @tics, [$yy, $label, $w];
	}
    }
    
    if( $me->{draw_tic_labels} ){
	# move margin
	$me->{margin_left} += $maxw;
	$me->adjust();
    }

    $me->{grid}{y} = [ @tics ];
}

sub drawgrid {
    my $me = shift;
    
    foreach my $tic (@{$me->{grid}{y}}){
	# ytics + horiz lines
	my $yy = $tic->[0];
	$me->{img}->line($me->xpt(-1), $yy, $me->xpt(-4), $yy,
			 $me->{color}{black});
	$me->{img}->line($me->xpt(0), $yy, $me->{width} - $me->{margin_right}, $yy,
			 gdStyled) if $me->{draw_grid};

	if( $me->{draw_tic_labels} ){
	    my $label = $tic->[1];
	    my $w = $tic->[2];
	    $me->{img}->string(gdTinyFont, $me->xpt(-$w), $yy-4,
			       $label,
			       $me->{color}{black});
	}
    }

    foreach my $tic (@{$me->{grid}{x}}){
	# xtics + vert lines
	my( $t, $ll, $label ) = @$tic;

	if( $ll ){
	    # solid line, red label
	    $me->{img}->line($me->xdatapt($t), $me->{margin_top},
			     $me->xdatapt($t), $me->ypt(-4),
			     $me->{color}{black} );
	}else{
	    # tic and grid
	    $me->{img}->line($me->xdatapt($t), $me->ypt(-1),
			     $me->xdatapt($t), $me->ypt(-4),
			     $me->{color}{black} );
	    $me->{img}->line($me->xdatapt($t), $me->{margin_top},
			     $me->xdatapt($t), $me->ypt(0),
			     gdStyled ) if $me->{draw_grid};
	}

	if( $me->{draw_tic_labels} ){
	    my $a = length($label) * 6 / 4;	# it looks better not quite centered
	    $me->{img}->string(gdSmallFont, $me->xdatapt($t)-$a, $me->ypt(-6),
			       $label, $ll ? $me->{color}{red} : $me->{color}{black} );
	}
    }
}

# this is much too ickky, please re-write
sub xtics {
    my $me = shift;
    my( $r, $step, $rd, $n2, $n3, $n4, $lt, $low, $t, @tics );

    # this is good for (roughly) 10 mins - 10 yrs
    return if $me->{xd_max} == $me->{xd_min};
    $r = ($me->{xd_max} - $me->{xd_min} ) / 3600;	# => hours
    $rd   = $r / 24;
    $step = 3600;
    $n2   = 24; $n3 = $n4 = 1;
    $low  = int($me->{xd_min} / 3600) * 3600;
    
    if( $r < 2 ){ 		# less than 2 hrs
	$low = int($me->{xd_min} / 600) * 600;
	$n2 = 1;
	$lt = 1;
	$step = 10 * 60;
    }elsif( $r < 48 ){		# less than 2 days
	$n2  = ($r < 13) ? 1 : ($r < 24) ? 2 : 4;
	$lt  = 1;
    }
    elsif( $r < 360 ){		# less than ~ 2 weeks
	$lt  = 2;
    }elsif( $rd < 1500 ){	# less than ~ 4yrs
	$n3  = ($rd < 80)  ? 7 : ($rd < 168) ? 14 : 32;
	$n4  = ($rd < 370) ? 1 : ($rd < 500) ? 2 : 4;
	$lt  = 3;
    }else{
	$n3 = 32; $n4 = 12;
	$lt  = 4;
    }

    # print STDERR "xtics min=$me->{xd_min} max=$me->{xd_max}  r=$r, st=$step, low=$low, $n2/$n3/$n4\n";
    for( $t=$low; $t<$me->{xd_max}; $t+=$step ){
	my $ll;
	next if $t < $me->{xd_min};
	my @lt = localtime $t;
	next if $lt[2] % $n2;
	next if ($lt[3] - 1) % $n3 || (($n3!=1) && $lt[3] > 22 );
	next if $lt[4] % $n4;
	if( $lt == 1 && !$lt[2] && !$lt[1] ||      # midnight
	    $lt == 2 && !$lt[6] ||                 # sunday
	    $lt == 3 && $lt[3] == 1 && $rd < 60 || # 1st of month
	    $lt == 3 && $lt[3] == 1 && $lt[4] == 0 # Jan 1
	    ){
	    $ll = 1;
	}
	
	my $label;
	if( $lt == 1){
	    $label = sprintf "%d:%0.2d", $lt[2], $lt[1];	# time
	}
	if( $lt == 2 ){
	    if( $ll ){
		# NB: strftime obeys LC_TIME for localized day/month names
		# (if locales are supported in the OS and perl)
		$label = strftime("%d/%b", @lt);	# date DD/Mon
	    }else{
		$label = strftime("%a", @lt);		# day of week
	    }
	}
	if( $lt == 3){
	    if( $lt[3] == 1 && $lt[4] == 0 ){
		$label = $lt[5] + 1900;			# year
	    }else{
		$label = strftime("%d/%b", @lt);	# date DD/Mon
	    }
	}
	if( $lt == 4){
	    $label = $lt[5] + 1900; # year
	}
	push @tics, [$t, $ll, $label];
    }
    $me->{grid}{x} = [@tics];
        
}

# it shall be inventoried, and every particle and utensil
# labelled to my will: as, item, two lips,
# indifferent red; item, two grey eyes, with lids to
# them; item, one neck, one chin, and so forth. Were
# you sent hither to praise me?
#   -- Shakespeare, Twelfth Night
sub clabels {
    my $me = shift;

    return unless $me->{draw_data_labels};

    my( $tw, $r, @cl, @cx );
    # round the neck of the bottle was a paper label, with the
    # words 'DRINK ME' beautifully printed on it in large letters
    #   -- Alice in Wonderland
    foreach my $d (@{$me->{data}}){
	my $l = $d->{opts}{label};
	my $c = $d->{opts}{color};
	next unless $l;
	my $w = length($l) * 5 + 6;
	
	if( $tw + $w > $me->{width} - 32 ){
	    $r ++;
	    $tw = 0;
	}
	push @cx, [$l, $tw, $r, $c];
	$tw += $w;
    }

    my $i = 0;
    foreach my $x (@cx){
	my $y = $me->{height} - ($r - $x->[2] + 1) * 10;
	my $c = $x->[3];
	$me->{img}->string(gdTinyFont, $x->[1] + 16, $y, $x->[0], $me->color({color => $c}));
    }
    if( @cx ){
	$me->{margin_bottom} += ($r + 1) * 10;
	$me->adjust();
    }
}

sub plot_data {
    my $me = shift;
    my $data = shift;
    my $opts = shift;

    return unless $data && @$data;
    if( @$data < 3 ){
	# ...
	return;
    }
    
    # 'What did they draw?' said Alice, quite forgetting her promise.
    #   -- Alice in Wonderland
    if( $opts->{style} eq 'line' ){
	# 'You can draw water out of a water-well,' said the Hatter
	#   -- Alice in Wonderland
	$me->draw_line( $data, $opts );
    }
    elsif( $opts->{style} eq 'filled' ){
	# I should think you could draw treacle out of a treacle-well
	#    -- Alice in Wonderland
	$me->draw_filled( $data, $opts );
    }
    elsif( $opts->{style} eq 'range' ){
	# did you ever see such a thing as a drawing of a muchness?
	#    -- Alice in Wonderland
	$me->draw_range( $data, $opts );
    }else{
	croak "unkown graph style--cannot draw";
    }
}

# A flattering painter, who made it his care
# To draw men as they ought to be, not as they are.
#   -- Oliver Goldsmith, Retaliation

sub draw_filled {
    my $me   = shift;
    my $data = shift;
    my $opts = shift;
    
    my $limit = 4 * ($me->{xd_max} - $me->{xd_min}) / @$data;
    my($px, $py) = ($data->[0]{time}, $data->[0]{value});
    
    foreach my $s ( @$data ){
	my $x = $s->{time};
	my $y = $s->{value};
	
	if( $me->xdatapt($x) - $me->xdatapt($px) > 1 ){
	    $px = $x - $limit if $x - $px > $limit;
	    
	    my $poly = GD::Polygon->new;
	    $poly->addPt($me->xdatapt($px), $me->ypt(0));
	    $poly->addPt($me->xdatapt($px), $me->ydatapt($py));
	    $poly->addPt($me->xdatapt($x),  $me->ydatapt($y));
	    $poly->addPt($me->xdatapt($x),  $me->ypt(0));
	    $me->{img}->filledPolygon($poly, $me->color($s, $opts));
	}else{
	    $me->{img}->line( $me->xdatapt($x), $me->ypt(0),
			      $me->xdatapt($x), $me->ydatapt($y),
			      $me->color($s, $opts) );
	}
	$px = $x; $py = $y;
    }
}

sub draw_line {
    my $me   = shift;
    my $data = shift;
    my $opts = shift;

    my $limit = 4 * ($me->{xd_max} - $me->{xd_min}) / @$data;
    my($px, $py) = ($data->[0]{time}, $data->[0]{value});

    foreach my $s ( @$data ){
	my $x = $s->{time};
	my $y = $s->{value};
	$px = $x - $limit if $x - $px > $limit;
	
	$me->{img}->line( $me->xdatapt($px), $me->ydatapt($py),
			  $me->xdatapt($x),  $me->ydatapt($y),
			  $me->color($s, $opts) );
	$px = $x; $py = $y;
    }
    
}

sub draw_range {
    my $me   = shift;
    my $data = shift;
    my $opts = shift;

    my $limit = 4 * ($me->{xd_max} - $me->{xd_min}) / @$data;
    
    my($px, $pn, $pm) = ($data->[0]{time}, $data->[0]{min}, $data->[0]{max});
    foreach my $s ( @$data ){
	  my $x = $s->{time};
	  my $a = $s->{min};
	  my $b = $s->{max};

	  if( $me->xdatapt($x) - $me->xdatapt($px) > 1 ){
	      my $poly = GD::Polygon->new;
	      $px = $x - $limit if $x - $px > $limit;
	      
	      $poly->addPt($me->xdatapt($px), $me->ydatapt($pn));
	      $poly->addPt($me->xdatapt($px), $me->ydatapt($pm));
	      $poly->addPt($me->xdatapt($x),  $me->ydatapt($b));
	      $poly->addPt($me->xdatapt($x),  $me->ydatapt($a));
	      $me->{img}->filledPolygon($poly, $me->color($s, $opts));
	  }else{
	      $me->{img}->line( $me->xdatapt($x),  $me->ydatapt($b),
				$me->xdatapt($x),  $me->ydatapt($a),
				$me->color($s, $opts) );
	  }
	  $px = $x; $pn = $a; $pm = $b;
    }
}

=head1 EXAMPLE IMAGES

    http://argus.tcp4me.com/shots.html

=head1 BUGS

There are no known bugs in the module.

=head1 SEE ALSO
    
    Yellowstone National Park.

=head1 AUTHOR

    Jeff Weisberg - http://www.tcp4me.com

=cut
    ;

1;
