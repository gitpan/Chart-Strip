#!/usr/local/bin/perl
# -*- perl -*-

use Chart::Strip;

my $img = Chart::Strip->new(
			    title   => 'Happiness of Club Members',
			    x_label => 'When',
			    y_label => 'Happiness Factor',
			    grid_on_top => 1,
			    );

my( $t, $data1, $data2, $data3, $data4 );
my $dt = 3600;

for($t=$^T; $t<$^T+$dt; $t+=$dt/1000){
    my $v = 10 + rand(5);
    push @$data1, {time => $t, value => $v};
    push @$data2, {time => $t, value => $v - 2 + rand(2) };
}
for($t=$^T; $t<$^T+$dt; $t+=$dt/50){
    my $v = 10 + rand(5);
    my $x = 5 + rand(2);
    push @$data3, {time => $t, value => $v};
    push @$data4, {time => $t, value => $x, min => $x - 1, max => $x + 1 + rand(1) };
}

$img->add_data( $data1, { label => 'Davey',  style => 'filled', color => '80FFFF' } );
$img->add_data( $data4, { label => 'Shelly', style => 'range',  color => '008844' } );
$img->add_data( $data4, {                    style => 'line',   color => '00aa44' } );
$img->add_data( $data2, { label => 'Jenna',  style => 'line',   color => 'FF80FF' } );
$img->add_data( $data3, { label => 'Harold', style => 'line',   color => '802000' } );

print $img->png();

