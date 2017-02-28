#!/usr1/local/bin/perl

$cat= $ARGV[0];
$ra = $ARGV[1];
$dec = $ARGV[2];
$equinox = $ARGV[3];
$radius = $ARGV[4];

$cmd= "xwebquery.pl host='www.asdc.asi.it' url='/cgi-bin/brodev' " .
       "method=POST ".
       "Cat='$cat' ".
       "ra='$ra' dec='$dec' " .
       "eqy='$equinox' radius='$radius' " ;
print "$cmd\n";

@out= `$cmd`;

for ($i=0; $i <= $#out; $i += 1) 
    {
	print $out[$i];
}
