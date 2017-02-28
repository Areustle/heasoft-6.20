#!/usr1/local/bin/perl

$ra = $ARGV[0];
$dec = $ARGV[1];
$equi = $ARGV[2];
$fits = $ARGV[3];
$radius = $ARGV[4];
$name=$ARGV[5];

if( $name eq '' ||
    $ra eq '' ||
    $dec eq '' ||
    $equi eq '' ||
    $radius eq '' ||
    $fits eq '') {

    die "
 
Usage st.pl     ra       dec   equinox    file format   image side  survey
          (degrees real )   B(J)year  fits or gif  (arcmin) (1 1st 2 2nd 3 2nd or 1st if 2nd not available)
        (hh:mm:ss. dd:mm:ss. )   

 e.g.  st.pl 180.000 30.000 B1950 f 5.0 3

\n";
}


$cmd= "xwebquery.pl host='stdatu.stsci.edu' url='/cgi-bin/dss_search' " .
       "method=GET ".
       "v='$name' ".
       "r='$ra' d='$dec' " .
       "e='$equi' " .
       "f='$fits' h='$radius' w='$radius' " .
       "c=none" ;

print STDERR "$cmd\n";

@out= `$cmd`;

for ($i=0; $i <= $#out; $i += 1) 
    {
	print $out[$i];
}
