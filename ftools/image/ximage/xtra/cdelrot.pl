#!/usr1/local/bin/perl5
#
#  Extract CD matrix from FITS file and calculate
#  corresponding cdelt1, cdelt2, crota2
#
#  Nov 17, 1999 -- Micah Johnson
#  Version 1.0
#
use Getopt::Std;
require "interface.pl";
require "utils.pl";
#
getopts('i:t:a:dh');
#** Note for using Getopts **
# Example 
# &Getopts('cde:hmqvw:x:');
# f and n require arguments
#

if (defined $opt_h) {
    print <<EOHELP1;

    FLAGS
       -h - Prints this help file
       -d - Debug

       the following flags require arguments
       -i - input FITS file
       -t - template file (optional output)
       -a - tolerance angle (Default 0.001)

    Usage example

    Calculate CD matrix for hst.fits create template header.txt
    cdelrot.pl -t header.txt -i hst.fits

    Print derived cdelt and crota values
    cdelrot.pl hst.fits

 
EOHELP1
&exit_wish;
    }
#
# Input file 
if(defined $opt_i){
    $input=$opt_i;
}elsif (scalar(@ARGV) == 1) {
    $input=$ARGV[0];
}else {
    $input=&getScalar("Enter input file name");
}

#
# Template file
$template = "";
if ( defined($opt_t) ) { $template = $opt_t; }

#
# Debug
$debug = defined($opt_d);

#
# Tolerance for derived rotation angles to agree
if ( defined($opt_a) ) {
   $toler = $opt_a;
} else {
   $toler = 0.001;
}

$rad = 57.2957795131;

#
#  Assume +0 if no extension given
#
if ( $input !~ /\+[0-9]+$/ && $input !~ /\[[0-9]+\]$/ ) { 
   $input = $input."+0";
}

#
#  Look up CD keywords
#
$cdfound = 0;
print "Look up CD matrix for $input\n";

@results = &runcom("punlearn fkeypar");
if ( $results[0] =~ /ERROR/ ) { die; }
@results = &runcom("fkeypar $input CD1_1");
if ( $results[0] =~ /ERROR/ ) { die; }
$exist = `pget fkeypar exist`;
chop $exist;
if ( $exist ne "yes" ) { 
   print " Assuming CD1_1 = 0\n";
   $cd11 = 0.;
} else {
   $cdfound = 1;
   $cd11 = `pget fkeypar value`;
   chop $cd11;
}
print " CD1_1: $cd11\n";


@results = &runcom("fkeypar $input CD1_2");
if ( $results[0] =~ /ERROR/ ) { die; }
$exist = `pget fkeypar exist`;
chop $exist;
if ( $exist ne "yes" ) { 
   print " Assuming CD1_2 = 0\n";
   $cd12 = 0.;
} else {
   $cdfound = 1;
   $cd12 = `pget fkeypar value`;
   chop $cd12;
}
print " CD1_2: $cd12\n";

@results = &runcom("fkeypar $input CD2_1");
if ( $results[0] =~ /ERROR/ ) { die; }
$exist = `pget fkeypar exist`;
chop $exist;
if ( $exist ne "yes" ) { 
   print " Assuming CD2_1 = 0\n";
   $cd21 = 0.;
} else {
   $cdfound = 1;
   $cd21 = `pget fkeypar value`;
   chop $cd21;
}
print " CD2_1: $cd21\n";

@results = &runcom("fkeypar $input CD2_2");
if ( $results[0] =~ /ERROR/ ) { die; }
$exist = `pget fkeypar exist`;
chop $exist;
if ( $exist ne "yes" ) { 
   print " Assuming CD2_2 = 0\n";
   $cd22 = 0.;
} else {
   $cdfound = 1;
   $cd22 = `pget fkeypar value`;
   chop $cd22;
}
print " CD2_2: $cd22\n";

$cdfound or die "No CD matrix keywords found\n";

#
# Algorithm from GETROT (IDLastro)
#


if ( $cd21 == 0 || $cd12 == 0 ) {
   $crota2 = 0.;
   $cdelt1 = $cd11;
   $cdelt2 = $cd22;
} else {

# IDL way
   $det = $cd11*$cd22 - $cd12*$cd21;
   if ( $det < 0 ) { $sgn = -1; } else { $sgn = 1; }
   if ( $debug && $det > 0 ) { 
      print "WARNING: Right-hand coordinate system\n";
   }
   $cdelt1 = $sgn*sqrt($cd11**2 + $cd21**2);
   $cdelt2 =     sqrt($cd12**2 + $cd22**2);
   if ( $cdelt1 > 0 ) { $sgn1 = 1; } else { $sgn1 = -1; }
   $rot  = atan2(     -$cd21/$rad, $sgn1*$cd11/$rad);
   $rot2 = atan2($sgn1*$cd12/$rad,       $cd22/$rad);
   if ( $debug ) {
      printf "cdelt1: %e\n", $cdelt1;
      printf "cdelt2: %e\n", $cdelt2;
      printf "rot   : %f\n", $rot*$rad;
      printf "rot2  : %f\n", $rot2*$rad;
   }

# CFITSIO way
#  $rot  = atan2($cd21*$rad, $cd11*$rad);
#  $rot2 = atan2(-$cd12*$rad, $cd22*$rad);
   if ( $debug ) {
      $crot = atan2($cd21/$rad, $cd11/$rad);
      $crot2 = atan2(-$cd12/$rad, $cd22/$rad);
      printf "CFITSIO rot : %f\n", $rad*$crot;
      printf "CFITSIO rot2: %f\n", $rad*$crot2;
      printf "CFITSIO cdelt1: %e\n", $cd11 / cos(($crot+$crot2)/2.);
      printf "CFITSIO cdelt1: %e\n", $cd22 / cos(($crot+$crot2)/2.);
   }
   $rdiff = abs($rot - $rot2);
   if ( $rdiff > $toler ) {
      print " Angles don't agree : $rot $rot2\n";
      print " Difference: $rdiff  Tolerance: $toler\n";
      print " Looks like there is some skewness between the axes.\n";
      exit;
   }
#  $cdelt1 = $cd11 / cos(($rot+$rot2)/2.);
#  $cdelt2 = $cd22 / cos(($rot+$rot2)/2.);
   $crota2 = (($rot + $rot2)*$rad)/2.;
}
#
#  Check for swapped RA/DEC
#
@results = &runcom("fkeypar $input CTYPE1");
if ( $results[0] =~ /ERROR/ ) { die; }
$exist = `pget fkeypar exist`;
chop $exist;
if ( $exist ne "yes" ) { 
   die "Failed to get CTYPE1\n";
} else {
   $ctype1 = `pget fkeypar value`;
   chop $ctype1;
}
print " CTYPE1: $ctype1\n";
@results = &runcom("fkeypar $input CTYPE2");
if ( $results[0] =~ /ERROR/ ) { die; }
$exist = `pget fkeypar exist`;
chop $exist;
if ( $exist ne "yes" ) { 
   die "Failed to get CTYPE2\n";
} else {
   $ctype2 = `pget fkeypar value`;
   chop $ctype2;
}
print " CTYPE2: $ctype2\n";

$swap = 0;
if ( substr($ctype1,1,4) eq "DEC-" || substr($ctype1,2,3) eq "LAT" ) {
   $swap = 1;
   print "The latitudinal axis is given first, so swap them\n";
#  $crota2 = 90. - $crota2;
   $crota2 = 90. + $crota2;
   if ( $crota2 >= 360. ) { $crota2 = $crota2 - 360.; }
   $temp = $cdelt1;
   $cdelt1 = $cdelt2;
   $cdelt2 = -$temp;

   @results = &runcom("fkeypar $input CRVAL1");
   if ( $results[0] =~ /ERROR/ ) { die; }
   $exist = `pget fkeypar exist`;
   chop $exist;
   if ( $exist ne "yes" ) { 
      die "Failed to get CRVAL1\n";
   } else {
      $crval1 = `pget fkeypar value`;
      chop $crval1;
   }
   print " CRVAL1: $crval1\n";

   @results = &runcom("fkeypar $input CRVAL2");
   if ( $results[0] =~ /ERROR/ ) { die; }
   $exist = `pget fkeypar exist`;
   chop $exist;
   if ( $exist ne "yes" ) { 
      die "Failed to get CRVAL2\n";
   } else {
      $crval2 = `pget fkeypar value`;
      chop $crval2;
   }
   print " CRVAL2: $crval2\n";

   $temp = $crval1;
   $crval1 = $crval2;
   $crval2 = $temp;

   $temp = $ctype1;
   $ctype1 = $ctype2;
   $ctype2 = $temp;
}
#
#  Output values
#

print "\nFITS template output: $template\n\n";
if ( $template ) {
  $opstr = ">$template";
} else {
  $opstr = ">-";
}

   open TEMPLATE, $opstr or die "Failed to open for output\n";
   if ( $swap ) {
      print TEMPLATE "CRVAL1 $crval1\n";
      print TEMPLATE "CTYPE1 $ctype1\n";
      print TEMPLATE "CRVAL2 $crval2\n";
      print TEMPLATE "CTYPE2 $ctype2\n";
   }
   print TEMPLATE "CDELT1 $cdelt1 / derived from CD matrix\n";
   print TEMPLATE "CDELT2 $cdelt2 / derived from CD matrix\n";
   print TEMPLATE "CROTA2 $crota2 / derived from CD matrix\n";
   print TEMPLATE "OLDCD1_1 $cd11 / old CD matrix value\n";
   print TEMPLATE "OLDCD1_2 $cd12 / old CD matrix value\n";
   print TEMPLATE "OLDCD2_1 $cd21 / old CD matrix value\n";
   print TEMPLATE "OLDCD2_2 $cd22 / old CD matrix value\n";
   print TEMPLATE "-CD1_1\n";
   print TEMPLATE "-CD1_2\n";
   print TEMPLATE "-CD2_1\n";
   print TEMPLATE "-CD2_2\n";

   close TEMPLATE;
