#!/usr1/local/bin/perl5
#
#  Generate a viewport configuration file for a configuration of
#  NxM to use with ximage's viewport command.
#
#  Mar. 17, 2000 -- Micah Johnson
#  Version 1.0
#
use Getopt::Std;
require "interface.pl";
require "utils.pl";
#
getopts('d:v:s:o:m:hc');
#

if (defined $opt_h) {
    print <<EOHELP1;

    Generate a viewport configuration file for a configuration of NxM
    viewports (i.e. N viewports wide and M viewports high) to use with
    ximage's viewport command.  The minimum amount of info needed by
    this script is the dimensions of the configuration (e.g. 2x2).  By
    default a /GIF device is assumed, which has dimensions of 850x680.
    The dimensions of some PGPLOT graphic devices, such as /XW, vary
    from system to system.  To find the actual dimensions, display an
    image in ximage with the device and run the command
    'viewport/devsz'.

    FLAGS
       -h - print help
       -c - turn on chat (for debugging)

       the following flags require arguments
       -v - viewport configuration (NxM or N,M)
       -d - device size in pixels  (NxM or N,M) Default=850x680
       -s - image size in pixels (NxM or N,M) square images are
            assumed by default
       -m - minimum margin from edge in device pixels
       -o - output viewport configuration file (Default=NxM.vpc)

    Usage examples

    Generate 2x2.vpc viewport configuration file for /GIF device

      vpcgen.pl -v 2x2 

    Generate 3x2.vpc viewport configuration file for /XW device
     (812x642 in this case) intended for images read with szx=300/szy=200

      vpcgen.pl -v 3x2 -d 812x642 -s 300x200 

EOHELP1
&exit_wish;
    }
#
# Chat setting
if ( defined($opt_c) ) {
   $chat = 1;
} else {
   $chat = 0;
}
#
# width, height of viewport configuration
$vpx = 0;
$vpy = 0;
if ( defined($opt_v) ) {
   $input=$opt_v;
} else {
   $input=&getScalar("Enter viewport configuration (NxM): ");
}
@xy = split /[,xX]/, $input;
if ( scalar(@xy) != 2 ) {
   die "Usage: -v NxM";
}
$vpx = $xy[0];
$vpy = $xy[1];
   
#
# width, height of device
$dvx = 0;
$dvy = 0;
if ( defined($opt_d) ) {
   @xy = split /[,xX]/, $opt_d;
   if ( scalar(@xy) != 2 ) {
      die "Usage: -d NxM";
   }
   $dvx = $xy[0];
   $dvy = $xy[1];
} else {
   $dvx = 850;
   $dvy = 680;
}
#
# width, height of image
$szx = 0;
$szy = 0;
if ( defined($opt_s) ) {
   @xy = split /[,xX]/, $opt_s;
   if ( scalar(@xy) != 2 ) {
      die "Usage: -s NxM";
   }
   $szx = $xy[0];
   $szy = $xy[1];
} else {
   $szx = 256;
   $szy = 256;
}
#
# minimum margin in device pixels
if ( defined($opt_m) ) {
   $minmar = $opt_m;
} else {
   if ( $vpx > $vpy ) {
      $minmar = int(0.1*$dvx);
   } else {
      $minmar = int(0.1*$dvy);
   }
   if ( $chat ) { print " Minimum margin: $minmar\n"; }
}
#
# Output vpc file
if ( defined($opt_o) ) {
    $outvpc = $opt_o; 
    if ( /^[^.]+$/ ) { $outvpc = $outvpc . ".vpc"; }
} else {
    $outvpc = "${vpx}x${vpy}.vpc";
}

open OUTVPC, ">$outvpc" or die "Failed to open output file: $outvpc\n";
print " Writing $outvpc...\n";
print OUTVPC "#\n";
print OUTVPC "# $outvpc\n";
print OUTVPC "# Specs: vpcgen.pl -v ${vpx}x${vpy} -d ${dvx}x${dvy} ",
                                "-s ${szx}x${szy} -m $minmar\n";
print OUTVPC "# Usage in ximage: viewport $outvpc\n";
print OUTVPC "#\n";

# Find size of individual viewports 
$xmax = int(($dvx - 2*$minmar)/$vpx + 0.0);
$ymax = int(($dvy - 2*$minmar)/$vpy + 0.0);
if ( $chat ) { print " Max size of viewport x: $xmax y: $ymax\n"; }
if ( $szx/$szy < $xmax/$ymax ) {
   if ( $chat ) { print " Limiting direction: Y\n"; }
   $y1 = $ymax;
   $x1 = $y1*($szx/$szy);
} else {
   if ( $chat ) { print " Limiting direction: X\n"; }
   $x1 = $xmax;
   $y1 = $x1*($szy/$szx);
}
if ( $chat ) { print " Size of viewport (pix): $x1 $y1\n"; }

$xmar = int(($dvx - $vpx*$x1)/2 + 0.0);
$ymar = int(($dvy - $vpy*$y1)/2 + 0.0);
if ( $chat ) { print " Final margin (pix): $xmar $ymar\n"; }

for ($j = 1; $j <= $vpy; $j++ ) {
   for ($i = 1; $i <= $vpx; $i++ ) {
      $v1 = ($xmar + ($i-1)*$x1)/$dvx;
      $v2 = ($xmar + $i*$x1)/$dvx;
      $v3 = 1.0 - ($ymar + $j*$y1)/$dvy;
      $v4 = 1.0 - ($ymar + ($j-1)*$y1)/$dvy;
      print OUTVPC "$v1 $v2 $v3 $v4\n";
      if ( $chat ) { 
         printf "%f %f %f %f\n",$v1*$dvx, $v2*$dvx, $v3*$dvy, $v4*$dvy; 
      }
   }
}

close OUTVPC;
