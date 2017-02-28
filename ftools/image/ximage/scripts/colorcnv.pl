#!/usr1/local/bin/perl5
#
#  Take file output from mkcolor, decompose it into red, green, and
#  blue fits images, then combine into color image of specified type
#  (GIF by default)
#
use Getopt::Std;
require "interface.pl";
require "utils.pl";
#
getopts('b:i:o:t:dh');
#** Note for using Getopts **
# Example 
# &Getopts('cde:hmqvw:x:');
# f and n require arguments
#

if (defined $opt_h) {
    print <<EOHELP1;

    Generate graphic from mkcolor FITS output

      colorcnv.pl [-b base] input_mkcolor_file [output graphic]

    FLAGS
       -h - prints this help file
       -d - debug

       the following flags require arguments
       -b - base of encoding in mkcolor image (Default = MKCBASE or 4)

    Usage examples

    Generate GIF graphic (m31.gif) of mkcolor file

      colorcnv.pl m31.img

    Generate TIFF graphic (m31color.tif) of mkcolor file encoded with a
      base of 8

      colorcnv.pl -b 8 m31.img m31color.tif

EOHELP1
&exit_wish;
    }

$tmp = "/tmp";
#
# Debug
$debug = defined($opt_d);
$ext = "jpg";
$flags = "-quality 95";


if ( scalar(@ARGV) > 2 ) {
    die "USAGE: colorcnv.pl [-b base] input_mkcolor_file [output graphic]\n";
}

#
# Input file
if (scalar(@ARGV) == 0) {
    $input=&getScalar("Enter input mkcolor FITS file");
} else {
    $input=$ARGV[0];
}
#
# Output file 
if(scalar(@ARGV) == 2) {
    $output = $ARGV[1];
} else {
    if ( $input =~ /^(.*)\.[^.]*$/ ) {
       $output = "$1.gif";
    } else {
       $output = "$input.gif";
    }
}
#
# Base of encoding
if ( defined($opt_b) ) {
   $base = $opt_b; 
} else {
   @results = &runcom("punlearn fkeypar");
   if ( $results[0] =~ /ERROR/ ) { die; }
   @results = &runcom("fkeypar $input+0 MKCBASE");
   if ( $results[0] =~ /ERROR/ ) { 
      print "Cannot find MKCBASE, defaulting to base of 4\n";
      $base = 4;
    }
   $exist = `pget fkeypar exist`;
   chop $exist;
   if ( $exist ne "yes" ) { 
      print " No MKCBASE keyword, assuming base of 4\n";
      $base = 4;
   } else {
      $base = `pget fkeypar value`;
      chop $base;
   }
}

$gfact = $base;
$bfact = $base*$base;
$scale = int(256./($base-1.));

# Extract fits files for Red, Green, Blue

print "Extract FITS files for Red, Green, Blue...\n\n";


# B = int(RGB/base^2)
$cmd = "fcarith infile=$input outfil=$tmp/b1tmp.img const=$bfact ".
       "ops=DIV clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }
$cmd = "fcarith infile=$tmp/b1tmp.img outfil=$tmp/bmtmp.img const=$bfact ".
       "ops=MUL clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

# RG = RGB - B*base^2
$cmd = "farith infil1=$input infil2=$tmp/bmtmp.img outfil=$tmp/rgtmp.img ".
       "ops=SUB clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

# G = int(RG/base)
$cmd = "fcarith infile=$tmp/rgtmp.img outfil=$tmp/g1tmp.img const=$gfact ".
       "ops=DIV clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }
$cmd = "fcarith infile=$tmp/g1tmp.img outfil=$tmp/gmtmp.img const=$gfact ".
       "ops=MUL clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

# R = RG - G*base
$cmd = "farith infil1=$tmp/rgtmp.img infil2=$tmp/gmtmp.img ".
       "outfil=$tmp/r1tmp.img ops=SUB clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

#
# Multiply by scaling to accommodate convert algorithm (expects range 256)
#
$cmd = "fcarith infile=$tmp/r1tmp.img const=$scale ops=MUL ".
       "outfil=$tmp/rtmp.img clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "fcarith infile=$tmp/g1tmp.img const=$scale ops=MUL ".
       "outfil=$tmp/gtmp.img clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "fcarith infile=$tmp/b1tmp.img const=$scale ops=MUL ".
       "outfil=$tmp/btmp.img clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

if (!$debug) {
   $cmd = "rm $tmp/gmtmp.img $tmp/rgtmp.img $tmp/bmtmp.img ".
             "$tmp/r1tmp.img $tmp/g1tmp.img $tmp/b1tmp.img";
   @results = &runcom($cmd);
   if ( $results[0] =~ /ERROR/ ) { die; }
}

print "Update DATAMIN, DATAMAX\n\n";

$datamax = ($base - 1)*$scale;
$cmd = "echo DATAMIN 0 > $tmp/colorcnv.hdr";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "echo DATAMAX $datamax >> $tmp/colorcnv.hdr";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "fmodhead $tmp/rtmp.img+0 $tmp/colorcnv.hdr";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "fmodhead $tmp/gtmp.img+0 $tmp/colorcnv.hdr";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "fmodhead $tmp/btmp.img+0 $tmp/colorcnv.hdr";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

print "Combine Red, Green, Blue FITS into graphic...\n\n";

$cmd = "fimgtrim $tmp/rtmp.img 0 0 0 0 outfile=$tmp/black.img clobber=yes";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "convert $flags fits:$tmp/rtmp.img $tmp/rtmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "convert $flags fits:$tmp/gtmp.img $tmp/gtmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "convert $flags fits:$tmp/btmp.img $tmp/btmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "convert $flags fits:$tmp/black.img $tmp/black.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

if ( !$debug ) {
   $cmd = "rm $tmp/rtmp.img $tmp/gtmp.img $tmp/btmp.img $tmp/black.img ".
          "$tmp/colorcnv.hdr";
   @results = &runcom($cmd);
   if ( $results[0] =~ /ERROR/ ) { die; }
}

$cmd = "combine $flags -compose ReplaceGreen $tmp/rtmp.$ext $tmp/black.$ext ".
       "$tmp/tmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "combine $flags -compose ReplaceBlue $tmp/tmp.$ext $tmp/black.$ext ".
       "$tmp/rtmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "combine $flags -compose ReplaceRed $tmp/gtmp.$ext $tmp/black.$ext ".
       "$tmp/tmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "combine $flags -compose ReplaceBlue $tmp/tmp.$ext $tmp/black.$ext ".
       "$tmp/gtmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "combine $flags -compose ReplaceRed $tmp/btmp.$ext $tmp/black.$ext ".
       "$tmp/tmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "combine $flags -compose ReplaceGreen $tmp/tmp.$ext $tmp/black.$ext ".
       "$tmp/btmp.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

$cmd = "combine $flags -compose plus $tmp/rtmp.$ext $tmp/gtmp.$ext ".
       "$tmp/tmp.$ext";
if ($debug) { print "$cmd\n" };
system ($cmd);
$cmd = "combine $flags -compose plus $tmp/tmp.$ext $tmp/btmp.$ext $tmp/rgb.$ext";
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }

if ( $output =~ /jpg$/ ) {
   $cmd = "convert -quality 95 $tmp/rgb.$ext $output";
} else {
   $cmd = "convert $tmp/rgb.$ext $output";
}
if ($debug) { print "$cmd\n" };
@results = &runcom($cmd);
if ( $results[0] =~ /ERROR/ ) { die; }
print "Outfile file: $output\n";

if (!$debug) {
   $cmd = "rm $tmp/rgb.$ext $tmp/tmp.$ext $tmp/btmp.$ext $tmp/gtmp.$ext ".
          "$tmp/rtmp.$ext $tmp/black.$ext";
   @results = &runcom($cmd);
   if ( $results[0] =~ /ERROR/ ) { die; }
}
