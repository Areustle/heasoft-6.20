#!/usr1/local/bin/perl5
#
#  Take text file with columns specifying x, y and size
#  in detector coordinates, and output a script which reads an
#  image with that size and center and calls a separate script
#  for each read in file.
#
#  Mar. 16, 2000 -- Micah Johnson
#  Version 1.0
#
use Getopt::Std;
require "interface.pl";
require "utils.pl";
#
getopts('p:s:i:o:r:m:h');
#

if (defined $opt_h) {
    print <<EOHELP1;

    Read text file containing columns for position and size in 
    detector coordinates, and create an XIMAGE script which 
    reads each source in as the entire image, then runs another
    script for each.

    Position and size columns need not be specified for SEARCH
    and EXCESS output from XIMAGE.  Those files will be automatically
    identified and the proper columns read.  The -p and -s flags
    may be used to override this default behavior.

    FLAGS
       -h - prints this help file

       the following flags require arguments
       -p - position columns "x,y" (Default = 1,2)
       -s - size column [half-width for box, radius for circle] (Default = 3)
       -i - input text file 
       -o - output script file (Default=srcimg.xco)
            Will contain a read and script line for each source
       -m - macro [form: \@script or command] (Default = display)
            script should contain %1% where unique source id is to be inserted
            for script the line written is "\@script [srcid]"
            for command the line written is "command"
       -r - rebin to use in read script (default = 1)

    Usage examples

    Generate "slide show" script, which displays each source image

      srcimg.pl -i output.srch -o slideshow
      ximage \@slideshow fits_file

    Generate script which outputs gif files for each source image

      srcimg.pl -m \@gengif -i output.xs -o allgif
      ximage \@allgif fits_file
    
      gengif.xco contains the following lines:

        cpd %1%.gif/gif
        disp

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
# Output script
if ( defined($opt_o) ) {
    $outxco = $opt_o; 
    if ( /^[^.]+$/ ) { $outxco = $outxco . ".xco"; }
} else {
    $outxco = "srcimg.xco";
}
#
# X,Y column numbers
$xcol = 0;
$ycol = 0;
if ( defined($opt_p) ) {
   @xy = split /,/, $opt_p;
   if ( scalar(@xy) != 2 ) {
      die "Usage: -p xcol,ycol";
   }
   $xcol = $xy[0];
   $ycol = $xy[1];
}

#
# Size column number
$scol = 0;
if ( defined($opt_s) ) { 
   $scol = $opt_s;
}
#
# Macro
if ( defined($opt_m) ) {
    $cmd = $opt_m; 
} else {
    $cmd = "disp";
}
#
# Rebin
if ( defined($opt_r) ) {
    $rebin = $opt_r; 
} else {
    $rebin = 1;
}

open INFILE, "$input" or die "Failed to open text file: $input\n";
@inrows = <INFILE>;
close INFILE;
open OUTXCO, ">$outxco" or die "Failed to open output script: $outxco\n";
print " Writing $outxco...\n";

# Determine input file root

if ( $input =~ /([^\/]*)\.[^\/]*$/ ) {
   $inroot = $1;
} elsif ( $input =~ /([^\/]*)$/ ) {
   $inroot = $1;
} else {
   $inroot = "srcimg";
}
# Search for EXCESS, SEARCH comments
if ( ! $xcol || ! $ycol || ! $scol ) {
   $type = "";
   foreach $line (@inrows) {
      if ( $line =~ /^! +Non-contiguous Excesses/ ) { $type = "excess"; }
      if ( $line =~ /^! +Source search results/ ) { $type = "search"; }
   }
   if ( $type eq "excess" ) {
      print " Identified as EXCESS output, assuming -p 4,5 -s 7\n";
      $xcol = 4;
      $ycol = 5;
      $scol = 7;
   } elsif ( $type eq "search" ) {
      print " Identified as SEARCH output, assuming -p 2,3 -s 10\n";
      $xcol = 2;
      $ycol = 3;
      $scol = 10;
   } else {
      print " Position and/or size unspecified, assuming -p 1,2 -s 3\n";
      $xcol = 1;
      $ycol = 2;
      $scol = 3;
   }

}
   
$numrows = 0;
$partrow = "";
$maxwid = length(scalar(@inrows));
foreach $line (@inrows) {

   if ( $line =~ /^!/ ) { next; }
   chop $line;
   if ( $line =~ /(^.*)-$/ ) {
      $partrow .= $1;
      next;
   } elsif ( $partrow ) {
      $row = $partrow . $line;
      $partrow = "";
   } else {
      $row = $line;
   }
   
   @rowval = split /\s+/, $row;
   $numrows ++;

   $numcols = scalar(@rowval);
   if ( $xcol > $numcols ) {
      print " X column number $xcol not in file\n";
      last;
   }
   if ( $ycol > $numcols ) {
      print " Y column number $ycol not in file\n";
      last;
   }
   if ( $scol > $numcols ) {
      print " Size column number $scol not in file\n";
      last;
   }
   $xpix = $rowval[$xcol-1]; 
   $ypix = $rowval[$ycol-1]; 
   $size = int(2.*$rowval[$scol-1]/$rebin);

   $srcid = sprintf "%s-%0${maxwid}d",$inroot,$numrows;

   print OUTXCO "read/size=$size/xp=$xpix/yp=$ypix/rebin=$rebin %1%\n";
   if ( $cmd =~ /^@/ ) {
      print OUTXCO "$cmd $srcid\n";
   } else {
      print OUTXCO "$cmd\n";
   }

}

close OUTXCO;
