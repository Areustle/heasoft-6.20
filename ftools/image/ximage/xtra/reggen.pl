#!/usr1/local/bin/perl5
#
#  Take text file with columns specifying x, y and size
#  in detector coordinates, and output a region file for each
#  row and an XIMAGE script which runs a macro for each region file.
#
#  Jan. 18, 2000 -- Micah Johnson
#  Version 1.0
#  Previously named cntbox.pl. Made so any command could be used
#  in the generated script instead of counts.  Also, added shortcuts for
#  excess and search output.
#
#  Mar. 9, 2000 -- Micah Johnson
#  Version 1.1
#  Revamped flags. Now includes circle (-c), SEARCH and EXCESS
#  output is automatically identified.  Command (-m) can be @script
#  Region file names padded with zeroes
#
#  Sept. 14, 2000 -- Micah Johnson
#  Version 1.2
#  Minor fix: The user no longer has to take into account a blank column 
#  as column #1 when there is space before the first column of values.
#
use Getopt::Std;
require "interface.pl";
require "utils.pl";
#
getopts('p:s:i:o:d:m:hc');
#** Note for using Getopts **
# Example 
# &Getopts('cde:hmqvw:x:');
# f and n require arguments
#

if (defined $opt_h) {
    print <<EOHELP1;

    Generate region files from text file containing columns for position
    and size in detector coordinates, and create an XIMAGE script
    which runs commands for each.

    Position and size columns need not be specified for SEARCH
    and EXCESS output from XIMAGE.  Those files will be automatically
    identified and the proper columns read.  The -p and -s flags
    may be used to override this default behavior.

    FLAGS
       -h - prints this help file
       -c - write circle regions instead of default boxes

       the following flags require arguments
       -p - position columns "x,y" (Default = 1,2)
       -s - size column [half-width for box, radius for circle] (Default = 3)
       -i - input text file 
       -d - region file directory
       -o - output script file (Default=reggen.xco)
            Will contain a line for each region, based on the macro settings
       -m - macro [form: \@script or command] (Default = counts)
            script should contain %1% where regionfile is to be inserted
            for script the line written is "\@script [regionfile]"
            for command the line written is "command/reg=[regionfile]"

    Usage examples

    Generate region files (in current dir) and script
      for search command output file

      reggen.pl -i output.srch

    Generate region files ( in xsregions dir) and script 
      to remove excesses

      reggen.pl -d xsregions -m remove/const=0 -i output.xs

EOHELP1
&exit_wish;
    }
#
# Circle logical
if ( defined($opt_c) ) {
    $circle = 1; 
} else {
    $circle = 0;
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
    $outxco = "reggen.xco";
}

#
# Region file directory
if ( defined($opt_d) ) {
    $regdir = $opt_d; 
} else {
    $regdir = "";
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
    $cmd = "counts";
}

if ( $regdir ) {
   if ( -e $regdir ) {
      die " Directory for region files already exists: $regdir\n";
   }
   mkdir $regdir, 0777 or die " Failed to create directory: $regdir\n";
   if ( $regdir !~ /\/$/ ) { $regdir .= "/" };
}

open INFILE, "$input" or die "Failed to open text file: $input\n";
@inrows = <INFILE>;
close INFILE;
open OUTXCO, ">$outxco" or die "Failed to open output script: $outxco\n";

# Determine input file root

if ( $input =~ /([^\/]*)\.[^\/]*$/ ) {
   $inroot = $1;
} elsif ( $input =~ /([^\/]*)$/ ) {
   $inroot = $1;
} else {
   $inroot = "reggen";
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

   if ( $rowval[0] =~ /^$/ ) { shift @rowval; }
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
   if ( $circle ) {
      $size = $rowval[$scol-1];
   } else { 
      $size = 2.*$rowval[$scol-1];
   }

   $regfile = sprintf "%s%s-%0${maxwid}d.reg",$regdir,$inroot,$numrows;
#  $regfile = "$regdir$inroot-$numrows.reg";
   open REGFILE, ">$regfile" or die "Failed to open region file: $regfile\n";
   if ( $circle ) {
      print REGFILE " CIRCLE($xpix, $ypix, $size)\n";
   } else {
      print REGFILE " BOX($xpix, $ypix, $size, $size, 0)\n";
   }
   close REGFILE;

   if ( $cmd =~ /^@/ ) {
      print OUTXCO "$cmd \"\"$regfile\"\"\n";
   } else {
      print OUTXCO "$cmd/reg=\"$regfile\"\n";
   }

}

close OUTXCO;
