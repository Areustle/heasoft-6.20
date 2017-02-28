#!/usr/bin/perl
#
# File name: gtiplot.pl
# Author: 
# $Date: 2016/09/21 17:01:28 $
# Version: 0
#
# +++CHANGE
#
# Tool Dependencies:
#   fplot
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#
# Modification History:
#

# Set up
use strict;
use warnings;

use ahlog ;
use ahapp ;
use ahgen qw (:ALL) ;
use File::Copy;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

my $infile     =""; # Input file
my $outfile    =""; # Output file
my $yvalue     =""; # position output intervals
my $xcol       = ""; # x column name
my $dxcol      = ""; # dx column name
my $ycol       = ""; # y column name and type
my $clobber    ="";   # clobber

my $gtiext     ="";   # gti extension name
my $error_status = 0; # error status
my @start = ();  # array of start gti times
my @stop = ();   # array of stop gti times
my @x = ();      # array of x values
my @dx = ();     # array of dx values
my @y = ();      # array of y values
my @ycols = ();   # array of y column names
my @yvalues = (); # array of y values
my $numycols = 0; # number of y columns

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$error_status = get_parameters () ;
unless ( $error_status == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($error_status);
}

$error_status = initialize () ;
unless ( $error_status == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($error_status);
}

$error_status = do_work () ;
unless ( $error_status == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($error_status);
}

$error_status = finalize () ;
unless ( $error_status == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($error_status);
}

# We're done.
ahapp::end_processing($error_status);


#########################
# Subroutines
#########################

sub get_parameters {

  $infile     = ahapp::query_parameter("infile");
  $outfile    = ahapp::query_parameter("outfile");
  $yvalue     = ahapp::query_parameter("yvalue");
  $xcol       = ahapp::query_parameter("xcol");
  $dxcol      = ahapp::query_parameter("dxcol");
  $ycol       = ahapp::query_parameter("ycol");
  $clobber    = $ahapp::clobber ? "yes" : "no";


  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;

  # Input file checking
  if (isRequiredFileNotFound($infile)) { return 1; }
  if (removeOutputFileIfClobbering($outfile,$ahapp::clobber) ) { return 1; }
  
  # parse lists of y column names and y values
  @yvalues = split(",",$yvalue);
  @ycols = split (",",$ycol);



  # Check to see if a column type is present for each entry in ycols array if 
  # not set 1D as the default column type
  my @ycols_tmp = ();
  for (my $ii=0; $ii<scalar(@ycols); ++$ii) {
    @ycols_tmp = split(" ",$ycols[$ii]);
    if (scalar(@ycols_tmp) < 2) {
      $ycols[$ii] = "$ycols_tmp[0] 1D";
    }
  }
  
  # Make sure ycols and yvalues are the same length
  if (scalar(@ycols) != scalar(@yvalues)) {
    ahlog::ah_err "There must be the same number of y column names as y values";
    return 1;
  }

  # set the number of y columns
  $numycols = scalar(@ycols);

  # Parse input file name
  my @infile_name = ahgen::parse_file_name($infile);
  
  $infile = $infile_name[0];
  if ($infile_name[1] eq "") {
    $gtiext = "GTI";    
  } else {
    $gtiext = $infile_name[1];
  }

  if (0 == ahgen::check_hdu_exists($infile_name[0],$gtiext)) {
    ahlog::ah_err "HDU $gtiext does not exist";
    return 1;
  }

  # Read gti column in specified extension
  @start = ahgen::read_column($infile,$gtiext,"START");
  @stop = ahgen::read_column($infile,$gtiext,"STOP");
 
     
  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;
 
  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;
  
  ahapp::add_temp_file("gti.coldef");
  ahapp::add_temp_file("gti.dat");

  for (my $ii=0; $ii<scalar(@start); ++$ii) {
    $x[$ii] = ($start[$ii]+$stop[$ii])/2;
    $dx[$ii] = ($stop[$ii]-$start[$ii])/2;
    for (my $jj=0; $jj<$numycols; ++$jj) {
      $y[$ii][$jj] = $yvalues[$jj];
    }
  }

  # Write output column definition file
  open (my $fhColdef, ">","gti.coldef");
  print $fhColdef "$xcol 1D \n";
  print $fhColdef "$dxcol 1D \n";
  for (my $jj=0; $jj<$numycols; ++$jj) {
    print $fhColdef "$ycols[$jj] \n";
  }
  close $fhColdef;



  # Write output dat file
  my $ycols_string = "";
  open (my $fhDat, ">","gti.dat");
  for (my $ii=0; $ii<scalar(@x); ++$ii) {
    $ycols_string = "";
    for (my $jj=0; $jj<$numycols; ++$jj) {
      $ycols_string = $ycols_string . "$y[$ii][$jj] ";
    } 
    print $fhDat $x[$ii] . " " . $dx[$ii] . " " . $ycols_string . "\n";
  }
 
  
 ahgen::run_ftool("ftcreate",
                  "cdfile=gti.coldef",
                  "datafile=gti.dat",
                  "outfile=$outfile",
                  "extname=GTI",
                  "clobber=$clobber");


  return 0;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  return 0;
  
}

# ------------------------------------------------------------------------------
