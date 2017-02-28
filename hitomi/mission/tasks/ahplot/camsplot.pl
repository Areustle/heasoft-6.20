#!/usr/bin/perl
#
# File name: ahplot.pl
# Author: M. S. Dutka NASA GSFC
# $Date: 2016/10/25 13:46:18 $
# Version: 0
#
# Create a region file and run the extractor tool to create an SXS
# image, lightcurve and spectrum file
#
# Tool Dependencies:
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
my $offset     =""; # Offset X axis to 0?
my $maxpts     =""; # Maximum points per graph
my $pltcmd     =""; # Any legal PLT command

my $telescop   ="HITOMI";

my $ahploterror = 0;  # ahplot exit status

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$ahploterror = get_parameters () ;
unless ( $ahploterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($ahploterror);
}

$ahploterror = initialize () ;
unless ( $ahploterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($ahploterror);
}

$ahploterror = do_work () ;
unless ( $ahploterror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($ahploterror);
}

$ahploterror = finalize () ;
unless ( $ahploterror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($ahploterror);
}

# We're done.
ahapp::end_processing($ahploterror);


#########################
# Subroutines
#########################

sub get_parameters {

  $infile     = ahapp::query_parameter("infile");
  $outfile    = ahapp::query_parameter("outfile");
  $offset     = ahapp::query_parameter("offset");
  $pltcmd     = ahapp::query_parameter("pltcmd");

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;
  
  # Input file checking
  if (isRequiredFileNotFound($infile)) { return 1; }
  if (removeOutputFileIfClobbering($outfile,$ahapp::clobber) ) { return 1; }

  # Check if the pltcmd is a text file
  # If so, make a copy of it to a temporary file
  # and add plot and quit commands.
  #  
  # These are failsafes in case the user did not put
  # either plot or quit into their pco file
  if ( $pltcmd =~ /^@/ ) {
    my $pltfile    ="ahplot.pco";
    ahapp::add_temp_file($pltfile);
    if(isRequiredFileNotFound(substr($pltcmd,1))) { return 1; }
    File::Copy::copy(substr($pltcmd,1),$pltfile);
    open PLT,">>$pltfile";
    print PLT "plot\n"; # force pgplot to plot the file
    print PLT "quit\n"; # force pgplot to quit
    close PLT;
    $pltcmd = "\@$pltfile";
  }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;
  my $filename = $infile;

  # Get number of rows in temporary file to pass to fplot
  $maxpts = ahgen::get_keyword($filename,"CAMS_OFFSETS","NAXIS2");

  # Run FPLOT to create the postscript file
  $status = ahgen::run_ftool("fplot",
                             "infile=$filename",
                             "maxpts=$maxpts",
                             "offset=$offset",
                             "xparm=TIME",
                             "yparm=X1 Y1 X2 Y2",
                             "rows=-",
                             "device=$outfile/ps",
                             "pltcmd=$pltcmd",
                             "binmode=DEFAULT",
                             "sensecase=no",
                           );

  if ( $status ) {
    ahlog::ah_err "fplot failed.";
    return $status;
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  return 0;
  
}

# ------------------------------------------------------------------------------
