#!/usr/bin/perl
#
# File name: ahbackscal.pl
# Author: E.D. Miller (MIT)
# $Date: 2016/10/25 18:22:31 $
# Version: 0
#
# Fix the BACKSCAL keyword in a spectrum file.
# This calculates BACKSCAL from the extraction region file and exposure map
# (which contains bad pixels and regions).  It assumes the extraction
# region and exposure map are # in the same coordinates.  
# The corrects the behavior of XSELECT, which only sets BACKSCAL according
# to the region file used, and does not account for bad pixels or other
# unexposed areas like outside of the FOV.
#
# Steps:
# 1) Mask the exposure map with the region file so that parts outside the
# region are set to zero.
# 2) Sum up the pixel values in the good region.
# 3) Normalize this sum by the exposure time or some other value.  This is
# set by a parameter to make the tool universal, because some "exposure
# maps" are already normalized to unity.
# 4) Divide this value by the number of pixels in the coordinate system,
# since this is how BACKSCAL is defined.
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

use File::Find;
use File::Spec::Functions;
use File::Basename;
use File::Copy;
use File::Path;
use Cwd 'abs_path';

use ahlog ;
use ahapp ;
use ahgen qw (:ALL);

# turn on AUTOFLUSH
$| = 1;

#########################
#  Input Parameters
#########################

# Query non-standard APE parameters.
my $infile = "";   # Input PHA spectrum file in which to fix BACKSCAL.
my $regfile = "";  # Input region file; this should be the region files used to
                   # extract the spectrum, and it must be in the same 
                   # coordinates as expfile
my $expfile = "";  # Input exposure file; any image can be used, as long as it
                   # is in the same coordinate system as the region file.
my $norm = ""; # The factor used to normalize the exposure map. In general, this
               # should be the exposure time, but it depends on the units of
               # the exposure map, which may be already normalized.
               # MAX = use the maximum value of the exposure map
               # EXPOSURE = use the value in the exposure map "EXPOSURE" keyword
               # value = a floating point number to use

#########################
#  Other Variables 
#########################

# Sum of the good pixel values in the exposure map.
my $sum_of_expo = 0.;

# Number of good pixels
my $num_good_pix = 0.;

# Final calculated BACKSCAL
my $backscal = 0.;

# Array to hold the pixel values.
my @values = ();

# Max value in exposure image
my $values_max = 0.;

# Temporary files
my $tmpimg = "tmp_fix_backscal.img";
my $tmplst = "tmp_fix_backscal.fits"; 

# Size (X,Y) and number of pixels in exposure map.
my $xsize = 0;
my $ysize = 0;
my $num_pix = 0;

# Old value of BACKSCAL from spectrum
my $backscal_old = 0;



#########################
#  Main Code Block 
#########################

ahapp::startup ();

ahapp::begin_processing ();

# Error status
my $ahbackscalerror = 0;

$ahbackscalerror = get_parameters () ;
unless ( $ahbackscalerror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($ahbackscalerror);
}

$ahbackscalerror = initialize () ;
unless ( $ahbackscalerror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($ahbackscalerror);
}

$ahbackscalerror = do_work () ;
unless ( $ahbackscalerror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($ahbackscalerror);
}

$ahbackscalerror = finalize () ;
unless ( $ahbackscalerror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($ahbackscalerror);
}

# We're done.
ahapp::end_processing($ahbackscalerror);

#########################
# Subroutines
#########################

sub get_parameters {
  # Query non-standard APE parameters.
  $infile           = ahapp::query_parameter("infile");  
  $regfile          = ahapp::query_parameter("regfile");
  $expfile          = ahapp::query_parameter("expfile");
  $norm             = ahapp::query_parameter("norm");

  if(ahgen::isRequiredFileNotFound($infile)) { return 1; }
  if(ahgen::isRequiredFileNotFound($regfile)) { return 1; }  
  if(ahgen::isRequiredFileNotFound($expfile)) { return 1; }

  return 0;
}

sub initialize {

  # Add Temporary files
  ahapp::add_temp_file($tmpimg);
  ahapp::add_temp_file($tmplst);

  # Size (X,Y) and number of pixels in exposure map.
  $xsize = ahgen::get_keyword($expfile, 0, "NAXIS1");
  $ysize = ahgen::get_keyword($expfile, 0, "NAXIS2");
  $num_pix = $xsize * $ysize;
  # Old value of BACKSCAL from spectrum
  $backscal_old = ahgen::get_keyword($infile, 1, "BACKSCAL");

  # Write all parameters to the log file.
  ahlog::ah_info "HIGH", ahapp::write_parameters () ;


 return 0;

}


sub do_work {
 
  my $norm_factor;

  if ( uc $norm eq "MAX" ) {
    # Normalize by the max value of the exposure map 
    $norm_factor = "MAX";
  } elsif ( uc $norm eq "EXPOSURE" ) {
    # Normalize by the EXPOSURE keyword in the exposure map
    $norm_factor = ahgen::get_keyword($expfile, 1, "EXPOSURE");
  } else {
    # Normalize by a floating point provided 
    # XXX Need to verify that this is a valid number!
    $norm_factor = $norm;
  }

  # Mask out the parts of the exposure map outside the region and
  # save to a temporary image.
  # The command line version of this command is the following:
  #
  # ftimgcalc \
  #    outfile=tmp_fix_backscal.img \
  #    expr='regfilter("region_SXI_100050012340_bkg.reg",A.P1,A.P2) ? (a) : (0)' \
  #    a=ah100050020sxi_p0100004b0_cl.expo \
  #    clobber=yes 
  #
  ahlog::ah_info "HIGH", "Running ftimgcalc.\n";
  my $expr = "regfilter\(\"$regfile\",A.P1,A.P2\) ? (a) : (0)";
  ahgen::run_ftool("ftimgcalc", "outfile=$tmpimg", "expr=$expr", "a=$expfile", 
                   "clobber=yes");

  # get sum and max value of the VALUE column in exposure map image
  ahlog::ah_info "HIGH", "Running fimgstat.\n";
  ahgen::run_ftool("fimgstat", $tmpimg, 0, "INDEF", "outfile=STDOUT","clobber=yes");
  
  # get values of parameters from fimgstat
  ahgen::run_ftool("pget","fimgstat","sum");
  $sum_of_expo = ahgen::get_tool_stdout;
  ahgen::run_ftool("pget","fimgstat","max");
  $values_max = ahgen::get_tool_stdout;
   

  if ($norm_factor eq "MAX") {
    $norm_factor = $values_max;
  }

  # Normalize by the exposure time to get the number of good pixels.
  # Note that it is fractional, because some of the pixels in SKY
  # coordinates have fractional exposure due to even small attitude
  # wobbles (e.g., that location on the sky moves in and out of a bad
  # pixel).
  $num_good_pix = $sum_of_expo / $norm_factor;

  # Divide the number of good pixels by the total number of pixels in X,Y
  # coordinates to get the BACKSCAL.
  $backscal = $num_good_pix / $num_pix;

  ahlog::ah_info "HIGH", "X size:           $xsize\n";
  ahlog::ah_info "HIGH", "Y size:           $ysize\n";
  ahlog::ah_info "HIGH", "Total pixels:     $num_pix\n";
  ahlog::ah_info "HIGH", "Sum of expmap:    $sum_of_expo\n";
  ahlog::ah_info "HIGH", "Num good pix:     $num_good_pix\n";
  ahlog::ah_info "HIGH", "Norm factor:      $norm_factor (using norm=$norm)\n";
  ahlog::ah_info "HIGH", "Old BACKSCAL:     $backscal_old\n";
  ahlog::ah_info "HIGH", "New BACKSCAL:     $backscal\n";

  if (set_keyword($infile, "SPECTRUM", "BACKSCAL", $backscal)) { return 1; }

  ahlog::ah_info "HIGH", "Spectrum file $infile has been updated.\n";

  return 0;

}


sub finalize {
 
  return 0;

}
