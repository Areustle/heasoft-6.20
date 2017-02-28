#!/usr/bin/perl
#
# File name: ahplot.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/10/26 17:57:44 $
# Version: 0
#
# Plot the R.A., DEC and ROLL from the given attitude file
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
use ahplot qw (:ALL) ;
use File::Copy;
use List::Util qw( min max ); # Finding minimum and maximum of an array

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

my $ehkfile     =""; # Input file
my $outfile    =""; # Output file
my $offset     =""; # Offset X axis to 0?
my $maxpts     =""; # Maximum points per graph
my $gtifile    =""; # input GTI file or list of files
my $timerange   =""; # input GTI poingting extension
my $qdp        =""; # stay in QDP?

my $telescop   ="HITOMI";

my $tmp_ehkfile="";

my @gtifilelist = ();


my $ahploterror = 0;  # ahplot exit status

my @ra = ();
my @dec = ();
my @roll = ();
my $rapos_0 = 0;
my $decpos_0 = 0;
my $rollpos_0 = 0;
my @raypos = ();
my @decypos = ();
my @rollypos = ();
my @gtirownums = ();
my $numgtifiles = 0;
my @row_pointing_start = ();
my @row_pointing_stop = ();
my $device = "";
my $gti_plot_file = "";
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

  $ehkfile     = ahapp::query_parameter("ehkfile");
  $outfile    = ahapp::query_parameter("outfile");
  $offset     = ahapp::query_parameter("offset");
  $gtifile    = ahapp::query_parameter("gtifile");
  $timerange  = ahapp::query_parameter("timerange");
  $qdp        = ahapp::query_parameter("qdp");

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;

  # Input file checking
  if (isRequiredFileNotFound($ehkfile)) { return 1; }
  if (removeOutputFileIfClobbering($outfile,$ahapp::clobber) ) { return 1; }


  # Read timerange (if provided) and obtain the row
  # numbers in RA,DEC,ROLL which correspnd to that 
  # time range
  if (uc $timerange ne "NONE") {
    # Determine the row ranges to read from the input EHK file either
    # from a user specified time range or a 
    my $ehkfilename = $ehkfile . "[EHK]"; 
    my ($start_time,$stop_time) = ahplot::parse_timerange($timerange);
    my ($rpstart,$rpstop) = ahplot::timerange_rownums($ehkfilename,$start_time,$stop_time);
    @row_pointing_start = @$rpstart;
    @row_pointing_stop = @$rpstop; 


    # Read only rows when the telscope is in pointing mode  
    for (my $ii=0; $ii<scalar(@row_pointing_start);++$ii) {
      push(@ra,ahgen::read_column($ehkfile,"EHK","RA",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
      push(@dec,ahgen::read_column($ehkfile,"EHK","DEC",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
      push(@roll,ahgen::read_column($ehkfile,"EHK","ROLL",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
    }
  } else { # Parameter timerange=NONE
    # Read all rows in EHK file
    @ra = ahgen::read_column($ehkfile,"EHK","RA");
    @dec = ahgen::read_column($ehkfile,"EHK","DEC");
    @roll = ahgen::read_column($ehkfile,"EHK","ROLL");
  }
 
  my @ra_pars = ahplot::calculate_gti_y_position_parameters(@ra);
  my @dec_pars = ahplot::calculate_gti_y_position_parameters(@dec);
  my @roll_pars = ahplot::calculate_gti_y_position_parameters(@roll);
  
  # first y postion of GTI intervals, subsequent gti intervals are stacked vertically  
  my @ypos_0 = ();
  $ypos_0[0] = $ra_pars[1];
  $ypos_0[1] = $dec_pars[1];
  $ypos_0[2] = $roll_pars[1];
 
  # difference in spacing between subsequent gti intervals
  my @deltay = ();
  $deltay[0] = $ra_pars[2];
  $deltay[1] = $dec_pars[2];
  $deltay[2] = $roll_pars[2];

  # Array containing plotting boudariy for each panel
  my @ymin = ();
  my @ymax = ();
 
  # Count the number of gtifiles
  # Parse list of input GTI files
  my @gtifilelist = ();
 
  if (uc $gtifile ne "NONE") {
    @gtifilelist = ahgen::get_file_list($gtifile);
  }  

  $numgtifiles = scalar(@gtifilelist);

  # Plotting boundaries for each panel
  $ymin[0] = $ra_pars[0];
  $ymin[1] = $dec_pars[0];
  $ymin[2] = $roll_pars[0];
  $ymax[0] = $ypos_0[0] + (($numgtifiles + 1) * $deltay[0]);
  $ymax[1] = $ypos_0[1] + (($numgtifiles + 1) * $deltay[1]);
  $ymax[2] = $ypos_0[2] + (($numgtifiles + 1) * $deltay[2]);

  # Getting tstart and tstop from EHK file these are used 
  # when offset mode = yes 
  my $tstart_ehk = ahgen::get_keyword($ehkfile,"EHK","TSTART");
  my $tstop_ehk = ahgen::get_keyword($ehkfile,"EHK","TSTOP");

  ahplot::write_gti_pco($outfile,$qdp,$offset,$tstart_ehk,$tstop_ehk,\@ymin,\@ymax);
 
  # Prepare additional input for make_gti_plot
  my @ycols = ('RA','DEC','ROLL');
  
  $gti_plot_file = "gti_plot.fits";

  if ($numgtifiles > 0) {
    ahplot::prepare_gti_plot_data($gtifile,"EHK","TIME","DX",$gti_plot_file,\@ypos_0,\@deltay,\@ycols);
  }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;
  my $filename = "";

  # Call parse file name to handle case if extension is given
  my @filename_parsed = ahgen::parse_file_name($ehkfile);
  if ($filename_parsed[1] eq "") { 
    $filename = $ehkfile . "[EHK]";
  } else {
    $filename = $ehkfile;
  } 
  
  my $tmp_ehkfile = $ehkfile . ".tmp";
  ahapp::add_temp_file($tmp_ehkfile);

  $status = ahgen::copy_fits_file($filename . "[col TIME]",$tmp_ehkfile,"copyall=no");

  # Set extension name to ehk in .tmp file
  $status = ahgen::set_keyword($tmp_ehkfile,
                               "1",
                               "EXTNAME",
                               "EHK",
                               "name of this binary table extension");  

 if ( $status ) {
    ahlog::ah_err "Creating temporary ehk file failed.";
    return $status;
  }

  # Add columns to fits file Neccesary for plotting GTI intervals
  $status = ahgen::add_column($tmp_ehkfile,"EHK","DX","0","1D");
  if ( $status ) {
    ahlog::ah_err "adding column failed.";
    return $status;
  }

  $status = ahgen::run_ftool("ftpaste",
                             "infile=$tmp_ehkfile\[EHK\]",
                             "pastefile=$filename\[col RA;DEC;ROLL\]",
                             "outfile=$tmp_ehkfile",
                             "clobber=yes");

 if ( $status ) {
    ahlog::ah_err "adding column to tmp EHK failed.";
    return $status;
  }

  # Add null row to fits file to separate data and time intervals plot
  ahplot::write_null_row("$tmp_ehkfile","TIME");
  
  my $plotfile = "plotfile.tmp";

  # skip if numgtifiles =0 set plotfile = tmp_ehk
  if ($numgtifiles > 0) {
    $status = ahgen::run_ftool("ftmerge",
                               "infile=$tmp_ehkfile,$gti_plot_file",
                               "outfile=$plotfile",
                               "clobber=YES");
  } else {
    $plotfile = $tmp_ehkfile;
  }

  if ( $status ) {
    ahlog::ah_err "failed to create plotfile.tmp";
    return $status;
  }

  # Get number of rows in plotting file to pass to fplot
  $maxpts = ahgen::get_keyword($plotfile,"EHK","NAXIS2");

  # Run FPLOT to create the postscript file
  if (uc $qdp eq "YES") {
    $device = "/XW"
  } else {
    $device = "$outfile/cps"
  }  
  $status = ahgen::run_ftool("fplot",
                             "infile=$plotfile",
                             "maxpts=$maxpts",
                             "offset=$offset",
                             "xparm=TIME[DX]",
                             "yparm=RA DEC ROLL",
                             "rows=-",
                             "device=$device",
                             "pltcmd=\@ahplot.pco",
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

