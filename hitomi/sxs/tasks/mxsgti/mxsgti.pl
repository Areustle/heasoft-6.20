#!/usr/bin/perl
#
# File name: mxsgti.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/04/18 20:54:40 $
# Version: 0
#
# Run mxstime and create GTI for MXS fine on & off and MXS coarse
#
# Tool Dependencies:
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#   gen/lib/perl/ahfilterlib
#
# Modification History:
#

# Set up
use strict;
use warnings;

use ahlog ;
use ahapp ;
use ahgen qw (:ALL) ;
use ahfilterlib qw (:ALL);

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

my %mxstime_pars;       # parameter hash for mxstime
my %gtiinvert_pars;     # parameter hash for gtiinvert 

my $infilehk="";        # input sxs hk file
my $outfilehk="";       # output sxs hk file
my $finegti="";         # output MXS fine GTI file (on)
my $coarsegti="";       # output MXS coarse GTI file (on)

my $mxsgtierror = 0;  # sxspipeline exit status

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$mxsgtierror = get_parameters () ;
unless ( $mxsgtierror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ; 
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($mxsgtierror);
}

$mxsgtierror = initialize () ;
unless ( $mxsgtierror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ; 
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($mxsgtierror);
}

$mxsgtierror = do_work () ;
unless ( $mxsgtierror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($mxsgtierror);
}

# We're done.
ahapp::end_processing($mxsgtierror);


#########################
# Subroutines
#########################

sub get_parameters {

  $infilehk   = ahapp::query_parameter("infilehk");
  $outfilehk  = ahapp::query_parameter("outfilehk");
  $finegti    = ahapp::query_parameter("finegti");
  $coarsegti  = ahapp::query_parameter("coarsegti");

  # mxstime parameters
  $mxstime_pars{infile}          = "";
  $mxstime_pars{outfile}         = "";
  $mxstime_pars{outgti}          = "mxstime.#.gti";
  $mxstime_pars{timfile}         = ahapp::query_parameter("timfile");
  $mxstime_pars{delayfile}       = ahapp::query_parameter("delayfile");
  $mxstime_pars{leapsecfile}     = ahapp::query_parameter("leapsecfile");
  $mxstime_pars{stimecol}        = ahapp::query_parameter("stimecol");
  $mxstime_pars{tioncol}         = ahapp::query_parameter("tioncol");
  $mxstime_pars{tioffcol}        = ahapp::query_parameter("tioffcol");
  $mxstime_pars{plslencol}       = ahapp::query_parameter("plslencol");
  $mxstime_pars{plsspccol}       = ahapp::query_parameter("plsspccol");
  $mxstime_pars{timeoncol}       = ahapp::query_parameter("timeoncol");
  $mxstime_pars{timeoffcol}      = ahapp::query_parameter("timeoffcol");
  $mxstime_pars{calctime}        = ahapp::query_parameter("calctime");
  $mxstime_pars{calcgti}         = ahapp::query_parameter("calcgti");
  $mxstime_pars{afterglow}       = ahapp::query_parameter("afterglow");
  $mxstime_pars{dtdecay}         = ahapp::query_parameter("dtdecay");
  $mxstime_pars{interp}          = ahapp::query_parameter("interp");

  # gtiinvert parameters
  $gtiinvert_pars{infile}        = "";
  $gtiinvert_pars{outfile}       = "";
  $gtiinvert_pars{outext}        = "";
  $gtiinvert_pars{margingti}     = ahapp::query_parameter("margingti");
  $gtiinvert_pars{tstart}        = ahapp::query_parameter("tstart");
  $gtiinvert_pars{tstop}         = ahapp::query_parameter("tstop");
  $gtiinvert_pars{dt}            = ahapp::query_parameter("dt");

  return 0;

  

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;
  
  # Input file checking
  if (isRequiredFileNotFound($infilehk)) { return 1; }

  # Check if output files exist
  if (removeOutputFileIfClobbering($outfilehk,$ahapp::clobber)) { return 1; }
  if (removeOutputFileIfClobbering($finegti,$ahapp::clobber)) { return 1; }
  if (removeOutputFileIfClobbering($coarsegti,$ahapp::clobber)) { return 1; }

  # Before processing, copy input files to output
  # copyFITSFile returns 0 if infile is set to none
  if (copyFITSFile($infilehk,$outfilehk)) { return 1; }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;

  my $ledgti="";   # temporary MXS led off GTI file
  my $outext="";   # output extension for MXS led off GTI file
  my %mgtime_pars; # unused required parameter for merge_gti

  # Set up temporary files
  my $tmpcoarse = 'mxstime.cs.gti';
  my $tmpfine   = 'mxstime.fn.gti'; 
  my $gti13cr   = 'mxstime.13cr.gti';
  my $gti24cr   = 'mxstime.24cr.gti';
  my $gti13croff= 'mxstime.13croff.gti';
  my $gti24croff= 'mxstime.24croff.gti';
  my $gti13on   = 'mxstime.13on.gti';
  my $gti24on   = 'mxstime.24on.gti';
  my $gti13off  = 'mxstime.13off.gti';
  my $gti24off  = 'mxstime.24off.gti';

  ahapp::add_temp_file($tmpfine);
  ahapp::add_temp_file($tmpcoarse);
  ahapp::add_temp_file($gti13cr);
  ahapp::add_temp_file($gti24cr);
  ahapp::add_temp_file($gti13croff);
  ahapp::add_temp_file($gti24croff);
  ahapp::add_temp_file($gti13on);
  ahapp::add_temp_file($gti24on);
  ahapp::add_temp_file($gti13off);
  ahapp::add_temp_file($gti24off);

  # Run tool mxstime
  $status = run_mxstime ($outfilehk,\%mxstime_pars) ;
  if ( $status ) { ahlog::ah_err "mxstime failed"; return $status; }

  # GTI invert to create the GTI Fine off files
  for my $led (1..4) {
    # Calculate the output extension name
    $outext = "GTIMXSFNOFF$led";
    $ledgti = "mxsoff.led$led.out";
    ahapp::add_temp_file($ledgti);

    # Run gtiinvert
    $status = run_gtiinvert ( "$tmpfine+$led", $ledgti, $outext, \%gtiinvert_pars) ;
    if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }

    # Run ftcopy/ftappend
    $status = ahgen::copy_hdu($ledgti,$outext,$tmpfine);
    if ( $status ) { ahlog::ah_err "copy_hdu failed"; return $status; }
  }

  # GTI invert to create the GTI Coarse off files
  for my $led (1..4) {
    # Calculate the output extension name
    $outext = "GTIMXSCSOFF$led";
    $ledgti = "mxscroff.led$led.out";
    ahapp::add_temp_file($ledgti);

    # Run gtiinvert
    $status = run_gtiinvert ( "$tmpcoarse+$led", $ledgti, $outext, \%gtiinvert_pars) ;
    if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }

    # Run ftcopy/ftappend
    $status = ahgen::copy_hdu($ledgti,$outext,$tmpcoarse);
    if ( $status ) { ahlog::ah_err "copy_hdu failed"; return $status; }
  }

  # Merge extensions (1 or 3) for coarse, fine GTIs
  # Coarse:
  $status = ahfilterlib::merge_gti([$tmpcoarse . "[GTIMXSCSON1]" , $tmpcoarse . "[GTIMXSCSON3]" ],
                                   $gti13cr, "OR","GTIMXSCSON13" ,\%mgtime_pars);
  if ( $status ) { ahlog::ah_err "merge_gti failed"; return $status; }

  # Run gtiinvert
  $status = run_gtiinvert ( $gti13cr . "[GTIMXSCSON13]" , $gti13croff , "GTIMXSCSOFF13", \%gtiinvert_pars) ;
  if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }

  # Fine:
  $status = ahfilterlib::merge_gti([$tmpfine . "[GTIMXSFNON1]" , $tmpfine . "[GTIMXSFNON3]" ],
                                   $gti13on, "OR","GTIMXSFNON13" ,\%mgtime_pars);
  if ( $status ) { ahlog::ah_err "merge_gti failed"; return $status; }

  # Run gtiinvert
  $status = run_gtiinvert ( $gti13on . "[GTIMXSFNON13]" , $gti13off , "GTIMXSFNOFF13", \%gtiinvert_pars) ;
  if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }

  # Need to write the TSTART/TSTOP keywords to the merged file
  my @tstart = ();
  my @tstop = ();
  my $naxis2 = 0;

  # read the NAXIS2 keyword for fine GTI
  $naxis2 = ahgen::get_keyword($gti13on,"GTIMXSFNON13","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "NAXIS2 keyword not defined in $gti13on"; return ahgen::get_error_flag;
  }
  # write the TSTART/TSTOP keywords if there is at least one row
  # If zero rows, set TSTART/TSTOP equal to zero
  if ($naxis2) {
    # Read the first row START column value 
    @tstart = ahgen::read_column($gti13on,"GTIMXSFNON13","START",1);
    if ( 1 != @tstart ) { ahlog::ah_err "Could not calculate TSTART value from START column"; return 1; }
    # Read the last row STOP column value 
    @tstop = ahgen::read_column($gti13on,"GTIMXSFNON13","STOP",$naxis2);
    if ( ahgen::get_error_flag ) { ahlog::ah_err "failed to read STOP column"; return $status; }
    if ( 1 != @tstop ) { ahlog::ah_err "Could not calculate TSTOP value from START column"; return 1; }
    # set the keywords
    $status = ahgen::set_keyword($gti13on,"GTIMXSFNON13","TSTART",$tstart[0] . " / Start time");
    if ( $status ) { ahlog::ah_err "fthedit TSTART failed for file $gti13on"; return $status; }
    $status = ahgen::set_keyword($gti13on,"GTIMXSFNON13","TSTOP",$tstop[0] . " / Stop time");
    if ( $status ) { ahlog::ah_err "fthedit TSTOP failed for file $gti13on"; return $status; }
  } else {
    $status = ahgen::set_keyword($gti13on,"GTIMXSFNON13","TSTART",0.0 . " / Start time");
    if ( $status ) { ahlog::ah_err "fthedit TSTART failed for file $gti13on"; return $status; }
    $status = ahgen::set_keyword($gti13on,"GTIMXSFNON13","TSTOP",0.0 . " / Stop time");
    if ( $status ) { ahlog::ah_err "fthedit TSTOP failed for file $gti13on"; return $status; }
  }

  # read the NAXIS2 keyword for coarse GTI
  $naxis2 = ahgen::get_keyword($gti13cr,"GTIMXSCSON13","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "NAXIS2 keyword not defined in $gti13cr"; return ahgen::get_error_flag;
  }
  # write the TSTART/TSTOP keywords if there is at least one row
  # If zero rows, set TSTART/TSTOP equal to zero
  if ($naxis2) {
    # Read the first row START column value 
    @tstart = ahgen::read_column($gti13cr,"GTIMXSCSON13","START",1);
    if ( 1 != @tstart ) { ahlog::ah_err "Could not calculate TSTART value from START column"; return 1; }
    # Read the last row STOP column value 
    @tstop = ahgen::read_column($gti13cr,"GTIMXSCSON13","STOP",$naxis2);
    if ( ahgen::get_error_flag ) { ahlog::ah_err "failed to read STOP column"; return $status; }
    if ( 1 != @tstop ) { ahlog::ah_err "Could not calculate TSTOP value from START column"; return 1; }
    # set the keywords
    $status = ahgen::set_keyword($gti13cr,"GTIMXSCSON13","TSTART",$tstart[0] . " / Start time");
    if ( $status ) { ahlog::ah_err "fthedit TSTART failed for file $gti13cr"; return $status; }
    $status = ahgen::set_keyword($gti13cr,"GTIMXSCSON13","TSTOP",$tstop[0] . " / Stop time");
    if ( $status ) { ahlog::ah_err "fthedit TSTOP failed for file $gti13cr"; return $status; }
  } else {
    $status = ahgen::set_keyword($gti13cr,"GTIMXSCSON13","TSTART",0.0 . " / Start time");
    if ( $status ) { ahlog::ah_err "fthedit TSTART failed for file $gti13cr"; return $status; }
    $status = ahgen::set_keyword($gti13cr,"GTIMXSCSON13","TSTOP",0.0 . " / Stop time");
    if ( $status ) { ahlog::ah_err "fthedit TSTOP failed for file $gti13cr"; return $status; }
  }

  # Merge extensions (2 or 4) for coarse, fine (on) and fine (off) GTIs
  # Coarse:
  $status = ahfilterlib::merge_gti([$tmpcoarse . "[GTIMXSCSON2]" , $tmpcoarse . "[GTIMXSCSON4]" ],
                                   $gti24cr, "OR","GTIMXSCSON24" ,\%mgtime_pars);
  if ( $status ) { ahlog::ah_err "merge_gti failed"; return $status; }

  # Run gtiinvert
  $status = run_gtiinvert ( $gti24cr . "[GTIMXSCSON24]" , $gti24croff , "GTIMXSCSOFF24", \%gtiinvert_pars) ;
  if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }

  # Fine:
  $status = ahfilterlib::merge_gti([$tmpfine . "[GTIMXSFNON2]" , $tmpfine . "[GTIMXSFNON4]" ],
                                   $gti24on, "OR","GTIMXSFNON24" ,\%mgtime_pars);
  if ( $status ) { ahlog::ah_err "merge_gti failed"; return $status; }
  # Run gtiinvert
  $status = run_gtiinvert ( $gti24on . "[GTIMXSFNON24]" , $gti24off , "GTIMXSFNOFF24", \%gtiinvert_pars) ;
  if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }

  # read the NAXIS2 keyword 
  $naxis2 = ahgen::get_keyword($gti24on,"GTIMXSFNON24","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "NAXIS2 keyword not defined in $gti24on"; return ahgen::get_error_flag;
  }
  # write the TSTART/TSTOP keywords if there is at least one row
  # If zero rows, set TSTART/TSTOP equal to zero
  if ($naxis2) {
    # Read the first row START column value 
    @tstart = ahgen::read_column($gti24on,"GTIMXSFNON24","START",1);
    if ( 1 != @tstart ) { ahlog::ah_err "Could not calculate TSTART value from START column"; return 1; }
    # Read the last row STOP column value 
    @tstop = ahgen::read_column($gti24on,"GTIMXSFNON24","STOP",$naxis2);
    if ( ahgen::get_error_flag ) { ahlog::ah_err "failed to read STOP column"; return $status; }
    if ( 1 != @tstop ) { ahlog::ah_err "Could not calculate TSTOP value from START column"; return 1; }
    # set the keywords
    $status = ahgen::set_keyword($gti24on,"GTIMXSFNON24","TSTART",$tstart[0] . " / Start time");
    if ( $status ) { ahlog::ah_err "fthedit TSTART failed for file $gti24on"; return $status; }
    $status = ahgen::set_keyword($gti24on,"GTIMXSFNON24","TSTOP",$tstop[0] . " / Stop time");
    if ( $status ) { ahlog::ah_err "fthedit TSTOP failed for file $gti24on"; return $status; }
  } else {
    $status = ahgen::set_keyword($gti24on,"GTIMXSFNON24","TSTART",0.0 . " / Start time");
    if ( $status ) { ahlog::ah_err "fthedit TSTART failed for file $gti24on"; return $status; }
    $status = ahgen::set_keyword($gti24on,"GTIMXSFNON24","TSTOP",0.0 . " / Stop time");
    if ( $status ) { ahlog::ah_err "fthedit TSTOP failed for file $gti24on"; return $status; }
  }

  # read the NAXIS2 keyword for coarse GTI
  $naxis2 = ahgen::get_keyword($gti24cr,"GTIMXSCSON24","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "NAXIS2 keyword not defined in $gti24cr"; return ahgen::get_error_flag;
  }
  # write the TSTART/TSTOP keywords if there is at least one row
  # If zero rows, set TSTART/TSTOP equal to zero
  if ($naxis2) {
    # Read the first row START column value 
    @tstart = ahgen::read_column($gti24cr,"GTIMXSCSON24","START",1);
    if ( 1 != @tstart ) { ahlog::ah_err "Could not calculate TSTART value from START column"; return 1; }
    # Read the last row STOP column value 
    @tstop = ahgen::read_column($gti24cr,"GTIMXSCSON24","STOP",$naxis2);
    if ( ahgen::get_error_flag ) { ahlog::ah_err "failed to read STOP column"; return $status; }
    if ( 1 != @tstop ) { ahlog::ah_err "Could not calculate TSTOP value from START column"; return 1; }
    # set the keywords
    $status = ahgen::set_keyword($gti24cr,"GTIMXSCSON24","TSTART",$tstart[0] . " / Start time");
    if ( $status ) { ahlog::ah_err "fthedit TSTART failed for file $gti24cr"; return $status; }
    $status = ahgen::set_keyword($gti24cr,"GTIMXSCSON24","TSTOP",$tstop[0] . " / Stop time");
    if ( $status ) { ahlog::ah_err "fthedit TSTOP failed for file $gti24cr"; return $status; }
  } else {
    $status = ahgen::set_keyword($gti24cr,"GTIMXSCSON24","TSTART",0.0 . " / Start time");
    if ( $status ) { ahlog::ah_err "fthedit TSTART failed for file $gti24cr"; return $status; }
    $status = ahgen::set_keyword($gti24cr,"GTIMXSCSON24","TSTOP",0.0 . " / Stop time");
    if ( $status ) { ahlog::ah_err "fthedit TSTOP failed for file $gti24cr"; return $status; }
  }

  # Append merged extensions to output GTI file
  if (ahgen::copy_hdu($gti13cr,"GTIMXSCSON13",$tmpcoarse)) { return 1; }
  if (ahgen::copy_hdu($gti24cr,"GTIMXSCSON24",$tmpcoarse)) { return 1; }
  if (ahgen::copy_hdu($gti13croff,"GTIMXSCSOFF13",$tmpcoarse)) { return 1; }
  if (ahgen::copy_hdu($gti24croff,"GTIMXSCSOFF24",$tmpcoarse)) { return 1; }
  if (ahgen::copy_hdu($gti13on,"GTIMXSFNON13",$tmpfine)) { return 1; }
  if (ahgen::copy_hdu($gti24on,"GTIMXSFNON24",$tmpfine)) { return 1; }
  if (ahgen::copy_hdu($gti13off,"GTIMXSFNOFF13",$tmpfine)) { return 1; }
  if (ahgen::copy_hdu($gti24off,"GTIMXSFNOFF24",$tmpfine)) { return 1; }

  # Copy the temporary output to 
  if (ahgen::copyFITSFile($tmpcoarse,$coarsegti)) { return 1; }
  if (ahgen::copyFITSFile($tmpfine,$finegti)) { return 1; }

  # Copy keywords to each extension
  foreach my $ext (0..12) {
    if (ahfilterlib::copy_keywords($infilehk,"HK_SXS_FWE][col -TSTART;-TSTOP",$coarsegti,$ext,"gti","all")) { return 1; }
    if (set_keyword($coarsegti,$ext,"DETNAM","PIXEL")) { return 1; }
    ahapp::write_parameters ($coarsegti,$ext) ;                 
    if (ahfilterlib::copy_keywords($infilehk,"HK_SXS_FWE][col -TSTART;-TSTOP",$finegti,$ext,"gti","all")) { return 1; }
    if (set_keyword($finegti,$ext,"DETNAM","PIXEL")) { return 1; }
    ahapp::write_parameters ($finegti,$ext) ;                 
  }

  return 0;

} 

# ------------------------------------------------------------------------------

sub run_mxstime {

  my $infile=shift;
  my %mxstime_pars=%{shift()};

  my $toolname="mxstime";
  my $tmpoutfile=formTemporaryFileName($infile,$toolname);
  my $outfile=$infile;

  my @params = (
    ['infile'       , $infile],
    ['outfile'      , $tmpoutfile],
  );
  my @ordered_pars = qw( outgti timfile delayfile leapsecfile stimecol tioncol 
                         tioffcol plslencol plsspccol timeoncol timeoffcol 
                         calctime calcgti afterglow dtdecay interp );
  foreach my $par (@ordered_pars) {
    if(defined $mxstime_pars{$par}) { push @params, [$par, $mxstime_pars{$par}]; }
  }

  if (isBadCALDBFileParameterValue($mxstime_pars{timfile},"timfile")) { return 1; }
  if (isBadCALDBFileParameterValue($mxstime_pars{delayfile},"delayfile")) { return 1; }
  if (isBadCALDBFileParameterValue($mxstime_pars{leapsecfile},"leapsecfile")) { return 1; }
  if (runTool($toolname,\@params)) { return 1; }
  if (copyOrMoveFileBasedOnCleanup($tmpoutfile, $outfile, $ahapp::cleanup)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_gtiinvert {

  my $infile=shift;
  my $outfile=shift;
  my $outgti=shift;
  my %gtiinvert_pars=%{shift()};

  my $toolname="gtiinvert";
  my $tmpoutfile=formTemporaryFileName($outfile,$toolname);

  my @params = (
    ['infile'       , $infile],
    ['outfile'      , $tmpoutfile],
    ['outext'       , $outgti],
  );
  my @ordered_pars = qw( margingti tstart tstop dt );
  foreach my $par (@ordered_pars) {
    if(defined $gtiinvert_pars{$par}) { push @params, [$par, $gtiinvert_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }
  if (copyOrMoveFileBasedOnCleanup($tmpoutfile, $outfile, $ahapp::cleanup)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------
