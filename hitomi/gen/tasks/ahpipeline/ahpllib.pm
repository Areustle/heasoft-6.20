#!/usr/bin/perl
#
# File name: ahpllib.pm
#  
# Description:
#
# This library contains several subroutines for running
# the tool pipeline calibration
#
# Author: A. J. Sargent NASA GSFC 
#
# $Date: 2016/11/28 14:55:18 $
#
# History:
#

package ahpllib ;

use strict ;
use warnings;

use ahlog ;
use ahapp ;
use ahgen qw (:ALL);

use File::Basename ;
use File::Find;
use File::Spec ;
use File::Spec::Functions qw ( rel2abs catfile ) ;
use File::Path;
use Cwd 'abs_path';

BEGIN {
      use Exporter () ;

      our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS) ;

      # set the version for version checking
      #$VERSION = 1.00 ;

      # if using RCS/CVS, this may be preferred
      $VERSION = sprintf "%d.%03d", q$Revision: 1.32 $ =~ /(\d+)/g;

      @ISA = qw(Exporter);
     
      # Functions exported by default.
      @EXPORT = qw();

      %EXPORT_TAGS = (ALL => \@EXPORT_OK ); # eg: TAG => [ qw!name1 name2! ],

      # your exported package globals go here,
      # as well as any optionally exported functions
      @EXPORT_OK = qw(&CheckInputDirectory
                      &CheckOutputDirectory
                      &FindInputFiles
                      &CheckEntryExitStage
                      &run_ftselect 
                      &run_coordevt
                      &run_hxisgdsff
                      &run_hxisgdpha
                      &run_hxisgdexpand
                      &run_hxievtid
                      &run_cams2att
                      &run_sgdevtid
                      &run_sximodegti
                      &run_sxiphas
                      &run_searchflickpix
                      &run_sxiflagpix
                      &run_sxipi
                      &run_anticopi
                      &run_sxsflagpix
                      &run_sxssecid
                      &run_sxsgain
                      &run_sxspha2pi
                      &run_sxsperseus
                      &run_gtiinvert 
                      &create_gti
                      &screen_events
                      &has_good_time 
                      &make_region_file
                      &extract
                      &merge_slew_and_pointing 
                      &form_output_file_name
                      );

      # Import the ahlog library so we use those functions here.

      unshift @INC, catfile dirname (rel2abs (__FILE__)) ;

      require ahlog  ;
      ahlog->import ;
      require ahapp  ;
      ahapp->import ;
      require ahgen  ;
      ahgen->import ;

      # Set up some defaults.

      }

our @EXPORT_OK;

# ------------------------------------------------------------------------------

sub CheckInputDirectory ($$) {

  my ( $indir, $outdir ) = @_;

  # For the instrument pipelines, it is okay for the input
  # directory to be the same as the output directory
  # Put a warning
  if ( $outdir eq $indir ) {
    ahlog::ah_out "Output directory same as input directory and clobber is on";
    ahlog::ah_out "Input directory  : $indir";
    ahlog::ah_out "Output directory : $outdir";
    if ( $ahapp::clobber ) {
      for (my $i1 = 5; $i1 > 0; $i1--) {
        ahlog::ah_out"$i1 seconds before continuing\n";
        sleep 1;
      } 
    }
  }

  if ( -e$indir )
  {
    ahlog::ah_info "LOW", "Input Directory Found : $indir";
    return 0;
  } else {
    ahlog::ah_err "Input Directory NOT FOUND : $indir";
    return 1;
  }

  return 0;
}

# ------------------------------------------------------------------------------

sub CheckOutputDirectory ($) {

  my $outdir = shift;

  if ( -e$outdir ) {

    ahlog::ah_info "LOW", "Output Directory Found : $outdir" ;

    unless ($ahapp::clobber) {
      ahlog::ah_err "Output directory exists \& clobber=no : $outdir" ;
      return 1;
    }

  } else {

    ahlog::ah_info "LOW", "Output Directory Not Found  : $outdir" ;
    ahlog::ah_info "LOW", "Making New Output Directory : $outdir" ;

    eval { mkpath( $outdir ); };
    if ( $@ ) {
      ahlog::ah_err "Cannot make directory : $outdir" ;
      return 1;
    }
  }

  return 0;

}

# ------------------------------------------------------------------------------

{

  my ( @filelist, $fulldirx, $stem, $pattern);

  sub FindInputFiles {
    ( $fulldirx, $stem, $pattern) = @_;
    @filelist  = {};
    $#filelist = -1;
    &File::Find::find( \&Wanted , $fulldirx );
    return @filelist;
  }

  sub Wanted {
    #/^.*$stem.*$inst.*\z/s &&
    #! /^\..*/s &&  # ignore files starting with a dot         
    /^$stem$pattern/s &&
    -f $_ &&
    push @filelist, $File::Find::name;
  }

}

# ------------------------------------------------------------------------------

sub CheckEntryExitStage ($$) {

  my $entry_stage = shift;
  my $exit_stage = shift;

  # Check entry stage is in range 1 - 2

  unless ( $entry_stage =~ /^(1|2|3)$/ ) {
    ahlog::ah_err "Entry stage must be one of 1,2 or 3" ;
    ahlog::ah_err "Entry stage = $entry_stage" ;
    return 1;
  }

  # Check exit stage is in range 1 - 2

  unless ( $exit_stage =~ /^(1|2|3)$/ ) {
    ahlog::ah_err "Exit stage must be one of 1,2 or 3" ;
    ahlog::ah_err "Exit stage = $exit_stage" ;
    return 1;
  }

  # Check entry stage is before/same as exit stage

  if ( $entry_stage > $exit_stage ) {
    ahlog::ah_err "Entry stage after exit stage" ;
    ahlog::ah_err "Entry stage = $entry_stage" ;
    ahlog::ah_err "Exit stage  = $exit_stage" ;
    return 1;
  }

  return 0;
}

# ------------------------------------------------------------------------------

sub merge_slew_and_pointing ($$$$$) {

  my $slew = shift;
  my $pointing = shift;
  my $all = shift;
  my $ext = shift;
  my $timecol = shift;
  my $merge = "merge.evt";

  my $status = 0;

  if ( ! -e $slew ) {
    $status = ahgen::run_ftool("ftcopy",$pointing."[$ext]",$all,"copyall=no","clobber=yes"); 
    if ($status) { ahlog::ah_err "ftcopy failed."; return 1; }

  } else {
    my $nrows_slew = ahgen::get_keyword($slew,$ext,"NAXIS2");
    unless ( defined $nrows_slew ) {
      ahlog::ah_out"In file '$slew', could not read keyword NAXIS2";
      return 1;
    }
    if( ! $nrows_slew ) {
      ahlog::ah_info "LOW", "No occurrences in slew file $slew. Using $pointing as $all.";
      $status = ahgen::run_ftool("ftcopy",$pointing."[$ext]",$all,"copyall=no","clobber=yes"); 
      if ($status) { ahlog::ah_err "ftcopy failed."; return 1; }
    } else {
      # Merge the slew and pointing files using ftmerge
      $status = ahgen::run_ftool("ftmerge","$pointing\[$ext] $slew\[$ext]",$merge,
                                 "copyall=no","skipbadfiles=yes","clobber=yes");
      if($status) { ahlog::ah_err "ftmerge failed."; unlink $merge; return $status; }

      # Sort slew and pointing by time
      $status = ahgen::run_ftool("ftsort",$merge."[$ext]",$all,$timecol); unlink $merge;
      if($status) { ahlog::ah_err "ftsort failed."; unlink $all; return $status; }
    }
  }

  # Update the OBS_MODE keyword to all
  ahgen::set_keyword($all,$ext,"OBS_MODE","ALL");

  return 0;


}

# ------------------------------------------------------------------------------

sub run_ftselect ($$$$) {

  my $toolname="ftselect";
  my $TmpInfile=shift;
  my $extension=shift;
  my $TmpOutfile=shift;
  my $expr=shift;
  my $status = 0;

  # ftselect parameters
  my @params = (
    ['infile'       , $TmpInfile],
    ['outfile'      , $TmpOutfile],
    ['expr'         , $expr],
    ['clobber'      , "yes"]
  );

  if(runTool($toolname,\@params)) { return 1; }

  # Check that not all rows were filtered
  my $naxis2 = ahgen::get_keyword($TmpOutfile,$extension,"NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $TmpOutfile, NAXIS2 keyword not defined.\n";
    return ahgen::get_error_flag;
  }
  if($naxis2 == 0) {
    # Not necessarily an error
    ahlog::ah_out "Filtered all rows in event file $TmpInfile";
    return 1;
  }

  return $status;

}
# ------------------------------------------------------------------------------

sub run_coordevt ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %coordevt_pars=%{shift()};

  my @params = ();
  my $toolname="coordevt";

  $coordevt_pars{infile} = $infile;
  $coordevt_pars{outfile} = $outfile;
  my @ordered_pars = qw( infile outfile teldeffile attfile dattfile orbfile startsys stopsys annaber 
                followsun orbaber attinterp dattinterp attdt dattdt chkattgap 
                chkdattgap attext attcol attform orbext orbcol orbform randomize
                seed randsys randscalesys infileext timecol inclfloatcol 
                inclfloatskycol floatcolsuffix startwithfloat blankcol btnull 
                itnull jtnull ktnull sbtnull uitnull ujtnull ra dec roll 
                clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $coordevt_pars{$par}) { push @params, [$par, $coordevt_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_hxisgdsff ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %hxisgdsff_pars=%{shift()};

  my @params = ();
  my $toolname="hxisgdsff";

  $hxisgdsff_pars{infile} = $infile;
  $hxisgdsff_pars{outfile} = $outfile;
  my @ordered_pars = qw( infile outfile remapfile clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $hxisgdsff_pars{$par}) { push @params, [$par, $hxisgdsff_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_hxisgdpha ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %hxisgdpha_pars=%{shift()};

  my @params = ();
  my $toolname="hxisgdpha";

  $hxisgdpha_pars{infile} = $infile;
  $hxisgdpha_pars{outfile} = $outfile;
  my @ordered_pars = qw( infile outfile gainfile badpixfile outnsubcol randomize seed datamode 
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $hxisgdpha_pars{$par}) { push @params, [$par, $hxisgdpha_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

###################
# HXI Pipeline
###################

sub run_hxievtid ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %hxievtid_pars=%{shift()};

  my @params = ();
  my $toolname="hxievtid";

  $hxievtid_pars{infile} = $infile;
  $hxievtid_pars{outfile} = $outfile;
  my @ordered_pars = qw(infile outfile remapfile fluorefile badpixfile enecutfile occurrenceid 
                        rejectbgo outcalfile skipreco clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $hxievtid_pars{$par}) { push @params, [$par, $hxievtid_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_cams2att ($$$$$) {

  my $infile1=shift;
  my $infile2=shift;
  my $outfile=shift; # delta attitude file
  my $offsetfile=shift;
  my %cams2att_pars=%{shift()};

  my @params = ();
  my $toolname="cams2att";

  if ( uc $infile1 eq "NONE" and uc $infile2 eq "NONE" ) { 
    ahlog::ah_err "Both input CAMS files are NONE for cams2att";
    return 1;
  }

  $cams2att_pars{infile1} = $infile1;
  $cams2att_pars{infile2} = $infile2;
  $cams2att_pars{outfile} = $outfile;
  $cams2att_pars{offsetfile} = $offsetfile;
  my @ordered_pars = qw( infile1 infile2 outfile instrume cams1teldef cams2teldef hxiteldef camstempxy
                         startstep stopstep inext outext flipsign tempcorfile1
                         tempcorfile2 prefiltfile1 prefiltfile2 offsetfile
                         filtoffset prefiltexpr filtexpr gtiexpr0 gtiexpr1
                         gtiexpr2 gtifile startsys deltaxcol deltaycol coscol
                         sincol clobber chatter logfile debug history mode cleanup );
  foreach my $par (@ordered_pars) {
    if(defined $cams2att_pars{$par}) { push @params, [$par, $cams2att_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

###################
# SGD Pipeline
###################

sub run_sgdevtid ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %sgdevtid_pars=%{shift()};

  my @params = ();
  my $toolname="sgdevtid";

  $sgdevtid_pars{infile} = $infile;
  $sgdevtid_pars{outfile} = $outfile;
  my @ordered_pars = qw( infile outfile remapfile fluorefile badpixfile probseqfile probfovfile
                         occurrenceid rejectbgo skipreco outtracefile numsignal 
                         d10 d1a1a d1a1b d1a2 d1a3 a b probaccept2 probaccept3 
                         probaccept4 distz paraoffset0 paraoffset1 paraoffset2 
                         weight0 weight1 weight2 weight3 delgmethod seed 
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sgdevtid_pars{$par}) { push @params, [$par, $sgdevtid_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

###################
# SXI Pipeline
###################

# ------------------------------------------------------------------------------

sub run_sximodegti ($$$$) {

  my $infile=shift;
  my $outfile=shift;
  my $mergefile=shift;
  my %sximodegti_pars=%{shift()};

  my @params = ();
  my $toolname="sximodegti";

  # force the required files
  $sximodegti_pars{infile} = $infile;
  $sximodegti_pars{outfile} = $outfile;
  $sximodegti_pars{mergefile} = $mergefile;

  # store the parameters in a pre-sorted list
  my @ordered_pars = qw( infile outfile mergefile tstart tstop 
                         clobber chatter logfile debug history mode cleanup );
  foreach my $par (@ordered_pars) {
    if(defined $sximodegti_pars{$par}) { push @params, [$par, $sximodegti_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_sxiphas ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %sxiphas_pars=%{shift()};

  my @params = ();
  my $toolname="sxiphas";

  # force the required files
  $sxiphas_pars{infile} = $infile;
  $sxiphas_pars{outfile} = $outfile;

  # store the parameters in a pre-sorted list
  my @ordered_pars = qw( infile outfile colbound clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxiphas_pars{$par}) { push @params, [$par, $sxiphas_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_searchflickpix ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %searchflickpix_pars=%{shift()};

  my @params = ();
  my $toolname="searchflickpix";

  # Force the required files
  $searchflickpix_pars{infile} = $infile;
  $searchflickpix_pars{outfile} = $outfile;

  # Store the parameters in a pre-sorted list
  my @ordered_pars = qw( infile outfile timecol chipcol xcol ycol chancol gradecol grade 
                         n_division cleanimg cellsize impfac logprob1 logprob2 iterate 
                         flagedge bthresh duration sigma firstchip lastchip xmin xmax 
                         ymin ymax chanmin chanmax clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $searchflickpix_pars{$par}) { push @params, [$par, $searchflickpix_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_sxiflagpix ($$$$$) {

  my $infile=shift;
  my $outfile=shift;
  my $hotpixfile=shift;
  my $flickpixfile=shift;
  my %sxiflagpix_pars=%{shift()};

  my @params = ();
  my $toolname="sxiflagpix";

  # Force the required files
  $sxiflagpix_pars{infile} = $infile;
  $sxiflagpix_pars{outfile} = $outfile;
  $sxiflagpix_pars{hotpixfile} = $hotpixfile;
  $sxiflagpix_pars{flickpixfile} = $flickpixfile;

  # Store the parameters in a pre-sorted list
  my @ordered_pars = qw( infile outfile hotpixfile flickpixfile outbadpix outbadimg 
                         badpixfile maskfile npixnbr nboundnbr citrailnbr ciprenbr 
                         gtifile echoflag echomap echonbr echomin echospth echofrac 
                         bad_status copyphas resetflags 
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxiflagpix_pars{$par}) { push @params, [$par, $sxiflagpix_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_sxipi ($$$$) {

  my $infile=shift;
  my $outfile=shift;
  my $hkfile=shift;
  my %sxipi_pars=%{shift()};

  my @params = ();
  my $toolname="sxipi";

  # Force the required files
  $sxipi_pars{infile}  = $infile;
  $sxipi_pars{outfile} = $outfile;
  $sxipi_pars{hkfile}  = $hkfile;

  # Store the parameters in a pre-sorted list
  my @ordered_pars = qw( infile outfile hkfile hkext hkcolstem hkvideoid vtevnoddfile 
                         chtrailfile ctifile spthfile gainfile patternfile startcol 
                         evnoddcor chtrailcor cticor gaincor ctigrade copygrade phcut
                         badpixopt spthiter spthcaldb spthoffset spthslope evtthre 
                         negthre deltatime debugcol randomize seed 
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxipi_pars{$par}) { push @params, [$par, $sxipi_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

###################
# SXS Pipeline
###################

sub run_mxsgti ($$$$$) {

  my $infile = shift;
  my $outfile = shift;
  my $mxfngti = shift;
  my $mxcsgti = shift;
  my %mxsgti_pars = %{shift()};

  my @params = ();
  my $toolname="mxsgti";

  $mxsgti_pars{infilehk} = $infile;
  $mxsgti_pars{outfilehk} = $outfile; 
  $mxsgti_pars{finegti} = $mxfngti;
  $mxsgti_pars{coarsegti} = $mxcsgti;

  my @ordered_pars = qw( infilehk outfilehk finegti coarsegti
                         timfile delayfile stimecol tioncol tioffcol plslencol 
                         plsspccol timeoncol timeoffcol calctime calcgti 
                         afterglow dtdecay interp margingti tstart tstop dt 
                         clobber chatter logfile debug history mode cleanup );
  foreach my $par (@ordered_pars) {
    if(defined $mxsgti_pars{$par}) { push @params, [$par, $mxsgti_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;

}

sub run_anticopi ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %sxsanticopi_pars=%{shift()};

  my @params = ();
  my $toolname="sxsanticopi";

  $sxsanticopi_pars{infile} = $infile;
  $sxsanticopi_pars{outfile} = $outfile;
  my @ordered_pars = qw( infile outfile gainantfile acphaoffset randomize seed clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxsanticopi_pars{$par}) { push @params, [$par, $sxsanticopi_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_sxsflagpix ($$$$) {

  my $infile=shift;
  my $outfile=shift;
  my $inantfile=shift;
  my %sxsflagpix_pars=%{shift()};

  my @params = ();
  my $toolname="sxsflagpix";

  $sxsflagpix_pars{infile} = $infile;
  $sxsflagpix_pars{outfile} = $outfile;
  $sxsflagpix_pars{inantfile} = $inantfile;
  my @ordered_pars = qw( infile inantfile outfile antpsp antshift gtifile calcant antdtpre 
                         antdtfol antswitch antphathr antdurthr calcctrec ctrecdt calcprox 
                         proxdt calcctel pixdeffile cteldt ctelnear calcctel2 cteldt2 ctelnear2 
                         pxpithr usepxpithr calcmxs ckrisetime inmxsgti mxsdt kalow kahigh kbeta 
                         dtflag resetflags clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxsflagpix_pars{$par}) { push @params, [$par, $sxsflagpix_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_sxssecid ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %sxssecid_pars=%{shift()};

  my @params = ();
  my $toolname="sxssecid";

  $sxssecid_pars{infile} = $infile;
  $sxssecid_pars{outfile} = $outfile;
  my @ordered_pars = qw( infile outfile itypecol dtprimary dtlowmid dtmidhigh tol pxpithr usepxpithr
                         ckctrec ckctel ckant ckrisetime regrade clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxssecid_pars{$par}) { push @params, [$par, $sxssecid_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_sxsseccor ($$$) {

  my $infile=shift;
  my $outfile=shift;
  my %sxsseccor_pars=%{shift()};

  my @params = ();
  my $toolname="sxsseccor";

  $sxsseccor_pars{infile} = $infile;
  $sxsseccor_pars{outfile} = $outfile;
  my @ordered_pars = qw( infile outfile pulsefile itypecol phaout
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxsseccor_pars{$par}) { push @params, [$par, $sxsseccor_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------
sub run_sxsgain ($$$) {

  my $infile=shift;
  my $driftfile=shift;
  my %sxsgain_pars=%{shift()};

  my @params = ();
  my $toolname="sxsgain";

  $sxsgain_pars{infile} = $infile;
  $sxsgain_pars{outfile} = $driftfile;
  my @ordered_pars = qw( infile outfile gainfile tempidx gaincoeff linefitfile linetocorrect itypecol ntemp calmethod
                         numevent minevent gtifile gapdt grpoverlap startenergy stopenergy extraspread pxphaoffset
                         broadening gridprofile fitwidth background spangti usemp ckrisetime
                         calcerr writeerrfunc ckant ckctrec ckctel extrap avgwinrad minwidth0 maxitcycle
                         r2tol searchstepshift maxdshift bisectolshift searchstepwidth maxdwidth
                         bisectolwidth minwidth nerrshift nerrwidth shifterrfac widtherrfac 
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxsgain_pars{$par}) { push @params, [$par, $sxsgain_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_sxspha2pi ($$$$) {

  my $infile=shift;
  my $outfile=shift;
  my $driftfile=shift;
  my %sxspha2pi_pars=%{shift()};

  my @params = ();
  my $toolname="sxspha2pi";

  $sxspha2pi_pars{infile} = $infile;
  $sxspha2pi_pars{outfile} = $outfile;
  $sxspha2pi_pars{driftfile} = $driftfile;
  my @ordered_pars = qw( infile outfile driftfile calcupi calcpi gainfile 
                         scalefile tempidx pxphaoffset secphacol addepicol 
                         method scaleepi scalegrade itypecol extended binwidth 
                         offset tlmax gapdt ntemp writetemp extrap randomize seed 
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxspha2pi_pars{$par}) { push @params, [$par, $sxspha2pi_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub run_sxsperseus ($$$$) {

  my $infile=shift;
  my $outfile=shift;
  my $driftfile=shift;
  my %sxsperseus_pars=%{shift()};

  my @params = ();
  my $toolname="sxsperseus";

  $sxsperseus_pars{infile} = $infile;
  $sxsperseus_pars{outfile} = $outfile;
  $sxsperseus_pars{driftfile} = $driftfile;
  my @ordered_pars = qw( infile outfile dgfile offsetfile driftfile
                         outrange method extended binwidth offset tlmax
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $sxsperseus_pars{$par}) { push @params, [$par, $sxsperseus_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------


sub run_gtiinvert {

  my $infile=shift;
  my $outfile=shift;
  my $outgti=shift;
  my %gtiinvert_pars=%{shift()};

  my @params = ();
  my $toolname="gtiinvert";

  $gtiinvert_pars{infile} = $infile;
  $gtiinvert_pars{outfile} = $outfile;
  $gtiinvert_pars{outext} = $outgti;
  my @ordered_pars = qw( infile outfile outext margingti tstart tstop dt
                         clobber chatter logfile debug history mode );
  foreach my $par (@ordered_pars) {
    if(defined $gtiinvert_pars{$par}) { push @params, [$par, $gtiinvert_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub create_gti {

    my %passed = @_;
    my @params = ();
    my $toolname="ahgtigen";

    # Need to have one infile defined to create a GTI file
    unless ($passed{infile}) {
        ahlog::ah_err "Filter file is missing for GTI creation";
        return 1;
    }

    # Need to define an output file name
    unless ($passed{outfile}) {
      ahlog::ah_err"Need to pass an outfile for GTI creation.";
      return 1;
    }

    # Need instrument type and label if using select file
    if ($passed{selectfile}) {
      unless ($passed{instrume}) {
        ahlog::ah_err "Need instrument name for CALDB query";
        return 1;
      }
      unless ($passed{label}) {
        ahlog::ah_err "Need label for CALDB query";
        return 1;
      }
    } else {
      # No input selection file, check for expression
      unless ($passed{gtiexpr}) {
        ahlog::ah_err "Need an expression or selection file to create GTI";
        return 1;
      }
    }

    # Set the defaults.
    my %ahgtigen_pars = (infile       => "none",
                         outfile      => "gti.out", 
                         gtifile      => "none", 
                         gtiexpr      => "none",
                         mergegti     => "and",
                         cpkeyword    => "none",
                         upkeyword    => "no",
                         instrume     => "none",
                         leapsecfile  => "REFDATA",
                         label        => "none",
                         selectfile   => "none",
                         prefr        => 0.,
                         postfr       => 1.,
                         logfile      => "ahgtigen.log",
                         chatter      => 2);

    # Overwrite the defaults with the passed in parameters
    @ahgtigen_pars{keys %passed} = values %passed;

    my @ordered_pars = qw( infile outfile gtifile gtiexpr mergegti cpkeyword
                           upkeyword leapsecfile instrume selectfile label 
                           chatter clobber debug logfile history mode);

    foreach my $par (@ordered_pars) {
      if(defined $ahgtigen_pars{$par}) { push @params, [$par, $ahgtigen_pars{$par}]; }
    }

    # Run ahgtigen to create the new GTI
    if (runTool($toolname,\@params)) { unlink "in.gti"; return 1; }
    unlink "in.gti";

    return 0;
}

# ------------------------------------------------------------------------------

sub screen_events {

    my %passed = @_;
    my @params = ();
    my $toolname="ahscreen";

    # Need to have one infile defined to screen events
    unless ($passed{infile}) {
      ahlog::ah_err "Event file is missing for screening";
      return 1;
    }

    # Need to define an output file name
    unless ($passed{outfile}) {
      ahlog::ah_err "Need to pass an outfile for GTI creation.";
      return 1;
    }

    # Need instrument type and label if using select file
    if ($passed{selectfile}) {
      unless ($passed{label}) {
        ahlog::ah_err "Need label for CALDB query";
        return 1;
      }
    } else {
      # No input selection file, check for expression
      unless ($passed{expr}) {
        ahlog::ah_err "Need an expression or selection file to create GTI";
        return 1;
      }
    }

    # Set the defaults.
    my %ahscreen_pars = (infile       => "uf.fits",
                         outfile      => "cl.fits", 
                         gtifile      => "none", 
                         expr         => "none",
                         selectfile   => "none",
                         label        => "none",
                         leapsecfile  => "none",
                         mergegti     => 'AND',
                         cpkeyword    => "none",
                         upkeyword    => "no",
                         chatter      => 2, 
                         logfile      => "ahscreen.log" 
                       );

    # Overwrite the defaults with the passed in parameters
    @ahscreen_pars{keys %passed} = values %passed;

    my @ordered_pars = qw( infile outfile gtifile expr selectfile label mergegti 
                           cpkeyword upkeyword leapsecfile 
                           chatter clobber debug logfile history mode);

    foreach my $par (@ordered_pars) {
      if(defined $ahscreen_pars{$par}) { push @params, [$par, $ahscreen_pars{$par}]; }
    }

    # Run ahgtigen to create the new GTI
    if (runTool($toolname,\@params)) { return 1; }
    # ahscreen creates extra files text files
    unlink "files.in";
    unlink "in.gti";
  
    return 0;
}

# ------------------------------------------------------------------------------

sub has_good_time {

  my $infile = shift;
  my $ext    = shift;

  # Check that not all rows were filtered
  my $naxis2 = ahgen::get_keyword($infile,$ext,"NAXIS2");
  unless ( defined $naxis2 ) {
    ahlog::ah_err "In file $infile, NAXIS2 keyword not defined.\n";
    return 0;
  }
  if($naxis2 == 0) {
    ahlog::ah_info "LOW", "No GTI found in file $infile";
    return 0;
  }

  return 1;

  
}

# ------------------------------------------------------------------------------

sub make_region_file {

    my %passed = @_;
    my $status = 0;

    # Set the defaults.
    my %input = (input        => "3.5,3.5",
     outfile      => "NONE",
     telescop     => "HITOMI",
     instrume     => "SXS",
     ra           => 0.0,
     dec          => 0.0,
     roll         => 0.0,
     teldeffile   => "CALDB",
     startsys     => "LOWEST",
     stopsys      => "HIGHEST",
     clobber      => "yes",
     regfile      => "region.out",
    );

    # Overwrite the defaults.
    @input{keys %passed} = values %passed;

    $status = ahgen::run_ftool ('coordpnt',
                                "input=$input{input}",
                                "outfile=$input{outfile}",
                                "telescop=$input{telescop}",
                                "instrume=$input{instrume}",
                                "ra=$input{ra}",
                                "dec=$input{dec}",
                                "roll=$input{roll}",
                                "teldeffile=$input{teldeffile}",
                                "startsys=$input{startsys}",
                                "stopsys=$input{stopsys}",
                                "chatter=2",
                                );

     ###################
     # Check for errors
     ###################
     if ( $status ) {
       ahlog::ah_err "Error in coordpnt!" ;
       return $status;
     }

     # Read the coordinate parameters
     my $optx = ahgen::run_ftool("pget","coordpnt","outx");
     ahlog::ah_debug "outx: $optx";
     my $opty = ahgen::run_ftool("pget","coordpnt","outy");
     ahlog::ah_debug "outy: $opty";

     open(my $REGFILE,'>',$input{regfile});
      print $REGFILE "CIRCLE($optx,$opty,130)";
     close $REGFILE;

     return $status;
}

# ------------------------------------------------------------------------------

sub extract {

    my %passed = @_;
    my $status = 0;

    # Need to have one of lcfile, phafile, or imgfile defined.
    unless ($passed{lcfile} or $passed{imgfile} or $passed{phafile}) {
      ahlog::ah_err "Need to pass either lcfile, imgfile or phafile.";
      return 1;
    }

    # Need some input files or file.
    unless ($passed{infile}) {
      ahlog::ah_err "Need input file or files.";
      return 1;
    }

    # Need instrument type.

    # Set the defaults.
    my %input = (lcfile   => "NONE",
                 imgfile  => "NONE",
                 phafile  => "NONE",
                 binlc    => 16.0,
                 lcthresh => 0.0,
                 lcstart  => -1.0,
                 region   => "NONE",
                 binning  => 1,
                 coords   => "sky",
                 specbin  => 1,
    );

    # Overwrite the defaults.
    @input{keys %passed} = values %passed;

    my $eventfile = $input{infile};

    ahlog::ah_debug "eventfile = $eventfile";

    # These should probably be in a config file somewhere.
    my ($ecol,$wtmapb,$xcolh,$ycolh,$xcolf,$ycolf);
    if (lc $input{type} eq "hxi") {
        $ecol =  "PI";
        $wtmapb = "yes";
        $xcolh = 'DETX';
        $ycolh = 'DETY'; 
        $xcolf = 'X';
        $ycolf = 'Y';
        ($xcolh,$xcolf,$ycolh,$ycolf) = ("DETX","DETX","DETY","DETY")  
        if ($input{coords} eq "det");
    }
    elsif (lc $input{type} eq "sgd") {
        $ecol  = 'PI';
        $wtmapb = "no";
        $xcolh = 'NONE';
        $ycolh = 'NONE';
        $xcolf = 'NONE';
        $ycolf = 'NONE';
    }
    elsif (lc $input{type} eq "sxi") {
        $ecol =  "PI";
        $wtmapb = "yes";
        $xcolh = 'DETX';
        $ycolh = 'DETY'; 
        $xcolf = 'X';
        $ycolf = 'Y';
        ($xcolh,$xcolf,$ycolh,$ycolf) = ("DETX","DETX","DETY","DETY")  
        if ($input{coords} eq "det");
    }
    elsif ($input{type} eq "sxs") {
        $ecol =  "PI";
        $wtmapb = "yes";
        $xcolh = 'DETX';
        $ycolh = 'DETY'; 
        $xcolf = 'X';
        $ycolf = 'Y';
        ($xcolh,$xcolf,$ycolh,$ycolf) = ("DETX","DETX","DETY","DETY")  
        if ($input{coords} eq "det");
    }
    elsif ($input{type} eq "antico") {
        $ecol  = 'PI';
        $wtmapb = "no";
        $xcolh = 'NONE';
        $ycolh = 'NONE';
        $xcolf = 'NONE';
        $ycolf = 'NONE';
    }
    else {
      ahlog::ah_err "Don't know type $input{type}";
      return 1;
    }

    # Print debug info.
    foreach my $key (sort keys %input) {
      ahlog::ah_debug "$key = $input{$key}";
    }

    # Create the extractor object.
    $status=ahgen::run_ftool("extractor",
                               "exitnow=no",
                               "filename=$eventfile",
                               "eventsout=NONE",
                               "imgfile=$input{imgfile}",
                               "binf=1",
                               "fullimage=yes",
                               "phafile=$input{phafile}",
                               "specbin=$input{specbin}",
                               "wtmapb=$wtmapb",
                               "wtmapfix=yes",
                               "swmapx=no",
                               "swmapy=no",
                               "binh=1",
                               "wmapver=2",
                               "fitsbinlc=$input{lcfile}",
                               "qdpfile=NONE",
                               "binlc=$input{binlc}",
                               "lcthresh=$input{lcthresh}",
                               "lcthwarn=3.0",
                               "lcstart=$input{lcstart}",
                               "lctzero=yes",
                               "unbinlc=NONE",
                               "regionfile=$input{region}",
                               "timefile=NONE",
                               "gtinam=GTI",
                               "xcolf=$xcolf",
                               "ycolf=$ycolf",
                               "zcolf=NONE",
                               "xint=1.0",
                               "yint=1.0",
                               "tcol=TIME",
                               "ecol=$ecol",
                               "ccol=NONE",
                               "gcol=GRADE",
                               "gstring=NONE",
                               "xcolh=$xcolh",
                               "ycolh=$ycolh",
                               "gtitxt=NONE",
                               "xronwn=NONE",
                               "events=EVENTS",
                               "gti=STDGTI",
                               "timeorder=no",
                               "timeref=40000.0",
                               "eventkey=NONE",
                               "phamax=TLMAX",
                               "xfkey=TLMAX",
                               "yfkey=TLMAX",
                               "xhkey=TLMAX",
                               "yhkey=TLMAX",
                               "copyall=yes",
                               "clobber=yes",
                               );

    # Check for errors.
    if ( $status ) { return $status; }

    # Make sure files were created.
    if ($input{lcfile} ne "NONE") {

      if (! -e $input{lcfile}) {
          ahlog::ah_err "Extraction failed for $eventfile.  Did not create $input{lcfile}.";
          return 1;
      }

      # Extractor sometimes creates files with no data (rows).
      my $num_rows = ahgen::get_keyword($input{lcfile},1,"NAXIS2");
      if ($num_rows == 0) {
        ahlog::ah_err "No rows in $input{lcfile}";
        return 1;
      }
  
    }

    if ($input{imgfile} ne "NONE" and ! -e $input{imgfile} ) {

      ahlog::ah_err "Extraction failed for $eventfile. " . 
                    "Did not create $input{imgfile}.";

    }

    if ($input{phafile} ne "NONE" and ! -e  $input{phafile}) {

      ahlog::ah_err "Extraction failed for $eventfile. " . 
                    "Did not create $input{phafile}.";

    }

    return 0;

}

# ------------------------------------------------------------------------------

sub form_output_file_name {

  my $infile = shift;
  my $outdir = shift;
  my $steminputs = shift;
  my $stemoutputs = shift;
  my $zpatt = qr/(\.Z|\.z|\.gzip|\.GZIP|\.gz|\.GZ|\.zip\.ZIP)?/;
  my $outfile = "";

  # Get the basename of the input file and calculate the output file name
  # Using the outdir and stemoutputs parameters
  my $basename = basename($infile);
  my ($filebase) = $basename =~ /$steminputs(.*)/;

  # Strip off any 'zip' pattern
  $filebase =~ s/$zpatt$//;

  $outfile = catfile($outdir , $stemoutputs . $filebase);

  return $outfile ;

}

# ------------------------------------------------------------------------------

1;
