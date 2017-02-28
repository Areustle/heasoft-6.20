#!/usr/bin/perl
#
# File name: sxspixgti.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/11/18 22:01:57 $
# Version: 0
#
# Run ahgtigen and create 36 extension GTI file for sxs pixels. 
# Create additional GTI file with one extension and columns START, STOP, PIXEL
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
use ahfilterlib ;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

my $mkffile="";       # input mkf file 
my $outfile="";       # output sxs gti file
my $outpixfile="";    # output sxs merged gti file
my $label="";         # label for ahgtigen
my $gtilost="";       # GTI lost file
my $gtifile="";       # GTI file to merge
my $teldef="";        # teldef file for coordevt
my $randomize="";     # Randomize coordinates when rebinning (teldef, yes, no)
my $seed=0;           # Random number generator seed (0=use system time)

my $makepixfiles=0;        # make output 36-extension pixel GTI files (0=no;1=yes)
my $outfile_on="";
my $outfile_off="";
my $gtiexpr;
my $mergegti;
my $upkeyword;
my $leapsecfile;
my $selectfile;
my $instarts;
my $instops;
my $time;
my $outstart;
my $outstop;
my $prefr;
my $postfr;

my $calc_mkf=0;       # Boolean to process MKF file
my $calc_gtilost=0;   # Boolean to process GTI lost 
my $tstart="DEFAULT"; # Parameter tstart for gtiinvert
my $tstop="DEFAULT";  # Parameter tstop for gtiinvert

my $sxspixgtierror = 0;  # sxspipeline exit status

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$sxspixgtierror = get_parameters () ;
unless ( $sxspixgtierror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;  
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($sxspixgtierror);
}

$sxspixgtierror = initialize () ;
unless ( $sxspixgtierror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;   
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($sxspixgtierror);
}

$sxspixgtierror = do_work () ;
unless ( $sxspixgtierror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($sxspixgtierror);
}

$sxspixgtierror = finalize () ;
unless ( $sxspixgtierror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($sxspixgtierror);
}

# We're done.
ahapp::end_processing($sxspixgtierror);


#########################
# Subroutines
#########################

sub get_parameters {

  $mkffile     = ahapp::query_parameter("mkffile");
  $outfile     = ahapp::query_parameter("outfile");
  $outpixfile  = ahapp::query_parameter("outpixfile");
  $label       = ahapp::query_parameter("label");
  $gtilost     = ahapp::query_parameter("gtilost");
  $gtifile     = ahapp::query_parameter("gtifile");

  # ahgtigen parameters
  $gtiexpr         = ahapp::query_parameter("gtiexpr");
  $mergegti        = ahapp::query_parameter("mergegti");
  $upkeyword       = ahapp::query_parameter("upkeyword");
  $leapsecfile     = ahapp::query_parameter("leapsecfile");
  $selectfile      = ahapp::query_parameter("selectfile");
  $instarts        = ahapp::query_parameter("instarts");
  $instops         = ahapp::query_parameter("instops");
  $time            = ahapp::query_parameter("time");
  $outstart        = ahapp::query_parameter("outstart");
  $outstop         = ahapp::query_parameter("outstop");
  $prefr           = ahapp::query_parameter("prefr");
  $postfr          = ahapp::query_parameter("postfr");

  $teldef      = ahapp::query_parameter("teldeffile");
  $randomize   = ahapp::query_parameter("randomize");
  $seed        = ahapp::query_parameter("seed");

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;
  
  # We need to check that we at least have one input
  # mkf or gti lost file. If they are both missing 
  # there is nothing to do.
  if ( ! isFileNone($mkffile)  ) {
    $calc_mkf = 1;
  }
  if ( ! isFileNone($gtilost)  ) {
    $calc_gtilost = 1;
  }

  if ( $calc_mkf == 0 and $calc_gtilost == 0 ) {
    ahlog::ah_err "No valid input files. Need an MKF and/or a GTI lost file";
    return 1;
  }

  # Input file checking
  # Check for MKF/ahgtigen file requirements
  if ( $calc_mkf ) {
    if (isRequiredFileNotFound($mkffile)) { return 1; }
    # Make sure we have a selection file
    if (isBadCALDBFileParameterValue($selectfile,"selectfile")) { return 1; }
    # Make sure label has ## marks
    if ( $label !~ /[#]{2}/) {
      ahlog::ah_err "Missing ## from label parameter";
      return 1;
    }
  } else {
    $label = "none";
    $selectfile = "none";
  }
  # Check for gti lost file requirements
  if ( $calc_gtilost ) {
    if (isRequiredFileNotFound($gtilost)) { return 1; }
    # Verify EXTNAME = GTILOST and DETNAM = PIXEL 
    my $extname = ahgen::get_keyword($gtilost,1,"EXTNAME");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "EXTNAME keyword not defined in $gtilost"; return ahgen::get_error_flag;
    }
    if ( uc $extname ne "GTILOST" ) {
      ahlog::ah_err "Extension must be GTILOST for GTI lost file (EXTNAME=$extname)";
      return 1;
    }
    my $detnam = ahgen::get_keyword($gtilost,1,"DETNAM");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "DETNAM keyword not defined in $gtilost"; return ahgen::get_error_flag;
    }
    if ( uc $detnam ne "PIXEL" ) {
      ahlog::ah_err "DETNAM must be PIXEL for GTI lost file (DETNAM=$detnam)";
      return 1;
    }
  }
  if (isOptionalFileNotFound($gtifile)) { return 1; }

  # Get the tstart, tstop values for gtiinvert
  #
  # Preference to use the MKF file TSTART, TSTOP values.
  # If no MKF is input, use the first row START column
  # and last row STOP column from the input GTI file
  if ( $calc_mkf ) {
      $tstart = ahgen::get_keyword($mkffile,"FILTER","TSTART");
      unless ( defined $tstart ) {
        ahlog::ah_out"In file '$mkffile', could not read keyword TSTART";
        return 1;
      }
      $tstop = ahgen::get_keyword($mkffile,"FILTER","TSTOP");
      unless ( defined $tstart ) {
        ahlog::ah_out"In file '$mkffile', could not read keyword TSTOP";
        return 1;
      }
  } elsif ( -e $gtifile ) {
      my $nrows = ahgen::get_keyword($gtifile,"GTI","NAXIS2");
      unless ( defined $nrows ) {
        ahlog::ah_out"In file '$gtifile', could not read keyword NAXIS2";
        return 1;
      }
      $status = ahgen::run_ftool("ftabpar","fitsfile=$gtifile\[GTI]","column=START","row=1");
      if($status) { ahlog::ah_err "Error reading column START row 1 from $gtifile\[GTI]"; return $status; }
      ahgen::run_ftool("pget","ftabpar","value");
      $tstart = ahgen::get_tool_stdout();
      $status = ahgen::run_ftool("ftabpar","fitsfile=$gtifile\[GTI]","column=STOP","row=$nrows");
      if($status) { ahlog::ah_err "Error reading column STOP row $nrows from $gtifile\[GTI]"; return $status; }
      ahgen::run_ftool("pget","ftabpar","value");
      $tstop  = ahgen::get_tool_stdout();
  }

  # Check if we are saving the 36-extension pixel files
  if ( lc $outfile ne "none" and lc $outfile ne " " ) {
    $makepixfiles = 1;
  }

  # Set up output file names for 36-extension files
  if ( $makepixfiles ) {
    $outfile_on  = $outfile . "_good.gti";
    $outfile_off = $outfile . "_bad.gti";
  } else {
    $outfile_on  = "tmp_sxspixgti_good.gti";
    $outfile_off = "tmp_sxspixgti_bad.gti";
    ahapp::add_temp_file($outfile_on);
    ahapp::add_temp_file($outfile_off);
  }

  # Check if output files exist
  if (removeOutputFileIfClobbering($outfile_on,$ahapp::clobber)) { return 1; }
  if (removeOutputFileIfClobbering($outfile_off,$ahapp::clobber)) { return 1; }
  if (removeOutputFileIfClobbering($outpixfile,$ahapp::clobber)) { return 1; }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $pixlist  = "ftmerge.txt";
  my $pixlistoff  = "ftmerge_off.txt";
  my $tmpmerge = "ftmerge.out";
  my $tmpmergeoff = "ftmergeoff.out";
  my $pixlist_expr="";
  my $pixlist_expr_off="";
  my $status = 0;
  my $clobber = $ahapp::clobber ? "yes":"no";
  my $cleanup = $ahapp::cleanup ? "yes":"no";

  ahapp::add_temp_file($pixlist);
  ahapp::add_temp_file($pixlistoff);
  ahapp::add_temp_file($tmpmerge);
  ahapp::add_temp_file($tmpmergeoff);

  # Run tool ahgtigen 36 times
  # Copy keywords to each extension
  foreach my $ipix (0..35) {

    my $pixnn  = sprintf('%02i',$ipix);
    my $extname= "GTIPIX$pixnn";
    my $extnameoff= "GTIPIXOFF$pixnn";
    my $detnam = "PIX$pixnn";
    (my $label_pix = $label ) =~ s/[#]{2}/sprintf('%02d',$ipix)/e;
    my $pixgti = "pix$pixnn.gti";
    my $pixgtioff = "pix$pixnn\_off.gti";
    my $pixgtilostoff = "pix$pixnn\_lostoff.gti";
    my @gtilist = ();
    ahapp::add_temp_file($pixgti);
    ahapp::add_temp_file($pixgtioff);
    ahapp::add_temp_file($pixgtilostoff);

    # Add the general GTI file to the list of GTI to merge
    if ( ! isFileNone($gtifile) ) { push @gtilist, $gtifile . "[GTI]"; }

    # Extract current pixel and run GTI invert
    if ( $calc_gtilost ) {
      # Run gtiinvert
      $status = run_gtiinvert ( $gtilost . "[1][PIXEL==$ipix]", $pixgtilostoff, "GTI", {tstart => $tstart, tstop => $tstop, clobber=>$clobber }) ;
      if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }
      push @gtilist, $pixgtilostoff . "[GTI]";
    }

    if ( @gtilist > 1 or $calc_mkf ) {
      my $gtilist = join ",", @gtilist;
      if ( @gtilist == 0 ) { $gtilist = "none"; }
      # Run ahgtigen and create Pixel GTI with corresponding label
      #
      # ***
      # The processing pipeline has an issue with the way that ahgtigen 
      # returns an "OK" status of 2 within a script. This is due to the fact 
      # that run_ftool prints an ahlog error message if there is a nonzero 
      # status. Manually run ahgtigen here in order to avoid invalid errors.
      # ***
      #
      #$status = run_ftool("ahgtigen","infile=$mkffile","outfile=$pixgti","gtifile=$gtilist",
      #                    "gtiexpr=$gtiexpr","mergegti=$mergegti","cpkeyword=none",
      #                    "upkeyword=$upkeyword","leapsecfile=$leapsecfile",
      #                    "instrume=SXS","selectfile=$selectfile","label=$label_pix",
      #                    "instarts=$instarts","instops=$instops","time=$time",
      #                    "outstart=$outstart","outstop=$outstop","prefr=$prefr","postfr=$postfr",
      #                    "chatter=0");
      ahlog::ah_out "FTOOLS CMD: ahgtigen infile=$mkffile outfile=$pixgti gtifile=$gtilist gtiexpr=$gtiexpr mergegti=$mergegti cpkeyword=none upkeyword=$upkeyword leapsecfile=$leapsecfile instrume=SXS selectfile=$selectfile label=$label_pix instarts=$instarts instops=$instops time=$time outstart=$outstart outstop=$outstop prefr=$prefr postfr=$postfr chatter=0 clobber=$clobber cleanup=$cleanup";
      `ahgtigen infile=$mkffile outfile=$pixgti gtifile=$gtilist gtiexpr=$gtiexpr mergegti=$mergegti cpkeyword=none upkeyword=$upkeyword leapsecfile=$leapsecfile instrume=SXS selectfile=$selectfile label=$label_pix instarts=$instarts instops=$instops time=$time outstart=$outstart outstop=$outstop prefr=$prefr postfr=$postfr chatter=0 clobber=$clobber cleanup=$cleanup`;
      $status = $? >> 8;
      if ( $status ) { 
        if ( $status != 2 ) {
          ahlog::ah_err "ahgtigen failed"; return $status; 
        } else {
          # Reset the status
          ahlog::ah_info "No exposure for pixel $ipix";
          $status = 0;
        }
      }
      if (set_keyword($pixgti,"GTI","EXTNAME",$extname)) { return 1; }
      # Run gtiinvert
      $status = run_gtiinvert ( $pixgti . "[$extname]", $pixgtioff, $extnameoff, {tstart => $tstart, tstop => $tstop, clobber=>$clobber }) ;
      if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }
      if (lc $upkeyword eq "yes") {
        $status = ahfilterlib::calc_timing_keys($pixgtioff,$extnameoff,$leapsecfile);
        if ( $status ) { ahlog::ah_err "failed to update timing keywords on inverted GTI"; return $status; }
      }
      
    } else {
      # We still want to create the same output files as if ahgtigen was run
      # Copy the GTI lost (off) to the GTIPIXnn extension
      if (ahgen::copy_hdu($pixgtilostoff,"GTI][col #EXTNAME=\"$extname\";START;STOP;",$pixgti)) { return 1; }
      # Copy the GTI lost (on) to the GTIPIXnnOFF extension
      if (ahgen::copy_hdu($gtilost,"1][col #EXTNAME=\"$extnameoff\";START;STOP;PIXEL;][PIXEL==$ipix",$pixgtioff)) { return 1; }
    }

    # Update the extension name and copy to output file
    #if (set_keyword($pixgtioff,$extnameoff,"EXTNAME",$extnameoff)) { return 1; }

    if (set_keyword($pixgti,$extname,"DETNAM",$detnam)) { return 1; }
    if (set_keyword($pixgtioff,$extnameoff,"DETNAM",$detnam)) { return 1; }

    # Copy the on and off pixel files to the output files, if necessary
    if ( $makepixfiles ) {
      if (ahgen::copy_hdu($pixgti,$extname,$outfile_on)) { return 1; }
      if (ahgen::copy_hdu($pixgtioff,$extnameoff,$outfile_off)) { return 1; }
    }

    # ftcalc fails if there are zero rows when adding a new column
    # Regardless, zero row files are unnecessary
    # Get number of rows in GTI files
    # read the NAXIS2 keyword for fine GTI
    my $naxis2 = ahgen::get_keyword($pixgti,$extname,"NAXIS2");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "NAXIS2 keyword not defined in $pixgti"; return ahgen::get_error_flag;
    }
    if ($naxis2) {
      # Add a pixel column to the GTI file
      $status = ahgen::run_ftool("ftcalc",$pixgti."[$extname]",$pixgti,"PIXEL",$ipix,"tform=1B","clobber=yes");
      if ( $status ) { ahlog::ah_err "ftcalc failed"; return $status; }
      # build a list of files to merge
      $pixlist_expr .= $pixgti."[$extname]\n";
    }

    my $naxis2_off = ahgen::get_keyword($pixgtioff,$extnameoff,"NAXIS2");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "NAXIS2 keyword not defined in $pixgti"; return ahgen::get_error_flag;
    }
    if ($naxis2_off) {
      # Add a pixel column to the inverted GTI file
      $status = ahgen::run_ftool("ftcalc",$pixgtioff."[$extnameoff]",$pixgtioff,"PIXEL",$ipix,"tform=1B","clobber=yes");
      if ( $status ) { ahlog::ah_err "ftcalc failed"; return $status; }
      # build a list of files to merge
      $pixlist_expr_off .= $pixgtioff."[$extnameoff]\n";
    }

    # Write the sxspixgti parameters to the output file
    if ( $makepixfiles ) {
      ahapp::write_parameters ($outfile_on,$extname) ;
      ahapp::write_parameters ($outfile_off,$extnameoff) ;
    }

  } # end loop on pixel

  # Create text file to merge pixel lists
  # +++ 2016-02-19 AS: What to do if input list is empty?
  
  # Set a file to copy keywords from
  my $keyfile = "";
  my $keyext = "";
  if ( $calc_mkf ) { $keyfile = $mkffile; $keyext="FILTER"}
  else { $keyfile = $gtilost; $keyext = "1";}

  # Create the column description file
  my $gtiempty="empty.gti";
  my $cdfile="gti.cdf";
  my $dtfile="gti.dat";
  ahapp::add_temp_file($gtiempty);
  ahapp::add_temp_file($cdfile);
  ahapp::add_temp_file($dtfile);
  open CDFILE, ">", $cdfile;
  print CDFILE "START 1D sec\n";
  print CDFILE "STOP 1D sec\n";
  print CDFILE "PIXEL 1B\n";
  close CDFILE;
  # Print to the data file
  open DTFILE, ">", $dtfile;
  close DTFILE;

  # Create a minimum empty GTI file in case no GTI were created
  # Run ftcreate and append to the output file
  $status = ahgen::run_ftool("ftcreate","cdfile=$cdfile","datafile=$dtfile",
                             "outfile=$gtiempty","extname=GTI",
                             "clobber=yes");
  if($status) { ahlog::ah_err "Error running ftcreate for GTI"; return $status; }

  open SXSPIXMG, ">", $pixlist;
  print SXSPIXMG $gtiempty . "[GTI]\n";
  print SXSPIXMG $pixlist_expr;
  close SXSPIXMG;

  # Merge the selected files
  $status = ahgen::run_ftool("ftmerge","\@$pixlist",$tmpmerge,"copyall=no","skipbadfile=no","clobber=yes");
  if ( $status ) { ahlog::ah_err "ftmerge failed"; return $status; }

  # Update the output extension name
  if (set_keyword($tmpmerge,"1","EXTNAME","GTIPIXEL")) { return 1; }
  if (set_keyword($tmpmerge,"1","HDUCLASS","OGIP","format conforms to OGIP/GSFC standards")) { return 1; }
  if (set_keyword($tmpmerge,"1","HDUCLAS1","GTI","Extension contains Good Time Intervals")) { return 1; }
  if (set_keyword($tmpmerge,"1","HDUCLAS2","ALL",)) { return 1; }

  # Sort the output file by START time, STOP time then PIXEL
  my $coordevt = $tmpmerge . ".coordevt";
  ahapp::add_temp_file($coordevt);
  $status = ahgen::run_ftool("ftsort",$tmpmerge."[GTIPIXEL]",$coordevt,"START,STOP,PIXEL","clobber=yes");
  if ( $status ) { ahlog::ah_err "ftsort failed"; return $status; }

  if (ahfilterlib::copy_keywords($keyfile,"$keyext][col -TSTART;-TSTOP",$coordevt,"GTIPIXEL","gti","all")) { return 1; }
  $status = ahgen::set_keyword($coordevt,"GTIPIXEL","INSTRUME","SXS");
  if($status) { ahlog::ah_err "Error updating INSTRUME keyword for GTIPIXEL"; return $status; }
  $status = ahgen::set_keyword($coordevt,"GTIPIXEL","DETNAM","PIXEL");
  if($status) { ahlog::ah_err "Error updating DETNAM keyword for GTIPIXEL"; return $status; }
  $status = ahgen::run_ftool("coordevt",$coordevt,$outpixfile,$teldef,"stopsys=FOC","infileext=GTIPIXEL","randomize=$randomize","seed=$seed");
  if ( $status ) { ahlog::ah_err "coordevt failed"; return $status; }

  my $pixel_rows = ahgen::get_keyword($outpixfile,"GTIPIXEL","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "NAXIS2 keyword not defined in $outpixfile"; return ahgen::get_error_flag;
  }

  # Update the TSTART and TSTOP keyword values
  if (lc $upkeyword eq "yes") {
    $status=ahfilterlib::calc_timing_keys($outpixfile,"GTIPIXEL",$leapsecfile);
    if ( $status ) {ahlog::ah_err "failed to write timing keywords for GTIPIXEL"; return $status; }
  }

  # Delete the tmp file
  unlink $tmpmerge;

  # Create text file to merge pixel lists
  open SXSPIXMG, ">", $pixlist;
  print SXSPIXMG $gtiempty . "[GTI]\n";
  print SXSPIXMG $pixlist_expr_off;
  close SXSPIXMG;

  # Merge the selected files
  $status = ahgen::run_ftool("ftmerge","\@$pixlist",$tmpmerge,"copyall=no","skipbadfile=no","clobber=yes");
  if ( $status ) { ahlog::ah_err "ftmerge failed"; return $status; }

  # Update the output extension name
  if (set_keyword($tmpmerge,"1","EXTNAME","GTIPIXELOFF")) { return 1; }
  if (set_keyword($tmpmerge,"1","HDUCLASS","OGIP","format conforms to OGIP/GSFC standards")) { return 1; }
  if (set_keyword($tmpmerge,"1","HDUCLAS1","GTI","Extension contains Good Time Intervals")) { return 1; }
  if (set_keyword($tmpmerge,"1","HDUCLAS2","ALL",)) { return 1; }

  $coordevt = $tmpmergeoff . ".coordevt";
  ahapp::add_temp_file($coordevt);

  # Sort the output file by START time, STOP time then PIXEL
  $status = ahgen::run_ftool("ftsort",$tmpmerge."[GTIPIXELOFF]",$coordevt,"START,STOP,PIXEL","clobber=yes");
  if ( $status ) { ahlog::ah_err "ftsort failed"; return $status; }

  if (ahfilterlib::copy_keywords($keyfile,"$keyext][col -TSTART;-TSTOP",$coordevt,"GTIPIXELOFF","gti","all")) { return 1; }
  $status = ahgen::set_keyword($coordevt,"GTIPIXELOFF","INSTRUME","SXS");
  if($status) { ahlog::ah_err "Error updating INSTRUME keyword for GTIPIXELOFF"; return $status; }
  $status = ahgen::set_keyword($coordevt,"GTIPIXELOFF","DETNAM","PIXEL");

  if($status) { ahlog::ah_err "Error updating DETNAM keyword for GTIPIXELOFF"; return $status; }
  # Run coordevt to calculate PIXEL -> FOC
  $status = ahgen::run_ftool("coordevt",$coordevt,$tmpmergeoff,$teldef,"stopsys=FOC","infileext=GTIPIXELOFF","randomize=$randomize","seed=$seed","clobber=$clobber");
  if ( $status ) { ahlog::ah_err "coordevt failed"; return $status; }
  if (ahgen::copy_hdu($tmpmergeoff,"GTIPIXELOFF",$outpixfile)) { return 1; }

  my $pixeloff_rows = ahgen::get_keyword($outpixfile,"GTIPIXELOFF","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "NAXIS2 keyword not defined in $outpixfile"; return ahgen::get_error_flag;
  }

  # Update the TSTART and TSTOP keyword values
  if (lc $upkeyword eq "yes") {
    $status=ahfilterlib::calc_timing_keys($outpixfile,"GTIPIXELOFF",$leapsecfile);
    if ( $status ) {ahlog::ah_err "failed to write timing keywords for GTIPIXEL"; return $status; }
  }

  # Write the sxspixgti parameters to the output file
  ahapp::write_parameters ($outpixfile,"GTIPIXEL") ;
  ahapp::write_parameters ($outpixfile,"GTIPIXELOFF") ;



  return 0;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  # Verify output files
  if ( $makepixfiles ) {
    $status = update_checksum_and_verify($outfile_on);
    unless ( $status ) { ahlog::ah_err "verify failed for $outfile_on"; return $status; }
    $status = update_checksum_and_verify($outfile_off);
    unless ( $status ) { ahlog::ah_err "verify failed for $outfile_off"; return $status; }
  }
  $status = update_checksum_and_verify($outpixfile);
  unless ( $status ) { ahlog::ah_err "verify failed for $outpixfile"; return $status; }

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
  my @ordered_pars = qw( margingti tstart tstop dt clobber );
  foreach my $par (@ordered_pars) {
    if(defined $gtiinvert_pars{$par}) { push @params, [$par, $gtiinvert_pars{$par}]; }
  }

  if (runTool($toolname,\@params)) { return 1; }
  if (copyOrMoveFileBasedOnCleanup($tmpoutfile, $outfile, $ahapp::cleanup)) { return 1; }

  return 0;
  
}

# ------------------------------------------------------------------------------
