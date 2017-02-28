#!/usr/bin/perl
#
# File name: sxsregext.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/12/15 02:37:34 $
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
use ahfilterlib ;

# turn on AUTOFLUSH
$| = 1;

# For a 2D array, sort the second dimension, then the first
sub sortxy { sort { $a->[1] <=> $b->[1] || $a->[0] <=> $b->[0] } @_ }

#########################
# Variable Definitions 
#########################

my $infile     =""; # Input event file
my $regmode    =""; # Region file type ([RADEC],DET)
my $region     =""; # Region file name

my $extract    =0;  # Extract image/lightcurve/spectra ([yes],no)
my $outroot    =""; # Root of output file names
my $resolist   =""; # List of ITYPE or ALL
my $teldeffile =""; # Input teldef file (or CALDB)
                                                       
my $outexp     =""; # Output exposure map
my $ehkfile    =""; # Input EHK file
my $delta      =0.0;# Range of off-axis angle (theta) in one map [arcmin]
my $numphi     =0;  # Number of azimuth (phi) bins"
my $badimgfile =""; # Input bad pixel image file (or NONE)
my $pixgtifile =""; # Flickering pixel list (or NONE)
my $instmap    =""; # Instrument map (or CALDB)
my $qefile     =""; # QE file (or CALDB)
my $contamifile=""; # Contamination file (or CALDB)
my $obffile    =""; # Optical blocking filter file for SXS (or CALDB)
my $fwfile     =""; # Filter wheel file for SXS (or CALDB)
my $gvfile     =""; # Gatevalve file for SXS (or CALDB)
my $maskcalsrc =""; # Mask out calibration sources? ([yes]/no)
my $fwtype     =""; # Filter wheel type for SXS ([OPEN], FE55, BE, NE, or POLY)
my $specmode   =""; # Type of input energy ([MONO] or SPEC)
my $specfile   =""; # Input spectrum file
my $specform   =""; # Format of spectrum file ([FITS] or ASCII)
my $extended   =""; # Use extended energy range mode?

my $telescop   ="HITOMI";
my $instrume   ="SXS";

my $outreg     =""; # Output region file
my $img        =""; # Output image file
my $lc         =""; # Output lightcurve file
my $pha        =""; # Output spectrum file

my @pixels     = ( 0 ) x 35;

my $sxsregexterror = 0;  # sxsregext exit status

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$sxsregexterror = get_parameters () ;
unless ( $sxsregexterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($sxsregexterror);
}

$sxsregexterror = initialize () ;
unless ( $sxsregexterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($sxsregexterror);
}

$sxsregexterror = do_work () ;
unless ( $sxsregexterror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($sxsregexterror);
}

$sxsregexterror = finalize () ;
unless ( $sxsregexterror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($sxsregexterror);
}

# We're done.
ahapp::end_processing($sxsregexterror);


#########################
# Subroutines
#########################

sub get_parameters {

  $infile     = ahapp::query_parameter("infile");
  $regmode    = ahapp::query_parameter("regmode");
  $region     = ahapp::query_parameter("region");
               
  $extract    = ahapp::query_parameter("extract",1);
  $outroot    = ahapp::query_parameter("outroot");
  $resolist   = ahapp::query_parameter("resolist");
  $teldeffile = ahapp::query_parameter("teldeffile");
               
  $outexp     = ahapp::query_parameter("outexp");
  $ehkfile    = ahapp::query_parameter("ehkfile");
  $delta      = ahapp::query_parameter("delta");
  $numphi     = ahapp::query_parameter("numphi");
  $badimgfile = ahapp::query_parameter("badimgfile");
  $pixgtifile = ahapp::query_parameter("pixgtifile");
  $instmap    = ahapp::query_parameter("instmap");
  $qefile     = ahapp::query_parameter("qefile");
  $contamifile= ahapp::query_parameter("contamifile");
  $obffile    = ahapp::query_parameter("obffile");
  $fwfile     = ahapp::query_parameter("fwfile");
  $gvfile     = ahapp::query_parameter("gvfile");
  $maskcalsrc = ahapp::query_parameter("maskcalsrc");
  $fwtype     = ahapp::query_parameter("fwtype");
  $specmode   = ahapp::query_parameter("specmode");
  $specfile   = ahapp::query_parameter("specfile");
  $specform   = ahapp::query_parameter("specform");
  $extended   = ahapp::query_parameter("extended",1);

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;
  
  # Input file checking
  if (isRequiredFileNotFound($infile)) { return 1; }
  if (isRequiredFileNotFound($region)) { return 1; }
  if ( uc $regmode eq "RADEC" ) {
    if (isRequiredFileNotFound($ehkfile)) { return 1; }
    # Setup output region file name
    $outreg = $outroot . ".reg";
    if (removeOutputFileIfClobbering($outreg,$ahapp::clobber) ) { return 1; }
    if (removeOutputFileIfClobbering($outexp,$ahapp::clobber)) { return 1; }
  } else {
    if ( ! $extract ) { 
      ahlog::ah_err "Nothing to do.";
      return 1;
    }
    # Use the input region file for the extractor input
    $outreg = $region;
  }

  # Make sure we have a selection file
  if (isBadCALDBFileParameterValue($teldeffile,"teldeffile")) { return 1; }

  # Check for valid resolist parameter
  if ( uc $resolist eq "ALL" ) {
    $resolist = "0,1,2,3,4";
  } 
  # Verify that we have valid values for ITYPE
  my @splitresolist = split ",",$resolist;
  foreach my $itype ( @splitresolist ) {
    if ( $itype !~ /[0-4]/ ) {
      ahlog::ah_err "Not a valid ITYPE(=$itype), must be between 0 and 4";
      return 1;
    } 
  }

  # Set up output file names
  if ( $extract ) {
    $img = $outroot . ".img";
    $lc  = $outroot . ".lc";
    $pha = $outroot . ".pha";
    if (removeOutputFileIfClobbering($img,$ahapp::clobber) ) { return 1; }
    if (removeOutputFileIfClobbering($lc ,$ahapp::clobber) ) { return 1; }
    if (removeOutputFileIfClobbering($pha,$ahapp::clobber) ) { return 1; }
  }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;

  # If 'regmode' is RADEC:
  #   a. run ahexpmap
  #   b. determine pixels to extract
  #   c. run ahmkregion to create DET region
  #   d. condense region file
  if ( uc $regmode eq "RADEC" ) {

    # Run ahexpmap
    $status = ahgen::run_ftool("ahexpmap",
                               "ehkfile=$ehkfile",
                               "gtifile=$infile",
                               "instrume=$instrume",
                               "badimgfile=$badimgfile",
                               "pixgtifile=$pixgtifile",
                               "outfile=$outexp",
                               "outmaptype=EXPOSURE",
                               "delta=$delta",
                               "numphi=$numphi",
                               "stopsys=SKY",
                               "instmap=$instmap",
                               "qefile=$qefile",
                               "contamifile=$contamifile",
                               "vigfile=NONE",
                               "obffile=$obffile",
                               "fwfile=$fwfile",
                               "gvfile=$gvfile",
                               "maskcalsrc=$maskcalsrc",
                               "fwtype=$fwtype",
                               "specmode=$specmode",
                               "specfile=$specfile",
                               "specform=$specform",
                               #"energy=$energy",
                               #"evperchan=$evperchan",
                               #"abund=$abund",
                               #"cols=$cols",
                               #"covfac=$covfac"
                             );
    if ( $status ) {
      ahlog::ah_err "ahexpmap failed.";
      return $status;
    }

    # Determine the number of rows (bins) in the output file
    my $naxis2 = ahgen::get_keyword($outexp,"OFFAXISHIST","NAXIS2");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "NAXIS2 keyword not defined in $outexp"; return ahgen::get_error_flag;
    }
    my $ra_nom = ahgen::get_keyword($outexp,"OFFAXISHIST","RA_NOM");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "RA_NOM keyword not defined in $outexp"; return ahgen::get_error_flag;
    }
    my $dec_nom = ahgen::get_keyword($outexp,"OFFAXISHIST","DEC_NOM");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "DEC_NOM keyword not defined in $outexp"; return ahgen::get_error_flag;
    }

    # Process the output exposure map
    my @ranomxp  = ahgen::read_column($outexp,"OFFAXISHIST","RANOMXP");
    if($naxis2 != @ranomxp  ) { ahlog::ah_err "Error reading RANOMXP column in $outexp"; return 1; }
    my @decnomxp = ahgen::read_column($outexp,"OFFAXISHIST","DECNOMXP");
    if($naxis2 != @decnomxp ) { ahlog::ah_err "Error reading DECNOMXP column in $outexp"; return 1; }
    my @pa_nomxp= ahgen::read_column($outexp,"OFFAXISHIST","PA_NOMXP");
    if($naxis2 != @pa_nomxp ) { ahlog::ah_err "Error reading PA_NOMXP column in $outexp"; return 1; }

    # Loop through each row in the output exposure map
    foreach my $bin ( 0 .. $naxis2 - 1 ) {

      # Get row element for each column
      my $ranomxp  = $ranomxp[$bin];
      my $decnomxp = $decnomxp[$bin];
      my $pa_nomxp = $pa_nomxp[$bin];

      # Create a temporary coordinate bin file
      my $binfile = "bin" . $bin . ".pnt";
      ahapp::add_temp_file($binfile);

      # Run coordpnt to convert the input sky region to pixel region
      $status = ahgen::run_ftool("coordpnt",
                                 "input=$region",
                                 "outfile=$binfile",
                                 "telescop=$telescop",
                                 "instrume=$instrume",
                                 "ra=$ranomxp",
                                 "dec=$decnomxp",
                                 "roll=$pa_nomxp",
                                 "ranom=$ra_nom",
                                 "decnom=$dec_nom",
                                 "teldeffile=$teldeffile",
                                 "startsys=RADEC",
                                 "stopsys=LOWEST",
                                 "pixeltest=partial",
                                 "clobber=yes",
                               );
      if ( $status ) {
        ahlog::ah_err "coordpnt failed.";
        return $status;
      }

      # Verify that a region file was created
      if (isRequiredFileNotFound($binfile)) { return 1; }

      # Open the output region file and parse each pixel region
      open REGFILE, $binfile;
      foreach my $row ( <REGFILE> ) {
        # Skip rows that are not pixel( ... )
        unless ( $row =~ /pixel\(.*\)/ ) { next; }
        # Read the string of pixels between parentheses
        $row =~ /pixel\((.*)\)/;
        # Split the string on commas
        my @pixel_list = split (",",$1);
        # Store the pixel in the pixels array
        foreach my $ipix ( @pixel_list ) {
          $pixels[$ipix] = 1;
        }
      }
      close REGFILE;
    }

    # Condense the list of pixels
    my @pixlist = ();
    foreach my $ipix ( 0 .. 35 ) { push @pixlist, $ipix if $pixels[$ipix]; }

    # Merge the pixels array into a comma-delimited string
    my $pixlist = join (",",@pixlist);

    # Run ahmkregion to convert pixel list to DET region
    $status = ahgen::run_ftool("ahmkregion",
                               "instrume=$instrume",
                               "ra=45.0",  # Not relevant
                               "dec=30.0", # Not relevant
                               "roll=0.0", # Not relevant
                               "teldeffile=$teldeffile",
                               "inregion=NONE",
                               "outroot=$outroot",
                               "outtextregion=no",
                               "outxco=no",
                               "pixlist=$pixlist",
                               "cleanup=yes");
      if ( $status ) {
        ahlog::ah_err "ahmkregion failed.";
        return $status;
      }

      # Verify that a DET BOX region file was created
      my $detreg = $outroot . ".DET.box.reg";
      if (isRequiredFileNotFound($detreg)) { return 1; }

      # Create the simplified region file
      my @allinfo = ();
      my $numboxes = 0;
      # Open the output region file and parse each pixel region
      open DETREG, $detreg;
      foreach my $row ( <DETREG> ) {
        # Skip rows that are not pixel( ... )
        unless ( $row =~ /box\(.*\)/ ) { next; }
        # Read the string of pixels between parentheses
        $row =~ /box\((.*)\)/;
        # Split the string on commas
        my ($xcen,$ycen,$xsize,$ysize,$angle) = split (",",$1);
        # round the center points
        my $xcenter = int($xcen + 0.5);
        my $ycenter = int($ycen + 0.5);
        # push the elements into a 2d array
        push @allinfo, [$xcenter,$ycenter, $xsize, $ysize];
        $numboxes += 1;
      }
      close DETREG;
      unless ( $numboxes ) { ahlog::ah_err "No regions found after running ahmkregion"; return 1; } 

      # Sort by ycenter, then xcenter
      my @sorted = sortxy (@allinfo);

      # Count the number of unique y-values
      my @nx = ( 0 ) x 6; # Initialize a 6-element array to zeroes
      foreach my $iy ( 0 .. $numboxes - 1 ) {
        # Get the ycenter value
        my $y1 = $sorted[$iy][1];
        # add one to ycenter value
        $nx[$y1-1] += 1;
      }

      # Condense the boxes
      my $offset = 0;
      my $x1 = 0;
      my $y1 = 0;
      my $x2 = 0;
      my $y2 = 0;
      my $xs = 0;
      my $ys = 0;
      my $prevx = -1;
      open OUTREG, ">",$outreg;
      print OUTREG "physical\n";
      foreach my $iy ( 1 .. 6 ) {
        my $nx = $nx[$iy-1]; # number of small boxes with that ycenter
        unless ( $nx ) { next; } # Skip to the next group if NX is zero
        my $start = 1; # initialize a new box
        foreach my $ix ( 0 .. $nx - 1 ) { # loop over all x boxes
          # Get the x and y center values from ahmkregion
          my $xcen = $sorted[$offset+$ix][0];
          my $ycen = $sorted[$offset+$ix][1];
          if ( $ix==0 ) { $prevx = $xcen; } # initialize the prevx variable
          if ( $start ) { # start a new box
            $x1 = $xcen - 0.5; # lower limit of box
            $x2 = $xcen + 0.5; # upper limit of box
            $ys = $sorted[$offset+$ix][3]; # ysize of box
            $start = 0; # start a new box
          } 
          if ( ($xcen - $prevx) <= 1 ) { # adjacency condition
            $x2 = $xcen + 0.5 unless $ix == 0; # extend upper limit of box
            if ( $ix == $nx - 1 ) { # special case: last box at this y; close the box;
              $start = 1; # prepare to start a new box
            }
          } else { # close the box
            $start = 1; # prepare to start a new box
          }
          if ( $start ) { # last box at this y; write to the region file;
            my $newxcen = 0.5*($x1+$x2); # Use the midpoint between the first and last x-box
            my $newycen = $ycen; # ycenter is the current y group
            my $newxsiz = $x2-$x1; # calculate the x box size
            my $newysiz = $ys; # y box size is always 1
            # Write to the region file
            print OUTREG "+box($newxcen,$newycen,$newxsiz,$newysiz)\n";
          }
          # Save the previous xcenter for next x
          $prevx = $xcen;
        } # end loop over x
        # append to the offset
        $offset += $nx;
      } # end loop over y
      close OUTREG;

  } # end if regmode eq RADEC

  # Run the extractor to create the image, lightcurve and spectra
  if ( $extract ) {

    # Create the ITYPE filtered image
    $status=ahgen::run_ftool("extractor",
                             "exitnow=no",
                             "filename=$infile",
                             "eventsout=NONE",
                             "imgfile=$img",
                             "binf=1",
                             "fullimage=yes",
                             "phafile=NONE",
                             "specbin=1",
                             "wtmapb=no",
                             "wtmapfix=yes",
                             "swmapx=no",
                             "swmapy=no",
                             "binh=1",
                             "wmapver=2",
                             "fitsbinlc=NONE",
                             "qdpfile=NONE",
                             "binlc=16",
                             "lcthresh=0",
                             "lcthwarn=3.0",
                             "lctzero=yes",
                             "unbinlc=NONE",
                             "regionfile=NONE",
                             "timefile=NONE",
                             "gtinam=GTI",
                             "xcolf=DETX",
                             "ycolf=DETY",
                             "zcolf=NONE",
                             "xint=1.0",
                             "yint=1.0",
                             "tcol=TIME",
                             "ecol=PI",
                             "ccol=NONE",
                             "gcol=ITYPE",
                             "gstring=$resolist",
                             "xcolh=X",
                             "ycolh=Y",
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
    if ( $status ) {
      ahlog::ah_err "extractor failed.";
      return $status;
    }

    # Create the grade and region filtered lightcurve and spectra
    if (!$extended) { 
      $status = ahgen::run_ftool("extractor",
                                 "exitnow=no",
                                 "filename=$infile",
                                 "eventsout=NONE",
                                 "imgfile=NONE",
                                 "binf=1",
                                 "fullimage=yes",
                                 "phafile=$pha",
                                 "specbin=1",
                                 "wtmapb=no",
                                 "wtmapfix=yes",
                                 "swmapx=no",
                                 "swmapy=no",
                                 "binh=1",
                                 "wmapver=2",
                                 "fitsbinlc=$lc",
                                 "qdpfile=NONE",
                                 "binlc=16",
                                 "lcthresh=0",
                                 "lcthwarn=3.0",
                                 "lctzero=yes",
                                 "unbinlc=NONE",
                                 "regionfile=$outreg",
                                 "timefile=NONE",
                                 "gtinam=GTI",
                                 "xcolf=DETX",
                                 "ycolf=DETY",
                                 "zcolf=NONE",
                                 "xint=1.0",
                                 "yint=1.0",
                                 "tcol=TIME",
                                 "ecol=PI",
                                 "ccol=NONE",
                                 "gcol=ITYPE",
                                 "gstring=$resolist",
                                 "xcolh=X",
                                 "ycolh=Y",
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
                                 "clobber=yes");    
    } else {
      $status=ahgen::run_ftool("extractor",
                               "exitnow=no",
                               "filename=$infile",
                               "eventsout=NONE",
                               "imgfile=NONE",
                               "binf=1",
                               "fullimage=yes",
                               "phafile=$pha",
                               "specbin=1",
                               "wtmapb=no",
                               "wtmapfix=yes",
                               "swmapx=no",
                               "swmapy=no",
                               "binh=1",
                               "wmapver=2",
                               "fitsbinlc=$lc",
                               "qdpfile=NONE",
                               "binlc=16",
                               "lcthresh=0",
                               "lcthwarn=3.0",
                               "lctzero=yes",
                               "unbinlc=NONE",
                               "regionfile=$outreg",
                               "timefile=NONE",
                               "gtinam=GTI",
                               "xcolf=DETX",
                               "ycolf=DETY",
                               "zcolf=NONE",
                               "xint=1.0",
                               "yint=1.0",
                               "tcol=TIME",
                               "ecol=PIE",
                               "ccol=NONE",
                               "gcol=ITYPE",
                               "gstring=$resolist",
                               "xcolh=X",
                               "ycolh=Y",
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
    }
      if ( $status ) {
        ahlog::ah_err "extractor failed.";
        return $status;
      }

  } # end if extract

  return 0;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  ## Replace the USER keyword to avoid unit test issues...
  $status = ahgen::run_ftool("fthedit",$img . "[PRIMARY]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $img"; return $status; }
  $status = ahgen::run_ftool("fthedit",$img . "[GTI]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $img"; return $status; }

  # Update checksum/datasum and verify the output files
  $status = update_checksum_and_verify($img);
  unless ( $status ) { ahlog::ah_err "verify failed for $img"; return $status; }

  ## Replace the USER keyword to avoid unit test issues...
  $status = ahgen::run_ftool("fthedit",$lc . "[PRIMARY]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $lc"; return $status; }
  $status = ahgen::run_ftool("fthedit",$lc . "[RATE]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $lc"; return $status; }
  $status = ahgen::run_ftool("fthedit",$lc . "[GTI]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $lc"; return $status; }
  $status = ahgen::run_ftool("fthedit",$lc . "[REG00101]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $lc"; return $status; }


  # Update checksum/datasum and verify the output files
  $status = update_checksum_and_verify($lc);
  unless ( $status ) { ahlog::ah_err "verify failed for $lc"; return $status; }
  
  if ( $extract ) {
      # Replace the USER keyword to avoid unit test issues...
  $status = ahgen::run_ftool("fthedit",$pha . "[PRIMARY]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create spectrum for file $pha"; return $status; }
  $status = ahgen::run_ftool("fthedit",$pha . "[SPECTRUM]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create spectrum for file $pha"; return $status; }
  $status = ahgen::run_ftool("fthedit",$pha . "[GTI]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create spectrum for file $pha"; return $status; }
  $status = ahgen::run_ftool("fthedit",$pha . "[REG00101]","USER","a","sxsregext");
  if($status) { ahlog::ah_err "Could not create spectrum for file $pha"; return $status; }
    $status = update_checksum_and_verify($pha);
    unless ( $status ) { ahlog::ah_err "verify failed for $pha"; return $status; }
  }

  return 0;
  
}

# ------------------------------------------------------------------------------
