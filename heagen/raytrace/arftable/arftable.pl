#!/usr/bin/perl
#
# File name: arftable.pl
# Author:    Kristin Rutkowski
# $Date: 2016/12/02 20:32:18 $
# Version: 1.0
#
# This Perl script will create ARF file from photon history file
#
# Tool dependencies:
#   ftcreate
#   ftcopy
#   ftappend
#   fthedit
#
# Library dependencies:
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#   gen/lib/perl/ahlog
#
# Modification history:
#
#  Ver   Date        Author  Description
#  1.0   2015-08-10  KLR     New header format. 
#



# note: offaxis angles are referred to as theta, azimuthal angles as phi


##################################################
#  Pragmas
##################################################

use strict;
use warnings;

use Math::Trig;         # for deg2rad

##################################################
#  Packages
##################################################

# empty parenthesis force explicit use of namespaces
use ahlog ();
use ahgen ();
use ahapp ();           # $clobber $chatter $logfile $debug $history $mode

use File::Copy;
use List::Util qw(first);
use List::Util qw( sum );

use Data::Dumper;       # to output arrays for debugging



##################################################
#  Startup
##################################################

my $CHUNKSIZE = 1000000;  #RSH161129 for buffering in photons

my $nargs = scalar @ARGV ;

# Query canonical APE parameters and start logging
ahapp::startup () ;



##################################################
#  Input Parameters
##################################################

# Query non-standard APE parameters.

# Description of parameters for object placement
# objecttype {string} CIRCLE or IMAGE 
# inimagefile [value1 value2 ..] {string, #values = #objects }
# objectheights [value1 value2 ..] {string, #values = #objects; units mm}  
# objectradii [value1 value2 ..] {string, #values = #objects; units mm} 
# imagerotangles [value1 value2 ..]  {real, #values = #objects; units degrees; sense is look-down & counter-clockwise}
# xoffsets [value1 value2 ..] {string, #values = #objects; units mm}
# yoffsets [value1 value2 ..] {string, #values = #objects; units mm}
# regioncenter [value1,value2] {real, 2 values, units arcmin}
# regionradius [value] {real, units arcmin} (negative value = no region selection)

my $inxrtevtfile     = ahapp::query_parameter("inxrtevtfile") ;
my $inrmffile        = ahapp::query_parameter("inrmffile") ;
my $outfileroot      = ahapp::query_parameter("outfileroot") ;
my $writeevtfile     = ahapp::query_parameter("writeevtfile",1) ;
my $tgtOffaxis       = ahapp::query_parameter("tgtOffaxis");
my $tgtAzimuth       = ahapp::query_parameter("tgtAzimuth");
my $mergePtg         = ahapp::query_parameter("mergePtg",1);
my $vignettingfile   = ahapp::query_parameter("vignetting");

my $objecttype       = ahapp::query_parameter("objecttype");
my $inimagefile      = ahapp::query_parameter("inimagefile");
my $objectheights    = ahapp::query_parameter("objectheights");
my $objectradii      = ahapp::query_parameter("objectradii");
my $imagerotangles   = ahapp::query_parameter("imagerotangles");
my $xoffsets         = ahapp::query_parameter("xoffsets");
my $yoffsets         = ahapp::query_parameter("yoffsets");
my $regioncenter     = ahapp::query_parameter("regioncenter");
my $regionradius     = ahapp::query_parameter("regionradius");

# 'normal' - mergePtg=no
#            the original mode of the tool.  interpolate between the two angles
#            directly above and below the target theta and phi
# 'merge'  - mergePtg=yes
#            create an EA then ARF using *every* angle
my $doMerge = $mergePtg;

my $doVignetting     = (lc($vignettingfile) ne "none");
my $doARF            = (lc($inrmffile) ne "none");

my $outeafile        = $outfileroot . "_ea.fits" ;
my $outarffile       = $outfileroot . ".arf" ;

#161110 Write an addtional output file
my $tmpevtfile       = "tmp_" . $outfileroot . "_evt.txt";
my $ftmpevt;  # File handle reference for $tmpevtfile
my $outevtfile       = $outfileroot . "_evt.fits";

my $doRegion         = ($regionradius >= 0);

# this will be initialized after we confirm which mode we're using
my $outputFile;

# put the object lists into arrays
my @objectHeightsArr    = split /[:,\s]+/, $objectheights ;
my @objectRadiusArr     = split /[:,\s]+/, $objectradii ;
my @imageRotAngleArr    = split /[:,\s]+/, $imagerotangles ;
my @xOffsetArr          = split /[:,\s]+/, $xoffsets ;
my @yOffsetArr          = split /[:,\s]+/, $yoffsets ;
my @cosImgRotAngleArr;
my @sinImgRotAngleArr;

foreach my $currAngle (@imageRotAngleArr) {
  push @cosImgRotAngleArr, cos(deg2rad($currAngle));
  push @sinImgRotAngleArr, sin(deg2rad($currAngle));
}

#region center x & y from input parameter regioncenter
my @regionCenterArr     = split /[:,\s]+/, $regioncenter ;
my $regx0;
my $regy0;

# see how many objects user wants
my $numObjects          = scalar @objectHeightsArr;
$numObjects             = 0 if uc($objecttype) eq 'NONE';


##################################################
#  Other Variables 
##################################################

my $status = 0;

# array that will hold the image files, from the inimagefile param
my @imageFiles;

# ENERG_LO and ENERG_HI columns from RMF file, and mid energy from those
my @rmf_energ_lo;
my @rmf_energ_hi;
my @rmf_energ_md;
my $numRMFEnergies = 0;
my $rmf_energ_lo_unit;
my $rmf_energ_hi_unit;

# energy units
my $energy_tunit_rmf;
my $energy_tunit_evt;

# name of extensions in history file
my $extension1 = "PHOTONHISTORY";
my $extension2 = "INPUTPHOTONS";

# name of extension in output EA file
my $eaextname   = "EFFECTIVEAREA";

# history file stores up to 8 individual path codes
my $maxPathCodes          = 8;
  
# Normalization radius in mm.  This may be requested as an input param later.
# This is written to the keyword NORMRAD in the output primary HDU
my $normRadmm             = 1e30;

# precision to which to compare floating point numbers
my $precision             = 16;

# array with input files
my @historyFiles;         
my $numHistoryFiles       = 0;

# how many files we will be using
my $numFilesToUse;
my @filesToUse;

# hash connecting each theta to the history file that has it
my %historyFileOfOffaxis = ();

# hash specifying which index each theta will have.  This is specific to the 
# history file with that theta, so that numInputPhotons can be accessed by 
# [fileIdx][offaxisIdx][azimIdx][energyIdx]
my %indexOfOffaxis      = ();

# total number of unique offaxis angles
my $numUnqOffaxis       = 0;

# these will be temp hashes for each file, to hold the values for that file
my %currOffaxisHash     = ();
my %currAzimHash        = ();
my %currEnergiesHash    = ();

# each history file needs to have same energy and azimuth grids
my %unqEnergieskevHash  = ();
my $numUnqEnergies      = 0;
my @currUnqEnergies     = ();
my %unqAzimDegHash      = ();
my $numUnqAzimuth       = 0;

# min and max of energy grid (currUnqEnergies) for easy reference later
my $minUnqEnergy        = 0;
my $maxUnqEnergy        = 0;

# hashes of azimuth and energy
my %indexOfAzim         = ();
my %indexOfEnergies     = ();

# to access numInputPhotons by iFile then iTheta, need to store each theta 
# for each file.
# thetaByFile[iFile][iTheta]
my %thetaByFile;

# variables for determining EA for a normal run (not vignetting)
my $theta_nIdx          = 0;
my $theta_np1Idx        = 0;
my $azim_nIdx           = 0;
my $azim_np1Idx         = 0;
my $theta_n             = 0;
my $theta_np1           = 0;
my $azim_n              = 0;
my $azim_np1            = 0;
my $theta_nFileIdx      = 0;
my $theta_np1FileIdx    = 0;
my $theta_nIdxInFile    = 0;
my $theta_np1IdxInFile  = 0;

# array of how many input photons for each file/offaxis/azimuth/energy
# This is direct input from the second extension of each history file
my @numInputPhotons;

# each history file needs to come from same TDF, and use same geom. area
my $initTDFN            = 0;
my $initGEOMAREAsqcm    = 0;

# number of photons input per energy/theta/roll
# (this was the input param numphoton to xrtraytrace)
my $initNUMPHOT         = 0;

# another keyword in input xrtraytrace history file
my $initFPMM2AM         = 0;

# booleans checked in the main code section (doWork)
my $isDesiredAngle      = 0;
my $isTheta_n           = 0;
my $isTheta_np1         = 0;
my $isPhi_n             = 0;
my $isPhi_np1           = 0;

# output from checkPathCode()
my $isDblRefl           = 0;
my $numEightInteractions= 0;



##################################################
#  Initializing 
##################################################

ahlog::ah_out "Start of tool arftable.pl" ;

# Pre-Processing
$status = ahapp::begin_processing();
unless ( $status == 0 ) {
  ahlog::ah_err "Begin processing - error" ;
  ahapp::end_processing(1);
}

# Write all parameters to the log file.
ahlog::ah_info "HIGH", ahapp::write_parameters () ;

# -----------------------
# check input params
# -----------------------

# regioncenter
if (scalar @regionCenterArr != 2) {
    ahlog::ah_err "Two numbers must be entered in regioncenter parameter, for x and y.  Exiting." ;
    ahapp::end_processing(1);
}
$regx0 = $regionCenterArr[0];
$regy0 = $regionCenterArr[1];


# -----------------------
# check input file(s)
# -----------------------

# get list of files from input file parameters
# if the param starts with '@' (for @filelist), make sure that file exists

# check the image file(s)
if (uc($objecttype) eq 'IMAGE') {
  @imageFiles = ahgen::get_file_list($inimagefile);
  foreach my $imagefile (@imageFiles) {
    if (! -e $imagefile) {
      ahlog::ah_err "File $imagefile does not exist. Exiting.";
      ahapp::end_processing(1);
    }
  }
}

# check history event file(s)
if ($inxrtevtfile =~ /^@/ and ! -e substr($inxrtevtfile,1)) {
  ahlog::ah_err "File $inxrtevtfile does not exist.";
  ahapp::end_processing(1);
}
@historyFiles = ahgen::get_file_list($inxrtevtfile);
$numHistoryFiles = @historyFiles;
foreach my $file (@historyFiles) {
  if (! -e $file) {
    ahlog::ah_err "File $file does not exist. Exiting.";
    ahapp::end_processing(1);
  }
}


# -----------------------
# confirm which mode
# -----------------------

# decide if we're doing 'normal' mode or 'merge' mode
# if user gives a list of history files, do the normal (interpolated) mode.
# if user said mergePtg=no, do normal mode.
# if user gave a single input history file AND said mergePtg=yes, then 
# combine all the angles.
if ( ($numHistoryFiles > 1) || (!$mergePtg) ) {
  $doMerge = 0;
} else {
  $doMerge = 1;
}

# if user wants vignetting file, ignore all other files
if ($doVignetting) {
  $doARF = 0;
  $doMerge = 0;
}

# now confirm that RMF file exists for creating the ARF file
if ($doARF) {
  if (! -e $inrmffile) {
    ahlog::ah_err "File $inrmffile does not exist. Exiting.";
    ahapp::end_processing(1);
  }
}

# now that we know which mode to use, set up the output file
$outputFile = $doVignetting ? $vignettingfile : $outeafile;

# -----------------------
# check output file(s)
# -----------------------

# Check if the output file already exists.  Unless clobber is set, this will
# cause the script to fail.
unless ($ahapp::clobber) {
  if  ( $doARF && (-e $outarffile) ) {
    ahlog::ah_err "Output ARF file $outarffile already exists but clobber was not set.  Exiting." ;
    ahapp::end_processing(1);
  }
  #161110
  if  (-e $outevtfile) {
    ahlog::ah_err "Output event file $outevtfile already exists but clobber was not set.  Exiting." ;
    ahapp::end_processing(1);
  }
  if  (-e $outputFile) {
    ahlog::ah_err "Output file $outputFile already exists but clobber was not set.  Exiting." ;
    ahapp::end_processing(1);
  }
}

# If output files already exist, delete them.  We've already checked clobber
unlink $outputFile if -e $outputFile ;
unlink $outarffile if ( $doARF && (-e $outarffile) );
if ($writeevtfile) {
  unlink $outevtfile if -e $outevtfile;  #161110
}

if ($writeevtfile) {
  ahapp::add_temp_file($tmpevtfile);
  open ($ftmpevt, ">", $tmpevtfile);
}

# -----------------------
# Print out a summary header message
# -----------------------

ahlog::ah_out "----------------------";

# statement about what files will be created
if ($doVignetting) {
  ahlog::ah_out "A vignetting file requested (EA or ARF files will not be created).";
} elsif ($doARF) {
  ahlog::ah_out "An EA file and an ARF file will be created.";
  ahlog::ah_out "The effective area will be calculated for the ARF using the energy grid from the RMF file.";
} else {
  ahlog::ah_out "An EA file will be created.";
  ahlog::ah_out "No RMF file was input so no ARF will be created.";
}

if ($writeevtfile) {
  ahlog::ah_out "An output event file will be created.";
}

# statement about the angles
if ($doMerge) {
  ahlog::ah_out "All offaxis and azimuthal angles will contribute to the effective area.";
}
if ( !$doMerge && !$doVignetting ) {
  ahlog::ah_out "An effective area will be calculated by interpolating the target offaxis ($tgtOffaxis) and target azimuth ($tgtAzimuth)";
}

# statement about objects
if ($numObjects > 0) {
  ahlog::ah_out "Number of objects placed in optical path: $numObjects";
  ahlog::ah_out "Type of object: $objecttype";
}

# statement about region selection
if ( (!$doMerge) && ($doRegion) ) {
  # the user entered a region, and may have forgotten that in non-merge mode,
  # region selection doesn't matter
  ahlog::ah_out "Region selection is disabled in this mode." ;
} elsif ($doRegion) {
  ahlog::ah_out "Selecting on region centered at x=$regx0 y=$regy0, radius=$regionradius" ;
} else {
  ahlog::ah_out "No region selection.";
}

ahlog::ah_out "----------------------";

# -----------------------
# get RMF energy grids
# -----------------------

# get the ENERG_LO and ENERG_HI columns from RMF file,
# and set up the central energy array
if ($doARF) {

  getRMFEnergyGrid(\@rmf_energ_lo, \@rmf_energ_hi, \$rmf_energ_lo_unit, \$rmf_energ_hi_unit);
  $numRMFEnergies = scalar @rmf_energ_lo;
  
  # store the TUNIT in another var, to match the evt file
  $energy_tunit_rmf = $rmf_energ_lo_unit;
  
  if ( (lc($energy_tunit_rmf) ne "kev") && (lc($energy_tunit_rmf) ne "ev") ) {
    ahlog::ah_err "RMF file energies must be in units of keV or eV";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }

  for (my $iEnergy = 0 ; $iEnergy < $numRMFEnergies ; ++$iEnergy) {
    $rmf_energ_md[$iEnergy] = 0.5 * ($rmf_energ_lo[$iEnergy] + $rmf_energ_hi[$iEnergy])
  }
  
  if (ahlog::getdebug()) {
    print Data::Dumper->Dump( [ \@rmf_energ_lo ], [ qw(*rmf_energ_lo) ] );
    print Data::Dumper->Dump( [ \@rmf_energ_md ], [ qw(*rmf_energ_md) ] );
    print Data::Dumper->Dump( [ \@rmf_energ_hi ], [ qw(*rmf_energ_hi) ] );
  }
  
}


# -----------------------
# get data from history files
# -----------------------

# now that we know each input file exists, get the data
my $iFile = 0;
foreach my $file (@historyFiles) {

  ahlog::ah_info "HIGH", "Initial processing of input history event file: $file";
  
  # create a copy of the history file with ftcopy
  my $tmpHistory = $file . ".tmp";
  # add to temp files, to delete later
  ahapp::add_temp_file($tmpHistory);
  
  # add the ! to clobber the temp outfile
  $status = ahgen::run_ftool("ftcopy",
                             "infile=$file",
                             "outfile=!$tmpHistory",
                             "copyall=YES",
                             "clobber=yes");
  
  # Check that photons were not originally from a file.  If they were from a 
  # file, there could have been multiple sources, and we don't want that. 
  my $srcmdl = ahgen::get_keyword($tmpHistory, $extension1, "SRCMDL");
  if ( (uc $srcmdl eq "PHOTONLIST") || (uc $srcmdl eq "GROUNDMODEL")) {
    ahlog::ah_err "History file $file must be made from photons not from a file.  When running xrtraytrace, use 'source' of point, flatcircle, or betamodel.";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }

  # make sure that the units of energy are the same in the 1st and 2nd extension
  my $energy_tunit_ext1 = ahgen::get_keyword($tmpHistory, $extension1, "TUNIT1");
  my $energy_tunit_ext2 = ahgen::get_keyword($tmpHistory, $extension2, "TUNIT3");
  if (lc $energy_tunit_ext1 ne lc $energy_tunit_ext2) {
    ahlog::ah_err "History file $file must use the same units for energy in both extensions.";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }
  
  # +++ 20160826 KLR there should be a general perl function to return, for example, the TUNIT of a column based on the column name, not the column number.  I happen to know that energy is column 1 and 3 in extensions 1 and respectively, but what if that changed.

  # Find the number of rows in second extension
  my $numRows = ahgen::get_keyword($tmpHistory, $extension2, "NAXIS2") ;

  # current number of unique offaxis theta angles, azimuthal (roll) phi angles, and energies
  my $currNumUnqOffaxis   = ahgen::get_keyword($tmpHistory, $extension2, "NOFFAXIS");
  my $currNumUnqAzimuth   = ahgen::get_keyword($tmpHistory, $extension2, "NAZIMUTH");
  my $currNumUnqEnergies  = ahgen::get_keyword($tmpHistory, $extension2, "NUMENRG");

  ahlog::ah_debug "NOFFAXIS=$currNumUnqOffaxis NAZIMUTH=$currNumUnqAzimuth NUMENRG=$currNumUnqEnergies" ;

  # grab each column into an array
  my @history2InitialTheta    = ahgen::read_column($tmpHistory, $extension2, "INITIALTHETA");
  my @history2InitialAzimDir  = ahgen::read_column($tmpHistory, $extension2, "INITIALAZIMDIR");
  my @history2Energy          = ahgen::read_column($tmpHistory, $extension2, "ENERGY");
  my @history2NumPhotons      = ahgen::read_column($tmpHistory, $extension2, "NUMPHOTONS");

  # clear the current hashes, so we can refill it with data from this file
  %currOffaxisHash    = ();
  %currAzimHash       = ();
  %currEnergiesHash   = ();

  # add the theta values to historyFileOfOffaxis hash, to record that they came 
  # from this history file (file index).  This will overwrite any duplicates, so 
  # we only have unique thetas as keys to the hash.
  for (my $iRow = 0 ; $iRow < $numRows ; $iRow++) {

    my $currOffaxis     = $history2InitialTheta[$iRow];
    my $currAzim        = $history2InitialAzimDir[$iRow];
    my $currEnergy      = $history2Energy[$iRow];
  
    $currOffaxisHash  { $currOffaxis }    = $iFile; 
    $currAzimHash     { $currAzim }       = $iFile; 
    $currEnergiesHash { $currEnergy }     = $iFile;
    
    # ahlog::ah_debug "$history2InitialTheta[$iRow] $history2InitialAzimDir[$iRow] $history2Energy[$iRow] $history2NumPhotons[$iRow] \n";

  } # end-loop through history file 2nd ext rows

  # create a sorted array of all the unique offaxis theta angles in this file
  #my @currUnqOffaxisArcmin = keys %currOffaxisHash;
  #@currUnqOffaxisArcmin= sort @currUnqOffaxisArcmin;
  my @currUnqOffaxisArcmin;
  foreach my $key (sort { $a <=> $b} keys %currOffaxisHash) {
    push @currUnqOffaxisArcmin, $key ;
  }
  
  # create a sorted array of all the unique azimuthal angles in this file
  my @currUnqAzimDeg;
  foreach my $key (sort { $a <=> $b} keys %currAzimHash) {
    push @currUnqAzimDeg, $key ;
  }
  
  # create a sorted array of all the unique energies in this file
  foreach my $key (sort { $a <=> $b} keys %currEnergiesHash) {
    push @currUnqEnergies, $key ;
  }
  # store the min and max for easy reference later
  $numUnqEnergies = $currNumUnqEnergies;
  $minUnqEnergy   = $currUnqEnergies[0];
  $maxUnqEnergy   = $currUnqEnergies[$numUnqEnergies-1];
  
  if (ahlog::getdebug()) {
    print Data::Dumper->Dump( [ \@currUnqOffaxisArcmin ], [ qw(*currUnqOffaxisArcmin) ] );
    print Data::Dumper->Dump( [ \@currUnqAzimDeg ], [ qw(*currUnqAzimDeg) ] );
    print Data::Dumper->Dump( [ \@currUnqEnergies ], [ qw(*currUnqEnergies) ] );
  }

  # make sure that multiple files don't have the same offaxis angles
  if ($iFile > 0) {
    for (my $iOff = 0 ; $iOff < $currNumUnqOffaxis ; ++$iOff) {
      if ( exists($historyFileOfOffaxis{$currUnqOffaxisArcmin[$iOff]}) ) {
        ahlog::ah_err "A single offaxis angle must be present in one history file only.  Multiple history files cannot contain the same offaxis angle.  Offaxis angle $currUnqOffaxisArcmin[$iOff] is in multiple files";
        ahlog::ah_err ahgen::get_tool_stderr;
        ahapp::end_processing(1);
      }
    } #end-loop through unique thetas in this file
  } # end-if checking thetas in multiple files

  # now that we've checked that the current theta values aren't in 
  # historyFileOfOffaxis yet, add them
  for (my $iOff = 0 ; $iOff < $currNumUnqOffaxis ; ++$iOff) {
    $historyFileOfOffaxis{$currUnqOffaxisArcmin[$iOff]} = $iFile;
  }
  # +++ or? @historyFileOfOffaxis{ keys %currOffaxis } = @currOffaxis{ keys %currOffaxis };

  # make sure all files have the same azimuthal and energy grids, and 
  # geometric area and TDF
  if ($iFile == 0) {
    # we're at the first history file.  Store the grids.

    # get the TUNIT of the energy (hopefully which is column 1)
    $energy_tunit_evt = ahgen::get_keyword($file, $extension1, "TUNIT1");
    if ( !$energy_tunit_evt ) {
      ahlog::ah_err "The TUNIT keyword for the energy (first) column must be defined.";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
    ahlog::ah_debug "TUNIT1 = $energy_tunit_evt";
      
    if ( (lc($energy_tunit_evt) ne "kev") && (lc($energy_tunit_evt) ne "ev") ) {
      ahlog::ah_err "History file energies must be in units of keV or eV";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
    
    %unqEnergieskevHash = %currEnergiesHash;

    $numUnqAzimuth = $currNumUnqAzimuth;
    %unqAzimDegHash = %currAzimHash;

    # store the index of the azimuth and energy
    # the indices for azimuth and energy need only be stored at the first file, 
    # since every file should have the same grids.
    # these will be used for access to numInputPhotons
    for (my $iAzim = 0 ; $iAzim < $currNumUnqAzimuth ; ++$iAzim) {
      $indexOfAzim{$currUnqAzimDeg[$iAzim]} = $iAzim;
    } 
    for (my $iEnergy = 0 ; $iEnergy < $currNumUnqEnergies ; ++$iEnergy) {
      $indexOfEnergies{$currUnqEnergies[$iEnergy]} = $iEnergy;
    }
    
    # will need to make sure each history file has the same aperture geometric 
    # area and used the same TDF.  Check those keywords later as loop through 
    # history files, comparing against the keywords in the first file
    $initGEOMAREAsqcm  = ahgen::get_keyword($file, $extension1, "GEOMAREA");
    $initTDFN          = ahgen::get_keyword($file, $extension1, "TDFN");
    
    # for doMerge mode (in which we only accept one history file)
    # we need the number of photons input per energy/theta/roll
    # (this was the input param numphoton to xrtraytrace)
    $initNUMPHOT       = ahgen::get_keyword($file, $extension1, "NUMPHOT");
    
    # needed for calculating object obstruction later
    $initFPMM2AM       = ahgen::get_keyword($file, $extension1, "FPMM2AM");
    
  } else {
    # we're at the next history file.  make sure its grids match the first

    # make sure azimuth hashes have the same number of keys
    if ( keys(%currAzimHash) != keys(%unqAzimDegHash) ) {
      ahlog::ah_err "The same azimuthal grid must be used for each file.";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
    for (my $iAzim = 0 ; $iAzim < $currNumUnqAzimuth ; ++$iAzim) {
      if ( !exists($unqAzimDegHash{$currUnqAzimDeg[$iAzim]}) ) {
        ahlog::ah_err "The same azimuthal grid must be used for each file.";
        ahlog::ah_err ahgen::get_tool_stderr;
        ahapp::end_processing(1);
      }
    } #end-loop through unique azimuthal angles in this file

    # make sure energy hashes have the same number of keys
    if ( keys(%currEnergiesHash) != keys(%unqEnergieskevHash) ) {
      ahlog::ah_err "The same energy grid must be used for each file.";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
    for (my $iEnergy = 0 ; $iEnergy < $currNumUnqEnergies ; ++$iEnergy) {
      if ( !exists($unqEnergieskevHash{$currUnqEnergies[$iEnergy]}) ) {
        ahlog::ah_err "The same energy grid must be used for each file.";
        ahlog::ah_err ahgen::get_tool_stderr;
        ahapp::end_processing(1);
      }
    } #end-loop through unique energies in this file
    
    # make sure every file has the same geometric area as the first one
    my $currGeomArea = ahgen::get_keyword($file, $extension1, "GEOMAREA");
    if ($currGeomArea != $initGEOMAREAsqcm) {
      ahlog::ah_err "All input history files must have been created using the same aperture (GEOMAREA keywords must all match).";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
    
    # make sure every file used the same TDF as the first one we found
    my $currTDFN = ahgen::get_keyword($file, $extension1, "TDFN");
    if ($currTDFN ne $initTDFN) {
      ahlog::ah_err "All input history files must have been created using the same telescope data files (TDFN keywords must all match).";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }

  } # end-if checking grids in multiple files

  # +++ 20150303 Should this be inside the history file loop?
  # we need to store the index of the offaxis in each file, for access to 
  # numInputPhotons
  for (my $iOff = 0 ; $iOff < $currNumUnqOffaxis ; ++$iOff) {
    $indexOfOffaxis{$currUnqOffaxisArcmin[$iOff]} = $iOff;
  } 

  # store how many photons at each theta/roll/energy
  for (my $iRow = 0 ; $iRow < $numRows ; $iRow++) {

    my $currOffaxis     = $history2InitialTheta[$iRow];
    my $currAzim        = $history2InitialAzimDir[$iRow];
    my $currEnergy      = $history2Energy[$iRow];
    my $currNumPhotons  = $history2NumPhotons[$iRow];
  
    my $currOffaxisIdx  = $indexOfOffaxis{$currOffaxis};
    my $currAzimIdx     = $indexOfAzim{$currAzim};
    my $currEnergyIdx   = $indexOfEnergies{$currEnergy};
  
    # store how many input photons for this file/offaxis/azimuth/energy
    $numInputPhotons[$iFile][$currOffaxisIdx][$currAzimIdx][$currEnergyIdx] = $currNumPhotons;
    
#     $thetaByFile[$iFile][$currOffaxisIdx] = $currOffaxis;
    # also store which index this particular theta is in this particular file,
    # to access numInputPhotons later.  (duplicates will be overwritten)
    $thetaByFile{$currOffaxis} = $currOffaxisIdx;
    
#     ahlog::ah_debug "numInputPhotons[$iFile][$currOffaxis][$currAzim][$currEnergy] \t = [$iFile][$currOffaxisIdx][$currAzimIdx][$currEnergyIdx] \t = $numInputPhotons[$iFile][$currOffaxisIdx][$currAzimIdx][$currEnergyIdx] \n";
    # +++ test this by manually changing some numPhotons values to make sure matrix comes out right
  
  } # end-loop through rows

  # increment which file index we're at
  $iFile++;
  
} # end-loop through all input history files

ahlog::ah_info "LOW", "Finished reading all the input history files, second extensions";

# create sorted array of unique offaxis theta angles
my @allUnqOffaxis;
foreach my $key (sort { $a <=> $b} keys %historyFileOfOffaxis) {
  push @allUnqOffaxis, $key ;
}
$numUnqOffaxis = keys(%historyFileOfOffaxis);

# create a hash of all the unique thetas, to store their index within allUnqOffaxis
my %indexOfAllUnqOffaxisHash;
for (my $iOff = 0 ; $iOff < $numUnqOffaxis ; ++$iOff) {
  $indexOfAllUnqOffaxisHash{$allUnqOffaxis[$iOff]} = $iOff;
}

# create sorted array of unique azimuth angles
my @allUnqAzimuth;
foreach my $key (sort { $a <=> $b} keys %unqAzimDegHash) {
  push @allUnqAzimuth, $key ;
}

# create a hash of all the unique azim, to store their index within allUnqAzimuth
my %indexOfAllUnqAzimuthHash;
for (my $iAzim = 0 ; $iAzim < $numUnqAzimuth ; ++$iAzim) {
  $indexOfAllUnqAzimuthHash{$allUnqAzimuth[$iAzim]} = $iAzim;
}

# Print info about files
ahlog::ah_info "LOW", "Geometric area (cm^2) = $initGEOMAREAsqcm";
ahlog::ah_info "LOW", "Energy Grid:";
for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) {
  ahlog::ah_info "LOW", "  $currUnqEnergies[$iEnergy]";
}
ahlog::ah_info "LOW", "Azimuthal Grid:";
for (my $iAzim = 0 ; $iAzim < $numUnqAzimuth ; ++$iAzim) {
  ahlog::ah_info "LOW", "  $allUnqAzimuth[$iAzim]";
}
ahlog::ah_info "LOW", "Offaxis Grid:";
for (my $iOff = 0 ; $iOff < $numUnqOffaxis ; ++$iOff) {
  ahlog::ah_info "LOW", "  $allUnqOffaxis[$iOff]";
}

# if there was only one input angle (offaxis or azimuth), and if it does not 
# equal the target angle, the code cannot complete
# this is not applicable for vignetting
if (!$doVignetting && !$doMerge) {
  if ( ($numUnqOffaxis == 1) && ($allUnqOffaxis[0] != $tgtOffaxis) ) {
      ahlog::ah_err "There is only one unique offaxis angle in the input file ($allUnqOffaxis[0]).  This must match the target offaxis angle ($tgtOffaxis).  Exiting." ;
      ahapp::end_processing(1);
  }
  if ( ($numUnqAzimuth == 1) && ($allUnqAzimuth[0] != $tgtAzimuth) ) {
      ahlog::ah_err "There is only one unique azimuthal angle in the input file ($allUnqAzimuth[0]).  This must match the target offaxis angle ($tgtAzimuth).  Exiting." ;
      ahapp::end_processing(1);
  }
}

# if we're doing vignetting, then we need all the input files
if ($doVignetting) {

  @filesToUse = @historyFiles;

} elsif ($doMerge) {

  # if we're combining all the angles into one EA.  We should only have one 
  # input history file for this case.
  @filesToUse = @historyFiles;
  
} else {

  # Determine the four surrounding theta/phi values, where theta_0 and phi_0 are 
  # the target offaxis and roll, and n is the index for the preceding values, 
  # and n+1 is the index for the values after. 
  # These are the indices we'll need for the surrounding offaxis and azimuth 
  # angles.  The offaxis is respective to the history file in which it's found.
  # These will be needed for access to numInputPhotons
  #     if phi_0 == target, then phi_n = phi_0 = phi_n+1 (same for theta)
  #     phi_n <= phi_0 <= phi_n+1
  #     theta_n <= theta_0 <= theta_n+1
  ($theta_nIdx, $theta_np1Idx) = bisectionLocate(\@allUnqOffaxis, $numUnqOffaxis, $tgtOffaxis);
  ($azim_nIdx, $azim_np1Idx) = bisectionLocate(\@allUnqAzimuth, $numUnqAzimuth, $tgtAzimuth);

  # NOTE: If the target offaxis (or azimuth) is exactly on the grid of input 
  # offaxis, we don't technically need to call bisectionLocate or interpolate. 
  # Doing it this way will make ea1 and ea2 (for example) equal, and make 
  # interpolate inefficient. But it will make code ready to distribute quicker.

  ahlog::ah_debug "theta_nIdx=$theta_nIdx theta_np1Idx=$theta_np1Idx \n";
  ahlog::ah_debug "azim_nIdx=$azim_nIdx azim_np1Idx=$azim_np1Idx \n";

  # store the offaxis and azimuthal angles we will need
  $theta_n     = $allUnqOffaxis[$theta_nIdx]; 
  $theta_np1   = $allUnqOffaxis[$theta_np1Idx];
  $azim_n      = $allUnqAzimuth[$azim_nIdx];
  $azim_np1    = $allUnqAzimuth[$azim_np1Idx];

  # note which file(s) we will need to read based on the target offaxis and roll.
  # even if both thetas are in the same file, this will put that same filename
  # into both entries
  $theta_nFileIdx = $historyFileOfOffaxis{$theta_n};
  $theta_np1FileIdx = $historyFileOfOffaxis{$theta_np1};
  $filesToUse[0] = $historyFiles[$theta_nFileIdx];
  if ($theta_nFileIdx != $theta_np1FileIdx) {
    $filesToUse[1] = $historyFiles[$theta_np1FileIdx];
  }

  # now that we know the theta indices, and which file each comes from, we need
  # to get the theta index particular to that file, so that numInputPhotons
  # can be accessed by file index then by theta index
  $theta_nIdxInFile    = $thetaByFile{$theta_n};
  $theta_np1IdxInFile  = $thetaByFile{$theta_np1};

  ahlog::ah_debug "theta_n   = $theta_n (index $theta_nIdx) (theta_nFileIdx=$theta_nFileIdx, theta_nIdxInFile=$theta_nIdxInFile) \n";
  ahlog::ah_debug "theta_np1 = $theta_np1 (index $theta_np1Idx) (theta_np1FileIdx=$theta_np1FileIdx, theta_np1IdxInFile=$theta_np1IdxInFile) \n";
  ahlog::ah_debug "azim_n    = $azim_n (index $azim_nIdx) \n";
  ahlog::ah_debug "azim_np1  = $azim_np1 (index $azim_np1Idx) \n";

} # end-if vignetting

$numFilesToUse = scalar @filesToUse;

ahlog::ah_info "HIGH", "Number of files to use = $numFilesToUse";
ahlog::ah_info "HIGH", "Using history file(s): @filesToUse";

# grab other keywords from the first history file, to write to output later
my $telescop          = ahgen::get_keyword($filesToUse[0], $extension1, "TELESCOP");
my $instrume          = ahgen::get_keyword($filesToUse[0], $extension1, "INSTRUME");
my $detnam            = ahgen::get_keyword($filesToUse[0], $extension1, "DETNAM");
my $cvsd0001          = ahgen::get_keyword($filesToUse[0], $extension1, "CVSD0001");
my $cvst0001          = ahgen::get_keyword($filesToUse[0], $extension1, "CVST0001");

# set up effective area (EA) arrays for the four surrounding theta/phi 
# combinations, the interpolated EA, and the final EA.  initialize to 0
# EA_1(E) for theta_n, phi_n
# EA_2(E) for theta_n, phi_n+1
# EA_3(E) for theta_n+1, phi_n
# EA_4(E) for theta_n+1, phi_n+1
my @ea1                 = (0) x $numUnqEnergies;
my @ea2                 = (0) x $numUnqEnergies; 
my @ea3                 = (0) x $numUnqEnergies; 
my @ea4                 = (0) x $numUnqEnergies;
my @ea_phi_0_theta_n    = (0) x $numUnqEnergies;
my @ea_phi_0_theta_np1  = (0) x $numUnqEnergies;
my @eaFinal             = (0) x $numUnqEnergies;

# the EA for when we combine all angles (if user entered mergePtg=yes)
my @eaMerge             = (0) x $numUnqEnergies;

# these are for vignetting, if that is requested
my @eaVignette          = ();

# This will hold the final vignette ea ratio.  We can't write over the 
# original, becuase then the values for theta=0 will be written as 1 after 
# it divides by itself.
my @eaVignetteFinal = ();

# now that we know number of unique offaxis, azimuth, energy: initialize arrays
# (numOffaxis, numAzim, numUnqEnergies)
for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
  for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) {
    for (my $iAzim = 0 ; $iAzim < $numUnqAzimuth; ++$iAzim) {
      $eaVignette[$iTheta][$iAzim][$iEnergy] = 0;
    }
    #$eaVigSumAzim[$iTheta][$iEnergy] = 0;
  }
}

# now that we've initizalized
my $foundZero = 0;
if ($doVignetting) {
  ahlog::ah_info "LOW", "Checking that theta=0 is one of the inputs";
  # make sure that theta=0 is one of the inputs
  for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
    if (equal($allUnqOffaxis[$iTheta], 0, $precision)) {
      $foundZero = 1;
      last;
    }
  }
  # for vignetting, we need more than one theta, one of which must be 0
  if (!$foundZero || ($numUnqOffaxis == 1)) {
    ahlog::ah_err "Vignetting file not created: One or more unique off-axis angle input is required in addition to on-axis data.";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
    
  }
}




# ----------------------------------------------
# If we have objects in image files, read them
# ----------------------------------------------

# declare up the arrays we will be using

# these are 1D, size=numObjects
my @imgNAXIS1 = ((0) x $numObjects);
my @imgCRPIX1 = ((0) x $numObjects);
my @imgCRVAL1 = ((0) x $numObjects);
my @imgCDELT1 = ((0) x $numObjects);
my @imgNAXIS2 = ((0) x $numObjects);
my @imgCRPIX2 = ((0) x $numObjects);
my @imgCRVAL2 = ((0) x $numObjects);
my @imgCDELT2 = ((0) x $numObjects);

# x and y boundary arrays
# these are 2D, size = numObjects by imgNAXIS1/2[each image]
my @imgxlo;
my @imgxhi;
my @imgylo;
my @imgyhi;

# the large array that will hold each image from the file
# this is 1D, size = numObjects by imgNAXIS1[each image] by imgNAXIS2[each image]
my @allImages;

# now read the files, if provided
if (uc($objecttype) eq 'IMAGE') {
  for (my $iObject = 0 ; $iObject < $numObjects ; ++$iObject) {
          
    # open this image file, and get some keywords
    my $currImgFile = $imageFiles[$iObject];
            
    #ahlog::ah_info "HIGH", "Opening image file $currImgFile.";

    $imgNAXIS1[$iObject] = ahgen::get_keyword($currImgFile, 0, "NAXIS1");
    $imgCRPIX1[$iObject] = ahgen::get_keyword($currImgFile, 0, "CRPIX1");
    $imgCRVAL1[$iObject] = ahgen::get_keyword($currImgFile, 0, "CRVAL1");
    $imgCDELT1[$iObject] = ahgen::get_keyword($currImgFile, 0, "CDELT1");
    $imgNAXIS2[$iObject] = ahgen::get_keyword($currImgFile, 0, "NAXIS2");
    $imgCRPIX2[$iObject] = ahgen::get_keyword($currImgFile, 0, "CRPIX2");
    $imgCRVAL2[$iObject] = ahgen::get_keyword($currImgFile, 0, "CRVAL2");
    $imgCDELT2[$iObject] = ahgen::get_keyword($currImgFile, 0, "CDELT2");
    
    # Check that these keywords were present
    if ( !$imgNAXIS1[$iObject] || !$imgCRPIX1[$iObject] || !$imgCRVAL1[$iObject] || !$imgCDELT1[$iObject] ||
         !$imgNAXIS2[$iObject] || !$imgCRPIX2[$iObject] || !$imgCRVAL2[$iObject] || !$imgCDELT2[$iObject] ) {
      ahlog::ah_err "Error reading keywords from $currImgFile";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
  
    #set up x and y boundary arrays
#     $imgxlo[$iObject] = ((0) x $imgNAXIS1[$iObject]);
#     $imgxhi[$iObject] = ((0) x $imgNAXIS1[$iObject]);
#     $imgylo[$iObject] = ((0) x $imgNAXIS2[$iObject]);
#     $imgyhi[$iObject] = ((0) x $imgNAXIS2[$iObject]);
    for (my $iX = 0 ; $iX < $imgNAXIS1[$iObject] ; ++$iX) {
        $imgxlo[$iObject][$iX] = (($iX+0.5 - $imgCRPIX1[$iObject]) * $imgCDELT1[$iObject]) + $imgCRVAL1[$iObject];
        $imgxhi[$iObject][$iX] = (($iX+1.5 - $imgCRPIX1[$iObject]) * $imgCDELT1[$iObject]) + $imgCRVAL1[$iObject];
    }
    for (my $iY = 0 ; $iY < $imgNAXIS2[$iObject] ; ++$iY) {
        $imgylo[$iObject][$iY] = (($iY+0.5 - $imgCRPIX2[$iObject]) * $imgCDELT2[$iObject]) + $imgCRVAL2[$iObject];
        $imgyhi[$iObject][$iY] = (($iY+1.5 - $imgCRPIX2[$iObject]) * $imgCDELT2[$iObject]) + $imgCRVAL2[$iObject];
    }
  
    # read image into array image array (naxis1,naxis2);
    # NAXIS1 = x = columns
    # NAXIS2 = y = rows
    my @currImage = ahgen::read_image($currImgFile, 0);
    
    $allImages[$iObject] = [ @currImage ];

  } # end-loop through objects
} # end-if IMAGE



##################################################
#  Main Code Block 
##################################################

# read files we decided to keep based on which file has which theta and roll
foreach my $file (@filesToUse) {

  ahlog::ah_info "HIGH", "Reading input file: $file";
  
  # we copied all the history files earlier, now just grab the name.
  my $tmpHistory = $file . ".tmp";
  
  # find out if history file contains only photons that hit the results plane
  my $resultsPlaneOnly = ahgen::get_keyword($tmpHistory, $extension1, "RSLPONLY");
	
  # if the history file contained only photons that hit the results plane, 
  # then we don't need to check for the resultsplane column.
  my $checkForResultsPlane = ( lc($resultsPlaneOnly) eq "no" ) ;

  # store how many total photons (rows) in current history file
  my $numPhotons = ahgen::get_keyword($tmpHistory, $extension1, "NAXIS2");

  #RSH161129
  my $numChunks = $numPhotons/$CHUNKSIZE;
  if ($numChunks*$CHUNKSIZE < $numPhotons) { $numChunks++; }

  my $fileRow = 0;  # Row in whole file; $iRow is row in chunk
  my $numSurvived = 0;  # Number of surviving photons (number written to output evt file)
  for (my $iChunk=0; $iChunk<$numChunks; $iChunk++) {

  #RSH161129
  my $chunkFirst = $iChunk*$CHUNKSIZE + 1;  # one-based
  my $chunkLast = $chunkFirst + $CHUNKSIZE - 1;
  if ($chunkLast > $numPhotons) { $chunkLast = $numPhotons; }
  my $numPhChunk = $chunkLast - $chunkFirst + 1;

  ahlog::ah_info "HIGH", "Reading chunk number $iChunk; " .
    "first row = $chunkFirst; last row = $chunkLast";

  # store the following history file columns in local arrays  RSH161129 chunk row range added
  my @historyEnergy               = ahgen::read_column($tmpHistory, $extension1, "ENERGY", $chunkFirst, $chunkLast);
  my @historyInitialThetaArcmin   = ahgen::read_column($tmpHistory, $extension1, "INITIALTHETA", $chunkFirst, $chunkLast);
  my @historyInitialAzimDirDeg    = ahgen::read_column($tmpHistory, $extension1, "INITIALAZIMDIR", $chunkFirst, $chunkLast);
  my @historyFINALXPOS            = ahgen::read_column($tmpHistory, $extension1, "FINALXPOS", $chunkFirst, $chunkLast);
  my @historyFINALYPOS            = ahgen::read_column($tmpHistory, $extension1, "FINALYPOS", $chunkFirst, $chunkLast);
  my @historyFINALXDIR            = ahgen::read_column($tmpHistory, $extension1, "FINALXDIR", $chunkFirst, $chunkLast);
  my @historyFINALYDIR            = ahgen::read_column($tmpHistory, $extension1, "FINALYDIR", $chunkFirst, $chunkLast);
  my @historyFINALZDIR            = ahgen::read_column($tmpHistory, $extension1, "FINALZDIR", $chunkFirst, $chunkLast);
  my @historyPathcode             = ahgen::read_column($tmpHistory, $extension1, "PATHCODE", $chunkFirst, $chunkLast);
  my @historyResultsPlane;
  if ($checkForResultsPlane) {
    @historyResultsPlane          = ahgen::read_column($tmpHistory, $extension1, "RESULTSPLANE");
  } else {
    # else, everything in file did hit results plane. initialize all true
    @historyResultsPlane = ((1) x $numPhotons);
  }

  ahlog::ah_out "Processing $numPhotons raytrace events." ;

  #RSH161129 loop through rows of chunk
  # loop through rows of current history file
  #for (my $iRow = 0 ; $iRow < $numPhotons ; $iRow++) {
  for (my $iRow = 0 ; $iRow < $numPhChunk ; $fileRow++, $iRow++) {
    
    #ahlog::ah_debug " **** iRow:$iRow theta:$historyInitialThetaArcmin[$iRow] roll:$historyInitialAzimDirDeg[$iRow] pathcode:$historyPathcode[$iRow] resultsplane:$historyResultsPlane[$iRow] \n";
    
    # if we're on a normal run (not vignetting, and not merging all angles), 
    # check for which angle we're at
    if ( (!$doVignetting) && (!$doMerge) ) {
    
      # reset these booleans to 0 before we go into checkForAzimOffaxis
      $isTheta_n = 0;
      $isTheta_np1 = 0;
      $isPhi_n = 0;
      $isPhi_np1 = 0;
    
      # only keep history file rows with the desired phi and theta values
      ($isDesiredAngle, $isTheta_n, $isTheta_np1, $isPhi_n, $isPhi_np1) = 
          checkForAzimOffaxis($historyInitialThetaArcmin[$iRow], $historyInitialAzimDirDeg[$iRow]);
      if (!$isDesiredAngle) {
        next;
      }
      
    }
    
    # Check if photon is inside the circular selection region
    if ( ($doMerge) && ($doRegion) ) {
      my $xdist = $initFPMM2AM * $historyFINALXPOS[$iRow] - $regx0;
      my $ydist = $initFPMM2AM * $historyFINALYPOS[$iRow] - $regy0;
      my $eventrad = sqrt( ($xdist**2) + ($ydist**2) );
      if ($eventrad > $regionradius) {
        # skip to next raytracing file row because event is rejected
        next; 
      }
    }
    
    # for now, set the photon as having survived the objects
    my $survivedObjects = 1;
    
    # See if the current photon hit the results plane and was a double 
    # reflection.  If it wasn't, move on to the next photon.
    ($isDblRefl, $numEightInteractions) = checkPathCode($historyPathcode[$iRow], $historyResultsPlane[$iRow]);
    if (!$isDblRefl) {
      next;
    }
    
    # now loop over objects to see if this photon would have survived them
    for (my $iObject = 0 ; $iObject < $numObjects ; ++$iObject) {
    
      #ahlog::ah_info "HIGH", "Checking if object $iObject was struck.";
      
      if ($survivedObjects) {
        
        # get new x,y intercept at height of this object
        # note that we should not get a "divide by zero" error here.  If 
        # FINALZDIR was 0, that should have been caught in checkPathCode, 
        # by checking the RESULTSPLANE column.  If FINALZDIR, then the photon 
        # obviously did not reach the results plane, and xrtraytrace should
        # have marked that as false.
        my $xnew = $historyFINALXPOS[$iRow] + ( ($historyFINALXDIR[$iRow]/$historyFINALZDIR[$iRow]) * $objectHeightsArr[$iObject] );
        my $ynew = $historyFINALYPOS[$iRow] + ( ($historyFINALYDIR[$iRow]/$historyFINALZDIR[$iRow]) * $objectHeightsArr[$iObject] );
        
        if (uc($objecttype) eq 'CIRCLE') {
        
          # radius relative to center of offset circle
          my $rad = sqrt( ($xnew-$xOffsetArr[$iObject])**2 + ($ynew-$yOffsetArr[$iObject])**2);
          if ($rad > $objectRadiusArr[$iObject]) {
            $survivedObjects=0;
          }
          
        } elsif (uc($objecttype) eq 'IMAGE') {
          
          # shift and rotate intercept position; note that the angles are 
          # negated because in the code the photon is rotated not the object
          my $xnewtrans = $xOffsetArr[$iObject] + ( ($xnew * $cosImgRotAngleArr[$iObject]) + ($ynew * $sinImgRotAngleArr[$iObject]) );
          my $ynewtrans = $yOffsetArr[$iObject] + ( ($ynew * $cosImgRotAngleArr[$iObject]) - ($xnew * $sinImgRotAngleArr[$iObject]) );
          
          # find the x and y indices for the image pixel corresponding to 
          # xnewtrans and ynewtrans (if either index ends up negative the 
          # event lies off the image area and will be interpreted as being 
          # blocked by the object)  
          # if we found matches, test whether the pixel at jx,jy is 
          # transparent or not after these loops
          my $jx = -1;
          for (my $iX = 0 ; $iX < $imgNAXIS1[$iObject] ; ++$iX) {
            if ( ($xnewtrans >= $imgxlo[$iObject][$iX]) && ($xnewtrans < $imgxhi[$iObject][$iX]) ) {
              $jx = $iX;
              last;
            }
          }
          my $jy = -1;
          for (my $iY = 0 ; $iY < $imgNAXIS2[$iObject] ; ++$iY) {
            if ( ($ynewtrans >= $imgylo[$iObject][$iY]) && ($ynewtrans < $imgyhi[$iObject][$iY]) ) {
              $jy = $iY;
              last;
            }
          }
          
          # now test whether the pixel at jx,jy is transparent or not
          if ( ($jx >= 0) && ($jy >= 0) ) {
            if ($allImages[$iObject][$jx][$jy] > 0) {
              $survivedObjects = 0;
            }
          } else {
              $survivedObjects = 0;
          }

        } # end-if testing objecttype
      } # end-if survivedObjects
    } # end-for through objects
    
    # go to the next photon row if this photon did not survive the objects
    next if !$survivedObjects;
    
    # if we've made it this far, we know this photon should be counted towards 
    # the appropriate effective area
    # figure out which energy index we're at
    my $currEnergy = $historyEnergy[$iRow];
    my $currEnergyIdx = $indexOfEnergies{$currEnergy};  
    
    #161110
    $numSurvived++;
    if ($writeevtfile) {
      if ($numSurvived % 1000 == 1) { 
        ahlog::ah_info "LOW","row=$fileRow, chunk row=$iRow, num survived=$numSurvived: " 
          . "$currEnergy $historyFINALXPOS[$iRow] $historyFINALYPOS[$iRow]"; 
      }
      print $ftmpevt $currEnergy, " ", $historyFINALXPOS[$iRow], " ", $historyFINALYPOS[$iRow], "\n";
    }
    
    ahlog::ah_debug "currEnergyIdx of $currEnergy = $currEnergyIdx \n";

    # if we're merging all the angles in the EA, it just all goes into one EA
    if ($doMerge) {
      
      $eaMerge[$currEnergyIdx]++;
      
    } elsif ($doVignetting) {
      # if we're doing vignetting, we need to use all the angles
      
      # figure out which theta and azim index we're at
      my $currOff = $historyInitialThetaArcmin[$iRow];
      #my $currOffIdx = $indexOfOffaxis{$currOff};
      my $currOffIdx = $indexOfAllUnqOffaxisHash{$currOff};
      
      my $currAzim = $historyInitialAzimDirDeg[$iRow];
      #my $currAzimIdx = $indexOfAzim{$currAzim};
      my $currAzimIdx = $indexOfAllUnqAzimuthHash{$currAzim};
    
      ahlog::ah_debug "calculating vignetting currOff=$currOff currOffIdx=$currOffIdx currAzim=$currAzim currAzimIdx=$currAzimIdx \n";
      
      $eaVignette[$currOffIdx][$currAzimIdx][$currEnergyIdx]++;
      
#      ahlog::ah_debug "eaVignette[$iTheta][$iAzim][$iEnergy] = $eaVignette[$iTheta][$iAzim][$iEnergy] \n";
      
    } else {
      # if we're on a normal run (not vignetting), calculate approp EAs
    
      ahlog::ah_debug "calculating normal ea \n";
      
      # now fill in effective area (EA) arrays for the four surrounding theta/phi 
      # combinations, whichever theta and phi this current row is
      # ea1(E) for theta_n, phi_n
      # ea2(E) for theta_n, phi_n+1
      # ea3(E) for theta_n+1, phi_n
      # ea4(E) for theta_n+1, phi_n+1

      # figure out which ea array this theta/phi combo belongs in
      # +++ 20150804 KLR This isn't the most efficient, if a target angle is exactly on the grid.
      #                  I probably should avoid interpolating at all if that is the case
      if ($isTheta_n) {
        if ($isPhi_n) {
          $ea1[$currEnergyIdx]++;
          #ahlog::ah_debug "isTheta_n and isPhi_n: ea1[$currEnergyIdx] = $ea1[$currEnergyIdx] \n";
        } 
        if ($isPhi_np1) {
          $ea2[$currEnergyIdx]++;
          #ahlog::ah_debug "isTheta_n and isPhi_np1: ea2[$currEnergyIdx] = $ea2[$currEnergyIdx] \n";
        }
      } 
      if ($isTheta_np1) {
        if ($isPhi_n) {
          $ea3[$currEnergyIdx]++;
          #ahlog::ah_debug "isTheta_np1 and isPhi_n: ea3[$currEnergyIdx] = $ea3[$currEnergyIdx] \n";
        }
        if ($isPhi_np1) {
          $ea4[$currEnergyIdx]++;
          #ahlog::ah_debug "isTheta_np1 and isPhi_np1: ea4[$currEnergyIdx] = $ea4[$currEnergyIdx] \n";
        }
      }
      
    } # end-if we're doing vignetting or not
  
  #} # end-loop through rows in history file
  } # end-loop through rows in chunk  RSH161129

  } # end-loop through chunks

} #end-loop through history files

ahlog::ah_info "HIGH", "Finished reading all input files.";

# alert the user if there were any pathcodes with 8 interactions.  Since the 
# history file only records 8 interactions, this photon may have interactions 
# we're missing.
if ($numEightInteractions > 1) {
  ahlog::ah_info "LOW", "There were $numEightInteractions photons with at least $maxPathCodes interactions.";
} elsif ($numEightInteractions > 0) {
  # there was only one.
  ahlog::ah_info "LOW", "There was $numEightInteractions photon with at least $maxPathCodes interactions.";
}

# finish calculating the EA now that we’ve counted the photons
# EA = (numimpactphotons/numtotalphotons) * geometric area


# if we merged all angles
if ($doMerge) {

  for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
    $eaMerge[$iEnergy] /= $initNUMPHOT;
    $eaMerge[$iEnergy] *= $initGEOMAREAsqcm;
  }

} elsif ($doVignetting) {
  # if we're doing vignetting, we need to use all the angles

  ahlog::ah_info "HIGH", "Calculating EA for vignetting file";

  for my $iTheta (0 .. $#allUnqOffaxis) {
    my $currTheta = $allUnqOffaxis[$iTheta];
    # get the indices needed for this current theta
    my $theta_FileIdx     = $historyFileOfOffaxis{$currTheta};
    my $theta_IdxInFile   = $thetaByFile{$currTheta};

    for (my $iAzim = 0 ; $iAzim < $numUnqAzimuth ; ++$iAzim) {
      for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
        $eaVignette[$iTheta][$iAzim][$iEnergy] /= $numInputPhotons[$theta_FileIdx][$theta_IdxInFile][$iAzim][$iEnergy];
        $eaVignette[$iTheta][$iAzim][$iEnergy] *= $initGEOMAREAsqcm;
      } # end-loop through energy
    } # end-loop through azimuth
  } # end-loop through offaxis

  my $theta0Idx = first { $allUnqOffaxis[$_] == 0.0 } 0..$#allUnqOffaxis;
  ahlog::ah_debug "theta0Idx=$theta0Idx \n";
  
  # now we can divide by EA_0 (EA at theta=0)
  for my $iTheta (0 .. $#allUnqOffaxis) {
    for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
      for (my $iAzim = 0 ; $iAzim < $numUnqAzimuth ; ++$iAzim) {
        ahlog::ah_debug "eaVignette[$iTheta][$iAzim][$iEnergy] / eaVignette[$theta0Idx][$iAzim][$iEnergy] = $eaVignette[$iTheta][$iAzim][$iEnergy]/$eaVignette[$theta0Idx][$iAzim][$iEnergy]  \n";
        $eaVignetteFinal[$iTheta][$iAzim][$iEnergy] = $eaVignette[$iTheta][$iAzim][$iEnergy] / $eaVignette[$theta0Idx][$iAzim][$iEnergy];
      }
    }
  }
  
  if (ahlog::getdebug()) {
    ahlog::ah_debug "After all EA Vignetting calculations \n";
    print Data::Dumper->Dump( [ \@eaVignette ], [ qw(*eaVignette) ] );
  }

} else {
  # we're not doing vignetting.  
  
  ahlog::ah_info "HIGH", "Calculating EA for target offaxis and target azimuth";

  if (ahlog::getdebug()) {
    print Data::Dumper->Dump( [ \@ea1 ], [ qw(*ea1) ] );
    print Data::Dumper->Dump( [ \@ea2 ], [ qw(*ea2) ] );
    print Data::Dumper->Dump( [ \@ea3 ], [ qw(*ea3) ] );
    print Data::Dumper->Dump( [ \@ea4 ], [ qw(*ea4) ] );
  }

  for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {

    #   ahlog::ah_debug " ***** iEnergy $iEnergy of $numUnqEnergies \n";
    #   ahlog::ah_debug "ea1[$iEnergy] = $ea1[$iEnergy] \n";
    #   ahlog::ah_debug "ea2[$iEnergy] = $ea2[$iEnergy] \n";
    #   ahlog::ah_debug "ea3[$iEnergy] = $ea3[$iEnergy] \n";
    #   ahlog::ah_debug "ea4[$iEnergy] = $ea4[$iEnergy] \n";

    #   ahlog::ah_debug "numInputPhotons[$theta_nFileIdx][$theta_nIdxInFile][$azim_nIdx][$iEnergy] = $numInputPhotons[$theta_nFileIdx][$theta_nIdxInFile][$azim_nIdx][$iEnergy] \n";
    #   ahlog::ah_debug "numInputPhotons[$theta_nFileIdx][$theta_nIdxInFile][$azim_np1Idx][$iEnergy] = $numInputPhotons[$theta_nFileIdx][$theta_nIdxInFile][$azim_np1Idx][$iEnergy] \n";
    #   ahlog::ah_debug "numInputPhotons[$theta_np1FileIdx][$theta_np1IdxInFile][$azim_nIdx][$iEnergy] = $numInputPhotons[$theta_np1FileIdx][$theta_np1IdxInFile][$azim_nIdx][$iEnergy] \n";
    #   ahlog::ah_debug "numInputPhotons[$theta_np1FileIdx][$theta_np1IdxInFile][$azim_np1Idx][$iEnergy] = $numInputPhotons[$theta_np1FileIdx][$theta_np1IdxInFile][$azim_np1Idx][$iEnergy] \n";

    $ea1[$iEnergy] /= $numInputPhotons[$theta_nFileIdx][$theta_nIdxInFile][$azim_nIdx][$iEnergy];
    $ea2[$iEnergy] /= $numInputPhotons[$theta_nFileIdx][$theta_nIdxInFile][$azim_np1Idx][$iEnergy];
    $ea3[$iEnergy] /= $numInputPhotons[$theta_np1FileIdx][$theta_np1IdxInFile][$azim_nIdx][$iEnergy];
    $ea4[$iEnergy] /= $numInputPhotons[$theta_np1FileIdx][$theta_np1IdxInFile][$azim_np1Idx][$iEnergy];
    $ea1[$iEnergy] *= $initGEOMAREAsqcm;
    $ea2[$iEnergy] *= $initGEOMAREAsqcm;
    $ea3[$iEnergy] *= $initGEOMAREAsqcm;
    $ea4[$iEnergy] *= $initGEOMAREAsqcm;

  } #end-loop through energies in EA

  ahlog::ah_debug "finished calculating ea1 ea2 ea3 ea4 \n";
  
  if (ahlog::getdebug()) {
     print Data::Dumper->Dump( [ \@ea1 ], [ qw(*ea1) ] );
     print Data::Dumper->Dump( [ \@ea2 ], [ qw(*ea2) ] );
     print Data::Dumper->Dump( [ \@ea3 ], [ qw(*ea3) ] );
     print Data::Dumper->Dump( [ \@ea4 ], [ qw(*ea4) ] );
  }

  # +++ 20160801 KLR should I add a check here, if ea1==ea2==etc, then don't interpolate?

  # now start interpolating effective areas to get the final EA

  # First: interpolate on phi (roll) at theta_n
  # ea_phi_0_theta_n(E) = interpolate btwn EA_1(E) and EA_2(E)
  @ea_phi_0_theta_n = @{interpolate(\@ea1, \@ea2, $azim_n, $azim_np1, $tgtAzimuth)};
  
  # Second: interpolate on phi (roll) at theta_n+1
  # ea_phi_0_theta_np1(E) = interpolate btwn EA_3(E) and EA_4(E)
  @ea_phi_0_theta_np1 = @{interpolate(\@ea3, \@ea4, $azim_n, $azim_np1, $tgtAzimuth)};

  # Third: interpolate on theta (off axis) to get final
  # eaFinal(E) = interpolate btwn EA_phi_0_theta_n(E) and EA_phi_0_theta_np1(E)
  @eaFinal = @{interpolate(\@ea_phi_0_theta_n, \@ea_phi_0_theta_np1, $theta_n, $theta_np1, $tgtOffaxis)};

  if (ahlog::getdebug()) {
    print Data::Dumper->Dump( [ \@ea_phi_0_theta_n ], [ qw(*ea_phi_0_theta_n) ] );
    print Data::Dumper->Dump( [ \@ea_phi_0_theta_np1 ], [ qw(*ea_phi_0_theta_np1) ] );
    print Data::Dumper->Dump( [ \@eaFinal ], [ qw(*eaFinal) ] );
    print Data::Dumper->Dump( [ \@currUnqEnergies ], [ qw(*currUnqEnergies) ] );
  }

}  # end-if we're doing vignetting or not
  
ahlog::ah_info "HIGH", "Finished Calculating EA";



# ----------------------
#  create the EA file
# ----------------------

if ($doMerge) {
  
  # for combining all the angles, just write the single output extension for the EA file
  
  # write the output EA bintable extension
  writeEAExtension($outputFile, $eaextname, $tgtOffaxis, $tgtAzimuth, \@eaMerge);

} elsif ($doVignetting) {
  
  # for vignetting, we want an extension for every theta and azimuth
  
  # in Perl, using ftcreate, can only create one file/extension at a time.  Add
  # those names to this array.  Then we'll merge them all into one file using 
  # ftmerge after the loops
  my @outfiles;
  
  # first, create a file with each extension
  my $extNum = 0;
  my $extname;
  my $currOffaxis = 0.0;
  my $currAzimuth = 0.0;
  for my $iTheta (0 .. $#allUnqOffaxis) {
    $currOffaxis = $allUnqOffaxis[$iTheta];
    
    for (my $iAzim = 0 ; $iAzim < $numUnqAzimuth ; ++$iAzim) {
      $currAzimuth = $allUnqAzimuth[$iAzim];
    
      ++$extNum;
      $extname = "VIGNETTE_EA_" . $extNum;
      ahlog::ah_debug "extname = $extname \n";
      
      # current name for this file (one for each extension, until combine)
      my $currOutfile = $outputFile . ".extension" . $extNum;
      push @outfiles, $currOutfile;
      ahapp::add_temp_file($currOutfile);
      
      # put the current ea (for this theta and roll) into its own array
      my @currEA;
      for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
        $currEA[$iEnergy] = $eaVignetteFinal[$iTheta][$iAzim][$iEnergy];
      }
      
      # write the output bintable extension
      writeEAExtension($currOutfile, $extname, $currOffaxis, $currAzimuth, \@currEA);

    } # end-loop through azimuth
  } # end-loop through offaxis
  
  # grab the first file we made, and make it the regular outfile
  $status = ahgen::run_ftool("ftcopy",
                  "infile=$outfiles[0]",
                  "outfile=$outputFile",
                  "clobber=yes");
  if ($status) {
    ahlog::ah_err "Error creating file $outputFile with first extension";
    ahapp::end_processing(1);
  }
  
  # now merge all the rest of the files into the outfile
  for (my $iFile = 1 ; $iFile < $extNum ; ++$iFile) {
      $status = ahgen::run_ftool("ftappend",
                    "infile=$outfiles[$iFile]",
                    "outfile=$outputFile");
    if ($status) {
      ahlog::ah_err "Error adding extension $iFile to $outputFile";
      ahapp::end_processing(1);
    }
  }
  
} else {

  # in normal mode (not vignetting), we create a single extension for the
  # target offaxis and target azimuth

  # write the output EA bintable extension
  writeEAExtension($outputFile, $eaextname, $tgtOffaxis, $tgtAzimuth, \@eaFinal);
  
} # end-if vignetting or not


#161110 write the bintable extension for the new file outevtfile, columns ENERGY, FINALXPOS, FINALYPOS

if ($writeevtfile) {
  close $ftmpevt;  # Close the text file with event data.
  writeEventFile($tmpevtfile, $outevtfile);
}

# ----------------------
#  create the ARF
# ----------------------

# If the user requested an actual ARF file, create that now, using whichever
# EA we have (either from the merge mode or from the normal mode)
if ($doARF) {

  ahlog::ah_info "HIGH", "Creating ARF." ;  
  
  # make sure that energy units in history event file are the same as the RMF
  # file.  If not, convert the energies from the history file to the same as RMF
  # The ARF file energies should always be written in the same units as the 
  # RMF because the user will want to use that ARF with the RMF.
  if ( lc($energy_tunit_rmf) ne lc($energy_tunit_evt) ) {
    
    # get the prefix to the unit
    my $rmfUnitPrefix = $energy_tunit_rmf;
    $rmfUnitPrefix =~ s/eV//; 
    my $evtUnitPrefix = $energy_tunit_evt;
    $evtUnitPrefix =~ s/eV//; 
  
    # converting the history file units to  RMF file units
    my $unitConversionFactor = getUnitConversionFactor($evtUnitPrefix, $rmfUnitPrefix);
    
#     my $factor = 1;
#     my $message = "Converting energies from history file into ";
#     # we know (from checking earlier) that the units are either eV or keV
#     if ( lc($energy_tunit_rmf) eq 'kev' ) {
#       # so evt must be in eV.  convert them to keV
#       $factor = .001;
#       $message = $message . "keV";
#     } elsif ( lc($energy_tunit_rmf) eq 'ev' ) {
#       # so evt must be in keV.  convert them to eV
#       $factor = 1000;
#       $message = $message . "keV";
#     }
    
    ahlog::ah_info "LOW", "Converting energies from history file to match RMF file";  
    $minUnqEnergy = $minUnqEnergy * $unitConversionFactor;
    $maxUnqEnergy = $maxUnqEnergy * $unitConversionFactor;
    foreach my $currEnergy (@currUnqEnergies) { $currEnergy = $currEnergy * $unitConversionFactor; }
    
  } # end-if testing that units match

  # SPECRESP array for ARF file (initialize all to 0)
  my @arfFinal = ((0) x $numRMFEnergies);

  # figure out which EA array to create the ARF from
  my @eaToUse = ( $doMerge ? @eaMerge : @eaFinal );
  
  # value of interpolated effective area
  my $outeffarea = 0;
  
  # Interpolate the EA for each energy onto the output energy grid (from RMF 
  # file). This is the final output EA for the output ARF file.
  for (my $iEnergy = 0 ; $iEnergy < $numRMFEnergies ; ++$iEnergy) {
    
    # make sure we're in the range, or else ARF stays zero
    if ( ($rmf_energ_md[$iEnergy] >= $minUnqEnergy) && 
         ($rmf_energ_md[$iEnergy] <= $maxUnqEnergy) ) {
      # now interpolate what this EA would be at that energy
      $outeffarea = interpolatePoint(\@currUnqEnergies, \@eaToUse, $rmf_energ_md[$iEnergy]);
      $arfFinal[$iEnergy] = $outeffarea;
    }
    
  if (ahlog::getdebug()) {
    print Data::Dumper->Dump( [ \@arfFinal ], [ qw(*arfFinal) ] );
  }
  
  } # end-for over EA energies
 
  writeARFFile($outarffile, \@arfFinal);
}



##################################################
#  Finishing
##################################################


ahlog::ah_debug "outputFile = $outputFile \n";

# Add CHECKSUM and DATASUM, and check that file we just created is valid
unless (ahgen::update_checksum_and_verify($outputFile)) {
  ahgen::ah_err "FITS file $outputFile failed FITS verification test." ;
  ahapp::end_processing(1);
}
ahlog::ah_info "HIGH", "Successfully created FITS file: $outputFile" ;    

# We're done
ahapp::end_processing($status);



##################################################
#  Subroutines
##################################################





#############################################################################

# +++ this would be useful in ahgen.pm maybe
sub getUnitConversionFactor {

  my $convertFrom = shift;
  my $convertTo   = shift;
  my $factor = 1;

  # hash of unit 
  my %prefixes = (
    'T' => 1e12,
    'G' => 1e9,
    'M' => 1e6,
    'k' => 1000,
    'h' => 100,
    'da' => 10,
    'd' => .1,
    'c' => .01,
    'm' => .001,
    'µ' => 1e-6,
    'n' => 1e-9,
    'dn' => 1e-10,
    'p' => 1e-12,
    'f' => 1e-15
  );

  while(my($currPrefix, $prefixFactor) = each %prefixes) {  
    if ( lc($convertFrom) eq lc($currPrefix) ) {
      $factor = $prefixes{$currPrefix};
    }
    if ( lc($convertTo) eq lc($currPrefix) ) {
      $factor = $factor / $prefixes{$currPrefix};
    }
  }

  #$outValue = $outValue * $factor;
  
  ahlog::ah_debug "convert $convertFrom to $convertTo";
  ahlog::ah_debug "conversion factor = $factor";

  return $factor;
  
} # end-sub getUnitConversionFactor

#############################################################################


# pass in an x and y array, and an x point on which to interpolate to get the 
# desired y point
sub interpolatePoint {

  ahlog::ah_debug "---- start interpolatePoint ---- \n";

  # -------------------------------------
  # define and initialize all variables
  # -------------------------------------
  
#   xGrid = @currUnqEnergies
#   yGrid = @eaToUse
#   xIn = $rmf_energ_md[$iEnergy]
#   yOut = $outeffarea
  
  # input parameters
  my $xGridRef    = shift;
  my @xGrid       = @{$xGridRef};
  my $yGridRef    = shift;
  my @yGrid       = @{$yGridRef};
  my $xIn         = shift;
  
  # return variable
  my $yOut        = 0;  
  
  ahlog::ah_debug "xIn = $xIn \n";
  
  
  # indices of the energies in the currUnqEnergies array,
  # used to interpolate for the value of the energy in the rmf_energ_md array
  my ($energyLoIdx, $energyHiIdx);
  my $slope ;
  
  # -------------------------------------
  
  # find the indices of the energies that surround this energy 
  ($energyLoIdx, $energyHiIdx) = bisectionLocate(\@currUnqEnergies, $numUnqEnergies, $xIn);
  
  # now do the linear interpolation on this point
  if (equal($xIn, $xGrid[$energyLoIdx], $precision)) {
    $yOut = $yGrid[$energyLoIdx];
  } elsif (equal($xIn, $xGrid[$energyHiIdx], $precision)) {
    $yOut = $yGrid[$energyHiIdx];
  } else {
    $slope = ($yGrid[$energyHiIdx] - $yGrid[$energyLoIdx]) / ($xGrid[$energyHiIdx]-$xGrid[$energyLoIdx]);
    $yOut = $yGrid[$energyLoIdx] + ($slope * ($xIn-$xGrid[$energyLoIdx]));
  }
  
  return $yOut;
  
} # end-sub interpolatePoint


#############################################################################

sub bisectionLocate {

  ahlog::ah_debug "---- start bisectionLocate ---- \n";

  # -------------------------------------
  # define and initialize all variables
  # -------------------------------------
  
  # input parameters
  my $xGridRef    = shift;
  my @xGrid       = @{$xGridRef};
  my $numGridPts  = shift;
  my $xIn         = shift;
  
  ahlog::ah_debug "numGridPts = $numGridPts \n";
  ahlog::ah_debug "xIn = $xIn \n";

  # initial values of pointers, they will change as the routine progresses
  my $lobin = 0;
  my $hibin = $numGridPts - 1;
  my $deltabins = $hibin - $lobin + 1;
  my $midbin = 0;
  my $foundBins = 0;
  
  my $firstX = $xGrid[$lobin];
  my $lastX  = $xGrid[$hibin];
  my $midX   = 0;
  
  ahlog::ah_debug "lobin=$lobin hibin=$hibin deltabins=$deltabins midbin=$midbin firstX=$firstX lastX=$lastX midX=$midX \n";
  
  # output parameters
  my $index1 = 0;
  my $index2 = 0;
  
  # -------------------------------------
 
 
  # case of XIN = highest grid value
  if ($xIn == $lastX) {
    $index1 = $numGridPts - 1;
    $index2 = $index1;
    return ($index1, $index2);
  }
  
  # case of XIN = lowest grid value
  if ($xIn == $firstX) {
    $index1 = 0;
    $index2 = $index1;
    return ($index1, $index2);
  }
 
  # make sure xin is valid
  if ( ($xIn < $firstX) || ($xIn > $lastX) ) {
    ahlog::ah_err "Input x=$xIn is not in range ($firstX,$lastX)";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }
  
  my $iter = 0;
  while ($deltabins > 2) {
    ahlog::ah_debug "iter=$iter: lobin=$lobin hibin=$hibin deltabins=$deltabins midbin=$midbin \n";
    
    $midbin = $lobin + int($deltabins/2);
    $midX = $xGrid[$midbin];
    if ( ($xIn > $midX) && ($xIn < $lastX) ) {
      $lobin = $midbin;
    } elsif ( ($xIn < $midX) && ($xIn > $firstX) ) {
      $hibin = $midbin;
    # Case of xIn exactly equal to the mid value
    } elsif ($xIn == $midX) {
      $index1 = $midbin - 1;
      $index2 = $midbin;
      $foundBins = 1;
      ahlog::ah_debug "foundBins\n";
      last;
    }
    # lobin and hibin move - following is the latest deltabins
    $deltabins = $hibin - $lobin + 1;

    $iter++;
  } # end-while
  
  # if we didn't find the correct bins earlier, set them to lobin and hibin
  if (!$foundBins) {
    ahlog::ah_debug "index1=lobin\n";
    # lobin and hibin should now point to the index1 and index2 respectively 
    $index1 = $lobin;
    $index2 = $hibin;
  }

  ahlog::ah_debug "---- end bisectionLocate ---- \n";
  
  return ($index1, $index2);

} # end-sub bisectionLocate



#############################################################################


# \brief Reads an RMF file.
# \param[out] energ_lo array of ENERG_LO column
# \param[out] energ_hi array of ENERG_HI column
#
# getRMFEnergyGrid Reads ENERG_LO and ENERG_HI columns from first extension in 
#             RMF file.
# 
sub getRMFEnergyGrid {

  # the output was passed in as references
  my $energ_lo = shift;
  my $energ_hi = shift;
  my $energ_lo_unit = shift;
  my $energ_hi_unit = shift;

  ahlog::ah_info "HIGH", "Reading energy columns from RMF file." ;

  # create a copy of the RMF file with ftcopy
  my $tmpRMFFile = $inrmffile . ".tmp";
  ahapp::add_temp_file($tmpRMFFile);
  # add the ! to clobber the temp outfile
  $status = ahgen::run_ftool("ftcopy",
                             "infile=$inrmffile\[1\]",
                             "outfile=!$tmpRMFFile",
                             "copyall=NO",
                             "clobber=yes");

  if ($status) {
    ahlog::ah_err "Error copying $inrmffile.";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }

  # get units of energy columns
  # (we're writing the variables that were passed in, as references)
  $$energ_lo_unit = ahgen::get_keyword($tmpRMFFile, 1, "TUNIT1");
  $$energ_hi_unit = ahgen::get_keyword($tmpRMFFile, 1, "TUNIT2");
  
  if ( !$energ_lo_unit || !$energ_hi_unit ) {
    ahlog::ah_err "Error reading units of energy columns in $tmpRMFFile";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }
  # +++ uncomment, then test this
#   if ( lc($$energ_lo_unit) ne lc($$energ_hi_unit))  {
#     ahlog::ah_err "Units of energy columns in $tmpRMFFile must be the same.";
#     ahlog::ah_err "TUNIT1 = $$energ_lo_unit ; TUNIT2 = $$energ_hi_unit";
#     ahlog::ah_err ahgen::get_tool_stderr;
#     ahapp::end_processing(1);
#   }
  
  # store the desired columns in arrays
  @{$energ_lo} = ahgen::read_column($tmpRMFFile, 1, "ENERG_LO");
  @{$energ_hi} = ahgen::read_column($tmpRMFFile, 1, "ENERG_HI");
  
  

} # end-sub getRMFEnergyGrid


#############################################################################


# \brief Analyzes the pathcode for a single photon to determine the properties
# \param[in] pathcode The string containing the entire pathcode.  It is an 
#                 array of three integers per interaction that record various 
#                 attributes of the interaction. The three integers are 
#                 referred to as A, BC, and D, and are defined as follows:
#      pathcode[0] = A = interaction type
#        1 = absorption or end of path on the results plane
#        2 = reflection only
#        3 = reflection followed by scattering
#        4 = transmission
#        9 = error
#      pathcode[1] = BC = impacted object
#        01 = results plane
#        02 = inner housing
#        03 = outer housing
#        04 = segment/sector sidewall
#        05 = pre-collimator foil
#        06 = primary mirror foil
#        07 = secondary mirror foil
#        08 = support structure
#        09 = top external object (above main telescope)
#        10 = bottom external object (below main telescope)
#      pathcode[2] = D = impacted face
#         1 = back (mirrors, pre-collimator foils)
#             i.e. the face oriented towards outer housing
#             for obstructions & focal plane, it is the top
#         2 = front (mirrors, pre-collimator foils)
#             i.e. the face oriented towards inner housing
#             for obstructions it is the bottom.
#         3 = mirror, pre-collimator foils: top face/edge 
#         4 = mirror, pre-collimator foils: bottom face/edge
#         5 = mirror, pre-collimator foils: sides
#         6 = Undetermined (but not back or front)
# \param[in] resultsPlane True if this photon hit the results plane
# \param[out] isDblRefl True if this photon was a double reflection, meaning it 
#                 reflected off the primary mirrors exactly once and the 
#                 secondary mirrors exactly once, then hit the results plane.  
#                 These reflections may include scattering.  There may also be 
#                 transmission events in the pathcode, and events involving the 
#                 precollimator or external objects: these do not effect the 
#                 double reflection status.
# \param[out] numEightInteractions The history file only writes up to 8
#                 interactions in the pathcode.  If there were 8 portions of 
#                 the current pathcode, we don't know if there were exactly 8 
#                 or more than 8.  We will still count this photon (as long as 
#                 it meets other criteria), but we will increment a counter of 
#                 these photons and alert the user to it after looping through 
#                 all the photons.
#
# checkPathCode analyzes the pathcode of a single photon to determine if this
# was a double reflection, and if this pathcode records 8 interactions.
# 
sub checkPathCode {

#  ahlog::ah_debug "---- start checkPathCode ---- \n";

  # -------------------------------------
  # define and initialize all variables
  # -------------------------------------
  
  # input parameters
  my $pathcode              = shift;
  my $resultsPlane          = shift;

  # set some constants
  my $indivPathCodeSize = 4;      # size of an individual path code

  # strings that represent reflection pathcodes to search for later.
  # these strings are hard coded into xrtraytrace
  my $priRefl       = "2062";
  my $priReflScat   = "3062";
  my $secRefl       = "2072";
  my $secReflScat   = "3072";

  # count how many reflections from primary and secondary mirrors
  my $numPrimRefl   = 0; 
  my $numSecRefl    = 0;
  
  # output parameters
  my $isDblRefl             = 0;
  my $hasEightInteractions  = 0;

  # -------------------------------------
  
  # first, check if photon hit the results plane
  # if we need to check the resultsplane column, and it was false (meaning 
  # the photon in this row did not hit the results plane), we can return from 
  # this function and move to next photon
  if (uc($resultsPlane) eq "F") {
#  if (!$resultsPlane) {
    return ($isDblRefl, $hasEightInteractions);
  }
  
  # next, check if this was a double reflection

  # Check pathcode column.  Only process double reflections, which could 
  # include transmission, scattering, and external objects.  We could accept 
  # all photons, which would be closer to real data, as real data doesn't know 
  # which was a double reflection, but we're deciding to only process double 
  # reflections.  Don't count if more than one primary + one secondary refl.

  # see if there were 8 interactions in this pathcode.  If so, we will alert
  # user later
  my $numPathCodeChars = length($pathcode);
  my $numInteractions = $numPathCodeChars / $indivPathCodeSize;
  if ($numInteractions = $maxPathCodes) {
    $hasEightInteractions++;
  }

  # break up the pathcode into individual pieces of 4
  my @currPathCodes = unpack("(A4)*", $pathcode);

  # loop through the pathcodes, looking for a double reflection
  foreach my $code (@currPathCodes) {
  
    # we only care about reflections from primary and secondary.  We can ignore
    # all other interactions. (transmission, external objects, etc)

    # search for reflections from primaries (can have scattering)
    if ( ($code == $priRefl) || ($code == $priReflScat) ) {
      $numPrimRefl++;
    }

    # seach for reflections from secondaries (can have scattering)
    if ( ($code == $secRefl) || ($code == $secReflScat) ) {
      $numSecRefl++;
    }

  } # end-loop through pathcode

  # Make sure there was exactly one primary reflection and one secondary 
  # reflection. If not, move on to the next photon.
  if ( ($numPrimRefl == 1) && ($numSecRefl == 1) ) {
    $isDblRefl = 1;
  }

#  ahlog::ah_debug "---- end checkPathCode ---- \n";
  
  return ($isDblRefl, $hasEightInteractions);
  
} # end-sub checkPathCode


#############################################################################


sub checkForAzimOffaxis {

#  ahlog::ah_debug "---- start checkForAzimOffaxis ---- \n";

  # -------------------------------------
  # define and initialize all variables
  # -------------------------------------
  
  # input parameters
  my $currOffaxis   = shift;
  my $currAzim      = shift;
  # note: theta_n, theta_np1, phi_n, phi_np1 are declared earlier using 'our'
  
  # default outputs, set to false
  my $isDesiredAngle = 0;
  my $isTheta_n = 0;
  my $isTheta_np1 = 0;
  my $isPhi_n = 0;
  my $isPhi_np1 = 0;

  # -------------------------------------
 
  if ($currOffaxis == $theta_n) {
    $isTheta_n = 1;
    $isDesiredAngle = 1;
  } 
  if ($currOffaxis == $theta_np1) {
    $isTheta_np1 = 1;
    $isDesiredAngle = 1;
  }

  if ($currAzim == $azim_n) {
    $isPhi_n = 1;
    $isDesiredAngle = 1;
  } 
  if ($currAzim == $azim_np1) {
    $isPhi_np1 = 1;
    $isDesiredAngle = 1;
  }

#  ahlog::ah_debug "---- end checkForAzimOffaxis ---- \n";
  ahlog::ah_debug "isDesiredAngle=$isDesiredAngle isTheta_n=$isTheta_n isTheta_np1=$isTheta_np1 isPhi_n=$isPhi_n isPhi_np1=$isPhi_np1 \n";
  return ($isDesiredAngle, $isTheta_n, $isTheta_np1, $isPhi_n, $isPhi_np1);

} # end-sub checkForAzimOffaxis


#############################################################################


# pass in two arrays, and the variables
sub interpolate {

  ahlog::ah_debug "---- start interpolate ---- \n";

  # -------------------------------------
  # define and initialize all variables
  # -------------------------------------
  
  # input parameters
  my $inArray1Ref    = shift;             # changing 'y' array: first
  my @inArray1       = @{$inArray1Ref};
  my $inArray2Ref    = shift;             # changing 'y' array: second
  my @inArray2       = @{$inArray2Ref};
  my $var1           = shift;             # changing 'x' value: first
  my $var2           = shift;             # changing 'x' value: second
  my $varNaught      = shift;             # changing 'x' value: target
  
  # output array
  my @outArray       = (0) x $numUnqEnergies;
  
  # -------------------------------------
  
  if (ahlog::getdebug()) {
    print Data::Dumper->Dump( [ \@inArray1 ], [ qw(*inArray1) ] );
    print Data::Dumper->Dump( [ \@inArray2 ], [ qw(*inArray2) ] );
    ahlog::ah_debug "var1=$var1 \n";
    ahlog::ah_debug "var2=$var2 \n";
  }
  
  # +++ 20160802 KLR is this a correct way to handle this situation?
  # if the user passed in angles that don't actually need interpolating, 
  # we want to avoid a 'divide by zero' perl error.  So just pass back the 
  # same array
  if ($var1 == $var2) {
  	@outArray = @inArray1;
  	
  } else {
	
		for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
			# interpolate the value between the two input arrays
		
			# +++ 20150804 I didn't think we should interpolate, not like described.  But what if tgtAzim is very close to azim_n, and far from azim_n-plus-1?
			#my $interpValue = ($inArray1[$iEnergy] + $inArray2[$iEnergy]) / 2;
		
			my $interpValue = $inArray1[$iEnergy] + 
												( ($varNaught-$var1) / ($var2-$var1) ) * 
												( $inArray2[$iEnergy] - $inArray1[$iEnergy]);
		
			# assign the interpolated value to the output
			$outArray[$iEnergy] = $interpValue;
		}
	}
  ahlog::ah_debug "---- end interpolate ---- \n";
  
  # return a reference to the output array
  return (\@outArray);
  
} # end-sub interpolate

#############################################################################

# to compare floating point numbers
# equal(NUM1, NUM2, PRECISION) : returns true if NUM1 and NUM2 are
# equal to PRECISION number of decimal places
# from https://www.safaribooksonline.com/library/view/perl-cookbook/1565922433/ch02s03.html
sub equal {
    my ($A, $B, $dp) = @_;
    return sprintf("%.${dp}g", $A) eq sprintf("%.${dp}g", $B);
} # end-sub equal

#############################################################################

sub average {  
  # make sure there are elements in the array
  if (@_ == 0) {
    ahgen::ah_err "0 elements in array" ;
    ahapp::end_processing(1);
  }
  return sum(@_)/@_;
} # end sub average


#############################################################################

sub writeEAExtension {

  # -------------------------------------
  # define and initialize all variables
  # -------------------------------------
  
  # input parameters
  my $currFileName    = shift;
  my $currExtName     = shift;
  my $currOffaxis     = shift;
  my $currAzimuth     = shift;
  my $eaRef           = shift;
  my @ea              = @{$eaRef};
  
  
  # $energy_tunit_evt was created in main program, and is accessible here
  
  # -------------------------------------
  
  ahlog::ah_info "HIGH", "Creating output file $currFileName" ; 
  
  ahlog::ah_debug "numUnqEnergies = $numUnqEnergies \n" ;
  
  # get the range of energy values.  currUnqEnergies is leftover from the last 
  # iteration through the history files, but it should have been the same for 
  # every file.
  my $energyValues = $currUnqEnergies[0] . "-" . $currUnqEnergies[$numUnqEnergies-1];

  # initialize the keyword values to the target angles.  
  my $offaxisToWrite    = $currOffaxis;
  my $azimToWrite       = $currAzimuth;
  
  # if we merged all the angles, get the averages
  if ($doMerge) {
    $offaxisToWrite = average(@allUnqOffaxis);
    $azimToWrite = average(@allUnqAzimuth);
  }
  
  # These are for CBD keywords
  my $cbdOffaxs   = "THETA(" . $offaxisToWrite . ")[arcmin]";
  my $cbdAzim     = "PHI(" . $azimToWrite . ")[deg]";
  my $cbdEnergy   = "ENERG(" . $energyValues . ")[keV]";
  
  # create text files to hold the column definitions, column data, and keywords
  # ftcreate needs these to create the output file
  my $columndeffile = "tmp_columns.txt";
  my $datafile      = "tmp_data.txt";
  my $headfile      = "tmp_head.txt";
  
  # add these to tmparray to cleanup later
  ahapp::add_temp_file($columndeffile);
  ahapp::add_temp_file($datafile);
  ahapp::add_temp_file($headfile);

  # the column name, unit, and comment will depend on if this is vignetting
  my $eaColumnName;
  my $eaColumnUnit;
  my $eaColumnComm;
  if ($doVignetting) {
    $eaColumnName = "EARATIO";
    $eaColumnUnit = "";
    $eaColumnComm = "Ratio of Effective Area per energy";
  } else {
    $eaColumnName = "EFFAREA";
    $eaColumnUnit = "cm**2";
    $eaColumnComm = "Effective Area per energy";
  }

  # create the column definition file, overwriting if it exists
  open my $fileHandleColumn, ">", $columndeffile or die "Can't open '$columndeffile'\n";
  print $fileHandleColumn "Energy 1D $energy_tunit_evt\n";
  print $fileHandleColumn "$eaColumnName 1D $eaColumnUnit\n";
  close $fileHandleColumn;
  
  # create the column data file, overwriting if it exists
  open my $fileHandleData, ">", $datafile or die "Can't open '$datafile'\n";
  for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
    print $fileHandleData $currUnqEnergies[$iEnergy] . " " . $ea[$iEnergy] . "\n";
  }
  close $fileHandleData;

  # create the column definition file, overwriting if exists
  open my $fileHandleHeader, ">", $headfile or die "Can't open '$headfile'\n";
  print $fileHandleHeader "TELESCOP  $telescop        /    Mission name\n";
  print $fileHandleHeader "INSTRUME  $instrume        /    Instrument name\n";
  print $fileHandleHeader "DETNAM    $detnam          /    Telescope name\n";
  print $fileHandleHeader "CVSD0001  $cvsd0001        /    UTC date when file should be first used\n";
  print $fileHandleHeader "CVST0001  $cvst0001        /    UTC time when file should be first used\n";
  print $fileHandleHeader "CBD10001  $cbdOffaxs       /    Offaxis angle\n";
  print $fileHandleHeader "CBD20001  $cbdAzim         /    Azimuthal roll angle\n";
  print $fileHandleHeader "CBD30001  $cbdEnergy       /    Energy values\n";
  print $fileHandleHeader "OFFAXIS   $offaxisToWrite  /    [arcmin] Source off-axis angle\n";
  print $fileHandleHeader "AZIMUTH   $azimToWrite     /    [deg] Source rotational (azimuthal) angle\n";
  print $fileHandleHeader "Energy    $energyValues    /    [keV] Energy or energy range of image\n";
  print $fileHandleHeader "NOFFAXIS  $numUnqOffaxis   /    Number of off-axis angles\n";
  print $fileHandleHeader "NAZIMUTH  $numUnqAzimuth   /    Number of azimuthal angles\n";
  print $fileHandleHeader "NORMRAD   $normRadmm       /    [mm] Normalization radius\n";
  close $fileHandleHeader;

  # create the output file
  $status = ahgen::run_ftool("ftcreate",
                   "cdfile=$columndeffile",
                   "datafile=$datafile",
                   "outfile=$currFileName",
                   "headfile=$headfile",
                   "extname=$currExtName",
                   "clobber=yes");
  if ($status) {
    ahlog::ah_err "Error creating file $currFileName";
    ahapp::end_processing(1);
  }

  # add comments to the TTYPEn keywords
  $status = ahgen::run_ftool("fthedit",
                   "infile=$currFileName",
                   "keyword=TTYPE1",
                   "operation=add",
                   "value=Energy",
                   "comment=Energy");
  $status = ahgen::run_ftool("fthedit",
                   "infile=$currFileName",
                   "keyword=TTYPE2",
                   "operation=add",
                   "value=$eaColumnName",
                   "comment=$eaColumnComm");
  if ($status) {
    ahlog::ah_err "Error editing TTYPEn keywords in $currFileName";
    ahapp::end_processing(1);
  }

  # stamp parameters to output FITS as HISTORY keywords
  if ($ahapp::history) {   
    ahapp::write_parameters($currFileName, $currExtName) ;
  }
  
  # go to primary extension to write some additional keywords
  ahgen::set_keyword($currFileName, 0, "NOFFAXIS", $numUnqOffaxis . " / Number of off-axis angles");
  ahgen::set_keyword($currFileName, 0, "NAZIMUTH", $numUnqAzimuth . " / Number of azimuthal angles");
  ahgen::set_keyword($currFileName, 0, "NORMRAD", $normRadmm . " / [mm] Normalization radius");


  ahlog::ah_debug "---- end writeEAExtension ---- \n";
  
} # end sub writeEAExtension()

#############################################################################


# \brief Write ARF file
# \param[in] currFileName 
# \param[in] arfRef Reference to ARF array
#
# writeARFFile Writes an ARF file
# 
sub writeARFFile {

  # -------------------------------------
  # define and initialize variables
  # -------------------------------------
  
  # input parameters
  my $fileName        = shift;
  my $arfRef          = shift;
  my @arf             = @{$arfRef};
  
  # these were created in the main program, and are accessible here: 
  # @rmf_energ_lo, @rmf_energ_hi, $rmf_energ_lo_unit, $rmf_energ_hi_unit
  
  my $extName     = "SPECRESP";
  
  # create text files to hold the column definitions, column data, and keywords
  # ftcreate needs these to create the output file
  my $columndeffile = "tmp_columns.txt";
  my $datafile      = "tmp_data.txt";
  my $headfile      = "tmp_head.txt";
  
  # add these to tmparray to cleanup later
  ahapp::add_temp_file($columndeffile);
  ahapp::add_temp_file($datafile);
  ahapp::add_temp_file($headfile);

  # -------------------------------------
  
  ahlog::ah_info "HIGH", "Creating output file $fileName" ;    

  ahlog::ah_debug "numUnqEnergies = $numUnqEnergies \n" ;
  
  # create the column definition file, overwriting if it exists
  open my $fileHandleColumn, ">", $columndeffile or die "Can't open '$columndeffile'\n";
  print $fileHandleColumn "ENERG_LO 1E $rmf_energ_lo_unit\n";
  print $fileHandleColumn "ENERG_HI 1E $rmf_energ_hi_unit\n";
  print $fileHandleColumn "SPECRESP 1E cm^2\n";
  close $fileHandleColumn;
  
  # create the column data file, overwriting if it exists
  open my $fileHandleData, ">", $datafile or die "Can't open '$datafile'\n";
  for (my $iEnergy = 0 ; $iEnergy < $numRMFEnergies ; ++$iEnergy) {
    print $fileHandleData $rmf_energ_lo[$iEnergy] . " " . $rmf_energ_hi[$iEnergy] . " " . $arf[$iEnergy] . "\n";
  }
  close $fileHandleData;

  # create the column definition file, overwriting if exists
  open my $fileHandleHeader, ">", $headfile or die "Can't open '$headfile'\n";
  print $fileHandleHeader "TELESCOP  $telescop        /    Mission name\n";
  print $fileHandleHeader "INSTRUME  $instrume        /    Instrument name\n";
  print $fileHandleHeader "DETNAM    $detnam          /    Telescope name\n";
  print $fileHandleHeader "GEOMAREA $initGEOMAREAsqcm /    Geometric area (cm^2)\n";
  print $fileHandleHeader "RTEVFILE  $inxrtevtfile    /    Input event file\n";
  print $fileHandleHeader "ARFVERSN  1992a            /    Obsolete\n";
  print $fileHandleHeader "HDUCLASS  OGIP             /    File format\n";
  print $fileHandleHeader "HDUVERS   1.1.0            /    Version of the file format\n";
  print $fileHandleHeader "HDUCLAS1  RESPONSE         /    Extension contains response data\n";
  print $fileHandleHeader "HDUVERS1  1.0.0            /    Obsolete\n";
  print $fileHandleHeader "HDUCLAS2  SPECRESP         /    Extension contains an ARF\n";
  print $fileHandleHeader "HDUVERS2  1.1.0            /    Obsolete\n";
  close $fileHandleHeader;

  # create the output file
  $status = ahgen::run_ftool("ftcreate",
                   "cdfile=$columndeffile",
                   "datafile=$datafile",
                   "outfile=$fileName",
                   "headfile=$headfile",
                   "extname=$extName",
                   "clobber=yes");
  if ($status) {
    ahlog::ah_err "Error creating file $fileName";
    ahapp::end_processing(1);
  }

  # add comments to the TTYPEn keywords
  $status = ahgen::run_ftool("fthedit",
                   "infile=$fileName",
                   "keyword=TTYPE1",
                   "operation=add",
                   "value=ENERG_LO",
                   "comment=Energy");
  $status = ahgen::run_ftool("fthedit",
                   "infile=$fileName",
                   "keyword=TTYPE2",
                   "operation=add",
                   "value=ENERG_HI",
                   "comment=Energy");
  $status = ahgen::run_ftool("fthedit",
                   "infile=$fileName",
                   "keyword=TTYPE3",
                   "operation=add",
                   "value=SPECRESP",
                   "comment=Effective Area");
  if ($status) {
    ahlog::ah_err "Error editing TTYPEn keywords in $fileName";
    ahapp::end_processing(1);
  }

  # stamp parameters to output FITS as HISTORY keywords
  if ($ahapp::history) {   
    ahapp::write_parameters($fileName, $extName) ;
  }
  
  ahlog::ah_debug "---- end writeARFFile ---- \n";
  
} # end sub writeARFFile()

#############################################################################

# \brief Write event file
# \param[in] tmpevtfile
# \param[in] outevtfile
#
# writeEventFile Writes an event file
# 
sub writeEventFile {  #161110

  # -------------------------------------
  # define and initialize variables
  # -------------------------------------
  
  # input parameters
  my $tmpevtfile      = shift;   # text file created during event loop
  my $outevtfile      = shift;   # output FITS file
  
  my $extName     = "RTEVENTS";    # extension name
  
  # create text files to hold the column definitions, column data, and keywords
  # ftcreate needs these to create the output file.  The column data
  # file already exists.
  my $columndeffile = "tmp_columns.txt";
  my $headfile      = "tmp_head.txt";
  
  # add these to tmparray to cleanup later
  ahapp::add_temp_file($columndeffile);
  ahapp::add_temp_file($headfile);

  # -------------------------------------
  
  ahlog::ah_info "HIGH", "Creating output file $outevtfile" ;    

  # create the column definition file, overwriting if it exists
  open my $fileHandleColumn, ">", $columndeffile or die "Can't open '$columndeffile'\n";
#ENERGY (keV), FINALXPOS (mm), FINALYPOS (mm)
  print $fileHandleColumn "ENERGY    D keV\n";
  print $fileHandleColumn "FINALXPOS D mm\n";
  print $fileHandleColumn "FINALYPOS D mm\n";
  close $fileHandleColumn;
  
  # create the header file, overwriting if exists
  open my $fileHandleHeader, ">", $headfile or die "Can't open '$headfile'\n";
  print $fileHandleHeader "TELESCOP  $telescop        /    Mission name\n";
  print $fileHandleHeader "INSTRUME  $instrume        /    Instrument name\n";
  print $fileHandleHeader "DETNAM    $detnam          /    Telescope name\n";
  print $fileHandleHeader "GEOMAREA $initGEOMAREAsqcm /    Geometric area (cm^2)\n";
  print $fileHandleHeader "RTEVFILE  $inxrtevtfile    /    Input event file\n";
  close $fileHandleHeader;

  # create the output file
  $status = ahgen::run_ftool("ftcreate",
                   "cdfile=$columndeffile",
                   "datafile=$tmpevtfile",
                   "outfile=$outevtfile",
                   "headfile=$headfile",
                   "extname=$extName",
                   "clobber=yes");
  if ($status) {
    ahlog::ah_err "Error creating file $outevtfile";
    ahapp::end_processing(1);
  }

  # stamp parameters to output FITS as HISTORY keywords
  if ($ahapp::history) {   
    ahapp::write_parameters($outevtfile, $extName) ;
  }
  
  ahlog::ah_debug "---- end writeEventFile ---- \n";
  
} # end sub writeEventFile()

#############################################################################





# Revision Log:
# $Log: arftable.pl,v $
# Revision 1.19  2016/12/02 20:32:18  rshill
# Buffer in photon history rows in chunks of 1000000 to mitigate memory problem with large files.
#
# Revision 1.18  2016/11/10 17:05:49  rshill
# Added boolean par writeevtfile to write file of surviving events.
#
# Revision 1.17  2016/08/29 20:40:14  klrutkow
# added consistency checking for energy TUNIT between files, and converting energy to be same units as RMF file if they are different.
#
# Revision 1.16  2016/08/22 18:10:52  klrutkow
# 1. fixed typo when calculating new y position ynew ; 2. improved logic for deciding which mode (merge, vignette, arf) ; 3. read TUNITn keywords from RMF file, to write to ARF file ; 4. add average() subroutine, for writing average of theta and phi to output file if merging angles ; 5. update which keywords to write to ARF file ; 6. add error checking if image file keywords aren't present ; 7. add some extra logging output
#
# Revision 1.15  2016/08/16 18:34:25  klrutkow
# 1. removed some leftover debugging printouts
#
# Revision 1.14  2016/08/16 18:32:24  klrutkow
# 1. fixed misc typos: when reading FPMM2AM keyword, when getting y intercept of photon with object, and when calculating pixel indices ; 2. moved the reading of the image file(s) to initialization section, rather than inside history file row loop
#
# Revision 1.13  2016/08/15 21:12:48  klrutkow
# 1. implemented ability to check for objects (gate valve, filters) in the photon path ; 2. fixed bug in checkPathCode when checking that a photon reached the results plane
#
# Revision 1.12  2016/08/12 16:01:21  klrutkow
# a few small fixes for the arf implementation
#
# Revision 1.11  2016/08/12 14:38:26  klrutkow
# updated with changes in order to implement the creation of an actual ARF file.  Also ability to use all angles, without interpolation for target angles.  Original mode still works as is.  New mode runs, but has not been tested yet
#
# Revision 1.10  2016/08/03 17:10:56  klrutkow
# added check inside interpolate, if var1==var2, to pass back inArray1, in order to avoid Perl divide-by-zero error
#
# Revision 1.9  2016/03/23 01:56:11  klrutkow
# add cvs log
#id:$ &
#

