#!/usr/bin/perl
#
# File name: eeftable.pl
# Author:    Kristin Rutkowski
# $Date: 2016/03/23 01:59:57 $
# Version: 1.0
#
# This Perl script will create an EEF file from photon history file
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



#########################
#  Pragmas
#########################

use strict;
use warnings;



#########################
#  Packages
#########################

# empty parenthesis force explicit use of namespaces
use ahlog ();
use ahgen ();
use ahapp ();               # $clobber $chatter $logfile $debug $history $mode

use List::Util qw(first);
use List::Util qw( sum );
use Math::Trig;             # sin, cos, tan, deg2rad

use Data::Dumper;           # to output arrays for debugging


#########################
#  Startup
#########################

my $nargs = scalar @ARGV ;

# Query canonical APE parameters and start logging
ahapp::startup () ;



#########################
#  Input Parameters
#########################

# Query non-standard APE parameters.
my $infile          = ahapp::query_parameter("infile") ;
my $outfile         = ahapp::query_parameter("outfile") ;
my $numRadVals      = ahapp::query_parameter("numRadVals");
my $deltaRadVals    = ahapp::query_parameter("deltaRadVals");
my $alpha           = ahapp::query_parameter("alpha");
my $mgOffaxis       = ahapp::query_parameter("mgOffaxis",1);
my $mgAzimuth       = ahapp::query_parameter("mgAzimuth",1);
my $mgEnergy        = ahapp::query_parameter("mgEnergy",1);

# perl stores bools as 0 or 1.  store boolean strings also, for when we're
# writing these to output fits files.
my $mgOffaxisBool    = ($mgOffaxis) ? "yes" : "no";
my $mgAzimuthBool    = ($mgAzimuth) ? "yes" : "no";
my $mgEnergyBool     = ($mgEnergy) ? "yes" : "no";
my $clobberBool      = ($ahapp::clobber) ? "yes" : "no";



#########################
#  Other Variables 
#########################

my $status = 0;

# name of extensions in history file
my $extension1 = "PHOTONHISTORY";
my $extension2 = "INPUTPHOTONS";

# set some constants
my $arcminToRad         = 2.908882167604234e-04;
my $radianToArcsec      = 206264.8005072553933132;
my $indivPathCodeSize   = 4;    # size of an individual path code
my $maxPathCodes        = 8;    # history file stores up to 8 individual path codes

# create a 4D array for the eef values
# this will hold the radial EEF for each roll, theta, and energy.
# if energy is to be averaged, then all the values will go into the first
# entry of that last dimension: eefValues[i][j][k][0]
# size: (numRadVals, numUnqAzim, numUnqOffaxis, numUnqEnergies)
my @eefValues = ();

# a corresponding array of normalized EEF values
# size: (numRadVals, numUnqAzim, numUnqOffaxis, numUnqEnergies)
my @normEEFValues = ();

# weighted total of EEF energies, for each Roll, Theta, Energy combination.
# If nothing is merged, there will be an extension for each combination and
# each extension will have to be normalized by it's respective
# totalWeightedEnergy.
# size: (numUnqAzim, numUnqOffaxis, numUnqEnergies)
my @totalWeightedEnergy = ();

# This will be a 4D array, holding the final EEF values, after any merging
# numAzimuth etc will be either 1 or numUnqAzimuth, based on merging
# size: (numRadVals, numAzimuth, numOffaxis, numEnergies)
my @finalEEF = ();

# 3D array of how many photons contributed to a particular EEF,
# for each eef, roll, theta, and energy.
# size: (numUnqAzim, numUnqOffaxis, numUnqEnergies)
my @totalpsfphotons = ();

# this will be a 3D array, holding final photon count that contributed to the
# EEF for a particular azim/theta/energy.  This array is for after any 
# possible merging
# size: (numAzimuth, numOffaxis, numEnergies)
my @finalPSFPhotons = ();

# +++ 20150806 KLR This will hold the radii of photons that are accepted
# the output will be be printed to an ascii file, for diagnostic purposes
# [roll][theta][energy]
my @acceptedPhotonRadialValuesArcsec = ();
my @numAcceptedPhotons = ();
my @acceptedPhotonPathcode = ();
my @acceptedPhotonRowNum = ();

#########################
#  Initializing 
#########################

ahlog::ah_out "Start tool eeftable.pl" ;

# Pre-Processing
$status = ahapp::begin_processing();
unless ( $status == 0 ) {
  ahlog::ah_err "BeginProcessing" ;
  ahapp::end_processing(1);
}

# Write all parameters to the log file.
ahlog::ah_info "HIGH", ahapp::write_parameters () ;

# Check if the output file already exists.  Unless clobber is set, this will
# cause the script to fail.
unless ($ahapp::clobber) {
  if  (-e $outfile) {
    ahlog::ah_err "Output file already exist but clobber was not set.  Exiting." ;
    ahapp::end_processing(1);
  }
}

# If $outfile already exists, delete it.  We have already checked that clobber
# has been set.
unlink $outfile if -e $outfile ;

# Make sure infile exists, before getting its data
if (! -e $infile) {
  ahlog::ah_err "File $infile does not exist. Exiting.";
  ahapp::end_processing(1);
}

ahlog::ah_info "HIGH", "Processing file: $infile";

# create a copy of the history file with ftselect
my $tmpHistory = $infile . ".ftselect.tmp";
ahapp::add_temp_file($tmpHistory);
# add the ! to clobber the temp outfile
$status = ahgen::run_ftool("ftcopy",
                           "infile=$infile",
                           "outfile=!$tmpHistory",
                           "copyall=YES",
                           "clobber=yes");

if ($status) {
  ahlog::ah_err "Error copying infile.";
  ahlog::ah_err ahgen::get_tool_stderr;
  ahapp::end_processing(1);
}

# Check that photons were not originally from a file.  If they were from a 
# file, there could have been multiple sources, and we don't want that.  
my $srcmdl = ahgen::get_keyword($tmpHistory, $extension1, "SRCMDL");
if ( (uc $srcmdl eq "PHOTONLIST") || (uc $srcmdl eq "GROUNDMODEL")) {
  ahlog::ah_err "History file $infile must be made from photons not from a file.  When running xrtraytrace, use 'source' of point, flatcircle, or betamodel.";
  ahlog::ah_err ahgen::get_tool_stderr;
  ahapp::end_processing(1);
}

# current number of unique offaxis theta angles, azimuthal (roll) phi angles, and energies
my $numUnqOffaxis   = ahgen::get_keyword($tmpHistory, $extension2, "NOFFAXIS");
my $numUnqAzimuth   = ahgen::get_keyword($tmpHistory, $extension2, "NAZIMUTH");
my $numUnqEnergies  = ahgen::get_keyword($tmpHistory, $extension2, "NUMENRG");

ahlog::ah_debug "numUnqOffaxis = $numUnqOffaxis \n";
ahlog::ah_debug "numUnqAzimuth = $numUnqAzimuth \n";
ahlog::ah_debug "numUnqEnergies = $numUnqEnergies \n";

# grab each column into an array
my @history2InitialTheta    = ahgen::read_column($tmpHistory, $extension2, "INITIALTHETA");
my @history2InitialAzimDir  = ahgen::read_column($tmpHistory, $extension2, "INITIALAZIMDIR");
my @history2Energy          = ahgen::read_column($tmpHistory, $extension2, "ENERGY");
my @history2NumPhotons      = ahgen::read_column($tmpHistory, $extension2, "NUMPHOTONS");

# hashes, so we can fill it with data from this file
my %currOffaxisHash    = ();
my %currAzimHash       = ();
my %currEnergiesHash   = ();
@currOffaxisHash{@history2InitialTheta} = ();
@currAzimHash {@history2InitialAzimDir} = ();
@currEnergiesHash{@history2Energy}      = ();

# arrays of unique values from history file
my @unqOffaxisArcmin;
my @unqAzimDeg;
my @unqEnergies;

# create a sorted array of all the unique offaxis theta angles in this file
foreach my $key (sort { $a <=> $b} keys %currOffaxisHash) {
  push @unqOffaxisArcmin, $key ;
}

# create a sorted array of all the unique azimuthal angles in this file
foreach my $key (sort { $a <=> $b} keys %currAzimHash) {
  push @unqAzimDeg, $key ;
}

# create a sorted array of all the unique energies in this file
foreach my $key (sort { $a <=> $b} keys %currEnergiesHash) {
  push @unqEnergies, $key ;
}

if (ahlog::getdebug()) {
  print Data::Dumper->Dump( [ \@unqOffaxisArcmin ], [ qw(*unqOffaxisArcmin) ] );
  print Data::Dumper->Dump( [ \@unqAzimDeg ], [ qw(*unqAzimDeg) ] );
  print Data::Dumper->Dump( [ \@unqEnergies ], [ qw(*unqEnergies) ] );
}

#ahlog::ah_debug "average of unqOffaxisArcmin = average(@unqOffaxisArcmin)";
#ahlog::ah_debug "average of unqAzimDeg = average(@unqAzimDeg)";
#ahlog::ah_debug "average of unqEnergies = average(@unqEnergies)";

# now that we know number of unique azimuth, etc, initialize the
# eef and totalpsfphotons arrays
# (numRadVals, numUnqAzim, numUnqOffaxis, numUnqEnergies)
for (my $iRad = 0 ; $iRad < $numRadVals ; ++$iRad) {
  for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
    for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
      for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) {
        $eefValues[$iRad][$iRoll][$iTheta][$iEnergy] = 0;
      }
    }
  }
}
for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
  for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
    for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) {
      $totalpsfphotons[$iRoll][$iTheta][$iEnergy] = 0;
    }
  }
}
  
# go to first extension

# store how many total photons (rows) in history file
my $numRows = ahgen::get_keyword($tmpHistory, $extension1, "NAXIS2") ;

ahlog::ah_debug "numRows in first extension = $numRows \n";

# get the telescope focal length [mm], and other keywords
my $telescop          = ahgen::get_keyword($tmpHistory, $extension1, "TELESCOP");
my $instrume          = ahgen::get_keyword($tmpHistory, $extension1, "INSTRUME");
my $detnam            = ahgen::get_keyword($tmpHistory, $extension1, "DETNAM");
my $cvsd0001          = ahgen::get_keyword($tmpHistory, $extension1, "CVSD0001");
my $cvst0001          = ahgen::get_keyword($tmpHistory, $extension1, "CVST0001");
my $focalLengthmm     = ahgen::get_keyword($tmpHistory, $extension1, "DFLMM");
my $fpmm2am           = ahgen::get_keyword($tmpHistory, $extension1, "FPMM2AM");

ahlog::ah_debug "telescop = $telescop \n";
ahlog::ah_debug "instrume = $instrume \n";
ahlog::ah_debug "detnam = $detnam \n";
ahlog::ah_debug "cvsd0001 = $cvsd0001 \n";
ahlog::ah_debug "cvst0001 = $cvst0001 \n";
ahlog::ah_debug "focalLengthmm = $focalLengthmm \n";


# find out if the history file contain only photons that hit the results plane
my $resultsPlaneOnly  = ahgen::get_keyword($tmpHistory, $extension1, "RSLPONLY");
# if the history file contained only photons that hit the results plane, then
# we don't need to check for the resultsplane column.
my $checkForResultsPlane = ( uc $resultsPlaneOnly eq "NO" ? 1 : 0);

ahlog::ah_debug "resultsPlaneOnly = $resultsPlaneOnly \n";
ahlog::ah_debug "checkForResultsPlane = $checkForResultsPlane \n";

# store the following history file columns in local arrays
# +++ KLR is it practical to store this info here, or should I grab it later
# when I need it and only store one row at a time?  how does cfitsio do this?  (history file may have ~10^9 rows)
my @historyEnergy               = ahgen::read_column($tmpHistory, $extension1, "energy");
my @historyInitialThetaArcmin   = ahgen::read_column($tmpHistory, $extension1, "initialtheta");
my @historyInitialAzimDirDeg    = ahgen::read_column($tmpHistory, $extension1, "initialazimdir");
my @historyFinalxPosmm          = ahgen::read_column($tmpHistory, $extension1, "finalxpos");
my @historyFinalyPosmm          = ahgen::read_column($tmpHistory, $extension1, "finalypos");
my @historyPathcode             = ahgen::read_column($tmpHistory, $extension1, "pathcode");

my @historyResultsPlane = ((1) x $numRows);
if ($checkForResultsPlane) {
  @historyResultsPlane          = ahgen::read_column($tmpHistory, $extension1, "resultsplane");
} else {
  # else, everything in file did hit the results plane.
  # initialize @historyResultsPlane with all true
  @historyResultsPlane          = (1) x $numRows;
}

# this will be needed for calculating the source center later
my $plateScaleArcsec = $focalLengthmm / $radianToArcsec;

ahlog::ah_debug "plateScaleArcsec = $plateScaleArcsec \n";

# store the initial unique energy for easy reference
my $initEnergy = $unqEnergies[0];

ahlog::ah_debug "initEnergy = $initEnergy \n";

# array of each energy, with spectral weighting
my @weights = ();
# weigh each energy: total += W_i * E_i
for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
  $weights[$iEnergy] = ($unqEnergies[$iEnergy] / $initEnergy )^(-$alpha);
}
# +++ 20150506 KLR are these values correct?
# +++ this may not be necessary if I just calculcate it on the fly later.
# decide which takes longer.  there may only be 5 or 100 energies, but 10e9 history file rows. 

# the array of radial distances. [arcsec]
my @radialValues = ();
for (my $ir = 0 ; $ir < $numRadVals ; ++$ir) {
  $radialValues[$ir] = $ir * $deltaRadVals;
}

if (ahlog::getdebug()) {
  print Data::Dumper->Dump( [ \@weights ], [ qw(*weights) ] );
  #print Data::Dumper->Dump( [ \@radialValues ], [ qw(*radialValues) ] );
}

# create two 2D arrays (for x and y) to hold center coords for each
# offaxis and roll angle
# size: (numUnqAzim, numUnqOffaxis)
my @xCenters = ();
my @yCenters = ();

# loop through each roll angle
for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth ; ++$iRoll) {
  
  my $currRollRad = deg2rad($unqAzimDeg[$iRoll]);
  
  # for each roll angle, loop through each off axis angle
  for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis ; ++$iTheta) {
    
    my $currOffaxisRad = $unqOffaxisArcmin[$iTheta] * $arcminToRad;
    
    # calculate the EEF center coordinate for this source
    my $flTanTheta = $focalLengthmm * tan($currOffaxisRad);
    my $xCenter = $flTanTheta * cos($currRollRad);
    my $yCenter = $flTanTheta * sin($currRollRad);
    
    # store those centers into the arrays
    $xCenters[$iRoll][$iTheta] = $xCenter; 
    $yCenters[$iRoll][$iTheta] = $yCenter;
  
  } # end-loop through offaxis angles

} # end-loop through roll angles

if (ahlog::getdebug()) {
  print Data::Dumper->Dump( [ \@xCenters ], [ qw(*xCenters) ] );
  print Data::Dumper->Dump( [ \@yCenters ], [ qw(*yCenters) ] );
  # what should these be?  They don't match xrtraytrace_sxt_POINT_nph1e3_4en_2off_3roll_eef.fits xcen and ycen
}

#########################
#  Main Code Block (doWork)
#########################

# count how many pathcodes we see with 8 interactions.  The history file only
# records up to the first 8 interactions.  If we see 8 later when we're 
# examining the pathcode, we don't know if there were actually 8 or more 
# than 8.  Alert the user to this.
my $numEightInteractions = 0;

# Normalization radius in mm.  This may be requested as an input param later.
# This is written to the keyword NORMRAD in the output primary HDU
my $normRadmm = 1e30;

# which radial bin the current photon belongs to, as loop through history file
my $currRadialBin = 0;

# loop through history file rows
for (my $iRow = 0 ; $iRow < $numRows ; ++$iRow) {
  
  #ahlog::ah_debug "** iRow: $iRow";
  
  # call the function to see if the current photon hit the results plane and 
  # was a double reflection.  If it wasn't, move on to the next photon.
  my ($isDblRefl, $currHasEightInteractions) = checkPathCode($historyPathcode[$iRow], $historyResultsPlane[$iRow]);
  $numEightInteractions += $currHasEightInteractions;
  if (!$isDblRefl) {
    #ahlog::ah_debug "not a double reflection \n";
    next;
  }
  
  # if we make it here, we know this photon path was a double reflection that 
  # hit the results plane
  
  # see which theta, roll, and energy we're at
  my $currThetaArcmin   = $historyInitialThetaArcmin[$iRow]; 
  my $currRollDeg       = $historyInitialAzimDirDeg[$iRow];
  my $currEnergy        = $historyEnergy[$iRow];
  
  # see which theta, roll, and energy bin we're in
  # +++ 20150629 KLR check for accuracy/tolerance here.
  my $currThetaBin  = first { $unqOffaxisArcmin[$_] == $currThetaArcmin } 0..$#unqOffaxisArcmin;
  my $currRollBin   = first { $unqAzimDeg[$_] == $currRollDeg } 0..$#unqAzimDeg;
  my $currEnergyBin = first { $unqEnergies[$_] == $currEnergy } 0..$#unqEnergies;
  
  # if the current theta or roll or energy weren't found, abort this run.  
  # That could be because a history file was merged from multiple files, with 
  # an inaccurate 2nd extension.
  if ( !defined($currThetaBin) )  {
    ahgen::ah_err "The offaxis value ($currThetaArcmin arcmin) in the first extension of the history file wasn't present in the second extension.  This could be a result of an error in merging history files or altering a history file.";
    ahapp::end_processing(1);
  }
  if ( !defined($currRollBin) )  {
    ahgen::ah_err "The azimuthal value ($currRollDeg deg) in the first extension of the history file wasn't present in the second extension.  This could be a result of an error in merging history files or altering a history file.";
    ahapp::end_processing(1);
  }
  if ( !defined($currEnergyBin) )  {
    ahgen::ah_err "The energy value ($currEnergy) in the first extension of the history file wasn't present in the second extension.  This could be a result of an error in merging history files or altering a history file.";
    ahapp::end_processing(1);
  }
  
  #ahlog::ah_debug "indexof(%d) in unqOffaxisArcmin = %4d \n", $currThetaArcmin, $currThetaBin;
  #ahlog::ah_debug "indexof(%d) in unqAzimDeg = %4d \n", $currRollDeg, $currRollBin;
  #ahlog::ah_debug "indexof(%d) in unqEnergies = %4d \n", $currEnergy, $currEnergyBin;
  
  # use those bins to to grab to the center coord
  my $currCentralX = $xCenters[$currRollBin][$currThetaBin]; 
  my $currCentralY = $yCenters[$currRollBin][$currThetaBin];
  
  # calculate radius of this photon r = sqrt(x^2 + y^2)
  my $currPhotonRadmm = sqrt( ($historyFinalxPosmm[$iRow] + $currCentralX) * ($historyFinalxPosmm[$iRow] + $currCentralX) +
                              ($historyFinalyPosmm[$iRow] + $currCentralY) * ($historyFinalyPosmm[$iRow] + $currCentralY) ); 
  
  
  # calculate which radial bin this photon belongs in
  my $radArcsec = $currPhotonRadmm / $plateScaleArcsec;
#  my $eefbinindex = int( $radArcsec / $deltaRadVals);
  
  if ($iRow+1 == 4997) {
    ahlog::ah_debug "row 4997";
    ahlog::ah_debug "historyFinalxPosmm[$iRow] = $historyFinalxPosmm[$iRow]";
    ahlog::ah_debug "historyFinalyPosmm[$iRow] = $historyFinalyPosmm[$iRow]";
    ahlog::ah_debug "currCentralX = $currCentralX";
    ahlog::ah_debug "currCentralY = $currCentralY";
    ahlog::ah_debug "currPhotonRadmm = $currPhotonRadmm";
    ahlog::ah_debug "plateScaleArcsec = $plateScaleArcsec";
    ahlog::ah_debug "radArcsec = $radArcsec";
  }
  
  #ahlog::ah_debug "eefbinindex = $eefbinindex";
  
  # calculate the current weighted energy
  # ( (E_i / E_0) ^ -alpha ) * E_i
  my $currWeightedEnergy = ( ($currEnergy / $initEnergy)^(-$alpha) ) * $currEnergy;
  # +++ is it faster to calculate this each time, or to use the weights array/hash and indexof()?
  
  # flag to see if this photon added to the EEF
  my $photonShouldCount = 0;
  
  # now, starting at the eefbinindex for this radial distance, add this 
  # weighted energy to all eefbins up to the end
#  for (my $iRad = $eefbinindex ; $iRad < $numRadVals ; ++$iRad) {
  for (my $iRad = 0 ; $iRad < $numRadVals ; ++$iRad) {
    # only increment if the radius is inside requested radial bins
    if ($radArcsec <= $radialValues[$iRad]) {
      #ahlog::ah_debug "add currWeightedEnergy to eefvalues: radialValues[$iRad] = %4d \n", $radialValues[$iRad];
      $eefValues[$iRad][$currRollBin][$currThetaBin][$currEnergyBin] += $currWeightedEnergy;
      $photonShouldCount = 1;   # flag that this photon (this row in the history file) should count towards totalpsfphotons
    }
  }
  
  # if we incremented the EEF because of this photon, we should count this photon towards totalpsfphotons
  if ($photonShouldCount) {
    ++$totalpsfphotons[$currRollBin][$currThetaBin][$currEnergyBin];
    
    # +++ for debugging
    my $currRow = $iRow+1;
    #ahlog::ah_debug "photon at row $currRow counts towards totalpsfphotons";
    ++$numAcceptedPhotons[$currRollBin][$currThetaBin][$currEnergyBin];
    my $photonNum = $numAcceptedPhotons[$currRollBin][$currThetaBin][$currEnergyBin];
    $acceptedPhotonRadialValuesArcsec[$currRollBin][$currThetaBin][$currEnergyBin][$photonNum] = $radArcsec;
    $acceptedPhotonPathcode[$currRollBin][$currThetaBin][$currEnergyBin][$photonNum] = $historyPathcode[$iRow];
    $acceptedPhotonRowNum[$currRollBin][$currThetaBin][$currEnergyBin][$photonNum] = $currRow;
    
  }
  
  # add to the total weighted energy for this roll/theta/energy, which is 
  # summed for every photon, until we get to the normalization radius
  if ($currPhotonRadmm <= $normRadmm) {
    $totalWeightedEnergy[$currRollBin][$currThetaBin][$currEnergyBin] += $currWeightedEnergy;
  }
  
} # end for-loop through rows in history file

# +++ create diagnostic files
if (ahlog::getdebug()) {

  # +++ 20150806 KLR add radial values of accepted photons to a diagnostic file
  my $radialDiagFile = "eefDiagnosticRadial.dat";
  my $radialDiagFileSorted = "eefDiagnosticRadialSorted.dat";
  my $extNum = 0;
  open my $fileHandleRadialDiagData, ">", $radialDiagFile or die "Can't open '$radialDiagFile'\n";
  open my $fileHandleRadialDiagDatasorted, ">", $radialDiagFileSorted or die "Can't open '$radialDiagFileSorted'\n";
  
  for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
    
    for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
      
      for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) {
        
        ++$extNum;
        print $fileHandleRadialDiagData "\n\n extension # $extNum \n";
        print $fileHandleRadialDiagData "Azimuth $iRoll = $unqAzimDeg[$iRoll]"  . "\n";
        print $fileHandleRadialDiagData "Offaxis $iTheta = $unqOffaxisArcmin[$iTheta]"  . "\n";
        print $fileHandleRadialDiagData "Energy $iEnergy = $unqEnergies[$iEnergy]"  . "\n\n";
        print $fileHandleRadialDiagData "photon number \t radialValues[arcsec] \t pathcode \t row number" . "\n";
        
        
        print $fileHandleRadialDiagDatasorted "\n\n extension # $extNum \n";
        print $fileHandleRadialDiagDatasorted "Azimuth $iRoll = $unqAzimDeg[$iRoll]"  . "\n";
        print $fileHandleRadialDiagDatasorted "Offaxis $iTheta = $unqOffaxisArcmin[$iTheta]"  . "\n";
        print $fileHandleRadialDiagDatasorted "Energy $iEnergy = $unqEnergies[$iEnergy]"  . "\n\n";
        print $fileHandleRadialDiagDatasorted "photon number \t radialValues[arcsec] " . "\n";
        
        # sort the array
        my @unsortedArrayArcsec;
        for (my $iPhoton = 1 ; $iPhoton <= $numAcceptedPhotons[$iRoll][$iTheta][$iEnergy] ; ++$iPhoton) {
          $unsortedArrayArcsec[$iPhoton] = $acceptedPhotonRadialValuesArcsec[$iRoll][$iTheta][$iEnergy][$iPhoton];
        }
        my @sortedArrayArcsec = sort @unsortedArrayArcsec;
        
        # this is 1-based because of when I assigned it in the row loop
        for (my $iPhoton = 1 ; $iPhoton <= $numAcceptedPhotons[$iRoll][$iTheta][$iEnergy] ; ++$iPhoton) {
          #print $fileHandleRadialDiagData $iPhoton . " " . $acceptedPhotonRadialValuesmm[$iRoll][$iTheta][$iEnergy][$iPhoton] . "\n";
          print $fileHandleRadialDiagData $iPhoton . "\t" . 
                  $acceptedPhotonRadialValuesArcsec[$iRoll][$iTheta][$iEnergy][$iPhoton] . "\t" . 
                  $acceptedPhotonPathcode[$iRoll][$iTheta][$iEnergy][$iPhoton] . "\t" . 
                  $acceptedPhotonRowNum[$iRoll][$iTheta][$iEnergy][$iPhoton] . "\n";
          print $fileHandleRadialDiagDatasorted $iPhoton . "\t" . $sortedArrayArcsec[$iPhoton] . "\n";
        }
      }
    }
  }
  

  # +++ 20150629 KLR throw the eefvalues into an output file (like IDL can put output into a file) so that
  #                  we can start plotting/manipulating/examining
  #                  just for diagnostic: just grab theta[0], roll[0], and every energy/rollbin
  my $diagnosticfile = "eefDiagnostic.dat";
  open my $fileHandleDiagData, ">", $diagnosticfile or die "Can't open '$diagnosticfile'\n";

  print $fileHandleDiagData "radialValues eefValues" . "\n";
  for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
    print $fileHandleDiagData "\n" . "Azimuth $iRoll = $unqAzimDeg[$iRoll]"  . "\n";
    for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
      print $fileHandleDiagData "Offaxis $iTheta = $unqOffaxisArcmin[$iTheta]"  . "\n";
      for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) {
        print $fileHandleDiagData "Energy $iEnergy = $unqEnergies[$iEnergy]"  . "\n";

        for (my $iRad = 0 ; $iRad < $numRadVals ; ++$iRad) {
          print $fileHandleDiagData $radialValues[$iRad] . " " .  $eefValues[$iRad][$iRoll][$iTheta][$iEnergy] . "\n";
        }

      } # end-for iEnergy
    } # end-for iTheta
  } # end-for iRoll
  close $fileHandleDiagData;
  
} # end-if debug, to print diagnostic ascii data files

if (ahlog::getdebug()) {
  print Data::Dumper->Dump( [ \@totalWeightedEnergy ], [ qw(*totalWeightedEnergy) ] );
}

# make sure that no entries in totalWeightedEnergy are 0, since we use that to
# normalize later.  If any are 0, the user likely used a history file too small
for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
  for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
    for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) {
      if ($totalWeightedEnergy[$iRoll][$iTheta][$iEnergy] == 0) {
        ahgen::ah_err "Some of the weighted energy values were zero.  Perhaps you used a history file too small?" ;
        ahapp::end_processing(1);
      }
    }
  }
}

# alert the user if there were any pathcodes with 8 interactions.  Since the
# history file only records 8 interactions, this photon may have had 
# interactions we're missing.  It's unlikely that a valid double reflection had 
# more than 8 interactions, so we are assuming that if there was a double 
# reflection with 8 interactions, that there wasn't another interaction that
# would invalidate that photon.
if ($numEightInteractions > 0) {
  ahlog::ah_info "LOW", "There were $numEightInteractions photons with at least $maxPathCodes interactions";
}

#for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
#  for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
#    for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) { 
#      ahlog::ah_debug ("totalWeightedEnergy[$iRoll][$iTheta][$iEnergy] = $totalWeightedEnergy[$iRoll][$iTheta][$iEnergy]");
#    }
#  }
#}

# go through all radial bins and normalize by the totalWeightedEnergy for that
# roll/theta/energy.  Remember that eefValues is a 4D array,
# by (radial bin, roll, theta, energy)
for (my $iRad = 0 ; $iRad < $numRadVals ; ++$iRad) {
  for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
    for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
      for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) { 
        $normEEFValues[$iRad][$iRoll][$iTheta][$iEnergy] = $eefValues[$iRad][$iRoll][$iTheta][$iEnergy] / $totalWeightedEnergy[$iRoll][$iTheta][$iEnergy];
      }
    }
  }
}
#
#for (my $iRad = 0 ; $iRad < $numRadVals ; ++$iRad) {
#  for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
#    for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
#      for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies; ++$iEnergy) {
#        #ahlog::ah_debug "normEEFValues[$iRad][$iRoll][$iTheta][$iEnergy]=%.6f \n", $normEEFValues[$iRad][$iRoll][$iTheta][$iEnergy];
#      }
#    }
#  }
#}


# initialize how many energies, etc we're going to use.  These are
# initialially set to the full number of unique values, but are reset inside
# createMergedEEF() to 1 if any of these values are to be merged
my $numEnergies     = $numUnqEnergies;
my $numOffaxis      = $numUnqOffaxis;
my $numAzimuth      = $numUnqAzimuth;

# now merge the requested dimensions (energy, offaxis, and/or azimuth)
#@finalEEF = @{createMergedEEF(\@normEEFValues, $numUnqEnergies, $numUnqOffaxis, $numUnqAzimuth)};
createMergedEEF(\@normEEFValues, \@totalpsfphotons, $numUnqEnergies, $numUnqOffaxis, $numUnqAzimuth, \@finalEEF, \@finalPSFPhotons);

writeOutputFile();




#########################
#  Finishing
#########################

# Add CHECKSUM and DATASUM, and check that file we just created is valid
unless (ahgen::update_checksum_and_verify($outfile)) {
  ahgen::ah_err "FITS file $outfile failed FITS verification test." ;
  ahapp::end_processing(1);
}
ahlog::ah_info "HIGH", "Successfully created FITS file: $outfile" ;

# We're done
ahapp::end_processing(0);





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

  #ahlog::ah_debug "---- start checkPathCode ---- \n";

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
  if (!$resultsPlane) {
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

#   ahlog::ah_debug "---- end checkPathCode ---- \n";
  
  return ($isDblRefl, $hasEightInteractions);
  
} # end-sub checkPathCode


#############################################################################


# \brief Creates merged array, if requested by user, from an array of EEF
#        values for each energy/offaxis/azimuth
# \param[in] mgEnergy
# \param[in] mgOffaxis
# \param[in] mgAzimuth
# \param[in] numRadVals
# \param[in] normEEFValues
# \param[in] totalpsfphotons
# \param[in] numUnqEnergies
# \param[in] numUnqOffaxis
# \param[in] numUnqAzim
# \param[out] finalEEF
# \param[out] finalPSFPhotons
#
# long desc
# 
sub createMergedEEF {
  
  ahlog::ah_debug "---- start createMergedEEF ---- \n";
  
  # -------------------------------------
  # define and initialize all variables
  # -------------------------------------
  
  # input parameters
  
  my( $normEEFValuesRef, $totalpsfphotonsRef, $numUnqEnergies, $numUnqOffaxis, $numUnqAzimuth, $finalEEFRef, $finalPSFPhotonsRef ) = @_;
  my @normEEFValuesArr = @{$normEEFValuesRef};
  my @totalpsfphotons = @{$totalpsfphotonsRef};
  my @finalEEFRef = @{$finalEEFRef};
  my @finalPSFPhotons = @{$finalPSFPhotonsRef};
  
  
  ahlog::ah_debug("numUnqEnergies=$numUnqEnergies numUnqOffaxis=$numUnqOffaxis numUnqAzimuth=$numUnqAzimuth");
  
  # mgEnergy, mgOffaxis, mgAzimuth, numRadVals are input params.
  # they were declared earlier, so didn't need to be passed in
  
  # numEnergies, numOffaxis, numAzimuth were declared earlier
  # these values will decide how many values we are to use, based on if
  # user requested to merge these values or not
  
  # These will be 4D arrays, holding the final EEF values
  # normEEFValues is the first 'input' - it's the (normalized) EEF array 
  # created by treating each energy/offaxis/roll separately
  # energyEEF will be the 'output' from merging energy values (if not merging 
  # energy values, then it will be an exact copy of normEEFValues).
  # offaxisEEF will be the 'output' from merging offaxis values, using 
  # energyEEF as 'input' (if not merging offaxis values, then offaxisEEF will 
  # be an exact copy of energyEEF)
  # finalEEF will be the 'output' from merging azimuthal values, using offaxis 
  # as 'input' (if not merging azimuthal values, then finalEEF will be an 
  # exact copy of offaxisEEF)
  # if merging on any of the values, there will be only be one entry for that 
  # applicable dimension.  For example, if merging on energy, the 
  # 4th dimension will only be size 1.  If not merging on energy, the 
  # 4th dimension will be the original size (numUnqEnergies).  This value is 
  # stored in numEnergies.
  # dimen: [$iRad][$iRoll][$iTheta][$iEnergy]
  my @energyEEF = ();
  my @offaxisEEF = ();
  
  # -------------------------------------
  
  # for each eefbin, calculate the final EEF 
  for (my $iRad = 0 ; $iRad < $numRadVals ; ++$iRad) {
  
    # ------- start merging energy
    
    for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
      for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
        
        if ($mgEnergy) {
          # we're merging energies, so make just one EEF extension for all
          # the energies
          # (there may or may not be extensions for each theta and roll)
          
          # the last dimension (energy) will only be size 1
          $numEnergies = 1;
          
          # initialize the eef for this roll/theta to 0, so we can add to it
          $energyEEF[$iRad][$iRoll][$iTheta][0] = 0;
          
          # sum all the energies that contributed
          for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
            $energyEEF[$iRad][$iRoll][$iTheta][0] += $normEEFValuesArr[$iRad][$iRoll][$iTheta][$iEnergy];
          } # end-loop through energies
          
          # now divide by total number energies, to get average
          $energyEEF[$iRad][$iRoll][$iTheta][0] /= $numUnqEnergies;
          
        } else {
          # we're not merging energies.  Make an EEF extension for each one
          # leave numEnergies at the default numUnqEnergies
          
          # copy all the energies that contributed
          for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
            $energyEEF[$iRad][$iRoll][$iTheta][$iEnergy] += $normEEFValuesArr[$iRad][$iRoll][$iTheta][$iEnergy];
          } # end-loop through energies
          
        } # end-if merging energy
        
      } # end-loop through theta
    } # end-loop through roll
    
    # ------- end merging energy
    
    # energyEEF is now either an exact copy of normEEFValues (if not merging 
    # energy) or all energy EEFs are combined into first entry of last
    # dimension (if we are merging energy)
  
    # ------- start merging offaxis
    
    # now see if we need to merge offaxis angles.  The 'output' from the energy 
    # part, energyEEF, will be the 'input' for the offaxis part.
    
    # roll is the outermost loop, we can have that outside the merging details
    for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
      
      if ($mgOffaxis) {
        # we're merging offaxis values, so make just one EEF extension for all 
        # the offaxis angles
        # (there may or may not be extensions for each energy and roll)
        
        $numOffaxis = 1;
        
        for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
          for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
            # remember that numEnergies is either 1 or numUnqEnergies,
            # depending on if we merged energies into one field or not
            $offaxisEEF[$iRad][$iRoll][0][$iEnergy] += $energyEEF[$iRad][$iRoll][$iTheta][$iEnergy];
          } # end-loop through energies
        } # end-loop through theta
        
        # now divide by total number theta, to get average
        for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
          $offaxisEEF[$iRad][$iRoll][0][$iEnergy] /= $numUnqOffaxis;
        } # end-loop through energies
        
      } else {
        # we're not merging offaxis.  Make an EEF extension for each one
        # leave numOffaxis as the default numUnqOffaxis
        
        # offaxisEEF will basically just be a copy of energyEEF
        for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
          for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
            # remember that numEnergies is either 1 or numUnqEnergies,
            # depending on if we merged energies into one field or not
            $offaxisEEF[$iRad][$iRoll][$iTheta][$iEnergy] = $energyEEF[$iRad][$iRoll][$iTheta][$iEnergy];
          } # end-loop through energies
        } # end-loop through theta
      
      } # end-if merging offaxis
      
    } # end-loop through roll
    
    # ------- end merging offaxis
  
    # offaxisEEF is now either an exact copy of energyEEF (if not merging 
    # offaxis) or all offaxis EEFs are combined into first entry of third 
    # (offaxis) dimension.  We will use this to create the finalEEF array, 
    # with azimuth
  
    # ------- start merging azimuth
  
    if ($mgAzimuth) {
      
      $numAzimuth = 1;
      
      for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
        # remember that numEnergies/numOffaxis are either 1 or
        # numUnqEnergies/numUnqOffaxis, depending on if we merged
        # energies/offaxis into one field or not
        for (my $iTheta = 0 ; $iTheta < $numOffaxis; ++$iTheta) {
          for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
            $finalEEF[$iRad][0][$iTheta][$iEnergy] += $offaxisEEF[$iRad][$iRoll][$iTheta][$iEnergy];
          } # end-loop through energies
        } # end-loop through theta
      } # end-loop through roll
      
      # now divide by total number azimuth, to get average
      for (my $iTheta = 0 ; $iTheta < $numOffaxis; ++$iTheta) {
        for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
          $finalEEF[$iRad][0][$iTheta][$iEnergy] /= $numUnqAzimuth;   # +++ finalEEF!
        } # end-loop through energies
      }
      
    } else {
      # we're not merging azimuth.  Make an EEF extension for each one
      # leave numAzim as the default numUnqAzim
      
      for (my $iRoll = 0 ; $iRoll < $numAzimuth; ++$iRoll) {
        # remember that numEnergies/numOffaxis are either 1 or
        # numUnqEnergies/numUnqOffaxis, depending on if we merged 
        # energies/offaxis into one field or not
        for (my $iTheta = 0 ; $iTheta < $numOffaxis ; ++$iTheta) {
          for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
            $finalEEF[$iRad][$iRoll][$iTheta][$iEnergy] = $offaxisEEF[$iRad][$iRoll][$iTheta][$iEnergy];
          } # end-loop through energies
        } # end-loop through theta
      } # end-loop through roll
      
    } # end-if merging azimuth
    
    # ------- end merging azimuth

  } # end-loop through eefbins
  
  # done creating finalEEF, after deciding what needs to be merged
  # (energy, offaxis, azimuth values)
  
  # ***********************************************
  # now we need to possibly merge totalpsfphotons, 
  # for each energy, offaxis, and roll
  # ***********************************************
  
  # +++ finish this
  
  # Similary to the arrays holding the final EEF values, these will hold the 
  # intermittent PSFPhoton arrays
  # totalpsfphotons is the first 'input', created by treating each 
  # energy/offaxis/roll separately
  # energyPhotons will be 'output' from merging energy values (if not merging 
  # energy values, it will be an exact copy of totalpsfphotons).
  # offaxisPhotons will be the 'output' from merging offaxis values, using 
  # energyPhotons as 'input' (if not merging offaxis values, offaxisPhotons will 
  # be an exact copy of energyPhotons)
  # finalPSFPhotons will be the 'output' from merging azimuthal values, using 
  # offaxis as 'input' (if not merging azimuthal values, finalPSFPhotons will be 
  # exact copy of offaxisPhotons)
  # if merging on any of the values, there will be only be one entry for that 
  # applicable dimension.  For example, if merging on energy, the 
  # 3rd dimension will only be size 1.  If not merging on energy, the 
  # 3rd dimension will be the original size (numUnqEnergies).  This value is
  # already stored in numEnergies.
  # dimen: [$iRoll][$iTheta][$iEnergy]
  my @energyPhotons = ();
  my @offaxisPhotons = ();
  
  # ------- start merging energy
    
  for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
    for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
      
      if ($mgEnergy) {
      
        # initialize the eef for this roll/theta to 0, so we can add to it
        # +++ I should probably initialize all of these (for eef too) before these if-blocks
        $energyPhotons[$iRoll][$iTheta][0] = 0;
        
        # sum all the energy photons that contributed
        for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
          $energyPhotons[$iRoll][$iTheta][0] += $totalpsfphotons[$iRoll][$iTheta][$iEnergy];
        }
        
      } else {
        # we're not merging energies
        
        # copy all the energy photons that contributed
        for (my $iEnergy = 0 ; $iEnergy < $numUnqEnergies ; ++$iEnergy) {
          $energyPhotons[$iRoll][$iTheta][$iEnergy] += $totalpsfphotons[$iRoll][$iTheta][$iEnergy];
        } # end-loop through energies
        
      } # end-if merging energy
      
    } # end-loop through theta
  } # end-loop through roll
  
  # ------- end merging energy
  
  # energyPhotons is now either an exact copy of totalpsfphotons (if not 
  # merging energy) or all energy photons are combined into first entry of last
  # dimension (if we are merging energy)

  # ------- start merging offaxis
  
  # now see if we need to merge offaxis angles.  The 'output' from the energy 
  # part, energyPhotons, will be the 'input' for the offaxis part.
  
  # roll is the outermost loop, we can have that outside the merging details
  for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
    
    if ($mgOffaxis) {
      
      for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
        for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
          # remember that numEnergies is either 1 or numUnqEnergies,
          # depending on if we merged energies into one field or not
          $offaxisPhotons[$iRoll][0][$iEnergy] += $energyPhotons[$iRoll][$iTheta][$iEnergy];
        } # end-loop through energies
      } # end-loop through theta
      
    } else {
      # we're not merging offaxis
      
      # offaxisPhotons will basically just be a copy of energyPhotons
      for (my $iTheta = 0 ; $iTheta < $numUnqOffaxis; ++$iTheta) {
        for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
          # remember that numEnergies is either 1 or numUnqEnergies,
          # depending on if we merged energies into one field or not
          $offaxisPhotons[$iRoll][$iTheta][$iEnergy] = $energyPhotons[$iRoll][$iTheta][$iEnergy];
        } # end-loop through energies
      } # end-loop through theta
    
    } # end-if merging offaxis
    
  } # end-loop through roll
  
  # ------- end merging offaxis
  
  # offaxisPhotons is now either an exact copy of energyPhotons (if not merging 
  # offaxis) or all offaxis photons are combined into first entry of third 
  # (offaxis) dimension.  We will use this to create the finalPSFPhotons array, 
  # with azimuth

  # ------- start merging azimuth

  if ($mgAzimuth) {
    
    $numAzimuth = 1;
    
    for (my $iRoll = 0 ; $iRoll < $numUnqAzimuth; ++$iRoll) {
      # remember that numEnergies/numOffaxis are either 1 or
      # numUnqEnergies/numUnqOffaxis, depending on if we merged
      # energies/offaxis into one field or not
      for (my $iTheta = 0 ; $iTheta < $numOffaxis; ++$iTheta) {
        for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
          $finalPSFPhotons[0][$iTheta][$iEnergy] += $offaxisPhotons[$iRoll][$iTheta][$iEnergy];
        } # end-loop through energies
      } # end-loop through theta
    } # end-loop through roll
    
  } else {
    # we're not merging azimuth
    
    for (my $iRoll = 0 ; $iRoll < $numAzimuth; ++$iRoll) {
      # remember that numEnergies/numOffaxis are either 1 or
      # numUnqEnergies/numUnqOffaxis, depending on if we merged 
      # energies/offaxis into one field or not
      for (my $iTheta = 0 ; $iTheta < $numOffaxis ; ++$iTheta) {
        for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
          $finalPSFPhotons[$iRoll][$iTheta][$iEnergy] = $offaxisPhotons[$iRoll][$iTheta][$iEnergy];
        } # end-loop through energies
      } # end-loop through theta
    } # end-loop through roll
    
  } # end-if merging azimuth
  
  # ------- end merging azimuth
  
  # done creating finalPSFPhotons, after deciding what needs to be merged
  # (energy, offaxis, azimuth values)
  

  ahlog::ah_debug "---- end createMergedEEF ---- \n";
  
  # includes a return reference to the output array
  
} # end-sub createMergedEEF()


#############################################################################

# \brief Write the output EEF FITS file from the array of EEF values
# \param[in] mgEnergy if user requested to merge energy values
# \param[in] mgOffaxis if user requested to merge offaxis values
# \param[in] mgAzimuth if user requested to merge azimuthal values
# \param[in] numRadVals numnber of radial bins
# \param[in] outfile name of output file
# \param[in] finalEEF 4dim array of EEF values
# \param[in] radialValues array of radial values
# \param[in] xCenters 2D array of center x values for each azimuth and offaxis
# \param[in] yCenters 2D array of center y values for each azimuth and offaxis
# \param[in] numEnergies
# \param[in] numOffaxis
# \param[in] numAzimuth
# \param[in] unqEnergies
# \param[in] unqOffaxisArcmin
# \param[in] unqAzimDeg
#
# long desc
# 
sub writeOutputFile {

  ahlog::ah_debug "---- start writeOutputFile ---- \n";

  # -------------------------------------
  # define and initialize all variables
  # -------------------------------------
  
  # mgEnergy, mgOffaxis, mgAzimuth, numRadVals are input params.
  # they were declared with 'my' earlier, so didn't need to be passed in
  
  # this will be updated inside each loop iteration, to print current
  # extension number as we create it
  my $extNum = 0;
  
  # a slice of the eef will be written to an extension.
  # size: numRadVals
  my @eefSlice;
  
  # in Perl, using ftcreate, can only create one file/extension at a time.  Add
  # those names to this array.  Then we'll merge them all into one file using 
  # ftmerge after the loops
  my @outfiles;
  
  # for CBD10001, CBD20001, CBD30001, and OFFAXIS, AZIMUTH, Energy keywords
  my $cbdOffValues;
  my $cbdAzimValues;
  my $cbdEnergyValues;
  my $offValues;
  my $azimValues;
  my $energyValues;
  
  # write either current xCenter and yCenter for theta and roll,
  # or if either of those were merged, write a range
  my $minXCenter;
  my $maxXCenter;
  my $minYCenter;
  my $maxYCenter;
  my $xCenterToWrite;
  my $yCenterToWrite;

  # -------------------------------------
  
  # if either offaxis or azimuth were merged, then the xCenter and yCenter 
  # keywords don't entirely make sense.  In this case, write a range:
  # XCENTER = min(xCenters) - max(xCenters)
  # YCENTER = min(yCenters) - max(yCenters)
  # xCenters and yCenters are 2D arrays, so do this in a 2D loop 
  # (min and max are for 1D arrays)
  if ($mgOffaxis || $mgAzimuth) {
  
    $minXCenter = $xCenters[0][0];
    $maxXCenter = $xCenters[0][0];
    $minYCenter = $yCenters[0][0];
    $maxYCenter = $yCenters[0][0];

    for (my $iAzim = 1 ; $iAzim < $numAzimuth ; ++$iAzim) {
      for (my $iOff = 1 ; $iOff < $numOffaxis; ++$iOff) {
        if ($xCenters[$iAzim][$iOff] < $minXCenter) {
          $minXCenter = $xCenters[$iAzim][$iOff]
        }
        if ($xCenters[$iAzim][$iOff] > $maxXCenter) {
          $maxXCenter = $xCenters[$iAzim][$iOff]
        }
        if ($yCenters[$iAzim][$iOff] < $minYCenter) {
          $minYCenter = $yCenters[$iAzim][$iOff]
        }
        if ($yCenters[$iAzim][$iOff] > $maxYCenter) {
          $maxYCenter = $yCenters[$iAzim][$iOff]
        }
      }
    }
  }
  
  # create an extension for each requested roll/theta/energy.  Any combination
  # of these may be merged into one extension for that variable(s).
  # note the end limit for the loops was initially set to the full number of 
  # unique values (ie: $numAzimuth was initialized to $numUnqAzimuth), but was
  # reset to 1 inside createMergedEEF() if any were merged
  for (my $iAzim = 0 ; $iAzim < $numAzimuth ; ++$iAzim) {
    for (my $iOff = 0 ; $iOff < $numOffaxis; ++$iOff) {
      for (my $iEnergy = 0 ; $iEnergy < $numEnergies ; ++$iEnergy) {
        
        # we're at a new extension
        ++$extNum;
        ahlog::ah_debug "extNum=$extNum iAzim=$iAzim iOff=$iOff iEnergy=$iEnergy \n";
        
        # current name of this EEF file (one for each extension, until combine)
        my $currOutfile = $outfile . ".extension" . $extNum;
        push @outfiles, $currOutfile;
        ahapp::add_temp_file($currOutfile);
        
        # name of extension in output EEF file
        my $extname = "EEF" . $extNum;
        
        # grab the necessary slice of the EEF for this extension, overwriting
        # slice from previous loop iteration
        for (my $iRad = 0 ; $iRad < $numRadVals ; ++$iRad) {
          $eefSlice[$iRad] = $finalEEF[$iRad][$iAzim][$iOff][$iEnergy];
        }
        
        # CBD10001, CBD20001, CBD30001, and OFFAXIS, AZIMUTH, Energy keywords
        # if merging:
        #     CBD keywords are a range from first unique value to last
        #     OFFAXIS, etc are the average of the unique values
        # if not merging: both keywords are the value at this iteration
        # +++ KLR add this to TRF description about output file
        if ($mgOffaxis) {
          $cbdOffValues = $unqOffaxisArcmin[0] . "-" . $unqOffaxisArcmin[$numUnqOffaxis-1]; 
          $offValues = average(@unqOffaxisArcmin);
        } else {
          $cbdOffValues = $unqOffaxisArcmin[$iOff];
          $offValues = $cbdOffValues;
        }
        if ($mgAzimuth) {
          $cbdAzimValues = $unqAzimDeg[0] . "-" . $unqAzimDeg[$numUnqAzimuth-1];
          $azimValues = average(@unqAzimDeg);
        } else {
          $cbdAzimValues = $unqAzimDeg[$iAzim];
          $azimValues = $cbdAzimValues;
        }
        if ($mgEnergy) {
          $cbdEnergyValues = $unqEnergies[0] . "-" . $unqEnergies[$numUnqEnergies-1]; 
          $energyValues = average(@unqEnergies);
        } else {
          $cbdEnergyValues = $unqEnergies[$iEnergy];
          $energyValues = $cbdEnergyValues;
        }
        my $cbdOffaxs   = "THETA(" . $cbdOffValues . ")[arcmin]"; 
        my $cbdAzim     = "PHI(" . $cbdAzimValues . ")[deg]";
        my $cbdEnergy   = "ENERG(" . $cbdEnergyValues . ")[keV]";
        
        # +++ KLR put a comment in TRF, about if file was merged what this value is
        my $TOTCTS = $totalpsfphotons[$iAzim][$iOff][$iEnergy];
        
        # create text files to hold the column definitions, column data, and keywords
        # ftcreate needs these to create the output file
        my $columndeffile = "tmp_columns.txt";
        my $datafile      = "tmp_data.txt";
        my $headfile      = "tmp_head.txt";
        
        # add these to tmparray to cleanup later
        ahapp::add_temp_file($columndeffile);
        ahapp::add_temp_file($datafile);
        ahapp::add_temp_file($headfile);
        
        # create the column definition file, overwriting if exists
        open my $fileHandleColumn, ">", $columndeffile or die "Can't open '$columndeffile'\n";
        print $fileHandleColumn "Radius 1D arcsec\n";
        print $fileHandleColumn "EEF 1D\n";
        close $fileHandleColumn;
        
        # create the column data file, overwriting if exists
        open my $fileHandleData, ">", $datafile or die "Can't open '$datafile'\n";
        for (my $iRad = 0 ; $iRad < $numRadVals ; ++$iRad) {
          print $fileHandleData $radialValues[$iRad] . " " . $eefSlice[$iRad] . "\n";
        }
        close $fileHandleData;
        
        # write either this current xCenter and yCenter for this theta and roll,
        # or if either of those were merged, write a range (calculated min
        # and max earlier)
        if ($mgOffaxis || $mgAzimuth) {
          $xCenterToWrite = "$minXCenter - $maxXCenter";
          $yCenterToWrite = "$minYCenter - $maxYCenter";
        } else {
          # not merging.  can write the actual value
          $xCenterToWrite = "$xCenters[$iAzim][$iOff]";
          $yCenterToWrite = "$yCenters[$iAzim][$iOff]";
        }
        
        # create the column definition file, overwriting if exists
        open my $fileHandleHeader, ">", $headfile or die "Can't open '$headfile'\n";
        print $fileHandleHeader "TELESCOP  $telescop                /    Mission name\n";
        print $fileHandleHeader "INSTRUME  $instrume                /    Instrument name\n";
        print $fileHandleHeader "DETNAM    $detnam                  /    Telescope name\n";
        print $fileHandleHeader "CVSD0001  $cvsd0001                /    UTC date when file should be first used\n";
        print $fileHandleHeader "CVST0001  $cvst0001                /    UTC time when file should be first used\n";
        print $fileHandleHeader "CBD10001  $cbdOffaxs               /    Offaxis angle\n";
        print $fileHandleHeader "CBD20001  $cbdAzim                 /    Azimuthal roll angle\n";
        print $fileHandleHeader "CBD30001  $cbdEnergy               /    Energy values\n";
        print $fileHandleHeader "OFFAXIS   $offValues               /    [arcmin] Source off-axis angle\n";
        print $fileHandleHeader "AZIMUTH   $azimValues              /    [deg] Source rotational (azimuthal) angle\n";
        print $fileHandleHeader "Energy    $energyValues            /    [keV] Energy or energy range of image\n";
        print $fileHandleHeader "NOFFAXIS  $numUnqOffaxis           /    Number of off-axis angles\n";
        print $fileHandleHeader "NAZIMUTH  $numUnqAzimuth           /    Number of azimuthal angles\n";
        print $fileHandleHeader "NORMRAD   $normRadmm               /    [mm] Normalization radius\n";
        print $fileHandleHeader "TOTCTS    $TOTCTS                  /    Total number of photons in EEF\n";
        print $fileHandleHeader "FPMM2AM   $fpmm2am                 /    Focal Plane (mm) to arcminutes scale factor\n";
        print $fileHandleHeader "XCENTER   $xCenterToWrite          /    Source center X-coordinate\n";
        print $fileHandleHeader "YCENTER   $yCenterToWrite          /    Source center Y-coordinate\n";
        print $fileHandleHeader "1CTYP2    'Radial distance'        /    Radial distance from peak\n";
        print $fileHandleHeader "1CUNI1    arcsec                   /    Units of radial distance from peak\n";
        print $fileHandleHeader "1CRPX1    1                        /    Reference pixel corresponding to start radius\n";
        print $fileHandleHeader "1CRVL1    0.0                      /    Radial distance at the reference pixel\n";
        print $fileHandleHeader "1CDLT1    $deltaRadVals            /    Radial distance increment per pixel\n";
        print $fileHandleHeader "MGOFFAXS  $mgOffaxisBool           /    Offaxis angles were merged or not\n";
        print $fileHandleHeader "MGAZIM    $mgAzimuthBool           /    Azimuthal angles were merged or not\n";
        print $fileHandleHeader "MGENERGY  $mgEnergyBool            /    Energies were merged or not\n";
        print $fileHandleHeader "EVTFILE   '$infile'                \n";   # +++ KLR update this when/if CALDB
        print $fileHandleHeader "COMMENT   EVTFILE = Input history event file\n";
        close $fileHandleHeader;
      
        # create the output file
        $status = ahgen::run_ftool("ftcreate",
                         "cdfile=$columndeffile",
                         "datafile=$datafile",
                         "outfile=$currOutfile",
                         "headfile=$headfile",
                         "extname=$extname",
                         "clobber=yes");
        if ($status) {
          ahlog::ah_err "Error creating file $currOutfile";
          ahapp::end_processing(1);
        }
        
        # add comments to the TTYPEn keywords
        $status = ahgen::run_ftool("fthedit",
                         "infile=$currOutfile",
                         "keyword=TTYPE1",
                         "operation=add",
                         "value=Radius",
                         "comment=Enclosed Radius");
        $status = ahgen::run_ftool("fthedit",
                         "infile=$currOutfile",
                         "keyword=TTYPE2",
                         "operation=add",
                         "value=EEF",
                         "comment=Enclosed Energy Function");
        if ($status) {
          ahlog::ah_err "Error editing TTYPEn keywords in $currOutfile";
          ahapp::end_processing(1);
        }
        
        # +++ 20150527 KLR This step seems to take a lot of time
        # Add parameters as HISTORY keywords to current extension
        if ($ahapp::history) {
          #ahlog::ah_debug "before history: write_parameters \n";
          ahapp::write_parameters($currOutfile, $extname);
          #ahlog::ah_debug "after history: write_parameters \n";
        }
        
      } # end-loop through enegy 
    } # end-loop through offaxis 
  } # end-loop through azimuth 
  # done writing data extensions (as separate files so far)
  
  ahlog::ah_debug "out of loops \n";
  
  # grab the first file we made, and make it the regular outfile
  $status = ahgen::run_ftool("ftcopy",
                  "infile=$outfiles[0]",
                  "outfile=$outfile",
                  "clobber=$clobberBool");
  if ($status) {
    ahlog::ah_err "Error creating file $outfile with first extension";
    ahapp::end_processing(1);
  }
  
  # now merge all the rest of the files into the outfile
  for (my $iFile=1 ; $iFile < $extNum ; ++$iFile) {
      $status = ahgen::run_ftool("ftappend",
                    "infile=$outfiles[$iFile]",
                    "outfile=$outfile");
    if ($status) {
      ahlog::ah_err "Error adding extension $iFile to $outfile";
      ahapp::end_processing(1);
    }
  }
  
  # go to primary extension to write some additional keywords
  ahgen::set_keyword($outfile, 0, "NOFFAXIS", $numUnqOffaxis . " / Number of off-axis angles");
  ahgen::set_keyword($outfile, 0, "NAZIMUTH", $numUnqAzimuth . " / Number of azimuthal angles");
  ahgen::set_keyword($outfile, 0, "NORMRAD", $normRadmm . " / [mm] Normalization radius");


  ahlog::ah_debug "---- end writeOutputFile ---- \n";

} # end sub writeOutputFile()


#############################################################################




# Revision Log:
# $Log: eeftable.pl,v $
# Revision 1.6  2016/03/23 01:59:57  klrutkow
# case-insensitive comparison to SRCMDL keyword and resultsPlaneOnly
#id:$ &
#

