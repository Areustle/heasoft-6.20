#!/usr/bin/perl
#
# File name: aharfgen.pl
# Author:    Michael Dutka
# $Date: 2017/01/13 23:42:30 $
# Version: 1.0
#
# This Perl script will generate ARF for SXS, SXI, HXI1, HXI2 (for HXI the 
# output is an RSP file = ARF*RMF). The scirpt drive the tools ahsxtarfgen for
# SXS and SXI) or ahhxiarfgen (for HXI1 and HXI2) 
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
#  1.0   2015-09-02  MSD     adding new tool aharfgen  
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
use ahapp ();           # $clobber $chatter $logfile $debug $history $mode

use File::Copy;
use List::Util qw(first);
use Scalar::Util qw(looks_like_number);
use ahfilterlib ;

use Data::Dumper;       # to output arrays for debugging
use POSIX "fmod";       # floating point modulo operator
use Math::Trig;     # Pi and trig functions


########################
#  Constants
#########################
use constant DEG2RAD => pi/180.0;
use constant RAD2DEG => 180.0/pi;
use constant ergtokeV => 1.60217662e-9;
use constant twopi => 2.0 * pi;
use constant RAD2ARCMIN => 3437.746675120923101;

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
my $xrtevtfile       = ahapp::query_parameter("xrtevtfile");
my $source_ra        = ahapp::query_parameter("source_ra");
my $source_dec       = ahapp::query_parameter("source_dec");
my @nompntpars       = split(" ",ahapp::query_parameter("nompntpars"));
my $telescop         = ahapp::query_parameter("telescop");
my $instrume         = ahapp::query_parameter("instrume");
my $teldeffile       = ahapp::query_parameter("teldeffile");
my $dattfile         = ahapp::query_parameter("dattfile");
my $filtoffsetfile   = ahapp::query_parameter("filtoffsetfile");
my $emapfile         = ahapp::query_parameter("emapfile") ;
my $qefile           = ahapp::query_parameter("qefile");
my $obffile          = ahapp::query_parameter("obffile");
my $fwfile           = ahapp::query_parameter("fwfile");
my $contamifile      = ahapp::query_parameter("contamifile");
my $abund            = ahapp::query_parameter("abund");
my $cols             = ahapp::query_parameter("cols");
my $covfac           = ahapp::query_parameter("covfac");
my $gatevalvefile    = ahapp::query_parameter("gatevalvefile");
my $sampling         = ahapp::query_parameter("sampling");
my $baffle           = ahapp::query_parameter("baffle");
my $rmffile          = ahapp::query_parameter("rmffile");
my @erange           = split(" ",ahapp::query_parameter("erange"));
my $onaxisffile      = ahapp::query_parameter("onaxisffile");
my $onaxiscfile      = ahapp::query_parameter("onaxiscfile");
my $polydeg          = ahapp::query_parameter("polydeg");
my $outfile          = ahapp::query_parameter("outfile");
my $regmode          = ahapp::query_parameter("regmode");
my $regionfile       = ahapp::query_parameter("regionfile");
my $mirrorfile       = ahapp::query_parameter("mirrorfile");
my $obstructfile     = ahapp::query_parameter("obstructfile");
my $frontreffile     = ahapp::query_parameter("frontreffile");
my $backreffile      = ahapp::query_parameter("backreffile") ;
my $pcolreffile      = ahapp::query_parameter("pcolreffile");
my $scatterfile      = ahapp::query_parameter("scatterfile");
my $numphoton        = ahapp::query_parameter("numphoton");
my $minphoton        = ahapp::query_parameter("minphoton");
my $sourcetype       = ahapp::query_parameter("sourcetype");
my $betapars         = ahapp::query_parameter("betapars");
my $flatradius       = ahapp::query_parameter("flatradius");
my $imgfile          = ahapp::query_parameter("imgfile");
my $auxtransfile     = ahapp::query_parameter("auxtransfile");
my $rmfthresh        = ahapp::query_parameter("rmfthresh");
my $seed             = ahapp::query_parameter("seed");
my $clobber          = $ahapp::clobber ? "yes" : "no";


# Write all parameters to the log file.
ahlog::ah_info "HIGH", ahapp::write_parameters () ;

#########################
#  Other Variables 
#########################

# string region file name passed to ahsxtarfgen
my $regionfilename;

# RMF file broken into name and extension
my @rmffile_parsed = ();

#temp variables
my $outxx = 0.0;
my $outyy = 0.0;
my @coorpntphi = ();

my $status = 0;

#Optical axis keywords
my $optaxisx = 0.0;
my $optaxisy = 0.0;
my $rotd = 0.0;
my $optxflip = 0.0;
my $optyflip = 0.0;
my $optxyflipratio = 0.0;

my $xmmperpixel = 0.0;
my $ymmperpixel = 0.0;
my $xmmperpixsq = 0.0;
my $ymmperpixsq = 0.0;
my $focallen = 0.0;

my $sinrotd = 0.0;
my $cosrotd = 0.0;

#simulator coordinate variables
my $simdetx = 0.0;
my $simdety = 0.0;
my $deltx = 0.0;
my $delty = 0.0;
my @simoffaxisval = ();

#number of exposure maps
my $numexpmaps = 0;
 
#gate valve keyowrds
my $gatevalvekeyword = "OPEN";

#number off axis angles, should be equal to number of exposure maps
my $numoffaxis = 0;

#offaxisval is in arcmin phangles is in degrees
my @offaxislo = ();
my @offaxishi = ();
my @offaxisval = ();
my @tmpoffaxisval = ();
my @phiangles = ();
my @timeinterval = ();
my $offaxisstring = "";


#exposure map fraction column OFFAXISHIST extension 
my @expfraction = ();

#new region file names created by coorpnt
my $currnewregionfile;
my @newregionfiles = ();

#sum of the azimuthal angles
my $sumofazimuth = 0.0;

#mean azimuthal roll angel
my $meanxrtphi = 0.0;
my $phistring = ""; 

#Boolean for running ray trace
my $runraytrace = 0; 

#Boolen doexpmap
my $doexpmap = 0;

#outx and outy parameters in coordpnt
my $outx = "";
my $outy = "";

#number of attitude groups
my $numattgrps = 1;

#energy units string
my $eunits;

# source detx and dety
my @srcdetx = ();
my @srcdety = ();

# Keywords exposure map
my $ra_nom = 0.0;
my $dec_nom = 0.0;
my $pa_nom = 0.0;

# New variables 02-24-16
my $cos_dec_src = 0.0;
my $ytop = 0.0;
my $xbot = 0.0;
my $phival = 0.0;

#Intemediate step offaxisval and phiangel calculation
my $top = 0.0;
my $bot = 0.0;
my @odeltx = ();
my @odelty = ();

# Used to establish lower and upper indices of each attitude
# group & the number in each group 
my @attgrpindxlo = ();
my @attgrpindxhi = ();
my @attgrpnumvals = ();
my @offaxisattgrp = ();

# used to determine mean offaxis angle per group
my $tmpsumoffaxis = 0.0;
my @meanattgrpoffaxis = ();

# Following are the nominal RA and DEC pointing and roll, per exposure map
my @ra_nomxp = ();
my @dec_nomxp = ();
my @pa_nomxp = ();
my @ra_pnt = ();
my @dec_pnt = ();

# Follow variables store the number in parameter erange
my $inputemin = 0.0;
my $inputemax = 0.0;
my $imgelo = 0.0;
my $imgehi = 0.0;
my $deltaimge = 0.0;

# Used to calculate DETX and DETY of any source (srctype = IMAGE option)
my @detxcoef0 = ();
my @detxcoef1 = ();
my @detxcoef2 = ();
my @detycoef0 = ();
my @detycoef1 = ();
my @detycoef2 = ();

#Variables use in determining the above coefficients
my $del_ra_pnt = 0.0;
my $del_dec_pnt = 0.0;
my $ra1 = 0.0; 
my $ra2 = 0.0; 
my $ra3 = 0.0;
my $ra12 = 0.0;
my $ra23 = 0.0;
my $dec1 = 0.0; 
my $dec2 = 0.0;
my $dec3 = 0.0; 
my $dec12 = 0.0;
my $dec23 =0.0;

my $detx1 = 0.0;
my $dety1 = 0.0;
my $detx2 = 0.0;
my $dety2 = 0.0;
my $detx3 = 0.0;
my $dety3 = 0.0;
my $detx12 = 0.0;
my $detx23 = 0.0;
my $dety12 = 0.0;
my $dety23 = 0.0;

# Fractional exposure, number of offaxis angel in raytracing file and number of 
# raytracing pairs
my @tfractionratio = ();
my $rtnumoffaxis = 0;
my $numrtpairs = ();

# intermediate steps in calculating simoffsetphi/theta
my $detxsim = 0.0; 
my $detysim = 0.0;
my $detxsimdelt = 0.0;
my $detysimdelt = 0.0;
my $tantheta = 0.0;
my $ynum = 0.0;
my $xden = 0.0; 
my $simphi = 0.0;
 
# simulated energy factor
my $simenergyfac = 0;

# Raytracing pair parameters
my @pairindexlo = ();
my @pairindexhi = ();
my @rtoffaxisval = ();
my @rtphiangles = ();
#Pointer to original index; might not be used
my @rtoffaxispointer = ();

# Pairs of raytracing events
my @numrtpairs = ();

# Variable for total exposure time and arrays for sin, cos of the critical 
# angles
my $totalexposure = 0.0;
my @cos_dec_pnt = ();
my @ra_pnt_nom = ();
my @dec_pnt_nom = ();
my @sin_ra_pnt_nom = ();
my @cos_ra_pnt_nom = ();
my @sin_dec_pnt_nom = ();
my @cos_dec_pnt_nom = ();
my $sin_ra_nom = 0.0;
my $cos_ra_nom = 0.0;
my $sin_dec_nom = 0.0;
my $cos_dec_nom = 0.0;
my $sin_pa_nom = 0.0;
my $cos_pa_nom = 0.0;
my @sin_pa_nomxp = ();
my @cos_pa_nomxp = ();

#Old full energy grid file parameters
my $numfullens = 0;
my @ecoarsefull = ();

# New energy grid file parameters
my $jesublo = 0;
my $jesubhi = 0;
my $numsubens = 0;
my $numrows = 0; 
my $numebinscoarse = 0;
my @ecoarsecen = 0;

#Dummy values given to heasim
my $cgsflux = 0.0;
my $constantarea = 0.0; 
my $simenergy = 0.0;
my $srcstring;

my $photonlist_coldef;

#Input pointing coordinates for heasim
my $rapoint = 0.0;
my $decpoint = 0.0;
my $roll = 0.0;

#source definition file given to simulator
my $srcfile;

#output mdb file created by simulator
my $mdbfile;

# Dummy arf file parameters
my $ne = 0.0; 
my @arf_elo = ();
my @arf_ehi = ();

# Simulator variables
my $numsimevents = 0;
my @rasim = ();
my @decsim = ();
my @sin_ra_sim = ();
my @cos_ra_sim = ();
my @sin_dec_sim = ();
my @cos_dec_sim = ();

# Off axis angles
my @simoffsettheta;
my @simoffsetphi;

#Quantities for converting SKY X,Y columns in heasim_events.fits to RA, DEC
my $ra_ref = 0.0; 
my $ra_refpixel = 0;
my $ra_delta = 0.0;
my $dec_ref = 0.0;
my $dec_refpixel = 0;
my $dec_delta = 0.0;
my @skyx = ();
my @skyy = ();

# Instrument keyword in auxtransfile
my $auxinstrume = "";

#########################
#  Main Code Block 
#########################

 
# Perform caldb query for coarse energy grid file 
if (uc $onaxiscfile eq "CALDB") {
  ahlog::ah_info "HIGH", "Querying CALDB for on axis coarse effective area file. \n";
  $onaxiscfile = ahfilterlib::call_quzcif($onaxiscfile,$instrume,"-",
                                          "EFFAREACRS","-","-",$telescop);
  if(ahgen::get_error_flag()) {
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1); 
  }
}

if (((uc $instrume eq "SXI") or (uc $instrume eq "SXS")) && (uc $rmffile eq "CALDB")) {
  ahlog::ah_err "Instrument is SXI or SXS and rmffile is CALDB, there is not a general RMF file for these instruments please specify one.";
  ahapp::end_processing(1);
}  

# Perform caldb query for RMF file only if instrument = HXI1 || HXI2
# and sourcetype = IMAGE 
if ((uc $sourcetype eq "IMAGE") && ((uc $instrume eq "HXI1") or (uc $instrume eq "HXI2"))) { 
  ahlog::ah_info "HIGH", "Querying CALDB for HXI response matrix file. \n";
  $rmffile = ahfilterlib::call_quzcif($rmffile,$instrume,"-",
                                      "RMF_LAYER0","-","-",$telescop);
  if(ahgen::get_error_flag()) {
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1); 
  }


}

# Perform caldb query for auxtransfile file 
if (uc $auxtransfile eq "CALDB") {
  ahlog::ah_info "HIGH", "Querying CALDB for auxiliary transmission file. \n";
  $auxtransfile = ahfilterlib::call_quzcif($auxtransfile,$instrume,"-",
                                          "AUXTRAN","-","-",$telescop);
  if(ahgen::get_error_flag()) {
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1); 
  }
} 


# Parse coarse energy grid file
my @onaxiscfile_parsed = ahgen::parse_file_name($onaxiscfile);

# Convert input string parameter erange to two numbers, or 4 numbers if we are 
# doing the extended source option that runs heasim:
$inputemin = $erange[0];
$inputemax = $erange[1];
if (uc $sourcetype eq "IMAGE") {
  $imgelo = $erange[2];
  $imgehi = $erange[3];
}

#Detemine if emapfile is an instrument map file or and exposure map file by 
#looking for and extension called OFFAXISHIST
if (-e $emapfile) {
  if (ahgen::check_hdu_exists($emapfile,"OFFAXISHIST")) {
    $doexpmap = 1;
    $numexpmaps = ahgen::get_keyword($emapfile,"OFFAXISHIST","NUMEXMAP");
  } else {
    $doexpmap = 0;
    $numexpmaps = 1;
    $numoffaxis = 1;
    if (uc $instrume eq "SXS") {
      $gatevalvekeyword = "CLOSED";
    }
  }
} else {
  ahlog::ah_err "File specified by emapfile parameter does not exist, aborting...";
  ahlog::ah_err ahgen::get_tool_stderr;
  ahapp::end_processing(1);
}  
 
if ($doexpmap) {
  if (uc $instrume eq "SXS") {
    $gatevalvekeyword = ahgen::get_keyword($emapfile,"OFFAXISHIST","GATEVALV");
    # In the following gatevalvefile is obtained directly 
    #from the input .par file
    if ((uc $gatevalvekeyword eq "CLOSED") && (uc $gatevalvefile eq "NONE")) {
      ahlog::ah_err "Gate valve is closed but you have not specified an input gate valve file";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
    if ((uc $gatevalvekeyword eq "OPEN") && (uc $gatevalvefile eq "NONE")) {
      ahlog::ah_err "Gate valve is open – ignoring the gate valve file";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
  } # end of if-block checking if instrume==SXS

  # Keyword numoffaxis
  $numoffaxis = ahgen::get_keyword($emapfile,"OFFAXISHIST","NAXIS2");

  # Read keywords for the pointing averaged over the whole observation
  $ra_nom = ahgen::get_keyword($emapfile,"OFFAXISHIST","RA_NOM");
  $dec_nom = ahgen::get_keyword($emapfile,"OFFAXISHIST","DEC_NOM");  
  $pa_nom = ahgen::get_keyword($emapfile,"OFFAXISHIST","PA_NOM");
  # Read optical axis keywords
  $optaxisx = ahgen::get_keyword($emapfile,"OFFAXISHIST","OPTAXISX");
  $optaxisy = ahgen::get_keyword($emapfile,"OFFAXISHIST","OPTAXISY");
  $rotd = ahgen::get_keyword($emapfile,"OFFAXISHIST","OPT_ROTD");
  $optxflip = ahgen::get_keyword($emapfile,"OFFAXISHIST","OPTXFLIP");
  $optyflip = ahgen::get_keyword($emapfile,"OFFAXISHIST","OPTYFLIP");
  $xmmperpixel = ahgen::get_keyword($emapfile,"OFFAXISHIST","DET_XSCL");
  $ymmperpixel = ahgen::get_keyword($emapfile,"OFFAXISHIST","DET_YSCL"); 
  $focallen = ahgen::get_keyword($emapfile,"OFFAXISHIST","FOCALLEN");

  $sinrotd = sin($rotd * DEG2RAD);
  $cosrotd = cos($rotd * DEG2RAD);
  
  $optxyflipratio = $optxflip/$optyflip;

  $xmmperpixsq = $xmmperpixel * $xmmperpixel;
  $ymmperpixsq = $ymmperpixel * $ymmperpixel;
 
  

  #get nominal RA and DEC pointing and roll angle for each exposure map
  @ra_nomxp = ahgen::read_column($emapfile,"OFFAXISHIST","RANOMXP");
  @dec_nomxp = ahgen::read_column($emapfile,"OFFAXISHIST","DECNOMXP");
  @pa_nomxp = ahgen::read_column($emapfile,"OFFAXISHIST","PA_NOMXP");
  @ra_pnt = ahgen::read_column($emapfile,"OFFAXISHIST","RA_PNT");
  @dec_pnt = ahgen::read_column($emapfile,"OFFAXISHIST","DEC_PNT");
} else { #doexpomaps equal false
  #set up 1-D double arrays: ra_nomxp(1), dec_nomxp(1), 
  #pa_nomxp(1), ra_pnt(1), dec_pnt(1)
  #Assign to the 5 values in the input parameter string nompntpars:
  push (@ra_nomxp,$nompntpars[0]);  
  push (@dec_nomxp,$nompntpars[1]);
  push (@pa_nomxp,$nompntpars[2]); 
  push (@dec_pnt,$nompntpars[3]); 
  push (@ra_pnt,$nompntpars[4]); 
  $ra_nom = $ra_nomxp[0];
  $dec_nom = $dec_nomxp[0];
} #End of if-block checking whether doexpomaps = True or False 

# For negative RA, use the off-axis angles already calculated by the exposure
# map generator for the default target; if we are not using an exposure map 
# (but instead an instrument) this option is used to set the off-axis and 
# azimuthal angles to zero

# The logic is changed here because we need to read OFFAXISLO and OFFAXISHI in 
# order to group the attitude bins by similar off-axis angle and this later to 
# replace the actual off-axis values by averages in a group. 

if ($doexpmap) { 
  @offaxislo = ahgen::read_column($emapfile,"OFFAXISHIST","OFFAXISLO"); 
  @offaxishi = ahgen::read_column($emapfile,"OFFAXISHIST","OFFAXISHI"); 

  if ($source_ra < 0.0) {
    for (my $kk=0; $kk<$numoffaxis; ++$kk) {
      if ($offaxislo[$kk] == 0.0) {
        $offaxisval[$kk] = 0.0
      } else {
        $offaxisval[$kk] = 0.5*($offaxislo[$kk]+$offaxishi[$kk])
      }
    }
    @phiangles = ahgen::read_column($emapfile,"OFFAXISHIST","AZIMUTH");
  }

  
} else {
  push(@offaxisval, 0.0);
  push(@phiangles, 0.0);
} 

# read TIMEINTERVAL & FRACTION columns and put inside conditional if exposure 
# map file is being used; the case of doexpomap = False is already accounted 
# for with dummy values
if ($doexpmap) {
  @timeinterval = ahgen::read_column($emapfile,"OFFAXISHIST","TIMEINTERVAL"); 
  @expfraction = ahgen::read_column($emapfile,"OFFAXISHIST","FRACTION");
}

# Establish how many attitude groups
for (my $gg=1; $gg<$numoffaxis; ++$gg) {
  if ($offaxislo[$gg] != $offaxislo[$gg-1]) {
    $numattgrps=$numattgrps+1;
  }
}

# Establish the lower and upper indices of each attitude group & the number in 
# each group
$attgrpindxlo[0]=0;
$attgrpindxhi[$numattgrps-1]=$numoffaxis-1;
$offaxisattgrp[0]=0;
my $idx=1;
for (my $ii=1; $ii<$numoffaxis; ++$ii) {
  if ($offaxislo[$ii] != $offaxislo[$ii-1]) {
    $attgrpindxlo[$idx]=$ii;
    $attgrpindxhi[$idx-1]=$ii-1;
    $idx=$idx+1;
  }
}
for (my $ii=0; $ii<$numattgrps; ++$ii) {
  $attgrpnumvals[$ii]= $attgrpindxhi[$ii]- $attgrpindxlo[$ii]+1;
  for (my $kk=$attgrpindxlo[$ii]; $kk<=$attgrpindxhi[$ii]; ++$kk) {
    $offaxisattgrp[$kk]=$ii;
  }
}


# Variables for total exposure time and arrays for sin, cos 
# of the critical angles
$sin_ra_nom = sin($ra_nom * DEG2RAD);
$cos_ra_nom = cos($ra_nom * DEG2RAD);
$sin_dec_nom = sin($dec_nom * DEG2RAD);
$cos_dec_nom = cos($dec_nom * DEG2RAD);
$sin_pa_nom = sin($pa_nom * DEG2RAD);
$cos_pa_nom = cos($pa_nom * DEG2RAD);

# If clobber is yes delete region list

if (-e "aharfgen_region.lis") {
  unlink "aharfgen_region.lis";
}
#open file for writing, test file containing list of region file names
open (my $fh, ">", "aharfgen_region.lis");

for (my $ii = 0 ; $ii < $numoffaxis ; ++$ii) {
  # Accumulate total exposure and calculate some trig. quantities for later use
  # +++ MSD Time interval is unitialized if doexpmap == FALSE
  $totalexposure = $totalexposure + $timeinterval[$ii];
  $sin_pa_nomxp[$ii] = sin($pa_nomxp[$ii] * DEG2RAD);
  $cos_pa_nomxp[$ii] = cos($pa_nomxp[$ii] * DEG2RAD);
  $dec_pnt_nom[$ii] = $dec_pnt[$ii] - $dec_nom;
  $ra_pnt_nom[$ii] = $ra_pnt[$ii] - $ra_nom;
  $sin_dec_pnt_nom[$ii] = sin($dec_pnt_nom[$ii] * DEG2RAD);
  $cos_dec_pnt_nom[$ii] = cos($dec_pnt_nom[$ii] * DEG2RAD);
  $sin_ra_pnt_nom[$ii] = sin($ra_pnt_nom[$ii] * DEG2RAD);
  $cos_ra_pnt_nom[$ii] = cos($ra_pnt_nom[$ii] * DEG2RAD); 
  $cos_dec_pnt[$ii] = cos($dec_pnt[$ii] * DEG2RAD);

  # Generate new set of region files in telescope coordinates for each off-axis 
  # angle in the exposure–map histogram. Note that the code should still work if
  # no region file is specified in the input; in that case downstream code will 
  # simply not reject events based on a region selection.
  if (uc $regionfile ne "NONE") {
    # outfile may be a list of file so it needs to be parse 
    # before the string is used
    my @outfiles = split(" ",$outfile);
    if (scalar(@outfiles) == 1) {
      @outfiles = split(",",$outfile);
    } 

    $currnewregionfile = $outfiles[0] . "region" . $ii . ".reg" ;
    push @newregionfiles, $currnewregionfile;
    if (uc $regmode eq "RADEC") {
      ahgen::run_ftool("coordpnt","input=$regionfile",
                       "outfile=$newregionfiles[$ii]",
                       "telescop=$telescop","instrume=$instrume",
                       "teldeffile=$teldeffile","startsys=RADEC","stopsys=DET",
                       "ra=$ra_nomxp[$ii]","dec=$dec_nomxp[$ii]",
                       "roll=$pa_nomxp[$ii]","ranom=$ra_nom","decnom=$dec_nom",
                       "clobber=yes");
    }  
    # write to list of region files
    if (uc $regmode eq "RADEC") {       
      print $fh $newregionfiles[$ii] . "\n";
    } elsif ($regmode eq "DET") {
      print $fh $regionfile . "\n";
    }
    
  }
  #For each off-axis position if RA/DEC != none then generate a new off-axis 
  #angle and azimuthal angle corresponding to the source position. Note that 
  #the raytracing event file 2nd extension will have the off-axis angles that 
  #were calculated and should over-ride the off-axis angles in the exposure-map
  # histogram (but the values of the histogram are still valid)
 
  if ($source_ra >= 0.0) {  
    ahgen::run_ftool("coordpnt","input=$source_ra,$source_dec","outfile=NONE",
                     "telescop=$telescop","instrume=$instrume",
                     "teldeffile=$teldeffile","startsys=RADEC","stopsys=DET",
                     "ra=$ra_nomxp[$ii]","dec=$dec_nomxp[$ii]",
                     "roll=$pa_nomxp[$ii]","ranom=$ra_nomxp[$ii]",
                     "decnom=$dec_nomxp[$ii]","clobber=yes","chatter=0");
    ahgen::run_ftool("pget","coordpnt","outx");
    $outxx = ahgen::get_tool_stdout;
    push (@srcdetx, $outxx);
    ahgen::run_ftool("pget","coordpnt","outy");
    $outyy = ahgen::get_tool_stdout;
    push (@srcdety, $outyy);
    #print "srcdetx = $srcdetx[$ii], srcdety = $srcdety[$ii] \n";
  }
  

} #end of loop over off axis angles
#TYMOD
for (my $ii =0; $ii <$numoffaxis; ++$ii) {
    $odeltx[$ii] = $srcdetx[$ii] - $optaxisx;
    $odelty[$ii] = $srcdety[$ii] - $optaxisy;
    #print "optaxisx = $optaxisx, opaxisy=$optaxisy \n";
    #print "odeltx = $odeltx[$ii], odelty = $odelty[$ii] \n";
    #print "focal length = $focallen, xscl^2 = $xmmperpixsq, yscl^2 = $ymmperpixsq \n";
    #print "sinrotd = $sinrotd, conrotd = $cosrotd \n";
    #print "optxflip = $optxflip \n";
    #print "optyflip = $optyflip \n";  
    $offaxisval[$ii] = RAD2ARCMIN*atan((sqrt(($odeltx[$ii] * $odeltx[$ii]*$xmmperpixsq)+($odelty[$ii] * $odelty[$ii]*$ymmperpixsq)))/$focallen);
    $top=($optxflip/$optyflip)*(($odeltx[$ii] *$sinrotd) - ($odelty[$ii] * $cosrotd));
    $bot= -1.0*(($odeltx[$ii] *$cosrotd) + ($odelty[$ii] * $sinrotd));
    #print "top = $top, bot = $bot \n";
        if (($bot != 0.0) && ($top != 0.0)) {
            $phival = RAD2DEG*atan2($top,$bot);
            #print "phival =$phival \n";
          if ($phival < 0.0) {
            $phiangles[$ii] = $phival + 360.0;
           } else {
               $phiangles[$ii] = $phival;
           }
         
    } else {
        $phiangles[$ii] = 0.0;
    }
    $sumofazimuth = $sumofazimuth + $phiangles[$ii];
    #print "theta = $offaxisval[$ii], phi = $phiangles[$ii] \n";

#end loop setting up theta and phi from source detx dety
}



#close region file list
close $fh;

#Calculate mean azimuthal (roll) angle in telescope coordinates; this will be 
#used as input to the raytracing code 
$meanxrtphi = $sumofazimuth / $numoffaxis;

# Calculate mean off-axis angle per group and then create a new off-axis angle 
# array that has the off-axis angles replaced by the group mean

#for (my $ii=0; $ii < $numattgrps; ++$ii) {
#  $tmpsumoffaxis=0.0;
#  for (my $jj=$attgrpindxlo[$ii]; $jj<=$attgrpindxhi[$ii]; ++$jj) { 
#    $tmpsumoffaxis=$tmpsumoffaxis+$tmpoffaxisval[$jj];
#  }
#  $meanattgrpoffaxis[$ii]= $tmpsumoffaxis/$attgrpnumvals[$ii];
#} #End loop over attitude groups

#Replace off-axis angles by the group mean
#for (my $ii=0; $ii < $numoffaxis; ++$ii) {
#  $offaxisval[$ii] = $meanattgrpoffaxis[$offaxisattgrp[$ii]];
#}

# See if xrtevtfile already exists, if it does then raytrace will not be run
if (-e $xrtevtfile) {
  $runraytrace = 0;
  ahlog::ah_info "HIGH", "AHARFGEN will use the existing raytracing event file $xrtevtfile. Make sure this is the file you intended to use.";
} else {
  $runraytrace = 1;
}

# For the raytracing we need to duplicate some of the off-axis values in order 
# to get better statistics for the larger fractional exposures (the raytracing 
# only allows a single parameter for the number of photons, regardless of the 
# number of theta/phi pairs)
for (my $ii = 0; $ii < $numoffaxis; ++$ii) {
  # Ratio of expfraction to the mean expfraction (=1/numoffaxis)
  $tfractionratio[$ii] = $expfraction[$ii] * $numoffaxis;
  # We start duplicating pairs of theta, phi if the exposure fraction is 
  # more than 1.5 X mean
  $numrtpairs[$ii] =  int($tfractionratio[$ii] + 0.5);
  # Num rt pairs should not be allowed to get to zero 
  if ($numrtpairs[$ii] < 1) { 
    $numrtpairs[$ii] = 1;
  } 
  $pairindexlo[$ii] = $rtnumoffaxis;
  $pairindexhi[$ii] = $rtnumoffaxis + $numrtpairs[$ii] - 1;
  $rtnumoffaxis = $rtnumoffaxis + $numrtpairs[$ii];
}

#Now setup the expanded arrays of theta and phi
for (my $ii=0; $ii<$numoffaxis; ++$ii) {
  for (my $jj=$pairindexlo[$ii]; $jj<=$pairindexhi[$ii]; ++$jj) {
    $rtoffaxisval[$jj] = $offaxisval[$ii];
    $rtphiangles[$jj] = $phiangles[$ii];
    $rtoffaxispointer[$jj] = $ii;
  }
}

# We are runing raytrace
if ($runraytrace) {
  if ($rtphiangles[0] == 0.0) { 
    $rtphiangles[0] = -360.0;
  } else { 
    if ($rtphiangles[0] < -360) {
      ahlog::ah_err "Phi angle is less than -360 degrees, this will cause uninteded consequences. Please find another way to express that angle.";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1);
    }
    $rtphiangles[0] = -1.0 * fmod(($rtphiangles[0]+360),360);
  }  

  # Convert any of the remaining azimuthal angles to positive if they are negative (by adding 360 degrees)
  for (my $ii=1; $ii<$rtnumoffaxis; ++$ii) {
    if ($rtphiangles[$ii]  < 0.0) {
      $rtphiangles[$ii] = fmod((360.0+$rtphiangles[$ii]), 360.0); 
    }
  }

  #Convert rtoffaxisval rtphiangle to strings
  for (my $kk = 0 ; $kk < $rtnumoffaxis ; ++$kk) {
    $rtoffaxisval[$kk] = sprintf("%.2f",$rtoffaxisval[$kk]);
    $offaxisstring = $offaxisstring . " " . $rtoffaxisval[$kk];
    $rtphiangles[$kk] = sprintf("%.2f",$rtphiangles[$kk]);
    $phistring = $phistring . " " . $rtphiangles[$kk];
  }


  #Get energy grid for raytracing from coarse-grid input ARF file. The energies
  #in that file are energy-bin boundaries, but we need the bin center energies.
  #Use “fcalc” to create a new fits file that will be used as input to 
  #xrtraytrace

  #The input energies to the raytracing will always have units of keV; convert 
  #the energy grid in the coarse-grid ARF from eV to keV if necessary
  $eunits = ahgen::get_keyword($onaxiscfile_parsed[0],$onaxiscfile_parsed[1],"TUNIT1");
  $numfullens = ahgen::get_keyword($onaxiscfile_parsed[0],$onaxiscfile_parsed[1],"NAXIS2");
  @ecoarsefull = ahgen::read_column($onaxiscfile_parsed[0],$onaxiscfile_parsed[1],"ENERGY");
  if ($eunits eq "eV") {
    ahgen::run_ftool("fcalc","infile=$onaxiscfile",
                     "outfile=arfgencgrid_full.fits","clname=ENERGY",
                     "expr=0.001*ENERGY","clobber=yes","copyall=no","copycol=no");
    ahgen::run_ftool("fthedit","infile=arfgencgrid_full.fits","keyword=EXTNAME",
                     "operation=ADD","value=ENERGYGRID");
  } elsif ($eunits eq "keV") {
    ahgen::run_ftool("fcalc","infile=$onaxiscfile",
                     "outfile=arfgencgrid_full.fits","clname=ENERGY",
                     "expr=ENERGY","clobber=yes","copyall=no","copycol=no");    
  } else {
    ahlog::ah_err "Energy unit in coarse grid ARF file are not keV or eV";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }
  ahgen::run_ftool("fthedit","infile=arfgencgrid_full.fits","keyword=EXTNAME",
                   "operation=ADD","value=ENERGYGRID");

  # Make a new energy grid file based on the input minimum and maximum energies specified
  $jesublo = 0;
  $jesubhi = $numfullens-1;
  for (my $ie = 0; $ie<=$numfullens-2; ++$ie) {
    if (($inputemin >= $ecoarsefull[$ie]) && ($inputemin < $ecoarsefull[$ie+1])) {
      $jesublo=$ie;
    } # end of if block searching low energy bin of range
    if (($inputemax >= $ecoarsefull[$ie]) && ($inputemax < $ecoarsefull[$ie+1])) {
      $jesubhi=$ie;
    } # end of if block searching high energy bin of range
  } # end loop over full energy grid
  $numsubens = $jesubhi - $jesublo + 1;
  ahgen::run_ftool("ftcopy","infile=arfgencgrid_full.fits","outfile=arfgencgrid.fits","clobber=yes");
  $numrows = $numfullens;
  if ($numsubens != $numfullens) {
    for (my $jj=0; $jj<$jesublo; ++$jj) {
      ahgen::run_ftool("ftdelrow","infile=arfgencgrid.fits","outfile=none","rows=1","confirm=YES","clobber=yes");
      $numrows = $numrows-1;
    }
    for (my $jj=$numfullens-1; $jj>$jesubhi; --$jj) {
      ahgen::run_ftool("ftdelrow","infile=arfgencgrid.fits","outfile=none","rows=$numrows","confirm=YES","clobber=yes");
      $numrows=$numrows-1;
    }
  }# end of if-block checking if emin and emax simply imply using the original energy grid

  # make sure the number of coarse grid points is greater than 2
  if (ahgen::get_keyword("arfgencgrid.fits",1,"NAXIS2") < 2) {
    ahlog::ah_err "Number of coarse grid points is less than 2 please pick a wider energy range";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }
  

  # For sourcetype=IMAGE option, before running raytracing, we will 
  # (a) Set up parameters to run heasim for 1 energy, using the input image 
  #     file imgfile
  # (b) Use the output event list from heasim to make a FITS file for input to 
  #     raytracing (photonlist mode). This FITS file will use the heasim output
  #     multiple times: for each energy, the photon off-axis and azimuthal 
  #     angles will be shifted for each off-axis position. 
  # (c) Make a FITS file with raytracing housekeeping information, 
  #     which will become ext. 2 of the raytracing event file.
  if (uc $sourcetype eq "IMAGE") {
    $numebinscoarse = ahgen::get_keyword("arfgencgrid.fits",1,"NAXIS2");
    @ecoarsecen = ahgen::read_column("arfgencgrid.fits",1,"Energy");
    # If no exposure map was specified, set fake exposure time
    if (!$doexpmap) {
      $totalexposure = 1.0e5;
    }
    # Make a dummy intermediate arf file for the sole purpose of running heasim; 
    # all energies will have a constant effective area
    $cgsflux = 1.0e-10;
    
   
    #Write ascii file for column info
    # If clobber is yes delete region list                                                               
    if (-e "arf_columns.lis") {
      unlink "arf_columns.lis";
    }

    open (my $fh2, ">", "arf_columns.lis");
    print $fh2 "ENERG_LO 1D keV \n";
    print $fh2 "ENERG_HI 1D keV \n";    
    print $fh2 "SPECRESP 1D cm**2 \n";
    close $fh2;
  
    #Write ascii file with the fake data
    @rmffile_parsed = ahgen::parse_file_name($rmffile);
    $ne = ahgen::get_keyword($rmffile_parsed[0],1,"NAXIS2");
    @arf_elo = ahgen::read_column($rmffile_parsed[0],1,"ENERG_LO");
    @arf_ehi = ahgen::read_column($rmffile_parsed[0],1,"ENERG_HI");
    
    $simenergyfac = 0.5*(($imgehi*$imgehi)-($imgelo*$imgelo))/($arf_ehi[$ne-1]-$arf_elo[0]);
    $constantarea = ($numphoton * ergtokeV * $simenergyfac)/$cgsflux/$totalexposure;

    if (-e "arf_data.dat") {
      unlink "arf_data.dat";
    }

    open (my $fh3,">","arf_data.dat");
    for (my $kk=0; $kk<$ne; ++$kk) {
      print $fh3 $arf_elo[$kk] . " " . $arf_ehi[$kk] . " " . $constantarea . "\n";
    }  
    close $fh3;
    
    #Make the fake arf file:
    ahgen::run_ftool("ftcreate","cdfile=arf_columns.lis","datafile=arf_data.dat",
                     "outfile=sim_arf.fits","extname=SPECRESP","clobber=yes");
    
    #Make the "source definition file" that will be used by heasim
    $srcfile = "srcfile.txt";
    if (-e $srcfile) {
      unlink $srcfile;
    } 
   
    $srcstring="0.0,0.0,0.0,pow,0.0," . $cgsflux . "," . $imgelo . "-" . $imgehi . 
               ",none,2,2,image(" . $imgfile . ",0,0,0,0)";
    open (my $fh4,">","srcfile.txt");
    print $fh4 $srcstring;
    close $fh4;
  
    # Get the heasim.mdb mdbfile (in $LHEA_DATA, i.e. “refdata” area) – note 
    # that the input parameter file heasim.par will already have the correct 
    # path and filename as the default.
    ahgen::run_ftool("pget","heasim","mdbfile");
    $mdbfile = ahgen::get_tool_stdout;

    # Setup other input parameters for heasim and run. The run uses the mean 
    # pointing position  for the whole observation, RA_NOM, DEC_NOM, PA_NOM, so
    # the off-axis angle and azimuthal angle of every photon is relative to this
    # position, which is at the center of coordinates in the simulated “image.”
    # Note that the position of the optical axis is not factored into the 
    # simulation: for each exposure map position, i, in the exposure map file 
    # we calculate the off-axis angle and azimuthal angle for each event by 
    # applying the offsets for the optical axis, RA_PNT(i), DEC_PNT(i), 
    # relative to the mean pointing position.
    $rapoint = $ra_nom;
    $decpoint = $dec_nom;
    $roll = $pa_nom;
    ahgen::run_ftool("heasim","getinfile=NO","mission=$telescop","instrume=$instrume","rapoint=$rapoint",
                     "decpoint=$decpoint","roll=$roll","skipfov=YES",
                     "insrcdeffile=srcfile.txt","outfile=heasim_events.fits","psffile=NONE",
                     "vigfile=NONE","rmffile=NONE","arffile=sim_arf.fits","intbackfile=NONE",
                     "psbackfile=NONE","difbackfile=NONE","pszbackfile=NONE","arfrmftol=1.0",
                     "flagsubex=NO","exposure=$totalexposure","subexposure=$totalexposure",
                     "resample=NO","dtpileup=0.0","filter=NONE","instmode=NONE",
                     "seed=$seed","mdbfile=$mdbfile","clobber=yes","debug=NO","mode=h");
 
    #Read the output file from heasim
    $numsimevents = ahgen::get_keyword("heasim_events.fits",1,"NAXIS2");
    $ra_ref = ahgen::get_keyword("heasim_events.fits",1,"TCRVL2");
    $ra_refpixel = ahgen::get_keyword("heasim_events.fits",1,"TCRPX2");
    $ra_delta = ahgen::get_keyword("heasim_events.fits",1,"TCDLT2");
    $dec_ref = ahgen::get_keyword("heasim_events.fits",1,"TCRVL3");
    $dec_refpixel = ahgen::get_keyword("heasim_events.fits",1,"TCRPX3");
    $dec_delta = ahgen::get_keyword("heasim_events.fits",1,"TCDLT3");
    @skyx = ahgen::read_column("heasim_events.fits",1,"X");
    @skyy = ahgen::read_column("heasim_events.fits",1,"Y");    

    #print "ra_ref = " . $ra_ref . "\n";
    #print "ra_refpixel = " . $ra_refpixel . "\n";
    #print "ra_delta = " . $ra_delta . "\n";
    #print "dec_ref = " . $dec_ref . "\n"; 
    #print "dec_refpixel = " . $dec_refpixel . "\n"; 
    #print "dec_delta = " . $dec_delta . "\n"; 


     
    for (my $kk=0; $kk<$numsimevents; ++$kk) {
      $rasim[$kk] = (($skyx[$kk]-$ra_refpixel)*$ra_delta)+$ra_ref;
      $decsim[$kk] = (($skyy[$kk]-$dec_refpixel)*$dec_delta)+$dec_ref;
      $sin_ra_sim[$kk] = sin($rasim[$kk]*DEG2RAD);
      $cos_ra_sim[$kk] = cos($rasim[$kk]*DEG2RAD);
      $sin_dec_sim[$kk] = sin($decsim[$kk]*DEG2RAD);
      $cos_dec_sim[$kk] = cos($decsim[$kk]*DEG2RAD);
    }
    
    # 160229 Calculate coefficients for formulae to compute DETX and DETY 
    # of any source position in RA & DEC by using coorpnt to fix the 
    # relation for 3 points. 
    $del_ra_pnt = 0.05;
    $del_dec_pnt = 0.1;

    for (my $jj=0; $jj<$numoffaxis; ++$jj) {
      $ra1= $ra_pnt[$jj] - (2.0*$del_ra_pnt);
      $ra2 = $ra_pnt[$jj];
      $ra3 = $ra_pnt[$jj] + $del_ra_pnt;
      $ra12 = $ra1-$ra2; 
      $ra23 = $ra2-$ra3;
      $dec1 = $dec_pnt[$jj] - (1.5*$del_dec_pnt);
      $dec2 = $dec_pnt[$jj];
      $dec3 = $dec_pnt[$jj] + $del_dec_pnt;
      $dec12 = $dec1 - $dec2;
      $dec23 = $dec2 - $dec3;

      ahgen::run_ftool("coordpnt","input=$ra1,$dec1","outfile=none","telescop=$telescop",  
                       "instrume=$instrume","teldeffile=$teldeffile","startsys=RADEC",
                       "stopsys=DET","ra=$ra_nomxp[$jj]","dec=$dec_nomxp[$jj]","roll=$pa_nomxp[$jj]",
                       "ranom=$ra_nomxp[$jj]","decnom=$dec_nomxp[$jj]");
      ahgen::run_ftool("pget","coordpnt","outx");
      $outx = ahgen::get_tool_stdout;
      $detx1 = $outx;
      ahgen::run_ftool("pget","coordpnt","outy");
      $outy = ahgen::get_tool_stdout;
      $dety1 = $outy;
      ahgen::run_ftool("coordpnt","input=$ra2,$dec2","outfile=none","telescop=$telescop",
                       "instrume=$instrume","teldeffile=$teldeffile","startsys=RADEC",
                       "stopsys=DET","ra=$ra_nomxp[$jj]","dec=$dec_nomxp[$jj]","roll=$pa_nomxp[$jj]",
                       "ranom=$ra_nomxp[$jj]","decnom=$dec_nomxp[$jj]");
      ahgen::run_ftool("pget","coordpnt","outx");
      $outx = ahgen::get_tool_stdout;
      $detx2 = $outx; 
      ahgen::run_ftool("pget","coordpnt","outy");
      $outy = ahgen::get_tool_stdout;
      $dety2 = $outy;
      ahgen::run_ftool("coordpnt","input=$ra3,$dec3","outfile=none","telescop=$telescop",
                       "instrume=$instrume","teldeffile=$teldeffile","startsys=RADEC",
                       "stopsys=DET","ra=$ra_nomxp[$jj]","dec=$dec_nomxp[$jj]","roll=$pa_nomxp[$jj]",
                       "ranom=$ra_nomxp[$jj]","decnom=$dec_nomxp[$jj]");
      ahgen::run_ftool("pget","coordpnt","outx");
      $outx = ahgen::get_tool_stdout;
      $detx3 = $outx;
      ahgen::run_ftool("pget","coordpnt","outy");
      $outy = ahgen::get_tool_stdout;
      $dety3 = $outy;
      #print "detx1 = $detx1 \n";
      #print "dety1 = $dety1 \n";
      #print "detx2 = $detx2 \n";
      #print "dety2 = $dety2 \n";
      #print "detx3 = $detx3 \n";
      #print "dety3 = $dety3 \n";

      $detx3 = $outx;
      $dety3 = $outy;
      $detx12 = $detx1 - $detx2;
      $detx23 = $detx2 - $detx3;
      $dety12 = $dety1 - $dety2;
      $dety23 = $dety2 - $dety3;

      #print "detx12 = $detx12 \n";
      #print "detx23 = $detx23 \n";
      #print "ra12 = $ra12 \n";
      #print "ra23 = $ra23 \n";
      #print "denominator = " . (($ra12*$dec23) - ($ra23*$dec12)) . "\n";
    
      $detxcoef1[$jj] = (($detx12*$dec23) - ($detx23*$dec12))/ (($ra12*$dec23) - ($ra23*$dec12));
      $detxcoef2[$jj] = (($detx12*$ra23) - ($detx23*$ra12))/(($ra23*$dec12)-($ra12*$dec23));
      $detxcoef0[$jj] = $detx1 - ($detxcoef1[$jj]*$ra1) - ($detxcoef2[$jj]*$dec1);
      $detycoef1[$jj] = (($dety12*$dec23) - ($dety23*$dec12))/(($ra12*$dec23)-($ra23*$dec12));
      $detycoef2[$jj] = (($dety12*$ra23) - ($dety23*$ra12))/(($ra23*$dec12)-($ra12*$dec23));
      $detycoef0[$jj] = $dety1 - ($detycoef1[$jj]*$ra1) - ($detycoef2[$jj]*$dec1);
    } #End of j loop over OFFAXISHIST rows to set up linear coefficients for det vs radec



    # Calculate the off-axis angle and azimuthal offsets for each off-axis  
    # position for each event; the units are radians because that is what is 
    # required by xrtraytrace
    for (my $jj=0; $jj<$numoffaxis; ++$jj) {
      for (my $kk=0; $kk<$numsimevents; ++$kk) {

        #160229 Commenting out this block
        #my $argdec = 0.5 * ($decsim[$kk]-$dec_pnt[$jj])*DEG2RAD;
        #my $argra = 0.5 * ($rasim[$kk] - $ra_pnt[$jj])*DEG2RAD;
        #my $hvoff = (sin($argdec) * sin($argdec)) + ($cos_dec_pnt[$jj]*
        #            $cos_dec_sim[$kk]*sin($argra)*sin($argra));
        #my $sinhalftheta = sqrt($hvoff);
        # my $theta = 2.0 * asin($sinhalftheta);
        #$simoffsettheta[$kk][$jj] = $theta;

        
        # 160229 New method of calculating raytracing theta, phi for each simulated event, 
        # using the detx dety linear coefficients derived earlier
        $detxsim = $detxcoef0[$jj] + ($detxcoef1[$jj]*$rasim[$kk]) + ($detxcoef2[$jj]*$decsim[$kk]);
        $detysim = $detycoef0[$jj] + ($detycoef1[$jj]*$rasim[$kk]) + ($detycoef2[$jj]*$decsim[$kk]);
        $detxsimdelt = $detxsim - $optaxisx;
        $detysimdelt = $detysim - $optaxisy;
        $tantheta = sqrt(($detxsimdelt*$detxsimdelt*$xmmperpixsq) + 
                         ($detysimdelt*$detysimdelt*$ymmperpixsq))/$focallen;
        $simoffsettheta[$kk][$jj] = atan($tantheta);
        $ynum = $optxyflipratio*( ($detxsimdelt * $sinrotd) - ($detysimdelt * $cosrotd));
        $xden = -1.0*(($detxsimdelt * $cosrotd) + ($detysimdelt * $sinrotd));
        if (($xden !=0.0) && ($ynum != 0.0)) {
          $simphi = atan2($ynum, $xden);
          if ($simphi <0.0) {
            $simoffsetphi[$kk][$jj] = $simphi+twopi;
          } else {
            $simoffsetphi[$kk][$jj] = $simphi;
          }
        } else {
          $simoffsetphi[$kk][$jj]=0.0;
        }

        # 160229 commenting out out formula
        # 160117 New formula for phi (the old one did not work); 
        # using the simple 2-D flat form for now
        #$ynum = -1.0 * (($rasim[$kk] - $ra_pnt[$jj]) * $sin_pa_nomxp[$jj] * $cos_dec_sim[$kk]) + 
        #        (($decsim[$kk] - $dec_pnt[$jj]) * $cos_pa_nomxp[$jj]);
        #$xden = -1.0 * (($rasim[$kk] - $ra_pnt[$jj]) * $cos_pa_nomxp[$jj] * $cos_dec_sim[$kk]) -
        #        (($decsim[$kk] - $dec_pnt[$jj]) * $sin_pa_nomxp[$jj]);
        #if (($xden != 0.0) && ($ynum != 0.0)) {
        #   $simphi = atan2($ynum,$xden);
        #  if ($simphi < 0.0) {
        #    $simoffsetphi[$kk][$jj] = $simphi + twopi;
        #  } else {
        #    $simoffsetphi[$kk][$jj] = $simphi;
        #  }
        #} # end if ($xden != 0.0) && ($ynum != 0.0)
      } #end of k-loop over simulated events
    } #end of j-loop over off-axis positions

    # We will now create a FITS file that will be used as input to xrtraytrace 
    # (source=photonlist option); first create an ascii column definition file

    #remove photon list file if it already exists
    $photonlist_coldef = "photonlist_coldef.lis";
    if (-e $photonlist_coldef) {
      unlink $photonlist_coldef;
    }     
 
    open (my $fh5,">",$photonlist_coldef);
    print $fh5 "ENERGY 1D keV \n";
    print $fh5 "THETA 1D radians \n";    
    print $fh5 "PHI 1D radians \n";
    close $fh5;

    # Create an ascii file for the photon list data; also create an ascii file 
    # that contains data that will be used to make a FITS file holding 
    # housekeeping data for the raytracing run.
    # Loop over each coarse energy and each off-axis position, writing new 
    # off-axis and azimuthal angles for each photon; write the photon list data
    # and the raytracing house keeping data. The raytracing code requires the 
    # units of both angles to be radians.
    if (-e "photonlist.dat") {
      unlink "photonlist.dat";
    }
    if (-e "raytracehk.dat") {
      unlink "raytracehk.dat";
    }

    my $mm = 0;
    open (my $fh6,">","photonlist.dat");
    open (my $fh7,">","raytracehk.dat");
    for (my $jj=0;$jj<$rtnumoffaxis;++$jj) {
      $mm = $rtoffaxispointer[$jj];
      for (my $ii=0; $ii<$numebinscoarse; ++$ii) {
        for (my $kk=0; $kk<$numsimevents; ++$kk) {
          print $fh6 $ecoarsecen[$ii] . " " . $simoffsettheta[$kk][$mm]
                . " " . $simoffsetphi[$kk][$mm] . "\n";
        }
        # Note that numsimevents should be equal to numphoton
        # (at least approximately)
        if ($jj == 0) {
          print $fh7 $rtoffaxisval[$jj] . " " .
        -1.0*$rtphiangles[$jj] . " " . $ecoarsecen[$ii] . " " . $numsimevents . "\n";
        } else {
           print $fh7 $rtoffaxisval[$jj] . " " .
        $rtphiangles[$jj] . " " . $ecoarsecen[$ii] . " " . $numsimevents . "\n"; 
        }
      }
    }

    # Close raytracehk.dat and photonlist.dat
    close $fh6;
    close $fh7;

    # Make the column definition file for the raytrace housekeeping
    if (-e "raytracehk_coldef.lis") {
      unlink "raytracehk_coldef.lis"; 
    }

    open (my $fh8,">","raytracehk_coldef.lis");
    print $fh8 "INITIALTHETA 1D arcmin \n";
    print $fh8 "INITIALAZIMDIR 1D degrees \n";
    print $fh8 "ENERGY 1D keV \n";
    print $fh8 "NUMPHOTONS 1J \n";
    # Close raytracehk_coldef.lis
    close $fh8;
     
    # Use fcreate to make the photon list file
    ahgen::run_ftool("ftcreate","cdfile=photonlist_coldef.lis",
                     "datafile=photonlist.dat","outfile=heasim_photonlist.fits",
                     "extname=PHOTONLIST","clobber=yes");
    # Add keywords to photon list file
    ahgen::run_ftool("fthedit","value=$ecoarsecen[0]",
                     "infile=heasim_photonlist.fits[1]","keyword=MINENERG",
                     "operation=add","comment=[keV]/Minimum energy in ENERGY column");
    ahgen::run_ftool("fthedit","value=$ecoarsecen[$numebinscoarse-1]",
                     "infile=heasim_photonlist.fits[1]","keyword=MAXENERG",
                     "operation=add","comment=[keV]/Maximum energy in ENERGY column");
    ahgen::run_ftool("fthedit","value=T","infile= heasim_photonlist.fits[1]",
                     "keyword=UNQELIST","operation=add",
                     "comment=True if all unique energies know");
 
    # Use fcreate to make the raytrace hk file
    ahgen::run_ftool("ftcreate","cdfile=raytracehk_coldef.lis",
                     "datafile=raytracehk.dat","outfile=raytracehk.fits",
                     "extname=INPUTPHOTONS","clobber=yes");
    
    # Add keywords to the raytrace hk file
    ahgen::run_ftool("fthedit","value=$telescop","infile=raytracehk.fits[1]",
                     "keyword=TELESCOP","operation=add","comment=Mission name");
    ahgen::run_ftool("fthedit","value=$instrume","infile=raytracehk.fits[1]",
                     "keyword=INSTRUME","operation=add","comment=Instrument name");
    ahgen::run_ftool("fthedit","value=$rtnumoffaxis","infile=raytracehk.fits[1]",
                     "keyword=NOFFAXIS","operation=add",
                   "comment=Number of off-axis angles");
    ahgen::run_ftool("fthedit","value=$numoffaxis","infile=raytracehk.fits[1]",
                     "keyword=NAZIMUTH","operation=add",
                     "comment=Number of azimuthal angles");
    ahgen::run_ftool("fthedit","value=$numebinscoarse",
                     "infile=raytracehk.fits[1]","keyword=NUMENRG",
                     "operation=add","comment=Number of unique photon energies");
  } # End of if-block checking if sourcetype=IMAGE


  
  ############################"END SOURCETYPE=IMAGE"###############################                  
  
  #Set up all xrtraytrace parameters and run xrtraytrace to create a 
  #new event file
  if (uc $sourcetype eq "IMAGE") {
    ahgen::run_ftool("xrtraytrace","instrume=$instrume","telescop=$telescop","mode=h","mirrorfile=$mirrorfile","obstructfile=$obstructfile",
                     "frontreffile=$frontreffile","backreffile=$backreffile","pcolreffile=$pcolreffile",
                     "scatterfile=$scatterfile","numphoton=$numphoton",
                     "energy=arfgencgrid.fits[ENERGYGRID]","seed=$seed","misalign=1 1 1 1 1 1",
                     "transmode=ALL","scattermode=ALL","source=photonlist",
                     "psrcfile=heasim_photonlist.fits[PHOTONLIST]","betapars=$betapars",
                     "flatradius=$flatradius","diagpars=1 1 1 1 1","offaxis=$offaxisstring",
                     "roll=$phistring","annulus=-1 10000 0 360.0","rectangle = 0 0 -100 -100",
                     "outeafile=NONE","outpsffile=NONE","psfpars=1 100 15","resultsplanez=0.0",
                     "resplaneonly=YES","outphistfile=$xrtevtfile","phisttype=BRIEF","externobjects=ALL",
                     "fastmode=YES","clobber=NO","chatter=1","logfile=NONE","history=YES");
  } else {
    ahgen::run_ftool("xrtraytrace","instrume=$instrume","telescop=$telescop","mode=h","mirrorfile=$mirrorfile","obstructfile=$obstructfile",
                     "frontreffile=$frontreffile","backreffile=$backreffile","pcolreffile=$pcolreffile",
                     "scatterfile=$scatterfile","numphoton=$numphoton",
                     "energy=arfgencgrid.fits[ENERGYGRID]","seed=$seed","misalign=1 1 1 1 1 1",
                     "transmode=ALL","scattermode=ALL","source=$sourcetype","betapars=$betapars",
                     "flatradius=$flatradius","diagpars=1 1 1 1 1","offaxis=$offaxisstring",
                     "roll=$phistring","annulus=-1 10000 0 360.0","rectangle = 0 0 -100 -100",
                     "outeafile=NONE","outpsffile=NONE","psfpars=1 100 15","resultsplanez=0.0",
                     "resplaneonly=YES","outphistfile=$xrtevtfile","phisttype=BRIEF","externobjects=ALL",
                     "fastmode=YES","clobber=NO","chatter=1","logfile=NONE","history=YES");
  }

  # Append the housekeeping file to the raytracing event file as a second extension
  # if the input photon mode in IMAGE
  if (uc $sourcetype eq "IMAGE") {
    ahgen::run_ftool("fappend","infile=raytracehk.fits[1]","outfile=$xrtevtfile");
  }
} #end if-block checking if runraytrace=True or False


# Set region file name
if (uc $regionfile eq "NONE") {
  $regionfilename = "NONE";
} else {
  $regionfilename = "\@aharfgen_region.lis";
}

# Compare INSTRUME keyword in auxtransfile and the INSTRUME parameter
if (uc $auxtransfile ne "NONE") {
  my @auxtransfile_parsed = ahgen::parse_file_name($auxtransfile);
  $auxinstrume = ahgen::get_keyword($auxtransfile_parsed[0],1,"INSTRUME");
  if ($instrume ne $auxinstrume) {    
    ahlog::ah_err "INSTRUME in input parameter file does not match INSTRUME in auxtransfile";
    ahapp::end_processing(1);
  }
}

# set up polydeg for ahsxtarfgen or hxirspeffimg
my $usepolydeg = "";
if (uc $polydeg eq "DEFAULT") { # default case will be handled by called tasks
  $usepolydeg = $polydeg;
} else {  # POLYDEG should be a number
  if (looks_like_number($polydeg)) { # is POLYDEG really in numeric format
    if ($polydeg > 5) {
      if ((uc $instrume eq "SXS") or (uc $instrume eq "SXI")) {
        if ($polydeg < 10) {
          $usepolydeg = $polydeg;
        } else {
          $usepolydeg = 10;
        }
      } else { # HXI
        $usepolydeg = 5;
      }
    } else { # POLYDEG <= 5 always OK
      $usepolydeg = $polydeg;
    }
  } else {
    ahlog::ah_err "POLYDEG parameter is neither DEFAULT nor numeric: $polydeg";
    ahapp::end_processing(1);
  }
} #end of if-block about polydeg

# ahsxtarfgen or hxirspeffimg 
if ((uc $instrume eq "SXS") or (uc $instrume eq "SXI")) {
  ahgen::run_ftool("ahsxtarfgen","clobber=$clobber","mode=h","telescop=$telescop","instrume=$instrume","emapfile=$emapfile",
                   "qefile=$qefile","obffile=$obffile","fwfile=$fwfile","contamifile=$contamifile",
                   "abund=$abund","cols=$cols","covfac=$covfac","gatevalvefile=$gatevalvefile",
                   "rmffile=$rmffile","onaxisffile=$onaxisffile","onaxiscfile=$onaxiscfile",
                   "polydeg=$usepolydeg", 
                   "regionfile=$regionfilename","xrtevtfile=$xrtevtfile","skyregfile=NONE","auxtransfile=$auxtransfile",
                   "outfile=$outfile","minphoton=$minphoton");
} elsif ((uc $instrume eq "HXI1") or (uc $instrume eq "HXI2")) {
  ahgen::run_ftool("hxirspeffimg","clobber=$clobber","mode=h","instrume=$instrume","dattfile=$dattfile",
                   "filtoffsetfile=$filtoffsetfile","emapfile=$emapfile","qefile=$qefile","vigfile=NONE",
                   "rmffile=$rmffile","onaxisffile=$onaxisffile","onaxiscfile=$onaxiscfile",
                   "polydeg=$usepolydeg", 
                   "xrtevtfile=$xrtevtfile","regionfile=$regionfilename","outfile=$outfile",
                   "minphoton=$minphoton","sampling=$sampling","baffle=$baffle","outflatfile=NONE",
                   "auxtransfile=$auxtransfile","rmfthresh=$rmfthresh");
} #end of if-block checking for which instrument to generate ARF


#Clean up temporary files
if ($ahapp::cleanup) {
  if (-e "arf_columns.lis") {
    unlink "arf_columns.lis";
  }
  if (-e "arf_data.dat") {
    unlink "arf_data.dat";
  }
  if ($runraytrace && uc $sourcetype eq "IMAGE") {
    if (-e $srcfile) {
      unlink $srcfile;
    }
    if (-e $photonlist_coldef) {
      unlink $photonlist_coldef;
    }
    if (-e "photonlist.dat") {
      unlink "photonlist.dat";
    }
    if (-e "raytracehk.dat") {
      unlink "raytracehk.dat";
    }
    if (-e "raytracehk_coldef.lis") {
     unlink "raytracehk_coldef.lis";
    }
  } 
}

# Revision Log:
# $Log: aharfgen.pl,v &
#
#revision 1.17
#date: 2016/01/28 14:48:18;  author: mdutka;  state: Exp;  lines: +16 -5
#Adding CALDB handling to aharfgen, correcting par file to conform with existing #standards
#
#revision 1.16
#date: 2016/01/27 14:45:58;  author: mdutka;  state: Exp;  lines: +9 -18
#fixing typo with the conversion of rtphiangle from negative to positive
#
#revision 1.15
#date: 2016/01/25 18:58:54;  author: mdutka;  state: Exp;  lines: +34 -31
#Adding correction based on TRF 2016-01-25.a
#
#revision 1.14
#date: 2016/01/22 19:03:20;  author: mdutka;  state: Exp;  lines: +231 -80
#updating aharfgen based on the outpt format created by ahexpogen
#
#revision 1.13
#date: 2016/01/04 15:47:31;  author: mdutka;  state: Exp;  lines: +58 -46
#Updateing aharfgen to handle extended sources
#
#revision 1.12
#date: 2015/12/29 21:12:29;  author: mdutka;  state: Exp;  lines: +469 -37
#Updateing aharfgen to handle extended sources
#
#revision 1.11
#date: 2015/12/16 16:00:24;  author: mdutka;  state: Exp;  lines: +15 -5
#Fixed issue with negative input pha angles
#
#revision 1.10
#date: 2015/12/15 00:31:18;  author: rshill;  state: Exp;  lines: +2 -2
#Added executable permission.
#
#revision 1.9
#date: 2015/11/13 21:37:00;  author: mdutka;  state: Exp;  lines: +65 -39
#arfgen can now work with instrument map as well as exposure map
#
#revision 1.8
#date: 2015/11/06 15:39:57;  author: mdutka;  state: Exp;  lines: +3 -1
#Updating aharfgen.pl
#
#revision 1.7
#date: 2015/11/04 19:01:58;  author: mdutka;  state: Exp;  lines: +2 -2
#Updating based on TRF Change 10-22
#
#revision 1.6
#date: 2015/11/03 16:03:02;  author: mdutka;  state: Exp;  lines: +47 -25
#Checking in arfgen script after debugging
#
#revision 1.5
#date: 2015/10/26 16:40:06;  author: mdutka;  state: Exp;  lines: +8 -2
#now read RA_NOM and DEC_NOM and using them as inputs to coordpnt
#
#revision 1.4
#date: 2015/10/14 16:30:55;  author: mdutka;  state: Exp;  lines: +29 -26
#checking in first complete version of aharfgen ahsxtarfgen
#
#revision 1.3
#date: 2015/10/01 16:26:16;  author: mdutka;  state: Exp;  lines: +29 -25
#adding bug fixes for aharfgen
#
#revision 1.2
#date: 2015/10/01 13:10:18;  author: mdutka;  state: Exp;  lines: +62 -31
#adding case insesitivty removing else check on units
#
#revision 1.1
#date: 2015/09/07 13:25:44;  author: mdutka;  state: Exp;
#Adding new tool aharfgen
