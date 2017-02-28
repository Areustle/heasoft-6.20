
#!/usr/bin/perl
# File name: sxsmkrmf.pl
# Author:    Michael Dutka
# $Date: 2016/03/21 18:10:08 $
# Version: 1.0
#
# This Perl script will generate a response (.rsp) file which accounts for the 
# effective area and energy response for the sxs instrument  
#
# Tool dependencies:
#   ftcreate
#   ftcopy
#   ftappend
#   fthedit
#   fhisto
#
# Library dependencies:
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#   gen/lib/perl/ahlog
#
# Modification history:
#
#  Ver   Date        Author  Description
#  1.0   2016-01-06  MSD     adding new tool sxsmkrmf  
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
use ahfilterlib;        # CALDB query

use File::Copy;
use List::Util qw(first);

use Math::Trig;         # Pi and trig functions
use File::Copy;         # File copying

#########################
#  Constants
#########################
use constant NPIXELS => 36;
use constant NITYPE => 5; 

#########################
#  Startup
#########################

my $nargs = scalar @ARGV ;

# Query canonical APE parameters and start logging
ahapp::startup () ;

#########################
#  Input Parameters
#########################
my $infile           = ahapp::query_parameter("infile");
my $outfile          = ahapp::query_parameter("outfile");
my $resolist         = ahapp::query_parameter("resolist");
my $regmode          = ahapp::query_parameter("regmode");
my $regionfile       = ahapp::query_parameter("regionfile");
my $pixlist_par      = ahapp::query_parameter("pixlist");
my $pixeltest        = ahapp::query_parameter("pixeltest");
my $rapoint          = ahapp::query_parameter("rapoint");
my $decpoint         = ahapp::query_parameter("decpoint");
my $rollpoint        = ahapp::query_parameter("roll");
my $teldeffile       = ahapp::query_parameter("teldeffile");
my $outrsp           = ahapp::query_parameter("outrsp");
my $outrspfile       = ahapp::query_parameter("outrspfile");
my $arfinfile        = ahapp::query_parameter("arfinfile");
my $time             = ahapp::query_parameter("time");
my $whichrmf         = ahapp::query_parameter("whichrmf");
my $rmfsigma         = ahapp::query_parameter("rmfsigma");
my $rmftau           = ahapp::query_parameter("rmftau");
my $eminin           = ahapp::query_parameter("eminin");
my $dein             = ahapp::query_parameter("dein");
my $nchanin          = ahapp::query_parameter("nchanin");
my $useingrd         = ahapp::query_parameter("useingrd");
my $eminout          = ahapp::query_parameter("eminout");
my $deout            = ahapp::query_parameter("deout");
my $nchanout         = ahapp::query_parameter("nchanout");
my $rmfthresh        = ahapp::query_parameter("rmfthresh");
my $emincont         = ahapp::query_parameter("emincont");

# Write all parameters to the log file.
ahlog::ah_info "HIGH", ahapp::write_parameters () ;

#########################
#  Variable Declarations
#########################

# Event File Header keywords
my $nevents = 0;
my $ra_nom = 0;
my $dateobs = 0;
my $dec_nom = 0;
my $pa_nom = 0;

my @pixels;

# teldeffile keywords
my $actxflip = 0;
my $actyflip = 0;
my $act_xoff = 0;
my $act_yoff = 0;
my $act_scal = 0;
my $actxpix1 = 0;
my $actypix1 = 0;
my $act_xsiz = 0;
my $act_ysiz = 0;
my $act_xscl = 0;
my $act_yscl = 0;
my $in0_xcen = 0;
my $in0_ycen = 0;
my $npixmap = 0;

# Intermediate conversion factors
my $act_xcen = 0;
my $act_ycen = 0;
my $xoffset = 0;
my $yoffset = 0;

my $actx = 0;
my $acty = 0; 
my $detx = 0;
my $dety = 0;

# Teldeffile columns
my @pixelx;
my @pixely; 

# pixel list from coordpnt
my $pixlst_str;
my @pixlst;


# final pixel list
my @pixel;


my $inregion = 0;
my $ninpix = 0;
my $nitypeold = 0;
my $ityperow = 0;
my $nitypenew = 0;

my @itype_column;
my @gradelist;

my $nrmfgrade = 0;
my $fraction = 0;
my $sum = 0;

# Pixel grades
my @grades;
my @gradelst;

# RMF grades and resolution
my @rmfgrades;
my @rmfnames;

# Varibles for constructing row filter string
my $first = 0;
my $rowfilter = 0;


#########################
#  Main Code Block 
#########################

# Make sure infile is a valid SXS event file
if (-e $infile) {
  if (uc(ahgen::get_keyword($infile,"EVENTS","TELESCOP")) ne "HITOMI") {
    ahlog::ah_err "TELESCOP keyword in EVENTS extension is not HITOMI, aborting...";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1); 
  }
  if (uc(ahgen::get_keyword($infile,"EVENTS","INSTRUME")) ne "SXS") {
    ahlog::ah_err "INSTRUME keyword in EVENTS extension is not SXS, aborting...";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1); 
  }  
} else {
  ahlog::ah_err "Event file does not exist, aborting...";
  ahlog::ah_err ahgen::get_tool_stderr;
  ahapp::end_processing(1); 
}




# Get keywords from events extension
$nevents = ahgen::get_keyword($infile,"EVENTS","NAXIS2");
$dateobs = ahgen::get_keyword($infile,"EVENTS","DATE-OBS");


# Look for teldeffile in CALDB
if(uc $teldeffile eq "CALDB") {
  ahlog::ah_info "HIGH", "Querying CALDB for teldef Configuration file.";
  $teldeffile = ahfilterlib::call_quzcif($teldeffile, "SXS", "-", "TELDEF",$dateobs, "-", "HITOMI");
  if(ahgen::get_error_flag()) { return 1; }
  # Remove last three character of teldeffile string, 
  # we do not want the extension number
  $teldeffile = substr($teldeffile, 0,-3);
}


$ra_nom = ahgen::get_keyword($infile,"EVENTS","RA_NOM");
$dec_nom = ahgen::get_keyword($infile,"EVENTS","DEC_NOM");
$pa_nom = ahgen::get_keyword($infile,"EVENTS","PA_NOM"); 


if ($rapoint < 0.0) {
  $rapoint = ahgen::get_keyword($infile,"EVENTS","RA_NOM");
} else {
  $rapoint = $rapoint;
}

if ($decpoint < -90.0) {
  $decpoint = ahgen::get_keyword($infile,"EVENTS","DEC_NOM");
} else {
  $decpoint = $decpoint;
}

if ($rollpoint < 0.0) {
  $rollpoint = ahgen::get_keyword($infile,"EVENTS","PA_NOM"); 
} else {
  $rollpoint = $rollpoint;
}

ahlog::ah_info "HIGH", "Total number of events considered: " . $nevents . "\n";
ahlog::ah_info "HIGH", "Pointing used: RA=" . $ra_nom . " DEC=" . $dec_nom . " ROLL=" . $pa_nom . "\n";

#  36-element pixels array will be set to 1 for pixels with at least on corner
#  within the selected region, 0 otherwise
my @allpixels = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1); 
my @nopixels = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);


if (uc $regionfile eq "NONE") {
  @pixels = @nopixels;
  push @pixlst, map { /(\d+)-(\d+)/ ? ($1 .. $2) : $_ } (split(",",$pixlist_par));
  my $ipx=0;
  for (my $jj=0; $jj< scalar @pixlst; ++$jj) {
    $ipx = $pixlst[$jj];
    $pixels[$ipx] = 1;
  } 
} elsif (uc $regionfile eq "ALLPIX") {
  @pixels = @allpixels;
} else {
  if (!-e $regionfile) {
    ahlog::ah_err "regionfile parameter does not equal NONE or ALLPIX and the file does not exist, aborting...";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1); 
  }
  @pixels = @nopixels;
  ahgen::run_ftool("coordpnt","input=$regionfile","outfile=pixlst.reg","telescop=HITOMI",
                   "instrume=SXS","teldeffile=$teldeffile","startsys=$regmode","stopsys=RAW",
                   "ra=$rapoint","dec=$decpoint","roll=$rollpoint","ranom=$ra_nom",
                   "decnom=$dec_nom","clobber=yes","pixeltest=$pixeltest");
  open (my $fhpl, "<", "pixlst.reg") or die ("Could not open pixel list file.");
  my $line;
  foreach $line (<$fhpl>) {
    # Search for line begining with +pixel
    if (substr($line,0,6) eq "+pixel") {
      # Remove "+pixel"
      $pixlst_str = substr($line,6); 
      # Remove leading and trailing ")"
      $pixlst_str = substr($pixlst_str,1);
      $pixlst_str = substr($pixlst_str,0,-2);
      # Split string into array and store in @pixlst 
      @pixlst = split(",",$pixlst_str);
    }
  }
  my $ipx = 0;
  for (my $jj=0; $jj<scalar @pixlst; ++$jj) {
    $ipx = $pixlst[$jj];
    if (($ipx<0) || ($ipx>35)) {
      ahlog::ah_err "Pixel number $ipx is out of range (0-35) aborting...";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1); 
    }
    $pixels[$ipx] = 1;
  }
  close $fhpl;
} # end if regionfile eq "NONE"
    


# Get the list of grades to combine

if (uc $resolist eq "ALL") {
  @grades = (1,1,1,1,1);
} else {
  @grades = (0,0,0,0,0);
  @gradelist = split(",",$resolist);
  my $idx = 0;
  for (my $jj=0; $jj<scalar @gradelist; ++$jj) {
    $idx = $gradelist[$jj];
    if (($idx < 0) || ($idx > 4)) {
      ahlog::ah_err "Resolution grade $idx is out of range (0-35) aborting...";
      ahlog::ah_err ahgen::get_tool_stderr;
      ahapp::end_processing(1); 
    }
    $grades[$idx] = 1;
  }
}

# Get the total number of events in the selected pixels 
 
# Transform pixels to rowfile string
$first = 0;
for (my $ipix=0; $ipix<NPIXELS; ++$ipix) {
  if ($pixels[$ipix] == 1) {
    if ($first == 0) {
      $rowfilter = "PIXEL==" . $ipix;
      $first = 1; 
    } else {
      $rowfilter = $rowfilter . " || PIXEL==" . $ipix;
    }
  }
}

my $ftlist_infile = $infile . "[EVENTS]" . "[$rowfilter]";
#print "Input to ftlist = $ftlist_infile \n";
   
ahgen::run_ftool("ftlist","infile=$ftlist_infile","option=K","outfile=-",
                 "include=NAXIS2");

# Strip characters from ftlist output
$ninpix = ahgen::get_tool_stdout;
$ninpix = substr($ninpix,6);
$ninpix =~ s/\D//g;
ahlog::ah_info "HIGH", "Total number of events in selected pixels: " . $ninpix . "\n";

#Calculate the weights and write to sxsfrac.dat ascii file
@rmfgrades = (0,1,1,2,2);
@rmfnames = ("H","M","M","L","L");

# Open file sxsfrac.dat
open (my $fh, ">", "sxsfrac.dat");

#Loop over selected pixels
for (my $ipix=0; $ipix < NPIXELS; ++$ipix) {
  if ($pixels[$ipix] == 1) {
    #Make the IYTPE histogram for this PIXEL
    ahlog::ah_info "HIGH", "Making ITYPE histogram for PIXEL=" . $ipix . "\n";
    my $fhisto_infile = $infile . "[EVENTS]" . "[PIXEL==" . $ipix . "]";  
    #print "fhisto infile = " . $fhisto_infile . "\n";
    ahgen::run_ftool("fhisto","infile=$fhisto_infile","outfile=histo.fits",
                     "column=ITYPE","binsz=1","lowval=0","highval=4",
                     "outcolx=ITYPE","outcoly=NEVENTS","clobber=yes");
    # number of events of previous itype (MP for MS, LP for LS, 
    # initialized at 0 for HP):
    $nitypeold = 0;
    #Read itype column
    @itype_column = ahgen::read_column("histo.fits","1dhisto","NEVENTS");  
    # Loop over all grades
    for (my $igrade=0; $igrade < NITYPE; ++$igrade) {
      # Get the number of events of this grade in the IYPE histogram 
      # for this PIXEL
      $ityperow = $igrade;
      $nitypenew = $itype_column[$ityperow];
      ahgen::ah_info "HIGH", "Number of events ITYPE=" . $igrade . " PIXEL=" . $ipix . ": " . $nitypenew . "\n";
      # The following accounts for the fact that there are no separate rmf files for MP/MS 
      if ($igrade==0 || $igrade==2 || $igrade==4) { 
        $nrmfgrade = $grades[$igrade] * $nitypenew + $nitypeold;
        if ($nrmfgrade != 0) {
          $fraction = $nrmfgrade / $ninpix;
          $sum = $sum + $fraction;
          ahgen::ah_info "HIGH", "Number of " . $rmfnames[$igrade] . 
                         " events, PIXEL ipix=" . $nrmfgrade . "\n";
          ahgen::ah_info "HIGH", "Fraction of events: " . $rmfnames[$igrade] . 
                         " PIXEL ipix=" . $fraction . "\n"; 
          $fraction = sprintf("%.16f",$fraction);
          # Write the line for this pixel, to list for input to sxsrmf
          print $fh $ipix . " " . $rmfgrades[$igrade] . " " . $fraction . "\n";
        } # end if numrmfgrad !=0 
      } # end if $igrade==0 || $igrade==2 || $igrade==4
      if ($igrade == 1 ||$igrade == 3) {
        # MP or LP – keep the number; add to M or L total in 
        # next round of the loop      
        $nitypeold = $grades[$igrade] * $nitypenew;
      } 
    }
  }
}
ahgen::ah_info "HIGH", "Final sum of fractional rmf contributions: " . $sum . "\n";
  
close $fh;

# Create the sxsfrac.fits weighting factor fits file
   
# remove files involed in final fits file creation if they exist
if (-e "label.hdr") {
  unlink "label.hdr";
}
if (-e "label.cdf") {
  unlink "label.cdf";
}

  
# Create files involved in creating the weighting factor file
# hdr -> header cdf -> column definition dat -> column data
open (my $fh1, ">", "label.hdr");  
open (my $fh2, ">", "label.cdf");  
  
# Write header file
print $fh1 "EXTNAME 'SXSFRAC' \ Name of the binary table extension \n";
print $fh1 "TELESCOP 'HITOMI' \ Mission or satellite name \n";
print $fh1 "INSTRUME 'SXS' \ Instrument name \n";
close $fh1;

# Write column definition file
print $fh2 "PIXEL J \n";
print $fh2 "GRADE I \n";
print $fh2 "WEIGHT 1E \n";
close $fh2;

# Create and verify fits file
ahgen::run_ftool("ftcreate","cdfile=label.cdf","datafile=sxsfrac.dat", 
                 "headfile=label.hdr","outfile=sxsfrac_unfiltered.fits","clobber=yes");

# Delete rows with ~0 weight from sxsfrac.fits weight factor file

ahgen::run_ftool("ftselect","infile=sxsfrac_unfiltered.fits",
                 "outfile=!sxsfrac.fits","expression=WEIGHT>1.0e-9",
                 "copyall=yes","clobber=yes");

ahgen::run_ftool("ftverify","sxsfrac.fits");

# Clean up temporary files
if (-e "label.hdr") {
  unlink "label.hdr";
}
if (-e "label.cdf") {
  unlink "label.cdf";
}
if (-e "sxsfrac_unfiltered.fits") {
  unlink "sxsfrac_unfiltered.fits";
}
if (-e "sxsfrac.dat") {
  unlink "sxsfrac.dat";
}
if (-e "histo.fits") {
  unlink "histo.fits";
}
if (-e "pixlst.reg") {
  unlink "pixlst.reg";
}



# Run sxsrmf using outlst and the other sxsrmf parameters

ahgen::run_ftool("pset","sxsrmf","infile=sxsfrac.fits");
ahgen::run_ftool("pset","sxsrmf","outfile=$outfile");
ahgen::run_ftool("pset","sxsrmf","outrsp=$outrsp");
ahgen::run_ftool("pset","sxsrmf","rspfile=$outrspfile");
ahgen::run_ftool("pset","sxsrmf","arffile=$arfinfile");
ahgen::run_ftool("pset","sxsrmf","time=$dateobs"); 
ahgen::run_ftool("pset","sxsrmf","whichrmf=$whichrmf");  
ahgen::run_ftool("pset","sxsrmf","rmfsigma=$rmfsigma");
ahgen::run_ftool("pset","sxsrmf","rmftau=$rmftau");
ahgen::run_ftool("pset","sxsrmf","eminin=$eminin");
ahgen::run_ftool("pset","sxsrmf","dein=$dein");
ahgen::run_ftool("pset","sxsrmf","nchanin=$nchanin");
ahgen::run_ftool("pset","sxsrmf","useingrd=$useingrd");
ahgen::run_ftool("pset","sxsrmf","eminout=$eminout");
ahgen::run_ftool("pset","sxsrmf","deout=$deout");
ahgen::run_ftool("pset","sxsrmf","nchanout=$nchanout");
ahgen::run_ftool("pset","sxsrmf","rmfthresh=$rmfthresh");
ahgen::run_ftool("pset","sxsrmf","emincont=$emincont");
ahgen::run_ftool("pset","sxsrmf","mode=h");
ahgen::run_ftool("pset","sxsrmf","clobber=yes");
ahgen::run_ftool("sxsrmf");


# Revision Log:
# $Log: sxsmkrmf,v &
#
#
#revision 1.3
#date: 2016/03/21 16:25:24;  author: mdutka;  state: Exp;  lines: +4 -1
#Adding parameter logging
#
#revision 1.2
#date: 2016/03/03 23:00:48;  author: mdutka;  state: Exp;  lines: +8 -8
#changing parameter name region to regionfile
#
#revision 1.1
#date: 2016/02/25 17:49:06;  author: mdutka;  state: Exp;
#Rename sxsarfrmf to sxsmkrmf
