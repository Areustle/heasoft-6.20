#! /user/bin/perl
#------------------------------------------------------------------------------
# File name: ahmkregion.pl
#
# Task name: ahmkregion.pl
#
# Version: 1.22
#
# Description: Make Hitomi Specific Region File to Plot Regions on DS9 or Ximage.
#
# Author: Joseph Wysk NASA GSFC
#
# History/Misc notes:
#
#
#Initial Template/Formatting Adapted From hxisgdshield.pl
#-------------------------------------------------------------------------------
#
# Tool Dependencies:
#   coordpnt
#
# Library Dependencies:
#   gen/lib/perl/ahlog.pm
#   gen/lib/perl/ahapp.pm
#   gen/lib/perl/ahgen.pm
#
# Modification History:
#
#
#########################
#  Pragmas / Setup
#########################

use strict;
use warnings;

#########################
#  Packages
#########################

# This form of 'use' forces explicit use of namespaces.
use File::Copy;
use ahlog ();
use ahgen ();
use ahapp ();

#########################
# Startup
##########################

my $nargs = scalar @ARGV ;

# Query canonical APE parameters and start logging
ahapp::startup () ;

# Pre-Processing
ahapp::begin_processing();

#########################
#  Input Parameters
#########################

#Status flag for exit conditions.
my $status = 0;

#These arrays store the variable file names to be cycled through during read/write processing
our @arbox = ();
my @artext = ();
my @arxco = ();

#Flag used to exit the check instrument loop.
my $flag = 0;
my @pixlst = ();
my @filtered = ();

# Query non-standard APE parameters.
my $instrument = uc ahapp::query_parameter("instrume");
my $pixlist = ahapp::query_parameter("pixlist");
my $cleanup    = lc ahapp::query_parameter("cleanup");

#Array to cylce through valid Hitomi instrument types
my @instcheck = qw( SXI SXS HXI1 HXI2 SGD1 SGD2 ALL );

#Loops over array to check for a valid Hitomi instrument name
for my $val (@instcheck){
  if ($instrument eq $val){
    $flag = 1;
    last;
  }
}

# Conditional to exit perl script gracefully and log error in the event of an invalid instrument name.
# This is set to exist by default unless the check instrument loop senses a valid instrument name.
if ($flag == 0) {
  ahlog::ah_err "$instrument is not a valid instrument entry.";
  ahapp::end_processing(1);
}
# End flag check for invalid instrument exit.

my $teldeffile = ahapp::query_parameter("teldeffile");
my $inregion = ahapp::query_parameter("inregion");
#	If this parameter is "CALDB" then use telescop=Hitomi and the value of the instrume keyword to find the appropriate TelDef file in the CALDB.
#	This is actually handled by coordpnt.c

if ($instrument eq "ALL"){
  unless ($teldeffile eq "CALDB"){
    ahlog::ah_warn "HIGH", "Parameter 'Teldeffile' must = 'CALDB' when Parameter 'Instrument' = 'ALL'";
    ahlog::ah_warn "HIGH", "Setting 'Teldeffile' from $teldeffile to 'CALDB'";
    $teldeffile = "CALDB";
    ahlog::ah_debug "Teldeffile=$teldeffile";
  }
  
  unless (uc $inregion eq "NONE"){
    ahlog::ah_warn "HIGH", "Parameter 'Inregion' must = 'NONE' when Parameter 'Instrument' = 'ALL'";
    ahlog::ah_warn "HIGH", "Setting 'Inregion' from $inregion to 'NONE' ";
    $inregion = "NONE";
    ahlog::ah_debug "Inregion=$inregion";	
  }
}

# Check if ra, dec, or roll are outside of physical range and gracefully exit if any are.
# Here, I'm using '$var eq ""' because ahapp::query_parameter returns "" for a parameter outside the min,max set in the par file. 
my $ra          = ahapp::query_parameter("ra") ;
if ($ra eq ""){
  ahlog::ah_err "Parameter ra is outside min,max range set in par file";
  ahapp::end_processing(1);
}

my $dec       = ahapp::query_parameter("dec");
if ($dec eq ""){
  ahlog::ah_err "Parameter dec is outside min,max range set in par file";
  ahapp::end_processing(1);
 }

my $roll       = ahapp::query_parameter("roll");
if ($roll eq ""){
  ahlog::ah_err "Parameter roll is outside min,max range set in par file";
  ahapp::end_processing(1);
 }
# End ra, dec, and roll checks.

#Roots used to generate file names.
#Sets root to instrument name if none is provided or to "Hitomi" if instrument=ALL.
my $outroot = ahapp::query_parameter("outroot");
my $allroot = "";
if (uc $outroot eq "NONE"){
  $outroot = $instrument;
  $allroot = "HITOMI"
} else {$allroot = $outroot;}
#End Root Check

#Declare script behavior flags and variable for seed region file name.
my $Derive_seed_region = 0; #Flag to derive seed region
my $Derive_intermed_regions = 0; #Flag to derive intermediate regions; ACT,DET,FOC,SKY.
my $SeedACTRegionName = ""; #Variable to handle the name of the seed region file. Varies with outroot.

#Check for region file
#If no region file or reference is supplied, script is set to derive seed and intermediate regions.
if (uc $inregion eq "NONE"){
  $Derive_seed_region = 1;
  $Derive_intermed_regions = 1;
  $SeedACTRegionName = "$outroot.ACT.box.reg"
} else { 
  if (-e $inregion) {
    $SeedACTRegionName = $inregion;
    $Derive_intermed_regions = 1;
    if ($instrument eq "ALL"){
      ahlog::ah_err "Functionality for 'Instrument'=ALL and User Specified Inregion file is not supported.";
      ahapp::end_processing(1);
    }
  } else {
      ahlog::ah_err "File $inregion does not exist";
      ahapp::end_processing(1);
  }
}
#End region check

#These are the on/off switch checks for this scripts file outputs.
#These parameters are read in here and checked several instances later on.
my $output_textregion = lc ahapp::query_parameter("outtextregion");
my $output_xcofile = lc ahapp::query_parameter("outxco");

# Write all parameters to the log file.
ahlog::ah_info "HIGH", ahapp::write_parameters () ;

my @arall = ();
my $allflag = 0;

if ($instrument eq "ALL") {
@arall = ("$allroot.FOC.box.reg","$allroot.SKY.box.reg","$allroot.FOC.text.reg","$allroot.SKY.text.reg","$allroot.FOC.xco","$allroot.SKY.xco");
$allflag = 1;
 chfile_exist(@arall) unless ($ahapp::clobber);
  
  for my $val (@arall){
    if (-e $val){ unlink $val; }
  }
}

our $loopmax = 1;
our $loopcounter = 0;
our @instdex = ();
if ($instrument eq "ALL"){	
  $loopmax = 5;
  @instdex = ("HXI1","HXI2","SXS","SXI","SGD1");	
}

while ($loopcounter < $loopmax) { 
  if ($loopmax > 1){
    $instrument = $instdex[$loopcounter];
    $outroot = $instrument;
  }

  # Checks if this script should derive a seed region file
  if ($Derive_seed_region == 1){
    unlink $SeedACTRegionName if -e $SeedACTRegionName; # Removes any lingering seed region files if they exist.
    file_write($SeedACTRegionName,"physical\n",'>'); 
  # The next few conditionals set the ACT seed region according to the instrumnet name as per the TRF.
    if ($instrument eq "SGD1" || $instrument eq "SGD2"){
      file_write($SeedACTRegionName,"box(1215.5,1215.5,1119.92,1119.92,45.0)\n",'>>'); 
    }
    
    if ($instrument eq "HXI1" || $instrument eq "HXI2"){
      file_write($SeedACTRegionName,"box(128.5,128.5,128,128,0)\n",'>>'); 
      file_write($SeedACTRegionName,"point(128.5,128.5)\n",'>>'); 
    }
    
    if ($instrument eq "SXI"){
      file_write($SeedACTRegionName,"box(160,320,320,640,0)\n",'>>'); 
      file_write($SeedACTRegionName,"box(480,320,320,640,0)\n",'>>'); 
    }
  
  # These positions and rotation angles for SXS are derived from the TelDef file
  # ah_sxs_teldef_20140101v03.fits using an IDL program documented in the TRF.
    if ($instrument eq "SXS"){
      my @nopixels = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
      my @pixels = @nopixels;
      my @pixpar = split ",", $pixlist;

      foreach my $split (@pixpar) { 
        if ( $split =~ "-" ) { 
          my $pix; 
          my ($pix1, $pix2) = split "-", $split; 
          if ( $pix1 eq "" and $pix2 ne "" ) { push @pixlst, (0 .. $pix2); next;} 
          if ( $pix2 eq "" and $pix1 ne "" ) { push @pixlst, ($pix1 .. 35); next;} 
          if ( $pix1 eq "" and $pix2 eq "" ) { push @pixlst, (0 .. 35); next;} 
          if ($pix1 < 0 || $pix1 > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; } 
          if ($pix2 < 0 || $pix2 > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; } 
          if ( $pix1 > $pix2 ) { push @pixlst, ($pix1 .. 35); push @pixlst, (0 .. $pix2); } 
          else { push @pixlst, ($pix1 .. $pix2); } 
        } elsif ( $split =~ ":" ) { 
          my $pix; 
          my ($pix1, $pix2) = split ":", $split; 
          if ( $pix1 eq "" and $pix2 ne "" ) { push @pixlst, (0 .. $pix2); next;} 
          if ( $pix2 eq "" and $pix1 ne "" ) { push @pixlst, ($pix1 .. 35); next;} 
          if ( $pix1 eq "" and $pix2 eq "" ) { push @pixlst, (0 .. 35); next;} 
          if ($pix1 < 0 || $pix1 > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; } 
          if ($pix2 < 0 || $pix2 > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; } 
          if ( $pix1 > $pix2 ) { push @pixlst, ($pix1 .. 35); push @pixlst, (0 .. $pix2); } 
          else { push @pixlst, ($pix1 .. $pix2); } 
        } else { 
          if ($split < 0 || $split > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; } 
          my $pix = $split; 
          push @pixlst, $pix; 
        }    
      } # end loop over split pixlst 

      @filtered = uniq(@pixlst);

      @filtered = sort { $a <=> $b } @filtered;
      @filtered = grep { $_ != '12' } @filtered;

      my $ipx=0;
      for (my $jj=0; $jj< scalar @filtered; ++$jj) {
        $ipx = $filtered[$jj];
        $pixels[$ipx] = 1;
      } 

      open(my $fh, '>>', $SeedACTRegionName) or endproc(1,$SeedACTRegionName);
      if ($pixels[0] == 1) { print $fh "box(3.00000,2.99279,1.0,1.0,-0.55381)\n"; } 
      if ($pixels[1] == 1) { print $fh "box(1.00541,2.98678,1.0,1.0,-0.51753)\n"; }
      if ($pixels[2] == 1) { print $fh "box(2.00361,2.98858,1.0,1.0,-0.45084)\n"; }
      if ($pixels[3] == 1) { print $fh "box(0.99940,1.96635,1.0,1.0,-0.24439)\n"; }
      if ($pixels[4] == 1) { print $fh "box(2.00481,1.97175,1.0,1.0,-0.31337)\n"; }
      if ($pixels[5] == 1) { print $fh "box(0.99399,0.96214,1.0,1.0,-0.10546)\n"; }
      if ($pixels[6] == 1) { print $fh "box(2.00240,0.96454,1.0,1.0,-0.03423)\n"; }
      if ($pixels[7] == 1) { print $fh "box(3.00601,1.98197,1.0,1.0,-0.27979)\n"; }
      if ($pixels[8] == 1) { print $fh "box(3.00901,0.97115,1.0,1.0,-0.17428)\n"; }
      if ($pixels[9] == 1) { print $fh "box(6.00901,2.97296,1.0,1.0,-0.34999)\n"; }
      if ($pixels[10] == 1) { print $fh "box(5.01022,2.97837,1.0,1.0,-0.17427)\n"; }
      if ($pixels[11] == 1) { print $fh "box(6.00601,1.96575,1.0,1.0,-0.31354)\n"; }
      if ($pixels[13] == 1) { print $fh "box(5.00901,1.97115,1.0,1.0,-0.10416)\n"; }
      if ($pixels[14] == 1) { print $fh "box(5.01623,0.97416,1.0,1.0,-0.35008)\n"; }
      if ($pixels[15] == 1) { print $fh "box(4.00721,1.98017,1.0,1.0,-0.03500)\n"; }
      if ($pixels[16] == 1) { print $fh "box(4.01022,0.97296,1.0,1.0,-0.07034)\n"; }
      if ($pixels[17] == 1) { print $fh "box(4.00901,2.98798,1.0,1.0,0.03575)\n"; }
      if ($pixels[18] == 1) { print $fh "box(4.00120,4.00060,1.0,1.0,0.03682)\n"; }
      if ($pixels[19] == 1) { print $fh "box(6.00962,3.98498,1.0,1.0,0.03334)\n"; }
      if ($pixels[20] == 1) { print $fh "box(5.00781,3.99459,1.0,1.0,0.35163)\n"; }
      if ($pixels[21] == 1) { print $fh "box(6.01683,4.99159,1.0,1.0,-0.20898)\n"; }
      if ($pixels[22] == 1) { print $fh "box(5.01082,4.99760,1.0,1.0,-0.06975)\n"; }
      if ($pixels[23] == 1) { print $fh "box(6.01743,6.00120,1.0,1.0,0.03500)\n"; }
      if ($pixels[24] == 1) { print $fh "box(5.01022,6.00120,1.0,1.0,-0.10500)\n"; }
      if ($pixels[25] == 1) { print $fh "box(4.00361,5.00781,1.0,1.0,-0.31372)\n"; }
      if ($pixels[26] == 1) { print $fh "box(4.00481,6.01262,1.0,1.0,0.03487)\n"; }
      if ($pixels[27] == 1) { print $fh "box(0.99820,4.00361,1.0,1.0,-0.31453)\n"; }
      if ($pixels[28] == 1) { print $fh "box(2.00000,4.00240,1.0,1.0,0.00064)\n"; }
      if ($pixels[29] == 1) { print $fh "box(0.99399,5.00781,1.0,1.0,0.03513)\n"; }
      if ($pixels[30] == 1) { print $fh "box(0.99639,6.01082,1.0,1.0,-0.06966)\n"; }
      if ($pixels[31] == 1) { print $fh "box(1.99820,5.00901,1.0,1.0,0.13919)\n"; }
      if ($pixels[32] == 1) { print $fh "box(1.99880,6.00841,1.0,1.0,-0.00021)\n"; }
      if ($pixels[33] == 1) { print $fh "box(2.99880,5.00481,1.0,1.0,-0.00038)\n"; }
      if ($pixels[34] == 1) { print $fh "box(3.00000,6.01142,1.0,1.0,-0.03509)\n"; }
      if ($pixels[35] == 1) { print $fh "box(2.99820,4.00060,1.0,1.0,0.13877)\n"; }
      close $fh;
    }	
  } #End of Derive_seed_region == 1

#########################
#	END SETUP	#
#########################

#########################
#	Begin Main	#
#########################

  # Declares the variables which will be used to store/generate file names.
  my $ACT_box_regionfile = "";
  my $DET_box_regionfile = "";
  my $FOC_box_regionfile = "";
  my $SKY_box_regionfile = "";
  
  my $ACT_text_regionfile = "";
  my $DET_text_regionfile = "";
  my $FOC_text_regionfile = "";
  my $SKY_text_regionfile = "";
  
  my $ACT_ximage_xcofile = "";
  my $DET_ximage_xcofile = "";
  my $FOC_ximage_xcofile = "";
  my $SKY_ximage_xcofile = "";


  # Flag to derive the intermediate regions from the seed region
  if ($Derive_intermed_regions == 1){
  # Sets up file names based on outroot variable.
    if ($instrument ne "SGD1" && $instrument ne "SGD2"){
      $ACT_box_regionfile = "$outroot.ACT.box.reg";
      $DET_box_regionfile = "$outroot.DET.box.reg";
    }
    $FOC_box_regionfile = "$outroot.FOC.box.reg";
    $SKY_box_regionfile = "$outroot.SKY.box.reg";
  
    our @boxcheck = ();
    if ($instrument ne "SGD1" && $instrument ne "SGD2"){
      @boxcheck = ( $DET_box_regionfile,$FOC_box_regionfile,$SKY_box_regionfile );         
    } else {
      @boxcheck = ( $FOC_box_regionfile,$SKY_box_regionfile );         
    }
  
  # Checks to see if box region files already exists and exists gracefully if clobber is not set
  # If clobber is set, these files will be overwritten by coordpnt if they already exist.
    chfile_exist(@boxcheck) unless ($ahapp::clobber);
  # End of clobber check
  
  if ($instrument ne "SGD1" && $instrument ne "SGD2"){
    if ($SeedACTRegionName ne $ACT_box_regionfile){ 
      copy("$SeedACTRegionName","$ACT_box_regionfile") or cpfail(1,$SeedACTRegionName,$ACT_box_regionfile);
    }
  } else {
      copy("$SeedACTRegionName","$FOC_box_regionfile") or cpfail(1,$SeedACTRegionName,$FOC_box_regionfile);
  }
  # Sets up file names for text regions output if turned on.
  if ($output_textregion eq "yes"){
  
  if ($instrument ne "SGD1" && $instrument ne "SGD2"){
    $ACT_text_regionfile = "$outroot.ACT.text.reg";
    $DET_text_regionfile = "$outroot.DET.text.reg";
  }
  $FOC_text_regionfile = "$outroot.FOC.text.reg";
  $SKY_text_regionfile = "$outroot.SKY.text.reg";
  
  # Array to cycle through text files and check for existence  
  our @txtregcheck = ();

  if ($instrument ne "SGD1" && $instrument ne "SGD2"){
    @txtregcheck = ( $ACT_text_regionfile,$DET_text_regionfile,$FOC_text_regionfile,$SKY_text_regionfile);         
  } else {
    @txtregcheck = ( $FOC_text_regionfile,$SKY_text_regionfile);         
  }
  # Checks to see if text region files already exists and exists gracefully if clobber is not set
  # If clobber is set, these files will be overwritten if they already exist.
    chfile_exist(@txtregcheck) unless ($ahapp::clobber);
  # End of clobber check
  
  # This will create the text files in the current working directory
  # If clobber has been set, the script will bypass the exist check above and simply overwrite the existing file with a new blank one.
  # If clobber has not been set, the script will exit before it gets to this block of code.
  for my $val (@txtregcheck){
    open(my $fh, '>', $val) or endproc(1,$val);
    close $fh;
  }
  # End of create text region files.
  
  } 
  # End $output_textregion eq "yes"
  
  #Setps up file names for ximage files if turned on.
  if ($output_xcofile eq "yes"){
  
  if ($instrument ne "SGD1" && $instrument ne "SGD2"){
    $ACT_ximage_xcofile = "$outroot.ACT.xco";
    $DET_ximage_xcofile = "$outroot.DET.xco";
  }
    $FOC_ximage_xcofile = "$outroot.FOC.xco";
    $SKY_ximage_xcofile = "$outroot.SKY.xco";
  
  # Array to cycle through .xco files and check for existence 
  our @xcocheck = ();

  if ($instrument ne "SGD1" && $instrument ne "SGD2"){
    @xcocheck = ( $ACT_ximage_xcofile, $DET_ximage_xcofile, $FOC_ximage_xcofile, $SKY_ximage_xcofile);
  } else {
    @xcocheck = ( $FOC_ximage_xcofile, $SKY_ximage_xcofile);
  }
  
  # Checks to see if .xco region files already exists and exists gracefully if clobber is not set
  # If clobber is set, these files will be overwritten if they already exist.
    chfile_exist(@xcocheck) unless ($ahapp::clobber);
  # End of clobber check
  
  # This will create the .xco files in the current working directory
  # If clobber has been set, the script will bypass the exist check above and simply overwrite the existing file with a new blank one.
  # If clobber has not been set, the script will exit before it gets to this block of code.
  for my $val (@xcocheck){
    open(my $fh, '>', $val) or endproc(1,$val);
    close $fh;
  }
  # End of create .xco files

  } #End $output_xcofile eq "yes"	
    
  } #End of Derive_intermed_regions == 1


  # This is where most of the "heavy lifting" is done by coordpnt.
  # Based on the Hitomi instrument, the script runs coordpnt to generate either box region files for DET, FOC, & SKY
  # or ds9 seed DET region files in the case of SXS
  
  if ($instrument eq "SGD1" || $instrument eq "SGD2"){
    if ($Derive_intermed_regions == 1){
      run_coordpnt($SKY_box_regionfile,"FOC","SKY");
    }
  }

  if ($instrument eq "HXI1" || $instrument eq "HXI2" || $instrument eq "SXS"){
    if ($Derive_intermed_regions == 1){
      run_coordpnt($DET_box_regionfile,"ACT","DET");
      run_coordpnt($FOC_box_regionfile,"ACT","FOC");
      run_coordpnt($SKY_box_regionfile,"ACT","SKY");
    }
  } elsif ($instrument eq "SXI")  {
  # Covers SXI ds9 seed generation
    if ($Derive_intermed_regions == 1){

      our @arsxi = ("ds9-c0-seed.DET.reg","ds9-c1-seed.DET.reg","ds9-c2-seed.DET.reg","ds9-c3-seed.DET.reg","ds9-c0-seed.FOC.reg","ds9-c1-seed.FOC.reg","ds9-c2-seed.FOC.reg","ds9-c3-seed.FOC.reg","ds9-c0-seed.SKY.reg","ds9-c1-seed.SKY.reg","ds9-c2-seed.SKY.reg","ds9-c3-seed.SKY.reg");
  
      # Checks to see if intermediate  region files already exists and exists gracefully if clobber is not set
      # If clobber is set, these files will be overwritten if they already exist.
      chfile_exist(@arsxi) unless ($ahapp::clobber);
      # End of clobber check

      # First for DET
      sxi_coordpnt("ds9-c0-seed.DET.reg","ACT","DET",0);
      sxi_coordpnt("ds9-c1-seed.DET.reg","ACT","DET",1);
      sxi_coordpnt("ds9-c2-seed.DET.reg","ACT","DET",2);
      sxi_coordpnt("ds9-c3-seed.DET.reg","ACT","DET",3);

      # Next for FOC
      sxi_coordpnt("ds9-c0-seed.FOC.reg","ACT","FOC",0);
      sxi_coordpnt("ds9-c1-seed.FOC.reg","ACT","FOC",1);
      sxi_coordpnt("ds9-c2-seed.FOC.reg","ACT","FOC",2);
      sxi_coordpnt("ds9-c3-seed.FOC.reg","ACT","FOC",3);

      # Next for SKY
      sxi_coordpnt("ds9-c0-seed.SKY.reg","ACT","SKY",0);
      sxi_coordpnt("ds9-c1-seed.SKY.reg","ACT","SKY",1);
      sxi_coordpnt("ds9-c2-seed.SKY.reg","ACT","SKY",2);
      sxi_coordpnt("ds9-c3-seed.SKY.reg","ACT","SKY",3);

    } #End $Derive_intermed_regions == 1
  } #End SXI branch

  # Now we're going to read in the information generated by coordpnt
  # in the box region files and write it to the text and xco files where appropriate.
  
  my $index0 = 0; #index used to cycle through region files and edit each one accordingly
  my $count = 1; #variable to keep track of line count and pull out desired information
  my $box = ""; #variable to store the box value to be written to a file
  my $point = ""; #variable to store the point value to be written to a file
  my $x = 0; #variable to store the x coordinate value to be written to a file
  my $y = 0; #variable to store the y cooordfinate value to be written to a file

  # Fill the arrays to store the variable file names to be cycled through during read/write processing
  
  if ($instrument ne "SGD1" && $instrument ne "SGD2"){
    @arbox = ($ACT_box_regionfile,$DET_box_regionfile,$FOC_box_regionfile,$SKY_box_regionfile);
    @artext = ($ACT_text_regionfile,$DET_text_regionfile,$FOC_text_regionfile,$SKY_text_regionfile);
    @arxco = ($ACT_ximage_xcofile,$DET_ximage_xcofile,$FOC_ximage_xcofile,$SKY_ximage_xcofile);
  } else { 
    @arbox = ($FOC_box_regionfile,$SKY_box_regionfile);
    @artext = ($FOC_text_regionfile,$SKY_text_regionfile);
    @arxco = ($FOC_ximage_xcofile,$SKY_ximage_xcofile);
  }

  if ($Derive_intermed_regions == 1){
  
    my @findword = ("circle","box","point");
    my $word = "";
    my $otext = "";
    my $yank = "";
    my $sxspixelcounter = 0;
    my $sxisegmentcounter = 0;
    my @splA = ();
    my @splB = ();
    my @trunc = ();
    my @m = ();
    my $x = 0;
    my $y = 0;
    my $yb = 0;
    my $yd = 0;
    my $ang = 0;
    my $boxflag = 0;
    my $sxiCCDindex = 0;
    my $segtrack = "";
    my $CCDtrack = "";
  
    if ($instrument eq "SXI"){
      our @arsxi = ("ds9-c0-seed.DET.reg","ds9-c1-seed.DET.reg","ds9-c2-seed.DET.reg","ds9-c3-seed.DET.reg","ds9-c0-seed.FOC.reg","ds9-c1-seed.FOC.reg","ds9-c2-seed.FOC.reg","ds9-c3-seed.FOC.reg","ds9-c0-seed.SKY.reg","ds9-c1-seed.SKY.reg","ds9-c2-seed.SKY.reg","ds9-c3-seed.SKY.reg");
      my $index1 = 0;
      my $index2 = 0;
      my $boxfile = "";
    
    foreach $boxfile (@arbox) {
      if ($boxfile ne "$outroot.ACT.box.reg"){
        unlink $boxfile if -e $boxfile;}
      }
      
      while ($index1 < 12) {
        my $index2 = ($index1/4)+1;
    
        open(my $fh, '<', $arsxi[$index1]) or endproc(1,$arsxi[$index1]); 
        my @lines = <$fh>;
        for (@lines){
          chomp($_);
          @trunc = split(/#/,$_); # Gets rid of any comments that might get in the way of findword
          
          foreach $word (@findword) {
            if ($trunc[0] =~ /$word/i){
              ahlog::ah_debug "index$index0: $_\n";
              $yank = $trunc[0];        
              ahlog::ah_debug "output file: $artext[$index0]\n"; # Helps check we're working with the right files
              file_write($arbox[$index2],"$yank\n",'>>'); 
            }
          }
        }
    $index1++;
      }
    
    my $sxids9file = "";
    unless ($cleanup eq "no") {
      foreach $sxids9file (@arsxi){
        unlink $sxids9file if -e $sxids9file;
      }
      }
    } # End of if ($instrument eq "SXI")
  
    while ($index0 < 4) {
      $boxflag = 0;
      if ($instrument eq "SXS") {$sxspixelcounter = 0;}		
      open(my $fh, '<', $arbox[$index0]) or endproc(1,$arbox[$index0]); 
      my @lines = <$fh>;
      for (@lines){
        chomp($_);
        @trunc = split(/#/,$_);
        foreach $word (@findword) {
          if ($trunc[0] =~ /$word/i){
            if (lc $word ne "point"){
              ahlog::ah_debug "index$index0: $_\n";
              $yank = $trunc[0];
              @splA = split(/,/,$yank);
              @splB = split(/\(/,$splA[0]);
              $x = $splB[1];
              $y = $splA[1];
              if ($instrument eq "SXI"){
                $ang = $splA[4];
                $ang = substr($ang,0,-1);
                $ang = $ang-90;
                if ($ang > 180) {$ang = $ang-180;}
                if ($ang > 90) {$ang = $ang+180;}
              }
              if ($instrument eq "HXI1" || $instrument eq "HXI2" || $instrument eq "SGD1" || $instrument eq "SGD2"){
                $yd = int($y);
                $yb = $yd+10;						
              } else {$yb = $y;}
              if ($instrument eq "SXS"){
               $otext = $filtered[$sxspixelcounter];
               $sxspixelcounter++;
              } elsif ($instrument eq "SXI") {
                $sxiCCDindex = (int($sxisegmentcounter/2)-1);
                if ($sxisegmentcounter%2 == 0 && $sxiCCDindex >= 0) {$otext = "CCD_ID=$sxiCCDindex Seg-AB";}
                elsif ($sxisegmentcounter%2 == 0) {$otext = "Seg-AB";}
                elsif ($sxiCCDindex >=0) {$otext = "CCD_ID=$sxiCCDindex Seg-CD";}
                else {$otext = "Seg-CD";}
                if ($sxisegmentcounter == 9) {$sxisegmentcounter =1;}
                $sxisegmentcounter++;
              } else {$otext = $instrument;}
            } else {
              $yank = $trunc[0];
            }
      
            if ($output_textregion eq "yes" && lc $word ne "point"){
              ahlog::ah_debug "output file: $artext[$index0]\n"; # Helps check we're working with the right files
              file_write($artext[$index0],"text($x,$yb) # text={$otext} color=red\n",'>>'); 
            } elsif ($output_textregion eq "yes"){
              ahlog::ah_debug "output file: $artext[$index0]\n"; # Helps check we're working with the right files
              open(my $M, '<', $artext[$index0]) or endproc(1,$artext[$index0]); 
              @m = <$M>;
              close $M;
              open($M, '>', $artext[$index0]) or endproc(1,$artext[$index0]); 
              print $M "$yank\n";
              print $M @m;
              close $M;
            }
            
            if ($output_xcofile eq "yes" && lc $word ne "point"){
              ahlog::ah_debug "output file: $arxco[$index0]\n"; # Helps check we're working with the right files
              open(my $fh, '>>', $arxco[$index0]) or endproc(1,$arxco[$index0]); 
              if ($boxflag == 0){
                print $fh "boxreg/disp/color=7/lwidth=5 $arbox[$index0]\n";
                $boxflag = 1;
              }
              if (uc $inregion eq "NONE"){
                if ($arxco[$index0] eq "$outroot.SKY.xco" && $instrument eq "SXI"){
                print $fh "label/xpix=$x/ypix=$y/angle=$ang/just=center/csize=0.5/text=\"$otext\"\n";}
              else {
                print $fh "label/xpix=$x/ypix=$y/just=center/text=\"$otext\"\n";}
                close $fh;
              }
            }	  
          }
        }
      } 
      $index0++;
    } # End while ($index0 < 4)
  } # End of if exists a user inregion file


  # Declares variables for proper lexical scope.
  my $xact = 0;
  my $yact = 0;
  my $xdet = 0;
  my $ydet = 0;
  my $xfoc = 0;
  my $yfoc = 0;
  my $xsky = 0;
  my $ysky = 0;

  if ($output_textregion eq "yes" || $output_xcofile eq "yes"){
    unless ($instrument eq "SGD1" || $instrument eq "SGD2"){
      if ($instrument eq "SXI"){
        $xact = 200;
        $yact = 600;
        $xdet = 300;
        $ydet = 1625;
        $xfoc = 300;
        $yfoc = 1625;
        $xsky = 300;
        $ysky = 1625;
      } elsif ($instrument eq "SXS"){
        $xact = 1.5;
        $yact = 7;
        $xdet = 1.5;
        $ydet = 7;
        $xfoc = 1150;
        $yfoc = 1150;
        ($xsky, $ysky) = get_sky_coords("ACT",3.5,3.5);
        $xsky = $xsky - 50;
        $ysky = $ysky + 50;	
      
      } elsif ($instrument eq "HXI1" || $instrument eq "HXI2"){
        $xact = 80;
        $yact = 210;
        $xdet = 80;
        $ydet = 210;
        $xfoc = 1000;
        $yfoc = 1000;
        ($xsky, $ysky) = get_sky_coords("ACT",128.5,128.5);
        $xsky = $xsky - 150; 
        $ysky = $ysky + 200;  
      }
    } #End of unless ($instrument eq "SGD1" || $instrument eq "SGD2")
  
  
    my @xloop = ($xact,$xdet,$xfoc,$xsky);
    my @yloop = ($yact,$ydet,$yfoc,$ysky);
  
  
    if ($output_textregion eq "yes"){
      my $coordindex = 0;
      my @cords = ("ACT","DET","FOC","SKY");
      my $lmax = 4;
      if ($instrument eq "SGD1" || $instrument eq "SGD2"){$lmax = 2;}
      while ($coordindex < $lmax){
    
        open(my $in,'<', $arbox[$coordindex]) or endproc(1,$arbox[$coordindex]);     
        open(my $out,'>', "tmp.txt") or endproc(1,"tmp.txt");     
        while(<$in>) {
          s/\b(physical)\b/#physical/g;
          print $out $_;
        }
        close $out;
        copy("tmp.txt",$arbox[$coordindex]) or cpfail(1,"tmp.txt",$arbox[$coordindex]);
        unlink "tmp.txt" if -e "tmp.txt";
        open(my $fh, '>>', $artext[$coordindex]) or endproc(1,$artext[$coordindex]);     
        unless ($instrument eq "SGD1" || $instrument eq "SGD2"){
        print $fh "text($xloop[$coordindex],$yloop[$coordindex]) # text={$instrument $cords[$coordindex]} color=red\n";}
        print $fh "\n";
        open(my $infh,'<',$arbox[$coordindex]) or endproc(1,$arbox[$coordindex]);     
        print $fh "# The following text has been copied from $arbox[$coordindex]:\n";
        while(<$infh>){
          print $fh $_;
        }
        close $fh;
        $coordindex++;
      } #End of while ($coordindex < lmax)
    } #End of if ($output_textregion eq "yes")
  
    unless ($instrument eq "SGD1" || $instrument eq "SGD2"){
      if ($output_xcofile eq "yes"){
        my $coordindex = 0;
        my @cords = ("ACT","DET","FOC","SKY");
        while ($coordindex < 4){ 
          file_write($arxco[$coordindex],"label/xpix=$xloop[$coordindex]/ypix=$yloop[$coordindex]/just=center/text=\"$instrument $cords[$coordindex]\"\n",'>>'); 
          $coordindex++;
        }   
      }
    } #End of if unless ($instrument eq "SGD1" || $instrument eq "SGD2")
  } #End of if ($output_textregion eq "yes" || $output_xcofile eq "yes")

  if ($allflag == 1) {
    my @arcop = ("$allroot.FOC.box.reg","$allroot.SKY.box.reg");
    my @arcatbox = ($FOC_box_regionfile,$SKY_box_regionfile);
    my $boxcat = 0;

    while ($boxcat < 2){
      open(my $in,'<', $arcatbox[$boxcat]) or endproc(1,$arcatbox[$boxcat]);     
      open(my $out,'>>', $arcop[$boxcat]) or endproc(1,$arcop[$boxcat]);     
      print $out "# The following text has been copied from $arcatbox[$boxcat]:\n";
      while(<$in>) {
        print $out $_;
      }
      print $out "\n";
      close $out;
      $boxcat++;
    }
  }

  $loopcounter++;
  if ($instrument eq "SGD1" || $instrument eq "SGD2"){
    if (uc $inregion eq "NONE"){
      unlink $SeedACTRegionName if -e $SeedACTRegionName; 
    }
  }
} # End of looping over all instruments 'while($loopcounter < $loopmax)'


if ($allflag == 1) {
  if ($output_textregion eq "yes"){
    copy("$allroot.FOC.box.reg","$allroot.FOC.text.reg") or cpfail(1,"$allroot.FOC.box.reg","$allroot.FOC.text.reg");
    copy("$allroot.SKY.box.reg","$allroot.SKY.text.reg") or cpfail(1,"$allroot.SKY.box.reg","$allroot.SKY.text.reg");
  }

  if ($output_xcofile eq "yes"){
    file_write("$allroot.FOC.xco","boxreg/disp/color=7/lwidth=5 $allroot.FOC.box.reg\n",'>'); 
    file_write("$allroot.SKY.xco","boxreg/disp/color=7/lwidth=5 $allroot.SKY.box.reg\n",'>'); 
  }
}

if ($allflag == 1) {
  if ($output_textregion eq "yes" || $output_xcofile eq "yes"){
  
    my @arxfoc = (886,886,1550,1550,1176,965,960,494);
    my @aryfoc = (1979,1311,1979,1311,1299,1253,1137,1207);
    my @arlabels = ("SXI CCD_ID=0","SXI CCD_ID=1","SXI CCD_ID=2","SXI CCD_ID=3","SXS","HXI1","HXI2","SGD1");
    my $labelcounter = 0;
    my $labelmax = 8;
    my $xfoc = 0;
    my $yfoc = 0;
    my $xsky = 0;
    my $ysky = 0;
  
    while ($labelcounter < $labelmax){
      $xfoc = $arxfoc[$labelcounter];
      $yfoc = $aryfoc[$labelcounter];
    
      if ($labelcounter < 4){ $instrument = "SXI";} else {$instrument = $arlabels[$labelcounter];}
      ($xsky, $ysky) = get_sky_coords("FOC",$xfoc,$yfoc);
    
      if ($output_textregion eq "yes"){
        file_write("$allroot.FOC.text.reg","text($xfoc,$yfoc) # text={$arlabels[$labelcounter]} color=red\n",'>>'); 
        file_write("$allroot.SKY.text.reg","text($xsky,$ysky) # text={$arlabels[$labelcounter]} color=red\n",'>>'); 
      }
    
      if ($output_xcofile eq "yes"){
        file_write("$allroot.FOC.xco","label/xpix=$xfoc/ypix=$yfoc/just=center/text=\"$arlabels[$labelcounter]\"\n",'>>'); 
        file_write("$allroot.SKY.xco","label/xpix=$xsky/ypix=$ysky/just=center/text=\"$arlabels[$labelcounter]\"\n",'>>'); 
      }
    
      $labelcounter++;
    } # End of while ($labelcounter < $labelmax)
  } # End of if ($output_textregion eq "yes" || $output_xcofile eq "yes")
  
  unless ($cleanup eq "no"){
    my @cleanfile = qw(
  ALL.ACT.box.reg
  HXI1.ACT.box.reg
  HXI1.ACT.text.reg
  HXI1.ACT.xco
  HXI1.DET.box.reg
  HXI1.DET.text.reg
  HXI1.DET.xco
  HXI1.FOC.box.reg
  HXI1.FOC.text.reg
  HXI1.FOC.xco
  HXI1.SKY.box.reg
  HXI1.SKY.text.reg
  HXI1.SKY.xco
  HXI2.ACT.box.reg
  HXI2.ACT.text.reg
  HXI2.ACT.xco
  HXI2.DET.box.reg
  HXI2.DET.text.reg
  HXI2.DET.xco
  HXI2.FOC.box.reg
  HXI2.FOC.text.reg
  HXI2.FOC.xco
  HXI2.SKY.box.reg
  HXI2.SKY.text.reg
  HXI2.SKY.xco
  SGD1.FOC.box.reg
  SGD1.FOC.text.reg
  SGD1.FOC.xco
  SGD1.SKY.box.reg
  SGD1.SKY.text.reg
  SGD1.SKY.xco
  SXI.ACT.box.reg
  SXI.ACT.text.reg
  SXI.ACT.xco
  SXI.DET.box.reg
  SXI.DET.text.reg
  SXI.DET.xco
  SXI.FOC.box.reg
  SXI.FOC.text.reg
  SXI.FOC.xco
  SXI.SKY.box.reg
  SXI.SKY.text.reg
  SXI.SKY.xco
  SXS.ACT.box.reg
  SXS.ACT.text.reg
  SXS.ACT.xco
  SXS.DET.box.reg
  SXS.DET.text.reg
  SXS.DET.xco
  SXS.FOC.box.reg
  SXS.FOC.text.reg
  SXS.FOC.xco
  SXS.SKY.box.reg
  SXS.SKY.text.reg
  SXS.SKY.xco);
  
    for my $val (@cleanfile){ 
      unlink $val if -e $val;
    }
  } # End of unless ($cleanup eq "no")
  
} # End of if ($allflag == 1)

#########################
#	End Main	#
#########################

#########################
#	Subroutines	#
#########################

# Error routine for an open file failure.
sub endproc {
  $status = shift;
  my $filename = shift;
  ahlog::ah_err "Could not open file '$filename' $!";
  ahapp::end_processing($status);
}

# Error routine for a copy failure.
sub cpfail {
  $status = shift;
  my $fname1 = shift;
  my $fname2 = shift;
  ahlog::ah_err "Failed to copy $fname1 to $fname2 $!";
  ahapp::end_processing($status);
}

# Check and exit if files given exist.
sub chfile_exist {
  my @filelist = @_;
  for my $val (@filelist){
    if (-e $val){
      ahlog::ah_err "Output file $val already exists but clobber was not set. Exiting ahmkregion.pl";
      ahapp::end_processing(1);
    }
  }
}

sub file_write {
  my ($filename, $printval, $mode) = @_;
  open(my $fh, $mode, $filename) or endproc(1,$filename);
  print $fh "$printval";
  close $fh;
}

# Run Coordpnt to generate an output region file (All instruments other than SXI).
sub run_coordpnt {
  my $coordpntoutfile = shift;
  my $startsys = shift;
  my $stopsys = shift;
  $status = ahgen::run_ftool( "coordpnt", "telescop=HITOMI", "instrume=$instrument", "ra=$ra", "dec=$dec", "roll=$roll", "teldeffile=$teldeffile", "startsys=$startsys", "input=$SeedACTRegionName", "clobber=yes", "outfile=$coordpntoutfile", "stopsys=$stopsys");
  if ($status) {
    ahlog::ah_err "Error coordpnt could not generate outfile=$coordpntoutfile";
    ahapp::end_processing(1);
  }
}

# Run Coordpnt to generate an output region file for SXI only.
# We're doing this because SXI needs to be seperated into its detector segments.
sub sxi_coordpnt {
  my $coordpntoutfile = shift;
  my $startsys = shift;
  my $stopsys = shift;
  my $rawtodetseg = shift;
  $status = ahgen::run_ftool( "coordpnt", "telescop=HITOMI", "instrume=$instrument", "ra=$ra", "dec=$dec", "roll=$roll", "teldeffile=$teldeffile", "startsys=$startsys", "input=$SeedACTRegionName", "clobber=yes", "outfile=$coordpntoutfile", "stopsys=$stopsys", "rawtodetseg=$rawtodetseg"); 
  if ($status) {
    ahlog::ah_err "Error coordpnt could not generate outfile=$coordpntoutfile";
    ahapp::end_processing(1);
  }
}

# Get the sky coordinates for center of instrument
sub get_sky_coords {
  my ($startsys, $inputx, $inputy) = @_;
  $status = ahgen::run_ftool("coordpnt", "telescop=HITOMI", "instrume=$instrument", "ra=$ra", "dec=$dec", "roll=$roll", "teldeffile=$teldeffile", "startsys=$startsys", "input=$inputx,$inputy", "chatter=1","outfile=none", "stopsys=SKY");
  if ($status) {
    ahlog::ah_err "Error running coordpnt to get $instrument sky values.";
    ahapp::end_processing(1);
  }
  $status = ahgen::run_ftool("pget","coordpnt","outx");
  if ($status) {
    ahlog::ah_err "Error getting outx from coordpnt";
    ahapp::end_processing(1);
  }
  my $outxx = ahgen::get_tool_stdout;
  $status = ahgen::run_ftool("pget","coordpnt","outy");
  if ($status) {
    ahlog::ah_err "Error getting outy from coordpnt";
    ahapp::end_processing(1);
  }
  my $outyy = ahgen::get_tool_stdout;
  return ($outxx,$outyy);
}

# Prevent repetition in pixlist input
sub uniq {
  my %seen;
  grep !$seen{$_}++, @_;
}

########################
#  Finishing
########################

ahlog::ah_info "HIGH", "ahmkregion.pl has finished running sucessfully";

ahapp::end_processing($status);
