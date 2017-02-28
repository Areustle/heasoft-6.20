#!/usr/bin/perl
#
# File name: cams2att.pl
# Author: R. S. Hill NASA GSFC
# $Date: 2016/03/22 19:33:58 $
# Version: 0
#
# Script to convert CAMS row data (FFF) to a delta attitude file using the
# cams2det and det2att2 tools.  The raw CAMS displacements are corrected for
# temperature, and the FFF is filtered by CAMS operating mode.  The intermediate
# offsets file is filtered by GTI based on CAMS data validity.
#
# Tool Dependencies:
#   ftcalc
#   ftselect
#   cams2det
#   maketime
#   ftcopy
#   det2att2
# 
# Library Dependencies:
#   gen/lib/perl/ahlog.pm
#   gen/lib/perl/ahapp.pm
#   gen/lib/perl/ahgen.pm
#   gen/lib/perl/ahfilterlib.pm
#
# Modification History:
#



#########################
#  Pragmas
#########################

use strict;
use warnings;

#########################
#  Packages
#########################

# This form of 'use' forces explicit use of namespaces.
use ahlog ();
use ahgen ();
use ahapp ();
use ahfilterlib ();

#########################
#  Subroutines
#########################

# 
# Subroutine to manage intermediate file names.  Checks for existence if needed
# for current startstep.  Sets to default name if NONE specified as parameter.
#
sub cams2att_check_interm_file {

    my $filenameref = shift ;         # Value of the corresponding filename parameter
    my $startstepval = shift ;        # Value of the startstep parameter
    my $filestartstep = shift ;       # Step for which file is starting point
    my $defaultfile = shift ;         # Default filename 
    my $clobberval = shift ;          # Value of clobber parameter
    my $tmpfilesref = shift;          # Ref to temporary files array

    my $actualname = $$filenameref ;  # Actual filename = value to return
    my $delete_flag = '' ;            # Flag to delete existing file
    my $default_flag = '' ;           # Flag indicating default filename used

    #
    # NONE supplied as parameter value.
    #
    if ($actualname =~ /^\s*none\s*$/i) {

        if ($startstepval == $filestartstep) { # Actual filename required for specied startstep.
            ahlog::ah_err "Filename NONE specified for startstep=$startstepval." ;
            exit 1 ;                  # Premature exit from tasktmp_ftcalcfile1.fits.
        }
        $actualname = $defaultfile;   # Use default filename.
        $default_flag = 1;
    }

    #
    # Actual filename is now established.  Determine from
    # step number how file will be used.
    #
    if ($startstepval < $filestartstep) {      # File is to be written.

        if (-e $actualname) {   # File exists, so clobber needs to be checked.

            if ($clobberval) {  # Clobber set.
                $delete_flag = 1 ;  # Delete before subroutine exit.
            } else {
                ahlog::ah_err "File $actualname already exists and clobber not set." ;
                if ($default_flag) {
                    ahlog::ah_err "Remove temporary files or set clobber." ;
                }
                exit 1;             # Premature exit from task.
            }
            }  # End if file exists.
        }  # End if file to be written.
    elsif ($startstepval == $filestartstep) {  # File is to be read, so it needs to exist.
        unless (-e $actualname) {
            ahlog::ah_err "File $actualname not found (required for startstep=$startstepval)." ;
            exit 1 ;                 # Premature exit from task.
        }
    }
    else {  # $startstepval > $filestartstep     File not needed.
        unless ($default_flag) {
            ahlog::ah_info "LOW", "File $actualname ignored.\n" ;
        }
    }

    if ($delete_flag) {
        unlink $actualname;  # Delete clobbered file.
    }
    push @$tmpfilesref, $actualname if $default_flag ;
    $$filenameref = $actualname ;
}  # End of sub cams2att_check_intermediate_file.

#  Subroutine to convert CORRTEMP header keyword to a logical value.
#  0 or undef --> do the correction; 1 --> skip the correction

sub cams2att_skip_corrtemp {

    my $infile = shift ;
    my $retval = "" ;
    my $corrtemp_kwd = ahgen::get_keyword($infile, 1, "CORRTEMP") ;
    if (defined($corrtemp_kwd)) {

        if (1 == $corrtemp_kwd) {
            $retval = 1 ;
        } elsif (0 == $corrtemp_kwd) {
            $retval = "" ;
        } else {
            ahlog::ah_err "CORRTEMP in $infile must have value 0 or 1" ;
            exit 1 ;
        }
    } else {
        $retval = "" ;
    }
} # End of cams2att_skip_corrtemp.

#  Subroutine to generate cphead input file to select keywords
#  for copying.
sub cams2att_cphead_keyfil {

    my $kfile = shift;

    my @must_be_copied = 
      ("TELESCOP", "INSTRUME", "OBS_ID", "OBJECT", "OBSERVER",
      "OBS_MODE", "RA_OBJ", "DEC_OBJ", "RA_NOM", "DEC_NOM",
      "PA_NUM", "EQUINOX", "RADECSYS", "TIMESYS", "MJDREFI",
      "MJDREFF", "TIMEUNIT", "TIMEREF", "TASSIGN", "GPSOFFET",
      "CLOCKAPP", "DATE-OBS", "DATE-END", "SMUUNIT", "TLM2FITS",
      "SEQPNUM", "MKFFF", "ORIGIN", "SOFTVER", "CALDBVER",
      "CAM1DATA", "CAM2DATA");

    my @must_not_be_copied =
      ("EXTNAME", "TSTART", "TEND", "TELAPSE", "ORIGSYS", "DESTSYS");

    return 1 unless defined(open(KEYFIL, ">", $kfile));

    foreach (@must_not_be_copied) {
        print KEYFIL ("!", $_, "\n");
    }

    foreach (@must_be_copied) {
        print KEYFIL ($_, "\n");
    }

    close KEYFIL;
    
    return "";

} # End of cams2att_cphead_keyfil

#  Subroutine to do all the output header edits.
sub cams2att_edit_output_header {

    my $kfile = shift;
    my $srcfile = shift;
    my $destfile = shift;
    my $status = "";

    # Write the keyword copying specification
    $status = cams2att_cphead_keyfil($kfile);
    if ($status) {
        ahlog::ah_err "Could not create keyword copying specification file." ;
        exit 1;
    }   

    # Apply the keyword copying specification
    $status = ahgen::run_ftool ("cphead",
                         "infile=$srcfile" . "[0]",
                         "outfile=$destfile" . "[0]", 
                         "keyfil=$kfile", 
                         "comment=yes",
                         "history=yes") ;
    if ($status) {
        ahlog::ah_err "Could not copy primary header keywords." ;
        exit 1;
    }   

    $status = ahgen::run_ftool ("cphead",
                         "infile=$srcfile" . "[1]",
                         "outfile=$destfile" . "[1]", 
                         "keyfil=$kfile", 
                         "comment=yes",
                         "history=yes") ;
    if ($status) {
        ahlog::ah_err "Could not copy extension header keywords." ;
        exit 1;
    }   

    # Fix the table keywords
    $status = ahgen::run_ftool ("fthedit",
                         "infile=$destfile" . "[1]",
                         "keyword=TTYPE1", "operation=add",
                         "value=TIME",
                         "comment=Seconds from 01 Jan 2014 00:00:00");
    if ($status) {
        ahlog::ah_err "Could not fix TTYPE1 header record in det2att2 output file.";
        exit 1;
    }   
    $status = ahgen::run_ftool ("fthedit",
                         "infile=$destfile" . "[1]",
                         "keyword=TTYPE2", "operation=add",
                         "value=QPARAM",
                         "comment=Attitude quaternion");
    if ($status) {
        ahlog::ah_err "Could not fix TTYPE2 header record in det2att2 output file.";
        exit 1;
    }
    $status = ahgen::run_ftool ("fthedit",
                         "infile=$destfile" . "[1]",
                         "insert=EXTNAME", 
                         "keyword=COMMENT", "operation=add",
                         "value=the ORIGSYS and DESTSYS keywords.");
    if ($status) {
        ahlog::ah_err "Could not add descriptive card number 4 in det2att2 output file.";
        exit 1;
    }
    $status = ahgen::run_ftool ("fthedit",
                         "infile=$destfile" . "[1]",
                         "insert=EXTNAME", 
                         "keyword=COMMENT", "operation=add",
                         "value=for the transformation between the coordinate systems specified by");
    if ($status) {
        ahlog::ah_err "Could not add descriptive card number 3 in det2att2 output file.";
        exit 1;
    }
    $status = ahgen::run_ftool ("fthedit",
                         "infile=$destfile" . "[1]",
                         "insert=EXTNAME", 
                         "keyword=COMMENT", "operation=add",
                         "value=the HITOMI/HXI unit specified by the INSTRUME keyword for");
    if ($status) {
        ahlog::ah_err "Could not add descriptive card number 2 in det2att2 output file.";
        exit 1;
    }
    $status = ahgen::run_ftool ("fthedit",
                         "infile=$destfile" . "[1]",
                         "insert=EXTNAME", 
                         "keyword=COMMENT", "operation=add",
                         "value=This file is a delta-attitude file, only applicable to");
    if ($status) {
        ahlog::ah_err "Could not add descriptive card number 1 in det2att2 output file.";
        exit 1;
    }

} # End of cams2att_edit_output_header

#  Subroutine to add ACTX, ACTY columns to output file.
sub cams2att_add_actx_acty {

    my $outfile = shift;
    my $hxiteldef = shift;
    my $status = "";

    my $actoptx = "";
    my $actopty = "";

    my @hxitd_pieces = split(/\[/, $hxiteldef);

    $actoptx = ahgen::get_keyword($hxitd_pieces[0], 0, "OPTACTX");
    $actopty = ahgen::get_keyword($hxitd_pieces[0], 0, "OPTACTY");

    # Call ftcopy with extended syntax (see "fhelp colfilter")
    $status = ahgen::run_ftool ("ftcopy",
                         "infile=$outfile" 
                           . '[col *,' 
                           . "ACTX(I)=$actoptx,ACTY(I)=$actopty,"
                           . "ACTX_FLOAT(D)=$actoptx,ACTY_FLOAT(D)=$actopty"
                           . ']', 
                         "outfile=$outfile", 
                         "clobber=yes");

    if ($status) {
        ahlog::ah_err "Could not add ACT coordinate columns using task ftcopy." ;
        exit 1;
    }   

} # End of cams2att_add_actx_acty

# Subroutine to add EULER and POINTING columns to output file
sub cams2att_add_euler_and_pointing {

    my $outfile = shift;
    my $status = "";

    $status = ahgen::run_ftool ("attconvert",
      "input=$outfile", "inform=QUAT", "outfile=$outfile",
      "outform=EULER,POINTING", "alignfile=STANDARD", 
      "incol=QPARAM", "outcol=EULER,POINTING",
      "clobber=yes");

    if ($status) {
        ahlog::ah_err "Could not add EULER and POINTING columns using task attconvert." ;
        exit 1;
    }   
} # End of cams2att_add_euler_and_pointing

# Subroutine to convert optical axis coordinates to time-dependent RAW
sub cams2att_convert_act_coords_to_raw {

    my $outfile = shift;
    my $hxiteldef = shift;
    my $status = "";

    $status = ahgen::run_ftool ("coordevt",
      "infile=$outfile", "outfile=$outfile", "teldeffile=$hxiteldef",
      "startsys=ACT", "stopsys=RAW", "clobber=yes", "infileext=ATTITUDE",
      "dattfile=$outfile", "attfile=$outfile", 
      "inclfloatcol=YES", "startwithfloat=YES");

    if ($status) {
        ahlog::ah_err "Could not add RAW coordinate columns using task coordevt." ;
        exit 1;
    }   
} # End of cams2att_convert_act_coords_to_raw 

#########################
#  Startup
#########################

# Set the force_debug flag to 1 to force output of debugging
# messages.
#my $force_debug = 1 ;
my $force_debug = '' ;

# turn on AUTOFLUSH
$| = 1;

my $nargs = scalar @ARGV ;

# Query canonical parameters and start logging. 
ahapp::startup ($force_debug) ;

ahlog::ah_debug "number of arguments $nargs" ;

#########################
#  Parameters
#########################

# Query parameters.

my $infile1      = ahapp::query_parameter ("infile1") ;      # Input CAMS1 file
my $infile2      = ahapp::query_parameter ("infile2") ;      # Input CAMS2 file
my $outfile      = ahapp::query_parameter ("outfile") ;      # Output delta attitude file
my $cams1teldef  = ahapp::query_parameter ("cams1teldef") ;  # CAMS1 teldef file (or CALDB)
my $cams2teldef  = ahapp::query_parameter ("cams2teldef") ;  # CAMS2 teldef file (or CALDB)
my $hxiteldef    = ahapp::query_parameter ("hxiteldef") ;    # HXI teldef file (or CALDB)
my $camstempxy   = ahapp::query_parameter ("camstempxy") ;   # CAMS temperature correction file (or CALDB)
my $startstep    = ahapp::query_parameter ("startstep") ;    # Starting step of calculation (1-5)
my $stopstep     = ahapp::query_parameter ("stopstep") ;     # Ending step of calculation (1-5)
my $instrume     = ahapp::query_parameter ("instrume") ;       # HXI unit (HXI1, HXI2)
my $inext        = ahapp::query_parameter ("inext") ;        # Input extension
my $outext       = ahapp::query_parameter ("outext") ;       # Output extension
my $flipsign     = ahapp::query_parameter ("flipsign") ;     # Flip sign of output offsets and angles (YES, NO)
my $tempcorfile1 = ahapp::query_parameter ("tempcorfile1") ; # Temperature corrected file for CAMS1
my $tempcorfile2 = ahapp::query_parameter ("tempcorfile2") ; # Temperature corrected file for CAMS2
my $prefiltfile1 = ahapp::query_parameter ("prefiltfile1") ; # Prefiltered file for CAMS1
my $prefiltfile2 = ahapp::query_parameter ("prefiltfile2") ; # Prefiltered file for CAMS2
my $offsetfile   = ahapp::query_parameter ("offsetfile") ;   # Offset file
my $filtoffset   = ahapp::query_parameter ("filtoffset") ;   # Filtered offset file
my $prefiltexpr  = ahapp::query_parameter ("prefiltexpr") ;  # Expression to filter input files
my $filtexpr     = ahapp::query_parameter ("filtexpr") ;     # Expression to filter offset file                
my $gtiexpr0     = ahapp::query_parameter ("gtiexpr0") ;     # Expression to create GTI for both CAMS
my $gtiexpr1     = ahapp::query_parameter ("gtiexpr1") ;     # Expression to create GTI for CAMS1
my $gtiexpr2     = ahapp::query_parameter ("gtiexpr2") ;     # Expression to create GTI for CAMS2
my $gtifile      = ahapp::query_parameter ("gtifile") ;      # GTI file
my $startsys     = ahapp::query_parameter ("startsys") ;     # Starting coordinate system
my $deltaxcol    = ahapp::query_parameter ("deltaxcol") ;    # Column with change in detector X coordinate
my $deltaycol    = ahapp::query_parameter ("deltaycol") ;    # Column with change in detector Y coordinate
my $coscol       = ahapp::query_parameter ("coscol") ;       # Column with sine of rotation angle
my $sincol       = ahapp::query_parameter ("sincol") ;       # Column with cosine of rotation angle

#########################
#  Other Variables
#########################

my @tmpfiles = ();    # Array of temporary files to be cleaned up
my $status = 0;       # Status returned by an FTOOL
my $naxis2 = 0;       # Number of rows in FITS BINTABLE
my $debug_bool_flag = "no" ;  # String form of debug flag
my $tmptempfile1 = "" ;       # Temporary file in temperature correction
my $tmptempfile2 = "" ;       # Temporary file in temperature correction
my $tmpcams1gtifile = "" ;    # Temporary CAMS1 GTI file
my $tmpcams2gtifile = "" ;    # Temporary CAMS2 GTI file
my $tmpkeywordfile = "" ;     # Temporary keyword copying specification
my $xexpr1 = "" ;     # ftcalc expression for X
my $yexpr1 = "" ;     # ftcalc expression for Y
my $xexpr2 = "" ;     # ftcalc expression for X
my $yexpr2 = "" ;     # ftcalc expression for Y
my $instrume1 = "" ;  # Value of INSTRUME keyword
my $instrume2 = "" ;  # Value of INSTRUME keyword
my $dateobs1 = "" ;  # Value of DATE-OBS keyword
my $dateobs2 = "" ;  # Value of DATE-OBS keyword
my $date1 = "" ;  # Date from DATE-OBS keyword
my $date2 = "" ;  # Date from DATE-OBS keyword
my $time1 = "" ;  # Time from DATE-OBS keyword
my $time2 = "" ;  # Time from DATE-OBS keyword
my $this_dateobs = "" ;  # Value of DATE-OBS keyword

#
#  Variables read from CALDB for CAMS temperature correction
#
my $caldbqueryresult = "" ;  # Result of calling quzcif to query CALDB
my @camscaldb = () ;       # Filename and extension number
my @derivx1 = () ;         # dX/dT [mm/K] (CAMS1)
my @derivy1 = () ;         # dY/dT [mm/K] (CAMS1)
my @temperaturex1 = () ;   # fiducial T for X correction [K] (CAMS1)
my @temperaturey1 = () ;   # fiducial T for Y correction [K] (CAMS1)
my @derivx2 = () ;         # dX/dT [mm/K] (CAMS2)
my @derivy2 = () ;         # dY/dT [mm/K] (CAMS2)
my @temperaturex2 = () ;   # fiducial T for X correction [K] (CAMS2)
my @temperaturey2 = () ;   # fiducial T for Y correction [K] (CAMS2)

#
#  For optical axis processing
#
my $actual_hxiteldef = "" ; # Result of querying CALDB for HXI TELDEF file

#########################
#  Main Code Block
#########################

# Convert debug flag into a yes/no boolean string required by some FTOOLS.
$debug_bool_flag = ($ahapp::debug)? "yes" : "no" ;

ahapp::begin_processing();

ahapp::print_input_parameters();

#
#  Truly temporary files (not like the intermediate files above)
#

if ($startstep <= 1) {

    #
    #  Step 1: Correct CAMS displacement for temperature
    #

    #  Check header of input file 2 to see if the temperature correction has
    #  been done on board.

    # Need to wrap calls to ftcalc in tests whether the input files exist,
    # because cams2det allows NONE for an input file.

    unless ($infile1 =~ /^\s*none\s*$/i) {

        unless (-e $infile1) { 
            ahlog::ah_err "Input file $infile1 not found." ;
            exit 1 ;
        }

        # Verify the CAMS unit.
        $instrume1 = ahgen::get_keyword($infile1, 1, "INSTRUME");
        unless ($instrume1 =~ /^cams1$/i) {
            ahlog::ah_err "INSTRUME in $infile1 is $instrume1 (should be CAMS1)." ;
            exit 1
        }

        # Get temperature correction CALDB.
        if ($camstempxy =~ /^\s*caldb\s*$/i) {
            $dateobs1 = ahgen::get_keyword($infile1, 1, "DATE-OBS");
            if (defined($dateobs1)) {
                ahlog::ah_info "HIGH", qq{DATE-OBS in $infile1 is '$dateobs1'.} ;
                unless ($dateobs1 =~ /^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d*)?$/) {
                    ahlog::ah_err "Bad format for DATE-OBS";
                    exit 1;
                }
            } else {
                ahlog::ah_info "HIGH", qq{DATE-OBS not found in $infile1.} ;
                ahlog::ah_info "HIGH", qq{Substituting "now".};
                $dateobs1 = "now";
            }
            ahlog::ah_info "HIGH", "Querying CALDB for TEMP_CORRECTION file.";
            $caldbqueryresult = ahfilterlib::call_quzcif($camstempxy, "CAMS1", "-",
                "TEMP_CORRECTION", $dateobs1, "-", "HITOMI");
            ahlog::ah_info "LOW", "Result from task quzcif: $caldbqueryresult";

            # All that is needed is the filename.  The extension numbers are
            # assumed by this script even though they might be specified
            # in the CALDB index as well.  The value of $camstempxy will
            # persist for the CAMS2 processing, so if the file is found for
            # CAMS1, the query will not be done a second time.
            @camscaldb = split(/[\[\]]/, $caldbqueryresult);
            $camstempxy = $camscaldb[0];
            ahlog::ah_info "HIGH", "Found TEMP_CORRECTION file in CALDB: $camstempxy"
        }

        # Set up true temporary file.
        $tmptempfile1 = "tmp_ftcalcfile1.fits" ;
        unlink $tmptempfile1 if -e $tmptempfile1;
        push (@tmpfiles, $tmptempfile1) ;

        # Set up intermediate file, which may or may not be kept.
        cams2att_check_interm_file (\$tempcorfile1, $startstep, 2, "tmp_tempcorfile1.fits", 
            $ahapp::clobber, \@tmpfiles) ;

        #  Check header of input file 1 to see if the temperature correction has
        #  been done on board.

        if (cams2att_skip_corrtemp($infile1)) {

            $xexpr1 = "X_RAW" ;
            $yexpr1 = "Y_RAW" ;

        } else {
          
            # Written with the idea that CAMS1 is in row 1 of extension 1 and CAMS2 
            # is in row 1 of extension 2 of the temperature coefficient CALDB file.  

            ahgen::set_quiet(1);
            @derivx1 = ahgen::read_column ($camstempxy, 1, "RT1_XCOEFF", 1, 1);
            @derivy1 = ahgen::read_column ($camstempxy, 1, "RT1_YCOEFF", 1, 1);
            @temperaturex1 = ahgen::read_column ($camstempxy, 1, "RT1_XOFFSET", 1, 1);
            @temperaturey1 = ahgen::read_column ($camstempxy, 1, "RT1_YOFFSET", 1, 1);
            ahgen::set_quiet(0);

            $xexpr1 = "X_RAW+($derivx1[0])*(THERMISTOR1_CAL-($temperaturex1[0]))" ;
            $yexpr1 = "Y_RAW+($derivy1[0])*(THERMISTOR1_CAL-($temperaturey1[0]))" ;
        }

        ahlog::ah_info "LOW", "X=$xexpr1\n";
        ahlog::ah_info "LOW", "Y=$yexpr1\n";

        $status = ahgen::run_ftool ("ftcalc", $infile1, $tmptempfile1, 
            "X", $xexpr1) ;

        if  ($status) {
            ahlog::ah_err "Errors detected trying to run ftool: ftcalc." ;
            ahlog::ah_err ahgen::get_tool_stderr () ;
            exit 1 ;
        }

        $status = ahgen::run_ftool ("ftcalc", $tmptempfile1, $tempcorfile1, 
            "Y", $yexpr1) ;

        if  ($status) {
            ahlog::ah_err "Errors detected trying to run ftool: ftcalc." ;
            ahlog::ah_err ahgen::get_tool_stderr () ;
            exit 1 ;
        }
    }

    unless ($infile2 =~ /^\s*none\s*$/i) {
        unless (-e $infile2) { 
            ahlog::ah_err "Input file $infile2 not found." ;
            exit 1 ;
        }

        # Get temperature correction CALDB filename.
        # The value of $camstempxy may be filled in by the CAMS1 query above.
        # If so, the query will be skipped here, because this filename is 
        # valid for CAMS2 as well.
        # Get temperature correction CALDB.
        if ($camstempxy =~ /^\s*caldb\s*$/i) {
            $dateobs2 = ahgen::get_keyword($infile2, 1, "DATE-OBS");
            if (defined($dateobs2)) {
                ahlog::ah_info "HIGH", qq{DATE-OBS in $infile2 is '$dateobs2'.} ;
                unless ($dateobs2 =~ /^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d*)?$/) {
                    ahlog::ah_err "Bad format for DATE-OBS";
                    exit 1;
                }
            } else {
                ahlog::ah_info "HIGH", qq{DATE-OBS not found in $infile2.} ;
                ahlog::ah_info "HIGH", qq{Substituting "now".};
                $dateobs2 = "now";
            }
            ahlog::ah_info "HIGH", "Querying CALDB for TEMP_CORRECTION file.";
            $caldbqueryresult = ahfilterlib::call_quzcif($camstempxy, "CAMS2", "-",
                "TEMP_CORRECTION", $dateobs2, "-", "HITOMI");
            ahlog::ah_info "LOW", "Result from task quzcif: $caldbqueryresult";

            @camscaldb = split(/[\[\]]/, $caldbqueryresult);
            $camstempxy = $camscaldb[0];
            ahlog::ah_info "HIGH", "Found TEMP_CORRECTION file in CALDB: $camstempxy"
        }

        # Verify the CAMS unit.
        $instrume2 = ahgen::get_keyword($infile2, 1, "INSTRUME");
        unless ($instrume2 =~ /^cams2$/i) {
            ahlog::ah_err "INSTRUME in $infile2 is $instrume2 (should be CAMS2)." ;
            exit 1;
        }

        # Set up true temporary file.
        $tmptempfile2 = "tmp_ftcalcfile2.fits" ;
        unlink $tmptempfile2 if -e $tmptempfile2;
        push (@tmpfiles, $tmptempfile2) ;

        # Set up intermediate file, which may or may not be kept.
        cams2att_check_interm_file (\$tempcorfile2, $startstep, 2, "tmp_tempcorfile2.fits", 
          $ahapp::clobber, \@tmpfiles) ;

        if (cams2att_skip_corrtemp($infile2)) {

            $xexpr2 = "X_RAW" ;
            $yexpr2 = "Y_RAW" ;

        } else {

            ahgen::set_quiet(1);
            @derivx2 = ahgen::read_column ($camstempxy, 2, "RT1_XCOEFF", 1, 1);
            @derivy2 = ahgen::read_column ($camstempxy, 2, "RT1_YCOEFF", 1, 1);
            @temperaturex2 = ahgen::read_column ($camstempxy, 2, "RT1_XOFFSET", 1, 1);
            @temperaturey2 = ahgen::read_column ($camstempxy, 2, "RT1_YOFFSET", 1, 1);
            ahgen::set_quiet(0);

            $xexpr2 = "X_RAW+($derivx2[0])*(THERMISTOR1_CAL-($temperaturex2[0]))" ;
            $yexpr2 = "Y_RAW+($derivy2[0])*(THERMISTOR1_CAL-($temperaturex2[0]))" ;

        }

        ahlog::ah_info "LOW", "X=$xexpr2\n";
        ahlog::ah_info "LOW", "Y=$yexpr2\n";

        $status = ahgen::run_ftool ("ftcalc", $infile2, $tmptempfile2, 
            "X", $xexpr2) ;

        if  ($status) {
            ahlog::ah_err "Errors detected trying to run ftool: ftcalc." ;
            ahlog::ah_err ahgen::get_tool_stderr () ;
            exit 1 ;
        }

        $status = ahgen::run_ftool ("ftcalc", $tmptempfile2, $tempcorfile2, 
            "Y", $yexpr2) ;

        if  ($status) {
            ahlog::ah_err "Errors detected trying to run ftool: ftcalc." ;
            ahlog::ah_err ahgen::get_tool_stderr () ;
            exit 1 ;
        }
    }
}  # End if $startstep <= 1

if ($startstep <= 2 && $stopstep >= 2) {

    #
    #  Step 2: Prefilter
    #

    # Need to wrap the actions in tests for input files specified as NONE.

    unless ($infile1 =~ /^\s*none\s*$/i) {

        # Set up intermediate file.
        cams2att_check_interm_file (\$prefiltfile1, $startstep, 3, "tmp_prefiltfile1.fits", 
            $ahapp::clobber, \@tmpfiles) ;

        $status = ahgen::run_ftool ("ftselect", $tempcorfile1, $prefiltfile1,
            $prefiltexpr) ;

        if  ($status) {
            ahlog::ah_err "Errors detected trying to run ftool: ftselect." ;
            ahlog::ah_err ahgen::get_tool_stderr () ;
            exit 1 ;
        }
    }

    unless ($infile2 =~ /^\s*none\s*$/i) {

        # Set up intermediate file.
        cams2att_check_interm_file (\$prefiltfile2, $startstep, 3, "tmp_prefiltfile2.fits", 
            $ahapp::clobber, \@tmpfiles) ;
            
        $status = ahgen::run_ftool ("ftselect", $tempcorfile2, $prefiltfile2,
            $prefiltexpr) ;

        if  ($status) {
            ahlog::ah_err "Errors detected trying to run ftool: ftselect." ;
            ahlog::ah_err ahgen::get_tool_stderr () ;
            exit 1 ;
        }
    }

}  # End if $startstep <= 2 && $stopstep >= 2

if ($startstep <= 3 && $stopstep >= 3) {

    #
    #  Step 3: Run cams2det
    #

    ahlog::ah_debug "Starting CAMS2DET on $infile1 and $infile2." ; 

    # Check to make sure that the required teldef files exist.
    # unless (-e $cams1teldef) { 
    #     ahlog::ah_err "TELDEF file $cams1teldef not found." ;
    #     exit 1 ;
   #  }

    # unless (-e $cams2teldef) { 
     #    ahlog::ah_err "TELDEF file $cams2teldef not found." ;
      #   exit 1 ;
    # }

    # unless (-e $hxiteldef) { 
     #    ahlog::ah_err "TELDEF file $hxiteldef not found." ;
      #   exit 1 ;
    # }

    # Set up intermediate file.
    cams2att_check_interm_file (\$offsetfile, $startstep, 4, "tmp_offsetfile.fits", 
        $ahapp::clobber, \@tmpfiles) ;

    $status = ahgen::run_ftool ("cams2det", 
                         "infile1=$prefiltfile1", 
                         "infile2=$prefiltfile2",
                         "outfile=$offsetfile",
                         "instrume=$instrume",
                         "cams1teldef=$cams1teldef",
                         "cams2teldef=$cams2teldef",
                         "hxiteldef=$hxiteldef",
                         "inext=$inext",
                         "outext=$outext",
                         "startsys=$startsys", 
                         "flipsign=$flipsign",
                         "clobber=yes",
                         "chatter=1",
                         "debug=$debug_bool_flag"
                         ) ;              

    if  ($status) {
        ahlog::ah_err "Errors detected trying to run HITOMI Tool: cams2det." ;
        ahlog::ah_err ahgen::get_tool_stderr () ;
        exit 1 ;
    }

    # Check that cams2det produced actual data.  If data was produced (NAXIS2 = 0)
    # flag an error and exit.

    # Get the value of the NAXIS2 keyword.
    $naxis2 = ahgen::get_keyword ($offsetfile, 1, "NAXIS2") ;

    # Check for an error while getting the value of the keyword.
    unless (defined $naxis2) {
        ahlog::ah_err "Could not retrieve value of NAXIS2 keyword from file: " .
               "$offsetfile." ;
        exit 1 ;
    }

    ahlog::ah_info "HIGH", "CAMS2DET generated a FITS table with $naxis2 rows." ;

    # Make sure that NAXIS2 is not 0.
    if  ($naxis2 == 0) {
        ahlog::ah_err "Ftool CAMS2DET did not generate any output (NAXIS2 = 0)." ;
        exit 1 ;
    }
}  # End if $startstep <= 3 && $stopstep >= 3


if ($startstep <= 4 && $stopstep >= 4) {

    #
    #  Step 4:  Filtering
    #

    # Run MAKETIME.

    ahlog::ah_debug "Starting MAKETIME on $offsetfile using expression: $gtiexpr0" ; 

    $status = ahgen::run_ftool ("maketime", 
                         "infile=$offsetfile" . "[1]",
                         "outfile=$gtifile",
                         "expr=$gtiexpr0",
                         "copykw=YES",
                         "histkw=YES",
                         "time=TIME",
                         "start=START",
                         "stop=STOP",
                         "compact=no",
                         "clobber=yes"
                         ) ;

    if  ($status) {
        ahlog::ah_err "Errors detected trying to run maketime for both CAMS units." ;
        ahlog::ah_err ahgen::get_tool_stderr () ;
        exit 1 ;
    }

    # If MAKETIME fails to generate output file or generates zero-length file,
    # force a valid output GTI file with no rows, using ftcopy with column and row
    # filters.
    if (!(-e $gtifile) || -z $gtifile) {

        $status = ahgen::run_ftool ("ftcopy", 
                             "infile=$filtoffset" . "[1][col START=0.0;STOP=0.0][#row>1&&#row<1]",
                             "outfile=$gtifile", "copyall=YES", "clobber=yes"
                             ) ;
        if  ($status) {
            ahlog::ah_err "Errors detected trying to run ftcopy to force GTI file." ;
            ahlog::ah_err ahgen::get_tool_stderr () ;
            exit 1 ;
        }
    }

    # Set some keywords.

    ahgen::set_keyword ($gtifile, 1, "EXTNAME", "GTI") ;

    ahapp::write_parameters ($gtifile, 1) ;

    ahlog::ah_debug "Starting FTSELECT on $offsetfile." ; 

    # Set up intermediate file, which may or may not be kept.
    cams2att_check_interm_file (\$filtoffset, $startstep, 5, "tmp_filtoffset.fits", 
        $ahapp::clobber, \@tmpfiles) ;

    $status = ahgen::run_ftool ("ftselect", 
                         "infile=$offsetfile" . "[1]",
                         "outfile=$filtoffset",
                         "expr=$filtexpr && gtifilter(\"$gtifile\")", 
                         "copyall=yes",
                         "clobber=yes",
                         "chatter=1"
                         ) ;

    if  ($status) {
        ahlog::ah_err "Errors detected trying to run ftselect." ;
        ahlog::ah_err ahgen::get_tool_stderr () ;
        exit 1 ;
    }

    # Get the value of the NAXIS2 keyword from FITS file created by ftselect.
    $naxis2 = ahgen::get_keyword ($filtoffset, 1, "NAXIS2") ;

    # Check for an error while getting the value of the keyword.
    unless (defined $naxis2) {
        ahlog::ah_err "Could not retrieve value of NAXIS2 keyword from file: " .
               "$filtoffset." ;
        exit 1 ;
    }

    ahlog::ah_info "HIGH", "FTSELECT generated a FITS table with $naxis2 rows." ;

    # Make sure that NAXIS2 is not 0.
    if  ($naxis2 == 0) {
        ahlog::ah_err "Ftool FTSELECT did not generate any output (NAXIS2 = 0)." ;
        exit 1 ;
    }

    ahapp::write_parameters ($filtoffset, 1) ;

}  # End if $startstep <= 4 && $stopstep >= 4.

if ($stopstep >= 5)
    {

    #
    #  Step 5:  Run det2att2.
    #

    ahlog::ah_debug "Starting DET2ATT2 on $filtoffset." ; 

    # Check if the output file already exists.  Unless clobber is set, this will
    # cause the script to fail.
    unless ($ahapp::clobber) {
        if  (-e $outfile) {
            ahlog::ah_err "Output file already exists but clobber was not set." ;
            exit 1 ;
        }
    }

    # If $outfile already exists, delete it.  We have already checked that clobber
    # has been set.
    unlink $outfile if -e $outfile ;

    $status = ahgen::run_ftool ("det2att2",
                         "infile=$filtoffset" . "[1]",
                         "outfile=$outfile",
                         "teldef=$hxiteldef",
                         "startsys=$startsys",
                         "deltaxcol=$deltaxcol",
                         "deltaycol=$deltaycol",
                         "coscol=$coscol",
                         "sincol=$sincol",
                         "clobber=yes"
                         ) ;

    if  ($status) {
        ahlog::ah_err "Errors detected trying to run det2att2." ;
        ahlog::ah_err ahgen::get_tool_stderr () ;
        exit 1 ;
    }

    # Set some keywords.

    ahapp::write_parameters ($outfile, 1) ;

    # Get the value of the NAXIS2 keyword from FITS file created by det2att2.
    $naxis2 = ahgen::get_keyword ($outfile, 1, "NAXIS2") ;

    # Check for an error while getting the value of the keyword.
    unless (defined $naxis2) 
        {
        ahlog::ah_err "Could not retrieve value of NAXIS2 keyword from file: " .
               "$filtoffset." ;
        exit 1 ;
        }
    ahlog::ah_info "HIGH", "DET2ATT2 generated a FITS table with $naxis2 rows." ;

    # Make sure that NAXIS2 is not 0.
    if  ($naxis2 == 0) {
        ahlog::ah_err "Ftool DET2ATT2 did not generate any output (NAXIS2 = 0)." ;
        exit 1;
    }

    # Set up true temporary file for keyword copying.
    $tmpkeywordfile = "tmp_cpheadkeyfil.txt" ;
    unlink $tmpkeywordfile if -e $tmpkeywordfile;
    push (@tmpfiles, $tmpkeywordfile) ;

    # Header edit.
    $status = cams2att_edit_output_header($tmpkeywordfile, $filtoffset, $outfile);
    if ($status) {
        ahlog::ah_err "Could not edit the output file header.";
        exit 1;
    }   

    # Find HXI TELDEF file; we cannot depend on a called task (e.g., cams2det)
    # for this, because we don't know what processing steps are being run
    $this_dateobs = ahgen::get_keyword($outfile, 1, "DATE-OBS") ;
    $actual_hxiteldef = ahfilterlib::call_quzcif($hxiteldef, $instrume, "-",
                "TELDEF", $this_dateobs, "-", "HITOMI");

    # Add optical axis coordinate columns.
    $status = cams2att_add_actx_acty($outfile, $actual_hxiteldef);
    if ($status) {
        ahlog::ah_err "Could not add optical axis columns to output file.";
        exit 1;
    }

    # Convert attitude.
    $status = cams2att_add_euler_and_pointing($outfile);
    if ($status) {
        ahlog::ah_err "Could not convert attitude to Euler angles and pointing.";
        exit 1;
    }

    # Convert optical axis coordinates to RAW.
    $status = cams2att_convert_act_coords_to_raw($outfile, $actual_hxiteldef);
    if ($status) {
        ahlog::ah_err "Could not convert ACTX and ACTY to RAWX and RAWY.";
        exit 1;
    }


}  # End if $stopstep >= 5.

foreach (@tmpfiles) { ahapp::add_temp_file($_); }

# Done.
ahapp::end_processing($status);

# $Log: cams2att.pl,v $
# Revision 1.35  2016/03/22 19:33:58  rshill
# Added cvs log listing to bottom of script.
#
