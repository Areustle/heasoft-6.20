#!/usr/bin/perl
#
# File name: ahfilterlib.pm
#  
# Description:
#
# This library contains several subroutines for filtering, merging and
# reading Hitomi FITS files.
#
# Author: A. J. Sargent NASA GSFC 
#
# $Date: 2016/04/18 20:55:16 $
#
# History:
#

package ahfilterlib ;

use strict ;
use warnings;

use ahlog ;
use ahgen ;

use File::Basename ;

############################
#      FITS Subroutines
############################

# 
# Description: 
#
# Merge event files using ftmerge
#
# Parameters:
# [in] infile_list Input @filelist of files to merge
# [in] outfile     Output merged event file 
# [out] status     Return 0 for successful creation of merged event file
#
sub merge_events ($$$) {

  my @infile_list = @{shift()};
  my $outfile = shift;
  my $copyall= shift;

  my $toolname="ftmerge";
  my $status = 0;

  if( @infile_list == 0 ) {
    # No input files, throw error
    ahlog::ah_err "No input files to merge";
    return 1;
  }

  if( @infile_list == 1 ) {
    # Only one input file. Just copy to output file
    if (ahgen::copyFITSFile($infile_list[0],$outfile)) { return 1; }
    return 0;
  } 

  # Calculate input file name
  my $infile = join " ", @infile_list;
  my $filelist = "tmpfilelist.in";
  if(length($infile) > 80 ) {
    open  LIST, ">$filelist";
    print LIST join "\n", @infile_list;
    print LIST "\n"; # FTOOLS need this
    close LIST;
    $infile = "\@$filelist";
    ahlog::ah_info "LOW", "Merging Event Files:\n";
    ahlog::ah_info "LOW", "  " . join "\n  ", @infile_list;
  }

  # Setup input/output parameters for mgtime
  my $tmpoutfile=ahgen::formTemporaryFileName("files.in",$toolname);
  my @params = (
    ['infile'   , $infile],
    ['outfile'  , $tmpoutfile],
    ['copyall'  , $copyall],
  );

  # Run ftmerge
  # The output table header will be identical to the first input table
  # header, except for the value of the NAXIS2 keyword that specifies the
  # number of rows in the table.
  if (ahgen::runTool($toolname,\@params)) { unlink $tmpoutfile; unlink $filelist; return 1; }
  unlink $filelist;
  if (ahgen::copyFITSFile($tmpoutfile, $outfile)) { unlink $tmpoutfile; return 1; }
  unlink $tmpoutfile;

  # Verify output file
  unless (ahgen::check_fits_file($outfile)) {
    ahlog::ah_err "File $outfile failed file verification (ftverify)";
    return ahgen::get_error_flag;
  }

  return $status;

}

# 
# Description: 
#
# Merge GTI files using mgtime
#
# Parameters:
# [in] infile_list Input @filelist of files to merge
# [in] outgti      Output merged GTI file 
# [in] merge       GTI merging mode: AND or OR
# [in] outext      Extension name of the merged GTI
# [in] mgtime_pars Hash array of parameters used in mgtime
# [out] status     Returns number of GTI files merged
#
sub merge_gti ($$$$$) {

  my @infile_list = @{shift()};
  my $outgti = shift;
  my $merge = shift;
  my $outext = shift;
  my %mgtime_pars = %{shift()};

  my @gtifiles = ();
  my $toolname="mgtime";
  my $status = 0;

  if( @infile_list == 0 ) {
    # No input GTI files, throw error
    ahlog::ah_err "No input GTI files to merge";
    return 1;
  }

  if( @infile_list == 1 ) {
    # Only one input GTI file. Just copy to output file
    my $ingti = $infile_list[0];

    # Check if a GTI extension was given.
    # If not, search for the first extension
    # with HDUCLAS1 equal to GTI
    my @filename = ahgen::parse_file_name($ingti);
    if ( $filename[1] eq "" ) {
      # Count the number of HDUs in the input file
      my $nhdu = ahgen::get_total_hdu($ingti);
      my $foundhdu=0;
      # Loop through each HDU and read the HDUCLAS1 keyword
      foreach my $hdu ( 1..$nhdu ) {
        # Find the first HDUCLAS1 == "GTI"
        my $hduclas1 = ahgen::get_keyword($ingti, $hdu, "HDUCLAS1" );
        if ( defined $hduclas1 ) {
          # If the HDUCLAS1 == GTI, set the HDU number
          if ( $hduclas1 eq "GTI" ) { 
            $ingti = $ingti . "[" . $hdu . "]"; 
            $foundhdu=1;
            last; 
          }
        }
      }
      unless ( $foundhdu ) { ahlog::ah_err "No GTI extension found in $ingti."; return 1; }
    }
    # *** The extension should be specified if there is only one input GTI file
    if (ahgen::copy_fits_file($ingti,$outgti,"copyall=no")) { return 1; }
    if (ahgen::set_keyword($outgti,"1","EXTNAME",$outext)) { return 1; }
    return 0;
  } 

  # Calculate input file name
  my $infile = join " ", @infile_list;
  my $gtilist = "tmpgtilist.gti";
  if(length($infile) > 80 ) {
    open  LIST, ">$gtilist";
    print LIST join "\n", @infile_list;
    print LIST "\n"; # FTOOLS need this
    close LIST;
    $infile = "\@$gtilist";
    ahlog::ah_info "LOW", "Merging GTI Files:\n";
    ahlog::ah_info "LOW", "  " . join "\n  ", @infile_list;
  }

  # Setup input/output parameters for mgtime
  my $tmpoutfile=ahgen::formTemporaryFileName("in.gti",$toolname);
  my @params = (
    ['ingtis'   , $infile],
    ['outgti'   , $tmpoutfile],
    ['merge'    , $merge],
  );
  # ignore the indates parameter unless specified
  if(!$mgtime_pars{indates}) { $mgtime_pars{indates} = " "; }
  my @ordered_pars = qw( instarts instops indates outstart outstop clobber );
  foreach my $par (@ordered_pars) {
    if(defined $mgtime_pars{$par}) { push @params, [$par, $mgtime_pars{$par}]; }
  }

  # Run mgtime
  if (ahgen::runTool($toolname,\@params)) { unlink $tmpoutfile; unlink $gtilist; return 1; }
  unlink $gtilist;

  # Update the output extension name
  # If the inputs to mgtime contain zero rows, the header from the first
  # input file is copied, including the extension name.
  if(ahgen::set_keyword($tmpoutfile,"1","EXTNAME",$outext)) { return 1; }

  # mgtime adds TIMEZERO keyword that we do not want. Delete any TIMEZERO keywords
  # If there were no rows in the final output GTI, then mgtime just copies
  # the header from the first GTI file and outputs a GTI with zero rows
  # Check if there are more than 0 rows and if so, then delete the TIMEZERO keyword
  my $naxis2 = ahgen::get_keyword($tmpoutfile,$outext,"NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $tmpoutfile, NAXIS2 keyword not defined.\n";
    return ahgen::get_error_flag;
  }
  if($naxis2 != 0) {
    ahgen::run_ftool("fthedit",$tmpoutfile."[$outext]","TIMEZERO","d");
  }

  # Move the temporary file to the output gti file
  if (ahgen::copyFITSFile($tmpoutfile, $outgti)) { unlink $tmpoutfile; return 1; }
  unlink $tmpoutfile; 

  # Verify output file
  if($naxis2 != 0) {
    unless (ahgen::check_fits_file($outgti)) {
      ahlog::ah_err "File $outgti failed file verification (ftverify)";
      return 1;
    }
  }

  return 0;

}

# 
# Description: 
#
# Copy a list of keywords or all keywords from one file to another.
# The extensions must be 
#
# Parameters:
# [in] infile      Input file to copy keys from
# [in] inext       Extension to read keywords from infile
# [in] outfile     Output file to copy keys to
# [in] outext      Extension to read keywords from infile
# [in] filetype    Keyword types to copy (Primary, HK, Event, Rate, Timing)
# [in] category    Category type to copy (all, coord, proc, obs)
# [out] status     Return 0 for successful keyword(s) copy
#
sub copy_keywords ($$$$$$) {

  # Copy header from the input to the output file
  # If cpkeys is set to 'ALL' copy entire header
  # Otherwise set specified keywords
  my $infile = shift;
  my $inext = shift;
  my $outfile = shift;
  my $outext = shift;
  my $filetype = shift;
  my $category = shift; 

  my $keys_file = "keys.list";
  my @keys = ();

  my $status = 0;

  # Coordinate keywords
  my @coordPrimaryKeys = split " ", q(
      RA_OBJ DEC_OBJ EQUINOX RADECSYS RA_NOM DEC_NOM PA_NOM
  );
  my @coordHKKeys = split " ", q(
      RA_OBJ DEC_OBJ EQUINOX RADECSYS RA_NOM DEC_NOM PA_NOM
  );
  my @coordEventKeys = split " ", q(
      RA_OBJ DEC_OBJ RA_PNT DEC_PNT EQUINOX RADECSYS RA_NOM DEC_NOM PA_NOM
      ABERRAT FOLOWSUN OPTDETX OPTDETY OPTFOCX OPTFOCY OPTSKYX OPTSKYY
  );
  my @coordRateKeys = split " ", q(
      RA_OBJ DEC_OBJ RA_PNT DEC_PNT EQUINOX RADECSYS RA_NOM DEC_NOM PA_NOM
      ABERRAT FOLOWSUN OPTDETX OPTDETY OPTFOCX OPTFOCY OPTSKYX OPTSKYY
  );
  my @coordGTIKeys = split " ", q(
      RA_OBJ DEC_OBJ EQUINOX RADECSYS RA_NOM DEC_NOM PA_NOM
  );

  # Observation keywords
  my @obsPrimaryKeys = split " ", q(
    TELESCOP INSTRUME OBS_ID OBJECT OBSERVER OBS_MODE
  );
  my @obsHKKeys = split " ", q(
    TELESCOP INSTRUME OBS_ID OBJECT OBSERVER OBS_MODE
    HDUCLASS HDUCLAS1 HDUCLAS2 ORBFILE ATTFILE TIMFILE
    RPTFILE# LEAPFILE
  );
  my @obsEventKeys = split " ", q(
    TELESCOP INSTRUME DETNAM FILTER DATAMODE
    OBS_ID OBJECT OBSERVER OBS_MODE
    HDUCLASS HDUCLAS1 ORBFILE ATTFILE TIMFILE
    RPTFILE# LEAPFILE
  );
  my @obsRateKeys = split " ", q(
    TELESCOP INSTRUME DETNAM FILTER DATAMODE
    OBS_ID OBJECT OBSERVER OBS_MODE
    HDUCLASS HDUCLAS1 ORBFILE ATTFILE TIMFILE
    RPTFILE# LEAPFILE
  );
  my @obsGTIKeys = split " ", q(
    TELESCOP INSTRUME DETNAM FILTER DATAMODE OBS_ID 
    OBJECT OBSERVER OBS_MODE 
  );

  # Processing keywords
  my @procPrimaryKeys = split " ", q(
    TLM2FITS PROCVER SEQPNUM MKFFF ORIGIN SOFTVER CALDBVER
  );
  my @procHKKeys = split " ", q(
    TLM2FITS PROCVER SEQPNUM MKFFF ORIGIN SOFTVER CALDBVER
  );
  my @procEventKeys = split " ", q(
    TLM2FITS PROCVER SEQPNUM MKFFF ORIGIN SOFTVER CALDBVER
  );
  my @procRateKeys = split " ", q(
    TLM2FITS PROCVER SEQPNUM MKFFF ORIGIN SOFTVER CALDBVER
  );
  my @procGTIKeys = split " ", q(
    TLM2FITS PROCVER SEQPNUM MKFFF ORIGIN SOFTVER CALDBVER
  );

  # Timing keywords
  my @timingPrimaryKeys = split " ", q(
    TIMESYS MJDREFI MJDREFF TIMEUNIT TIMEREF TASSIGN
    GPSOFFET CLOCKAPP TSTART TSTOP TELAPSE
    DATE-OBS DATE-END SMUUNIT
  );
  my @timingHKKeys = split " ", q(
    TIMESYS MJDREFI MJDREFF TIMEUNIT TIMEREF TASSIGN
    GPSOFFET CLOCKAPP TSTART TSTOP TELAPSE
    DATE-OBS DATE-END SMUUNIT
  );
  my @timingEventKeys = split " ", q(
    TIMESYS MJDREFI MJDREFF TIMEUNIT TIMEREF TASSIGN
    GPSOFFET CLOCKAPP TSTART TSTOP TELAPSE
    DATE-OBS DATE-END SMUUNIT ONTIME LIVETIME
    EXPOSURE TIMEPIXR TIMEDEL
  );
  my @timingRateKeys = split " ", q(
    TIMESYS MJDREFI MJDREFF TIMEUNIT TIMEREF TASSIGN
    GPSOFFET CLOCKAPP TSTART TSTOP TELAPSE
    DATE-OBS DATE-END SMUUNIT ONTIME LIVETIME
    EXPOSURE TIMEPIXR TIMEDEL
  );
  my @timingGTIKeys = split " ", q(
    TIMESYS MJDREFI MJDREFF TIMEUNIT TIMEREF TASSIGN
    GPSOFFET CLOCKAPP TSTART TSTOP TELAPSE
    DATE-OBS DATE-END SMUUNIT LTISTART LTISTOP
  );


  # Set the keywords to copy 
  if(lc $category eq "all") {
    if (lc $filetype eq "primary") {
      @keys = (@coordPrimaryKeys, @procPrimaryKeys, @obsPrimaryKeys, @timingPrimaryKeys);
    } elsif(lc $filetype eq "events") {
      @keys = (@coordEventKeys, @procEventKeys, @obsEventKeys, @timingEventKeys);
    } elsif (lc $filetype eq "hk") {
      @keys = (@coordHKKeys, @procHKKeys, @obsHKKeys, @timingHKKeys);
    } elsif (lc $filetype eq "rate") {
      @keys = (@coordRateKeys, @procRateKeys, @obsRateKeys, @timingRateKeys);
    } elsif (lc $filetype eq "gti") {
      @keys = (@coordGTIKeys, @procGTIKeys, @obsGTIKeys, @timingGTIKeys);
    } else {
      ahlog::ah_err "Invalid filetype for copying keywords";
      return 1;
    }
  } elsif (lc $category eq "coord") {
    if (lc $filetype eq "primary") {
      @keys = @coordPrimaryKeys;
    } elsif(lc $filetype eq "events") {
      @keys = @coordEventKeys;
    } elsif (lc $filetype eq "hk") {
      @keys = @coordHKKeys;
    } elsif (lc $filetype eq "rate") {
      @keys = @coordRateKeys;
    } elsif (lc $filetype eq "gti") {
      @keys = @coordGTIKeys;
    } else {
      ahlog::ah_err "Invalid filetype for copying keywords";
      return 1;
    }
  } elsif (lc $category eq "proc") {
    if (lc $filetype eq "primary") {
      @keys = @procPrimaryKeys;
    } elsif(lc $filetype eq "events") {
      @keys = @procEventKeys;
    } elsif (lc $filetype eq "hk") {
      @keys = @procHKKeys;
    } elsif (lc $filetype eq "rate") {
      @keys = @procRateKeys;
    } elsif (lc $filetype eq "gti") {
      @keys = @procGTIKeys;
    } else {
      ahlog::ah_err "Invalid filetype for copying keywords";
      return 1;
    }
  } elsif (lc $category eq "obs") {
    if (lc $filetype eq "primary") {
      @keys = @obsPrimaryKeys;
    } elsif(lc $filetype eq "events") {
      @keys = @obsEventKeys;
    } elsif (lc $filetype eq "hk") {
      @keys = @obsHKKeys;
    } elsif (lc $filetype eq "rate") {
      @keys = @obsRateKeys;
    } elsif (lc $filetype eq "gti") {
      @keys = @obsGTIKeys;
    } else {
      ahlog::ah_err "Invalid filetype for copying keywords";
      return 1;
    }
  } elsif (lc $category eq "timing") {
    if (lc $filetype eq "primary") {
      @keys = @timingPrimaryKeys;
    } elsif(lc $filetype eq "events") {
      @keys = @timingEventKeys;
    } elsif (lc $filetype eq "hk") {
      @keys = @timingHKKeys;
    } elsif (lc $filetype eq "rate") {
      @keys = @timingRateKeys;
    } elsif (lc $filetype eq "gti") {
      @keys = @timingGTIKeys;
    } else {
      ahlog::ah_err "Invalid filetype for copying keywords";
      return 1;
    }
  } else {
    # User-specified, comma-delimited list of keywords to copy
    @keys = split ",", $category; 
  }

  # Read the keywords from the source file
  ahgen::set_quiet(1);
  ahlog::ah_debug "Reading keywords in file $infile";
  $status = ahgen::run_ftool("ftlist",
                   "infile=$infile\[$inext\]",
                   "option=K",
                   "outfile=$keys_file",
                   "include=".join(",",@keys),
                   "colhead=no",
                   "rownum=no",
                   "clobber=yes");
  ahgen::set_quiet();
  if ( $status ) {
    unlink $keys_file;
    ahlog::ah_err "Error reading keywords from $infile to write to $outfile\n";
    return $status ;
  }

  # Write the keywords to the destination file
  ahlog::ah_out "Writing keywords for file $outfile";
  $status = ahgen::set_keyword($outfile, $outext, "\@$keys_file" );
  # Delete temporary file
  unlink $keys_file;
  if( $status ) {
    ahlog::ah_err "Error updating keywords from $keys_file to file $infile\n";
    return $status;
  }


  # Return all keywords and status
  return $status ;

} # copy_keywords

############################
#    CALDB Subroutines
############################

# 
# Description: 
#
# Check if CALDB environment variables are set
#
# Parameters:
# [out] status     Return 0 if CALDB, CALDBALIAS or CALDBCONFIG 
#                  environment variables are not set
#
sub check_caldb {

  # CALDB, if set, provides three environment variables.
  # It is here assumed that if the three environment variables
  # are present and valid, then CALDB is set properly
  
  # The variables are:
  # $CALDB        - Points to top directory of caldb installation
  # $CALDBALIAS   - Single FITS table of instrument name alias.
  # $CALDBCONFIG  - Single ".txt" file, which is configuration file.
  #                 Lists datasets on system by mission/instrument
  #                 and their location.

  my $status = 0;

  if ( exists( $ENV{CALDB} ) ) {
    ah_debug "Environment variable CALDB = $ENV{CALDB}\n";
  } else {
    ah_debug "Environment variable CALDB not set\n";
    $status = 1;
  }
  if ( exists( $ENV{CALDBALIAS} ) ) {
    ah_debug "Environment variable CALDBALIAS = $ENV{CALDBALIAS}\n";
  } else {
    ah_debug "Environment variable CALDBALIAS not set\n";
    $status = 1;
  }
  if ( exists( $ENV{CALDBCONFIG} ) ) {
    ah_debug "Environment variable CALDBCONFIG = $ENV{CALDBCONFIG}\n";
  } else {
    ah_debug "Environment variable CALDBCONFIG not set\n";
    $status = 1;
  }

  return $status;

}

# 
# Description: 
#
# The function performs a query to the CALDB database based
# on instrument, codename, date and time parameters 
# for the input parameter description.
# The function returns a single file as the result of the query.
#
# Parameters:
# [in] filename    Name of instrument 
# [in] inst        Name of instrument 
# [in] codename    Calibration dataset codename
# [in] dateobs     Requested Date in yyyy-mm-dd format (or now)
# [in] expression  Boolean selection for boundary params or "-" if not required
# [in] Mission     (optional) Set instrument or use Hitomi as default
# [out] outfile    Resolved file in format: path/to/file.fits[EXT#] 
#
sub call_quzcif ($$$$$$;$) {

  my $filename = shift;
  my $inst = shift;
  my $detector = shift;
  my $codename = shift;
  my $dateobs = shift;
  my $expression = shift;
  my $mission = shift; 

  if(! defined $mission) { $mission = "HITOMI"; }
  my $filter = "-";
  my $date = "-";
  my $time = "-";
  my $maxret = 1;
  my $retrieve = "no";

  my $outfile = "";

  my @outlist = ();
  my $status = 0;

  if (ahgen::isFileCALDB($filename)) {

    $status = check_caldb();
    if ($status) {
      ahlog::ah_err "CALDB not set up properly";
      ahgen::set_error_flag($status);
      return $outfile;
    }

    # Convert TSTART to calendar time or use latest time
    if(lc $dateobs ne "now") {
      ($date, $time) = $dateobs =~ /^(\d\d\d\d-\d\d-\d\d)T(\d\d:\d\d:\d\d)/;
      unless ($date) { $date = "now"; }
      unless ($time) { $time = "now"; }
    }

    # Run quzcif to get CALDB file 
    ahlog::ah_info "HIGH", "Querying CALDB using quzcif with the following parameters:";
    ahlog::ah_info "HIGH", "  TELESCOPE:    $mission";
    ahlog::ah_info "HIGH", "  INSTRUMENT:   $inst";
    ahlog::ah_info "HIGH", "  DETNAM:       $detector";
    ahlog::ah_info "HIGH", "  FILTER:       $filter";
    ahlog::ah_info "HIGH", "  CODENAME:     $codename";
    ahlog::ah_info "HIGH", "  DATE:         $date";
    ahlog::ah_info "HIGH", "  TIME:         $time";
    ahlog::ah_info "HIGH", "  EXPRESSION:   $expression";
    ahlog::ah_info "HIGH", "  MAXRET:       $maxret";
    ahlog::ah_info "HIGH", "  RETRIEVE:     $retrieve";
    $status = ahgen::run_ftool("quzcif","MISSION=$mission","INSTRUMENT=$inst",
                               "DETECTOR=$detector","FILTER=$filter",
                               "CODENAME=$codename","DATE=$date","TIME=$time",
                               "EXPR=$expression","MAXRET=$maxret",
                               "RETRIEVE=$retrieve");
    if ($status) { 
      ahlog::ah_err "Error retrieving CALDB file from quzcif";
      ahgen::set_error_flag($status);
      return $outfile;
    }

    # Verify that only one file was found
    my $nfound = qx/pget quzcif nfound/;
    if ($nfound != 1){   
      ahlog::ah_err "No $mission CALDB files found for $inst on ${date}T${time}";
      ahgen::set_error_flag(1);
      return $outfile;
    }   

    # Split quzcif stdout by line
    my @quzcif_out = split /\n/, ahgen::get_tool_stdout();

    # Parse files and extensions, look for errors
    foreach my $field (@quzcif_out) {

      if($field =~ /(ERROR|OFFLINE)/i) {
        # An error occurred during the calling of quzcif
        # Or file is OFFLINE and only backed up to magnetic tape
        ahlog::ah_err "CallQuzcif: Error occurred during calibration file getting:\n$field\n";
        ahgen::set_error_flag(1);
        return $outfile;
      }

      # Clean up whitespace and get filename and extension
      my ($file, $ext) = split (/\s+/, $field);
      my $cdbfile = "$file\[$ext]";

      # Verify this is a valid FITS file, if not, return current list of files.
      unless (ahgen::check_fits_file($cdbfile)) {
        ahlog::ah_err "File $cdbfile failed file verification (ftverify)";
        ahgen::set_error_flag(1);
        return $outfile;
      }

      # Save filename, then ext to output array
      $outfile = $cdbfile;
      last;

    }
    # Save the output file. There should only be one.
  } elsif (lc $filename eq "refdata") {
    ahlog::ah_err "REFDATA not set up yet.";
    ahgen::set_error_flag(1);
    return $outfile;
  } else {
    $outfile = $filename;
  }

  # Return a single filename and extensions of valid CALDB file
  return $outfile;

}

# 
# Description: 
#
# Calculate an expression to be used in either ftselect or maketime based on
# input label, subsystem and filetype
#
# Parameters:
# [in] labelfile   CALDB file to read label and expression information from
# [in] label       Simple label to read from high extension in labelfile
# [in] subsys      Instrument to lookup in CALDB and labelfile
# [in] from        Filtype to filter (mkf, ehk or events)
# [out] expr       Expression to be used for filtering
#
sub get_high_caldb_expr ($$$$) {

  my $labelfile = shift;
  my $label = shift;
  my $subsys = shift;
  my $from = shift;

  my @labels = ();
  my $expr = "";

  my $status = 0;

  # Each type of screening includes two categories to screen for GTI and EVENTS
  # GTI screening is made using either the '.mkf' or '.ehk' extension
  # EVENT screening is done using the column in the event file
  #
  # e.g. to get cleaned events for any instrument, expression is needed:
  # - Create GTI from MKF
  # - Create GTI from EHK

  # Get labels to read from CALDB file
  # Read row with matching label, detname and from columns to get basiclabels, 
  # instrument and common GTI
  # (Should only be one row)

  # +++ 2015-06-25 AS: Need to determine extension name
  my $filter = "LABEL==\"$label\"&&SUBSYS==\"$subsys\"";
  my $readcolumn = "";

  if ( $from =~ /[mkf|ehk|hk]/i ) {
    $readcolumn = "MKEHK"
  } elsif ( $from =~ /event/i ) {
    $readcolumn = "EVENT"
  }

  # Open label file with filters on label, subsystem and from columns.
  my @row = ahgen::read_column($labelfile,$filter,$readcolumn);
  if(ahgen::get_error_flag) {
    ahlog::ah_debug "ahgen::read_column";
    return "";
  }
  if($#row>0) {
    # Too many matches, need to narrow results
    ahlog::ah_err "Too many matches for $label in $labelfile.";
    ahgen::set_error_flag(1);
    return "";
  }

  # Parse output, split on commas
  @labels = split /,/, $row[0]; 

  # Check that we got at least one label
  unless( @labels ) {
    # No elements in label array
    ahlog::ah_err "Error getting labels";
    ahgen::set_error_flag(1);
    return "";
  }

  return @labels;

}

sub get_caldb_expr ($$$$) {

  my $labelfile = shift;
  my $subsys = shift;
  my $from = shift;
  my $inlabels = shift;

  my @labels = ();
  my $expr = "";

  my $status = 0;
  
  # Parse inlabels, split on commas
  @labels = split /,/, $inlabels; 

  # Check that we got at least one label
  unless( @labels ) {
    # No elements in label array
    ahlog::ah_err "Error getting labels";
    ahgen::set_error_flag(1);
    return $expr;
  }

  # Get expression from CALDB file
  # Read rows with matching label and detname rows to get expressions
  # (Can be multiple rows)
  foreach my $basiclabel (@labels) {

    ahlog::ah_out "Using LABEL: $basiclabel";

    my $filter = "LABEL==\"$basiclabel\"&&SUBSYS==\"$subsys\"&&FROMFILE==\"$from\"";

    my @expr = ahgen::read_column($labelfile,$filter,"EXPR");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "Could not read column EXPR in $labelfile";
      return $expr;
    }

    # Check that we got results, if not, move to next label
    unless (@expr) { next; }

    foreach my $row (@expr) {
      $expr .= "$row&&" ;
    }
  }

  # Clean up expressions, remove extraneous &&s
  $expr =~ s/^(\&\&|\|\|)//;
  $expr =~ s/(\&\&|\|\|)$//;

  if(! defined $expr) {
    ahlog::ah_err "CALDB expression not defined";
    ahgen::set_error_flag(1);
    return $expr;
  }

  if(length($expr)==0) {
    ahlog::ah_err "CALDB expression not defined";
    ahgen::set_error_flag(1);
    return $expr;
  }

  ahlog::ah_debug "LABELS:\n";
  ahlog::ah_debug "            " . $_  . "\n" foreach (@labels);
  ahlog::ah_debug "SUBSYS:     " . $subsys ."\n";
  ahlog::ah_debug "FROM:       " . $from ."\n";
  ahlog::ah_debug "EXPRESSION: " . $expr ."\n";

  return $expr;

}

# 
# Description: 
#
# Get list GTI files used to create merged GTI for ahgtigen
#
# Parameters:
# [in] labelfile   CALDB file to read label and expression information from
# [in] label       Simple label to read from high extension in labelfile
# [in] subsys      Instrument to lookup in CALDB and labelfile
# [in] from        Filtype to filter (mkf, ehk or events)
# [out] outgti     Array of GTI files used in screening
#
sub get_caldb_gti ($$$$) {

  my $labelfile = shift;
  my $label = shift;
  my $subsys = shift;
  my $from = shift;

  my @outgti = ();
  my $status = 0;

  # Get labels to read from CALDB file
  # Read row with matching label, detname and from columns to get basiclabels, 
  # instrument and common GTI
  # (Should only be one row)

  # +++ 2015-06-25 AS: Need to determine extension name
  my $filter = "LABEL==\"$label\"&&SUBSYS==\"$subsys\"";

  # Open label file with filters on label, subsystem and from columns.
  my @comm = ahgen::read_column($labelfile,$filter,"GTICOMM");
  if(ahgen::get_error_flag) {
    ahlog::ah_debug "ahgen::read_column";
    return @outgti;
  }
  if($#comm>0) {
    # Too many matches, need to narrow results
    ahlog::ah_err "Too many matches for $label in $labelfile.";
    ahgen::set_error_flag(1);
    return @outgti;
  }
  # Open label file with filters on label, subsystem and from columns.
  my @inst = ahgen::read_column($labelfile,$filter,"GTIINST");
  if(ahgen::get_error_flag) {
    ahlog::ah_debug "ahgen::read_column";
    return @outgti;
  }
  if($#inst>0) {
    # Too many matches, need to narrow results
    ahlog::ah_err "Too many matches for $label in $labelfile.";
    ahgen::set_error_flag(1);
    return @outgti;
  }

  # Parse output, split on commas
  foreach my $file ( split (/,/, $comm[0]), 
                     split (/,/, $inst[0]) ) {
    # Skip file if not needed
    if ($file =~ /none/i) { next; }

    # Save GTI
    push @outgti, $file;

  }

  return @outgti;

}

##############################
#     timing subroutines     #
##############################

# 
# Description: 
#
# Calculate various timing keywords for GTI extension including:
# DATE-OBS, DATE-END, TIME-OBS, TIME-END, TELAPSE, ONTIME, EXPOSURE,
#
# Parameters:
# [in] gtifile       input GTI file
# [in] gtiext        input GTI file extension
# [in] leapfile      CALDB leapsecond file
# [out] status       Return 0 for successful creation of GTI keys file
#
sub calc_timing_keys ($$$) {

  # Pass in GTI, extensions to read from.
  # Pass in file name to write list of keywords to
  # Subroutine returns a nonzero status if there was
  # an error while writing the keyword file
  my $gtifile = shift;
  my $extension = shift;
  my $leapfile = shift;

  my $tstart=-1;
  my $tstop=-1;
  my $dateobs = 0;
  my $timeobs = 0;
  my $dateend = 0;
  my $timeend = 0;
  my $ontime = 0;
  my $telapse = 0;
  my $exposure = 0;

  my $status = 0;

  # Merge the extension with the name of the FITS file.
  my $fn_ext = "$gtifile\[$extension\]" ;

  my $last_row = ahgen::get_keyword($gtifile,$extension,"NAXIS2");
  if( defined $last_row ) {
    if($last_row != 0 ) {

      ahgen::set_quiet(1);
      if(ahgen::run_ftool("ftlist",$fn_ext,"T","columns=START","rows=1","rownum=No","colheader=No")) { return 1; }
      $tstart = ahgen::get_tool_stdout() + 0;
      if(ahgen::run_ftool("ftlist",$fn_ext,"T","columns=STOP","rows=$last_row","rownum=No","colheader=No")) { return 1;}
      $tstop  = ahgen::get_tool_stdout() + 0;

      # Calculate DATE-OBS, TIME-OBS, DATE-END, TIME-END from TSTART and TSTOP
      my $start_date = convert_seconds_to_datetime($leapfile,$tstart,"2014-01-01T00:00:00","c1");
      my $stop_date = convert_seconds_to_datetime($leapfile,$tstop,"2014-01-01T00:00:00","c1");
      if ( ahgen::get_error_flag ) {
        ahlog::ah_err "Error converting date/time from ahtimeconv\n";
        return ahgen::get_error_flag;
      }

      # Calculate timing keywords ONTIME, EXPOSURE, etc.
      ($ontime) = get_gti_sum($gtifile, $extension);
      if(ahgen::get_error_flag) {
        ahlog::ah_err "Error calculating GTI sum from file $gtifile.\n";
        return ahgen::get_error_flag;
      }

      # Calculate TELAPSE from observation time
      $telapse = $tstop-$tstart;

      ahgen::set_keyword($gtifile,$extension,"TSTART",$tstart,"Start time");
      ahgen::set_keyword($gtifile,$extension,"TSTOP",$tstop,"Stop time");
      ahgen::set_keyword($gtifile,$extension,"DATE-OBS",$start_date,"Start date");
      ahgen::set_keyword($gtifile,$extension,"DATE-END",$stop_date,"Stop date");
      ahgen::set_keyword($gtifile,$extension,"ONTIME",$ontime,"On-source time");
      ahgen::set_keyword($gtifile,$extension,"EXPOSURE",$ontime,"Exposure time");
      ahgen::set_keyword($gtifile,$extension,"TELAPSE",$telapse,"Elapsed time");
      ahgen::set_quiet();

      # Check for errors
      if(ahgen::get_error_flag) {
        ahlog::ah_err "Error updating timing keywords to $gtifile.\n";
        return ahgen::get_error_flag;
      }

    }
  }

  return 0;

} # end calc_timing_keys

# 
# Description: 
#
# Calculate GTI sum in seconds using ftool gtisum. Outputs an array
# of GTI info [gtisum, tstart, tstop]
#
# Parameters:
# [in] infile     Input GTI file
# [in] extension  Input GTI file extension
# [out] gtisum    Output sum of GTI in seconds
# [out] tstart    Output TSTART of GTI
# [out] tstop     Output TSTOP of GTI
#
sub get_gti_sum ($$) {

  my $infile = shift;
  my $extension = shift;
  my $status = 0;

  my $gtisum = 0.;
  my $tstart = 0.;
  my $tstop = 0.;

  # Run the gtisum tool and print to stdout
  ahgen::set_quiet(1);
  $status = ahgen::run_ftool("gtisum","gtifile=$infile","outfile=STDOUT");
  ahgen::set_quiet();
  if($status) {
    ahlog::ah_err "Error running tool gtisum on file $infile.\n";
    ahgen::set_error_flag($status);
    return (-1,-1,-1);
  }

  # Copy std out from gtisum
  my @gti = split /\n/, $ahgen::tool_std_output;
  # skip first row
  @gti = @gti[1 .. $#gti];

  # Read standard output from gtisum and save gtisum, tstart, tstop
  foreach(@gti) {
    # Skip the first line of output
    if($_ =~ m/GTI/ and $_ !~ m/Ext\./) {
      my @gtiout = split ' ', $_;
      $gtisum = $gtiout[-1];
      $tstart = $gtiout[-3];
      $tstop = $gtiout[-2];
      last;
    }
  }
  
  return ($gtisum, $tstart, $tstop);

}

# 
# Description: 
#
# Convert time in MJD into UTC format (YYYY-MM-DDT00:00:00)
#
# Parameters:
# [in] intime     input time
# [in] utcepoch   epoch time
# [in] format     epoch format (c1, c2, j, m)
# [out] outtime   output time in UTC format
#
sub convert_seconds_to_datetime ($$$$) {

  my $leapfile = shift;
  my $intime = shift;
  my $utcepoch = shift;
  my $format = shift;

  my $status = 0;

  my $outtime = "";

  ahgen::set_quiet(1);
  # Compute and set DATE_OBS keyword using ahtimeconv
  $status = ahgen::run_ftool ("ahtimeconv",
                              "intime=$intime",         # input mission time as seconds since epoch
                              "insys=M",
                              "inform=s",
                              "outsys=U",            # output as UTC in calendar format
                              "outform=c1",
                              "epochtime=$utcepoch",
                              "epochsys=U",
                              "epochform=$format",
                              "leapsecfile=$leapfile");
  ahgen::set_quiet();
  # Remove the log file
  unlink "ahtimeconv.log";
  if ($status) {   
    ahlog::ah_err "Error encountered while running ahtimeconv . " .
                  "Returned status: " . ahgen::get_error_flag ;
    return undef;
  }   

  # Get output parameter from ahtime conv
  ahgen::run_ftool("pget", "ahtimeconv", "outtime");
  if (ahgen::get_error_flag) {   
    ahlog::ah_err "Error encountered while running ahtimeconv . " .
                  "Returned status: " . ahgen::get_error_flag ;
    return undef;
  }   
  $outtime = ahgen::get_tool_stdout;

  # Remove extraneous new line
  chomp $outtime;

  return $outtime;

}

1;

#
# Revision Log:
# $Log: ahfilterlib.pm,v $
# Revision 1.57  2016/04/18 20:55:16  asargent
# Check if for defined parameters rather than direct parameter when passing parameters to ftool
#
# Revision 1.56  2016/04/11 08:39:43  asargent
# merge_gti(): Only check_fits_file if there is more than 0 rows. remove ahtimeconv log file after running
#
# Revision 1.55  2016/04/06 02:51:51  asargent
# mergegti: check if there are output rows before removing TIMEZERO keyword. If not, do not remove TIMEZERO keyword since it is not there.
#
# Revision 1.54  2016/03/29 15:57:27  asargent
# Removed splitting of date-obs/date-end variables.
#
# Revision 1.53  2016/03/29 14:55:10  asargent
# Condense DATE-OBS & TIME-OBS and DATE-END & TIME-END keywords to single keywords
#
# Revision 1.52  2016/03/12 01:01:46  asargent
# Remove timezero keyword from mgtime file
#
# Revision 1.51  2016/03/12 00:41:14  asargent
# Added comments to timing keywords
#
# Revision 1.50  2016/03/12 00:15:24  asargent
# Removed HDUCLAS keywords from keyword list
#
# Revision 1.49  2016/03/10 02:32:05  asargent
# In calc_timing_keys, added check that NAXIS2 keyword was found.
#
# Revision 1.48  2016/03/08 17:35:09  asargent
# Deleted temporary file created when using a list of input files to mgtime
#
# Revision 1.47  2016/02/19 00:08:42  klrutkow
# call_quzcif: changed mission to HITOMI
#
# Revision 1.46  2016/01/05 22:03:47  asargent
# Updated calc_timing_keys reference date when calculating DATE-OBS
#
# Revision 1.45  2016/01/05 21:45:14  asargent
# Search for GTI extension if only one GTI file is being merged.
#
# Revision 1.44  2016/01/02 18:12:11  asargent
# Fixed bug when only a single GTI file was passed in for merging and all extensions were being copied to output file.
#
# Revision 1.43  2015/12/31 22:18:46  asargent
# Update the output extension name if there is only one GTI file to merge.
#
# Revision 1.42  2015/12/08 16:13:58  asargent
# Bug fix (merge_gti): mgtime normally sets the output file EXTNAME=STDGTI. If none of the input files have any GTI, the first input GTI header is copied including the EXTNAME which caused the editing of the output extension name to fail. Fixed by updating extension 1 rather than STDGTI.
#
# Revision 1.41  2015/10/26 18:57:26  asargent
# Fixed bug when parsing DATE-OBS during call_quzcif
#
# Revision 1.40  2015/10/20 16:23:07  asargent
# Cleaned up merge_gti, merge_events, and calc_timing_keys functions
#
# Revision 1.39  2015/08/19 20:52:06  asargent
# use runTool function for merge_gti
#
# Revision 1.38  2015/08/14 18:18:41  asargent
# Fixed bug where copy_keywords was deleting keyword file prematurely
#
# Revision 1.37  2015/08/13 18:29:18  asargent
# Added leapsecfile variable to convert_seconds_to_datetime
#
# Revision 1.36  2015/08/13 14:48:13  asargent
# Removed calling of convert_seconds_to_datetime in call_quzcif
#
# Revision 1.35  2015/08/12 19:49:55  asargent
# Updated call_quzcif to default to the input file if not using CALDB or REFDATA
#
# Revision 1.34  2015/08/11 17:46:55  asargent
# Fixed bug of status return when an error occurred writing keywords in copy_keywords
#
# Revision 1.33  2015/08/11 16:49:04  asargent
# Updated copy_keywords function to specify categories
#
# Revision 1.32  2015/08/05 20:16:54  rshill
# In call_quzcif: more log output; initializing  to null string.
#
# Revision 1.31  2015/08/05 18:16:44  asargent
# Fixed input argument number in get_gti_sum
#
# Revision 1.30  2015/08/05 18:03:13  asargent
# Updated caldb query functions, added parameter number requirements
#
# Revision 1.29  2015/07/08 19:33:46  asargent
# Updated print statements
#
# Revision 1.28  2015/06/25 19:21:23  asargent
# Updated CALDB labelfile accessor subroutines
#
# Revision 1.27  2015/06/24 18:42:06  asargent
# Updated calc_timing_keys function
#
# Revision 1.26  2015/06/24 17:36:55  asargent
# Bug fixes to merge_gti subroutine
#
# Revision 1.25  2015/06/24 17:17:11  asargent
# Perl cleanup of ahfilterlib
#
# Revision 1.24  2015/06/22 20:25:52  asargent
# Working version of cleaned up ahfilterlib.
#
#
