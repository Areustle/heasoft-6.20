#!/usr/bin/perl
#-------------------------------------------------------------------------------
#
# File name: ahcalctime.pl
#  
# Task name: ahcalctime
#       
# Description:
#
# Script to recalculate the time in event files.
# Intended to be used if the pipeline runs a sequence, but then an 
# error is uncovered with the time.
# Duplicates some initialization functionality of the ahpipeline script.
#
# 
# Author/Date: Kristin Rutkowski NASA GSFC / 20160119
#
# Tool Dependencies:
#   ftsort
#   ftcopy
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#
# History:
#
# Most of this was already written inside ahpipeline.pl, by A. Sargent
# Testing and reworking done with M. Witthoeft
#

#-----------------------------------------------------------

# Set up

use strict;

use ahlog ;
use ahgen ;
use ahapp ;
use ahfilterlib ;   # call_quzcif

use File::Find;
use File::Spec::Functions qw( splitpath catfile );  
use File::Basename;
use File::Path qw( make_path remove_tree);
use Cwd qw( cwd abs_path );
use IO::Uncompress::Gunzip qw(gunzip $GunzipError) ;
use IO::Compress::Gzip qw(gzip $GzipError) ;

use Astro::FITS::CFITSIO qw( :longnames :constants );

use Data::Dumper;           # +++ to output arrays for debugging

#########################
# Variable Definitions 
#########################

my $error = 0;

# Set flag to non-zero (preferably 1) to force ouput of debugging messages.
my $force_debug = '';
#$force_debug = 1;

my %Params = (
  indir             => "", # Input Directory
  outdir            => "", # Output Directory

  verify_input      => 0,  # Use ftverify
  sorttime          => 0,  # sort time column after calling ahtime
  
  # ahmktim specific params
  timext            => "", # tim file new extension name
  gaptime           => 0,  # gap time
  
  # ahtime specific params
  timecol           => "", # time column
  gticolumns        => "", # gti columns

  # shared parameters
  cleanup           => 0,  # Remove temp files?

);

# list of input files
# FindInputFiles also adds files of each type to this hash
my %files = (
  attitude              => "",
  housekeeping          => "",
  extended_housekeeping => "",
  makefilter            => "",
  orbit                 => "",
  timfile               => "",
  sxi_event_uf          => [],
  sxi_hk                => "",
  sxs_event_uf          => [],
  sxs_hk                => "",
  sxs_lost_gti          => "",
  hx1_event_uf          => [],
  hx2_event_uf          => [],
  hx1_hk                => "",
  hx2_hk                => "",
  sg1_event_uf          => [],
  sg2_event_uf          => [],
  sg1_hk                => "", 
  sg2_hk                => "" 
);

# patterns of input files we want to copy over to the output
our $zpatt = qr/(\.Z|\.z|\.gzip|\.GZIP|\.gz|\.GZ|\.zip\.ZIP)?/;
my %patterns = (
  attitude              => qr/(ah[0-9]{9}\.att)$zpatt$/,
  housekeeping          => qr/(ah[0-9]{9}gen.*\.hk)$zpatt$/,
  extended_housekeeping => qr/(ah[0-9]{9}\.ehk)$zpatt$/,
  makefilter            => qr/(ah[0-9]{9}\.mkf)$zpatt$/,
  orbit                 => qr/(ah[0-9]{9}\.orb)$zpatt$/,
  timfile               => qr/(ah[0-9]{9}\.tim)$zpatt$/,
  sxi_event_uf          => qr/(ah[0-9]{9}sxi.*_uf\.evt)$zpatt$/,
  sxi_hk                => qr/(ah[0-9]{9}sxi.*\.hk)$zpatt$/,
  sxs_event_uf          => qr/(ah[0-9]{9}sxs.*_uf\.evt)$zpatt$/,
  sxs_hk                => qr/(ah[0-9]{9}sxs.*\.hk1)$zpatt$/,
  sxs_lost_gti          => qr/(ah[0-9]{9}sxs_el\.gti)$zpatt$/,
  hx1_event_uf          => qr/(ah[0-9]{9}hx1.*_uf\.evt)$zpatt$/,
  hx2_event_uf          => qr/(ah[0-9]{9}hx2.*_uf\.evt)$zpatt$/,
  hx1_hk                => qr/(ah[0-9]{9}hx1.*\.hk)$zpatt$/,
  hx2_hk                => qr/(ah[0-9]{9}hx2.*\.hk)$zpatt$/,
  sg1_event_uf          => qr/(ah[0-9]{9}sg1.*_uf\.evt)$zpatt$/,
  sg2_event_uf          => qr/(ah[0-9]{9}sg2.*_uf\.evt)$zpatt$/,
  sg1_hk                => qr/(ah[0-9]{9}sg1.*\.hk)$zpatt$/,
  sg2_hk                => qr/(ah[0-9]{9}sg2.*\.hk)$zpatt$/
  );

my %archive_dirs = (
  attitude              => 'auxil',
  housekeeping1         => 'auxil',
  housekeeping2         => 'auxil',
  extended_housekeeping => 'auxil',
  makefilter            => 'auxil',
  orbit                 => 'auxil',
  timfile               => 'auxil',
  obsgti                => 'auxil',
  # Instrument directories
  hx1_event_uf          => 'hxi/event_uf',
  hx2_event_uf          => 'hxi/event_uf',
  hx1_hk                => 'hxi/hk',
  hx2_hk                => 'hxi/hk',
  sg1_event_uf          => 'sgd/event_uf',
  sg2_event_uf          => 'sgd/event_uf',
  sg1_hk                => 'sgd/hk',
  sg2_hk                => 'sgd/hk',
  sxi_event_uf          => 'sxi/event_uf',
  sxi_hk                => 'sxi/hk',
  sxs_event_uf          => 'sxs/event_uf',
  sxs_hk                => 'sxs/hk',
  sxs_lost_gt           => 'sxs/event_uf',
);

my %caldb_files = (
  'timing' => {
    leapsecfile      => "",  #
    coldeffile       => "",  #
    delayfile        => "",  #
    offsetfile       => "",  #
    frqtemfile       => ""
  },
  'sxs' => {
    coeftime         => ""   #
  }
);

my %instrument = (
  sxi                => "",  # SXI
  sxs                => "",  # SXS
  hx1                => "",  # HXI HXI1
  hx2                => "",  # HXI HXI2
  sg1                => "",  # SGD SGD1
  sg2                => ""   # SGD SGD2
);


my @filelist_output;
my $fptr1   = "";
my @parlist = ();

# Copy function for output intermediate files
my $copyfunc = 0;

# list of input files
my @filelist_parsed;
  

#########################
#  Main Code Block 
#########################

# Run Processing Subroutines
ahapp::startup ($force_debug) ;

# Pre-Processing
ahapp::begin_processing();

$error = GetInputParameters();
unless ( $error == 0 ) {
  ahlog::ah_err "GetInputParameters" ;
  ahapp::end_processing($error);
}

ahapp::print_input_parameters();


# get the TIMECOLDEF CALDB file, to be used in SortLookupTable, which is 
# necessary for running ahtime
my $coldeffile = ahfilterlib::call_quzcif("CALDB","GEN","-","TIMECOLDEF","-","-");
if( ahgen::get_error_flag ) {
  ahlog::ah_err "Error querying CALDB for TIMECOLDEF";
  return ahgen::get_error_flag;
}
unless ( defined $coldeffile ) { ahlog::ah_err "Error querying CALDB for TIMECOLDEF"; return 1; }
# remove the extension that quzcif returns (so that the ahgen functions
# can specify the extension as expected)
$coldeffile =~ s/\[.*\]//g;



# Define copy/move function to use based on whether or not to keep temp files
if ( ahapp::getcleanup ) {
  ah_debug "Cleanup = yes: using File::Copy::move";
  $copyfunc = \&File::Copy::move;
} else {
  ah_debug "Cleanup = no: using File::Copy::copy";
  $copyfunc = \&File::Copy::copy;
}

$error = CheckInputDirectory();
unless ( $error == 0 ) {
  ahlog::ah_err "CheckInputDirectory" ;
  ahapp::end_processing($error);
}

$error = CheckOutputDirectory();
unless ( $error == 0 ) {
  ahlog::ah_err "CheckOutputDirectory" ;
  ahapp::end_processing($error);
}

$error = CheckCalibrationFiles();
unless ( $error == 0 ) {
  ahlog::ah_err "CheckCalibrationFiles" ;
  ahapp::end_processing($error);
}

$error = FindInputFiles();
unless ( $error == 0 ) {
  ahlog::ah_err "FindInputFiles" ;
  ahapp::end_processing($error);
}

$error = CopyInputToOutput();
unless ( $error == 0 ) {
  ahlog::ah_err "CopyInputToOutput" ;
  ahapp::end_processing($error);
}

# Main Processing:

ahlog::ah_info "HIGH", "\n===========================================================\n";
ahlog::ah_info "HIGH", "                   Running Timing Tasks\n";
ahlog::ah_info "HIGH", "===========================================================\n\n";

# Calibrate timing on input files
$error = RunTiming();
unless ( $error == 0) {
  ahlog::ah_err "Error running RunTiming";
  ahapp::end_processing($error);
}

# Post-Processing
$error = FileHistory();
unless ( $error == 0 ) {
  ahlog::ah_err "FileHistory" ;
  ahapp::end_processing($error);
}

# Finish
ahapp::end_processing;



############################
# Pre-Processing Functions 
############################

sub GetInputParameters {
  
  #my @reqparm = qw(indir outdir steminputs entry_stage exit_stage instrument);

  # Get required parameters
  $Params{indir}                     = ahapp::query_parameter("indir");
  $Params{outdir}                    = ahapp::query_parameter("outdir");
  
  $Params{verify_input}              = ahapp::query_parameter("verify_input",1);
  $Params{sorttime}                  = ahapp::query_parameter("sorttime",1);
  
  $Params{timext}                    = ahapp::query_parameter("timext");
  $Params{gaptime}                   = ahapp::query_parameter("gaptime");
  
  $Params{timecol}                   = ahapp::query_parameter("timecol");
  $Params{gticolumns}                = ahapp::query_parameter("gticolumns");
  
  $Params{steminputs}                = "ah";

  # Timing CALDB files
  $caldb_files{timing}{frqtemfile}   = ahapp::query_parameter("frqtemfile");
  $caldb_files{timing}{coldeffile}   = ahapp::query_parameter("coldeffile");
  $caldb_files{timing}{delayfile}    = ahapp::query_parameter("delayfile");
  $caldb_files{timing}{leapsecfile}  = ahapp::query_parameter("leapsecfile");
  $caldb_files{timing}{offsetfile}   = ahapp::query_parameter("offsetfile");
  $caldb_files{sxs}{coeftime}        = ahapp::query_parameter("sxs_coeftime");

  if ($Params{outdir} !~ /\/$/) {
    $Params{outdir} = $Params{outdir} . "/";
  }
  
  ah_info "HIGH", ahapp::write_parameters() ; 
  
  return 0;

} # end sub GetInputParameters


sub CheckInputDirectory {

  if ( $Params{outdir} eq $Params{indir} ) {
    ahlog::ah_err "Output directory same as input directory";
    ahlog::ah_err "Input directory  : $Params{indir}";
    ahlog::ah_err "Output directory : $Params{outdir}";
    return 1;
  }

  if ( -e$Params{indir} )
  {
    ahlog::ah_info "LOW", "Input Directory Found : $Params{indir}";
    return 0;
  } else {
    ahlog::ah_err "Input Directory NOT FOUND : $Params{indir}";
    return 1;
  }

  return 0;
  
} # end sub CheckInputDirectory


sub CheckOutputDirectory {

  if ( -e$Params{outdir} ) {

    ahlog::ah_info "LOW", "Output Directory Found : $Params{outdir}" ;

    unless ($ahapp::clobber) {
      ahlog::ah_err "Output directory exists \& clobber=no : $Params{outdir}" ;
      return 1;
    }
    
    # if dir exists, and clobber=yes, just delete the dir, with a log message.
    ahlog::ah_info "HIGH", "Removing existing output directory : $Params{outdir}" ;
    remove_tree($Params{outdir}, {error => \my $err});
    if (@$err) {
      ahlog::ah_err "Problem removing $Params{outdir}" ;
      for my $diag (@$err) {
        my ($file, $message) = %$diag;
        if ($file eq '') {
            ahlog::ah_err "General error: $message";
        }
        else {
            ahlog::ah_err "Problem unlinking $file: $message";
        }
      }
      return 1;
    }
    
    # now make the directory fresh
    ahlog::ah_info "LOW", "Making New Output Directory : $Params{outdir}" ;

    eval { make_path( $Params{outdir} ); };
    if ( $@ ) {
      ahlog::ah_err "Cannot make directory : $Params{outdir}" ;
      return 1;
    }

  } else {

    ahlog::ah_info "LOW", "Output Directory Not Found  : $Params{outdir}" ;
    ahlog::ah_info "LOW", "Making New Output Directory : $Params{outdir}" ;

    eval { make_path( $Params{outdir} ); };
    if ( $@ ) {
      ahlog::ah_err "Cannot make directory : $Params{outdir}" ;
      return 1;
    }
  }

  if ( CheckInclusion($Params{indir}, $Params{outdir} )) {
    ahlog::ah_err "Output directory $Params{outdir}";
    ahlog::ah_err "The output directory cannot be linear to (above or below) the input directory";
    ahlog::ah_err "Please specify another directory for output";
    return 1;
  }

  return 0;

} # end sub CheckOutputDirectory


# Common section for CheckInclusion subroutine
# called by CheckOutputDirectory
{
  my ( $indir, $outdir );

  # Build a hash of the subdirectories of $outdir

  my %suboutdirs;
  my $found = 0;

  sub CheckInclusion {
    ( $indir, $outdir ) = @_;
    $indir  = abs_path( $indir );
    $outdir = abs_path( $outdir );

    # Traverse the $outdir filesystems, noting the subdirs

    File::Find::find( { wanted => \&buildsub, follow => 1, follow_skip => 2 }, $outdir );

    # Traverse the $indir filesystems

    File::Find::find( { wanted => \&lookout, follow => 1, follow_skip => 2 }, $indir );
    return $found;
  }

  sub buildsub {
    my ( $dev, $ino );
    unless ( defined $File::Find::fullname ) {
      return;
    }
    if ( -d $_ || -d $File::Find::fullname ) {
      ( $dev, $ino ) = stat _;
      $suboutdirs{$File::Find::fullname} = [ $_, $dev, $ino ];
    }
  }

  sub lookout {
    my ( $dev, $ino );
    unless ( defined $File::Find::fullname ) {
      return;
    }
    if ( -d $_ || -d $File::Find::fullname ) {
      if ( $suboutdirs{$File::Find::fullname} ) {
        ( ( $dev, $ino ) = stat _ );
        if ( $dev == $suboutdirs{$File::Find::fullname}->[ 1 ] &&
           $ino == $suboutdirs{$File::Find::fullname}->[ 2 ] ) {
          $File::Find::prune = 1;
          $found             = 1;
        }
      }
    }
  }

} # end sub CheckInclusion, common section


sub CheckCalibrationFiles {

  ah_debug "Starting CheckCalibrationFiles\n";

  my $caldbisset = 1;
  # CALDB, if set, provides three environment variables.
  # It is here assumed that if the three environment variables
  # are present and valid, then CALDB is set properly
  
  # The variables are:
  # $CALDB        - Points to top directory of caldb installation
  # $CALDBALIAS   - Single FITS table of instrument name alias.
  # $CALDBCONFIG  - Single ".txt" file, which is configuration file.
  #                 Lists datasets on system by mission/instrument
  #                 and their location.

  if ( exists( $ENV{CALDB} ) ) {
    ah_debug "Environment variable CALDB = $ENV{CALDB}\n";
  } else {
    ah_debug "Environment variable CALDB not set\n";
    $caldbisset = 0;
  }
  if ( exists( $ENV{CALDBALIAS} ) ) {
    ah_debug "Environment variable CALDBALIAS = $ENV{CALDBALIAS}\n";
  } else {
    ah_debug "Environment variable CALDBALIAS not set\n";
    $caldbisset = 0;
  }
  if ( exists( $ENV{CALDBCONFIG} ) ) {
    ah_debug "Environment variable CALDBCONFIG = $ENV{CALDBCONFIG}\n";
  } else {
    ah_debug "Environment variable CALDBCONFIG not set\n";
    $caldbisset = 0;
  }
  
  # Check if user-input CALDB files exist and are okay, 
  # so we don't waste time in later stages
#  my @instruments = [];
#  if( $instrument{sxi} == 1 ) { push @instruments, "sxi"; }
#  if( $instrument{hx1} == 1 or $instrument{hx2} == 1 ) { push @instruments, "hxi"; }
#  if( $instrument{sg1} == 1 or $instrument{sg2} == 1 ) { push @instruments, "sgd"; }
#  if( $instrument{sxs} == 1 ) { push @instruments, "sxs"; }
  
  my @instruments = [];
  push @instruments, "sxs";
  push @instruments, "timing";
  
  foreach my $det (@instruments) {
    # iterate through caldb files and verify if user-input
    foreach my $key (keys %{$caldb_files{$det}}) {
      ahlog::ah_debug "Searching for $det calibration file: $key";
      my $det_calfile = $caldb_files{$det}{$key};
      if(uc($det_calfile) !~ /(CALDB|NONE|REFDATA)/) {
        unless(-e $det_calfile) {
          ahlog::ah_err "User-input calibration File $det_calfile: " . $det_calfile . " does not exist.";
          ahlog::ah_err "Exiting.\n";
          return 1;
        }
        if($Params{verify_input}) {
          ahgen::check_fits_file($det_calfile);
          if ( ahgen::get_error_flag ) {
            ahlog::ah_err "User-input calibration File $det_calfile: " . $det_calfile . " failed file verification (ftverify)";
            return ahgen::get_error_flag;
          }
        }
      } elsif ($det_calfile =~ /(CALDB|REFDATA)/i) {
        unless ($caldbisset) {
          ahlog::ah_err "CALDB environment must be set in order to retrieve $det CALDB file: $key";
          ahlog::ah_err "Exiting.\n";
          return 1;
        }
      
      }
    }
  }

  return 0;
  
} # end sub CheckCalibrationFiles


sub FindInputFiles {

  # Find all files in the input directory 

  my @filelist_unparsed = GetFileList( $Params{indir} , $Params{steminputs});

  if ( $#filelist_unparsed == -1 ) {
    ahlog::ah_err  "No input files found with '$Params{steminputs}' stem" ;
    return 1;
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "File(s) to check:";
    foreach my $filename ( @filelist_unparsed ) {
      ahlog::ah_debug "  $filename\n";
    }
  }

  # Load names of relevant files from input data directory
  foreach my $filename ( sort @filelist_unparsed ) {
    foreach my $filetype ( keys %patterns ) {
    
#      ahlog::ah_debug "filename = $filename";
#      ahlog::ah_debug "patterns{filetype} = patterns{$filetype} = $patterns{$filetype}";

      if ( $filename =~ /$patterns{$filetype}/ ) {

#        ahlog::ah_debug "filename = $filename";

        # if this filetype is an array (like event_uf)
        if ( ( ref( $files{$filetype} ) ) eq "ARRAY" ) {
          # +++ KLR make sure this grep is doing what it should 
          if ( grep { basename($filename) eq basename($_) } @{$files{$filetype}} ) {
            ahlog::ah_out "WARNING: (array) Duplicate files found with name: " . basename($filename) ;
          } else {
            push @{$files{$filetype}}, $filename;
            push @filelist_parsed, $filename;
          }
        } else {
          # this filetype is a single file (like sxi hk)
          if ( $files{$filetype} ) {
            ahlog::ah_out "WARNING: (single) Duplicate files found with name: " . basename($filename) ;
          } else {
            $files{$filetype} = $filename;
            push @filelist_parsed, $filename;
          }
        }  # end-if this hash value is an array or not
      
      } # end-if this file matches the pattern
    } # end-loop through the pattern types
  } # end-loop through unparsed filelist

  # Verify input files were found
  if ( !@filelist_parsed ) {
    ahlog::ah_err "Failed to find any input files" ;
    return 1;
  }
  
  # now that we have the input files, check what instruments we have
  for my $det ("sxi", "hx1", "hx2", "sg1", "sg2", "sxs") {
    my @detmatches = grep { /$det/ } @filelist_parsed;
    if (@detmatches) {
      $instrument{$det} = 1;
    } else {
      $instrument{$det} = 0;
    }
  }  
  
  if ( ahgen::getdebug ) {
    print "filelist_parsed = \n " . join("\n ",@filelist_parsed) . "\n" ;
    print "instrument: \n" . Dumper( \%instrument );
    print "files: \n" . Dumper( \%files );
  }
  
  # Verify files as FITS files
  foreach my $filename ( sort @filelist_parsed ) {
    my $VerifyInputFile = VerifyInputFile( $filename );
    unless ( $VerifyInputFile == 0 ) {
      ahlog::ah_err "Error verifying input file $filename in VerifyInputFile" ;
      return $VerifyInputFile;
    }
  }

  return 0;
  
} # end sub FindInputFiles


sub VerifyInputFile {

  my $filename = shift;

  unless ( $Params{verify_input} ) {
    ahlog::ah_debug "VerifyInputFile Omitted for file $filename\n" ;
    return 0;
  }

  # The FITS files (various suffixes) are checked with FTVERIFY

  ahlog::ah_debug "\nRunning VerifyInputFile for input data files\n";
  
  ahgen::check_fits_file ( $filename );
  if ( ahgen::get_error_flag ) {
    ahlog::ah_err "HIGH", "ftverify failed for file $filename\n" ;
    return ahgen::get_error_flag ;
  } 

  ahlog::ah_debug "ftverify passed for file $filename\n" ;

  return 0;

}    # VerifyInputFile

# Common section for GetFileList Task
{
  my ( @list, $fulldirx, $stem );

  sub GetFileList {
    ( $fulldirx, $stem ) = @_;
    @list  = {};
    $#list = -1;
    &File::Find::find( { wanted => \&Wanted, follow => 1, follow_skip => 2 }, $fulldirx );
    return @list;
  }

  sub Wanted {
    /^.*$stem.*\z/s &&
    ! /^\..*/s &&  # ignore files starting with a dot        
    -f $_ &&
    push @list, $File::Find::name;
  }

} # end common section for GetFileList


sub CopyInputToOutput {

  ahlog::ah_debug "\nRunning CopyInputToOutput \n";

  my $indir = $Params{indir};
  my $outdir = $Params{outdir};

  my $status = 0;

  foreach my $filename ( @filelist_parsed ) {
  
    ahlog::ah_info "LOW", "Copying File $filename" ;
    
    # +++ KLR this should be verified.  this is probably wrong if user passed in full paths
    my $outfilename = cwd . "/" . $filename;
    $outfilename =~ s/$indir/$outdir/;

    # see if the directory exists.  If not, create it
    my ($volume,$outfiledir,$file) = File::Spec->splitpath( $outfilename );
    make_path($outfiledir);

    # copy the file
    ahgen::copy_fits_file( $filename, $outfilename );
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error copying $filename to $outfilename" ;
      return ahgen::get_error_flag ;
    }
    # make a copy of the original tim file, and remove the second ext
    if ( $outfilename =~ /(.*)(\.tim.*)/ ) {
      
      # rename the original tim file
      my $newtimfilename = $1 . "_orig" . $2 ;
      rename ($outfilename , $newtimfilename) ;
      
      $status = ahgen::run_ftool( "ftcopy", 
                                  "infile=$newtimfilename" , 
                                  "outfile=$outfilename", 
                                  "copyall=YES",
                                  "clobber=yes");
      if ($status) {
        ahlog::ah_err "Error running tool ftcopy on tim file";
        return $status;
      }
      $status = ahgen::run_ftool( "ftdelhdu", 
                                  "infile=$outfilename\[2\]", 
                                  "outfile=$outfilename", 
                                  "confirm=NO",
                                  "clobber=yes");
      if ($status) {
        ahlog::ah_err "Error running tool ftdelhdu on new tim file";
        return $status;
      }
    }
    
  }
  
  # now that files have been copied to output, update the files in $files
  # note the ^ so that only the beginning of the string, which should be the 
  # indir, is replaced
  for (values %files) { s/^$indir/$outdir/; }
  foreach my $key (keys(%files)) {
    if ( ( ref( $files{$key} ) ) eq "ARRAY" ) {
      s/^$indir/$outdir/ for @{$files{$key}};
    }
  }
  if ( ahgen::getdebug ) {
    print "files after $indir to $outdir replace: \n" . Dumper(\%files);
  }
  
  return 0;

} # end sub CopyInputToOutput




############################
#  Timing
############################


# General algorithm:
# 1.  run ahmktim with input ah<seqid>.tim and ah<seqid>.hk1
# 2.  run ahtime on general HK file, lookupfile=NONE
#     If desired, sort general HK file on timecol column
# 3.  SXS
#     a. run sxssamcnt on SXS HK file
#     b. run ahtime on SXS HK file, lookupfile=NONE
#        If desired, sort SXS HK file on timecol column
#     c. create temp lookup file, sorted on TI1HK column
#     d. run sxssamcnt on lost GTI file
#     e. run ahtime on lost GTI file, lookupfile=temp file
#        If desired, sort lost GTI file on timecol column
#     f. loop through event files
#         i.   run sxssamcnt
#         ii.  run ahtime, still lookupfile=temp file
#              If desired, sort event file on timecol column
# 4.  loop over (hx1, hx2, sxi, sg2, sg2)
#     a. create temp lookup file, sorted on TI1HK column
#     b. run ahtime on instrument HK file, lookupfile=temp file (except SXI =NONE)
#        If desired, sort instrument HK file on timecol column
#     c. loop through event files
#         i.   run ahtime, still lookupfile=temp file (or NONE for SXI)
#              If desired, sort event file on timecol column
# 


sub RunTiming {
  
  ### STEPS
  # 1. run ahmktim with input ah<seqid>.tim and ah<seqid>.hk1
  # 2. SXS: run sxssamcnt on all:
  #           - sxs/event_uf/_uf.evt
  #           - lost .gti
  #           - hk/.hk1
  # 3. run ahtime only on:
  #           - <seq>/<inst>/hk/*.hk
  #           - <seq>/<inst>/event_uf/*uf.evt
  #           - <seq>/sxs/event_uf/sxs*el.gti
  #           - <seq>/auxil/*hk


  ahlog::ah_debug "\nRunning RunTiming \n";

  my $hkfile = "";
  my @evtfiles = ();
  
  # temporary lookupfile for calling ahtime
  my $templookup;
  
  my $status = 0;
  
  if ($Params{sorttime}) {
    ahlog::ah_info "HIGH", "Sorting the ahtime output files adds considerably to the amount of time this tool takes.  It may take over an hour." ;
  } else {
    ahlog::ah_info "HIGH", "The ahtime output files will not be sorted on the '$Params{timecol}' column" ;
  }

  ############# setup parameters for each tool #############

  # +++ KLR why not pass in chatter, history params?

  # ahmktim parameters
  my %ahmktim = (
    'infile'         => $files{housekeeping},
    'frqtemfile'     => $caldb_files{timing}{frqtemfile},
    'timfile'        => $files{timfile},
    'outtimfile'     => "",
    'outgtifile'     => "",       # +++ GPS gti file, question for Lorella: when is this used?
    'leapsecfile'    => $caldb_files{timing}{leapsecfile},
    'hkgpsext'       => "",    # no param
    'hktempext'      => "",    # no param
    'timext'         => $Params{timext},
    'gaptime'        => $Params{gaptime},
    'stimecol'       => "S_TIME",    # no param
    'l32ticol'       => "L32TI",    # no param (note this, and mention to lorella)
    'tempcol'        => "",    # no param
    'packheadcol'    => "PACKET_HEADER",    # no param
    'gpsacol'        => "",     # no param
    'gpsbcol'        => "",    # no param
    'gpsccol'        => "",    # no param
    'gpsdcol'        => "",    # no param
    'clobber'        => "YES",
    'chatter'        => 0,
    'history'        => $ahapp::history ? "YES" : "NO"
  );

  ## sxssamcnt parameters
  my %sxssamcnt = (
    'infile'       => "",
    'outfile'      => "",
    'coeftime'     => $caldb_files{sxs}{coeftime},
    'col1'         => "DEFAULT",
    'col2'         => "DEFAULT",
    'col3'         => "DEFAULT",
    'col4'         => "DEFAULT",
    'col5'         => "DEFAULT",
    'timever1'     => "DEFAULT",
    'timever2'     => "DEFAULT",
    'outcol'       => "DEFAULT",
    'clobber'      => "YES"
  );

  ## ahtime parameters
  my %ahtime = (
    'infile'        => "",
    'outfile'       => "",
    'timfile'       => "",
    'lookupfile'    => "",    
    'leapsecfile'   => $caldb_files{timing}{leapsecfile},
    'coldeffile'    => $caldb_files{timing}{coldeffile},
    'delayfile'     => $caldb_files{timing}{delayfile},
    'offsetfile'    => $caldb_files{timing}{offsetfile},
    'timecol'       => $Params{timecol},
    'gticolumns'    => $Params{gticolumns},
    'calctime'      => "YES",
    'calcutc'       => "YES",
    'interp'        => "twopoint",
    'chatter'       => 0,
    'clobber'       => "YES"
  );
  
  # +++ KLR Before calling any of these tools, the script could check that 
  #         required infile/param is available

  ############# run ahmktim #############

  ahlog::ah_debug "Running ahmktim on $ahmktim{infile}";

  if (! -e $ahmktim{infile}) {
    ahlog::ah_err "Input general HK file for ahmktim not found; expecting ah<seq>gen*.hk[.gz] .";
    return 1;
  }
  if (! -e $ahmktim{timfile}) {
    ahlog::ah_err "Input TIM file for ahmktim not found; expecting ah<seq>.tim[.gz] .";
    return 1;
  }

  my $timfile_stem = $ahmktim{timfile};
  $timfile_stem =~ s/\.tim(.*)//;
  $ahmktim{outtimfile} = $timfile_stem . "ahmktim.tim" . $1;
  $ahmktim{outgtifile} = $timfile_stem . "ahmktim.gti" . $1;

  # ahmktim extension and column params depend on the SMUUNIT
  # +++ KLR maybe put this check before copying the dirs to outdir, so we can 
  #     catch an error before removing any existing outdir
  my $SMUUNIT = ahgen::get_keyword($ahmktim{infile}, 1, "SMUUNIT");
  ahlog::ah_debug "General HK file ($ahmktim{infile}) SMUUNIT = $SMUUNIT";
  if (uc $SMUUNIT eq "A") {
    $ahmktim{hkgpsext} = "HK_SMU_A_DHFS_SIB2GEN_dhfs_tlm_attseq";
    $ahmktim{hktempext} = "HK_SMU_A_AUX_HCE_HK2";
    $ahmktim{tempcol} = "HCE_A_SENS_SMU_A_TEMP_CAL";
    $ahmktim{gpsacol} = "SMU_A_DHFS_TI_MNG_TIM_CRNT_TIM";
    $ahmktim{gpsbcol} = "SMU_A_DHFS_TI_MNG_TIM_GPS_SYC_STAT";
    $ahmktim{gpsccol} = "SMU_A_DHFS_TI_MNG_TIM_AUT_SYC";
    $ahmktim{gpsdcol} = "SMU_A_DHFS_TI_MNG_TIM_GPS_STAT";
  } elsif (uc $SMUUNIT eq "B") {
    # SMUUNIT = B
    $ahmktim{hkgpsext} = "HK_SMU_B_DHFS_SIB2GEN_dhfs_tlm_attseq";
    $ahmktim{hktempext} = "HK_SMU_B_AUX_HCE_HK3";
    $ahmktim{tempcol} = "HCE_B_SENS_SMU_B_TEMP_CAL";
    $ahmktim{gpsacol} = "SMU_B_DHFS_TI_MNG_TIM_CRNT_TIM";
    $ahmktim{gpsbcol} = "SMU_B_DHFS_TI_MNG_TIM_GPS_SYC_STAT";
    $ahmktim{gpsccol} = "SMU_B_DHFS_TI_MNG_TIM_AUT_SYC";
    $ahmktim{gpsdcol} = "SMU_B_DHFS_TI_MNG_TIM_GPS_STAT";
  } else {
    ahlog::ah_err "SMUUNIT in general HK file ($ahmktim{infile}) must equal A or B";
    return 1 ;
  }

  if ( ahgen::getdebug ) {
    print "ahmktim parameters: " . Dumper(\%ahmktim);
  }

  $status = ahgen::run_ftool( "ahmktim", map ( "$_=$ahmktim{$_}" , keys %ahmktim ) );
  if($status) {
    ahlog::ah_err "Error running tool ahmktim";
    return $status;
  }
  
  # Copy ahmktim output to output directory
  ahlog::ah_debug "Copying/Moving $ahmktim{outtimfile} to $files{timfile}";
  unless( $copyfunc->( $ahmktim{outtimfile}, $files{timfile}) ) {
    ahgen::set_error_flag(1);
    ahlog::ah_err "Failed to move $ahmktim{outtimfile} to $files{timfile}";
    return ahgen::get_error_flag;
  }

  # Update ahtime timfile
  $ahtime{timfile} = $files{timfile};


  ############# run ahtime on gen HK file  #############
  
  $ahtime{infile} = $files{housekeeping};
  my $genhkfile_stem = $ahtime{infile};
  $genhkfile_stem =~ s/\.hk(.*)//;
  $ahtime{outfile} = $genhkfile_stem . ".ahtime.hk" . $1;
  $ahtime{lookupfile} = "NONE";
  
  if ( ahgen::getdebug ) {
    print "ahtime parameters: " . Dumper(\%ahtime);
  }

  ahlog::ah_debug "Running ahtime on $ahtime{infile}";

  $status = ahgen::run_ftool( "ahtime", map ( "$_=$ahtime{$_}" , keys %ahtime ) );
  if($status) {
    ahlog::ah_err "Error running tool ahtime";
    return $status;
  }
      
  # sort the file based on new time column
  if ($Params{sorttime}) {
    ahlog::ah_debug "Sorting $ahtime{outfile} on $ahtime{timecol}";
    $status = ahgen::sort_fits_file_all_hdus($ahtime{outfile},$ahtime{outfile},$ahtime{timecol});
    if($status) {
      ahlog::ah_err "Error running tool ftsort";
      return $status;
    }
  } # end-if sorting

  # Copy ahtime output to output directory
  ahlog::ah_debug "Copying/Moving $ahtime{outfile} to $ahtime{infile}";
  unless( $copyfunc->( $ahtime{outfile}, $ahtime{infile}) ) {
    ahgen::set_error_flag(1);
    ahlog::ah_err "Failed to move $ahtime{outfile} to $ahtime{infile}";
    return ahgen::get_error_flag;
  }

  ############# Handle SXS #############

  if( $instrument{sxs} == 1 ) {

    ############# setup input/output HK files #############

    my $hkfile = $files{sxs_hk};

    my $hkfile_stem = $hkfile;
    $hkfile_stem =~ s/\.hk(.*)//;

    $sxssamcnt{infile} = $hkfile;
    $sxssamcnt{outfile} = $hkfile_stem . ".sxssamcnt.hk" . $1;
    
    $ahtime{infile} = $hkfile;
    $ahtime{outfile} = $hkfile_stem . ".ahtime.hk" . $1;

    ############# other input/output files #############
    
    # lookupfile is as follows:
    # SXS hk: NONE
    
    $ahtime{lookupfile} = "NONE";

    ############# run sxssamcnt (HK file) #############

    ahlog::ah_debug "Running sxsamcnt on $sxssamcnt{infile}";

    $status = ahgen::run_ftool( "sxssamcnt", map ( "$_=$sxssamcnt{$_}" , keys %sxssamcnt ) );
    if($status) {
      ahlog::ah_err "Error running tool sxssamcnt";
      return $status;
    }

    # Copy sxssamcnt output to output directory
    ahlog::ah_debug "Copying/Moving $sxssamcnt{outfile} to $hkfile";
    unless( $copyfunc->( $sxssamcnt{outfile}, $hkfile) ) {
      ahgen::set_error_flag(1);
      ahlog::ah_err "Failed to move $sxssamcnt{outfile} to $hkfile";
      return ahgen::get_error_flag;
    }

    ############# run ahtime (HK file)  #############

    ahlog::ah_debug "Running ahtime on $ahtime{infile}";

    $status = ahgen::run_ftool( "ahtime", map ( "$_=$ahtime{$_}" , keys %ahtime ) );
    if($status) {
      ahlog::ah_err "Error running tool ahtime";
      return $status;
    }

    # sort the file based on new time column
    if ($Params{sorttime}) {
      ahlog::ah_debug "Sorting $ahtime{outfile} on $ahtime{timecol}";
      $status = ahgen::sort_fits_file_all_hdus($ahtime{outfile},$ahtime{outfile},$ahtime{timecol});
      if($status) {
        ahlog::ah_err "Error running tool ftsort";
        return $status;
      }
      
    } # end-if sorting

    # Copy ahtime output to output directory
    ahlog::ah_debug "Copying/Moving $ahtime{outfile} to $hkfile";
    unless( $copyfunc->( $ahtime{outfile}, $hkfile) ) {
      ahgen::set_error_flag(1);
      ahlog::ah_err "Failed to move $ahtime{outfile} to $hkfile";
      return ahgen::get_error_flag;
    }
    
    ############# get ready for lost GTI file #############

    my $lostgtifile_stem = $files{sxs_lost_gti};
    $lostgtifile_stem =~ s/\.gti(.*)//;
    
    $sxssamcnt{infile} = $files{sxs_lost_gti};
    $sxssamcnt{outfile} = $lostgtifile_stem . ".sxssamcnt.gti" . $1;

    $ahtime{infile} = $files{sxs_lost_gti};
    $ahtime{outfile} = $lostgtifile_stem . ".ahtime.gti" . $1;
    
    # prepare the file that will be the lookup file
    # +++ finish comments
    
    # 1) extract the lookup table extensions into a temporary file
    # 2) sort the temporary file on the lookup table's U32TI
    #    column (TI1HK in the column definitions CALDB file)
    # 3) run ahtime using the temporary file
    # 4) throw away the temporary file 
    $templookup = $files{sxs_hk};
    $templookup = $templookup . ".tmp";
    SortLookupTable("sxs", $templookup, $files{sxs_hk});
    $ahtime{lookupfile} = $templookup;
      
    if ( ahgen::getdebug ) {
      ah_debug "params for SXS lost GTI file" ;
      print "sxssamcnt parameters: " . Dumper(\%sxssamcnt);
      print "ahtime parameters: " . Dumper(\%ahtime);
    }

    ############# run sxssamcnt (lost GTI file) #############
    
    ahlog::ah_debug "Running sxsamcnt on $sxssamcnt{infile}";

    $status = ahgen::run_ftool( "sxssamcnt", map ( "$_=$sxssamcnt{$_}" , keys %sxssamcnt ) );
    if($status) {
      ahlog::ah_err "Error running tool sxssamcnt";
      return $status;
    }

    # Copy sxssamcnt output to output directory
    ahlog::ah_debug "Copying/Moving $sxssamcnt{outfile} to $files{sxs_lost_gti}";
    unless( $copyfunc->( $sxssamcnt{outfile}, $files{sxs_lost_gti}) ) {
      ahgen::set_error_flag(1);
      ahlog::ah_err "Failed to move $sxssamcnt{outfile} to $files{sxs_lost_gti}";
      return ahgen::get_error_flag;
    }

    ############# run ahtime (lost GTI file)  #############
    
    ahlog::ah_debug "Running ahtime on $ahtime{infile}";
    
    # for the lost gti file, check that of the first two extensions
    # (both called GTILOST, one is for pixel and one is for antico),
    # at least one has a row, or else ahtime will fail
    my $pxrows = ahgen::get_keyword($ahtime{infile}, "1", "NAXIS2");
    my $acrows = ahgen::get_keyword($ahtime{infile}, "2", "NAXIS2");
    ahlog::ah_debug "pxrows = $pxrows ; acrows = $acrows";
    
    if ($pxrows + $acrows == 0) {
      ahlog::ah_debug "Lost GTI file $ahtime{infile} has zero rows; not running ahtime";
    } else {
    
      $status = ahgen::run_ftool( "ahtime", map ( "$_=$ahtime{$_}" , keys %ahtime ) );
      if($status) {
        ahlog::ah_err "Error running tool ahtime";
        return $status;
      }
      
      # sort the file based on new time column
      if ($Params{sorttime}) {
        ahlog::ah_debug "Sorting $ahtime{outfile} on $ahtime{timecol}";
        $status = ahgen::sort_fits_file_all_hdus($ahtime{outfile},$ahtime{outfile},$ahtime{timecol});
        if($status) {
          ahlog::ah_err "Error running tool ftsort";
          return $status;
        }
      } # end-if sorting

      # Copy ahtime output to output directory
      ahlog::ah_debug "Copying/Moving $ahtime{outfile} to $files{sxs_lost_gti}";
      unless( $copyfunc->( $ahtime{outfile}, $files{sxs_lost_gti}) ) {
        ahgen::set_error_flag(1);
        ahlog::ah_err "Failed to move $ahtime{outfile} to $files{sxs_lost_gti}";
        return ahgen::get_error_flag;
      }
    }
        

    ############# loop over event files #############

    push ( my @sxsfiles , @{$files{sxs_event_uf}} );
    foreach my $sxsfile ( @sxsfiles ) {
      
      # If this event file has 0 rows, skip to next one, or else ahtime fails
      if (ahgen::get_keyword($sxsfile, "EVENTS", "NAXIS2") == 0) {
        ahlog::ah_debug "Event file $sxsfile has zero rows; skipping";
        next;
      }
      
      my $sxsfile_stem = $sxsfile;
      $sxsfile_stem =~ s/\.evt(.*)//;

      $sxssamcnt{infile} = $sxsfile;
      $sxssamcnt{outfile} = $sxsfile_stem . ".sxssamcnt.evt" . $1;
      
      $ahtime{infile} = $sxsfile;
      $ahtime{outfile} = $sxsfile_stem . ".ahtime.evt" . $1;

      ############# other input/output files #############
      
      # lookupfile is as follows:
      # SXS Event: still the temp lookup file
      
      $sxssamcnt{col5} = "DEFAULT";
      
      if ( ahgen::getdebug ) {
        ah_debug "params for SXS event file" ;
        print "sxssamcnt parameters:" . Dumper(\%sxssamcnt);
        print "ahtime parameters: " . Dumper(\%ahtime);
      }

      ############# run sxssamcnt #############

      ahlog::ah_debug "Running sxssamcnt on $sxssamcnt{infile}";

      $status = ahgen::run_ftool( "sxssamcnt", map ( "$_=$sxssamcnt{$_}" , keys %sxssamcnt ) );
      if($status) {
        ahlog::ah_err "Error running tool sxssamcnt";
        return $status;
      }

      # Copy sxssamcnt output to output directory
      ahlog::ah_debug "Copying/Moving $sxssamcnt{outfile} to $sxsfile";
      unless( $copyfunc->( $sxssamcnt{outfile}, $sxsfile) ) {
        ahgen::set_error_flag(1);
        ahlog::ah_err "Failed to move $sxssamcnt{outfile} to $sxsfile";
        return ahgen::get_error_flag;
      }

      ############# run ahtime #############

      ahlog::ah_debug "Running ahtime on $ahtime{infile}";

      $status = ahgen::run_ftool( "ahtime", map ( "$_=$ahtime{$_}" , keys %ahtime ) );
      if($status) {
        ahlog::ah_err "Error running tool ahtime";
        return $status;
      }
      
      # sort the file based on new time column
      if ($Params{sorttime}) {
        ahlog::ah_debug "Sorting $ahtime{outfile} on $ahtime{timecol}";
        $status = ahgen::sort_fits_file_all_hdus($ahtime{outfile},$ahtime{outfile},$ahtime{timecol});
        if($status) {
          ahlog::ah_err "Error running tool ftsort";
          return $status;
        }
      } # end-if sorting

      # Copy ahtime output to output directory
      ahlog::ah_debug "Copying/Moving $ahtime{outfile} to $sxsfile";
      unless( $copyfunc->( $ahtime{outfile}, $sxsfile ) ) {
        ahgen::set_error_flag(1);
        ahlog::ah_err "Failed to move $ahtime{outfile} to $sxsfile";
        return ahgen::get_error_flag;
      }
      
    }

    # Delete temporary lookup file
    if ( ahapp::getcleanup ) {
      if ( -e $templookup) {
        unlink($templookup);
      }
    }

    # Output results
    if( ahapp::getdebug ) {
      ahlog::ah_debug "\nUpdated timing for files:";
      ahlog::ah_debug "HK File       : $files{sxs_hk}";
      ahlog::ah_debug "Lost GTI File : $files{sxs_lost_gti}";
      foreach(@{$files{sxs_event_uf}}) {  
        ahlog::ah_debug "Event File    : $_";
      }
    }

  } # end if sxs


  ############# Loop through instruments #############
  # Loop through SXI, HXI1, HXI2, SGD1, SGD2
  # SXS is separate

  for my $det ("sxi", "hx1", "hx2", "sg1", "sg2") {

    # Clear out event file array
    @evtfiles = ();

    if( $instrument{$det} == 1 ) {
    
      ahlog::ah_debug "Processing $det files";

      ############# setup input/output HK files #############

      my $hkfile = $files{$det."_hk"};
      ahlog::ah_debug "instrument hkfile = $hkfile ";

      my $hkfile_stem = $hkfile;
      $hkfile_stem =~ s/\.hk(.*)//;
      $ahtime{infile} = $hkfile;
      $ahtime{outfile} = $hkfile_stem . ".ahtime.hk" . $1;

      ############# other input/output files #############
      
      # lookupfile is as follows (HK files):
      # HXI hk: HXI HK file (self referential)
      # SGD hk: SGD HK file (self referential)
      # SXI hk: NONE
      
      if ( $det eq "sxi" ) {
        $ahtime{lookupfile} = "NONE";
      } else {
      
        # create a temp lookup file, with the approp extensions
        # sorted based on the coldef file columns
        $templookup = $files{$det."_hk"};
        $templookup = $templookup . ".tmp";
        SortLookupTable($det, $templookup, $files{$det."_hk"});
        $ahtime{lookupfile} = $templookup;

      }

      ############# run ahtime (HK files)  #############

      if ( ahgen::getdebug ) {
        print "ahtime parameters: " . Dumper(\%ahtime);
      }
      
      ahlog::ah_debug "Running ahtime on $ahtime{infile}";

      $status = ahgen::run_ftool( "ahtime", map ( "$_=$ahtime{$_}" , keys %ahtime ) );
      if($status) {
        ahlog::ah_err "Error running tool ahtime";
        return $status;
      }
      
      # sort the file based on new time column
      if ($Params{sorttime}) {
        ahlog::ah_debug "Sorting $ahtime{outfile} on $ahtime{timecol}";
        $status = ahgen::sort_fits_file_all_hdus($ahtime{outfile},$ahtime{outfile},$ahtime{timecol});
        if($status) {
          ahlog::ah_err "Error running tool ftsort";
          return $status;
        }
      } # end-if sorting

      # Copy ahtime output to output directory
      ahlog::ah_debug "Copying/Moving $ahtime{outfile} to $files{$det.'_hk'}";
      unless( $copyfunc->( $ahtime{outfile}, $files{$det."_hk"}) ) {
        ahgen::set_error_flag(1);
        ahlog::ah_err "Failed to move $ahtime{outfile} to $files{$det.'_hk'}";
        return ahgen::get_error_flag;
      }
      
      # end time hk file

      ############# loop over event files #############

      ahlog::ah_debug "Running ahtime on event files";
      
      push ( my @evtfiles , @{$files{$det."_event_uf"}} );

      if ( ahgen::getdebug ) {
        print "evtfiles: " . Dumper(\@evtfiles);
      }
      
      foreach my $evtfile ( @evtfiles ) {
      
        # If this event file has 0 rows, skip to next one, or else ahtime fails
        if (ahgen::get_keyword($evtfile, "EVENTS", "NAXIS2") == 0) {
          ahlog::ah_debug "Event file $evtfile has zero rows; skipping";
          next;
        }

        my $evtfile_stem = $evtfile;
        $evtfile_stem =~ s/\.evt(.*)//;

        $ahtime{infile} = $evtfile;
        $ahtime{outfile} = $evtfile_stem . ".ahtime.evt" . $1;

        ############# other input/output files #############
        
        # lookup files used are the same as they were for the HK files:
        # HXI event: HXI HK file 
        # SGD event: SGD HK file
        # SXI event: NONE
        
        ############# run ahtime #############

        ahlog::ah_debug "Running ahtime on $ahtime{infile}";

        $status = ahgen::run_ftool( "ahtime", map ( "$_=$ahtime{$_}" , keys %ahtime ) );
        if($status) {
          ahlog::ah_err "Error running tool ahtime";
          return $status;
        }

        # sort the file based on new time column
        if ($Params{sorttime}) {
          ahlog::ah_debug "Sorting $ahtime{outfile} on $ahtime{timecol}";
          $status = ahgen::sort_fits_file_all_hdus($ahtime{outfile},$ahtime{outfile},$ahtime{timecol});
          if($status) {
            ahlog::ah_err "Error running tool ftsort";
            return $status;
          }
        } # end-if sorting

        # Copy ahtime output to output directory
        ahlog::ah_debug "Copying/Moving $ahtime{outfile} to $evtfile";
        unless( $copyfunc->( $ahtime{outfile}, $evtfile) ) {
          ahgen::set_error_flag(1);
          ahlog::ah_err "Failed to move $ahtime{outfile} to $evtfile";
          return ahgen::get_error_flag;
        }


      } # end file loop

      # Delete temporary lookup file
      if ( ahapp::getcleanup ) {
        if ( -e $templookup) {
          unlink($templookup);
        }
      }

      # Output results
      if( ahapp::getdebug ) {
        ahlog::ah_debug "\nUpdated timing for files: \n";
        ahlog::ah_debug "HK File    : ". $files{$det."_hk"}. "\n";
        foreach(@{$files{$det."_event_uf"}}) {  
          ahlog::ah_debug "Event File : $_";
        }
      }
        
    } # end if instrument
  } # end loop over instrument


  ############# stage 1 timing end #############

  ahlog::ah_debug "\nEnd RunTiming\n";

  return ahgen::get_error_flag;

} # end sub RunTiming


sub SortLookupTable {
  
  # KLR comment....
  
  my $inst = shift;
  my $infile_temp = shift;
  my $infile_orig = shift;
  my $outfile = $infile_temp;
  my $status = 0;
  
  # SXI doesn't use a lookup table
  if ($inst =~ /sxi/i) { return 0; }
  
  # the $inst passed in won't exactly match the strings in the coldef file yet
  $inst =~ s/hx/hxi/;
  $inst =~ s/sg/sgd/;
  
  # get the 1 or 2 to decide which instrument this is
  my $inst_base = $inst;
  my $det = 0 ;
  if ($inst =~ m/([\w]{3})([0-9])/ ) { $inst_base = $1 ; $det = $2 };
  
  # if the infile already exists, we can return
  if (-e $infile_temp) {
    ahlog::ah_debug "$infile_temp already exists";
    return 0;
  }
  
  # copy primary, so we can use ftappend later to add all the necessary HDUs
  ahlog::ah_debug "Creating temp lookup file $infile_temp";
  $status = ahgen::run_ftool ("ftcopy", 
                              "infile='$infile_orig\[0\]'" , 
                              "outfile=$infile_temp", 
                              "copyall=NO",
                              "clobber=yes");
  if ($status) {
    ahlog::ah_err "Error running tool ftcopy";
    return $status;
  }
  
  # The TIMECOLDEF CALDB file, $coldeffile, was retrieved from CALDB at the 
  # top of this script
  
  my $numrows   = ahgen::get_keyword($coldeffile, "TIMECOLDEF", 'NAXIS2' );
  my @system    = ahgen::read_column($coldeffile, "TIMECOLDEF", "SYSTEM");
  my @hkextname = ahgen::read_column($coldeffile, "TIMECOLDEF", "HKEXTNAME");
  my @ti1hk     = ahgen::read_column($coldeffile, "TIMECOLDEF", "TI1HK");
  
  # hash that will hold the columns and extensions for each instrument
  my %column_in;
  
  for (my $isys = 0 ; $isys < $numrows ; ++$isys) {
    if ( ($system[$isys] =~ m/$inst_base/i) && ($hkextname[$isys] !~ /na/i) ) {
      # replace the pound signs with the appropriate number
      my $extension = $hkextname[$isys];
      $extension =~ s/#/$det/g ;
      my $column = $ti1hk[$isys];
      $column =~ s/#/$det/g ;
      $column_in{$extension } = $column;
    }
  }
  
  if ( ahgen::getdebug ) {
    print "column_in: \n " . Dumper(\%column_in);
  }
  
  # copy each extension into the temporary file
  for my $ext (keys %column_in) {
    ahlog::ah_debug "Copying extension $ext into temporary lookup file";
    $status = ahgen::run_ftool ("ftappend", 
                                "infile='$infile_orig\[$ext\]'" , 
                                "outfile=$infile_temp");
    if ($status) {
      ahlog::ah_err "Error running tool ftappend";
      return $status;
    }
  }
  
  # sort each extension on the relevant TI1HK column
  for my $ext (keys %column_in) {
    ahlog::ah_debug "Sorting $infile_temp\[$ext\] on '$column_in{$ext}'";
    $status = ahgen::sort_fits_file($infile_temp, $ext, $outfile, "$column_in{$ext}");
    if($status) {
      ahlog::ah_err "Error running tool ftsort on $infile_temp\[$ext\]";
      return $status;
    }
  }
  
} # end sub SortLookupTable



############################
# Post-Processing Functions 
############################

sub FileHistory {

  if($ahapp::history == 0) {
    return 0;
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning FileHistory\n" ;
  }

  # Ensure each file name is processed only once

  @filelist_output = sort( @filelist_output );
  my $prev = 'nonesuch';
  my @filelist_output_unique = grep( $_ ne $prev && ( ( $prev ) = $_ ), @filelist_output );

  my $status = 0;
  my $fptr = "";

  # Check each file

  foreach my $infile ( @filelist_output_unique ) {

    ahlog::ah_debug "\nStamping ahpipeline parameters on file $infile\n" ;

    my $hdutotal = 0;

    # Open file and count number of HDU's (Headers)

    fits_open_file( $fptr1, $infile, READWRITE, $status );
    if ( $status ) {
      ahlog::ah_err "Unable to open FITS file : $infile File Status = $status" ;
      return $status;
    }
    fits_get_num_hdus( $fptr1, $hdutotal, $status );
    if ( $status ) {
        ahlog::ah_err "Unable find number of HDU's in FITS file $infile File Status = $status" ;
        return $status;
    }

    # Loop on HDUs
    for ( my $i1 = 1 ; $i1 <= $hdutotal ; $i1++ ) {

      fits_movabs_hdu( $fptr1, $i1, ANY_HDU, $status );
      unless ( $status == 0 ) {
        ahlog::ah_err "Unable to move to HDU #$i1 in FITS file : $infile File Status = $status" ;
        return $status;
      }

      # Only add file history if relevant input parameter is set
      if ( $ahapp::history ) {
        # Writing history using "write_parameters" is too slow because of its need
        # to open/close the file each time it writes a keyword. Use fits_write_history
        # instead.
        @parlist = ahapp::write_parameters();
        foreach (@parlist) {
          fits_write_history($fptr1, $_ , $status);
          if( $status ) { last; }
        }
        if ( $status ) {
          ahlog::ah_err "Unable to write history to output FITS file : $infile header number = $i1 File Status = $status" ;
          return $status;
        } else {
          ahlog::ah_info "HIGH", "Updated history to FITS file $infile, header number $i1" ;
        }
      }

      # Update the CHECKSUM and DATASUM keywords
      fits_write_chksum( $fptr1, $status );
      unless ( $status == 0 ) {
        ahlog::ah_err "FITS checksum not written to file : $infile header number = $i1 File Status = $status" ;
        return $status;
      }
    }

    # Close file
    fits_close_file( $fptr1, $status );
    if ( $status ) {
      ahlog::ah_err "Unable to close FITS file : $infile File Status = $status" ;
      return $status;
    }

  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of FileHistory \n" ;
  }

  return 0;

} # end sub FileHistory




# Revision Log:
# $Log: ahcalctime.pl,v $
# Revision 1.14  2016/08/12 17:43:58  mwitthoe
# ahcalctime: 1) fix general HK file name template needed by ahmktim; 2) check that ahmktim input files exist before running tool; 3) remove temporary instrument lookup tables after time assignment
#
# Revision 1.13  2016/03/22 21:26:59  klrutkow
# per issue 610: write parameters to log
#
#
# Revision 1.12  2016/03/22 18:52:56  klrutkow
# added CVS log to bottom of file
# 
#








