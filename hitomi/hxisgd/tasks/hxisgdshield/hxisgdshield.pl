#! /usr/bin/perl
#-------------------------------------------------------------------------------
# File name: hxisgdshield.pl
#
# Task name: hxisgdshield.pl
#
# Description: compute light curves, scans, or spectra from HXI/SGD shield data.
#
# Author/Date: James Peachey NASA GSFC
#
# History: for full history, see CVS change log at bottom of file.
#   Initially, code was developed from trf_hxisgdshield_15-05-06.docx.
#   Then algorithm was made faster by making COUNT a vector column and
#   thus allowing counts for all energies to be computed in a single pass.
#   This was then fed back into trf_hxisgdshield_15-06-17.docx.
#-------------------------------------------------------------------------------

# Set up.
use strict;
use warnings;

use ahapp;
use ahgen;
use ahlog;

# Turn on AUTOFLUSH
$| = 1;

# Script globals.
#-------------------------------------------------------------------------------
# For ease of comparison to TRF, which uses the construction par.infile, use
# a hash for all parameters. Also this makes it clear in the code which
# variables come from parameters.
our %par; # Hash of all the user (Ape) parameters.
our $tstart; # Beginning of time range being processed.
our $tstop; # End of time range being processed.
our $emin; # Minimum energy.
our $emax; # Maximum energy.
our $eminlimit = 0; # Lower limit on energy.
our $emaxlimit = 0; # Upper limit on energy.
our $instrument; # Name of instrument, from input file.
our @column; # Columns to bin.
# SPEC and SCAN parameters.
our $exposure; # Exposure time = diff between user-supplied TSTART and TSTOP.
# SPEC-only parameters.
our $dec_emin; # Minimum energy for deconvolved spectrum.
our $dec_emax; # Maximum energy for deconvolved spectrum.
our $offset; # Location (channel) of data in the output spectrum.
our @bound; # Location (channel) where binning changes in the input spectrum.
our @bin_factor; # Binning factor for each range given by the @bound array.
our $shieldname; # Name of the shield "instrument" used in output PHA file.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Main function execution. Call the top-level (main) subroutine.
#-------------------------------------------------------------------------------
exit &hxisgdshield();

sub hxisgdshield {
  #-------------------------------------------------------------------------------
  # Main script. Follow standard form for a tool.
  #-------------------------------------------------------------------------------
  my $status = &startup();

  if (!$status) {
    # Get user input parameters.
    $status = &getpar();
  }

  if (!$status) {
    # Perform initializations needed for the tool to run.
    $status = &initialize();
  }

  if (!$status) {
    # Do the main work of the tool (create the output LC/SPEC/SCAN product.
    $status = &dowork();
  }

  # Clean-up.
  if (&finalize($status)) { $status = 1; }
  if (&shutdown()) { $status = 1; }

  return $status;
}

#-------------------------------------------------------------------------------

sub startup {
  # Set the force_debug flag to non-zero (preferably 1) to force ouput of debugging messages.
  # Note that ahapp::startup performs no error checking of any kind, and does not return a status.
  my $force_debug = 0;
  &ahapp::startup($force_debug);
  return 0;
}

sub getpar {
  my $status = 0;
  # Prompt/construct hash in the order parameters appear in the file.
  foreach my $par (qw(infile outfile_root outfile_type datatype columnname
                      energybin tstart tstop deconvolve hkfile
                      hkext gticut_pre gticut_post cleanup clobber history)) {
    &ahgen::set_error_flag(0);
    $par{$par} = &ahapp::query_parameter($par);
    $status = &ahgen::get_error_flag();
    last if ($status);
  }

  # Issue some warnings.
  if ($par{'outfile_type'} !~ /^LC$/i and $par{'energybin'} !~ /^all$/i) {
    ahlog::ah_warn "HIGH", "The parameter energybin = $par{'energybin'} is ignored in $par{outfile_type} mode";
  } elsif ($par{'outfile_type'} !~ /^SPEC$/i and $par{'outfile_type'} !~ /^SCAN$/i) {
    if ($par{'tstart'} !~ /^TSTART$/i) {
      ahlog::ah_warn "HIGH", "The parameter tstart = $par{'tstart'} is used only in SPEC or SCAN mode";
    }
    if ($par{'tstop'} !~ /^TSTOP$/i) {
      ahlog::ah_warn "HIGH", "The parameter tstop = $par{'tstop'} is used only in SPEC or SCAN mode";
    }
  }
  return $status;
}

sub initialize {
  my $status = 0;

  # Pre-Processing
  &ahapp::begin_processing();

  # Set up for run: determine LC/SPEC/SCAN and set energy ranges etc.
  $status = &setrunmode();

  if (!$status) {
    # Choose columns for which to compute sums, based on instrument, and/or user parameter columnname.
    $status = &selectcolumns();
  }

  return $status;
}

sub dowork {
  # This code is consistent with TRF trf_hxisgdshield_15-06-17.docx.
  my $status = 0; # Error status.

  my $ii; # Loop index for looping over columns.
  # Step 4-3.
  # Loop over the columns selected based on user choice.
  for ($ii = 0; $ii < @column and !$status; $ii++) {
    my $infile = $par{'infile'}; # Input file supplied by user.
    my $column_ana = $column[$ii]; # Column name.
    my $ext; # Extension where this column is located.

    # Locate the extension where this column is located, based on the column name.
    $status = &setcolumnextension($column_ana, \$ext);

    # Set tstart/tstop and compute exposure.
    if (!$status) {
      $status = &set_time_range($infile, $ext);
    }
    
    if (!$status) {
      if ($par{'outfile_type'} =~ /^LC$/i) {
        $status = &createlc($infile, $ext, $column_ana);
      } elsif ($par{'outfile_type'} =~ /^SPEC$/i) {
        $status = &createspec($infile, $ext, $column_ana);
      } elsif ($par{'outfile_type'} =~ /^SCAN$/i) {
        $status = &createscan($infile, $ext, $column_ana);
      }
    }
  }
  return $status;
}

sub finalize {
  my $status = shift;
  # Note that end_processing calls ahapp::shutdown. It should not.
  # Note that end_processing has no means of indicating errors. It should.
  &ahapp::end_processing($status);
  return $status;
}

sub shutdown {
  my $status = 0;
  # Should call this, but end_processing already called it (in finalize).
  # Note that shutdown has no means of indicating errors. It should.
  # &ahapp::shutdown();
  return $status;
}

# Utility function for checking whether a value is in the range [min, max]. Returns 1 (true)
# if the value is *not* in the range, blank if it is.
sub outofrange { my ($val, $min, $max) = @_; defined $val and $val ne "" or $val = 0; return ($min > $val or $max < $val); }

sub setrunmode {
  my $status = 0;

  undef $emin; undef $emax;

  # Configure constants needed for each data type.
  if ($par{'datatype'} =~ /^SCL$/i) { $emaxlimit = 0; }
  elsif ($par{'datatype'} =~ /^HIST$/i) { $emaxlimit = 127; }
  elsif ($par{'datatype'} =~ /^GRB$/i) { $emaxlimit = 31; }

  # Interpret parameters to determine what type of data product to make (LC, SCAN, SPEC)
  # and configure the script to handle that data product properly.
  if ($par{'outfile_type'} =~ /^LC$/i) {
    # Note the parameter file has enumerated options SCL|HIST|GRB: this check is not strictly necessary.
    # Validate choices for datatype.
    if ($par{'datatype'} !~ /^SCL$/i && $par{'datatype'} !~ /^HIST$/i && $par{'datatype'} !~ /^GRB$/i) {
      ahlog::ah_err "Parameter \"datatype\" = \"$par{datatype}\" is invalid for LC mode; must be HIST, or GRB.";
      $status = 1;
    }

    # Parse energybin to get energy range.
    if (!$status) {
      if ($par{'energybin'} !~ /^all$/i) {
        # Parse parameter 7-96 into emin = 7, emax = 96. Use a regexp that requires both limits and the dash between.
        # At this stage, allow any integer for emin, emax, to keep this part general.
        if ($par{'energybin'} =~ /^\s*([+-]?\d+)\s*-\s*([+-]?\d+)\s*$/) { $emin = $1; $emax = $2; }
        else {
          ahlog::ah_err "Parameter \"energybin\" = \"$par{energybin}\" is invalid; must be \"all\" or a range, e.g., \"5-15\"";
          $status = 1;
        }
      }
    }

    # Validate selected energy limits.
    if (!$status) {
      # In case of "all", emin and emax will still be undefined at this point, so define them.
      defined $emin or $emin = $eminlimit; defined $emax or $emax = $emaxlimit;

      # Check whether chosen $emin is within the limits for this datatype.
      if (&outofrange($emin, $eminlimit, $emaxlimit)) {
        ahlog::ah_err "Lower energy limit $emin is outside of the allowed range [$eminlimit, $emaxlimit] for datatype $par{datatype}.";
        $status = 1;
      }

      # Check whether chosen $emax is within the limits for this datatype.
      if (&outofrange($emax, $eminlimit, $emaxlimit)) {
        ahlog::ah_err "Upper energy limit $emax is outside of the allowed range [$eminlimit, $emaxlimit] for datatype $par{datatype}.";
        $status = 1;
      }

      # Check whether emax is greater than or equal to emin.
      if (&outofrange($emax, $emin, $emax + 1)) {
        ahlog::ah_err "Energy limits [$emin, $emax] are not in ascending order.";
        $status = 1;
      }
    }

  } elsif ($par{'outfile_type'} =~ /^SPEC$/i) {
    # Validate choices for datatype.
    if ($par{'datatype'} !~ /^HIST$/i && $par{'datatype'} !~ /^GRB$/i) {
      ahlog::ah_err "Parameter \"datatype\" = \"$par{datatype}\" is invalid for SPEC mode; must be HIST, or GRB.";
      $status = 1;
    }

    if (!$status) {
      $emin = 0;
      $dec_emin = 0;
      $dec_emax = 4095;
      if ($par{'datatype'} =~ /^HIST$/i) {
        $emax = 127;
        $offset = 24;
        @bound = (68,80,112,116,124,128);
        @bin_factor = (2,8,16,64,128,512);
      } elsif ($par{'datatype'} =~ /^GRB$/i) {
        $emax = 31;
        $offset = 48;
        @bound = (9,17,22,26,29,31,32);
        @bin_factor = (16,32,64,128,256,512,1024);
      } # else { error already trapped above, see "Validate choices for datatype." }
    }
  } elsif ($par{'outfile_type'} =~ /^SCAN$/i) {
    # Validate choices for datatype.
    if ($par{'datatype'} !~ /^SCL$/i) {
      ahlog::ah_err "Parameter \"datatype\" = \"$par{datatype}\" is invalid for SCAN mode; must be SCL.";
      $status = 1;
    }
  } else {
    # Should not hit this case because outfile_type is enumerated to be either LC, SPEC, or SCAN.
    ahlog::ah_err "Parameter \"outfile_type\" = \"$par{outfile_type}\" is invalid; must be LC, SPEC, or SCAN.";
    $status = 1;
  }

  # If valid energy limits were not supplied, undefine them both. This is to prevent calling code from ignoring an error at this step.
  if ($status) { undef $emin; undef $emax; }

  return $status;
}

# Corresponds to TRF "Step 3: set column input"
sub selectcolumns {
  my $status = 0;

  $instrument = ahgen::get_keyword("$par{infile}", "1", "INSTRUME");

  # Clean up any previous column assignments.
  undef @column;

  if ($instrument !~ /^HXI[12]?$/i and $instrument !~ /^SGD[12]?$/i) {
    ahlog::ah_err "File given by parameter \"infile\": $par{infile} has data for instrument \"$instrument\", not HXI, HXI1, HXI2, SGD, SGD1, or SGD2";
    $status = 1;
  }

  if (!$status and $par{'columnname'} !~ /^all$/i) {
    # Select the columns based on the instrument if the columns were not already selected by the user parameter columnname.
    @column = split /[,\s]+/, $par{'columnname'};
  } elsif (!$status) {
    my @columnbase;

    # First set up all HXI cases (cases 1 through 3 in TRF).
    if ($instrument =~ /HXI/i) {
      if ($par{'datatype'} =~ /^SCL/i and $par{'outfile_type'} =~ /^LC$/i) {
        # Case 1-1.
        @columnbase = qw(SH_FBGO SH_HITPAT SH_UD SH_SUD);
      } elsif ($par{'datatype'} =~ /^SCL/i and $par{'outfile_type'} =~ /^SCAN$/i) {
        # Case 1-2.
        @columnbase = qw(SH_FBGO SH_HITPAT);
      } elsif ($par{'datatype'} =~ /^HIST$/i) {
        # Case 2.
        @columnbase = qw(SH_HIST);
      } elsif ($par{'datatype'} =~ /^GRB$/i) {
        # Case 3.
        @columnbase = qw(SH_GRB);
      } else {
        ahlog::ah_err "Data type $par{datatype} is not valid for HXI data.";
        $status = 1;
      }
    } else { # $instrument =~ /SGD/i.
      if ($par{'datatype'} =~ /^SCL/i and $par{'outfile_type'} =~ /^LC$/i) {
        # Case 4-1.
        @columnbase = qw(SH1_FBGO SH1_HITPAT SH1_UD SH1_SUD SH2_FBGO SH2_HITPAT SH2_UD SH2_SUD);
      } elsif ($par{'datatype'} =~ /^SCL/i and $par{'outfile_type'} =~ /^SCAN/i) {
        # Case 4-2.
        @columnbase = qw(SH1_FBGO SH1_HITPAT SH2_FBGO SH2_HITPAT);
      } elsif ($par{'datatype'} =~ /^HIST$/i) {
        # Case 5.
        @columnbase = qw(SH1_HIST SH2_HIST);
      } elsif ($par{'datatype'} =~ /^GRB$/i) {
        # Case 6.
        @columnbase = qw(SH1_GRB SH2_GRB);
      } else {
        ahlog::ah_err "Data type $par{datatype} is not valid for SGD data.";
        $status = 1;
      }
    }

    if (!$status) {
      # To each column name base numbers from 1 - 6 will be appended (GRB case) or 1 - 13 (other cases).
      my $maxcolnum;
      if ($par{'datatype'} =~ /^GRB$/i) {
        $maxcolnum = 6;
      } else {
        $maxcolnum = 13;
      }

      # Assign columns and extensions in which to find them.
      foreach my $ii (0..$#columnbase) {
        foreach my $jj (1..$maxcolnum) {
          my $column = $columnbase[$ii].$jj;
          push @column, $column;
        }
      }
    }
  }

  return $status;
}

sub setcolumnextension {
  my $status = 0;
  my $column = shift;
  my $ext = shift;
  # Corresponds to TRF "4-1: Determine extension to read".
  if ($column =~ /^SH_/i) {
    $shieldname = "SHIELD";
    if ($column =~ /HIST/i) {
      $$ext = 2;
    } else {
      $$ext = 1;
    }
  } elsif ($column =~ /^SH1_/i) {
    $shieldname = "SHIELD1";
    if ($column =~ /HIST/i) {
      $$ext = 2;
    } else {
      $$ext = 1;
    }
  } elsif ($column =~ /^SH2_/i) {
    $shieldname = "SHIELD2";
    if ($column =~ /HIST/i) {
      $$ext = 4;
    } elsif ($column =~ /GRB/i) {
      $$ext = 2;
    } else {
      $$ext = 3;
    }
  } else {
    ahlog::ah_err "Unable to determine extension containing column $column.";
    $status = 1;
  }
  return $status;
}

sub set_time_range {
  my $status = 0;
  my $infile = shift;
  my $ext = shift;

  if ($par{'outfile_type'} !~ /^LC$/i) {
    # Use the tstart parameter and/or the TSTART keyword to set the low end of time range.
    if ($par{'tstart'} =~ /^tstart$/i) {
      $tstart = ahgen::get_keyword($infile, $ext, 'TSTART');
      ahlog::ah_info "Using start time from TSTART keyword = $tstart.";
    } else {
      $tstart = $par{'tstart'};
      ahlog::ah_info "Using start time from tstart user parameter = $tstart.";
    }
  
    # Use the tstop parameter and/or the TSTOP keyword to set the upper end of time range.
    if ($par{'tstop'} =~ /^tstop$/i) {
      $tstop = ahgen::get_keyword($infile, $ext, 'TSTOP');
      ahlog::ah_info "Using stop time from TSTOP keyword = $tstop.";
    } else {
      $tstop = $par{'tstop'};
      ahlog::ah_info "Using stop time from tstop user parameter = $tstop.";
    }
  
    # Check for valid range, and compute exposure if all is well.
    if ($tstart >= $tstop) {
      ahlog::ah_err "TSTART = $tstart may not be greater than or equal to TSTOP = $tstop.";
      $status = 1;
    } else {
      $exposure = $tstop - $tstart;
    }
  }
  return $status;
}

sub setbitshrink {
  my $status = 0;
  my $column = shift;
  my $bit = shift;
  my $shrink = shift;

  # Corresponds to TRF "4-2: Set parameters".
  if ($column =~ /_FBGO/i) {
    $$bit = 16;
    $$shrink = 1;
  } elsif ($column =~ /_HITPAT/i) {
    $$bit = 16;
    $$shrink = 1;
  } elsif ($column =~ /_UD/i) {
    $$bit = 8;
    $$shrink = 2;
  } elsif ($column =~ /_SUD/i) {
    $$bit = 4;
    $$shrink = 1;
  } elsif ($column =~ /_HIST/i) {
    $$bit = 8;
    $$shrink = 1;
  } elsif ($column =~ /_GRB/i) {
    $$bit = 8;
    $$shrink = 1;
  } else {
    ahlog::ah_err "Unable to determine shrink factor and number of bits for column $column.";
    $status = 1;
  }
  return $status;
}

sub computecount {
  # This function computes the COUNTS column from a specified column in a
  # file+extension by taking the difference between subsequent rows, and adding 2^$bit
  # when a row "rolls over". The input column may be a scalar or a vector. The output
  # "COUNTS" column will match the format of the input column. This operation is
  # shared between the LC and SPEC modes.
  my $status = 0; # Error status.
  my $infile = shift; # Input file name.
  my $ext = shift; # Input file extension.
  my $column_ana = shift; # Input column from which to compute the counts.
  my $outfile = shift; # Name of output file.
  my $jj; # Top-level loop variable for looping over energy channels.

  # Clean up any previously-created temporary files. Do this even if cleanup is not enabled.
  unlink 'tmp1.fits'; unlink $outfile;

  my $bit; # Number of bits in the input channel.
  my $shrink; # Factor by which input data was "shrunk".
  # Set $bit and $shrink appropriately for the selected column.
  $status = &setbitshrink($column_ana, \$bit, \$shrink);

  if ($eminlimit < $emaxlimit) {
    if (!$status) {
      # Create an expression that will add a boolean "ERANGE" column. For energies
      # inside the range [emin, emax] the column will be T, otherwise F. This can
      # be used below to sum only energies inside the range.
      my $erangewidth = $emaxlimit - $eminlimit + 1;
      my $expr = "col *,ERANGE(${erangewidth}L)={";
      for ($jj = $eminlimit; $jj <= $emaxlimit; $jj++) {
        if (&outofrange($jj, $emin, $emax)) {
          $expr.= 'F,';
        } else {
          $expr.= 'T,';
        }
      }
      chop $expr; # Kill the trailing comma.
      $expr .= '}'; # Add the trailing brace.
  
      # Similarly, now add an expression that will add an "HSTSHRNK" column
      # to hold the value for hist_shrink for each energy. This can then be
      # used in the calculation expression to compute the COUNT column
      # correctly for each energy in one pass.
      $expr .= ",HSTSHRNK(${erangewidth}J)={";
      for ($jj = $eminlimit; $jj <= $emaxlimit; $jj++) {
        $expr .= ($column_ana =~ /HIST/i and ($jj <= 15)) ? 4 : 1;
        $expr .= ',';
      }
      chop $expr; # Kill the trailing comma.
      $expr.= '}'; # Add the trailing brace.
  
      # Use the expression constructed above in a call to ftcopy to add
      # these columns. The columns ERANGE and HSTSHRNK can then be used to
      # compute the COUNT vector-valued column in one pass. for all energies.
      $status = &ahgen::run_ftool("ftcopy", "infile=$infile+${ext}[$expr]", "outfile=$outfile");
    }
  
    if (!$status) {
      rename $outfile, 'tmp1.fits';
      $infile = 'tmp1.fits';
  
      # Use fcalc to create the COUNT vector column, using the
      # ERANGE column to set to 0 counts outside the energy range
      # and HSTSHRNK column to multiply by the correct value of
      # histshrink.
      my $expr = "ERANGE ? ".
              "(${column_ana}-${column_ana}{-1} >= 0 ? ".
              "(${column_ana}-${column_ana}{-1})*2^$shrink*HSTSHRNK: ".
              "(${column_ana}-${column_ana}{-1}+2^$bit)*2^$shrink*HSTSHRNK)".
              " : 0";
      $status = &ahgen::run_ftool("fcalc", "infile=$infile+$ext", "outfile=$outfile", "clname=COUNT", "expr=$expr");
    }
  } else {
      # Use fcalc to create the COUNT scalar column.
      my $expr = "${column_ana}-${column_ana}{-1} >= 0 ? ".
              "(${column_ana}-${column_ana}{-1})*2^$shrink: ".
              "(${column_ana}-${column_ana}{-1}+2^$bit)*2^$shrink";
      $status = &ahgen::run_ftool("fcalc", "infile=$infile+$ext", "outfile=$outfile", "clname=TOTCOUNT", "expr=$expr");
  }



  # for all the files just created, add CHECKSUM and DATASUM, and 
  # check that files are valid
  for my $currtmpfile ("tmp1.fits", "$outfile") {
    if (-e $currtmpfile) {
      &ahgen::update_checksum_and_verify($currtmpfile);
    }
  }

  return $status;
}

sub createlc {
  # This code is consistent with TRF trf_hxisgdshield_15-06-17.docx.
  my $status = 0; # Error status.
  my $infile = shift;
  my $ext = shift;
  my $column_ana = shift;

  # Compute the COUNTS column, stored in a temporary file.
  $status = &computecount($infile, $ext, $column_ana, 'tmp2.fits');

  if (!$status) {
    # Move the output file from the previous step to become the input to the next step.
    rename 'tmp2.fits', 'tmp1.fits';
    $infile = 'tmp1.fits';

    if ($eminlimit < $emaxlimit) {
      # Step 4-4.
      # Sum the COUNT vector column to produce a scalar TOTCOUNT column in a second temporary file.
      $status = &ahgen::run_ftool("fcalc", "infile=$infile+$ext", "outfile=tmp2.fits", "clname=TOTCOUNT", "expr=SUM(COUNT)");
    }
  }

  if (!$status) {
    # Move the output file from the previous step to become the input to the next step.
    rename 'tmp2.fits', $infile;

    # Step 4-5.
    # Clean up temporary files and output files that may have been created in a previous run.
    # Do this even if cleanup is "no".
    unlink 'timebin.fits', 'rate.fits', 'error.fits', 'fracexp.fits', "$par{outfile_root}_${column_ana}.lc";

    # Compute the TIMEBIN column from the difference in times between subsequent rows.
    $status = &ahgen::run_ftool('fcalc', "infile=$infile+$ext", 'outfile=timebin.fits',
      'clname=TIMEBIN', 'expr=TIME-TIME{-1}');
  }

  if (!$status) {
    $infile = 'timebin.fits'; # Output from last step becomes input to this step.
    # Compute the RATE column by dividing total counts by time difference.
    $status = &ahgen::run_ftool('fcalc', "infile=$infile+$ext", 'outfile=rate.fits',
      'clname=RATE', 'expr=TOTCOUNT/TIMEBIN');
  }

  if (!$status) {
    $infile = 'rate.fits'; # Output from last step becomes input to this step.
    # Compute the ERROR column by dividing square root of total counts by time difference.
    $status = &ahgen::run_ftool('fcalc', "infile=$infile+$ext", 'outfile=error.fits',
      'clname=ERROR', 'expr=sqrt(TOTCOUNT)/TIMEBIN');
  }

  if (!$status) {
    $infile = 'error.fits'; # Output from last step becomes input to this step.
    # Set the FRACEXP column to a constant value of 1.
    $status = &ahgen::run_ftool('fcalc', "infile=$infile+$ext", 'outfile=fracexp.fits',
      'clname=FRACEXP', 'expr=1');
  }

  if (!$status) {
    $infile = 'fracexp.fits'; # Output from last step becomes input to this step.
    # The light curve calculations were complete at the end of the previous step.
    # Now just create a clean output file containing only the standard light curve
    # columns: TIME, RATE, ERROR and FRACEXP.
    my $outfile = "$par{outfile_root}_${column_ana}.lc";
    $status = &ahgen::run_ftool('ftabcopy', "infile=$infile+$ext", "outfile=$outfile",
      'columns=TIME,RATE,ERROR,FRACEXP', 'rows=-', "clobber=$par{clobber}");

    # Write the parameters to the output file now that it exists.
    if ($par{'history'} =~ /^yes$/i) {
      &ahapp::write_parameters($outfile, 1);
    }
    
    # Add CHECKSUM and DATASUM, and check that file we just created is valid
    &ahgen::update_checksum_and_verify($outfile);
  }
  
  # for extra files created, and if user decided to keep temp files, 
  # add CHECKSUM and DATASUM, and check that files are valid.  
  if ($par{'cleanup'} =~ /^n/i) {
    for my $currtmpfile ('tmp1.fits', 'tmp2.fits', 'timebin.fits', 'rate.fits', 'error.fits', 'fracexp.fits', 'rate.fits', 'error.fits', 'fracexp.fits', "$par{outfile_root}_${column_ana}.lc") {
      if (-e $currtmpfile) {
        &ahgen::update_checksum_and_verify($currtmpfile);
      }
    }
  }

  if (!$status and $par{'cleanup'} =~ /^y/i) {
    # Clean up now if cleanup parameter set.
    unlink 'tmp1.fits', 'tmp2.fits', 'timebin.fits', 'rate.fits', 'error.fits', 'fracexp.fits';
  }

  return $status;
}

sub createspec {
  # This code is consistent with TRF trf_hxisgdshield_15-06-17.docx.
  my $status = 0; # Error status.
  my $infile = shift;
  my $ext = shift;
  my $column_ana = shift;

  # Clean up from any previous runs.
  unlink 'tmp_time.fits', 'tmp_spec.tbl';

  # Step 4-3: Calculate spectra
  $status = &ahgen::run_ftool("fselect", "infile=$infile+$ext", "outfile=tmp_time.fits",
    "expr=TIME>=$tstart && TIME<=$tstop");

  if (!$status) {
    $infile = 'tmp_time.fits';
    $status = &computecount($infile, $ext, $column_ana, 'tmp2.fits');
  }

  if (!$status) {
    rename 'tmp2.fits', $infile;

    $status = &ahgen::run_ftool("fsumrows", "infile=$infile+$ext", "outfile=tmp2.fits", "cols=COUNT", "rows=-",
      "operation=SUM", "sametype=yes");
  }

  if (!$status) {
    rename 'tmp2.fits', $infile;

    # Extension number is always 1 for the output from fsumrows.
    # Extract the summed data and write it to a text file to use as input to ascii2pha.
    $status = &ahgen::run_ftool("ftlist", "infile=$infile+1", "option=t", "outfile=tmp_spec.tbl",
      "rownum=no", "colheader=no");
  }

  if (!$status) {
    # Create the PHA file.
    my $detchans = $emax - $emin + 1;
    my $outfile = "$par{outfile_root}_${column_ana}.pha";
    $status = &ahgen::run_ftool("ascii2pha", "infile=tmp_spec.tbl", "outfile=$outfile",
      "chanpres=no", "dtype=1", "qerror=no", "rows=-", "fchan=$emin", "tlmin=0", "detchans=$detchans",
      "pois=no", "telescope=HITOMI", "instrume=$instrument", "detnam=$shieldname", "filter=NONE", "exposure=$exposure",
      "clobber=$par{clobber}");

    # Write the parameters to the output file now that it exists.
    if ($par{'history'} =~ /^yes$/i) {
      &ahapp::write_parameters($outfile, 1);
    }
    
    # Add CHECKSUM and DATASUM, and check that file we just created is valid
    &ahgen::update_checksum_and_verify($outfile);
  }
  
  # for all the files just created, add CHECKSUM and DATASUM, and 
  # check that files are valid
  for my $currtmpfile ("tmp_time.fits") {
    if (-e $currtmpfile) {
      &ahgen::update_checksum_and_verify($currtmpfile);
    }
  }

  if (!$status) {
    if ($par{'deconvolve'} =~ /y/i) {
      $status = &createdeconvolvedspec($column_ana, 'tmp_spec.tbl');
    }
  }

  return $status;
}

sub createdeconvolvedspec {
  # This code is consistent with TRF trf_hxisgdshield_15-06-17.docx.
  # This function operates on a text file containing a spectrum to compute a deconvolved spectrum.
  my $status = 0; # Error status.
  my $column_ana = shift; # Column that was the source for the input spectrum, used only to name the output file.
  my $inputspecfile = shift; # Name of the input spectrum file.
  my $deconvspecfile = 'spec_deconv.tbl'; # Name of temporary text file containing deconvolved spectrum.
  my @inputspec; # The input spectrum, before deconvolution.

  # Read input file.
  if (open INFILE, $inputspecfile) {
    @inputspec = <INFILE>; close INFILE;
    chomp @inputspec;
  } else {
    ahlog::ah_err "createdeconvolvedspec: unable to open file $inputspecfile.";
    $status = 1;
  }

  my @deconvspec; # The data for the deconvolved output spectrum.
  if (!$status) {
    unlink $deconvspecfile;

    # First section of the output spectrum consists of leading zeroes up
    # to the cutoff given by the $offset variable.
    my $ii;
    for ($ii = 0; $ii < $offset; $ii++) {
      my $line = sprintf("%.12f", 0.);
      push @deconvspec, $line;
    }

    # Compute the remainder of the output spectrum from the input spectrum.
    my $boundary_min = 0; # Lower boundary of section of the input spectrum currently being deconvolved.
    my $loop; # Loop variable for looping over the bounds: the points where the input file binning changes.

    # +++ 2015-07-27 JP: after checking with Hiroya, change to bounds, both code and TRF.
    # Loop over the bounds, each of which had a different binning applied.
    for ($loop = 0; $loop < @bound; $loop++) {
      my $boundary_max = $bound[$loop]; # The end point for this bound.
      my $bin = $bin_factor[$loop]; # The binning factor by which to deconvolve this bound.
      my $jj; # Loop variable for looping over the input channels, which are expanded into output channels.

      # Within this bound, loop over the input channels to expand them into the output channels.
      for ($jj = $boundary_min; $jj < $boundary_max; $jj++) {
        my $ii; # Loop variable for looping over output channels derived from the current input channel.
        # Loop over output channels derived from the input channel.
        # Each input channel with counts = X is turned into $bin output channels with rate X/$bin/$exposure.
        my $drate = $inputspec[$jj]/$bin/$exposure;
        for ($ii = 0; $ii < $bin; $ii++) {
          # Spread the counts in this input channel evenly over the output channel, and divide by the exposure
          # to produce "RATE" data.
          my $line = sprintf("%.12f", $drate);
          push @deconvspec, $line;
        }
      }
      # The next input channel will pick up right after the ($boundary_max - $boundary_min + 1) input channels
      # looped over $jj.
      $boundary_min = $boundary_max;
    }
    # Write the deconvolved spectrum in text form, to be used as input to ascii2pha.
    if (open OUTFILE, ">$deconvspecfile") {
      foreach my $line (@deconvspec) {
        print OUTFILE "$line\n";
      }
      close OUTFILE;
    } else {
      ahlog::ah_err "createdeconvolvedspec: unable to open for writing file $deconvspecfile";
      $status = 1;
    }
  }
  if (!$status) {
    # For clarity, compute the "detchans" for this case.
    my $detchans = $dec_emax - $dec_emin + 1;
    my $outfile = "$par{outfile_root}_${column_ana}_deconv.pha";
    # Use ascii2pha to create a PHA file from the text version of the deconvolved spectrum.
    $status = &ahgen::run_ftool("ascii2pha", "infile=$deconvspecfile", "outfile=$outfile",
      "chanpres=no", "dtype=2", "qerror=no", "rows=-", "fchan=$dec_emin", "tlmin=0", "detchans=$detchans",
      "pois=no", "telescope=HITOMI", "instrume=$instrument", "detnam=$shieldname", "filter=NONE", "exposure=$exposure",
      "clobber=$par{clobber}");

    # Write the parameters to the output file now that it exists.
    if ($par{'history'} =~ /^yes$/i) {
      &ahapp::write_parameters($outfile, 1);
    }
    
    # Add CHECKSUM and DATASUM, and check that file we just created is valid
    &ahgen::update_checksum_and_verify($outfile);
  }
  
  return $status;
}

sub createscan {
  # This code is consistent with TRF trf_hxisgdshield_15-06-17.docx.
  my $status = 0; # Error status.
  my $infile = shift;
  my $ext = shift;
  my $column_ana = shift;

  # Step 4-3.
  my $bit;
  my $shrink;
  $status = &setbitshrink($column_ana, \$bit, \$shrink);

  if (!$status) {
    unlink 'test_time.fits', 'test1.fits', 'test2.fits', 'test3.fits';
    $status = &ahgen::run_ftool("fcalc", "infile=$infile+$ext", "outfile=test_time.fits", "clname=TIMEBIN",
      "expr=TIME-TIME{-1}");
  }

  if (!$status) {
    $infile = 'test_time.fits';
    $status = &ahgen::run_ftool("fcalc", "infile=$infile+$ext", "outfile=test1.fits", "clname=RATE",
      "expr=${column_ana}-${column_ana}{-1} >= 0 ? ".
                 "(${column_ana}-${column_ana}{-1})*2^$shrink/TIMEBIN: ".
                 "(${column_ana}-${column_ana}{-1}+2^$bit)*2^$shrink/TIMEBIN");
  }

  if (!$status) {
    $infile = 'test1.fits';
    $status = &ahgen::run_ftool("fcalc", "infile=$infile+$ext", "outfile=test2.fits", "clname=ERROR",
      "expr=${column_ana}-${column_ana}{-1} >= 0 ? ".
                 "sqrt((${column_ana}-${column_ana}{-1})*2^$shrink)/TIMEBIN: ".
                 "sqrt((${column_ana}-${column_ana}{-1}+2^$bit)*2^$shrink)/TIMEBIN");
  }

  if (!$status) {
    $infile = 'test2.fits';
    $status = &ahgen::run_ftool("fcalc", "infile=$infile+$ext", "outfile=test3.fits", "clname=FRACEXP",
      "expr=1");
  }

  if (!$status) {
    $infile = 'test3.fits';
    my $outfile = "$par{outfile_root}_${column_ana}.lc";
    $status = &ahgen::run_ftool('ftabcopy', "infile=$infile+$ext", "outfile=$outfile",
      'columns=TIME,RATE,ERROR,FRACEXP', 'rows=-', "clobber=$par{clobber}");

    # Write the parameters to the output file now that it exists. Note that this set of history
    # will be propagated to the GTI-filtered file as well.
    if ($par{'history'} =~ /^yes$/i) {
      &ahapp::write_parameters($outfile, 1);
    }
    
    # Add CHECKSUM and DATASUM, and check that file we just created is valid
    &ahgen::update_checksum_and_verify($outfile);
  }

  if (!$status) {
    unlink 'test.fits', 'test_pre.fits', 'test_after.fits', 'test_gti.fits';
    $infile=$par{'hkfile'};
    $status = &ahgen::run_ftool('fselect', "infile=${infile}[$par{hkext}]", "outfile=test.fits",
      "expr=TIME>=$tstart&&TIME<=$tstop");
  }

  if (!$status) {
    $infile = 'test.fits';
    $status = &ahgen::run_ftool('fcalc', "infile=${infile}[$par{hkext}]", "outfile=test_pre.fits", "clname=START",
      "expr=TIME-$par{gticut_pre}");
  }

  if (!$status) {
    $infile = 'test_pre.fits';
    $status = &ahgen::run_ftool('fcalc', "infile=${infile}[$par{hkext}]", "outfile=test_after.fits", "clname=STOP",
      "expr=TIME+$par{gticut_post}");
  }

  if (!$status) {
    $infile = 'test_after.fits';
    $status = &ahgen::run_ftool('fappend', "infile=${infile}[$par{hkext}][col START,STOP]", "outfile=$par{outfile_root}_${column_ana}.lc");
  }

  if (!$status) {
    $infile = "$par{outfile_root}_${column_ana}.lc";
    $status = &ahgen::run_ftool('fthedit', "infile=${infile}[$par{hkext}]", "keyword=EXTNAME", "operation=add", "value=GTI");
  }

  if (!$status) {
    $infile="$par{outfile_root}_${column_ana}.lc";
    # GTI-filtered file will have this tool's parameter history block copied from the input file.
    $status = &ahgen::run_ftool('fselect', "infile=$infile", "outfile=$par{outfile_root}_${column_ana}_gti.lc",
      'expr=gtifilter()', "clobber=$par{clobber}");
  }
  
  # for all the files just created, add CHECKSUM and DATASUM, and 
  # check that files are valid
  for my $currtmpfile ("test_time.fits", "test1.fits", "test2.fits", "test3.fits", "$par{outfile_root}_${column_ana}_gti.lc", "$par{outfile_root}_${column_ana}.lc") {
    if (-e $currtmpfile) {
      &ahgen::update_checksum_and_verify($currtmpfile);
    }
  }
  
  return $status;
}
# $Log: hxisgdshield.pl,v $
# Revision 1.24  2016/02/29 18:46:24  klrutkow
# added call to ahgen::update_checksum_and_verify for all outfiles
#
# Revision 1.23  2016/02/19 00:22:39  klrutkow
# change mission to HITOMI
#
# Revision 1.22  2015/10/30 18:48:24  peachey
# Corrected a bug in which many extra channels with 0 counts were being
# added to all deconvolved spectra after the first one was created. The
# "offset" used to put in the initial 0s was being increased more with
# each file. There is no reason to change the offset value, the increment
# was probably a holdover from a previous version.
#
# Revision 1.21  2015/10/30 15:58:09  peachey
# Explicitly specify the housekeeping extension when performing fselect and fappend
# steps in SCAN mode.
#
# Revision 1.20  2015/08/24 19:41:13  peachey
# Temporarily change this script to work around a bug in ahlog which causes
# messages with "LOW" priority to be printed as errors. Thus for now,
# changing warnings to have "HIGH" priority, which mysteriously causes
# ahlog to print them properly, i.e., as warnings. Need to change this
# back when ahlog.pm behaves properly.
#
# Revision 1.19  2015/08/21 16:58:45  peachey
# Unrelated changes to sub getpar for processing parameters.
# 1. Check error status after each call to query_parameter, in order
#    to exit the loop the first time a parameter error occurs. Indicate
#    such errors by setting status.
# 2. Issue warnings for hidden parameters that are ignored because of the
#    mode of operation, i.e., energybin used for SPEC or SCAN, or
#    tstart/tstop used for LC.
#
# Revision 1.18  2015/08/19 18:48:34  peachey
# Write history keywords in output files.
#
# Revision 1.17  2015/08/12 15:31:07  peachey
# Accept user parameter tstart=TSTART and tstop=TSTOP, either/both of
# which cause the corresponding keywords to be read from the input file
# and used for time filtering.
#
# Revision 1.16  2015/08/07 13:59:23  peachey
# For SCL cases, do not bother with ERANGE, HSTSHRNK, or COUNTS intermediate columns.
#
# Revision 1.15  2015/08/07 13:10:21  peachey
# Remove unused functions setupcolumn and create_all_data_products.
#
# Revision 1.14  2015/08/07 13:02:43  peachey
# Change 'bond' to 'bound'
#
# Revision 1.13  2015/07/28 17:37:21  peachey
# Oops, forgot at the last moment to reset the channel
# to 0 in the convolved spectrum.
#
# Revision 1.12  2015/07/27 20:03:52  peachey
# Change spectra to start with channel 0 rather than 1. Additional
# miscellaneous fixes.
#
# Revision 1.11  2015/07/13 17:53:45  peachey
# Cosmetic improvements made while reconciling with TRF.
#
# Revision 1.10  2015/07/08 17:56:59  peachey
# Handle clobber for final output files consistently. Turn off the force_debug option.
#
# Revision 1.9  2015/07/08 17:27:50  peachey
# Final changes needed for SCAN mode: fix typos and add a call to fthedit
# to change the name of the GTI extension to GTI.
#
# Revision 1.8  2015/06/22 13:25:59  peachey
# 1. Add full SPEC mode, including deconvolved spectrum output.
# 2. Add full SCAN mode, but the last stage is commented out and remains
# untested since I do not yet have a HK file.
# Regarding SCAN and LC, there are some questions concerning the numbering
# of the channels, the way the channel numbers are added to the spectrum,
# and whether the TRF is correct as to the type of each spectrum
# (COUNTS v. RATE). Otherwise this version is pretty consistent with
# trf_hxisgdshield_15-06-17.docx.
#
# Revision 1.7  2015/06/17 17:14:41  peachey
# Rename createlc and createlc2 so that the current code is named createlc
# and the original version is createlc_trf_20150506.
#
# Revision 1.6  2015/06/17 16:04:07  peachey
# Some cosmetic code improvements and improvements to the comments.
# This version is consistent with TRF trf_hxisgdshield_15-06-17.docx.
#
# Revision 1.5  2015/06/05 20:16:21  peachey
# Improve comments.
#
# Revision 1.4  2015/06/05 19:53:50  peachey
# Fix a few small bugs. Enable debug mode always for now. Use the more efficient
# algorithm rather than the one in the TRF.
#
# Revision 1.3  2015/06/03 22:54:14  peachey
# Initial functional version of the script. Includes alternate version of the
# algorithm for lightcurves which is much more efficient.
#
# Revision 1.2  2015/05/22 14:10:33  peachey
# Restructure more along standard lines.
#
# Revision 1.1  2015/05/08 13:00:10  peachey
# Initial shell of hxisgdshield script.
#
