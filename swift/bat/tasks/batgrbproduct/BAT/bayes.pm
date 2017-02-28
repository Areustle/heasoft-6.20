
package BAT::bayes;
use Astro::FITS::CFITSIO qw(:longnames :constants);
use BAT::log;

#
# Compute Bayesian block GTI and burst duration measures
#
# Step 70
#
# This module also writes additional GTIs which contain the total
# burst duration and the background intervals, as reported by the task
# battblocks.
#
# $self - class name (ignored)
# $info - generic information hash reference
#    INPUTS: $info->{qmap} - quality map
#            $info->{tlookback} - battblocks parameter value
#            $info->{tpeak} - battblocks parameter value
#    OUTPUTS: $info->{t90dur/t90err} - T90 estimate and uncertainty
#             $info->{t50dur/t50err} - T50 estimate and uncertainty
#             $info->{tpeak} - estimated time of peak flux
#             $info->{cntflu} - estimated total fluence
#             $info->{gti_bb} - name of Bayesian block GTI
#             $info->{gti_t90} - name of T90 GTI
#             $info->{gti_t50} - name of T90 GTI
#             $info->{gti_peak} - name of peak-flux GTI
#             $info->{gti_bkg1} - name of background interval #1
#             $info->{gti_bkg2} - name of background interval #2
#             $info->{gti_tot} - name of total burst GTI
# $infiles - scalar string containg input file name(s) or wildcard specs
#            if undefined, then assume that batbinevt/battblocks have already
#            been run, and read their output.
# $bbfile - output Bayesian block GTI file
# $durfile - output duration measure GTI file
# $erange - energy range to analyze (scalar string, e.g. "15-350")
# $tbinsize - time binsize in seconds for temporary light curve
# $lc4ms - name of temporary light curve
# $peakint - peak flux interval [seconds] (default 1.0)
#
# RETURNS: revised $info
#
# EXCEPTIONS: * no matching input files
#             * failed batbinevt
#             * failed battblocks
#             * could not open Bayesian blocks GTI file
#             * could not bracket burst
#             * could not create background or GTI_TOT intervals
#
# MODIFICATIONS:
#   02 Sep 2004 CM
#     Add $tbinsize parameter (interface change)
#   28 Sep 2004 CM
#     Add $peakint parameter (interface change)
#     Now read TOTDUR, TOTSTART, TOTSTOP keywords into hash parameters
#
sub compute {
    my ($self,$info,$log,$infiles,$bbfile,$durfile,$erange,$tbinsize,$lc4ms,$peakint) = @_;
    my ($status,$qmap,$inlist,$basedir,$cmd);
    my ($tlookback,$tpeak,$bbstartl,$bbstopl,$tmpfile,$fits);
    my ($t90dur,$t90err,$t50dur,$t50err,$tpeak,$cntflu,$tstart,$tstop);
    my ($bkgstr);
    my (@globlist,@errlog,@bbstart,@bbstop);

    if ($infiles) {
      $qmap = $info->{qmap};
      
      if ("$infiles" =~ m/^@/) { 
	@globlist = ("$infiles");   # Batch file
      } else {
	@globlist = glob("$infiles");
      }
      die "ERROR: no input files" if ($#globlist == -1);
      $inlist = join(",",@globlist);
      if ($info->{tbkgsub}) { $bkgstr = "YES"; } else { $bkgstr = "NO"; }
      
      # Default name of 4ms light curve
      unless ($lc4ms) {
	$basedir = $globlist[0];
	$basedir =~ s|/[^/]*$||;   # Strip trailing filename
	
	$lc4ms = "$basedir/swbev_4ms.lc.tmp";
      }
      $peakint = 1.0 unless ($peakint);
      
      unlink($lc4ms);
      $cmd = "batbinevt infile='$inlist' outfile='$lc4ms' outtype=LC timedel=$tbinsize timebinalg=u energybins='$erange' " .
	"detmask='$qmap' " .
	  "ecol=ENERGY weighted=YES outunits=RATE clobber=yes";
      $status = BAT::log->callnote($log,$cmd,
		   "Creating trial $tbinsize second light curve");
      if ($status || ! -f $lc4ms ) {
	die "ERROR: batbinevt failed while estimating GTIs";
      }
      
      $tlookback = $info->{tlookback};
      
      unlink($bbfile); unlink($durfile);
      $cmd = "battblocks infile='$lc4ms' outfile='$bbfile' durfile='$durfile' " .
	"tlookback=$tlookback " .
	  "bkgsub='$bkgstr' timecol=TIME countscol=RATE errcol=ERROR ".
	    "hduclas3=RATE tpeak=$peakint clobber=yes";
      
      $status = BAT::log->callnote($log,$cmd,
		   "Estimating Bayesian Blocks from trial $tbinsize second light curve");
      if ($status || ! -f $bbfile ) {
	die "ERROR: battblocks failed while estimating GTIs";
      }
    }

    die "ERROR: battblocks did not create $bbfile"  if (! -f $bbfile );
    die "ERROR: battblocks did not create $durfile" if (! -f $durfile );

    # Read the Bayesian blocks...
    # ... and also read the T50/90 keywords
    $status = 0;
    $status = SimpleFITS->open("<$bbfile")->move("STDGTI")
	->readcol("START",{type=>TDOUBLE},$bbstartl)
	->readcol("STOP",{type=>TDOUBLE},$bbstopl)
	->readkey("T90DUR",$t90dur)  # T90 value and error
	->readkey("T90ERR",$t90err)
	->readkey("T50DUR",$t50dur)  # T50 value and error
	->readkey("T50ERR",$t50err)
	->readkey("TPEAK",$tpeak)    # Epoch of peak flux
	->readkey("CNTFLU",$cntflu)  # Counts fluence for GRB

	->readkey("TOTDUR",$totdur)  # Total duration
	->readkey("TOTSTART",$totstart) # Start of total burst
	->readkey("TOTSTOP",$totstop)# End of total burst

	->close()
	->status();

    die "ERROR: could not open/read $bbfile (status $status)" if ($status);

    @bbstart = @$bbstartl; @bbstop = @$bbstopl;
    die "ERROR: too few Bayesian blocks to bracket the GRB" if ($#bbstart < 2);

    # Store the statistics
    $info->{t90dur} = $t90dur;
    $info->{t90err} = $t90err;
    $info->{t50dur} = $t50dur;
    $info->{t50err} = $t50err;
    $info->{tpeak}  = $tpeak;
    $info->{cntflu} = $cntflu;
    $info->{totdur} = $totdur;
    $info->{totstart} = $totstart;
    $info->{totstop} = $totstop;

    # Store the GTI file names
    $info->{gti_bb}  = "$bbfile";
    $info->{gti_t90} = "$durfile"."[GTI_T90]";
    $info->{gti_t50} = "$durfile"."[GTI_T50]";
    $info->{gti_peak} = "$durfile"."[GTI_PEAK]";

    # Same as TOTSTART and TOTSTOP (in build 9.1)
    $tstart = $bbstop[0];
    $tstop  = $bbstart[$#bbstart];

    $tmpfile = "$bbfile" . ".tmp"; 
    unlink("$tmpfile");
    my $fname = "$durfile"."[1]";
    $cmd = "ftcopy infile='$fname' outfile='$tmpfile' clobber=yes copyall=no";
    BAT::log->call($log,$cmd);
    die "ERROR: could not create GTI_BKG1" if (! -f $tmpfile);

    # Write first background interval
    $status = SimpleFITS->open("+<$tmpfile")->move("2")
	->writecol("START",{type=>TDOUBLE},-1.0e307)
	->writecol("STOP",{type=>TDOUBLE},$tstart)
	->writekey("TSTART",-1.0e307)
	->writekey("TSTOP",$tstart)
	->writekey("EXTNAME","GTI_BKG1")
	->close()->status();

    die "ERROR: could not write GTI_BKG1 (status=$status)" if ($status);

    my $fname = "$tmpfile"."[GTI_BKG1]";
    system("ftappend '$fname' $durfile");
    die $? if ($?);

    $info->{gti_bkg1} = "$durfile"."[GTI_BKG1]";

    # Write second background interval
    $status = SimpleFITS->open("+<$tmpfile")->move("2")
	->writecol("START",{type=>TDOUBLE},$tstop)
	->writecol("STOP",{type=>TDOUBLE},1.0e307)
	->writekey("TSTART",$tstop)
	->writekey("TSTOP",1.0e307)
	->writekey("EXTNAME","GTI_BKG2")
	->close()->status();

    die "ERROR: could not write GTI_BKG2 (status=$status)" if ($status);

    my $fname = "$tmpfile"."[GTI_BKG2]";
    system("ftappend '$fname' $durfile");
    die $? if ($?);
    $info->{gti_bkg2} = "$durfile"."[GTI_BKG2]";

    # Check for GTI_TOT (build 9.1 has it already)
    $fits = SimpleFITS->open("<$durfile")->move("GTI_TOT");
    $status = $fits->status();
    $fits->setstatus(0)->close();
    undef($fits);

    # If it does not exist, then create one
    if ($status) {
	$status = SimpleFITS->open("+<$tmpfile")->move("2")
	    ->writecol("START",{type=>TDOUBLE},$tstart)
	    ->writecol("STOP",{type=>TDOUBLE},$tstop)
	    ->writekey("TSTART",$tstart)
	    ->writekey("TSTOP",$tstop)
	    ->writekey("EXTNAME","GTI_TOT")
	    ->close()->status();

	die "ERROR: could not write GTI_TOT (status=$status)" if ($status);
	
	my $fname = "$tmpfile"."[GTI_TOT]";
	system("ftappend '$fname' $durfile");
	die $? if ($?);
    }
    $info->{gti_tot} = "$durfile"."[GTI_TOT]";

    unlink("$tmpfile");
    return $info;
}

1;
