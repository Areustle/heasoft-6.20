
package BAT::lc;
use BAT::log;

# Compute mask-weighted (or unweighted) spectra
#
# Step 160
#
# This module accumulates light curves from the input files.
#
# $self - class name (ignored)
# $infiles - input file wildcard spec, or @batch file
# $lcfile - output file name
# $erange - energy range to process
# $qmap - detector quality map (defaults to "NONE")
# $tbinsize - time binning method and bin size
#             a number alone indicates "uniform" binning
#             a "g" indicates "GTI" binning
# $weighted - "YES"/"NO" whether to weighte the light curves (default "YES")
#
# RETURNS: nothing
#
# EXCEPTIONS: * no matching input files
#             * batbinevt failed
# WARNING: * failed to create output file
#
# MODIFICATIONS:
#   28 Sep 2004 CM
#     Add $outunits parameter (interface change)
#

sub make {
    my ($self,$log,$infiles,$lcfile,$erange,$qmap,$gti,$tbinsize,$weighted,$outunits) = @_;
    my ($status,$inlist,$cmd);
    my (@globlist,@errlog);

    $qmap   = "NONE" unless($qmap);
    $gti    = "NONE" unless($gti);
    $weighted = "YES" unless($weighted);
    $outunits = "RATE" unless($outunits);
    if ($tbinsize =~ m/^g/) {
	# Bin the data according to the GTI
	$timedel = 0;
	$tbinalg = "g";
    } else {
	# Uniform time binning
	$timedel = $tbinsize;
	$tbinalg = "u";
    }

    if ("$infiles" =~ m/^@/) { 
	@globlist = ("$infiles");   # Batch file
    } else {
	@globlist = glob("$infiles");
    }
    die "ERROR: no input files" if ($#globlist == -1);
    $inlist = join(",",@globlist);

    ## NOTE:  Do not change this command line without changing the 
    ##  task help example.
    $cmd = "batbinevt infile='$inlist' outfile=$lcfile outtype=LC timedel=$timedel timebinalg=$tbinalg energybins='$erange' " .
	"detmask='$qmap' weighted=$weighted outunits=$outunits ecol=ENERGY " .
	"gtifile='$gti' clobber=yes";

    unlink($lcfile);
    $? = BAT::log->callnote($log,$cmd,
	    "Creating $tbinsize second light curve for energies $erange");
    if ($?) {
	die "ERROR: batbinevt failed while accumulating $lcfile";
    }


    # Note: this case is not fatal, *IF* there were no overlapping
    # good times between the input file and the good time interval
    # file.  In that case batbinevt does not create an output file.

    if (! -f $lcfile) {
	warn "WARNING: batbinevt did not create light curve $lcfile (could be no good times)";
    }

}

sub makeall {
    my ($self,$info,$inputs,$outputs,$gtis,$ebins,$tbins,$weighted) = @_;
    my ($qmap,$n,$i,$ename,$ebin,$erange,$gti);
    my (@ebands,@erangel);

    # Quality map
    $qmap = $info->{qmap};

    $n = $#$inputs;
    foreach $i (0 .. $n) {

	# Good time interval
	$gti = $$gtis[$i];
	$weight = $$weighted[$i];
	$tbin = $$tbins[$i];
	$ebin = $$ebins[$i];

	BAT::spectra->make($$inputs[$i],$$outputs[$i],$ebin,$qmap,$gti,
			   $tbin, $weight);
    }

    return $info;
}


1;
