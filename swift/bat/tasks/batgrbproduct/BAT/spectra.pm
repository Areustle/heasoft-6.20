
package BAT::spectra;
use Astro::FITS::CFITSIO qw(:longnames :constants);
use BAT::log;

# Compute mask-weighted spectra
#
# Step 130
#
# This module accumulates spectra from the input files.
#
# $self - class name (ignored)
# $infiles - input file wildcard spec, or @batch file
# $lcfile - output file name
# $erange - energy bin edges (e.g. "CALDB:80" for 80-bin spectrum)
# $qmap - detector quality map (defaults to "NONE")
# $gti - good time interval (defaults to "NONE")
# $outunits - output units: "COUNTS" or "RATE" (defaults to "COUNTS")
#
# RETURNS: nothing
#
# EXCEPTIONS: * no matching input files
#             * batbinevt failed
# WARNING: * failed to create output file
#

sub make {
    my ($self,$log,$infiles,$phafile,$erange,$qmap,$gti,$opts) = @_;
    my ($status,$inlist,$cmd);
    my (@globlist,@errlog);

    $qmap   = "NONE" unless($qmap);
    $gti    = "NONE" unless($gti);
    if ($opts->{outunits}) { $outunits = $opts->{outunits}; }
    else                   { $outunits = "COUNTS"; } 
    
    if ("$infiles" =~ m/^@/) { 
	@globlist = ("$infiles");   # Batch file
    } else {
	@globlist = glob("$infiles");
    }
    die "ERROR: no input files" if ($#globlist == -1);
    $inlist = join(",",@globlist);

    ## NOTE:  Do not change this command line without changing the 
    ##  task help example.
    $cmd = "batbinevt infile='$inlist' outfile='$phafile' outtype=PHA timedel=0 timebinalg=u energybins='$erange' detmask='$qmap' ".
	"ecol=ENERGY weighted=YES outunits='$outunits' gtifile='$gti' clobber=yes";

    unlink($phafile);
    $? = BAT::log->callnote($log,$cmd,
      "Accumulating spectrum for energy bins $erange and time interval $gti");
    if ($?) {
	die "ERROR: batbinevt failed while accumulating spectrum $phafile";
    }

    # Note: this case is not fatal, *IF* there were no overlapping
    # good times between the input file and the good time interval
    # file.  In that case batbinevt does not create an output file.

    if (! -f $phafile) {
	warn "WARNING: batbinevt did not create spectrum $phafile (could be no good times)";
    } else {
	# BUG workaround - remove EXPOSURE keyword, which clashes with
	# the EXPOSURE column
	# print "   (removing erroneous EXPOSURE keyword)\n";

	# Attempt to remove the keyword regardless of whether it
	# exists.
	SimpleFITS->open("<$phafile")->move("1")
	    ->delkey("EXPOSURE")
	    ->close();

	# Other modifications to the spectrum file:
	#   1. add proper ray-tracing keywords
	#   2. add proper systematic error vector
	if (-f $opts->{auxfile}) {
	    $auxfile = $opts->{auxfile};
	    $cmd = "batupdatephakw infile='$phafile' auxfile='$auxfile'";
	    $? = BAT::log->callnote($log,$cmd,
				"Updating spectral keywords for $phafile");
	}
	$cmd = "batphasyserr infile='$phafile' syserrfile=CALDB";
	$? = BAT::log->callnote($log,$cmd,
				"Updating systematic error for $phafile");
    }
	    

}

sub makeall {
    my ($self,$info,$inputs,$outputs,$gtis,$ebins,$opts) = @_;
    my ($qmap,$n,$i,$ename,$ebin,$erange,$gti);
    my (@ebands,@erangel);

    # Quality map
    $qmap = $info->{qmap};

    $n = $#$inputs;
    foreach $i (0 .. $n) {

	# Good time interval
	$gti = $$gtis[$i];

#	print "infile=$$inputs[$i] outfile=$$outputs[$i] ebins=$ebins qmap=$qmap gti=$gti\n";
	BAT::spectra->make($$inputs[$i],$$outputs[$i],$ebins,$qmap,$gti,$opts);
    }

    return $info;
}


1;
