
package BAT::dpi;
use BAT::log;

# Compute Detector plane images (DPIs)
#
# Step 80
#
# This module accumulates detector plane images from the input files.
# 
# $self - class name (ignored)
# $infiles - wildcard pattern matching input files (or @batch file)
# $dpifile - output detector plane image file
# $erange - input energy range (scalar string of the for "15-25,25-50")
# $qmap - detector quality map (defaults to "NONE")
# $gti - good time interval selection (defaults to "NONE" = no selection)
#
# RETURNS: nothing
#
# EXCEPTIONS: * no matching input files
#             * batbinevt failed
# WARNINGS: * no output file created
#
sub make {
    my ($self,$log,$infiles,$dpifile,$erange,$qmap,$gti) = @_;
    my ($status,$inlist,$cmd);
    my (@globlist,@errlog);

    $erange = "-" unless ($erange);
    $qmap   = "NONE" unless($qmap);
    $gti    = "NONE" unless($gti);
    
    if ("$infiles" =~ m/^@/) { 
	@globlist = ("$infiles");   # Batch file
    } else {
	@globlist = glob("$infiles");
    }
    die "ERROR: no input files" if ($#globlist == -1);
    $inlist = join(",",@globlist);

    unlink($dpifile);
    $cmd = "batbinevt infile='$inlist' outfile=$dpifile outtype=DPI timedel=0 timebinalg=u energybins=$erange detmask='$qmap' " .
	"ecol=ENERGY weighted=NO outunits=COUNTS gtifile='$gti' clobber=yes";
    $? = BAT::log->callnote($log,$cmd,
			    "Creating detector map for GTI=$gti");
    if ($?) {
	die "ERROR: batbinevt failed while estimating computing DPI $dpifile";
    }


    # Note: this case is not fatal, *IF* there were no overlapping
    # good times between the input file and the good time interval
    # file.  In that case batbinevt does not create an output file.

    if (! -f $dpifile) {
	warn "WARNING: batbinevt did not create DPI $dpifile (could be no good times)";
    }

}

sub makeall {
    my ($self,$info,$inputs,$outputs,$eranges,$gtis) = @_;
    my ($qmap,$n,$i,$ename,$ebin,$erange,$gti);
    my (@ebands,@erangel);

    # Quality map
    $qmap = $info->{qmap};

    $n = $#$inputs;
    foreach $i (0 .. $n) {

	# Determine which set of energy bands we will process with
	$erange = $$eranges[$i];
	$erange = "-" unless ($erange);

	# Good time interval
	$gti = $$gtis[$i];

	BAT::dpi->make($$inputs[$i],$$outputs[$i],$erange,$qmap,$gti);
    }

    return $info;
}


1;
