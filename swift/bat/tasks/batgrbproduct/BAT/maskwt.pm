
package BAT::maskwt;
use BAT::log;

# Compute mask weighting for the desired source
#
# Step 60
#
# This module computes the mask weighting for a source of interest,
# given its position in sky coordinates.
#
# $self - class name (ignored)
# $info - generic information hash reference
#         INPUTS: $info->{attitude} - attitude file name
#                 $info->{maskwt_corrections} - batmaskwtevt 'corrections' param
#                 $info->{teldef} - teldef file to use
#                 $info->{aperture} - aperture file to use
#                 $info->{qmap} - quality map
#                 $info->{bat_z} - BAT image parameter
#                 $info->{origin_z} - BAT image parameter
# $infiles - input event file wildcard spec, or @batch file
# $ra - source right ascension (deg)
# $dec - source declination (deg)
#
# RETURNS: revised $info
#
# EXCEPTIONS: * batmaskwtevt failed
#
# MODIFICATIONS:
#   02 Sep 2004 CM
#     Debugging changes
#

sub event {
    my ($self, $info, $log, $infiles, $ra, $dec, $auxfile) = @_;
    my ($attitude, $corrections, $teldef, $aperture, $qmap);
    my ($cmd,$pcodethresh);
    my (@infilelist, @errlog);
    
    # Extract the input parameters
    $attitude    = $info->{attitude};
    $corrections = $info->{maskwt_corrections};
    $teldef      = $info->{teldef};
    $aperture    = $info->{aperture};
    $qmap        = $info->{qmap};
    $bat_z       = $info->{bat_z};
    $origin_z    = $info->{origin_z};
    $auxfile     = "NONE" unless ($auxfile);
    if (defined($info->{evt_pcodethresh})) {
	$pcodethresh = $info->{evt_pcodethresh};
    } else {
	$pcodethresh = 0.0;
    }

    # Step through each input file in the glob pattern
#    print "infiles='$infiles'\n";
    @infilelist = glob($infiles);
#    print "infile='$infile'\n";
    foreach $infile (@infilelist) {
	print "     file $infile...\n";

	unlink ($auxfile) if (-f $auxfile);
	$cmd = "batmaskwtevt infile='$infile' attitude='$attitude' ra='$ra' dec='$dec' " .
	    "bat_z=$bat_z aperture='$aperture' detmask='$qmap' " .
	    "coord_type=sky rebalance=YES corrections='$corrections' " .
	    "teldef='$teldef' origin_z=$origin_z auxfile='$auxfile' " .
	    "pcodethresh=$pcodethresh clobber=yes";
	$? = BAT::log->callnote($log,$cmd,
				"Performing mask weighting calculation");
	if ($?) {
	    die "ERROR: batmaskwtevt failed (status = $?)";
	}
    }


    return $info;
}


1;
