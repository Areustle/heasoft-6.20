
# Compute BAT sky images and partial coding maps
#
# Steps 85 and 90
#
# This module computes sky images from detector plane images.
#
# sky - create sky map or partial coding map
#
# $self - class name (ignored)
# $info - generic information hash reference
#         INPUTS: $info->{attitude} - attitude file name
#                 $info->{image_corrections} - batfftimage 'corrections' param
#                 $info->{teldef} - teldef file to use
#                 $info->{aperture} - aperture file to use
#                 $info->{qmap} - quality map
#                 $info->{bat_z} - BAT image parameter
#                 $info->{origin_z} - BAT image parameter
#                 $info->{pcodethresh} - partial coding threshold
# $dpifile - input detector plane image file name
# $imgfile - output sky image file name
# $bkgfile - input background image file name (defaults to "NONE")
# $pcodemap - "YES"/"NO" for partial coding map (defaults to "NO")
#
# RETURNS: revised $info
#
# EXCEPTIONS: * failed to create output
#
package BAT::image;
use BAT::log;

sub sky {
    my ($self,$info,$log,$dpifile,$imgfile,$bkgfile,$pcodemap) = @_;
    my ($status,$cmd,$attitude,$corrections,$teldef);
    my ($aperture,$qmap,$bat_z,$origin_z,$pcodethresh);
    my (@errlog);

    # Default values
    $bkgfile = "NONE" unless($bkgfile);
    $pcodemap = "NO" unless($pcodemap);

    # Retrieve auxiliary values
    $attitude    = $info->{attitude};
    $corrections = $info->{image_corrections};
    $teldef      = $info->{teldef};
    $aperture    = $info->{aperture};
    $qmap        = $info->{qmap};
    $bat_z       = $info->{bat_z};
    $origin_z    = $info->{origin_z};
    $pcodethresh = $info->{pcodethresh};

    unlink($imgfile);
    # XXX NOTE: clobber=NO is a workaround for a bug in the build 9
    # batfftimage task.  Since we unlink($imgfile) just above, this is
    # equivalent to clobber=yes
    $cmd = "batfftimage infile='$dpifile' outfile=$imgfile attitude='$attitude' bkgfile='$bkgfile' " .
	"bat_z=$bat_z origin_z=$origin_z teldef='$teldef' aperture='$aperture' " .
	"pcodethresh=$pcodethresh corrections=$corrections " . 
	"detmask='$qmap' clobber=no pcodemap=$pcodemap";
    $? = BAT::log->callnote($log,$cmd,
			    "Creating sky map from $dpifile");
    if ($?) {
	die "ERROR: batfftimage failed while computing sky image $imgfile";
    }

    return $info;
}

# sky - create sky map or partial coding map
#
# $self - class name (ignored)
# $info - generic information hash reference (see sky())
# $dpifile - input detector plane image file name
# $imgfile - output sky image file name
#
# RETURNS: revised $info
#
# EXCEPTIONS: * failed to create output
#
sub pcodemap {
    my ($self,$info,$log,$dpifile,$imgfile) = @_;

    return $self->sky($info,$log,$dpifile,$imgfile,"NONE","YES");
}


1;
