
# image_params - determine BAT imaging parameters from the telemetry
#
# Step 40
#
# The purpose of the script is to determine the source position and
# BAT focal length for ground testing.
#
# In flight, the sources will all be at infinity, so all X-rays will
# arrive as parallel rays.  On the ground, however, this is not true.
# Several of the BAT software tasks have parameters to compensate for
# near-field sources.  The principle value is the source *height* or
# bat_z value. 
#
# During ground testing, this value may be entered into the BAT flight
# software (may, but not must!), which then gets reported in the BBR
# task telemetry.  In order to derive images in focus on the ground,
# this value must be set.
#
# In flight, bat_z must be set to 0.
#
# The other value is the focal length, which describes the offset of
# the mask.  For the purposes of comparing to the flight software, the
# focal length value must be input to the origin_z parameter of the
# batfftimage/batmaskwtimg/batmaskwtevt tasks.  This places the origin
# of coordinates at the top of the mask, which is where the flight
# software measures from.  
#
# This task scans for BAT housekeeping files, opens the BBR task
# engineering housekeeping extension, and extracts the relevant
# quantities.  If there are multiple files or values, it takes the
# last one, presuming this is the most up-to-date.
#
# C. Markwardt

package BAT::image_params;

use Astro::FITS::CFITSIO qw(:longnames :constants);
use SimpleFITS;

sub set {
    my ($self,$info,$path,$ext) = @_;
    my ($fits,$filename,$status,$nrows,$invdist,$flength,$bat_z);

    # These are the flight parameters, no reference to the telemetry.
    # In flight, the source is at infinity, and none of this matters.

    # Default values
    $info->{bat_z} = 0.0;
    $info->{invdist} = 0.0;
    $info->{flength} = 0.0;
    $info->{origin_z} = 0.0;

    if (! $info->{ground_test}) { return $info; }

    unless ($ext) { $ext = "hk161x001"; }

    # Scan for BAT engineering housekeeping file
    @filenames = glob($path);
    foreach $filename (@filenames) {
	$status = 0;
	next unless ($fits = SimpleFITS->open("<$filename"));

	$status = $fits->move("$ext")->status();
	
	# Read number of rows
	$status = $fits->readkey("NAXIS2",$nrows)->status() unless $status;
	
	# Read final rows of INVDIST and FLENGTH columns
	$status = $fits->readcol("BBRIIMGINVDIST",{type=>TDOUBLE,rows=>$nrows},$invdist)
	               ->readcol("BBRIIMGFLENGTH",{type=>TDOUBLE,rows=>$nrows},$flength)
		       ->status() unless $status;

	# Compute bat_z
	if (! $status && $$invdist[0] > 0 && $$flength[0] > 0) {
	    $bat_z = 100.0 * (1.0 / $$invdist[0] + $$flength[0]) + 0.35;
	}
	
	
	$fits->close();
    }
    
    # Default is to set to zero, meaning source at infinity
    unless ($bat_z) { 
	return $info;
    }

    $info->{bat_z}   = $bat_z;
    $info->{invdist} = $$invdist[0];
    $info->{flength} = $$flength[0];
    $info->{origin_z}= $$flength[0] * 100.0 + 0.35;

    return $info;
}

1;
