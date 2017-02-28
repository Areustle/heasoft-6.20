
package BAT::config;

# Configurable parameters
#
# Step 10
#
# This module sets basic BAT processing parameters.
#
# $self - class name (ignored)
# $info - generic information hash reference.  See below for output
#         parameters.
#
# RETURNS: revised $info
#
# MODIFICATIONS:
#   28 Sep 2004 CM
#     Add $peakint parameter to config hash
#

sub set {
    my ($self,$info) = @_;

    # Ground test flag.
    #   1: assume we are in the near-field limit
    #   0: flight light configuration (far-field)
    $info->{ground_test} = 1;
    
    # Position refinement enablement flag
    #   1: enable position refinement (if other conditions are met)
    #   0: disable position refinement
    $info->{pos_refinement_enable} = 1;

    # Energy band information
    #   Total energy band == One band for 1-channel maps
    $info->{"1chan"}     = "15-350";         # keV

    # Individual energy bands
    #   Listing of the energy bands to process for 4-channel maps
    $info->{"4chan"}     = "15-25,25-50,50-100,100-350"; # keV

    # Energy bands for spectra
    # Drawn from BAT CALDB area (80-channel EBOUNDS extension)
    $info->{"80chan"}    = "CALDB:80";


    # Partial coding threshold.  Minimum detector plane exposure
    # threshold.
    $info->{pcodethresh} = 0.05;  # Fraction of 1.0


    # BAT estimated error radius
    #   * used in position refinement  (~4 arcmin to start)
    $info->{err_rad} = 4.0/60.0; # deg

    # BAT corrections to apply
    #   ... to mask weight calculations
    $info->{maskwt_corrections} = "flatfield,ndets,pcode,maskwt";

    #   ... to images
    $info->{image_corrections} = "autocollim,flatfield,ndets,pcode,maskwt";


    # Lookback time for battblocks, used speed up the analysis.
    $info->{tlookback} = 10.0;
    # Interval for peak flux calculation
    $info->{peakint}   = 1.0;  # [seconds]
    
    # Peak XX second flux interval, is selectable, 1.0 seconds here
    $info->{tpeak} = 1.0;

    # Determine default behavior of tasks (note: not all scripts obey this)
    $info->{chatter} = 3;
    $info->{clobber} = "yes";

    # Full width of postage stamp images (pixels)
    $info->{postage_width} = 100;
    

    return $info;
}

1;
