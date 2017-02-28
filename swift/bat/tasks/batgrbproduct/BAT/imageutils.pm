

package BAT::imageutils;
use BAT::log;

# General image utilities
#
# sky2pix - convert from sky RA/DEC to pixel coordinates
#
# This function computes the pixel position, given an RA/DEC position.
#
# $self - class name (ignored)
# $image - image file name
# $sky - reference to sky position, as in [$ra, $dec] (degrees)
#
# RETURNS: reference to pixel position, as in [$xpix, $ypix]
#
# EXCEPTIONS: * sky2xy FTOOLS task failed
#             * invalid coordinates
#
sub sky2pix {
    my ($self,$log,$image,$sky) = @_;
    my ($ra,$dec,$cmd,$valid,$xpix,$ypix);
    my @results;

    $ra = $$sky[0];  $dec = $$sky[1];

    $cmd = "sky2xy infile='$image' xsky='$ra' ysky='$dec'";
    $? = BAT::log->call($log, $cmd);
    if ($?) { die "ERROR: sky2xy failed"; }
    
    $valid = `pget sky2xy valid`;
    chomp($valid);
    die "ERROR: sky2xy coordinates were invalid" if ($valid =~ m/^[nN]/);

    $xpix = `pget sky2xy xpix`;
    $ypix = `pget sky2xy ypix`;
    chomp($xpix); chomp($ypix);

    return [$xpix, $ypix];
}

# readpix - read pixel value at a given position
#

# This function returns a pixel value, given a pixel position.
#
# $self - class name (ignored)
# $image - image file name
# $pos - reference to pixel position, as in [$xpix, $ypix]
#
# RETURNS: pixel value
#
# EXCEPTIONS: * fimgpar FTOOLS task failed
#             * invalid pixel value
#
sub readpix {
    my ($self,$log,$image,$pos) = @_;
    my ($i,$j,$cmd,$pcodestr,$undefstr);
    my @results;

    $i = int($$pos[0]); $j = int($$pos[1]);
    
    $cmd = "fimgpar fitsfile='$image' pixel='$i,$j'";
    $? = BAT::log->call($log,$cmd);
    if ($?) { die "ERROR: fimgpar failed"; }
    
    $pcodestr = `pget fimgpar outvalue`;
    $undefstr = `pget fimgpar undef`;
    chomp($pcodestr);  chomp($undefstr);

    die "ERROR: fimgpar value was invalid" if ($undefstr =~ m/^[yY]/);

    return $pcodestr + 0.0;
}

1;
