
# Determine the partial coding for a particular point on the sky, for
# a given pointing direction of the spacecraft.

package BAT::pcode;

use BAT::imageutils;
use BAT::log;

sub fromimage {
    my ($self,$log,$image,$skypos) = @_;
    my ($pixpos);

#    print "image=$image skypos=$$skypos[0] $$skypos[1]\n";
    $pixpos = BAT::imageutils->sky2pix($log,$image,$skypos);
    $pcode  = BAT::imageutils->readpix($log,$image,$pixpos);

    return $pcode;
}

1;
