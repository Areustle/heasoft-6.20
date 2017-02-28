
# $Source: /headas/headas/swift/gen/lib/perl/Astro/Constants.pm,v $
# $Revision: 1.3 $
# $Date: 2002/11/21 16:39:22 $
#
#   Conversion of GTDS CELEM algorithm to perl
#
# $Log: Constants.pm,v $
# Revision 1.3  2002/11/21 16:39:22  wiegand
# Added conversions from radians back into RA hours, minutes, seconds and
# DEC degrees, minutes, seconds.  Made conversions to radians take their
# overall polarity from the sign of the most significant argument.
#
# Revision 1.2  2002/09/30 18:35:05  wiegand
# Added conversions of RA (hours, minutes, seconds) and DEC (degrees, minutes,
# seconds) to radians.
#
# Revision 1.1  2002/08/19 19:51:55  wiegand
# Initial revision
#

use strict;

package Astro::Constants;

use base qw(Exporter);

@Astro::Constants::EXPORT_OK = qw(
	AU KM M
	RADIANS DEGREES ARCSECS
	CENTURY DAY

	JD2000 DAYS_IN_JULIAN_CENTURY

	G G_m3kg_1s_2 MASS_SUN_kg
	EARTH_OBLIQUITY_J2000

	M_PER_KM KM_PER_M M_PER_AU KM_PER_AU

	raHMStoRadians decDMStoRadians
);

%Astro::Constants::EXPORT_TAGS = (

	names => [ qw(M_PER_KM KM_PER_M M_PER_AU KM_PER_AU
		JD2000 DAYS_IN_JULIAN_CENTURY
		G G_m3kg_1s_2 MASS_SUN_kg
		EARTH_OBLIQUITY_J2000) ],

	units => [ qw(AU KM M RADIANS DEGREES ARCSECS CENTURY DAY) ],
);

use Math;

use constant G_m3kg_1s_2 => 6.672e-11; # gravitational constant m^3/kg/s^2 *
use constant G => G_m3kg_1s_2;

use constant MASS_SUN_kg => 1.9891e30;

use constant M_PER_AU => 1.4959787e11;
use constant KM_PER_AU => M_PER_AU / 1000;

use constant EARTH_OBLIQUITY_J2000 =>
	Math::degreesToRadians(23 + 26. / 60 + 21. / 3600);

use constant JD2000 => 2451545.0;
	# julian day of J2000 system (Jan 1.5 2000)

use constant DAYS_IN_JULIAN_CENTURY => 36525;


# distance units
use constant AU    => 'au';
use constant KM    => 'km';
use constant M     => 'm';

# angle units
use constant RADIANS => 'radians';
use constant DEGREES => 'degrees';
use constant ARCSECS => 'arcsecs';

# rate units
use constant CENTURY => 'century';  # julian century
use constant DAY     => 'day';


my %SCALE_FACTOR = (
	# angle conversions
	DEGREES_RADIANS => Math::PI / 180,
	RADIANS_DEGREES => 180 / Math::PI,
	ARCSECS_DEGREES => 1 / 3600,
	DEGREES_ARCSECS => 3600,

	# distance conversions
	AU_M => M_PER_AU,
	AU_KM => KM_PER_AU,
	M_AU => 1 / M_PER_AU,
	KM_AU => 1 / KM_PER_AU,
	KM_M => 1000,
	M_KM => 1e-3,

	# rate conversions
	CENTURY_DAY => 36525,
	DAY_CENTURY => 1 / 36525,
);


sub getScale
{
	my ($from, $to) = @_;
	my $key = uc(qq(${from}_${to}));
	my $scale = $SCALE_FACTOR{$key} || 1;
	return $scale;
}


sub raHMStoRadians
{
	my ($h, $m, $s) = @_;

	# convert to degrees
	my $tmp = 3600 * $h + 60 * $m + $s;
	my $degrees = $tmp / (60 * 4);

	return Math::degreesToRadians($degrees);
}


sub decDMStoRadians
{
	my ($d, $m, $s) = @_;

	my $neg = ($d < 0) ? -1 : 1;

	# convert to degrees
	my $degrees = $neg * ($neg * $d + $m / 60 + $s / 3600);

	return Math::degreesToRadians($degrees);
}


sub raRadiansToHMS
{
	my ($radians) = @_;

	my $norm = Math::remainder($radians, Math::PIx2);

	my $tmp = Math::radiansToDegrees($norm);

	$tmp /= 15;
	my $h = int($tmp);

	$tmp -= $h;
	$tmp *= 60;
	my $m = int($tmp);

	$tmp -= $m;
	$tmp *= 60;
	my $s = $tmp;

	return ($h, $m, $s);
}


sub decRadiansToDMS
{
	my ($radians) = @_;

	# normalize to [ -Math::PIo2, Math::PIo2 ]

	my $neg = ($radians < 0);

	my $degrees = Math::radiansToDegrees(abs($radians));

	my $tmp = $degrees;
	my $d = int($tmp);

	$tmp -= $d;
	$tmp *= 60;
	my $m = int($tmp);

	$tmp -= $m;
	$tmp *= 60;
	my $s = $tmp;

	$d *= -1 if $neg;

	return ($d, $m, $s);
}

1;

