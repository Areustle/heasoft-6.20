
# $Source: /headas/headas/swift/gen/lib/perl/Astro/GTDS.pm,v $
# $Revision: 1.3 $
# $Date: 2002/08/19 20:13:26 $
#
#   Conversion of GTDS CELEM algorithm to perl
#
# $Log: GTDS.pm,v $
# Revision 1.3  2002/08/19 20:13:26  wiegand
# Store element rates with j2000 values.  Allow user to specify distance
# and angle units for elements.
#
# Revision 1.2  2002/08/19 19:52:31  wiegand
# Maintain angle, distance, time units with elements
#
##############################################################################

use strict;


##############################################################################

package Astro::Elements;


sub new
{
	my ($class, %args) = @_;

	my $object = bless(\%args, $class);

	return $object;
}


sub tag { return shift->{tag} }

sub distanceUnits { return shift->{distanceUnits} }
sub angleUnits { return shift->{angleUnits} }
sub rateUnits { return shift->{rateUnits} }


sub _convertAny
{
	my ($self, $unit, $to, @keys) = @_;
	my $scale = Astro::Constants::getScale($self->{$unit}, $to);
	foreach my $k (@keys) {
		$self->{$k} *= $scale;
	}
	$self->{$unit} = $to;
}


sub convertDistances
{
	my ($self, $units, @keys) = @_;
	$self->_convertAny(distanceUnits => $units, @keys);
}


sub convertAngles
{
	my ($self, $units, @keys) = @_;
	$self->_convertAny(angleUnits => $units, @keys);
}


sub convertRates
{
	my ($self, $units, @keys) = @_;
	$self->_convertAny(rateUnits => $units, @keys);
}


##############################################################################

package Astro::AlmanacElements;

use base qw(Astro::Elements);

use Astro::Constants qw(:names :units);


sub a { return shift->{a} }    # semimajor axis (distance)
sub e { return shift->{e} }    # eccentricity
sub i { return shift->{i} }    # inclination (angle)
sub O { return shift->{O} }    # longitude of ascending node (angle)
sub w { return shift->{w} }    # longitude of perihelion (angle)
sub L { return shift->{L} }    # mean longitude (angle)


# finds keplerian elements at an epoch given the epoch, base elements,
# and element rates

sub atEpoch
{
	my ($base, %args) = @_;

	my $rate = $args{rate};

	my $q = ($args{jd} - JD2000) / DAYS_IN_JULIAN_CENTURY;
	$rate->convertRates(DAY);

	# satisfy units
	$base->convertDistances($args{distanceUnits})
		if $args{distanceUnits};
	$base->convertAngles($args{angleUnits})
		if $args{angleUnits};

	# make units of rate consistent with base
	$rate->convertDistances($base->distanceUnits);
	$rate->convertAngles($base->angleUnits);

	my $out = Astro::AlmanacElements->new(
		a => $base->a + $rate->a * $q,
		e => $base->e + $rate->e * $q,
		i => $base->i + $rate->i * $q,
		O => $base->O + $rate->O * $q,
		w => $base->w + $rate->w * $q,
		L => $base->L + $rate->L * $q,
		distanceUnits => $base->distanceUnits,
		angleUnits => $base->angleUnits,
		);

	return $out;
}


sub convertDistances
{
	my ($self, $units) = @_;
	$self->SUPER::convertDistances($units, qw(a));
}

sub convertAngles
{
	my ($self, $units) = @_;
	$self->SUPER::convertAngles($units, qw(i O w L));
}

sub convertRates
{
	my ($self, $units) = @_;
	$self->SUPER::convertRates($units, qw(a e i O w L));
}


# these are the astronomical almanac planetary mean orbit values
	# base has a in AU, angles in deg
	# rate has a in AU/Cy, angles in arcsec per Cy

my %PLANET_ELEMENTS = (
	EARTH => {
		base => {
			a => 1.00000011,
			e => 0.01671022,
			i => 0.00005,
			O => -11.26064,
			w => 102.94719,
			L => 100.46435,

			distanceUnits => AU,
			angleUnits => DEGREES,
		},
		rate => {
			a => -0.00000005,
			e => -0.00003804,
			i => -46.94,
			O => -18228.25,
			w => 1198.28,
			L => 129597740.63,

			distanceUnits => AU,
			angleUnits => ARCSECS,
			rateUnits => CENTURY,
		},
	},
);

sub planetElements
{
	my %args = @_;

	my $body = uc($args{body});
	if (not $PLANET_ELEMENTS{$body}) {
		die("planetElements: invalid body '$body'");
	}

	my $base = Astro::AlmanacElements->new(
		%{ $PLANET_ELEMENTS{$body}{base} },
	);

	my $rate = Astro::AlmanacElements->new(
		%{ $PLANET_ELEMENTS{$body}{rate} },
	);

	my $elements = $base->atEpoch(jd => $args{jd}, rate => $rate,
		distanceUnits => $args{distanceUnits},
		angleUnits => $args{angleUnits},
		);

	$elements->{tag} = $args{body};

	return $elements;
}


##############################################################################

package Astro::GTDS::Elements;

use base qw(Astro::Elements);


sub a { return shift->{a} }    # semimajor axis (distance)
sub e { return shift->{e} }    # eccentricity
sub i { return shift->{i} }    # inclination (angle)
sub O { return shift->{O} }    # longitude of ascending node (angle)
sub p { return shift->{p} }    # argument of perigee (angle)
sub M { return shift->{M} }    # mean anomaly (angle)


sub convertDistances
{
	my ($self, $units) = @_;
	$self->SUPER::convertDistances($units, qw(a));
}

sub convertAngles
{
	my ($self, $units) = @_;
	$self->SUPER::convertAngles($units, qw(i O p M));
}

sub convertRates
{
	my ($self, $units) = @_;
	$self->SUPER::convertAngles($units, qw(a e i O p M));
}


##############################################################################

package Astro::GTDS;

use POSIX;

use Astro::Constants qw(:names :units);



sub convertAlmanacToGTDSElements
{
	my ($almanac) = @_;

	my $gtds = Astro::GTDS::Elements->new(
		tag => $almanac->tag,
		a => $almanac->a,
		e => $almanac->e,
		i => Math::degreesToRadians($almanac->i),
		O => Math::degreesToRadians($almanac->O),
		p => Math::degreesToRadians($almanac->w - $almanac->O),
		M => Math::degreesToRadians($almanac->L - $almanac->w),
		distanceUnits => $almanac->distanceUnits,
		angleUnits => $almanac->angleUnits,
	);

	return $gtds;
}


sub celem
{
	my %args = @_;

	my $kep = $args{elements};
	my ($cose, $sine, $r, $xo, $yo, $xod, $yod);

	if (($kep->a <= 0) && ($kep->e > 1)) {

		# hyperbolic orbit

		my ($e1, $e2, $dele, $temp);

		$e2 = Math::remainder($kep->M / $kep->e, Math::PIx2);
		for (my $iter = 0, $e1 = $e2 + 2 * $args{tolerance};
					(fabs($e1 - $e2) > $args{tolerance})
						&& ($iter < $args{maxIterations});
					++$iter) {
			$e1 = $e2;
			$dele = ($kep->M - $kep->e * POSIX::sinh($e1) + $e1)
					/ ($kep->e * POSIX::cosh($e1) - 1);
			$e2 = $e1 + $dele;
		}

		# eccentric anomaly computed, now get xo, yo, r
		$cose = POSIX::cosh($e2);
		$sine = POSIX::sinh($e2);
		$temp = $kep->e * $kep->e - 1;
		$xo = $kep->a * ($cose - $kep->e);
		$yo = -$kep->a * sqrt($temp) * $sine;
		$r = $kep->a * (1 - $kep->e * $cose);
		$xod = -sqrt(-$args{GMC} * $kep->a) * $sine / $r;
		$yod = sqrt(-$args{GMC} * $kep->a * $temp) * $cose / $r;
	}
	else {

		# elliptic orbit
		my ($e1, $e2, $f, $d, $temp);

		# first find eccentric anomaly via Newtons (Miles Standish version)

		$e2 = $kep->M;
		for (my $iter = 0, $e1 = $e2 + 2 * $args{tolerance};
				($iter < $args{maxIterations})
						&& (fabs($e1 - $e2) > $args{tolerance});
				++$iter) {
			$e1 = $e2;
			$f = $e1 - $kep->e * sin($e1) - $kep->M;
			$d = 1 - $kep->e * cos($e1 - $f / 2);
			$e2 = $e1 - $f / $d;
		}

		# have eccentric anomaly, now get xo, yo, r
		$cose = cos($e2);
		$sine = sin($e2);
		$temp = 1 - $kep->e * $kep->e;
		$xo = $kep->a * ($cose - $kep->e);
		$yo = $kep->a * sqrt($temp) * $sine;

		# compute magnitude of radius vector
		$r = $kep->a * (1 - $kep->e * $cose);
		$xod = -sqrt($args{GMC} * $kep->a) * $sine / $r;
		$yod = sqrt($args{GMC} * $kep->a * $temp) * $cose / $r;
	}

	my $coso	= cos($kep->p);
	my $sino	= sin($kep->p);
	my $cosom	= cos($kep->O);
	my $sinom	= sin($kep->O);
	my $cosi	= cos($kep->i);
	my $sini	= sin($kep->i);

	my $b11 = $coso * $cosom - $sino * $sinom * $cosi;
	my $b21 = $coso * $sinom + $sino * $cosom * $cosi;
	my $b31 = $sino * $sini;
	my $b12 = -$sino * $cosom - $coso * $sinom * $cosi;
	my $b22 = -$sino * $sinom + $coso * $cosom * $cosi;
	my $b32 = $coso * $sini;


	# now multiply B matrix by vectors for position, velocity

	my $position = [
		$b11 * $xo + $b12 * $yo,
		$b21 * $xo + $b22 * $yo,
		$b31 * $xo + $b32 * $yo,
	];

	my $velocity = [
		$b11 * $xod + $b12 * $yod,
		$b21 * $xod + $b22 * $yod,
		$b31 * $xod + $b32 * $yod,
	];

	return ($position, $velocity);
}



# add something nearly identical for earth orbiters?

sub planetPositionVelocity
{
	my %args = @_;

	my $jd = $args{jd};

	my $almanac = Astro::AlmanacElements::planetElements(
		body => $args{body},
		jd => $args{jd},
		);

	my $gtds = convertAlmanacToGTDSElements($almanac);
	$gtds->convertDistances($args{distanceUnits} || 'km');

	my $scale = Astro::Constants::getScale($gtds->distanceUnits, 'm');
	my $GMC = MASS_SUN_kg * G / ($scale ** 3);

	my ($pecliptic, $vecliptic) = celem(
		elements => $gtds,
		GMC => $GMC,
		tolerance => 1e-9,
		maxIterations => 10,
		);

	my ($position, $velocity, $type);

	if ($args{ecliptic}) {
		$type = 'ecliptic';
		$position = $pecliptic;
		$velocity = $vecliptic;
	}
	else {
		$type = 'j2000';

		my $coso = cos(EARTH_OBLIQUITY_J2000);
		my $sino = sin(EARTH_OBLIQUITY_J2000);

		my $pj2000 = [
			$pecliptic->[0],
			$pecliptic->[1] * $coso - $pecliptic->[2] * $sino,
			$pecliptic->[1] * $sino + $pecliptic->[2] * $coso,
		];

		my $vj2000 = [
			$vecliptic->[0],
			$vecliptic->[1] * $coso - $vecliptic->[2] * $sino,
			$vecliptic->[1] * $sino + $vecliptic->[2] * $coso,
		];

		$position = $pj2000;
		$velocity = $vj2000;
	}

	if ($args{verbose}) {
		displayVector("position: $type", $position);
		displayVector("velocity: $type", $velocity);
	}

	return ($position, $velocity);
}


1;

