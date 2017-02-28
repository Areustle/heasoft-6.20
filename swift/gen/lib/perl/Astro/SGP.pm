
# $Source: /headas/headas/swift/gen/lib/perl/Astro/SGP.pm,v $
# $Revision: 1.1 $
# $Date: 2002/08/23 16:19:01 $
#
#	Conversion of Space Geodesy Project (SGP) routines from
#	Pascal / C to Perl
#
#	Comment from 
#	C source code derived mostly from NORADS's spacetrack report #3
#	FORTRAN routines and Dr. T. Kelso's Pascal ports of same. This
#	file contains SGP, SGP4, SGP8, SDP4 and SDP8 satellite ephemeris
#	routines for the prediction of a satellites position and velocity
#	from its two-line element (tle) set of orbital parameters.
#
# $Log: SGP.pm,v $
# Revision 1.1  2002/08/23 16:19:01  wiegand
# Initial revision
#

use strict;

package Astro::SGP;

use constant PI => 4 * atan2(1, 1);
use constant PIx2 => 2 * PI;



sub addDefault (\%$$)
{
	my ($href, $key, $value) = @_;

	if (not exists($href->{$key})) {
		$href->{$key} = $value;
	}
}


sub modulus ($$)
{
	my ($num, $den) = @_;
use POSIX;
	my $modulus = $num - POSIX::floor($num / $den);
	return $modulus;
}


sub new
{
	my ($class, %args) = @_;

	addDefault(%args, pi => PI);
	addDefault(%args, twopi => 2 * $args{pi});
	addDefault(%args, pio2 => $args{pi} / 2);
	addDefault(%args, ae => 1);
	addDefault(%args, de2ra => $args{pi} / 180);
	addDefault(%args, tothrd => 2 / 3);
	addDefault(%args, xmnpda => 1440);

# earth equatorial radius - kolometers (WGS '72)
	addDefault(%args, xkmper => 6378.135);

# earth flattening (WGS '72)
	addDefault(%args, f => 1 / 298.26);

# earth gravitational constant (WGS '72)
	addDefault(%args, ge => 398600.8);

# J2 harmonic (WGS '72)
	addDefault(%args, J2 => 1.0826158e-3);
# J3 harmonic (WGS '72)
	addDefault(%args, J3 => -2.53881e-6);
# J4 harmonic (WGS '72)
	addDefault(%args, J4 => -1.65597e-6);

	addDefault(%args, ck2 => $args{J2} / 2);
	addDefault(%args, ck4 => -3 * $args{J4} / 8);
	addDefault(%args, xj3 => $args{J3});

	addDefault(%args, qo => $args{ae} + 120 / $args{xkmper});
	addDefault(%args, s => $args{ae} + 78 / $args{xkmper});
	addDefault(%args, e6a => 1e-6);

	addDefault(%args, xke => sqrt(3600 * $args{ge} / ($args{xkmper} ** 3)));
	addDefault(%args, qoms2t => ($args{qo} - $args{s}) ** 4);

	my $object = bless(\%args, $class);

	return $object;
}


sub pi       { return shift->{pi} }
sub twopi    { return shift->{twopi} }
sub pio2     { return shift->{pio2} }
sub ae       { return shift->{ae} }
sub de2ra    { return shift->{de2ra} }
sub tothrd   { return shift->{tothrd} }
sub xmnpda   { return shift->{xmnpda} }

sub xkmper   { return shift->{xkmper} }
sub f        { return shift->{f} }
sub ge       { return shift->{ge} }
# sub xj2      { return shift->{xj2} }
sub xj3      { return shift->{xj3} }
# sub xj4      { return shift->{xj4} }

sub ck2      { return shift->{ck2} }
sub ck4      { return shift->{ck4} }
sub qo       { return shift->{qo} }
sub so       { return shift->{so} }
sub s        { return shift->{s} }
sub e6a      { return shift->{e6a} }

sub qoms2t   { return shift->{qoms2t} }
sub xke      { return shift->{xke} }


sub sgp4
{
	my ($sgp, $tle, $minutes, %args) = @_;

	my $sgp4 = $args{sgp4} || SGP4->new($sgp, $tle, %args);
	my $tsince = $minutes;

# Update for secular gravity and atmospheric drag.
	my $xmdf = $tle->xmo + $sgp4->xmdot * $tsince;
	my $omgadf = $tle->omegao + $sgp4->omgdot * $tsince;
	my $xnoddf = $tle->xnodeo + $sgp4->xnodot * $tsince;
	my $omega = $omgadf;
	my $xmp = $xmdf;
	my $tsq = $tsince * $tsince;
	my $xnode = $xnoddf + $sgp4->xnodcf * $tsq;
	my $tempa = 1 - $sgp4->c1 * $tsince;
	my $tempe = $tle->bstar * $sgp4->c4 * $tsince;
	my $templ = $sgp4->t2cof * $tsq;

	if (not $sgp4->simple) {
		my $delomg = $sgp4->omgcof * $tsince;
		my $delm = $sgp4->xmcof
				* (((1 + $sgp4->eta * cos($xmdf)) ** 3) - $sgp4->delmo);
		my $temp = $delomg + $delm;
		my $xmp = $xmdf + $temp;
		$omega = $omgadf - $temp;
		my $tcube = $tsq * $tsince;
		my $tfour = $tsince * $tcube;
		$tempa = $tempa - $sgp4->d2 * $tsq
				- $sgp4->d3 * $tcube - $sgp4->d4 * $tfour;
		$tempe = $tempe + $tle->bstar * $sgp4->c5 * (sin($xmp) - $sgp4->sinmo);
		$templ = $templ + $sgp4->t3cof * $tcube
				+ $tfour * ($sgp4->t4cof + $tsince * $sgp4->t5cof);
	}

	my $a = $sgp4->aodp * ($tempa ** 2);
	my $e = $tle->eo - $tempe;
	my $xl = $xmp + $omega + $xnode + $sgp4->xnodp * $templ;
	my $beta = sqrt(1 - $e * $e);
	my $xn = $sgp->xke / ($a ** 1.5);

# Long period periodics
	my $axn = $e * cos($omega);
	my $temp = 1 / ($a * $beta * $beta);
	my $xll = $temp * $sgp4->xlcof * $axn;
	my $aynl = $temp * $sgp4->aycof;
	my $xlt = $xl + $xll;
	my $ayn = $e * sin($omega) + $aynl;

# Solve Kepler's' Equation
	my $capu = modulus($xlt - $xnode, PIx2);
	my $temp2 = $capu;

	my ($temp3, $temp4, $temp5, $temp6, $epw);
	my ($cosepw, $sinepw);

	for (my $i = 0; $i < 10; ++$i) {
		$sinepw = sin($temp2);
		$cosepw = cos($temp2);
		$temp3 = $axn * $sinepw;
		$temp4 = $ayn * $cosepw;
		$temp5 = $axn * $cosepw;
		$temp6 = $ayn * $sinepw;
		$epw = ($capu - $temp4 + $temp3 - $temp2)
				/ (1 - $temp5 - $temp6) + $temp2;
		last if abs($epw - $temp2) <= $sgp->e6a;
		$temp2 = $epw;
	}

# Short period preliminary quantities
	my $ecose = $temp5 + $temp6;
	my $esine = $temp3 - $temp4;
	my $elsq = $axn * $axn + $ayn * $ayn;
	$temp = 1 - $elsq;
	my $pl = $a * $temp;
	my $r = $a * (1 - $ecose);
	my $temp1 = 1 / $r;
	my $rdot = $sgp->xke * sqrt($a) * $esine * $temp1;
	my $rfdot = $sgp->xke * sqrt($pl) * $temp1;
	$temp2 = $a * $temp1;
	my $betal = sqrt($temp);
	$temp3 = 1 / (1 + $betal);
	my $cosu = $temp2 * ($cosepw - $axn + $ayn * $esine * $temp3);
	my $sinu = $temp2 * ($sinepw - $ayn - $axn * $esine * $temp3);
	my $u = atan2($sinu, $cosu);
	my $sin2u = 2 * $sinu * $cosu;
	my $cos2u = 2 * $cosu * $cosu - 1;
	$temp = 1 / $pl;
	$temp1 = $sgp->ck2 * $temp;
	$temp2 = $temp1 * $temp;

# Update for short periodics
	my $rk = $r * (1 - 1.5 * $temp2 * $betal * $sgp4->x3thm1)
			+ 0.5 * $temp1 * $sgp4->x1mth2 * $cos2u;
	my $uk = $u - 0.25 * $temp2 * $sgp4->x7thm1 * $sin2u;
	my $xnodek = $xnode + 1.5 * $temp2 * $sgp4->cosio * $sin2u;
	my $xinck = $tle->xincl
			+ 1.5 * $temp2 * $sgp4->cosio * $sgp4->sinio * $cos2u;
	my $rdotk = $rdot - $xn * $temp1 * $sgp4->x1mth2 * $sin2u;
	my $rfdotk = $rfdot
			+ $xn * $temp1 * ($sgp4->x1mth2 * $cos2u + 1.5 * $sgp4->x3thm1);

# Orientation vectors
	my $sinuk = sin($uk);
	my $cosuk = cos($uk);
	my $sinik = sin($xinck);
	my $cosik = cos($xinck);
	my $sinnok = sin($xnodek);
	my $cosnok = cos($xnodek);
	my $xmx = -$sinnok * $cosik;
	my $xmy = $cosnok * $cosik;
	my $ux = $xmx * $sinuk + $cosnok * $cosuk;
	my $uy = $xmy * $sinuk + $sinnok * $cosuk;
	my $uz = $sinik * $sinuk;
	my $vx = $xmx * $cosuk - $cosnok * $sinuk;
	my $vy = $xmy * $cosuk - $sinnok * $sinuk;
	my $vz = $sinik * $cosuk;

	my $position = [
		$rk * $ux,
		$rk * $uy,
		$rk * $uz,
		$sgp->xkmper,
	];

	my $velocity = [
		$rdotk * $ux + $rfdotk * $vx,
		$rdotk * $uy + $rfdotk * $vy,
		$rdotk * $uz + $rfdotk * $vz,
		$sgp->xkmper / 60.,
	];

	return ($position, $velocity);
}



package SGP4;


sub new
{
	my ($class, $sgp, $tle, %args) = @_;

# Recover original mean motion (xnodp) and
# semimajor axis (aodp) from input elements.

	my $a1 = ($sgp->xke / $tle->xno) ** $sgp->tothrd;
	my $cosio = cos($tle->xincl);
	my $theta2 = $cosio * $cosio;
	my $x3thm1 = 3 * $theta2 - 1;
	my $eosq = $tle->eo * $tle->eo;
	my $betao2 = 1 - $eosq;
	my $betao = sqrt($betao2);
	my $del1 = 1.5 * $sgp->ck2 * $x3thm1 / ($a1 * $a1 * $betao * $betao2);
	my $ao = $a1 * (1 - $del1 * (0.5 * $sgp->tothrd
              + $del1 * (1 + 134 / 81 * $del1)));
	my $delo = 1.5 * $sgp->ck2 * $x3thm1 / ($ao * $ao * $betao * $betao2);
	my $xnodp = $tle->xno / (1 + $delo);
	my $aodp = $ao / (1 - $delo);

# For perigee less than 220 kilometers, the "simple" flag is set
# and the equations are truncated to linear variation in sqrt a
# and quadratic variation in mean anomaly.  Also, the c3 term,
# the delta omega term, and the delta m term are dropped.

	my $simple = (($aodp * (1 - $tle->eo) / $sgp->ae)
			< (220 / $sgp->xkmper + $sgp->ae));

# For perigee below 156 km, the
# values of s and qoms2t are altered.
	my $s4 = $sgp->s;
	my $qoms24 = $sgp->qoms2t;
	my $perige = ($aodp * (1 - $tle->eo) - $sgp->ae) * $sgp->xkmper;
	if ($perige < 156) {
		if ($perige <= 98) {
			$s4 = 20;
		}
		else {
			$s4 = $perige - 78;
		}

		$qoms24 = ((120 - $s4) * $sgp->ae / $sgp->xkmper) ** 4;
		$s4 = $s4 / $sgp->xkmper + $sgp->ae;
	}

	my $pinvsq = 1 / ($aodp * $aodp * $betao2 * $betao2);
	my $tsi = 1 / ($aodp - $s4);
	my $eta = $aodp * $tle->eo * $tsi;
	my $etasq = $eta * $eta;
	my $eeta = $tle->eo * $eta;
	my $psisq = abs(1 - $etasq);
	my $coef = $qoms24 * ($tsi ** 4);
	my $coef1 = $coef / ($psisq ** 3.5);
	my $c2 = $coef1 * $xnodp *
		($aodp * (1 + 1.5 * $etasq + $eeta * (4 + $etasq))
			+ 0.75 * $sgp->ck2 * $tsi / $psisq * $x3thm1
				* (8 + 3 * $etasq * (8 + $etasq)));
	my $c1 = $tle->bstar * $c2;
	my $sinio = sin($tle->xincl);
	my $a3ovk2 = -$sgp->xj3 / $sgp->ck2 * ($sgp->ae ** 3);
	my $c3 = $coef * $tsi * $a3ovk2 * $xnodp * $sgp->ae * $sinio / $tle->eo;
	my $x1mth2 = 1 - $theta2;
	my $c4 = 2 * $xnodp * $coef1 * $aodp * $betao2 *
		($eta * (2 + 0.5 * $etasq) + $tle->eo * (0.5 + 2 * $etasq)
			- 2 * $sgp->ck2 * $tsi / ($aodp * $psisq)
				* (-3 * $x3thm1 * (1 - 2 * $eeta + $etasq * (1.5 - .5 * $eeta))
					+ 0.75 * $x1mth2 * (2 * $etasq
						- $eeta * (1 + $etasq)) * cos(2 * $tle->omegao)));
	my $c5 = 2 * $coef1 * $aodp * $betao2
			* (1 + 2.75 * ($etasq + $eeta) + $eeta * $etasq);
	my $theta4 = $theta2 * $theta2;
	my $temp1 = 3 * $sgp->ck2 * $pinvsq * $xnodp;
	my $temp2 = $temp1 * $sgp->ck2 * $pinvsq;
	my $temp3 = 1.25 * $sgp->ck4 * $pinvsq * $pinvsq * $xnodp;
	my $xmdot = $xnodp + 0.5 * $temp1 * $betao * $x3thm1
				+ 0.0625 * $temp2 * $betao
					* (13 - 78 * $theta2 + 137 * $theta4);
	my $x1m5th = 1 - 5 * $theta2;
	my $omgdot = -0.5 * $temp1 * $x1m5th
               + 0.0625 * $temp2 * (7 - 114 * $theta2 + 395 * $theta4)
               + $temp3 * (3 - 36 * $theta2 + 49 * $theta4);
	my $xhdot1 = -$temp1 * $cosio;
	my $xnodot = $xhdot1 + (0.5 * $temp2 * (4 - 19 * $theta2)
                   + 2 * $temp3 * (3 - 7 * $theta2)) * $cosio;
	my $omgcof = $tle->bstar * $c3 * cos($tle->omegao);
	my $xmcof = -$sgp->tothrd * $coef * $tle->bstar * $sgp->ae / $eeta;
	my $xnodcf = 3.5 * $betao2 * $xhdot1 * $c1;
	my $t2cof = 1.5 * $c1;
	my $xlcof = 0.125 * $a3ovk2 * $sinio * (3 + 5 * $cosio) / (1 + $cosio);
	my $aycof = 0.25 * $a3ovk2 * $sinio;
	my $delmo = (1 + $eta * cos($tle->xmo)) ** 3;
	my $sinmo = sin($tle->xmo);
	my $x7thm1 = 7 * $theta2 - 1;

	my ($d2, $d3, $d4, $t3cof, $t4cof, $t5cof) = ();

	if (not $simple) {
	  my $c1sq = $c1 * $c1;
	  $d2 = 4 * $aodp * $tsi * $c1sq;
	  my $temp = $d2 * $tsi * $c1 / 3;
	  $d3 = (17 * $aodp + $s4) * $temp;
	  $d4 = 0.5 * $temp * $aodp * $tsi * (221 * $aodp + 31 * $s4) * $c1;
	  $t3cof = $d2 + 2 * $c1sq;
	  $t4cof = 0.25 * (3 * $d3 + $c1 * (12 * $d2 + 10 * $c1sq));
	  $t5cof = 0.2 * (3 * $d4 + 12 * $c1 * $d3 + 6 * $d2 * $d2
                  + 15 * $c1sq * (2 * $d2 + $c1sq));
	}

	my %data = (
		simple => $simple,
		aodp => $aodp,
		aycof => $aycof,
		c1 => $c1,
		c4 => $c4,
		c5 => $c5,
		cosio => $cosio,
		d2 => $d2,
		d3 => $d3,
		d4 => $d4,
		delmo => $delmo,
		eta => $eta,
		omgcof => $omgcof,
		omgdot => $omgdot,
		sinio => $sinio,
		xnodp => $xnodp,
		sinmo => $sinmo,
		t2cof => $t2cof,
		t3cof => $t3cof,
		t4cof => $t4cof,
		t5cof => $t5cof,
		x1mth2 => $x1mth2,
		x3thm1 => $x3thm1,
		x7thm1 => $x7thm1,
		xmcof => $xmcof,
		xmdot => $xmdot,
		xnodcf => $xnodcf,
		xnodot => $xnodot,
		xlcof => $xlcof,
	);

	my $object = bless(\%data, $class);

	return $object;
}


sub simple   { return shift->{simple} }
sub aodp     { return shift->{aodp} }
sub aycof    { return shift->{aycof} }
sub c1       { return shift->{c1} }
sub c4       { return shift->{c4} }
sub c5       { return shift->{c5} }
sub cosio    { return shift->{cosio} }
sub d2       { return shift->{d2} }
sub d3       { return shift->{d3} }
sub d4       { return shift->{d4} }
sub delmo    { return shift->{delmo} }
sub eta      { return shift->{eta} }
sub omgcof   { return shift->{omgcof} }
sub omgdot   { return shift->{omgdot} }
sub sinio    { return shift->{sinio} }
sub xnodp    { return shift->{xnodp} }
sub sinmo    { return shift->{sinmo} }
sub t2cof    { return shift->{t2cof} }
sub t3cof    { return shift->{t3cof} }
sub t4cof    { return shift->{t4cof} }
sub t5cof    { return shift->{t5cof} }
sub x1mth2   { return shift->{x1mth2} }
sub x3thm1   { return shift->{x3thm1} }
sub x7thm1   { return shift->{x7thm1} }
sub xmcof    { return shift->{xmcof} }
sub xmdot    { return shift->{xmdot} }
sub xnodcf   { return shift->{xnodcf} }
sub xnodot   { return shift->{xnodot} }
sub xlcof    { return shift->{xlcof} }


1;

