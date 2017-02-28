# $Source: /headas/headas/swift/gen/lib/perl/Astro/TLE.pm,v $
# $Revision: 1.1 $
# $Date: 2002/11/21 17:45:09 $
#
#	Conversion of Space Geodesy Project (SGP) routines from
#	Pascal / C to Perl
#
# $Log: TLE.pm,v $
# Revision 1.1  2002/11/21 17:45:09  rwiegand
# Perl modules for task objects, math and astronomy.
#
# Revision 1.2  2002/08/26 17:54:41  wiegand
# Added support for reading from file handle
#
# Revision 1.1  2002/08/23 16:19:19  wiegand
# Initial revision
#

use strict;

package Astro::TLE;

use Astro::Julian qw(:functions);
use Astro::SGP;


use constant EOF => 'eof';


sub new
{
	my ($class, %args) = @_;

	my $object = bless(\%args, $class);

	return $object;
}


sub isRaw     { return shift->{raw} }

sub epoch     { return shift->{epoch} }
sub xndt2o    { return shift->{xndt2o} }
sub xndd6o    { return shift->{xndd6o} }
sub bstar     { return shift->{bstar} }

sub xincl     { return shift->{xincl} }
sub xnodeo    { return shift->{xnodeo} }
sub eo        { return shift->{eo} }
sub omegao    { return shift->{omegao} }
sub xmo       { return shift->{xmo} }
sub xno       { return shift->{xno} }

sub deepSpace { return shift->{deepSpace} }


sub preprocess
{
	my ($tle, $sgp) = @_;

	if ($tle->isRaw) {
		$tle->{raw} = 0;
		$tle->{xnodeo} *= $sgp->de2ra;
		$tle->{omegao} *= $sgp->de2ra;
		$tle->{xmo} *= $sgp->de2ra;
		$tle->{xincl} *= $sgp->de2ra;
		my $temp = $sgp->twopi / $sgp->xmnpda / $sgp->xmnpda;
		$tle->{xno} *= $temp * $sgp->xmnpda;
		$tle->{xndt2o} *= $temp;
		$tle->{xndd6o} *= $temp / $sgp->xmnpda;
		$tle->{bstar} /= $sgp->ae;
	}

# Period > 225 minutes is deep space
	{
		my $dd1 = ($sgp->xke / $tle->xno);
		my $dd2 = $sgp->tothrd;
		my $a1 = $dd1 ** $dd2;
		my $r1 = cos($tle->xincl);
		$dd1 = (1.0 - $tle->eo * $tle->eo);
		my $temp = $sgp->ck2 * 1.5 * ($r1 * $r1 * 3.0 - 1.0) / ($dd1 ** 1.5);
		my $del1 = $temp / ($a1 * $a1);
		my $ao = $a1 * (1.0 - $del1 * ($sgp->tothrd * .5
        	+ $del1 * ($del1 * 134 / 81 + 1.0)));
		my $delo = $temp / ($ao * $ao);
		my $xnodp = $tle->xno / ($delo + 1.0);

# Select a deep-space/near-earth ephemeris
		$tle->{deepSpace} = ($sgp->twopi / $xnodp > 225);
	}
}


sub toEpoch
{
	my ($self, $epoch, %args) = @_;

	my $sgp = $args{sgp} || Astro::SGP->new;

	# (nearly) make a working copy
	my $tle = Astro::TLE->new(%{ $self });
	$tle->preprocess($sgp);

	my ($p, $v);

	my $j0 = Astro::Julian::ymdToJulian(@{ $tle->epoch });
	my $j1 = Astro::Julian::ymdToJulian(@{ $epoch });

	my $minutes = ($j1 - $j0) * $sgp->xmnpda;

	if ($tle->deepSpace) {
		die('TLE.toEpoch: elements are deep space, but SDP not implemented');
	}
	else {
		my $model = $args{model} || 'sgp4';

		($p, $v) = $sgp->$model($tle, $minutes);
	}

	return ($p, $v);
}


my %CHECKSUM_VALUE = (
	'0' => 0,
	'1' => 1,
	'2' => 2,
	'3' => 3,
	'4' => 4,
	'5' => 5,
	'6' => 6,
	'7' => 7,
	'8' => 8,
	'9' => 9,
	'-' => 1,
);

sub computeChecksum
{
	my ($s) = @_;

	my $sum = 0;
	for (my $i = 0; $i < length($s); ++$i) {
		my $c = substr($s, $i, 1);
		if (exists($CHECKSUM_VALUE{$c})) {
			$sum += $CHECKSUM_VALUE{$c};
		}
	}

	my $checksum = $sum % 10;

	return $checksum;
}


sub isValid
{
	my ($self) = @_;
	return not $self->{error};
}


sub badFormat
{
	my ($self, $problem) = @_;
	if (not $self->{error}) {
		$self->{error} = $problem;
		if ($problem eq EOF) {
			$self->{eof} = 1;
		}
	}
}


sub atEof
{
	my ($self) = @_;
	return $self->{eof};
}


sub raw
{
	my ($tle, $field) = @_;
	my $key = qq(raw:$field);
	return $tle->{$key};
}


sub get
{
	my ($tle, $in, %args) = @_;

	my $re = $args{re};

	my $s = substr($in, $args{o}, $args{l});
	if ($s !~ /^$args{re}$/) {
		$tle->badFormat("$s at offset $args{o} does not match $args{re}");
	}
	elsif ($args{n}) {
		$tle->{qq(raw:$args{n})} = $s;
	}
}


sub parseLine1
{
	my ($tle, $in) = @_;

	my $reSpace = qr( );

	# the implicit exponent format of the motion6d2t and bstar fields
	my $reExp = qr([-+ ]\d{5}[-+]\d);

	# type indicator, NORAD uses [1-5] internally
	my $reType = qr([0]);

	$tle->get($in, o =>  0, l =>  1, re => qr([1]));
	$tle->get($in, o =>  1, l =>  1, re => $reSpace);
	$tle->get($in, o =>  2, l =>  5, re => qr(\d+),  n => 'satellite1');
	$tle->get($in, o =>  7, l =>  1, re => qr([UC]), n => 'class');
	$tle->get($in, o =>  8, l =>  1, re => $reSpace);
	$tle->get($in, o =>  9, l =>  8, re => qr([ A-Z0-9]+), n => 'designator');
	$tle->get($in, o => 17, l =>  1, re => $reSpace);
	$tle->get($in, o => 18, l => 14, re => qr(\d{5}\.\d+), n => 'epoch');
	$tle->get($in, o => 32, l =>  1, re => $reSpace);
	$tle->get($in, o => 33, l => 10, re => qr([-+ ][.]\d+), n => 'motion2dt');
	$tle->get($in, o => 43, l =>  1, re => $reSpace);
	$tle->get($in, o => 44, l =>  8, re => $reExp, n => 'motion6d2t');
	$tle->get($in, o => 52, l =>  1, re => $reSpace);
	$tle->get($in, o => 53, l =>  8, re => $reExp, n => 'bstar');
	$tle->get($in, o => 61, l =>  1, re => $reSpace);
	$tle->get($in, o => 62, l =>  1, re => $reType, n => 'type');
	$tle->get($in, o => 63, l =>  1, re => $reSpace);
	$tle->get($in, o => 64, l =>  4, re => qr( *\d*), n => 'element');
	$tle->get($in, o => 68, l =>  1, re => qr(\d), n => 'checksum1');

	if ($tle->raw('checksum1') != computeChecksum(substr($in, 0, -1))) {
		$tle->badFormat('line 1 checksum mismatch');
	}

	return $tle->{error};
}


sub parseLine2
{
	my ($tle, $in) = @_;

	my $reSpace = qr( );
	my $reAngle = qr( *\d*\.\d{4});
	my $reRevs  = qr( *\d*\.\d{8});

	$tle->get($in, o =>  0, l =>  1, re => qr([2]));
	$tle->get($in, o =>  1, l =>  1, re => $reSpace);
	$tle->get($in, o =>  2, l =>  5, re => qr(\d+),  n => 'satellite2');
	$tle->get($in, o =>  7, l =>  1, re => $reSpace);
	$tle->get($in, o =>  8, l =>  8, re => $reAngle, n => 'inclination');
	$tle->get($in, o => 16, l =>  1, re => $reSpace);
	$tle->get($in, o => 17, l =>  8, re => $reAngle, n => 'raan');
	$tle->get($in, o => 25, l =>  1, re => $reSpace);
	$tle->get($in, o => 26, l =>  7, re => qr(\d+), n => 'eccentricity');
	$tle->get($in, o => 33, l =>  1, re => $reSpace);
	$tle->get($in, o => 34, l =>  8, re => $reAngle, n => 'argperigee');
	$tle->get($in, o => 42, l =>  1, re => $reSpace);
	$tle->get($in, o => 43, l =>  8, re => $reAngle, n => 'meananomaly');
	$tle->get($in, o => 51, l =>  1, re => $reSpace);
	$tle->get($in, o => 52, l => 11, re => $reRevs, n => 'meanmotion');
	$tle->get($in, o => 63, l =>  5, re => qr([ ]*\d+), n => 'revolution');
	$tle->get($in, o => 68, l =>  1, re => qr(\d), n => 'checksum2');

	if ($tle->raw('checksum2') != computeChecksum(substr($in, 0, -1))) {
		$tle->badFormat('line 2 checksum mismatch');
	}

	return $tle->{error};
}



sub load
{
	my ($fh, %args) = @_;

	my $error = undef;

	my ($line0, $line1, $line2) = ();

	if ($args{lines} > 2) {
		$line0 = <$fh>;
		$line1 = <$fh> if defined($line0);
		$line2 = <$fh> if defined($line1);
	}
	else {
		$line1 = <$fh>;
		$line2 = <$fh> if defined($line1);
	}

	foreach ($line0, $line1, $line2) {
		$_ =~ s/\s+$//g if defined;
	}

	my $tle = __PACKAGE__->new(
		'raw:line0' => $line0,
		'raw:line1' => $line1,
		'raw:line2' => $line2,
	);

	if (not defined($line2)) {
		$error = EOF;
	}
	elsif (defined($line0) and (length($line0) < 1)) {
		# not sure what constraints can be applied
	}
	elsif ($tle->parseLine1($line1)) {
		$error = "bad line 1 $line1";
	}
	elsif ($tle->parseLine2($line2)) {
		$error = "bad line 2 $line2";
	}
	else {
		# TLE has been parsed
	}

	if ($error) {
		$tle->badFormat($error);
	}
	else {
		$tle->cook;
	}

	return $tle;
}


# given a string aaaaase where each a and e is in [0-9], s in [+-]
# convert to real number 0.aaaaa * 10^se
sub ietn
{
	my ($s) = @_;
	my $sgn = (substr($s, 0, 1) eq '-') ? -1 : 1;
	my $man = substr($s, 1, -2);
	my $exp = substr($s, -2);
	my $n = $sgn * ("0.$man" . "E$exp");
	return $n;
}


# given an TLE angle field which might contains leading blanks, convert
# to real number
sub asdn
{
	my ($s) = @_;
	$s =~ s/^\s+//;
	return $s;
}


sub cook
{
	my ($tle) = @_;

	# convert raw strings to numerics

	my $epoch = $tle->raw('epoch');
	my $yy = int($epoch / 1000);
	my $year = ($yy < 57) ? (2000 + $yy) : (1900 + $yy);
	my $doy = $epoch % 1000;
	my $fraction = substr($epoch, 5);

	my $jd = ydToJulian($year, $doy, $fraction);
	my (undef, $month, $day) = julianToYMD($jd);

	$tle->{epoch} = [ $year, $month, $day, $fraction ];

	$tle->{xndt2o}   = asdn($tle->raw('motion2dt'));
	$tle->{xndd6o}   = ietn($tle->raw('motion6d2t'));
	$tle->{bstar}    = ietn($tle->raw('bstar'));

	$tle->{xincl}    = asdn($tle->raw('inclination'));
	$tle->{xnodeo}   = asdn($tle->raw('raan'));
	$tle->{eo}       = '0.' . $tle->raw('eccentricity');
	$tle->{omegao}   = asdn($tle->raw('argperigee'));
	$tle->{xmo}      = asdn($tle->raw('meananomaly'));
	$tle->{xno}      = $tle->raw('meanmotion');

}


1;

