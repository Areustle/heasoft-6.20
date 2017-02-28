# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/Convert.pm,v $
# $Revision: 1.10 $
# $Date: 2008/10/24 21:11:01 $
#
#	Convert UVOT coordinates between SKY, DET, RAW coordinates
#
#	The attitude coordinator tool may be invoked in the future
#	to ensure the calculations are consistent with the TELDEF and
#	to include distortion map affects.
#
# $Log: Convert.pm,v $
# Revision 1.10  2008/10/24 21:11:01  rwiegand
# Do not require teldef to be a file.
#
# Revision 1.9  2004/10/17 15:55:35  rwiegand
# Allow caller to pass in alignment matrix.
#
# Revision 1.8  2004/09/23 19:46:52  rwiegand
# Use coord/coordfits routines to convert Swift RA/DEC/ROLL to quaternions.
#
# Revision 1.7  2004/09/18 13:28:32  rwiegand
# Added routines for converting Swift RA, DEC, ROLL to quaternions and
# creating attitude history file.
#
# Revision 1.7  2004/09/18 12:56:29  rwiegand
# Support for converting Swift RA, DEC, ROLL to quaternions and creating
# and attitude history file.
#
# Revision 1.6  2004/06/17 21:15:29  rwiegand
# Allow user to specify MET for use in querying attitude history.
#
# Revision 1.5  2004/05/03 21:56:19  rwiegand
# Confirm that applyxform output matches input.  Override RAW CDELT/CUNIT
# values to be pixels.
#
# Revision 1.4  2004/04/30 14:58:41  rwiegand
# Use new attitude tool applyxform.
#
# Revision 1.3  2004/01/05 16:49:45  rwiegand
# Modified the calculation of SKY positions from DET coordinates.
#
# Revision 1.2  2003/12/15 22:03:37  rwiegand
# Added conversion from SKY to DET coordinates
#
# Revision 1.1  2003/11/26 20:00:19  rwiegand
# Modules for sharing among UVOT tasks.
#
# Revision 1.1  2003/11/07 16:17:19  wiegand
# Initial revision
#

use strict;

package UVOT::Convert;
use base qw(UVOT::Subtask);

use Task qw(:codes);
use Math;
use FileHandle;

use Astro::FITS::CFITSIO qw(:constants :longnames);



sub setSystem
{
	my ($self, $tag, $system) = @_;

	$self->{$tag} = $system;

	my $pix1 = $tag . 'Pix1';
	my $pix2 = $tag . 'Pix2';

	$self->{$pix1} = $system . 'X'
		if not $self->{$pix1};
	$self->{$pix2} = $system . 'Y'
		if not $self->{$pix2};

	my $world1 = $tag . 'World1';
	my $world2 = $tag . 'World2';

	$self->{$world1} = $system . '1'
		if not $self->{$world1};
	$self->{$world2} = $system . '2'
		if not $self->{$world2};
}


sub initialize
{
	my ($self, %args) = @_;

	if (not exists($args{from})) {
		$self->error(BAD_INPUT, "must specify from coordinate system");
	}

	if (not exists($args{to})) {
		$self->error(BAD_INPUT, "must specify to coordinate system");
	}

	$self->setSystem(from => $args{from});
	$self->setSystem(to => $args{to});

	foreach my $key (qw(teldef ra dec roll
				mjdref misstime segment aberration timemargin 
			)) {
		if ($args{$key}) {
			$self->{$key} = $args{$key};
		}
	}

	if (not exists($self->{teldef})) {
		$self->error(BAD_INPUT, "must specify teldef file");
	}
	elsif (not $self->{teldef}) {
		$self->error(BAD_INPUT, "invalid teldef file '$self->{teldef}'");
	}

	$self->{attfile} = 'none';
	if (exists($self->{ra})
			and exists($self->{dec})
			and exists($self->{roll})) {
		my $path = $self->temporary('attfile');
		$self->constantAttitudeFile($path,
				$self->{ra}, $self->{dec}, $self->{roll});
		$self->{attfile} = $path;
	}	

	$self->{mjdref} ||= 51910;
	$self->{aberration} ||= 'no';
	$self->{timemargin} ||= 32;
	$self->{segment} ||= 0;

	my $fromKey1 = $args{fromWorld} ? $self->{fromWorld1} : $self->{fromPix1};
	my $fromKey2 = $args{fromWorld} ? $self->{fromWorld2} : $self->{fromPix2};

	if ($args{objects}) {
		$self->{objects} = $args{objects};
	}
	elsif ($args{points}) {
		$self->{objects} = map {
				{ $fromKey1 => $_->[0], $fromKey2 => $_->[1], }
			} @{ $args{points} };
	}
	elsif ($args{x} and $args{y}) {
		my $n = Math::min(scalar(@{$args{x}}), scalar(@{$args{y}}));
		$self->{objects} = map {
				{ $fromKey1 => $args{x}[$_], $fromKey2 => $args{y}[$_] }
			} 0 .. $n - 1;
	}
	else {
		$self->error(BAD_INPUT, "objects not provided");
	}

	$self->applyToObjects(sub {
			my ($o) = @_;
			$o->{BAD} ||= 0;
		});
}


sub applyToObjects
{
	my ($self, $method) = @_;

	foreach my $o (@{ $self->{objects} }) {
		$method->($o);
	}
}


sub convertRawToDetector
{
	my ($self, %args) = @_;
	$self->convert(%args, from => 'RAW', to => 'DET');
	return $self->{objects};
}


sub convertRawToSky
{
	my ($self, %args) = @_;
	$self->convert(%args, from => 'RAW', to => 'SKY');
	return $self->{objects};
}


sub convertDetectorToRaw
{
	my ($self, %args) = @_;
	$self->convert(%args, from => 'DET', to => 'RAW');
	return $self->{objects};
}


sub convertDetectorToSky
{
	my ($self, %args) = @_;
	$self->convert(%args, from => 'DET', to => 'SKY');
	return $self->{objects};
}


sub convertSkyToDetector
{
	my ($self, %args) = @_;
	$self->convert(%args, from => 'SKY', to => 'DET');
	return $self->{objects};
}


sub convertSkyToRaw
{
	my ($self, %args) = @_;
	$self->convert(%args, from => 'SKY', to => 'RAW');
	return $self->{objects};
}


sub convert
{
	my ($self, %args) = @_;

	$self->initialize(%args);

	$self->adjustFromWorld
		if $self->isValid and $args{fromWorld};

	$self->getTransform
		if $self->isValid;

	$self->applyTransform
		if $self->isValid;

	$self->adjustToWorld
		if $self->isValid and $args{toWorld};
}


sub getTransform
{
	my ($self) = @_;

	if ($self->{from} eq 'SKY' or $self->{to} eq 'SKY') {

		if (not -f $self->{attfile}) {
			$self->error(BAD_INPUT,
				"invalid attitude file '$self->{attfile}' for SKY transform");
			return;
		}
	}

	$self->{xform} = $self->temporary('xform');

	my $getxform = $self->buildCommand('getxform',
			teldef => $self->{teldef},
			from => $self->{from},
			to => $self->{to},

			image => 'no',
			segment => $self->{segment},
			ra => $self->{ra},
			dec => $self->{dec},
			attfile => $self->{attfile},

			mjdref => $self->{mjdref},
			time => $self->{misstime},
			aberration => $self->{aberration},
			timemargin => $self->{timemargin},

			outfile => $self->{xform},
			);

	$self->runCommand($getxform);
}


sub applyTransform
{
	my ($self) = @_;

	if (not $self->{xform}) {
		$self->error(BAD_INPUT, "no transform set");
	}
	elsif (not -f $self->{xform}) {
		$self->error(BAD_INPUT,
			"missing transform file '$self->{xform}'");
	}

	return if not $self->isValid;

	my $in = $self->temporary('in', ext => '.tmp');
	my $out = $self->temporary('out', ext => '.tmp');

	my $fh = FileHandle->new($in, 'w');
	if (not $fh) {
		$self->error(BAD_OUTPUT, "unable to create $in [$!]");
		return;
	}

	$self->applyToObjects(sub {
			my ($o) = @_;
			$fh->print("$o->{$self->{fromPix1}} $o->{$self->{fromPix2}}\n");
		});

	$fh->close;

	my $applyxform = $self->buildCommand('applyxform',
			infile => $in,
			transform => $self->{xform},
			outfile => $out,
			);

	$self->runCommand($applyxform);
	return if not $self->isValid;

	$fh = FileHandle->new($out, 'r');
	if (not $fh) {
		$self->error(BAD_OUTPUT, "unable to open $out [$!]");
		return;
	}

	$self->applyToObjects(sub {
			my ($o) = @_;
			my $line = <$fh>;
			# could also verify that $from1, $from2 match $s...
			my ($from1, $from2, $to1, $to2) = split(/\s+/, $line);
			if (abs($from1 - $o->{$self->{fromPix1}}) > 0.01
					or abs($from2 - $o->{$self->{fromPix2}}) > 0.01) {
				die("from mismatch");
			}
			$o->{$self->{toPix1}} = $to1;
			$o->{$self->{toPix2}} = $to2;
		});

	$fh->close;
}


sub getWCS
{
	my ($self, $system) = @_;

	my $outfile = $self->temporary('wcs');

	# get WCS keywords (by running getwcs, then loading)
	my $getwcs = $self->buildCommand('getwcs',
			teldef => $self->{teldef},
			coord => $system,
			segment => $self->{segment},
			ra => $self->{ra},
			dec => $self->{dec},
			outfile => $outfile,
			);

	$self->runCommand($getwcs);
	return if not $self->isValid;

	my $status = 0;
	my $fits = Astro::FITS::CFITSIO::open_file($outfile, READONLY, $status);
	if ($status) {
		$self->error(BAD_EXECUTE, "unable to open $outfile [$status]");
		return;
	}

	my @keys = qw(CRVAL1 CRVAL2 CRPIX1 CRPIX2 CDELT1 CDELT2 CROTA2 TYPE);
	my %wcs = map { $_ => '' } @keys;

	$fits->read_img_coord(@wcs{@keys}, $status);

	if (not $wcs{TYPE}) {
		my $ctype1;
		my $comment;
		$fits->read_key_str('CTYPE1', $ctype1, $comment, $status);
		if ($ctype1 =~ /^(RAW|DET)[XY]$/) {
			$wcs{TYPE} = '-CAR';
		}
	}

	if ($system eq 'RAW') {
		$self->warning("overriding WCS keywords for RAW coordinates");
		$wcs{CRVAL1} = $wcs{CRPIX1};
		$wcs{CRVAL2} = $wcs{CRPIX2};
		$wcs{CDELT1} = 1;
		$wcs{CDELT2} = 1;
	}

	{
		my $tmp = 0;
		$fits->close_file($tmp);
	}

	if ($status) {
		$self->error(BAD_INPUT, "unable to read WCS keywords [$status]");
		return;
	}

	return \%wcs;
}


sub adjustFromWorld
{
	my ($self) = @_;

	my $wcs = $self->getWCS($self->{from});

	return if not $wcs;

	$self->applyToObjects(sub {
			my ($o) = @_;

			my ($status, $xpix, $ypix) = (0);

			Astro::FITS::CFITSIO::fits_world_to_pix(
					$o->{$self->{fromWorld1}}, $o->{$self->{fromWorld2}},
					$wcs->{CRVAL1}, $wcs->{CRVAL2},
					$wcs->{CRPIX1}, $wcs->{CRPIX2},
					$wcs->{CDELT1}, $wcs->{CDELT2},
					$wcs->{CROTA2}, $wcs->{TYPE},
					$xpix, $ypix,
					$status,
					);

			if ($status) {
				$self->error(BAD_EXECUTE,
						"unable to convert to pixel coordinates [$status]");
			}
			else {
				$o->{$self->{fromPix1}} = $xpix;
				$o->{$self->{fromPix2}} = $ypix;
			}
		});
}


sub adjustToWorld
{
	my ($self) = @_;

	my $wcs = $self->getWCS($self->{to});

	return if not $wcs;

	$self->applyToObjects(sub {
			my ($o) = @_;

			my ($status, $world1, $world2) = (0);

			Astro::FITS::CFITSIO::fits_pix_to_world(
					$o->{$self->{toPix1}}, $o->{$self->{toPix2}},
					$wcs->{CRVAL1}, $wcs->{CRVAL2},
					$wcs->{CRPIX1}, $wcs->{CRPIX2},
					$wcs->{CDELT1}, $wcs->{CDELT2},
					$wcs->{CROTA2}, $wcs->{TYPE},
					$world1, $world2,
					$status,
					);

			if ($status) {
				$self->error(BAD_EXECUTE,
						"unable to convert to world coordinates [$status]");
			}
			else {
				$o->{$self->{toWorld1}} = $world1;
				$o->{$self->{toWorld2}} = $world2;
			}
		});
}


sub rmToQuat
# excerpted from coord/coordfits
{
	my ($rm) = @_;

	my $m00 = $rm->[0][0];
	my $m01 = $rm->[0][1];
	my $m02 = $rm->[0][2];

	my $m10 = $rm->[1][0];
	my $m11 = $rm->[1][1];
	my $m12 = $rm->[1][2];

	my $m20 = $rm->[2][0];
	my $m21 = $rm->[2][1];
	my $m22 = $rm->[2][2];

	my $maxi = 0;
	my $best = 1 + $m00 - $m11 - $m22;

	my $test = 1 - $m00 + $m11 - $m22;
	if ($best < $test) {
		$best = $test;
		$maxi = 1;
	}

	$test = 1 - $m00 - $m11 + $m22;
	if ($best < $test) {
		$best = $test;
		$maxi = 2;
	}

	$test = 1 + $m00 + $m11 + $m22;
	if ($best < $test) {
		$best = $test;
		$maxi = 3;
	}

	my $qmaxi = sqrt($best) / 2;
	my $recip = 1 / (4 * $qmaxi);

	my $q = undef;

	if ($maxi == 0) {
		$q = [
			$qmaxi,
			$recip * ($m01 + $m10),
			$recip * ($m20 + $m02),
			$recip * ($m12 - $m21),
		];
	}
	elsif ($maxi == 1) {
		$q = [
			$recip * ($m01 + $m10),
			$qmaxi,
			$recip * ($m12 + $m21),
			$recip * ($m20 - $m02),
		];
	}
	elsif ($maxi == 2) {
		$q = [
			$recip * ($m20 + $m02),
			$recip * ($m12 + $m21),
			$qmaxi,
			$recip * ($m01 - $m10),
		];
	}
	elsif ($maxi == 3) {
		$q = [
			$recip * ($m12 - $m21),
			$recip * ($m20 - $m02),
			$recip * ($m01 - $m10),
			$qmaxi,
		];
	}

	return $q;
}


sub eulerToRM
# excerpted from coord/coordfits
{
	my ($phi, $theta, $psi) = @_;

	my $sinphi = sin($phi);
	my $cosphi = cos($phi);

	my $sintheta = sin($theta);
	my $costheta = cos($theta);

	my $sinpsi = sin($psi);
	my $cospsi = cos($psi);


	my $rm = [
		[
			$cospsi * $costheta * $cosphi - $sinpsi * $sinphi,
			$cospsi * $costheta * $sinphi + $sinpsi * $cosphi,
			-$cospsi * $sintheta
		],
		[
			-$sinpsi * $costheta * $cosphi - $cospsi * $sinphi,
			-$sinpsi * $costheta * $sinphi + $cospsi * $cosphi,
			$sinpsi * $sintheta
		],
		[
			$sintheta * $cosphi,
			$sintheta * $sinphi,
			$costheta
		]
	];

	return $rm;
}



sub eulerToQuat
# excerpted from coord/coordfits
{
	my ($phi, $theta, $psi) = @_;

	my $rm = eulerToRM($phi, $theta, $psi);

	my $q = rmToQuat($rm);

	return $q;
}



sub productOfQuats
# excerpted from coord/coordfits
{
	my ($q1, $q2) = @_;

	my $q10 = $q1->[0];
	my $q11 = $q1->[1];
	my $q12 = $q1->[2];
	my $q13 = $q1->[3];

	my $q20 = $q2->[0];
	my $q21 = $q2->[1];
	my $q22 = $q2->[2];
	my $q23 = $q2->[3];

	my $q = [
		 $q23 * $q10 + $q22 * $q11 - $q21 * $q12 + $q20 * $q13,
		-$q22 * $q10 + $q23 * $q11 + $q20 * $q12 + $q21 * $q13,
		 $q21 * $q10 - $q20 * $q11 + $q23 * $q12 + $q22 * $q13,
		-$q20 * $q10 - $q21 * $q11 - $q22 * $q12 + $q23 * $q13,
	];

	return $q;
}



sub swiftRaDecRollDegreesToQuaternion
{
	my ($ra, $dec, $roll, %args) = @_;

	# alignment matrix is transpose of ALIGNM*
	my $align = $args{align} || [
		[ 0, 0, 1 ],
		[ 1, 0, 0 ],
		[ 0, 1, 0 ],
	];

	my $qAlign = rmToQuat($align);

	my @z = ($ra, 90 - $dec, -$roll + 90);
	my @angles = map { Math::degreesToRadians($_) } @z;

	my $q = eulerToQuat(@angles);
	$q = productOfQuats($q, $qAlign);

	return $q;
}



sub constantAttitudeFile
{
	my ($self, $path, $ra, $dec, $roll) = @_;

	my $q = swiftRaDecRollDegreesToQuaternion($ra, $dec, $roll,
			align => $self->{align});

	my $status = 0;
	my $fits = Astro::FITS::CFITSIO::create_file($path, $status);
	if ($status) {
		$self->error(BAD_OUTPUT,
				"unable to create $path [$status]");
		return;
	}

	if ($fits->create_tbl(BINARY_TBL, 0, 0, 0, 0, 0, 'ATTITUDE', $status)) {
		$self->error(BAD_OUTPUT,
				"unable to create attitude table [$status]");
	}

	my @columns = (
		{ name => 'TIME',
			form => 'D',
			comment => 'Spacecraft clock time',
			unit => 's',
		},
		{ name => 'QPARAM',
			form => '4D',
			comment => 'Quaternion of pointing',
		},
		{ name => 'POINTING',
			form => '3D',
			comment => 'RA, Dec, Roll from telemetry',
			unit => 'deg',
		}
	);

	my %columns = map { $_->{name} => $_ } @columns;
	$columns{TIME}{data} = [ 0, 1e12 ];
	$columns{QPARAM}{data} = [ @$q, @$q ];
	$columns{POINTING}{data} = [ ($ra, $dec, $roll) x 2 ];

	foreach my $c (@columns) {
		$self->getTask->writeColumn($fits, $c, $c->{data});
	}

	if ($fits) {
		my $tmp = 0;
		$fits->close_file($tmp);
	}

	return $status;
}


sub testResult
{
	my ($tag, $t, $q) = @_;
	my $left = join(' ', map { sprintf('%3d', $_) } @$t);
	my $right = join(' ', map { sprintf('%+.4f', $_) } @$q);
	print "$tag: $left => $right\n";
}


sub testRADecRollToQuaternion
{
	my @tests = (
		[ 0, 0, 0 ],
		[ 90, 0, 0 ],
		[ 0, 90, 0 ],
		[ 0, 45, 0 ],
		[ 45, 45, 0 ],
		[ 0, 0, 20 ],
		[ 90, 0, 20 ],
	);

	my @order = qw(
		123
		132
		213
		231
		312
		321
	);


	my @f = (undef, \&rm1, \&rm2, \&rm3);

	my $align = [
		[ 0, 1, 0 ],
		[ 0, 0, 1 ],
		[ 1, 0, 0 ],
	];
	my $qAlign = rmToQuat($align);


	foreach my $t (@tests) {

		# my @z = ($t->[0], -$t->[1], -$t->[2]);
		my @z = ($t->[0], 90 - $t->[1], -$t->[2] + 90);
		my @angles = map { Math::degreesToRadians($_) } @z;

		if (undef) {
			my $q = swiftRaDecRollDegreesToQuaternion(@$t);
			testResult('old', $t, $q);
		}

		if (undef) {
			my $q = eulerToQuat(@angles);
			$q = productOfQuats($q, $qAlign);
			testResult('new', $t, $q);
		}

		if (1) {
			my $q = swiftRaDecRollDegreesToQuaternion(@$t);
			testResult('fix', $t, $q);
		}

		next;

		foreach my $o (@order) {

			my $rm = [ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1] ];

			for (my $i = 0; $i < 3; ++$i) {
				my $c = substr($o, $i, 1);
				$rm = rmMult($f[$c]->($angles[$i]), $rm);
			}

			my $q = rmToQuat($rm);

			testResult($o, $t, $q);
		}
	}
}


1;

