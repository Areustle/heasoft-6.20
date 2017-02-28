# $Source: /headas/headas/attitude/tasks/tristarid/StarID/SquareRADec.pm,v $
# $Revision: 1.2 $
# $Date: 2005/10/31 13:56:10 $
#
# $Log: SquareRADec.pm,v $
# Revision 1.2  2005/10/31 13:56:10  rwiegand
# Renamed the catalog partition paramater to catspec.  This removes the
# need for a dummy directory when the actual data is not even local.
#
# Revision 1.1  2005/10/07 20:42:32  rwiegand
# Source catalog modules.
#
# Revision 1.1  2004/06/17 21:23:28  rwiegand
# The code for loading a partitioned star catalog was modularized from the
# uvotstarid since several tasks need it.  The Partition and SquareRADec
# modules were relocated here with some refactoring and a new Partition
# subclass was added for dealing with the hierarchical triangular mesh
# partitioning used by the Guide Star Catalog folks.
#

package StarID::SquareRADec;
use base(StarID::Partition);

use POSIX;

use Math;

my $DEBUG = 0;


# divide the sky into 2*order ra by order dec regions
# coordinate system rotated by pi/2 near poles- for abs(phi) > phiCritical

sub initialize
{
	my ($self) = @_;

	$self->{order} ||= 36;
	$self->{phiCritical} ||= Math::PI / 4;

# we don't actually need plates to implement findPlate...
# for preprocessing the star catalog, we want to be able to enumerate the
# adjacent plates to avoid iterating over all of them.  An alternative
# approach it to perturb the star position by +/-overlap in ra/dec and add
# the star to each unique plate found

	$self->{workPlate} = StarID::Partition::Plate->new;

	$self->{plateAngle} = Math::PI / $self->{order};

	if (defined($self->{overlapAngle})) {
		my $angle = $self->{overlapAngle};
my $ratio = $angle / $self->{plateAngle};
print "overlap ratio is $ratio\n" if $DEBUG;
		if (($angle < 0) or ($angle > $self->{plateAngle} / 2)) {
			die("[buildPlates] invalid overlap angle $angle");
		}
	}
	elsif (defined($self->{overlapRatio})) {
		my $ratio = $self->{overlapRatio};
		if (($ratio < 0) or ($ratio > 0.5)) {
			die("[buildPlates] invalid overlap ratio $ratio");
		}
		$self->{overlapAngle} = $self->{plateAngle} * $ratio;
print "overlap angle is $self->{overlapAngle}\n" if $DEBUG;
	}
	else {
		$self->{overlapAngle} = 0;
	}
}


sub plateAngle
{
	return shift->{plateAngle};
}


sub overlapAngle
{
	return shift->{overlapAngle};
}


sub phiCritical
{
	return shift->{phiCritical};
}


sub normalizeRA
{
	my ($alpha) = @_;
	my $ra = POSIX::fmod($alpha + Math::PIx2, Math::PIx2);
	return $ra;
}


sub getPlate
{
	my ($self, $spec) = @_;

	my $plate = $self->{workPlate};

	my $ra = $spec->ra;
	my $dec = $spec->dec;

	my $raPrime;
	my $decPrime;

	if (abs($dec) > $self->phiCritical) {
		# rotate coords
		my $alpha = atan2(sin($dec), sin($ra) * cos($dec));
		$raPrime = normalizeRA($alpha);
		$decPrime = POSIX::asin(cos($ra) * cos($dec));
		$plate->{rotated} = 1;
	}
	else {
		$raPrime = $ra;
		$decPrime = $dec;
		$plate->{rotated} = 0;
	}

	$plate->{raIndex} = POSIX::floor($raPrime / $self->{plateAngle});
	$plate->{decIndex} = POSIX::floor(($decPrime + Math::PIo2)
			/ $self->{plateAngle});

	$plate->{path} = $self->key($plate);

	return $plate;
}


sub findNear
{
	my ($self, $spec, $e) = @_;

	my $tweaked = Partition::FindArgs->new;
	my @found;
	my %found;

	foreach my $delta (
			[   0,   0 ],
			[ -$e, -$e ],
			[ -$e,   0 ],
			[ -$e, +$e ],
			[   0, +$e ],
			[ +$e, +$e ],
			[ +$e,   0 ],
			[ +$e, -$e ],
			[   0, -$e ],
			) {

		$tweaked->{ra} = normalizeRA($spec->ra + $delta->[0]);
		$tweaked->{dec} = $spec->dec + $delta->[1];

		my $p = $self->findPlate($tweaked);

		my @id = ($p->{rotated}, $p->{raIndex}, $p->{decIndex});
		my $name = $self->key(\@id);

		if (not $found{$name}) {
			$found{$name} = 1;
			push(@found, \@id);
		}
	}

# post-process to ensure nothing skipped?  or depend on caller
# for reasonable input?

	return @found;
}


sub findPossibleOverlaps
{
	my ($self, $spec) = @_;

	my $e = $self->overlapAngle;
	if ($e > $self->plateAngle / 2) {
		die("[findPossibleOverlaps] too much overlap");
	}

	my @plates = $self->findNear($spec, $e);

	return @plates;
}


sub key
{
	my ($self, $p) = @_;

	my ($ra, $dec, $rot);

	if (UNIVERSAL::isa($p, 'ARRAY')) {
		$rot  = $p->[0];
		$ra   = $p->[1];
		$dec  = $p->[2];
	}
	elsif (UNIVERSAL::isa($p, 'HASH')) {
		$rot  = $p->{rotated};
		$ra   = $p->{raIndex};
		$dec  = $p->{decIndex};
	}
	else {
		die("SquareRADec.key: wants HASH or ARRAY");
	}

	# since RA has a larger range than DEC, we attach the rotation
	# bit to DEC
	# whether to use subdirectories should be configurable
	my $key = "a$ra/r${rot}d$dec";

	return $key;
}


sub writeSummary
{
	my ($self, $fh) = @_;

	$self->SUPER::writeSummary($fh);

	$self->writeSummaryKeys($fh, qw(order overlapAngle phiCritical));
}


1;

