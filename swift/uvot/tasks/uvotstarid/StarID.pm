# $Source: /headas/headas/swift/uvot/tasks/uvotstarid/StarID.pm,v $
# $Revision: 1.27 $
# $Date: 2005/03/21 14:05:12 $
#
# $Log: StarID.pm,v $
# Revision 1.27  2005/03/21 14:05:12  rwiegand
# Simplified an expression.
#
# Revision 1.26  2005/03/04 19:52:43  rwiegand
# Support for sorting catalog in memory.  Split parameter for maximum
# correction into a rotation limit and a translation limit.
#
# Revision 1.25  2004/06/17 21:33:50  rwiegand
# Updated parameter names for Martin.  Use new HEAdas perl task support.
# Partitioned catalog loading placed in UVOT/xxx modules.
#
# Revision 1.24  2004/05/10 17:53:40  rwiegand
# Raised chatter level for describing each thesis during analyzeThesis.
#
# Revision 1.23  2004/02/13 22:33:56  rwiegand
# Divided hypothesis processing into a source specific stage and a more
# in-depth analysis of those that pass the first filter.
#
# Revision 1.22  2003/11/26 20:38:38  rwiegand
# Renamed keys for consistency with UVOT perl libraries.
#
# Revision 1.21  2003/08/12 15:20:22  rwiegand
# Resolve star position from either unit vector or RA, DEC.  Form star
# identifier from all keys beginning with 'id'.
#
# Revision 1.20  2003/02/06 21:17:08  rwiegand
# Added support for byte swapping binary catalog during loading.
#
# Revision 1.19  2002/12/12 21:00:06  rwiegand
# Updated to use PIL parameter files (via pquery2).
#
# Revision 1.18  2002/11/25 22:18:39  rwiegand
# Only perform a Thesis cloud analysis when there are alternative doublets
# (which can be either unambiguous or ambiguous).
#
# Revision 1.17  2002/11/21 16:37:08  wiegand
# Made most logging chatter dependent.  Added another stage of comparing
# hypotheses, but disabled for now.
#
# Revision 1.16  2002/11/19 21:50:19  wiegand
# Run qMethod on each acceptable thesis.  More tightly constrain thesis
# cloud.  Compute loss associated with favorite acceptable theses.
#
# Revision 1.15  2002/11/18 19:47:56  wiegand
# Reworked doublet processing.  Develop for each source a score based on
# how many unambiguous and ambiguous doublets correspond to a particular
# (pivot) reference.  Rank these scores and pass highest contenders on
# to consistency checking round.
#
# Revision 1.13  2002/11/13 16:24:50  wiegand
# Perform hypothesis testing on match results
#
# Revision 1.12  2002/09/30 20:47:13  wiegand
# The _createMatch objects created from postprocessMatches were not
# pairs of observation and reference vectors.
#
# Revision 1.11  2002/09/30 18:31:01  wiegand
# Save more doublet meta-information to help determine ambiguities later.
#
# Revision 1.10  2002/08/19 13:01:56  wiegand
# Use Math::Eigen instead of shelling out to emote for eigenpairs
#
# Revision 1.9  2002/08/16 17:25:32  wiegand
# Implemented q method.  Shells out to emote to solve for eigenpairs.
#
# Revision 1.8  2002/08/12 18:32:23  wiegand
# Moved code for performing star identification by direct match followed
# by doublet match to subclass of StarID::Task.
#
# Revision 1.7  2002/07/25 13:26:13  wiegand
# Genericized loading of catalog
#
# Revision 1.6  2002/07/19 13:32:21  wiegand
# Allow user to specify doublet match setup routine.  Added methods for
# applying proper motion and velocity aberration.
#
# Revision 1.5  2002/06/27 20:23:24  wiegand
# Added skeleton for loading catalog
#
# Revision 1.4  2002/06/18 17:04:10  rwiegand
# Reorganized from starid.pl
#
# Revision 1.3  2002/06/18 15:31:24  rwiegand
# Reorganized StarID around tasks
#
# Revision 1.2  2002/06/17 22:51:29  rwiegand
# Added support for dodecahedra with sub-plates
#
# Revision 1.1  2002/06/17 22:23:27  rwiegand
# Initial revision


use strict;

use Math;


package StarID;


####################################################################

package StarID::Object;


sub new
{
	my ($class, %args) = @_;
	my $object = bless({ %args }, $class);
	return $object;
}


sub id
{
	return shift->{ID};
}


sub ra
{
	return shift->{RA};
}


sub dec
{
	return shift->{DEC};
}


sub mag
{
	return shift->{MAG};
}


sub rate
{
	return shift->{RATE};
}


sub unit
{
	return shift->{UNIT};
}


# sub angle ($self, $other)
#
# here
#	tN = thetaN = right ascension
#	oN = phiN = declination
#
# the formula
#	angle = acos(sin(o1)sin(o2) + cos(o1)cos(o2)cos(t1 - t2))
# is sensitive to rounding for small separations,
# but we can safely use Sinott's
#	angle = 2 * asin(sqrt(sin^2((o2 - o1)/2)
#				+ cos(o2)cos(o1)sin^2((t2-t1)/2)))
#
# given vectors v1 and v2
#	angle = acos(dot(v1, v2) / norm(v1) / norm(v2));
# for unit vectors u1 and u2
#	angle = acos(dot(u1, u2))

sub angle
{
	my ($self, $other) = @_;
	my $angle = Math::u3angle($self->unit, $other->unit);
	return $angle;
}


sub cosangle
{
	my ($self, $other) = @_;
	my $cosangle = Math::u3cosangle($self->unit, $other->unit);
	return $cosangle;
}


sub poserr
{
	return shift->{POS_ERR};
}


sub magerr
{
	return shift->{MAG_ERR};
}


sub type
{
	return shift->{TYPE};
}


####################################################################

package StarID::Source;

use base qw(StarID::Object);

sub directed
{
	return shift->{directed};
}


sub doublets
{
	return shift->{doublets};
}


####################################################################

package StarID::Catalog;


sub new
{
	my ($class, %args) = @_;

	my %data = (
		contents => $args{contents} || [ ],
	);

	my $object = bless(\%data, $class);

	return $object;
}


sub contents
{
	return shift->{contents};
}


sub size
{
	my $array = shift->contents;
	return scalar(@$array);
}


sub apply
{
	my ($self, $method) = @_;

	foreach my $o (@{ $self->contents }) {
		$method->($o);
	}
}


sub query
{
	my ($self, $query) = @_;

	my @result;

	foreach my $o (@{ $self->contents }) {
		if ($query->($o)) {
			push(@result, $o);
		}
	}

	my $subcatalog = Catalog->new(contents => \@result);

	return $subcatalog;
}


sub add
{
	my ($self, @objects) = @_;

	foreach my $o (@objects) {
		if (not UNIVERSAL::isa($o, 'StarID::Object')) {
			die(__PACKAGE__ . ": [add] invalid object $o");
		}
	}

	push(@{ $self->{contents} }, @objects);
}


####################################################################

package StarID::Catalog::Linear;
use base qw(StarID::Catalog);


####################################################################

package StarID::Catalog::Indexed;
use base qw(StarID::Catalog);

use Math;



sub new
{
	my ($first, %args) = @_;

	my $object = $first->SUPER::new(%args);

	my $p = $args{center};

	my $r = $p->{radius} || Math::degreesToRadians(1);
	my $n = $args{n} || 2;

	my $unit = Math::rd2unit($p->ra, $p->dec);

	my ($a, $c);
	if (abs($unit->[0]) < abs($unit->[2])) {
		my $q = $unit->[0] / $unit->[2];
		$a = sqrt(1 / (1 + $q * $q));
		$c = -$a * $q;
	}
	else {
		my $q = $unit->[2] / $unit->[0];
		$c = sqrt(1 / (1 + $q * $q));
		$a = -$c * $q;
	}

	$object->{unit} = $unit;

	my $rPrime = sin($r) / cos($r);
	$object->{k} = $rPrime / $n;
	$object->{q} = $n / $rPrime;
	$object->{scale} = $n / $r;

	$object->{children} = { };
	$object->{nx} = [ $a, 0, $c ];

	$object->{ny} = [
		$c * $unit->[1],
		$a * $unit->[2] - $c * $unit->[0],
		-$a * $unit->[1]
	];

	$object->apply(sub {
			my ($o) = @_;
			place($object, $o);
		});

	return $object;
}


sub contents
{
	my ($self, $pos, $radius) = @_;

	if (not defined($pos) or not defined($radius)) {
		return $self->{contents};
	}

	my $unit = $pos->unit;
	my $ox = Math::v3dot($self->{nx}, $unit);
	my $oy = Math::v3dot($self->{ny}, $unit);

	my $cx = $self->{q} * $ox;
	my $cy = $self->{q} * $oy;

	my $sx = POSIX::floor($cx);
	my $sy = POSIX::floor($cy);

	my $delta = $radius * $self->{scale};

	my $i0 = POSIX::floor($cx - $delta);
	my $i1 = POSIX::ceil($cx + $delta);
	my $j0 = POSIX::floor($cy - $delta);
	my $j1 = POSIX::ceil($cy + $delta);

	my @objects;

	my $mincos = cos($radius);

	my $uprime = [ $ox, $oy, 1 ];
	Math::v3normalize($uprime);
	my $check = sub {
		my ($x, $y) = @_;
		my $checkpoint = [ $x, $y, $self->{q} ];
		Math::v3normalize($checkpoint);
		my $cosangle = Math::u3cosangle($uprime, $checkpoint);
		return $cosangle >= $mincos;
	};

	for (my $i = $i0; $i < $i1; ++$i) {

		for (my $j = $j0; $j < $j1; ++$j) {

			my $include = 0;

			my $testX = ($i < $sx) ? $i + 1 : $i;
			my $testY = ($j < $sy) ? $j + 1 : $j;

			if ($i == $sx and $j == $sy) {
				$include = 1;
			}
			elsif ($i == $sx) {
				$include = $check->($cx, $testY);
			}
			elsif ($j == $sy) {
				$include = $check->($testX, $cy);
			}
			else {
				# these unit vectors could be pre-calculated...
				$include = $check->($testX, $testY);
			}

			if ($include) {
				$self->addContents(\@objects, $i, $j);
			}
		}
	}

	return \@objects;
}


sub addContents
{
	my ($self, $aref, $i, $j) = @_;

	my $cat = $self->fetch($i, $j);

	if ($cat) {
		push(@$aref, @{ $cat->contents });
	}
}


sub fetch
{
	my ($self, $sx, $sy, $create) = @_;

	my $hash = $self->{children};
	my $key = qq($sx,$sy);

	my $out = undef;

	if (exists($hash->{$key})) {
		$out = $hash->{$key};
	}
	elsif ($create) {
		$out = $hash->{$key} = StarID::Catalog::Linear->new;
	}

	return $out;
}


sub place
{
	my ($self, $object) = @_;

	my $u = $object->unit;

	my $ox = Math::v3dot($self->{nx}, $u);
	my $oy = Math::v3dot($self->{ny}, $u);

	my $sx = POSIX::floor($self->{q} * $ox);
	my $sy = POSIX::floor($self->{q} * $oy);

	my $sub = $self->fetch($sx, $sy, 1);

	$sub->add($object);
}


####################################################################

package StarID::Task;

use base qw(Task);
use Task qw(:codes);


sub mark
{
	my ($self, $key) = @_;
	$self->{mark}->{$key} = 1;
}


sub marked
{
	my ($self, $key) = @_;
	return exists($self->{mark}->{$key});
}


sub unmark
{
	my ($self, $key) = @_;
	delete($self->{mark}->{$key});
}


sub execute
{
	my ($self) = @_;

	$self->report('StarID.Task.execute: subclass should override');
}


sub target
{
	return shift->{target};
}


sub sources
{
	return shift->{sources};
}


sub catalog
{
	return shift->{catalog};
}


sub solution
{
	return shift->{solution};
}


sub resolveObject
{
	my ($self, $raw) = @_;

	# use state to correct raw catalog data
	#	proper motion
	#	precession/nutation
	#	velocity aberration

	my $object;

	my $resolve = $self->{resolveObject};
	if (ref($resolve) eq 'CODE') {
		$object = $resolve->($raw);
	}
	else {
		$object = $raw;
	}

	return $object;
}


####################################################################

package StarID::Task::DirectDoublet;

use base qw(StarID::Task);

use Task qw(:codes);

use Thesis;
use atFunctions qw(atRotVect);

use constant END_DIRECT   => 'endDirect';
use constant END_DOUBLET  => 'endDoublet';


sub initialize
{
	my ($self) = @_;

	my $args = $self->args;

	$self->{lossLimit} = -1;

	if (not $args->{minAssertions}) {
		$args->{minAssertions} = 3;
	}

	if (not $args->{maxAssertions}) {
		$args->{maxAssertions} = 10;
	}

	if (not $args->{iterationLimit}) {
		$args->{iterationLimit} = 100;
	}

	if (not $args->{maxCandidates}) {
		$args->{maxCandidates} = 10;
	}

	if (not $args->{maxTheses}) {
		$args->{maxTheses} = 10;
	}

	if (not $args->{lossCorrection}) {
		$args->{lossCorrection} = 2;
	}
}


sub directed
{
	return shift->{directed};
}


sub doubleted
{
	return shift->{doubleted};
}


sub execute
{
	my ($self) = @_;

	$self->initialize
		if $self->isValid;

	$self->orderSources
		if $self->isValid;

	$self->directMatch
		if $self->isValid;

	$self->doubletMatch
		if $self->isValid;

	$self->evaluateCandidates
		if $self->isValid;
}


sub orderSources
{
	my ($self) = @_;
	my $order = $self->{orderSources};
	if (ref($order) eq 'CODE') {
		my @ordered = sort $order @{ $self->sources };
		$self->{sources} = \@ordered;
	}
}


sub qMethod
{
	my ($self, $matches) = @_;

	# returns either a reference to a quaternion (array of 4 elements),
	# or a string indicating what went wrong

	# see ADS/quest.m for combining observations/reference
	# see Jama source for finding eigenvalues/eigenvectors

	my $problem = undef;

	if (not ref($matches) or (@{ $matches } < 2)) {
		$problem = 'need list of at least 2 matched observations';
		$self->error(BAD_EXECUTE, "qMethod: $problem");
		return $problem;
	}

	my @B = (
		[ 0, 0, 0 ],
		[ 0, 0, 0 ],
		[ 0, 0, 0 ],
	);

	# we arrange to normalize weights so B, S, sigma, Z, K will match quest.m
	my $totalWeight = 0;
	foreach my $i (@{ $matches }) {
		if (not $i->{weight}) {
			$i->{weight} = 1;
		}
		$totalWeight += $i->{weight};
	}

	# temporaries for loop
	my $wi = [ 0, 0, 0 ];
	my $vi = [ 0, 0, 0 ];

	my $outer = [
		[ 0, 0, 0 ],
		[ 0, 0, 0 ],
		[ 0, 0, 0 ],
	];

	# calculate B = WVt
	foreach my $i (@{ $matches }) {

		my $weight = sqrt($i->{weight} / $totalWeight);

		my $ub = $i->{observation};
		my $ur = $i->{reference};

		Math::v3scalex($ub, $weight, $wi);
		Math::v3scalex($ur, $weight, $vi);

		Math::v3outerx($wi, $vi, $outer);

		Math::madd(3, 3, \@B, $outer, \@B);
	}

	$self->report("qMethod: B " . $self->stringify(\@B))
		if $self->chatter(8);

	# @Zt is transpose(Z)
	my @Zt = (
		$B[1][2] - $B[2][1],
		$B[2][0] - $B[0][2],
		$B[0][1] - $B[1][0],
	);
	$self->report("qMethod: Zt " . $self->stringify(\@Zt))
		if $self->chatter(8);

	# sigma = trace(B)
	my $sigma = $B[0][0] + $B[1][1] + $B[2][2];
	$self->report("qMethod: sigma $sigma")
		if $self->chatter(8);

	# S = B + Bt,
	# where Bt = transpose(B)
	my @S = (
		[ $B[0][0] + $B[0][0], $B[0][1] + $B[1][0], $B[0][2] + $B[2][0] ],
		[ $B[1][0] + $B[0][1], $B[1][1] + $B[1][1], $B[1][2] + $B[2][1] ],
		[ $B[2][0] + $B[0][2], $B[2][1] + $B[1][2], $B[2][2] + $B[2][2] ],
	);
	$self->report("qMethod: S " . $self->stringify(\@S))
		if $self->chatter(8);

	# K = [ S - Io  Z ]
	#     [ Zt      o ]
	# where o=sigma, Zt=transpose(Z)

	my @K = (
		[ $S[0][0] - $sigma, $S[0][1], $S[0][2], $Zt[0] ],
		[ $S[1][0], $S[1][1] - $sigma, $S[1][2], $Zt[1] ],
		[ $S[2][0], $S[2][1], $S[2][2] - $sigma, $Zt[2] ],
		[ @Zt, $sigma ],
	);
	$self->report("qMethod: K " . $self->stringify(\@K))
		if $self->chatter(8);

require Math::Eigen;
	my $decomp = Math::Eigen->new(matrix => \@K);

# optimal q corresponds to maximum eigenvalue of K
	my $eigreal = $decomp->getRealEigenvalues;
	my $eigimag = $decomp->getImagEigenvalues;

	my $selindex = undef;
	for (my $i = 0; $i < @{ $eigreal }; ++$i) {
		if ($eigimag->[$i] == 0) {
			if (not defined($selindex)
				or ($eigreal->[$i] > $eigreal->[$selindex])) {
				$selindex = $i;
			}
		}
	}

	if (defined($selindex)) {
		my $quaternion = [ 0, 0, 0, 0 ];
		my $eigvect = $decomp->getEigenvectorMatrix;

		for (my $i = 0; $i < @{ $eigvect }; ++$i) {
			$quaternion->[$i] = $eigvect->[$i][$selindex];
		}
		$self->report("found optimal quaternion "
				. $self->stringify($quaternion))
			if $self->chatter(8);
		return $quaternion;
	}
	else {
		$problem = 'no real eigenvalue found';
		$self->error(BAD_EXECUTE, "qMethod: $problem");
	}

	return $problem;
}


# sub StarID::Task::directMatch
#
# $match is like:
#	sub {
#		my ($task, $source, $object) = @_;
#		my $candidate = 0;
#		my $info = $task->{directInfo};
#
#		if (abs($source->mag - $object->mag) > $info->{magTolerance}) {
#			# exceeded magnitude tolerance
#		}
#		elsif (Math::v3angle($source->unit, $object->unit)
#				> $info->{posTolerance}) {
#			# exceeded position tolerance
#		}
#		# additional tests?
#		else {
#			$candidate = 1;
#		}
#
#		return $candidate;
#	}
# and, $task->{directInfo} has been set up with, for example,
#		magTolerance = 0.3;
#		posTolerance = 10 * ARC_SEC;

sub directMatch
{
	my ($self) = @_;

	my $match = $self->{directMatch};

	$self->{directed} = [ ];
	$self->unmark(END_DIRECT);

	my $catalog = $self->catalog;
	my $radius = $self->{directMatchRadius};

	foreach my $source (@{ $self->sources }) {

		my @directed;

		foreach my $object (@{ $catalog->contents($source, $radius) }) {
			if ($match->($self, $source, $object)) {
				push(@directed, $object);
			}
		}

		if (@directed) {
			push(@{ $self->{directed} }, $source);
			$source->{directed} = \@directed;
			my $count = @directed;
			$self->report("source $source->{ID} has $count direct match(s)")
				; # if $self->chatter(5);
		}

		last if $self->marked(END_DIRECT);
	}
}


sub doubletMatch
{
	my ($self) = @_;

	my $match = $self->{doubletMatch};
	my $sources = $self->directed;

	# initialize results
	$self->{candidates} = [ ];
	$self->{doubleted} = [ ];
	foreach my $s (@{ $sources }) {
		$s->{doublets} = [ ];
	}

	foreach my $s (@{ $sources }) {

		my $seen = 0;

		foreach my $o (@{ $sources }) {

			if ($seen) {
				$self->doubletMatchSource($s, $o);
			}
			elsif ($s == $o) {
				$seen = 1;
			}
		}

		$self->postprocessDoublets($s);
	}
}


sub reportThesis
{
	my ($self, $thesis, %args) = @_;
	my $s = '';
	foreach my $x (@{ $thesis->assertions }) {
		$s .= "\n\t" . $x->stringify;
	}
	$s .= "\n\trating $thesis->{ratedLoss}";
	my $tag = $args{tag} ? "[$args{tag}]" : '';
	$self->report("thesis$tag$s");
}


sub evaluateCandidates
{
	my ($self) = @_;

	if (not $self->{candidates}) {
		$self->warning('no candidates');
		return;
	}

	my $args = $self->args;

	my $candidates = $self->{candidates};
	$self->{theses} = [ ];

	my $count = @$candidates;
	$self->report("$count candidates");

	foreach my $x (@{ $candidates }) {

		$self->analyzeCandidate(
				pivot => $x->{pivot},
				optional => $x->{optional},
				);
	}

	if (@{ $self->{theses} }) {

		$self->{solution} = $self->{theses}[0];

		$self->reportThesis($self->{solution})
			if $self->chatter(2);

		if ($self->chatter(7)) {
			my $i = 0;
			foreach my $thesis (@{ $self->{theses} }) {
				++$i;
				next if $thesis == $self->{solution};
				$self->reportThesis($thesis, tag => $i);
			}
		}
		# $self->report('advancing thesis ' . $self->stringify($self->{solution}));
	}
}


sub rateThesis
{
	my ($self, $thesis) = @_;

	if ($self->chatter(7)) {
		my $s = 'rating';
		foreach my $a (@{ $thesis->assertions }) {
			$s .= "\n\t" . $a->stringify;
		}
		$self->report($s);
	}

	# prepare for and invoke qMethod
	my @matches;
	foreach my $x (@{ $thesis->assertions }) {
		push(@matches, {
				observation => $x->[0]->unit,
				reference => $x->[1]->unit,
			});
	}

	# determine the optimal quaternion
	my $quaternion = undef;
	if (my $result = $self->qMethod(\@matches)) {
		if (ref($result)) {
			$quaternion = $result;
		}
		else {
			$self->warning("rateThesis: qMethod failed: $result");
			return;
		}
	}

	# compute initial and final loss (average since theses have
	# varying number of assertions)
	my $j0 = 0;
	my $j1 = 0;
	my $b2r = [ 0, 0, 0, 0 ];
	{
		my $q = $quaternion;
		# inverse quaternion
		$b2r = [ -$q->[0], -$q->[1], -$q->[2], $q->[3] ];
	}

	foreach my $x (@{ $thesis->assertions }) {

		my $observation = $x->[0];
		my $reference = $x->[1];

		my $d0 = Math::v3subtractx($observation->unit, $reference->unit);
		$j0 += Math::v3norm2($d0);

		my $prime = atRotVect(q => $b2r, x => $observation->unit);
		my $d1 = Math::v3subtractx($prime, $reference->unit);
		$j1 += Math::v3norm2($d1);
	}

	$thesis->{bodyToReference} = $b2r;

	$thesis->{initialLoss} = $j0;
	$thesis->{finalLoss} = $j1;

	my $count = @{ $thesis->assertions };
	$thesis->{meanLoss} = $j1 / $count;
	my $factor = $self->args->{lossCorrection} ** ($count - 3);
	$thesis->{ratedLoss} = $thesis->{meanLoss} / $factor;

	$self->report(
			sprintf('loss: initial %.3e final %.3e mean %.3e rate[%d] %.3e',
				$j0, $j1, $thesis->{meanLoss}, $count, $thesis->{ratedLoss}))
		if $self->chatter(6);
}


# given information on a particular source, find the best 3 assertion fit
sub analyzeSourceAssertions
{
	my ($self, %info) = @_;

	my $args = $self->args;

	my @elements = ($info{pivot});

	my $optional = X::Compound->new(
			elements => $info{optional},
			minElements => 2,
			maxElements => 2,
			);
	push(@elements, $optional);

	my $compound = X::Compound->new(
			elements => \@elements,
			# the compound must include each of its children
			minElements => scalar(@elements),
			);

	if ($self->chatter(7)) {
		my $s = 'analyzing pivot ' . $info{pivot}->stringify;
		foreach my $x (@{ $info{optional} }) {
			$s .= "\n\t" . $x->stringify;
		}
		$self->report($s);
	}

	my $cloud = Thesis::Cloud->new(
			compound => $compound,
			tool => $self->{tool},
			);

	my $minLoss = undef;

	for (my $iterator = undef;
			my $result = $cloud->analyze(
					iterator => $iterator,
					);
			) {

		foreach my $thesis (@{ $result->{accepted} }) {

			$self->rateThesis($thesis);

			if (defined($thesis->{ratedLoss}) and
					(not defined($minLoss)
						or $thesis->{ratedLoss} < $minLoss)) {
				$minLoss = $thesis->{ratedLoss};
			}
		}

		my $accepted = @{ $result->{accepted} };
		$self->{countAccepted} += $accepted;
		$self->{countConsidered} += $cloud->{counter};
		$self->report("found $accepted consistent theses [$self->{countAccepted}/$self->{countConsidered}]")
			if $self->chatter(7);

		$iterator = $result->{iterator};
		last if not $iterator;
	}

	$self->report("cloud dissipated")
		if $self->chatter(7);

	if (defined($minLoss)) {

		my $lossLimit = $self->{candidateLossLimit};

		if (not defined $lossLimit or $minLoss < $lossLimit) {
			my %entry = (
				pivot => $info{pivot},
				optional => $info{optional},
				minLoss => $minLoss,
			);

			$self->updateLossList(
					name => 'candidates',
					list => [ \%entry ],
					loss => 'minLoss',
					max => $args->{maxCandidates},
					);
		}
	}
}


sub updateLossList
{
	my ($self, %args) = @_;

	my $lossKey = $args{loss};
	my $listKey = $args{name};

	my $old = $self->{$listKey};

	my @sorted = sort { $a->{$lossKey} <=> $b->{$lossKey} }
			@$old, @{ $args{list} };

	$self->{$listKey} = [ splice(@sorted, 0, $args{max}) ];

	my $limit = undef;

	if (@{ $self->{$listKey} } > 0) {

		$limit = $self->{$listKey}[-1]{$lossKey};
		my $min = $self->{$listKey}[0]{$lossKey};

		if ($limit < $self->{$listKey . 'LossLimit'}) {
			$self->{$listKey . 'LossLimit'} = $limit;
			$self->report(sprintf("$listKey loss range [%.3e, %.3e]",
					$min, $limit))
				if $self->chatter(5);
		}
	}

	return $limit;
}


sub analyzeCandidate
{
	my ($self, %info) = @_;

	if ($self->chatter(5)) {
		my $s = 'analyzing pivot ' . $info{pivot}->stringify;
		foreach my $x (@{ $info{optional} }) {
			$s .= "\n\t" . $x->stringify;
		}
		$self->report($s);
	}

	my $args = $self->args;

	if (@{ $info{optional} } < $args->{minAssertions} - 1) {
		$self->report('insufficient support')
			if $self->chatter(6);
		return;
	}

	my @elements = ($info{pivot});

	my $optional = X::Compound->new(
			elements => $info{optional},
			minElements => $args->{minAssertions} - 1,
			maxElements => $args->{maxAssertions} - 1,
			);
	push(@elements, $optional);

	my $compound = X::Compound->new(
			elements => \@elements,
			# the compound must include each of its children
			minElements => scalar(@elements),
			);

	my $cloud = Thesis::Cloud->new(
			compound => $compound,
			tool => $self->{tool},
			);

	my $lossLimit = $self->{thesesLossLimit};

	for (my $iterator = undef;
			my $result = $cloud->analyze(
					iterator => $iterator,
					iterationLimit => $args->{iterationLimit},
					);
			) {

		my @filtered;

		foreach my $thesis (@{ $result->{accepted} }) {

			$self->rateThesis($thesis);

			if (not defined($lossLimit)
					or $thesis->{ratedLoss} < $lossLimit) {
				push(@filtered, $thesis);
			}
		}

		$lossLimit = $self->updateLossList(
				name => 'theses',
				list => \@filtered,
				loss => 'ratedLoss',
				max => $args->{maxTheses},
				);

		$iterator = $result->{iterator};
		last if not $iterator;
	}

	$self->report("cloud dissipated")
		if $self->chatter(7);
}


sub postprocessDoublets
{
	my ($self, $source) = @_;

	$self->report('postprocessing doublets ' . $source->id);

	my $doublets = $source->doublets;

	if (@$doublets < 1) {
		return;
	}

	my %matched;
	my %invert;

	foreach my $d (@{ $doublets }) {

		my $other = $d->[0];

		for (my $i = 1; $i < @$d; ++$i) {

			my $p = $d->[$i];

			my $id = $p->[0]->id;
			$invert{$id} = $p->[0];

			push(@{ $matched{$id} }, [ $other, $p->[1] ]);
		}
	}

	while (my ($k, $v) = each(%matched)) {
		if (@$v > 1) {
			my $pivot = Thesis::Assertion->new($source => $invert{$k});
			my @optional;
			foreach my $p (@$v) {
				push(@optional, Thesis::Assertion->new($p->[0] => $p->[1]));
			}

			$self->analyzeSourceAssertions(
					pivot => $pivot,
					optional => \@optional,
					);
		}
	}

}


# StarID::Task::doubletMatchSource
#
# $match is like:
#	sub {
#		my ($task, $s1, $s2) = @_;
#		my $doublet = 0;
#		my $info = $task->{doubletInfo};
#		if (abs($info->{d0} - $s1->angle($s2)) > $info->{tolerance}) {
#			# exceeded doublet tolerance
#		else {
#			$doublet = 1;
#		}
#		return $doublet;
#	}
# where $task->{doubletInfo} is prepared with, for example,
#		d0 = $s1->angle($other);
#		tolerance = ARC_SEC_r;

sub doubletMatchSource
{
	my ($self, $source, $other) = @_;

	my $match = $self->{doubletMatch};
	my $prepare = $self->{doubletPrepare};
	if (ref($prepare) eq 'CODE') {
		$prepare->($self, $source, $other);
	}

	my @doublets;

	foreach my $s (@{ $source->directed }) {

		foreach my $o (@{ $other->directed }) {

			if ($s == $o) {
				# ignore
			}
			elsif ($match->($self, $s, $o)) {
				push(@doublets, [ $s, $o ]);
			}
		}
	}

	if (@doublets) {
		push(@{ $source->{doublets} }, [ $other, @doublets ]);
	}
}


sub doubletPrepareBasic
{
	my ($self, $source, $other) = @_;

	if (not exists($self->{doubletInfo})) {
		$self->{doubletInfo} = { };
	}
	my $info = $self->{doubletInfo};
	$info->{source} = $source;
	$info->{other} = $other;
	$info->{angle} = $source->angle($other);
}


####################################################################

package StarID::Tools;

use Math;


sub normalizeRA ($)
{
	my ($alpha) = @_;
	my $ra = Math::remainder($alpha, Math::PIx2);
	return $ra;
}


sub normalizeDEC ($)
{
	my ($delta) = @_;
	# first get -pi < $delta < pi
	my $dec0 = POSIX::fmod($delta, Math::PI);
	my $dec;
	if (abs($dec0) > Math::PIo2) {
		my $signum = ($dec0 < 0) ? -1 : 1;
		$dec = $signum * Math::PI - $dec0;
	}
	else {
		$dec = $dec0;
	}
	return $dec;
}


# account for proper motion
#
#	(based on attitude/src/ads/utilities/proper.m)
#
#	pmScale => number of units of pmRate that have passed

sub applyProperMotion
{
	my %args = @_;

	my $pmScale = $args{pmScale} || 1;
	my $epsilon = $args{epsilon} || 1e-2;

	my ($ra0, $dec0) = Math::v3rdl($args{unit});

	my $decMotion = $args{pmRate} * cos($args{pmAngle});
	my $raMotion = 0;

	my $cosdec = cos($dec0);
	if (abs($cosdec) > $epsilon) {
		$raMotion = -$args{pmRate} * sin($args{pmAngle}) / $cosdec;
	}

	my $trueRA = normalizeRA($ra0 + $raMotion * $pmScale);
	my $trueDEC = normalizeDEC($dec0 + $decMotion * $pmScale);

	my $trueUnit = Math::rd2unit($trueRA, $trueDEC);

	my $outUnit = $args{outUnit} || $args{unit};
	@{ $outUnit } = @{ $trueUnit };

	return $outUnit;
}


# account for velocity aberration
#
#	(based on attitude/src/ads/utilities/abber.m)
#
#	aberration => aberration vector
#		OR
#	vCraft => Spacecraft velocity about Earth (km/s)
#	vEarth => Earth's velocity about sun (km/s)

sub applyVelocityAberration
{
	my %args = @_;

	my $aberration;

	if ($args{aberration}) {
		$aberration = $args{aberration};
	}
	else {
		my $c = $args{c} || 2.99792458e5; # speed of light (km/s)
		my $vCraft = $args{vCraft};
		my $vEarth = $args{vEarth};

		my $vSum = Math::v3addx($vCraft, $vEarth);
		$aberration = Math::v3scale($vSum, 1 / $c);
	}

	my $unit = $args{unit};

	my $outUnit = $args{outUnit} || $args{unit};
	Math::v3subtractx($unit, $aberration, $outUnit);
	Math::v3normalize($outUnit);

	return $outUnit;
}



1;

