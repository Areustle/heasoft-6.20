# $Source: /headas/headas/swift/uvot/tasks/uvotstarid/Thesis.pm,v $
# $Revision: 1.9 $
# $Date: 2003/11/26 20:39:18 $
#
#	Given a set of assocations (hypotheses)
#		{ h1:o1=r1, h2:o2=r2, ... }
#	where oi from O and ri from R are distinct,
#	and a set of possible associations (alternates)
#		{ a1:choice(o11=r11 and o12=r12, o11=r13 and o12=r14, ...),
#			a2:choice(o23=r21 and o22=r22, o21=r23 and o22=r24), ...), ... }
#	where the choice operator indicates that at most one of its operands
#	can be true,
#	find sets of consistent hypotheses meeting some request(s).
#
# $Log: Thesis.pm,v $
# Revision 1.9  2003/11/26 20:39:18  rwiegand
# Added underscore to object initialization method.
#
# Revision 1.8  2002/11/21 16:33:59  wiegand
# Made logging chatter dependent
#
# Revision 1.7  2002/11/15 14:38:14  wiegand
# Corrected Thesis.test for case of duplicate assertion
#
# Revision 1.6  2002/11/12 19:28:08  rwiegand
# Added Assertion class.  Basic object definitions moved from Set.pm to X.pm
#
# Revision 1.5  2002/11/12 18:32:57  rwiegand
# Renamed pairs to assertions.  Split into Thesis::{Base, Atomic, Cloud}.
#
# Revision 1.4  2002/10/17 20:00:09  rwiegand
# Inserted working Thesis::Element iteration.  Shifted some of the code
# between Thesis and its subclasses.
#
# Revision 1.2  2002/10/11 15:40:54  rwiegand
# Now longer any need to flatten Thesis::Elements to list since the iterator
# takes care of that.  Corrected Pair iterator.  Reimplemented Compound
# iterator using looping instead of recursion.
#
# Revision 1.1  2002/10/11 14:20:50  rwiegand
# Initial revision
#

use strict;

package Thesis;

use X;

##############################################################################
package Thesis::Assertion;

use base qw(X::AtomList);

sub new
{
	my ($class, $from, $to) = @_;

	my $object = bless([ $from, $to ], $class);

	return $object;
}


sub addAssertions
{
	my ($self, $list) = @_;
	push(@{ $list }, $self);
}


sub stringify
{
	my ($self) = @_;
	my $s = $self->[0]->id . ' => ' . $self->[1]->id;
	return $s;
}



##############################################################################
package Thesis::AssertionList;

use base qw(X::AtomList);


sub new
{
	my ($class, @assertions) = @_;

	my $object = bless([ @assertions ], $class);

	return $object;
}


sub addAssertions
{
	my ($self, $list) = @_;
	push(@{ $list }, @{ $self });
}


sub stringify
{
	my ($self) = @_;
	my $s = 'AssertionList { ';
	foreach my $assert (@{ $self }) {
		$s .= $assert->stringify;
		$s .= ', ',
	}
	chop($s);
	chop($s);
	$s .= ' }';
	return $s;
}



##############################################################################
package Thesis::Atomic;

use base qw(Task X::Object);

my $DEBUG_TEST = 0;


sub _initialize
{
	my ($self) = @_;

	if (not $self->{atoms}) {
		die('Thesis.Atomic.initialize: missing atoms');
	}

	my @assertions = ();

	foreach my $atom (@{ $self->atoms }) {
		$atom->addAssertions(\@assertions);
	}

	$self->{assertions} = \@assertions;

	$self->simplify;
}


sub atoms
{
	return shift->{atoms};
}


sub assertions
{
	return shift->{assertions};
}


sub invalidate
{
	my ($self, $error) = @_;
	if (not $self->{error}) {
		$self->{error} = $error;
	}
	$self->report("thesis[$self->{id}] problem: $error")
		if $self->chatter(8);
}


sub isValid
{
	my ($self) = @_;
	return not $self->{error};
}


sub test
{
	my ($self, $args) = @_;

	delete($self->{error});

	if (@{ $self->assertions } < $self->{minAssertions}) {
		$self->invalidate('not enough assertions');
	}

	if ($self->chatter(8)) {
		my $s = 'Thesis.test';
		foreach my $assert (@{ $self->assertions }) {
			my $k = $assert->[0]->id;
			my $v = $assert->[1]->id;
			$s .= "\n\t$k => $v";
		}
		$self->report($s);
	}

	my %forward = ();
	my %backward = ();

	foreach my $assert (@{ $self->assertions }) {

		my $k = $assert->[0]->id;
		my $v = $assert->[1]->id;

		my $forward = exists($forward{$k}) ? $forward{$k} : undef;
		my $backward = exists($backward{$v}) ? $backward{$v} : undef;

		if ($forward and ($forward ne $v)) {
			my $s = "key $k associated with $forward and $v";
			$self->invalidate($s);
			last;
		}
		elsif ($backward and ($backward ne $k)) {
			my $s = "value $v associated with $backward and $k";
			$self->invalidate($s);
			last;
		}
		else {
			$forward{$k} = $v;
			$backward{$v} = $k;
		}
	}

}


# remove redundant hypotheses
sub simplify
{
	my ($self) = @_;

	my %simplified = ();

	foreach my $p (@{ $self->assertions }) {
		my $s = $p->[0]->id . ' => ' . $p->[1]->id;
		if (not $simplified{$s}) {
			$simplified{$s} = $p;
		}
	}

	if (keys(%simplified) != @{ $self->assertions }) {
		$self->{pairs} = [ values(%simplified) ];
		my $count = @{ $self->{assertions} };
		$self->report("simplified to $count assertions")
			if $self->chatter(8);
	}
}


##############################################################################
package Thesis::Cloud;

use base qw(Task X::Object);

my $DEBUG = 0;


sub _initialize
{
	my ($self) = @_;

	if (not $self->{compound}) {
		die('Thesis.Cloud.initialize: missing compound');
	}
}


sub analyze
{
	my ($self, %args) = @_;

	my $iterator = $args{iterator};
	if (not $iterator) {
		$iterator = $self->{compound}->makeIterator;
	}

	if ($args{minAssertions}) {
		$self->{minAssertions} = $args{minAssertions};
	}

	my $iterationLimit = $args{iterationLimit} || -1;
	my $solutionLimit = $args{solutionLimit} || -1;

	my @accepted = ();

	my $partial = undef; 
	my $iteration = 0; 
	my $atoms;

	while (not $partial and ($atoms = $iterator->())) {

		my $thesis = $self->makeAtomic($atoms);

		$thesis->test;

		if ($thesis->isValid) {
			push(@accepted, $thesis);
			if (@accepted == $solutionLimit) {
				$partial = 1;
			}
		}

		if (++$iteration == $iterationLimit) {
			$partial = 1;
		}
	}

	my %result = (
		accepted => \@accepted,
		($partial ? (iterator => $iterator) : ()),
	);

	return \%result;
}


sub makeAtomic
{
	my ($self, $atoms) = @_;

	++$self->{counter};

	my $thesis = Thesis::Atomic->new(
		tool => $self->{tool},
		id => $self->{counter},
		atoms => $atoms,
		minAssertions => $self->{minAssertions} || 1,
		);

	return $thesis;
}


1;

