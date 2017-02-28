# $Source: /headas/headas/swift/uvot/tasks/uvotstarid/Set.pm,v $
# $Revision: 1.10 $
# $Date: 2005/03/04 19:52:43 $
#
#
# $Log: Set.pm,v $
# Revision 1.10  2005/03/04 19:52:43  rwiegand
# Support for sorting catalog in memory.  Split parameter for maximum
# correction into a rotation limit and a translation limit.
#
# Revision 1.9  2002/11/25 22:17:08  rwiegand
# Updated PowerSet.makeIterator to return undef for bogus input instead of
# catching it on the first iteration.
#
# Revision 1.8  2002/11/19 21:49:04  wiegand
# Argh.  When iterating power sets, detect maximum request larger than
# set itself.
#
# Revision 1.7  2002/11/18 19:42:27  wiegand
# When iterating power sets, fix case when minimum request is larger than
# set itself.  Distinguish output variable from output indices.
#
# Revision 1.6  2002/11/12 19:29:00  rwiegand
# Moved element classes to X.pm
#
# Revision 1.5  2002/11/12 16:12:01  rwiegand
# Added Set::Element classes
#
# Revision 1.4  2002/10/11 19:12:47  rwiegand
# Reduced memory used for iterating PowerSets
#
# Revision 1.3  2002/10/11 15:38:04  rwiegand
# Replaced SetSet with more flexible Collection::makeIterator.  For iterating
# PowerSets, use set attributes instead of iterator constructor arguments
# for restricting the iteration.
#
# Revision 1.2  2002/10/08 21:22:22  rwiegand
# Added Pairs
#
# Revision 1.1  2002/10/08 20:38:29  rwiegand
# Initial revision
#

use strict;
use integer;


##############################################################################

package Collection;


sub new
{
	my ($class, %args) = @_;
	my $object = bless(\%args, $class);
	return $object;
}


sub makeIterator
{
	my @collections = @_;

	# set up a list of iterators
	# step the least significant iterator which advances the state

	my $null = undef;

	my @state = ();
	foreach my $c (@collections) {
		my $iterator = $c->makeIterator;
		push(@state, { iterator => $iterator });
	}

	my $iterator = sub {

		my $position;

		# locate the most significant change
		for ($position = 0; $position < @state; ++$position) {
			my $state = $state[$position];
			my $next = $state->{iterator}->();
			if (defined($next)) {
				$state->{current} = $next;
				last;
			}
		}

		if ($position == @state) {
			return undef;
		}

		my @out = ();

		for (my $i = 0; $i < @state; ++$i) {

			my $state = $state[$i];

			if ($i < $position) {
				# reset preceeding to starting position
				$state->{iterator} = $collections[$i]->makeIterator;
				$state->{current} = $state->{iterator}->();
			}
			elsif ($i == $position) {
			}
			elsif (not exists($state->{current})) {
				$state->{current} = $state->{iterator}->();
			}

			push(@out, $state->{current});
		}

		return \@out;
	};

	return $iterator;
}



##############################################################################

package Set;

use base qw(Collection);


sub elements
{
	return shift->{elements};
}


sub makeIterator
{
	my ($set) = @_;

	my $i = 0;

	my $iterator = sub {
		my $out = undef;
		if ($i < @{ $set->elements }) {
			$out = $set->elements->[$i];
			++$i;
		}
		return $out;
	};

	return $iterator;
}


sub difference
{
	my ($set1, $set2) = @_;

	my %tmp = ();

	foreach my $o (@{ $set1->elements }) {
		my $s = Object::idString($o);
		$tmp{$s} = $o;
	}

	foreach my $o (@{ $set2->elements }) {
		my $s = Object::idString($o);
		delete($tmp{$s});
	}

	return bless([ values(%tmp) ]);
}



##############################################################################

package PowerSet;

use base qw(Set);


sub elements
{
	return shift->{elements};
}


my $DEBUG = 0;

sub makeIterator
{
	my ($set) = @_;

	my $power = @{ $set->elements };

	my $min = $set->{minElements} || 0;
	my $max = $set->{maxElements} || $power;
	if ($max > $power) {
		$max = $power;
	}

	if (($min > $power) or ($max < $min)) {
print "PowerSet.makeIterator: bad input min=$min, max=$max, elements=$power\n";
		return undef;
	}

	my $count = $min;
	my $first = 1;

	# initial choice
	my @current = (0 .. $count - 1);
print "\nmakeIterator: elements=$power, min=$min, max=$max\n" if $DEBUG;

	my $iterator;
	$iterator = sub {

print "iterate: elements=$power, count=$count, min=$min, max=$max, current=@current\n" if $DEBUG;

		my $selected = undef;

		if ($first) {
			$first = undef;
			$selected = [ @current ];
		}

		while (defined($count) and not $selected) {
print "loop: count=$count, min=$min, max=$max, current=@current\n" if $DEBUG;

			# find least significant position that can be advanced
			my $p;
			for ($p = 0; $p < $count - 1; ++$p) {
				if ($current[$p] + 1 < $current[$p + 1]) {
					last;
				}
			}

			if (($p <= $count - 1) and ($current[$p] < $power - 1)) {

				# incrementing $pth element of @current
				++$current[$p];

				for (my $i = 0; $i < $p; ++$i) {
					$current[$i] = $i;
				}

			}
			elsif ($p >= $count - 1) {
				# can't advance at this $count
				if ($count < $max) {
					++$count;
					@current = (0 .. $count - 1);
				}
				else {
					$count = undef;
				}
			}

			if (defined($count)) {
				$selected = [ @current ];
			}
		}

		my $out = undef;

		if (defined($selected)) {
			my $v = $set->elements;
			# translate from indices to set elements
			$out = [ map { $v->[$_] } @current ];
		}

		return $out;
	};

	return $iterator;
}


sub testIteration
{
	my ($set) = @_;

	my $x;
	for (my $i = $set->makeIterator; $x = $i->(); ) {
		if (@$x) {
			print "got { ", join(', ', @$x), " }\n";
		}
		else {
			print "got { }\n";
		}
	}

	print "done\n";
}


sub timeIteration
{
	my ($set) = @_;

	# print "\niterating over [$elements, $min, $max]\n";
	my $total = 0;
	my $x;
	for (my $i = $set->makeIterator; $x = $i->(); ) {
		$total += @$x;
	}

	print "done [$total]\n";
}


sub testPowerSet
{
	# my $func = \&timeIteration;
	my $func = \&testIteration;

	foreach my $n (0 .. 8) {
		$func->(PowerSet->new(elements => [ 1 .. $n ]));
	}

	$func->(PowerSet->new(elements => [ 1 .. 5 ],
		minElements => 2,
		));

	$func->(PowerSet->new(elements => [ 1 .. 5 ],
		maxElements => 3,
		));

}


1;

