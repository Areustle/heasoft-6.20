# $Source: /headas/headas/swift/uvot/tasks/uvotstarid/X.pm,v $
# $Revision: 1.1 $
# $Date: 2002/11/12 19:26:25 $
#
#
# $Log: X.pm,v $
# Revision 1.1  2002/11/12 19:26:25  rwiegand
# Initial revision
#

use strict;
use integer;

use XBase;
use Set;

my $DEBUG = 0;


##############################################################################

package X::Atomic;

use base qw(X::Object);


sub atoms
{
	return shift->{atoms};
}


sub makeIterator
{
	my ($self) = @_;

	my $first = 1;

	my $iterator = sub {
print "iterating $self\n" if $DEBUG;

		my $out = undef;

		if ($first) {
			$first = 0;
			$out = [ $self ];
		}
		else {
print "at end\n" if $DEBUG;
		}

		return $out;
	};

	return $iterator;
}


##############################################################################

package X::AtomList;

use base qw(X::Atomic);


sub new
{
	my ($class, @elements) = @_;

	my $object = bless(\@elements, $class);

	return $object;
}


sub elements
{
	my ($self) = @_;
	return $self;
}



##############################################################################

package X::Compound;

use base qw(X::Object);


sub elements
{
	return shift->{elements};
}


sub makeIterator
{
	my ($self) = @_;

	my $powerIter = PowerSet::makeIterator($self);
	my $collIter = undef;

	my $iterator = sub {
print "iterating $self\n" if $DEBUG;

		my $out = undef;

		if (not defined($powerIter)) {
print "at end\n" if $DEBUG;
		}

		while ($powerIter and not $out) {

			if (defined($collIter)) {
				my $list = $collIter->();
				if (not defined($list)) {
					$collIter = undef;
print "end current\n" if $DEBUG;
				}
				elsif (@$list) {
					my $last = @$list - 1;
					$out = $list->[0];
					foreach my $x (@$list[1 .. $last]) {
						push(@$out, @$x);
					}
				}
				else {
					$out = $list;
				}
			}
			else {
				my $list = $powerIter->();
				if (not defined($list)) {
					# end of power set
					$powerIter = undef;
					$collIter = undef;
print "end power\n" if $DEBUG;
				}
				elsif (@$list) {
					$collIter = Collection::makeIterator(@$list);
				}
				else {
					$out = [ ];
				}
			}
		}

		return $out;
	};

	return $iterator;
}



##############################################################################

package X::Void;


sub id
{
	my $s = '{ }';
	return $s;
}


sub makeIterator
{
	my ($self) = @_;

	my $first = 1;

	my $iterator = sub {
print "iterating $self\n" if $DEBUG;

		my $out = undef;

		if ($first) {
			$first = 0;
			$out = [ ];
		}

		return $out;
	};

	return $iterator;
}



1;

