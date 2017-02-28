# $Source: /headas/headas/swift/gen/lib/perl/atFunctions.pm,v $
# $Revision: 1.2 $
# $Date: 2002/09/30 20:46:21 $
#
#	Some atFunctions converted to perl
#
# $Log: atFunctions.pm,v $
# Revision 1.2  2002/09/30 20:46:21  wiegand
# Added conversion of quaternion to rotation matrix
#
# Revision 1.1  2002/09/30 20:29:13  wiegand
# Initial revision
#

package atFunctions;

use strict;

use base qw(Exporter);

@atFunctions::EXPORT_OK = qw(atRotVect atQuatToRM);
%atFunctions::EXPORT_TAGS = (
	functions => [ qw(atRotVect atQuatToRM) ],
);



# ROTATE A VECTOR WITH ROTATION MATRIX.
# Converted from atFunctions/atRotVect

sub atRotVect
{
	my %args = @_;

	my $rm = $args{rm};
	if (not $rm and $args{q}) {
		$rm = atQuatToRM($args{q});
	}

	my $in = $args{in} || $args{x};
	my $out = $args{out} || $args{y} || [ ];

	for (my $i = 0; $i < 3; ++$i) {

		my $y = 0;

		for (my $j = 0; $j < 3; ++$j) {
			$y += $rm->[$i][$j] * $in->[$j];
		}

    		$out->[$i] = $y;
	}

	return $out;
}


sub atQuatToRM ($;$)
{
	my ($q, $rm) = @_;

	if (not $rm) {
		$rm = [ [ ], [ ], [ ] ];
	}

	my $q0 = $q->[0];
	my $q1 = $q->[1];
	my $q2 = $q->[2];
	my $q3 = $q->[3];

	$rm->[0] = [
		$q0 * $q0 - $q1 * $q1 - $q2 * $q2 + $q3 * $q3,
		2 * ($q0 * $q1 + $q2 * $q3),
		2 * ($q0 * $q2 - $q1 * $q3),
	];

	$rm->[1] = [
		2 * ($q0 * $q1 - $q2 * $q3),
		-$q0 * $q0 + $q1 * $q1 - $q2 * $q2 + $q3 * $q3,
		2 * ($q1 * $q2 + $q0 * $q3),
	];

	$rm->[2] = [
		2 * ($q0 * $q2 + $q1 * $q3),
		2 * ($q1 * $q2 - $q0 * $q3),
		-$q0 * $q0 - $q1 * $q1 + $q2 * $q2 + $q3 * $q3,
	];

	return $rm;
}


1;

