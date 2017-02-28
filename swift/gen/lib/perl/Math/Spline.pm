# $Source: /headas/headas/swift/gen/lib/perl/Math/Spline.pm,v $
# $Revision: 1.2 $
# $Date: 2004/10/17 11:36:45 $
#
# $Log: Spline.pm,v $
# Revision 1.2  2004/10/17 11:36:45  rwiegand
# Avoid running off end of array.
#
# Revision 1.1  2004/10/13 22:33:36  rwiegand
# Extracted spline code for sharing.
#

use strict;

package Math::Spline;


use constant NATURAL => 1e30;



# From Numerical Recipes. Given arrays x(1:n) and y(1:n) containing a
# tabulated function, i.e., y_i = f(x_i) with x_1 < x_2 < ... < x_n,
# and given values yp1 and ypn for the first derivative of the 
# interpolating function at points 1 and n, respectively, this routine
# returns an array y2(1:n) of length n which contains the second
# derivatives of the interpolating function at the taubulated points
# x_i. If yp1 and/or ypn are equal to 1 x 10^30 or larger, the routine
# is signaled to set the corresponding boundary condition for a 
# natural spline, with zero second derivative on that boundary. Parameter
# nmax is the largest anticipated value of n.
#	Perl translation shifts arrays to 0 .. n-1

sub cubic
{
	my ($x, $y, $yp1, $ypn, $y2) = @_;

	my $n = @$x;
	my @u;
	if ($yp1 > .99e30) {
		$y2->[0] = 0;
		$u[0] = 0;
	}
	else {
		$y2->[0] = -.5;
		$u[0] = (3 / ($x->[1] - $x->[0])) *
				(($y->[1] - $y->[0]) / ($x->[1] - $x->[0]) - $yp1)
	}

	for (my $i = 1; $i < $n - 1; ++$i) {
		my $sig = ($x->[$i] - $x->[$i-1]) / ($x->[$i+1] - $x->[$i-1]);
		my $p = $sig * $y2->[$i-1] + 2;
		$y2->[$i] = ($sig - 1) / $p;
		$u[$i] = (6 * (($y->[$i+1] - $y->[$i]) / ($x->[$i+1] - $x->[$i])
				- ($y->[$i] - $y->[$i-1]) / ($x->[$i] - $x->[$i-1]))
				/ ($x->[$i+1] - $x->[$i-1]) - $sig * $u[$i-1]) / $p;
	}

	my $qn;
	my $un;

	if ($ypn > .99e30) {
		$qn = 0;
		$un = 0;
	}
	else {
		$qn = 0.5;
		$un = (3 / ($x->[$n-1] - $x->[$n-2]))
				* ($ypn - ($y->[$n-1] - $y->[$n-2])
					/ ($x->[$n-1] - $x->[$n-2]));
	}

	$y2->[$n-1] = ($un  - $qn * $u[$n-2]) / ($qn * $y2->[$n-2] + 1);

	for (my $k = $n - 2; $k >= 0; --$k) {
		$y2->[$k] = $y2->[$k] * $y2->[$k+1] + $u[$k];
	}

}


# From Numerical Recipes p.110. Given the arrays xa(1:n) and ya(1:n)
# of length n, which tabulate a function (with the xa's in order), and
# given a value of x, this routine returns a cubic spline interpolated
# value y.
#	Perl translation shifts arrays to 0 .. n-1

sub splint
{
	my ($xa, $ya, $y2a, $x, $yref) = @_;

	my $problem = undef;

	my $n = @$xa;
	my $klo = 0;
	my $khi = $n - 1;

	my $array = UNIVERSAL::isa($x, 'ARRAY');

	my @x = $array ? @$x : ($x);
	my $first = $x[0];

	while ($khi - $klo > 1) {
		my $k = int(($khi + $klo) / 2);
		if ($xa->[$k] > $first) {
			$khi = $k;
		}
		else {
			$klo = $k;
		}
	}

	my $index = 0;
	my $last = $n - 1;
	foreach my $x (@x) {

		# update bounds
		while ($khi < $last and $x > $xa->[$khi]) {
			++$khi;
		}
		$klo = $khi - 1;

		my $h = $xa->[$khi] - $xa->[$klo];
		if ($h == 0) {
			$problem = 'bad xa input in splint';
		}
		else {
			my $a = ($xa->[$khi] - $x) / $h;
			my $b = ($x - $xa->[$klo]) / $h;
			my $y = $a * $ya->[$klo] + $b * $ya->[$khi]
					+ (($a ** 3 - $a) * $y2a->[$klo]
						+ ($b ** 3 - $b) * $y2a->[$khi])
							* $h ** 2 / 6;
			if ($array) {
				$yref->[$index++] = $y;
			}
			else {
				$$yref = $y;
			}
		}
	}

	return $problem;
}



1;

