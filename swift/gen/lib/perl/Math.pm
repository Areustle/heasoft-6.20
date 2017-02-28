# $Source: /headas/headas/swift/gen/lib/perl/Math.pm,v $
# $Revision: 1.16 $
# $Date: 2012/08/07 16:46:12 $
#
# $Log: Math.pm,v $
# Revision 1.16  2012/08/07 16:46:12  rwiegand
# Detect cosine out of domain and fix if close enough.
#
# Revision 1.15  2008/04/04 21:46:29  rwiegand
# Added matrix multiplication function.
#
# Revision 1.14  2008/03/26 21:07:29  rwiegand
# Added some quaternion routines.
#
# Revision 1.13  2007/10/05 19:20:25  rwiegand
# Added a couple functions for operating on quaternions.
#
# Revision 1.12  2005/10/06 13:48:07  rwiegand
# Added createSystem and v3kxpy and aliases toDegrees, toRadians.
#
# Revision 1.11  2004/11/03 21:00:17  rwiegand
# Corrected signum [first use apparently].
#
# Revision 1.10  2003/10/23 18:06:42  rwiegand
# Added a function to apply a rotation matrix to a vector [3d].
#
# Revision 1.9  2002/11/22 22:15:55  rwiegand
# Avoid importing any functions from POSIX module into namespace.
#
# Revision 1.8  2002/11/21 16:33:44  wiegand
# Added signum
#
# Revision 1.7  2002/09/30 18:24:30  wiegand
# Corrected bug in hypot.  Implemented v3addx
#
# Revision 1.6  2002/08/16 20:03:58  wiegand
# Added hypotenuse, vector outer product and matrix addition functions.
#
# Revision 1.5  2002/08/12 18:31:05  wiegand
# Added tan, v3cosangle, u3cosangle, v3scalex
#
# Revision 1.4  2002/07/25 13:20:52  wiegand
# Added max, min, log10 and conversion from RA/DEC to vector
#
# Revision 1.3  2002/07/19 13:28:36  wiegand
# Replace use of Math::Trig with POSIX module.
#
# Revision 1.2  2002/06/18 17:05:28  rwiegand
# Corrected v3rdl for points near poles
#
# Revision 1.1  2002/06/17 22:25:38  rwiegand
# Initial revision
#

use strict;

package Math;

use POSIX qw(isxdigit);


use constant PIo4  => atan2(1, 1);
use constant PIo2  => 2 * PIo4;
use constant PI    => 4 * PIo4;
use constant PIx2  => 2 * PI;


use constant ARC_MIN_d  => 1 / 60;
use constant ARC_SEC_d  => ARC_MIN_d / 60;

use constant ARC_MIN_r  => ARC_MIN_d * PI / 180;
use constant ARC_SEC_r  => ARC_MIN_r / 60;

use constant EPSILON    => 1e-9;


use constant LOGe10     => log(10);



sub signum
{
	my ($x) = @_;
	my $signum;

	if ($x > 0) {
		$signum = 1;
	}
	elsif ($x < 0) {
		$signum = -1;
	}
	else {
		$signum = 0;
	}

	return $signum;
}


sub max (@)
{
	my $max = shift;
	foreach my $x (@_) {
		if ($x > $max) {
			$max = $x;
		}
	}
	return $max;
}


sub min (@)
{
	my $min = shift;
	foreach my $x (@_) {
		if ($x < $min) {
			$min = $x;
		}
	}
	return $min;
}


sub remainder ($$)
{
	my ($num, $den) = @_;
	my $remainder = $num - POSIX::floor($num / $den);
	return $remainder;
}


sub hypot ($$)
{
	my ($x, $y) = @_;

	# sqrt($x^2 + $y ^ 2) without under/overflow
	my $r;
	if (abs($x) > abs($y)) {
		$r = $y / $x;
		$r = abs($x) * sqrt(1 + $r * $r);
	}
	elsif ($y != 0) {
		$r = $x / $y;
		$r = abs($y) * sqrt(1 + $r * $r);
	}
	else {
		$r = 0;
	}
	return $r;
}


sub tan ($)
{
	my ($radians) = @_;
	my $tan = POSIX::tan($radians);
	return $tan;
}


sub toRadians
{
	&degreesToRadians;
}


sub toDegrees
{
	&radiansToDegrees;
}


sub degreesToRadians ($)
{
	my ($degrees) = @_;
	my $radians = PI * $degrees / 180;
	return $radians;
}


sub radiansToDegrees ($)
{
	my ($radians) = @_;
	my $degrees = $radians * 180 / PI;
	return $degrees;
}


sub log10 ($)
{
	my ($in) = @_;
	my $out = log($in) / LOGe10;
	return $out;
}


sub dotProductL ($$$)
{
	my ($u, $v, $length) = @_;
	my $out = 0;
	for (my $i = 0; $i < $length; ++$i) {
		$out += $u->[$i] * $v->[$i];
	}
	return $out;
}


sub dotProduct ($$)
{
	my ($u, $v) = @_;
	my $length = scalar(@{ $u });
	if ($length != scalar(@{ $v })) {
		die("[dotProduct] length mismatch");
	}
	return dotProductL($u, $v, $length);
}


sub v2dot ($$)
{
	my ($u, $v) = @_;
	my $out = $u->[0] * $v->[0]
		+ $u->[1] * $v->[1];
	return $out;
}


sub v2norm2 ($)
{
	my ($v) = @_;
	my $norm2 = $v->[0] * $v->[0] + $v->[1] * $v->[1];
	return $norm2;
}


sub v2norm ($)
{
	my ($u) = @_;
	my $norm2 = v2norm2($u);
	my $norm = sqrt($norm2);
	return $norm;
}


sub u2angle ($$)
{
	my ($u, $v) = @_;
	my $dot = v2dot($u, $v);
	my $angle;
	if (abs($dot) > 1 and abs($dot) < 1+1e-12) {
		if ($dot < 0) {
			$angle = PI;
		}
		else {
			$angle = 0;
		}
	}
	else {
		$angle = POSIX::acos($dot);
	}
	return $angle;
}


sub v2angle ($$)
{
	my ($u, $v) = @_;
	my $dot = v2dot($u, $v);
	my $unorm = v2norm($u);
	my $vnorm = v2norm($v);
	my $angle = POSIX::acos($dot / $unorm / $vnorm);
	return $angle;
}


sub v2xnorm2 ($$)
{
	my ($u, $v) = @_;
	my $tmp;
	my $norm2 = ($tmp = ($u->[0] - $v->[0])) * $tmp
		+ ($tmp = ($u->[1] - $v->[1])) * $tmp
		+ ($tmp = ($u->[2] - $v->[2])) * $tmp;
	return $norm2;
}


sub v2xnorm ($$)
{
	my ($u, $v) = @_;
	my $norm2 = v2xnorm2($u, $v);
	my $norm = sqrt($norm2);
	return $norm;
}


sub v3dot ($$)
{
	my ($u, $v) = @_;
	my $out = $u->[0] * $v->[0]
		+ $u->[1] * $v->[1]
		+ $u->[2] * $v->[2];
	return $out;
}


sub v3norm2 ($)
{
	my ($v) = @_;
	my $norm2 = $v->[0] * $v->[0] + $v->[1] * $v->[1] + $v->[2] * $v->[2];
	return $norm2;
}


sub v3norm ($)
{
	my ($v) = @_;
	my $norm2 = v3norm2($v);
	my $norm = sqrt($norm2);
	return $norm;
}


sub v3normalize ($)
{
	my ($v) = @_;
	my $inv = 1 / v3norm($v);
	foreach my $i (0 .. 2) {
		$v->[$i] *= $inv;
	}
}


sub u3angle ($$)
{
	my ($u, $v) = @_;
	my $angle;
	my $dot = v3dot($u, $v);
	if ($dot > 0.9) {
		# more accurate for small angles
		my $tmp = 0;
		for (my $i = 0; $i < 3; ++$i) {
			my $delta = $u->[$i] - $v->[$i];
			$tmp += $delta * $delta;
		}
		my $d = sqrt($tmp);
		$angle = 2 * POSIX::asin($d / 2);
	}
	else {
		$angle = POSIX::acos($dot);
	}
	return $angle;
}


sub u3cosangle ($$)
{
	my ($u, $v) = @_;
	my $cosangle = v3dot($u, $v);
	return $cosangle;
}


sub v3angle ($$)
{
	my ($u, $v) = @_;
	my $dot = v3dot($u, $v);
	my $unorm = v3norm($u);
	my $vnorm = v3norm($v);
	my $angle = POSIX::acos($dot / $unorm / $vnorm);
	return $angle;
}


sub v3cosangle ($$)
{
	my ($u, $v) = @_;
	my $dot = v3dot($u, $v);
	my $unorm = v3norm($u);
	my $vnorm = v3norm($v);
	my $cosangle = $dot / $unorm / $vnorm;
	return $cosangle;
}


sub v3xcross ($$$)
{
	my ($u, $v, $o) = @_;
	$o->[0] = $u->[1] * $v->[2] - $u->[2] * $v->[1];
	$o->[1] = $u->[2] * $v->[0] - $u->[0] * $v->[2];
	$o->[2] = $u->[0] * $v->[1] - $u->[1] * $v->[0];
	return $o;
}


sub v3cross ($$)
{
	my ($u, $v) = @_;
	my $o = [
		$u->[1] * $v->[2] - $u->[2] * $v->[1],
		$u->[2] * $v->[0] - $u->[0] * $v->[2],
		$u->[0] * $v->[1] - $u->[1] * $v->[0],
	];
	return $o;
}


sub v3xnorm2 ($$)
{
	my ($u, $v) = @_;
	my $tmp;
	my $norm2 = ($tmp = ($u->[0] - $v->[0])) * $tmp
		+ ($tmp = ($u->[1] - $v->[1])) * $tmp
		+ ($tmp = ($u->[2] - $v->[2])) * $tmp;
	return $norm2;
}


sub v3xnorm ($$)
{
	my ($u, $v) = @_;
	my $norm2 = v3xnorm2($u, $v);
	my $norm = sqrt($norm2);
	return $norm;
}


sub rdl2vector ($$$;$)
{
	my ($ra, $dec, $len, $v) = @_;

	if (not UNIVERSAL::isa($v, 'ARRAY')) {
		$v = [ undef, undef, undef ];
	}

	$v->[0] = $len * cos($ra) * cos($dec);
	$v->[1] = $len * sin($ra) * cos($dec);
	$v->[2] = $len * sin($dec);

	return $v;
}


sub rd2unit ($$;$)
{
	my ($ra, $dec, $v) = @_;

	if (not UNIVERSAL::isa($v, 'ARRAY')) {
		$v = [ undef, undef, undef ];
	}

	$v->[0] = cos($ra) * cos($dec);
	$v->[1] = sin($ra) * cos($dec);
	$v->[2] = sin($dec);

	return $v;
}


sub v3rdl ($)
{
	my ($v) = @_;

	my $ra;
	my $dec;

	my $rho2 = $v->[0] * $v->[0] + $v->[1] * $v->[1];
	my $norm = sqrt($rho2 + $v->[2] * $v->[2]);
	if ($norm == 0) {
		return (undef, undef, 0);
	}

	my $rho = sqrt($rho2);
	$dec = POSIX::asin($v->[2] / $norm);

	if ($rho < EPSILON) {
		$ra = 0;
	}
	else {
		my $c = $v->[0] / $rho;
		my $s = $v->[1] / $rho;

		if (abs($s) < EPSILON) {
			$ra = (1 - $c / abs($c)) * PIo2;
		}
		else {
			$ra = 2 * POSIX::atan((1 - $c) / $s);
		}
	}

	while ($ra > PIx2) {
		$ra -= PIx2;
	}

	while ($ra < 0) {
		$ra += PIx2;
	}

	return ($ra, $dec, $norm);
}


sub v3rotate ($$$)
{
	my ($p, $n, $angle) = @_;

	my $s = sin($angle);
	my $c = cos($angle);
	my $c_1 = 1 - $c;

	my $o = [
		($n->[0] * $n->[0] * $c_1 + $c) * $p->[0]
			+ ($n->[0] * $n->[1] * $c_1 - $n->[2] * $s) * $p->[1]
			+ ($n->[0] * $n->[2] * $c_1 + $n->[1] * $s) * $p->[2],

		($n->[0] * $n->[1] * $c_1 + $n->[2] * $s) * $p->[0]
			+ ($n->[1] * $n->[1] * $c_1 + $c) * $p->[1]
			+ ($n->[1] * $n->[2] * $c_1 - $n->[0] * $s) * $p->[2],

		($n->[0] * $n->[2] * $c_1 - $n->[1] * $s) * $p->[0]
			+ ($n->[1] * $n->[2] * $c_1 + $n->[0] * $s) * $p->[1]
			+ ($n->[2] * $n->[2] * $c_1 + $c) * $p->[2],
	];

	return $o;
}


sub v3interpolate ($$$)
{
	my ($va, $vb, $fraction) = @_;
	my $normal = v3cross($va, $vb);
	v3normalize($normal);
	my $angle = v3angle($va, $vb) * $fraction;
	my $o = v3rotate($va, $normal, $angle);
	return $o;
}


sub v3add ($$)
{
	my ($t, $v) = @_;
	$t->[0] += $v->[0];
	$t->[1] += $v->[1];
	$t->[2] += $v->[2];
	return $t;
}


sub v3addx ($$;$)
{
	my ($u, $v, $o) = @_;
	if (not $o) {
		$o = [ undef, undef, undef ];
	}
	$o->[0] = $u->[0] + $v->[0];
	$o->[1] = $u->[1] + $v->[1];
	$o->[2] = $u->[2] + $v->[2];
	return $o;
}


sub v3subtract ($$)
{
	my ($t, $v) = @_;
	$t->[0] -= $v->[0];
	$t->[1] -= $v->[1];
	$t->[2] -= $v->[2];
	return $t;
}


sub v3subtractx ($$;$)
{
	my ($u, $v, $o) = @_;
	if (not $o) {
		$o = [ undef, undef, undef ];
	}
	$o->[0] = $u->[0] - $v->[0];
	$o->[1] = $u->[1] - $v->[1];
	$o->[2] = $u->[2] - $v->[2];
	return $o;
}


sub v3scalex ($$;$)
{
	my ($v, $scale, $o) = @_;
	if (not $o) {
		$o = [ undef, undef, undef ];
	}
	$o->[0] = $v->[0] * $scale;
	$o->[1] = $v->[1] * $scale;
	$o->[2] = $v->[2] * $scale;
	return $o;
}


sub v3kxpy ($$$;$)
{
	my ($k, $x, $y, $o) = @_;
	if (not $o) {
		$o = [ undef, undef, undef ];
	}
	$o->[0] = $k * $x->[0] + $y->[0];
	$o->[1] = $k * $x->[1] + $y->[1];
	$o->[2] = $k * $x->[2] + $y->[2];
	return $o;
}


sub v3outerx ($$;$)
{
	my ($u, $v, $o) = @_;

	if (not $o) {
		$o = [
			[ undef, undef, undef ],
			[ undef, undef, undef ],
			[ undef, undef, undef ],
		];
	}

	$o->[0][0] = $u->[0] * $v->[0];
	$o->[0][1] = $u->[0] * $v->[1];
	$o->[0][2] = $u->[0] * $v->[2];

	$o->[1][0] = $u->[1] * $v->[0];
	$o->[1][1] = $u->[1] * $v->[1];
	$o->[1][2] = $u->[1] * $v->[2];

	$o->[2][0] = $u->[2] * $v->[0];
	$o->[2][1] = $u->[2] * $v->[1];
	$o->[2][2] = $u->[2] * $v->[2];

	return $o;
}



sub madd ($$$$;$)
{
	my ($rows, $cols, $A, $B, $O) = @_;

	if (not UNIVERSAL::isa($O, 'ARRAY')) {
		$O = [ ];
		for (my $i = 0; $i < $rows; ++$i) {
			$O->[$i] = [ (undef) x $cols ];
		}
	}

	for (my $i = 0; $i < $rows; ++$i) {
		for (my $j = 0; $j < $rows; ++$j) {
			$O->[$i][$j] = $A->[$i][$j] + $B->[$i][$j];
		}
	}

	return $O;
}


sub rmv3
{
	my ($rm, $v) = @_;

	my $o = [ 0, 0, 0 ];

	for (my $i = 0; $i < 3; ++$i) {

		my $q = 0;

		for (my $j = 0; $j < 3; ++$j) {
			$q += $rm->[$i][$j] * $v->[$j];
		}

		$o->[$i] = $q;
	}

	return $o;
}



sub createSystem
{
	my ($r) = @_;

	my $i1 = abs($r->[1]) > abs($r->[0]) ? 1 : 0;
	$i1 = abs($r->[2]) > abs($r->[$i1]) ? 2 : $i1;

	my @s = (0, 0, 0);
	$s[$i1] = 1;

	my $u1 = v3cross($r, \@s);
	v3normalize($u1);

	my $u2 = v3cross($r, $u1);
	v3normalize($u2);

	return ($u1, $u2);
}


sub v3rm
{
	my ($v, $rm) = @_;
	my $p = [ 0, 0, 0 ];
	for (my $i = 0; $i < 3; ++$i) {
		my $y = 0;
		for (my $j = 0; $j < 3; ++$j) {
			$y += $rm->[$i][$j] * $v->[$j];
		}
		$p->[$i] = $y;
	}
	return $p;
}


sub v3q
{
	my ($v, $q) = @_;
	my $rm = q2rm($q);
	return v3rm($v, $rm);
}


sub q2rm
{
	my ($q) = @_;

	my $rm = [ [ 0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ];

	# diagonal elements
	$rm->[0][0] = $q->[0] * $q->[0] - $q->[1] * $q->[1]
					- $q->[2] * $q->[2] + $q->[3] * $q->[3];
	$rm->[1][1] = -$q->[0] * $q->[0] + $q->[1] * $q->[1]
					- $q->[2] * $q->[2] + $q->[3] * $q->[3];
	$rm->[2][2] = -$q->[0] * $q->[0] - $q->[1] * $q->[1]
					+ $q->[2] * $q->[2] + $q->[3] * $q->[3];

	# off diagonal
	$rm->[0][1] = 2.0 * ($q->[0] * $q->[1] + $q->[2] * $q->[3]);
	$rm->[1][0] = 2.0 * ($q->[0] * $q->[1] - $q->[2] * $q->[3]);

	$rm->[0][2] = 2.0 * ($q->[0] * $q->[2] - $q->[1] * $q->[3]);
	$rm->[2][0] = 2.0 * ($q->[0] * $q->[2] + $q->[1] * $q->[3]);

	$rm->[1][2] = 2.0 * ($q->[1] * $q->[2] + $q->[0] * $q->[3]);
	$rm->[2][1] = 2.0 * ($q->[1] * $q->[2] - $q->[0] * $q->[3]);

	return $rm;
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


sub getQuatOfChange
{
	my ($q1, $q2) = @_;
	my $q1Inv = [ -$q1->[0], -$q1->[1], -$q1->[2], $q1->[3] ];
	my $qChange = productOfQuats($q1Inv, $q2);
	return $qChange;
}


sub mmult
{
	my ($m, $n) = @_;
	my $mcols = @{ $m->[0] };
	my $nrows = @{ $n };
	if ($mcols != $nrows) {
		die('mmult: size mismatch');
	}

	my $mrows = @{ $m };
	my $ncols = @{ $n->[0] };
	my @o;

	for (my $i = 0; $i < $mrows; ++$i) {
		my $mi = $m->[$i];
		my @orow;
		for (my $j = 0; $j < $ncols; ++$j) {
			my $tmp = 0;
			for (my $k = 0; $k < $mcols; ++$k) {
				$tmp += $mi->[$k] * $n->[$k][$j];
			}
			push(@orow, $tmp);
		}
		push(@o, \@orow);
	}

	return \@o;
}


1;

