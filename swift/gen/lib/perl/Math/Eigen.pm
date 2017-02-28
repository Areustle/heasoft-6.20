
# $Source: /headas/headas/swift/gen/lib/perl/Math/Eigen.pm,v $
# $Revision: 1.4 $
# $Date: 2002/08/16 20:02:27 $
#
#	Trivially modified Jama source
#	see http://math.nist.gov/javanumerics/jama
#
# $Log: Eigen.pm,v $
# Revision 1.4  2002/08/16 20:02:27  wiegand
# Replaced pow(base, exponent) with base ** exponent.  Save size of matrix
# in key n.
#
# Revision 1.3  2002/08/16 19:36:19  wiegand
# Corrected compilation errors
#
# Revision 1.2  2002/08/16 19:21:08  wiegand
# Formatted with perltidy -sbl
#

use strict;

package Math::Eigen;

# Symmetric Householder reduction to tridiagonal form.

sub tred2
{
    my ($self) = @_;

    # This is derived from the Algol procedures tred2 by
    # Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    # Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    # Fortran subroutine in EISPACK.

    my $n = $self->{n};
    my $d = $self->{d};
    my $e = $self->{e};
    my $V = $self->{V};

    for ( my $j = 0 ; $j < $n ; $j++ ) {
        $d->[$j] = $V->[ $n - 1 ][$j];
    }

    # Householder reduction to tridiagonal form.

    for ( my $i = $n - 1 ; $i > 0 ; $i-- ) {

        # Scale to avoid under/overflow.

        my $scale = 0.0;
        my $h     = 0.0;
        for ( my $k = 0 ; $k < $i ; $k++ ) {
            $scale = $scale + abs( $d->[$k] );
        }
        if ( $scale == 0.0 ) {
            $e->[$i] = $d->[ $i - 1 ];
            for ( my $j = 0 ; $j < $i ; $j++ ) {
                $d->[$j] = $V->[ $i - 1 ][$j];
                $V->[$i][$j] = 0.0;
                $V->[$j][$i] = 0.0;
            }
        }
        else {

            # Generate Householder vector.

            for ( my $k = 0 ; $k < $i ; $k++ ) {
                $d->[$k] /= $scale;
                $h += $d->[$k] * $d->[$k];
            }
            my $f = $d->[ $i - 1 ];
            my $g = sqrt($h);
            if ( $f > 0 ) {
                $g = -$g;
            }
            $e->[$i] = $scale * $g;
            $h = $h - $f * $g;
            $d->[ $i - 1 ] = $f - $g;
            for ( my $j = 0 ; $j < $i ; $j++ ) {
                $e->[$j] = 0.0;
            }

            # Apply similarity transformation to remaining columns.

            for ( my $j = 0 ; $j < $i ; $j++ ) {
                $f = $d->[$j];
                $V->[$j][$i] = $f;
                $g = $e->[$j] + $V->[$j][$j] * $f;
                for ( my $k = $j + 1 ; $k <= $i - 1 ; $k++ ) {
                    $g += $V->[$k][$j] * $d->[$k];
                    $e->[$k] += $V->[$k][$j] * $f;
                }
                $e->[$j] = $g;
            }
            $f = 0.0;
            for ( my $j = 0 ; $j < $i ; $j++ ) {
                $e->[$j] /= $h;
                $f += $e->[$j] * $d->[$j];
            }
            my $hh = $f / ( $h + $h );
            for ( my $j = 0 ; $j < $i ; $j++ ) {
                $e->[$j] -= $hh * $d->[$j];
            }
            for ( my $j = 0 ; $j < $i ; $j++ ) {
                $f = $d->[$j];
                $g = $e->[$j];
                for ( my $k = $j ; $k <= $i - 1 ; $k++ ) {
                    $V->[$k][$j] -= ( $f * $e->[$k] + $g * $d->[$k] );
                }
                $d->[$j] = $V->[ $i - 1 ][$j];
                $V->[$i][$j] = 0.0;
            }
        }
        $d->[$i] = $h;
    }

    # Accumulate transformations.

    for ( my $i = 0 ; $i < $n - 1 ; $i++ ) {
        $V->[ $n - 1 ][$i] = $V->[$i][$i];
        $V->[$i][$i] = 1.0;
        my $h = $d->[ $i + 1 ];
        if ( $h != 0.0 ) {
            for ( my $k = 0 ; $k <= $i ; $k++ ) {
                $d->[$k] = $V->[$k][ $i + 1 ] / $h;
            }
            for ( my $j = 0 ; $j <= $i ; $j++ ) {
                my $g = 0.0;
                for ( my $k = 0 ; $k <= $i ; $k++ ) {
                    $g += $V->[$k][ $i + 1 ] * $V->[$k][$j];
                }
                for ( my $k = 0 ; $k <= $i ; $k++ ) {
                    $V->[$k][$j] -= $g * $d->[$k];
                }
            }
        }
        for ( my $k = 0 ; $k <= $i ; $k++ ) {
            $V->[$k][ $i + 1 ] = 0.0;
        }
    }
    for ( my $j = 0 ; $j < $n ; $j++ ) {
        $d->[$j] = $V->[ $n - 1 ][$j];
        $V->[ $n - 1 ][$j] = 0.0;
    }
    $V->[ $n - 1 ][ $n - 1 ] = 1.0;
    $e->[0] = 0.0;
}

#  tridiagonal QL algorithm.

sub tql2
{
    my ($self) = @_;

    # This is derived from the Algol procedures tql2, by
    # Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    # Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    # Fortran subroutine in EISPACK.

    my $n = $self->{n};
    my $d = $self->{d};
    my $e = $self->{e};
    my $V = $self->{V};

    for ( my $i = 1 ; $i < $n ; $i++ ) {
        $e->[ $i - 1 ] = $e->[$i];
    }
    $e->[ $n - 1 ] = 0.0;

    my $f    = 0.0;
    my $tst1 = 0.0;
    my $eps  = 2.0 ** -52.0;
    for ( my $l = 0 ; $l < $n ; $l++ ) {

        # Find small subdiagonal element

        $tst1 = Math::max( $tst1, abs( $d->[$l] ) + abs( $e->[$l] ) );
        my $m = $l;
        while ( $m < $n ) {
            if ( abs( $e->[$m] ) <= $eps * $tst1 ) {
                last;
            }
            $m++;
        }

        # If $m == $l, $d->[$l] is an eigenvalue,
        # otherwise, iterate.

        if ( $m > $l ) {
            my $iter = 0;
            do {
                $iter = $iter + 1;    # Could check iteration count here.

                # Compute implicit shift

                my $g = $d->[$l];
                my $p = ( $d->[ $l + 1 ] - $g ) / ( 2.0 * $e->[$l] );
                my $r = Math::hypot( $p, 1.0 );
                if ( $p < 0 ) {
                    $r = -$r;
                }
                $d->[$l] = $e->[$l] / ( $p + $r );
                $d->[ $l + 1 ] = $e->[$l] * ( $p + $r );
                my $dl1 = $d->[ $l + 1 ];
                my $h   = $g - $d->[$l];
                for ( my $i = $l + 2 ; $i < $n ; $i++ ) {
                    $d->[$i] -= $h;
                }
                $f = $f + $h;

                # Implicit QL transformation.

                $p = $d->[$m];
                my $c   = 1.0;
                my $c2  = $c;
                my $c3  = $c;
                my $el1 = $e->[ $l + 1 ];
                my $s   = 0.0;
                my $s2  = 0.0;
                for ( my $i = $m - 1 ; $i >= $l ; $i-- ) {
                    $c3 = $c2;
                    $c2 = $c;
                    $s2 = $s;
                    $g  = $c * $e->[$i];
                    $h  = $c * $p;
                    $r  = Math::hypot( $p, $e->[$i] );
                    $e->[ $i + 1 ] = $s * $r;
                    $s = $e->[$i] / $r;
                    $c = $p / $r;
                    $p = $c * $d->[$i] - $s * $g;
                    $d->[ $i + 1 ] = $h + $s * ( $c * $g + $s * $d->[$i] );

                    # Accumulate transformation.

                    for ( my $k = 0 ; $k < $n ; $k++ ) {
                        $h = $V->[$k][ $i + 1 ];
                        $V->[$k][ $i + 1 ] = $s * $V->[$k][$i] + $c * $h;
                        $V->[$k][$i] = $c * $V->[$k][$i] - $s * $h;
                    }
                }
                $p = -$s * $s2 * $c3 * $el1 * $e->[$l] / $dl1;
                $e->[$l] = $s * $p;
                $d->[$l] = $c * $p;

                # Check for convergence.

            } while ( abs( $e->[$l] ) > $eps * $tst1 );
        }
        $d->[$l] = $d->[$l] + $f;
        $e->[$l] = 0.0;
    }

    # Sort eigenvalues and corresponding vectors.

    for ( my $i = 0 ; $i < $n - 1 ; $i++ ) {
        my $k = $i;
        my $p = $d->[$i];
        for ( my $j = $i + 1 ; $j < $n ; $j++ ) {
            if ( $d->[$j] < $p ) {
                $k = $j;
                $p = $d->[$j];
            }
        }
        if ( $k != $i ) {
            $d->[$k] = $d->[$i];
            $d->[$i] = $p;
            for ( my $j = 0 ; $j < $n ; $j++ ) {
                $p = $V->[$j][$i];
                $V->[$j][$i] = $V->[$j][$k];
                $V->[$j][$k] = $p;
            }
        }
    }
}

# Nonsymmetric reduction to Hessenberg form.

sub orthes
{
    my ($self) = @_;

    # This is derived from the Algol procedures orthes and ortran,
    # by Martin and Wilkinson, Handbook for Auto. Comp.,
    # Vol.ii-Linear Algebra, and the corresponding
    # Fortran subroutines in EISPACK.

    my $n   = $self->{n};
    my $ort = $self->{ort};
    my $H   = $self->{H};
    my $V   = $self->{V};

    my $low  = 0;
    my $high = $n - 1;

    for ( my $m = $low + 1 ; $m <= $high - 1 ; $m++ ) {

        # Scale column.

        my $scale = 0.0;
        for ( my $i = $m ; $i <= $high ; $i++ ) {
            $scale = $scale + abs( $H->[$i][ $m - 1 ] );
        }
        if ( $scale != 0.0 ) {

            # Compute Householder transformation.

            my $h = 0.0;
            for ( my $i = $high ; $i >= $m ; $i-- ) {
                $ort->[$i] = $H->[$i][ $m - 1 ] / $scale;
                $h += $ort->[$i] * $ort->[$i];
            }
            my $g = sqrt($h);
            if ( $ort->[$m] > 0 ) {
                $g = -$g;
            }
            $h = $h - $ort->[$m] * $g;
            $ort->[$m] = $ort->[$m] - $g;

            # Apply Householder similarity transformation
            # $H = (I-u*u'/$h)*$H->*(I-u*u')/$h)

            for ( my $j = $m ; $j < $n ; $j++ ) {
                my $f = 0.0;
                for ( my $i = $high ; $i >= $m ; $i-- ) {
                    $f += $ort->[$i] * $H->[$i][$j];
                }
                $f = $f / $h;
                for ( my $i = $m ; $i <= $high ; $i++ ) {
                    $H->[$i][$j] -= $f * $ort->[$i];
                }
            }

            for ( my $i = 0 ; $i <= $high ; $i++ ) {
                my $f = 0.0;
                for ( my $j = $high ; $j >= $m ; $j-- ) {
                    $f += $ort->[$j] * $H->[$i][$j];
                }
                $f = $f / $h;
                for ( my $j = $m ; $j <= $high ; $j++ ) {
                    $H->[$i][$j] -= $f * $ort->[$j];
                }
            }
            $ort->[$m] = $scale * $ort->[$m];
            $H->[$m][ $m - 1 ] = $scale * $g;
        }
    }

    # Accumulate transformations (Algol'$s ortran).

    for ( my $i = 0 ; $i < $n ; $i++ ) {
        for ( my $j = 0 ; $j < $n ; $j++ ) {
            $V->[$i][$j] = ( $i == $j ? 1.0 : 0.0 );
        }
    }

    for ( my $m = $high - 1 ; $m >= $low + 1 ; $m-- ) {
        if ( $H->[$m][ $m - 1 ] != 0.0 ) {
            for ( my $i = $m + 1 ; $i <= $high ; $i++ ) {
                $ort->[$i] = $H->[$i][ $m - 1 ];
            }
            for ( my $j = $m ; $j <= $high ; $j++ ) {
                my $g = 0.0;
                for ( my $i = $m ; $i <= $high ; $i++ ) {
                    $g += $ort->[$i] * $V->[$i][$j];
                }

                # Double division avoids possible underflow
                $g = ( $g / $ort->[$m] ) / $H->[$m][ $m - 1 ];
                for ( my $i = $m ; $i <= $high ; $i++ ) {
                    $V->[$i][$j] += $g * $ort->[$i];
                }
            }
        }
    }
}

# Complex scalar division.

sub cdiv
{
    my ( $xr, $xi, $yr, $yi ) = @_;

    my ( $r, $d, $cdivr, $cdivi );

    if ( abs($yr) > abs($yi) ) {
        $r     = $yi / $yr;
        $d     = $yr + $r * $yi;
        $cdivr = ( $xr + $r * $xi ) / $d;
        $cdivi = ( $xi - $r * $xr ) / $d;
    }
    else {
        $r     = $yr / $yi;
        $d     = $yi + $r * $yr;
        $cdivr = ( $r * $xr + $xi ) / $d;
        $cdivi = ( $r * $xi - $xr ) / $d;
    }

    return ( $cdivr, $cdivi );
}

# Nonsymmetric reduction from Hessenberg to real Schur form.

sub hqr2
{
    my ($self) = @_;

    # This is derived from the Algol procedure hqr2,
    # by Martin and Wilkinson, Handbook for Auto. Comp.,
    # Vol.ii-Linear Algebra, and the corresponding
    # Fortran subroutine in EISPACK.

    # Initialize

    my $nn = $self->{n};
    my $d  = $self->{d};
    my $e  = $self->{e};
    my $H  = $self->{H};
    my $V  = $self->{V};

    my $n       = $nn - 1;
    my $low     = 0;
    my $high    = $nn - 1;
    my $eps     = 2.0 ** -52.0;
    my $exshift = 0.0;
    my ( $p, $q, $r, $s, $z, $t, $w, $x, $y ) = ( 0, 0, 0, 0, 0 );

    # Store roots isolated by balanc and compute matrix $norm

    my $norm = 0.0;
    for ( my $i = 0 ; $i < $nn ; $i++ ) {
        if ( $i < $low | $i > $high ) {
            $d->[$i] = $H->[$i][$i];
            $e->[$i] = 0.0;
        }
        for ( my $j = Math::max( $i - 1, 0 ) ; $j < $nn ; $j++ ) {
            $norm = $norm + abs( $H->[$i][$j] );
        }
    }

    # Outer loop over eigenvalue index

    my $iter = 0;
    while ( $n >= $low ) {

        # Look for single small sub-diagonal element

        my $l = $n;
        while ( $l > $low ) {
            $s = abs( $H->[ $l - 1 ][ $l - 1 ] ) + abs( $H->[$l][$l] );
            if ( $s == 0.0 ) {
                $s = $norm;
            }
            if ( abs( $H->[$l][ $l - 1 ] ) < $eps * $s ) {
                last;
            }
            $l--;
        }

        # Check for convergence
        # One root found

        if ( $l == $n ) {
            $H->[$n][$n] = $H->[$n][$n] + $exshift;
            $d->[$n] = $H->[$n][$n];
            $e->[$n] = 0.0;
            $n--;
            $iter = 0;

            # Two roots found

        }
        elsif ( $l == $n - 1 ) {
            $w = $H->[$n][ $n - 1 ] * $H->[ $n - 1 ][$n];
            $p = ( $H->[ $n - 1 ][ $n - 1 ] - $H->[$n][$n] ) / 2.0;
            $q = $p * $p + $w;
            $z = sqrt( abs($q) );
            $H->[$n][$n] = $H->[$n][$n] + $exshift;
            $H->[ $n - 1 ][ $n - 1 ] = $H->[ $n - 1 ][ $n - 1 ] + $exshift;
            $x = $H->[$n][$n];

            # Real pair

            if ( $q >= 0 ) {
                if ( $p >= 0 ) {
                    $z = $p + $z;
                }
                else {
                    $z = $p - $z;
                }
                $d->[ $n - 1 ] = $x + $z;
                $d->[$n] = $d->[ $n - 1 ];
                if ( $z != 0.0 ) {
                    $d->[$n] = $x - $w / $z;
                }
                $e->[ $n - 1 ] = 0.0;
                $e->[$n] = 0.0;
                $x = $H->[$n][ $n - 1 ];
                $s = abs($x) + abs($z);
                $p = $x / $s;
                $q = $z / $s;
                $r = sqrt( $p * $p + $q * $q );
                $p = $p / $r;
                $q = $q / $r;

                # Row modification

                for ( my $j = $n - 1 ; $j < $nn ; $j++ ) {
                    $z = $H->[ $n - 1 ][$j];
                    $H->[ $n - 1 ][$j] = $q * $z + $p * $H->[$n][$j];
                    $H->[$n][$j] = $q * $H->[$n][$j] - $p * $z;
                }

                # Column modification

                for ( my $i = 0 ; $i <= $n ; $i++ ) {
                    $z = $H->[$i][ $n - 1 ];
                    $H->[$i][ $n - 1 ] = $q * $z + $p * $H->[$i][$n];
                    $H->[$i][$n] = $q * $H->[$i][$n] - $p * $z;
                }

                # Accumulate transformations

                for ( my $i = $low ; $i <= $high ; $i++ ) {
                    $z = $V->[$i][ $n - 1 ];
                    $V->[$i][ $n - 1 ] = $q * $z + $p * $V->[$i][$n];
                    $V->[$i][$n] = $q * $V->[$i][$n] - $p * $z;
                }

                # Complex pair

            }
            else {
                $d->[ $n - 1 ] = $x + $p;
                $d->[$n] = $x + $p;
                $e->[ $n - 1 ] = $z;
                $e->[$n] = -$z;
            }
            $n    = $n - 2;
            $iter = 0;

            # No convergence yet

        }
        else {

            # Form shift

            $x = $H->[$n][$n];
            $y = 0.0;
            $w = 0.0;
            if ( $l < $n ) {
                $y = $H->[ $n - 1 ][ $n - 1 ];
                $w = $H->[$n][ $n - 1 ] * $H->[ $n - 1 ][$n];
            }

            # Wilkinson's original ad hoc shift

            if ( $iter == 10 ) {
                $exshift += $x;
                for ( my $i = $low ; $i <= $n ; $i++ ) {
                    $H->[$i][$i] -= $x;
                }
                $s =
                  abs( $H->[$n][ $n - 1 ] ) + abs( $H->[ $n - 1 ][ $n - 2 ] );
                $x = $y = 0.75 * $s;
                $w = -0.4375 * $s * $s;
            }

            # MATLAB'$s new ad hoc shift

            if ( $iter == 30 ) {
                $s = ( $y - $x ) / 2.0;
                $s = $s * $s + $w;
                if ( $s > 0 ) {
                    $s = sqrt($s);
                    if ( $y < $x ) {
                        $s = -$s;
                    }
                    $s = $x - $w / ( ( $y - $x ) / 2.0 + $s );
                    for ( my $i = $low ; $i <= $n ; $i++ ) {
                        $H->[$i][$i] -= $s;
                    }
                    $exshift += $s;
                    $x = $y = $w = 0.964;
                }
            }

            $iter = $iter + 1;    # (Could check iteration count here.)

            # Look for two consecutive small sub-diagonal elements

            my $m = $n - 2;
            while ( $m >= $l ) {
                $z = $H->[$m][$m];
                $r = $x - $z;
                $s = $y - $z;
                $p = ( $r * $s - $w ) / $H->[ $m + 1 ][$m] + $H->[$m][ $m + 1 ];
                $q = $H->[ $m + 1 ][ $m + 1 ] - $z - $r - $s;
                $r = $H->[ $m + 2 ][ $m + 1 ];
                $s = abs($p) + abs($q) + abs($r);
                $p = $p / $s;
                $q = $q / $s;
                $r = $r / $s;
                if ( $m == $l ) {
                    last;
                }
                if (
                    abs( $H->[$m][ $m - 1 ] ) * ( abs($q) + abs($r) ) < $eps * (
                        abs($p) * (
                            abs( $H->[ $m - 1 ][ $m - 1 ] ) + abs($z) +
                              abs( $H->[ $m + 1 ][ $m + 1 ] )
                        )
                    )
                  )
                {
                    last;
                }
                $m--;
            }

            for ( my $i = $m + 2 ; $i <= $n ; $i++ ) {
                $H->[$i][ $i - 2 ] = 0.0;
                if ( $i > $m + 2 ) {
                    $H->[$i][ $i - 3 ] = 0.0;
                }
            }

            # Double QR step involving rows $l:$n and columns $m:$n

            for ( my $k = $m ; $k <= $n - 1 ; $k++ ) {
                my $notlast = ( $k != $n - 1 );
                if ( $k != $m ) {
                    $p = $H->[$k][ $k - 1 ];
                    $q = $H->[ $k + 1 ][ $k - 1 ];
                    $r = ( $notlast ? $H->[ $k + 2 ][ $k - 1 ] : 0.0 );
                    $x = abs($p) + abs($q) + abs($r);
                    if ( $x != 0.0 ) {
                        $p = $p / $x;
                        $q = $q / $x;
                        $r = $r / $x;
                    }
                }
                if ( $x == 0.0 ) {
                    last;
                }
                $s = sqrt( $p * $p + $q * $q + $r * $r );
                if ( $p < 0 ) {
                    $s = -$s;
                }
                if ( $s != 0 ) {
                    if ( $k != $m ) {
                        $H->[$k][ $k - 1 ] = -$s * $x;
                    }
                    elsif ( $l != $m ) {
                        $H->[$k][ $k - 1 ] = -$H->[$k][ $k - 1 ];
                    }
                    $p = $p + $s;
                    $x = $p / $s;
                    $y = $q / $s;
                    $z = $r / $s;
                    $q = $q / $p;
                    $r = $r / $p;

                    # Row modification

                    for ( my $j = $k ; $j < $nn ; $j++ ) {
                        $p = $H->[$k][$j] + $q * $H->[ $k + 1 ][$j];
                        if ($notlast) {
                            $p = $p + $r * $H->[ $k + 2 ][$j];
                            $H->[ $k + 2 ][$j] = $H->[ $k + 2 ][$j] - $p * $z;
                        }
                        $H->[$k][$j] = $H->[$k][$j] - $p * $x;
                        $H->[ $k + 1 ][$j] = $H->[ $k + 1 ][$j] - $p * $y;
                    }

                    # Column modification

                    for ( my $i = 0 ; $i <= Math::min( $n, $k + 3 ) ; $i++ ) {
                        $p = $x * $H->[$i][$k] + $y * $H->[$i][ $k + 1 ];
                        if ($notlast) {
                            $p = $p + $z * $H->[$i][ $k + 2 ];
                            $H->[$i][ $k + 2 ] = $H->[$i][ $k + 2 ] - $p * $r;
                        }
                        $H->[$i][$k] = $H->[$i][$k] - $p;
                        $H->[$i][ $k + 1 ] = $H->[$i][ $k + 1 ] - $p * $q;
                    }

                    # Accumulate transformations

                    for ( my $i = $low ; $i <= $high ; $i++ ) {
                        $p = $x * $V->[$i][$k] + $y * $V->[$i][ $k + 1 ];
                        if ($notlast) {
                            $p = $p + $z * $V->[$i][ $k + 2 ];
                            $V->[$i][ $k + 2 ] = $V->[$i][ $k + 2 ] - $p * $r;
                        }
                        $V->[$i][$k] = $V->[$i][$k] - $p;
                        $V->[$i][ $k + 1 ] = $V->[$i][ $k + 1 ] - $p * $q;
                    }
                }    # ($s != 0)
            }    # $k loop
        }    # check convergence
    }    # while ($n >= $low)

    # Backsubstitute to find vectors of upper triangular form

    if ( $norm == 0.0 ) {
        return;
    }

    for ( $n = $nn - 1 ; $n >= 0 ; $n-- ) {
        $p = $d->[$n];
        $q = $e->[$n];

        # Real vector

        if ( $q == 0 ) {
            my $l = $n;
            $H->[$n][$n] = 1.0;
            for ( my $i = $n - 1 ; $i >= 0 ; $i-- ) {
                $w = $H->[$i][$i] - $p;
                $r = 0.0;
                for ( my $j = $l ; $j <= $n ; $j++ ) {
                    $r = $r + $H->[$i][$j] * $H->[$j][$n];
                }
                if ( $e->[$i] < 0.0 ) {
                    $z = $w;
                    $s = $r;
                }
                else {
                    $l = $i;
                    if ( $e->[$i] == 0.0 ) {
                        if ( $w != 0.0 ) {
                            $H->[$i][$n] = -$r / $w;
                        }
                        else {
                            $H->[$i][$n] = -$r / ( $eps * $norm );
                        }

                        # Solve real equations

                    }
                    else {
                        $x = $H->[$i][ $i + 1 ];
                        $y = $H->[ $i + 1 ][$i];
                        $q =
                          ( $d->[$i] - $p ) * ( $d->[$i] - $p ) + $e->[$i] *
                          $e->[$i];
                        $t = ( $x * $s - $z * $r ) / $q;
                        $H->[$i][$n] = $t;
                        if ( abs($x) > abs($z) ) {
                            $H->[ $i + 1 ][$n] = ( -$r - $w * $t ) / $x;
                        }
                        else {
                            $H->[ $i + 1 ][$n] = ( -$s - $y * $t ) / $z;
                        }
                    }

                    # Overflow control

                    $t = abs( $H->[$i][$n] );
                    if ( ( $eps * $t ) * $t > 1 ) {
                        for ( my $j = $i ; $j <= $n ; $j++ ) {
                            $H->[$j][$n] = $H->[$j][$n] / $t;
                        }
                    }
                }
            }

            # Complex vector

        }
        elsif ( $q < 0 ) {
            my $l = $n - 1;

            # Last vector component imaginary so matrix is triangular

            if ( abs( $H->[$n][ $n - 1 ] ) > abs( $H->[ $n - 1 ][$n] ) ) {
                $H->[ $n - 1 ][ $n - 1 ] = $q / $H->[$n][ $n - 1 ];
                $H->[ $n - 1 ][$n] =
                  -( $H->[$n][$n] - $p ) / $H->[$n][ $n - 1 ];
            }
            else {
                my ( $cr, $ci ) = cdiv(
                    0.0,
                    -$H->[ $n - 1 ][$n],
                    $H->[ $n - 1 ][ $n - 1 ] - $p, $q
                );
                $H->[ $n - 1 ][ $n - 1 ] = $cr;
                $H->[ $n - 1 ][$n] = $ci;
            }
            $H->[$n][ $n - 1 ] = 0.0;
            $H->[$n][$n] = 1.0;
            for ( my $i = $n - 2 ; $i >= 0 ; $i-- ) {
                my ( $ra, $sa, $vr, $vi );
                $ra = 0.0;
                $sa = 0.0;
                for ( my $j = $l ; $j <= $n ; $j++ ) {
                    $ra = $ra + $H->[$i][$j] * $H->[$j][ $n - 1 ];
                    $sa = $sa + $H->[$i][$j] * $H->[$j][$n];
                }
                $w = $H->[$i][$i] - $p;

                if ( $e->[$i] < 0.0 ) {
                    $z = $w;
                    $r = $ra;
                    $s = $sa;
                }
                else {
                    $l = $i;
                    if ( $e->[$i] == 0 ) {
                        my ( $cr, $ci ) = cdiv( -$ra, -$sa, $w, $q );
                        $H->[$i][ $n - 1 ] = $cr;
                        $H->[$i][$n] = $ci;
                    }
                    else {

                        # Solve complex equations

                        $x  = $H->[$i][ $i + 1 ];
                        $y  = $H->[ $i + 1 ][$i];
                        $vr =
                          ( $d->[$i] - $p ) * ( $d->[$i] - $p ) + $e->[$i] *
                          $e->[$i] - $q * $q;
                        $vi = ( $d->[$i] - $p ) * 2.0 * $q;
                        if ( $vr == 0.0 & $vi == 0.0 ) {
                            $vr =
                              $eps * $norm * (
                                abs($w) + abs($q) + abs($x) + abs($y) + abs($z)
                              );
                        }
                        my ( $cr, $ci ) = cdiv(
                            $x * $r - $z * $ra + $q * $sa,
                            $x * $s - $z * $sa - $q * $ra,
                            $vr, $vi
                        );
                        $H->[$i][ $n - 1 ] = $cr;
                        $H->[$i][$n] = $ci;
                        if ( abs($x) > ( abs($z) + abs($q) ) ) {
                            $H->[ $i + 1 ][ $n - 1 ] =
                              ( -$ra - $w * $H->[$i][ $n - 1 ] + $q *
                                  $H->[$i][$n] ) / $x;
                            $H->[ $i + 1 ][$n] =
                              ( -$sa - $w * $H->[$i][$n] - $q *
                                  $H->[$i][ $n - 1 ] ) / $x;
                        }
                        else {
                            my ( $cr, $ci ) = cdiv(
                                -$r - $y * $H->[$i][ $n - 1 ],
                                -$s - $y * $H->[$i][$n],
                                $z, $q
                            );
                            $H->[ $i + 1 ][ $n - 1 ] = $cr;
                            $H->[ $i + 1 ][$n] = $ci;
                        }
                    }

                    # Overflow control

                    $t = Math::max( abs( $H->[$i][ $n - 1 ] ),
                        abs( $H->[$i][$n] ) );
                    if ( ( $eps * $t ) * $t > 1 ) {
                        for ( my $j = $i ; $j <= $n ; $j++ ) {
                            $H->[$j][ $n - 1 ] = $H->[$j][ $n - 1 ] / $t;
                            $H->[$j][$n] = $H->[$j][$n] / $t;
                        }
                    }
                }
            }
        }
    }

    # Vectors of isolated roots

    for ( my $i = 0 ; $i < $nn ; $i++ ) {
        if ( $i < $low | $i > $high ) {
            for ( my $j = $i ; $j < $nn ; $j++ ) {
                $V->[$i][$j] = $H->[$i][$j];
            }
        }
    }

    # Back transformation to get eigenvectors of original matrix

    for ( my $j = $nn - 1 ; $j >= $low ; $j-- ) {
        for ( my $i = $low ; $i <= $high ; $i++ ) {
            $z = 0.0;
            for ( my $k = $low ; $k <= Math::min( $j, $high ) ; $k++ ) {
                $z = $z + $V->[$i][$k] * $H->[$k][$j];
            }
            $V->[$i][$j] = $z;
        }
    }
}

sub new
{
    my ( $class, %args ) = @_;

    my $A = $args{matrix};

    my $self = bless( {}, $class );

    my $n = $self->{n} = scalar( @{ $A->[0] } );
    my $V = $self->{V} = [];
    my $d = $self->{d} = [];
    my $e = $self->{e} = [];

    my $issymmetric = 1;
    for ( my $j = 0 ; ( $j < $n ) & $issymmetric ; $j++ ) {
        for ( my $i = 0 ; ( $i < $n ) & $issymmetric ; $i++ ) {
            $issymmetric = ( $A->[$i][$j] == $A->[$j][$i] );
        }
    }

    if ($issymmetric) {
        for ( my $i = 0 ; $i < $n ; $i++ ) {
            for ( my $j = 0 ; $j < $n ; $j++ ) {
                $V->[$i][$j] = $A->[$i][$j];
            }
        }

        # Tridiagonalize.
        $self->tred2();

        # Diagonalize.
        $self->tql2();

    }
    else {
        my $H   = $self->{H}   = [];
        my $ort = $self->{ort} = [];

        for ( my $j = 0 ; $j < $n ; $j++ ) {
            for ( my $i = 0 ; $i < $n ; $i++ ) {
                $H->[$i][$j] = $A->[$i][$j];
            }
        }

        # Reduce to Hessenberg form.
        $self->orthes();

        # Reduce Hessenberg to real Schur form.
        $self->hqr2();
    }

	return $self;
}

sub getV
{
    return shift->{V};
}

sub getEigenvectorMatrix
{
    &getV;
}

sub getRealEigenvalues
{
    return shift->{d};
}

sub getImagEigenvalues
{
    return shift->{e};
}

sub getD
{
    my ($self) = @_;

    my $n = $self->{n};
    my $d = $self->{d};
    my $e = $self->{e};

    my $D = [];

    for ( my $i = 0 ; $i < $n ; $i++ ) {
        for ( my $j = 0 ; $j < $n ; $j++ ) {
            $D->[$i][$j] = 0.0;
        }
        $D->[$i][$i] = $d->[$i];
        if ( $e->[$i] > 0 ) {
            $D->[$i][ $i + 1 ] = $e->[$i];
        }
        elsif ( $e->[$i] < 0 ) {
            $D->[$i][ $i - 1 ] = $e->[$i];
        }
    }

    return $D;
}


1;

