# $Source: /headas/headas/swift/gen/lib/perl/Math/Polynomial.pm,v $
# $Revision: 1.2 $
# $Date: 2004/11/03 21:07:36 $
#
# $Log: Polynomial.pm,v $
# Revision 1.2  2004/11/03 21:07:36  rwiegand
# Added stringify options.
#
# Revision 1.1  2004/03/02 18:13:11  rwiegand
# Find roots of polynomial.
#

use strict;

package Math::Polynomial;


use constant EPSILON => 1e-6;



sub new
{
	my ($first, @coeff) = @_;
	return bless(\@coeff);
}


sub value
{
	my ($poly, $x) = @_;

	my $y = 0;
	for (my $i = 0; $i < @$poly; ++$i) {
		$y += $poly->[$i] * $x ** $i;
	}

	return $y;
}


sub derivative
{
	my ($poly, $x) = @_;

	my $derivative = 0;
	for (my $i = 1; $i < @$poly; ++$i) {
		$derivative += $i * $poly->[$i] * $x ** ($i - 1);
	}

	return $derivative;
}



# y = f(x)

# find some x such that f(x) = 0

# y' = f(x')
# x'' = x' - f(x') / f'(x')

sub findRoot
{
	my ($poly, %args) = @_;

	my $root = undef;

	my $epsilon = $args{epsilon} || EPSILON;
	my $guess = $args{guess} || 0;

	my $last = $guess + ($epsilon < 1 ? 1 : $epsilon);
	my $limit = $args{limit} || 30;
	my $verbose = $args{verbose} || 0;

	for (my $iters = 0; $iters < $limit; ++$iters) {

		my $value = value($poly, $guess);
		my $derivative = derivative($poly, $guess);
		if ($derivative == 0) {
			my $next = ($last + $guess) / 2;
			print "avoiding zero derivative by moving from $guess to $next\n"
				if $verbose;
			$guess = $next;
			next;
		}

		$last = $guess;

		$guess -= $value / $derivative;

		print "guess $guess, value $value, derivative $derivative\n"
			if $verbose;

		if (abs($last - $guess) < $epsilon) {
			$root = $guess;
			last;
		}
	}

	return $root;
}


sub stringify
{
	my ($self, %args) = @_;

	my $var = $args{var} || 'x';
	my $sym = $args{expsym} || '^';

	my $s = '';
	for (my $i = 0; $i < @{ $self }; ++$i) {
		if ($self->[$i]) {
			$s .= ' + ' if $s;

			if ($args{format}) {
				$s .= sprintf($args{format}, $self->[$i]);
			}
			else {
				$s .= $self->[$i];
			}

			$s .= $var if $i > 0;
			$s .= $sym . $i if $i > 1;
		}
	}

	return $s;
}


1;

