# $Source: /headas/headas/attitude/tasks/tristarid/StarID/UserCat.pm,v $
# $Revision: 1.2 $
# $Date: 2008/05/08 21:41:42 $
#
#	Allow the user to supply their own catalog data.
#
# $Log: UserCat.pm,v $
# Revision 1.2  2008/05/08 21:41:42  rwiegand
# Only keep objects which are within the specified bounds.
#
# Revision 1.1  2008/04/18 18:57:52  rwiegand
# Allow user to specify catalog objects in a text file.
#

use strict;

package StarID::UserCat;
use base qw(StarID::Partition);
use Task qw(:codes);

use FileHandle;
use Math;



sub platePath
{
	my ($self, $plate) = @_;
	return $plate->{path};
}


sub getPlate
{
	my ($self, $spec) = @_;

	my $task = $self->{task};
	$self->{bounds} = $spec;

	my $plate = StarID::Partition::Plate->new(
				id => 'dynamic',
				spec => $spec,
				);

	my $in = FileHandle->new($self->{path});
	if (not $in) {
		$task->error(BAD_INPUT, "unable to open $self->{path}[$!]");
	}

	my @label = split(',', $self->{fields});
	my $nfields = @label;

	my @contents;

	my $resolve = "resolve$self->{data}";
	if (not $self->can($resolve)) {
		$task->warning("unable to $resolve");
		$resolve = 'resolveRadians';
	}

	if ($task->isValid) {

		my $mincos = 0;
		if (not $self->{unbounded} and $spec and $spec->{radius} > 0) {
			$mincos = cos($spec->{radius});
		}

		while (<$in>) {

			next if /^\s*$/;
			next if /^\s*#/;

			$_ =~ s/^\s+//;
			my @field = split(/\s+/);

			if (@field == $nfields) {

				my %data = map { $label[$_] => $field[$_] } (0 .. $nfields - 1);

				if (my $object = $self->$resolve(\%data)) {
					if ($mincos > 0) {
						my $cos = Math::u3cosangle($spec->{UNIT}, $object->{UNIT});
						if ($cos < $mincos) {
							next;
						}
					}
					push(@contents, $object);
				}
			}
		}

		undef($in);

		$plate->{path} = $self->{path};
	}

	$plate->{contents} = \@contents;

	return $plate;
}


sub resolveRadians
{
	my ($self, $href) = @_;

	$href->{UNIT} = Math::rd2unit($href->{RA}, $href->{DEC});

	# MAG and TYPE preserved 
	if (not defined($href->{MAG})) {
		$href->{MAG} = '30';
	}

	if (not defined($href->{TYPE})) {
		$href->{TYPE} = 'UNKNOWN';
	}

	my $object = bless($href, 'StarID::Source');
}


sub resolveUser
{
	my ($self, $href) = @_;

	$href->{RA} = Math::toRadians($href->{RA_deg});
	$href->{DEC} = Math::toRadians($href->{DEC_deg});

	$href->{UNIT} = Math::rd2unit($href->{RA}, $href->{DEC});

	# MAG and TYPE preserved 
	if (not defined($href->{MAG})) {
		$href->{MAG} = '30';
	}

	if (not defined($href->{TYPE})) {
		$href->{TYPE} = 'UNKNOWN';
	}

	my $object = bless($href, 'StarID::Source');
}



1;

