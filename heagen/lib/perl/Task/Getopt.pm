# $Source: /headas/headas/heagen/lib/perl/Task/Getopt.pm,v $
# $Revision: 1.1 $
# $Date: 2013/08/27 15:13:44 $
#
#	This Task subclass provides support for Getopt::Long option parsing
#
# $Log: Getopt.pm,v $
# Revision 1.1  2013/08/27 15:13:44  irby
# Task.pm is needed by barycorr (heagen), so relocate these items to
# heagen to avoid heagen (mission-independent) having a dependency on
# Swift (mission-specific):
#
#    swift/gen/lib/perl/Task.pm
#    swift/gen/lib/perl/Task/Makefile
#    swift/gen/lib/perl/Task/FITS.pm
#    swift/gen/lib/perl/Task/Getopt.pm
#    swift/gen/lib/perl/Task/HEAdas.pm
#    swift/gen/lib/perl/Task/Subtask.pm
#
# Visit their old location (swift/gen/lib/perl) to view their CVS history.
#
# Revision 1.1  2002/09/30 18:34:31  wiegand
# Initial revision
#

package Task::Getopt;

use strict;

use base qw(Task);

use Task qw(:codes);
use Getopt::Long;


sub parseOptions
{
	my ($self, %args) = @_;

	local(@main::ARGV) = @{ $args{args} };

	if ($self->isValid and $args{internal}) {
		foreach my $p (keys(%{ $args{internal} })) {
			$self->set($p, $args{internal}{$p});
		}
	}

	if ($self->isValid and $args{spec}) {
		if (not Getopt::Long::GetOptions($self->args, @{ $args{spec} })) {
			$self->fatal(BAD_INPUT, "unable to parse options: $!");
		}
	}

	if ($self->isValid and $args{default}) {
		foreach my $p (keys(%{ $args{default} })) {
			my $v = $self->get($p);
			if (not defined($v)) {
				$self->set($p, $args{default}{$p});
			}
		}
	}
}


1;

