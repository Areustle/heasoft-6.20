# $Source: /headas/headas/heagen/lib/perl/Task/Subtask.pm,v $
# $Revision: 1.1 $
# $Date: 2013/08/27 15:13:44 $
#
# $Log: Subtask.pm,v $
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
# Revision 1.1  2004/12/29 21:19:16  rwiegand
# Promoted from UVOT library.
#

package Task::Subtask;
use base qw(Task);


sub _initialize
{
	my ($self) = @_;
	$self->SUPER::_initialize;
	$self->{task} ||= Task->new;
}


sub getTask
{
	return shift->{task};
}


sub temporary
{
	my ($self, $base, %args) = @_;
	return $self->getTask->temporary($base, %args);
}


1;

