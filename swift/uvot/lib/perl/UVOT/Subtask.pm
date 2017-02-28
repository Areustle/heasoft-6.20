# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/Subtask.pm,v $
# $Revision: 1.1 $
# $Date: 2004/06/17 21:19:49 $
#
# $Log: Subtask.pm,v $
# Revision 1.1  2004/06/17 21:19:49  rwiegand
# A Task subclass for sharing data (e.g., temporary files) with another Task.
#

package UVOT::Subtask;
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

