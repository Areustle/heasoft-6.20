# $Source: /headas/headas/attitude/tasks/tristarid/StarID/Partition.pm,v $
# $Revision: 1.1 $
# $Date: 2005/10/07 20:42:32 $
#
# $Log: Partition.pm,v $
# Revision 1.1  2005/10/07 20:42:32  rwiegand
# Source catalog modules.
#
# Revision 1.1  2004/06/17 21:23:28  rwiegand
# The code for loading a partitioned star catalog was modularized from the
# uvotstarid since several tasks need it.  The Partition and SquareRADec
# modules were relocated here with some refactoring and a new Partition
# subclass was added for dealing with the hierarchical triangular mesh
# partitioning used by the Guide Star Catalog folks.
#
# Revision 1.5  2002/11/21 18:13:46  wiegand
# Removed support for polyhedral partitions.
#
# Revision 1.4  2002/07/25 13:21:53  wiegand
# Implemented creation based on loading summary file
#
# Revision 1.3  2002/07/19 13:30:41  wiegand
# Debugged SquareRADec partition
#
# Revision 1.2  2002/06/27 20:24:52  wiegand
# Moved loading of catalog to StarID.  Added SquareRADec partition subclass
#
# Revision 1.1  2002/06/18 17:06:22  rwiegand
# Initial revision

use strict;

package StarID::Partition::Plate;


sub new
{
	my ($class, %args) = @_;

	my $object = bless(\%args, $class);

	return $object;
}


sub id
{
	return shift->{id} || 'anonymous';
}


sub path
{
	return shift->{path} || 'none';
}


sub spec
{
	return shift->{spec};
}



##############################################################################

package StarID::Partition;


sub new
{
	my ($class, %args) = @_;
	my $object = bless(\%args, $class);
	$object->initialize;
	return $object;
}


sub initialize
{
}


sub writeSummary
{
	my ($self, $fh) = @_;

	$self->{type} = ref($self);

	$fh->print("# partition summary\n");
	$self->writeSummaryKeys($fh, qw(type fields format pack));
}


sub writeSummaryKeys
{
	my ($self, $fh, @keys) = @_;

	foreach my $key (@keys) {
		$fh->print("$key => $self->{$key}\n");
	}
}


sub platePath
{
	my ($self, $plate) = @_;
	my $pp = $plate->path;
	return "$self->{directory}/$pp";
}



1;

