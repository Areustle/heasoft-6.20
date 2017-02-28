
# $Source: /headas/headas/swift/uvot/tasks/uvotstarid/XBase.pm,v $
# $Revision: 1.2 $
# $Date: 2003/11/26 20:39:18 $
#
#
# $Log: XBase.pm,v $
# Revision 1.2  2003/11/26 20:39:18  rwiegand
# Added underscore to object initialization method.
#
# Revision 1.1  2002/11/12 19:25:37  rwiegand
# Initial revision
#

use strict;
use integer;

##############################################################################
package X::Object;

sub new
{
	my ($class, %args) = @_;

	my $object = bless(\%args, $class);

	$object->_initialize;

	return $object;
}


sub _initialize
{
}


1;

