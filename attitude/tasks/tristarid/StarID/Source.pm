# $Source: /headas/headas/attitude/tasks/tristarid/StarID/Source.pm,v $
# $Revision: 1.1 $
# $Date: 2005/10/07 20:42:32 $
#
# $Log: Source.pm,v $
# Revision 1.1  2005/10/07 20:42:32  rwiegand
# Source catalog modules.
#

use strict;

use Math;


####################################################################

package StarID::Source;


sub new
{
	my ($class, %args) = @_;
	my $object = bless({ %args }, $class);
	return $object;
}


sub id
{
	return shift->{ID};
}


sub ra
{
	return shift->{RA};
}


sub dec
{
	return shift->{DEC};
}


sub mag
{
	return shift->{MAG};
}


sub rate
{
	return shift->{RATE};
}


sub unit
{
	return shift->{UNIT};
}


sub angle
{
	my ($self, $other) = @_;
	my $angle = Math::u3angle($self->unit, $other->unit);
	return $angle;
}


sub cosangle
{
	my ($self, $other) = @_;
	my $cosangle = Math::u3cosangle($self->unit, $other->unit);
	return $cosangle;
}


sub poserr
{
	return shift->{POS_ERR};
}


sub magerr
{
	return shift->{MAG_ERR};
}


sub type
{
	return shift->{TYPE};
}



1;

