#! perl
# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/Filter.pm,v $
# $Revision: 1.3 $
# $Date: 2005/08/31 16:49:53 $
#
#	UVOT filter information
#
#
# $Log: Filter.pm,v $
# Revision 1.3  2005/08/31 16:49:53  rwiegand
# Fixed bug related to returning from an each(%hash).
#
# Revision 1.2  2005/08/29 11:52:36  rwiegand
# Added function to return list of filters.
#
# Revision 1.1  2005/06/15 20:19:13  rwiegand
# UVOT filter information.
#

use strict;

package UVOT::Filter;


my %FILTER = (
	UGRISM	=> { code => 'gu',
		grism => 1,
	},
	VGRISM	=> { code => 'gv',
		grism => 1,
	},
	UVW1	=> { code => 'w1',
	},
	UVW2	=> { code => 'w2',
	},
	UVM2	=> { code => 'm2',
	},
	U	=> { code => 'uu',
	},
	V	=> { code => 'vv',
	},
	MAGNIFIER => { code => 'mg',
	},
	B	=> { code => 'bb',
	},
	WHITE	=> { code => 'wh',
	},
	BLOCKED	=> { code => 'bl',
	},
	UNKNOWN	=> { code => 'un',
	},
);




sub isValid
{
	my ($filter) = @_;
	return exists($FILTER{$filter});
}


sub list
{
	my ($mode) = @_;
	if (not $mode or $mode =~ /^key/i) {
		return keys(%FILTER);
	}
	elsif ($mode eq /^code$/i) {
		return map { $_->{code} } values(%FILTER);
	}
}


sub isGrism
{
	my ($filter) = @_;
	return $FILTER{$filter}{grism};
}


sub filterCode
{
	my ($filter) = @_;
	return $FILTER{$filter}{code};
}

sub codeFilter
{
	my ($code) = @_;
	my $filter = 0;
	while (my ($key, $value) = each(%FILTER)) {
		if ($value->{code} eq $code) {
			$filter = $key;
		}
	}
	return $filter;
}


1;

