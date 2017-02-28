
# Environment variable check
#
# Page 10
#
#
# This module checks that the appropriate processing environment is
# set.  It checks for the HEADAS, CALDB, and CALDBCONFIG variables.
#
# RETURNS: nothing
#
# EXCEPTIONS: * any missing keyword
#
package BAT::env;

sub check {

    die "ERROR: HEADAS environment variable was not set" if (! $ENV{HEADAS} );

    die "ERROR: LHEASOFT environment variable was not set" if (! $ENV{LHEASOFT} );
    
    die "ERROR: CALDB environment variable was not set" if (! $ENV{CALDB} );
    
    die "ERROR: CALDBCONFIG environment variable was not set" 
	if (! $ENV{CALDBCONFIG} );

}

1;
