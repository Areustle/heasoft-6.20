/*  Perl interface to Headas's heauinit librarys */
/*  Author: Ziqin Pan                           */
/*  December 31, 2003                           */

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "Av_CharPtrPtr.h"

#include "headas.h"



MODULE = HEACORE::HEAINIT		PACKAGE = HEACORE::HEAINIT		

int
headas_init (argc, argv)
	int argc
	char ** argv
        OUTPUT:
                RETVAL

int
headas_close (taskStatus)
	int taskStatus
	OUTPUT:
		RETVAL

