/**************************************************************************************

	I N T E G R A L   S C I E N C E   D A T A   C E N T E R

	P A R A M E T E R   I N T E R F A C E   L I B R A R Y

Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
File:		pset.c
Description:	Parameter Interface Library - sample C program
Authors:	Jerzy.Borkowski@obs.unige.ch (JB)
History:	17-Aug-98 (JB) : version 1.0 release

**************************************************************************************/
/*
 * this program demonstrates how to call PIL library from C code
 * note : this program uses only PIL library (calls PILInit())
 * so it is _NOT_ appropriate for ISDC executables which should
 * call CommonInit(). Should you wish to see sample ISDC compliant
 * program look for it in  Common library directory or see ISDCcopy.c
 * file in this directory.
 *
 * This program tries to mimic "pset" program/command found
 * in IRAF / XPI / SAOrd packages. It sets the value of specified
 * parameter(s) in parameter file. Program does _NOT_
 * prompt for new value. Program bypasses official API
 * routines, so it is not recommended to follow it. It is included
 * for testing/debug purposes only.
 * note: indirections are not handled at the moment (contrary to PIL kernel)
 * note: in case of error original contents of parameter file is restored
*/

#include <stdio.h>
/* SCREW 1145: stdlib needed for getenv. */
#include <stdlib.h>
#include <string.h>
#include "pil.h"

int main(int argc, char **argv){
    int r, j, parcnt, minmaxflag;
    PIL_PARAM pp;
    PIL_VALUE vmin, vmax, v;
    int pset_err_logger(char *);
    
    if (argc < 2){
	printf("usage:\n\tpset par_fname [par1=value] [par2=value] [ ... ]\n\n");
	return(10);
    }

/* PILSpecialMode |= PIL_PSET_MODE | PIL_BYPASS_NAMING; */
/* We want pset to follow PFILES (MJT 13March2002)      */
/* With change to PIL_get_by_value this mode will now   */
/* also ensure that all parameters are queried by       */
/* PIL_get_by_value()       (MJT 26Sept2002)            */
    PILSpecialMode |= PIL_PSET_MODE;
    
    strncpy(PILModuleName, argv[1], PIL_LINESIZE - 1);
    PILModuleName[PIL_LINESIZE - 1] = 0;
    
    /* Get rid of argument 1, which gave the name of the par file/tool. */
    for (j = 2; j < argc; ++j) {
      argv[j - 1] = argv[j];
    }

    if (PIL_OK != (r = PILInit(argc - 1, argv))){
	printf("PILInit failed : %s\n", PIL_err_handler(r));
	return(10);
    }
    PILSetLoggerFunction(pset_err_logger);

    /* enable 'batch mode' via HEADASNOQUERY environment variable */
    if (getenv("HEADASNOQUERY")) PILOverrideQueryMode(PIL_QUERY_OVERRIDE);

    if (2 == argc){ /* prompt for all parameters */
	if (PIL_OK != (r = PILGetNumParameters(&parcnt))){
	    PIL_log_error("PILGetNumParameters failed", r);
	    return(1);
	}
	for (j=0; j<parcnt; j++){
	    if (PIL_OK != (r = PILGetParameter(j, &pp, &minmaxflag, &vmin, &vmax))){
		PIL_log_error("PILGetParameter failed", r);
		return(1);
	    }
	    if (PIL_FORMAT_OK != pp.format) continue;   /* comment, empty or bad format */
	    PIL_get_by_value(&PILDefaultPFile, pp.strname, &v, pp.type);
	}
/* This block doesn't handle spaces around "=" and PILInit does all the parsing anyway!
    }else{ 
        char buf[PIL_LINESIZE], errbuf[PIL_LINESIZE];
	for (j=2; j<argc; j++){
	    strncpy(buf, argv[j], PIL_LINESIZE - 1);
	    buf[PIL_LINESIZE - 1] = 0;
	    if (NULL == (p = strchr(buf, '='))) continue;
	    *(p++) = 0;
	    
	    if (PIL_OK != (r = PIL_put_by_string(&PILDefaultPFile, buf, p))){
		sprintf(errbuf,"unable to assign value to %s parameter", buf);
		PIL_log_error(errbuf, r);
		break;
	    }
	}
*/
    }
    
    PILClose(r);
    return(r);
}

int pset_err_logger(char *s)
{
    fprintf(stderr, "\n%s\n", s ? s : "NULLstring");
    return(0);
}
