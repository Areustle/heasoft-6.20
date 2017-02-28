/**************************************************************************************

	I N T E G R A L   S C I E N C E   D A T A   C E N T E R

	P A R A M E T E R   I N T E R F A C E   L I B R A R Y

Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
File:		pget.c
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
 * This program tries to mimic "pget" program/command found
 * in IRAF / XPI / SAOrd packages. It displays value of given
 * parameter from parameter file. Program does _NOT_
 * prompt for new value. Program bypasses official API
 * routines, so it is not recommended to follow it. It is included
 * for testing/debug purposes only.
 * note: indirections are not handled at the moment (contrary to PIL kernel)
 *
*/

#include <stdio.h>
/* SCREW 1145: Use string.h instead of strings.h. */
#include <string.h>
#include "pil.h"

int main(int argc, char **argv){
    int r, i, j;
    char buf[PIL_LINESIZE];
    int pget_err_logger(char *);
    
    if (argc < 2){
	printf("usage:\n\tpget parameter_filename [par1 par2 par3 ... ]\n\n");
	return(10);
    }

/* PILSpecialMode |= PIL_NO_POSITIONAL | PIL_BYPASS_NAMING; */
/* We want pget to follow PFILES (MJT 13March2002)          */
    PILSpecialMode |= PIL_NO_POSITIONAL;

    strncpy(PILModuleName, argv[1], PIL_LINESIZE - 1);
    PILModuleName[PIL_LINESIZE - 1] = 0;

    if (PIL_OK != (r = PILInit(1, argv))){
	printf("PILInit failed : %s\n", PIL_err_handler(r));
	return(10);
    } 
    PILSetLoggerFunction(pget_err_logger);

    for (j=2; j<argc; j++){
	if (PIL_OK != (r = PIL_find_name(&PILDefaultPFile, argv[j], &i))){
	    sprintf(buf,"unable to find %s parameter", argv[j]);
	    PIL_log_error(buf, r);
	    break;
	}
	printf("%s\n", PILDefaultPFile.pp[i].strvalue);
    }

    PILClose(r);
    return(r);
}

int pget_err_logger(char *s)
{
    fprintf(stderr, "\n%s\n", s ? s : "NULLstring");
    return(0);
}
