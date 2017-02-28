/**************************************************************************************

	I N T E G R A L   S C I E N C E   D A T A   C E N T E R

	P A R A M E T E R   I N T E R F A C E   L I B R A R Y

Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
File:		plist.c
Description:	Parameter Interface Library - sample C program
Authors:	Jerzy.Borkowski@obs.unige.ch (JB)
History:	17-Aug-98 (JB) : version 1.0 release
		04-Feb-00 (JB) : modified to use PILGetNumParameters/PILGetParameter
		15-Jan-01 (JB) : SPR 165 implemented

**************************************************************************************/

/*
 * this program demonstrates how to call PIL library from C code
 * note : this program uses only PIL library (calls PILInit())
 * so it is _NOT_ appropriate for ISDC executables which should
 * call CommonInit(). Should you wish to see sample ISDC compliant
 * program look for it in  Common library directory or see ISDCcopy.c
 * file in this directory.
 *
 * This program tries to mimic "plist" program/command found
 * in IRAF / XPI / SAOrd packages. It displays all parameters
 * from given parameter file. Format of display is a bit
 * different from IRAF / XPI / SAORd one. Program does _NOT_
 * prompt for new value. Program bypasses official API
 * routines, so it is not recommended to follow it. It is included
 * for testing/debug purposes only.
 *
 * 21March2002: (MJT) Changes to closely mimic XPI version for use in HEAdas
 *                    Also removed several unused variables,
 *                         Replaced direct access to PILModuleName global 
 *                           with API function call,
 *                         Removed the "-x" option (doesn't work anyway)
*/


#include <stdio.h>
#include <stdlib.h>
/* SCREW 1145: Use string.h instead of strings.h. */
#include <string.h>
#include "pil.h"

int main(int argc, char **argv){
    char minmaxstr[PIL_LINESIZE+100];
    int	r, i, parcnt, minmaxflag;
    char dotparbuf[9999];
    char strname[PIL_LINESIZE+100], *parpath, parname[PIL_LINESIZE+100];
    char strvalue[PIL_LINESIZE+100];
    PIL_VALUE vmin, vmax;
    PIL_PARAM pp;

    int plist_err_logger(char *);
  
    if (2 != argc){
	fprintf(stderr,"usage:\n\tplist parameter_filename\n");
	exit(1);
    }
    
/* PILSpecialMode |= PIL_NO_POSITIONAL | PIL_BYPASS_NAMING | PIL_OPEN_RDONLY;*/
/* We want PFILES to be searched (MJT 13March2002)  */
    PILSpecialMode |= PIL_NO_POSITIONAL | PIL_OPEN_RDONLY;

    strncpy(dotparbuf, argv[1], 9990);
    dotparbuf[9989] = 0;
  
/* need to set module name since the relevant file is NOT plist.par! */
    if (PIL_OK != (r = PILSetModuleName(dotparbuf))){
	fprintf(stderr,"PILSetModuleName failed: %s\n", PIL_err_handler(r));
	exit(1);
    }

    if (PIL_OK != (r = PILInit(1, argv))){
	printf("PILInit failed : %s\n", PIL_err_handler(r));
	exit(1);
    }
    PILSetLoggerFunction(plist_err_logger);

    if (PIL_OK != (r = PILGetNumParameters(&parcnt))){
	PIL_log_error("PILGetNumParameters failed", r);
	exit(1);
    } 
  
    if (PIL_OK != (r = PILGetParFilename(&parpath))){
	PIL_log_error("PILGetParFilename failed", r);
	exit(1);
    } 
    strcpy(parname,parpath);
    printf("Parameters for %s\n",parname);

    for (i=0; i<parcnt; i++){
	if (PIL_OK != (r = PILGetParameter(i, &pp, &minmaxflag, &vmin, &vmax))){
	    PIL_log_error("PILGetParameter failed", r);
	    exit(1);
	}
	if (PIL_FORMAT_OK != pp.format) continue;	/* comment, empty or bad format */
    
	minmaxstr[0] = 0;	/* are min/max values defined ? */
	if (1 == minmaxflag){
	    sprintf(minmaxstr, "(min=%s, max=%s)", pp.strmin, pp.strmax);
	}
	else if (2 == minmaxflag){
	    sprintf(minmaxstr, "(enumlist=[%s])", pp.strmin);
	}

	if (pp.mode & PIL_MODE_HIDDEN){
	    strcpy(strname,"(");
	    strcat(strname,pp.strname);
	    strcpy(strvalue,pp.strvalue);
	    strcat(strvalue,")");
	} else {
	    strcpy(strname,pp.strname);
	    strcpy(strvalue,pp.strvalue);
	}
	
	if (0 == minmaxflag){
	    printf("%13s = %-16s %s\n", strname, strvalue, pp.strprompt);
	} else {
	    printf("%13s = %-16s %s %s\n", strname, strvalue, pp.strprompt, minmaxstr);
	}
    }
  
    PILClose(PIL_OK);
    exit(PIL_OK);
}

int plist_err_logger(char *s)
{
    fprintf(stderr, "\n%s\n", s ? s : "NULLstring");
    return(0);
}
