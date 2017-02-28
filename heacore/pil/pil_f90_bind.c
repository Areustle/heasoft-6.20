/**************************************************************************************

	I N T E G R A L   S C I E N C E   D A T A   C E N T E R

	P A R A M E T E R   I N T E R F A C E   L I B R A R Y

Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
File:		pil_f90_bind.c
Description:	Parameter Interface Library - C to F90 binding routines
Authors:	Jerzy.Borkowski@obs.unige.ch (JB)
History:	06-Oct-97 (JB) : support for command line arguments added
		10-May-98 (JB) : PILGetReal4 function added 
		13-May-98 (JB) : version 0.7 released. Code cleanup, unnecesary
			dependencies removed. Uses/links only PIL library. Changes
			to Makefile.
		13-Aug-98 (JB) : version 1.0 released. More compatible with Common
			library some new functions: PILReloadParameters,
			PILFlushParameters, PILGetParFilename, locking of parameter
			file during read/write implemented.
		17-Aug-98 (JB) : reformatted, more comments added
		08-Mar-02 (JB) : SPR 01050 : added PILGetDOL()

**************************************************************************************/


#ifndef PIL_F90_BIND_INCLUDED
#define PIL_F90_BIND_INCLUDED


#define	CFORTRAN_PROTOTYPING

#include <stdio.h>
#include <cfortran.h>
#include <pil.h>



FCALLSCFUN2(INT, PILGetBool, PILGETBOOL, pilgetbool, STRING, INTV)
FCALLSCFUN2(INT, PILGetInt, PILGETINT, pilgetint, STRING, INTV)
FCALLSCFUN2(INT, PILGetReal, PILGETREAL, pilgetreal, STRING, DOUBLEV)
FCALLSCFUN2(INT, PILGetReal4, PILGETREAL4, pilgetreal4, STRING, FLOATV)
FCALLSCFUN2(INT, PILGetString, PILGETSTRING, pilgetstring, STRING, PSTRING)
FCALLSCFUN2(INT, PILGetFname, PILGETFNAME, pilgetfname, STRING, PSTRING)
FCALLSCFUN2(INT, PILGetDOL, PILGETDOL, pilgetdol, STRING, PSTRING)

FCALLSCFUN2(INT, PILPutBool, PILPUTBOOL, pilputbool, STRING, INT)
FCALLSCFUN2(INT, PILPutInt, PILPUTINT, pilputint, STRING, INT)
FCALLSCFUN2(INT, PILPutReal, PILPUTREAL, pilputreal, STRING, DOUBLE)
FCALLSCFUN2(INT, PILPutString, PILPUTSTRING, pilputstring, STRING, STRING)
FCALLSCFUN2(INT, PILPutFname, PILPUTFNAME, pilputfname, STRING, STRING)

FCALLSCFUN1(INT, PILSetModuleName, PILSETMODULENAME, pilsetmodulename, STRING)
FCALLSCFUN1(INT, PILSetModuleVersion, PILSETMODULEVERSION, pilsetmoduleversion, STRING)

FCALLSCFUN1(INT, PILF90AddArg, PILF90ADDARG, pilf90addarg, STRING)

FCALLSCFUN0(INT, PILF90Init, PILF90INIT, pilf90init)
FCALLSCFUN1(INT, PILF90Close, PILCLOSE, pilclose, INT)

extern int pilgetparfilename_(char *A1 , unsigned C1)
 {
   char *B1 = NULL;
   int A0;
   unsigned l;
   

   A0 = PILGetParFilename(&B1);

   if (PIL_OK == A0) if (B1)
     {
       l = strlen(B1);
       memcpy(A1, B1, ((l < C1) ? l : C1));
       if (C1 > l) memset(A1 + l, ' ', C1 - l);
     }

   return A0;
 }


FCALLSCFUN0(INT, PILLockPFile, PILLOCKPFILE, pillockpfile)
FCALLSCFUN0(INT, PILUnlockPFile, PILUNLOCKPFILE, pilunlockpfile)

FCALLSCFUN0(INT, PILFlushParameters, PILFLUSHPARAMETERS, pilflushparameters)
FCALLSCFUN0(INT, PILReloadParameters, PILRELOADPARAMETERS, pilreloadparameters)

FCALLSCFUN1(INT, PILOverrideQueryMode, PILOVERRIDEQUERYMODE, piloverridequerymode, INT)

FCALLSCFUN3(INT, PILGetIntVector, PILGETINTVECTOR, pilgetintvector, STRING, INT, INTV)
FCALLSCFUN3(INT, PILGetIntVarVector, PILGETINTVARVECTOR, pilgetintvarvector, STRING, INTV, INTV)
FCALLSCFUN3(INT, PILGetRealVector, PILGETREALVECTOR, pilgetrealvector, STRING, INT, DOUBLEV)
FCALLSCFUN3(INT, PILGetRealVarVector, PILGETREALVARVECTOR, pilgetrealvarvector, STRING, INTV, DOUBLEV)
FCALLSCFUN3(INT, PILGetReal4Vector, PILGETREAL4VECTOR, pilgetreal4vector, STRING, INT, FLOATV)
FCALLSCFUN3(INT, PILGetReal4VarVector, PILGETREAL4VARVECTOR, pilgetreal4varvector, STRING, INTV, FLOATV)

FCALLSCFUN1(INT, PILSetReadlinePromptMode, PILSETREADLINEPROMPTMODE, pilsetreadlinepromptmode, INT)
FCALLSCFUN1(INT, PILSetCmndArgMode, PILSETCMNDARGMODE, pilsetcmndargmode, INT)


#endif 		/* PIL_F90_BIND_INCLUDED */
