/*************************************************/
/* File:         xstinitable.h                   */
/* Description:  Header file for xstinitable.c   */
/*                                               */
/*************************************************/

#ifndef XSTINITABLE_H
#define XSTINITABLE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "fitsio2.h"     /* cfitsio defined constants             */
#include "fitsio.h" 
#include "xpi.h"         /* parameter file functions, e.g. Uclgst */
#include "cfortran.h"    /* for access to the SLALIB routines     */
#include "xstarutillib.h"
#include "xstartablelib.h"

/* define message length for error & status messages */
#define ERRMSG 1024

/* I believe 1000 is the limiting length of the FTOOL command line but that
   may be wrong.  Check this value if xstinitable is having problems 
   generating XSTAR parameter strings. I'm letting XSTINITABLE manipulate 
   it as a MAXPARMLINELENGTH character string internally and will apply the
   test of 1000 characters in the Perl wrapper. */
#define MAXPARMLINELENGTH 2000

/* Function prototypes */
void xstinitable(void);
int  Get_Parms(Parameter_struct*,int);
int  Display_Parms(Parameter_struct*);
int  Write_FITS_ParmTable(char*, Parameter_struct*, int);
int  Generate_Combinations(int, int*, int*,int);
int  Write_Text_Parmlist(char*, Parameter_struct*, int);
int  Generate_Control(char*, char*, Parameter_struct*);
int  Generate_Constants(char*, char*, int, int*, Parameter_struct*);
int  Generate_Interpolated(char*, char* ,int, int*, int*, Parameter_struct*);
int  Generate_Additional(char*, char*, int, int, int*, Parameter_struct*);

#endif
