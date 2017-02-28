/*
 *	nu_misc.h: --- module providing miscellaneous useful functions ---
 *
 *	DESCRIPTION:
 *		The module provides several generally useful functions with different
 *		flavour.
 *
 *	EXPORTED FUNCTIONS:
 *
 *	AUTHOR:
 *		
 */

#ifndef NU_MISC_H
#define NU_MISC_H

					/********************************/
					/*        header files          */
					/********************************/

#include <time.h>

/* nustar local headers */
#include "nu_basic.h"


					/********************************/
					/*      defines / typedefs      */
					/********************************/




					/********************************/
					/*      function prototypes     */
					/********************************/


extern char *StripExtension (char *filename);
extern char *GetFilenameExtension (const char *name, char *ext);
extern char *SplitFilePath(const char *FilePath, char *DirName, char *BaseName);
extern BOOL  FileExists (const char *Filename);
extern char *DeriveFileName (const char *OldName, char *NewName, const char *ext);
extern int UVecMatch (const unsigned val, const unsigned Vector[], const unsigned n);

extern void GetGMTDateTime(char *DateStr);

extern void get_ran2seed(long *seed);

extern double ComputeDoubleMedian(double * dvalue, int length);

extern int FilterTimesByGTI(char *infile, char *extname, char *gtifile, char *gtiextname);

extern void RemoveTrailingSlash(char *filename);

#endif
