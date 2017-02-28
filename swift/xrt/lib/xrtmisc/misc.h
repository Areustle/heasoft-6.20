/*
 *	misc.h: --- module providing miscellaneous useful functions ---
 *
 *	DESCRIPTION:
 *		The module provides several generally useful functions with different
 *		flavour.
 *
 *	EXPORTED FUNCTIONS:
 *
 *	AUTHOR:
 *		UL, Tue May 30 10:11:40 METDST 1995
 */

#ifndef MISC_H
#define MISC_H

					/********************************/
					/*        header files          */
					/********************************/

#include "basic.h"
#include <time.h>

					/********************************/
					/*      defines / typedefs      */
					/********************************/

typedef struct {
			double lo, hi;
		} DblInt_t;

typedef struct {
			int	lo, hi;
		} IntInt_t;

					/********************************/
					/*      function prototypes     */
					/********************************/

#if defined(NOANSILIB)
extern double difftime				(time_t t1,
									 time_t t0);
#endif

extern char *StripExtension			(char *filename);

extern char	*GetFilenameExtension	(const char *name,
									 char *ext);

extern char *GetExtFilenameExt		(const char *name,
									 char *ext);

extern BOOL	FileExists				(const char *Filename);

extern BOOL	BigEndianMachine		(void);

/*
 *	SYNOPSIS:
 *		BOOL FileExistsOnPath(const char *FileName, const char *PathList,
 *							  char *ActPath)
 *
 *	DESCRIPTION:
 *
 *	PARAMETERS:
 *		I	: FileName	: the name of the file to be checked for existence
 *			  PathList	: colon or semi-colin separated list of paths
 *						  which are to be inspected for the location of
 *						  the file
 *		O	: ActPath	: the actual path under which the file can be
 *						  accessed
 *
 *	RETURNS:
 *		T/F: file was found/was not found under one of the paths
 */
extern BOOL FileExistsOnPath		(const char *FileName,
									 const char *PathList,
									 char *ActPath);

extern char *DeriveFileName			(const char *OldName,
									 char *NewName,
									 const char *ext);

extern char *SplitFilePath			(const char *FilePath,
									 char *DirName,
									 char *BaseName);

extern char *CheckPathName			(char *PathName);

extern char *toup					(char *s);

extern char *tolo					(char *s);

extern char *ChangeCase				(char *s);

extern void GetDate					(char DateStr[9]);

extern void GetTime					(char *TimeStr);

/*
 *	SYNOPSIS:
 *		int Asctime2tmstruct(const char *AscTime, struct tm *timeptr)
 *
 *	DESCRIPTION:
 *		Converts a date/time string of the form "Mon May 30 14:49:20 1994" to
 *		<struct tm>.
 *
 *	PARAMETERS:
 *		I	:	AscTime	:	ASCII string to convert
 *		O	:	*timeptr:	time structure to be filled
 *
 *	RETURNS:
 *		-1/0: conversion failure/success
 */
extern int Asctime2tmstruct(const char *AscTime,
							struct tm *timeptr);



/*
 *	SYNOPSIS:
 *	    int Asctime2tmstruct2(const char *AscTime, struct tm *timeptr)
 *
 *	DESCRIPTION:
 *		Converts a date/time string of the form "30/05/94 14:49:20" to
 *		<struct tm>.
 *
 *	PARAMETERS:
 *		I	:	AscTime	:	ASCII string to convert
 *		O	:	*timeptr:	time structure to be filled
 *
 *	RETURNS:
 *		-1/0: conversion failure/success
 */
extern int Asctime2tmstruct2(const char *AscTime,
							 struct tm *timeptr);



/*
 *	SYNOPSIS:
 *		char *Unsigned2DecHex(const unsigned UVal, char *HexVal);
 *		char *Double2DecHex(const double DVal, char *HexVal);
 *
 *	DESCRIPTION:
 *		Converts an unsigned/double value into a corresponding hex-string.
 *
 *	PARAMETERS:
 *		I	:	U/DVal	: value to convert
 *		O	:	<none>
 *		I/O	:	HexVal	: pointer to space for storing the resulting string;
 *						  if <HexVal> is NULL, the result will be written
 *						  into a static vector of appropriate size
 *
 *	RETURNS:
 *		pointer to space containing the resulting hex string
 */
extern char *Unsigned2DecHex		(const unsigned UVal,
									 char *HexVal);

extern char *Double2DecHex			(const double DVal,
									 char *HexVal);



/*
 *	SYNOPSIS:
 *		char *StrDup(const char *String)
 *
 *	DESCRIPTION:
 *		Returns a pointer to a string which is a duplicate of the one to which
 *		<String> points. The space for the new string is allocated via
 *		"malloc".
 *
 *	PARAMETERS:
 *		I	:	String	: pointer to the string to duplicate
 *
 *	RETURNS:
 *		Pointer to the duplicated string; if "malloc" failed to allocate the
 *		necessary space, NULL is returned.
 */
extern char *StrDup					(const char *String);


extern int stricmp					(const char *s1,
									 const char *s2);



/*
 *	SYNOPSIS:
 *		char *StrRed(char *String)
 *
 *	DESCRIPTION:
 *		REDuce a STRing, i.e., strip-off trailing white-space character.
 *
 *	PARAMETERS:
 *		I/O:	String	: string to reduce - updated on exit
 *
 *	RETURNS:
 *		value of <String>
 */
extern char *StrRed					(char *String);


extern int	UVecMatch				(const unsigned val,
									 const unsigned Vector[],
									 const unsigned n);

extern int	StrVecMatch				(const char *String,
									 char *StringVec[],
									 const unsigned n);

extern int	StrVecMatchI			(const char *String,
									 char *StringVec[],
									 const unsigned n);

extern int	ExpandDoubleIntervals	(const char *String,
									 DblInt_t Interval[],
									 const unsigned MaxIntervals);
		

/*
 *	SYNOPSIS:
 *		unsigned long WrapAdd(const unsigned nbits, const unsigned long value,
 *							  const long AddVal)
 *
 *	DESCRIPTION:
 *		Function to perform addition and subtraction considering "wrap-around"
 *		effects. Some examples are better than any description:
 *
 *		a) WrapAdd(8, 250, 10)		-> 5
 *		b) WrapAdd(16, 65537, 0)	-> 1
 *		c) WrapAdd(8, 4, -9)		-> 251
 *
 *	PARAMETERS:
 *	 I	:	nbits	: number of significant bits of <value>; maximum value
 *					  is the bit size of unsigned long, i.e. 32 on most
 *					  machines
 *			value	: the value to which a quantity is to be added
 *			AddVal	: the value to add - can be negative or 0
 *	 O	:	<none> 
 *	 I/O:	<none>
 *
 *	RETURNS:
 *		The result of the addition considering possible "wrap-arounds".
 */
extern unsigned long WrapAdd		(const unsigned nbits,
									 const unsigned long value,
									 const long AddVal);

/*
 *	SYNOPSIS:
 *		unsigned MaxVector(const unsigned Vector[], const unsigned nElements,
 *						   unsigned *MaxVal)
 *
 *	DESCRIPTION:
 *		Simple functions to locate the maximum of a one-dimensional unsigned
 *		vector <Vector> of length <nElements>.
 *
 *	PARAMETERS:
 *		I	: Vector	: the vector whose maximum is sought
 *			  nElements	: number of elements in <Vector>
 *		O	: MaxVal	: the value of the maximum
 *
 *	RETURNS:
 *		The index of the found element in the vector.
 */
extern unsigned MaxVector		(const unsigned Vector[],
								 const unsigned nElements,
								 unsigned *MaxVal);

extern unsigned MaxDVector		(const double Vector[],
								 const unsigned nElements,
								 double *MaxVal);

extern unsigned MinDVector		(const double Vector[],
								 const unsigned nElements,
								 double *MaxVal);

extern unsigned SplitLines		(const char *Line,
								 const unsigned length,
								 char *(**StrVec)[]);

extern BOOL IsClose				(const double Val,
								 const double NominalVal,
								 const double Tolerance);

extern int InWhichIntInterval	(const int Val,
								 const int nInts,
								 const int Bounds[]);

extern IntInt_t *FindInterval	(const double Vec[],
								 unsigned nElem,
								 double LoVal,
								 double HiVal);

extern char *FormatDouble		(const double Val,
								 char String[]);

extern unsigned UniRndNumInit	(unsigned seed);

extern double UniRndNum			(void);

extern unsigned CeilPow2		(unsigned);

extern int VectorMedian			(const double DVec[],
								 unsigned Size,
								 double *Median);

extern int AdjustIndex4Points	(const double Vec[],
								 double Val,
								 int Ind,
								 unsigned nRecs,
								 double Limit);


/*
 *	SYNOPSIS:
 *		BOOL Next4PointsClose(const double Vec[4], double Val, double Limit);
 *
 *	DESCRIPTION:
 *		Simple function to test whether four consecutive points in a vector
 *		lie close together.
 *
 *	PARAMETERS:
 *		I	: Vec	: vector with four elements
 *			  Val	: elements in <Vec> are to be checked against <Val>
 *			  Limit	: maximum allowed deviation of vector elements from
 *					  <Val>	
 *
 *	RETURNS:
 *		T/F: the maximum absolute difference between <Val> and each vector
 *			 element does not/does exceed <Limit>
 */
extern BOOL Next4PointsClose	(const double Vec[4],
								 double Val,
								 double Limit);

extern void GetGMTDateTime(char *DateStr);

#endif
