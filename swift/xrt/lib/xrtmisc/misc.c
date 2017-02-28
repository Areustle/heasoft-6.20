/*
 *	misc.c: --- miscellaneous functions ---
 *
 *	CHANGE HISTORY:
 *             26/03/02 - 1.3.1 - FT - First Working Version from Uwe Lammer
 *             05/07/02 - 1.3.2 - BS - Modified "DeriveFileName" function to 
 *                                     handle compressed file names
 *             02/02/05 - 1.3.3 - BS - Bug fixed on DeriveFileName routine
 *
 *	AUTHOR:
 *		UL, Fri Sep  1 14:40:51 METDST 1995
 *           Italian Swift Archive Center (FRASCATI)             
 */

#define MISC_C
#define MISC_VERSION		1.3.3

						/********************************/
						/*         header files         */
						/********************************/

#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "basic.h"
#include "astro.h"
#include "misc.h"
#include "ranlux.h"
#include "xrt_termio.h"

#define MISC_C

#if defined(NOANSILIB)
#define _TBIAS	0
double difftime(time_t t1, time_t t0)
{
	t0 -= _TBIAS, t1 -= _TBIAS;

	return (t0 <= t1 ? (double)(t1 - t0) : -(double)(t1 - t0));
} /* difftime */

#endif

BOOL BigEndianMachine(void)
{
	union word {
		unsigned int								_1;
		struct { unsigned char	b1, b2, b3, b4; }	_2;
	} w = { 1 };

	return w._2.b1 != '\1';
} /* BigEndianMachine */



char *StripExtension(char *filename)
{
	char *tmp;

	if ((tmp = strrchr(filename, '.')))
		*tmp = '\0';
	return filename;
} /* stripext */



static char *FileNameExt(const char *name, char *ext, char extchar)
{
	static char ExtName[MAXEXT_LEN];
	char		*s, *tmp;

	s = ext ? ext : ExtName;

	*s = '\0';
	if ((tmp = strrchr(name, extchar)))
		strcpy(s, tmp+1);

	return s;
}



char *GetFilenameExtension(const char *name, char *ext)
{
	return FileNameExt(name, ext, '.');
} /* GetFilenameExtension */



char *GetExtFilenameExt(const char *name, char *ext)
{
	return FileNameExt(name, ext, '_');
} /* GetExtFilenameExt */



BOOL FileExists(const char *FileName)
{
	FILE *FilePtr;

	if (!(FilePtr = fopen(FileName, "r")))
		return FALSE;
	else {
		fclose(FilePtr);
		return TRUE;
	}
} /* FileExists */



char *CheckPathName(char *PathName)
{
	unsigned len = strlen(PathName);

	if (len > 0 && PathName[len-1] != '/') {
		PathName[len] = '/';
		PathName[len+1] = '\0';
	}

	return PathName;
} /* CheckPathName */



BOOL FileExistsOnPath(const char *FileName, const char *PathList, char *ActPath)
{
	char PathListCopy[10*MAXFNAME_LEN], TmpName[MAXFNAME_LEN];
	char *tmp;

	if (!FileName || !FileName[0])
		return FALSE;

	/*
	 *	first, check if file can be found with name as is
	 */
	if (FileExists(FileName)) {
		ActPath[0] = '\0';
		return TRUE;
	}
	/*
	 *	now, check each component in PathList
	 */
	strncpy(PathListCopy, PathList, sizeof(PathListCopy));
	tmp = strtok(PathListCopy, ":; ");
	while(tmp) {
		unsigned len;

		CheckPathName(strcpy(TmpName, tmp));
		len = strlen(TmpName);
		strcat(TmpName, FileName);
		if (FileExists(TmpName)) {
			strncpy(ActPath, TmpName, len);
			ActPath[len] = '\0';
			return TRUE;
		}
		tmp = strtok(NULL, ":; ");
	}
	return FALSE;
} /* FileExistsOnPath */



char *DeriveFileName(const char *OldName, char *NewName, const char *ext)
{
	char gext[MAXEXT_LEN], fext[MAXEXT_LEN], *tmp;

	gext[0]='\0';
	strcpy(NewName, OldName);
	GetFilenameExtension(NewName, fext);
	StripExtension(NewName);
	if (!strcmp(fext, "gz") || !strcmp(fext, "Z"))
	  {
	    strcpy(gext,fext);
	    GetFilenameExtension(NewName, fext);
	    StripExtension(NewName);
	  }
	/*	if ((tmp = strrchr(NewName, '_')))
	 *tmp = '\0';*/
	strcat(strcat(strcat(NewName, ext), "."), fext);

	return NewName;
} /* DeriveFileName */



char *SplitFilePath(const char *FilePath, char *DirName, char *BaseName)
{
	static char	Base[MAXFNAME_LEN];		/* place to store basename of file	*/
	char		*tmp,					/* pointer to last '/' in FilePath	*/
				*BasePtr;				/* pointer to Base					*/

	BasePtr = BaseName ? BaseName : Base;
	if ((tmp = strrchr(FilePath, '/'))) {
		unsigned len = tmp-FilePath+1;

		if (DirName) {
			strncpy(DirName, FilePath, len);
			DirName[len] = '\0';
		}
		strcpy(BasePtr, tmp+1);
	} else {
		if (DirName)
			*DirName = '\0';
		strcpy(BasePtr, FilePath);
	}

	return BasePtr;
} /* SplitFilePath */



char *toup(char *s)
{
	register unsigned i;

	for (i=0; s[i]; ++i)
		s[i] = toupper(s[i]);
	return s;
} /* toup */



char *tolo(char *s)
{
	register unsigned i;

	for (i=0; s[i]; ++i)
		s[i] = tolower(s[i]);
	return s;
} /* tolo */



char *ChangeCase(char *s)
{
	unsigned n;

	for (n=0; s[n]; ++n)
		if (islower(s[n]))
			return toup(s);
		else if (isupper(s[n]))
			return tolo(s);

	return s;
} /* ChangeCase */



void GetDate(char DateStr[9])
{
	time_t t = time((time_t)NULL);
	struct tm *tmptr;
	unsigned i;

	tmptr = localtime(&t);
	i = strftime(DateStr, 10, "%d/%m/%y", tmptr);
	/*
	 * NOTE: bug in standard C library on HP-UX: strftime fallisiously
	 *		 returns 0 with 9 as size of DateStr
	 */
} /* GetDate */

void GetGMTDateTime(char DateStr[25])
{
  int timeref = 0;
  int status=0;
  ffgstm( DateStr, &timeref, &status );
} /* GetNewDate */

void GetNewDate(char DateStr[11])
{
	time_t t = time((time_t)NULL);
	struct tm *tmptr;
	unsigned i;

	tmptr = localtime(&t);
	i = strftime(DateStr, 12, "%Y-%m-%d", tmptr);
	/*
	 * NOTE: bug in standard C library on HP-UX: strftime fallisiously
	 *		 returns 0 with 9 as size of DateStr
	 */
} /* GetNewDate */

void GetTime(char TimeStr[9])
{
	time_t t = time((time_t)NULL);

	strftime(TimeStr, 10, "%H:%M:%S", localtime(&t));
	/*
	 * NOTE: bug in standard C library on HP-UX: strftime fallisiously
	 *		 returns 0 with 9 as size of TimeStr
	 */
} /* GetTime */

void GetYear(char YearStr[5])
{
	time_t t = time((time_t)NULL);
	struct tm *tmptr;
	unsigned i;

	tmptr = localtime(&t);
	i = strftime(YearStr, 5, "%Y", tmptr);
	/*
	 * NOTE: bug in standard C library on HP-UX: strftime fallisiously
	 *		 returns 0 with 9 as size of DateStr
	 */
} /* GetYear */


int	UVecMatch(const unsigned val, const unsigned Vector[], const unsigned n)
{
	unsigned	i;

	for (i=0; i<n; ++i)
		if (val == Vector[i])
			return i;

	return -1;
} /* UVecMatch */



int stricmp(const char *s1, const char *s2)
{
	char	*s1dup, *s2dup;
	int		res;

	s1dup = toup(StrDup(s1));
	s2dup = toup(StrDup(s2));

	res = strcmp(s1dup, s2dup);
	free(s1dup);
	free(s2dup);

	return res;
} /* stricmp */



int StrVecMatch(const char *String, char *StringVec[], const unsigned n)
{
	unsigned i, maxi;

	maxi = n ? n : UINT_MAX;
	for (i=0; i<maxi && StringVec[i]; ++i)
		if (STREQ(String, StringVec[i]))
			return i;

	return -1;
} /* StrVecMatch */



int StrVecMatchI(const char *String, char *StringVec[], const unsigned n)
{
	unsigned i, j, maxi;

	maxi = n ? n : UINT_MAX;
	for (i=0; i<maxi && StringVec[i]; ++i) {
		for (j = 0; String[j] && StringVec[i][j]; ++j)
			if (toupper(String[j]) != toupper(StringVec[i][j]))
				break;
		if (String[j] == '\0' && StringVec[i][j] == '\0')
			return i;
	}
	return -1;
} /* StrVecMatchI */



/*
 *	... exported ...
 */
char *StrDup(const char *String)
{
	char		*tmp;
	unsigned	len = strlen(String) + 1;

	if ((tmp = (char *)malloc(len)))
		return strncpy(tmp, String, len);
	else {
		Error("StrDup", "malloc failed to allocate %u bytes for new string",
			  len);
		return CPNULL;
	}
} /* StrDup */



/*
 *	... exported ...
 */
char *StrRed(char *String)
{
	int		len = strlen(String);

	while (--len >= 0 && isspace(String[len]))
		String[len] = '\0';

	return String;
} /* StrRed */



int Asctime2tmstruct(const char *AscTime, struct tm *timeptr)
{
	static char *MonthName[] = {
			"Jan", "Feb", "Mar", "Apr", "May", "Jun",
			"Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

	unsigned	day, hour, minute, second, year;
	int			month;
	char		AscMonth[4];

	if (sscanf(AscTime, "%*3s %3s %3u %2u:%2u:%2u %4u", AscMonth,
			   &day, &hour, &minute, &second, &year) != 6)
		return (time_t)-1;
	if ((month = StrVecMatch(AscMonth, MonthName, NUM_OF(MonthName))) < 0 ||
		month >= NUM_OF(MonthName))
		return (time_t)-1;

	timeptr->tm_sec	  = second;
	timeptr->tm_min   = minute;
	timeptr->tm_hour  = hour;
	timeptr->tm_mday  = day;
	timeptr->tm_mon   = month - 1;
	timeptr->tm_year  = year - 1900;
	timeptr->tm_wday  = -1; /* unknown */
	timeptr->tm_yday  = -1; /* unknown */
	timeptr->tm_isdst = 0;	/* no daylight saving */

	NormUTC(timeptr);

	return 0;
} /* Asctime2tmstruct */



int Asctime2tmstruct2(const char *AscTime, struct tm *timeptr)
{
	if (sscanf(AscTime, "%2d/%2d/%2d %2d:%2d:%2d",
			   &timeptr->tm_mday, &timeptr->tm_mon, &timeptr->tm_year,
			   &timeptr->tm_hour, &timeptr->tm_min, &timeptr->tm_sec) != 6)
		return -1;

	--timeptr->tm_mon;			/* has to be in the range [0, 11]	*/
	timeptr->tm_wday  = -1;		/* unknown							*/
	timeptr->tm_yday  = -1;		/* unknown							*/
	timeptr->tm_isdst = 0;		/* no daylight saving				*/

	NormUTC(timeptr);

	return 0;
} /* Asctime2tmstruct2 */



char *Unsigned2DecHex(const unsigned UVal, char *HexVal)
{
	static char tmp[10], *s;

	s = HexVal ? HexVal : tmp;
	sprintf(s, "%.4X.%.4X", UVal >> 16, UVal & 0xffff);

	return s;
} /* Unsigned2DecHex */



char *Double2DecHex(const double DVal, char *HexVal)
{
	static char tmp[10], *s;
	double	dum;

	s = HexVal ? HexVal : tmp;
	sprintf(s, "%.4X.%.4X", (unsigned)DVal % 0x10000,
							(unsigned)(0x10000 * modf(DVal, &dum)));

	return s;
} /* Double2DecHex */



unsigned long WrapAdd(const unsigned nbits, const unsigned long value,
					  const long AddVal)
{
	unsigned long MaxVal_1, val;

	MaxVal_1 = 1 << nbits;
	val = (AddVal >= 0 ? AddVal : -AddVal) % MaxVal_1;
	if (AddVal >=0)
		return (value+val) % MaxVal_1;
	else
		return (MaxVal_1-val+value) % MaxVal_1;
} /* WrapAdd */



unsigned MaxVector(const unsigned Vector[], const unsigned nElements,
				   unsigned *MaxVal)
{
	register unsigned n, ind, val;

	val = Vector[0];
	ind = 0;
	for (n=1; n<nElements; ++n)
		if (Vector[n] > val) {
			ind = n;
			val = Vector[n];
		}

	*MaxVal = val;
	return ind;
} /* MaxVector */



unsigned MaxDVector(const double Vector[], const unsigned nElements,
				    double *MaxVal)
{
	register unsigned	n, ind;
	double				val;

	val = Vector[0];
	ind = 0;
	for (n=1; n<nElements; ++n)
		if (Vector[n] > val) {
			ind = n;
			val = Vector[n];
		}

	*MaxVal = val;

	return ind;
} /* MaxDVector */



unsigned MinDVector(const double Vector[], const unsigned nElements,
				    double *MaxVal)
{
	register unsigned	n, ind;
	double				val;

	val = Vector[0];
	ind = 0;
	for (n=1; n<nElements; ++n)
		if (Vector[n] < val) {
			ind = n;
			val = Vector[n];
		}

	*MaxVal = val;

	return ind;
} /* MinDVector */



int ExpandDoubleIntervals(const char *String,
						  DblInt_t Interval[],
						  const unsigned MaxIntervals)
{
	char		tmp[256];
	char		*s;
	unsigned	nInts;

	if (!String || *String == '\0')
		if (MaxIntervals > 0) {
			Interval[0].lo = 0.;
			Interval[0].hi = HUGE_VAL;
			return 1;
		} else
			return -1;
	nInts = 0;
	strncpy(tmp, String, sizeof(tmp));
	s = strtok(tmp, ",;");
	while (s) {
		char *t;

		if (nInts >= MaxIntervals) {
			Error("ExpandDoubleIntervals", "Too many intervals");
			return -1;
		}
		if (*s != '-') {
			sscanf(s, "%lf", &Interval[nInts].lo);
			t = strchr(s, '-');
		} else {
			Interval[nInts].lo = 0.;
			t = s;
		}
		if (t)
			if (*++t)
				sscanf(t, "%lf", &Interval[nInts].hi);
			else
				Interval[nInts].hi = HUGE_VAL;
		else {
			Error("ExpandDoubleIntervals", "Missing upper boundary");
			Interval[nInts].hi = HUGE_VAL;
		}
		if (Interval[nInts].lo >= Interval[nInts].hi) {
			Error("ExpandDoubleIntervals", "lower boundary is larger than "
				  "upper one");
			return -1;
		}
		++nInts;
		s = strtok(NULL, ",;");
	}

	return (int)nInts;
} /* ExpandDoubleIntervals */


BOOL IsClose(const double Val, const double NominalVal, const double Tolerance)
{
	return 100.*fabs(Val - NominalVal)/fabs(NominalVal) <= Tolerance;
} /* IsClose */



int InWhichIntInterval(const int Val, const int nInts, const int Bounds[])
{
	int	mid, lo, hi;

	if (nInts <= 0 || Val < Bounds[0] || Val >= Bounds[nInts])
		return -1;

	lo = 0;
	hi = nInts - 1;

	while (lo < hi-1) {
		mid = (lo+hi)/2;
		if (Val < Bounds[mid])
			hi = mid - 1;
		else
			lo = mid;
	}

	return Val < Bounds[hi] ? lo : hi;
}



IntInt_t *FindInterval(const double Vec[], unsigned nElem, double LoVal,
					   double HiVal)
{
	static IntInt_t	IndInt;
	int				i;

	if (LoVal >= HiVal || nElem < 2)
		return NULLPTR(IntInt_t);

	/*
	 *	find largest index i (0 <= i < nElem) with Vec[i] <= LoVal
	 */
	for (i=0; i < nElem && Vec[i] <= LoVal; ++i);
	if (i >= nElem)
		return NULLPTR(IntInt_t);
	else
		IndInt.lo = MAX(i-1, 0);

	/*
	 *	find smallest integer i (0 <= i < nElem) with Vec[i] >= HiVal
	 */
	for (i=IndInt.lo; i < nElem && Vec[i] < HiVal; ++i);
	if (i == IndInt.lo)
		return NULLPTR(IntInt_t);
	else
		IndInt.hi = i >= nElem ? nElem-1 : i;
	
	return &IndInt;
} /* FindInterval */



char *FormatDouble(const double Val, char String[])
{
	double	dum;

	sprintf(String, "%.15G", Val);
	if (modf(Val, &dum) == 0. && !strchr(String, 'E'))
		strcat(String, ".0");

	return String;
} /* FormatDouble */



unsigned UniRndNumInit(unsigned seed)
{
	srand(seed ? seed : (unsigned)time((time_t *)NULL));
	srand(seed ? seed : (unsigned)time((time_t *)NULL));
	seed = rand();

	RANLUX_RLUXGO(2, seed);

	return seed;
} /* UniRndNumInit */


#define RNDNUM_BUFF_LEN     100         /* 100 numbers in buffer        */
double UniRndNum(void)
{
    static float RndNumBuff[RNDNUM_BUFF_LEN];
    static unsigned BuffIndx = RNDNUM_BUFF_LEN;

    if (BuffIndx < RNDNUM_BUFF_LEN)
        /*
         *  still unused numbers in buffer
         */
        return (double)RndNumBuff[BuffIndx++];
    else {
        /*
         *  re-fill the buffer
         */
        RANLUX_RLUX(RndNumBuff, RNDNUM_BUFF_LEN);
        BuffIndx = 0;

        return UniRndNum();
    }
} /* UniRndNum */



unsigned CeilPow2(unsigned Num)
{
	unsigned Pow2[] = { 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048,
						4096, 8192, 16384, 32768 },
			 n;
	
	if (Num <= Pow2[NUM_OF(Pow2) - 1])
		for (n=0; n<NUM_OF(Pow2); ++n)
			if (Pow2[n] >= Num)
				return Pow2[n];

	return 0;
} /* CeilPow2 */



/*----------------------------------------------------------------------------
 *	SYNOPSIS:
 *		int DblCmp(const double *a, const double *b)
 *
 *	DESCRIPTION:
 *		Support function for C-library function `qsort'
 *
 *	PARAMETERS:
 *		I:	a	: pointer to first double
 *			b	: pointer to second double
 *
 *	RETURNS:
 *		-1/0/1: *a<*b /  *a==*b / *a>*b
 *--------------------------------------------------------------------------*/
static int DblCmp(const void *a, const void *b)
{
	if (*(double *)a < *(double *)b)
		return -1;
	else if (*(double *)a > *(double *)b)
		return 1;
	else
		return 0;
} /* DblCmp */



/*----------------------------------------------------------------------------
 *	SYNOPSIS:
 *		int VectorMedian(double DVec, unsigned Size, double *Median);
 *
 *	DESCRIPTION:
 *		Find the Median of a double vector
 *
 *	PARAMETERS:
 *		I:	DVec	: the double vector
 *			Size	: number of elements in <DVec>
 *		O:	*Median	: the computed Median in <DVec>
 *
 *	RETURNS:
 *		0/1: success/memory problems
 *--------------------------------------------------------------------------*/
int VectorMedian(const double DVec[], unsigned Size, double *Median)
{
	unsigned	Cntr;
	double		*LocVec;

	/*
	 *	make local copy of DVec
	 */
	GETMEM(LocVec, double, Size, "VectorMedian", 1);
	memcpy(LocVec, DVec, Size * sizeof(double));

	/*
	 *	sort local vector
	 */
	qsort(LocVec, Size, sizeof(double), DblCmp);

	Cntr = Size / 2;
	if (2*Cntr == Size)
		*Median = .5 * (LocVec[Cntr-1] + LocVec[Cntr]);
	else
		*Median = LocVec[Cntr];

	free(LocVec);
	return 0;
} /* VectorMedian */



BOOL Next4PointsClose(const double Vec[4], double Val, double Limit)
{
	unsigned	i;

	for (i=0; i<4; ++i)
		if (fabs(Vec[i] - Val) > Limit)
			return FALSE;

	return TRUE;
} /* Next4PointsClose */



int AdjustIndex4Points(const double TVec[], double T, int ind, unsigned nRecs,
					   double Limit)
{
	int	NomInd = ind - 2;

	if (NomInd >= 0 && NomInd + 3 < nRecs &&
			Next4PointsClose(TVec+NomInd, T, Limit))
		/*
		 *	the most common case: all indices within bounds and all 4 points
		 *	not too far apart
		 */
		return NomInd;
	else {
		int	i;

		for (i=NomInd-2; i<NomInd+3; ++i)
			if (i >= 0 && i + 3 < nRecs && Next4PointsClose(TVec+i, T, Limit))
				return i;	/* index i is ok */

		/*
		 *	no good index found
		 */
		return -1;
	}
} /* AdjustIndex4Points */

#ifdef HPUX
/*
 *      HP-UX does not seems to have cbrt/rint
 */
double cbrt(double x) {
	if (x > 0.)
		return exp(log(x)/3.);
	else
		return 0.;
} /* cbrt */

#if 0
double rint(double x)
{
	if (x > 0.)
		return (double)(int)(x + .5);
	else
		return -(double)(int)(-x + .5);
} /* rint */
#endif

#endif
