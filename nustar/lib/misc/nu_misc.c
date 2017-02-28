/*
 *	nu_misc.c: --- miscellaneous functions ---
 *
 *	CHANGE HISTORY:
 *
 *      0.1.0 - NS 25/10/2010 - First version
 *      0.1.1 - NS 10/03/2011 - Added 'get_ran2seed' routine
 *      0.1.2 - NS 10/03/2011 - Added 'SplitFilePath' routine
 *      0.1.3 - NS 30/04/2012 - Added 'ComputeDoubleMedian' routine
 *      0.1.4 - NS 24/07/2012 - Added 'FilterTimesByGTI' routine
 *      0.1.5 - NS 13/10/2014 - Added 'RemoveTrailingSlash' routine
 *
 *	AUTHOR:
 *
 *           ASDC - ASI Science Data Center             
 */

#define NU_MISC_C
#define NU_MISC_VERSION		0.1.5


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

/* highfits header */
#include <fitsio.h>
#include <headas_gti.h>

/* nustar local headers */
#include "nu_defs.h"

/* misc header */
#include "nu_misc.h"
#include "nu_termio.h"


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


char *StripExtension(char *filename)
{
	char *tmp;

	if ((tmp = strrchr(filename, '.')))
		*tmp = '\0';
	return filename;
} /* StripExtension */


char *GetFilenameExtension(const char *name, char *ext)
{
	return FileNameExt(name, ext, '.');
} /* GetFilenameExtension */

char *SplitFilePath(const char *FilePath, char *DirName, char *BaseName)
{
  static char	Base[MAXFNAME_LEN];		/* place to store basename of file	*/
  char		*tmp;				/* pointer to last '/' in FilePath	*/
  char          *BasePtr;			/* pointer to Base			*/
  
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


char *DeriveFileName(const char *OldName, char *NewName, const char *ext)
{
  char gext[MAXEXT_LEN], fext[MAXEXT_LEN];
  /* char *tmp; */

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


int UVecMatch(const unsigned val, const unsigned Vector[], const unsigned n)
{
	unsigned	i;

	for (i=0; i<n; ++i)
		if (val == Vector[i])
			return i;

	return -1;
} /* UVecMatch */


void GetGMTDateTime(char DateStr[25])
{
  int timeref = 0;
  int status=0;
  ffgstm( DateStr, &timeref, &status );
} /* GetNewDate */


void get_ran2seed(long int *seed)
{
  char taskname[MAXFNAME_LEN];
  get_toolnamev(taskname);

  *seed = (long int)(time((time_t *)NULL));
  srand( *seed );
  *seed = rand();
  ( *seed > 0 ) ? (*seed *= -1.) : *seed;

  headas_chat(CHATTY,"%s: Info: get_ran2seed: Init ran2 seed to %ld\n",taskname, *seed);

} /* get_ran2seed */


/*
 *
 *  ComputeDoubleMedian
 *
 *  DESCRIPTION:
 *    Routine to Calculate the median value for array of double
 *
 *
 */
double ComputeDoubleMedian(double * dvalue, int length) 
{
  int    mod=0, jj, iii, ind;
  double *median, level;
  double medianval,diff_db;

  median=(double *)malloc(length*sizeof(double));

  for (jj=0; jj< length; jj++)
    {
      level=dvalue[jj];
      
      if(jj==0)
	median[jj]=level;
      else 
	if(level >= median[jj-1])
	  median[jj]=level;
	else
	  {
	    for(iii=jj; iii >= 0 ; iii--)
	      {
		if(!iii)
		  median[iii]=level;
		else 
		  if (level < median[iii-1])
		    {
		      median[iii]=median[iii-1];
		    }
		  else
		    {
		      median[iii]=level;
		      iii=0;
		    }
	      }
	  }
    }

  mod = length%2;
  if (mod)
    {
      medianval = median[(int)(length/2.0)];
    }
  else
    {
      ind=(int)(length/2.0);
      if(!ind)
	diff_db=median[ind];
      else
	diff_db=((median[ind] + median[ind - 1])/2.);
      medianval=diff_db;
    }
  
  free(median);
  return medianval;

} /* ComputeMedian */


int FilterTimesByGTI(char *infile, char *extname, char *gtifile, char *gtiextname){

  fitsfile *inunit=NULL;   /* Input and Output fits file pointer */
  long    nrows, *rowlist;
  int     i, k, nrowlist, time_colnum, *segs;
  double  *time;
  int     status=OK;
  char    taskname[MAXFNAME_LEN];
  struct gti_struct  gti; 

  get_toolnamev(taskname);


  /* Read GTI info from gtifile */
  if(HDgti_read(gtifile, &gti, gtiextname, 0, 0, 0, 0, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' file.\n", taskname, gtifile);
      goto FilterTimesByGTI_end;
    }


  /* Open input file */
  if ( fits_open_file(&inunit, infile, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, infile);
      goto FilterTimesByGTI_end;
    }

  
  /* Move in <extname> extension in input file */
  if (fits_movnam_hdu(inunit, ANY_HDU, extname, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", taskname, extname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, infile); 
      goto FilterTimesByGTI_end;
    }

  /* Get rows number */
  if (fits_get_num_rows(inunit, &nrows, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to get the total number of rows in\n", taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", taskname, infile);
      goto FilterTimesByGTI_end;    
    }

  /* Get TIME column index */
  if ( fits_get_colnum(inunit, CASEINSEN, CLNM_TIME, &time_colnum, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", taskname, infile);
      goto FilterTimesByGTI_end;
    }


  time = (double*)calloc(nrows, sizeof(double));
  segs = (int*)calloc(nrows, sizeof(int));
  rowlist  = (long*)calloc(nrows, sizeof(long));

  if( time==NULL || segs==NULL || rowlist==NULL )
    {
      headas_chat(NORMAL, "%s: Error: FilterTimesByGTI: memory allocation failure.\n", taskname);
      goto FilterTimesByGTI_end;
    }


  if (fits_read_col(inunit, TDOUBLE, time_colnum, 1, 1, nrows, NULL, time, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s column\n", taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", taskname, infile); 
      goto FilterTimesByGTI_end;
    }

  HDgti_where(&gti, nrows, time, segs, &status);
  if(status!=OK){
    headas_chat(NORMAL, "%s: Error: unable to check if time %f is included in GTI intervals\n", taskname, time);
    goto FilterTimesByGTI_end;
  }

  nrowlist = 0;

  for(k=0; k<nrows; k++)
    {
      if(segs[k]<0)
	{
	  rowlist[nrowlist] = k+1;
	  nrowlist++;
	}
    }
      
  if (fits_delete_rowlist(inunit, rowlist, nrowlist, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to delete row not in GTI\n", taskname);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", taskname, infile);
      goto FilterTimesByGTI_end;
    }
  
  
  if ( fits_close_file(inunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", taskname, infile);
      return NOT_OK;
    }

  
  return OK;
  
 FilterTimesByGTI_end:
  
  if (inunit != NULL)
    fits_close_file(inunit, &status);
  
  return NOT_OK;


} /* FilterTimesByGTI */


void RemoveTrailingSlash(char *filename){
  
  size_t stringLen;
  
  stringLen = strlen(filename);
  
  while(0u < stringLen && '/' == filename[stringLen - 1u]){
    filename[stringLen - 1u] = '\0';
    stringLen--;
  }
  
} /* RemoveTrailingSlash */
