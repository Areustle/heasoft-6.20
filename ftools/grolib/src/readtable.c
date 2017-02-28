/*C*******************************************************************************
C SUBROUTINE: read_table
C
C DESCRIPTION: Program for reading tjd-start, tjd-stop, x-ra, x-dec, z-ra, and
C     z-dec from an ascii file.  The program reads the file until it finds a 
C     record in the file that matches on the VP number specfied by the user.
C     If the program doesn't find a matching record, it returns an error status.
C
C AUTHOR/DATE: Sandhia Bansal - 12/13/01
C
C NOTES:
C
C HIDDEN ARGUMENTS:
C
C ARGUMENTS:
C     input:
C        vpnum     - user specified value for vp
C
C     output:
C        tjd_start - start time (r*4)
C        tjd_stop  - stop time  (r*4)
C        zradeg    - r.a. of CGRO z-axis in degrees (r*4)
C        zdecdg    - dec. of CGRO z-axis in degrees (r*4)
C        xradeg    - r.a. of CGRO x-axis in degrees (r*4)
C        xdecdg    - dec. of CGRO x-axis in degrees (r*4)
C        status    - program status
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C MODIFICATION HISTORY:
c
c made $LHEA_DATA/ the default directory for "vp_list.fits" file (crs 05/2004)
c
C*******************************************************************************
*/

#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include <stdio.h>
#include <stdlib.h>

#define  SIZE  90

void readtable(char *data_dir, int *vpnum, float *tjd_start, float *tjd_stop, 
	       float *xradeg, float *xdecdg, float *zradeg, float *zdecdg, 
	       int *all, int *status);


void readtable(char *data_dir, int *vpnum, float *tjd_start, float *tjd_stop, 
	       float *xradeg, float *xdecdg, float *zradeg, float *zdecdg, 
	       int *all, int *status)
{
   float     *fnul=NULL;

   long      nrows=0L;

   int       irows=0;
   int       *anynul=NULL;
   int       i=0;

   short     *viewPer=NULL;
   short     *snul=NULL;

   char      msg[80];
   char      found=0;
   char      fname[80];

   fitsfile  *fptr=NULL;



   *status = 0;

   if (getenv("LHEA_DATA") != NULL) 
       strcat(strcpy(fname, getenv("LHEA_DATA")), "/vp_list.fits");
	else
	 strcat(strcpy(fname, data_dir), "vp_list.fits");

   *status = fits_open_file(&fptr, fname, READONLY, status);

   *status = fits_movnam_hdu(fptr, BINARY_TBL, "VP_Table", 0, status);
   *status = fits_get_num_rows(fptr, &nrows, status);

   if (*status == 0)
   {
      irows = (int)nrows;

      viewPer = (short *) malloc(irows*sizeof(short));
      *status = fits_read_col(fptr, TSHORT, 1, 1L, 1L, nrows, snul, viewPer,
			      anynul, status);
      if (*status == 0)
      {
	 if ((*vpnum >= viewPer[0]) && (*vpnum <= viewPer[irows-1]))
	 {
	    while ((i < irows) && (*vpnum >= viewPer[i]))
	    {
	       if (*vpnum == viewPer[i])
	       {
		  found = 1;
		  break;
	       }
	       else
		  ++i;
	    }

	    if (found)
	    {
	       *status = fits_read_col(fptr, TFLOAT, 4, i+1, 1L, 1L, fnul, 
				       tjd_start, anynul, status);
	       *status = fits_read_col(fptr, TFLOAT, 7, i+1, 1L, 1L, fnul, 
				       tjd_stop, anynul, status);
	       printf("vp: %d tjdStart: %f tjdStop: %f \n", 
		      *vpnum, *tjd_start, *tjd_stop);
	       if ((all) && (*status==0))
	       {
		  *status = fits_read_col(fptr, TFLOAT, 8, i+1, 1L, 1L, fnul, 
					  zradeg, anynul, status);
		  *status = fits_read_col(fptr, TFLOAT, 9, i+1, 1L, 1L, fnul, 
				          zdecdg, anynul, status);
	          *status = fits_read_col(fptr, TFLOAT, 10, i+1, 1L, 1L, fnul, 
			  	          xradeg, anynul, status);
	          *status = fits_read_col(fptr, TFLOAT, 11, i+1, 1L, 1L, fnul, 
				          xdecdg, anynul, status);
		  printf("zra: %f zdec: %f xra: %f xdec: %f \n",
			 *zradeg, *zdecdg, *xradeg, *xdecdg);
	       }
	    }
	 }

	 if (!found)
	 {
	    *status = 1;
	    sprintf(msg, "viewPer# %d does not exist in VP_Table extension", *vpnum);
	    Fcerr(msg);
	 }
      }
   }

   if ((*status < 0) || (*status > 1))
      fits_report_error(stderr, *status);

}



FCALLSCSUB10(readtable,READTABLE,readtable,PSTRING,PINT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PINT,PINT)
