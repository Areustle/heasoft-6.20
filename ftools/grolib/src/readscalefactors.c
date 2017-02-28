#include "fitsio.h"
#include "cfortran.h"
#include "xpi.h"

void ReadScaleFactors(char *pFits, int *viewP, float *fact, int *status);


/*******************************************************************************
 * Processing for each energy level
 * Read the FITS file whose name is passed, get its energy levels. Then read the
 * scale factor file and get the factors for each energy range. Display the
 * values found in the window.
 ******************************************************************************/
void ReadScaleFactors(char *pFits, int *viewP, float *fact, int *status)
{
   fitsfile *fptr=NULL;

   float *fnulval=NULL;

   long  nrows=0L;

   int	 found, i, j;
   int   *anynul=NULL;
   int   hdutype=0;

   short *nulval=NULL;
   short *vp=NULL;

   char	 ScalFact[80];


   *status = 0;

   /* Try to open the scale factor file */
   sprintf(ScalFact, "%sscale.factor.fits", pFits);

   if (fits_open_file(&fptr, ScalFact, READONLY, status))
      fprintf(stderr, "Cannot open %s\n", ScalFact);

   else
   {
      /* Look for the viewing period prefix */
      *status = fits_movabs_hdu(fptr, 2, &hdutype, status);

      if (*status == 0)
      {
         *status = fits_get_num_rows(fptr, &nrows, status);
	 if (*status == 0)
	 {
            vp = (short *) malloc((int)nrows*sizeof(short));
            *status = fits_read_col(fptr, TSHORT, 1, 1L, 1L, nrows, nulval, vp, anynul, 
				    status);

            if ((*viewP >= vp[0]) || (*viewP <= vp[nrows-1]))
            {
               found = 0;
	       for (i=0; i<nrows && !found; ++i)
	          if (*viewP == vp[i])
	             found = 1;

   	       if (found)
	       {
	          for (j=2; j<=11; ++j)
	          {
                     *status = fits_read_col(fptr, TFLOAT, j, i, 1L, 1L, fnulval, 
					     &fact[j-2], anynul, status);
	          }
	       }
            }
	 }
      }
            
      *status = fits_close_file(fptr, status);
   }
}

FCALLSCSUB4(ReadScaleFactors, READSCALEFACTORS, read_scale_factors, PSTRING, PINT, PFLOAT, PINT)
