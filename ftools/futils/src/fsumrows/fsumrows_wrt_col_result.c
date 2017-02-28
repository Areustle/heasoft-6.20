/******************************************************************************

Filename:
     fsumrows_wrt_col_result.c

Function:
     fsumrows_wrt_col_result

Description:
     Write a cell of data of an arbitrary data type to an ASCII or binary
     table, flagging NULL values.
 
Author: 


Modification history:

Usage:
     void fsumrows_wrt_col_result (fitsfile *fptr, int type, int colnum,
                                   long nelem, void *buffer, char *nularray,
                                   int nullval, int *status);

Primary Variables:
     --- Inputs ---
     fptr     : Input FITS file pointer
     type     : FITS data type to be written out of buffer
     colnum   : Column number to be written to
     nelem    : Number of elements to be written
     buffer   : Pointer to array holding the data
     nularray : Array holding the NULL flags
     nullval  : Value to assign to integer NULL data
     status   : FITSIO error code

     --- Internal ---
     nul_*    : NULL value properly converted to data type 'type'

******************************************************************************/

#include <ftools.h>
#if (defined __APPLE__) || (defined __CYGWIN__)
#include <float.h>
#include <limits.h>
#define MAXDOUBLE DBL_MAX
#define MAXFLOAT  FLT_MAX
#define MAXINT    INT_MAX
#else
#include <values.h>
#endif

#include "fsumrows.h"

void fsumrows_wrt_col_result (fitsfile *fptr,
                              int type,
                              int colnum,
                              long nelem,
                              void *buffer,
                              char *nularray,
                              int nullval,
                              int *status)

{

   long elem;
   unsigned char nul_byt;
   unsigned short nul_usht;
   short nul_sht;
   unsigned long nul_ulng;
   long nul_lng;
   float nul_flt;
   double nul_dbl;

   switch (type)
   {
      case TBYTE:
         nul_byt = (unsigned char)nullval;
         for (elem = 0; elem < nelem; elem++)
	 {
            if (nularray[elem])
            {
               ((unsigned char *)buffer)[elem] = nul_byt;
            }
         }
         fits_write_colnull_byt (fptr, colnum, 1L, 1L, nelem,
                                 (unsigned char *)buffer, nul_byt, status);
         break;
      case TUSHORT:
         nul_usht = (unsigned short)nullval;
         for (elem = 0; elem < nelem; elem++)
	 {
            if (nularray[elem])
            {
               ((unsigned short *)buffer)[elem] = nul_usht;
            }
         }
         fits_write_colnull_usht (fptr, colnum, 1L, 1L, nelem,
                                  (unsigned short *)buffer, nul_usht, status);
         break;
      case TSHORT:
         nul_sht = (short)nullval;
         for (elem = 0; elem < nelem; elem++)
	 {
            if (nularray[elem])
            {
               ((short *)buffer)[elem] = nul_sht;
            }
         }
         fits_write_colnull_sht (fptr, colnum, 1L, 1L, nelem,
                                 (short *)buffer, nul_sht, status);
         break;
      case TULONG:
         nul_ulng = (unsigned long)nullval;
         for (elem = 0; elem < nelem; elem++)
	 {
            if (nularray[elem])
            {
               ((unsigned long *)buffer)[elem] = nul_ulng;
            }
         }
         fits_write_colnull_ulng (fptr, colnum, 1L, 1L, nelem,
                                  (unsigned long *)buffer, nul_ulng, status);
         break;
      case TLONG:
         nul_lng = (long)nullval;
         for (elem = 0; elem < nelem; elem++)
	 {
            if (nularray[elem])
            {
               ((long *)buffer)[elem] = nul_lng;
            }
         }
         fits_write_colnull_lng (fptr, colnum, 1L, 1L, nelem,
                                 (long *)buffer, nul_lng, status);
         break;
      case TFLOAT:
         nul_flt = MAXFLOAT;
         for (elem = 0; elem < nelem; elem++)
	 {
            if (nularray[elem])
            {
               ((float *)buffer)[elem] = nul_flt;
            }
         }
         fits_write_colnull_flt (fptr, colnum, 1L, 1L, nelem,
                                 (float *)buffer, nul_flt, status);
         break;
      case TDOUBLE:
         nul_dbl = MAXDOUBLE;
         for (elem = 0; elem < nelem; elem++)
	 {
            if (nularray[elem])
            {
               ((double *)buffer)[elem] = nul_dbl;
            }
         }
         fits_write_colnull_dbl (fptr, colnum, 1L, 1L, nelem,
                                 (double *)buffer, nul_dbl, status);
         break;
      default:
         *status = NOT_OK;
         break;
   }

   return;

}
