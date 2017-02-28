/* 
--------------------------------------------------------------------------------
Function:
     CreateNewFits

Description:
   
     This routine initialises the output file. 
     Step1: calls CheckFile to test if there is pre-existing file
            and do the needful
     Step2: opens and initilises the new file for writing
     Step3: calls FFPHPR to write mandatory keywords for Primary
            calls FFPDAT for date stamp
     Step4: returns to the called function

     NOTE: file is kept opened after this. 

     Returns 0 -- everything ok
             

Functions called:

    FITS functions:
      ffinit -- opens and initialises the new FITS file
      ffphpr -- writes the user defined mandatory keywords for Primary Array
      ffpdat -- writes the date stamp
 
    general utility functions:
      Printerror -- prints FITSIO error messages 

Author and modification history:
      Sandhia Bansal & Banashree M Seifert (July, 1997)
 
Usage:
int CreateNewFits(fitsfile **infp1, char *filename, int simple, int bitpix, 
                  int naxis, long *naxes, long pcount, long gcount, 
                  int extend, int clobber, int *status)


------------------------------------------------------------------------------*/
#include "general.h"
#include "fitsio.h"        /* cfitsio defined constants         */

int CreateNewFits(fitsfile **infp1, char *filename, int simple, int bitpix, 
		  int naxis, long *naxes, long pcount, long gcount, int extend, 
                  int chatter, int clobber, int *status)
{  
    char str[4], subinfo[100];
    int exist=0;
   
    /* check that there is no preexisting file in that name */

    CheckFile(filename, chatter, clobber);

   /* initialise output file */

   *status = 0;
    if(ffinit(infp1, filename, status)) Printerror(*status);
   
/*-----------------------------------------------------------
           Write Mandatory Primary Array Keywords
-------------------------------------------------------------*/

   if(ffphpr(*infp1, simple, bitpix, naxis, naxes, pcount, gcount, extend, 
	     status))
      Printerror(*status);

   if(ffpdat(*infp1, status))
      Printerror(*status);

   return 0;
  
} 





