/* 
-------- Function: -------------------------------------------------------------

     WriteRmf1


-------- Description: ----------------------------------------------------------

     This subroutine Creates and Writes the RMF extension for an RMF file 
     in one of the formats conforming to the HDUVERS2='1.*.*' family.
     Currently the following formats are supported (see OGIP/92-002a)
     HDUVERS2 = '1.0.0'
     HDUVERS2 = '1.1.0'
     HDUVERS2 = '1.2.0'
     but HDUVERS2 = '1.0.0' & '1.1.0' will be overridden such that '1.2.0' 
     is written. 

   Assumptions:
   
      The FITS file is open and has had the Primary Header written
        !!! Note !!!! File is left open at the end  
        and  MUST BE CLOSED               by fits_close_file 
        or   ANOTHER EXTENSION ADDED      by fits_create_table
        in order to (automatically) write the mandatory END header keyword.

	The matrix will be written as a VARIABLE LENGTH ARRAY if the reduction 
	in storage requirements (ie total number of stroed values) exceeds a 
	factor 3.0 over that obtained using a FIXED length array.


-------- Functions called: -----------------------------------------------------

   FITS functions:
      fits_create_table    : (FITSIO) Creates a new FITS extension file
      fits_write_col       : (FITSIO) Writes the data
      fits_write_comment   : (FITSIO) Writes a FITS comment keyword  
      fit_write_history    : (FITSIO) Writes a FITS history keyword
      fits_write_key       : (FITSIO) Writes a keyword
      fits_write_key_lng   : (FITSIO) Writes a keyword in LONG format
      fits_write_key_str   : (FITSIO) Writes a keyword in STRING format

   General Utility Functions:
      DispMsg      -- displays messages
      Printerror   -- prints FITSIO error messages
      PrintWarning -- prints FITSIO warnings


-------- Usage: ----------------------------------------------------------------

   int WriteRmf1(fitsfile *fptr, int chatter, int nk_history, char **history, 
                 int nk_comm, char **comment, char *rmfversn, char *hduclas3, 
		 char *telescop, char *instrume, char *detnam, char *filter, 
		 float areascal, char *chantype, int flchan, int ichan, 
		 int ienerg, float *energ_lo, float *energ_hi, int *ngrp, 
		 int **f_chan, int **n_chan, float **fmatrix, float lo_thresh)

   Input Parameters:		 
       FPTR          fitsfile  : Pointer to the output FITS file
       CHATTER       int       : Chattiness flag for o/p (5 quite,10 normal,
                                 >20 silly)
       NK_HISTORY    int       : No. records to be written as HISTORY records
       HISTORY       char      : Array of history strings to be written
       NK_COMM       int       : No. records to be written as COMMENT records
       COMMENT       char      : Array of comment strings to be written
       RMFVERSN      char      : String denoting OGIP HDUVERS2 family  
       HDUCLAS3      char      : String containing HDUCLAS3 value
       TELESCOP      char      : String listing telescope/mission
       INSTRUME      char      : String listing instrument/detector
       DETNAM        char      : String listing specific detector name
       FILTER        char      : String listing instrument filter in use
       AREASCAL      float     : Area scaling factor
       CHANTYPE      char      : Type of detector channels in use (PHA, PI)
       FLCHAN        int       : Lowest legal channel for this detector
       ICHAN         int       : No. channels in the full array
       IENERG        int       : No. energy bins
       ENERG_LO      float     : Array containing lower bound to each energy bin
       ENERG_HI      float     : Array containing upper bound to each energy bin
       NGRP          int       : Array containing no. channel subsets at each 
                                 energy
       F_CHAN        int       : Array containing 1st chan of each subset at 
                                 each energy
       N_CHAN        int       : Array containing no. chans within each subset 
	                         at each energy
       FMATRIX       float     : Array containing the full matrix
       LO_THRESH     float     : The lower threshold used to construct the matrix

   Output Parameters:
       STATUS        int       : 0=OK.


-------- Include files ---------------------------------------------------------

   <stdio.h>
   <stdlib.h>
   <string.h>
   "general.h"


-------- Origin: ---------------------------------------------------------------

   This function is basically a C version of wtrmf3.f.


-------- Authors/Modification History: -----------------------------------------

   Sandhia Bansal  (1.0.0; Jul 97)

----------------------------------------------------------------------------- */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"



int WriteRmf1(fitsfile *fptr, int chatter, int nk_history, char **history, 
	      int nk_comm, char **comment, char *rmfversn, char *hduclas3, 
	      char *telescop, char *instrume, char *detnam, char *filter, 
	      float areascal, char *chantype, int flchan, int ichan, 
	      int ienerg, float *energ_lo, float *energ_hi, int *ngrp, 
	      int **f_chan, int **n_chan, float **fmatrix, float lo_thresh)
{

   float *values;

   long nrows=(long) ienerg; 
   long firstrow=1, firstelem=1;
   long lvalue=0L;
   long *larray;

   int  status=0;
   int  tfields=6;
   int  tlmax=flchan+ichan-1;
   int  siz_ngrp=0, siz_mat=0, nvar=0;
   int  i=0, j=0, k=0, ie=0, ic=0;
   int  sum=0;
   int  nfixed=0;
   int  qvar=0;
   int  varidat=0;
   int  istart=0, istop=0;
   int  itemp=0;

   char *progname = "WriteRmf1";
   char *version="1.0.0";
   char errstr[FLEN_ERRMSG];
   char wrnstr[FLEN_ERRMSG];
   char message[FLEN_ERRMSG];
   char hduvers2[6];
   char *ttype[] = {"ENERG_LO", "ENERG_HI", "N_GRP", "F_CHAN", "N_CHAN", 
		    "MATRIX"};
   char *tform[] = {" ",        " ",        " ",     " ",      " ",      
		    " "     };
   char *tunit[] = {"keV",      "keV",      " ",     " ",      " ",      
		    " "     };
   char extname[FLEN_VALUE];
   char comm[FLEN_COMMENT];
   char  temp[20];

   /* Initializations */
   strcpy(wrnstr, "** ");
   strcat(strcat(strcat(strcat(wrnstr, progname), " "), version), " WARNING: ");
   strcpy(errstr, "** ");
   strcat(strcat(strcat(strcat(errstr, progname), " "), version), " ERROR: ");

   /* Give user info if requested */
   strcat(strcat(strcat(strcpy(message, " ... using "), progname), 
		 " Version "), version);
   DispMsg(chatter, 15, message);

   if (rmfversn[0] != '1')
   {
      strcat(strcpy(message, wrnstr), " Format/subroutine mismatch");
      DispMsg(1, 1, message);
      
      strcpy(message, 
	     " ...... This routine writes only the 1.*.* family of formats");
      DispMsg(1, 1, message);
      
      strcat(strcpy(message, " ...... Requested Format: "), rmfversn);
      DispMsg(1, 1, message);
      
      status = 15;
   }

   if (status == 0)
   {
      if (strcmp(rmfversn, "1.2.0")==0)
	 strcpy(hduvers2, rmfversn);

      else if ((strcmp(rmfversn, "1.0.0")==0) || (strcmp(rmfversn, "1.1.0")==0))
      {
	 strcpy(hduvers2, "1.2.0");

	 strcpy(message, wrnstr);
	 strcat(strcat(message, " Old format requested: "), rmfversn);
	 DispMsg(1, 1, message);

	 strcpy(message, "Resetting format (HDUVERS2) to ");
	 strcat(message, hduvers2);
	 DispMsg(1, 1, message);
      }

      else
      {
	 strcpy(hduvers2, "1.2.0");

	 strcpy(message, wrnstr);
	 strcat(strcat(message, " Unknown format: "), rmfversn);
	 DispMsg(1, 1, message);

	 strcpy(message, " ...... Resetting format (HDUVERS2) to ");
	 strcat(message, hduvers2);
	 DispMsg(1, 1, message);
      }

      /* Calculate the necessary dimensions of the arrays (columns) */
      for (i=0; i<ienerg; i++)
      {
	 siz_ngrp = Max(siz_ngrp, ngrp[i]);
	 sum = 0;
	 for (j=0; j<ngrp[i]; j++)
	 {
	    sum += n_chan[i][j];
	    nvar += sum;
	    siz_mat = Max(siz_mat, sum);
	 }
      }

      /* Decide whether a variable length array for the matrix column makes
	 sense for the matrix column 
	 ... no. elements stored in the fixed length case */
      nfixed = siz_mat * ienerg;
      if ((siz_mat > 3) && (nvar != nfixed) && (nvar <= 3*nfixed))
      {
	 qvar = 1;
	 varidat = 4 * nvar;
      }
	 
      sprintf(temp, "%d", siz_ngrp);
      strcat(strcpy(message, "MAX # elements in F_CHAN & N_CHAN arrays = "), 
	     temp);
      DispMsg(chatter, 15, message);

      sprintf(temp, "%d", siz_mat);
      strcat(strcpy(message, "Max # elements in MATRIX array = "), temp);
      DispMsg(chatter, 15, message);

      if (qvar == 1)
	 strcpy(message, "Using Variable length array for MATRIX column");
      else
	 strcpy(message, "Using fixed length array for MATRIX column");
      DispMsg(chatter, 15, message);

      for (i=0; i<tfields; i++)
	 tform[i] = (char *) malloc(FLEN_VALUE*sizeof(char));

      strcpy(tform[0], "E");
      strcpy(tform[1], "E");
      strcpy(tform[2], "I");
      sprintf(tform[3], "%dJ", siz_ngrp);
      sprintf(tform[4], "%dJ", siz_ngrp);
      if (qvar == 1)
	 strcpy(tform[5], "PE");
      else
	 sprintf(tform[5], "%dE", siz_mat);

      if (strcmp(hduclas3, "REDIST") == 0)
	 strcpy(extname, "MATRIX");
      else
	 strcpy(extname, "SPECRESP MATRIX");

      if (fits_create_tbl(fptr, BINARY_TBL, nrows, tfields, ttype, tform, tunit,
			  extname, &status))
      {
	 DispMsg(1, 1, "Error while creating RMF extension");
	 Printerror(status);
      }

      strcpy(message, " ... new extension created");
      DispMsg(chatter, 15, message);

      strcpy(message, " ... written the extension header keywords");
      DispMsg(chatter, 15, message);

      /* New for HDUVERS2 1.2.0 */
      if (fits_write_key_lng(fptr, "TLMIN4", flchan,
			     "Minimum value legally allowed in column 4", 
			     &status))
      {
	 PrintWarning(progname, version, "Error while writing TLMIN4 keyword", 
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key_lng(fptr, "TLMAX4", tlmax,
			     "Maximum value legally allowed in column 4", 
			     &status))
      {
	 PrintWarning(progname, version, "Error while writing TLMAX4 keyword", 
		      1, 1, status);
	 status = 0;
      }

      /* Write the HDUCLASn & HDUVERSn keywords */
      if (fits_write_key(fptr, TSTRING, "HDUCLASS", "OGIP",
			 "Format confirms to OGIP standard", &status))
      {
	 PrintWarning(progname, version, "Error while writing HDUCLASS keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key(fptr, TSTRING, "HDUCLAS1", "RESPONSE",
			 "dataset relates to spectral response", &status))
      {
	 PrintWarning(progname, version, "Error while writing HDUCLAS1 keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key(fptr, TSTRING, "HDUVERS1", "1.0.0",
			 "Version of family of formats", &status))
      {
	 PrintWarning(progname, version, "Error while writing HDUVERS1 keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key(fptr, TSTRING, "HDUCLAS2", "RSP_MATRIX",
			 "Nominal energies of PHA chan boundaries", &status))
      {
	 PrintWarning(progname, version, "Error while writing HDUCLAS2 keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key(fptr, TSTRING, "HDUVERS2", hduvers2,
			 "Version of format (OGIP memo CAL/GEN/92-002a)", 
			 &status))
      {
	 PrintWarning(progname, version, "Error while writing HDUVERS2 keyword",
		      1, 1, status);
	 status = 0;
      }
	 
      if (strcmp(hduclas3, "REDIST") == 0)
	 strcpy(message, "photon redistribution matrix (only)");
      else if (strcmp(hduclas3, "FULL") == 0)
	 strcpy(message, "convolved w/ all effects (det + optics)");
      else if (strcmp(hduclas3, "DETECTOR") == 0)
	 strcpy(message, "convolved w/ detector effects (only)");
      else
	 strcpy(message, "WARNING This is NOT an OGIP-approved value");

      if (fits_write_key(fptr, TSTRING, "HDUCLAS3", hduclas3, message, &status))
      {
	 PrintWarning(progname, version, "Error while writing HDUCLAS3 keyword",
		      1, 1, status);
	 status = 0;
      }

      strcpy(message, " ... written the HDUCLAS/HDUVERSkeywords");
      DispMsg(chatter, 15, message);

      /* Add the other (passed) OGIP required keywords */
      if (fits_write_key(fptr, TSTRING, "TELESCOP", telescop,
			 "Mission/Satellite name", &status))
      {
	 PrintWarning(progname, version, "Error while writing TELESCOP keyword",
		      1, 1, status);
	 status = 0;
      }
      
      if (fits_write_key(fptr, TSTRING, "INSTRUME", instrume,
			 "Instrument/Detector name", &status))
      {
	 PrintWarning(progname, version, "Error while writing INSTRUME keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key(fptr, TSTRING, "DETNAM", detnam,
			 "Specific detector name in use", &status))
      {
	 PrintWarning(progname, version, "Error while writing DETNAM keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key(fptr, TSTRING, "FILTER", filter,
			 "filter in use", &status))
      {
	 PrintWarning(progname, version, "Error while writing FILTER keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key_lng(fptr, "DETCHANS", ichan,
			     "Total number of detector channels", &status))
      {
	 PrintWarning(progname, version, "Error while writing DETCHANS keyword",
		      1, 1, status);
	 status = 0;
      }

      if ((strcmp(chantype, "PHA")) && (strcmp(chantype, "PI")))
	 strcpy(comm, "WARNING This is NOT an OGIP-approved value");
      else
	 strcpy(comm, "Detector Channel Type in use (PHA or PI)");

      if (fits_write_key_str(fptr, "CHANTYPE", chantype, comm, &status))
      {
	 PrintWarning(progname, version, "Error while writing CHANTYPE keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key_flt(fptr, "LO_THRESH", lo_thresh, DECIMALS,
			     "Lower threshold for stored matrix", &status))
      {
	 PrintWarning(progname, version, 
		      "Error while writing LO_THRESH keyword", 1, 1,
		      status);
	 status = 0;
      }

      if (fits_write_key(fptr, TFLOAT, "EFFAREA", &areascal,
			 "Area scaling factor", &status))
      {
	 PrintWarning(progname, version, "Error while writing ERRAREA keyword",
		      1, 1, status);
	 status = 0;
      }

      if (fits_write_key(fptr, TSTRING, "RMFVERSN", "1992a",
			 "OGIP classification of FITS format", &status))
      {
	 PrintWarning(progname, version, "Error while writing RMFVERSN keyword",
		      1, 1, status);
	 status = 0;
      }

      strcpy(message, " ... written the OGIP required keywords");
      DispMsg(chatter, 15, message);

      /* Add the (passed) history cards, adding one related to this program */
      i=0, itemp=0;
      while (i<nk_history)
      {
	 if (fits_write_history(fptr, history[i++], &status))
	 {
	    itemp = status;
	    strcpy(comm, " - (missing record) fitsio illegal character ?");
	    status = 0;
	    fits_write_history(fptr, comm, &status);
	 }
      }

      strcpy(comm, "RSP_MATRIX extension written by ");
      strcat(strcat(strcat(comm, progname), " "), version);
      fits_write_history(fptr, comm, &status);

      if ((itemp != 0) || (status != 0))
	 PrintWarning(progname, version, 
		      "Error while writing at least one History record", 
		      1, 1, status);
	 
      status = 0;
      
      strcpy(message, " ... Written the history keywords");
      DispMsg(chatter, 15, message);

      /* Add the (passed) comment cards */
      i=0, itemp=0;
      while (i<nk_comm)
      {
	 if (fits_write_comment(fptr, comment[i++], &status))
	 {
	    itemp = status;
	    strcpy(comm, " - (missing record) fitsio illegal character ?");
	    status = 0;
	    fits_write_history(fptr, comm, &status);
	 }
      }

      if (itemp != 0)
	 PrintWarning(progname, version, 
		      "Error while writing at least one Comment record", 
		      1, 1, status);

      strcpy(message, " ... Written the comment keywords");
      DispMsg(chatter, 15, message);
      
      /* Write the data */
      for (ie=0; ie<ienerg; ie++)
      {
	 /* the energy bin */
	 if (fits_write_col(fptr, TFLOAT, 1, firstrow, firstelem, 1,
			    &energ_lo[ie], &status))
	    Printerror(status);

	 if (fits_write_col(fptr, TFLOAT, 2, firstrow, firstelem, 1,
			    &energ_hi[ie], &status))
	    Printerror(status);

	 /* the Grping info */
	 lvalue = (long)ngrp[ie];
	 if (fits_write_col(fptr, TLONG, 3, firstrow, firstelem, 1,
			    &lvalue, &status))
	    Printerror(status);
	    
	 larray = (long *) malloc(ngrp[ie]*sizeof(long));
	 for (j=0; j<ngrp[ie]; j++)
	 {
	    larray[j] = (long) f_chan[ie][j];
	 }

	 if (fits_write_col(fptr, TLONG, 4, firstrow, firstelem, ngrp[ie],
			    larray, &status))
	    Printerror(status);

	 for (j=0; j<ngrp[ie]; j++)
	    larray[j] = (long) n_chan[ie][j];

	 if (fits_write_col(fptr, TLONG, 5, firstrow, firstelem, ngrp[ie],
			    larray, &status))
	    Printerror(status);

	 free(larray);
	 
	 /* the matrix */
	 values = (float *) malloc(siz_mat*sizeof(float));
	 for (k=0; k<siz_mat; k++)
	    values[k] = 0.0;

	 k = 0;
	 for (j=0; j<ngrp[ie]; j++)
	 {
	    if (flchan == 0)
	    {
	       istart = f_chan[ie][j];
	       istop  = f_chan[ie][j] + n_chan[ie][j] - 1;
	    }

	    else
	    {
	       istart = f_chan[ie][j] - 1;
	       istop  = f_chan[ie][j] + n_chan[ie][j] - 2;
	    }

	    for (ic=istart; ic<=istop; ic++)
	    {
	       values[k++] = fmatrix[ie][ic];
	    }
		  
	 }

	 if (qvar == 1)
	 {
	    if (fits_write_col(fptr, TFLOAT, 6, firstrow, firstelem, k,
			       values, &status))
	       Printerror(status);
	 }
	 else
	 {
	    if (fits_write_col(fptr, TFLOAT, 6, firstrow, firstelem, siz_mat,
			       values, &status))
	       Printerror(status);
	 }
	    
	 free(values);
	 firstrow++;
      }
   }

   if (status != 0)
   {
      strcpy(message, errstr);
      DispMsg(1, 1, message);
      strcpy(message, "FATAL - aborting");
      DispMsg(1, 1, message);
   }
   
   else
   {
      strcpy(message, " ... Successfully written the RSP_MATRIX data");
      DispMsg(chatter, 15, message);
   }

   return(status);
}
