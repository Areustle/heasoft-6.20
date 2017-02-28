/**************************************************************************************
 *                                ARRTIM.C
 *
 *  Program: BARYTIME v2.2
 *  Date: March 4, 1994
 *
 *  Programmer: Joseph M. Fierro, Stanford University, EGRET project
 *
 *  Content: Procedure to read photon events, compute barycentric arrival
 *           times.
 *
 *  Main procedure: ARRTIM()
 *
 *  Called by: RUN_PROC()
 *
 *  External calls:
 *   DELAY(): Computes propagation delay to Solar System barycenter
 *
 *  Method:
 *   Initialize basic variables
 *   While (reading next record does not hit end of file)
 *     If ((event meets selection criteria) and (pulsar not blocked by sun))
 *       Save updated record to new file
 *
 *************************************************************************************/
#include "smdb.h"


#define  BETWEEN(A,B,C)	(((A) >= (B)) && ((A) <= (C)))

extern FILE *newfile, *pfile, *timeline_file;
extern double ra, dec, consiz, angmin, angmax, emin, emax, zenmax, raddeg;

double dir[3], frc;
double cosmin, cosmax, zencut, cosdec, sindec;

int jd;


/**************************************  ARRTIM  **************************************
 *  Initialize variables, read in gamma-ray events that fall within 
 *  user-defined error limits, calculate SSBC arrival time. 
 *************************************************************************************/
int Arrtim(char *misc_dir, char *input_file, fitsfile *ofptr)
{
   double         *dnulval=NULL;

   float          *fnulval=NULL;

   long           nrows=0L, goodRows=0L;
   long           *lnulval=NULL;

   int            inrows=0;
   int            status=0;
   int            i=0;
   int            ic = 1;
   int            *anynul=NULL;

   short          *snulval=NULL;

   unsigned char  *ucnulval=NULL;
   char           qBad=0;
   char           *msg = (char *) malloc(80*sizeof(char));

   fitsfile       *fptr=NULL;

   Record         record;


   InitVar();

  
   status = fits_open_file(&fptr, input_file, READONLY, &status);
   if (status != 0)
      Fcerr(strcat(strcpy(msg, "Error while opening"), input_file));

   if (status == 0)
   {
      status = fits_movnam_hdu(fptr, BINARY_TBL, "EGRET_SMDB", 0, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while moving to EGRET_SMDB extension"));
   }

   if (status == 0)
   {
      status = fits_get_num_rows(fptr, &nrows, &status);
      if (status != 0)
         Fcerr(strcpy(msg, 
	       "Error while moving to retreiving number of rows in EGRET_SMDG "\
   	       "extension"));
   }

   if (status == 0)
   {
      goodRows = nrows;

      inrows = (int) nrows;

      record.good   = (char *) calloc(inrows, sizeof(char));
      record.time    = (double *) malloc(inrows*sizeof(double));
      record.mills   = (long *) malloc(inrows*sizeof(long));
      record.mics    = (short *) malloc(inrows*sizeof(short));
      record.jdtrun  = (short *) malloc(inrows*sizeof(short));
      record.scposn  = (float **) malloc(3*sizeof(float *));
      for (i=0; i<3; ++i)
         record.scposn[i] = (float *) malloc(inrows*sizeof(float));
      record.tags  = (long *) malloc(inrows*sizeof(long));
      record.coincidence  = (short *) malloc(inrows*sizeof(short));
      record.flags  = (unsigned char *) malloc(2*inrows*sizeof(char));
      record.tasc  = (unsigned char **) malloc(2*sizeof(unsigned char *));
      for (i=0; i<2; ++i)
      {
         record.tasc[i]  = (unsigned char *) malloc(inrows*sizeof(unsigned char));
      }
      record.XZ_proj  = (float *) malloc(inrows*sizeof(float));
      record.YZ_proj  = (float *) malloc(inrows*sizeof(float));
      record.zenith   = (float *) malloc(inrows*sizeof(float));
      record.azimuth  = (float *) malloc(inrows*sizeof(float));
      record.grra  = (float *) malloc(inrows*sizeof(float));
      record.grdec  = (float *) malloc(inrows*sizeof(float));
      record.grlat  = (float *) malloc(inrows*sizeof(float));
      record.grlong  = (float *) malloc(inrows*sizeof(float));
      record.energy  = (float *) malloc(inrows*sizeof(float));
      record.delta_e  = (float *) malloc(inrows*sizeof(float));
      record.frdet  = (double *) malloc(inrows*sizeof(double));
      record.binphase  = (float *) malloc(inrows*sizeof(float));
      record.phase  = (float *) malloc(inrows*sizeof(float));
      record.pra  = (float *) malloc(inrows*sizeof(float));
      record.pdec  = (float *) malloc(inrows*sizeof(float));
      record.bcvec = (long **) malloc(3*sizeof(long *));
      for (i=0; i<3; ++i)
         record.bcvec[i] = (long *) malloc(inrows*sizeof(long));

      status = fits_read_col(fptr, TLONG, ic, 1, 1, nrows, lnulval, record.mills, 
			     anynul, &status);

      if (status == 0)
      {
         ic = 2;
         status = fits_read_col(fptr, TSHORT, ic, 1, 1, nrows, snulval, record.mics, 
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 3;
         status = fits_read_col(fptr, TSHORT, ic, 1, 1, nrows, snulval, record.jdtrun,
				anynul, &status);
      }

      if (status == 0)
      {
         ic = 4;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, 
				record.scposn[0], anynul, &status);
      }

      if (status == 0)
      {
         ic = 5;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, 
				record.scposn[1], anynul, &status);
      }

      if (status == 0)
      {
         ic = 6;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, 
				record.scposn[2], anynul, &status);
      }

      if (status == 0)
      {
         ic = 7;
         status = fits_read_col(fptr, TLONG, ic, 1, 1, nrows, lnulval, record.tags, 
				anynul, &status);
      }

      if (status == 0)
      {
         ic = 8;
         status = fits_read_col(fptr, TSHORT, ic, 1, 1, nrows, snulval, 
				record.coincidence, anynul, &status);
      }

      if (status == 0)
      {
         ic = 9;
         status = fits_read_col(fptr, TBYTE, ic, 1, 1, 2*nrows, ucnulval, record.flags,
				anynul, &status);
      }

      if (status == 0)
      {
         ic = 10;
         status = fits_read_col(fptr, TBYTE, ic, 1, 1, nrows, ucnulval, record.tasc[0],
				anynul, &status);
      }

      if (status == 0)
      {
         ic = 11;
         status = fits_read_col(fptr, TBYTE, ic, 1, 1, nrows, ucnulval, record.tasc[1],
				anynul, &status);
      }

      if (status == 0)
      {
         ic = 13;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.XZ_proj,
	   		        anynul, &status);
      }

      if (status == 0)
      {
         ic = 14;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.YZ_proj,
	   		        anynul, &status);
      }

      if (status == 0)
      {
         ic = 15;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.zenith, 
	  		        anynul, &status);
      }

      if (status == 0)
      {
         ic = 16;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.azimuth,
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 17;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.grra, 
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 18;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.grdec, 
	   		        anynul, &status);
      }

      if (status == 0)
      {
         ic = 19;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.grlat, 
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 20;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.grlong, 
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 21;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.energy, 
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 22;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.delta_e,
	   		        anynul, &status);
      }

      if (status == 0)
      {
         ic = 25;
         status = fits_read_col(fptr, TDOUBLE, ic, 1, 1, nrows, dnulval, record.frdet, 
		   	        anynul, &status);
      }

      if (status == 0)
      {
         ic = 26;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, 
				record.binphase, anynul, &status);
      }

      if (status == 0)
      {
         ic = 27;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.phase, 
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 28;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.pra, 
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 29;
         status = fits_read_col(fptr, TFLOAT, ic, 1, 1, nrows, fnulval, record.pdec, 
			       anynul, &status);
      }

      if (status == 0)
      {
         ic = 30;
         status = fits_read_col(fptr, TLONG, ic, 1, 1, nrows, lnulval, record.bcvec[0],
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 31;
         status = fits_read_col(fptr, TLONG, ic, 1, 1, nrows, lnulval, record.bcvec[1],
			        anynul, &status);
      }

      if (status == 0)
      {
         ic = 32;
         status = fits_read_col(fptr, TLONG, ic, 1, 1, nrows, lnulval, record.bcvec[2],
			        anynul, &status);
      }

      if (status == 0)
      {
         for (i=0; i<inrows && status==0; ++i)
         {
            if (ReadInfile(&record, i)) 
            {
               if (status = Delay(misc_dir, jd, frc, &record, i))
               {
                  if (status == 1)
                  {
		     status = 0;
                     SaveRecord(&record, i);
		  }
               }
  
               else
               {
                  if (pfile != NULL)
                     fprintf(pfile, "Source occulted by Sun.\n");
               }
            }
 
            qBad = 1;
         }

	 if (status == 0)
  	 {
            goodRows = ExtractGoodRecords(nrows, &record);
 
            if (qBad)
            {
               status = fits_update_key(ofptr, TLONG, "NAXIS2", &goodRows, NULL, 
					&status);
               if (status != 0)
                  Fcerr(strcpy(msg, 
	                "Error while updating NAXIS2 key in EGRET_SMDB extension"));
            }

            if (status == 0)
               status = WriteRecords(ofptr, goodRows, &record);
         }
      }

      else
      {
          sprintf(msg, "Error while reading column %d", ic);
          Fcerr(msg);
      }
   }

   return status;
}


/*************************************  INIT_VAR  *************************************
 *  Set initial values for not explicitly defined variables.
 *************************************************************************************/
void InitVar()
{ 
   ra = ra*raddeg;
   dec = dec*raddeg;
   cosdec = cos(dec);
   sindec = sin(dec);
   dir[0] = cos(ra)*cosdec;
   dir[1] = sin(ra)*cosdec;
   dir[2] = sindec;

   angmin = angmin*raddeg;
   angmax = angmax*raddeg;
   cosmax = (consiz == -1) ? cos(angmin) : 1;
   cosmin = cos(angmax);
   if (zenmax > 0)
      zencut = zenmax*raddeg;

   rewind(timeline_file);
}

/***********************************  READINFILE  *************************************
 *  Reject events recorded during excluded time.
 *  Check if event meets user-defined selection criteria.
 *************************************************************************************/
int ReadInfile(Record *record, int irec)
{
   static double  t1, t2, last_t = 99999;
   double         t, cosang, sigma;


   frc = (record->mills[irec] + record->mics[irec]/1000.)/(SECDAY*1000) - 0.5;
   t = record->jdtrun[irec] + frc + .5;
   if (t < last_t) 
   {
      rewind(timeline_file);
      t1 = t2 = 0;
   }

   last_t = t;
   while (t2 < t) 
      GetExcludeRange(&t1, &t2);

   if (t > t1) 
      return (0); 

   jd = record->jdtrun[irec] + 40001;

   cosang = cos(ra - record->grra[irec])*cosdec*cos(record->grdec[irec]) +
 	    sindec*sin(record->grdec[irec]);
   sigma = PSF_68(record->energy[irec]);

   if (consiz != -1) 
   {
      double angle1, angle2;
      angle1 = (consiz*sigma < angmax) ? consiz*sigma : angmax;
      angle2 = (angle1 > angmin) ? angle1 : angmin;
      cosmin = cos(angle2);
   }

   if (zenmax < 0)
      zencut = 1.92 + zenmax*sigma;

   return (BETWEEN(cosang, cosmin, cosmax) && 
		BETWEEN(record->energy[irec], emin, emax) &&
   				(record->zenith[irec] < zencut));

   return (1); 
}


/***********************************  SAVERECORD  *************************************
 *  Calculate print results and save to file.
 *************************************************************************************/
void SaveRecord(Record *record, int irec)
{
   if (pfile != NULL)
      fprintf(pfile, "%16.8f %9.3f %7.3f %6.0f    (%5.1f,% 5.1f,% 5.1f)\n",
     	      2400000+jd+record->frdet[irec], record->grra[irec]/raddeg, 
	      record->grdec[irec]/raddeg,
      	      record->energy[irec], record->bcvec[0][irec]*1.e-6, 
	      record->bcvec[1][irec]*1.e-6,
     	      record->bcvec[2][irec]*1.e-6);

   record->binphase[irec] = record->phase[irec] = 0;
   record->pra[irec] = ra;
   record->pdec[irec] = dec;
   record->good[irec] = 1;
}


/********************************  EXTRACTGOODRECORDS  ********************************
 *  Calculate print results and save to file.
 *************************************************************************************/
long ExtractGoodRecords(long nrows, Record *record)
{
   long  irec=0, j=0;
   long  goodRows=0;

   int   ind=0, gind=0;

   
   for (irec=0; irec<nrows; ++irec)
   {
      if (record->good[irec] == 1)
      {
	 record->time[goodRows] = 2440001.0 + record->jdtrun[irec] +
					      record->frdet[irec];

 	 if (irec != goodRows)
         {
	    record->mills[goodRows] = record->mills[irec];
 	    record->mics[goodRows] = record->mics[irec];
	    record->jdtrun[goodRows] = record->jdtrun[irec];
	    for (j=0; j<3; ++j)
            {
	       record->scposn[j][goodRows] = record->scposn[j][irec];
	       record->bcvec[j][goodRows]  = record->bcvec[j][irec];
            }
	    record->tags[goodRows] = record->tags[irec];
	    record->coincidence[goodRows] = record->coincidence[irec];
	    ind  = irec*2;
	    gind = goodRows*2;
	    record->flags[gind] = record->flags[ind];
	    record->flags[gind+1] = record->flags[ind+1];
            for (j=0; j<2; ++j)
	       record->tasc[j][goodRows] = record->tasc[j][irec];
	    record->XZ_proj[goodRows] = record->XZ_proj[irec];
	    record->YZ_proj[goodRows] = record->YZ_proj[irec];
	    record->zenith[goodRows] = record->zenith[irec];
	    record->azimuth[goodRows] = record->azimuth[irec];
	    record->grra[goodRows] = record->grra[irec];
	    record->grdec[goodRows] = record->grdec[irec];
	    record->grlat[goodRows] = record->grlat[irec];
	    record->grlong[goodRows] = record->grlong[irec];
	    record->energy[goodRows] = record->energy[irec];
	    record->delta_e[goodRows] = record->delta_e[irec];
	    record->frdet[goodRows] = record->frdet[irec];
	    record->binphase[goodRows] = record->binphase[irec];
	    record->phase[goodRows] = record->phase[irec];
	    record->pra[goodRows] = record->pra[irec];
	    record->pdec[goodRows] = record->pdec[irec];
	 }

         ++goodRows;
      }
   }

   return goodRows;
}


/***********************************  WRITERECORDS  ***********************************
 *  Write records to the output file..
 *************************************************************************************/
int WriteRecords(fitsfile *ofptr, long nrows, Record *record)
{
   int   status=0;
   int   hdutype=0;
   int   ic=1;

   char  *msg=(char *) malloc(80*sizeof(char));




   strcpy(msg, "Writing data to output file...");
   Fcerr(msg);

   status = fits_write_col(ofptr, TDOUBLE, ic, 1, 1, nrows, record->time, &status);
   if (status != 0)
      Fcerr(strcpy(msg, "Error while writing to EGRET_SMDB:column 1"));

   if (status == 0)
   {
      ic = 2;
      status = fits_write_col(ofptr, TLONG, 2, 1, 1, nrows, record->mills, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while writing to EGRET_SMDB:column 2"));
   }

   if (status == 0)
   {
      ic = 3;
      status = fits_write_col(ofptr, TSHORT, ic, 1, 1, nrows, record->mics, &status);
   }

   if (status == 0)
   {
      ic = 4;
      status = fits_write_col(ofptr, TSHORT, ic, 1, 1, nrows, record->jdtrun, &status);
   }

   if (status == 0)
   {
      ic = 5;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->scposn[0], 
			      &status);
   }

   if (status == 0)
   {
      ic = 6;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->scposn[1], 
			      &status);
   }

   if (status == 0)
   {
      ic = 7;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->scposn[2],
			      &status);
   }

   if (status == 0)
   {
      ic = 8;
      status = fits_write_col(ofptr, TLONG, ic, 1, 1, nrows, record->tags, &status);
   }

   if (status == 0)
   {
      ic = 9;
      status = fits_write_col(ofptr, TSHORT, ic, 1, 1, nrows, record->coincidence, 
	 		      &status);
   }

   if (status == 0)
   {
      ic = 10;
      status = fits_write_col(ofptr, TBYTE, ic, 1, 1, 2*nrows, record->flags, &status);
   }

   if (status == 0)
   {
      ic = 11;
      status = fits_write_col(ofptr, TBYTE, ic, 1, 1, nrows, record->tasc[0], &status);
   }

   if (status == 0)
   {
      ic = 12;
      status = fits_write_col(ofptr, TBYTE, ic, 1, 1, nrows, record->tasc[1], &status);
   }

   if (status == 0)
   {
      ic = 14;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->XZ_proj, 
			      &status);
   }

   if (status == 0)
   {
      ic = 15;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->YZ_proj, 
			      &status);
   }

   if (status == 0)
   {
      ic = 16;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->zenith, &status);
   }

   if (status == 0)
   {
      ic = 17;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->azimuth, 
 			      &status);
   }

   if (status == 0)
   {
      ic = 18;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->grra, &status);
   }

   if (status == 0)
   {
      ic = 19;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->grdec, &status);
   }

   if (status == 0)
   {
      ic = 20;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->grlat, &status);
   }

   if (status == 0)
   {
      ic = 21;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->grlong, &status);
   }

   if (status == 0)
   {
      ic = 22;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->energy, &status);
   }

   if (status == 0)
   {
      ic = 23;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->delta_e, 
			      &status);
   }

   if (status == 0)
   {
      ic = 26;
      status = fits_write_col(ofptr, TDOUBLE, ic, 1, 1, nrows, record->frdet, &status);
   }

   if (status == 0)
   {
      ic = 27;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->binphase, 
			      &status);
   }

   if (status == 0)
   {
      ic = 28;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->phase, &status);
   }

   if (status == 0)
   {
      ic = 29;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->pra, &status);
   }

   if (status == 0)
   {
      ic = 30;
      status = fits_write_col(ofptr, TFLOAT, ic, 1, 1, nrows, record->pdec, &status);
   }

   if (status == 0)
   {
      ic = 31;
      status = fits_write_col(ofptr, TLONG, ic, 1, 1, nrows, record->bcvec[0], 
			      &status);
   }

   if (status == 0)
   {
      ic = 32;
      status = fits_write_col(ofptr, TLONG, ic, 1, 1, nrows, record->bcvec[1], 
			      &status);
   }

   if (status == 0)
   {
      ic = 33;
      status = fits_write_col(ofptr, TLONG, ic, 1, 1, nrows, record->bcvec[2], 
			      &status);
   }

   /* Update TSTART  and  TSTOP keys */
   if (status == 0)
   {
      status = fits_update_key(ofptr, TDOUBLE, "TSTART", &record->time[0], 
	                          "Observation Start Time", &status);
      if (status != 0)
         Fcerr(strcpy(msg, 
		      "Error while updating TSTART keyword in EGRET_SMDB extension"));
   }

   else
   {
      sprintf(msg, "Error while writing to EGRET_SMDF:column %d", ic);
      Fcerr(msg);
   }

   if (status == 0)
   {
      status = fits_update_key(ofptr, TDOUBLE, "TSTOP", &record->time[nrows-1], 
	                       "Observation End Time", &status);
      if (status != 0)
         Fcerr(strcpy(msg, 
	  	      "Error while updating TSTOP keyword in EGRET_SMDB extension"));
   }

   return status;
}
