/**************************************************************************************
 *                                DELAY.C
 *
 *  Program: PULSAR v3.3, BARYTIME v2.2 
 *  Date: March 2, 1994
 *
 *  Programmer: Joseph M. Fierro, Stanford University, EGRET project
 *
 *  Content: Procedure to compute propagation delay to Solar System barycenter.
 *
 *  Main procedure: DELAY(MISC_DIR, JDUTC, FRC)
 *   (CHAR *) MISC_DIR: Directory containing timeline, ephem.gro and leap.sec
 *                      files
 *   (INT) JDUTC: Modified Julian Day of pulse arrival
 *   (DOUBLE) FRC: Fraction of day from Greenwich noon when pulse arrived
 *
 *  Called by: ARRTIM()
 *
 *  External calls: NONE
 *
 *  Method:
 *   Compute index of ephemeris memory corresponding to input date
 *   If (input date is not in memory) then
 *    Load next NDAYCH days of ephemeris data into memory
 *   Calculate position of Sun, Earth, and velocity of Earth
 *   Calculate SSBC-to-GRO vector and Sun-to-GRO vector
 *   Calculate distance and angular size of sun
 *   If (object was blocked by sun)
 *     Return (no delay calculated)
 *   Add all propagation effects   
 *   Return (delay was calculated)
 *
 *************************************************************************************/
#include "smdb.h"


#define NDAYCH	16			/* Number of Ephemeris days in memory */
#define SPEED_OF_LIGHT	2.99792458e+5		/* Speed of light (km/s) */
#define MSOL	4.925490943389e-6	/* Mass of Sun in GR units (light-s) */
#define SUNRAD	2.315			/* Polar radius of Sun (light-sec) */
#define TDTUTC0	42.184			/* Initial TDT-UTC offset (1972) */

#define DOT(A, B)	(A[0]*B[0] + A[1]*B[1] + A[2]*B[2])

/* extern Record record; */
extern double dir[3];

static struct {
  double earth[4][3], solar[3], tdbtdt;  /* Earth, Sun positions, time error */
} eph[NDAYCH];

static double tdtut[NDAYCH], tdbdot[NDAYCH];
double rce[3], rcs[3], vce[3];
double etut;
static int jdch0;
int nset;


/**************************************  DELAY  ***************************************
 *  If given date is not in ephemeris memory, load new chunk of ephemeris.
 *  Get positions of Sun and Earth and speed of Earth.
 *  Make sure sun does not block position.
 *  Calculate when the photon would have arrived at Solar System Barycenter.
 *************************************************************************************/
int Delay(char *misc_dir, int jdutc, double frc, Record *record, int irec)
{
   double  rca[3], rsa[3];
   double  total, sundis, sunsiz, cth;
   double  temp[3];

   int     i;
   int     status=0;

   char  *msg=(char *) malloc(80*sizeof(char));
 

   jdutc = jdutc + 2400000;
   nset = jdutc - jdch0;

   if ((nset < 0) || (nset >= NDAYCH))
      status = LoadEphemData(misc_dir, jdutc);
   
   if (status == 0)
   {
      GetPositions(frc);

      for (i = 0; i < 3; i++) 
      {
         rca[i] = rce[i] + record->scposn[i][irec]/SPEED_OF_LIGHT;  
	                                 /* SSBC-to-GRO vector */
         record->bcvec[i][irec]= 0.5+rca[i]*1e+6;     
				/* SSBC vector in light-microsecs */
         rsa[i] = rca[i] - rcs[i];              /* Sun-to-GRO vector */
      }

      /* Calculate the time delay due to the gravitational field of the Sun */
      /* (I.I. Shapiro, Phys. Rev. Lett. 13, 789 (1964)).                   */ 
      sundis = sqrt(DOT(rsa, rsa));

      sunsiz = SUNRAD/sundis;
      cth = DOT(dir, rsa)/sundis;
 
      if ((cth + 1) < 0.5*sunsiz*sunsiz)
         return 0;

      /* Sum all propagation effects */
      for (i=0; i<3; ++i)
         temp[i] = record->scposn[i][irec];


      total = etut + DOT(dir, rca) + DOT(temp, vce)/SPEED_OF_LIGHT + 2*MSOL*log(1+cth);
      record->frdet[irec] = frc + total/SECDAY;

      return 1;
   }

   return 2;
}


/*********************************  LOAD_EPHEM_DATA  **********************************
 *  Beginning with given date, read next NDAYCH records from ephemeris file,
 *  containing Earth and Sun derivatives, and the TDB-TDT time difference.
 *  Compute TDT-UTC time difference and first derivative of TDB-TDT.
 *************************************************************************************/
int LoadEphemData(char *misc_dir, int jdutc)
{
   double           solar[3], earth[4][3];
   double           *nulval=NULL;
   double           temp[16];

   static long      nrows=0L;
   long             firstrow=0L;
   long             *lnulval=NULL;
   static long      nleaps, *jdleap=NULL;

   static int       jd0, jd1;
   int              leap, i=0, j=0, k=0, l=0;
   int              hdutype=0;
   int              status=0;
   int              *anynul=NULL;
   int              ic=1;

   char             filename[128], buf[128]; 
   char             *msg = (char *) malloc(80*sizeof(char));

   static FILE      *ephem_file;
   FILE             *utc_file;

   static fitsfile  *fptr=NULL;





   if (!jdch0) 
   {
      strcat(strcpy(filename, misc_dir), "pulsar_ephem_lib.fits");

      if (fptr == NULL)
      {
         status = fits_open_file(&fptr, filename, READONLY, &status);
	 if (status != 0)
	    strcat(strcpy(msg, "Error while opening "), filename);

	 if (status == 0)
	 {
            status = fits_movnam_hdu(fptr, BINARY_TBL, "LEAP_SECS", 0, &status);
  	    if (status != 0)
	       strcpy(msg, "Error while moving to LEAP_SECS extension");
	 }

	 if (status == 0)
  	 {
            status = fits_get_num_rows(fptr, &nleaps, &status);
	    if (status != 0)
	       strcpy(msg, 
		      "Error while retreiving number of rows in LEAP_SECS extension");
	 }

	 if (status == 0)
  	 {
	    jdleap = (long *) malloc((int)nleaps*sizeof(long));
            status = fits_read_col(fptr, TLONG, 1, 1L, 1L, nleaps, lnulval, jdleap, 
			           anynul, &status);
	 }

	 if (status == 0)
  	 {
            status = fits_movnam_hdu(fptr, BINARY_TBL, "EPHEM_GRO", 0, &status);
  	    if (status != 0)
	       strcpy(msg, "Error while moving to EPHEM_GRO extension");
	 }

	 if (status == 0)
  	 {
            status = fits_get_num_rows(fptr, &nrows, &status);
	    if (status != 0)
	       strcpy(msg, 
		      "Error while retreiving number of rows in EPHEM_GRO extension");
	 }

	 if (status == 0)
  	 {
            status = fits_read_key(fptr, TINT, "JD0", &jd0, NULL, &status);
	    if (status != 0)
	       strcpy(msg, 
		      "Error while reading JD0 keyword from EPHEM_GRO extension");
	 }

	 if (status == 0)
  	 {
            status = fits_read_key(fptr, TINT, "JD1", &jd1, NULL, &status);
	    if (status != 0)
	       strcpy(msg, 
		      "Error while reading JD1 keyword from EPHEM_GRO extension");
	 }

	 if (status == 0)
	 {
            if ((jd1 - jd0) < (NDAYCH-1)) 
            {
	       strcpy(msg, "Ephemeris file is too short\n");
	       status = 1;
	    }
         }
      }
   }

   if (status == 0)
   {
      if ((jdutc < jd0) || (jdutc > jd1))
      {
         strcpy(msg, "Event date outside range of ephemeris file");
	 status = 1;
      }
 
      else
      {
         jdch0 = (jdutc + (NDAYCH - 1) > jd1) ? jd1 - (NDAYCH - 1) : jdutc;
         for (leap = nleaps; (jdch0 <= (int)jdleap[leap-1]) && (leap > 0); leap--);

         firstrow = jdch0-jd0 + 1;

         status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			        anynul, &status);
         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
              eph[i].earth[0][0] = temp[i];

            ic = 2;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
		 	           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[0][1] = temp[i];

            ic = 3;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }
  
         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[0][2] = temp[i];

            ic = 4;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
	 		           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[1][0] = temp[i];

            ic = 5;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[1][1] = temp[i];

            ic = 6;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
		  	           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[1][2] = temp[i];

            ic = 7;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[2][0] = temp[i];

            ic = 8;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[2][1] = temp[i];

            ic = 9;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[2][2] = temp[i];

            ic = 10;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[3][0] = temp[i];

            ic = 11;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[3][1] = temp[i];

            ic = 12;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
	 		           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].earth[3][2] = temp[i];

            ic = 13;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].solar[0] = temp[i];

            ic = 14;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].solar[1] = temp[i];

            ic = 15;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].solar[2] = temp[i];
 
            ic = 16;
            status = fits_read_col(fptr, TDOUBLE, ic, firstrow, 1, NDAYCH, nulval, temp, 
			           anynul, &status);
         }

         if (status == 0)
         {
            for (i=0; i<NDAYCH; ++i)
               eph[i].tdbtdt = temp[i]; 

            for (nset = 0; nset < NDAYCH - 1; nset++) 
            {
               tdtut[nset] = TDTUTC0 + leap;
               tdbdot[nset] = eph[nset+1].tdbtdt - eph[nset].tdbtdt;
            }

            tdtut[nset] = TDTUTC0 + leap;
            tdbdot[NDAYCH - 1] = tdbdot[NDAYCH - 2];
            if ((leap < nleaps) && (jdch0+nset > (int)jdleap[leap]))
               for (nset = (int)jdleap[leap]-jdch0+1; nset < NDAYCH; nset++)
                  tdtut[nset] = tdtut[nset] + 1;

            nset = jdutc - jdch0;
         }

         else
	    sprintf(msg, "Error while reading column %d", ic);
      }
   }

   if (status != 0)
      Fcerr(msg);

   return status;
}


/**********************************  GET_POSITIONS  ***********************************
 *  Calculate position of Sun and position and velocity of the Earth with 
 *  respect to the Solar System barycenter (SSBC) and the difference between 
 *  Ephemeris Time (TDB) and Universal Time (UTC) at the same epoch.
 *************************************************************************************/
void GetPositions(double frc)
{
   double  dt, dt2, dt3;

   int     i;


   /* Use Taylor series to get the coordinates at the desired time. */
   dt = frc + tdtut[nset]/SECDAY;
   dt2 = dt*dt/2;
   dt3 = dt2*dt/3;
   for (i = 0; i < 3; i++) 
   {
      rcs[i] = eph[nset].solar[i];
      rce[i] = eph[nset].earth[0][i] + dt*eph[nset].earth[1][i] +
               dt2*eph[nset].earth[2][i] + dt3*eph[nset].earth[3][i];
      vce[i] = (eph[nset].earth[1][i] + dt*eph[nset].earth[2][i])/SECDAY;
   }

   /* Make rough computation of time derivative of TDB-TDT. */
   etut = eph[nset].tdbtdt + dt*tdbdot[nset] + tdtut[nset];
}

