/**************************************************************************************
 *                                    WRTHDR.C
 *
 *  Program: PULSAR v3.3, BARYTIME v2.2 
 *  Date: March 2, 1994
 *
 *  Programmer: Joseph M. Fierro, Stanford University, EGRET project
 *
 *  Content: Procedure to write standard SMDB FITS header.
 *
 *  Main procedure: WRITE_HEADER(FILE_TYPE)
 *   (INT) FILE_TYPE: Specifies type of output file
 *
 *  Called by: GET_PARAMETERS()
 *
 *  External calls: NONE
 *
 *  Method:
 *   Create output file FITS header from external variables
 *
 *************************************************************************************/
#include "smdb.h"


extern FILE *newfile;
extern Date start, stop;
extern double obs_time, vp_ra, vp_dec, x_ra, x_dec;
extern double ra, dec, consiz, mincon, angmin, angmax;
extern double emin, emax, zenmax, zenoff;
extern double epoch, radio, f0, f1, f2;
extern double pb, a1, e, t0, omz, omdot, gamm, pbdot;
extern double raddeg, spdec, cpdec, glong_off, ra_off;
extern int binary;
extern char desc[], pname[];
extern char vp[];

int size, type_number;
static int days_in_month[12] = {31,28,31,30,31,30,31,31,30,31,30,31};



/***********************************  WRITE_HEADER  ***********************************
 *  Create FITS header for output file.
 *************************************************************************************/
int WriteHeader(fitsfile *fptr, fitsfile *ofptr, int file_type)
{
   double  start_yydd, start_sec, stop_yydd, stop_sec;
   double  vp_long, vp_lat, x_long, x_lat, offset;
   double  value=0.0;

   time_t  cur_time;

   int     vp_num;
   int     status=0;
   int     hdutype=0;

   char    cur_date[16], gro_vp[8], comment[70];
   char    start_date[16], start_hour[16], stop_date[16], stop_hour[16];
   char    *msg=(char *) malloc(80*sizeof(char));




   Fcerr(strcpy(msg, "Creating headers in output file..."));

   /* Copy first extension header */
   status = fits_copy_header(fptr, ofptr, &status);
   if (status != 0)
      Fcerr(strcpy(msg, 
		   "Error while coping primary header from input to output file"));

   if (status == 0)
   {
      status = fits_update_key(ofptr, TSTRING, "CREATOR", CREATOR, NULL, &status);
      status = 0;

      cur_time = time(NULL);

      GetDates(start, &start_yydd, &start_sec, start_date, start_hour);
      GetDates(stop, &stop_yydd, &stop_sec, stop_date, stop_hour);

      CelGal(vp_ra, vp_dec, &vp_long, &vp_lat);
      CelGal(x_ra, x_dec, &x_long, &x_lat);
      offset = cos((vp_ra-ra)*raddeg)*cos(vp_dec*raddeg)*cos(dec*raddeg) + 
               sin(vp_dec*raddeg)*sin(dec*raddeg);
      offset = (offset >= 1) ? 0 : acos(offset)/raddeg;


      if (start.year <= 1998) 
         strcpy(comment, "Date this FITS file was created (dd/mm/yy)");

      else 
         strcpy(comment, "Date this FITS file was created (yyyy-mm-dd)");

      strftime(cur_date, 16, "%d/%m/%y", localtime(&cur_time));
      status = fits_update_key(ofptr, TSTRING, "DATE", cur_date, comment, &status);
      status = 0;

      if (start.year <= 1998) 
         strcpy(comment, "Data start date (dd/mm/yy)");

      else 
         strcpy(comment, "Data start date (yyyy-mm-dd)");

      status = fits_update_key(ofptr, TSTRING, "DATE-OBS", start_date, comment, 
			       &status);
      status = 0;

      if (start.year <= 1998) 
         strcpy(comment, "Data end date (dd/mm/yy)");

      else 
         strcpy(comment, "Data end date (yyyy-mm-dd)");

      status = fits_update_key(ofptr, TSTRING, "DATE-END", stop_date, comment, 
			       &status);
      status = 0;

      if (consiz != -1) 
      {
         status = fits_update_key(ofptr, TSTRING, "VARANG", "T", 
	                          "Energy-dependent angular selection", &status);
         status = 0;
  
         status = fits_update_key(ofptr, TFLOAT, "CONSIZ", &consiz, 
	          "Variable acceptance cone size (X 68% PSF)", &status);
         status = 0;

         status = fits_update_key(ofptr, TFLOAT, "MINCON", &mincon, 
	                          "Minimum acceptance cone size (deg)", &status);
         status = 0;

         if (consiz*PSF_68(emin) < angmax*raddeg)
            angmax = consiz*PSF_68(emin)/raddeg;
      } 

      else
      {
         status = fits_update_key(ofptr, TSTRING, "VARANG", "F", 
	                          "Energy-dependent acceptance cone", &status);
         status = 0;
      }

      if (zenoff != -1) 
      {
         status = fits_update_key(ofptr, TSTRING, "VARZEN", "T", 
	                          "Energy-dependent zenith selection", &status);
         status = 0;

         status = fits_update_key(ofptr, TSTRING, "ZENOFF", &zenoff, 
	                          "Offset from earth limb (X 68% PSF)", &status);
         status = 0;

         if ((1.92 - zenoff*PSF_68(emax)) < zenmax*raddeg)
            zenmax = (1.92 - zenoff*PSF_68(emax))/raddeg;
      } 

      else
      {
         status = fits_update_key(ofptr, TSTRING, "VARZEN", "F", 
	                          "Energy-dependent zenith selection", &status);
         status = 0;
      }

      /* Copy input:second extension header to output file */
      status = fits_movabs_hdu(fptr, 2, &hdutype, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while moving to 2nd extension "\
			   "in input file"));

      else
      {
         status = fits_copy_header(fptr, ofptr, &status);
         if (status != 0)
            Fcerr(strcpy(msg, "Error while copying header information from "\
		              "input:2nd extension to output file"));

	 else
         {
            /* Set HDUCLAS1 to EVENT.
               The timeing software looks at this value and determines whether or not 
	       the data is binned.
               HDUCLAS1 == EVENT -->> Data is unbinned. */
            status = fits_update_key(ofptr, TSTRING, "HDUCLAS1", "EVENT", NULL, 
				     &status);
            if (status != 0)
               Fcerr(strcpy(msg, "Error while updating HDUCLAS1 keyword"));

	    else
	    {
               /* Write some new keys to the extension */
               status = TimeKeywds(ofptr, "EGRET_SMDB");

               if (status == 0)
               {
                  /* Insert a new column TIME in 1st position */
                  status = fits_insert_col(ofptr, 1, "TIME", "1D", &status);
                  if (status != 0)
                     Fcerr(strcpy(msg, "Error while inserting TIME column"));

		  else
		  {
                     status = fits_write_key(ofptr, TSTRING, "TUNIT1", "d", 
	 	         	             "Physical unit of field 1", &status);
  
                     if (status != 0)
                        Fcerr(strcpy(msg, 
	    		    "Error while writing TUNIT keyword for the TIME column"));
		  }
	       }
	    }
  	 }
      }
   }

   return status;
}


/************************************  GET_DATES  *************************************
 *  Create dates for FITS header using Date structure.
/*************************************************************************************/
void GetDates(Date date, double *yydd, double *seconds, char *ddmmyy, char *hhmmss)
{
   int i;
   int century;


   *yydd = date.day;

   days_in_month[1] = (date.year % 4) ? 28 : 29;
   for (i = 1; i < date.month; i++)
      *yydd += days_in_month[i - 1];

   /* Y2k adjustment */
   if (date.year > 65 ) 
      century=1900;
   else 
      century=2000;

   *yydd = (century+date.year) + (*yydd/1000);
   *seconds = date.seconds + (date.minute + date.hour*60)*60;

   /* new FITS date format for y2k - new format starts in 1999*/
   if (date.year <= 1998) 
      sprintf(ddmmyy, "%.2d/%.2d/%.2d", date.day, date.month, date.year);

   else
      sprintf(ddmmyy, "%.4d-%.2d-%.2d", date.year, date.month, date.day);

   sprintf(hhmmss, "%.2d:%.2d:%06.3f", date.hour, date.minute, date.seconds); 
}


/*************************************  CEL_GAL  **************************************
 *  Convert input celestial coordinates (J2000) to galactic coordinates.
 *************************************************************************************/
void CelGal(double in_ra, double in_dec, double *glong, double *glat)
{
   double  sra, cra, sdec, cdec;


   sra = sin((in_ra - ra_off)*raddeg);
   cra = cos((in_ra - ra_off)*raddeg);
   sdec = sin(in_dec*raddeg);
   cdec = cos(in_dec*raddeg);

   *glong = glong_off + atan2((cdec*sra*cpdec + sdec*spdec), cdec*cra)/raddeg;
   if (*glong < 0)
      *glong = *glong + 360;

   *glat = asin(sdec*cpdec - cdec*sra*spdec)/raddeg;
}
