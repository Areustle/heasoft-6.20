/**************************************************************************************
 *                                     GETPAR.C
 *
 *  Program: BARYTIME v2.2
 *  Date: March 4, 1994
 *
 *  Programmer: Joseph M. Fierro, Stanford University, EGRET project
 *
 *  Content: Procedure to load run parameters.
 *
 *  Main procedure: GET_PARAMETERS(FILE_TYPE)
 *   (INT) FILE_TYPE: Specifies type of input file
 *
 *  Called by: RUN_PROC()
 *
 *  External calls: 
 *   WRITE_HEADER(): Writes standard FITS header for SMDB format files
 *
 *  Method:
 *   Set default values
 *   If (input file is not a summary database file)
 *     Read FITS header
 *   Else
 *     Find corresponding viewing period in timeline file
 *   If (observation time has not been calculated)
 *     Calculate observation time from timeline file
 *   Create output file FITS header
 *   Print header information for ASCII output file
 *   If (making an energy-dependent angular cut)
 *     Set minimum angle to minimum cone size
 *
 *************************************************************************************/
#include "smdb.h"

#define JD0		24981			/* Julian Day offset */

enum parameter_types {LOWER_LIMIT, UPPER_LIMIT};


extern FILE *newfile, *pfile, *timeline_file;
extern double ra, dec, consiz, angmin, angmax, emin, emax, zenmax;
extern double raddeg, spdec, cpdec, glong_off, ra_off;

Date start, stop;
double obs_time, vp_ra, vp_dec, x_ra, x_dec, mincon, zenoff;
double epoch, radio, f0, f1, f2;
double pb, a1, e, t0, omz, omdot, gamm, pbdot;
int binary;
int variable_acceptance, variable_zenith;
static int days_in_month[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
char vp[32], desc[32], pname[1];
char coord[80], rastring[80], decstring[80];



/**********************************  GET_PARAMETERS  **********************************
 *  Set default values.  Get run parameters from FITS header and input items.
 *  Write new FITS header.
 *************************************************************************************/
int GetParameters(char *input_file, char *printfile, fitsfile *ofptr, char *angsel, 
		  char *zensel, int file_type)
{
   fitsfile  *fptr=NULL;

   int       status=0;



   
   status = SetDefaults(input_file, angsel, zensel);

   if (status == 0)
   {
      status = ScanHeader(input_file, &fptr);

      if (status == 0)
      {
         if (!obs_time)
            CalcObstime();

         status = WriteHeader(fptr, ofptr, SMDB_FITS);

         if (status == 0)
         {
            status = PrintHeader(input_file, printfile);

    	    if (status == 0)
 	    {
               if (variable_acceptance)
                  angmin = mincon;
               else
                  consiz = -1;

               if (variable_zenith)
                  zenmax = -zenoff;
 	    }
	 }
      }
   }

   return status;
}


/***********************************  SET_DEFAULTS  ***********************************
 *  Set default values for input items.
 *************************************************************************************/
int SetDefaults(char *input_file, char *angsel, char *zensel)
{
   int   status=0;

   char  ch;
   char  *msg=(char *) malloc(80*sizeof(char));



   emin = 50; emax = 99999;
   consiz = -1; mincon = angmin = 0; angmax = 30;
   zenoff = -1; zenmax = 90;
   obs_time = vp_ra = vp_dec = x_ra = x_dec = 0;
   sprintf(vp, "");
   sprintf(desc, "");

   ch = toupper(angsel[0]);
   if (ch == 'F')
      variable_acceptance = 0;
   else if (ch == 'E')
      variable_acceptance = 1;
   else
   {
      status = 1;
      Fcerr(strcpy(msg, "Invalid value of angsel"));
   }

   if (status == 0)
   {
      ch = toupper(zensel[0]);
      if (ch == 'F')
         variable_zenith = 0;
      else if (ch == 'E')
         variable_zenith = 1;
      else
      {
	 status = 1;
         Fcerr(strcpy(msg, "Invalid value of zensel"));
      }
   }

   return status;
}


/***********************************  SCAN_HEADER  ************************************
 *  Read file parameters from FITS header.
 *  Make sure input limits are within file limits.
 *************************************************************************************/
int ScanHeader(char *input_file, fitsfile **fptr)
{
   double    fconsiz=0, fmincon=0, fangmin=0, fangmax=0, femin=0, femax=0;
   double    fzenoff=0, fzenmax=0; 
 
   int       status=0;

   char      value[20];
   char      *msg=(char *) malloc(80*sizeof(char));



   status = fits_open_file(fptr, input_file, READONLY, &status);
   if (status != 0)
      Fcerr(strcat(strcpy(msg, "Error while opening "), input_file));

   if (status == 0)
   {
      status = fits_read_key(*fptr, TDOUBLE, "OBS-TIME", &obs_time, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TSTRING, "DATE-OBS", value, NULL, &status);
      sscanf(value, "%d-%d-%d", &start.year, &start.month, &start.day);
      status = 0;

      status = fits_read_key(*fptr, TSTRING, "TIME-OBS", value, NULL, &status);
      sscanf(value, "%d:%d:%lf", &start.hour, &start.minute, &start.seconds);
      status = 0;

      status = fits_read_key(*fptr, TSTRING, "DATE-END", value, NULL, &status);
      sscanf(value, "%d-%d-%d", &stop.year, &stop.month, &stop.day);
      status = 0;

      status = fits_read_key(*fptr, TSTRING, "TIME-END", value, NULL, &status);
      sscanf(value, "%d:%d:%lf", &stop.hour, &stop.minute, &stop.seconds);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "SC-Z-RA", &vp_ra, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "SC-Z-DEC", &vp_dec, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "SC-X-RA", &x_ra, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "SC-X-DEC", &x_dec, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "TARG-RA", &ra, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "TARG-DEC", &dec, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "CONSIZ", &fconsiz, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "MINCON", &fmincon, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "MINANG", &fangmin, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "MAXANG", &fangmax, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "MINENG", &femin, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "MAXENG", &femax, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "ZENOFF", &fzenoff, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TDOUBLE, "ZENMAX", &fzenmax, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TSTRING, "OBJECT", desc, NULL, &status);
      status = 0;

      status = fits_read_key(*fptr, TSTRING, "EGTVPN", vp, NULL, &status);
      status = 0;

      status = GetInput(&consiz, &mincon, &angmin, &angmax, &emin, &emax, &zenoff, 
		  	&zenmax, coord, rastring, decstring);

      if (status == 0)
      {
         GetCoord();

         if (variable_acceptance) 
         {
            if (consiz == -1)
               consiz = 99999;
            GetLimit(fconsiz, &consiz, UPPER_LIMIT);

            if (consiz == 99999)
               consiz = 1;
            GetLimit(fmincon, &mincon, UPPER_LIMIT);
          } 

         else
         {
            GetLimit(fangmin, &angmin, LOWER_LIMIT);
         }

         GetLimit(fangmax, &angmax, UPPER_LIMIT);
 
         GetLimit(femin, &emin, LOWER_LIMIT);
 
         GetLimit(femax, &emax, UPPER_LIMIT);

         if (variable_zenith) 
         {
            GetLimit(fzenoff, &zenoff, LOWER_LIMIT);
            if (zenoff == -1)
               zenoff = 4;
         }  

         else
         {
            GetLimit(fzenmax, &zenmax, UPPER_LIMIT);
         }
      }
   }

   return status;
}

/************************************  GET_LIMIT  *************************************
 *  If input limit outside file limit, set input limit to file limit.
 *************************************************************************************/
void GetLimit(double file_value, double *value, int type)
{
   if (((type == UPPER_LIMIT) && (*value > file_value)) ||
   	((type == LOWER_LIMIT) && (*value < file_value))) 
   {
      *value = file_value;
   }
}


/*************************************  GET_INPUT  ************************************
 *  Get the input parameters from input parameter file barytime.par.
 *************************************************************************************/
int GetInput(double *consiz, double *mincon, double *angmin, double *angmax,
	     double *emin, double *emax, double *zenoff, double *zenmax,
	     char *coord, char *rastring, char *decstring)
{
   int   status=0;
   int   BufLen_2=81;

   char  *msg=(char *) malloc(80*sizeof(char));
   char  *field=(char *) malloc(30*sizeof(char));



   if (variable_acceptance)
   {
      strcpy(field, "consiz");
      Uclgsd(field, consiz, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading consiz parameter"));

      if (status == 0)
      {
         strcpy(field, "mincon");
         Uclgsd(field, mincon, &status);
         if (status != 0)
            Fcerr(strcpy(msg, "Error while reading mincon parameter"));
      }
   }

   else
   {
      strcpy(field, "angmin");
      Uclgsd(field, angmin, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading angmin parameter"));
   }

   if (status == 0)
   {
      strcpy(field, "angmax");
      Uclgsd(field, angmax, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading angmax parameter"));
   }

   if (status == 0)
   {
      strcpy(field, "emin");
      Uclgsd(field, emin, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading emin parameter"));
   }

   if (status == 0)
   {
      strcpy(field, "emax");
      Uclgsd(field, emax, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading emax parameter"));
   }

   if (status == 0)
   {
      if (variable_zenith)
      {
         strcpy(field, "zenoff");
         Uclgsd(field, zenoff, &status);
         if (status != 0)
            Fcerr(strcpy(msg, "Error while reading zenoff parameter"));
      }

      else
      {
         strcpy(field, "zenmax");
         Uclgsd(field, zenmax, &status);
         if (status != 0)
            Fcerr(strcpy(msg, "Error while reading zenmax parameter"));
      }
   }

   if (status == 0)
   {
      strcpy(field, "coord");
      Uclgst(field, coord, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading coord parameter"));
   }

   if (status == 0)
   {
      strcpy(field, "rastring");
      Uclgst(field, rastring, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading rastring parameter"));
   }

   if (status == 0)
   {
      strcpy(field, "decstring");
      Uclgst(field, decstring, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading decstring parameter"));
   }

   return status;
}


/************************************  GET_COORD  *************************************
 *  Scan input item for first three numbers entered.
 *  For longitude item, convert to decimal number in the range {0, 360}.
 *  For latitude item, convert to decimal number in the range {-180, 180}.
 *************************************************************************************/
void GetCoord()
{
   double  min, sec;
   int     n;


   min = sec = 0;
   n = sscanf(rastring, "%lf %lf %lf", &ra, &min, &sec);

   if (strlen(rastring)) 
   {
      ra = fabs(ra) + (fabs(min) + (fabs(sec)/60))/60;
      if (rastring[0] == '-')
         ra = -ra;
   }

   if (n > 1)
      ra = ra*15;

   ra = fmod(ra, 360.0);
   while (ra < 0)
      ra = ra + 360;

   min = sec = 0;
   n += sscanf(decstring, "%lf %lf %lf", &dec, &min, &sec);
   if (strlen(decstring)) 
   {
      dec = fabs(dec) + (fabs(min) + (fabs(sec)/60))/60;
      if (decstring[0] == '-')
         dec = -dec;
   }

   dec = fabs(fmod(dec+90, 360.0)) - 90;
   if (dec > 90)
      dec = 180 - dec;
	
   if ((n > -2) && toupper(coord[0]) == 'G')   
      GalCel();
}


/*************************************  GAL_CEL  **************************************
 *  Convert current coordinates from galactic to celestial (J2000).
 *************************************************************************************/
void GalCel()
{
   double  slong, clong, slat, clat;

   slong = sin((ra - glong_off)*raddeg);
   clong = cos((ra - glong_off)*raddeg);
   slat = sin(dec*raddeg);
   clat = cos(dec*raddeg);

   ra = ra_off + atan2((clat*slong*cpdec - slat*spdec), clat*clong)/raddeg;
   if (ra > 360)
      ra = ra - 360;
   dec = asin(clat*slong*spdec + slat*cpdec)/raddeg;
}


/***********************************  CALC_OBSTIME  ***********************************
 *  Calculate effective observation time by subtracting total excluded time 
 *  from net observation time.
 *************************************************************************************/
void CalcObstime()
{
   double  start_t, stop_t, t1, t2, exclude;


   rewind(timeline_file);

   t1 = t2 = exclude = 0;   
   start_t = Julian(start);
   stop_t = Julian(stop);
   while (t2 <= start_t)
      GetExcludeRange(&t1, &t2);

   if (t1 < start_t)
      t1 = start_t;

   while (t2 < stop_t) 
   {
      exclude += t2 - t1;
      GetExcludeRange(&t1, &t2);
   }

   if (t1 < stop_t)
      exclude += stop_t - t1;

   obs_time = (stop_t - start_t - exclude)*SECDAY;
}


/********************************  GET_EXCLUDE_RANGE  *********************************
 *  Get beginning and end times of next excluded period from timeline file.
 *************************************************************************************/
void GetExcludeRange(double *t1, double *t2)
{
   static Date  cur;

   int          exclude = 1;

   char         date[16], time[16], keyword[64];



   date[10] = time[14] = keyword[50] = '\0';
   do 
   {
      fscanf(timeline_file, "%*7c%10c%14c%50c", date, time, keyword);
      sscanf(date, "%d/%d/%d", &cur.month, &cur.day, &cur.year);
      sscanf(time, "%d:%d:%lf", &cur.hour, &cur.minute, &cur.seconds);
   } while (strncmp(keyword, "ALBEDO MODE", 8) &&
            strncmp(keyword, "CALIBRATION", 8) &&
            strncmp(keyword, "EXCLUDE ", 8) &&
            strncmp(keyword, "TEST MODE", 8) && !feof(timeline_file));

   *t1 = Julian(cur);
   while (exclude && !feof(timeline_file)) 
   {
      fscanf(timeline_file, "%*7c%10c%14c%50c", date, time, keyword);
      sscanf(date, "%d/%d/%d", &cur.month, &cur.day, &cur.year);
      sscanf(time, "%d:%d:%lf", &cur.hour, &cur.minute, &cur.seconds);

      if (!strncmp(keyword, "ALBEDO MODE", 8) ||
          !strncmp(keyword, "CALIBRATION", 8) ||
          !strncmp(keyword, "EXCLUDE ", 8) ||
          !strncmp(keyword, "TEST MODE", 8))
         exclude++;

      if (!strncmp(keyword, "END ", 4))
         exclude--;
   }

   *t2 = Julian(cur);
   if (feof(timeline_file))
      *t1 = *t2 = 99999;

   return;
}

/**************************************  JULIAN  **************************************
 *  Convert Gregorian date mm/dd/yy, hr:mn:sc to truncated Julian day.
 *************************************************************************************/
double Julian(Date date)
{
   double  days, seconds;

   int     i;


   days = date.day;
   days_in_month[1] = (date.year % 4) ? 28 : 29;

   for (i = 1; i < date.month; i++)
      days += days_in_month[i - 1];

   days += (int) (date.year*365.25 - .01) - JD0;
   seconds = date.seconds + (date.minute + date.hour*60)*60;

   return (days + seconds/SECDAY);
}


/***********************************  PRINT_HEADER  ***********************************
 *  Output run parameters into header of the ASCII output file.
 *************************************************************************************/
int PrintHeader(char *input_file, char *printfile)
{
   time_t  cur_time;

   int     status=0;

   char    start_time[32], end_time[32], run_time[16];
   char    msg[80];


   cur_time = time(NULL);
   strftime(run_time, 16, "%a %b %e %Y", localtime(&cur_time));
   sprintf(start_time, "%.2d/%.2d/%.2d, %.2d:%.2d:%06.3f", start.month, 
   	start.day, start.year, start.hour, start.minute, start.seconds);
   sprintf(end_time, "%.2d/%.2d/%.2d, %.2d:%.2d:%06.3f", stop.month, 
   	stop.day, stop.year, stop.hour, stop.minute, stop.seconds);

   pfile = NULL;

   if (strlen(printfile) > 0)
      pfile = fopen(printfile, "w");

   if (pfile == NULL)
   {
      status = 1;
      Fcerr(strcat(strcpy(msg, "Could not create ASCII file "), printfile));
   }

   else
   {
      fprintf(pfile, "Barytime v2.2 -- Printout         %s\n\n", run_time);

      fprintf(pfile, " Source file             %s\n", input_file);
      fprintf(pfile, " Start of observation    %s\n", start_time);
      fprintf(pfile, " End of observation      %s\n\n", end_time);

      fprintf(pfile, " Right ascension (2000)  %.10g deg\n", ra);
      fprintf(pfile, " Declination (2000)      %.10g deg\n", dec);
      if (consiz != -1)
         fprintf(pfile, " Acceptance cone size    %lf X 68%% PSF\n", consiz);
      if (mincon != 0.0)
         fprintf(pfile, " Minimum cone size       %lf deg\n", mincon);
      if (angmin != 0.0)
         fprintf(pfile, " Minimum incident angle  %lf deg\n", angmin);
      fprintf(pfile, " Maximum incident angle  %lf deg\n", angmax);

      fprintf(pfile, " Minimum energy          %lf MeV\n", emin);
      fprintf(pfile, " Maximum energy          %lf MeV\n", emax);
      if (zenoff != -1)
         fprintf(pfile, " Offset from earth limb  %lf X 68%% PSF\n", zenoff);
      fprintf(pfile, " Maximum zenith angle    %lf deg\n\n", zenmax);

      fprintf(pfile, "Arrival time         RA      DEC    Energy   ");
      fprintf(pfile, "Barycenter Vector\n");
      fprintf(pfile, "(Julian Days)       (deg)   (deg)   (MeV)    ");
      fprintf(pfile, "(light-seconds) \n");
      fprintf(pfile, "----------------   ------- -------  ------   ");
      fprintf(pfile, "----------------------\n");
   }

   return status;
}
