/* readinput.c */

#include "intmap.h"


void readinput(char *data_dir, char *cmapfile, char *exphst, 
	       int *expflag, float *specin, float *sfact, char *misc_dir, 
	       char *calib_dir, char *output_dir, char *cal_bin_dir,
	       int *status)
{
   int    BufLen_2=81;

   char   mapfile[80];
   char   msg[80];
   char   field[30];




   strcpy(field, "miscdir");
   Uclgst(field, misc_dir, status);
   if (*status != 0)
      strcpy(msg, "Error while reading miscdir");

   if (*status == 0)
   {
      strcpy(field, "datadir");
      Uclgst(field, data_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading datadir");
   }

   if (*status == 0)
   {
      strcpy(field, "outputdir");
      Uclgst(field, output_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading outputdir");
   }

   if (*status == 0)
   {
      strcpy(field, "tempfildir");
      Uclgst(field, cal_bin_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading tempfildir");
   }

   if (*status == 0)
   {
      strcpy(field, "calibdir");
      Uclgst(field, calib_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading calibdir");
   }

   if (*status == 0)
   {
      strcpy(field, "cmapfile");
      Uclgst(field, cmapfile, status);
      if (*status != 0)
         strcpy(msg, "Error while reading cmapfile.");

      else
	 strcpy(strchr(cmapfile, '\0'), ".fits");
      /* strcpy(index(cmapfile, '\0'), ".fits"); */
   }

   if (*status == 0)
   {
      strcpy(field, "expflag");
      Uclgsi(field, expflag, status);
      if (*status != 0)
         strcpy(msg, "Error while reading expflag.");
   }

   if (*status == 0)
   {
      strcat(strcpy(mapfile, data_dir), cmapfile);
      *status = namelist(data_dir, mapfile, exphst, specin, sfact);
   }

   else
      Fcerr(msg);
}

FCALLSCSUB11(readinput,READINPUT,read_input,PSTRING,PSTRING,PSTRING,PINT,PFLOAT,PFLOAT,PSTRING,PSTRING,PSTRING,PSTRING,PINT)
