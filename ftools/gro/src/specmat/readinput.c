/* readinput.c */

#include "specmat.h"


void readinput(char *misc_dir, char *data_dir, char *calib_dir, char *cal_bin_dir, 
	       char *output_dir, char *exphistfilebase, char *outputfile,
	       char *selfile, char *scalefacfile, char *rmffile, char *objname, 
	       int *evclass, int *clobber, int *status)
{
   int    BufLen_2=FILESZ+1;

   char   field[30];
   char   msg[80];


 
   strcpy(field, "miscdir");
   Uclgst(field, misc_dir, status);
   if (*status != 0)
      strcpy(msg, "Error while reading miscdir.");

   if (*status == 0)
   {
      strcpy(field, "datadir");
      Uclgst(field, data_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading datadir.");
   }

   if (*status == 0)
   {
      strcpy(field, "calibdir");
      Uclgst(field, calib_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading calibdir.");
   }

   if (*status == 0)
   {
      strcpy(field, "tempfildir");
      Uclgst(field, cal_bin_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading tempfildir.");
   }

   if (*status == 0)
   {
      strcpy(field, "outputdir");
      Uclgst(field, output_dir, status);
      if (*status != 0)
         strcpy(msg, "Error while reading outputdir.");
   }

   if (*status == 0)
   {
      strcpy(field, "exphistfilebase");
      Uclgst(field, exphistfilebase, status);
      if (*status != 0)
         strcpy(msg, "Error while reading exphistfilebase.");
   }

   if (*status == 0)
   {
      strcpy(field, "outputfile");
      Uclgst(field, outputfile, status);
      if (*status != 0)
         strcpy(msg, "Error while reading outputfile.");
   }

   if (*status == 0)
   {
      strcpy(field, "likefile");
      Uclgst(field, selfile, status);
      if (*status != 0)
         strcpy(msg, "Error while reading likefile.");
   }

   if (*status == 0)
   {
      strcpy(field, "scalefacfile");
      Uclgst(field, scalefacfile, status);
      if (*status != 0)
         strcpy(msg, "Error while reading scalefacfile.");
   }

   if (*status == 0)
   {
      strcpy(field, "rmffil");
      Uclgst(field, rmffile, status);
      if (*status != 0)
         strcpy(msg, "Error while reading rmffil.");
   }

   if (*status == 0) 
   {
      strcpy(field, "objnam");
      Uclgst(field, objname, status);
      if (*status != 0)
         strcpy(msg, "Error while reading objname.");
   }

   if (*status == 0)
   {
      strcpy(field, "evclass");
      Uclgsi(field, evclass, status);
      if (*status != 0)
         strcpy(msg, "Error while reading evclass.");
   }

   if (*status == 0)
   {
      strcpy(field, "clobber");
      Uclgsi(field, clobber, status);
      if (*status != 0)
         strcpy(msg, "Error while reading clobber flag.");
   }

   if (*status != 0)
      Fcerr(msg);
}


FCALLSCSUB14(readinput, READINPUT, read_input, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PSTRING, PINT, PINT, PINT)
