/**************************************************************************************
 *                              BARYTIME.C
 *
 *  Program: BARYTIME v2.2
 *  Date: October 10, 1994
 *
 *  Programmer: Joseph M. Fierro, Stanford University, EGRET project
 *
 *  Content: Main program and routines which use the XView Toolkit to create
 *           program windows and subwindows.
 *
 *  Main procedure: MAIN()
 *
 *  Called by:
 *
 *  External calls:
 *   GETPAR(): Gets run parameters
 *   ARRTIM(): Reads file, computes SSBC arrival time
 *
 *  Method:
 *   Create main window
 *   Create panel items for data to be input by user
 *   Start window and react to user input
 *
 *  Modification History:
 *  Sandhia Bansal  01/16/02      Replaced xview interface with input parameter 
 *                                files.  Modified the program to read FITS input
 *                                files instead of binaries.
 *************************************************************************************/
#include "smdb.h"

/* J2000 coordinates of galactic north pole and x-axis */ 
#define N_GAL_POLE_RA	 (12 + (51 + (26.2754/60))/60)*15
#define N_GAL_POLE_DEC	 (27 + (07 + (41.705/60))/60)
#define GAL_CENTER_DEC	-(28 + (56 + (10.219/60))/60)

FILE *pfile, *timeline_file;


double ra, dec, consiz, angmin, angmax, emin, emax, zenmax;
double raddeg, spdec, cpdec, glong_off, ra_off;

char printfile[80];


/**************************************   MAIN   **************************************
 *  Setup BARYTIME program main window and subwindows using Xview procedures.
 *************************************************************************************/
void Barytime()
{
   fitsfile *ofptr=NULL;

   int   file_type;
   int   status=0;
   int   clobber=0;

   char  *misc_dir    = (char *) malloc(80*sizeof(char));
   char  *input_file  = (char *) malloc(80*sizeof(char));
   char  *output_dir  = (char *) malloc(80*sizeof(char));
   char  *output_file = (char *) malloc(80*sizeof(char));
   char  *printfile   = (char *) malloc(80*sizeof(char));
   char  *angsel      = (char *) malloc(80*sizeof(char));
   char  *zensel      = (char *) malloc(80*sizeof(char));




   pfile = NULL;

   status = OpenInfile(misc_dir, output_dir, input_file, output_file, printfile, 
		       angsel, zensel, &file_type, &clobber);

   if (status == 0)
   {
      InitGlobalVariables(misc_dir);

      FileExists(printfile, &clobber, &status);
      if (status == 0)
      {
         status = OpenNewfile(output_dir, output_file, &ofptr, clobber); 
         if (status == 0)
         {
            status = GetParameters(input_file, printfile, ofptr, angsel, zensel, 
		  		   file_type);
 
   	    if (status == 0)
               Arrtim(misc_dir, input_file, ofptr);
         }

         if (ofptr)
            status = fits_close_file(ofptr, &status);
    
         if (pfile !=NULL) 
            fclose(pfile);
      }
   }

   return;
}



/******************************  INIT_GLOBAL_VARIABLES  *******************************
 *  Initialize values for global variables.
 *************************************************************************************/
void InitGlobalVariables(char *misc_dir)
{ 
   char  filename[128];


   strcat(strcpy(filename, misc_dir), "timeline");

   timeline_file = fopen(filename, "r");
   if (timeline_file == NULL) {
      fprintf(stderr, "Cannot open file %s \n", filename);

     exit(1);
   }

   raddeg = acos(0.0)/90.0;

   spdec = sin((90 - N_GAL_POLE_DEC)*raddeg);
   cpdec = cos((90 - N_GAL_POLE_DEC)*raddeg);
   glong_off = -asin(sin(GAL_CENTER_DEC*raddeg)/spdec)/raddeg;
   ra_off = 90 + N_GAL_POLE_RA;
}



/***********************************  OPEN_INFILE  ************************************
 *  If four numbers were entered, check SMDB_DIR for corresponding SMDB file.
 *  If file is in local directory, make sure it is a FITS file of type
 *  EGRET_SMDB or EGRET_PULSAR.
 *  If file doesn't exist in local or SMDB_DIR, or doesn't match expected
 *  format, return error.
 *************************************************************************************/
int OpenInfile(char *misc_dir, char *output_dir, char *filename, char *output_file, 
	       char *printfile, char *angsel, char *zensel, int *file_type, 
	       int *clobber)
{
   int  BufLen_2 = 81;
   int  status=0;

   char *data_dir=(char *) malloc(80*sizeof(char));
   char *msg=(char *) malloc(80*sizeof(char));
   char *field=(char *) malloc(30*sizeof(char));

 

   strcpy(field, "miscdir");
   Uclgst(field, misc_dir, &status);
   if (status != 0)
      Fcerr(strcpy(msg, "Error while reading misc_dir"));

   if (status == 0)
   {
      strcpy(field, "outputdir");
      Uclgst(field, output_dir, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading outputdir"));
   }

   if (status == 0)
   {
      strcpy(field, "datadir");
      Uclgst(field, data_dir, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading datadir"));
   }

   if (status == 0)
   {
      strcpy(field, "inputfile");
      Uclgst(field, filename, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading inputfile"));
   }

   if (status == 0)
   {
      strcpy(filename, strcat(strcat(strtok(data_dir, " "), filename), ".fits"));
      /* strcpy(index(filename, '\0'), ".fits"); */
                                         /* append .fits to input filename */
      strcpy(field, "outputfile");
      Uclgst(field, output_file, &status);
      if (status != 0)
         strcpy(msg, "Error while reading outputfile");
   }


   if (status == 0)
   {
      *file_type = SMDB_FITS;

      strcpy(field, "printfile");
      Uclgst(field, printfile, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading printfile"));
   }

   if (status == 0)
   {
      strcpy(field, "clobber");
      Uclgsi(field, clobber, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading clobber flag"));
   }

   if (status == 0)
   {
      strcpy(field, "angsel");
      Uclgst(field, angsel, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading angsel"));
   }

   if (status == 0)
   {
      strcpy(field, "zensel");
      Uclgst(field, zensel, &status);
      if (status != 0)
         Fcerr(strcpy(msg, "Error while reading zensel"));
   }

   return status;
}


/***********************************  OPEN_NEWFILE  ***********************************
 *  Check that output file does not already exist and can be created.
 *************************************************************************************/
int OpenNewfile(char *output_dir, char *filename, fitsfile **ofptr, int clobber)
{
   int  status=0;

   char msg[128];



   filename = strcat(strcat(strtok(output_dir, " "), filename), ".fits");

   FileExists(filename, &clobber, &status);

   if (status == 0)
   {
      status = fits_create_file(ofptr, filename, &status);

      if (status != 0)
         Fcerr(strcat(strcpy(msg, "Error while creating "), filename));
   }

   return status;
}
