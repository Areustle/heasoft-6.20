#include "intmap.h"


/*******************************************************************************
 * Processing for each energy level
 * Read the FITS file whose name is passed, get its energy levels. Then read the
 * scale factor file and get the factors for each energy range. Display the
 * values found in the window.
 ******************************************************************************/
int listlevel(char *pFits, char *fname, int *LEVELS, float *fact, 
   	      float *specen, char **energy)
{
   FILE	*file;
   fitsfile *fptr=NULL;

   float *fnulval=NULL;

   long  nrows=0L;

   int   status=0;
   int	 found, num, viewP, i, j, level, dirsz;
   int   *anynul=NULL;
   int   hdutype=0;

   short *nulval=NULL;
   short *vp=NULL;

   char	 ScalFact[80], line[512], *linep, footer[80];
   char	 sfact[10][10], ctype1[25], ctype2[25]; 
   char	 record[2880], string[256], filename[80], *keyWord, tmpeng[25], *getenv();



   /* Generate the complete name of the input counts file */
   strcpy(filename, fname);
   if ((file = fopen(filename, "r")) == NULL) 
   {
      fprintf(stderr, "Cannot open file %s\n", filename);
   }

   /* Initialize variables to read the FITS header */
   found  = 0;
   *LEVELS = 0;

   /* Loop to read the energy levels from the FITS file header */
   while (!found) 
   {
      if ((num = fread(record,1,2880,file)) != 2880) 
      {
         fprintf(stderr, "Characters read = %d\n", num);
         fprintf(stderr, "Error reading file %s\n", filename);
         exit(0);
      }

      /* Get the next FITS keyword */
      for (keyWord = record; keyWord < (record+2880); keyWord += 80) 
      {
         if (strncmp(keyWord, "END ", 4) == 0) 
 	 {
	    found = 1;
 	    break;
         }

         /* Test if the data type and energy keywords are found */
         else if (strncmp(keyWord, "CTYPE1", 6) == 0) 
            strncpy(ctype1, keyWord+11, 20);

         else if (strncmp(keyWord, "CTYPE2", 6) == 0) 
            strncpy(ctype2, keyWord+11, 20);

         else if (strncmp(keyWord, "MINENG", 6) == 0) 
	 {
            sscanf(keyWord+6, "%2d", &level);
            sprintf(energy[level-1],"%2d %0.5s - ", level, keyWord+25);
	    if (level > *LEVELS)
	       *LEVELS = level;
         }

         else if (strncmp(keyWord, "MAXENG", 6) == 0) 
	 {
            sscanf(keyWord+6, "%2d", &level);
            sprintf(energy[level-1]+11,"%0.5s       ", keyWord+25);
         }

         else if ((strncmp(keyWord, "HISTORY", 7) == 0) && 
   		  (strncmp(keyWord+8, "ENRG", 4) == 0))  
	 {
            sprintf(energy[(*LEVELS)++],"%2d %0.5s - %0.5s       ",
	    *LEVELS,keyWord+19,keyWord+25);
         }
      }
   }

   fclose(file);





/* ************************************************************************************/

   /* Read Scale Factors file */
   dirsz = 0;
   while(pFits[dirsz] != '\0')
      ++dirsz;

   /* Look for the viewing period prefix */
   sscanf(fname+dirsz+9, "%d", &viewP);

   ReadScaleFactors(pFits, &viewP, fact, &status);

   if (status == 0)
   {
      if (!found) 
      {
         printf("Cannot find viewing period %d in scale factor file.\n",viewP);
         status = 1;
      }

      else  
      {
         /* Find if the exposure file for this counts file already exist */
         if ((file = fopen(filename, "r")) == NULL) 
         {
            fprintf(stderr, "Cannot open %s\n", filename);
            status = 1;
         }
   
         else  
         {
            for (level=0; level<10; level++) 
	    {
	       specen[level] = 2.1;
	       /* fact[level] = .938;    >>>>>>> needs to be deleted <<<<<<<< */
	    }
         }
      }
   }

   fflush(NULL);
   return status;
}

