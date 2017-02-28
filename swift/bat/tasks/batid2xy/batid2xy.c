#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "bat_gswdev.h"

/*
HISTORY
-------
  Version 1.0 written by Hans Krimm, USRA/NASA-GSFC  21-Jan-2003
*/

#define TOOLSUB batid2xy
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Structure for passing all PIL parameters to the main routine. */
typedef struct {
  char infile[PIL_PATH_MAX];
  char outfile[PIL_PATH_MAX];
   int detid,detx,dety;
   int block,dm,side,det;
} BATID2XY_PARAMS;   

/* Function prototypes */
int batid2xy(void);
int batid2xy_getpar (BATID2XY_PARAMS *params);
int batid2xy_printsummary (BATID2XY_PARAMS *params,long int *nevt,char *taskname,char *version);

/*-------------------------------------------------------------*/
int batid2xy(void)
{
   int status;
   int blkcol,dmcol,sidecol,detcol,detxcol,detycol;
   long int i,j,loc;
   long int nevt=0; /* Number of events in input file*/
   unsigned short int detid[NUM_ELEMENTS];

   short int block[NUM_ELEMENTS],dm[NUM_ELEMENTS],det[NUM_ELEMENTS];
   short int *tblock, *tdm, *tdet;
   short int *row, *col;
   short int detx[NUM_ELEMENTS],dety[NUM_ELEMENTS];
   int *location;
   short int ablock, adm, aside, adet;
   short int adetx, adety,aid; 
   int conv_xy=0, found=0;

   static char taskname[80] = "batid2xy";
   static char version[8] = "2.00";
   fitsfile *infptr = 0, *outfptr = 0, *wrtptr = 0;

   BATID2XY_PARAMS params;
   char keyname[9] = "";
   char keyvalue[80];              /* Value stored in header key */

   /* Register taskname and version. */
   set_toolname(taskname);
   set_toolversion(version);

   /*  get input parameters */
   status=batid2xy_getpar(&params);
   if (status) {
      fprintf(stderr, "Error reading parameters from .par file \n");
      goto cleanup;   
   }    
 

   /* If an input file is specified, then read the input
      values from the file.  Based on the BATIDMOD keyword,
      the file will contain values either as detid,  
      as block/dm/side/detector or as detx,dety. */

     if (*params.infile) {
 
         /* First check to see if the outfile name is the same as the
	    infile name.  If so, then prepare to write the output as
	    columns added to the infile. */
         if (!strcmp(params.infile,params.outfile)) {
	    headas_chat(5,"Will write to the input file\n");
            if (fits_open_file(&infptr,params.infile,READWRITE,&status)) goto cleanup; 
         } else {
            if (fits_open_file(&infptr,params.infile,READONLY,&status)) goto cleanup; 
         }            
         headas_chat(5,"Opened file %s %d\n",params.infile,status);

      /* Move to the correct HDU in the input file. The file must contain
         a "DETID" extension.*/
         if (fits_movnam_hdu(infptr,BINARY_TBL,"DETID",0,&status)) goto cleanup;
      /* Now look for the BATIDMOD keyword.  If it is absent, assume
         that the file has data in columns BLOCK, DM, SIDE, DET (the
         old default */
         strcpy(keyname,"BATIDMOD");
         if (!(fits_read_key(infptr,TSTRING,keyname,keyvalue,NULL,&status)));
	 else strcpy(keyvalue,"BDSD");
         status=0;
         headas_chat(5,"The value of the BATIDMOD keyword is %s\n",keyvalue);

      /* Get the number of detector IDs in the input file */
         headas_chat(5,"Moved to extension DETID %d\n",status);
         if (fits_get_num_rows(infptr,&nevt,&status)) goto cleanup;
         headas_chat(4,"Number of detectors is %d\n",nevt);

      /* Get the column numbers for the input file.  The column names 
         to be found depend on the value of the BATIDMOD keyword. */

         if (!strcmp(keyvalue,"BDSD")) {
	    /* The column numbers that are read are BLOCK, DM, SIDE, DET */
            if (fits_get_colnum(infptr,CASEINSEN,"BLOCK",&blkcol,&status)) goto cleanup;
            if (fits_get_colnum(infptr,CASEINSEN,"DM",&dmcol,&status)) goto cleanup;
            if (fits_get_colnum(infptr,CASEINSEN,"SIDE",&sidecol,&status)) goto cleanup;
            if (fits_get_colnum(infptr,CASEINSEN,"DET",&detcol,&status)) goto cleanup;
            headas_chat(5,"Status fits_get_colnum is %d\n",status);  
            headas_chat(5,"blkcol %d dmcol %d sidecol %d detcol %d\n",blkcol,dmcol,sidecol,detcol);  
    
            for (i=0;i<nevt;i++) {

               if (fits_read_col(infptr,TSHORT,blkcol,i+1,1,1,0,&ablock,0,&status)) goto cleanup; 
               if (fits_read_col(infptr,TSHORT,dmcol,i+1,1,1,0,&adm,0,&status)) goto cleanup; 
               if (fits_read_col(infptr,TSHORT,sidecol,i+1,1,1,0,&aside,0,&status)) goto cleanup; 
               if (fits_read_col(infptr,TSHORT,detcol,i+1,1,1,0,&adet,0,&status)) goto cleanup; 
               detid[i] =  2048*ablock + 256*adm + 128*aside + adet;
               if (i < 6) headas_chat(5,"Detid is %d block %d dm %d side %d det %d\n",detid[i],ablock,adm,aside,adet); 

            }
	 }
         if (!strcmp(keyvalue,"DETID")) {
	    /* The column numbers that are read are BLOCK, DM, SIDE, DET */
            if (fits_get_colnum(infptr,CASEINSEN,"DETID",&detcol,&status)) goto cleanup;
            headas_chat(5,"Status fits_get_colnum is %d\n",status);  
    
            for (i=0;i<nevt;i++) {

               if (fits_read_col(infptr,TSHORT,detcol,i+1,1,1,0,&adet,0,&status)) goto cleanup; 
               detid[i] =  adet;
               if (i < 6) headas_chat(5,"Detid is %d\n",detid[i]); 

            }
	 }
         if (!strcmp(keyvalue,"DETXY")) {
	    headas_chat(5,"Processing branch DETXY\n");
	    /* The column numbers that are read are DETX, DETY */
            if (fits_get_colnum(infptr,CASEINSEN,"DETX",&detxcol,&status)) goto cleanup;
            if (fits_get_colnum(infptr,CASEINSEN,"DETY",&detycol,&status)) goto cleanup;
            headas_chat(5,"Status fits_get_colnum is %d\n",status);  
    
            for (i=0;i<nevt;i++) {

               if (fits_read_col(infptr,TSHORT,detxcol,i+1,1,1,0,&adetx,0,&status)) goto cleanup; 
               if (fits_read_col(infptr,TSHORT,detycol,i+1,1,1,0,&adety,0,&status)) goto cleanup; 
               detx[i] =  adetx;
               dety[i] =  adety;
               if (i < 6) headas_chat(5,"Detx/Dety is %d,%d\n",detx[i],dety[i]); 

            }
	 }
     } else {
       /* If there is no input file, then accept the values from the 
	  command line. */
       
       nevt=1;
       headas_chat(5,"%d\t%d\t%d\t%d\n",params.block,params.dm,params.side,params.det);
       if (params.detid == -1)
          detid[0] = 2048 * params.block + 256 * params.dm + 128 * params.side + params.det;  
       else detid[0] = params.detid;

       detx[0] = params.detx;
       dety[0] = params.dety;
     }          

   /* If the code needs to convert from DETX DETY to DETID, then we need
      to set up some special arrays (since the core routine batidconvert
      is not reversible. */
   if (((params.detx > -1) && (params.dety > -1)) || 
       (!strcmp(keyvalue,"DETXY"))) {

     headas_chat(5,"Doing the special conversion from DETX and DETY\n");
     /* Set up an array that gives the location within the BAT map of each
        detector */
     location  = (int *)malloc((unsigned) NUM_ELEMENTS*sizeof(int));
     tblock  = (short int *)malloc((unsigned) NUM_ELEMENTS*sizeof(short int));
     tdm  = (short int *)malloc((unsigned) NUM_ELEMENTS*sizeof(short int));
     tdet  = (short int *)malloc((unsigned) NUM_ELEMENTS*sizeof(short int));
     row = (short int *)malloc((unsigned) NUM_ELEMENTS*sizeof(short int));
     col  = (short int *)malloc((unsigned) NUM_ELEMENTS*sizeof(short int));

     for (i=0;i<NUM_ELEMENTS;i++) detid[i]=i;
     batidconvert(NUM_ELEMENTS,detid,tblock,tdm,tdet,row,col);
     for (i=0;i<NUM_ELEMENTS;i++) location[i]=col[i]+row[i]*DAP_COLS;
     conv_xy = 1;

      for (i=0;i<nevt;i++) {
	 loc = detx[i]+dety[i]*DAP_COLS;
         found = 0;
         for (j=0;j<NUM_ELEMENTS;j++) {
	    if (loc == location[j]) {
	       block[i] = tblock[j];
               dm[i] = tdm[j];
               det[i] = tdet[j];
               found++;
            }
         } 
         if (!found) {
	    block[i] = -1;
	    dm[i] = -1;
	    det[i] = -1;
         }
      }
   } else {

      batidconvert(nevt,detid,block,dm,det,dety,detx);
          
   }

   headas_chat(0,"DETID\tBlock\tDM\tSIDE\tDET\tX\tY\n");

   for (i=0;i<nevt;i++) {
      if (dm[i] > -1) {
         adm=dm[i]/2;
         aside=(int)(2.0*((float)dm[i]/2.0-(float)adm)); 
         aid=2048*block[i] + 128*dm[i] + det[i];      
         headas_chat(0,"%d\t%d\t%d\t%d\t%d\t%d\t%d\n",aid,block[i],adm,aside,det[i],detx[i],dety[i]);
      } else {
         headas_chat(0,"X\tX\tX\tX\tX\t%d\t%d\n",detx[i],dety[i]);
      }
   } 

    if (*params.infile) {
       /* Write optional history keywords */
       /* Replaced headas_parstamp with HDpar_stamp  4-Dec-2003  */
       status=HDpar_stamp(wrtptr,0,&status); 
       /* status=headas_parstamp(wrtptr,0); */
       if (status) goto cleanup;
    }

    /* Print out a summary of operations (unless chatter=0) */
    status=batid2xy_printsummary(&params,&nevt,taskname,version);
    if (status) goto cleanup;

    if (*params.outfile) {
       /* First branch is the case for which the outfile is the same
	  as the infile.  In this case add columns to the infile */
       if (!strcmp(params.infile,params.outfile)) {
          
       }        
    }

    if (*params.infile) {
       if (fits_close_file(infptr,&status)) goto cleanup;
    }

  return(status);

cleanup:

    if (wrtptr) fits_close_file(wrtptr, &status);
    if ( outfptr) fits_close_file(outfptr,  &status);
    if ( infptr) fits_close_file(infptr,  &status);

    if (status) fits_report_error(stderr, status);
    return(status);
}

/*-------------------------------------------------------------*/

/* Routine to make all calls to PIL to fill the structure containing
   all user supplied paramters. */

int batid2xy_getpar (
    BATID2XY_PARAMS *params)

{
 
   int len,status=0;
   char *cptr;

   if ((status = PILGetString("infile", params->infile)))
      fprintf(stderr, "Error reading the 'infile' parameter.\n");

   else if ((status = PILGetFname("outfile", params->outfile)))
      fprintf(stderr, "Error reading the 'outfile' parameter.\n");

   else if ((status = PILGetInt("block", &params->block)))
      fprintf(stderr, "Error reading the 'calfile' parameter.\n");

   else if ((status = PILGetInt("dm", &params->dm)))
      fprintf(stderr, "Error reading the 'calfile' parameter.\n");

   else if ((status = PILGetInt("side", &params->side)))
      fprintf(stderr, "Error reading the 'calfile' parameter.\n");

   else if ((status = PILGetInt("det", &params->det)))
      fprintf(stderr, "Error reading the 'calfile' parameter.\n");

   else if ((status = PILGetInt("detid", &params->detid)))
      fprintf(stderr, "Error reading the 'detid' parameter.\n");

   else if ((status = PILGetInt("detx", &params->detx)))
      fprintf(stderr, "Error reading the 'detx' parameter.\n");

   else if ((status = PILGetInt("dety", &params->dety)))
      fprintf(stderr, "Error reading the 'dety' parameter.\n");

   else { 

        /* remove leading blanks in output file string */
        cptr = params->outfile;
        while (*cptr == ' ') cptr++;  
        if (cptr != params->outfile) {
            len = strlen(cptr);
            memmove(params->outfile, cptr, len + 1);
        }

    }

    if (!strcmp(params->infile, "none") || !strcmp(params->infile, "NONE") ) *params->infile = '\0';
    if (!strcmp(params->outfile, "none") || !strcmp(params->outfile, "NONE") ) *params->outfile = '\0';

    headas_chat(4,"The input file is called %s\n",params->infile);
    headas_chat(4,"The output file is called %s\n",params->outfile);

    return(status);

}



/*-------------------------------------------------------------*/

int batid2xy_printsummary (
   BATID2XY_PARAMS *params,
   long int *nevt,
   char *taskname,
   char *version)

{ 
    headas_chat(2,"%s v%s completed\n",taskname,version);
    if (*params->infile) { 
       headas_chat(2,"Input events file is %s\n",params->infile);
       if (*params->outfile) headas_chat(1,"%s\n",params->outfile);
       else headas_chat(2,"%s\n",params->infile);
    }

    return(0);
}
