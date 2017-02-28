/*
   File:   xstar2table.c
   Author: W.T. Bridgman, RITSS
   Date:   January 1999

   Description:
   Routine to write ATABLES & MTABLES files for a multiple xstar run.  
-----------------------------------------------------------------------
   Code Outline:

   START
   Read XSTAR2TABLE Parameter file
   Open XSTAR spectrum output file
      Read XSTAR parameter extension
      Read XSTAR spectrum extension
      Close XSTAR spectrum file

   Open ATABLE_IN, ATABLE_OUT, & MTABLE FITS files

   for each TABLE file:
      If ENERGIESEXT flag set
          Build ENERGIES extension from selection range
          Build header for SPECTRA extension
          Write LASTSPEC=0 keyword
      else
         Check that ENERGIES extension matches

   Compute (row,column) for FITS table
   Build PARMVAL array

   Build SPECTRUM=TRANSMITTED/INCIDENT
   Check LASTSPEC=LOOPCONTROL-1?
      if no, then exit with error message
   Write to MTABLE
   Update LASTSPEC value

   Build SPECTRUM=EMIT_OUTWARD
   Check LASTSPEC=LOOPCONTROL-1?
      if no, then exit with error message
   Write to ATABLE_OUT
   Update LASTSPEC value

   Build SPECTRUM=EMIT_INWARD
   Check LASTSPEC=LOOPCONTROL-1?
      if no, then exit with error message
   Write to ATABLE_IN
   Update LASTSPEC value

   Close all files
   Clean-up dynamically allocated structures
   END

   Caveats & other potential gotchas:

   Known Platform-Dependent Issues:
   --------------------------------
   * sprintf() is implemented as the standard on OSF compiler (returns an
     int) but NOT on the Sun compiler (returns char*).  Therefore, do not
     use the return value from this function.
   * Always assume that string literals are being written to read-only
     memory.  Do not try to modify them.

-----------------------------------------------------------------------

   To Do:
   ------

-----------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "xpi.h"         /* parameter file functions, e.g. Uclgst */
#include "cfortran.h"    /* for access to the SLALIB routines     */

#include "xstar2table.h"
#include "xstarutillib.h"

#define  ERRMSG 255 

void xstar2table(void) {

   char  progname[]="xstar2table";
   char  version[]="1.0";

   /* temporary and work variables */
   int           status  = 0;
   int           verbose = 0;
   int           j, ie, ic, numstrings=0, fileno, addmodel, interIndex;
   char          **cmdstrings;
   char          msg[ERRMSG],msg2[ERRMSG],s[ERRMSG]="\0";
   long          nEnergyBins, eBinLow, eBinHigh, ncombos, binNum, curbin;
   long          tableRow, tableColumn;
   long          nintparams, naddparams, curIPtr;
   float         *energyLow, *energyHigh, *spectrum, *paramvals;
   float         luminosity, enorm;

   /* use of FLEN_FILENAME & FLEN_COMMENT macros in strings is 
      required for XPI interface                  */
   XSTAR2TABLE_Parms inputParmlist;
   XSTAR_Parms       xstarParameters; /* parameters for the XSTAR run */
   XSTAR_Spec        xstarSpectra;    /* spectra from the XSTAR run */
   Parameter_struct  xstar2xspecTable; /* parameters for the XSTAR2XSPEC run */
   fitsfile          *fspecptr;
   fitsfile          *ftable[3]; /* Create a table for the output file pointers */

   /************* some initializations ** ******************/
   for (j=0;j<21;j++) {
     xstar2xspecTable.physical[j].delta=-1.;
   }

   /************************************* ******************/
   sprintf(msg,"%s v%s",progname,version);c_fcecho(msg);
   sprintf(msg,"Compiled: %s %s",__DATE__,__TIME__);c_fcecho(msg);

   if(getenv("XSTARDEBUG")!=(char*)NULL) {
     strcpy(s,getenv("XSTARDEBUG"));printf("environment:%s\n",s);
     if(isdigit(s[0])) verbose=atoi(s);/* set for verbose logging? */
   }

   /************* grab parameters via XPI ******************/
   if(verbose) c_fcecho("xstar2table: Retrieving Input Parameters");
   status=Get_XSTAR2TABLE_Parms(&inputParmlist,verbose);
   if(status) {
     sprintf(msg,"Get_XSTAR2TABLE_Parms return code = %d",status);
     c_fcerr(msg);
   }

   /***** extract spectral data from XSTAR output **********/
   if(verbose) {
     sprintf(msg,"xstar2table: Opening XSTAR FITS file %s.",inputParmlist.xstarSpecFile);
     c_fcecho(msg);
   }
   if (fits_open_file(&fspecptr, inputParmlist.xstarSpecFile, READONLY, 
		      &status)) PrintError(status);
   status=Read_XSTAR_Parmlist(fspecptr,&xstarParameters,verbose);
   if(status) {
     sprintf(msg,"Read_XSTAR_Parmlist return code = %d",status);
     c_fcerr(msg);
   }

   status=Read_XSTAR_Spectra(fspecptr,&xstarSpectra,verbose);
   if(status) {
     sprintf(msg,"Read_XSTAR_Spectra return code = %d",status);
     c_fcerr(msg);
   }
   if (fits_close_file(fspecptr, &status) ) PrintError( status );
   if(verbose) {
     sprintf(msg,"xstar2table: Closing XSTAR FITS file %s.",inputParmlist.xstarSpecFile);
     c_fcecho(msg);
   }

   /**********************************************************/
   /* Open the MTABLE & ATABLE files for output              */
   /* This portion assumes the ATABLE_IN, ATABLE_OUT, and    */
   /* MTABLE files have already been created from the        */
   /* XSTINITABLE.FITS file.                                 */
   /* This step also sets the keyword for the model type,    */
   /* adding the keyword if not already present              */
   if(verbose) {
     sprintf(msg,"xstar2table: Opening ATABLE IN FITS file %s.",inputParmlist.atable_inFile);
     c_fcecho(msg);
   }
   if (fits_open_file(&ftable[0], inputParmlist.atable_inFile, READWRITE, &status)) PrintError(status);
   addmodel=1;
   if(fits_update_key(ftable[0],TLOGICAL,"ADDMODEL",&addmodel,"Additive Model",&status)) PrintError(status);

   if(verbose) {
     sprintf(msg,"xstar2table: Opening ATABLE OUT FITS file %s.",inputParmlist.atable_outFile);
     c_fcecho(msg);
   }
   if (fits_open_file(&ftable[1], inputParmlist.atable_outFile, READWRITE, &status)) PrintError(status);
   addmodel=1;
   if(fits_update_key(ftable[1],TLOGICAL,"ADDMODEL",&addmodel,"Additive Model",&status)) PrintError(status);

   if(verbose) {
     sprintf(msg,"xstar2table: Opening MTABLE FITS file %s.",inputParmlist.mtable_File);
     c_fcecho(msg);
   }
   if (fits_open_file(&ftable[2], inputParmlist.mtable_File, READWRITE, &status)) PrintError(status);
   addmodel=0;
   if(fits_update_key(ftable[2],TLOGICAL,"ADDMODEL",&addmodel,"Multiplicative Model",&status)) PrintError(status);
   
   /*********** Read XSTINITABLE Parameter Table *************/
   /* for the sake of being definitive, we'll use ATABLE_In for this */
   status=Read_FITS_ParmTable(ftable[0],&xstar2xspecTable,verbose);
   /* extract some additional supporting quantities */
   if(verbose) c_fcecho("xstar2table: Extracting parameter support quantities.");
   if(fits_read_key(ftable[0],TLONG,"NINTPARM",&nintparams,(char*)NULL,
               &status)) PrintError(status);/* number of interpolated parameters */
   if(fits_read_key(ftable[0],TLONG,"NADDPARM",&naddparams,(char*)NULL,
	       &status)) PrintError(status);/* number of additional parameters */
   status=Display_FITS_ParmTable(&xstar2xspecTable);

   /* energy range testing */
   if(verbose) c_fcecho("xstar2table: Slicing the Energy table...");
   status=SliceEnergySpectra(xstar2xspecTable.energyLow,xstar2xspecTable.energyHigh,
			       &xstarSpectra,&nEnergyBins,&eBinLow,&eBinHigh);
   
   /* generate empty paramvals array */
   paramvals=(float*)malloc(nintparams*sizeof(float));

   /* compute number of interpolated parameter combinations */
   /* and build PARAMVALS array                             */
   /* Note that the loading order of the PARAMVALS array is */
   /* the same as the table parameter number                */
   /* This is important to note when making changes to this */
   /* program and XSTINITABLE as these orderings must       */
   /* remain synchronized for XSPEC.                        */
   if(verbose) c_fcecho("xstar2table: Build the PARAMVALS array...");
   ncombos=1;
   curIPtr=0; /* initialize interpolated parameter pointer */
   for(j=0;j<xstar2xspecTable.numberOfParameters;j++){
     if(xstar2xspecTable.physical[j].type==kInterpolated) {
       /* found an interpolated parameter, now manage it */
       ncombos*=(long)xstar2xspecTable.physical[j].numberOfValues; /* compute number of combinations */
       /* identify the value of this parameter in the current XSTAR run */
       interIndex=GetInterParmIndex(xstar2xspecTable.physical[j].name,&xstarParameters);
       paramvals[curIPtr]=xstarParameters.physical[interIndex];
       if(verbose){
	 sprintf(msg,"%3d) %-16s %10.3E.",curIPtr+1,
			 xstarParameters.parmName[interIndex],xstarParameters.physical[interIndex]);
	 c_fcecho(msg);
       }
       curIPtr++; /* increment pointer */
       if(curIPtr>nintparams){
	 sprintf(msg,"xstar2table: PARAMVALS Pointer = %d is Invalid.",curIPtr);
	 c_fcecho(msg);
       }
     }
   } /* end of 'j' loop */
   if(verbose) {
      sprintf(msg,"xstar2table: %d entries = [",nintparams);
      for(j=0;j<nintparams;j++) {
	sprintf(msg2,"%10.2E ",paramvals[j]);
	strcat(msg,msg2);
      }
      strcat(msg,"]");
      c_fcecho(msg);
   }

   /******** Build the ENERGIES extension if loopcontrol==1 *******/
   if(xstarParameters.loopcontrol==1) {
   if(verbose) c_fcecho("xstar2table: Building the ENERGIES extension...");
      /* allocate memory for the selected energy array */
      energyLow=(float*)malloc(nEnergyBins*sizeof(float));
      energyHigh=(float*)malloc(nEnergyBins*sizeof(float));
      /* build the energy tables (remembering to convert to keV) */
      for(binNum=0;binNum<nEnergyBins;binNum++){
	energyLow[binNum]=xstarSpectra.energy[binNum+eBinLow]/1000.0;
	energyHigh[binNum]=xstarSpectra.energy[binNum+eBinLow+1]/1000.0;
        /* if(verbose) printf("%05d) %10.2f - %10.2f eV\n",binNum+1,energyLow[binNum],energyHigh[binNum]);*/
      }
     /* Write the ENERGIES extension to all three tables     */
     /* This will usually be the first call of XSTAR2TABLE   */
     status=Write_FITS_Energies(ftable[0],nEnergyBins,energyLow,energyHigh,verbose);
     status=Write_FITS_Energies(ftable[1],nEnergyBins,energyLow,energyHigh,verbose);
     status=Write_FITS_Energies(ftable[2],nEnergyBins,energyLow,energyHigh,verbose);

     /* Write SPECTRA headers for the files */
     status=Create_FITS_Spectra_Header(ftable[0],ncombos,nintparams,naddparams,nEnergyBins,
				       "photons/cm^2/s",verbose);
     status=Create_FITS_Spectra_Header(ftable[1],ncombos,nintparams,naddparams,nEnergyBins,
				       "photons/cm^2/s",verbose);
     status=Create_FITS_Spectra_Header(ftable[2],ncombos,nintparams,naddparams,nEnergyBins,
				       " ",verbose);

     free(energyLow); /* free up memory since we're now done with it */
     free(energyHigh);
   } else {
     /* Check ENERGIES extension */
     c_fcecho("xstar2table: Install a check of the ENERGIES extension here?");
     c_fcecho("xstar2table: Does it match the previous ENERGIES extension?");
   }

   /*    compute (row,column) for output spectrum */
   tableRow=1+(int)((xstarParameters.loopcontrol-1)/(naddparams+1));
   tableColumn=(xstarParameters.loopcontrol-1)%(naddparams+1);
   if(verbose){
     sprintf(msg,"xstar2table: Installing spectrum in (row,column)=(%4d,%4d).",tableRow,tableColumn);
     c_fcecho(msg);
   }

   /* get luminosity for this model.  We'll search the table so it doesn't matter what order
      the table is in, just that rlrad38 is located in it. */
   luminosity=GetPhysicalParameter(&xstarParameters,"rlrad38");
   if(verbose){
     sprintf(msg,"xstar2table: Luminosity of this model: %10.5e",luminosity);
     c_fcecho(msg);
   }
   if(luminosity<=0){
     sprintf(msg,"xstar2table: %10.5e is an invalid luminosity value.\nExiting...",luminosity);
     c_fcerr(msg);
     exit(EXIT_FAILURE);
   }

   spectrum=(float*)malloc(nEnergyBins*sizeof(float));

   /* Build ATABLE IN components */
   for(binNum=0;binNum<nEnergyBins;binNum++) {
     curbin=binNum+eBinLow;
     enorm=8.356e-7*(xstarSpectra.energy[curbin+1]-xstarSpectra.energy[curbin])
       /(luminosity*xstarSpectra.energy[curbin]); /* compute normalization */
     spectrum[binNum]=enorm*xstarSpectra.emit_inward[curbin];
   }
   status=Write_FITS_Spectra(ftable[0],xstarParameters.loopcontrol,tableRow,tableColumn,nEnergyBins,
			     spectrum,nintparams,paramvals,verbose);

   /* Build ATABLE OUT components  */
   for(binNum=0;binNum<nEnergyBins;binNum++) {
     curbin=binNum+eBinLow;
     enorm=8.356e-7*(xstarSpectra.energy[curbin+1]-xstarSpectra.energy[curbin])
       /(luminosity*xstarSpectra.energy[curbin]); /* compute normalization */
     spectrum[binNum]=enorm*xstarSpectra.emit_outward[curbin];
   }
   status=Write_FITS_Spectra(ftable[1],xstarParameters.loopcontrol,tableRow,tableColumn,nEnergyBins,
			     spectrum,nintparams,paramvals,verbose);

   /*    Build MTABLE components  */
   for(binNum=0;binNum<nEnergyBins;binNum++) {
     /* test for divide-by-zero */
     if(xstarSpectra.incident[binNum+eBinLow]==0.0) {
     if(verbose) printf("%5d) %12.5e  %12.5e - division by zero error\n",binNum+1,xstarSpectra.transmitted[binNum+eBinLow],xstarSpectra.incident[binNum+eBinLow]);
        spectrum[binNum]=0.0;
     } else {
        spectrum[binNum]=xstarSpectra.transmitted[binNum+eBinLow]/xstarSpectra.incident[binNum+eBinLow];
        if((spectrum[binNum]<0.0)||(spectrum[binNum]>1.0)){
          sprintf(msg,"%5d) %12.5e  %12.5e -> %12.5e - range error",binNum+1,
	       xstarSpectra.transmitted[binNum+eBinLow],xstarSpectra.incident[binNum+eBinLow],
               spectrum[binNum]);
	  c_fcerr(msg);
        }
     }
   }
   status=Write_FITS_Spectra(ftable[2],xstarParameters.loopcontrol,tableRow,tableColumn,nEnergyBins,
			     spectrum,nintparams,paramvals,verbose);

   /* Close all open files */
   if(verbose) c_fcecho("xstar2table: Closing ATABLE/MTABLE FITS files.");
   for (fileno=0;fileno<3;fileno++) {
      status = 0;
      if (fits_close_file(ftable[fileno], &status) ) PrintError( status );
   }
   /********************* Big cleanup!! **********************/
   /* release dynamically allocated sample lists */
   if(verbose) c_fcecho("xstar2table: Memory cleanup");
   free(paramvals);
   free(spectrum);
   for(j=0;j<xstarParameters.parmCount;j++){
   	free(xstarParameters.parmName[j]);
   }
   free(xstarSpectra.energy);
   free(xstarSpectra.incident);
   free(xstarSpectra.transmitted);
   free(xstarSpectra.emit_inward);
   free(xstarSpectra.emit_outward);
   /* for(j=0;j<xstar2xspecTable.numberOfParameters;j++) {
     if(xstar2xspecTable.physical[j].valueList!=(float*)NULL) 
       free(xstar2xspecTable.physical[j].valueList);
   }*/
   if(verbose) c_fcecho("xstar2table: Memory cleanup complete");

   /* Sentry value to signal successful completion 
      to xstar2xspec script.
      ALTER AT YOUR OWN RISK!!!!!!!!!! */
   c_fcecho("xstar2table: Successful Completion");
}

/************************************************************
 * Function:                                                *
 *    Get_XSTAR2TABLE_Parms                                 *
 *                                                          *
 * Description:                                             *
 *      gets the parameters for the task xstar2table using  *
 *      the Xanadu Parameter Interface (XPI).               *
 *                                                          *
 *  Input Parameters:                                       *
 *     All values are returned from the call                *
 *  Output Values:                                          *
 *                                                          *
 * Author:                                                  *        
 *      W.T. Bridgman                                       *
 *      January 1999                                        *
 *      Based on code by                                    *
 *      Banashree M Seifert & Jeff Silvis                   *
 ************************************************************/
int Get_XSTAR2TABLE_Parms(XSTAR2TABLE_Parms *parms,int verbose)
 {
     
   /*  BufLen_2 is required by XPI and it has to be one less than the
    length of the character variables that Uclgst pulls
    */
     int        BufLen_2=FLEN_FILENAME-1;  /* required by cfortran.h*/
     char       tempstr[FLEN_FILENAME];    /* required by cfortran.h*/
     char       parmstr[FLEN_FILENAME];
     int        tempint;
     int        status=0;

     /* Now load the additional parameters */
     /* Input XSTAR spectrum file name */
     Uclgst("xstarspec", tempstr, &status);
     strcpy(parms->xstarSpecFile,tempstr);
     if(parms->xstarSpecFile[0] =='\0'){
        status=1;
        c_fcerr("\nGet_XSTAR2TABLE_Parms: XSTAR Spectrum File Name is required ");
     }

     /* Input ATABLE_IN file name */
     Uclgst("atableinfile", tempstr, &status);
     strcpy(parms->atable_inFile,tempstr);
     if(parms->atable_inFile[0] =='\0'){
        status=1;
        c_fcerr("\nGet_XSTAR2TABLE_Parms: ATABLE_IN file name is required ");
     }     

     /* Input ATABLE_OUT file name */
     Uclgst("atableoutfile", tempstr, &status);
     strcpy(parms->atable_outFile,tempstr);
     if(parms->atable_outFile[0] =='\0'){
        status=1;
        c_fcerr("\nGet_XSTAR2TABLE_Parms: ATABLE_OUT file name is required ");
     }     

     /* Input MTABLE file name */
     Uclgst("mtablefile", tempstr, &status);
     strcpy(parms->mtable_File,tempstr);
     if(parms->mtable_File[0] =='\0'){
        status=1;
        c_fcerr("\nGet_XSTAR2TABLE_Parms: MTABLE file name is required ");
     }     

     return(status);
 }

int Read_XSTAR_Parmlist(fitsfile *fptr, XSTAR_Parms* parms, int verbose)
{
  int    status=0;
  int    frow=1, felem=1, anynull,typecode,j;
  char   **parnames, **partype, **parcomm, strnull[]=" ";
  long   nrows,repeat,width;
  char   msg[ERRMSG];
  float  floatnull, *parvals;
  /* When adding parameters to XSTAR, make sure that this sequence  */
  /* of loading the variables is kept in sync with that defined in  */
  /* the XSTINITABLE ftool and XSTAR itself.                        */
  char   *physParms[]={"cfrac","temperature","pressure",
			     "density","trad","rlrad38","column","rlogxi",
                             "habund","heabund","liabund","beabund",
                             "babund","cabund","nabund","oabund",
                             "fabund","neabund","naabund","mgabund",
                             "alabund","siabund","pabund","sabund",
                             "clabund","arabund","kabund",
                             "caabund","scabund","tiabund","vabund",
                             "crabund","mnabund","feabund","coabund",
                             "niabund","cuabund","znabund"};
  int    nPhysParms=38;

  /* move to PARMLIST HDU */
  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Moving to PARAMETERS extension.");
  if(fits_movnam_hdu(fptr,BINARY_TBL,"PARAMETERS",0,&status))
           PrintError(status);

  if(fits_get_num_rows(fptr,&nrows,&status)) PrintError(status);
  /* allocate some memory for nrows of string pointers & floats */
  parnames=(char**)malloc(nrows*sizeof(char*));
  partype=(char**)malloc(nrows*sizeof(char*));
  parcomm=(char**)malloc(nrows*sizeof(char*));
  parvals=(float*)malloc(nrows*sizeof(float));
  if((parnames==(char**)NULL)||(partype==(char**)NULL)||(parcomm==(char**)NULL)
     ||(parvals==(float*)NULL)) {
       c_fcecho("Read_XSTAR_Parmlist: Unable to allocate parameter arrays.  Exiting...\n");
       exit(EXIT_FAILURE);
  }

  /* load parameter names, types & comments */
  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Loading Parameter Names.\n");
  if(fits_get_coltype(fptr,2,&typecode,&repeat,&width,&status)) PrintError(status);
  for(j=0;j<nrows;j++) parnames[j]=(char*)malloc((repeat+1)*sizeof(char));
  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Reading Table.\n");
  if(fits_read_col(fptr,TSTRING,2,frow,felem,nrows,strnull,parnames,
		    &anynull,&status)) PrintError(status);

  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Loading Parameter Types.");
  if(fits_get_coltype(fptr,4,&typecode,&repeat,&width,&status)) PrintError(status);
  for(j=0;j<nrows;j++) partype[j]=(char*)malloc((repeat+1)*sizeof(char));
  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Reading Table.");
  if(fits_read_col(fptr,TSTRING,4,frow,felem,nrows,strnull,partype,
		    &anynull,&status)) PrintError(status);


  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Loading Parameter Comment.");
  if(fits_get_coltype(fptr,5,&typecode,&repeat,&width,&status)) PrintError(status);
  for(j=0;j<nrows;j++) parcomm[j]=(char*)malloc((repeat+1)*sizeof(char));
  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Reading Table.");
  if(fits_read_col(fptr,TSTRING,5,frow,felem,nrows,strnull,parcomm,
		    &anynull,&status)) PrintError(status);

  /* load floating point values */
  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Loading Floating-point values.");
  if(fits_read_col(fptr,TFLOAT,3,frow,felem,nrows,&floatnull,parvals,
		    &anynull,&status)) PrintError(status);

  /***************** Now for the tough part! ************************/
  /* We're going to parse this set of strings and reals to populate */
  /* the parameter table in a fashion where we can understand some  */
  /* of the physics.                                                */
  /*                                                                */
  if(verbose) c_fcecho("Read_XSTAR_Parmlist: Loading Parameter Values.");
  for(j=0;j<nPhysParms;j++){
    parms->parmName[j]=(char*)malloc(1+strlen(physParms[j]));
    strcpy(parms->parmName[j],physParms[j]);
    parms->physical[j]=ParseFloat(parms->parmName[j],nrows,parnames,parvals,partype);
    if(verbose) {
      sprintf(msg,"%03d) %-20s: %10.3g",j+1,parms->parmName[j],parms->physical[j]);
      c_fcecho(msg);
    }

  } 
  parms->parmCount=nPhysParms;

  parms->spectrumUnits=ParseInteger("spectun",nrows,parnames,parvals,partype);
  parms->numberOfSteps=ParseInteger("nsteps",nrows,parnames,parvals,partype);
  parms->numberOfIterations=ParseInteger("niter",nrows,parnames,parvals,partype);
  parms->writeSwitch=ParseInteger("lwrite",nrows,parnames,parvals,partype);
  parms->printSwitch=ParseInteger("lprint",nrows,parnames,parvals,partype);
  parms->stepSizeChoiceSwitch=ParseInteger("lstep",nrows,parnames,parvals,partype);
  parms->numberOfPasses=ParseInteger("npass",nrows,parnames,parvals,partype);
  parms->constantPressureSwitch=ParseInteger("lcpres",nrows,parnames,parvals,partype);
  parms->loopcontrol=ParseInteger("loopcontrol",nrows,parnames,parvals,partype);

  /* print the table data */
  if(verbose) {
    sprintf(msg,"     Spectrum Units:           %d",parms->spectrumUnits);c_fcecho(msg);
    sprintf(msg,"     Number of Steps:          %d",parms->numberOfSteps);c_fcecho(msg);
    sprintf(msg,"     Number of Iterations:     %d",parms->numberOfIterations);c_fcecho(msg);
    sprintf(msg,"     Write Switch:             %d",parms->writeSwitch);c_fcecho(msg);
    sprintf(msg,"     Print Switch:             %d",parms->printSwitch);c_fcecho(msg);
    sprintf(msg,"     Step Size Switch:         %d",parms->stepSizeChoiceSwitch);c_fcecho(msg);
    sprintf(msg,"     Number of Passes:         %d",parms->numberOfPasses);c_fcecho(msg);
    sprintf(msg,"     Constant Pressure Switch: %d",parms->constantPressureSwitch);c_fcecho(msg);
    sprintf(msg,"     Loop Control:             %d",parms->loopcontrol);c_fcecho(msg);
  }

  /* now clean up dynamic memory */
  free(parvals);
  for(j=0;j<nrows;j++) {
    free(parnames[j]);
    free(partype[j]);
    free(parcomm[j]);
  } /* end of j loop */

  return(status);
}

/* Extract the value of a single physical parameter from the 
   XSTAR physical parameter table */
float GetPhysicalParameter(XSTAR_Parms *parms, char *parmname) {
  int j=0, result;
  int done=0;
  char msg[ERRMSG];

  do {
    result=strcmp(parms->parmName[j],parmname);
    /* did we find it? */
    if(result==0) done=1;
    else j++;
    if(j>=parms->parmCount){
      sprintf(msg,"GetPhysicalParameter: Parameter %s not found.",parmname);
      c_fcerr(msg);
      return 0.0;
    }
  } while(!done);
  return parms->physical[j];
}

/* Determine the parameter index in the XSTAR_Parm structure for the 
   specified parname.  This function is used to identify the correct parameter
   matchups between the parameter table in the ATABLE/MTABLE files and the
   one stored in the XSTAR spectrum file. */
int GetInterParmIndex(char *parname, XSTAR_Parms* parms) {
  int index=-1, j=0, test;
  char msg[ERRMSG];

  do { 
    test=strcmp(parname,parms->parmName[j]);
    if(test==0) index=j; 
    else j++; 
  } while((index==-1)&&(j<parms->parmCount));
  if(index==-1){
    sprintf(msg,"GetIntParmIndex: Parameter %s not found...",parname);
    c_fcerr(msg);
  }
  return index;
}
/* Read in the SPECTRUM extension from the XSTAR output FITS file and load it
   into a data structure */
int Read_XSTAR_Spectra(fitsfile *fptr, XSTAR_Spec* spec, int verbose)
{
  int    status=0;
  int    frow, felem, anynull;
  long   nrows, j;
  float  floatnull;
  char   msg[ERRMSG];

  /* move to Spectral HDU */
  if(verbose) c_fcecho("Read_XSTAR_Spectra: Moving to SPECTRAL extension.");
  if(fits_movnam_hdu(fptr,ASCII_TBL,"XSTAR_SPECTRA",0,&status))
           PrintError(status);

  if(fits_get_num_rows(fptr,&nrows,&status)) PrintError(status);
  spec->nBins=nrows;

  /* now allocate space for the data arrays */
  if(verbose) c_fcecho("Read_XSTAR_Spectra: Allocating storage for SPECTRAL extension.");
  spec->energy=(float*)malloc(nrows*sizeof(float));
  spec->incident=(float*)malloc(nrows*sizeof(float));
  spec->transmitted=(float*)malloc(nrows*sizeof(float));
  spec->emit_inward=(float*)malloc(nrows*sizeof(float));
  spec->emit_outward=(float*)malloc(nrows*sizeof(float));
  if((spec->energy!=NULL)&&(spec->incident!=NULL)&&(spec->transmitted!=NULL)
     &&(spec->emit_inward!=NULL)&&(spec->emit_outward!=NULL)){
    /* storage allocated successfully, now load the data */
    frow=1;
    felem=1;
    floatnull=0.0;
    if(verbose) c_fcecho("Read_XSTAR_Spectra: Reading data for SPECTRAL extension.");
    if(fits_read_col(fptr,TFLOAT,1,frow,felem,nrows,&floatnull,
		     spec->energy,&anynull,&status)) PrintError(status);
    if(fits_read_col(fptr,TFLOAT,2,frow,felem,nrows,&floatnull,
		     spec->incident,&anynull,&status)) PrintError(status);
    if(fits_read_col(fptr,TFLOAT,3,frow,felem,nrows,&floatnull,
		     spec->transmitted,&anynull,&status)) PrintError(status);
    if(fits_read_col(fptr,TFLOAT,4,frow,felem,nrows,&floatnull,
		     spec->emit_inward,&anynull,&status)) PrintError(status);
    if(fits_read_col(fptr,TFLOAT,5,frow,felem,nrows,&floatnull,
		     spec->emit_outward,&anynull,&status)) PrintError(status);
  } else {
    sprintf(msg,"Read_XSTAR_Spectra: Unable to allocate %d bytes for spectral table.\n", 5*nrows*sizeof(float));
    c_fcerr(msg);
    status=MEM_ALLOC_FAILURE;
  }
  return(status);
}

/* Extract an integer value with name parname from the parameter table */
int  ParseInteger(char* parname, int numparms, char **names, 
		 float *values, char **types){
  int value;
  int found=0, pointer=0;
  char msg[ERRMSG];

  while(!found&&(pointer<numparms)){
    if(strcmp(parname,names[pointer])==0) found=1;
    else pointer++;
  }
  if(!found) {
    sprintf(msg,"ParseInteger: Parameter %s not found.  Exiting...",parname);
    c_fcerr(msg);
    exit(EXIT_FAILURE);
  }
  if(strcmp(types[pointer],"integer")==0) {
    value=(int)values[pointer];
    /* printf("%d) %s=%d\n",pointer+1,names[pointer],value); */
  } else {
    sprintf(msg,"ParseInteger: Parameter %s type mismatch.  Expected %s.  Found %s[%d].  Exiting...",parname,"integer",types[pointer],pointer);
    c_fcerr(msg);
    exit(EXIT_FAILURE);
  }
  return(value);
}

/* Extract a floating-point value with name parname from the parameter table */
float ParseFloat(char* parname, int numparms, char **names, 
		 float *values, char **types){
  float value;
  int found=0,pointer=0;
  char msg[ERRMSG];

  while(!found&&(pointer<numparms)){
    if(strcmp(parname,names[pointer])==0) found=1;
    else pointer++;
  }
  if(!found) {
    sprintf(msg,"ParseFloat: Parameter %s not found.  Exiting...",parname);
    c_fcerr(msg);
    exit(EXIT_FAILURE);
  }
  if(strcmp(types[pointer],"real")==0) {
    value=values[pointer];
    /* printf("%d) %s=%10.4e\n",pointer+1,names[pointer],value);*/
  } else {
    sprintf(msg,"ParseFloat: Parameter %s type mismatch.  Expected %s.  Found %s[%d].  Exiting...",
	    parname,"real",types[pointer],pointer);
    c_fcerr(msg);
    exit(EXIT_FAILURE);
  }
  return(value);
}

/* Perform a slice on the energy binning */
int SliceEnergySpectra(float elow, float ehigh, XSTAR_Spec *spectra, 
			 long *nEnergyBins, long *eBinLow, long *eBinHigh) {
   int status=0;
   long j;
   char msg[ERRMSG];

   /* check for really silly error */
   if(ehigh<elow) {
     sprintf(msg,"SliceEnergySpectra: ENERGYHIGH=%10.5g must be greater than ENERGYLOW=%10.5g.  Exiting...",
	    ehigh,elow);
     c_fcerr(msg);
     exit(EXIT_FAILURE);
   }
   /* check if specified energies are in range */
   if((elow<spectra->energy[0])
      ||(elow>spectra->energy[spectra->nBins-1])) {
      sprintf(msg,"SliceEnergySpectra: ENERGYLOW=%10.5g is outside of (%10.5g,%10.5g).  Resetting to %10.5g.",
	     elow,spectra->energy[0],spectra->energy[spectra->nBins-1],spectra->energy[0]);
      c_fcecho(msg);
      elow=spectra->energy[0];
   }
   if((ehigh<spectra->energy[0])
      ||(ehigh>spectra->energy[spectra->nBins-1])) {
      sprintf(msg,"SliceEnergySpectra: ENERGYHIGH=%10.5g is outside of (%10.5g,%10.5g).  Resetting to %10.5g.",
	     ehigh,spectra->energy[0],spectra->energy[spectra->nBins-1],spectra->energy[spectra->nBins-1]);
      c_fcecho(msg);
      ehigh=spectra->energy[spectra->nBins-1];
   }

   /* Select range of new energy table */
   for(j=1;j<spectra->nBins;j++){
     if((spectra->energy[j]>=elow)&&(spectra->energy[j]<=ehigh)){
       /* is it the lowest selected bin? */
       if(spectra->energy[j-1]<elow){
         *nEnergyBins=1;
         *eBinLow=j-1;
       } else { 
         /* is it the highest selected bin? */
         if(spectra->energy[j+1]>ehigh) *eBinHigh=j-1;
	 else (*nEnergyBins)++;
       }
     } 
   } /* end of 'j' loop */
   sprintf(msg,"SliceEnergySpectra: Selected %d bins in range (%10.5g-%10.5g) eV with indices (%d-%d).",
     *nEnergyBins,spectra->energy[*eBinLow],spectra->energy[*eBinHigh],*eBinLow,*eBinHigh);
   c_fcecho(msg);
   return(status);
}
