/*
   File:   xstartablelib.c
   Author: W.T. Bridgman, RITSS
   Date:   January 1999

   Description:

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
#include <math.h>

#include "xstartablelib.h"
#include "xstarutillib.h"

#define ERRMSG 2048

/************************************************************
 * Function:                                                *
 *    Display_FITS_ParmTable                                *
 *                                                          *
 * Description:                                             *
 *      Display parameters retrieved from .par file with    *
 *      interpretation.                                     *
 *                                                          *
 *  Input Parameters:                                       *
 *     All values are returned from the call                *
 *  Output Values:                                          *
 *                                                          *
 ************************************************************/
int Display_FITS_ParmTable(Parameter_struct *parms)
{
  int           status=0; /* initialize status code */
  int           curParmNum,curValueNum;
  var_type      type;
  method_type   method;
  char          tempstr[20],msg[ERRMSG],msg2[ERRMSG];

  /* Display Modeling control parameters */

  /* Display physical parameters with variation options */
  for(curParmNum=0;curParmNum<parms->numberOfParameters;curParmNum++) {
    sprintf(msg,"%3d) %-12s: ",curParmNum+1,parms->physical[curParmNum].name);

    /* How does it vary? */
    type=parms->physical[curParmNum].type;
    switch(type) {
    case kConstant:
      sprintf(msg2,"Value is constant = %g",parms->physical[curParmNum].initialValue);
      strcat(msg,msg2);
      c_fcecho(msg);
      break;
    case kAdditive:
      sprintf(msg,"Value is additive, evaluated at 0.0 and %g",
	     parms->physical[curParmNum].initialValue);
      strcat(msg,msg2);
      c_fcecho(msg);
      break;
    case kInterpolated:
      /* Sampling method */
      method=parms->physical[curParmNum].method;
      switch(method) {
      case kLinear:
	strcpy(tempstr,"Linearly");
        break;
      case kLogarithmic:
	strcpy(tempstr,"Logarithmically");
        break;
      default:
        sprintf(msg,"Display_FITS_ParmTable: Illegal Method Type=%d",method);
	c_fcerr(msg);
        c_fcerr("Display_FITS_ParmTable: How the hell did it get to this point?!");
        exit(EXIT_FAILURE);
      } /* end switch 'method' */

      strcat(msg,"Value is interpolated.");
      c_fcecho(msg);
      sprintf(msg,"                   %s sampled at %d points:",
	     tempstr,parms->physical[curParmNum].numberOfValues);
      c_fcecho(msg);
      /* list values of sampled points */
      if(parms->physical[curParmNum].valueList!=(float*)NULL){
	sprintf(msg,"                   ");
	for(curValueNum=0;curValueNum<parms->physical[curParmNum].numberOfValues;curValueNum++) {
	  sprintf(msg2,"%g  ",parms->physical[curParmNum].valueList[curValueNum]);
	  strcat(msg,msg2);
	}
	c_fcecho(msg);
      } else {
	c_fcerr("Display_FITS_ParmTable: No sample table!  Fatal Error!!");
        exit(EXIT_FAILURE);
      }
      break;
    default:
      sprintf(msg,"Display_FITS_ParmTable: Illegal Variable Type=%d",type);
      c_fcerr(msg);
      c_fcerr("Display_FITS_ParmTable: How the hell did it get to this point?!");
      exit(EXIT_FAILURE);
    } /* end switch 'type' */
  } /* end 'curParmNum' loop */

  /* print modeling control parameters */
  c_fcecho("\nModel Control Parameters:");
  sprintf(msg,"Spectrum Type:            %s",parms->spectrumName);
  c_fcecho(msg);
  sprintf(msg,"Spectrum File:            %s",parms->spectrumFile);
  c_fcecho(msg);
  /* spectral units */
  switch(parms->spectrumUnits){
  case 0:
    strcpy(tempstr,"Energy");
    break;
  case 1:
    strcpy(tempstr,"Photons");
    break;
  default:
    sprintf(msg,"Disp_Parms: %d spectrum units is undefined.",
	   parms->spectrumUnits);
    c_fcerr(msg);
  } 
  sprintf(msg,"Spectrum Units:           %s",tempstr);c_fcecho(msg);
  Flag2String(tempstr,parms->redshift);
  sprintf(msg,"Is redshift a parameter?: %s",tempstr);c_fcecho(msg);

  sprintf(msg,"Number of Steps:          %d",parms->numberOfSteps);c_fcecho(msg);
  sprintf(msg,"Number of Iterations:     %d",parms->numberOfIterations);c_fcecho(msg);
  Flag2String(tempstr,parms->writeSwitch);
  sprintf(msg,"Write Switch:             %s",tempstr);c_fcecho(msg);
  Flag2String(tempstr,parms->printSwitch);
  sprintf(msg,"Print Switch:             %s",tempstr);c_fcecho(msg);
  sprintf(msg,"Step Size Choice Switch:  %d",parms->stepSizeChoiceSwitch);c_fcecho(msg);
  sprintf(msg,"Number of Passes:         %d",parms->numberOfPasses);c_fcecho(msg);
  Flag2String(tempstr,parms->constantPressureSwitch);
  sprintf(msg,"Constant Pressure Switch: %s",tempstr);c_fcecho(msg);
  
  sprintf(msg,"Courant multiplier (emult)%9.2f",parms->emult);c_fcecho(msg);
  sprintf(msg,"taumax for Courant step:  %9.2f",parms->taumax);c_fcecho(msg);
  sprintf(msg,"Minimum electron fraction:%9.2f",parms->xeemin);c_fcecho(msg);
  sprintf(msg,"Critical ion abundance:   %9.2f",parms->critf);c_fcecho(msg);
  sprintf(msg,"Turbulent velocity (km/s):%9.2f",parms->vturbi);c_fcecho(msg);
  sprintf(msg,"radius exponent:          %9.2f",parms->radexp);c_fcecho(msg);
  sprintf(msg,"number of energy bins:    %d",parms->ncn2);c_fcecho(msg);
  sprintf(msg,"Model Name:               %s",parms->modelName);c_fcecho(msg);
  sprintf(msg,"Energy Range:             %10.2f - %10.2f eV", 
         parms->energyLow,parms->energyHigh);c_fcecho(msg);
  return(status);
}
/********************************************************
 *  Read_FITS_ParmTable:                                *
 *                                                      *
 *  Abstract:                                           *
 *     Read a FITS file and fill in the parameter       *
 *     table list.                                      *
 *                                                      *
 *  Input Parameters:                                   *
 *     status = status code returned by CFITSIO         *
 *                                                      *
 ********************************************************/
int Read_FITS_ParmTable(fitsfile* fptr, Parameter_struct *parms, int verbose){
   int   status=0;
   int   frow, felem, intnull, j, k;
   int   anynull,typecode;

   /* temporary storage locations */
   char  **tempstr,msg[ERRMSG];
   char  strnull[]=" ";
   float *tempfloat, **value, floatnull;
   long  *tempint, maxcol, nrows, repeat, width;

   frow=1;
   felem=1;
   floatnull=0.0;
   intnull=0;
   anynull=0;
   
   /* Get REDSHIFT from primary HDU */
   if(verbose) c_fcecho("Read_FITS_ParmTable: Moving to PRIMARY extension.");
   if(fits_movabs_hdu(fptr,1,&typecode,&status)) PrintError(status);
   if(fits_read_key(fptr,TLOGICAL,"REDSHIFT",&(parms->redshift),
		    (char*)NULL,&status)) PrintError(status);


   /* move to PARAMETERS HDU */
   if(verbose) c_fcecho("Read_FITS_ParmTable: Moving to PARAMETERS extension.");
   if(fits_movnam_hdu(fptr,BINARY_TBL,"PARAMETERS",0,&status))
           PrintError(status);

   if(fits_get_num_rows(fptr,&nrows,&status)) PrintError(status);
   parms->numberOfParameters=nrows;

   /* load parameter names */
   if(verbose) c_fcecho("Read_FITS_ParmTable: Loading Parameter Names.");
   if(fits_get_coltype(fptr,1,&typecode,&repeat,&width,&status)) 
     PrintError(status);
   /* must allocate memory for EACH STRING in this process */
   tempstr=(char**)malloc(nrows*sizeof(char*));
   for(j=0;j<nrows;j++) tempstr[j]=(char*)malloc((repeat+1)*sizeof(char));
   if(tempstr==(char**)NULL) {
       c_fcerr("Read_FITS_ParmTable: Unable to allocate parameter name strings.  Exiting...");
       exit(EXIT_FAILURE);
   }
   if(verbose) c_fcecho("Read_FITS_ParmTable: Reading Table.");
   if(fits_read_col(fptr,TSTRING,1,frow,felem,nrows,strnull,tempstr,
		    &anynull,&status)) PrintError(status);
   if(verbose) c_fcecho("Read_FITS_ParmTable: Copying Parameter Names.");
   for(j=0;j<nrows;j++) strcpy(parms->physical[j].name,tempstr[j]);

   /* load floating point values */
   if(verbose) c_fcecho("Read_FITS_ParmTable: Loading Floating-point values.");
   tempfloat=(float*)malloc(nrows*sizeof(float));
   if(fits_read_col(fptr,TFLOAT,3,frow,felem,nrows,&floatnull,tempfloat,
		    &anynull,&status)) PrintError(status);
   for(j=0;j<nrows;j++) parms->physical[j].initialValue=tempfloat[j];

   if(fits_read_col(fptr,TFLOAT,4,frow,felem,nrows,&floatnull,tempfloat,
		    &anynull,&status)) PrintError(status);
   for(j=0;j<nrows;j++) parms->physical[j].delta=tempfloat[j];

   if(fits_read_col(fptr,TFLOAT,5,frow,felem,nrows,&floatnull,tempfloat,
		    &anynull,&status)) PrintError(status);
   for(j=0;j<nrows;j++) parms->physical[j].hardMinimum=tempfloat[j];

   if(fits_read_col(fptr,TFLOAT,6,frow,felem,nrows,&floatnull,tempfloat,
		    &anynull,&status)) PrintError(status);
   for(j=0;j<nrows;j++) parms->physical[j].softMinimum=tempfloat[j];

   if(fits_read_col(fptr,TFLOAT,7,frow,felem,nrows,&floatnull,tempfloat,
		    &anynull,&status)) PrintError(status);
   for(j=0;j<nrows;j++) parms->physical[j].softMaximum=tempfloat[j];

   if(fits_read_col(fptr,TFLOAT,8,frow,felem,nrows,&floatnull,tempfloat,
		    &anynull,&status)) PrintError(status);
   for(j=0;j<nrows;j++) parms->physical[j].hardMaximum=tempfloat[j];

   /* load long integer values */
   if(verbose) c_fcecho("Read_FITS_ParmTable: Loading integer values.");
   /* tempint=(long*)malloc(nrows*sizeof(float)) */
   tempint=(long*)malloc(nrows*sizeof(long));
   if(fits_read_col(fptr,TLONG,2,frow,felem,nrows,&intnull,tempint,
		    &anynull,&status)) PrintError(status);
   for(j=0;j<nrows;j++) parms->physical[j].method=(method_type)tempint[j];

   if(fits_read_col(fptr,TLONG,9,frow,felem,nrows,&intnull,tempint,
		    &anynull,&status)) PrintError(status);
   maxcol=0;
   for(j=0;j<nrows;j++) {
     parms->physical[j].numberOfValues=tempint[j];
     /* set parameter types (additive/interpolated) */
     if(tempint[j]>0) { 
       parms->physical[j].type=kInterpolated;
     } else {
       parms->physical[j].type=kAdditive;
     }
     if(tempint[j]>maxcol) maxcol=tempint[j];
   }

   /* load column list */
   if(verbose) c_fcecho("Read_FITS_ParmTable: Loading value lists.");
   value=matrix_alloc(nrows,maxcol);
   for(j=0;j<nrows;j++){
     if(fits_read_col(fptr,TFLOAT,10,j+1,1,maxcol,&floatnull,value[j],
		      &anynull,&status)) PrintError(status);
     if(parms->physical[j].numberOfValues>0){
       parms->physical[j].valueList
	 =(float*)malloc(parms->physical[j].numberOfValues*sizeof(float));
       for(k=0;k<parms->physical[j].numberOfValues;k++)
          parms->physical[j].valueList[k]=value[j][k];
     }
   } /* end of 'j' loop */

   /* now load the fixed parameters */
   if(verbose) c_fcecho("Read_FITS_ParmTable: Loading fixed parameters.");
   /* spectrum name */
   if(fits_read_key(fptr,TSTRING,"SPECTRUM",parms->spectrumName,(char*)NULL,
			&status)) PrintError(status);
   /* spectrum file */
   if(fits_read_key(fptr,TSTRING,"SPECFILE",parms->spectrumFile,(char*)NULL,
			&status)) PrintError(status);
   /* spectrum units */
   if(fits_read_key(fptr,TINT,"SPECUNIT",&(parms->spectrumUnits),(char*)NULL,
			&status)) PrintError(status);
   /* number of steps */
   if(fits_read_key(fptr,TINT,"NSTEPS",&(parms->numberOfSteps),(char*)NULL,
			&status)) PrintError(status);
   /* number of iterations */
   if(fits_read_key(fptr,TINT,"NITER",&(parms->numberOfIterations),(char*)NULL,
			&status)) PrintError(status);
   /* write switch */
   if(fits_read_key(fptr,TINT,"WRITESW",&(parms->writeSwitch),(char*)NULL,
			&status)) PrintError(status);
   /* print switch */
   if(fits_read_key(fptr,TINT,"PRINTSW",&(parms->printSwitch),(char*)NULL,
			&status)) PrintError(status);
   /* step size */
   if(fits_read_key(fptr,TINT,"STEPSIZE",&(parms->stepSizeChoiceSwitch),
		    (char*)NULL,&status)) PrintError(status);
   /* number of passes */
   if(fits_read_key(fptr,TINT,"NPASS",&(parms->numberOfPasses),(char*)NULL,
			&status)) PrintError(status);
   /* constant pressure switch */
   if(fits_read_key(fptr,TINT,"PRESSSW",&(parms->constantPressureSwitch),
		    (char*)NULL,&status)) PrintError(status);
   /* Courant multiplier */
   if(fits_read_key(fptr,TFLOAT,"EMULT",&(parms->emult),
		    (char*)NULL,&status)) PrintError(status);
   /* tau max for Courant step */
   if(fits_read_key(fptr,TFLOAT,"TAUMAX",&(parms->taumax),
		    (char*)NULL,&status)) PrintError(status);
   /* minimum electron fraction*/
   if(fits_read_key(fptr,TFLOAT,"XEEMIN",&(parms->xeemin),
		    (char*)NULL,&status)) PrintError(status);
   /* critical ion abundance */
   if(fits_read_key(fptr,TFLOAT,"CRITF",&(parms->critf),
		    (char*)NULL,&status)) PrintError(status);
   /* turbulent velocity*/
   if(fits_read_key(fptr,TFLOAT,"VTURBI",&(parms->vturbi),
		    (char*)NULL,&status)) PrintError(status);
   /* radius exponent*/
   if(fits_read_key(fptr,TFLOAT,"RADEXP",&(parms->radexp),
		    (char*)NULL,&status)) PrintError(status);
   /* number of energy bins*/
   if(fits_read_key(fptr,TINT,"NCN2",&(parms->ncn2),
		    (char*)NULL,&status)) PrintError(status);
   /* model name */
   if(fits_read_key(fptr,TSTRING,"MODELNAM",parms->modelName,(char*)NULL,
			&status)) PrintError(status);
   /* energy range of interest */
   if(fits_read_key(fptr,TFLOAT,"ELOW",&(parms->energyLow),
		    (char*)NULL,&status)) PrintError(status);
   if(fits_read_key(fptr,TFLOAT,"EHIGH",&(parms->energyHigh),
		    (char*)NULL,&status)) PrintError(status);

   if(verbose) c_fcecho("Read_FITS_ParmTable: Done loading control parameters.");
   /* free up dynamically allocated blocks */
   free(tempint);
   free(tempfloat);

   return(status);
}

/********************************************************
 *  Write_FITS_Energies:                                *
 *                                                      *
 *  Abstract:                                           *
 *     Write an ENERGIES extension in accordance with   *
 *     OGIP/92-009                                      *
 *                                                      *
 *  Input Parameters:                                   *
 *     foutptr = pointer to FITS file open for writing  *
 *     nbins   = number of energy bins                  *
 *     energyLow= low edge of energy bin (keV)          *
 *     energyHigh=high edge of energy bin (keV)         *
 *                                                      *
 *  Returns:                                            *
 *     status code                                      *
 ********************************************************/
int Write_FITS_Energies(fitsfile *foutptr,long nbins, 
			float *energyLow, float *energyHigh, int verbose)
{
  int          status=0;
  const long   kTfields=2;
  char         extname[]="ENERGIES";
  char         *ttype[]={"ENERG_LO","ENERG_HI"};
  char         *tform[]={"E","E"};
  char         *tunit[]={"keV","keV"};
  char         msg[ERRMSG];

  /* Build the table header */
  if(verbose) c_fcecho("Write_FITS_Energies: Write ENERGIES extension.");
  if(fits_create_tbl(foutptr,BINARY_TBL,nbins,kTfields,ttype,
		     tform,tunit,extname,&status)) PrintError(status);

  /* write columns of data */
  if(verbose) c_fcecho("Write_FITS_Energies: Write Column 1.");
  if(fits_write_col(foutptr,TFLOAT,1,1,1,nbins,energyLow,
		    &status)) PrintError(status);
  if(verbose) c_fcecho("Write_FITS_Energies: Write Column 2.");
  if(fits_write_col(foutptr,TFLOAT,2,1,1,nbins,energyHigh,
		    &status)) PrintError(status);

  /* OGIP required keywords */
  if(fits_write_key(foutptr,TSTRING,"HDUCLAS1","XSPEC TABLE MODEL",
	"model spectra for XSPEC",&status)) PrintError(status);
  if(fits_write_key(foutptr,TSTRING,"HDUCLAS2","ENERGIES",
	"extension containing energy bin info",&status)) PrintError(status);
  if(fits_write_key(foutptr,TSTRING,"HDUVERS1","1.0.0",
	"version of format",&status)) PrintError(status);

  return(status);
}

/********************************************************
 *  Create_FITS_Spectra_Header:                         *
 *                                                      *
 *  Abstract:                                           *
 *     Build FITS Binary table header for the ATABLE &  *
 *     MTABLE SPECTRA extension                         *
 *                                                      *
 *  Input Parameters:                                   *
 *     foutptr = pointer to FITS file open for writing  *
 *               MUST BE AT THE SPECTRA HDU!            *
 *     ncombos  = number of interpolated combinations   *
 *     nintparm = number of interpolated parameters     *
 *     naddparm = number of additional parameters       *
 *     nspecchan= number of spectral channels           *
 *  Returns:                                            *
 *     status code                                      *
 ********************************************************/
int Create_FITS_Spectra_Header(fitsfile *foutptr, long ncombos, 
   long nintparm, long naddparm, long nspecchan, char* units, int verbose) {
   int   status=0;
   char  extname[]="SPECTRA";
   /* char  kUnits[]="photons/cm^2/s";*/ /* let strings point to this */
   char  tempstr[20],tempstr2[20],msg[ERRMSG];
   char  **ttype, **tform, **tunit;
   long  j, zero=0;
   int   tfields;

   if(verbose)c_fcecho("Create_FITS_Spectra_Header: Creating TABLE Header...");
/* allocate space for column headers */
   ttype=(char**)malloc((naddparm+2)*sizeof(char*));
   tform=(char**)malloc((naddparm+2)*sizeof(char*));
   tunit=(char**)malloc((naddparm+2)*sizeof(char*));
   if((ttype==(char**)NULL)||(tform==(char**)NULL)||(tunit==(char**)NULL)){
     c_fcerr("Create_FITS_Spectra_Header: Failure in allocating column headers.  Exiting...");
     exit(EXIT_FAILURE);
   }
   /* setup initial column headers */
   ttype[0]=(char*)malloc(1+strlen("PARAMVAL"));
   strcpy(ttype[0],"PARAMVAL");
   sprintf(tempstr,"%dE",nintparm);
   tform[0]=(char*)malloc(1+strlen(tempstr));
   strcpy(tform[0],tempstr);
   tunit[0]=(char*)malloc(1+strlen(" "));
   strcpy(tunit[0]," ");

   ttype[1]=(char*)malloc(1+strlen("INTPSPEC"));
   strcpy(ttype[1],"INTPSPEC");
   sprintf(tempstr2,"%dE",nspecchan); /* tempstr2 will be used a lot so keep */
   tform[1]=tempstr2;                 /* it and point to it */
   tunit[1]=units;

   /* now build the headers for additional spectra */
   for(j=2;j<naddparm+2;j++){
      sprintf(tempstr,"ADDSP%03d",j-1);
      ttype[j]=(char*)malloc(1+strlen(tempstr));
      strcpy(ttype[j],tempstr);
      tform[j]=tempstr2;  /* just point to the value constructed earlier */
      tunit[j]=units; /* just point to this constant */
   } /* end of 'j' loop */
   tfields=naddparm+2;

   /* now write the header */
   if(fits_create_tbl(foutptr,BINARY_TBL,ncombos,tfields,ttype,tform,
      tunit,extname,&status)) PrintError(status);

   /* install our bookkeeping tag */
   if(fits_write_key(foutptr,TLONG,"LASTSPEC",&zero,
      "last spectrum written to table",&status)) PrintError(status);

   /* OGIP required keywords */
   if(fits_write_key(foutptr,TSTRING,"HDUCLAS1","XSPEC TABLE MODEL",
	"model spectra for XSPEC",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUCLAS2","MODEL SPECTRA",
	"extension containing model spectra",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUVERS1","1.0.0",
	"version of format",&status)) PrintError(status);

   return(status);
}
/********************************************************
 *  Write_FITS_Spectra:                                 *
 *                                                      *
 *  Abstract:                                           *
 *     Write a spectrum to the SPECTRA extension in     *
 *     accordance with OGIP/92-009                      *
 *     Also checks that the LASTSPEC value in the       *
 *     extension header is equal to loopcontrol-1 to    *
 *     better check spectra placement.                  *
 *                                                      *
 *  Input Parameters:                                   *
 *     foutptr = pointer to FITS file open for writing  *
 *               MUST BE AT THE SPECTRA HDU!            *
 *     row     = FITS row number                        *
 *     column  = 0 = interpolated model spectrum        *
 *               >0 = additional parameter model spec   *
 *     nbins   = number of energy bins                  *
 *     spectrum= array of nbins floats                  *
 *     iparms  = number of interpolated parameters      *
 *     parvals = array of iparms interpolated values    *
 *                                                      *
 *  Returns:                                            *
 *     status code                                      *
 ********************************************************/
int Write_FITS_Spectra(fitsfile *foutptr, long loopcontrol, long row, 
		       long column,
		       long nbins, float *spectrum, 
		       int iparms, float *parmvals,int verbose)
{
  int status=0;
  long lastspec;
  char comment[FLEN_FILENAME],msg[ERRMSG];

  /* make sure we're updating the correct HDU */
  if(fits_movnam_hdu(foutptr,BINARY_TBL,"SPECTRA",0,&status)) 
    PrintError(status);

  /* verify that we're on the correct spectrum */
  if(fits_read_key(foutptr,TLONG,"LASTSPEC",&lastspec,comment,&status)) 
    PrintError(status);
  if(lastspec+1==loopcontrol) {
    if(column==0) {
      /* Write INTPSPEC spectral record */
      if(verbose) c_fcecho("Write_FITS_Spectra: Write INTPSPEC Column 1 & 2.");
      if(fits_write_col(foutptr,TFLOAT,1,row,1,iparms,parmvals,&status)) 
        PrintError(status);
      if(fits_write_col(foutptr,TFLOAT,2,row,1,nbins,spectrum,&status)) 
        PrintError(status);
    } else {
      /* Write ADDSPxxx spectral record */
      /* might want to compare the parvals array to the one first written */
      if(verbose) {
	sprintf(msg,"Write_FITS_Spectra: Write ADDSP Column %d.\n",1+column);
	c_fcecho(msg);
      }
      if(fits_write_col(foutptr,TFLOAT,2+column,row,1,nbins,spectrum,&status)) 
        PrintError(status);
    }

    /* Update the LASTSPEC keyword */
    lastspec=loopcontrol;
    if(fits_update_key(foutptr,TLONG,"LASTSPEC",&loopcontrol,
	"Last spectrum written",&status)) PrintError(status);
  } else {
    sprintf(msg,"Write_FITS_Spectra: Error! Loopcontrol=%d when it should have been %d.\n",loopcontrol,lastspec+1);
    c_fcerr(msg);
    status=SPECTRA_WRITE_ERROR;
  }

  return(status);
}

