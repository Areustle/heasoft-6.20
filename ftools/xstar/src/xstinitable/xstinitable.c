/*
   File:   xstinitable.c
   Author: W.T. Bridgman, RITSS
   Date:   January 1999

   Description:
   Routine to setup parameter files for a multiple xstar run.  The output
   from this program will then be sent to other routines to complete the
   process.
-----------------------------------------------------------------------
   Code Outline:

   START
   Read Parameter File
   Display Parameter File
   Generate FITS file from parameter file
   Generate Text file containing XSTAR calls
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
#include "xstinitable.h"
#include "cftools.h"

int kNumPhysicalParms=NUMPHYSICALPARMS; /* enable type enforcement */


static void safe_fcecho(char* msg, int loop_num, int loop_modulus, char *init)
{
   /* Prevent fcecho buf overflow by printing only short lines when iterating */
   if (loop_num && loop_num % loop_modulus == 0) {
	c_fcecho(msg);
	strcpy(msg, init);
   }
}

void xstinitable(void) {
   char  progname[]="xstinitable";
   char  version[]="1.0";
   char  fitsOutput[]="!xstinitable.fits"; /* output FITS file name */
   /* '!' in the above file name enables the new 'delete if already 
      exists' option in cfitsio                                     */
   char  textOutput[]="xstinitable.lis"; /* output text file name */

   /* temporary and work variables */
   int           status  = 0;
   int           verbose = 0;
   int           j;
   char          msg[ERRMSG],s[ERRMSG]="\0";

   Parameter_struct inputParmlist;

   sprintf(msg,"%s v%s",progname,version);c_fcecho(msg);
   sprintf(msg,"Compiled: %s %s",__DATE__,__TIME__);c_fcecho(msg);
  



   if(getenv("XSTARDEBUG")!=(char*)NULL) {
     strcpy(s,getenv("XSTARDEBUG"));printf("environment:%s\n",s);
     if(isdigit(s[0])) verbose=atoi(s);/* set for verbose logging? */
   }

   /************* grab parameters via XPI ******************/
   if(verbose) c_fcecho("xstinitable: Retrieving Input Parameters");
   status=Get_Parms(&inputParmlist,verbose);
   if(status) { 
       sprintf(msg,"Get_Parms: Data Entry Error.  Return Code=%d",status);
       c_fcecho(msg);
       exit(EXIT_FAILURE);
   }

   /* display parameters */
   if(verbose) c_fcecho("xstinitable: Displaying Input Parameters");
   status=Display_Parms(&inputParmlist); 
   if(status){
     sprintf(msg,"Display_Parms return code = %d",status);
     c_fcecho(msg);
   }


   /***************** Write the output FITS file ******************/
   if(verbose) c_fcecho("xstinitable: Write the FITS parameter file");
   status=Write_FITS_ParmTable(fitsOutput,&inputParmlist,verbose);
   if(status){
     sprintf(msg,"Write_FITS_ParmTable return code = %d",status);
     c_fcecho(msg);
   }

   /**********************************************************/
   /* build the text file of containing all the possible 
      permutations of the xstar parameter lists */
   if(verbose) c_fcecho("xstinitable: Write the text parameter file");
   status=Write_Text_Parmlist(textOutput,&inputParmlist,verbose);
   if(status){
     sprintf(msg,"Write_Text_Parmtable return code = %d",status);
     c_fcecho(msg);
   }
   
   /********************* Big cleanup!! **********************/
   /* release dynamically allocated sample lists */
   if(verbose) c_fcecho("xstinitable: Memory cleanup");
   for(j=0;j<inputParmlist.numberOfParameters;j++) {
     if(inputParmlist.physical[j].valueList!=(float*)NULL) 
       free(inputParmlist.physical[j].valueList);
   }
   if(verbose) c_fcecho("xstinitable: Memory cleanup complete");

   /* Sentry value to signal successful completion 
      to xstar2xspec script.
      ALTER AT YOUR OWN RISK!!!!!!!!!! */
   c_fcecho("xstinitable: Successful Completion");
}

/************************************************************
 * Function:                                                *
 *    Get_Parms                                             *
 *                                                          *
 * Description:                                             *
 *      gets the parameters for the task xstinitable using  *
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
int Get_Parms(Parameter_struct *parms,int verbose)
 {
     
   /*  BufLen_2 is required by XPI and it has to be one less than the
    length of the character variables that Uclgst pulls
    */
     int        BufLen_2=FLEN_FILENAME-1;  /* required by cfortran.h*/
     char       tempstr[FLEN_FILENAME];    /* required by cfortran.h*/
     char       *parmstr; /* pointer to string buffer */
     int        tempint=0, specFileFlag=0;
     float      tempfloat=0.0;

     int        curParmNum, curValueNum, numberOfValues, status=0;
     float      minimum, maximum, stepsize;
     char       msg[ERRMSG],msg2[ERRMSG];

     char       *parmnames[]={"trad","cfrac","temperature","pressure",
			     "density","rlrad38","column","rlogxi",
                             "habund","heabund","liabund","beabund",
                             "babund","cabund","nabund","oabund",
                             "fabund","neabund","naabund","mgabund",
                             "alabund","siabund","pabund","sabund",
                             "clabund","arabund","kabund",
                             "caabund","scabund","tiabund","vabund",
                             "crabund","mnabund","feabund","coabund",
                             "niabund","cuabund","znabund"};

     /* set some reasonable defaults for safety */
     strcpy(parms->spectrumFile,"spect.dat");
     parms->spectrumUnits=0;

     /* Input spectrum name */
     Uclgst("spectrum", tempstr, &status);
     strcpy(parms->spectrumName,tempstr);
     if(parms->spectrumName[0] =='\0'){
        status=1;
        c_fcerr("\nGet_Parms: Spectrum Name is required.");
     }

     /* if 'file' spectrum specified, get the apppropriate info */
     printf("%s %s %d\n",parms->spectrumName,"file",strcmp(parms->spectrumName,"file"));
     if(strcmp(parms->spectrumName,"file")==0) {
       /* Input spectrum file name */
       Uclgst("spectrum_file", tempstr, &status);
       strcpy(parms->spectrumFile,tempstr);
       if(parms->spectrumFile[0] =='\0'){
	 status=1;
	 c_fcerr("\nGet_Parms: Spectrum file is required .");
       }

       /* input spectrum type (0=energy, 1=photons) */
       Uclgsi("spectun",&tempint,&status);
       if(status) {
	 sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "spectun");
	 c_fcerr(msg);
       }
       sprintf(msg,"\n spectun= %d",tempint);
       parms->spectrumUnits=tempint;
       specFileFlag=1; /* flag file specification so we can skip reading trad */
     } /* end of 'file' option */

     /* dynamically allocating string buffer this way per Peter Wilson's 
        suggestion.  This method will bypass some string manipulation
        logic in the XPI wrappers that is rarely used in other FTOOLS. */
     parmstr=(char*)malloc(FLEN_FILENAME*sizeof(char));

     parms->numberOfParameters=NUMPHYSICALPARMS;
     if(verbose){
       sprintf(msg,"Get_Parms: File length buffer=%d",FLEN_FILENAME);
       c_fcecho(msg);
     }

     /* loop over the physical parameters list */
     for(curParmNum=0;curParmNum<parms->numberOfParameters;curParmNum++){

        if(verbose){
	  sprintf(msg,"Get_Parms: Reading Parameter Set #%d, %s",
			  curParmNum+1,parmnames[curParmNum]);
	  c_fcecho(msg);
	}

	/* assign the sequence number for use downstream */
        parms->physical[curParmNum].XSTARsequenceNumber=curParmNum+1;

        /* setup parameter name */
        strcpy(parms->physical[curParmNum].name,parmnames[curParmNum]);

        /* if spectrum file is specified, then skip entry of 'trad', assign some
	   reasonable defaults to keep latter parts of the code happy, and move
	   on to the next iteration of the loop */ 
	if((strcmp(parmnames[curParmNum],"trad")==0)&&(specFileFlag==1)) {
          if(verbose)c_fcecho("Get_Parms: Parameter is ignored.");
	  parms->physical[curParmNum].initialValue=0.0;
	  parms->physical[curParmNum].type=kConstant; /* dummy entry */
          parms->physical[curParmNum].method=kLinear; /* dummy entry */
          parms->physical[curParmNum].delta=-1.0;
	  parms->physical[curParmNum].softMinimum=0.0;
	  parms->physical[curParmNum].softMaximum=0.0;
	  parms->physical[curParmNum].numberOfValues=0;
          parms->physical[curParmNum].valueList=(float*)NULL;
	  continue;
	}

        /* Get initial value */
        strcpy(parmstr,parmnames[curParmNum]);
        if(verbose){
	  sprintf(msg,"Get_Parms: Reading Parameter %s...",parmstr);
	  c_fcecho(msg);
	}
	Uclgsr(parmstr, &tempfloat, &status); 
        if(status) {
	  sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,parmstr);
	  c_fcecho(msg);
	}
        parms->physical[curParmNum].initialValue=tempfloat;

        /* Get parameter type */
        strcpy(parmstr,parmnames[curParmNum]);
        strcat(parmstr,"typ");
        if(verbose){
	  sprintf(msg,"Get_Parms: Reading Parameter %s...",parmstr);
	  c_fcecho(msg);
	}
        Uclgsi(parmstr,&tempint,&status);
        if(status) {
	  sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,parmstr);
	  c_fcecho(msg);
	}
        parms->physical[curParmNum].type=(var_type)tempint;
	
        /* process parameter type options */
        switch(parms->physical[curParmNum].type){
	case kConstant:
          /* validate non-varying parameters */
          if(verbose)c_fcecho("Get_Parms: Parameter is constant.");
          parms->physical[curParmNum].method=kLinear; /* dummy entry */
          parms->physical[curParmNum].delta=-1.0;
	  parms->physical[curParmNum].softMinimum=0.0;
	  parms->physical[curParmNum].softMaximum=0.0;
	  parms->physical[curParmNum].numberOfValues=0;
          parms->physical[curParmNum].valueList=(float*)NULL;
	  break;
	case kAdditive:
	  /* validate additive parameters */
          if(verbose)c_fcecho("Get_Parms: Parameter is additive.");
	  parms->physical[curParmNum].softMaximum
	    =parms->physical[curParmNum].initialValue; /* set soft max to initial value */
          parms->physical[curParmNum].method=kLinear; /* dummy entry */
          parms->physical[curParmNum].delta=-1.0;
	  parms->physical[curParmNum].softMinimum=0.0;
	  parms->physical[curParmNum].numberOfValues=0;
          parms->physical[curParmNum].valueList=(float*)NULL;
	  break;
	case kInterpolated:
          /* validate interpolated parameters */
          /* get interpolation type */
          if(verbose)c_fcecho("Get_Parms: Parameter is interpolated...");
          strcpy(parmstr,parmnames[curParmNum]);
          strcat(parmstr,"int");
          if(verbose){
	    sprintf(msg,"Get_Parms: Reading Parameter %s...",parmstr);
	    c_fcecho(msg);
	  }
          Uclgsi(parmstr,&tempint,&status);
          if(status) {
	    sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,parmstr);
	    c_fcecho(msg);
	  }
          parms->physical[curParmNum].method=(method_type)tempint;
	  
	  /* set soft maximum to initial value */
	  parms->physical[curParmNum].softMaximum
	    =parms->physical[curParmNum].initialValue;
          maximum=parms->physical[curParmNum].softMaximum;
          /* get value of soft minimum */
          strcpy(parmstr,parmnames[curParmNum]);
          strcat(parmstr,"sof");
          if(verbose){
	    sprintf(msg,"Get_Parms: Reading Parameter %s...",parmstr);
	    c_fcecho(msg);
	  }
          Uclgsr(parmstr,&minimum,&status);
          if(status) {
	    sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,parmstr);
	    c_fcecho(msg);
	  }
          parms->physical[curParmNum].softMinimum=minimum;
	 
	  /* get number of values for interpolation */
          strcpy(parmstr,parmnames[curParmNum]);
          strcat(parmstr,"nst");
          if(verbose){
	    sprintf(msg,"Get_Parms: Reading Parameter %s...",parmstr);
	    c_fcecho(msg);
	  }
          Uclgsi(parmstr,&numberOfValues,&status);
          if(status) {
	    sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, parmstr);
	    c_fcecho(msg);
	  }
          parms->physical[curParmNum].numberOfValues=numberOfValues;

	  /* now build sampling table */
	  parms->physical[curParmNum].valueList=(float*)calloc(numberOfValues,sizeof(float));
          if(parms->physical[curParmNum].valueList==(float*)NULL) {
	      c_fcecho("\nGet_Parms: Failure on allocating Sampling Table.");
	      exit(EXIT_FAILURE);
	  }
	  if(verbose){
	    sprintf(msg,"Get_Parms: Min=%g, Max=%g, Steps=%d",
			    minimum, maximum,numberOfValues);
	    c_fcecho(msg);
	  }
	  switch(parms->physical[curParmNum].method) {
	  case kLinear:
	    /* perform linear sampling */
            if(verbose)c_fcecho("Get_Parms: Performing Linear Sampling.");
            stepsize=(maximum-minimum)/(float)(numberOfValues-1);
	    msg[0]='\0'; /* clear output string */
            for(curValueNum=0;curValueNum<numberOfValues;curValueNum++){
	      parms->physical[curParmNum].valueList[curValueNum]=minimum+curValueNum*stepsize;
              if(verbose){
		sprintf(msg2," %g",parms->physical[curParmNum].valueList[curValueNum]);
		strcat(msg,msg2);
		safe_fcecho(msg, curValueNum, 5, "");
	      }
	    }
	    if(verbose && msg[0] != '\0')
		c_fcecho(msg);
            /* set a default value for delta */
            parms->physical[curParmNum].delta=stepsize;
            /* reset set initial value to arithmetic mean */
            parms->physical[curParmNum].initialValue=(maximum+minimum)/2.0;
	    break;
	  case kLogarithmic:
	    /* perform logarithmic sampling */
            if(verbose)c_fcecho("Get_Parms: Performing Logarithmic Sampling.");
            if((minimum<=0.0)||(maximum<=0.0)){
	      c_fcecho("\nGet_Parms: Minimum and Maximum must be greater zero for logarithmic sampling!");
	      exit(EXIT_FAILURE);
	    }
            stepsize=(log10(maximum)-log10(minimum))/(float)(numberOfValues-1);
	    msg[0]='\0'; /* initialize output string */
            for(curValueNum=0;curValueNum<numberOfValues;curValueNum++){
	      parms->physical[curParmNum].valueList[curValueNum]=minimum*pow(10.0,(float)curValueNum*stepsize);
              if(verbose){
		sprintf(msg2," %g",parms->physical[curParmNum].valueList[curValueNum]);
		strcat(msg,msg2);
		safe_fcecho(msg, curValueNum, 5, "");
	      }
	    }
	    if(verbose && msg[0] != '\0')
		c_fcecho(msg);
            /* set a default value for delta */
            parms->physical[curParmNum].delta
	      =parms->physical[curParmNum].valueList[1]-parms->physical[curParmNum].valueList[0];
            /* reset set initial value to geometric mean */
            parms->physical[curParmNum].initialValue=(float)sqrt((double)maximum*(double)minimum);
	    break;
	  default:
              sprintf(msg,"\nGet_Parms: Undefined method type %d",
		   parms->physical[curParmNum].method);
	      c_fcerr(msg);

	  } /* end of switch 'method' */
	  break;

	default:
            sprintf(msg,"\nGet_Parms: Undefined parameter type %d",
		   parms->physical[curParmNum].type);
	    c_fcerr(msg);
	} /* end of switch 'type' */

	/* set default hard maxima & minima */
        parms->physical[curParmNum].hardMinimum=parms->physical[curParmNum].softMinimum;
	parms->physical[curParmNum].hardMaximum=parms->physical[curParmNum].softMaximum;
     } /* end of curParmNum loop */

     free(parmstr);/* free-up the buffer */

     /* Now load the additional parameters */
     /* input redshift flag (0=no, 1=yes) */
     Uclgsi("redshift",&tempint,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "redshift");
       c_fcerr(msg);
     }
     parms->redshift=tempint;

     /* input number of steps */
     Uclgsi("nsteps",&tempint,&status);
     if(status){
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "nsteps");
       c_fcerr(msg);
     }
     parms->numberOfSteps=tempint;
    
     /* input number of iterations */
     Uclgsi("niter",&tempint,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "niter");
       c_fcecho(msg);
     }
     parms->numberOfIterations=tempint;

     /* input write switch (0=no, 1=yes) */
     Uclgsi("lwrite",&tempint,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "lwrite");
       c_fcerr(msg);
     }
     parms->writeSwitch=tempint;

     /* input print switch (0=no, 1=yes) */
     Uclgsi("lprint",&tempint,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "lprint");
       c_fcerr(msg);
     }
     parms->printSwitch=tempint;

     /* input step size choice*/
     Uclgsi("lstep",&tempint,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "lstep");
       c_fcerr(msg);
     }
     parms->stepSizeChoiceSwitch=tempint;

     /* input number of passes */
     Uclgsi("npass",&tempint,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "npass");
       c_fcerr(msg);
     }
     parms->numberOfPasses=tempint;
     
     /* input constant pressure switch*/
     Uclgsi("lcpres",&tempint,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status, "lcpress");
       c_fcerr(msg);
     }
     parms->constantPressureSwitch=tempint;

     /* Input model name */
     Uclgst("modelname", tempstr, &status);
     strcpy(parms->modelName,tempstr);
     if(parms->modelName[0] =='\0'){
        status=1;
        c_fcerr("\nGet_Parms: Model Name is required ");
     }

     /* Input courant multiplier */
     Uclgsr("emult", &tempfloat, &status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"emult");
       c_fcerr(msg);
     }
     parms->emult=tempfloat;

     /* Input tau max for courant step */
     Uclgsr("taumax", &tempfloat, &status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"taumax");
       c_fcerr(msg);
     }
     parms->taumax=tempfloat;

     /* Input minimum elctron fraction */
     Uclgsr("xeemin", &tempfloat, &status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"xeemin");
       c_fcerr(msg);
     }
     parms->xeemin=tempfloat;

     /* Input min ion fraction for multilevel calculation */
     Uclgsr("critf", &tempfloat, &status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"critf");
       c_fcerr(msg);
     }
     parms->critf=tempfloat;

     /* Input turbulent velocity */
     Uclgsr("vturbi", &tempfloat, &status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"vturbi");
       c_fcerr(msg);
     }
     parms->vturbi=tempfloat;

     /* Input radius exponent*/
     Uclgsr("radexp", &tempfloat, &status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"radexp");
       c_fcerr(msg);
     }
     parms->radexp=tempfloat;

     /* Input number of energy bins*/
     Uclgsi("ncn2", &tempint, &status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"ncn2");
       c_fcerr(msg);
     }
     parms->ncn2=tempint;

     /* input energy low range */
     Uclgsr("elow",&tempfloat,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"elow");
       c_fcerr(msg);
     }
     parms->energyLow=tempfloat;

     /* input energy high range */
     Uclgsr("ehigh",&tempfloat,&status);
     if(status) {
       sprintf(msg,"\nGet_Parms: Error %d retrieving %s.",status,"elow");
       c_fcerr(msg);
     }
     parms->energyHigh=tempfloat;
     /* range check on energy limits */
     if(parms->energyHigh<=parms->energyLow) {
       status=2;
       sprintf(msg,"\nGet_Parms: ENERGYLOW=%10.1f must be less than ENERGYHIGH=%10.1f.",parms->energyLow,parms->energyHigh);
       c_fcerr(msg);
     }

     return(status);
 }

/************************************************************
 * Function:                                                *
 *    Display_Parms                                         *
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
int Display_Parms(Parameter_struct *parms)
{
  int           status=0; /* initialize status code */
  int           curParmNum,curValueNum;
  var_type      type;
  method_type   method;
  char          tempstr[20],msg[ERRMSG],msg2[ERRMSG];

  /* Display Modeling control parameters */
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
  case 2:
    strcpy(tempstr,"log");
    break;
  default:
    sprintf(msg,"Disp_Parms: %d spectrum units is undefined.",
	   parms->spectrumUnits);
    c_fcerr(msg);
  } 
  sprintf(msg,"Spectrum Units:           %s",tempstr);c_fcecho(msg);

  /* Display physical parameters with variation options */
  for(curParmNum=0;curParmNum<parms->numberOfParameters;curParmNum++) {
    sprintf(msg,"%3d) %-12s: ",parms->physical[curParmNum].XSTARsequenceNumber,
	   parms->physical[curParmNum].name);

    /* How does it vary? */
    type=parms->physical[curParmNum].type;
    switch(type) {
    case kConstant:
      sprintf(msg2,"Value is constant = %g",parms->physical[curParmNum].initialValue);
      strcat(msg,msg2);
      c_fcecho(msg);
      break;
    case kAdditive:
      sprintf(msg2,"Value is additive, evaluated at 0.0 and %g",
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
        sprintf(msg,"Display_Parms: Illegal Method Type=%d",method);
	c_fcerr(msg);
        c_fcerr("Display_Parms: How the hell did it get to this point?!");
        exit(EXIT_FAILURE);
      } /* end switch 'method' */

      strcat(msg,"Value is interpolated.");
      c_fcecho(msg);
      sprintf(msg,"                   %s sampled at %d points:",
	     tempstr,parms->physical[curParmNum].numberOfValues);
      c_fcecho(msg);

      /* list values of sampled points */
      if(parms->physical[curParmNum].valueList!=(float*)NULL){
	strcpy(msg,"                   ");
	for(curValueNum=0;curValueNum<parms->physical[curParmNum].numberOfValues;curValueNum++) {
	  sprintf(msg2,"%g  ",parms->physical[curParmNum].valueList[curValueNum]);

	  strcat(msg,msg2);
	  safe_fcecho(msg, curValueNum, 5, "                   ");
	}
	if (msg[0] != ' ')
	   c_fcecho(msg);
      } else {
	c_fcerr("Display_Parms: No sample table!  Fatal Error!!");
        exit(EXIT_FAILURE);
      }
      break;
    default:
      sprintf(msg,"Display_Parms: Illegal Variable Type=%d",type);
      c_fcerr(msg);
      c_fcerr("Display_Parms: How the hell did it get to this point?!");
      exit(EXIT_FAILURE);
    } /* end switch 'type' */
  } /* end 'curParmNum' loop */


  /* print modeling control parameters */
  c_fcecho("\nModel Control Parameters:");
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
  
  sprintf(msg,"Model Name:               %s",parms->modelName);c_fcecho(msg);

  sprintf(msg,"Courant multiplier (emult)%g",parms->emult);c_fcecho(msg);
  sprintf(msg,"taumax for Courant step:  %g",parms->taumax);c_fcecho(msg);
  sprintf(msg,"Minimum electron fraction:%g",parms->xeemin);c_fcecho(msg);
  sprintf(msg,"Critical ion abundance:   %g",parms->critf);c_fcecho(msg);
  sprintf(msg,"Turbulent velocity (km/s):%g",parms->vturbi);c_fcecho(msg);
  sprintf(msg,"radius exponent:          %g",parms->radexp);c_fcecho(msg);
  sprintf(msg,"number of energy bins:    %d",parms->ncn2);c_fcecho(msg);
  sprintf(msg,"Energy Band Low End (eV): %9.2f",parms->energyLow);c_fcecho(msg);
  sprintf(msg,"Energy Band High End (eV):%9.2f",parms->energyHigh);c_fcecho(msg);

  return(status);
}
/********************************************************
 *  Write_FITS_ParmTable:                               *
 *                                                      *
 *  Abstract:                                           *
 *     Write a FITS file with the parameter table list  *
 *                                                      *
 *  Input Parameters:                                   *
 *     status = status code returned by CFITSIO         *
 *                                                      *
 ********************************************************/
int Write_FITS_ParmTable(char* fitsFile, Parameter_struct *parms, int verbose){
   int status=0;

   /* FITS output parameters */
   const int     kTfields=10;
   fitsfile      *foutptr;
   int           simple, bitpix, naxis, extend, j, i;
   char          tempstr[20], msg[ERRMSG];
   long          naxes[2], pcount, gcount;
   long          naxis2, maxNumberOfValues;
   int           addptr, intptr, naddparms, ninterparms;
   char          extname[]="PARAMETERS";
   char          *ttype[]={"NAME","METHOD","INITIAL","DELTA",
                            "MINIMUM","BOTTOM","TOP","MAXIMUM",
                            "NUMBVALS","VALUE"};
   char          *tform[]={"12A","J","E","E","E","E","E","E","J","xxxxE"};
   char          *tunit[]={"","","","","","","","","",""};
   char          *name[NUMPHYSICALPARMS];
   long          method[NUMPHYSICALPARMS], numbvals[NUMPHYSICALPARMS];
   float         initial[NUMPHYSICALPARMS], delta[NUMPHYSICALPARMS];
   float         minimum[NUMPHYSICALPARMS], bottom[NUMPHYSICALPARMS];
   float         top[NUMPHYSICALPARMS], maximum[NUMPHYSICALPARMS];
   float         **value;

   /* create the output FITS file */
   if (fits_create_file(&foutptr, fitsFile, &status)) 
        PrintError(status);

   if(verbose) c_fcecho("Write_FITS_ParmTable: Write the Primary FITS header...");
   simple=1; /* simple=true */
   bitpix=16;
   naxis=0;
   naxes[0]=0;
   naxes[1]=0;
   extend=1; /* extend=true */
   pcount=0;
   gcount=1;
   if(fits_write_grphdr(foutptr,simple,bitpix,naxis,naxes,pcount,gcount,
       extend,&status)) PrintError(status);

   if(fits_write_date(foutptr,&status)) PrintError(status); /* write date */

   /* Include MODLNAME, MODLUNIT, REDSHIFT, ADDMODEL keywords? */
   /* truncate to 12 characters for XSPEC limit */
   strncpy(tempstr,parms->modelName,12);
   tempstr[12]='\0'; /* make sure it's NULL at the end */
   if(fits_write_key(foutptr,TSTRING,"MODLNAME",tempstr,
	"first 12 characters of model name",&status)) PrintError(status);
   /* set units */
   if(parms->spectrumUnits==1) strcpy(tempstr,"photons/cm**2/s");
   else strcpy(tempstr,"ergs/cm**2/s");
   if(fits_write_key(foutptr,TSTRING,"MODLUNIT",tempstr,
        "model units",&status)) PrintError(status);
   /* redshift option */
   if(fits_write_key(foutptr,TLOGICAL,"REDSHIFT",&(parms->redshift),
        "Is redshift a parameter?",&status)) PrintError(status);

   /* OGIP required keywords */
   if(fits_write_key(foutptr,TSTRING,"HDUCLASS","OGIP",
	"format conforms to OGIP standard",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUDOC","OGIP/92-009",
	"document defining format",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUCLAS1","XSPEC TABLE MODEL",
	"model spectra for XSPEC",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUVERS1","1.0.0",
	"version of format",&status)) PrintError(status);
   /* done with primary extension */

   /* Now build the physical parameter extension */
   if(verbose) c_fcecho("Write_FITS_ParmTable: Building Physical Parameter Table Header.");
   /* what's the largest number of interpolated values we need to allocate? */
   maxNumberOfValues=0;
   naddparms=0;
   ninterparms=0;
   for(j=0;j<kNumPhysicalParms;j++){
     if(maxNumberOfValues<parms->physical[j].numberOfValues)
       maxNumberOfValues=parms->physical[j].numberOfValues;
     /* update the additional & interpolated parameter counters */
     switch(parms->physical[j].type){
     case kAdditive: naddparms++;
         break;
     case kInterpolated: ninterparms++;
         break;
     } /* end of 'type' switch */
   } /* end of 'j' loop */
   sprintf(tempstr,"%dE",maxNumberOfValues);/* update format string */
   tform[9]=(char*)malloc(1+strlen(tempstr));
   strcpy(tform[9],tempstr);

   if(verbose) c_fcecho("Write_FITS_ParmTable: Writing Physical Parameter Table Header.");
   if(fits_create_tbl(foutptr,BINARY_TBL,naddparms+ninterparms,kTfields,ttype,
		      tform,tunit,extname,&status)) PrintError(status);
   /* build actual table columns */
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Allocating %d x %d array.",
		      (naddparms+ninterparms),maxNumberOfValues);
     c_fcecho(msg);
   }
   value=matrix_alloc((naddparms+ninterparms),maxNumberOfValues);
   if(verbose) c_fcecho("Write_FITS_ParmTable: Building Physical Parameter Table columns.");

   /* Note that we are building this table by writing rows of the interpolated parameters
      first, followed by rows of the additive parameters */
   intptr=0; /* pointer for building interpolated table */
   addptr=ninterparms; /* pointer for building additive table *behind* interpolated table */
   for(j=0;j<kNumPhysicalParms;j++){
     switch(parms->physical[j].type){
     case kConstant:
       /* manage constant parameters as a keyword */
       strncpy(tempstr,parms->physical[j].name,8); /* truncate @ 8 characters */
       tempstr[8]='\0'; /* make sure it's NULL at the end */
       if(fits_write_key(foutptr,TFLOAT,tempstr,&(parms->physical[j].initialValue),
	  "physical parameter held constant",&status)) PrintError(status);
       break;
     case kAdditive:
       /* manage additive parameters */
       name[addptr]    =(char*)malloc((1+strlen(parms->physical[j].name))*sizeof(char));
       strcpy(name[addptr],parms->physical[j].name);
       method[addptr]  =(int)parms->physical[j].method;
       initial[addptr] =parms->physical[j].initialValue;
       delta[addptr]   =parms->physical[j].delta;
       minimum[addptr] =parms->physical[j].hardMinimum;
       bottom[addptr]  =parms->physical[j].softMinimum;
       top[addptr]     =parms->physical[j].softMaximum;
       maximum[addptr] =parms->physical[j].hardMaximum;
       numbvals[addptr]=parms->physical[j].numberOfValues;
       addptr++;
       break;
     case kInterpolated:
       /* manage interpolated parameters */
       name[intptr]    =(char*)malloc((1+strlen(parms->physical[j].name))*sizeof(char));
       strcpy(name[intptr],parms->physical[j].name);
       method[intptr]  =(int)parms->physical[j].method;
       initial[intptr] =parms->physical[j].initialValue;
       delta[intptr]   =parms->physical[j].delta;
       minimum[intptr] =parms->physical[j].hardMinimum;
       bottom[intptr]  =parms->physical[j].softMinimum;
       top[intptr]     =parms->physical[j].softMaximum;
       maximum[intptr] =parms->physical[j].hardMaximum;
       numbvals[intptr]=parms->physical[j].numberOfValues;
       if(parms->physical[j].numberOfValues > 0)
         for(i=0;i<parms->physical[j].numberOfValues;i++){
          value[intptr][i]=parms->physical[j].valueList[i];
         } /* end of 'i' loop */
       intptr++;
       break;
     default:
        sprintf(msg,"Get_Parms: Undefined parameter type %d",parms->physical[j].type);
	c_fcerr(msg);
     } /* end of 'switch' */
   } /* end of 'j' loop */

   if(verbose) c_fcecho("Write_FITS_ParmTable: Writing Physical Parameter Table columns.");
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",1,tform[0]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TSTRING,1,1,1,(naddparms+ninterparms),name,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",2,tform[1]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TLONG,2,1,1,(naddparms+ninterparms),method,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",3,tform[2]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TFLOAT,3,1,1,(naddparms+ninterparms),initial,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",4,tform[3]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TFLOAT,4,1,1,(naddparms+ninterparms),delta,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",5,tform[4]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TFLOAT,5,1,1,(naddparms+ninterparms),minimum,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",6,tform[5]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TFLOAT,6,1,1,(naddparms+ninterparms),bottom,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",7,tform[6]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TFLOAT,7,1,1,(naddparms+ninterparms),top,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",8,tform[7]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TFLOAT,8,1,1,(naddparms+ninterparms),maximum,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",9,tform[8]);
     c_fcecho(msg);
   }
   if(fits_write_col(foutptr,TLONG,9,1,1,(naddparms+ninterparms),numbvals,&status)) PrintError(status);
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: Column %d: %s",10,tform[9]);
     c_fcecho(msg);
   }
   for(j=0;j<(naddparms+ninterparms);j++)
     if(fits_write_col(foutptr,TFLOAT,10,j+1,1,maxNumberOfValues,
	 value[j],&status)) PrintError(status);

   /* add supplemental keywords */
   /* NINTPARM, NADDPARM */
   if(verbose) {
     sprintf(msg,"Write_FITS_ParmTable: %d additional parameters & %d interpolated parameters.",naddparms,ninterparms);
     c_fcecho(msg);
   }
   if(fits_write_comment(foutptr,"Required by XSPEC",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"NINTPARM",&ninterparms,
	"number of interpolated parameters",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"NADDPARM",&naddparms,
	"number of additional parameters",&status)) PrintError(status);

   if(fits_write_key(foutptr,TFLOAT,"ELOW",&(parms->energyLow),
	  "energy band low end (eV)",&status)) PrintError(status);
   if(fits_write_key(foutptr,TFLOAT,"EHIGH",&(parms->energyHigh),
	  "energy band high end (eV)",&status)) PrintError(status);

   /* non-physical control parameters */
   if(fits_write_comment(foutptr,"Non-Physical Control Parameters",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"SPECTRUM",parms->spectrumName,
	"spectrum name", &status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"SPECFILE",parms->spectrumFile,
        "spectrum file", &status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"SPECUNIT",&(parms->spectrumUnits),
        "spectral units (0=energy, 1=photons)",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"NSTEPS",&(parms->numberOfSteps),
        "Number of steps", &status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"NITER",&(parms->numberOfIterations),
        "Number of iterations",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"WRITESW",&(parms->writeSwitch),
        "write switch",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"PRINTSW",&(parms->printSwitch),
        "print switch",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"STEPSIZE",&(parms->stepSizeChoiceSwitch),
        "step size choice switch",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"NPASS",&(parms->numberOfPasses),
        "number of passes",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"PRESSSW",&(parms->constantPressureSwitch),
        "constant pressure switch",&status)) PrintError(status);
   if(fits_write_key(foutptr,TFLOAT,"EMULT",&(parms->emult),
	  "courant multiplier",&status)) PrintError(status);
   if(fits_write_key(foutptr,TFLOAT,"TAUMAX",&(parms->taumax),
	  "tau max for courant step",&status)) PrintError(status);
   if(fits_write_key(foutptr,TFLOAT,"XEEMIN",&(parms->xeemin),
	  "minimum elctron fraction",&status)) PrintError(status);
   if(fits_write_key(foutptr,TFLOAT,"CRITF",&(parms->critf),
	  "critical ion abundance",&status)) PrintError(status);
   if(fits_write_key(foutptr,TFLOAT,"VTURBI",&(parms->vturbi),
	  "turbulent velocity (km/s)",&status)) PrintError(status);
   if(fits_write_key(foutptr,TFLOAT,"RADEXP",&(parms->radexp),
	  "radius exponent",&status)) PrintError(status);
   if(fits_write_key(foutptr,TINT,"NCN2",&(parms->ncn2),
	  "number of energy bins",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"MODELNAM",parms->modelName,
        "model name",&status)) PrintError(status);

   /* OGIP required keywords */
   if(fits_write_comment(foutptr,"OGIP Required Keywords",&status))
        PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUCLASS","OGIP",
	"format conforms to OGIP standard",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUDOC","OGIP/92-009",
	"document defining format",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUCLAS1","XSPEC TABLE MODEL",
	"model spectra for XSPEC",&status)) PrintError(status);
   if(fits_write_key(foutptr,TSTRING,"HDUVERS1","1.0.0",
	"version of format",&status)) PrintError(status);

   /* close the FITS file */
   if (fits_close_file(foutptr, &status) ) PrintError( status );
   if(verbose) c_fcecho("Write_FITS_ParmTable: FITS file is closed");

   /* free-up memory */
   if(verbose) c_fcecho("Write_FITS_ParmTable: Releasing Memory.");
   matrix_dealloc(value,(naddparms+ninterparms),maxNumberOfValues);

   return(status);
}

/********************************************************
 *  Generate_Combinations:                              *
 *                                                      *
 *  Abstract:                                           *
 *  Generate all multiplicative permutations of a       *
 *  multiply sampled parameter table.                   *
 ********************************************************
 *  Input:                                              *
 *     numpars (int) - number of parameters to vary     *
 *     curpars (int*) - array with *current* parameter  *
 *                        samples                       *
 *     maxpars (int*) - array with *maximum* number of  *
 *                        samples                       *
 *                        per parameter                 *
 *  Output:                                             *
 *     status code: 0 = everything okay                 *
 *                  1 = end of permutations             *
 *                                                      *
 *  curpars is updated with each call.                  *
 *                                                      *
 *  Goes through the list permuting the *highest* index *
 *  the fastest so that it will stay in synch with the  *
 *  way the PARAMVAL ordering is described in the       *
 *  SPECTRA header of the table models.                 *
 *  To go over all combinations >=0, initialize the     *
 *  curpars array as: curpars[i]=0;  (i<numpars-1)      *
 *                    curpars[i]=-1; (i=numpars-1)      *
 ********************************************************/
int Generate_Combinations(int numpars, int* curpars, 
			  int* maxpars, int verbose) {
  int j,status,done;

  status=0;
  j=numpars-1;
  done=0; /* not done, 1=done */
  while(status==0 && done!=1) {
    curpars[j]++; /* increment current parameter pointer */
    if(curpars[j]==maxpars[j]) {
       if(j==0) status=1; /* all combinations complete */
       else {
	 curpars[j]=0; /* resets counter */
         j--; /* jump to next 'digit' */
       }
    } else done=1; /* else done incrementing */
  } /* end of 'while' */
  return(status);
}

/********************************************************
 *  Write_Text_Parmlist:                                *
 *                                                      *
 *  Abstract:                                           *
 *     Write a text file of the XSTAR paramter values   *
 *                                                      *
 *  Input Parameters:                                   *
 *     status = 0: everything is okay                   *
 *              1: filename missing                     *
 *                                                      *
 ********************************************************/
int Write_Text_Parmlist(char* textOutput, Parameter_struct *parms, 
			int verbose){
   int           status=0;
   /* text output parameters */
   FILE          *textfileptr;
   int           aptr,iptr,cptr,curParmPtr, loopstat=0, counter, slen;
   int           loopCtr, j, k;
   int           additivePointers[NUMPHYSICALPARMS],
                 constantPointers[NUMPHYSICALPARMS],
                 interpolatedPointers[NUMPHYSICALPARMS],
                 maxInterpSamples[NUMPHYSICALPARMS],
                 curInterpSample[NUMPHYSICALPARMS];
   char          delimiter[10], msg[ERRMSG], msg2[ERRMSG];
   char          parmString[MAXPARMLINELENGTH],
                 tmpString[MAXPARMLINELENGTH],
                 interpString[MAXPARMLINELENGTH],
                 holdString[MAXPARMLINELENGTH],
                 addString[MAXPARMLINELENGTH];

   /* build tables of constant, additive & interpolated parameter pointers  */
   /* Note that these entries are assigned in order of INCREASING parameter */
   /* number.  Therefore lower parameter numbers will change the fastest in */
   /* generating the permutations.  This is an important consideration when */
   /* contemplating changes to the XSTAR2TABLE FTOOL.                       */
   aptr=0;
   iptr=0;
   cptr=0;
   for(curParmPtr=0;curParmPtr<kNumPhysicalParms;curParmPtr++) {
     switch(parms->physical[curParmPtr].type){
     case kConstant: 
       constantPointers[cptr]=curParmPtr;
       cptr++;
       break;
     case kAdditive:
       additivePointers[aptr]=curParmPtr;
       aptr++;
       break;
     case kInterpolated:
       interpolatedPointers[iptr]=curParmPtr;
       maxInterpSamples[iptr]=parms->physical[curParmPtr].numberOfValues;
       iptr++;
       break;
     default:
       sprintf(msg,"Write_Text_Parmlist: Undefined parameter type %d",
		   parms->physical[curParmPtr].type);   
       c_fcecho(msg);
     } /* end switch 'type' */
   } /* end 'curParmPtr' loop */

   /* initialize current interpolated sample pointers */
   if(iptr>0) {
      for(j=0;j<iptr;j++) curInterpSample[j]=0;
      curInterpSample[iptr-1]=-1; 
   }

   if(verbose){
      sprintf(msg,"%d Interpolated Parms: ",iptr);
      if(iptr>0) for(j=0;j<iptr;j++) {
	sprintf(msg2,"%s ",parms->physical[interpolatedPointers[j]].name);
	strcat(msg,msg2);
      }
      c_fcecho(msg);
      sprintf(msg,"%d Additive Parms: ",aptr);
      if(aptr>0) for(j=0;j<aptr;j++) {
	sprintf(msg2,"%s ",parms->physical[additivePointers[j]].name);
	strcat(msg,msg2);
      }
      c_fcecho(msg);
      sprintf(msg,"%d Constant Parms: ",cptr);
      if(cptr>0) for(j=0;j<cptr;j++) {
	sprintf(msg2,"%s ",parms->physical[constantPointers[j]].name);
	strcat(msg,msg2);
      }
      c_fcecho(msg);
   }

   /* make sure there is a filename & strings to write */
   if(textOutput!=(char*)NULL){
      if(verbose) {
	sprintf(msg,"Write_Text_Parmlist: Opening/creating %s.",textOutput);
	c_fcecho(msg);
      }
      if(!(textfileptr=fopen(textOutput,"w"))){
	sprintf(msg,"Write_Text_Parmlist: Unable to open %s",textOutput);
	c_fcerr(msg);
      }

      /* generate parameter lists here */
      loopstat=0;
      counter=0;
      loopCtr=0;
      while(loopstat==0) {
        /* update interpolation permutation table */
	loopstat=Generate_Combinations(iptr,curInterpSample,maxInterpSamples,verbose);
        /* if permuation is valid, generate parameter list */
        if(loopstat==0) {
           counter++;
           if((iptr>0)&&verbose) { 
              sprintf(msg,"(%4d) ",counter);  
              for(j=0;j<iptr;j++) {
		sprintf(msg2,"%d ",curInterpSample[j]); 
		strcat(msg,msg2);
	      }
              c_fcecho(msg); 
           } 

           strcpy(delimiter," "); /* delimiter is a space */

           /* build the constant portion of the parameter string */
           strcpy(parmString,"xstar "); /* initialize command string */
           status=Generate_Control(tmpString,delimiter,parms);
           strcat(parmString,tmpString);
           status=Generate_Constants(tmpString,delimiter,cptr,
		  constantPointers,parms);
           strcat(parmString,tmpString);

           /* build the interpolated portion of the parameter string */
           status=Generate_Interpolated(interpString,delimiter,iptr,
		  interpolatedPointers,curInterpSample,parms);
           strcat(parmString,interpString);

           /* save current version of parmString */
	   strcpy(holdString,parmString);

           /* build the additional portion of the parameter string */
           if(aptr>0){
             /* Must generate (aptr+1) additional parameter lists */
             /* One has all additional parameters=0.0, the others */
             /* have all but one of the additional parameters=0.0 */
             for(k=-1;k<aptr;k++){
               status=Generate_Additional(addString,delimiter,aptr,k,
                  additivePointers,parms);
               /* now re-assemble the parameter list */
               strcpy(parmString,holdString);
	       strcat(parmString,addString);
               /* append loopcontrol parameter */
               loopCtr++;
	       sprintf(tmpString,"loopcontrol=%d",loopCtr);
               slen=strlen(tmpString);
               if(strlen(parmString)+strlen(delimiter)+slen<MAXPARMLINELENGTH) {
		 strcat(parmString,delimiter);
	         strcat(parmString,tmpString);
	       } else {
	          sprintf(msg,"Write_Text_Parmlist: Paramter list is %d characters too long.",
		     strlen(parmString)+strlen(delimiter)+slen-MAXPARMLINELENGTH);
		  c_fcerr(msg);
	       }

               fprintf(textfileptr,"%s\n",parmString); /* write parameter string to file */

             } /* end of 'k' loop */
           } else {
	     /* If there are no additive parameters, make sure the loop control variable
		is appended to the end of the command line before writing it */
	     if(aptr==0) {
               /* append loopcontrol parameter */
               loopCtr++;
	       sprintf(tmpString,"loopcontrol=%d",loopCtr);
               slen=strlen(tmpString);
               if(strlen(parmString)+strlen(delimiter)+slen<MAXPARMLINELENGTH) {
		 strcat(parmString,delimiter);
	         strcat(parmString,tmpString);
	       } else {
	          sprintf(msg,"Write_Text_Parmlist: Paramter list is %d characters too long.",
		     strlen(parmString)+strlen(delimiter)+slen-MAXPARMLINELENGTH);
		  c_fcerr(msg);
	       }
	       fprintf(textfileptr,"%s\n",parmString); /* write parameter string to file */
	     } else {
	       sprintf(msg,"Write_Text_Parmlist: Number of Additive parameters invalid %d",aptr);
	       c_fcerr(msg);
	     }
	   }/* additional parameters complete */

        } /* end of 'if' conditional */
      } /* end of 'while' loop for ALL parameter lists */

      if(verbose) {
	sprintf(msg,"Write_Text_Parmlist: Closing %s.",textOutput);
	c_fcecho(msg);
      }
      if(fclose(textfileptr)){
	sprintf(msg,"Write_Text_Parmlist: Unable to close %s",textOutput);
	c_fcerr(msg);
      }

   } else {
      c_fcerr("Write_Text_Parmlist: Missing filename or string list.");
      status=1;
   }
   return(status);
}
/***********************************************************
 * Generate_Control:                                       *
 *                                                         *
 * Return code:                                            *
 *     0 = no errors                                       *
 *     1 = ill-defined error building parameter list       *
 *     2 = parameter list too long error                   *
 ***********************************************************/
int Generate_Control(char* parmlist, char* delimiter, 
		     Parameter_struct *parms){
  int status=0;
  int j,slen;
  char tmpString[MAXPARMLINELENGTH], msg[ERRMSG];

  strcpy(parmlist,""); /* clear the output string */

  sprintf(tmpString,"spectrum='%s' ",parms->spectrumName);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"spectrum_file='%s' ",parms->spectrumFile);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"spectun=%d ",parms->spectrumUnits);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"nsteps=%d ",parms->numberOfSteps);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"niter=%d ",parms->numberOfIterations);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"lwrite=%d ",parms->writeSwitch);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"lprint=%d ",parms->printSwitch);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"lstep=%d ",parms->stepSizeChoiceSwitch);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"npass=%d ",parms->numberOfPasses);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"lcpres=%d ",parms->constantPressureSwitch);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"emult=%f ",parms->emult);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"taumax=%f ",parms->taumax);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"xeemin=%f ",parms->xeemin);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"critf=%g ",parms->critf);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"vturbi=%f ",parms->vturbi);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"radexp=%f ",parms->radexp);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"ncn2=%d ",parms->ncn2);
  strcat(parmlist,tmpString);
  sprintf(tmpString,"modelname='%s' ",parms->modelName);
  strcat(parmlist,tmpString);

  if(strlen(parmlist) > MAXPARMLINELENGTH) {
     sprintf(msg,"Generate_Constants: Paramter list is %d characters too long.",
	     strlen(parmlist)-MAXPARMLINELENGTH);
     c_fcerr(msg);
     status=2;
  }
  return(status);
}
/***********************************************************
 * Generate_Constants:                                     *
 *                                                         *
 * Return code:                                            *
 *     0 = no errors                                       *
 *     1 = ill-defined error building parameter list       *
 *     2 = parameter list too long error                   *
 ***********************************************************/
int Generate_Constants(char* parmlist, char* delimiter, int cptr,
			int constantPointers[], Parameter_struct *parms){
  int status=0;
  int j,slen;
  char tmpString[MAXPARMLINELENGTH], msg[ERRMSG];

  strcpy(parmlist,""); /* clear the output string */
  for(j=0;j<cptr;j++){
     sprintf(tmpString,"%s=%g",
	     parms->physical[constantPointers[j]].name,
	     parms->physical[constantPointers[j]].initialValue);
     slen=strlen(tmpString);
     if(slen>0) {
	strcat(parmlist,delimiter);
	strcat(parmlist,tmpString);
     } else {
	sprintf(msg,"Generate_Constants: Error building parameter list, slen=%d.",slen);
	c_fcerr(msg);
        status=1;
     }
  } /* end of 'j' loop */
  if(strlen(parmlist) > MAXPARMLINELENGTH) {
     sprintf(msg,"Generate_Constants: Paramter list is %d characters too long.",
	     strlen(parmlist)-MAXPARMLINELENGTH);
     c_fcerr(msg);
     status=2;
  }
  return(status);
}
/***********************************************************
 * Generate_Interpolated:                                  *
 *                                                         *
 * Return code:                                            *
 *     0 = no errors                                       *
 *     1 = ill-defined error building parameter list       *
 *     2 = parameter list too long error                   *
 ***********************************************************/
int Generate_Interpolated(char* parmlist, char* delimiter, int iptr,
			int interpolatedPointers[], 
			int curInterpSample[], Parameter_struct *parms){
  int status=0;
  int j,slen;
  char tmpString[MAXPARMLINELENGTH],msg[ERRMSG];

  strcpy(parmlist,"");
  if(iptr>0){
    for(j=0;j<iptr;j++){
      sprintf(tmpString,"%s=%g",parms->physical[interpolatedPointers[j]].name,
	     parms->physical[interpolatedPointers[j]].valueList[curInterpSample[j]]);
      slen=strlen(tmpString);
      /* was parameter entry actually written? */
      if(strlen(parmlist)+strlen(delimiter)+slen > MAXPARMLINELENGTH) {
	   sprintf(msg,"Generate_Interpolated: Paramter list is %d characters too long.",
		 strlen(parmlist)+strlen(delimiter)+slen-MAXPARMLINELENGTH);
	   c_fcerr(msg);
           status=2;
      }
      if(slen>0) {
	  strcat(parmlist,delimiter);
	  strcat(parmlist,tmpString);
      } else {
	  sprintf(msg,"Generate_Interpolated: Error building parameter list, slen=%d",slen);
	  c_fcerr(msg);
          status=1;
      }
    } /* end of 'j' loop */
  } /* interpolated parameters complete */
  return(status);
}
/***********************************************************
 * Generate_Additional:                                    *
 *                                                         *
 * Return code:                                            *
 *     0 = no errors                                       *
 *     1 = ill-defined error building parameter list       *
 *     2 = parameter list too long error                   *
 ***********************************************************/
int Generate_Additional(char* parmlist, char* delimiter, int aptr, int current,
			int additivePointers[], Parameter_struct *parms){
  int status=0;
  int j,slen;
  char tmpString[MAXPARMLINELENGTH],msg[ERRMSG];

  strcpy(parmlist,""); /* clear the output string */
  for(j=0;j<aptr;j++){
     if(current!=j) {
	sprintf(tmpString,"%s=0.0",
	     parms->physical[additivePointers[j]].name);
        slen=strlen(tmpString);
     } else {
	sprintf(tmpString,"%s=%g",
	     parms->physical[additivePointers[j]].name,
	     parms->physical[additivePointers[j]].initialValue);
        slen=strlen(tmpString);
     }
     if(slen>0) {
	strcat(parmlist,delimiter);
	strcat(parmlist,tmpString);
     } else {
	sprintf(msg,"Generate_Additional: Error building parameter list, slen=%d",slen);
	c_fcerr(msg);
        status=1;
     }
  } /* end of 'j' loop */
  if(strlen(parmlist) > MAXPARMLINELENGTH) {
     sprintf(msg,"Generate_Additional: Paramter list is %d characters too long.",
	     strlen(parmlist)-MAXPARMLINELENGTH);
     c_fcerr(msg);
     status=2;
  }
  return(status);
}
