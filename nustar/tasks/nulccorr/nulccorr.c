/*
 * 
 *	nulccorr.c
 *
 *	INVOCATION:
 *
 *		nulccorr [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 16/10/12 - First version
 *        0.1.1 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.1.2 - NS 15/11/12 - Bug fixed in 'FindClosestHkIndex' routine
 *        0.1.3 - NS 06/06/13 - Handle PSF and EXPOSURE correction
 *        0.1.4 - NS 10/07/13 - Handle new input parameters of nuexpomap task
 *        0.1.5 - NS 17/07/13 - Handle energy-dependent CALDB query for 2D-PSF file
 *        0.1.6 - NS 04/12/13 - Set DEADAPP keyword to TRUE in output corrected light curve 
 *        0.1.7 - NS 31/03/14 - Added 'inskyinstrfile' and 'inaspecthistofile' input parameters
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nulccorr  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nulccorr.h"


/********************************/
/*         definitions          */
/********************************/

#define NULCCORR_C
#define NULCCORR_VERSION      "0.1.7"
#define PRG_NAME               "nulccorr"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nulccorr_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nulccorr.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nulccorr_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 16/10/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nulccorr_getpar()
{
  
  /* Input Light Curve File Name */
  if(PILGetFname(PAR_LCFILE, global.par.lcfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_LCFILE);
      goto Error;
    }

  /* Input Housekeeping File Name */
  if(PILGetFname(PAR_HKFILE, global.par.hkfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HKFILE);
      goto Error;
    }
  
  /* Output Corrected Light Curve File Name */
  if(PILGetFname(PAR_OUTLCFILE, global.par.outlcfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTLCFILE);
      goto Error;	
    }

  /* Input Event File Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }

  /* extended */
  if(PILGetBool(PAR_EXTENDED, &global.par.extended))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EXTENDED); 
      goto Error;	 
    }

  /* expoflag */
  if(PILGetBool(PAR_EXPOFLAG, &global.par.expoflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EXPOFLAG); 
      goto Error;	 
    }

  /* psfflag */
  if(PILGetBool(PAR_PSFFLAG, &global.par.psfflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSFFLAG); 
      goto Error;	 
    }

  
  if(PILGetFname(PAR_INSKYINSTRFILE, global.par.inskyinstrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INSKYINSTRFILE);
      goto Error;
    }

  if(PILGetFname(PAR_INASPECTHISTOFILE, global.par.inaspecthistofile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INASPECTHISTOFILE);
      goto Error;
    }


  if( (global.par.expoflag||global.par.psfflag) && ( (!strcasecmp(global.par.inskyinstrfile, DF_NONE)) && (!strcasecmp(global.par.inaspecthistofile, DF_NONE)) ) )
    global.runexpomap = TRUE;
  else
    global.runexpomap = FALSE;


  if(global.runexpomap){
    
    if(PILGetFname(PAR_PIXPOSFILE, global.par.pixposfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIXPOSFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_ALIGNFILE, global.par.alignfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ALIGNFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_MASTASPECTFILE, global.par.mastaspectfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_MASTASPECTFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_ATTFILE, global.par.attfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ATTFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_TELDEF, global.par.teldef)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TELDEF);
	goto Error;	
      }

    if(PILGetFname(PAR_INSTRPROBMAPFILE, global.par.instrprobmapfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INSTRPROBMAPFILE);
	goto Error;	
      }

    if(PILGetFname(PAR_DET1REFFILE, global.par.det1reffile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DET1REFFILE);
	goto Error;	
      }

  }


  /*  Vignetting File Name */
  if(PILGetFname(PAR_VIGNFILE, global.par.vignfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_VIGNFILE);
      goto Error;	
    }

  if(PILGetBool(PAR_ABERRATION, &global.par.aberration))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ABERRATION);
      goto Error;	
    }

  /* vignflag */
  if(PILGetBool(PAR_VIGNFLAG, &global.par.vignflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_VIGNFLAG); 
      goto Error;	 
    }

  if(PILGetReal(PAR_ENERGY, &global.par.energy))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ENERGY); 
      goto Error;	 
    }

  /*  Optical Axis File Name */
  if(PILGetFname(PAR_OPTAXISFILE, global.par.optaxisfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OPTAXISFILE);
      goto Error;	
    }

  if(PILGetInt(PAR_PIXBIN, &global.par.pixbin))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIXBIN); 
      goto Error;	 
    }

  if(PILGetBool(PAR_CUTMAPS, &global.par.cutmaps))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CUTMAPS);
      goto Error;	
    }

  if(PILGetInt(PAR_BOXSIZE, &global.par.boxsize))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BOXSIZE); 
      goto Error;	 
    }

  if(PILGetReal(PAR_PERCENT, &global.par.percent))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PERCENT); 
      goto Error;	 
    }

  if(PILGetFname(PAR_SKYINSTRFILE, global.par.skyinstrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SKYINSTRFILE);
      goto Error;	
    }

  /* inistseed */
  if(PILGetBool(PAR_INITSEED, &global.par.initseed))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INITSEED); 
      goto Error;	 
    }
  
  if(PILGetFname(PAR_CORRFILE, global.par.corrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CORRFILE);
      goto Error;	
    }

  if( global.par.psfflag && !global.par.extended )
    {
      if(PILGetFname(PAR_PSFFILE, global.par.psffile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSFFILE);
	  goto Error;	
	}
    }

  if( global.par.expoflag && global.par.extended )
    {
      if(PILGetFname(PAR_PHAFILE, global.par.phafile)) 
	{
	  headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PHAFILE);
	  goto Error;
	}
    }


  get_history(&global.hist);
  nulccorr_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nulccorr_getpar */


/*
 *	nulccorr_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nulccorr_checkinput();
 *             int headas_chat(int , char *, ...);
 *             int strcasecmp(const char *s1, const char *s2);
 *             FitsFileUnit_t OpenReadFitsFile(char *name);
 *             int fits_movnam_hdu(fitsfile *fptr,int hdutype, char *extname, int extver, int *status);
 *             int CloseFitsFile(FitsFileUnit_t file);
 *             FitsHeader_t  RetrieveFitsHeader(FitsFileUnit_t unit);
 *             int fits_get_hdu_num(fitsfile *fptr, int *hdunum);
 *             FitsFileUnit_t OpenWriteFitsFile(char *name);
 *             int fits_get_num_hdus(fitsfile *fptr, int *hdunum, int *status);
 *             int fits_movabs_hdu(fitsfile *fptr, int hdunum, > int * hdutype, int *status );
 *             int fits_copy_hdu(fitsfile *infptr, fitsfile *outfptr, int morekeys, int *status);
 *             int HDpar_stamp(fitsfile *fptr, int hdunum);
 *             int ChecksumCalc(FitsFileUnit_t unit);
 *             int RenameFile (char *, char *);
 *             int CopyFile(char *source, char *destination);
 *             int CalGetFileName(int maxret, char *DateObs, char *TimeObs, char *DateEnd, char *TimeEnd,const char *DataSet, 
 *                                char *CalFileName, char *expr, long *extno, const char *instrument, const char *detnam);
 *	
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 16/10/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nulccorr_work()
{
  int               status=OK, inExt, outExt, evExt;
  LTYPE             logical;
  long              extfile;
  double            skyx=500.5, skyy=500.5;
  int               skysize=1000;
  char              cmd[BUF_SIZE], aberration[5], initseed[5], clobber[5], history[5], vignflag[5];
  char              psfquery[BUF_SIZE], skyinstrfile[PIL_LINESIZE], aspecthistofile[PIL_LINESIZE];
  HKRow_t           *hkinfo=NULL;
  int               hknrows=0;
  PsfCorrInfo_t     *psfcorrinfo=NULL;
  int               psfcorrcount=0;
  FitsHeader_t      head;
  FitsCard_t        *card;
  FitsFileUnit_t    inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */ 
  struct gti_struct gti;
  pid_t             pid;
  
  /* Get pid */
  pid=getpid();


  if(nulccorr_checkinput())
    goto Error;


  if(global.par.aberration)
    sprintf(aberration, "yes");
  else
    sprintf(aberration, "no");

  if(global.par.initseed)
    sprintf(initseed, "yes");
  else
    sprintf(initseed, "no");

  if(global.par.vignflag)
    sprintf(vignflag, "yes");
  else
    sprintf(vignflag, "no");

  if(headas_clobpar)
    sprintf(clobber, "yes");
  else
    sprintf(clobber, "no");

if (global.hist)
    sprintf(history, "yes");
  else
    sprintf(history, "no");

  
  /* Get Observation Info from input file */
  if( GetObsInfo(global.par.infile, KWVL_EXTNAME_EVT, &global.obsinfo) ){
    headas_chat(NORMAL, "%s: Error: Unable to get info from input '%s' file.\n", global.taskname, global.par.infile);
    goto Error;
  }


  /* PSF correction */
  if(global.par.psfflag && !global.par.extended)
    {
      skyx=500.5;
      skyy=500.5;
      skysize=1000;


      /* Read REGION info from input lcfile */
      if( ReadRegionInfo(global.par.lcfile, "REG00101", &global.reginfo) ){
	headas_chat(NORMAL, "%s: Error: Unable to get REGION Info from input '%s' file.\n", global.taskname, global.par.lcfile);
	goto Error;
      }
      
      /* Only 'CIRCLE' 'ANNULUS' and 'ELLIPSE' Regions are supported */
      if( strcmp(global.reginfo.shape, "CIRCLE") && strcmp(global.reginfo.shape, "ANNULUS") && strcmp(global.reginfo.shape, "ELLIPSE") ){
	headas_chat(NORMAL, "%s: Error: Region shape '%s' not supported for PSF correction.\n", global.taskname, global.reginfo.shape);
	goto Error;
      }


      /* Derive CALDB psffile name */  
      if ( !strcasecmp(global.par.psffile,DF_CALDB) )
	{
	  sprintf(psfquery,"THETA.eq.0.and.ENERGY.eq.%f", global.par.energy);

	  if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_PSF_DSET, global.par.psffile, psfquery, &extfile, global.obsinfo.instrume, HD_DETNAM))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB for psffile parameter.\n", global.taskname);
	      goto Error;
	    }
	  extfile++;
	}


      if(global.par.cutmaps)
	{
	  if( !strcmp(global.reginfo.shape, "CIRCLE") )
	    {
	      skyx = global.reginfo.X[0];
	      skyy = global.reginfo.Y[0];
	      skysize = (int)(global.reginfo.R[0]*2 +0.5);
	    }
	  if( !strcmp(global.reginfo.shape, "ANNULUS") || !strcmp(global.reginfo.shape, "ELLIPSE") )
	    {
	      skyx = global.reginfo.X[0];
	      skyy = global.reginfo.Y[0];
	      skysize = (int)(MAX(global.reginfo.R[0],global.reginfo.R1[0])*2 +0.5);
	    }
	}


      sprintf(global.tmpout.expomap, "%dtmp_expo.fits", (int)pid);
      sprintf(global.tmpout.aspecthisto, "%dtmp_asphisto.fits", (int)pid);
      sprintf(global.tmpout.offset, "%dtmp_offset.fits", (int)pid);


      if( !global.runexpomap ){

	strcpy(skyinstrfile, global.par.inskyinstrfile);
	strcpy(aspecthistofile, global.par.inaspecthistofile);

	if( CheckInputSkyInstrFile(skyinstrfile, global.par.energy, global.par.vignflag) )
	  goto Error;

      }
      else{

	/* Execute expomap task */      
	sprintf(cmd, "nuexpomap infile=%s pixposfile=%s alignfile=%s mastaspectfile=%s attfile=%s teldef=%s instrprobmapfile=%s aberration=%s det1reffile=%s pixbin=%d expomapfile=%s aspecthistofile=%s offsetfile=%s det1instrfile=NONE det2instrfile=NONE skyinstrfile=%s vignflag=%s skyx=%f skyy=%f skysize=%d initseed=%s energy=%f chatter=%d clobber=%s history=%s percent=%f indet2instrfile=NONE",
		global.par.infile, global.par.pixposfile, global.par.alignfile, global.par.mastaspectfile, global.par.attfile, global.par.teldef,
		global.par.instrprobmapfile, aberration, global.par.det1reffile, global.par.pixbin, global.tmpout.expomap, global.tmpout.aspecthisto, 
		global.tmpout.offset, global.tmpout.skyinstrfile, vignflag, skyx, skyy, skysize, initseed, global.par.energy, headas_chatpar, clobber, history,
		global.par.percent);
	headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
	fflush(stdout);
	status = system(cmd);
	if(status!=0){
	  headas_chat(NORMAL, "%s: Error: unable to create temporary Exposure Map file\n", global.taskname);
	  goto Error;
	}

	strcpy(skyinstrfile, global.tmpout.skyinstrfile);
	strcpy(aspecthistofile, global.tmpout.aspecthisto);

      }


      /* Compute PSF Correction */
      if(ComputePSFCorrection(aspecthistofile, skyinstrfile, global.par.psffile, skyx, skyy, &global.reginfo, &psfcorrinfo, &psfcorrcount)){
	headas_chat(NORMAL, "%s: Error: unable to compute PSF correction values.\n", global.taskname);
	goto Error;
      }


      /* Create output correction file */
      if(global.createcorrfile)
	{
	  if(WritePsfCorrFile(psfcorrinfo, psfcorrcount, aspecthistofile, global.par.corrfile)){
	    headas_chat(NORMAL, "%s: Error: Unable to create output correction file '%s'\n", global.taskname, global.par.corrfile);
	    goto Error;
	  }
	  
	  headas_chat(NORMAL, "%s: Info: output correction file '%s' created.\n", global.taskname, global.par.corrfile);
	}


      /* Cleanup Temporary Files */
      
      if( (!global.createskyinstrfile) && FileExists(global.tmpout.skyinstrfile)){
	if(remove (global.tmpout.skyinstrfile) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.skyinstrfile);
	}
      }
      
      if(FileExists(global.tmpout.expomap)){
	if(remove (global.tmpout.expomap) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.expomap);
	}
      }
	  
      if(FileExists(global.tmpout.aspecthisto)){
	if(remove (global.tmpout.aspecthisto) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.aspecthisto);
	}
      }
      
      if(FileExists(global.tmpout.offset)){
	if(remove (global.tmpout.offset) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.offset);
	}
      }
       
    } /* END PSF correction -> if(global.par.psfflag && !global.par.extended) */


  /* EXPOSURE correction */
  if(global.par.expoflag && global.par.extended)
    {
      skyx=500.5;
      skyy=500.5;
      skysize=1000;

      sprintf(global.tmpout.expomap, "%dtmp_expo.fits", (int)pid);
      sprintf(global.tmpout.aspecthisto, "%dtmp_asphisto.fits", (int)pid);
      sprintf(global.tmpout.offset, "%dtmp_offset.fits", (int)pid);


      if( !global.runexpomap ){

	strcpy(skyinstrfile, global.par.inskyinstrfile);
	strcpy(aspecthistofile, global.par.inaspecthistofile);

	if( CheckInputSkyInstrFile(skyinstrfile, global.par.energy, global.par.vignflag) )
	  goto Error;

      }
      else{

	/* Execute expomap task */      
	sprintf(cmd, "nuexpomap infile=%s pixposfile=%s alignfile=%s mastaspectfile=%s attfile=%s teldef=%s instrprobmapfile=%s aberration=%s det1reffile=%s pixbin=%d expomapfile=%s aspecthistofile=%s offsetfile=%s det1instrfile=NONE det2instrfile=NONE skyinstrfile=%s vignflag=%s skyx=%f skyy=%f skysize=%d initseed=%s energy=%f chatter=%d clobber=%s history=%s percent=%f indet2instrfile=NONE",
		global.par.infile, global.par.pixposfile, global.par.alignfile, global.par.mastaspectfile, global.par.attfile, global.par.teldef,
		global.par.instrprobmapfile, aberration, global.par.det1reffile, global.par.pixbin, global.tmpout.expomap, global.tmpout.aspecthisto, 
		global.tmpout.offset, global.tmpout.skyinstrfile, vignflag, skyx, skyy, skysize, initseed, global.par.energy, headas_chatpar, clobber, history,
		global.par.percent);
	headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
	fflush(stdout);
	status = system(cmd);
	if(status!=0){
	  headas_chat(NORMAL, "%s: Error: unable to create temporary Exposure Map file\n", global.taskname);
	  goto Error;
	}

	strcpy(skyinstrfile, global.tmpout.skyinstrfile);
	strcpy(aspecthistofile, global.tmpout.aspecthisto);

      }


      /* Compute EXPOSURE Correction */
      if( ComputeEXPOSURECorrection(aspecthistofile, skyinstrfile, global.par.phafile, &psfcorrinfo, &psfcorrcount) ){
	headas_chat(NORMAL, "%s: Error: unable to compute EXPOSURE correction values.\n", global.taskname);
	goto Error;
      }
      

      /* Create output correction file */
      if(global.createcorrfile)
	{
	  if(WritePsfCorrFile(psfcorrinfo, psfcorrcount, aspecthistofile, global.par.corrfile)){
	    headas_chat(NORMAL, "%s: Error: Unable to create output correction file '%s'\n", global.taskname, global.par.corrfile);
	    goto Error;
	  }
	  
	  headas_chat(NORMAL, "%s: Info: output correction file '%s' created.\n", global.taskname, global.par.corrfile);
	}


      /* Cleanup Temporary Files */
      
      if( (!global.createskyinstrfile) && FileExists(global.tmpout.skyinstrfile)){
	if(remove (global.tmpout.skyinstrfile) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.skyinstrfile);
	}
      }
      
      if(FileExists(global.tmpout.expomap)){
	if(remove (global.tmpout.expomap) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.expomap);
	}
      }
	  
      if(FileExists(global.tmpout.aspecthisto)){
	if(remove (global.tmpout.aspecthisto) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.aspecthisto);
	}
      }
      
      if(FileExists(global.tmpout.offset)){
	if(remove (global.tmpout.offset) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.offset);
	}
      }

    }   /* END EXPOSURE correction -> if(global.par.expoflag && global.par.extended) */



  /* Read Housekeeping input file */
  if( ReadHouseKeeping(global.par.hkfile, &hkinfo, &hknrows) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s file\n", global.taskname, global.par.hkfile);
      goto Error;
    }


  /* Read GTI info from GTI extension of input lcfile */
  if(HDgti_read(global.par.lcfile, &gti, KWVL_EXTNAME_GTI, 0, 0, 0, 0, &status)){
    headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' lcfile.\n", global.taskname, global.par.lcfile);
    goto Error;
  }


  /* Open readonly input light curve file */
  if ((inunit=OpenReadFitsFile(global.par.lcfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.lcfile);
      goto Error;
    }
  
  /* Move in 'RATE' extension in input lcfile */
  if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_RATE, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_RATE);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.lcfile);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.lcfile);
	}
      goto Error;
    }

  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    


  /* Retrieve INSTRUME from input lcfile */  
  if(ExistsKeyWord(&head, KWNM_INSTRUME, &card))
    {
      strcpy(global.lcinfo.instrume, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto Error;
    }

  /* Retrieve TSTART from input lcfile */  
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    {
      global.lcinfo.tstart = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto Error;
    }

  /* Retrieve TIMEDEL from input lcfile */  
  if(ExistsKeyWord(&head, KWNM_TIMEDEL, &card))
    {
      global.lcinfo.timedel = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEDEL);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto Error;
    }

  /* Retrieve TIMEPIXR from input lcfile */  
  if(ExistsKeyWord(&head, KWNM_TIMEPIXR, &card))
    {
      global.lcinfo.timepixr = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEPIXR);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto Error;
    }

  /* Retrieve TIMEZERO from input lcfile */  
  if(ExistsKeyWord(&head, KWNM_TIMEZERO, &card))
    {
      global.lcinfo.timezero = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEZERO);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.lcfile);
      goto Error;
    }

  /* Retrieve NULCCO from input lcfile */
  if(!(ExistsKeyWord(&head, KWNM_NULCCO, NULLNULLCARD)) || !(GetLVal(&head, KWNM_NULCCO)))
    {
      global.lcinfo.nulcco=FALSE;
    }
  else
    {
      global.lcinfo.nulcco=TRUE;
    }
 
  
  /* Get 'RATE' ext number */
  if (!fits_get_hdu_num(inunit, &evExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_RATE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.lcfile);      
      goto Error;
    }


  /* Create output lcfile */
  if ((outunit = OpenWriteFitsFile(global.tmpout.outlcfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(CHATTY, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' temporary file.\n", global.taskname, global.tmpout.outlcfile);
      goto Error;
    }
  
  /* Get number of hdus in input lcfile */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.lcfile);
      goto Error;
    }
  
  /* Copy all extension before 'RATE' extension  from input to output file */
  outExt=1;
    
  while(outExt<evExt && status == OK)
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile);
	  goto Error;
	}
      if(fits_copy_hdu( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.lcfile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpout.outlcfile);
	  goto Error;
	}      

      logical=TRUE;
      if(fits_update_key(outunit, TLOGICAL, KWNM_DEADAPP, &logical, CARD_COMM_DEADAPP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEADAPP);
	  goto Error;
	}

      outExt++;
    }
  
  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, evExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,evExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile);
      goto Error;
    }

 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file.\n", global.taskname, global.par.lcfile);


  /* Compute light curve correction */
  if (ComputeCorrection(inunit, outunit, hkinfo, hknrows, &gti, psfcorrinfo, psfcorrcount))
    {
      headas_chat(NORMAL, "%s: Error: Unable to compute light curve correction.\n", global.taskname);
      goto Error;
    }
  
  if(global.warning)
    goto ok_end;


  /* Set NULCCO keyword to true */
  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_NULCCO, &logical, CARD_COMM_NULCCO, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_NULCCO);
      headas_chat(CHATTY, "%s: Error: in '%s' temporary file.\n", global.taskname, global.tmpout.outlcfile);
      goto Error;
    }

  logical=TRUE;
  if(fits_update_key(outunit, TLOGICAL, KWNM_DEADAPP, &logical, CARD_COMM_DEADAPP, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEADAPP);
      goto Error;
    }
  
  outExt++;


  /* copy any extension after the extension to be operated on */
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile);
	  goto Error;
	}
      if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, global.par.lcfile);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, global.tmpout.outlcfile);
	  goto Error;
	}

      logical=TRUE;
      if(fits_update_key(outunit, TLOGICAL, KWNM_DEADAPP, &logical, CARD_COMM_DEADAPP, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEADAPP);
	  goto Error;
	}
      
      outExt++;
    }


  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, evExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto Error;
    }

  
  /* Update checksum and datasum keywords in all extensions */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, global.par.outlcfile);
      goto Error;
    }
  
  /* close input and output files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.lcfile);
      goto Error;
    }  
  if (CloseFitsFile(outunit)) 
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, global.par.outlcfile);
      goto Error;
    }


  /* rename temporary file into output event file */
  if (RenameFile(global.tmpout.outlcfile,global.par.outlcfile) == -1)
    {
      
      headas_chat(NORMAL, "%s: Error: Unable to rename temporary file.\n", global.taskname);
      goto Error;
    }
  
  headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outlcfile);

  
  /* Free memory */
  
  if(global.par.psfflag && !global.par.extended)
    {
      free(psfcorrinfo);
    }
  
  if(global.par.expoflag && global.par.extended)
    {
      free(psfcorrinfo);
    }

  
 ok_end:
  
  if(global.warning && strcasecmp(global.par.lcfile, global.par.outlcfile) )
    {
      if(CopyFile(global.par.lcfile, global.par.outlcfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to create outlcfile.\n", global.taskname);
	  goto Error;
	}
    }

  return OK; 
  
 Error:

  return NOT_OK;

} /* nulccorr_work */


/*
 *	nulccorr
 *
 *	DESCRIPTION:
 *                 main function
 *               
 *      FUNCTION CALL:
 *             void set_toolversion(char *);
 *             void set_toolname(char *);   
 *             void get_toolnamev(char *);
 *             int headas_chat(int ,char *, ...);
 *             void GetNuSTARDASVersion(Version_t verstr);
 *             void nulccorr_getpar(void);
 * 	       void nulccorr_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 16/10/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nulccorr()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NULCCORR_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nulccorr_getpar() == OK) 
    {
      
      if ( nulccorr_work()) 
	{
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  goto pdcorr_end;
	}
      else if(global.warning)
	{
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  headas_chat(MUTE, "%s: Exit with Warning.\n", global.taskname);
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  if (FileExists(global.tmpout.outlcfile))
	    remove (global.tmpout.outlcfile);
	}
      else
	{
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
	  headas_chat(MUTE,"---------------------------------------------------------------------\n");
	}
    }
  
  return OK;
  
 pdcorr_end:
  
  if (FileExists(global.tmpout.outlcfile))
    remove (global.tmpout.outlcfile);
  return NOT_OK;
  
} /* nulccorr */


/*
 *	nulccorr_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 16/10/12 - First version
 *
 */
void nulccorr_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Light Curve file                    :'%s'\n",global.par.lcfile);
  headas_chat(NORMAL,"Name of the input Housekeeping file                   :'%s'\n",global.par.hkfile);
  headas_chat(NORMAL,"Name of the output Corrected Light Curve file         :'%s'\n",global.par.outlcfile);
  headas_chat(NORMAL,"Name of the input FITS Event file                     :'%s'\n",global.par.infile);
  if(global.runexpomap){
    headas_chat(NORMAL,"Name of the input pixel location file                 :'%s'\n",global.par.pixposfile);
    headas_chat(NORMAL,"Name of the input alignment file                      :'%s'\n",global.par.alignfile);
    headas_chat(NORMAL,"Name of the input Mast Aspect Solution file           :'%s'\n",global.par.mastaspectfile);
    headas_chat(NORMAL,"Name of the input attitude file                       :'%s'\n",global.par.attfile);
    headas_chat(NORMAL,"Name of the input teldef calibration file             :'%s'\n",global.par.teldef);
    headas_chat(NORMAL,"Name of the input Instrument Probability Map file     :'%s'\n",global.par.instrprobmapfile);
    headas_chat(NORMAL,"Name of the input DET1 Reference Point file           :'%s'\n",global.par.det1reffile);
    headas_chat(NORMAL,"Bin size of aspect histogram in pixels                :'%d'\n",global.par.pixbin);
    if (global.par.vignflag)
      headas_chat(NORMAL,"Apply Vignetting correction                           : yes\n");
    else
      headas_chat(NORMAL,"Apply Vignetting correction                           : no\n");
  }
  if( global.par.psfflag && !global.par.extended )
    headas_chat(NORMAL,"Name of the input PSF File                            :'%s'\n",global.par.psffile);
  headas_chat(NORMAL,"Name of the input Vignetting file                     :'%s'\n",global.par.vignfile);
  headas_chat(NORMAL,"Name of the input Optical Axis file                   :'%s'\n",global.par.optaxisfile);
  /* headas_chat(NORMAL,"Name of the output SKY Instrument Map file            :'%s'\n",global.par.skyinstrfile); */
  headas_chat(NORMAL,"Name of the output correction file                    :'%s'\n",global.par.corrfile);
  headas_chat(NORMAL,"Energy value for PSF and vignetting calculation (keV) :'%f'\n",global.par.energy);

  if (global.par.psfflag)
    headas_chat(NORMAL,"Apply PSF correction                                  : yes\n");
  else
    headas_chat(NORMAL,"Apply PSF correction                                  : no\n");

  if (global.par.expoflag)
    headas_chat(NORMAL,"Apply EXPOSURE correction                             : yes\n");
  else
    headas_chat(NORMAL,"Apply EXPOSURE correction                             : no\n");

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");

} /* nulccorr_info */


/*
 *	nulccorr_checkinput
 *
 *	DESCRIPTION:
 *         Check input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *               int strcasecmp(const char *s1, const char *s2);
 *               GetFilenameExtension(char *, char *);
 *               int strcmp(const char *s1, const char *s2);
 *               FileExists(char *);
 *               remove(char *);
 *               pid_t getpid(void);
 *               int sprintf(char *str, const char *format, ...);
 *               char *DeriveFileName(const char *OldName, char *NewName, const char *ext);
 *               char *strcpy(char *, char *);
 *               
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 16/10/12 - First version
 *
 */
int nulccorr_checkinput(void)
{
  int            overwrite=0;
  char           stem[10] , ext[MAXEXT_LEN] ;
  pid_t          pid;
  
  pid=getpid();


  /* Check if outlcfile == NONE */    
  if (!(strcasecmp (global.par.outlcfile, DF_NONE)))
    {
      /* If outlcfile == NONE check if input file is compressed */
      GetFilenameExtension(global.par.lcfile, ext);
      if (!(strcmp(ext, "gz")) || !(strcmp(ext, "Z")))
	{
	  headas_chat(NORMAL, "%s: Error: %s\n", global.taskname, global.par.lcfile);
	  headas_chat(NORMAL, "%s: Error: input file is compressed, cannot update it.\n", global.taskname);
	  goto check_end;
	}
      else
	/* Overwrite input file */
	overwrite=1;      
    }
  else
    {
      if(FileExists(global.par.outlcfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outlcfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outlcfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outlcfile);
	      if( strcmp(global.par.outlcfile,global.par.lcfile) ){
		if(remove (global.par.outlcfile) == -1)
		  {
		    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outlcfile);
		    goto check_end;
		  }
	      }
	    }
	}
    }

  /* Derive temporary Corrected Light Curve filename */
  sprintf(stem, "%dtmp", (int)pid);
  if (overwrite)
    {
      DeriveFileName(global.par.lcfile, global.tmpout.outlcfile, stem);
      strcpy(global.par.outlcfile, global.par.lcfile);
    }
  else
    DeriveFileName(global.par.outlcfile, global.tmpout.outlcfile, stem);

  if(FileExists(global.tmpout.outlcfile))
    if(remove (global.tmpout.outlcfile) == -1)
      {
	headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.outlcfile);
	goto check_end;
      }


  /* Check if skyinstrfile == NONE */    
  if (!(strcasecmp (global.par.skyinstrfile, DF_NONE)))
    {
      global.createskyinstrfile = FALSE;

      /* Derive temporary file name */
      sprintf(global.tmpout.skyinstrfile, "%dtmp_sky.fits", (int)pid);
    }
  else
    {
      global.createskyinstrfile = TRUE;

      strcpy(global.tmpout.skyinstrfile, global.par.skyinstrfile);

      if(FileExists(global.tmpout.skyinstrfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.skyinstrfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.skyinstrfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.skyinstrfile);
	      if(remove (global.tmpout.skyinstrfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.skyinstrfile);
		  goto check_end;
		}
	    }
	}
    }


  /* Check if corrfile == NONE */    
  if (!(strcasecmp (global.par.corrfile, DF_NONE)))
    {
      global.createcorrfile = FALSE;

      /* Derive temporary file name */
      sprintf(global.tmpout.corrfile, "%dtmp_corr.fits", (int)pid);
    }
  else
    {
      global.createcorrfile = TRUE;

      strcpy(global.tmpout.corrfile, global.par.corrfile);

      if(FileExists(global.tmpout.corrfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.corrfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.corrfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.corrfile);
	      if(remove (global.tmpout.corrfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.corrfile);
		  goto check_end;
		}
	    }
	}
    }


  /* Check consistency of 'inskyinstrfile' and 'inaspecthistofile' input parameters */
  if( ( strcasecmp(global.par.inskyinstrfile, DF_NONE) && !strcasecmp(global.par.inaspecthistofile, DF_NONE) ) ||
      ( !strcasecmp(global.par.inskyinstrfile, DF_NONE) && strcasecmp(global.par.inaspecthistofile, DF_NONE) ) )
    {
      headas_chat(NORMAL, "%s: Error: 'inskyinstrfile' is not consistent with 'inaspecthistofile' input parameter.\n", global.taskname);
      goto check_end;  
    }

  
  if( (!global.par.extended) && global.par.vignflag && (!global.par.psfflag) )
    {
      headas_chat(NORMAL, "%s: Error: Can not apply vignetting correction if 'psfflag=no'.\n", global.taskname);
      goto check_end;  
    }

  if( global.par.extended && global.par.vignflag && (!global.par.expoflag) )
    {
      headas_chat(NORMAL, "%s: Error: Can not apply vignetting correction if 'expoflag=no'.\n", global.taskname);
      goto check_end;  
    }


 
  return OK;

 check_end:
  return NOT_OK;
}


/*
 *
 *      ReadHouseKeeping
 *
 *	DESCRIPTION:
 *           Routine to read Housekeeping input file
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadHouseKeeping(char *filename, HKRow_t **hkinfo, int *hknrows)
{
  unsigned           FromRow, ReadRows, n, nCols;
  int                hkcount=0, status=OK;
  HKCol_t            hkcol;
  Bintable_t	     hktable;
  FitsHeader_t	     hkhead;
  FitsFileUnit_t     hkunit=NULL;

  TMEMSET0( &hktable, Bintable_t );
  TMEMSET0( &hkhead, FitsHeader_t );

 
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, filename);

  /* Open readonly input hk file */
  if ((hkunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadHouseKeeping_end;
    }
 
  /* Move in HK1FPM extension in input hk file */
  if (fits_movnam_hdu(hkunit, ANY_HDU, KWVL_EXTNAME_HK1FPM, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_HK1FPM);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      if( CloseFitsFile(hkunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto ReadHouseKeeping_end;
    }
  
  /* Retrieve header pointer */
  hkhead=RetrieveFitsHeader(hkunit);

  /* Read bintable */
  GetBintableStructure(&hkhead, &hktable, BINTAB_ROWS, 0, NULL);
  nCols=hktable.nColumns;

  if(!hktable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadHouseKeeping_end;
    }


  /* Get needed columns number from name */

  /* Time */
  if ((hkcol.TIME=ColNameMatch(CLNM_TIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadHouseKeeping_end;
    }

  /* LIVETIME */
  if ((hkcol.LIVETIME=ColNameMatch(CLNM_LIVETIME, &hktable)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LIVETIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadHouseKeeping_end;
    }


  EndBintableHeader(&hkhead, &hktable);
 

  /* Allocate memory to storage all data */
  *hknrows = hktable.MaxRows;
  *hkinfo = (HKRow_t *)calloc(*hknrows, sizeof(HKRow_t));
  if(*hkinfo==NULL){
    headas_chat(CHATTY,"%s: Error: ReadHouseKeeping: memory allocation failure.\n", global.taskname);
    goto ReadHouseKeeping_end;
  }
  

  /* Read blocks of bintable rows */
  FromRow = 1;
  ReadRows=hktable.nBlockRows;
  while(ReadBintable(hkunit, &hktable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  (*hkinfo)[hkcount].Time = DVEC(hktable,n,hkcol.TIME);
	  (*hkinfo)[hkcount].LiveTime = DVEC(hktable,n,hkcol.LIVETIME);	  
	  
	  hkcount++;
	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;

    }/* while */ 


  return OK;
  
 ReadHouseKeeping_end:
  if (hkhead.first)
    ReleaseBintable(&hkhead, &hktable);
  
  return NOT_OK;

} /* ReadHouseKeeping */


/*
 *
 *      ComputeCorrection
 *
 *	DESCRIPTION:
 *           Routine to compute light curve correction and update 'RATE' bintable.
 *                            
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *               FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit);
 *               void GetBintableStructure (FitsHeader_t *header, Bintable_t *table,
 *     			                    const int MaxBlockRows, const unsigned nColumns,
 *  					    const unsigned ActCols[]);
 *               int ColNameMatch(const char *ColName, const Bintable_t *table);
 *               void AddColumn(FitsHeader_t *header, Bintable_t *table, const char *ttype,
 * 			        const char *TypeComment, const char *tform, const unsigned present, ...);
 *               void GetGMTDateTime(char *);
 *               int sprintf(char *str, const char *format, ...);
 *               void AddHistory(FitsHeader_t *header, const char *Comment);
 *               void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table);
 *               void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header, Bintable_t *table);
 *               int  ReadBintable(const FitsFileUnit_t unit, Bintable_t *table, const unsigned nColumns,
 *				   const unsigned ActCols[], const unsigned FromRow, unsigned *nRows);
 *               int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows, BOOL last);
 *               unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t *table);
 *               int fits_get_colnum(FitsFileUnit_t unit, int casesen, char *templt, int *colnum,
 *                                   int *status);
 *               int fits_delete_col(FitsFileUnit_t unit, int colnum, int *status);
 *               
 *
 *      CHANGE HISTORY:
 *        0.1.0: - NS 16/10/12 - First version
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeCorrection(FitsFileUnit_t evunit, FitsFileUnit_t ounit, HKRow_t *hkinfo, int hknrows, struct gti_struct *gti, PsfCorrInfo_t *psfcorrinfo, int psfcorrcount)
{
  unsigned           FromRow, ReadRows, n,nCols, OutRows=0;  
  int                totrows;
  double             time;
  double             rate, error, corrfac;
  LcCol_t            indxcol;
  Bintable_t	     outtable;
  FitsHeader_t	     head;
  
  
  TMEMSET0( &outtable, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(evunit);
  
  GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);
  
  if(!outtable.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.lcfile);
      global.warning=1;
      return OK;
    }
  
  nCols=outtable.nColumns;
  totrows=outtable.MaxRows;


  /* TIME */
  if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_TIME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile); 
      goto ComputeCorrection_end;
    }

  /* RATE */
  if ((indxcol.RATE=ColNameMatch(CLNM_RATE, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RATE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile); 
      goto ComputeCorrection_end;
    }

  /* ERROR */
  if ((indxcol.ERROR=ColNameMatch(CLNM_ERROR, &outtable)) == -1)
    { 
      headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_ERROR);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile); 
      goto ComputeCorrection_end;
    }


  /* Add new columns if needed */
  
  if(global.lcinfo.nulcco)
    {
      headas_chat(NORMAL, "%s: Info: %s keyword set to TRUE\n", global.taskname, KWNM_NULCCO);
      headas_chat(NORMAL, "%s: Info: '%s' and '%s' columns will be overwritten\n", global.taskname, CLNM_RATE, CLNM_ERROR);


      if((indxcol.RATE_ORIG=ColNameMatch(CLNM_RATE_ORIG, &outtable)) == -1)
	{ 
	  headas_chat(NORMAL, "%s: Error: %s keyword set to TRUE\n", global.taskname, KWNM_NULCCO);
	  headas_chat(NORMAL, "%s: Error: but %s column does not exist\n",  global.taskname, CLNM_RATE_ORIG);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile);
	  goto ComputeCorrection_end;
	}

      if((indxcol.ERROR_ORIG=ColNameMatch(CLNM_ERROR_ORIG, &outtable)) == -1)
	{ 
	  headas_chat(NORMAL, "%s: Error: %s keyword set to TRUE\n", global.taskname, KWNM_NULCCO);
	  headas_chat(NORMAL, "%s: Error: but %s column does not exist\n",  global.taskname, CLNM_ERROR_ORIG);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.lcfile);
	  goto ComputeCorrection_end;
	}
    }
  else
    { 
      headas_chat(NORMAL, "%s: Info: %s keyword not found or set to FALSE\n", global.taskname,KWNM_NULCCO);
      

      if((indxcol.RATE_ORIG=ColNameMatch(CLNM_RATE_ORIG, &outtable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_RATE_ORIG);
	  headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outlcfile);
	  AddColumn(&head, &outtable, CLNM_RATE_ORIG, CARD_COMM_RATE_ORIG, "1E", TNONE);
	  indxcol.RATE_ORIG=ColNameMatch(CLNM_RATE_ORIG, &outtable);
	}
      else
	{
	  headas_chat(NORMAL, "%s: Info: existing column %s will be overwritten\n", global.taskname, CLNM_RATE_ORIG);
	}
      
      if((indxcol.ERROR_ORIG=ColNameMatch(CLNM_ERROR_ORIG, &outtable)) == -1)
	{
	  headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_ERROR_ORIG);
	  headas_chat(NORMAL, "%s: Info: in '%s' file.\n", global.taskname, global.par.outlcfile);
	  AddColumn(&head, &outtable, CLNM_ERROR_ORIG, CARD_COMM_ERROR_ORIG, "1E", TNONE);
	  indxcol.ERROR_ORIG=ColNameMatch(CLNM_ERROR_ORIG, &outtable);
	}
      else
	{
	  headas_chat(NORMAL, "%s: Info: existing column %s will be overwritten\n", global.taskname, CLNM_RATE_ORIG);
	}
    }


  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s: light curve corrected.", global.taskname, global.nustardas_v,global.date );
      AddHistory(&head, global.strhist);
    }
  
  EndBintableHeader(&head, &outtable); 
  
  /* write out new header to new file */
  FinishBintableHeader(ounit, &head, &outtable);



  FromRow = 1;
  ReadRows=outtable.nBlockRows;
  OutRows = 0;

  /* Read input bintable */
  while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows; ++n)
	{

	  /* Copy RATE and ERROR values in RATE_ORIG and ERROR_ORIG columns  */

	  if(!global.lcinfo.nulcco)
	    {
	      EVEC(outtable,n,indxcol.RATE_ORIG) = EVEC(outtable,n,indxcol.RATE);
	      EVEC(outtable,n,indxcol.ERROR_ORIG) = EVEC(outtable,n,indxcol.ERROR);
	    }


	  time = DVEC(outtable,n,indxcol.TIME);
	  rate = EVEC(outtable,n,indxcol.RATE_ORIG);
	  error = EVEC(outtable,n,indxcol.ERROR_ORIG);

	  if(rate>0)
	    {
	      /* Compute livetime correction factor */
	      
	      if( ComputeCorrFac(hkinfo, hknrows, gti, time, &corrfac) )
		{
		  headas_chat(NORMAL, "%s: Error: Unable to compute correction factor for TIME=%f\n", global.taskname, time);
		  goto ComputeCorrection_end;
		}
	      
	      /* Apply livetime correction factor */
	      
	      rate = rate*corrfac;
	      error = error*corrfac;


	      if( (global.par.psfflag && !global.par.extended) || (global.par.expoflag && global.par.extended) )
		{
		  /* Compute psf/exposure correction factor */
		  
		  if( ComputePsfCorrFac(psfcorrinfo, psfcorrcount, gti, time, &corrfac) )
		    {
		      headas_chat(NORMAL, "%s: Error: Unable to compute psf/exposure correction factor for TIME=%f\n", global.taskname, time);
		      goto ComputeCorrection_end;
		    }
		  
		  /* Apply psf/exposure correction factor */
		  
		  rate = rate*corrfac;
		  error = error*corrfac;
		}

	    }


	  /* Update RATE and ERROR columns with corrected values */

	  EVEC(outtable,n,indxcol.RATE) = (ETYPE)rate;
	  EVEC(outtable,n,indxcol.ERROR) = (ETYPE)error;


	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(ounit, &outtable, OutRows, FALSE);
	      OutRows=0;
	    }
	  
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  WriteFastBintable(ounit, &outtable, OutRows, TRUE);
  ReleaseBintable(&head, &outtable);



  return OK;
  
 ComputeCorrection_end:
  if (head.first)
    ReleaseBintable(&head, &outtable);
  
  return NOT_OK;
  
} /* ComputeCorrection */


int ComputeCorrFac(HKRow_t *hkinfo, int hknrows, struct gti_struct *gti, double time_in, double *corrfac){

  int               i=0, j=0, k=0, status=OK;
  double            time, timestop, weight, livetime_mean, corr, hkbin=1;
  double            sum_livetime=0., sum_weight=0.;
  double            dtime_gti=0;


  /* LC bin time */
  time = time_in + global.lcinfo.timezero - (global.lcinfo.timedel*global.lcinfo.timepixr);

  /* LC bin end time */
  timestop = time + global.lcinfo.timedel;


  /* Search the first HK row with the nearest Time before 'time' value */
  FindClosestHkIndex(hkinfo, hknrows, time, &i);
  
  if(hkinfo[i].Time > time){
    headas_chat(NORMAL, "%s: Error: LC TIME=%f not included in housekeeping temporal range\n", global.taskname, time);
    goto ComputeCorrFac_end;
  }  

  
  /* Search the first HK row with Time over 'timestop' value */
  for(j=i; j<hknrows; j++)
    {
      if( hkinfo[j].Time > timestop )
	break;
    }
  j--;  /* HK row with the nearest Time before 'timestop' value */
  

/*   /\* Debug only *\/ */
/*   if(time<global.lcinfo.tstart+5*global.lcinfo.timedel){ */
/*     headas_chat(NORMAL, "DEBUG: HK START ROW TIME = %f HK START ROW = %d  HK STOP ROW = %d\n", hkinfo[i].Time, i+1, j+1); */
/*   } */

  
  /* Compute LiveTime mean */

  for(k=i; k<=j; k++)
    {
      if(i==j)  /* only one HK row */
	{
	  weight = 1.;
	}
      else if(k==i)  /* first HK bin */
	{
	  weight = hkbin - ( time - hkinfo[k].Time ) ;
	}
      else if(k==j)  /* last HK bin */
	{
	  weight = timestop - hkinfo[k].Time ;
	}
      else
	{
	  weight = 1.;
	}
      

/*       /\* Debug only *\/ */
/*       if(time<global.lcinfo.tstart+5*global.lcinfo.timedel){ */
/* 	headas_chat(NORMAL, "DEBUG: livetime=%f weight=%f \n", hkinfo[k].LiveTime, weight); */
/*       } */
      
      /* Compute overlap exposure of the HK time interval with GTI */
      dtime_gti = HDgti_exp(hkinfo[k].Time, hkinfo[k].Time+hkbin, gti, &status);
      if(status!=OK){
	headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the hk time=%f with GTI\n", global.taskname, hkinfo[k].Time);
	goto ComputeCorrFac_end;
      }
      
      /* HK bin included in GTI time interval */
      if( dtime_gti>0 )
	{
	  sum_livetime += weight*hkinfo[k].LiveTime ;
	  sum_weight += weight ;

	  if(!(hkinfo[k].LiveTime>0))
	    headas_chat(CHATTY, "%s: Warning: HK LiveTime = 0 for TIME=%f\n", global.taskname, time);
	}
      else
	{
/* 	  /\* Debug only *\/ */
/* 	  if(time<global.lcinfo.tstart+5*global.lcinfo.timedel) */
/* 	    headas_chat(NORMAL, "DEBUG: intersection with GTI zero\n"); */

	  if(!(hkinfo[k].LiveTime>0))
	    headas_chat(CHATTY, "%s: Warning: HK LiveTime = 0 for TIME=%f (GTI zero)\n", global.taskname, time);
	}

      if(!(hkinfo[k].LiveTime>0))
	headas_chat(CHATTY, "%s: Warning: HK LiveTime = 0 for TIME=%f\n", global.taskname, time);
      
    }

  
  if(!(sum_livetime>0))
    headas_chat(CHATTY, "%s: Warning: LiveTime Sum = 0  for TIME=%f\n", global.taskname, time);
  
  if(!(sum_weight>0))
    headas_chat(CHATTY, "%s: Warning: Weight Sum = 0  for TIME=%f\n", global.taskname, time);
  
  
  livetime_mean = (sum_weight>0) ? (sum_livetime/sum_weight) : 0 ;
  corr = (livetime_mean>0) ? (1/livetime_mean) : 0 ;
  

/*   /\* Debug only *\/ */
/*   if(time<global.lcinfo.tstart+5*global.lcinfo.timedel){ */
/*     headas_chat(NORMAL, "DEBUG: sum_livetime=%f sum_weight=%f \n", sum_livetime, sum_weight); */
/*     headas_chat(NORMAL, "DEBUG: livetime_mean=%f corr=%f\n",livetime_mean,corr); */
/*   } */

  *corrfac = corr;
  
  return OK;
  
 ComputeCorrFac_end:
  
  return NOT_OK;
   
} /* ComputeCorrFac */


void FindClosestHkIndex(HKRow_t *info, int nrows, double time, int *index){
  
  int      i=0, low=0, high=0, mid;
  
  /* Find appropriate row ( after loop index 'i' indicates the first row over 'time' value) */
  low = 0;
  high = nrows-1;
  
  while (low != high) {
    mid = low + (high-low)/2;
    if (info[mid].Time <= time) {
      /* This index, and everything below it, must not be the first element
       * greater than what we're looking for because this element is no greater
       * than the element.
       */
      low = mid + 1;
    }
    else {
      /* This element is at least as large as the element, so anything after it can't
       * be the first element that's at least as large.
       */
      high = mid;
    }
  }
  i = low;

  /* index should contains nearest row under 'time' value */
  if( (i>0) && (info[i].Time>time) ){
    i--;
  }

  *index = i;
   
} /* FindClosestHkIndex */


void FindClosestOptAxisIndex(OptAxisInfo_t *info, int nrows, double time, int *index){
  
  int      i=0, low=0, high=0, mid;
  
  /* Find appropriate row ( after loop index 'i' indicates the first row over 'time' value) */
  low = 0;
  high = nrows-1;
  
  while (low != high) {
    mid = low + (high-low)/2;
    if (info[mid].time <= time) {
      /* This index, and everything below it, must not be the first element
       * greater than what we're looking for because this element is no greater
       * than the element.
       */
      low = mid + 1;
    }
    else {
      /* This element is at least as large as the element, so anything after it can't
       * be the first element that's at least as large.
       */
      high = mid;
    }
  }
  i = low;

  /* index should contains nearest row under 'time' value */
  if( (i>0) && (info[i].time>time) ){
    i--;
  }

  *index = i;
   
} /* FindClosestOptAxisIndex */


void FindClosestPsfCorrInfoIndex(PsfCorrInfo_t *info, int nrows, double time, int *index){
  
  int      i=0, low=0, high=0, mid;
  
  /* Find appropriate row ( after loop index 'i' indicates the first row over 'time' value) */
  low = 0;
  high = nrows-1;
  
  while (low != high) {
    mid = low + (high-low)/2;
    if (info[mid].tstart <= time) {
      /* This index, and everything below it, must not be the first element
       * greater than what we're looking for because this element is no greater
       * than the element.
       */
      low = mid + 1;
    }
    else {
      /* This element is at least as large as the element, so anything after it can't
       * be the first element that's at least as large.
       */
      high = mid;
    }
  }
  i = low;

  /* index should contains nearest row under 'time' value */
  if( (i>0) && (info[i].tstart>time) ){
    i--;
  }

  *index = i;
   
} /* FindClosestPsfCorrInfoIndex */


/* ReadRegionInfo */
int ReadRegionInfo(char *filename, char *extname, RegionExtInfo_t *reginfo)
{
  int                   status=OK; 
  char                  shape[17], shape0[17];
  double                rotang;
  RegionExtCol_t        incol;
  unsigned              FromRow, ReadRows, n, nCols;
  Bintable_t	        table;
  FitsHeader_t          inhead;            /* Extension header pointer */
  FitsFileUnit_t        inunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &inhead, FitsHeader_t );


  /* Open read only input event file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadRegionInfo_end;
    }
  
  /* Move to <extname> extension */

  if (fits_movnam_hdu(inunit, ANY_HDU, extname, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find REGION extension '%s' in\n", global.taskname, extname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      /* Close input event file */
      if (CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);  
	}
      goto ReadRegionInfo_end;
    }
    

  /* Get extension header pointer  */
  inhead=RetrieveFitsHeader(inunit);  


  GetBintableStructure(&inhead, &table, BINTAB_ROWS, 0, NULL);

  nCols=table.nColumns;  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadRegionInfo_end;
    }

  /* Get needed columns number from name */
  
  if ((incol.X=ColNameMatch(CLNM_X, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadRegionInfo_end;
    }
  
  if ((incol.Y=ColNameMatch(CLNM_Y, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadRegionInfo_end;
    }
  
  if ((incol.SHAPE=ColNameMatch(CLNM_SHAPE, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SHAPE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadRegionInfo_end;
    }
  
  if ((incol.R=ColNameMatch(CLNM_R, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_R);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadRegionInfo_end;
    }
  
  if ((incol.ROTANG=ColNameMatch(CLNM_ROTANG, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ROTANG);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadRegionInfo_end;
    }
  
  if ((incol.COMPONENT=ColNameMatch(CLNM_COMPONENT, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_COMPONENT);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadRegionInfo_end;
    }

  EndBintableHeader(&inhead, &table);
 
  FromRow = 1;
  ReadRows=table.nBlockRows;
  
  while(ReadBintable(inunit, &table, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  strcpy(shape, SVEC(table, n, incol.SHAPE)); 
	  
	  headas_chat(CHATTY,"%s: Info: shape '%s'\n", global.taskname, shape); 

	  if(n==0)
	    {
	      strcpy(shape0, shape);

	      reginfo->X[0]= DVEC(table, n, incol.X) ;
	      reginfo->Y[0]= DVEC(table, n, incol.Y) ;


	      if(!strcmp(shape, "CIRCLE")) 
		{
		  strcpy(reginfo->shape, "CIRCLE");
		  
		  if(table.Multiplicity[incol.R]!=1)
		    {
		      headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname,table.Multiplicity[incol.R] );
		      headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		      goto ReadRegionInfo_end;
		    }
	  
		  reginfo->R[0]=DVEC(table, n, incol.R);
		  	  
		}
	      else if(!strcmp(shape, "ANNULUS"))
		{
		  strcpy(reginfo->shape, "ANNULUS");

		  if(table.Multiplicity[incol.R]!= 2)
		    {
		      headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname, table.Multiplicity[incol.R] );
		      headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		      goto ReadRegionInfo_end;
		    }
		  reginfo->R[0]  = DVECVEC(table,n,incol.R,0);
		  reginfo->R1[0] = DVECVEC(table,n,incol.R,1);
		  	  
		}
	      else if(!strcmp(shape, "ELLIPSE"))
		{
		  strcpy(reginfo->shape, "ELLIPSE");

		  if(table.Multiplicity[incol.R]!= 2)
		    {
		      headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname, table.Multiplicity[incol.R] );
		      headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		      goto ReadRegionInfo_end;
		    }
		  reginfo->R[0]  = DVECVEC(table,n,incol.R,0);
		  reginfo->R1[0] = DVECVEC(table,n,incol.R,1);
		  reginfo->rotang[0] = DVEC(table,n,incol.ROTANG);
		  	  
		}
	      else if(!strcmp(shape, "BOX"))
		{
		  strcpy(reginfo->shape, "BOX");

		  rotang=DVEC(table,n,incol.ROTANG);

		  reginfo->rotang[0]=rotang;

		  if(table.Multiplicity[incol.R]!= 2)
		    {
		      headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname,table.Multiplicity[incol.R] );
		      headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		      goto ReadRegionInfo_end;
		    }
		  
		  reginfo->R1[0]  = DVECVEC(table,n,incol.R,0);
		  reginfo->R[0] = DVECVEC(table,n,incol.R,1);
	  
		}
	      else
		{
		  headas_chat(NORMAL, "%s: Error: shape not yet implemented\n", global.taskname);
		  goto ReadRegionInfo_end;
		}
	    }
	  else if(n==1 && !strcmp(shape, "!CIRCLE") && !strcmp(shape0, "CIRCLE"))
	    {
	      strcpy(reginfo->shape, "CIRCLE-CIRCLE");

	      reginfo->X[1]= DVEC(table, n, incol.X) ;
	      reginfo->Y[1]= DVEC(table, n, incol.Y) ;

	      if(table.Multiplicity[incol.R]!=1)
		{
		  headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname,table.Multiplicity[incol.R] );
		  headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		  goto ReadRegionInfo_end;
		}
	      
	      reginfo->R[1]=DVEC(table, n, incol.R);
		  	    
	    }
	  else if(n==1 && !strcmp(shape, "!BOX") && !strcmp(shape0, "BOX"))
	    {
	      strcpy(reginfo->shape, "BOX-BOX");

	      reginfo->X[1]= DVEC(table, n, incol.X) ;
	      reginfo->Y[1]= DVEC(table, n, incol.Y) ;

	      rotang=DVEC(table,n,incol.ROTANG);
	      
	      reginfo->rotang[1]=rotang;
	      
	      if(table.Multiplicity[incol.R]!= 2)
		{
		  headas_chat(NORMAL, "%s: Error: '%s' multiplicity column is %d\n", global.taskname,table.Multiplicity[incol.R] );
		  headas_chat(NORMAL, "%s: Error: but region shape is '%s'. \n", global.taskname, shape);
		  goto ReadRegionInfo_end;
		}
	      
	      reginfo->R1[1]  = DVECVEC(table,n,incol.R,0);
	      reginfo->R[1] = DVECVEC(table,n,incol.R,1);
	      
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: This region shape is not yet implemented\n", global.taskname);
	      goto ReadRegionInfo_end;
	    }
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 

  
  ReleaseBintable(&inhead, &table);


  return OK; 
  
 ReadRegionInfo_end:
  return NOT_OK;


}/* ReadRegionInfo */


int ReadOptAxisFile(char *filename, OptAxisInfo_t ** optinfo, int *optcount, OptAxisKeys_t *optkeys){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  OptAxisCol_t       indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsCard_t         *card;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, filename);
  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
 
  /* Move in OPTICAL_AXIS extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_OPTICAL_AXIS, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OPTICAL_AXIS);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto ReadOptAxisFile_end;
    }

  /* Retrieve header pointer */     
  head=RetrieveFitsHeader(unit);

  /* Retrieve INSTRUME */
  if(ExistsKeyWord(&head, KWNM_INSTRUME, &card))
    {
      strcpy(optkeys->instrume, card->u.SVal);
    }
  else
    {
      strcpy(optkeys->instrume, "\0");
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  /* Retrieve TSTART */
  if(ExistsKeyWord(&head, KWNM_TSTART, &card))
    {
      optkeys->tstart = card->u.DVal;
      strcpy(optkeys->tstart_comm, card->Comment);
    }
  else
    {
      optkeys->tstart = 0.0;
      strcpy(optkeys->tstart_comm, "\0");
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_TSTART);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  /* Retrieve TSTOP */
  if(ExistsKeyWord(&head, KWNM_TSTOP, &card))
    {
      optkeys->tstop = card->u.DVal;
      strcpy(optkeys->tstop_comm, card->Comment);
    }
  else
    {
      optkeys->tstop = 0.0;
      strcpy(optkeys->tstop_comm, "\0");
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }


/*   /\* Check if optaxisfile is appropriate *\/ */

/*   if( global.obsinfo.tstart<optkeys->tstart || global.obsinfo.tstop>optkeys->tstop ) */
/*     headas_chat(NORMAL, "%s: Warning: pha file time range not covered by %s time range (check TSTART and/or TSTOP)\n", global.taskname, filename); */
  
/*   if( strcasecmp(global.obsinfo.instrume,optkeys->instrume) ) */
/*     headas_chat(NORMAL, "%s: Warning: INSTRUME keywords of %s file and pha file are not consistent\n", global.taskname, filename); */


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadOptAxisFile_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto ReadOptAxisFile_end;
    }

  if ((indxcol.X_OA = GetColNameIndx(&table, CLNM_X_OA)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_OA);
      goto ReadOptAxisFile_end;
    }

  if ((indxcol.Y_OA = GetColNameIndx(&table, CLNM_Y_OA)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_OA);
      goto ReadOptAxisFile_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *optcount = table.MaxRows;
 *optinfo = (OptAxisInfo_t *)calloc(*optcount, sizeof(OptAxisInfo_t));
 if(*optinfo==NULL){
   headas_chat(CHATTY,"%s: Error: ReadOptAxisFile: memory allocation failure.\n", global.taskname);
   goto ReadOptAxisFile_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<*optcount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {
	 (*optinfo)[count].time = DVEC(table,n,indxcol.TIME);
	 (*optinfo)[count].x_oa = DVEC(table,n,indxcol.X_OA);
	 (*optinfo)[count].y_oa = DVEC(table,n,indxcol.Y_OA);

	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 
   
  *optcount = count;


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      return NOT_OK;
    }

  return OK;

  
 ReadOptAxisFile_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadOptAxisFile  */


int ReadAspectHistoExt(char *filename, AspHistoInfo_t **histoinfo, int *histocount){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  AspHistoCol_t      indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
 
  /* Move in ASP_HIST extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_ASP_HIST, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_ASP_HIST);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto ReadAspectHistoExt_end;
    }
  
  /* Retrieve header pointer */
  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadAspectHistoExt_end;
    }

  /* Get needed columns number from name */
  if ((indxcol.X_BIN = GetColNameIndx(&table, CLNM_X_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_BIN);
      goto ReadAspectHistoExt_end;
    }

  if ((indxcol.Y_BIN = GetColNameIndx(&table, CLNM_Y_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_BIN);
      goto ReadAspectHistoExt_end;
    }

  if ((indxcol.TSTART = GetColNameIndx(&table, CLNM_TSTART)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TSTART);
      goto ReadAspectHistoExt_end;
    }

  if ((indxcol.TSTOP = GetColNameIndx(&table, CLNM_TSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TSTOP);
      goto ReadAspectHistoExt_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *histocount = table.MaxRows;
 *histoinfo = (AspHistoInfo_t *)calloc(*histocount, sizeof(AspHistoInfo_t));
 if(*histoinfo==NULL){
   headas_chat(CHATTY,"%s: Error: ReadAspectHistoExt: memory allocation failure.\n", global.taskname);
   goto ReadAspectHistoExt_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<*histocount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {
	 (*histoinfo)[count].x_bin = JVEC(table,n,indxcol.X_BIN);
	 (*histoinfo)[count].y_bin = JVEC(table,n,indxcol.Y_BIN);
	 (*histoinfo)[count].tstart = DVEC(table,n,indxcol.TSTART);
	 (*histoinfo)[count].tstop = DVEC(table,n,indxcol.TSTOP);

	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 
   
  *histocount = count;


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      return NOT_OK;
    }


  return OK;
  
 ReadAspectHistoExt_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

} /* ReadAspectHistoExt */


int ReadAspectHistoCompExt(char *filename, AspHistoCompBINInfo_t **histocmpinfo, int *histocmpcount){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  AspHistoCompCol_t  indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
 
  /* Move in ASP_HIST_COMP extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_ASP_HIST_COMP, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_ASP_HIST_COMP);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto ReadAspectHistoCompExt_end;
    }
  
  /* Retrieve header pointer */
  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadAspectHistoCompExt_end;
    }

  /* Get needed columns number from name */
  if ((indxcol.X_BIN = GetColNameIndx(&table, CLNM_X_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_BIN);
      goto ReadAspectHistoCompExt_end;
    }

  if ((indxcol.Y_BIN = GetColNameIndx(&table, CLNM_Y_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_BIN);
      goto ReadAspectHistoCompExt_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *histocmpcount = table.MaxRows;
 *histocmpinfo = (AspHistoCompBINInfo_t *)calloc(*histocmpcount, sizeof(AspHistoCompBINInfo_t));
 if(*histocmpinfo==NULL){
   headas_chat(CHATTY,"%s: Error: ReadAspectHistoCompExt: memory allocation failure.\n", global.taskname);
   goto ReadAspectHistoCompExt_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<*histocmpcount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {
	 (*histocmpinfo)[count].x_bin = JVEC(table,n,indxcol.X_BIN);
	 (*histocmpinfo)[count].y_bin = JVEC(table,n,indxcol.Y_BIN);

	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 
   
  *histocmpcount = count;


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      return NOT_OK;
    }


  return OK;
  
 ReadAspectHistoCompExt_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

} /* ReadAspectHistoCompExt */


int ComputePSFCorrection(char *aspecthistofile, char *skyinstrfile, char *psffile, double skyx, double skyy, RegionExtInfo_t *reginfo, PsfCorrInfo_t **corrinfo, int *corrcount){

  int                i=0, j=0, iopt=0, iphi, itheta;
  double             *phi_array, *theta_array, *psf_frac_off;
  OptAxisInfo_t      *optinfo=NULL;
  OptAxisKeys_t      optkeys;
  int                optcount;
  AspHistoInfo_t     *histoinfo=NULL;
  int                histocount;
  AspHistoCompBINInfo_t   *histobininfo=NULL;
  int                     histobincount;


  /* Retrieve Optical Axis info from input optaxisfile*/
  if( ReadOptAxisFile(global.par.optaxisfile, &optinfo, &optcount, &optkeys) )
    {
      headas_chat(NORMAL, "%s: Error: unable to read Optical Axis file '%s'.\n", global.taskname, global.par.optaxisfile);
      goto ComputePSFCorrection_end;
    }


  /* Read Aspect Histogram info */
  if( ReadAspectHistoExt(aspecthistofile, &histoinfo, &histocount) ){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, aspecthistofile);
    goto ComputePSFCorrection_end;
  }


  /* Allocate memory to storage PSF correction data */
  *corrcount = histocount;
  *corrinfo = (PsfCorrInfo_t *)calloc(*corrcount, sizeof(PsfCorrInfo_t));
  if(*corrinfo==NULL){
    headas_chat(CHATTY,"%s: Error: ComputePSFCorrection: memory allocation failure.\n", global.taskname);
    goto ComputePSFCorrection_end;
  }


  /* Compute OFFAXIS (theta) and AZIMUTH (phi) values */
  for(i=0; i<histocount; i++){

    (*corrinfo)[i].tstart = histoinfo[i].tstart;
    (*corrinfo)[i].tstop = histoinfo[i].tstop;

    /* Search the first Optical Axis row with the nearest time before 'histoinfo[i].tstart' value */
    FindClosestOptAxisIndex(optinfo, optcount, histoinfo[i].tstart, &iopt);
    if( optinfo[iopt].time > histoinfo[i].tstart ){
      headas_chat(NORMAL, "%s: Error: Aspect Histogram TSTART=%f not included in Optical Axis temporal range\n", global.taskname, histoinfo[i].tstart);
      goto ComputePSFCorrection_end;
    }

    (*corrinfo)[i].theta = sqrt( pow((skyx - optinfo[iopt].x_oa),2) + pow((skyy - optinfo[iopt].y_oa),2) ) * SUBPIX_SIZE_ARCMIN;
    (*corrinfo)[i].phi = NormalaziedAt360Angle( atan2((skyy-optinfo[iopt].y_oa),(skyx-optinfo[iopt].x_oa)) * (180.0/M_PI) );

  }


  /* Read BIN info from Aspect Histogram Compressed ext */
  if( ReadAspectHistoCompExt(aspecthistofile, &histobininfo, &histobincount) ){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, aspecthistofile);
    goto ComputePSFCorrection_end;
  }
  

  /* For each BIN compute OFFAXIS (theta) and AZIMUTH (phi) median values */

  phi_array = (double *)malloc(histocount*sizeof(double));
  theta_array = (double *)malloc(histocount*sizeof(double));

  for(i=0; i<histobincount; i++){

    itheta = 0;
    iphi = 0;
    
    for(j=0; j<histocount; j++){

      if( histoinfo[j].x_bin == histobininfo[i].x_bin &&  histoinfo[j].y_bin == histobininfo[i].y_bin )
	{
	  phi_array[iphi] = (*corrinfo)[j].phi ;
	  theta_array[iphi] = (*corrinfo)[j].theta ;
	  itheta++;
	  iphi++;
	}
    }

    histobininfo[i].phi_median = ComputeDoubleMedian(phi_array, iphi);
    histobininfo[i].theta_median = ComputeDoubleMedian(theta_array, itheta);

  }
  /* Free memory */
  free(phi_array);
  free(theta_array);


  /* For each BIN compute FRACTION value */

  psf_frac_off = (double *)malloc(histobincount*sizeof(double));

  for(i=0; i<histobincount; i++){
    if( ComputePsfFracOff(skyinstrfile, i+1, psffile, histobininfo[i].theta_median, histobininfo[i].phi_median, reginfo, &psf_frac_off[i]) ){
      headas_chat(CHATTY, "%s: Error: unable to compute PSF correction (bin=%d)\n", global.taskname, i);
      goto ComputePSFCorrection_end;
    }
  }

  for(i=0; i<histobincount; i++)
    headas_chat(CHATTY,"X_BIN=%d Y_BIN=%d PHI_MEDIAN=%f THETA_MEDIAN=%f PSF_FRAC=%f\n", 
		histobininfo[i].x_bin, histobininfo[i].y_bin, histobininfo[i].phi_median, histobininfo[i].theta_median, psf_frac_off[i] );


  /* Store FRACTION values */
  for(j=0; j<histocount; j++){
    for(i=0; i<histobincount; i++){

      if( (histoinfo[j].x_bin == histobininfo[i].x_bin) && (histoinfo[j].y_bin == histobininfo[i].y_bin) )
	{
	  (*corrinfo)[j].fraction = psf_frac_off[i];
	  break;
	}
    }
  }



  /* Free memory */
  free(psf_frac_off);
  free(optinfo);
  free(histoinfo);
  free(histobininfo);

  return OK;

 ComputePSFCorrection_end:
  return NOT_OK;

} /* ComputePSFCorrection */


double NormalaziedAt360Angle(double angle){

  double outangle;
  
  outangle = fmod(angle,360.0);
  outangle = outangle>=0 ? outangle : outangle+360;

  return outangle;

} /* NormalaziedAt360Angle */


/* Only for extended="no" */
int ComputePsfFracOff(char *expofile, int expoextnum, char *psffile, double theta, double phi, RegionExtInfo_t *reginfo, double *psf_frac_off){

  int           xx, yy, ii, jj, cx, cy, status=OK, psfextnum;
  double        sumrcts=1.0, expoval, dist, xx1, yy1, angle, a, b, psf_frac=0.0;
  char          cmd[BUF_SIZE];
  static float  psf[PSF_ROWS][PSF_PIXS];
  float         *expo=NULL;
  static double frac[PSF_ROWS][PSF_PIXS];
  static float  fractmp[PSF_ROWS][PSF_PIXS];
  ExposureInfo_t expoinfo;
  PsfBinInfo_t   psfbininfo;
  pid_t          pid;
  
  /* Get pid */
  pid=getpid();

  cx = (int)(reginfo->X[0]+0.5) -1;
  cy = (int)(reginfo->Y[0]+0.5) -1;


  /* Get bin info from input psf file */
  if(GetPsfBinInfo(psffile, &psfbininfo)){
    headas_chat(NORMAL, "%s: Error: Unable to read psf file %s\n", global.taskname, psffile);
    goto ComputePsfFracOff_end;
  }


  /* Get PSF ext num */
  psfextnum = GetInitBinIndex(psfbininfo.psfbin, 0, 0, psfbininfo.psfbin_dim-1, theta);
  psfextnum++;


  /* Create a temporary psf file with rotation */

  sprintf(global.tmpout.combinexform_in, "%dtmp_trasform%d.txt", (int)pid, (int)theta);
  sprintf(global.tmpout.combinexform_out, "%dtmp_trasform%d.xform", (int)pid, (int)theta);
  sprintf(global.tmpout.rotpsffile, "%dtmp_psf_%d.fits", (int)pid, (int)theta);

  if(WritePsfCombineXformInFile(global.tmpout.combinexform_in, phi)){
    headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.tmpout.combinexform_in);
    goto ComputePsfFracOff_end;
  }

  /* Execute combinexform */
  sprintf(cmd, "combinexform command=@%s outfile=%s", global.tmpout.combinexform_in, global.tmpout.combinexform_out);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.combinexform_out);
    goto ComputePsfFracOff_end;
  }

  /* Execute imagetrans */
  sprintf(cmd, "imagetrans infile=%s[%d] outfile=%s transform=%s inverse=none method=AREA dimenx=325 dimeny=325 seed=0 bitpix=-32 zeronull=no history=yes", 
	  psffile, psfextnum, global.tmpout.rotpsffile, global.tmpout.combinexform_out );
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: Unable to create '%s' file.\n", global.taskname, global.tmpout.rotpsffile);
    goto ComputePsfFracOff_end;
  }


  if(ReadPSFFile(global.tmpout.rotpsffile, 1, psf, &sumrcts)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file for theta %f\n", global.taskname, psffile, theta);
    goto ComputePsfFracOff_end;
  }
  
  for(yy=0; yy<PSF_ROWS; yy++){
    for(xx=0; xx<PSF_PIXS; xx++){
      frac[yy][xx] = (double)psf[yy][xx] / sumrcts ;
    }
  }


  if(ReadExpoFile(expofile, expoextnum, &expo, &expoinfo)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file for theta %f\n", global.taskname, expofile, theta);
    goto ComputePsfFracOff_end;
  }


  for(yy=0; yy<PSF_ROWS; yy++){
    for(xx=0; xx<PSF_PIXS; xx++){

      fractmp[yy][xx] = 0;

      ii = cy - expoinfo.crval2p + expoinfo.crpix2p - (int)PSF_PIXS/2 + yy ;
      jj = cx - expoinfo.crval1p + expoinfo.crpix1p - (int)PSF_ROWS/2 + xx  ;

      if(ii>=0 && ii<expoinfo.ywidth && jj>=0 && jj<expoinfo.xwidth )
	expoval = (double)expo[ii*expoinfo.xwidth + jj] / expoinfo.duration ;
/* 	expoval = (double)expo[ii*expoinfo.xwidth + jj] / (expoinfo.ontime*expoinfo.deadc) ; */
      else
	expoval = 0.0;


      frac[yy][xx] = frac[yy][xx] * expoval;


      if( !strcmp(global.reginfo.shape, "CIRCLE") )
	{
	  dist = sqrt( pow((yy - PSF_CENTERY),2) + pow((xx - PSF_CENTERX),2) );
	  
	  if(dist <= reginfo->R[0])
	    {
	      psf_frac +=  frac[yy][xx];
	      fractmp[yy][xx] = frac[yy][xx];
	    }

	}
      if( !strcmp(global.reginfo.shape, "ANNULUS") )
	{
	  dist = sqrt( pow((yy - PSF_CENTERY),2) + pow((xx - PSF_CENTERX),2) );
	  
	  if(dist >= reginfo->R[0] && dist <= reginfo->R1[0] )
	    {
	      psf_frac +=  frac[yy][xx];
	      fractmp[yy][xx] = frac[yy][xx];
	    }

	}
      if( !strcmp(global.reginfo.shape, "ELLIPSE") )
	{
	  angle = reginfo->rotang[0] *(M_PI/180.);
	  a = reginfo->R[0];
	  b = reginfo->R1[0];

	  xx1 = (xx - PSF_CENTERX)*cos(-angle) - (yy - PSF_CENTERY)*sin(-angle);
	  yy1 = (xx - PSF_CENTERX)*sin(-angle) + (yy - PSF_CENTERY)*cos(-angle);

	  if( (pow((xx1/a),2)+pow((yy1/b),2))<=1 )
	    {
	      psf_frac +=  frac[yy][xx];
	      fractmp[yy][xx] = frac[yy][xx];
	    }
	}
      
    }
  }

  *psf_frac_off = psf_frac;


  /* Cleanup Temporary Files */
  
  if ( FileExists(global.tmpout.combinexform_in) ) {
    if(remove (global.tmpout.combinexform_in) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.combinexform_in);
    }
  }
  
  if ( FileExists(global.tmpout.combinexform_out) ) {
    if(remove (global.tmpout.combinexform_out) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.combinexform_out);
    }
  }

  if ( FileExists(global.tmpout.rotpsffile) ) {
    if(remove (global.tmpout.rotpsffile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.rotpsffile);
    }
  }


  return OK;

 ComputePsfFracOff_end:
  return NOT_OK;

} /* ComputePsfFrac */

int WritePsfCombineXformInFile(char *filename, double phi){
  
  FILE	   *file;
  
  if (!(file = fopen(filename, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,filename);
    goto WritePsfCombineXformInFile_end;
  }
 
  fprintf(file, "trans(%d,%d)\n", -163, -163);
  fprintf(file, "rot(%f)\n", -phi+180);
  fprintf(file, "trans(%d,%d)\n", 163, 163);
  
  fclose(file);
  
  
  return OK;
  
 WritePsfCombineXformInFile_end:
  return NOT_OK;


} /* WritePsfCombineXformInFile */

int ReadPSFFile(char *filename, int extnum, float psf[PSF_ROWS][PSF_PIXS], double *sumrcts){

  int                   status=OK, anynull=0;
  long                  dimen; 
  FitsCard_t            *card;
  FitsHeader_t           head;
  FitsFileUnit_t        inunit=NULL; 
  

  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPSFFile_end;
    }
  
  /* Move in <extnum> extension */
  if(fits_movabs_hdu(inunit, extnum, NULL, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d' in\n", global.taskname,extnum);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto ReadPSFFile_end;
    }
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    

  /* Get SUMRCTS value */
  if(ExistsKeyWord(&head, "SUMRCTS", &card))
    {
      *sumrcts = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "SUMRCTS");
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadPSFFile_end;
    }

  dimen=PSF_ROWS*PSF_PIXS;
  if( fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, psf[0], &anynull, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to read image\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPSFFile_end;
    }

  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPSFFile_end;
    }


  return OK;

 ReadPSFFile_end:
  return NOT_OK;


} /* ReadPSFFile */

int ReadExpoFile(char *filename, int extnum, float **expomap, ExposureInfo_t *info){

  int                   status=OK, anynull=0, nfound;
  long                  naxes[2], dimen;
  FitsCard_t            *card;
  FitsHeader_t           head;
  FitsFileUnit_t        inunit=NULL; 
  

  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }
  
  
  /* Move in <extnum> extension */
  if(fits_movabs_hdu(inunit, extnum+1, NULL, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d' in\n", global.taskname,extnum);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto ReadExpoFile_end;
    }

  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    

  /* Get DURATION value */
  if(ExistsKeyWord(&head, KWNM_DURATION, &card))
    {
      info->duration = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DURATION);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

/*   /\* Get ONTIME value *\/ */
/*   if(ExistsKeyWord(&head, KWNM_ONTIME, &card)) */
/*     { */
/*       info->ontime = card->u.DVal; */
/*     } */
/*   else */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_ONTIME); */
/*       headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename); */
/*       goto ReadExpoFile_end; */
/*     } */

/*   /\* Get LIVETIME value *\/ */
/*   if(ExistsKeyWord(&head, KWNM_LIVETIME, &card)) */
/*     { */
/*       info->livetime = card->u.DVal; */
/*     } */
/*   else */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_LIVETIME); */
/*       headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename); */
/*       goto ReadExpoFile_end; */
/*     } */

/*   /\* Get DEADC value *\/ */
/*   if(ExistsKeyWord(&head, KWNM_DEADC, &card)) */
/*     { */
/*       info->deadc = card->u.DVal; */
/*     } */
/*   else */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DEADC); */
/*       headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename); */
/*       goto ReadExpoFile_end; */
/*     } */

  /* Get CRPIX1P value */
  if(ExistsKeyWord(&head, KWNM_CRPIX1P, &card))
    {
      info->crpix1p = (int)(0.5+card->u.DVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRPIX1P);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get CRPIX2P value */
  if(ExistsKeyWord(&head, KWNM_CRPIX2P, &card))
    {
      info->crpix2p = (int)(0.5+card->u.DVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRPIX2P);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get CRVAL1P value */
  if(ExistsKeyWord(&head, KWNM_CRVAL1P, &card))
    {
      info->crval1p = (int)(0.5+card->u.DVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRVAL1P);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get CRVAL2P value */
  if(ExistsKeyWord(&head, KWNM_CRVAL2P, &card))
    {
      info->crval2p = (int)(0.5+card->u.DVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_CRVAL2P);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }


  if (fits_read_keys_lng(inunit, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }
  
  info->xwidth = (int)naxes[0];
  info->ywidth = (int)naxes[1];
  
  dimen = info->xwidth * info->ywidth ;
  *expomap = (float*)malloc(sizeof(float)*dimen);
  if(*expomap==NULL){
    headas_chat(CHATTY,"%s: Error: ReadExpoFile: memory allocation failure.\n", global.taskname);
    goto ReadExpoFile_end;
  }

  if( fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, *expomap, &anynull, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to read image\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }


  return OK;

 ReadExpoFile_end:
  return NOT_OK;


} /* ReadExpoFile */

int GetPsfBinInfo(char *filename, PsfBinInfo_t *psfbininfo){

  int                   status=OK, inExt;
  FitsCard_t            *card;
  FitsHeader_t           head;
  FitsFileUnit_t        inunit=NULL; 


  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetPsfBinInfo_end;
    }

  /* Get number of hdus in input file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetPsfBinInfo_end;
    }

  psfbininfo->psfbin_dim = inExt-1;

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto GetPsfBinInfo_end;
    }
  
  /* Retrieve header pointer */ 
  head=RetrieveFitsHeader(inunit);

  /* Get PSFBIN value */
  if(ExistsKeyWord(&head, "PSFBIN", &card))
    {
      psfbininfo->psfbin = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "PSFBIN");
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetPsfBinInfo_end;
    }


  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetPsfBinInfo_end;
    }


  return OK;

 GetPsfBinInfo_end:
  return NOT_OK;

} /* GetPsfBinInfo */


/*
NOTE: index=0 =>  offset+(imin*binsize) < value < offset+(imin*binsize+binsize)
 */
int GetInitBinIndex(double binsize, double offset, int imin, int imax, double value){
  
  int i;
  value -= offset;

  i = (value<imin*binsize-binsize/2) ? imin : ( value>=imax*binsize+binsize/2 ? imax : ((value-imin*binsize+binsize/2)/binsize +imin) );
  
  return i;

} /* GetInitBinIndex */


int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo){

  int            status=OK;
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL;


  /* Open readonly file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
    
  /* Move in <extname> extension  */
  if (fits_movnam_hdu(inunit, ANY_HDU, extname, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, extname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto GetObsInfo_end;
    }
    
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    
  

  /* Retrieve INSTRUME from input file */  
  if(ExistsKeyWord(&head, KWNM_INSTRUME, &card))
    {
      strcpy(obsinfo->instrume, card->u.SVal);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }
    
  /* Retrieve date-obs and time-obs from input file  */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      obsinfo->dateobs=card->u.SVal;
      if(!(strchr(obsinfo->dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    obsinfo->timeobs=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	      goto GetObsInfo_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }
    
  /* Retrieve date-end and time-end from input file  */
  if (ExistsKeyWord(&head, KWNM_DATEEND, &card))
    {
      obsinfo->dateend=card->u.SVal;
      if(!(strchr(obsinfo->dateend, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEEND, &card))
	    obsinfo->timeend=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEEND);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	      goto GetObsInfo_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEEND);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }


  /* close input file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      return NOT_OK;
    }
 
  return OK;
  
 GetObsInfo_end:
  
  CloseFitsFile(inunit);
  return NOT_OK;

} /* GetObsInfo */


int WritePsfCorrFile(PsfCorrInfo_t *psfcorrinfo, int psfcorrcount, char *infile, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open input file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, infile);
      goto WritePsfCorrFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WritePsfCorrFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WritePsfCorrFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WritePsfCorrFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WritePsfCorrFile_end;
    }
  hducount=1;


  /* Retrieve header pointer */
  head=RetrieveFitsHeader(outunit);

  /* Add creator */
  sprintf(crval,"%s (%s)", global.taskname, nustardas_v);
  AddCard(&head, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 
  
  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&head, KWNM_DATE, S, date, CARD_COMM_DATE);  
  
  /* Write header */
  if(WriteUpdatedHeader(outunit, &head))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WritePsfCorrFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WritePsfCorrFile_end;
      }
    }

  /* Create 'FRACTION' Ext  */
  if (WritePsfCorrExt(psfcorrinfo, psfcorrcount, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_FRACTION);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WritePsfCorrFile_end;
    }
  hducount++;

  /* Add history */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WritePsfCorrFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WritePsfCorrFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WritePsfCorrFile_end;
    }


  return OK;
  
 WritePsfCorrFile_end:

  return NOT_OK;

} /* WritePsfCorrFile */


int WritePsfCorrExt(PsfCorrInfo_t *psfcorrinfo, int nrows, FitsFileUnit_t ounit){

  int                n, count=0;
  unsigned           OutRows=0;
  PsfCorrCol_t       indxcol;
  Bintable_t	     table; 
  FitsHeader_t	     newhead;
  char               crval[FLEN_VALUE];
  char               date[25];
  Version_t          nustardas_v;          /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );
  
  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(BINTAB_ROWS, &table);

  /* Add columns */

  AddColumn(&newhead, &table, "TSTART", "Start time", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, "TSTOP", "Stop time", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, "FRACTION", "LC PSF/EXPOSURE fraction", "1D",TNONE);
  AddColumn(&newhead, &table, "THETA", "Offaxis", "1D", TUNIT, UNIT_ARCMIN, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, "PHI", "Azimuth", "1D", TUNIT, UNIT_DEG, CARD_COMM_PHYSUNIT);


  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_FRACTION, CARD_COMM_EXTNAME);

  /* Add creator */
  sprintf(crval,"%s (%s)", global.taskname, nustardas_v);
  AddCard(&newhead, KWNM_CREATOR, S, crval,CARD_COMM_CREATOR); 

  /* Add creation date */
  GetGMTDateTime(date);
  AddCard(&newhead, KWNM_DATE, S, date, CARD_COMM_DATE); 

  
  /* Finish bintable header */
  EndBintableHeader(&newhead, &table);

  /* Write bintable in file */
  FinishBintableHeader(ounit, &newhead, &table);


  GetBintableStructure(&newhead, &table, BINTAB_ROWS, 0, NULL);

  /* Get columns index from name */

  if ((indxcol.TSTART = GetColNameIndx(&table, "TSTART")) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, "TSTART");
      goto WritePsfCorrExt_end;
    }

  if ((indxcol.TSTOP = GetColNameIndx(&table, "TSTOP")) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, "TSTOP");
      goto WritePsfCorrExt_end;
    }

  if ((indxcol.FRACTION = GetColNameIndx(&table, "FRACTION")) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, "FRACTION");
      goto WritePsfCorrExt_end;
    }

  if ((indxcol.THETA = GetColNameIndx(&table, "THETA")) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, "THETA");
      goto WritePsfCorrExt_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, "PHI")) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, "PHI");
      goto WritePsfCorrExt_end;
    }


  OutRows = 0;
  count = 0;

  while(count<nrows){

    for(n=0; (n<BINTAB_ROWS)&&(count<nrows); n++){
      
      DVEC(table, n, indxcol.TSTART) = psfcorrinfo[count].tstart;
      DVEC(table, n, indxcol.TSTOP) = psfcorrinfo[count].tstop;
      DVEC(table, n, indxcol.FRACTION) = psfcorrinfo[count].fraction;
      DVEC(table, n, indxcol.THETA) = psfcorrinfo[count].theta;
      DVEC(table, n, indxcol.PHI) = psfcorrinfo[count].phi;

      count++;
      
      if(++OutRows>=BINTAB_ROWS)
	{
	  WriteFastBintable(ounit, &table, OutRows, FALSE);
	  OutRows=0;
	}      
    }

  }


  WriteFastBintable(ounit, &table, OutRows, TRUE);
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&newhead, &table);


  return OK;
  
 WritePsfCorrExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WritePsfCorrExt */


int ComputePsfCorrFac(PsfCorrInfo_t *psfcorrinfo, int psfcorrcount, struct gti_struct *gti, double time, double *corrfac){

  int               i=0, j=0, k=0, status=OK;
  double            weight, fraction_mean, corr, psfbin, t_lc=0, t_lc_stop=0;
  double            sum_fraction=0., sum_weight=0.;
  double            dtime_gti=0;


  /* LC time */
  t_lc = time + global.lcinfo.timezero - (global.lcinfo.timedel*global.lcinfo.timepixr);

  /* LC bin end time */
  t_lc_stop = t_lc + global.lcinfo.timedel;


  /* Search the first psfcorrinfo row with the nearest tstart before 't_lc' value */
  FindClosestPsfCorrInfoIndex(psfcorrinfo, psfcorrcount, t_lc, &i);
  
/*   if(psfcorrinfo[i].tstart > t_lc){ */
/*     headas_chat(NORMAL, "%s: Error: LC TIME=%f not included in psf correction temporal range\n", global.taskname, t_lc); */
/*     goto ComputePsfCorrFac_end; */
/*   } */


  /* Search the first psfcorrinfo row with tstart over 't_lc_stop' value */
  for(j=i; j<psfcorrcount; j++)
    {
      if( psfcorrinfo[j].tstart > t_lc_stop )
	break;
    }
  j = (j==i) ? j : j-1;  /* psfcorrinfo row with the nearest tstart before 't_lc_stop' value */
  
  
  /* Compute 'fraction' mean */

  for(k=i; k<=j; k++)
    {

      psfbin = psfcorrinfo[k].tstop - psfcorrinfo[k].tstart ;

      if(i==j)  /* only one psfcorrinfo row */
	{
	  weight = psfbin;
	}
      else if(k==i)  /* first psfcorrinfo */
	{
	  weight = psfbin - ( t_lc - psfcorrinfo[k].tstart ) ;
	}
      else if(k==j)  /* last psfcorrinfo */
	{
	  weight = t_lc_stop - psfcorrinfo[k].tstart ;
	}
      else
	{
	  weight = psfbin;
	}

      
      /* Compute overlap exposure of the psf time interval with GTI */
      dtime_gti = HDgti_exp(psfcorrinfo[k].tstart, psfcorrinfo[k].tstop, gti, &status);
      if(status!=OK){
	headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time=%f with GTI\n", global.taskname, psfcorrinfo[k].tstart);
	goto ComputePsfCorrFac_end;
      }
      
      /* psf bin included in GTI time interval */
      if( dtime_gti>0 )
	{
	  sum_fraction += weight*psfcorrinfo[k].fraction ;
	  sum_weight += weight ;

	  if(!(psfcorrinfo[k].fraction>0))
	    headas_chat(CHATTY, "%s: Warning: PSF FRACTION = 0 for TIME=%f\n", global.taskname, t_lc);
	}
      else
	{
	  if(!(psfcorrinfo[k].fraction>0))
	    headas_chat(CHATTY, "%s: Warning: PSF FRACTION = 0 for TIME=%f (GTI zero)\n", global.taskname, t_lc);
	}

      if(!(psfcorrinfo[k].fraction>0))
	headas_chat(CHATTY, "%s: Warning: PSF FRACTION = 0 for TIME=%f\n", global.taskname, t_lc);
      
    }

  
  if(!(sum_fraction>0))
    headas_chat(CHATTY, "%s: Warning: PSF FRACTION Sum = 0  for TIME=%f\n", global.taskname, t_lc);
  
  if(!(sum_weight>0))
    headas_chat(CHATTY, "%s: Warning: PSF Weight Sum = 0  for TIME=%f\n", global.taskname, t_lc);
  
  
  fraction_mean = (sum_weight>0) ? (sum_fraction/sum_weight) : 0 ;
  corr = (fraction_mean>0) ? (1/fraction_mean) : 0 ;


  *corrfac = corr;
  
  return OK;
  
 ComputePsfCorrFac_end:
  
  return NOT_OK;
   
} /* ComputePsfCorrFac */


int ReadPhaImg(char *filename, float **img, PhaImgInfo_t *imginfo){

  int                   status=OK, anynull=0, nfound;
  long                  dimen; 
  long                  naxes[2];
  int                   x0, y0;
  FitsHeader_t          head;
  FitsFileUnit_t        inunit=NULL; 
  

  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);    


  if (fits_read_keys_lng(inunit, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    } 
  if (fits_read_key(inunit,TINT,KWNM_X_OFFSET, &x0,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_X_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
      
    } 
  if (fits_read_key(inunit,TINT,KWNM_Y_OFFSET, &y0,NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_Y_OFFSET);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }


  dimen = naxes[0]*naxes[1];
  *img = (float*)malloc(sizeof(float)*dimen);
  if(*img==NULL){
    headas_chat(CHATTY,"%s: Error: ReadPhaImg: memory allocation failure.\n", global.taskname);
    goto ReadPhaImg_end;
  }


  if( fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, *img, &anynull, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to read image\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }

  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPhaImg_end;
    }

  imginfo->x_width = (int)naxes[0];
  imginfo->y_width = (int)naxes[1];
  imginfo->x_offset = x0;
  imginfo->y_offset = y0;


  return OK;

 ReadPhaImg_end:
  return NOT_OK;


} /* ReadPhaImg */


int ComputeExpoCorr(char *expofile, int expoextnum, float *phaimg, PhaImgInfo_t *phaimginfo, RegBox_t *box, double *expocorr){

  int           xx, yy, n_pix, pha_xx, pha_yy, count;
  int           xmin, xmax, ymin, ymax;
  int           pha_xdim, pha_ydim, pha_xoffset, pha_yoffset;
  double        sum_pix, *boxexpocorr, *countsbox;
  double        sum_countsbox, sum_countsexpo;
  float         *expo=NULL;
  ExposureInfo_t expoinfo;

  boxexpocorr =  (double*)malloc(box->nbox*sizeof(double));
  countsbox = (double*)malloc(box->nbox*sizeof(double));


  pha_xdim = phaimginfo->x_width;
  pha_ydim = phaimginfo->y_width;
  pha_xoffset = phaimginfo->x_offset;
  pha_yoffset = phaimginfo->y_offset;


  if(ReadExpoFile(expofile, expoextnum, &expo, &expoinfo)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file\n", expofile);
    goto ComputeExpoCorr_end;
  }


  for(count=0; count<box->nbox; count++){

    sum_pix = 0;
    n_pix = 0;
    countsbox[count] = 0;

    
    xmin = box->boxinfo[count].x_min -1;
    xmax = box->boxinfo[count].x_max -1;
    
    ymin = box->boxinfo[count].y_min -1;
    ymax = box->boxinfo[count].y_max -1;


    for(yy=ymin; yy<=ymax; yy++){
      for(xx=xmin; xx<=xmax; xx++){
	
	if( yy>=0 && yy<expoinfo.ywidth && xx>=0 && xx<expoinfo.xwidth ){
	  
	  /* pixel coordinates in pha image coordinates */
	  pha_yy = yy - pha_yoffset +1;
	  pha_xx = xx - pha_xoffset +1;  
	  
	  /* pixel in pha image */
	  if( pha_yy>=0 && pha_yy<pha_ydim && pha_xx>=0 && pha_xx<pha_xdim ){
	    
	    if(phaimg[pha_yy*pha_xdim+pha_xx]>=0.){
	      
	      sum_pix += (double)expo[yy*expoinfo.xwidth + xx] / expoinfo.duration ;
	      n_pix++;

	      countsbox[count] += (double)phaimg[pha_yy*pha_xdim+pha_xx];
	    }
	    
	  }

	}
      
      }
    }

    boxexpocorr[count] = n_pix>0 ? sum_pix/n_pix : 0;
    headas_chat(CHATTY, "skyinstrmap=%d SubImages Box %d expocorr=%f countsbox=%f sum_pix=%f n_pix=%d\n", expoextnum, count, boxexpocorr[count], countsbox[count], sum_pix, n_pix);

  }

  sum_countsbox = 0;
  sum_countsexpo = 0;

  for(count=0; count<box->nbox; count++){

    sum_countsbox += countsbox[count];

    if(boxexpocorr[count]>0)
      sum_countsexpo += countsbox[count]/boxexpocorr[count];
  }

  *expocorr = sum_countsexpo>0 ? sum_countsbox/sum_countsexpo : 0 ;

  
  free(boxexpocorr);
  free(countsbox);

  return OK;

 ComputeExpoCorr_end:
  return NOT_OK;

} /* ComputeExpoCorr */


int ComputeEXPOSURECorrection(char *aspecthistofile, char *skyinstrfile, char *phafile, PsfCorrInfo_t **corrinfo, int *corrcount){

  int                i=0, j=0;
  const float        fnan = log(-1);
  double             *expocorr;
  float              *phaimg=NULL;
  PhaImgInfo_t       phaimginfo;
  RegBox_t           regbox;
  AspHistoInfo_t     *histoinfo=NULL;
  int                histocount;
  AspHistoCompBINInfo_t   *histobininfo=NULL;
  int                     histobincount;


  /* Read Aspect Histogram info */
  if( ReadAspectHistoExt(aspecthistofile, &histoinfo, &histocount) ){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, aspecthistofile);
    goto ComputeEXPOSURECorrection_end;
  }


  /* Allocate memory to storage EXPOSURE correction data */
  *corrcount = histocount;
  *corrinfo = (PsfCorrInfo_t *)calloc(*corrcount, sizeof(PsfCorrInfo_t));
  if(*corrinfo==NULL){
    headas_chat(CHATTY,"%s: Error: ComputeEXPOSURECorrection: memory allocation failure.\n", global.taskname);
    goto ComputeEXPOSURECorrection_end;
  }


  /* Store TSTART and TSTOP values */
  for(i=0; i<histocount; i++){
    (*corrinfo)[i].tstart = histoinfo[i].tstart;
    (*corrinfo)[i].tstop = histoinfo[i].tstop;
    (*corrinfo)[i].theta = fnan;
    (*corrinfo)[i].phi = fnan;
  }


  /* Read BIN (skyinstrmap) info from Aspect Histogram Compressed ext */
  if( ReadAspectHistoCompExt(aspecthistofile, &histobininfo, &histobincount) ){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, aspecthistofile);
    goto ComputeEXPOSURECorrection_end;
  }


  /* Read PHA image extension */
  if(ReadPhaImg(phafile, &phaimg, &phaimginfo)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file\n", phafile);
    goto ComputeEXPOSURECorrection_end;
  }

  /* Compute SubImages Box limits */
  if(ComputeBoxFromPhaImg(&phaimginfo, global.par.boxsize, &regbox)){
    headas_chat(NORMAL, "%s: Error: Unable to compute subimages boxes\n",  global.taskname);
    goto ComputeEXPOSURECorrection_end;
  }


  /* For each BIN (skyinstrmap) compute FRACTION value */

  expocorr = (double *)malloc(histobincount*sizeof(double));

  for(i=0; i<histobincount; i++){
    if( ComputeExpoCorr(skyinstrfile, i+1, phaimg, &phaimginfo, &regbox, &expocorr[i]) ){
      headas_chat(CHATTY, "%s: Error: unable to compute EXPOSURE correction (bin=%d)\n", global.taskname, i);
      goto ComputeEXPOSURECorrection_end;
    }
  }


  /* Store FRACTION values */
  for(j=0; j<histocount; j++){

    for(i=0; i<histobincount; i++){
      if( histoinfo[j].x_bin == histobininfo[i].x_bin &&  histoinfo[j].y_bin == histobininfo[i].y_bin )
	{
	  (*corrinfo)[j].fraction = expocorr[i];
	  break;
	}
    }

  }


  /* Free memory */
  free(phaimg);
  free(expocorr);
  free(histoinfo);
  free(histobininfo);


  return OK;

 ComputeEXPOSURECorrection_end:
  return NOT_OK;

} /* ComputeEXPOSURECorrection */


int ComputeBoxFromPhaImg(PhaImgInfo_t *phaimginfo, int boxsize, RegBox_t *box){

  int xmin, xmax, ymin, ymax;
  int nx=0, ny=0, i=0, j=0, cnt=0;

  headas_chat(CHATTY, "%s: Info: Computing SubImages boxes\n",  global.taskname);

  xmin = phaimginfo->x_offset ;
  xmax = phaimginfo->x_offset + phaimginfo->x_width -1 ;
  ymin = phaimginfo->y_offset ;
  ymax = phaimginfo->y_offset + phaimginfo->y_width -1 ;


  if( boxsize>phaimginfo->x_width && boxsize>phaimginfo->y_width ){

    headas_chat(NORMAL, "%s: Warning: Subimage box size is larger than the spectrum extraction region,", global.taskname);
    headas_chat(NORMAL, "%s: Warning: resetting boxsize to the extraction region size\n", global.taskname);

    box->nbox = 1;
    box->boxinfo = (RegBoxInfo_t*) malloc(sizeof(RegBoxInfo_t));
    if(box->boxinfo==NULL){
      headas_chat(CHATTY,"%s: Error: ComputeBoxFromPhaImg: memory allocation failure.\n", global.taskname);
      return NOT_OK;
    }
    box->boxinfo[0].x_min = xmin;
    box->boxinfo[0].x_max = xmax;
    box->boxinfo[0].y_min = ymin;
    box->boxinfo[0].y_max = ymax;
    
    box->boxinfo[0].cx = phaimginfo->x_offset + phaimginfo->x_width/2 ;
    box->boxinfo[0].cy = phaimginfo->y_offset + phaimginfo->y_width/2 ;

    return OK;
  }

  nx = ((phaimginfo->x_width)%boxsize)>0 ? (int)(phaimginfo->x_width)/boxsize + 1 : (int)(phaimginfo->x_width)/boxsize;
  ny = ((phaimginfo->y_width)%boxsize)>0 ? (int)(phaimginfo->y_width)/boxsize + 1 : (int)(phaimginfo->y_width)/boxsize;

  box->nbox = nx*ny;
  box->boxinfo = (RegBoxInfo_t*) malloc(box->nbox * sizeof(RegBoxInfo_t));
  if(box->boxinfo==NULL){
    headas_chat(CHATTY,"%s: Error: ComputeBoxFromPhaImg: memory allocation failure.\n", global.taskname);
    return NOT_OK;
  }

  cnt=0;

  for(j=0; j<ny; j++){
    for(i=0; i<nx; i++){
    
      box->boxinfo[cnt].x_min = xmin + i*boxsize;
      box->boxinfo[cnt].y_min = ymin + j*boxsize;
      box->boxinfo[cnt].x_max = box->boxinfo[cnt].x_min + boxsize -1;
      box->boxinfo[cnt].y_max = box->boxinfo[cnt].y_min + boxsize -1;

      box->boxinfo[cnt].cx = box->boxinfo[cnt].x_min + (boxsize-1)/2 ;
      box->boxinfo[cnt].cy = box->boxinfo[cnt].y_min + (boxsize-1)/2 ;
      
    
      headas_chat(CHATTY, "%s: Info: Box %d - cx=%f cy =%f x_min=%d x_max=%d y_min=%d y_max=%d\n", 
		  global.taskname, cnt, box->boxinfo[cnt].cx, box->boxinfo[cnt].cy,
		  box->boxinfo[cnt].x_min, box->boxinfo[cnt].x_max, box->boxinfo[cnt].y_min, box->boxinfo[cnt].y_max);

      cnt++;
    }
  }


  return OK;

} /* ComputeBoxFromPhaImg */


int CheckInputSkyInstrFile(char *filename, double energy, BOOL vignflag){

  double         key_energy, sens=0.0000001;
  FitsCard_t     *card;
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL;


  /* Open readonly file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }

    
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);


  /* Check VIGNAPP value */
  if( (ExistsKeyWord(&head, KWNM_VIGNAPP, NULLNULLCARD)) && (GetLVal(&head, KWNM_VIGNAPP)) )
    {
      if(!vignflag)
	{
	  headas_chat(NORMAL, "%s: Error: value of parameter 'vignflag' not consistent with input exposure map.\n", global.taskname);
	  goto CheckInputSkyInstrFile_end;
	}

      /* Retrieve VIGNEN from input file */  
      if(ExistsKeyWord(&head, KWNM_VIGNEN, &card))
	{
	  key_energy = card->u.DVal;

	  if( (key_energy>energy+sens) || (key_energy<energy-sens) )
	    {
	      headas_chat(NORMAL, "%s: Error: value of parameter 'energy' not consistent with input exposure map.\n", global.taskname);
	      goto CheckInputSkyInstrFile_end;
	    }
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_VIGNEN);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	  goto CheckInputSkyInstrFile_end;
	}
    }
  else
    {
      if(vignflag)
	{
	  headas_chat(NORMAL, "%s: Error: value of parameter 'vignflag' not consistent with input exposure map.\n", global.taskname);
	  goto CheckInputSkyInstrFile_end;
	}
    }


  /* close input file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      return NOT_OK;
    }

  return OK;


 CheckInputSkyInstrFile_end:
    
  CloseFitsFile(inunit);
  return NOT_OK;
 
}
