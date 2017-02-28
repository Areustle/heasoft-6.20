/*
 * 
 *	numkarf.c
 *
 *	INVOCATION:
 *
 *		numkarf [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 13/12/11 - First version
 *        0.1.1 - NS 11/01/12 - Handle new Off-Axis Histogram file format
 *        0.1.2 - NS 26/01/12 - Handle new algorithm for PSF correction
 *        0.1.3 - NS 08/01/12 - Handle extended sources while creating arf file
 *        0.1.4 - NS 01/03/12 - Handle new format of ARF, 2D-PSF and vignetting CALDB files
 *        0.1.5 - NS 14/03/12 - Bug fixed
 *        0.1.6 - NS 02/05/12 - Handle source azimuthal angle in PSF correction
 *                            - Handle 'ANNULUS' and 'ELLIPSE' extraction region for extended="no"
 *                            - Modified 'ComputeBoxWeight' routine
 *                            - Added consistence check of input files
 *        0.1.7 - NS 16/05/12 - Added 'phibin' input parameter
 *        0.1.8 - GF 06/07/12 - Corrected the scan of the PSF file extensions
 *        0.1.9 - NS 23/07/12 - Improved performance if 'psfflag'="no"
 *        0.2.0 - NS 24/07/12 - Replaced 'fltime' ftool call with 'FilterTimesByGTI' routine
 *        0.2.1 - NS 17/09/12 - Handle long file naming
 *        0.2.2 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.2.3 - NS 03/12/12 - Handle different extraction regions for extended="yes"
 *                            - Bug fixed in 'WriteOffAxisExt' routine
 *        0.2.4 - NS 10/12/12 - Handle new definition of PA_PNT keyword (PA_PNT_new=270-PA_PNT_old)
 *                            - Handle new input parameters of nuexpomap task
 *        0.2.5 - NS 12/03/13 - Handle Aperture Stop correction
 *                            - Modified 'phi_det1' computation in vignetting correction
 *                            - Handle Ghost Rays correction
 *        0.2.6 - NS 12/04/13 - Handle DETABS correction
 *        0.2.7 - NS 13/06/13 - Handle memory allocation failure
 *        0.2.8 - NS 01/07/13 - Bug fixed
 *        0.2.9 - NS 10/07/13 - Handle new input parameters of nuexpomap task
 *        0.3.0 - NS 17/07/13 - Handle energy-dependent 2d-PSF file in PSF correction
 *        0.3.1 - NS 11/09/13 - Handle bad values of 'grflag' and 'psfflag' input parameters for extended sources 
 *        0.3.2 - NS 28/03/14 - Added 'inexpomapfile' input parameter
 *        0.3.3 - NS 08/04/14 - Handle compressed Optical Axis input file
 *        0.3.4 - NS 20/05/15 - Added 'pilowarf', 'pihigh' and flatflag input parameters
 *        0.3.5 - NS 08/06/15 - Bug fixed in 'addarf' execution when using flat distribution
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB numkarf  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "numkarf.h"


/********************************/
/*         definitions          */
/********************************/

#define NUMKARF_C
#define NUMKARF_VERSION      "0.3.5"
#define PRG_NAME               "numkarf"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	numkarf_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 numkarf.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void numkarf_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 13/12/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numkarf_getpar()
{

  /* Input PHA File Name */
  if(PILGetFname(PAR_PHAFILE, global.par.phafile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PHAFILE);
      goto Error;
    }
  
  /* Output ARF File Name */
  if(PILGetFname(PAR_OUTFILE, global.par.outfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;	
    }

  /*  Optical Axis File Name */
  if(PILGetFname(PAR_OPTAXISFILE, global.par.optaxisfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OPTAXISFILE);
      goto Error;	
    }

  /*  Off-Axis File Name */
  if(PILGetFname(PAR_OFFAXISFILE, global.par.offaxisfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OFFAXISFILE);
      goto Error;	
    }

  /*  Off-Axis Histogram File Name */
  if(PILGetFname(PAR_OFFAXISHISTO, global.par.offaxishisto)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OFFAXISHISTO);
      goto Error;	
    }

  /*  Aperture Stop Histogram File Name */
  if(PILGetFname(PAR_APSTOPHISTO, global.par.apstophisto)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_APSTOPHISTO);
      goto Error;	
    }

  /*  Ghost Rays Histogram File Name */
  if(PILGetFname(PAR_GRHISTO, global.par.grhisto)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRHISTO);
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

  if(PILGetFname(PAR_INEXPOMAPFILE, global.par.inexpomapfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INEXPOMAPFILE);
      goto Error;	
    }
  if ( !(strcasecmp (global.par.inexpomapfile, DF_NONE)) || !global.par.extended )
    global.getexpomapfile = FALSE;
  else
    global.getexpomapfile = TRUE;

  
  if(!global.getexpomapfile){

    if(PILGetFname(PAR_PIXPOSFILE, global.par.pixposfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIXPOSFILE);
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


  if(PILGetFname(PAR_ALIGNFILE, global.par.alignfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ALIGNFILE);
      goto Error;	
    }
   

  if(PILGetBool(PAR_ABERRATION, &global.par.aberration))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ABERRATION);
      goto Error;	
    }

  if(PILGetInt(PAR_PIXBIN, &global.par.pixbin))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIXBIN); 
      goto Error;	 
    }

  if(PILGetReal(PAR_PHIBIN, &global.par.phibin))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PHIBIN); 
      goto Error;	 
    }

  if(PILGetReal(PAR_APSTOPPHIBIN, &global.par.apstopphibin))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_APSTOPPHIBIN); 
      goto Error;	 
    }

  if(PILGetReal(PAR_GRPHIBIN, &global.par.grphibin))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRPHIBIN); 
      goto Error;	 
    }

  /*  input ARF File Name */
  if(PILGetFname(PAR_INARFFILE, global.par.inarffile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INARFFILE);
      goto Error;	
    }

  /*  input PSF Grouping File Name */
  if(PILGetFname(PAR_GRPPSFFILE, global.par.grppsffile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRPPSFFILE);
      goto Error;	
    }

  /*  input PSF Dir Name */
  if(PILGetFname(PAR_PSFDIR, global.par.psfdir)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSFDIR);
      goto Error;	
    }

  /*  Vignetting File Name */
  if(PILGetFname(PAR_VIGNFILE, global.par.vignfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_VIGNFILE);
      goto Error;	
    }

  if(PILGetFname(PAR_APSTOPCORRFILE, global.par.apstopcorrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_APSTOPCORRFILE);
      goto Error;	
    }

  if(PILGetFname(PAR_GRCORRFILE, global.par.grcorrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRCORRFILE);
      goto Error;	
    }

  if(PILGetFname(PAR_DETABSCORRFILE, global.par.detabscorrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DETABSCORRFILE);
      goto Error;	
    }
 
  /* psfflag */
  if(PILGetBool(PAR_PSFFLAG, &global.par.psfflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSFFLAG); 
      goto Error;	 
    }
 
  /* vignflag */
  if(PILGetBool(PAR_VIGNFLAG, &global.par.vignflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_VIGNFLAG); 
      goto Error;	 
    }

  if(PILGetBool(PAR_APSTOPFLAG, &global.par.apstopflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_APSTOPFLAG); 
      goto Error;	 
    }

  if(PILGetBool(PAR_GRFLAG, &global.par.grflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_GRFLAG); 
      goto Error;	 
    }

  if(PILGetBool(PAR_DETABSFLAG, &global.par.detabsflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DETABSFLAG); 
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

  if(PILGetInt(PAR_PILOWARF, &global.par.pilowarf))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PILOWARF); 
      goto Error;	 
    }

  if(PILGetInt(PAR_PIHIGHARF, &global.par.pihigharf))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIHIGHARF); 
      goto Error;	 
    }

  if(PILGetBool(PAR_FLATFLAG, &global.par.flatflag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_FLATFLAG); 
      goto Error;	 
    }

  if(PILGetReal(PAR_PERCENT, &global.par.percent))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PERCENT); 
      goto Error;	 
    }

  /* inistseed */
  if(PILGetBool(PAR_INITSEED, &global.par.initseed))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INITSEED); 
      goto Error;	 
    }

  /* cleanup */
  if(PILGetBool(PAR_CLEANUP, &global.par.cleanup))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CLEANUP); 
      goto Error;	 
    }

  if(PILGetFname(PAR_SRCREGIONFILE, global.par.srcregionfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCREGIONFILE);
      goto Error;	
    }



  get_history(&global.hist);
  numkarf_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* numkarf_getpar */


/*
 *	numkarf_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int numkarf_checkinput();
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
 *        0.1.0 - NS 13/12/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numkarf_work()
{
  int                status=0, i, j;
  int                skysize=1000;
  double             skyx=500.5, skyy=500.5;
  long               extfile;
  char               cmd[BUF_SIZE], aberration[5], initseed[5];
  char               BaseName[MAXFNAME_LEN], DirName[MAXFNAME_LEN], expomapfile[PIL_LINESIZE];
  OptAxisInfo_t      *optinfo=NULL;
  OptAxisKeys_t      optkeys;
  OffAxisHistoInfo_t *histoinfo=NULL;
  VignInfo_t         *vigninfo=NULL;
  DETABSInfo_t       detabsinfo[4];
  AlignInfo_t        aligninfo;
  RegBox_t           regbox;
  float              *phaimg=NULL;
  PhaImgInfo_t       phaimginfo;
  int                optcount, vigncount, histocount;
  double             *vigncorr, *detabscorr;
  PsfCorrVal_t       psfcorr;
  ApStopCorrVal_t    apstopcorr;
  GhostRaysCorrVal_t grcorr;
  struct gti_struct  gti;
  pid_t              pid;
  
  /* Get pid */
  pid=getpid();

  if(numkarf_checkinput())
    goto Error;

  if(global.par.aberration)
    sprintf(aberration, "yes");
  else
    sprintf(aberration, "no");

  if(global.par.initseed)
    sprintf(initseed, "yes");
  else
    sprintf(initseed, "no");


  /* Create the task temporary directory */
  if(mkdir(global.tmpout.dirname,0777)){
    headas_chat(NORMAL, "%s: Error: Unable to create the temporary directory '%s'\n", global.taskname, global.tmpout.dirname);
    goto Error;
  }

  /* Create local link to input files */
  
  SplitFilePath(global.par.infile, DirName, BaseName);
/*   sprintf(global.tmpout.loc_infile, "%s/%s", global.tmpout.dirname, BaseName); */
  sprintf(global.tmpout.loc_infile, "%dtmplnk_%s", (int)pid, BaseName);
  
  if( CreateAbsSymbolicLink(global.par.infile, global.tmpout.loc_infile) )
    {
      headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.loc_infile, global.par.infile);
      goto Error;
    }

  SplitFilePath(global.par.phafile, DirName, BaseName);
  sprintf(global.tmpout.loc_phafile, "%dtmplnk_%s", (int)pid, BaseName);
  
  if( CreateAbsSymbolicLink(global.par.phafile, global.tmpout.loc_phafile) )
    {
      headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.loc_phafile, global.par.phafile);
      goto Error;
    } 

  SplitFilePath(global.par.srcregionfile, DirName, BaseName);
/*   sprintf(global.tmpout.loc_srcregionfile, "%s/%s", global.tmpout.dirname, BaseName); */
  sprintf(global.tmpout.loc_srcregionfile, "%dtmplnk_%s", (int)pid, BaseName);
      
  if( CreateAbsSymbolicLink(global.par.srcregionfile, global.tmpout.loc_srcregionfile) )
    {
      headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.loc_srcregionfile, global.par.srcregionfile);
      goto Error;
    }


  /* Get Observation Info from input PHA file */
  if( GetObsInfo(global.par.phafile, KWVL_EXTNAME_SPECTRUM, &global.obsinfo) ){
    headas_chat(NORMAL, "%s: Error: Unable to get info from input '%s' file.\n", global.taskname, global.par.phafile);
    goto Error;
  }

  /* Derive CALDB inarffile name */  
  if ( !strcasecmp(global.par.inarffile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_SPECRESP_DSET, global.par.inarffile, HD_EXPR, &extfile, global.obsinfo.instrume, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for inarffile parameter.\n", global.taskname);
      	  goto Error;
      	}
      extfile++;
    }

  /* Derive CALDB vignfile name */  
  if ( !strcasecmp(global.par.vignfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_TVIGNET_DSET, global.par.vignfile, HD_EXPR, &extfile, global.obsinfo.instrume, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for vignfile parameter.\n", global.taskname);
      	  goto Error;
      	}
      extfile++;
    }
  
/*   Retrieve Vignetting info from input vignfile */
/*   NOTE: 'global.filesinfo' global variable is updated with the informations */
/*         about the vignetting file structure */
  if( ReadVignFile(global.par.vignfile, &vigninfo, &vigncount) )
    {
      headas_chat(NORMAL, "%s: Error: unable to read Vignetting file.\n", global.taskname);
      goto Error;
    }


  if ( global.par.detabsflag )
    {

      /* Derive CALDB detabscorrfile name */
      if ( !strcasecmp(global.par.detabscorrfile,DF_CALDB) )
	{
	  if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_DETABS_DSET, global.par.detabscorrfile, HD_EXPR, &extfile, global.obsinfo.instrume, HD_DETNAM))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to query CALDB for detabscorrfile parameter.\n", global.taskname);
	      goto Error;
	    }
	  extfile++;
	}
      
      
      if( ReadDETABSFile(global.par.detabscorrfile, detabsinfo) )
	{
	  headas_chat(NORMAL, "%s: Error: unable to read DETABS Correction file.\n", global.taskname);
	  goto Error;
	}

    }


  /* Create optical axis GTI file */
  if(CreateFilteredOptAxisFile(global.par.optaxisfile, global.par.phafile, global.tmpout.optaxisgti))
    goto Error;


  /* Retrieve filtered Optical Axis info from input optaxisfile*/
  if( ReadOptAxisFile(global.tmpout.optaxisgti, &optinfo, &optcount, &optkeys) )
    {
      headas_chat(NORMAL, "%s: Error: unable to read fitered Optical Axis file '%s'.\n", global.taskname, global.tmpout.optaxisgti);
      goto Error;
    }


  /* Derive CALDB alignfile name */  
  if ( !strcasecmp(global.par.alignfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_ALIGN_DSET, global.par.alignfile, "type.eq.systems", &extfile, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for alignfile parameter.\n", global.taskname);
      	  goto Error;
      	}
      extfile++;
    }

  /* Retrieve alignment info from input alignfile*/
  if( ReadAlignInfo(global.par.alignfile, &aligninfo) )
    {
      headas_chat(NORMAL, " %s: Error: unable to read alignment file.\n", global.taskname);
      goto Error;
    }


  /* ------------------------------------------------------------------------------------------------ */
  /*     Extended Source    */
  /* ------------------------------------------------------------------------------------------------ */
  if(global.par.extended){


    /* Create temporary Filtered PHA File */
    sprintf(global.tmpout.loc_filtered_phafile, "%d_srcreg-xsel.pha", (int)pid);

    if( FilterPhaFile(global.tmpout.loc_infile, global.tmpout.loc_phafile, global.tmpout.loc_srcregionfile, global.par.pilowarf, global.par.pihigharf, global.tmpout.loc_filtered_phafile) ){
      headas_chat(NORMAL, "%s: Error: Unable to create temporary filtered pha file.\n", global.taskname);
      goto Error;
    }

    /* Update temporary link to filtered pha file */
    if( unlink(global.tmpout.loc_phafile) ){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.loc_phafile);
    }
    if( CreateAbsSymbolicLink(global.tmpout.loc_filtered_phafile, global.tmpout.loc_phafile) ) {
      headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.loc_phafile, global.tmpout.loc_filtered_phafile);
      goto Error;
    }


    if(!global.getexpomapfile){

      /* Execute expomap task */
      sprintf(global.tmpout.expomap, "%dtmp_expo.fits", (int)pid);
      sprintf(global.tmpout.aspecthisto, "%dtmp_asphisto.fits", (int)pid);
    
      sprintf(cmd, "nuexpomap infile=%s pixposfile=%s alignfile=%s mastaspectfile=%s attfile=%s teldef=%s instrprobmapfile=%s aberration=%s det1reffile=%s pixbin=%d expomapfile=%s aspecthistofile=%s offsetfile=NONE det1instrfile=NONE det2instrfile=NONE skyinstrfile=NONE vignflag=no skyx=500.5 skyy=500.5 skysize=1000 initseed=%s percent=%f indet2instrfile=NONE",
	      global.par.infile, global.par.pixposfile, global.par.alignfile, global.par.mastaspectfile, global.par.attfile, global.par.teldef,
	      global.par.instrprobmapfile, aberration, global.par.det1reffile, global.par.pixbin, global.tmpout.expomap, global.tmpout.aspecthisto, initseed,
	      global.par.percent );
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to create temporary Exposure Map file\n", global.taskname);
	goto Error;
      }
    
      strcpy(expomapfile, global.tmpout.expomap);
    }
    else{
      strcpy(expomapfile, global.par.inexpomapfile);
    }

    /* Read PHA image extension */
    if(ReadPhaImg(global.tmpout.loc_phafile, &phaimg, &phaimginfo)){
      headas_chat(NORMAL, "%s: Error: Unable to read '%s' file\n", global.tmpout.loc_phafile);
      goto Error;
    }
    
    /* Compute SubImages Box limits */
    if(ComputeBoxFromPhaImg(&phaimginfo, global.par.boxsize, &regbox)){
      headas_chat(NORMAL, "%s: Error: Unable to compute subimages boxes\n",  global.taskname);
       goto Error;
    }


    /* Compute Exposure correction coefficients */
    if(ComputeExpoCorr(expomapfile, phaimg, &phaimginfo, &regbox)){
      headas_chat(NORMAL, "%s: Error: Unable to compute exposure correction coefficients for subimages boxes\n",  global.taskname);
       goto Error;
    }


    /* Create temporary Filtered Event File */

/*     sprintf(global.tmpout.loc_filtered_infile, "%s/%d_outxsel.evt", global.tmpout.dirname, (int)pid); */
/*     sprintf(global.tmpout.xsel_infile, "%s/%d_xsel.xco", global.tmpout.dirname, pid); */
    sprintf(global.tmpout.loc_filtered_infile, "%d_srcreg-xsel.evt", (int)pid);
    sprintf(global.tmpout.xsel_infile, "%d_srcreg-xsel.xco", (int)pid);

    if(CreateXselXco(global.tmpout.loc_infile, "NONE", global.tmpout.loc_srcregionfile, global.tmpout.loc_filtered_infile, global.tmpout.xsel_infile))
      {
	headas_chat(NORMAL, "%s: Error: unable to create xselect temporary input file %s.\n", global.taskname, global.tmpout.xsel_infile);
	goto Error;
      }
    
    sprintf(cmd, "xselect @%s", global.tmpout.xsel_infile);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: unable to create temporary filtered event file '%s'\n", global.taskname, global.tmpout.loc_filtered_infile);
      goto Error;
    }

    if(FileExists(global.tmpout.xsel_infile)){
      if(remove (global.tmpout.xsel_infile) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.xsel_infile);
      }
    }
    

    for(i=0; i<regbox.nbox; i++){

      /* Create temporary Off-Axis File */
      sprintf(global.tmpout.offaxisfile, "%dtmp_oaf_%d.fits", (int)pid, i);

      if(WriteOffAxisFile(&aligninfo, optinfo, optcount, &optkeys, regbox.boxinfo[i].cx, regbox.boxinfo[i].cy, global.tmpout.optaxisgti, global.tmpout.offaxisfile))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.tmpout.offaxisfile);
	  goto Error;
	}

      /* Copy GTI extension from PHA to Off-Axis file */
      if( CopyHDUExt(global.tmpout.loc_phafile, global.tmpout.offaxisfile, KWVL_EXTNAME_GTI) ){
	headas_chat(NORMAL, "%s: Error: Unable to copy %s extension from '%s' to '%s' file.\n", global.taskname, KWVL_EXTNAME_GTI, global.tmpout.loc_phafile, global.tmpout.offaxisfile);
	goto Error;
      }

      /* Read GTI info from offaxisfile GTI extension */
      if(HDgti_read(global.tmpout.offaxisfile, &gti, KWVL_EXTNAME_GTI, 0, 0, 0, 0, &status)){
	headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' file.\n", global.taskname, global.tmpout.offaxisfile);
	goto Error;
      }

      /* Compute Off-Axis Histogram */
      /*   NOTE: 'global.filesinfo' global variable is updated with the informations */
      /*         about the off-axis histogram file structure */
      if(ComputeOffAxisHisto(&gti, global.tmpout.offaxisfile, global.filesinfo.thetabin, global.filesinfo.theta_dim, &histoinfo, &histocount)){
	headas_chat(NORMAL, "%s: Error: Unable to compute Off-Axis Histogram.\n", global.taskname);
	goto Error;
      }
      
      /* Compute Vignetting correction coefficients */
      regbox.boxinfo[i].vigncorr = (double*)calloc(global.filesinfo.energ_lo_dim, sizeof(double));
      ComputeVignCorr(vigninfo, vigncount, histoinfo, histocount, regbox.boxinfo[i].vigncorr);


      /* Compute Aperture Stop correction coefficient */
      if(global.par.apstopflag)
	{
	  /* Compute Aperture Stop correction coefficients */
	  if( ComputeApertureStopCorr(&gti, global.tmpout.offaxisfile, &apstopcorr) ){
	    headas_chat(NORMAL, "%s: Error: Unable to compute Aperture Stop correction coefficients.\n", global.taskname);
	    goto Error;
	  }
	}
      else
	{
	  apstopcorr.n_energy = 0;
	}


      /* Ghost Rays correction NOT applyed for extended sources */
      grcorr.n_energy = 0;


      /* Compute DETABS correction coefficients */

      regbox.boxinfo[i].detabscorr = (double*)malloc(global.filesinfo.energ_lo_dim*sizeof(double));
      
      if(global.par.detabsflag)
	{

	  /* Create temporary Region File */
	  /* sprintf(global.tmpout.loc_boxregionfile, "%s/%d_%d.reg", global.tmpout.dirname, (int)pid, i); */
	  sprintf(global.tmpout.loc_boxregionfile, "boxreg%d_%d.reg", (int)pid, i);
	  
	  if( CreateRegionFile(&regbox.boxinfo[i], global.tmpout.loc_boxregionfile) )
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.tmpout.loc_boxregionfile);
	      goto Error;
	    }


	  if( ComputeDETABSCorr(detabsinfo, global.tmpout.loc_filtered_infile, global.tmpout.loc_phafile, global.tmpout.loc_boxregionfile, global.filesinfo.energ_lo_dim, regbox.boxinfo[i].detabscorr) ){
	    headas_chat(NORMAL, "%s: Error: Unable to compute DETABS correction coefficients.\n", global.taskname);
	    goto Error;
	  }

	  /* Cleanup temporary Region File */
	  if(FileExists(global.tmpout.loc_boxregionfile)){
	    if(remove (global.tmpout.loc_boxregionfile) == -1){
	      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.loc_boxregionfile);
	    }
	  }

	}
      else
	{
	  for(j=0; j<global.filesinfo.energ_lo_dim; j++)
	    regbox.boxinfo[i].detabscorr[j] = 1.0;
	}


      /* Create 'PsfCorrVal_t' struct needed by 'CreateARFFile' routine */
      psfcorr.n_energy = 1;
      psfcorr.en = (double*)calloc(psfcorr.n_energy, sizeof(double));
      psfcorr.corr = (double*)calloc(psfcorr.n_energy, sizeof(double));
      psfcorr.en[0] = 0;
      psfcorr.corr[0] = regbox.boxinfo[i].expocorr;

      /* Create temporary ARF File */
      sprintf(regbox.boxinfo[i].arffile, "%dtmp_arf_%d.fits", (int)pid, i);

      if ( CreateARFFile(global.par.inarffile, regbox.boxinfo[i].arffile, regbox.boxinfo[i].vigncorr, &psfcorr, &apstopcorr, &grcorr, regbox.boxinfo[i].detabscorr) ){
	headas_chat(NORMAL, "%s: Error: unable to create '%s' ARF file.\n", global.taskname, regbox.boxinfo[i].arffile);
	goto Error;
      }


      if(UpdateArfKeys(&global.obsinfo,regbox.boxinfo[i].arffile)){
	headas_chat(NORMAL, "%s: Error: unable to update '%s' ARF file.\n", global.taskname, regbox.boxinfo[i].arffile);
	goto Error;
      }

      /* Free memory */

      FreeApStopCorrVal(&apstopcorr);
      FreePsfCorrVal(&psfcorr);
      free(histoinfo);


      /* Cleanup Temporary Files */

      if(FileExists(global.tmpout.offaxisfile)){
	if(remove (global.tmpout.offaxisfile) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.offaxisfile);
	}
      }


    } /* for(i=0; i<regbox.nbox; i++) */


    /* Compute Box weight */
    if(global.par.flatflag){
      if(SetFlatBoxWeight(global.tmpout.loc_phafile, &regbox)){
	headas_chat(NORMAL, "%s: Error: Unable to compute subimages boxes weight\n",  global.taskname);
	goto Error;
      }
    }
    else{
      if(ComputeBoxWeight(global.tmpout.loc_phafile, &global.reginfo, vigninfo, &regbox)){
	headas_chat(NORMAL, "%s: Error: Unable to compute subimages boxes weight\n",  global.taskname);
	goto Error;
      }
    }

    /* Write temporary addarf input ascii file name */
    sprintf(global.tmpout.arflistfile, "%dtmp_arflist.txt", (int)pid);
    if(WriteArfListFile(&regbox, global.tmpout.arflistfile)){
      headas_chat(NORMAL, "%s: Error: Unable to write '%s' temporary file.\n", global.taskname, global.tmpout.arflistfile);
      goto Error; 
    }

    /* Derive temporary addarf output file name */
    SplitFilePath(global.par.outfile, DirName, BaseName);
    sprintf(global.tmpout.addarf_outfile, "%s/%s", global.tmpout.dirname, BaseName);
    
    /* Execute addarf */
    sprintf(cmd, "addarf list=@%s out_ARF=%s", global.tmpout.arflistfile, global.tmpout.addarf_outfile);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: unable to create '%s' output ARF file.\n", global.taskname, global.tmpout.addarf_outfile);
      goto Error;
    }

    if(UpdateArfKeys(&global.obsinfo, global.tmpout.addarf_outfile)){
      headas_chat(NORMAL, "%s: Error: unable to update '%s' ARF file.\n", global.taskname, global.tmpout.addarf_outfile);
      goto Error;
    }

    /* Rename addrmf output file */
    if ( RenameFile(global.tmpout.addarf_outfile, global.par.outfile) == -1 )
      {
	headas_chat(NORMAL, "%s: Error: Unable to copy temporary file '%s' to '%s'.\n", global.taskname, global.tmpout.addarf_outfile, global.par.outfile);
	goto Error;
      }


    /* Cleanup Temporary Files */

    if ( FileExists(global.tmpout.expomap ) ) {
      if(remove (global.tmpout.expomap) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.expomap);
      }
    }
    
    if ( FileExists(global.tmpout.aspecthisto ) ) {
      if(remove (global.tmpout.aspecthisto) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.aspecthisto);
      }
    }

    if(FileExists(global.tmpout.loc_filtered_infile)){
      if(remove (global.tmpout.loc_filtered_infile) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.loc_filtered_infile);
      }
    }

    if(FileExists(global.tmpout.loc_filtered_phafile)){
      if(remove (global.tmpout.loc_filtered_phafile) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.loc_filtered_phafile);
      }
    }

    if(global.par.cleanup && FileExists(global.tmpout.arflistfile)){
      if(remove (global.tmpout.arflistfile) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.arflistfile);
      }
    }

    for(i=0; i<regbox.nbox; i++){
      if(global.par.cleanup && FileExists(regbox.boxinfo[i].arffile)){
	if(remove (regbox.boxinfo[i].arffile) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,regbox.boxinfo[i].arffile);
	}
      }
    }

  }
  /* ------------------------------------------------------------------------------------------------ */
  /*     End -> Extended Source    */
  /* ------------------------------------------------------------------------------------------------ */
  else{

    /* Read REGION info from input PHA file */
    if( ReadRegionInfo(global.par.phafile, "REG00101", &global.reginfo) ){
      headas_chat(NORMAL, "%s: Error: Unable to get REGION Info from input '%s' file.\n", global.taskname, global.par.phafile);
      goto Error;
    } 
    if( !strcmp(global.reginfo.shape, "CIRCLE") )
      headas_chat(CHATTY, "%s: Info: REGION -> SHAPE='%s' X=%f Y=%f R=%f\n", global.taskname, global.reginfo.shape, global.reginfo.X[0], global.reginfo.Y[0], global.reginfo.R[0]);
    if( !strcmp(global.reginfo.shape, "ANNULUS") )
      headas_chat(CHATTY, "%s: Info: REGION -> SHAPE='%s' X=%f Y=%f R1=%f R2=%f\n", global.taskname, global.reginfo.shape, global.reginfo.X[0], global.reginfo.Y[0], global.reginfo.R[0], global.reginfo.R1[0]);
    if( !strcmp(global.reginfo.shape, "ELLIPSE") )
      headas_chat(CHATTY, "%s: Info: REGION -> SHAPE='%s' X=%f Y=%f A=%f B=%f ANGLE=%f\n", global.taskname, global.reginfo.shape, global.reginfo.X[0], global.reginfo.Y[0], global.reginfo.R[0], global.reginfo.R1[0], global.reginfo.rotang[0]);
    
    
    /* Only 'CIRCLE' 'ANNULUS' and 'ELLIPSE' Regions are supported */
    if( strcmp(global.reginfo.shape, "CIRCLE") && strcmp(global.reginfo.shape, "ANNULUS") && strcmp(global.reginfo.shape, "ELLIPSE") ){
      headas_chat(NORMAL, "%s: Error: Region shape '%s' not supported for point-like sources.\n", global.taskname, global.reginfo.shape);
      goto Error;
    }


    /* Ghost Rays correction supported only for 'CIRCLE' regions */
    if( strcmp(global.reginfo.shape, "CIRCLE") && global.par.grflag ){
      headas_chat(NORMAL, "%s: Error: Ghost Rays correction is supported only for circular extraction regions.\n", global.taskname);
      goto Error;
    }


    /* Read PHA image extension */
    if(ReadPhaImg(global.par.phafile, &phaimg, &phaimginfo)){
      headas_chat(NORMAL, "%s: Error: Unable to read '%s' file\n", global.par.phafile);
      goto Error;
    }

    /* Define exposure maps center and size */
    if(global.par.cutmaps){
      skyx = global.reginfo.X[0] ;
      skyy = global.reginfo.Y[0] ;
      skysize = (int)MAX(phaimginfo.x_width,phaimginfo.y_width);
    }
    else{
      skyx = 500.5;
      skyy = 500.5;
      skysize = 1000;
    }

    /* Write output Off-Axis File */
    if(WriteOffAxisFile(&aligninfo, optinfo, optcount, &optkeys, global.reginfo.X[0], global.reginfo.Y[0], global.tmpout.optaxisgti, global.tmpout.offaxisfile))
      {
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.tmpout.offaxisfile);
	goto Error;
      }
    
    /* Copy GTI extension from PHA to Off-Axis file */
    if( CopyHDUExt(global.par.phafile, global.tmpout.offaxisfile, KWVL_EXTNAME_GTI) ){
      headas_chat(NORMAL, "%s: Error: Unable to copy %s extension from '%s' to '%s' file.\n", global.taskname, KWVL_EXTNAME_GTI, global.par.phafile, global.tmpout.offaxisfile);
      goto Error;
    }

    if(global.createoffaxisfile)
      headas_chat(NORMAL, "%s: Info: Off-Axis File '%s' created.\n", global.taskname, global.tmpout.offaxisfile);
    

    /* Read GTI info from offaxisfile GTI extension */
    if(HDgti_read(global.tmpout.offaxisfile, &gti, KWVL_EXTNAME_GTI, 0, 0, 0, 0, &status)){
      headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' file.\n", global.taskname, global.tmpout.offaxisfile);
      goto Error;
    }

    /* Compute Off-Axis Histogram */
    /*   NOTE: 'global.filesinfo' global variable is updated with the informations */
    /*         about the off-axis histogram file structure */
    if(ComputeOffAxisHisto(&gti, global.tmpout.offaxisfile, global.filesinfo.thetabin, global.filesinfo.theta_dim, &histoinfo, &histocount)){
      headas_chat(NORMAL, "%s: Error: Unable to compute Off-Axis Histogram.\n", global.taskname);
      goto Error;
    }

    /* Create output Off-Axis Histogram file */
    if(WriteOffAxisHistoFile(histoinfo, histocount, global.tmpout.offaxisfile, global.tmpout.offaxishisto)){
      headas_chat(NORMAL, "%s: Error: Unable to create output Off-Axis Histogram file '%s'\n", global.taskname, global.tmpout.offaxishisto);
      goto Error;
    }
  
    if(global.createoffaxishisto)
      headas_chat(NORMAL, "%s: Info: Off-Axis Histogram File '%s' created.\n", global.taskname, global.tmpout.offaxishisto);
    

    /* Compute PSF correction coefficients */
    if(global.par.psfflag)
      {
	if( ComputePsfCorrByEnergy(&gti, global.tmpout.offaxisfile, skyx, skyy, skysize, &psfcorr) ){
	  headas_chat(NORMAL, "%s: Error: Unable to compute PSF correction coefficients.\n", global.taskname);
	  goto Error;
	}
      }
    else
      {
	psfcorr.n_energy = 0;
      }


    /* Compute Vignetting correction coefficients */
    vigncorr = (double*)calloc(global.filesinfo.energ_lo_dim, sizeof(double));
    ComputeVignCorr(vigninfo, vigncount, histoinfo, histocount, vigncorr);
    

    /* Compute Aperture Stop correction coefficients */
    if(global.par.apstopflag)
      {
	if( ComputeApertureStopCorr(&gti, global.tmpout.offaxisfile, &apstopcorr) ){
	  headas_chat(NORMAL, "%s: Error: Unable to compute Aperture Stop correction coefficients.\n", global.taskname);
	  goto Error;
	}
      }
    else
      {
	apstopcorr.n_energy = 0;
      }
    

    /* Compute Ghost Rays correction coefficients */
    if(global.par.grflag)
      {
     

	if( ComputeGhostRaysCorr(&gti, global.tmpout.offaxisfile, global.reginfo.R[0], &grcorr) ){
	  headas_chat(NORMAL, "%s: Error: Unable to compute Ghost Rays correction coefficients.\n", global.taskname);
	  goto Error;
	}
      }
    else
      {
	grcorr.n_energy = 0;
      }


    /* Compute DETABS correction coefficients */

    detabscorr = (double*)malloc(global.filesinfo.energ_lo_dim*sizeof(double));

    if(global.par.detabsflag)
      {
	if( ComputeDETABSCorr(detabsinfo, global.tmpout.loc_infile, global.tmpout.loc_phafile, global.tmpout.loc_srcregionfile, global.filesinfo.energ_lo_dim, detabscorr) ){
	  headas_chat(NORMAL, "%s: Error: Unable to compute DETABS correction coefficients.\n", global.taskname);
	  goto Error;
	}
      }
    else
      {
	for(j=0; j<global.filesinfo.energ_lo_dim; j++)
	  detabscorr[j] = 1.0;
      }


    /* Correct output ARF file */
    if ( CreateARFFile(global.par.inarffile, global.par.outfile, vigncorr, &psfcorr, &apstopcorr, &grcorr, detabscorr) )
      {
	headas_chat(NORMAL, "%s: Error: unable to create '%s' output ARF file.\n", global.taskname, global.par.outfile);
	goto Error;
      }


    /* Free memory */

    FreeApStopCorrVal(&apstopcorr);
    FreePsfCorrVal(&psfcorr);


    if(UpdateArfKeys(&global.obsinfo,global.par.outfile)){
      headas_chat(NORMAL, "%s: Error: unable to update '%s' ARF file.\n", global.taskname, global.par.outfile);
      goto Error;
    }

    /* Cleanup Temporary Files */
    
    if ( (!global.createoffaxisfile) && FileExists(global.tmpout.offaxisfile) ) {
      if(remove (global.tmpout.offaxisfile) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.offaxisfile);
      }
    }
    
    if ( (!global.createoffaxishisto) && FileExists(global.tmpout.offaxishisto) ) {
      if(remove (global.tmpout.offaxishisto) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.offaxishisto);
      }
    }

  } /* End -> else [if(global.par.extended)] */


  /* Cleanup Temporary File */

  if ( FileExists(global.tmpout.optaxisgti) ) {
    if(remove (global.tmpout.optaxisgti) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.optaxisgti);
    }
  }

  if ( FileExists("xselect.log") ) {
    if(remove ("xselect.log") == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove 'xselect.log' temporary file.\n", global.taskname);
    }
  }

  if ( FileExists("xsel_timefile.asc") ) {
    if(remove ("xsel_timefile.asc") == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove 'xsel_timefile.asc' temporary file.\n", global.taskname);
    }
  }


  /* Cleanup Temporary local links */

  if( unlink(global.tmpout.loc_infile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.loc_infile);
  }

  if( unlink(global.tmpout.loc_phafile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.loc_phafile);
  }

  if( unlink(global.tmpout.loc_srcregionfile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.loc_srcregionfile);
  }


  /* Delete the task temporary directory */
  if(rmdir(global.tmpout.dirname)){
    perror("rmdir");
    headas_chat(NORMAL, "%s: Warning: Unable to delete the temporary directory '%s'\n", global.taskname, global.tmpout.dirname);
  }


  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  
  
/*  ok_end:  */

  return OK; 
  
 Error:

  return NOT_OK;
} /* numkarf_work */


/*
 *	numkarf
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
 *             void numkarf_getpar(void);
 * 	       void numkarf_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 13/12/11 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int numkarf()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUMKARF_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( numkarf_getpar() == OK) 
    {
      
      if ( numkarf_work()) 
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
	}
      
    }
  
  return OK;
  
 pdcorr_end:

  return NOT_OK;
  
} /* numkarf */


/*
 *	numkarf_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 13/12/11 - First version
 *
 */
void numkarf_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input PHA FITS File                       :'%s'\n",global.par.phafile);
  headas_chat(NORMAL,"Name of the output ARF FITS File                      :'%s'\n",global.par.outfile);
  headas_chat(NORMAL,"Name of the input Optical Axis File                   :'%s'\n",global.par.optaxisfile);
  headas_chat(NORMAL,"Name of the output Off-Axis File                      :'%s'\n",global.par.offaxisfile);
  headas_chat(NORMAL,"Name of the output Off-Axis Histogram                 :'%s'\n",global.par.offaxishisto);
  headas_chat(NORMAL,"Name of the input ARF File                            :'%s'\n",global.par.inarffile);
  headas_chat(NORMAL,"Name of the input 2D-PSF Grouping File                :'%s'\n",global.par.grppsffile);
  headas_chat(NORMAL,"Name of the input 2D-PSF files directory              :'%s'\n",global.par.psfdir);

  headas_chat(NORMAL,"Name of the input Vignetting File                     :'%s'\n",global.par.vignfile);
  if (global.par.psfflag)
    headas_chat(NORMAL,"Apply PSF correction                                  : yes\n");
  else
    headas_chat(NORMAL,"Apply PSF correction                                  : no\n");

  if (global.par.vignflag)
    headas_chat(NORMAL,"Apply Vignetting correction                           : yes\n");
  else
    headas_chat(NORMAL,"Apply Vignetting correction                           : no\n");

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");

} /* numkarf_info */


/*
 *	numkarf_checkinput
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
 *        0.1.0: - NS 13/12/11 - First version
 *
 */
int numkarf_checkinput(void)
{
  pid_t    tmp;
  
  /* Get pid */
  tmp=getpid();

  /* Check if offaxisfile == NONE */    
  if (!(strcasecmp (global.par.offaxisfile, DF_NONE)))
    {
      global.createoffaxisfile = FALSE;

      /* Derive temporary file name */
      sprintf(global.tmpout.offaxisfile, "%dtmp_oaf.fits", (int)tmp);
    }
  else
    {
      global.createoffaxisfile = TRUE;

      strcpy(global.tmpout.offaxisfile, global.par.offaxisfile);

      if(FileExists(global.tmpout.offaxisfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.offaxisfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.offaxisfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.offaxisfile);
	      if(remove (global.tmpout.offaxisfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.offaxisfile);
		  goto check_end;
		}
	    }
	}
    }


  /* Check if offaxishisto == NONE */    
  if (!(strcasecmp (global.par.offaxishisto, DF_NONE)))
    {
      global.createoffaxishisto = FALSE;

      /* Derive temporary file name */
      sprintf(global.tmpout.offaxishisto, "%dtmp_oah.fits", (int)tmp);
    }
  else
    {
      global.createoffaxishisto = TRUE;

      strcpy(global.tmpout.offaxishisto, global.par.offaxishisto);

      if(FileExists(global.tmpout.offaxishisto))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.offaxishisto);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.offaxishisto);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.offaxishisto);
	      if(remove (global.tmpout.offaxishisto) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.offaxishisto);
		  goto check_end;
		}
	    }
	}
    }


  /* Check if apstophisto == NONE */    
  if (!(strcasecmp (global.par.apstophisto, DF_NONE)))
    {
      global.createapstophisto = FALSE;

      /* Derive temporary file name */
      sprintf(global.tmpout.apstophisto, "%dtmp_oah.fits", (int)tmp);
    }
  else
    {
      global.createapstophisto = TRUE;

      strcpy(global.tmpout.apstophisto, global.par.apstophisto);

      if(FileExists(global.tmpout.apstophisto))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.apstophisto);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.apstophisto);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.apstophisto);
	      if(remove (global.tmpout.apstophisto) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.apstophisto);
		  goto check_end;
		}
	    }
	}
    }


  /* Check if grhisto == NONE */    
  if (!(strcasecmp (global.par.grhisto, DF_NONE)))
    {
      global.creategrhisto = FALSE;

      /* Derive temporary file name */
      sprintf(global.tmpout.grhisto, "%dtmp_grh.fits", (int)tmp);
    }
  else
    {
      global.creategrhisto = TRUE;

      strcpy(global.tmpout.grhisto, global.par.grhisto);

      if(FileExists(global.tmpout.grhisto))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.grhisto);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.grhisto);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.grhisto);
	      if(remove (global.tmpout.grhisto) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.grhisto);
		  goto check_end;
		}
	    }
	}
    }


  /* Check if outfile already exists */
  if(FileExists(global.par.outfile))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outfile);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outfile);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
	  if(remove (global.par.outfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
	      goto check_end;
	    }
	}
    }

  
  /* Derive temporary Optical Axis GTI file name */
  sprintf(global.tmpout.optaxisgti, "%dtmp_gti.fits", (int)tmp);

  if(FileExists(global.tmpout.optaxisgti))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.optaxisgti);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.optaxisgti);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.optaxisgti);
	  if(remove (global.tmpout.optaxisgti) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.optaxisgti);
	      goto check_end;
	    }
	}
    }

  /* Set temporary task directory name */
  sprintf(global.tmpout.dirname, "%d_tmp_numkarf", (int)tmp);


  /* Azimuth angle bin size must be submultiple of 360  */
  if(fmod(360.0,global.par.phibin)!=0)
    {
      headas_chat(NORMAL, "%s: Error: Azimuth angle bin size %f not allowed (must be submultiple of 360)\n", global.taskname, global.par.phibin);
      goto check_end;
    }

  /* Azimuth angle bin size for Aperture Stop must be submultiple of 360  */
  if(fmod(360.0,global.par.apstopphibin)!=0)
    {
      headas_chat(NORMAL, "%s: Error: Azimuth angle bin size %f for Aperture Stop not allowed (must be submultiple of 360)\n", global.taskname, global.par.apstopphibin);
      goto check_end;
    }


  if( global.par.psfflag && global.par.extended )
    {
      headas_chat(NORMAL, "%s: Warning: PSF correction used only for point-like sources, setting 'psfflag' parameter to 'no'.\n", global.taskname);
      global.par.psfflag = FALSE;
    }


  if( global.par.grflag && global.par.extended )
    {
      headas_chat(NORMAL, "%s: Warning: Ghost Rays correction supported only for point-like sources, setting 'grflag' parameter to 'no'.\n", global.taskname);
      global.par.grflag = FALSE;
    }

 
  return OK;

 check_end:
  return NOT_OK;

}  /* numkarf_checkinput */


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
  

  if(ExistsKeyWord(&head, KWNM_TELESCOP, &card))
    {
      strcpy(obsinfo->telescop, card->u.SVal);
      strcpy(obsinfo->telescop_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TELESCOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Retrieve INSTRUME from input file */  
  if(ExistsKeyWord(&head, KWNM_INSTRUME, &card))
    {
      strcpy(obsinfo->instrume, card->u.SVal);
      strcpy(obsinfo->instrume_comm, card->Comment);
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

  /* Retrieve observation start time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      obsinfo->tstart=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Retrieve observation end time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      obsinfo->tstop=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Get MJDREF value */
  status = 0;
  obsinfo->mjdref = HDget_frac_time(inunit, KWNM_MJDREF, 0,0, &status);
  if(status){
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREF);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto GetObsInfo_end;
  }

  /* Retrieve PA_PNT from input event file */
  if((ExistsKeyWord(&head, KWNM_PA_PNT, &card)))
    {
      obsinfo->pa_pnt=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_PA_PNT);
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
      return NOT_OK;
    }
  
  /* Move to <extname> extension */

  if (fits_movnam_hdu(inunit, ANY_HDU, extname, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find REGION extension '%s' in\n", global.taskname, extname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
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

  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      return NOT_OK;
    }

  return OK; 
  
 ReadRegionInfo_end:

  CloseFitsFile(inunit);

  if (inhead.first)
    ReleaseBintable(&inhead, &table);
  
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


  /* Check if optaxisfile is appropriate for input pha file */

  if( global.obsinfo.tstart<optkeys->tstart || global.obsinfo.tstop>optkeys->tstop )
    headas_chat(NORMAL, "%s: Warning: pha file time range not covered by %s time range (check TSTART and/or TSTOP)\n", global.taskname, filename);
  
  if( strcasecmp(global.obsinfo.instrume,optkeys->instrume) )
    headas_chat(NORMAL, "%s: Warning: INSTRUME keywords of %s file and pha file are not consistent\n", global.taskname, filename);



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

  if ((indxcol.DET2X_APSTOP = GetColNameIndx(&table, CLNM_DET2X_APSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DET2X_APSTOP);
      goto ReadOptAxisFile_end;
    }

  if ((indxcol.DET2Y_APSTOP = GetColNameIndx(&table, CLNM_DET2Y_APSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DET2Y_APSTOP);
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
	 (*optinfo)[count].det2x_apstop = DVEC(table,n,indxcol.DET2X_APSTOP);
	 (*optinfo)[count].det2y_apstop = DVEC(table,n,indxcol.DET2Y_APSTOP);

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


int WriteOffAxisFile(AlignInfo_t *aligninfo, OptAxisInfo_t *optinfo, int optcount, OptAxisKeys_t *optkeys, double cx, double cy, char *infile, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open metrology input file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, infile);
      goto WriteOffAxisFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteOffAxisFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WriteOffAxisFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteOffAxisFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOffAxisFile_end;
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
      goto WriteOffAxisFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteOffAxisFile_end;
      }
    }

  /* Create 'OFF_AXIS' Ext  */
  if (WriteOffAxisExt(aligninfo, optinfo, optcount, optkeys, cx, cy, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_OFF_AXIS);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteOffAxisFile_end;
    }
  hducount++;

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteOffAxisFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteOffAxisFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteOffAxisFile_end;
    }


  return OK;
  
 WriteOffAxisFile_end:

  return NOT_OK;


} /* WriteOffAxisFile */


int WriteOffAxisExt(AlignInfo_t *aligninfo, OptAxisInfo_t *optinfo, int nrows, OptAxisKeys_t *optkeys, double cx, double cy, FitsFileUnit_t ounit){

  int             n, count=0;
  double          det2x_oa=0., det2y_oa=0., deltax_det2, deltay_det2, deltax_cen, deltay_cen;
  AtQuat          Qdet2ob;
  AtRotMat        Rdet2ob;  /* rotation matrix */
  unsigned        OutRows=0;
  double          dist, offaxis, phi;
  OffAxisCol_t    indxcol;
  Bintable_t	  table; 
  FitsHeader_t	  newhead;
  char            crval[FLEN_VALUE];
  char            date[25];
  Version_t       nustardas_v;          /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );


  /*  FPMA or FPMB */
  if(!strcasecmp(global.obsinfo.instrume,KWVL_INSTRUME_FPMA)){
    det2x_oa = aligninfo->x_det2a;
    det2y_oa = aligninfo->y_det2a;
    memcpy( Qdet2ob, aligninfo->Qdet2Aob, 4*sizeof(double) );
  }
  else{
    det2x_oa = aligninfo->x_det2b;
    det2y_oa = aligninfo->y_det2b;
    memcpy( Qdet2ob, aligninfo->Qdet2Bob, 4*sizeof(double) );
  }


  /* Find rotation matrix */
  if(atQuatToRM(Qdet2ob, Rdet2ob)){
    headas_chat(CHATTY,"Error: unable to compute rotation matrix.\n");
    goto WriteOffAxisExt_end;
  }
  

  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(BINTAB_ROWS, &table);

  /* Add columns */

  AddColumn(&newhead, &table, CLNM_TIME, "Event Time (seconds since Jan 2010 00:00:00 UTC)", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_OFF_AXIS, CARD_COMM_OFF_AXIS, "1D", TUNIT, UNIT_ARCMIN, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_PHI, CARD_COMM_PHI, "1D", TUNIT, UNIT_DEG, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DELTAX_CEN, CARD_COMM_DELTAX_CEN, "1D", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DELTAY_CEN, CARD_COMM_DELTAY_CEN, "1D", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);


  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_OFF_AXIS, CARD_COMM_EXTNAME);

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

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto WriteOffAxisExt_end;
    }

  if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS);
      goto WriteOffAxisExt_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, CLNM_PHI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_PHI);
      goto WriteOffAxisExt_end;
    }

  if ((indxcol.DELTAX_CEN = GetColNameIndx(&table, CLNM_DELTAX_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAX_CEN);
      goto WriteOffAxisExt_end;
    }

  if ((indxcol.DELTAY_CEN = GetColNameIndx(&table, CLNM_DELTAY_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAY_CEN);
      goto WriteOffAxisExt_end;
    }


  OutRows = 0;
  count = 0;
  
  while(count<nrows){
    
    for(n=0; (n<BINTAB_ROWS)&&(count<nrows); n++){
      
      /* Compute the distance from the center (cx,cy) */
      dist = sqrt( pow((cx - optinfo[count].x_oa),2) + pow((cy - optinfo[count].y_oa),2) );
      
      /* Convert to arcmin */
      offaxis = dist * SUBPIX_SIZE_ARCMIN;
      
      DVEC(table, n, indxcol.TIME)=optinfo[count].time;
      DVEC(table, n, indxcol.OFF_AXIS)= offaxis;
      
      /* Compute the source azimuthal angle */
      phi = atan2( (cy-optinfo[count].y_oa), (cx-optinfo[count].x_oa) ) * (180/M_PI);
      phi = NormalaziedAt360Angle(phi);
      
      DVEC(table, n, indxcol.PHI)= phi;

      deltax_det2 = (optinfo[count].det2x_apstop - det2x_oa) * SUBPIX_SIZE_MM ;
      deltay_det2 = (optinfo[count].det2y_apstop - det2y_oa) * SUBPIX_SIZE_MM ;

      deltax_cen = deltax_det2 * Rdet2ob[0][0] + deltay_det2 * Rdet2ob[0][1] ;
      deltay_cen = deltax_det2 * Rdet2ob[1][0] + deltay_det2 * Rdet2ob[1][1] ;
      
      DVEC(table, n, indxcol.DELTAX_CEN)= deltax_cen;
      DVEC(table, n, indxcol.DELTAY_CEN)= deltay_cen;


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
  
 WriteOffAxisExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteOffAxisExt */


int ComputeOffAxisHisto(struct gti_struct *gti, char *infile, double offbinsize, int offbinnum, OffAxisHistoInfo_t **histoinfo, int *histocount){

  unsigned           FromRow, ReadRows, n, nCols;
  int                i, j, status=OK, init=OK, offidx, phiidx;
  double             tstart=0, tstop=0, dtime=0, off_axis=0, phi=0;
  OffAxisCol_t       indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Set Azimuth angle bin number and size */
  global.filesinfo.phibin = global.par.phibin;
  global.filesinfo.phibin_dim = 360/global.filesinfo.phibin;

  /* Set Off-axis bin number and size */
  global.filesinfo.offbin = offbinsize;
  global.filesinfo.offbin_dim = offbinnum;


  /* headas_chat(NORMAL, "%s: Info: Processing '%s' file to create Off-Axis Histogram.\n", global.taskname, infile); */
  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, infile);
      return NOT_OK;
    }
 
  /* Move in OFF_AXIS extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_OFF_AXIS, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OFF_AXIS);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile); 
      goto ComputeOffAxisHisto_end;
    }
  
  head=RetrieveFitsHeader(unit);


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, infile);
      goto ComputeOffAxisHisto_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto ComputeOffAxisHisto_end;
    }

  if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS);
      goto ComputeOffAxisHisto_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, CLNM_PHI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_PHI);
      goto ComputeOffAxisHisto_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *histocount = offbinnum*global.filesinfo.phibin_dim;
 *histoinfo = (OffAxisHistoInfo_t *)calloc(*histocount, sizeof(OffAxisHistoInfo_t));
 if(*histoinfo==NULL){
   headas_chat(CHATTY,"%s: Error: ComputeOffAxisHisto: memory allocation failure.\n", global.taskname);
   goto ComputeOffAxisHisto_end;
 }

 for(i=0; i<offbinnum; i++){
   for(j=0; j<global.filesinfo.phibin_dim; j++){
     (*histoinfo)[ i*global.filesinfo.phibin_dim + j ].off_axis = offbinsize*i;
     (*histoinfo)[ i*global.filesinfo.phibin_dim + j ].phi = global.filesinfo.phibin*j + global.filesinfo.phibin/2;
     (*histoinfo)[ i*global.filesinfo.phibin_dim + j ].duration = 0;
   }
 }


 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
   {
     for(n=0; n<ReadRows ; n++)
       {

	 if(init){
	   /* Update 'tstart' 'off_axis' and 'phi' values */
	   tstart = DVEC(table, n, indxcol.TIME);
	   off_axis = DVEC(table, n, indxcol.OFF_AXIS);
	   phi = DVEC(table, n, indxcol.PHI);
	   init = NOT_OK;
	 }
	 else{
	   /* Update 'tstop' value */
	   tstop = DVEC(table, n, indxcol.TIME);
	   dtime = tstop-tstart;


	   /* Compute bin offidx (between 0 - offbinnum-1) */	   
	   offidx = GetInitBinIndex(offbinsize, 0, 0, offbinnum-1, off_axis);

	   /* Compute bin phiidx (between 0 - global.filesinfo.phibin_dim-1) */	   
	   phiidx = GetCenterBinIndex(global.filesinfo.phibin, 0, 0, global.filesinfo.phibin_dim-1, phi);
	   
	   /* Compute overlap exposure of the time bin with GTI */
	   dtime = HDgti_exp(tstart, tstop, gti, &status);
	   if(status!=OK){
	     headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
	     goto ComputeOffAxisHisto_end;
	   }

	   /* Update duration value */
	   (*histoinfo)[offidx*global.filesinfo.phibin_dim + phiidx].duration += dtime;


	   /* Update 'tstart' 'off_axis' and 'phi' values */
	   tstart = DVEC(table, n, indxcol.TIME);
	   off_axis = DVEC(table, n, indxcol.OFF_AXIS);
	   phi = DVEC(table, n, indxcol.PHI);
	 }

       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 


 /* Update duration value of the last row */	   
 tstop = tstart + dtime;

 offidx = GetInitBinIndex(offbinsize, 0, 0, offbinnum-1, off_axis);
 phiidx = GetCenterBinIndex(global.filesinfo.phibin, 0, 0, global.filesinfo.phibin_dim-1, phi);

 dtime = HDgti_exp(tstart, tstop, gti, &status);
 if(status!=OK){
   headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
   goto ComputeOffAxisHisto_end;
 }

 (*histoinfo)[offidx*global.filesinfo.phibin_dim + phiidx].duration += dtime;


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile);
      return NOT_OK;
    }

  return OK;

  
 ComputeOffAxisHisto_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ComputeOffAxisHisto */


int WriteOffAxisHistoFile(OffAxisHistoInfo_t *histoinfo, int histocount, char *infile, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open metrology input file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, infile);
      goto WriteOffAxisHistoFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteOffAxisHistoFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WriteOffAxisHistoFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteOffAxisHistoFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOffAxisHistoFile_end;
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
      goto WriteOffAxisHistoFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteOffAxisHistoFile_end;
      }
    }

  /* Create 'OFFAXIS_HISTO' Ext  */
  if (WriteOffAxisHistoExt(histoinfo, histocount, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_OFFAXIS_HISTO);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteOffAxisHistoFile_end;
    }
  hducount++;

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteOffAxisHistoFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteOffAxisHistoFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteOffAxisHistoFile_end;
    }


  return OK;
  
 WriteOffAxisHistoFile_end:

  return NOT_OK;

} /* WriteOffAxisHistoFile */


int WriteOffAxisHistoExt(OffAxisHistoInfo_t *histoinfo, int nrows, FitsFileUnit_t ounit){

  int                n, count=0;
  unsigned           OutRows=0;
  OffAxisHistoCol_t  indxcol;
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

  AddColumn(&newhead, &table, CLNM_OFF_AXIS, CARD_COMM_OFF_AXIS, "1D", TUNIT, UNIT_ARCMIN, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_PHI, CARD_COMM_PHI, "1D", TUNIT, UNIT_DEG, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DURATION, CARD_COMM_DURATION, "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_OFFAXIS_HISTO, CARD_COMM_EXTNAME);

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

  if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS);
      goto WriteOffAxisHistoExt_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, CLNM_PHI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_PHI);
      goto WriteOffAxisHistoExt_end;
    }

  if ((indxcol.DURATION = GetColNameIndx(&table, CLNM_DURATION)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DURATION);
      goto WriteOffAxisHistoExt_end;
    }


  OutRows = 0;
  count = 0;

  while(count<nrows){

    for(n=0; (n<BINTAB_ROWS)&&(count<nrows); n++){
      
      DVEC(table, n, indxcol.OFF_AXIS) = histoinfo[count].off_axis;
      DVEC(table, n, indxcol.PHI) = histoinfo[count].phi;
      DVEC(table, n, indxcol.DURATION) = histoinfo[count].duration;

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
  
 WriteOffAxisHistoExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteOffAxisHistoExt */


/* int ReadOffAxisHistoFile(char *filename, OffAxisHistoInfo_t ** info, int *ncount){ */

/*   unsigned           FromRow, ReadRows, n, nCols; */
/*   int                count=0, status=OK; */
/*   OffAxisHistoCol_t  indxcol; */
/*   Bintable_t	     table; */
/*   FitsHeader_t	     head; */
/*   FitsFileUnit_t     unit=NULL; */

/*   TMEMSET0( &table, Bintable_t ); */
/*   TMEMSET0( &head, FitsHeader_t ); */

/*   headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, filename); */
/*   /\* Open readonly input file *\/ */
/*   if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0) */
/*     { */
/*       headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname); */
/*       headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename); */
/*       return NOT_OK; */
/*     } */
 
/*   /\* Move in 'OFFAXIS_HISTO' extension in input file *\/ */
/*   if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_OFFAXIS_HISTO, 0, &status)) */
/*     { */
/*       headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OFFAXIS_HISTO); */
/*       headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); */
/*       goto ReadOffAxisHistoFile_end; */
/*     } */
  
/*   head=RetrieveFitsHeader(unit); */


/*   GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL); */
/*   if(!table.MaxRows) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename); */
/*       goto ReadOffAxisHistoFile_end; */
/*     } */


/*   /\* Get needed columns number from name *\/ */

/*   if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 ) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS); */
/*       goto ReadOffAxisHistoFile_end; */
/*     } */

/*   if ((indxcol.DURATION = GetColNameIndx(&table, CLNM_DURATION)) == -1 ) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DURATION); */
/*       goto ReadOffAxisHistoFile_end; */
/*     } */


/*   EndBintableHeader(&head, &table); */


/*  /\* Allocate memory to storage all data *\/ */
/*  *ncount = table.MaxRows; */
/*  *info = (OffAxisHistoInfo_t *)calloc(*ncount, sizeof(OffAxisHistoInfo_t)); */
/*  if(*info==NULL){ */
/*    headas_chat(CHATTY,"%s: Error: ReadOffAxisHistoFile: memory allocation failure.\n", global.taskname); */
/*    goto ReadOffAxisHistoFile_end; */
/*  } */

/*  /\* Read Bintable *\/ */
/*  FromRow = 1; */
/*  ReadRows=table.nBlockRows; */
/*  nCols=table.nColumns; */

/*  while((count<*ncount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)) */
/*    { */
/*      for(n=0; n<ReadRows ; ++n) */
/*        { */
/* 	 (*info)[count].off_axis = DVEC(table, n, indxcol.OFF_AXIS); */
/* 	 (*info)[count].duration = JVEC(table, n, indxcol.DURATION); */

/* 	 count++; */
/*        } */

/*      FromRow += ReadRows; */
/*      ReadRows = BINTAB_ROWS; */
/*    }/\* while *\/ */
   
/*   *ncount = count; */


/*   /\* Free memory allocated with bintable data *\/ */
/*   ReleaseBintable(&head, &table); */
  
/*   /\* Close file *\/ */
/*   if (CloseFitsFile(unit)) */
/*     { */
/*       headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname); */
/*       headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); */
/*       return NOT_OK; */
/*     } */

/*   return OK; */

  
/*  ReadOffAxisHistoFile_end: */

/*   CloseFitsFile(unit); */

/*   if (head.first) */
/*     ReleaseBintable(&head, &table); */
  
/*   return NOT_OK; */


/* } /\* ReadOffAxisHistoFile  *\/ */


int ReadVignFile(char *filename, VignInfo_t ** info, int *ncount){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  VignCol_t          indxcol;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
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
 
  /* Move in VIGNET extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_VIGNET, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_VIGNET);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadVignFile_end;
    }

  /* Retrieve header pointer */  
  head=RetrieveFitsHeader(unit);


  if((ExistsKeyWord(&head, "THETABIN", &card))) {
    global.filesinfo.thetabin = card->u.DVal;
    global.filesinfo.thetabin = global.filesinfo.thetabin/60; /* from arcsec to arcmin */
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "THETABIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadVignFile_end;
  }


  GetBintableStructure(&head, &table, BINTAB_ROWS_SMALL, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadVignFile_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.AZIMUTH = GetColNameIndx(&table, CLNM_AZIMUTH)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_AZIMUTH);
      goto ReadVignFile_end;
    }

  if ((indxcol.ENERG_LO = GetColNameIndx(&table, CLNM_ENERG_LO)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_ENERG_LO);
      goto ReadVignFile_end;
    }
  global.filesinfo.energ_lo_dim = table.Multiplicity[indxcol.ENERG_LO];
  global.energy_dim = global.filesinfo.energ_lo_dim;

  if ((indxcol.THETA = GetColNameIndx(&table, CLNM_THETA)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_THETA);
      goto ReadVignFile_end;
    }
  global.filesinfo.theta_dim = table.Multiplicity[indxcol.THETA];


  if ((indxcol.VIGNET = GetColNameIndx(&table, CLNM_VIGNET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_VIGNET);
      goto ReadVignFile_end;
    }
  global.filesinfo.vignet_dim = table.Multiplicity[indxcol.VIGNET];


  if(global.filesinfo.vignet_dim != (global.filesinfo.energ_lo_dim*global.filesinfo.theta_dim))
    {
      headas_chat(NORMAL, "%s: Error: bad multiplicity of vignetting file columns.\n", global.taskname);
      goto ReadVignFile_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *ncount = table.MaxRows;
 *info = (VignInfo_t *)calloc(*ncount, sizeof(VignInfo_t));
 if(*info==NULL){
   headas_chat(CHATTY,"%s: Error: ReadVignFile: memory allocation failure.\n", global.taskname);
   goto ReadVignFile_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<*ncount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {

	 (*info)[count].azimuth = EVEC(table, n, indxcol.AZIMUTH);

	 (*info)[count].energ_lo = (float*)calloc(global.filesinfo.energ_lo_dim, sizeof(float));
	 EVECVEC_ARRAY_READ( (*info)[count].energ_lo, global.filesinfo.energ_lo_dim, table, n, indxcol.ENERG_LO);

	 (*info)[count].theta = (float*)calloc(global.filesinfo.theta_dim, sizeof(float));
	 EVECVEC_ARRAY_READ( (*info)[count].theta, global.filesinfo.theta_dim, table, n, indxcol.THETA);

	 (*info)[count].vignet = (float*)calloc(global.filesinfo.vignet_dim, sizeof(float));
	 EVECVEC_ARRAY_READ( (*info)[count].vignet, global.filesinfo.vignet_dim, table, n, indxcol.VIGNET);

	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS_SMALL;
   }/* while */
   
  *ncount = count;


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

  
 ReadVignFile_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadVignFile  */


void ComputeVignCorr(VignInfo_t *vigninfo, int vigncount, OffAxisHistoInfo_t *histoinfo, int histocount, double *vigncorr){

  int     i=0, j=0, k=0, idx1=0, idx2=0;
  double  dtot=0, phi_det1=0;
  double  vign_interpol=0;


  /* Compute the total duration */
  for(i=0; i<histocount; i++){
    dtot += histoinfo[i].duration ;
  }


  for(j=0; j<global.filesinfo.energ_lo_dim; j++){
    
    vigncorr[j] = 0;
    
    for(i=0; i<global.filesinfo.theta_dim; i++){
      
      for(k=0; k<global.filesinfo.phibin_dim; k++){
	

	phi_det1 = 180.0 - histoinfo[ i*global.filesinfo.phibin_dim + k ].phi + global.obsinfo.pa_pnt + 120.;
	phi_det1 = NormalaziedAt360Angle(phi_det1);
	

	for(idx1=0; idx1<vigncount; idx1++)
	  if( vigninfo[idx1].azimuth > phi_det1 )
	    break;
	
	if(idx1==vigncount)
	  {
	    idx1 = vigncount-1;
	    idx2 = 0;
	  }
	else
	  {
	    idx2 = (idx1-1)>=0 ? (idx1-1) : idx1;
	  }


	InterpolateValues(vigninfo[idx1].azimuth, vigninfo[idx2].azimuth, phi_det1, 
			  vigninfo[idx1].vignet[i*global.filesinfo.energ_lo_dim + j], vigninfo[idx2].vignet[i*global.filesinfo.energ_lo_dim + j], 
			  &vign_interpol);	

	
	vigncorr[j] += ( vign_interpol * histoinfo[ i*global.filesinfo.phibin_dim + k ].duration)/dtot ;

      }

    }

  }


/*   for(j=0; j<global.filesinfo.energ_lo_dim; j++){ */
    
/*     vigncorr[j] = 0; */
    
/*     for(i=0; i<global.filesinfo.theta_dim; i++){ */
/*       vigncorr[j] += (vigninfo[0].vignet[ i*global.filesinfo.energ_lo_dim + j ] * histoinfo[i].duration)/dtot ; */
/*     } */

/*   } */

  
} /* ComputeVignCorr  */


void ComputePsfCorr(OffAxisBinInfo_t *bininfo, double *psf_frac_off, int binnum, double *psfcorr){
  
  double corr = 0.0, duration_tot=0.0;
  int    i;
  
  for(i=0; i<binnum; i++){
    
    if(bininfo[i].duration<=0)
      continue;
    
    duration_tot += bininfo[i].duration;
  }

  for(i=0; i<binnum; i++){
    
    if(bininfo[i].duration<=0)
      continue;
    
    corr += ( bininfo[i].duration * psf_frac_off[i] ) / duration_tot;
  }

  *psfcorr = corr;
  
} /* ComputePsfCorr */


int CreateARFFile(char *inarf, char *outarf, double *vigncorr, PsfCorrVal_t *psfcorr, ApStopCorrVal_t *apstopcorr, GhostRaysCorrVal_t *grcorr, double *detabscorr){

  int            status = OK, inExt, outExt, specExt;
  FitsFileUnit_t inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */


  /* Open readonly input ARF file */
  if ((inunit=OpenReadFitsFile(inarf)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, inarf);
      goto CreateARFFile_end;
    }

  /* Move in SPECRESP extension in input file */
  if (fits_movnam_hdu(inunit, ANY_HDU, KWVL_EXTNAME_SPECRESP, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_SPECRESP);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, inarf);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, inarf);
	}
      goto CreateARFFile_end;
    }

  
  /* Get SPECRESP ext number */
  if (!fits_get_hdu_num(inunit, &specExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_SPECRESP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, inarf);
      goto CreateARFFile_end;
    }


  /* Create output file */
  if ((outunit = OpenWriteFitsFile(outarf)) <= (FitsFileUnit_t )0)
    {
      headas_chat(CHATTY, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outarf);
      goto CreateARFFile_end;
    }
  
  /* Get number of hdus in input file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, inarf);
      goto CreateARFFile_end;
    }
  
  /* Copy all extension before SPECRESP extension from input to output file */
  outExt=1;
    
  while(outExt<specExt && status == OK)
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, inarf);
	  goto CreateARFFile_end;
	}
      if(fits_copy_hdu( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, inarf);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, outarf);
	  goto CreateARFFile_end;
	}
      outExt++;
    }
  
  /* make sure get specified header by using absolute location */
  if(fits_movabs_hdu( inunit, specExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,specExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, inarf);
      goto CreateARFFile_end;
    }

  /* Correct ARF */
  if (CorrectArf(inunit, outunit, vigncorr, psfcorr, apstopcorr, grcorr, detabscorr))
    {
      headas_chat(NORMAL, "%s: Error: Unable to apply correction to output ARF file.\n", global.taskname);
      goto CreateARFFile_end;
    }


   outExt++;
  /* copy any extension after the extension to be operated on */
  while ( status == OK && outExt <= inExt)
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, inarf);
	  goto CreateARFFile_end;
	}
      if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	{
	  headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, inarf);
	  headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, outarf);
	  goto CreateARFFile_end;
	}
      
      outExt++;
    }


  /* Add history if parameter history set */
  if(HDpar_stamp(outunit, specExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto CreateARFFile_end;
    }

  
  /* Update checksum and datasum keywords in all extensions */
  if (ChecksumCalc(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, outarf);
      goto CreateARFFile_end;
    }
  
  /* close input and output files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, inarf);
      goto CreateARFFile_end;
    }
  if (CloseFitsFile(outunit))
    {
      headas_chat(CHATTY, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n ", global.taskname, outarf);
      goto CreateARFFile_end;
    }

  headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, outarf);


  return OK;

 CreateARFFile_end:

  return NOT_OK;

} /* CreateARFFile */


int CorrectArf(FitsFileUnit_t inunit, FitsFileUnit_t outunit, double *vigncorr, PsfCorrVal_t *psfcorr, ApStopCorrVal_t *apstopcorr, GhostRaysCorrVal_t *grcorr, double *detabscorr){

  unsigned           FromRow, ReadRows, n, nCols, OutRows=0;
  int                count=0;
  double             dspecresp, energy_arf, vign, psf, apstop, gr, detabs;
  ARFCol_t           indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );
  
  head=RetrieveFitsHeader(inunit);


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: file is empty.\n", global.taskname);
      goto CorrectArf_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.ENERG_LO = GetColNameIndx(&table, CLNM_ENERG_LO)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_ENERG_LO);
      goto CorrectArf_end;
    }

  if ((indxcol.ENERG_HI = GetColNameIndx(&table, CLNM_ENERG_HI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_ENERG_HI);
      goto CorrectArf_end;
    }

  if ((indxcol.SPECRESP = GetColNameIndx(&table, CLNM_SPECRESP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_SPECRESP);
      goto CorrectArf_end;
    }


  /* Add history */
  GetGMTDateTime(global.date);
  if(global.hist)
    {
      sprintf(global.strhist, "File modified by '%s' (%s) at %s", global.taskname, global.nustardas_v,global.date );
      AddHistory(&head, global.strhist);
    }


  EndBintableHeader(&head, &table);
  
  /* write out new header to new file */
  FinishBintableHeader(outunit, &head, &table);


  FromRow = 1;
  ReadRows=table.nBlockRows;
  OutRows = 0;
  nCols=table.nColumns;

  /* Read input bintable */
  while( (count<global.filesinfo.energ_lo_dim) && (ReadBintable(inunit, &table, nCols, NULL,FromRow,&ReadRows) == 0) )
    {
      for(n=0; n<ReadRows; ++n)
	{

	  dspecresp = EVEC(table, n, indxcol.SPECRESP);
	  
	  if( global.par.psfflag && !global.par.extended )
	    {
	      energy_arf = ((double)EVEC(table, n, indxcol.ENERG_LO) + (double)EVEC(table, n, indxcol.ENERG_HI))/2.;
	      GetPsfCorr(psfcorr, energy_arf, &psf);
	    }
	  else if(global.par.extended)
	    psf = psfcorr->corr[0];
	  else
	    psf = 1;
	  
	  if(global.par.vignflag)
	    vign = vigncorr[count];
	  else
	    vign = 1;

	  /* Aperture Stop correction */
	  if(global.par.apstopflag)
	    {
	      energy_arf = ((double)EVEC(table, n, indxcol.ENERG_LO) + (double)EVEC(table, n, indxcol.ENERG_HI))/2.;
	      GetApStopCorr(apstopcorr, energy_arf, &apstop);
	    }
	  else
	    apstop = 1;

	  /* Ghost Rays correction */
	  if(global.par.grflag)
	    {
	      energy_arf = ((double)EVEC(table, n, indxcol.ENERG_LO) + (double)EVEC(table, n, indxcol.ENERG_HI))/2.;
	      GetGhostRaysCorr(grcorr, energy_arf, &gr);
	    }
	  else
	    gr = 1;

	  /* DETABS correction */
	  if(global.par.detabsflag)
	    {
	      detabs = detabscorr[count];
	    }
	  else
	    detabs = 1;


	  dspecresp = dspecresp * psf * vign * apstop * gr * detabs;

	  EVEC(table, n, indxcol.SPECRESP) = (float)dspecresp;



	  count++;

	  if(++OutRows>=BINTAB_ROWS)
	    {
	      WriteFastBintable(outunit, &table, OutRows, FALSE);
	      OutRows=0;
	    }
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;

   }/* while */
   
  
  WriteFastBintable(outunit, &table, OutRows, TRUE);
  ReleaseBintable(&head, &table);

  return OK;

  
 CorrectArf_end:

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* CorrectArf */


/*
NOTE: index=0 =>  offset+(imin*binsize-binsize/2) < value < offset+(imin*binsize+binsize/2)
 */
int GetCenterBinIndex(double binsize, double offset, int imin, int imax, double value){
  
  int i;
  value -= offset;

  i = (value<imin*binsize) ? imin : ( value>=imax*binsize ? imax : ((value-imin*binsize)/binsize +imin) );
  
  return i;

} /* GetCenterBinIndex */


/*
NOTE: index=0 =>  offset+(imin*binsize) < value < offset+(imin*binsize+binsize)
 */
int GetInitBinIndex(double binsize, double offset, int imin, int imax, double value){
  
  int i;
  value -= offset;

  i = (value<imin*binsize-binsize/2) ? imin : ( value>=imax*binsize+binsize/2 ? imax : ((value-imin*binsize+binsize/2)/binsize +imin) );
  
  return i;

} /* GetInitBinIndex */


int ComputeOffAxisBin(struct gti_struct *gti, char *infile, OffAxisBinInfo_t *bininfo, int binnum, double binsize){

  unsigned           FromRow, ReadRows, n, nCols;
  int                i, j, status=OK, init=-1, index=0, new_index;
  int                bin_imin, bin_imax;
  double             tstart=0, tstop=0, dtime=0, off_axis=0, new_off_axis, phi=0., *dval;
  double             laststart=0, laststop=0;  
  OffAxisCol_t       indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  bin_imin = 0;
  bin_imax = binnum-1;

  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, infile);
      return NOT_OK;
    }
 

  /* Move in OFF_AXIS extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_OFF_AXIS, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OFF_AXIS);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile); 
      goto ComputeOffAxisBin_end;
    }
  
  head=RetrieveFitsHeader(unit);


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, infile);
      goto ComputeOffAxisBin_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto ComputeOffAxisBin_end;
    }

  if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS);
      goto ComputeOffAxisBin_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, CLNM_PHI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_PHI);
      goto ComputeOffAxisBin_end;
    }


 EndBintableHeader(&head, &table);


 /* init bininfo data */
 for(i=0; i<binnum; i++){
   bininfo[i].off_axis = binsize*(i+bin_imin);
   bininfo[i].duration = 0;
   bininfo[i].phi_median = 0;
   bininfo[i].gticount = 0;
   bininfo[i].gti = NULL;
 }


 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
   {
     for(n=0; n<ReadRows ; n++)
       {
	 
	 if(n==ReadRows-2)
	   laststart = DVEC(table, n, indxcol.TIME);

	 if(n==ReadRows-1)
	   laststop = DVEC(table, n, indxcol.TIME);	 


	 if(init<0){
	   /* Init 'tstart' 'off_axis' 'phi' and 'index' values */
	   tstart = DVEC(table, n, indxcol.TIME);
	   off_axis = DVEC(table, n, indxcol.OFF_AXIS);
	   phi = DVEC(table, n, indxcol.PHI);
	   index = GetInitBinIndex(binsize, 0, bin_imin, bin_imax, off_axis);
	   init = 1;
	 }
	 else{
	   tstop = DVEC(table, n, indxcol.TIME);
	   new_off_axis = DVEC(table, n, indxcol.OFF_AXIS);	   
	   new_index = GetInitBinIndex(binsize, 0, bin_imin, bin_imax, new_off_axis);
	   /* headas_chat(NORMAL,"DEBUG new_index=%d new_off_axis=%f\n",new_index,new_off_axis); */

	   /* New bin */
	   if(new_index!=index){
	    
	     /* Compute overlap exposure of the time bin with GTI */
	     dtime = HDgti_exp(tstart, tstop, gti, &status);
	     if(status!=OK){
	       headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
	       goto ComputeOffAxisBin_end;
	     }
 
	     if(dtime>0){
	       bininfo[index].duration += dtime;
	       bininfo[index].gticount = bininfo[index].gticount + 1 ;

	       /* headas_chat(NORMAL,"DEBUG index=%d gticount=%d\n",index,bininfo[index].gticount); */

	       bininfo[index].gti = (GTIBinInfo_t*) realloc( bininfo[index].gti, (bininfo[index].gticount * sizeof(GTIBinInfo_t)) );
	       if(bininfo[index].gti==NULL){
		 headas_chat(CHATTY,"%s: Error: ComputeOffAxisBin: memory allocation failure.\n", global.taskname);
		 goto ComputeOffAxisBin_end;
	       }

	       bininfo[index].gti[bininfo[index].gticount-1].tstart = tstart;
	       bininfo[index].gti[bininfo[index].gticount-1].tstop = tstop;
	       bininfo[index].gti[bininfo[index].gticount-1].phi = phi;

	       /* headas_chat(NORMAL,"DEBUG BININDEX=%d TSTART=%f TSTOP=%f DTIME=%f\n",index,tstart,tstop,dtime); */
	     }

	     /* Update 'tstart' 'off_axis' 'phi' and 'index' values */
	     tstart = DVEC(table, n, indxcol.TIME);
	     off_axis = DVEC(table, n, indxcol.OFF_AXIS);
	     phi = DVEC(table, n, indxcol.PHI);
	     index = new_index;
	   }

	 }

       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 


 /* Update duration value of the last row */	   
 tstop += (laststop-laststart);

 /* Compute overlap exposure of the time bin with GTI */
 dtime = HDgti_exp(tstart, tstop, gti, &status);
 if(status!=OK){
   headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
   goto ComputeOffAxisBin_end;
 }
 
/*  headas_chat(NORMAL,"DEBUG ultimo step index=%d tstart=%f tstop=%f dtime=%f\n", index,tstart,tstop,dtime ); */

 if(dtime>0){
   bininfo[index].duration += dtime;
   bininfo[index].gticount = bininfo[index].gticount + 1 ;
   
   bininfo[index].gti = (GTIBinInfo_t*) realloc( bininfo[index].gti, (bininfo[index].gticount * sizeof(GTIBinInfo_t)) );
   if(bininfo[index].gti==NULL){
     headas_chat(CHATTY,"%s: Error: ComputeOffAxisBin: memory allocation failure.\n", global.taskname);
     goto ComputeOffAxisBin_end;
   }
   
   bininfo[index].gti[bininfo[index].gticount-1].tstart = tstart;
   bininfo[index].gti[bininfo[index].gticount-1].tstop = tstop;
   bininfo[index].gti[bininfo[index].gticount-1].phi = phi;
  /*  headas_chat(NORMAL,"DEBUG last BININDEX=%d TSTART=%f TSTOP=%f DTIME=%f\n",index,tstart,tstop,dtime); */

 }


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile);
      return NOT_OK;
    }


  /* Compute phi median */
  for(i=0; i<binnum; i++){

    dval = (double *)malloc(bininfo[i].gticount*sizeof(double));

    for(j=0; j<bininfo[i].gticount; j++)
      dval[j] = bininfo[i].gti[j].phi;

    bininfo[i].phi_median = ComputeDoubleMedian(dval, bininfo[i].gticount);

    free(dval);
  }


  for(i=0; i<binnum; i++)
    {
      headas_chat(CHATTY,"BIN=%d OFF_AXIS=%f DURATION=%f PHI_BIN=%f\n", i, bininfo[i].off_axis, bininfo[i].duration, bininfo[i].phi_median);

      for(j=0; j<bininfo[i].gticount; j++)
	headas_chat(CHATTY,"TSTART=%f TSTOP=%f\n", bininfo[i].gti[j].tstart, bininfo[i].gti[j].tstop);
    }


  return OK;

  
 ComputeOffAxisBin_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

} /* ComputeOffAxisBin */


int WriteBinGTI(char *filename, GTIBinInfo_t * gtiinfo, int gticount){

  int                status=OK, i;
  fitsfile           *fptr = NULL;
  struct gti_struct  gti;

  /* Initialize GTI */
  HDgti_init(&gti);

  gti.mjdref = global.obsinfo.mjdref;
  gti.timezero = 0;

  /* Start filling in output GTI structure */
  gti.start = (double *)malloc(2*sizeof(double)*gticount);
  if (gti.start == 0) goto WriteBinGTI_end;
  gti.stop = gti.start + gticount;
  gti.dptr = (void *) gti.start;
  
  gti.ngti = gticount;
  gti.maxgti = gticount;

  /* Write gti values in gti struct */
  for(i=0; i<gticount; i++){
    gti.start[i] = gtiinfo[i].tstart;
    gti.stop[i] = gtiinfo[i].tstop;
  }
  
  /* Create output file */
  if (fits_create_file(&fptr, filename, &status)) {
    headas_chat(NORMAL, "%s: Error: Unable to create '%s' file.\n", global.taskname, filename);
    goto WriteBinGTI_end;
  }

  /* Write GTI ext */
  HDgti_write(fptr, &gti, KWVL_EXTNAME_GTI, "START", "STOP", &status);
  if (status) {
    headas_chat(NORMAL, "%s: Error: Unable to write %s extension to '%s' file.\n", global.taskname, KWVL_EXTNAME_GTI, filename);
    goto WriteBinGTI_end;
  }

  /* close output file */
  if(fits_close_file(fptr, &status)){
    headas_chat(NORMAL, "%s: Error: Unable to close '%s' file.\n", global.taskname, filename);
    goto WriteBinGTI_end;
  }

  return OK;
  
 WriteBinGTI_end:
  return NOT_OK;

} /* WriteBinGTI */


/* Only for extended="no" */
int ComputePsfFracOff(char *expofile, char *psffile, int bin, double phi, RegionExtInfo_t *reginfo, double *psf_frac_off){

  int           xx, yy, ii, jj, cx, cy, extnum, status=OK;
  double        sumrcts=1.0, expoval, dist, xx1, yy1, angle, a, b, psf_frac=0.0;
  char          cmd[BUF_SIZE];
  static float  psf[PSF_ROWS][PSF_PIXS];
  float         *expo=NULL;
  static double frac[PSF_ROWS][PSF_PIXS];
  static float  fractmp[PSF_ROWS][PSF_PIXS];
  ExposureInfo_t expoinfo;
  pid_t          pid;
  
  /* Get pid */
  pid=getpid();

  cx = (int)(reginfo->X[0]+0.5) -1;
  cy = (int)(reginfo->Y[0]+0.5) -1;

  /* Create a temporary psf file with rotation */

  sprintf(global.tmpout.combinexform_in, "%dtmp_trasform%d.txt", (int)pid, bin);
  sprintf(global.tmpout.combinexform_out, "%dtmp_trasform%d.xform", (int)pid, bin);
  sprintf(global.tmpout.rotpsffile, "%dtmp_psf_%d.fits", (int)pid, bin);

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

  /* Skip the primary HDU */
  /* NOTE: fits extension number start from 1 while 'bin' start from 0 */
  extnum = bin+1;

  sprintf(cmd, "imagetrans infile=%s[%d] outfile=%s transform=%s inverse=none method=AREA dimenx=325 dimeny=325 seed=0 bitpix=-32 zeronull=no history=yes", 
	  psffile, extnum, global.tmpout.rotpsffile, global.tmpout.combinexform_out );
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: Unable to create '%s' file.\n", global.taskname, global.tmpout.rotpsffile);
    goto ComputePsfFracOff_end;
  }


  if(ReadPSFFile(global.tmpout.rotpsffile, 1, psf, &sumrcts)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file for bin %d\n", psffile, bin);
    goto ComputePsfFracOff_end;
  }
  
  for(yy=0; yy<PSF_ROWS; yy++){
    for(xx=0; xx<PSF_PIXS; xx++){
      frac[yy][xx] = (double)psf[yy][xx] / sumrcts ;
    }
  }


  if(ReadExpoFile(expofile, &expo, &expoinfo)){
    goto ComputePsfFracOff_end;
  }


  for(yy=0; yy<PSF_ROWS; yy++){
    for(xx=0; xx<PSF_PIXS; xx++){

      fractmp[yy][xx] = 0;

      ii = cy - expoinfo.crval2p + expoinfo.crpix2p - (int)PSF_PIXS/2 + yy ;
      jj = cx - expoinfo.crval1p + expoinfo.crpix1p - (int)PSF_ROWS/2 + xx  ;

      if(ii>=0 && ii<expoinfo.ywidth && jj>=0 && jj<expoinfo.xwidth )
	expoval = (double)expo[ii*expoinfo.xwidth + jj] / (expoinfo.ontime*expoinfo.deadc) ;
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


/*   if(WritePsfFracTmpFile(fractmp,bin)) */
/*     goto ComputePsfFracOff_end; */

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


int ReadExpoFile(char *filename, float **expomap, ExposureInfo_t *info){

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
      return NOT_OK;
    }
  
  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);


  /* Check VIGNAPP value */
  if( (ExistsKeyWord(&head, KWNM_VIGNAPP, NULLNULLCARD)) && (GetLVal(&head, KWNM_VIGNAPP)) )
    {
      headas_chat(NORMAL, "%s: Error: Input Exposure Map is corrected for vignetting.\n", global.taskname);
      goto ReadExpoFile_end;
    }

  /* Get ONTIME value */
  if(ExistsKeyWord(&head, KWNM_ONTIME, &card))
    {
      info->ontime = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_ONTIME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get LIVETIME value */
  if(ExistsKeyWord(&head, KWNM_LIVETIME, &card))
    {
      info->livetime = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_LIVETIME);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

  /* Get DEADC value */
  if(ExistsKeyWord(&head, KWNM_DEADC, &card))
    {
      info->deadc = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DEADC);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto ReadExpoFile_end;
    }

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
      return NOT_OK;
    }


  return OK;

 ReadExpoFile_end:

  CloseFitsFile(inunit);

  return NOT_OK;


} /* ReadExpoFile */


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


int ComputeExpoCorr(char *expofile, float *phaimg, PhaImgInfo_t *phaimginfo, RegBox_t *box){

  int           xx, yy, count=0, n_pix, pha_xx, pha_yy;
  int           xmin, xmax, ymin, ymax;
  int           pha_xdim, pha_ydim, pha_xoffset, pha_yoffset;
  double        sum_pix;
  float         *expo=NULL;
  ExposureInfo_t expoinfo;

  pha_xdim = phaimginfo->x_width;
  pha_ydim = phaimginfo->y_width;
  pha_xoffset = phaimginfo->x_offset;
  pha_yoffset = phaimginfo->y_offset;

  if(ReadExpoFile(expofile, &expo, &expoinfo)){
    goto ComputeExpoCorr_end;
  }

  for(count=0; count<box->nbox; count++){

    sum_pix = 0;
    n_pix = 0;
    
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

	      sum_pix += (double)expo[yy*expoinfo.xwidth + xx] / expoinfo.livetime ;
	      n_pix++;
	    }

	  }
	}

      }
    }

    box->boxinfo[count].expocorr = n_pix>0 ? sum_pix/n_pix : 0;
    headas_chat(CHATTY, "SubImages Box %d expocorr=%f sum_pix=%f n_pix=%d\n", count, box->boxinfo[count].expocorr, sum_pix, n_pix);
  }


  return OK;

 ComputeExpoCorr_end:
  return NOT_OK;

} /* ComputeExpoCorr */


int ComputeBoxWeight(char *phafile, RegionExtInfo_t *reginfo, VignInfo_t *vigninfo, RegBox_t *box){

  int            xx, yy, count=0, en_cnt=0;
  int            xdim, ydim, xoffset, yoffset;
  int            xmin, xmax, ymin, ymax;
  double         *ratebox, *countsbox;
  double         ratetot=0.;
  float          *pha=NULL;
  PhaImgInfo_t   phainfo;

  
  ratebox = (double*)calloc(box->nbox, sizeof(double));
  countsbox = (double*)calloc(box->nbox, sizeof(double));

  for(count=0; count<box->nbox; count++){
    countsbox[count] = 0.;
    ratebox[count] = 0.;
  }


  if(ReadPhaImg(phafile, &pha, &phainfo)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file\n", phafile);
    goto ComputeBoxWeight_end;
  }

  xdim = phainfo.x_width;
  ydim = phainfo.y_width;
  xoffset = phainfo.x_offset;
  yoffset = phainfo.y_offset;

  for(count=0; count<(box->nbox); count++){

    /* Box range values in pha image coordinates */
    xmin = box->boxinfo[count].x_min - xoffset;
    xmin = xmin>0 ? (xmin<xdim ? xmin : xdim-1 ) : 0;
    
    xmax = box->boxinfo[count].x_max - xoffset;
    xmax = xmax>0 ? (xmax<xdim ? xmax : xdim-1 ) : 0;
    
    ymin = box->boxinfo[count].y_min - yoffset;
    ymin = ymin>0 ? (ymin<ydim ? ymin : ydim-1 ) : 0;

    ymax = box->boxinfo[count].y_max - yoffset;
    ymax = ymax>0 ? (ymax<ydim ? ymax : ydim-1 ) : 0;
    
    headas_chat(CHATTY, "%s: Info: Box=%d xmin=%d xmax=%d ymin=%d ymax=%d (PHA file)\n",global.taskname,count,xmin,xmax,ymin,ymax);


    box->boxinfo[count].weight = 0.0;
    
    for(yy=ymin; yy<=ymax; yy++){
      for(xx=xmin; xx<=xmax; xx++){
	
	if(pha[yy*xdim+xx]>0.)
	  countsbox[count] += (double)pha[yy*xdim+xx];
	  
      }
    }

  }

  /* Search energy index */
  for(en_cnt=0; en_cnt<global.filesinfo.energ_lo_dim; en_cnt++)
    {
      if(vigninfo[0].energ_lo[en_cnt]>10.0)
	break;
    }

  /* Compute 'ratebox[]' and 'ratetot' values */
  for(count=0; count<box->nbox; count++){

    ratebox[count] = countsbox[count] / ( box->boxinfo[count].vigncorr[en_cnt] * box->boxinfo[count].expocorr * box->boxinfo[count].detabscorr[en_cnt] );
    if(isnan(ratebox[count]))
      ratebox[count] = 0.0;

    ratetot += ratebox[count];
  }

  /* Compute weight value */
  for(count=0; count<box->nbox; count++){

    box->boxinfo[count].weight = ratebox[count]/ratetot ;
    if(isnan(box->boxinfo[count].weight))
      box->boxinfo[count].weight = 0.0;

    headas_chat(CHATTY,"%s: Info: Box=%d weight=%f countsbox=%f ratebox=%f ratetot=%f\n",
		global.taskname, count, box->boxinfo[count].weight, countsbox[count], ratebox[count], ratetot );
  }

  
  free(ratebox);
  free(countsbox);

  return OK;

 ComputeBoxWeight_end:
  return NOT_OK;

} /* ComputeBoxWeight */


int SetFlatBoxWeight(char *phafile, RegBox_t *box){

  int            xx, yy, count=0, nbox=0;
  int            xdim, ydim, xoffset, yoffset;
  int            xmin, xmax, ymin, ymax;
  float          *pha=NULL;
  PhaImgInfo_t   phainfo;


  if(ReadPhaImg(phafile, &pha, &phainfo)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file\n", phafile);
    goto SetFlatBoxWeight_end;
  }

  xdim = phainfo.x_width;
  ydim = phainfo.y_width;
  xoffset = phainfo.x_offset;
  yoffset = phainfo.y_offset;

  for(count=0; count<(box->nbox); count++){

    /* Box range values in pha image coordinates */
    xmin = box->boxinfo[count].x_min - xoffset;
    xmin = xmin>0 ? (xmin<xdim ? xmin : xdim-1 ) : 0;
    
    xmax = box->boxinfo[count].x_max - xoffset;
    xmax = xmax>0 ? (xmax<xdim ? xmax : xdim-1 ) : 0;
    
    ymin = box->boxinfo[count].y_min - yoffset;
    ymin = ymin>0 ? (ymin<ydim ? ymin : ydim-1 ) : 0;

    ymax = box->boxinfo[count].y_max - yoffset;
    ymax = ymax>0 ? (ymax<ydim ? ymax : ydim-1 ) : 0;
    
    headas_chat(CHATTY, "%s: Info: Box=%d xmin=%d xmax=%d ymin=%d ymax=%d (PHA file)\n",global.taskname,count,xmin,xmax,ymin,ymax);


    box->boxinfo[count].weight = 0.0;
    
    for(yy=ymin; yy<=ymax; yy++){
      for(xx=xmin; xx<=xmax; xx++){
	
	if(pha[yy*xdim+xx]>0.)
	  box->boxinfo[count].weight = 1.0;
	  
      }
    }

    if(box->boxinfo[count].weight>0.0)
      nbox++;

  }


  for(count=0; count<box->nbox; count++){
    if(box->boxinfo[count].weight>0.0)
      box->boxinfo[count].weight = nbox>0 ? 1.0/nbox : 0.0;
  }


  return OK;

 SetFlatBoxWeight_end:
  return NOT_OK;


} /* SetFlatBoxWeight */


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


int WriteArfListFile(RegBox_t *box, char *outfile){

  int      i;
  FILE	   *file;
  
  if (!(file = fopen(outfile, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,outfile);
    goto WriteArfListFile_end;
  }
  
  for(i=0; i<box->nbox; i++){

    if( box->boxinfo[i].weight>0 )
      fprintf(file, "%s %f\n",box->boxinfo[i].arffile, box->boxinfo[i].weight);
  }   
  
  fclose(file);
  
  return OK;
  
 WriteArfListFile_end:
    return NOT_OK;

}  /* WriteArfListFile */


int UpdateArfKeys(ObsInfo_t *obsinfo, char *filename){

  fitsfile       *fptr=NULL;
  int            status = OK, bitpix=16;
  FitsCard_t     *card;
  FitsHeader_t   head;

  /* Open input file */  
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  
  /* Primary HDU */
  if(fits_movabs_hdu(fptr, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }


  if(fits_update_key(fptr, TSTRING, KWNM_TELESCOP, obsinfo->telescop, obsinfo->telescop_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TELESCOP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
      
  if(fits_update_key(fptr, TSTRING, KWNM_INSTRUME, obsinfo->instrume, obsinfo->instrume_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }

  if(fits_update_key(fptr, TINT, "BITPIX", &bitpix, "number of bits per data pixel", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, "BITPIX");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }

  /* 'SPECRESP' HDU */
  if(fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_SPECRESP,0,&status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to find  '%s' extension\n", global.taskname, KWVL_EXTNAME_SPECRESP);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }

  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(fptr);
  
  if(ExistsKeyWord(&head, "ARFVERSN", &card)){
    if(fits_delete_key(fptr, "ARFVERSN", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "ARFVERSN");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "DETNAM", &card)){
    if(fits_delete_key(fptr, "DETNAM", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "DETNAM");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "FILTER", &card)){
    if(fits_delete_key(fptr, "FILTER", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "FILTER");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "HDUDOC", &card)){
    if(fits_delete_key(fptr, "HDUDOC", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "HDUDOC");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "PHAFILE", &card)){
    if(fits_delete_key(fptr, "PHAFILE", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PHAFILE");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "ORIGIN", &card)){
    if(fits_delete_key(fptr, "ORIGIN", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "ORIGIN");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "CCLS0001", &card)){
    if(fits_delete_key(fptr, "CCLS0001", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "CCLS0001");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "CDTP0001", &card)){
    if(fits_delete_key(fptr, "CDTP0001", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "CDTP0001");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "CCNM0001", &card)){
    if(fits_delete_key(fptr, "CCNM0001", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "CCNM0001");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "CVSD0001", &card)){
    if(fits_delete_key(fptr, "CVSD0001", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "CVSD0001");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "CVST0001", &card)){
    if(fits_delete_key(fptr, "CVST0001", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "CVST0001");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  if(ExistsKeyWord(&head, "CDES0001", &card)){
    if(fits_delete_key(fptr, "CDES0001", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "CDES0001");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateArfKeys_end;
    }
  }

  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdateArfKeys_end;
    }


   /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdateArfKeys_end;
    }


  return OK;

 UpdateArfKeys_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);

  return NOT_OK;

} /* UpdateArfKeys */


int GetPsfBinInfo(char *filename, int *psfbin_dim, double *psfbin){

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

  *psfbin_dim = inExt-1;

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
      *psfbin = card->u.DVal;
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


int CreateTemporaryEvt(char *infile, char *gtifile, char *gtiextname, char *outfile){

  int                status=OK, inExt, outExt, gtiExt;
  FitsFileUnit_t     inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */ 
  struct gti_struct  gti;


  /* Read GTI info from GTI extension */
  if(HDgti_read(gtifile, &gti, gtiextname, 0, 0, 0, 0, &status)){
    headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' file.\n", global.taskname, gtifile);
    goto CreateTemporaryEvt_end;
  }


  /* Open readonly input event file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, infile);
      goto CreateTemporaryEvt_end;
    }
  
  /* Move in GTI extension in input file */
  if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_GTI, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_GTI);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile);
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, infile);
	}
      goto CreateTemporaryEvt_end;
    }
  
  /* Get GTI ext number */
  if (!fits_get_hdu_num(inunit, &gtiExt))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_GTI);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, infile);      
      goto CreateTemporaryEvt_end;
    }


  /* Create output file */
  if ((outunit = OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL, "%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' temporary file.\n", global.taskname, outfile);
      goto CreateTemporaryEvt_end;
    }
  
  /* Get number of hdus in input events file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, infile);
      goto CreateTemporaryEvt_end;
    }
  
  /* Copy all extension before GTI extension  from input to output file */
  outExt=1;
  
  while(outExt<gtiExt && status == OK)
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, infile);
	  goto CreateTemporaryEvt_end;
	}
      if(fits_copy_hdu( inunit, outunit, 0, &status ))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(NORMAL,"%s: Error: from %s input file\n", global.taskname, infile);
	  headas_chat(NORMAL,"%s: Error: to %s temporary output file.\n",global.taskname, outfile);
	  goto CreateTemporaryEvt_end;
	}      
      outExt++;
    }
  
  /* make sure get specified extension by using absolute location */
  if(fits_movabs_hdu(inunit, gtiExt, NULL, &status ))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,gtiExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto CreateTemporaryEvt_end;
    }

  /* Write GTI ext */
  HDgti_write(outunit, &gti, KWVL_EXTNAME_GTI, "START", "STOP", &status);
  if (status) {
    headas_chat(NORMAL, "%s: Error: Unable to write %s extension to '%s' file.\n", global.taskname, KWVL_EXTNAME_GTI, outfile);
    goto CreateTemporaryEvt_end;
  }
  outExt++;


  /* copy any extension after the extension to be operated on */
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, infile);
	  goto CreateTemporaryEvt_end;
	}
      if(fits_copy_hdu ( inunit, outunit, 0, &status ))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
	  headas_chat(NORMAL,"%s: Error: from %s input file\n", global.taskname, infile);
	  headas_chat(NORMAL,"%s: Error: to %s temporary output file.\n",global.taskname, outfile);
	  goto CreateTemporaryEvt_end;
	}
      
      outExt++;
    }
  
  /* Update checksum and datasum keywords in all extensions */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto CreateTemporaryEvt_end;
    }
  
  /* close input and output files */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, infile);
      goto CreateTemporaryEvt_end;
    }  
  if (CloseFitsFile(outunit)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto CreateTemporaryEvt_end;
    }


  return OK;
  
 CreateTemporaryEvt_end:

  return NOT_OK;

} /* CreateTemporaryEvt */


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


double NormalaziedAt360Angle(double angle){

  double outangle;
  
  outangle = fmod(angle,360.0);
  outangle = outangle>=0 ? outangle : outangle+360;

  return outangle;

} /* NormalaziedAt360Angle */


void InterpolateValues(double down, double up, double this, double valuedown, double valueup, double *value){

  double delta, frac1, frac2;

  delta = up - down;

  if(delta<DOUBLE_SENS && delta>-DOUBLE_SENS)
    {
      *value = valuedown;
    }
  else
    {
      frac1 = (up - this) / delta;
      frac2 = (this - down) /delta;

      *value = valuedown * frac1 + valueup * frac2;
    }

}/* InterpolateValues */


int WritePsfFracTmpFile(float fractmp[PSF_ROWS][PSF_PIXS], int bin){
  
  int                 status=OK;
  int                 bitpix=0, naxis=0;
  long                naxes[2], group, firstelem, nelements;
  FitsFileUnit_t      inunit=NULL;
  char                filename[PIL_LINESIZE];
  pid_t               pid;
  
  /* Get pid */
  pid=getpid();

  sprintf(filename, "%dtmp_bin%dpsffrac.fits", (int)pid, bin);


  /* Create output primary array */
  if(fits_create_file(&inunit, filename, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto WritePsfFracTmpFile_end;
    }

  naxis=2;
  naxes[0]=PSF_ROWS;
  naxes[1]=PSF_PIXS;
  bitpix=-32;

  if(fits_create_img(inunit, bitpix, naxis, naxes, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto WritePsfFracTmpFile_end;
    }
  
  group=0;
  firstelem=1;
  nelements=PSF_ROWS*PSF_PIXS;
  
  if(fits_write_img_flt(inunit, group, firstelem, nelements, fractmp[0], &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write image extension\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto WritePsfFracTmpFile_end;
    }
  

  /* Calculate checksum and add it in file */
  if (ChecksumCalc(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto WritePsfFracTmpFile_end;
    }
  

  if(fits_close_file(inunit, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto WritePsfFracTmpFile_end;
    }
  

  return OK;
  
 WritePsfFracTmpFile_end:
  return NOT_OK;
  
} /* WritePsfFracTmpFile */


int ComputeApertureStopOffAxisHisto(struct gti_struct *gti, char *infile, ApStopCorrInfo_t *apstopinfo, ApStopOffAxisHistoData_t **histodata, int *histocount, ApStopOffAxisHistoInfo_t *histoinfo){

  unsigned           FromRow, ReadRows, n, nCols;
  int                i, j, x, y, status=OK, init=OK, offidx, phiidx, deltaxidx, deltayidx;
  double             tstart=0, tstop=0, dtime=0, off_axis=0, phi=0, deltax_cen=0., deltay_cen=0.;
  double             offmin, deltaxmin, deltaymin;
  OffAxisCol_t       indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  double             phibin, offbin, deltaxbin, deltaybin;
  int                n_offbin, n_phibin, n_deltaxbin, n_deltaybin;


  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Update apstopoffaxishisto file info */

  histoinfo->ash_phibin = global.par.apstopphibin;
  histoinfo->ash_phibin_dim = 360/histoinfo->ash_phibin;

  histoinfo->ash_offbin = apstopinfo->as_theta_bin / ASH_OFFBIN_SCALE ;
  histoinfo->ash_offbin_min = apstopinfo->as_theta_min;
  histoinfo->ash_offbin_dim = apstopinfo->as_theta_dim * ASH_OFFBIN_SCALE ;

  histoinfo->ash_deltaxbin = apstopinfo->as_deltax_cen_bin;
  histoinfo->ash_deltaxbin_min = apstopinfo->as_deltax_cen_min;
  histoinfo->ash_deltax_dim = apstopinfo->as_deltax_cen_dim;

  histoinfo->ash_deltaybin = apstopinfo->as_deltay_cen_bin;
  histoinfo->ash_deltaybin_min = apstopinfo->as_deltay_cen_min;
  histoinfo->ash_deltay_dim = apstopinfo->as_deltay_cen_dim;


  /* Set local variables */

  phibin = histoinfo->ash_phibin;
  n_phibin = histoinfo->ash_phibin_dim;

  offbin = histoinfo->ash_offbin;
  offmin = histoinfo->ash_offbin_min;
  n_offbin = histoinfo->ash_offbin_dim;

  deltaxbin = histoinfo->ash_deltaxbin;
  deltaxmin = histoinfo->ash_deltaxbin_min;
  n_deltaxbin = histoinfo->ash_deltax_dim;

  deltaybin = histoinfo->ash_deltaybin;
  deltaymin = histoinfo->ash_deltaybin_min;
  n_deltaybin = histoinfo->ash_deltay_dim;


  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, infile);
      return NOT_OK;
    }
 
  /* Move in OFF_AXIS extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_OFF_AXIS, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OFF_AXIS);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile); 
      goto ComputeApertureStopOffAxisHisto_end;
    }
  
  head=RetrieveFitsHeader(unit);


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, infile);
      goto ComputeApertureStopOffAxisHisto_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto ComputeApertureStopOffAxisHisto_end;
    }

  if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS);
      goto ComputeApertureStopOffAxisHisto_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, CLNM_PHI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_PHI);
      goto ComputeApertureStopOffAxisHisto_end;
    }

  if ((indxcol.DELTAX_CEN = GetColNameIndx(&table, CLNM_DELTAX_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAX_CEN);
      goto ComputeApertureStopOffAxisHisto_end;
    }

  if ((indxcol.DELTAY_CEN = GetColNameIndx(&table, CLNM_DELTAY_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAY_CEN);
      goto ComputeApertureStopOffAxisHisto_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *histocount = n_offbin*n_phibin*n_deltaxbin*n_deltaybin;
 *histodata = (ApStopOffAxisHistoData_t *)calloc(*histocount, sizeof(ApStopOffAxisHistoData_t));
 if(*histodata==NULL){
   headas_chat(CHATTY,"%s: Error: ComputeApertureStopOffAxisHisto: memory allocation failure.\n", global.taskname);
   goto ComputeApertureStopOffAxisHisto_end;
 }

 for(i=0; i<n_offbin; i++){
   for(j=0; j<n_phibin; j++){
     for(x=0; x<n_deltaxbin; x++){
       for(y=0; y<n_deltaybin; y++){

	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].off_axis = offbin*i + offbin/2 + offmin ;
	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].phi = phibin*j + phibin/2 ;
	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].deltax = deltaxbin*x + deltaxmin;
	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].deltay = deltaybin*y + deltaymin;
	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].duration = 0;

       }
     }
   }
 }


 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
   {
     for(n=0; n<ReadRows ; n++)
       {

	 if(init){
	   /* Update 'tstart' 'off_axis' 'phi' 'deltax_cen' and 'deltay_cen' values */
	   tstart = DVEC(table, n, indxcol.TIME);
	   off_axis = DVEC(table, n, indxcol.OFF_AXIS);
	   phi = DVEC(table, n, indxcol.PHI);
	   deltax_cen = DVEC(table, n, indxcol.DELTAX_CEN);
	   deltay_cen = DVEC(table, n, indxcol.DELTAY_CEN);
	   init = NOT_OK;
	 }
	 else{
	   /* Update 'tstop' value */
	   tstop = DVEC(table, n, indxcol.TIME);
	   dtime = tstop-tstart;


	   /* Update histogram if DELTAX_CEN and DELTAY_CEN haven't NULL values */
	   if( !isnan(deltax_cen) && !isnan(deltay_cen) ){
	     
	     /* Compute bin offidx (between 0 - n_offbin-1) */	   
	     offidx = GetCenterBinIndex(offbin, offmin, 0, n_offbin-1, off_axis);
	     
	     /* Compute bin phiidx (between 0 - n_phibin-1) */	   
	     phiidx = GetCenterBinIndex(phibin, 0, 0, n_phibin-1, phi);
	     
	     /* Compute bin deltaxidx (between 0 - n_deltaxbin-1) */	   
	     deltaxidx = GetInitBinIndex(deltaxbin, deltaxmin, 0, n_deltaxbin-1, deltax_cen);
	     
	     /* Compute bin deltayidx (between 0 - n_deltaybin-1) */	   
	     deltayidx = GetInitBinIndex(deltaybin, deltaymin, 0, n_deltaybin-1, deltay_cen);
	     
	     
	     /* Compute overlap exposure of the time bin with GTI */
	     dtime = HDgti_exp(tstart, tstop, gti, &status);
	     if(status!=OK){
	       headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
	       goto ComputeApertureStopOffAxisHisto_end;
	     }
	     
	     /* Update duration value */
	     (*histodata)[ ((offidx*n_phibin + phiidx)*n_deltaxbin + deltaxidx)*n_deltaybin + deltayidx ].duration += dtime;
	     
	   }


	   /* Update 'tstart' 'off_axis' 'phi' 'deltax_cen' and 'deltay_cen' values */
	   tstart = DVEC(table, n, indxcol.TIME);
	   off_axis = DVEC(table, n, indxcol.OFF_AXIS);
	   phi = DVEC(table, n, indxcol.PHI);
	   deltax_cen = DVEC(table, n, indxcol.DELTAX_CEN);
	   deltay_cen = DVEC(table, n, indxcol.DELTAY_CEN);
	 }

       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 


 /* Update duration value of the last row */	   
 tstop = tstart + dtime;
   
 offidx = GetCenterBinIndex(offbin, offmin, 0, n_offbin-1, off_axis);
 phiidx = GetCenterBinIndex(phibin, 0, 0, n_phibin-1, phi);
 deltaxidx = GetInitBinIndex(deltaxbin, 0, deltaxmin, n_deltaxbin-1, deltax_cen);
 deltayidx = GetInitBinIndex(deltaybin, 0, deltaymin, n_deltaybin-1, deltay_cen);

 dtime = HDgti_exp(tstart, tstop, gti, &status);
 if(status!=OK){
   headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
   goto ComputeApertureStopOffAxisHisto_end;
 }

 (*histodata)[ ((offidx*n_phibin + phiidx)*n_deltaxbin + deltaxidx)*n_deltaybin + deltayidx ].duration += dtime;

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile);
      return NOT_OK;
    }

  return OK;

  
 ComputeApertureStopOffAxisHisto_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ComputeApertureStopOffAxisHisto */


int ReadApStopCorrFile(char *filename, ApStopCorrData_t ** apstop_data, int *ncount, ApStopCorrInfo_t *apstop_info){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK, jj;
  char               KeyWord[FLEN_KEYWORD];
  double             aperture_tscale=1., aperture_tzero=0.;
  ApStopCorrCol_t    indxcol;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
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
 
  /* Move in APERTURE extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_APERTURE, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, KWVL_EXTNAME_APERTURE);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadApStopCorrFile_end;
    }

  /* Retrieve header pointer */  
  head=RetrieveFitsHeader(unit);

  if((ExistsKeyWord(&head, "THETABIN", &card))) {
    apstop_info->as_theta_bin = card->u.DVal;
    apstop_info->as_theta_bin = apstop_info->as_theta_bin/60; /* from arcsec to arcmin */
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "THETABIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }
  
  if((ExistsKeyWord(&head, "THETAMIN", &card))) {
    apstop_info->as_theta_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "THETAMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }

  if((ExistsKeyWord(&head, "XCENBIN", &card))) {
    apstop_info->as_deltax_cen_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "XCENBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }

  if((ExistsKeyWord(&head, "XCENMIN", &card))) {
    apstop_info->as_deltax_cen_min = card->u.DVal;
  }
  else{
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "XCENMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }

  if((ExistsKeyWord(&head, "YCENBIN", &card))) {
    apstop_info->as_deltay_cen_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "YCENBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }

  if((ExistsKeyWord(&head, "YCENMIN", &card))) {
    apstop_info->as_deltay_cen_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "YCENMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }


  if((ExistsKeyWord(&head, "ENLOBIN", &card))) {
    apstop_info->as_energ_lo_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "ENLOBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }

  if((ExistsKeyWord(&head, "ENLOMIN", &card))) {
    apstop_info->as_energ_lo_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "ENLOMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }

  if((ExistsKeyWord(&head, "ENHIBIN", &card))) {
    apstop_info->as_energ_hi_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "ENHIBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }

  if((ExistsKeyWord(&head, "ENHIMIN", &card))) {
    apstop_info->as_energ_hi_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "ENHIMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }


  GetBintableStructure(&head, &table, BINTAB_ROWS_SMALL, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadApStopCorrFile_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.AZIMUTH = GetColNameIndx(&table, CLNM_AZIMUTH)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_AZIMUTH);
      goto ReadApStopCorrFile_end;
    }


  if ((indxcol.ENERG_LO = GetColNameIndx(&table, CLNM_ENERG_LO)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_ENERG_LO);
      goto ReadApStopCorrFile_end;
    }
  apstop_info->as_energ_lo_dim = table.Multiplicity[indxcol.ENERG_LO];


  if ((indxcol.ENERG_HI = GetColNameIndx(&table, CLNM_ENERG_HI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_ENERG_HI);
      goto ReadApStopCorrFile_end;
    }
  apstop_info->as_energ_hi_dim = table.Multiplicity[indxcol.ENERG_HI];


  if ((indxcol.THETA = GetColNameIndx(&table, CLNM_THETA)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_THETA);
      goto ReadApStopCorrFile_end;
    }
  apstop_info->as_theta_dim = table.Multiplicity[indxcol.THETA];


  if ((indxcol.DELTAX_CEN = GetColNameIndx(&table, CLNM_DELTAX_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAX_CEN);
      goto ReadApStopCorrFile_end;
    }
  apstop_info->as_deltax_cen_dim = table.Multiplicity[indxcol.DELTAX_CEN];


  if ((indxcol.DELTAY_CEN = GetColNameIndx(&table, CLNM_DELTAY_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAY_CEN);
      goto ReadApStopCorrFile_end;
    }
  apstop_info->as_deltay_cen_dim = table.Multiplicity[indxcol.DELTAY_CEN];


  if ((indxcol.APERTURE = GetColNameIndx(&table, CLNM_APERTURE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_APERTURE);
      goto ReadApStopCorrFile_end;
    }
  apstop_info->as_aperture_dim = table.Multiplicity[indxcol.APERTURE];

  sprintf(KeyWord, "TSCALE%d", (indxcol.APERTURE+1));
  if((ExistsKeyWord(&head, KeyWord, &card))) {
     aperture_tscale = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KeyWord);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }

  sprintf(KeyWord, "TZERO%d", (indxcol.APERTURE+1));
  if((ExistsKeyWord(&head, KeyWord, &card))) {
     aperture_tzero = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KeyWord);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadApStopCorrFile_end;
  }


  if(apstop_info->as_aperture_dim != (apstop_info->as_energ_lo_dim*apstop_info->as_theta_dim*apstop_info->as_deltax_cen_dim*apstop_info->as_deltay_cen_dim))
    {
      headas_chat(NORMAL, "%s: Error: bad multiplicity of aperture stop correction file columns.\n", global.taskname);
      goto ReadApStopCorrFile_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *ncount = table.MaxRows;
 *apstop_data = (ApStopCorrData_t *)calloc(*ncount, sizeof(ApStopCorrData_t));
 if(*apstop_data==NULL){
   headas_chat(CHATTY,"%s: Error: ReadApStopCorrFile: memory allocation failure.\n", global.taskname);
   goto ReadApStopCorrFile_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<*ncount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {

	 (*apstop_data)[count].azimuth = EVEC(table, n, indxcol.AZIMUTH);

/* 	 (*apstop_data)[count].energ_lo = (float*)calloc(apstop_info->as_energ_lo_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*apstop_data)[count].energ_lo, apstop_info->as_energ_lo_dim, table, n, indxcol.ENERG_LO); */

/* 	 (*apstop_data)[count].energ_hi = (float*)calloc(apstop_info->as_energ_hi_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*apstop_data)[count].energ_hi, apstop_info->as_energ_hi_dim, table, n, indxcol.ENERG_HI); */

/* 	 (*apstop_data)[count].theta = (float*)calloc(apstop_info->as_theta_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*apstop_data)[count].theta, apstop_info->as_theta_dim, table, n, indxcol.THETA); */

/* 	 (*apstop_data)[count].deltax_cen = (float*)calloc(apstop_info->as_deltax_cen_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*apstop_data)[count].deltax_cen, apstop_info->as_deltax_cen_dim, table, n, indxcol.DELTAX_CEN); */

/* 	 (*apstop_data)[count].deltay_cen = (float*)calloc(apstop_info->as_deltay_cen_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*apstop_data)[count].deltay_cen, apstop_info->as_deltay_cen_dim, table, n, indxcol.DELTAY_CEN); */

	 (*apstop_data)[count].aperture = (float*)malloc(apstop_info->as_aperture_dim*sizeof(float));
	 if((*apstop_data)[count].aperture==NULL){
	   headas_chat(CHATTY,"%s: Error: ReadApStopCorrFile: memory allocation failure.\n", global.taskname);
	   goto ReadApStopCorrFile_end;
	 }

	 for(jj=0; jj<apstop_info->as_aperture_dim; jj++)
	   (*apstop_data)[count].aperture[jj] = IVECVEC(table,n,indxcol.APERTURE,jj)*aperture_tscale + aperture_tzero ;


	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS_SMALL;
   }/* while */
   
  *ncount = count;


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

  
 ReadApStopCorrFile_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadApStopCorrFile  */


int WriteApStopOffAxisHistoFile(ApStopOffAxisHistoData_t *histodata, int histocount, char *infile, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open metrology input file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, infile);
      goto WriteApStopOffAxisHistoFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteApStopOffAxisHistoFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WriteApStopOffAxisHistoFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteApStopOffAxisHistoFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteApStopOffAxisHistoFile_end;
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
      goto WriteApStopOffAxisHistoFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteApStopOffAxisHistoFile_end;
      }
    }

  /* Create 'OFFAXIS_HISTO' Ext  */
  if (WriteApStopOffAxisHistoExt(histodata, histocount, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_OFFAXIS_HISTO);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteApStopOffAxisHistoFile_end;
    }
  hducount++;

  /* Add history to Bad Pixel Ext */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteApStopOffAxisHistoFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteApStopOffAxisHistoFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteApStopOffAxisHistoFile_end;
    }


  return OK;
  
 WriteApStopOffAxisHistoFile_end:

  return NOT_OK;

} /* WriteApStopOffAxisHistoFile */


int WriteApStopOffAxisHistoExt(ApStopOffAxisHistoData_t *histodata, int nrows, FitsFileUnit_t ounit){

  int                n, count=0;
  unsigned           OutRows=0;
  ApStopOffAxisHistoCol_t  indxcol;
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

  AddColumn(&newhead, &table, CLNM_OFF_AXIS, CARD_COMM_OFF_AXIS, "1D", TUNIT, UNIT_ARCMIN, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_PHI, CARD_COMM_PHI, "1D", TUNIT, UNIT_DEG, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DELTAX, "Aperture Stop - Optical Axis X distance in OB frame", "1D", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DELTAY, "Aperture Stop - Optical Axis Y distance in OB frame", "1D", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DURATION, CARD_COMM_DURATION, "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_OFFAXIS_HISTO, CARD_COMM_EXTNAME);

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

  if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS);
      goto WriteApStopOffAxisHistoExt_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, CLNM_PHI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_PHI);
      goto WriteApStopOffAxisHistoExt_end;
    }

  if ((indxcol.DELTAX = GetColNameIndx(&table, CLNM_DELTAX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAX);
      goto WriteApStopOffAxisHistoExt_end;
    }

  if ((indxcol.DELTAY = GetColNameIndx(&table, CLNM_DELTAY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAY);
      goto WriteApStopOffAxisHistoExt_end;
    }

  if ((indxcol.DURATION = GetColNameIndx(&table, CLNM_DURATION)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DURATION);
      goto WriteApStopOffAxisHistoExt_end;
    }


  OutRows = 0;
  count = 0;

  while(count<nrows){

    for(n=0; (n<BINTAB_ROWS)&&(count<nrows); n++){
      
      DVEC(table, n, indxcol.OFF_AXIS) = histodata[count].off_axis;
      DVEC(table, n, indxcol.PHI) = histodata[count].phi;
      DVEC(table, n, indxcol.DELTAX) = histodata[count].deltax;
      DVEC(table, n, indxcol.DELTAY) = histodata[count].deltay;
      DVEC(table, n, indxcol.DURATION) = histodata[count].duration;

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
  
 WriteApStopOffAxisHistoExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteApStopOffAxisHistoExt */


int ComputeApertureStopCorr(struct gti_struct *gti, char *offaxisfile, ApStopCorrVal_t *apstopcorr){

  ApStopOffAxisHistoInfo_t   apstophistoinfo;
  ApStopOffAxisHistoData_t   *apstophistodata=NULL;
  int                        apstophistocount;
  ApStopCorrInfo_t   apstopinfo;
  ApStopCorrData_t   *apstopdata=NULL;
  int                apstopcount;
  long               extfile;


  /* Derive CALDB apstopcorrfile name */
  if ( !strcasecmp(global.par.apstopcorrfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_APERTURE_DSET, global.par.apstopcorrfile, HD_EXPR, &extfile, global.obsinfo.instrume, HD_DETNAM))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for apstopfile parameter.\n", global.taskname);
	  goto ComputeApertureStopCorr_end;
	}
      extfile++;
    }
  
  /*   Retrieve Aperture Stop Correction info from input apstopcorrfile */
  if( ReadApStopCorrFile(global.par.apstopcorrfile, &apstopdata, &apstopcount, &apstopinfo) )
    {
      headas_chat(NORMAL, "%s: Error: unable to read Aperture Stop Correction file.\n", global.taskname);
      goto ComputeApertureStopCorr_end;
    }


  /* Compute Aperture Stop Histogram */
  if(ComputeApertureStopOffAxisHisto(gti, offaxisfile, &apstopinfo, &apstophistodata, &apstophistocount, &apstophistoinfo)){
    headas_chat(NORMAL, "%s: Error: Unable to compute Aperture Stop Histogram.\n", global.taskname);
    goto ComputeApertureStopCorr_end;
  }
  
  
  if( global.createapstophisto && !global.par.extended )
    {
      /* Create output Aperture Stop Histogram file */
      if(WriteApStopOffAxisHistoFile(apstophistodata, apstophistocount, global.tmpout.offaxisfile, global.par.apstophisto)){
	headas_chat(NORMAL, "%s: Error: Unable to create output Off-Axis Histogram file '%s'\n", global.taskname, global.par.apstophisto);
	goto ComputeApertureStopCorr_end;
      }
      
      headas_chat(NORMAL, "%s: Info: Aperture Stop Histogram File '%s' created.\n", global.taskname, global.par.apstophisto);
    }


  /* Allocate memory to storage correction values */
  apstopcorr->n_energy = apstopinfo.as_energ_lo_dim;
  apstopcorr->en = (double*)calloc(apstopcorr->n_energy, sizeof(double));
  apstopcorr->corr = (double*)calloc(apstopcorr->n_energy, sizeof(double));
  if(apstopcorr->en==NULL || apstopcorr->corr==NULL){
    headas_chat(CHATTY,"%s: Error: ComputeApertureStopCorr: memory allocation failure.\n", global.taskname);
    goto ComputeApertureStopCorr_end;
  }


  /* Compute Aperture Stop correction coefficients */
  ComputeApertureCorrection(&apstopinfo, apstopdata, apstopcount, &apstophistoinfo, apstophistodata, apstophistocount, apstopcorr);


    /* Free memory */

  free(apstophistodata);

  FreeApStopCorrData(apstopdata, apstopcount);


  return OK;

 ComputeApertureStopCorr_end:
  return NOT_OK;
  
} /* ComputeApertureStopCorr */


int ReadAlignInfo(char *filename, AlignInfo_t *aligninfo){

  unsigned           FromRow, ReadRows, nCols;
  int                status=OK;
  AlignCol_t         col;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

 
  headas_chat(CHATTY, "%s: Info: Processing %s file.\n", global.taskname, filename);
  /* Open readonly input align file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
 
  /* Move in SYSTEM_ALIGNMENT extension in input align file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_SYSTEM_ALIGNMENT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_SYSTEM_ALIGNMENT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadAlignInfo_end;
    }
  
  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  /* Get needed columns number from name */

  if ((col.Q_DET2A_OB=ColNameMatch(CLNM_Q_DET2A_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_DET2A_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_DET2B_OB=ColNameMatch(CLNM_Q_DET2B_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_DET2B_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }


  EndBintableHeader(&head, &table);


  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {

    DVECVEC_ARRAY_READ(aligninfo->Qdet2Aob, 4, table, 0, col.Q_DET2A_OB);
    DVECVEC_ARRAY_READ(aligninfo->Qdet2Bob, 4, table, 0, col.Q_DET2B_OB);
  }

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Move in OPTICAL_AXIS extension in input align file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_OPTICAL_AXIS, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OPTICAL_AXIS);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      if( CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	}
      goto ReadAlignInfo_end;
    }
  
  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  /* Get needed columns number from name */

  if ((col.X_DET2A=ColNameMatch(CLNM_X_DET2A, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_DET2A);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Y_DET2A=ColNameMatch(CLNM_Y_DET2A, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_DET2A);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.X_DET2B=ColNameMatch(CLNM_X_DET2B, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X_DET2B);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Y_DET2B=ColNameMatch(CLNM_Y_DET2B, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y_DET2B);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }


  EndBintableHeader(&head, &table);


  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {
    aligninfo->x_det2a = DVEC(table,0,col.X_DET2A);
    aligninfo->y_det2a = DVEC(table,0,col.Y_DET2A);
    aligninfo->x_det2b = DVEC(table,0,col.X_DET2B);
    aligninfo->y_det2b = DVEC(table,0,col.Y_DET2B);
  }

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadAlignInfo_end;
    }



  return OK;

 ReadAlignInfo_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);

  return NOT_OK;

} /* ReadAlignInfo */


void ComputeApertureCorrection(ApStopCorrInfo_t *apinfo, ApStopCorrData_t *apdata, int apcount, ApStopOffAxisHistoInfo_t *histoinfo, ApStopOffAxisHistoData_t *histodata, int histocount, ApStopCorrVal_t *apstopcorr){

  int     i=0, j=0, e=0, x=0, y=0;
  int     n_energy, n_offbin, n_phibin, n_deltaxbin, n_deltaybin;
  double  dtot=0, phi_ob=0, off_axis=0;
  double  aperture=0;


  /* Set local variables */

  n_energy = apinfo->as_energ_lo_dim;

  n_phibin = histoinfo->ash_phibin_dim;
  n_offbin = histoinfo->ash_offbin_dim;
  n_deltaxbin = histoinfo->ash_deltax_dim;  /* must be equal to apinfo->as_deltax_cen_dim */
  n_deltaybin = histoinfo->ash_deltay_dim;  /* must be equal to apinfo->as_deltay_cen_dim */


  /* Compute the total duration */
  for(i=0; i<histocount; i++){
    dtot += histodata[i].duration ;
  }


  for(e=0; e<n_energy; e++){
    
    apstopcorr->corr[e] = 0.;
    /* energy mean */
    apstopcorr->en[e] = ( (e*apinfo->as_energ_lo_bin + apinfo->as_energ_lo_min)+(e*apinfo->as_energ_hi_bin + apinfo->as_energ_hi_min) )/2;
    
    for(i=0; i<n_offbin; i++){  /* theta */
      for(j=0; j<n_phibin; j++){
	for(x=0; x<n_deltaxbin; x++){
	  for(y=0; y<n_deltaybin; y++){

	    if( histodata[((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y].duration>0 )
	      {
		phi_ob = 180.0 - histodata[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y  ].phi + global.obsinfo.pa_pnt + 120.;
		phi_ob = NormalaziedAt360Angle(phi_ob);
		
		off_axis = histodata[((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y].off_axis ;
		
		
		/* Compute aperture value */
		aperture = GetAperture(apinfo, apdata, apcount, off_axis, phi_ob, e, x, y);
		
		
		/* Compute correction value */
		apstopcorr->corr[e] += ( aperture * histodata[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y  ].duration)/dtot ;
	   
	      }

	  }
	}
      }
    }

  } /* END -> for(e=0; e<n_energy; e++) */

  
} /* ComputeApertureCorrection  */


void FreeApStopCorrVal(ApStopCorrVal_t *apstopcorr){

  if(apstopcorr->n_energy>0)
    {
      apstopcorr->n_energy=0;
      free(apstopcorr->en);
      free(apstopcorr->corr);
    }

} /* FreeApStopCorrVal */


void FreePsfCorrVal(PsfCorrVal_t *psfcorr){

  if(psfcorr->n_energy>0)
    {
      psfcorr->n_energy=0;
      free(psfcorr->en);
      free(psfcorr->corr);
    }

} /* FreePsfCorrVal */


int GetApStopCorr(ApStopCorrVal_t *apstopcorr, double en, double *corr){

  int      i,j;
  double   enbin=0.;

  
  if(apstopcorr->n_energy>1)
    enbin = apstopcorr->en[1] - apstopcorr->en[0];


  FindClosestApStopCorrIndex(apstopcorr, en, &i);


  if( (i==0 && en<apstopcorr->en[0]-enbin/2) || (i==apstopcorr->n_energy-1 && en>(apstopcorr->en[i]+enbin/2)) )
    {
      *corr = 1.;
    }
  else
    {
      j = (i+1<apstopcorr->n_energy) ? i+1 : i ;

      InterpolateValues(apstopcorr->en[i], apstopcorr->en[j], en, apstopcorr->corr[i], apstopcorr->corr[j], corr);
    }


  return OK;

} /* GetApStopCorr */


void FindClosestApStopCorrIndex(ApStopCorrVal_t *apstopcorr, double value, int *index){
  
  int      i=0, low=0, high=0, mid;
  
  /* Find appropriate row ( after loop index 'i' indicates the first row over 'value' value) */
  low = 0;
  high = apstopcorr->n_energy-1;
  
  while (low != high) {
    mid = low + (high-low)/2;
    if (apstopcorr->en[mid] <= value) {
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

  /* index should contains nearest row under 'value' value */
  if( (i>0) && (apstopcorr->en[i]>value) ){
    i--;
  }

  *index = i;
   
} /* FindClosestApStopCorrIndex */


double GetAperture(ApStopCorrInfo_t *apinfo, ApStopCorrData_t *apdata, int apcount, double off_axis, double phi, int i_energy, int i_deltax, int i_deltay){

  int     irow1=0, irow2=0, itheta1=0, itheta2=0;
  int     n_energy, n_theta, n_deltax, n_deltay;
  double  theta1, theta2, aperture1, aperture2, aperture_interpol1, aperture_interpol2, aperture_interpol;


  /* Set local variables */

  n_energy = apinfo->as_energ_lo_dim;
  n_theta = apinfo->as_theta_dim ;
  n_deltax = apinfo->as_deltax_cen_dim ;
  n_deltay = apinfo->as_deltay_cen_dim ;


  /* Compute aperture azimuth row index */

  for(irow1=0; irow1<apcount; irow1++)
    if( apdata[irow1].azimuth > phi )
      break;
	    
  if(irow1==apcount)
    {
      irow1 = apcount-1;
      irow2 = 0;
    }
  else
    {
      irow2 = (irow1-1)>=0 ? (irow1-1) : irow1;
    }
  

  /* Compute aperture itheta indexes (between 0 - n_theta-1) and values */

  itheta1 = GetCenterBinIndex(apinfo->as_theta_bin, apinfo->as_theta_min, 0, n_theta-1, off_axis);
  itheta2 = (itheta1+1<n_theta) ? (itheta1+1) : itheta1 ;
  
  theta1 = itheta1*apinfo->as_theta_bin + apinfo->as_theta_min ;
  theta2 = itheta2*apinfo->as_theta_bin + apinfo->as_theta_min ;


  /* Compute the aperture interpolation (off_axis) for the first azimuth row  */

  aperture1 = apdata[irow1].aperture[ ((itheta1*n_deltax + i_deltax)*n_deltay + i_deltay)*n_energy + i_energy ];
  aperture2 = apdata[irow1].aperture[ ((itheta2*n_deltax + i_deltax)*n_deltay + i_deltay)*n_energy + i_energy ];

  InterpolateValues(theta1, theta2, off_axis, aperture1, aperture2, &aperture_interpol1);

  /* Compute the aperture interpolation (off_axis) for the second azimuth row  */

  aperture1 = apdata[irow2].aperture[ ((itheta1*n_deltax + i_deltax)*n_deltay + i_deltay)*n_energy + i_energy ];
  aperture2 = apdata[irow2].aperture[ ((itheta2*n_deltax + i_deltax)*n_deltay + i_deltay)*n_energy + i_energy ];

  InterpolateValues(theta1, theta2, off_axis, aperture1, aperture2, &aperture_interpol2);


  /* Compute the aperture interpolation (azimuth) for the two previous interpolated values */

  InterpolateValues(apdata[irow1].azimuth, apdata[irow2].azimuth, phi, aperture_interpol1, aperture_interpol2, &aperture_interpol);
  

  return aperture_interpol;


} /* GetAperture */


/* ----------------------------------------------------------------------------------------------------------------------- */
/*                          Ghost Rays Correction Routines    ---->>                                                       */
/* ----------------------------------------------------------------------------------------------------------------------- */


int ReadGhostRaysCorrFile(char *filename, GhostRaysCorrData_t ** gr_data, int *ncount, GhostRaysCorrInfo_t *gr_info){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK, jj;
  char               KeyWord[FLEN_KEYWORD];
  double             ghostrays_tscale=1., ghostrays_tzero=0.;
  GhostRaysCorrCol_t indxcol;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
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
 
  /* Move in GHOST_RAYS extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_GHOST_RAYS, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, KWVL_EXTNAME_GHOST_RAYS);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadGhostRaysCorrFile_end;
    }

  /* Retrieve header pointer */  
  head=RetrieveFitsHeader(unit);

  if((ExistsKeyWord(&head, "THETABIN", &card))) {
    gr_info->theta_bin = card->u.DVal;
    gr_info->theta_bin = gr_info->theta_bin/60; /* from arcsec to arcmin */
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "THETABIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }
  
  if((ExistsKeyWord(&head, "THETAMIN", &card))) {
    gr_info->theta_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "THETAMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "XCENBIN", &card))) {
    gr_info->deltax_cen_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "XCENBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "XCENMIN", &card))) {
    gr_info->deltax_cen_min = card->u.DVal;
  }
  else{
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "XCENMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "YCENBIN", &card))) {
    gr_info->deltay_cen_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "YCENBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "YCENMIN", &card))) {
    gr_info->deltay_cen_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "YCENMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }


  if((ExistsKeyWord(&head, "ENLOBIN", &card))) {
    gr_info->energ_lo_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "ENLOBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "ENLOMIN", &card))) {
    gr_info->energ_lo_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "ENLOMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "ENHIBIN", &card))) {
    gr_info->energ_hi_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "ENHIBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "ENHIMIN", &card))) {
    gr_info->energ_hi_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "ENHIMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "RADBIN", &card))) {
    gr_info->radius_bin = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "RADBIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  if((ExistsKeyWord(&head, "RADMIN", &card))) {
    gr_info->radius_min = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "RADMIN");
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  GetBintableStructure(&head, &table, BINTAB_ROWS_SMALL, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadGhostRaysCorrFile_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.AZIMUTH = GetColNameIndx(&table, CLNM_AZIMUTH)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_AZIMUTH);
      goto ReadGhostRaysCorrFile_end;
    }


  if ((indxcol.ENERG_LO = GetColNameIndx(&table, CLNM_ENERG_LO)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_ENERG_LO);
      goto ReadGhostRaysCorrFile_end;
    }
  gr_info->energ_lo_dim = table.Multiplicity[indxcol.ENERG_LO];


  if ((indxcol.ENERG_HI = GetColNameIndx(&table, CLNM_ENERG_HI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_ENERG_HI);
      goto ReadGhostRaysCorrFile_end;
    }
  gr_info->energ_hi_dim = table.Multiplicity[indxcol.ENERG_HI];


  if ((indxcol.THETA = GetColNameIndx(&table, CLNM_THETA)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_THETA);
      goto ReadGhostRaysCorrFile_end;
    }
  gr_info->theta_dim = table.Multiplicity[indxcol.THETA];


  if ((indxcol.DELTAX_CEN = GetColNameIndx(&table, CLNM_DELTAX_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAX_CEN);
      goto ReadGhostRaysCorrFile_end;
    }
  gr_info->deltax_cen_dim = table.Multiplicity[indxcol.DELTAX_CEN];


  if ((indxcol.DELTAY_CEN = GetColNameIndx(&table, CLNM_DELTAY_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAY_CEN);
      goto ReadGhostRaysCorrFile_end;
    }
  gr_info->deltay_cen_dim = table.Multiplicity[indxcol.DELTAY_CEN];


  if ((indxcol.RADIUS = GetColNameIndx(&table, CLNM_RADIUS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_RADIUS);
      goto ReadGhostRaysCorrFile_end;
    }
  gr_info->radius_dim = table.Multiplicity[indxcol.RADIUS];


  if ((indxcol.GHOSTRAYS = GetColNameIndx(&table, CLNM_GHOSTRAYS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_GHOSTRAYS);
      goto ReadGhostRaysCorrFile_end;
    }
  gr_info->ghostrays_dim = table.Multiplicity[indxcol.GHOSTRAYS];

  sprintf(KeyWord, "TSCALE%d", (indxcol.GHOSTRAYS+1));
  if((ExistsKeyWord(&head, KeyWord, &card))) {
     ghostrays_tscale = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KeyWord);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }

  sprintf(KeyWord, "TZERO%d", (indxcol.GHOSTRAYS+1));
  if((ExistsKeyWord(&head, KeyWord, &card))) {
     ghostrays_tzero = card->u.DVal;
  }
  else {
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KeyWord);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto ReadGhostRaysCorrFile_end;
  }


  if(gr_info->ghostrays_dim != (gr_info->energ_lo_dim*gr_info->theta_dim*gr_info->deltax_cen_dim*gr_info->deltay_cen_dim*gr_info->radius_dim))
    {
      headas_chat(NORMAL, "%s: Error: bad multiplicity of ghost rays correction file columns.\n", global.taskname);
      goto ReadGhostRaysCorrFile_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *ncount = table.MaxRows;
 *gr_data = (GhostRaysCorrData_t *)calloc(*ncount, sizeof(GhostRaysCorrData_t));
 if(*gr_data==NULL){
   headas_chat(CHATTY,"%s: Error: ReadGhostRaysCorrFile: memory allocation failure.\n", global.taskname);
   goto ReadGhostRaysCorrFile_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<*ncount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {

	 (*gr_data)[count].azimuth = EVEC(table, n, indxcol.AZIMUTH);

/* 	 (*gr_data)[count].energ_lo = (float*)calloc(gr_info->energ_lo_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*gr_data)[count].energ_lo, gr_info->energ_lo_dim, table, n, indxcol.ENERG_LO); */

/* 	 (*gr_data)[count].energ_hi = (float*)calloc(gr_info->energ_hi_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*gr_data)[count].energ_hi, gr_info->energ_hi_dim, table, n, indxcol.ENERG_HI); */

/* 	 (*gr_data)[count].theta = (float*)calloc(gr_info->theta_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*gr_data)[count].theta, gr_info->theta_dim, table, n, indxcol.THETA); */

/* 	 (*gr_data)[count].deltax_cen = (float*)calloc(gr_info->deltax_cen_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*gr_data)[count].deltax_cen, gr_info->deltax_cen_dim, table, n, indxcol.DELTAX_CEN); */

/* 	 (*gr_data)[count].deltay_cen = (float*)calloc(gr_info->deltay_cen_dim, sizeof(float)); */
/* 	 EVECVEC_ARRAY_READ( (*gr_data)[count].deltay_cen, gr_info->deltay_cen_dim, table, n, indxcol.DELTAY_CEN); */

	 (*gr_data)[count].ghostrays = (float*)malloc(gr_info->ghostrays_dim*sizeof(float));
	 if((*gr_data)[count].ghostrays==NULL){
	   headas_chat(CHATTY,"%s: Error: ReadGhostRaysCorrFile: memory allocation failure.\n", global.taskname);
	   goto ReadGhostRaysCorrFile_end;
	 }

	 for(jj=0; jj<gr_info->ghostrays_dim; jj++)
	   (*gr_data)[count].ghostrays[jj] = IVECVEC(table,n,indxcol.GHOSTRAYS,jj)*ghostrays_tscale + ghostrays_tzero ;


	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS_SMALL;

   }/* while */
   
  *ncount = count;


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

  
 ReadGhostRaysCorrFile_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadGhostRaysCorrFile  */


int ComputeGhostRaysOffAxisHisto(struct gti_struct *gti, char *infile, GhostRaysCorrInfo_t *grinfo, GhostRaysOffAxisHistoData_t **histodata, int *histocount, GhostRaysOffAxisHistoInfo_t *histoinfo){

  unsigned           FromRow, ReadRows, n, nCols;
  int                i, j, x, y, status=OK, init=OK, offidx, phiidx, deltaxidx, deltayidx;
  double             tstart=0, tstop=0, dtime=0, off_axis=0, phi=0, deltax_cen=0., deltay_cen=0.;
  double             offmin, deltaxmin, deltaymin;
  OffAxisCol_t       indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  double             phibin, offbin, deltaxbin, deltaybin;
  int                n_offbin, n_phibin, n_deltaxbin, n_deltaybin;


  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Update grhisto file info */

  histoinfo->phibin = global.par.grphibin;
  histoinfo->phibin_dim = 360/histoinfo->phibin;

  histoinfo->offbin = grinfo->theta_bin / GRH_OFFBIN_SCALE ;
  histoinfo->offbin_min = grinfo->theta_min;
  histoinfo->offbin_dim = grinfo->theta_dim * GRH_OFFBIN_SCALE ;

  histoinfo->deltaxbin = grinfo->deltax_cen_bin;
  histoinfo->deltaxbin_min = grinfo->deltax_cen_min;
  histoinfo->deltax_dim = grinfo->deltax_cen_dim;

  histoinfo->deltaybin = grinfo->deltay_cen_bin;
  histoinfo->deltaybin_min = grinfo->deltay_cen_min;
  histoinfo->deltay_dim = grinfo->deltay_cen_dim;


  /* Set local variables */

  phibin = histoinfo->phibin;
  n_phibin = histoinfo->phibin_dim;

  offbin = histoinfo->offbin;
  offmin = histoinfo->offbin_min;
  n_offbin = histoinfo->offbin_dim;

  deltaxbin = histoinfo->deltaxbin;
  deltaxmin = histoinfo->deltaxbin_min;
  n_deltaxbin = histoinfo->deltax_dim;

  deltaybin = histoinfo->deltaybin;
  deltaymin = histoinfo->deltaybin_min;
  n_deltaybin = histoinfo->deltay_dim;


  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, infile);
      return NOT_OK;
    }
 
  /* Move in OFF_AXIS extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_OFF_AXIS, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_OFF_AXIS);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile); 
      goto ComputeGhostRaysOffAxisHisto_end;
    }
  
  head=RetrieveFitsHeader(unit);


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, infile);
      goto ComputeGhostRaysOffAxisHisto_end;
    }


  /* Get needed columns number from name */

  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto ComputeGhostRaysOffAxisHisto_end;
    }

  if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS);
      goto ComputeGhostRaysOffAxisHisto_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, CLNM_PHI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_PHI);
      goto ComputeGhostRaysOffAxisHisto_end;
    }

  if ((indxcol.DELTAX_CEN = GetColNameIndx(&table, CLNM_DELTAX_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAX_CEN);
      goto ComputeGhostRaysOffAxisHisto_end;
    }

  if ((indxcol.DELTAY_CEN = GetColNameIndx(&table, CLNM_DELTAY_CEN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAY_CEN);
      goto ComputeGhostRaysOffAxisHisto_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *histocount = n_offbin*n_phibin*n_deltaxbin*n_deltaybin;
 *histodata = (GhostRaysOffAxisHistoData_t *)calloc(*histocount, sizeof(GhostRaysOffAxisHistoData_t));
 if(*histodata==NULL){
   headas_chat(CHATTY,"%s: Error: ComputeGhostRaysOffAxisHisto: memory allocation failure.\n", global.taskname);
   goto ComputeGhostRaysOffAxisHisto_end;
 }

 for(i=0; i<n_offbin; i++){
   for(j=0; j<n_phibin; j++){
     for(x=0; x<n_deltaxbin; x++){
       for(y=0; y<n_deltaybin; y++){

	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].off_axis = offbin*i + offbin/2 + offmin ;
	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].phi = phibin*j + phibin/2 ;
	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].deltax = deltaxbin*x + deltaxmin;
	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].deltay = deltaybin*y + deltaymin;
	 (*histodata)[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y ].duration = 0;

       }
     }
   }
 }


 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
   {
     for(n=0; n<ReadRows ; n++)
       {

	 if(init){
	   /* Update 'tstart' 'off_axis' 'phi' 'deltax_cen' and 'deltay_cen' values */
	   tstart = DVEC(table, n, indxcol.TIME);
	   off_axis = DVEC(table, n, indxcol.OFF_AXIS);
	   phi = DVEC(table, n, indxcol.PHI);
	   deltax_cen = DVEC(table, n, indxcol.DELTAX_CEN);
	   deltay_cen = DVEC(table, n, indxcol.DELTAY_CEN);
	   init = NOT_OK;
	 }
	 else{
	   /* Update 'tstop' value */
	   tstop = DVEC(table, n, indxcol.TIME);
	   dtime = tstop-tstart;


	   /* Update histogram if DELTAX_CEN and DELTAY_CEN haven't NULL values */
	   if( !isnan(deltax_cen) && !isnan(deltay_cen) ){
	     
	     /* Compute bin offidx (between 0 - n_offbin-1) */	   
	     offidx = GetCenterBinIndex(offbin, offmin, 0, n_offbin-1, off_axis);
	     
	     /* Compute bin phiidx (between 0 - n_phibin-1) */	   
	     phiidx = GetCenterBinIndex(phibin, 0, 0, n_phibin-1, phi);
	     
	     /* Compute bin deltaxidx (between 0 - n_deltaxbin-1) */	   
	     deltaxidx = GetInitBinIndex(deltaxbin, deltaxmin, 0, n_deltaxbin-1, deltax_cen);	     

	     /* Compute bin deltayidx (between 0 - n_deltaybin-1) */	   
	     deltayidx = GetInitBinIndex(deltaybin, deltaymin, 0, n_deltaybin-1, deltay_cen);
	     
	     /* Compute overlap exposure of the time bin with GTI */
	     dtime = HDgti_exp(tstart, tstop, gti, &status);
	     if(status!=OK){
	       headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
	       goto ComputeGhostRaysOffAxisHisto_end;
	     }
	     
	     /* Update duration value */
	     (*histodata)[ ((offidx*n_phibin + phiidx)*n_deltaxbin + deltaxidx)*n_deltaybin + deltayidx ].duration += dtime;
	     
	   }


	   /* Update 'tstart' 'off_axis' 'phi' 'deltax_cen' and 'deltay_cen' values */
	   tstart = DVEC(table, n, indxcol.TIME);
	   off_axis = DVEC(table, n, indxcol.OFF_AXIS);
	   phi = DVEC(table, n, indxcol.PHI);
	   deltax_cen = DVEC(table, n, indxcol.DELTAX_CEN);
	   deltay_cen = DVEC(table, n, indxcol.DELTAY_CEN);
	 }

       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 


 /* Update duration value of the last row */	   
 tstop = tstart + dtime;
   
 offidx = GetCenterBinIndex(offbin, offmin, 0, n_offbin-1, off_axis);
 phiidx = GetCenterBinIndex(phibin, 0, 0, n_phibin-1, phi);
 deltaxidx = GetInitBinIndex(deltaxbin, 0, deltaxmin, n_deltaxbin-1, deltax_cen);
 deltayidx = GetInitBinIndex(deltaybin, 0, deltaymin, n_deltaybin-1, deltay_cen);

 dtime = HDgti_exp(tstart, tstop, gti, &status);
 if(status!=OK){
   headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
   goto ComputeGhostRaysOffAxisHisto_end;
 }

 (*histodata)[ ((offidx*n_phibin + phiidx)*n_deltaxbin + deltaxidx)*n_deltaybin + deltayidx ].duration += dtime;

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile);
      return NOT_OK;
    }

  return OK;

  
 ComputeGhostRaysOffAxisHisto_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ComputeGhostRaysOffAxisHisto */


int WriteGhostRaysOffAxisHistoFile(GhostRaysOffAxisHistoData_t *histodata, int histocount, char *infile, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  /* Open metrology input file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, infile);
      goto WriteGhostRaysOffAxisHistoFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteGhostRaysOffAxisHistoFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WriteGhostRaysOffAxisHistoFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteGhostRaysOffAxisHistoFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteGhostRaysOffAxisHistoFile_end;
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
      goto WriteGhostRaysOffAxisHistoFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteGhostRaysOffAxisHistoFile_end;
      }
    }

  /* Create 'OFFAXIS_HISTO' Ext  */
  if (WriteGhostRaysOffAxisHistoExt(histodata, histocount, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_OFFAXIS_HISTO);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteGhostRaysOffAxisHistoFile_end;
    }
  hducount++;

  /* Add history */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteGhostRaysOffAxisHistoFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteGhostRaysOffAxisHistoFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteGhostRaysOffAxisHistoFile_end;
    }


  return OK;
  
 WriteGhostRaysOffAxisHistoFile_end:

  return NOT_OK;

} /* WriteGhostRaysOffAxisHistoFile */


int WriteGhostRaysOffAxisHistoExt(GhostRaysOffAxisHistoData_t *histodata, int nrows, FitsFileUnit_t ounit){

  int                n, count=0;
  unsigned           OutRows=0;
  GhostRaysOffAxisHistoCol_t  indxcol;
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

  AddColumn(&newhead, &table, CLNM_OFF_AXIS, CARD_COMM_OFF_AXIS, "1D", TUNIT, UNIT_ARCMIN, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_PHI, CARD_COMM_PHI, "1D", TUNIT, UNIT_DEG, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DELTAX, "Aperture Stop - Optical Axis X distance in OB frame", "1D", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DELTAY, "Aperture Stop - Optical Axis Y distance in OB frame", "1D", TUNIT, UNIT_MM, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_DURATION, CARD_COMM_DURATION, "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_OFFAXIS_HISTO, CARD_COMM_EXTNAME);

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

  if ((indxcol.OFF_AXIS = GetColNameIndx(&table, CLNM_OFF_AXIS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_OFF_AXIS);
      goto WriteGhostRaysOffAxisHistoExt_end;
    }

  if ((indxcol.PHI = GetColNameIndx(&table, CLNM_PHI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_PHI);
      goto WriteGhostRaysOffAxisHistoExt_end;
    }

  if ((indxcol.DELTAX = GetColNameIndx(&table, CLNM_DELTAX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAX);
      goto WriteGhostRaysOffAxisHistoExt_end;
    }

  if ((indxcol.DELTAY = GetColNameIndx(&table, CLNM_DELTAY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DELTAY);
      goto WriteGhostRaysOffAxisHistoExt_end;
    }

  if ((indxcol.DURATION = GetColNameIndx(&table, CLNM_DURATION)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DURATION);
      goto WriteGhostRaysOffAxisHistoExt_end;
    }


  OutRows = 0;
  count = 0;

  while(count<nrows){

    for(n=0; (n<BINTAB_ROWS)&&(count<nrows); n++){
      
      DVEC(table, n, indxcol.OFF_AXIS) = histodata[count].off_axis;
      DVEC(table, n, indxcol.PHI) = histodata[count].phi;
      DVEC(table, n, indxcol.DELTAX) = histodata[count].deltax;
      DVEC(table, n, indxcol.DELTAY) = histodata[count].deltay;
      DVEC(table, n, indxcol.DURATION) = histodata[count].duration;

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
  
 WriteGhostRaysOffAxisHistoExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteGhostRaysOffAxisHistoExt */


int ComputeGhostRaysCorr(struct gti_struct *gti, char *offaxisfile, double reg_radius, GhostRaysCorrVal_t *grcorr){

  GhostRaysOffAxisHistoInfo_t   grhistoinfo;
  GhostRaysOffAxisHistoData_t   *grhistodata=NULL;
  int                           grhistocount;
  double                reg_radius_mm;
  GhostRaysCorrInfo_t   grinfo;
  GhostRaysCorrData_t   *grdata=NULL;
  int                   grcount;
  long                  extfile;


  /* Convert radius to mm */
  reg_radius_mm = reg_radius*SUBPIX_SIZE_MM ;


  /* Derive CALDB grcorrfile name */
  if ( !strcasecmp(global.par.grcorrfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_GHOSTRAYS_DSET, global.par.grcorrfile, HD_EXPR, &extfile, global.obsinfo.instrume, HD_DETNAM))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for grcorrfile parameter.\n", global.taskname);
	  goto ComputeGhostRaysCorr_end;
	}
      extfile++;
    }
  
  /*   Retrieve Ghost Rays Correction info from input grcorrfile */
  if( ReadGhostRaysCorrFile(global.par.grcorrfile, &grdata, &grcount, &grinfo) )
    {
      headas_chat(NORMAL, "%s: Error: unable to read Ghost Rays Correction file.\n", global.taskname);
      goto ComputeGhostRaysCorr_end;
    }


  /* Compute Ghost Rays Histogram */
  if(ComputeGhostRaysOffAxisHisto(gti, offaxisfile, &grinfo, &grhistodata, &grhistocount, &grhistoinfo)){
    headas_chat(NORMAL, "%s: Error: Unable to compute Ghost Rays Histogram.\n", global.taskname);
    goto ComputeGhostRaysCorr_end;
  }
  
  
  if( global.creategrhisto && !global.par.extended )
    {
      /* Create output Ghost Rays Histogram file */
      if(WriteGhostRaysOffAxisHistoFile(grhistodata, grhistocount, global.tmpout.offaxisfile, global.par.grhisto)){
	headas_chat(NORMAL, "%s: Error: Unable to create output Ghost Rays Histogram file '%s'\n", global.taskname, global.par.grhisto);
	goto ComputeGhostRaysCorr_end;
      }
      
      headas_chat(NORMAL, "%s: Info: Ghost Rays Histogram File '%s' created.\n", global.taskname, global.par.grhisto);
    }


  /* Allocate memory to storage correction values */
  grcorr->n_energy = grinfo.energ_lo_dim;
  grcorr->en = (double*)calloc(grcorr->n_energy, sizeof(double));
  grcorr->corr = (double*)calloc(grcorr->n_energy, sizeof(double));
  if(grcorr->en==NULL || grcorr->corr==NULL){
    headas_chat(CHATTY,"%s: Error: ComputeGhostRaysCorr: memory allocation failure.\n", global.taskname);
    goto ComputeGhostRaysCorr_end;
  }


  /* Compute Ghost Rays correction coefficients */
  ComputeGhostRaysCorrection(&grinfo, grdata, grcount, &grhistoinfo, grhistodata, grhistocount, reg_radius_mm, grcorr);

  
  /* Free memory */

  free(grhistodata);

  FreeGhostRaysCorrData(grdata, grcount);


  return OK;

 ComputeGhostRaysCorr_end:
  return NOT_OK;
  
} /* ComputeGhostRaysCorr */


void ComputeGhostRaysCorrection(GhostRaysCorrInfo_t *grinfo, GhostRaysCorrData_t *grdata, int grcount, GhostRaysOffAxisHistoInfo_t *histoinfo, GhostRaysOffAxisHistoData_t *histodata, int histocount, double reg_radius, GhostRaysCorrVal_t *grcorr){

  int     i=0, j=0, e=0, x=0, y=0;
  int     n_energy, n_offbin, n_phibin, n_deltaxbin, n_deltaybin;
  double  dtot=0, phi_ob=0, off_axis=0;
  double  ghostrays=0;


  /* Set local variables */

  n_energy = grinfo->energ_lo_dim;

  n_phibin = histoinfo->phibin_dim;
  n_offbin = histoinfo->offbin_dim;
  n_deltaxbin = histoinfo->deltax_dim;  /* must be equal to grinfo->deltax_cen_dim */
  n_deltaybin = histoinfo->deltay_dim;  /* must be equal to grinfo->deltay_cen_dim */


  /* Compute the total duration */
  for(i=0; i<histocount; i++){
    dtot += histodata[i].duration ;
  }


  for(e=0; e<n_energy; e++){
    
    grcorr->corr[e] = 0.;
    /* energy mean */
    grcorr->en[e] = ( (e*grinfo->energ_lo_bin + grinfo->energ_lo_min)+(e*grinfo->energ_hi_bin + grinfo->energ_hi_min) )/2;
    
    for(i=0; i<n_offbin; i++){  /* theta */
      for(j=0; j<n_phibin; j++){
	for(x=0; x<n_deltaxbin; x++){
	  for(y=0; y<n_deltaybin; y++){

	    if( histodata[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y  ].duration>0 )
	      {
		phi_ob = 180.0 - histodata[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y  ].phi + global.obsinfo.pa_pnt + 120.;
		phi_ob = NormalaziedAt360Angle(phi_ob);
		
		off_axis = histodata[((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y].off_axis ;
		
		
		/* Compute ghostrays value */
		ghostrays = GetGhostRays(grinfo, grdata, grcount, off_axis, phi_ob, reg_radius, e, x, y);
		
		
		/* Compute correction value */
		grcorr->corr[e] += ( ghostrays * histodata[ ((i*n_phibin + j)*n_deltaxbin + x)*n_deltaybin + y  ].duration)/dtot ;
	      }

	  }
	}
      }
    }

  } /* END -> for(e=0; e<n_energy; e++) */

  
} /* ComputeGhostRaysCorrection  */


void FreeGhostRaysCorrVal(GhostRaysCorrVal_t *grcorr){

  if(grcorr->n_energy>0)
    {
      grcorr->n_energy=0;
      free(grcorr->en);
      free(grcorr->corr);
    }

} /* FreeGhostRaysCorrVal */


int GetGhostRaysCorr(GhostRaysCorrVal_t *grcorr, double en, double *corr){

  int      i,j;
  double   enbin=0.;

  
  if(grcorr->n_energy>1)
    enbin = grcorr->en[1] - grcorr->en[0];


  FindClosestGhostRaysCorrIndex(grcorr, en, &i);


  if( (i==0 && en<grcorr->en[0]-enbin/2) || (i==grcorr->n_energy-1 && en>(grcorr->en[i]+enbin/2)) )
    {
      *corr = 1.;
    }
  else
    {
      j = (i+1<grcorr->n_energy) ? i+1 : i ;

      InterpolateValues(grcorr->en[i], grcorr->en[j], en, grcorr->corr[i], grcorr->corr[j], corr);
    }


  return OK;

} /* GetGhostRaysCorr */


void FindClosestGhostRaysCorrIndex(GhostRaysCorrVal_t *grcorr, double value, int *index){
  
  int      i=0, low=0, high=0, mid;
  
  /* Find appropriate row ( after loop index 'i' indicates the first row over 'value' value) */
  low = 0;
  high = grcorr->n_energy-1;
  
  while (low != high) {
    mid = low + (high-low)/2;
    if (grcorr->en[mid] <= value) {
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

  /* index should contains nearest row under 'value' value */
  if( (i>0) && (grcorr->en[i]>value) ){
    i--;
  }

  *index = i;
   
} /* FindClosestGhostRaysCorrIndex */


double GetGhostRays(GhostRaysCorrInfo_t *grinfo, GhostRaysCorrData_t *grdata, int grcount, double off_axis, double phi, double radius, int i_energy, int i_deltax, int i_deltay){

  int     irow1=0, irow2=0, itheta1=0, itheta2=0, iradius1=0, iradius2=0;
  int     n_energy, n_theta, n_deltax, n_deltay, n_radius;
  double  theta1, theta2, radius1, radius2, ghostrays_interpol;
  double  ghostrays_p1_r1_t1, ghostrays_p1_r1_t2, ghostrays_interpol_p1_r1, ghostrays_p1_r2_t1, ghostrays_p1_r2_t2, ghostrays_interpol_p1_r2, ghostrays_interpol_p1;
  double  ghostrays_p2_r1_t1, ghostrays_p2_r1_t2, ghostrays_interpol_p2_r1, ghostrays_p2_r2_t1, ghostrays_p2_r2_t2, ghostrays_interpol_p2_r2, ghostrays_interpol_p2;


  /* Set local variables */

  n_energy = grinfo->energ_lo_dim;
  n_theta = grinfo->theta_dim ;
  n_deltax = grinfo->deltax_cen_dim ;
  n_deltay = grinfo->deltay_cen_dim ;
  n_radius = grinfo->radius_dim ;


  /* Compute ghostrays azimuth row index */

  for(irow1=0; irow1<grcount; irow1++)
    if( grdata[irow1].azimuth > phi )
      break;
	    
  if(irow1==grcount)
    {
      irow1 = grcount-1;
      irow2 = 0;
    }
  else
    {
      irow2 = (irow1-1)>=0 ? (irow1-1) : irow1;
    }
  

  /* Compute ghostrays itheta indexes (between 0 - n_theta-1) and values */

  itheta1 = GetCenterBinIndex(grinfo->theta_bin, grinfo->theta_min, 0, n_theta-1, off_axis);
  itheta2 = (itheta1+1<n_theta) ? (itheta1+1) : itheta1 ;
  
  theta1 = itheta1*grinfo->theta_bin + grinfo->theta_min ;
  theta2 = itheta2*grinfo->theta_bin + grinfo->theta_min ;


  /* Compute ghostrays iradius indexes (between 0 - n_radius-1) and values */

  iradius1 = GetInitBinIndex(grinfo->radius_bin, grinfo->radius_min, 0, n_radius-1, radius);
  iradius2 = (iradius1+1<n_radius) ? (iradius1+1) : iradius1 ;
  
  radius1 = iradius1*grinfo->radius_bin + grinfo->radius_min ;
  radius2 = iradius2*grinfo->radius_bin + grinfo->radius_min ;


  /* Compute the ghostrays interpolation (off_axis) for the first azimuth row and first radius [irow1 and iradius1] */

  ghostrays_p1_r1_t1 = grdata[irow1].ghostrays[ (((itheta1*n_deltax + i_deltax)*n_deltay + i_deltay)*n_radius + iradius1)*n_energy + i_energy ];
  ghostrays_p1_r1_t2 = grdata[irow1].ghostrays[ (((itheta2*n_deltax + i_deltax)*n_deltay + i_deltay)*n_radius + iradius1)*n_energy + i_energy ];

  InterpolateValues(theta1, theta2, off_axis, ghostrays_p1_r1_t1, ghostrays_p1_r1_t2, &ghostrays_interpol_p1_r1);


  /* Compute the ghostrays interpolation (off_axis) for the first azimuth row and second radius [irow1 and iradius2] */

  ghostrays_p1_r2_t1 = grdata[irow1].ghostrays[ (((itheta1*n_deltax + i_deltax)*n_deltay + i_deltay)*n_radius + iradius2)*n_energy + i_energy ];
  ghostrays_p1_r2_t2 = grdata[irow1].ghostrays[ (((itheta2*n_deltax + i_deltax)*n_deltay + i_deltay)*n_radius + iradius2)*n_energy + i_energy ];

  InterpolateValues(theta1, theta2, off_axis, ghostrays_p1_r2_t1, ghostrays_p1_r2_t2, &ghostrays_interpol_p1_r2);


  /* Compute the ghostrays interpolation (radius) for the two previous interpolated values */
  InterpolateValues(radius1, radius2, radius, ghostrays_interpol_p1_r1, ghostrays_interpol_p1_r2, &ghostrays_interpol_p1);



  /* Compute the ghostrays interpolation (off_axis) for the second azimuth row and first radius [irow2 and iradius1] */

  ghostrays_p2_r1_t1 = grdata[irow2].ghostrays[ (((itheta1*n_deltax + i_deltax)*n_deltay + i_deltay)*n_radius + iradius1)*n_energy + i_energy ];
  ghostrays_p2_r1_t2 = grdata[irow2].ghostrays[ (((itheta2*n_deltax + i_deltax)*n_deltay + i_deltay)*n_radius + iradius1)*n_energy + i_energy ];

  InterpolateValues(theta1, theta2, off_axis, ghostrays_p2_r1_t1, ghostrays_p2_r1_t2, &ghostrays_interpol_p2_r1);


  /* Compute the ghostrays interpolation (off_axis) for the second azimuth row and second radius [irow2 and iradius2] */

  ghostrays_p2_r2_t1 = grdata[irow2].ghostrays[ (((itheta1*n_deltax + i_deltax)*n_deltay + i_deltay)*n_radius + iradius2)*n_energy + i_energy ];
  ghostrays_p2_r2_t2 = grdata[irow2].ghostrays[ (((itheta2*n_deltax + i_deltax)*n_deltay + i_deltay)*n_radius + iradius2)*n_energy + i_energy ];

  InterpolateValues(theta1, theta2, off_axis, ghostrays_p2_r2_t1, ghostrays_p2_r2_t2, &ghostrays_interpol_p2_r2);


  /* Compute the ghostrays interpolation (radius) for the two previous interpolated values */
  InterpolateValues(radius1, radius2, radius, ghostrays_interpol_p2_r1, ghostrays_interpol_p2_r2, &ghostrays_interpol_p2);



  /* Compute the ghostrays interpolation (azimuth) */

  InterpolateValues(grdata[irow1].azimuth, grdata[irow2].azimuth, phi, ghostrays_interpol_p1, ghostrays_interpol_p2, &ghostrays_interpol);



  return ghostrays_interpol;


} /* GetGhostRays */


/* ----------------------------------------------------------------------------------------------------------------------- */
/*                   <<---- Ghost Rays Correction Routines                                                                 */
/* ----------------------------------------------------------------------------------------------------------------------- */


void FreeApStopCorrData(ApStopCorrData_t *apstopdata, int apstopcount){

  int i;

  for(i=0; i<apstopcount; i++)
    free(apstopdata[i].aperture);

  free(apstopdata);

} /* FreeApStopCorrData */


void FreeGhostRaysCorrData(GhostRaysCorrData_t *grdata, int grcount){

  int i;

  for(i=0; i<grcount; i++)
    free(grdata[i].ghostrays);

  free(grdata);

} /* FreeGhostRaysCorrData */


int ReadDETABSFile(char *filename, DETABSInfo_t detabsinfo[4]) {
  
  int            status=OK, inExt, outExt;
  char		 extname[FLEN_KEYWORD];
  char		 detnam[FLEN_VALUE];
  BOOL           detabs0=0, detabs1=0, detabs2=0, detabs3=0;
  FitsCard_t     *card;
  Bintable_t     table;                /* Bintable pointer */  
  FitsHeader_t   head;                 /* Extension pointer */
  FitsFileUnit_t inunit=NULL;          /* File pointer */

  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  
  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadDETABSFile_end;
    }
  else
    headas_chat(CHATTY, "%s: Info: Reading '%s' file.\n", global.taskname,filename);
  

  /* Get number of hdus in input file */
  if (fits_get_num_hdus(inunit, &inExt, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadDETABSFile_end;
    }


  outExt=1;
  status=OK;
  while ( status == OK && outExt <= inExt) 
    {
      if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto ReadDETABSFile_end;
	}
      
      /* Retrieve header pointer */    
      head=RetrieveFitsHeader(inunit);    


      if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	strcpy(extname, card->u.SVal);
      else
	strcpy(extname, "NONAME");

      if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	strcpy(detnam, card->u.SVal);
      else
	strcpy(detnam, "-");


      if( !strcmp(extname,KWVL_EXTNAME_DETABS) && !strcmp(detnam,KWVL_DETNAM_DET0) ){
	if(ReadDETABSInfo(inunit, &detabsinfo[0])){
	  headas_chat(NORMAL, "%s: Error: Unable to read HDU %d in '%s' file.\n", global.taskname, outExt, filename);
	  goto ReadDETABSFile_end;
	}
	detabs0=1;
      }
      
      if( !strcmp(extname,KWVL_EXTNAME_DETABS) && !strcmp(detnam,KWVL_DETNAM_DET1) ){
	if(ReadDETABSInfo(inunit, &detabsinfo[1])){
	  headas_chat(NORMAL, "%s: Error: Unable to read HDU %d in '%s' file.\n", global.taskname, outExt, filename);
	  goto ReadDETABSFile_end;
	}
	detabs1=1;
      }

      if( !strcmp(extname,KWVL_EXTNAME_DETABS) &&  !strcmp(detnam,KWVL_DETNAM_DET2) ){
	if(ReadDETABSInfo(inunit, &detabsinfo[2])){
	  headas_chat(NORMAL, "%s: Error: Unable to read HDU %d in '%s' file.\n", global.taskname, outExt, filename);
	  goto ReadDETABSFile_end;
	}
	detabs2=1;
      }

      if( !strcmp(extname,KWVL_EXTNAME_DETABS) &&  !strcmp(detnam,KWVL_DETNAM_DET3) ){
	if(ReadDETABSInfo(inunit, &detabsinfo[3])){
	  headas_chat(NORMAL, "%s: Error: Unable to read HDU %d in '%s' file.\n", global.taskname, outExt, filename);
	  goto ReadDETABSFile_end;
	}
	detabs3=1;
      }
      

      outExt++;
    }
  
    
  /* Close offset file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadDETABSFile_end;
    }
  

  if(!detabs0){
    headas_chat(NORMAL, "%s: Error: Unable to get DETABS info for DET_ID=%d in '%s' file.\n", global.taskname, KWVL_DET_ID_0, filename);
    goto ReadDETABSFile_end;
  }
  if(!detabs1){
    headas_chat(NORMAL, "%s: Error: Unable to get DETABS info for DET_ID=%d in '%s' file.\n", global.taskname, KWVL_DET_ID_1, filename);
    goto ReadDETABSFile_end;
  }
  if(!detabs2){
    headas_chat(NORMAL, "%s: Error: Unable to get DETABS info for DET_ID=%d in '%s' file.\n", global.taskname, KWVL_DET_ID_2, filename);
    goto ReadDETABSFile_end;
  }
  if(!detabs3){
    headas_chat(NORMAL, "%s: Error: Unable to get DETABS info for DET_ID=%d in '%s' file.\n", global.taskname, KWVL_DET_ID_3, filename);
    goto ReadDETABSFile_end;
  }


  return OK;
  
 ReadDETABSFile_end:
  
  return NOT_OK;
  
} /* ReadDETABSFile */


int ReadDETABSInfo(FitsFileUnit_t inunit, DETABSInfo_t *detabs_det)
{
  int            n, count=0;
  unsigned       idetabs;
  unsigned       FromRow, ReadRows, nCols; 
  Bintable_t     table;                /* Bintable pointer */  
  FitsHeader_t   head;                 /* Extension pointer */


  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  head = RetrieveFitsHeader(inunit);
  
  /* Read bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: empty DETABS extension.\n", global.taskname);
      goto ReadDETABSInfo_end;
    }

  /* Get columns index from name */

  if ((idetabs = GetColNameIndx(&table, CLNM_DETABS)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist.\n", global.taskname, CLNM_DETABS);
      goto ReadDETABSInfo_end;
    }


  /* Allocate memory to storage all data */
  detabs_det->nrows = table.MaxRows;
  detabs_det->detabs = (float *)malloc(detabs_det->nrows*sizeof(float));
  if(detabs_det->detabs==NULL){
    headas_chat(CHATTY,"%s: Error: ReadDETABSInfo: memory allocation failure.\n", global.taskname);
    goto ReadDETABSInfo_end;
  }

  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;

  while((count<detabs_det->nrows) && (ReadBintable(inunit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
    {
      for (n=0; n<ReadRows; ++n)
	{
	  detabs_det->detabs[count] = EVEC(table,n,idetabs);

	  count++;
	}

      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }

  detabs_det->nrows = count;


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);

  return OK;
  
 ReadDETABSInfo_end:
  
  return NOT_OK;

} /* ReadDETABSInfo */


int CreateXselXco(char *evtfile, char *gtifile, char *regfile, char *evtfiltered, char *xcofile){
  
  char     BaseName[MAXFNAME_LEN], DirName1[MAXFNAME_LEN], DirName[MAXFNAME_LEN];
  FILE	   *file;
  pid_t    pid;
  
  /* Get pid */
  pid=getpid();
  
  
  /* Get dirname and basename of input evt file */
  SplitFilePath(evtfile, DirName1, BaseName);

  if(DirName1[0]=='/'){
    strcpy(DirName, DirName1);
  }
  else{
    sprintf(DirName, "./%s", DirName1);
  }

  
  if (!(file = fopen(xcofile, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,xcofile);
    goto CreateXselXco_end;
  }
  
  fprintf(file, "xsel%d\n", pid);
  fprintf(file, "read eve %s\n", BaseName);
  fprintf(file, "%s\n", DirName);
  fprintf(file, "\n");
  fprintf(file, "set xyname X Y\n");

  if (strcasecmp (gtifile, DF_NONE)){  /* gtifile != NONE */
    fprintf(file, "filter time file \"%s\"\n", gtifile);
    fprintf(file, "filter pha_cutoff 35 1909\n");
  }

  fprintf(file, "filter region %s\n", regfile);
  fprintf(file, "extract events copyall=yes\n");
  fprintf(file, "save events ./%s\n", evtfiltered);
  fprintf(file, "no\n");
  fprintf(file, "quit\n");
  fprintf(file, "no\n");

  fclose(file);
  
  
  return OK;
  
 CreateXselXco_end:
  return NOT_OK;
  
} /* CreateXselXco */


int ComputeDETABSCorr(DETABSInfo_t detabsinfo[4], char *evtfile, char *phafile, char *regfile, int detabscount, double *detabscorr){

  int         i, status=OK, evtcnt[4], totevt=0;
  char        filtered_evtfile[PIL_LINESIZE];
  char        xsel_infile[PIL_LINESIZE];
  char        cmd[BUF_SIZE];
  pid_t       pid;
  
  /* Get pid */
  pid=getpid();


  if( detabsinfo[0].nrows!=detabscount || detabsinfo[1].nrows!=detabscount ||
      detabsinfo[2].nrows!=detabscount || detabsinfo[3].nrows!=detabscount )
    {
      headas_chat(NORMAL, "%s: Error: DETABS Correction file not appropriate for the requested correction.\n", global.taskname);
      goto ComputeDETABSCorr_end; 
    }


  /* Derive temporary file name */
/*   sprintf(filtered_evtfile, "%s/%d_xsel.evt", global.tmpout.dirname, pid); */
/*   sprintf(xsel_infile, "%s/%d_xsel.xco", global.tmpout.dirname, pid); */
  sprintf(filtered_evtfile, "%dboxreg-xsel.evt", (int)pid);
  sprintf(xsel_infile, "%dboxreg-xsel.xco", (int)pid);


  /* Create Xselect temporary xco input file */
  if(CreateXselXco(evtfile, phafile, regfile, filtered_evtfile, xsel_infile))
    {
      headas_chat(NORMAL, "%s: Error: unable to create xselect temporary input file %s.\n", global.taskname, xsel_infile);
      goto ComputeDETABSCorr_end; 
    }

  
  /* Execute xselect ftool */
  sprintf(cmd, "xselect @%s", xsel_infile);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: unable to create temporary filtered event file '%s'\n", global.taskname, filtered_evtfile);
    goto ComputeDETABSCorr_end;
  }


  /* Compute events count */
  if( GetEvtCnt(filtered_evtfile, evtcnt, &totevt) )
    {
      headas_chat(NORMAL, "%s: Error: unable to compute events count in input file %s.\n", global.taskname, filtered_evtfile);
      goto ComputeDETABSCorr_end; 
    }


  /* Compute correction values */
  for(i=0; i<detabscount; i++) {
    if(totevt!=0)
      detabscorr[i] = (detabsinfo[0].detabs[i]*evtcnt[0]+detabsinfo[1].detabs[i]*evtcnt[1]+detabsinfo[2].detabs[i]*evtcnt[2]+detabsinfo[3].detabs[i]*evtcnt[3])/totevt;
    else
      detabscorr[i] = 1;
  }


  /* Cleanup Temporary Files */

  if ( FileExists(filtered_evtfile) ) {
    if(remove (filtered_evtfile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname, filtered_evtfile);
    }
  }

  if ( FileExists(xsel_infile) ) {
    if(remove (xsel_infile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname, xsel_infile);
    }
  }


  return OK;
  
 ComputeDETABSCorr_end:

  return NOT_OK;

} /* ComputeDETABSCorr */


int GetEvtCnt(char *filename, int evtcnt[4], int *totevt){

  int                i, n, status=OK;
  int                det_id;
  unsigned           idet_id;
  unsigned           FromRow, ReadRows, nCols;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<4; i++)
    evtcnt[i] = 0;

  /* Open read only input file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetEvtCnt_end;
    }
  
  /* Move in events extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto GetEvtCnt_end;
    }

  head = RetrieveFitsHeader(unit);
  
  /* Read Bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {

      /* Free memory allocated with bintable data */
      ReleaseBintable(&head, &table);

      /* Close file */
      if (CloseFitsFile(unit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
	  return NOT_OK;
	}
      
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, filename);
      return OK;
    }


  /* Get columns index */

  if ((idet_id = GetColNameIndx(&table, CLNM_DET_ID)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DET_ID);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto GetEvtCnt_end;
    }


  EndBintableHeader(&head, &table);
  

  /* Total number of events */
  *totevt = table.MaxRows;


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  det_id = BVEC(table,n,idet_id);
	  
	  if(det_id<0||det_id>4){
	    headas_chat(NORMAL,"%s: Warning: DET_ID=%d out of range value.\n", global.taskname, det_id);
	    continue;
	  }

	  evtcnt[det_id]++;

	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);

  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto GetEvtCnt_end;
    }

  
  for(i=0; i<4; i++)
    headas_chat(CHATTY,"%s: Info: DET_ID=%d - event counts=%d\n",global.taskname, i, evtcnt[i]);


  return OK;
  
 GetEvtCnt_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 } /* GetEvtCnt */


int CreateRegionFile(RegBoxInfo_t *boxinfo, char *filename){

  FILE	   *file;

  if (!(file = fopen(filename, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n", global.taskname, filename);
    goto CreateRegionFile_end;
  }
  
  fprintf(file, "BOX(%f,%f,%d,%d,0)\n", boxinfo->cx+0.5, boxinfo->cy+0.5, (boxinfo->x_max-boxinfo->x_min+1), (boxinfo->y_max-boxinfo->y_min+1) );

  fclose(file);
  
  return OK;
  
 CreateRegionFile_end:
  return NOT_OK;
  
} /* CreateRegionFile */


/*
 *
 *      ReadGrpPsfFile
 *
 *	DESCRIPTION:
 *           Routine to get 2D-PSF Grouping File data
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadGrpPsfFile(char *filename, GrpPsfInfo_t *info){

  int                n, count=0, status=OK;
  GrpPsfCol_t        indxcol;
  unsigned           FromRow, ReadRows, nCols;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     gunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Open read only input file */
  if ((gunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadGrpPsfFile_end;
    }

  /* Move in GRP2DPSF extension in input file */
  if (fits_movnam_hdu(gunit, ANY_HDU, KWVL_EXTNAME_GRP2DPSF, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, KWVL_EXTNAME_GRP2DPSF);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
      goto ReadGrpPsfFile_end;
    }

  head = RetrieveFitsHeader(gunit);
  
  /* Read grprmf bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadGrpPsfFile_end;
    }


  /* Get columns index from name */

  if ((indxcol.ENERG_LO = GetColNameIndx(&table, CLNM_ENERG_LO)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENERG_LO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGrpPsfFile_end;
    }

  if ((indxcol.ENERG_HI = GetColNameIndx(&table, CLNM_ENERG_HI)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENERG_HI);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGrpPsfFile_end;
    }

  if ((indxcol.PSFFILE = GetColNameIndx(&table, CLNM_2DPSFFILE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_2DPSFFILE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadGrpPsfFile_end;
    }
  info->psffile_dim = table.Multiplicity[indxcol.PSFFILE];


  EndBintableHeader(&head, &table);
  
  
  /* Allocate memory to storage all data */
  info->nrows = table.MaxRows;
  info->row = (GrpPsfRow_t *)calloc(info->nrows, sizeof(GrpPsfRow_t));
  if(info==NULL){
    headas_chat(CHATTY,"%s: Error: ReadGrpPsfFile: memory allocation failure.\n", global.taskname);
    goto ReadGrpPsfFile_end;
  }

  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while((count<info->nrows) && (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
    {
      for (n=0; n<ReadRows; ++n)
	{
	  info->row[count].energy_lo = EVEC(table,n,indxcol.ENERG_LO);
	  info->row[count].energy_hi = EVEC(table,n,indxcol.ENERG_HI);

	  info->row[count].psffile = (char *)malloc(info->psffile_dim*sizeof(char));
	  strcpy(info->row[count].psffile, SVEC(table,n,indxcol.PSFFILE));

	  count++;
	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);

  info->nrows = count;


  /* Close file */
  if (CloseFitsFile(gunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadGrpPsfFile_end;
    }



  return OK;
  
 ReadGrpPsfFile_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadGrpPsfFile */


int ComputePsfCorrByEnergy(struct gti_struct *gti, char *offaxisfile, double skyx, double skyy, int skysize, PsfCorrVal_t *psfcorr){

  int                status=0, i=0, ie=0, psfbin_dim, this_psfbin_dim;
  /* int                j; */
  double             psfbin, this_psfbin, *psf_frac_off, corr;
  char               cmd[BUF_SIZE], aberration[5], initseed[5];
  char               BaseName[MAXFNAME_LEN], CaldbName[MAXFNAME_LEN], DirName[MAXFNAME_LEN];
  char               psffile[MAXFNAME_LEN];
  char               **expomapfile;
  long               extfile;
  int                gtiext1=-1, gtiext2=-1;
  GrpPsfInfo_t       grppsfinfo;
  OffAxisBinInfo_t   *bininfo;
  pid_t              pid;
  
  /* Get pid */
  pid=getpid();


 if(global.par.aberration)
    sprintf(aberration, "yes");
  else
    sprintf(aberration, "no");

  if(global.par.initseed)
    sprintf(initseed, "yes");
  else
    sprintf(initseed, "no");


  /* Derive CALDB grppsffile name */  
  if ( !strcasecmp(global.par.grppsffile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_GRPPSF_DSET, global.par.grppsffile, HD_EXPR, &extfile, global.obsinfo.instrume, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for grppsffile parameter.\n", global.taskname);
      	  goto ComputePsfCorrByEnergy_end; 
      	}
      extfile++;
    }

  /* Derive CALDB psfdir name */  
  if ( !strcasecmp(global.par.psfdir,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_GRPPSF_DSET, CaldbName, HD_EXPR, &extfile, global.obsinfo.instrume, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for psfdir input parameter.\n", global.taskname);
      	  goto ComputePsfCorrByEnergy_end; 
      	}
      extfile++;

      SplitFilePath(CaldbName, DirName, BaseName);
      strcpy(global.par.psfdir, DirName);
      headas_chat(CHATTY,"Name of the input PSF files directory:'%s'\n",global.par.psfdir);
    }

  
  /* Retrieve 2D-PSF Grouping info from input grppsffile */
  if( ReadGrpPsfFile(global.par.grppsffile, &grppsfinfo) )
    {
      headas_chat(NORMAL, "%s: Error: unable to read 2D-PSF Grouping file.\n", global.taskname);
      goto ComputePsfCorrByEnergy_end; 
    }


  /* Allocate memory to storage correction values */
  psfcorr->n_energy = grppsfinfo.nrows ;
  psfcorr->en = (double*)calloc(psfcorr->n_energy, sizeof(double));
  psfcorr->corr = (double*)calloc(psfcorr->n_energy, sizeof(double));
  if(psfcorr->en==NULL || psfcorr->corr==NULL){
    headas_chat(CHATTY,"%s: Error: ComputePsfCorrByEnergy: memory allocation failure.\n", global.taskname);
    goto ComputePsfCorrByEnergy_end;
  }
  
  
  /* Get bin info from the first input psf file */
  sprintf(psffile, "%s/%s", global.par.psfdir, grppsfinfo.row[0].psffile);
  if(GetPsfBinInfo(psffile, &psfbin_dim, &psfbin)){
    headas_chat(NORMAL, "%s: Error: Unable to read psf file %s\n", global.taskname, psffile);
    goto ComputePsfCorrByEnergy_end; 
  }
  
  /* Compute Off-Axis bin */
  bininfo = (OffAxisBinInfo_t*)calloc(psfbin_dim, psfbin_dim*sizeof(OffAxisBinInfo_t));
  if( ComputeOffAxisBin(gti, offaxisfile, bininfo, psfbin_dim, psfbin) ){
    headas_chat(NORMAL, "%s: Error: Unable to compute Off-Axis bin.\n", global.taskname);
    goto ComputePsfCorrByEnergy_end; 
  }
  
/*   for(i=0; i<psfbin_dim; i++){ */
/*     headas_chat(NORMAL,"DEBUG OFFAXIS=%f DURATION=%f GTICOUNT=%d\n",bininfo[i].off_axis,bininfo[i].duration,bininfo[i].gticount); */
/*     for(j=0; j<bininfo[i].gticount; j++){ */
/*       headas_chat(NORMAL,"DEBUG TSTART=%f TSTOP=%f\n",bininfo[i].gti[j].tstart,bininfo[i].gti[j].tstop); */
/*     } */
/*   } */

    
  psf_frac_off = (double*)calloc(psfbin_dim, sizeof(double));

  expomapfile = (char **)calloc(psfbin_dim, sizeof(char *));
  for(i=0; i<psfbin_dim; i++)
    expomapfile[i]=(char *)malloc((sizeof(char))*MAXFNAME_LEN);


  for(ie=0; ie<psfcorr->n_energy; ie++){
    
    /* Get bin info from the selected input psf file */
    sprintf(psffile, "%s/%s", global.par.psfdir, grppsfinfo.row[ie].psffile);
    if(GetPsfBinInfo(psffile, &this_psfbin_dim, &this_psfbin)){
      headas_chat(NORMAL, "%s: Error: Unable to read psf file %s\n", global.taskname, psffile);
      goto ComputePsfCorrByEnergy_end; 
    }
    
    /* Check that all input psf file have the same bin value and dimension */
    if(this_psfbin_dim!=psfbin_dim || this_psfbin!=psfbin)
      {
	headas_chat(NORMAL, "%s: Error: bad format of psf file '%s'\n", global.taskname, psffile);
	goto ComputePsfCorrByEnergy_end;	
      }
    
    for(i=0; i<psfbin_dim; i++){
      
      if(bininfo[i].duration<=0)
	continue;

      /* For each offaxis bin, create the exposure map only for the first energy loop */
      /* and reuse it for the other energy values */
      if(ie==0)
	{

	  /* Create temporary bingti file */
	  sprintf(global.tmpout.bingti, "%dtmp_bingti%d.fits", (int)pid, i);
	  if(WriteBinGTI(global.tmpout.bingti, bininfo[i].gti, bininfo[i].gticount)){
	    headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.bingti);
	    goto ComputePsfCorrByEnergy_end;
	  }


	  /* Create DET1 Reference Point filtered file */
	  sprintf(global.tmpout.det1refgti, "%dtmp_det1refgti%d.fits", (int)pid, i);

	  if(CreateFilteredDet1RefFile(global.par.det1reffile, global.tmpout.bingti, global.tmpout.det1refgti))
	    goto ComputePsfCorrByEnergy_end;

	  
	  /* Get GTI extension number from event and bingti files */
      
	  if(GetHDUExtNum(global.par.infile, KWVL_EXTNAME_GTI, &gtiext1))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to find  '%s' extension\n", global.taskname, KWVL_EXTNAME_GTI);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
	      goto ComputePsfCorrByEnergy_end;
	    }
      
	  if(GetHDUExtNum(global.tmpout.bingti, KWVL_EXTNAME_GTI, &gtiext2))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to find  '%s' extension\n", global.taskname, KWVL_EXTNAME_GTI);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.tmpout.bingti);
	      goto ComputePsfCorrByEnergy_end;
	    }
      
	  /* Create temporary mggti file */
	  sprintf(global.tmpout.mggti, "%dtmp_mggti%d.fits", (int)pid, i);
	  sprintf(cmd, "mgtime ingtis=\"%s[%d],%s[%d]\" outgti=%s merge=AND", global.tmpout.loc_infile, gtiext1-1, global.tmpout.bingti, gtiext2-1, global.tmpout.mggti);
	  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
	  
	  fflush(stdout);
	  status = system(cmd);
	  if(status!=0){
	    headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.mggti);
	    goto ComputePsfCorrByEnergy_end;
	  }
      
	  /* Create temporary event file */
	  sprintf(global.tmpout.evtfile, "%dtmp_%d.evt", (int)pid, i);
	  if(CreateTemporaryEvt(global.par.infile, global.tmpout.mggti, "STDGTI", global.tmpout.evtfile))
	    {
	      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.evtfile);
	      goto ComputePsfCorrByEnergy_end;
	    }
      
	  /* Execute expomap task */
	  sprintf(expomapfile[i], "%dtmp_expo%d.fits", (int)pid, i);
	  sprintf(global.tmpout.aspecthisto, "%dtmp_asphisto%d.fits", (int)pid, i);
	  
	  sprintf( cmd, "nuexpomap infile=%s pixposfile=%s alignfile=%s mastaspectfile=%s attfile=%s teldef=%s instrprobmapfile=%s aberration=%s det1reffile=%s pixbin=%d expomapfile=%s aspecthistofile=%s offsetfile=NONE det1instrfile=NONE det2instrfile=NONE skyinstrfile=NONE vignflag=no skyx=%f skyy=%f skysize=%d initseed=%s  percent=%f indet2instrfile=NONE",
		   global.tmpout.evtfile, global.par.pixposfile, global.par.alignfile, global.par.mastaspectfile, global.par.attfile, global.par.teldef,
		   global.par.instrprobmapfile, aberration, global.tmpout.det1refgti, global.par.pixbin, expomapfile[i], global.tmpout.aspecthisto,
		   skyx, skyy, skysize, initseed, global.par.percent );
	  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
	  
	  fflush(stdout);
	  status = system(cmd);
	  if(status!=0){
	    headas_chat(NORMAL, "%s: Error: unable to create temporary Exposure Map file\n", global.taskname);
	    goto ComputePsfCorrByEnergy_end;
	  }

      
	  /* Cleanup Temporary Files */
	  
	  if ( FileExists(global.tmpout.bingti ) ) {
	    if(remove (global.tmpout.bingti) == -1){
	      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.bingti);
	    }
	  }
	  
	  if ( FileExists(global.tmpout.det1refgti ) ) {
	    if(remove (global.tmpout.det1refgti) == -1){
	      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.det1refgti);
	    }
	  }
	  
	  if ( FileExists(global.tmpout.mggti ) ) {
	    if(remove (global.tmpout.mggti) == -1){
	      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.mggti);
	    }
	  }
	  
	  if ( FileExists(global.tmpout.evtfile ) ) {
	    if(remove (global.tmpout.evtfile) == -1){
	      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.evtfile);
	    }
	  }
	  
	  if ( FileExists(global.tmpout.aspecthisto ) ) {
	    if(remove (global.tmpout.aspecthisto) == -1){
	      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.aspecthisto);
	    }
	  }
	  
	} /* End -> if(ie==0) */
      
      
      if( ComputePsfFracOff(expomapfile[i], psffile, i, bininfo[i].phi_median, &global.reginfo, &psf_frac_off[i]) ){
	headas_chat(NORMAL, "%s: Error: unable to compute psf correction for Off-Axis bin %d\n", global.taskname, i);
	goto ComputePsfCorrByEnergy_end;
      }
      
      headas_chat(NORMAL,"%s: Info: PSF fraction=%f off-axis=%f duration=%f\n", global.taskname, psf_frac_off[i], bininfo[i].off_axis, bininfo[i].duration);
      
    } /* for(i=0; i<psfbin_dim; i++) */


    /* Compute Psf correction coefficient */
  ComputePsfCorr(bininfo, psf_frac_off, psfbin_dim, &corr);
  headas_chat(NORMAL,"%s: Info: energy band %.1f - %.1f (keV): total PSF correction factor=%f\n", global.taskname, grppsfinfo.row[ie].energy_lo, grppsfinfo.row[ie].energy_hi, corr );
  
  psfcorr->en[ie] = (grppsfinfo.row[ie].energy_lo + grppsfinfo.row[ie].energy_hi)/2;
  psfcorr->corr[ie] = corr;


  } /* for(ie=0; ie<psfcorr->n_energy; ie++) */
   

  /* Free memory */

  free(psf_frac_off);
  free(bininfo);


  /* Free memory and temporary expomap files */
  for(i=0; i<psfbin_dim; i++){
  
    if ( FileExists(expomapfile[i] ) ) {
      if(remove (expomapfile[i]) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,expomapfile[i]);
      }
    }
    
    free(expomapfile[i]);
  }

  free(expomapfile);


  return OK;
    
 ComputePsfCorrByEnergy_end:
  return NOT_OK;
    
} /* ComputePsfCorrByEnergy */


int GetPsfCorr(PsfCorrVal_t *psfcorr, double en, double *corr){

  int      i,j;
  double   a, b, e1, e2, c1, c2;
  double   enbin=0.;


  if(psfcorr->n_energy<=1)
    {
      headas_chat(NORMAL,"%s: Warning: not enough energy values, setting PSF correction coefficient to 1.\n", global.taskname);
      *corr = 1.;
      return NOT_OK;
    }

  
  if(psfcorr->n_energy>1)
    enbin = psfcorr->en[1] - psfcorr->en[0];


  FindClosestPsfCorrIndex(psfcorr, en, &i);


  if(i==0 && en<psfcorr->en[0]-enbin/2)
    {
      e1 = psfcorr->en[0];
      e2 = psfcorr->en[1];
      c1 = psfcorr->corr[0];
      c2 = psfcorr->corr[1];

      a = (c2-c1)/(e2-e1);
      b = 0.5*(c1+c2) - 0.5*a*(e1+e2);

      *corr = en * a + b;
    }
  else if(i==psfcorr->n_energy-1 && en>(psfcorr->en[i]+enbin/2))
    {
      e1 = psfcorr->en[psfcorr->n_energy-2];
      e2 = psfcorr->en[psfcorr->n_energy-1];
      c1 = psfcorr->corr[psfcorr->n_energy-2];
      c2 = psfcorr->corr[psfcorr->n_energy-1];

      a = (c2-c1)/(e2-e1);
      b = 0.5*(c1+c2) - 0.5*a*(e1+e2);

      *corr = en * a + b;
    }
  else
    {
      j = (i+1<psfcorr->n_energy) ? i+1 : i ;

      InterpolateValues(psfcorr->en[i], psfcorr->en[j], en, psfcorr->corr[i], psfcorr->corr[j], corr);
    }


  return OK;

} /* GetPsfCorr */


void FindClosestPsfCorrIndex(PsfCorrVal_t *psfcorr, double value, int *index){
  
  int      i=0, low=0, high=0, mid;
  
  /* Find appropriate row ( after loop index 'i' indicates the first row over 'value' value) */
  low = 0;
  high = psfcorr->n_energy-1;
  
  while (low != high) {
    mid = low + (high-low)/2;
    if (psfcorr->en[mid] <= value) {
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

  /* index should contains nearest row under 'value' value */
  if( (i>0) && (psfcorr->en[i]>value) ){
    i--;
  }

  *index = i;
   
} /* FindClosestPsfCorrIndex */


int CreateFilteredOptAxisFile(char *infile, char *phafile, char *outfile){

  int                status=OK;
  char               zipfile[MAXFNAME_LEN], ext[MAXEXT_LEN], cmd[BUF_SIZE];


  GetFilenameExtension(infile, ext);
  if ( !(strcmp(ext, "gz")) )
    {
      sprintf(zipfile, "%s.gz", outfile);

      if(CopyFile(infile, zipfile))
	{
	  headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, zipfile);
	  goto CreateFilteredOptAxisFile_end;
	}

      /* Uncompress input file */
      sprintf(cmd,"gunzip -fq %s", zipfile);
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to uncompress temporary file '%s'\n", global.taskname, zipfile);
	goto CreateFilteredOptAxisFile_end;
      }
    }
  else
    {
      if(CopyFile(infile, outfile))
	{
	  headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, outfile);
	  goto CreateFilteredOptAxisFile_end;
	}
    }


  if(FilterTimesByGTI(outfile, KWVL_EXTNAME_OPTICAL_AXIS, phafile, KWVL_EXTNAME_GTI))
    {
      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, outfile);
      goto CreateFilteredOptAxisFile_end;
    }


  return OK;

 CreateFilteredOptAxisFile_end:
  return NOT_OK;

} /* CreateFilteredOptAxisFile */


int CreateFilteredDet1RefFile(char *infile, char *gtifile, char *outfile){

  int                status=OK;
  char               zipfile[MAXFNAME_LEN], ext[MAXEXT_LEN], cmd[BUF_SIZE];


  GetFilenameExtension(infile, ext);
  if ( !(strcmp(ext, "gz")) )
    {
      sprintf(zipfile, "%s.gz", outfile);

      if(CopyFile(infile, zipfile))
	{
	  headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, zipfile);
	  goto CreateFilteredDet1RefFile_end;
	}

      /* Uncompress input file */
      sprintf(cmd,"gunzip -fq %s", zipfile);
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to uncompress temporary file '%s'\n", global.taskname, zipfile);
	goto CreateFilteredDet1RefFile_end;
      }
    }
  else
    {
      if(CopyFile(infile, outfile))
	{
	  headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, outfile);
	  goto CreateFilteredDet1RefFile_end;
	}
    }


  if(FilterTimesByGTI(outfile, KWVL_EXTNAME_DET1_REFPOINT, gtifile, KWVL_EXTNAME_GTI))
    {
      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, outfile);
      goto CreateFilteredDet1RefFile_end;
    }


  return OK;

 CreateFilteredDet1RefFile_end:
  return NOT_OK;

} /* CreateFilteredDet1RefFile */


int FilterPhaFile(char *evtfile, char *phafile, char *regfile, int pilowarf, int pihigharf, char *phafiltered){

  char     BaseName[MAXFNAME_LEN], DirName1[MAXFNAME_LEN], DirName[MAXFNAME_LEN];
  char     cmd[BUF_SIZE];
  int      status=0;
  char     xcofile[PIL_LINESIZE];
  FILE	   *file;
  pid_t    pid;
  
  /* Get pid */
  pid=getpid();


  /* Derive temporary *.xco command file  */
  sprintf(xcofile, "%d_pha-xsel.xco", (int)pid);

  
  /* Get dirname and basename of input evt file */
  SplitFilePath(evtfile, DirName1, BaseName);

  if(DirName1[0]=='/'){
    strcpy(DirName, DirName1);
  }
  else{
    sprintf(DirName, "./%s", DirName1);
  }

  
  if (!(file = fopen(xcofile, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,xcofile);
    goto FilterPhaFile_end;
  }
  
  fprintf(file, "xsel%d\n", pid);
  fprintf(file, "read eve %s\n", BaseName);
  fprintf(file, "%s\n", DirName);
  fprintf(file, "\n");
  fprintf(file, "set xyname X Y\n");
  fprintf(file, "filter time file \"%s\"\n", phafile);
  fprintf(file, "filter pha_cutoff %d %d\n", pilowarf, pihigharf);
  fprintf(file, "filter region %s\n", regfile);
  fprintf(file, "extract spectrum\n");
  fprintf(file, "save spectrum ./%s\n", phafiltered);
  fprintf(file, "quit\n");
  fprintf(file, "no\n");

  fclose(file);


  /* Execute xselect */
  sprintf(cmd, "xselect @%s", xcofile);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: unable to create temporary filtered pha file '%s'\n", global.taskname, phafiltered);
    goto FilterPhaFile_end;
  }

  if(FileExists(xcofile)){
    if(remove (xcofile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,xcofile);
    }
  }
  
  
  return OK;
  
 FilterPhaFile_end:
  return NOT_OK;

} /* FilterPhaFile */
