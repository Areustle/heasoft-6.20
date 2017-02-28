/*
 * 
 *	nuexpomap.c
 *
 *	INVOCATION:
 *
 *		nuexpomap [parameter=value ...]
 *
 *	DESCRIPTION:
 *              
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 18/01/12 - First version
 *        0.1.1 - NS 16/02/12 - Update exposure map using 'DEADC' keyword value 
 *        0.1.2 - NS 24/02/12 - Handle CALDB query for 'instrmapfile' input parameter
 *                            - Added NONE option for 'aspecthistofile' input parameter
 *                            - Update 'BUNIT' keyword in output instrument map files
 *                            - Apply vignetting correction in DET2 instrument map file
 *        0.1.3 - NS 27/04/12 - Handle bad pixels while creating output DET1 Instrument Map File
 *        0.1.4 - NS 14/05/12 - Added consistence check of input files
 *                            - Handle azimuth angle in vignetting correction
 *        0.1.5 - GF 05/07/12 - Handle offset-file ith just one row
 *        0.1.6 - NS 24/07/12 - Replaced 'fltime' ftool call with 'FilterTimesByGTI' routine
 *        0.1.7 - NS 29/08/12 - Handle long file naming
 *        0.1.8 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.1.9 - NS 04/12/12 - Added 'skyx', 'skyy' and 'skysize' input parameters
 *        0.2.0 - NS 25/02/13 - Modified 'phi' computation in vignetting correction
 *        0.2.1 - NS 14/06/13 - Handle memory allocation failure
 *        0.2.2 - NS 10/07/13 - Modified algorithm to handle bad pixels while creating output DET1 Instrument Map File
 *        0.2.3 - NS 05/03/14 - Added 'indet2instrfile' input parameter
 *                            - Improved performances while executing 'fchecksum' of output sky instrument map file 
 *                            - Handle compressed DET1 Reference Pixel input file
 *        0.2.4 - RF 14/04/16 - Update the checksum of the optional output file 'offsetfile' and 'aspecthistofile' 
 *        
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nuexpomap  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nuexpomap.h"


/********************************/
/*         definitions          */
/********************************/

#define NUEXPOMAP_C
#define NUEXPOMAP_VERSION      "0.2.4"
#define PRG_NAME               "nuexpomap"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nuexpomap_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 nuexpomap.par    
 *         
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nuexpomap_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 18/01/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuexpomap_getpar()
{
  
  /* Input Event File Name */
  if(PILGetFname(PAR_INFILE, global.par.infile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
      goto Error;
    }

  if(PILGetFname(PAR_INDET2INSTRFILE, global.par.indet2instrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INDET2INSTRFILE);
      goto Error;	
    }
  if (!(strcasecmp (global.par.indet2instrfile, DF_NONE)))
    global.getdet2instrfile = FALSE;
  else
    global.getdet2instrfile = TRUE;


  if(!global.getdet2instrfile){

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
    if(PILGetFname(PAR_OFFSETFILE, global.par.offsetfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OFFSETFILE);
	goto Error;	
      }
    if (!(strcasecmp (global.par.offsetfile, DF_NONE)))
      global.createoffsetfile = FALSE;
    else
      global.createoffsetfile = TRUE;


    if(PILGetFname(PAR_ASPECTHISTOFILE, global.par.aspecthistofile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ASPECTHISTOFILE);
	goto Error;	
      }
    if (!(strcasecmp (global.par.aspecthistofile, DF_NONE)))
      global.createaspecthistofile = FALSE;
    else
      global.createaspecthistofile = TRUE;


    if(PILGetFname(PAR_DET1INSTRFILE, global.par.det1instrfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DET1INSTRFILE);
	goto Error;	
      }
    if (!(strcasecmp (global.par.det1instrfile, DF_NONE)))
      global.createdet1instrfile = FALSE;
    else
      global.createdet1instrfile = TRUE;


    if(PILGetFname(PAR_DET2INSTRFILE, global.par.det2instrfile)) 
      {
	headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_DET2INSTRFILE);
	goto Error;	
      }
    if (!(strcasecmp (global.par.det2instrfile, DF_NONE)))
      global.createdet2instrfile = FALSE;
    else
      global.createdet2instrfile = TRUE;

  }
  else{
    
    strcpy(global.par.offsetfile, DF_NONE);
    global.createoffsetfile = FALSE;

    strcpy(global.par.aspecthistofile, DF_NONE);
    global.createaspecthistofile = FALSE;

    strcpy(global.par.det1instrfile, DF_NONE);
    global.createdet1instrfile = FALSE;

    strcpy(global.par.det2instrfile, DF_NONE);
    global.createdet2instrfile = FALSE;

  }


  if(PILGetFname(PAR_ALIGNFILE, global.par.alignfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ALIGNFILE);
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

  if(PILGetBool(PAR_ABERRATION, &global.par.aberration))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ABERRATION);
      goto Error;	
    }

  if(PILGetFname(PAR_VIGNFILE, global.par.vignfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_VIGNFILE);
      goto Error;	
    }

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
  
  if(PILGetReal(PAR_SKYX, &global.par.skyx))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SKYX); 
      goto Error;	 
    }

  if(PILGetReal(PAR_SKYY, &global.par.skyy))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SKYY); 
      goto Error;	 
    }

  if(PILGetInt(PAR_SKYSIZE, &global.par.skysize))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SKYSIZE); 
      goto Error;	 
    }

  if(PILGetInt(PAR_PIXBIN, &global.par.pixbin))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PIXBIN); 
      goto Error;	 
    }

/*   if(PILGetString(PAR_STEMOUT, global.par.stemout))  */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_STEMOUT); */
/*       goto Error;	 */
/*     } */


  if(PILGetFname(PAR_SKYINSTRFILE, global.par.skyinstrfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SKYINSTRFILE);
      goto Error;	
    }
  if (!(strcasecmp (global.par.skyinstrfile, DF_NONE)))
    global.createskyinstrfile = FALSE;
  else
    global.createskyinstrfile = TRUE;


  if(PILGetFname(PAR_EXPOMAPFILE, global.par.expomapfile)) 
    {
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EXPOMAPFILE);
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


  get_history(&global.hist);
  nuexpomap_info();
  
  return OK;
  
 Error:
  return NOT_OK;
  
} /* nuexpomap_getpar */


/*
 *	nuexpomap_work
 *
 *
 *	DESCRIPTION:
 *     
 *       
 *
 *      FUNCTION CALL:
 *             int nuexpomap_checkinput();
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
 *        0.1.0 - NS 18/01/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuexpomap_work()
{
  int                status=OK, i=0, iskydet=0;
  int                det1xx=0, det1yy=0, det2xx=0, det2yy=0, det2size=600, skysize=1000;
  int                xmin_sky=0, ymin_sky=0;
  char               cmd[BUF_SIZE], aberration[5], initseed[5];
  char               BaseName[MAXFNAME_LEN], DirName[MAXFNAME_LEN];
  long               extfile=-1;
  OffsetInfo_t       *offinfo=NULL;
  int                offcount=0;
  AspHistoCompInfo_t *histocmpinfo=NULL;
  int                histocmpcount=0;
  AlignInfo_t        aligninfo;
  MastInfo_t         mastinfo;
  SkyDetInfo_t       *skydetinfo=NULL;
  int                skydetcount=0;
  Det1InstrMapInfo_t *det1info=NULL;
  Det2InstrMapInfo_t *det2info=NULL;
  ExposureInfo_t     expoinfo;
  VignInfo_t         vigninfo;
  static float       instrmap[4][DET1_ROWS][DET1_PIXS];
  pid_t              pid;
  FitsFileUnit_t     outunit=NULL;
  
  /* Get pid */
  pid=getpid();


  if(nuexpomap_checkinput())
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

  /* Get Observation Info from input file */
  if( GetObsInfo(global.par.infile, KWVL_EXTNAME_EVT, &global.obsinfo) ){
    headas_chat(NORMAL, "%s: Error: Unable to get info from input '%s' file.\n", global.taskname, global.par.infile);
    goto Error;
  }

  /* Check if attfile and infile are consistent */
  if( CheckAttitude(global.par.attfile) ){
    goto Error;
  }

  /* Check if det1ref and infile are consistent */
  if( !global.getdet2instrfile ){
    if( CheckDet1Ref(global.par.det1reffile) ){
      goto Error;
    }
  }

  /* Derive CALDB teldef filename */  
  if ( !strcasecmp(global.par.teldef,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_TELDEF_DSET, global.par.teldef, HD_EXPR, &extfile, global.obsinfo.instrume, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for teldef parameter.\n", global.taskname);
      	  goto Error;
      	}
      extfile++;
    }


  /* Get Alignment data from input alignfile */
  if(ReadAlignFile(&aligninfo)){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, global.par.alignfile);
    goto Error;
  }


  if( !global.getdet2instrfile ){

  /* Create DET1 Reference Point filtered file */
  if(CreateFilteredDet1RefFile(global.par.det1reffile, global.par.infile, global.tmpout.det1refgti))
    goto Error;


  /* Compute Offset values */
  if(ComputeOffsets(global.tmpout.det1refgti, &offinfo, &offcount)){
    headas_chat(NORMAL, "%s: Error: unable to compute offset values.\n", global.taskname);
    goto Error; 
  }


  /* Write output Offset File */
  if(global.createoffsetfile)
  {

    if(WriteOffsetFile(offinfo, offcount, global.tmpout.det1refgti, global.par.offsetfile)){
      headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.par.offsetfile);
      goto Error;  
    }

    /* Copy GTI extension from infile */
    if( CopyHDUExt(global.par.infile, global.par.offsetfile, KWVL_EXTNAME_GTI) ){
      headas_chat(NORMAL, "%s: Error: Unable to copy %s extension from '%s' to '%s' file.\n", global.taskname, KWVL_EXTNAME_GTI, global.par.infile, global.par.offsetfile);
      goto Error;
    }

    /* Open read/write file */
    if ((outunit=OpenReadWriteFitsFile(global.par.offsetfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.offsetfile);
      goto Error;
    }
      
    /* Update checksum and datasum keywords */
    if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, global.par.offsetfile);
      goto Error;
    }
      
    /* Close file */
    if (CloseFitsFile(outunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.offsetfile);
      goto Error;
    }  
      
    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.offsetfile);
  }

  /* Write output Aspect Histogram File */
  if(WriteAspectHistoFile(offinfo, offcount, global.tmpout.det1refgti, global.par.aspecthistofile)){
    headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.par.aspecthistofile);
    goto Error;
  }

  /* Copy GTI extension from infile */
  if( CopyHDUExt(global.tmpout.det1refgti, global.par.aspecthistofile, KWVL_EXTNAME_GTI) ){
    headas_chat(NORMAL,"%s: Error: Unable to copy %s extension from '%s' to '%s' file.\n",global.taskname,KWVL_EXTNAME_GTI,global.tmpout.det1refgti,global.par.aspecthistofile);
    goto Error;
  }
    
  if(global.createaspecthistofile)
  {
    /* Open read/write file */
    if ((outunit=OpenReadWriteFitsFile(global.par.aspecthistofile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.aspecthistofile);
      goto Error;
    }
      
    /* Update checksum and datasum keywords */
    if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, global.par.aspecthistofile);
      goto Error;
    }
      
    /* Close file */
    if (CloseFitsFile(outunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.aspecthistofile);
      goto Error;
    }  

    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.aspecthistofile);
  }


  /* Read Aspect Histogram info from compressed ext */
  if( ReadAspectHistoCompExt(global.par.aspecthistofile, &histocmpinfo, &histocmpcount) ){
    headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, global.par.aspecthistofile);
    goto Error;
  }


  /* Read Instrument Probability Map File */
  if( ReadInstrProbMapFile(&global.obsinfo, instrmap) ){
    headas_chat(NORMAL, "%s: Error: Unable to read instrument probability map file.\n", global.taskname);
    goto Error;
  }


  /* Compute detector coordinates from input sky coordinates */
  if(global.par.skysize<SKY_MAX){
    
    sprintf(global.tmpout.skytodetfile, "%dtmp_skytodet.fits", (int)pid);
    
    sprintf(cmd, "nuskytodet pntra=%f pntdec=%f attfile=%s instrument=%s skydetfile=%s skyxref=%f skyyref=%f mastaspectfile=%s alignfile=%s teldef=%s aberration=%s initseed=%s", global.obsinfo.ranom, global.obsinfo.decnom, global.par.attfile, global.obsinfo.instrume, global.tmpout.skytodetfile, global.par.skyx, global.par.skyy, global.par.mastaspectfile, global.par.alignfile, global.par.teldef, aberration, initseed);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: unable to create temporary sky reference pixel file '%s'\n", global.taskname, global.tmpout.skytodetfile);
      goto Error;
    }
    
    /* Read temporary sky reference pixel file */
    if( ReadSkyDetFile(global.tmpout.skytodetfile, &skydetinfo, &skydetcount) ){
      headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, global.tmpout.skytodetfile);
      goto Error;
    }
    
    if ( FileExists(global.tmpout.skytodetfile) ) {
      if(remove (global.tmpout.skytodetfile) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.skytodetfile);
      }
    }
    
  }

  /* Allocate memory to store DET1 instrument map info */
  det1info = (Det1InstrMapInfo_t*)malloc(sizeof(Det1InstrMapInfo_t)*histocmpcount);
  if(det1info==NULL){
    headas_chat(CHATTY,"%s: Error: memory allocation failure.\n", global.taskname);
    goto Error;
  }

  for(i=0; i<histocmpcount; i++){

    if(global.par.skysize<SKY_MAX)
      {
	FindClosestSkyDetIndex(skydetinfo, skydetcount, histocmpinfo[i].ref_time, &iskydet);
	
	det1info[i].det1size = (int)(sqrt(2)*global.par.skysize + 20 + 0.5);

	if( skydetinfo[iskydet].det1x==KWVL_DET1NULL || skydetinfo[iskydet].det1y==KWVL_DET1NULL ){
	  
	  det1info[i].isgood = FALSE;
	  
	  det1info[i].det1x = -det1info[i].det1size-1;
	  det1info[i].det1y = -det1info[i].det1size-1;
	  
	}
	else{
	  det1info[i].isgood = TRUE;
	  
	  det1info[i].det1x = (int)(skydetinfo[iskydet].det1x - det1info[i].det1size/2 + 0.5 );
	  /* det1info[i].det1x = det1info[i].det1x<DET1X_MIN ? DET1X_MIN : det1info[i].det1x; */
	  /* det1info[i].det1x = det1info[i].det1x>DET1X_MAX ? DET1X_MAX : det1info[i].det1x; */
	  
	  det1info[i].det1y = (int)(skydetinfo[iskydet].det1y - det1info[i].det1size/2 + 0.5 );
	  /* det1info[i].det1y = det1info[i].det1y<DET1Y_MIN ? DET1Y_MIN : det1info[i].det1y; */
	  /* det1info[i].det1y = det1info[i].det1y>DET1Y_MAX ? DET1Y_MAX : det1info[i].det1y; */
	}

      }
    else
      {
	det1info[i].isgood = TRUE;
	det1info[i].det1size = 360;
	det1info[i].det1x = 1;
	det1info[i].det1y = 1;
      }
  }


  /* Write output DET1 Instrument Map File */
  if( WriteDET1InstrFile(instrmap, histocmpinfo, histocmpcount, global.par.aspecthistofile, global.par.infile, &global.obsinfo, det1info, global.par.det1instrfile) ){
    headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.par.det1instrfile);
    goto Error;
  }

  if(UpdateInstrMapKeys(global.par.det1instrfile)){
    headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.det1instrfile);
    goto Error;
  }


  if(global.createdet1instrfile)
    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.det1instrfile);
 
  
  /* Write output DET2 Instrument Map File */

  /* copy the primary header from DET1 intrument map file */
  sprintf(cmd, "ftcopy infile=%s+0 outfile=%s copyall=no chatter=1 history=no clobber=no", global.par.det1instrfile, global.par.det2instrfile);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: unable to create DET2 Instrument Map File '%s'\n", global.taskname, global.par.det2instrfile);
    goto Error;
  }


  /* Allocate memory to store DET2 instrument map info */
  det2info = (Det2InstrMapInfo_t*)malloc(sizeof(Det2InstrMapInfo_t)*histocmpcount);
  if(det2info==NULL){
    headas_chat(CHATTY,"%s: Error: memory allocation failure.\n", global.taskname);
    goto Error;
  }


  for(i=0; i<histocmpcount; i++){
    
    if( GetMastInfo(global.par.mastaspectfile, histocmpinfo[i].ref_time, &mastinfo) ){
      headas_chat(NORMAL, "%s: Error: Unable to get mast aspect data for img %d.\n", global.taskname, i+1);
      goto Error;
    }

    /* Derive temporary combinexform input file name */
    sprintf(global.tmpout.combinexform_in, "%dtmp_trasform%d.txt", (int)pid, i);

    if(WriteCombineXformInFile(global.tmpout.combinexform_in, &mastinfo, &aligninfo)){
      headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.tmpout.combinexform_in);
      goto Error; 
    }

    /* Derive temporary combinexform output file name */
    sprintf(global.tmpout.combinexform_out, "%dtmp_trasform%d.xform", (int)pid, i);


    if(global.par.skysize<SKY_MAX){
      
      /* Define DET2 size */
      det2size = det1info[i].det1size + 20;

      det1xx = det1info[i].det1x-1 ;
      det1yy = det1info[i].det1y-1 ;


      /* Execute combinexform */
      sprintf(cmd, "combinexform command=@%s outfile=%s", global.tmpout.combinexform_in, global.tmpout.combinexform_out);
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.combinexform_out);
	goto Error;
      }

      /* Derive temporary applyxform input and output file name */
      sprintf(global.tmpout.applyxform_in, "%dtmp_from_det1_%d.txt", (int)pid, i);
      sprintf(global.tmpout.applyxform_out, "%dtmp_to_det2_%d.txt", (int)pid, i);

      if(WriteDet1ToDet2ApplyXformInFile(global.tmpout.applyxform_in, det1xx, det1yy)){
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.tmpout.applyxform_in);
	goto Error; 
      }

      /* Execute applyxform */
      sprintf(cmd, "applyxform infile=%s transform=%s outfile=%s", global.tmpout.applyxform_in, global.tmpout.combinexform_out, global.tmpout.applyxform_out);
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.applyxform_out);
	goto Error;
      }

      if(ReadDet1ToDet2ApplyXformOutFile(global.tmpout.applyxform_out, &det2xx, &det2yy)){
	headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, global.tmpout.applyxform_out);
	goto Error; 
      }
      
      /* Update combinexform command file  */
      if(UpdateCombineXformInFile(global.tmpout.combinexform_in, det1xx, det1yy, det2xx, det2yy)){
	headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.tmpout.combinexform_in);
	goto Error; 
      }

      if ( FileExists(global.tmpout.combinexform_out) ) {
	if(remove (global.tmpout.combinexform_out) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.combinexform_out);
	}
      }

      if ( FileExists(global.tmpout.applyxform_in) ) {
	if(remove (global.tmpout.applyxform_in) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.applyxform_in);
	}
      }
      
      if ( FileExists(global.tmpout.applyxform_out) ) {
	if(remove (global.tmpout.applyxform_out) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.applyxform_out);
	}
      }

    }
    else{
      /* Define DET2 size */
      det2size = 600;

      det1xx = 0;
      det1yy = 0;
      det2xx = 0;
      det2yy = 0;
    }

    /* Execute combinexform with updated command file */
    sprintf(global.tmpout.combinexform_out, "%dtmp_trasform_upd%d.xform", (int)pid, i);
    sprintf(cmd, "combinexform command=@%s outfile=%s", global.tmpout.combinexform_in, global.tmpout.combinexform_out);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.combinexform_out);
      goto Error;
    }

    /* Execute imagetrans */
    sprintf(cmd, "imagetrans infile=%s[%d] outfile=%s transform=%s inverse=none method=AREA dimenx=%d dimeny=%d seed=0 bitpix=-32 zeronull=no history=yes", 
	    global.par.det1instrfile, i+1, global.par.det2instrfile, global.tmpout.combinexform_out, det2size, det2size );
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: Unable to create '%s' file.\n", global.taskname, global.par.det2instrfile);
      goto Error;
    }

    det2info[i].crval1 = -det2xx;
    det2info[i].crval2 = -det2yy;
    det2info[i].naxis1 = det2size;
    det2info[i].naxis2 = det2size;

    /* note: i+2 because ext number start from 1 and we need to skip the primary extension */
    if(UpdateDet2InstrMapKeys(global.par.det2instrfile, i+2, &det2info[i])){
      headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.det2instrfile);
      goto Error;
    }


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

  } /* for(i=0; i<histocmpcount; i++) */


  /* Apply Vignetting corretion to DET2 Instrument Map File */
  if (global.par.vignflag)
    {
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

      /* Retrieve Vignetting info from input vignfile*/
      if( ReadVignFile(global.par.vignfile, &vigninfo) )
	{
	  headas_chat(NORMAL, "%s: Error: unable to read Vignetting file.\n", global.taskname);
	  goto Error;
	}

      /* Apply Vignetting Correction */
      if(ApplyVignetting(global.par.det2instrfile, &vigninfo, &aligninfo))
	{
	  headas_chat(NORMAL, "%s: Error: unable to apply Vignetting correction to '%s' file.\n", global.taskname, global.par.det2instrfile);
	  goto Error;
	}
    }

  /* Update vignetting keywords */
  if(UpdateVignettingKeys(global.par.det2instrfile)){
    headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.det2instrfile);
    goto Error;
  }

  if(global.createdet2instrfile)
    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.det2instrfile);
  
  /* End -> Write output DET2 Instrument Map File */

  }
  else{

    if(global.par.skysize<SKY_MAX)
      {
	headas_chat(NORMAL, "%s: Error: Input parameter skysize is < 1000\n", global.taskname);
	goto Error;
      }

    /* Get needed info from input indet2instrfile file and     */
    /* set variables used also in case 'indet2instrfile=NONE'  */
    /* [ histocmpinfo histocmpcount det1info ]                 */

    if( GetInfoFromDet2InstrMap(global.par.indet2instrfile, &histocmpinfo, &histocmpcount) )
      {
	goto Error;
      }

    /* Allocate memory to store DET1 instrument map info */
    det1info = (Det1InstrMapInfo_t*)malloc(sizeof(Det1InstrMapInfo_t)*histocmpcount);
    if(det1info==NULL){
      headas_chat(CHATTY,"%s: Error: memory allocation failure.\n", global.taskname);
      goto Error;
    }
    for(i=0; i<histocmpcount; i++){
      det1info[i].isgood = TRUE;
      det1info[i].det1size = 360;
      det1info[i].det1x = 1;
      det1info[i].det1y = 1;
    }


    /* Create corrected DET2 Instrument Map file */
    if(CopyFile(global.par.indet2instrfile, global.par.det2instrfile))
      {
	headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.par.det2instrfile);
	goto Error;
      }

    if (global.par.vignflag)
      {
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
	
	/* Retrieve Vignetting info from input vignfile*/
	if( ReadVignFile(global.par.vignfile, &vigninfo) )
	  {
	    headas_chat(NORMAL, "%s: Error: unable to read Vignetting file.\n", global.taskname);
	    goto Error;
	  }
	
	/* Apply Vignetting Correction */
	if(ApplyVignetting(global.par.det2instrfile, &vigninfo, &aligninfo))
	  {
	    headas_chat(NORMAL, "%s: Error: unable to apply Vignetting correction to '%s' file.\n", global.taskname, global.par.det2instrfile);
	    goto Error;
	  }
      }

    /* Update vignetting keywords */
    if(UpdateVignettingKeys(global.par.det2instrfile)){
      headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.det2instrfile);
      goto Error;
    }
    
  }

  
  /* Write output SKY Instrument Map File */

  /* copy the primary header from DET2 intrument map file */
  sprintf(cmd, "ftcopy infile=%s+0 outfile=%s copyall=no chatter=1 history=no clobber=no", global.par.det2instrfile, global.par.skyinstrfile);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: unable to create SKY Instrument Map File '%s'\n", global.taskname, global.par.skyinstrfile);
    goto Error;
  }
  

  expoinfo.livetime = global.obsinfo.livetime;
  expoinfo.deadc = global.obsinfo.deadc;
  expoinfo.ontime = 0.0;

  for(i=0; i<histocmpcount; i++){

    /* update exposure time variable */
    expoinfo.ontime += histocmpinfo[i].duration;


    /* Create local copy of getxform attitude file */
    SplitFilePath(global.par.attfile, DirName, BaseName);
    sprintf(global.tmpout.loc_attfile, "%s/%s", global.tmpout.dirname, BaseName);
    
    if( CreateAbsSymbolicLink(global.par.attfile, global.tmpout.loc_attfile) )
      {
	headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.loc_attfile, global.par.attfile);
	goto Error;
      }

    /* Derive temporary getxform output file name */
    sprintf(global.tmpout.getxform_out, "%dtmp_getxform%d.xform", (int)pid, i);
    

    /* Execute getxform */
    sprintf(cmd, "getxform teldef=%s from=RAW to=SKY image=yes segment=1 ra=%f dec=%f mjdref=%.12f time=%f attfile=%s aberration=%s history=yes outfile=%s",
	    global.par.teldef, global.obsinfo.ranom, global.obsinfo.decnom, global.obsinfo.mjdref, histocmpinfo[i].ref_time, global.tmpout.loc_attfile, aberration, 
	    global.tmpout.getxform_out);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.getxform_out);
      goto Error;
    }


    if(global.par.skysize<SKY_MAX){

      /* Define SKY size */
      skysize = (int)(sqrt(2)*det2size+0.5);
 
      /* Derive temporary applyxform input and output file name */
      sprintf(global.tmpout.applyxform_in, "%dtmp_from_det2_%d.txt", (int)pid, i);
      sprintf(global.tmpout.applyxform_out, "%dtmp_to_sky_%d.txt", (int)pid, i);


      if(WriteDet2ToSkyApplyXformInFile(global.tmpout.applyxform_in, &det2info[i])){
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.tmpout.applyxform_in);
	goto Error;
      }

      /* Execute applyxform */
      sprintf(cmd, "applyxform infile=%s transform=%s outfile=%s", global.tmpout.applyxform_in, global.tmpout.getxform_out, global.tmpout.applyxform_out);
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.applyxform_out);
	goto Error;
      }

      if(ReadDet2ToSkyApplyXformOutFile(global.tmpout.applyxform_out, &xmin_sky, &ymin_sky)){
	headas_chat(NORMAL, "%s: Error: Unable to read '%s' file.\n", global.taskname, global.tmpout.applyxform_out);
	goto Error; 
      }


      /* Derive temporary combinexform input file name */
      sprintf(global.tmpout.combinexform_in, "%dtmp_skytrasform%d.txt", (int)pid, i);
      
      if(WriteSkyCombineXformInFile(global.tmpout.combinexform_in, xmin_sky, ymin_sky, &det2info[i], global.tmpout.getxform_out)){
	headas_chat(NORMAL, "%s: Error: Unable to write '%s' file.\n", global.taskname, global.tmpout.combinexform_in);
	goto Error; 
      }
      
      /* Derive temporary combinexform output file name */
      sprintf(global.tmpout.combinexform_out, "%dtmp_skytrasform%d.xform", (int)pid, i);
      
      /* Execute combinexform */
      sprintf(cmd, "combinexform command=@%s outfile=%s", global.tmpout.combinexform_in, global.tmpout.combinexform_out);
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.combinexform_out);
	goto Error;
      }

      /* Execute imagetrans */
      sprintf(cmd, "imagetrans infile=%s[%d] outfile=%s transform=%s inverse=none method=AREA dimenx=%d dimeny=%d seed=0 bitpix=-32 zeronull=no history=yes", 
	      global.par.det2instrfile, i+1, global.par.skyinstrfile, global.tmpout.combinexform_out, skysize, skysize );
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.skyinstrfile);
	goto Error;
      }


      if ( FileExists(global.tmpout.applyxform_in) ) {
	if(remove (global.tmpout.applyxform_in) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.applyxform_in);
	}
      }
      
      if ( FileExists(global.tmpout.applyxform_out) ) {
	if(remove (global.tmpout.applyxform_out) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.applyxform_out);
	}
      }
      
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
      
    }
    else{
      /* Define SKY size */
      skysize = 1000;

      xmin_sky = 1;
      ymin_sky = 1;

      /* Execute imagetrans */
      sprintf(cmd, "imagetrans infile=%s[%d] outfile=%s transform=%s inverse=none method=AREA dimenx=%d dimeny=%d seed=0 bitpix=-32 zeronull=no history=yes", 
	      global.par.det2instrfile, i+1, global.par.skyinstrfile, global.tmpout.getxform_out, skysize, skysize );
      headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
      
      fflush(stdout);
      status = system(cmd);
      if(status!=0){
	headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.skyinstrfile);
	goto Error;
      }

    }
  
    /* Execute getwcs */
    sprintf(cmd, "getwcs teldef=%s coord=SKY segment=1 ra=%f dec=%f outfile=%s[%d] history=yes", 
	    global.par.teldef, global.obsinfo.ranom, global.obsinfo.decnom, global.par.skyinstrfile, i+1 );
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.skyinstrfile);
      goto Error;
    }

 
    /* Create skywcs temporary file */
    sprintf(global.tmpout.skywcs, "%dtmp_%d_skywcs", (int)pid, i);
    if(CreateSkyWCS(global.tmpout.skywcs, xmin_sky, ymin_sky, skysize)){
      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, global.tmpout.skywcs);
      goto Error;
    }


    /* Execute fhedit */
    sprintf(cmd, "fthedit infile=%s[%d] keyword=@%s operation=add value=0 chatter=1 history=no protect=yes longstring=no",
	    global.par.skyinstrfile, i+1, global.tmpout.skywcs );
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
    fflush(stdout);
    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.skyinstrfile);
      goto Error;
    }

    /* Remove local link of getxform attitude file  */
    if( unlink(global.tmpout.loc_attfile) ){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.loc_attfile);
    }

    /* Cleanup Temporary Files */

    if ( FileExists(global.tmpout.getxform_out) ) {
      if(remove (global.tmpout.getxform_out) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.getxform_out);
      }
    }

    if ( FileExists(global.tmpout.skywcs) ) {
      if(remove (global.tmpout.skywcs) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.skywcs);
      }
    }

  } /* for(i=0; i<histocmpcount; i++) */


  /* Create local link of fchecksum input file */
  SplitFilePath(global.par.skyinstrfile, DirName, BaseName);
  sprintf(global.tmpout.ximage_skyinstrfile, "%s/%s", global.tmpout.dirname, BaseName);
    
  if( CreateAbsSymbolicLink(global.par.skyinstrfile, global.tmpout.ximage_skyinstrfile) )
    {
      headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.ximage_skyinstrfile, global.par.skyinstrfile);
      goto Error;
    }
  
  /* Execute fchecksum */
  sprintf(cmd, "fchecksum infile=%s update=yes", global.tmpout.ximage_skyinstrfile );
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.tmpout.ximage_skyinstrfile);
    goto Error;
  }

  /* Remove local link of fchecksum input file  */
  if( unlink(global.tmpout.ximage_skyinstrfile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.ximage_skyinstrfile);
  }


  if(global.createskyinstrfile)
    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.skyinstrfile);
  
  /* End -> Write output SKY Instrument Map File */


  /* Write output Exposure Map File */


  /* Create local copy of ximage input file */
  SplitFilePath(global.par.skyinstrfile, DirName, BaseName);
  sprintf(global.tmpout.ximage_skyinstrfile, "%s/%s", global.tmpout.dirname, BaseName);

  if( CreateAbsSymbolicLink(global.par.skyinstrfile, global.tmpout.ximage_skyinstrfile) )
    {
      headas_chat(CHATTY, "%s: Error: Unable to create symbolic link '%s' to '%s'.\n", global.taskname, global.tmpout.ximage_skyinstrfile, global.par.skyinstrfile);
      goto Error;
    }

  /* Derive temporary ximage output file name */
  SplitFilePath(global.par.expomapfile, DirName, BaseName);
  sprintf(global.tmpout.ximage_expomapfile, "%s/%s", global.tmpout.dirname, BaseName);

  /* Derive temporary ximage commands input file name */
  sprintf(global.tmpout.ximage_in, "%dtmp_ximage.xco", (int)pid);
  
  if(WriteXimageInFile(global.tmpout.ximage_skyinstrfile, global.tmpout.ximage_expomapfile, histocmpcount, det1info, global.tmpout.ximage_in)){
    headas_chat(NORMAL, "%s: Error: Unable to write '%s' temporary file.\n", global.taskname, global.tmpout.ximage_in);
    goto Error; 
  }

  /* Execute Ximage */
  sprintf(cmd, "ximage @%s", global.tmpout.ximage_in);
  headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
  
  fflush(stdout);
  status = system(cmd);
  if(status!=0){
    headas_chat(NORMAL, "%s: Error: unable to create output Exposure Map File '%s'\n", global.taskname, global.par.expomapfile);
    goto Error;
  }

  /* Rename output exposure map file */
  if ( RenameFile(global.tmpout.ximage_expomapfile,global.par.expomapfile) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: Unable to copy temporary file '%s' to '%s'.\n", global.taskname, global.tmpout.ximage_expomapfile, global.par.expomapfile);
      goto Error;
    }
  
  /* Remove local link of ximage input file  */
  if( unlink(global.tmpout.ximage_skyinstrfile) ){
    headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmpout.ximage_skyinstrfile);
  }

  /* Update exposure map using DEADC value */
  if(UpdateExposureMapWithDEADC(global.par.expomapfile, global.obsinfo.deadc)){
    headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.expomapfile);
    goto Error;
  }

  /* Update exposure keywords */
  if(UpdateExposureKeyword(global.par.expomapfile, &expoinfo)){
    headas_chat(NORMAL, "%s: Error: Unable to update '%s' file.\n", global.taskname, global.par.expomapfile);
    goto Error;
  }

  headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.expomapfile);


  /* Cleanup Temporary Files */
 
  if ( FileExists(global.tmpout.det1refgti) ) {
    if(remove (global.tmpout.det1refgti) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.det1refgti);
    }
  }

  if ( FileExists(global.tmpout.ximage_in) ) {
    if(remove (global.tmpout.ximage_in) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.tmpout.ximage_in);
    }
  }

  if( (!global.createoffsetfile) && FileExists(global.par.offsetfile) ){
    if(remove (global.par.offsetfile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.par.offsetfile);
    }
  }

  if( (!global.createaspecthistofile) && FileExists(global.par.aspecthistofile) ){
    if(remove (global.par.aspecthistofile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.par.aspecthistofile);
    }
  }
  
  if( (!global.createdet1instrfile) && FileExists(global.par.det1instrfile) ){
    if(remove (global.par.det1instrfile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.par.det1instrfile);
    }
  }
  
  if( (!global.createdet2instrfile) && FileExists(global.par.det2instrfile) ){
    if(remove (global.par.det2instrfile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.par.det2instrfile);
    }
  }
  
  if( (!global.createskyinstrfile) && FileExists(global.par.skyinstrfile) ){
    if(remove (global.par.skyinstrfile) == -1){
      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname,global.par.skyinstrfile);
    }
  }
  

  /* Delete the task temporary directory */
  if(rmdir(global.tmpout.dirname)){
    headas_chat(NORMAL, "%s: Warning: Unable to delete the temporary directory '%s'\n", global.taskname, global.tmpout.dirname);
  }


  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");

  
  return OK; 
  
 Error:

  return NOT_OK;

} /* nuexpomap_work */


/*
 *	nuexpomap
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
 *             void nuexpomap_getpar(void);
 * 	       void nuexpomap_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *          
 *      CHANGE HISTORY:
 *        0.1.0 - NS 18/01/12 - First version
 *          
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int nuexpomap()
{
  /* set HEADAS globals */
  set_toolname(PRG_NAME);
  set_toolversion(NUEXPOMAP_VERSION);
  
  get_toolnamev(global.taskname);
  
  GetNuSTARDASVersion(global.nustardas_v);
  global.warning=0;
  
  /* Get parameter values */ 
  if ( nuexpomap_getpar() == OK) 
    {
      
      if ( nuexpomap_work()) 
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
  
} /* nuexpomap */


/*
 *	nuexpomap_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *          
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 18/01/12 - First version
 *
 */
void nuexpomap_info(void)
{
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input Event file                          :'%s'\n",global.par.infile);

  if(!global.getdet2instrfile){
    headas_chat(NORMAL,"Name of the input pixel location file                 :'%s'\n",global.par.pixposfile);
    headas_chat(NORMAL,"Name of the input Mast Aspect Solution file           :'%s'\n",global.par.mastaspectfile);
    headas_chat(NORMAL,"Name of the input Instrument Probability Map file     :'%s'\n",global.par.instrprobmapfile);
  }

  headas_chat(NORMAL,"Name of the teldef calibration file                   :'%s'\n",global.par.teldef);
  headas_chat(NORMAL,"Name of the input alignment file                      :'%s'\n",global.par.alignfile);
  headas_chat(NORMAL,"Name of the input vignetting file                     :'%s'\n",global.par.vignfile);
  if (global.par.vignflag)
    headas_chat(NORMAL,"Apply Vignetting correction                           : yes\n");
  else
    headas_chat(NORMAL,"Apply Vignetting correction                           : no\n");
  headas_chat(NORMAL,"Energy value for vignetting calculation (keV)         :'%f'\n",global.par.energy);

  if(!global.getdet2instrfile){
    headas_chat(NORMAL,"Name of the input DET1 Reference Point file           :'%s'\n",global.par.det1reffile);
    headas_chat(NORMAL,"Bin size of aspect histogram in pixels                :'%d'\n",global.par.pixbin);
    headas_chat(NORMAL,"Name of the output Offset file                        :'%s'\n",global.par.offsetfile);
    headas_chat(NORMAL,"Name of the output Aspect Histogram file              :'%s'\n",global.par.aspecthistofile);
    headas_chat(NORMAL,"Name of the output DET1 Instrument Map file           :'%s'\n",global.par.det1instrfile);
    headas_chat(NORMAL,"Name of the output DET2 Instrument Map file           :'%s'\n",global.par.det2instrfile);
  }

  headas_chat(NORMAL,"Name of the output SKY Instrument Map file            :'%s'\n",global.par.skyinstrfile);
  headas_chat(NORMAL,"Name of the output Exposure Map file                  :'%s'\n",global.par.expomapfile);

  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
  else
    headas_chat(CHATTY,"Overwrite existing output file                         : no\n");
  
  headas_chat(NORMAL,"---------------------------------------------------------------------\n");
} /* nuexpomap_info */


/*
 *	nuexpomap_checkinput
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
 *        0.1.0: - NS 18/01/12 - First version
 *
 */
int nuexpomap_checkinput(void)
{
  pid_t          pid;

  /* Get pid */
  pid=getpid();


  /* Check if inputfile exists */
  if(!FileExists(global.par.infile)){
    headas_chat(NORMAL, "%s: Error: File '%s' not found.\n", global.taskname,global.par.infile);
    goto check_end;
  }

  /* Check if attfile exists */
  if(!FileExists(global.par.attfile)){
    headas_chat(NORMAL, "%s: Error: File '%s' not found.\n", global.taskname,global.par.attfile);
    goto check_end;
  }

  if(!global.getdet2instrfile){

    /* Check if mastaspectfile exists */
    if(!FileExists(global.par.mastaspectfile)){
      headas_chat(NORMAL, "%s: Error: File '%s' not found.\n", global.taskname,global.par.mastaspectfile);
      goto check_end;
    }

    /* Check if det1reffile exists */
    if(!FileExists(global.par.det1reffile)){
      headas_chat(NORMAL, "%s: Error: File '%s' not found.\n", global.taskname,global.par.det1reffile);
      goto check_end;
    }

  }
  
  /* Derive temporary DET1 Reference Point GTI file name */
  sprintf(global.tmpout.det1refgti, "%dtmp_det1refgti.fits", (int)pid);

  if(FileExists(global.tmpout.det1refgti))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.tmpout.det1refgti);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.tmpout.det1refgti);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.tmpout.det1refgti);
	  if(remove (global.tmpout.det1refgti) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.tmpout.det1refgti);
	      goto check_end;
	    }
	}
    }

  if(global.createoffsetfile)
    {
      if(FileExists(global.par.offsetfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.offsetfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.offsetfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.offsetfile);
	      if(remove (global.par.offsetfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.offsetfile);
		  goto check_end;
		}
	    }
	}
    }
  else{
    /* Derive temporary  file name */
    sprintf(global.par.offsetfile, "%dtmp_offset.fits", (int)pid);
  }

  if(global.createaspecthistofile)
    {
      if(FileExists(global.par.aspecthistofile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.aspecthistofile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.aspecthistofile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.aspecthistofile);
	      if(remove (global.par.aspecthistofile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.aspecthistofile);
		  goto check_end;
		}
	    }
	}
    }
  else{
    /* Derive temporary  file name */
    sprintf(global.par.aspecthistofile, "%dtmp_aspecthisto.fits", (int)pid);
  }

  if(global.createdet1instrfile)
    {
      if(FileExists(global.par.det1instrfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.det1instrfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.det1instrfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.det1instrfile);
	      if(remove (global.par.det1instrfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.det1instrfile);
		  goto check_end;
		}
	    }
	}
    }
  else{
    /* Derive temporary  file name */
    sprintf(global.par.det1instrfile, "%dtmp_det1instr.fits", (int)pid);
  }


  if(global.createdet2instrfile)
    {
      if(FileExists(global.par.det2instrfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.det2instrfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.det2instrfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.det2instrfile);
	      if(remove (global.par.det2instrfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.det2instrfile);
		  goto check_end;
		}
	    }
	}
    }
  else{
    /* Derive temporary  file name */
    sprintf(global.par.det2instrfile, "%dtmp_det2instr.fits", (int)pid);
  }

  if(global.createskyinstrfile)
    {
      if(FileExists(global.par.skyinstrfile))
	{
	  headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.skyinstrfile);
	  if(!headas_clobpar)
	    {
	      headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.skyinstrfile);
	      headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	      goto check_end;
	    }
	  else
	    {
	      headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	      headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.skyinstrfile);
	      if(remove (global.par.skyinstrfile) == -1)
		{
		  headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
		  headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.skyinstrfile);
		  goto check_end;
		}
	    }
	}
    }
  else{
    /* Derive temporary  file name */
    sprintf(global.par.skyinstrfile, "%dtmp_skyinstr.fits", (int)pid);
  }

  if(FileExists(global.par.expomapfile))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.expomapfile);
      if(!headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.expomapfile);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto check_end;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, global.par.expomapfile);
	  if(remove (global.par.expomapfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.expomapfile);
	      goto check_end;
	    }
	}
    }


  if( (global.par.skyx+global.par.skysize/2-0.5 > SKY_MAX) || (global.par.skyx-global.par.skysize/2+0.5<1) ){
    headas_chat(NORMAL, "%s: Error: skyx=%f and skysize=%d input parameter values not allowed\n", global.taskname, global.par.skyx, global.par.skysize);
    goto check_end;
  }

  if( (global.par.skyy+global.par.skysize/2-0.5 > SKY_MAX) || (global.par.skyy-global.par.skysize/2+0.5<1) ) {
    headas_chat(NORMAL, "%s: Error: skyy=%f and skysize=%d input parameter values not allowed\n", global.taskname, global.par.skyy, global.par.skysize);
    goto check_end;
  }


  /* Set temporary task directory name */
  sprintf(global.tmpout.dirname, "%d_tmp_nuexpomap", (int)pid);


  return OK;

 check_end:

  return NOT_OK;

}


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

  if(ExistsKeyWord(&head, KWNM_OBS_ID, &card))
    {
      strcpy(obsinfo->obs_id, card->u.SVal);
      strcpy(obsinfo->obs_id_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_OBS_ID );
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if (ExistsKeyWord(&head, KWNM_TARG_ID, &card))
    {
      obsinfo->targ_id=card->u.JVal;
      strcpy(obsinfo->targ_id_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TARG_ID );
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if(ExistsKeyWord(&head, KWNM_OBJECT, &card))
    {
      strcpy(obsinfo->object, card->u.SVal);
      strcpy(obsinfo->object_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_OBJECT );
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if(ExistsKeyWord(&head, KWNM_RA_OBJ, &card))
    {
      obsinfo->raobj = card->u.DVal;
      strcpy(obsinfo->raobj_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_RA_OBJ);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if(ExistsKeyWord(&head, KWNM_DEC_OBJ, &card))
    {
      obsinfo->decobj = card->u.DVal;
      strcpy(obsinfo->decobj_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DEC_OBJ);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Get RA_NOM value */
  if(ExistsKeyWord(&head, KWNM_RA_NOM, &card))
    {
      obsinfo->ranom = card->u.DVal;
      strcpy(obsinfo->ranom_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_RA_NOM);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Get DEC_NOM value */
  if(ExistsKeyWord(&head, KWNM_DEC_NOM, &card))
    {
      obsinfo->decnom = card->u.DVal;
      strcpy(obsinfo->decnom_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DEC_NOM);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }
    
  if(ExistsKeyWord(&head, KWNM_RA_PNT, &card))
    {
      obsinfo->rapnt = card->u.DVal;
      strcpy(obsinfo->rapnt_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_RA_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if(ExistsKeyWord(&head, KWNM_DEC_PNT, &card))
    {
      obsinfo->decpnt = card->u.DVal;
      strcpy(obsinfo->decpnt_comm, card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DEC_PNT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if (ExistsKeyWord(&head, KWNM_EQUINOX, &card))
    {
      obsinfo->equinox = card->u.DVal;
      strcpy(obsinfo->equinox_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_EQUINOX);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if (ExistsKeyWord(&head, KWNM_TIMESYS, &card))
    {
      strcpy(obsinfo->timesys,card->u.SVal);
      strcpy(obsinfo->timesys_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMESYS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if (ExistsKeyWord(&head, KWNM_MJDREFI, &card))
    {
      obsinfo->mjdrefi=card->u.JVal;
      strcpy(obsinfo->mjdrefi_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_MJDREFI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if (ExistsKeyWord(&head, KWNM_MJDREFF, &card))
    {
      obsinfo->mjdreff=card->u.DVal;
      strcpy(obsinfo->mjdreff_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_MJDREFF);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if (ExistsKeyWord(&head, KWNM_TIMEUNIT, &card))
    {
      strcpy(obsinfo->timeunit,card->u.SVal);
      strcpy(obsinfo->timeunit_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEUNIT);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Retrieve observation start time from input event file */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      obsinfo->tstart=card->u.DVal;
      strcpy(obsinfo->tstart_comm,card->Comment);
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
      strcpy(obsinfo->tstop_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if((ExistsKeyWord(&head, KWNM_TELAPSE, &card)))
    {
      obsinfo->telapse=card->u.DVal;
      strcpy(obsinfo->telapse_comm,card->Comment);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_TELAPSE);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  /* Retrieve date-obs and time-obs from input file  */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      strcpy(obsinfo->dateobs,card->u.SVal);
      strcpy(obsinfo->dateobs_comm,card->Comment);
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
      strcpy(obsinfo->dateend,card->u.SVal);
      strcpy(obsinfo->dateend_comm,card->Comment);
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

  /* Get MJDREF value */
  status = 0;
  obsinfo->mjdref = HDget_frac_time(inunit, KWNM_MJDREF, 0,0, &status);
  if(status){
    headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_MJDREF);
    headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
    goto GetObsInfo_end;
  }

  if((ExistsKeyWord(&head, KWNM_DEADC, &card)))
    {
      obsinfo->deadc=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_DEADC);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
      goto GetObsInfo_end;
    }

  if((ExistsKeyWord(&head, KWNM_LIVETIME, &card)))
    {
      obsinfo->livetime=card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname,KWNM_LIVETIME);
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


int ComputeOffsets(char *filename, OffsetInfo_t **offinfo, int *offcount){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  double             x_det1_min=DET1_MAX, y_det1_min=DET1_MAX;
  Det1RefCol_t       indxcol;
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
 
  /* Move in DET1_REFPOINT extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_DET1_REFPOINT, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_DET1_REFPOINT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto ComputeOffsets_end;
    }
  
  /* Retrieve header pointer */
  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ComputeOffsets_end;
    }

  /* Get needed columns number from name */
  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto ComputeOffsets_end;
    }

  if ((indxcol.X_DET1 = GetColNameIndx(&table, CLNM_X_DET1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_DET1);
      goto ComputeOffsets_end;
    }

  if ((indxcol.Y_DET1 = GetColNameIndx(&table, CLNM_Y_DET1)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_DET1);
      goto ComputeOffsets_end;
    }

 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *offcount = table.MaxRows;
 *offinfo = (OffsetInfo_t *)calloc(*offcount, sizeof(OffsetInfo_t));
 if(*offinfo==NULL){
   headas_chat(CHATTY,"%s: Error: ComputeOffsets: memory allocation failure.\n", global.taskname);
   goto ComputeOffsets_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<*offcount) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {
	 (*offinfo)[count].time = DVEC(table,n,indxcol.TIME);
	 (*offinfo)[count].x_offset = DVEC(table,n,indxcol.X_DET1);
	 (*offinfo)[count].y_offset = DVEC(table,n,indxcol.Y_DET1);

	 x_det1_min = MIN( DVEC(table,n,indxcol.X_DET1), x_det1_min );
	 y_det1_min = MIN( DVEC(table,n,indxcol.Y_DET1), y_det1_min );

	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 
   
  *offcount = count;


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      return NOT_OK;
    }

  
  /* Compute X_OFFSET and Y_OFFSET using the min value of X_DET1 and Y_DET1 */
  for(count=0; count<(*offcount); count++){
    (*offinfo)[count].x_offset = (*offinfo)[count].x_offset - x_det1_min ;
    (*offinfo)[count].y_offset = (*offinfo)[count].y_offset - y_det1_min ;

    /* headas_chat(NORMAL,"DEBUG X_OFFSET=%f Y_OFFSET=%f \n",(*offinfo)[count].x_offset,(*offinfo)[count].y_offset); */
  }


  return OK;
  
 ComputeOffsets_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

} /* ComputeOffsets */



int WriteOffsetFile(OffsetInfo_t *offinfo, int offcount, char *infile, char *outfile){

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
      goto WriteOffsetFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteOffsetFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WriteOffsetFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteOffsetFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteOffsetFile_end;
    }
  hducount=1;


  /* Retrieve header pointer */
  head=RetrieveFitsHeader(outunit);

  /* Add INSTRUME */
  if(!strcasecmp(global.obsinfo.instrume,KWVL_INSTRUME_FPMA))
    sprintf(crval,"FPMA");
  else
    sprintf(crval,"FPMB");

  AddCard(&head, KWNM_INSTRUME, S, crval,CARD_COMM_INSTRUME);
 
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
      goto WriteOffsetFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteOffsetFile_end;
      }
    }

  /* Create 'OFFSET' Ext  */
  if (WriteOffsetExt(offinfo, offcount, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_OFFSET);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteOffsetFile_end;
    }
  hducount++;

  /* Add history */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteOffsetFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteOffsetFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteOffsetFile_end;
    }


  return OK;
  
 WriteOffsetFile_end:

  return NOT_OK;


} /* WriteOffsetFile */


int WriteOffsetExt(OffsetInfo_t *offinfo, int offcount, FitsFileUnit_t ounit){

  int             n, count=0;
  unsigned        OutRows=0;
  OffsetCol_t     indxcol;
  Bintable_t	  table; 
  FitsHeader_t	  newhead;
  char            crval[FLEN_VALUE];
  char            date[25];
  Version_t       nustardas_v;          /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );


  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(BINTAB_ROWS, &table);

  /* Add columns */

  AddColumn(&newhead, &table, CLNM_TIME, "Event Time (seconds since Jan 2010 00:00:00 UTC)", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_X_OFFSET, CARD_COMM_X_OFFSET, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_Y_OFFSET, CARD_COMM_Y_OFFSET, "1D", TUNIT, UNIT_PIXEL, CARD_COMM_PHYSUNIT);

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_OFFSET, CARD_COMM_EXTNAME);

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
      goto WriteOffsetExt_end;
    }

  if ((indxcol.X_OFFSET = GetColNameIndx(&table, CLNM_X_OFFSET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_OFFSET);
      goto WriteOffsetExt_end;
    }

  if ((indxcol.Y_OFFSET = GetColNameIndx(&table, CLNM_Y_OFFSET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_OFFSET);
      goto WriteOffsetExt_end;
    }


  OutRows = 0;
  count = 0;
  
  while(count<offcount){
    
    for(n=0; (n<BINTAB_ROWS)&&(count<offcount); n++){
      
      DVEC(table, n, indxcol.TIME) = offinfo[count].time;
      DVEC(table, n, indxcol.X_OFFSET) = offinfo[count].x_offset;
      DVEC(table, n, indxcol.Y_OFFSET) = offinfo[count].y_offset;
      
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
  
 WriteOffsetExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteOffsetExt */


int WriteAspectHistoFile(OffsetInfo_t *offinfo, int offcount, char *infile, char *outfile){

  int                 status = OK, hducount=0; 
  char                date[25];
  AspHistoInfo_t      *histoinfo=NULL;
  int                 histocount=0;
  AspHistoCompInfo_t  *histocmpinfo=NULL;
  int                 histocmpcount=0;
  FitsHeader_t	      head;
  FitsFileUnit_t      outunit=NULL, inunit=NULL; 
  char                crval[FLEN_VALUE];
  Version_t           nustardas_v;     /* NuSTARDAS version */
  struct gti_struct   gti;

  GetNuSTARDASVersion(nustardas_v);


  /* Read GTI info from <infile> GTI extension */
  if(HDgti_read(infile, &gti, KWVL_EXTNAME_GTI, 0, 0, 0, 0, &status)){
    headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' file.\n", global.taskname, infile);
    goto WriteAspectHistoFile_end;
  }


  /* Compute Aspect Histogram */
  if(ComputeAspHisto(offinfo, offcount, &gti, &histoinfo, &histocount)){
    headas_chat(NORMAL,"%s: Error: unable to compute aspect histogram.\n", global.taskname);
    goto WriteAspectHistoFile_end;
  }

  /* Compute compressed Aspect Histogram */
  if(ComputeAspHistoComp(histoinfo, histocount, &histocmpinfo, &histocmpcount)){
    headas_chat(NORMAL,"%s: Error: unable to compute compressed aspect histogram.\n", global.taskname);
    goto WriteAspectHistoFile_end;
  }

  /* Open input file */
  if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, infile);
      goto WriteAspectHistoFile_end;
    }

  /* Build primary header */
  if ((outunit=OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, outfile);
      goto WriteAspectHistoFile_end;
    }

  /* Move to input file primary header to copy it in new file */
  if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary header\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, infile);
      goto WriteAspectHistoFile_end;
    }
  
  if(fits_copy_hdu(inunit, outunit, 0, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to copy primary header to\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, outfile);
      goto WriteAspectHistoFile_end;
    }

  /* Move to primary header */
  if(fits_movabs_hdu(outunit, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, outfile);
      goto WriteAspectHistoFile_end;
    }
  hducount=1;


  /* Retrieve header pointer */
  head=RetrieveFitsHeader(outunit);


  /* Add INSTRUME */
  if(!strcasecmp(global.obsinfo.instrume,KWVL_INSTRUME_FPMA))
    sprintf(crval,"FPMA");
  else
    sprintf(crval,"FPMB");

  AddCard(&head, KWNM_INSTRUME, S, crval,CARD_COMM_INSTRUME);
 
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
      goto WriteAspectHistoFile_end; 
    }

  /* Delete PROCVER keyword */
  if(ExistsKeyWord(&head, "PROCVER", NULL))
    {
      if(fits_delete_key(outunit, "PROCVER", &status )){
	headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PROCVER");
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, outfile);
	goto WriteAspectHistoFile_end;
      }
    }

  /* Create 'ASP_HIST' Ext  */
  if (WriteAspectHistoExt(histoinfo, histocount, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_ASP_HIST);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteAspectHistoFile_end;
    }
  hducount++;

  /* Add history */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteAspectHistoFile_end;
    }

  /* Create 'ASP_HIST_COMP' Ext  */
  if (WriteAspectHistoCompExt(histocmpinfo, histocmpcount, outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to append '%s' extension\n", global.taskname, KWVL_EXTNAME_ASP_HIST_COMP);
      headas_chat(NORMAL, "%s: Error: to %s file.\n",global.taskname, outfile);
      goto WriteAspectHistoFile_end;
    }
  hducount++;

  /* Add history */
  if(HDpar_stamp(outunit, hducount, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteAspectHistoFile_end;
    }

  /* Update checksum and datasum keywords */
  if (ChecksumCalc(outunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
      goto WriteAspectHistoFile_end;
    }
  
  /* close output files */
  if ( fits_close_file(outunit, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
      goto WriteAspectHistoFile_end;
    }


  return OK;
  
 WriteAspectHistoFile_end:

  return NOT_OK;


} /* WriteAspectHistoFile */


int WriteAspectHistoExt(AspHistoInfo_t *histoinfo, int histocount, FitsFileUnit_t ounit){

  int             n, count=0;
  unsigned        OutRows=0;
  AspHistoCol_t   indxcol;
  Bintable_t	  table; 
  FitsHeader_t	  newhead;
  char            crval[FLEN_VALUE];
  char            date[25];
  Version_t       nustardas_v;          /* NuSTARDAS version */

  GetNuSTARDASVersion(nustardas_v);

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &newhead, FitsHeader_t );


  /* Create a new bintable header and get pointer to it */
  newhead = NewBintableHeader(BINTAB_ROWS, &table);

  /* Add columns */

  AddColumn(&newhead, &table, CLNM_X_BIN, CARD_COMM_X_BIN, "1J",TNONE);
  AddColumn(&newhead, &table, CLNM_Y_BIN, CARD_COMM_Y_BIN, "1J",TNONE);
  AddComment(&newhead,"The columns X_BIN and Y_BIN give the bin number according to the following definition:");
  AddComment(&newhead,"X_BIN =1 for 0<=X_OFFSET<1*pixbin");
  AddComment(&newhead,"X_BIN =2 for 1*pixbin<=X_OFFSET<2*pixbin");
  AddComment(&newhead,"etc.");
  AddColumn(&newhead, &table, CLNM_DURATION, CARD_COMM_DURATION, "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_TSTART, "Start time of the bin", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_TSTOP, "End time of the bin", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_REF_TIME, CARD_COMM_REF_TIME, "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_ASP_HIST, CARD_COMM_EXTNAME);

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

  if ((indxcol.X_BIN = GetColNameIndx(&table, CLNM_X_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_BIN);
      goto WriteAspectHistoExt_end;
    }

  if ((indxcol.Y_BIN = GetColNameIndx(&table, CLNM_Y_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_BIN);
      goto WriteAspectHistoExt_end;
    }

  if ((indxcol.DURATION = GetColNameIndx(&table, CLNM_DURATION)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DURATION);
      goto WriteAspectHistoExt_end;
    }

  if ((indxcol.TSTART = GetColNameIndx(&table, CLNM_TSTART)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TSTART);
      goto WriteAspectHistoExt_end;
    }

  if ((indxcol.TSTOP = GetColNameIndx(&table, CLNM_TSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TSTOP);
      goto WriteAspectHistoExt_end;
    }

  if ((indxcol.REF_TIME = GetColNameIndx(&table, CLNM_REF_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_REF_TIME);
      goto WriteAspectHistoExt_end;
    }


  OutRows = 0;
  count = 0;
  
  while(count<histocount){
    
    for(n=0; (n<BINTAB_ROWS)&&(count<histocount); n++){

      /* NOTE! Bin number in input file start from 1 while in histoinfo start from 0 */
      JVEC(table, n, indxcol.X_BIN) = histoinfo[count].x_bin +1;
      JVEC(table, n, indxcol.Y_BIN) = histoinfo[count].y_bin +1;      
      DVEC(table, n, indxcol.DURATION) = histoinfo[count].duration;
      DVEC(table, n, indxcol.TSTART) = histoinfo[count].tstart;
      DVEC(table, n, indxcol.TSTOP) = histoinfo[count].tstop;
      DVEC(table, n, indxcol.REF_TIME) = histoinfo[count].ref_time;

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
  
 WriteAspectHistoExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteAspectHistoExt */


int WriteAspectHistoCompExt(AspHistoCompInfo_t *histoinfo, int histocount, FitsFileUnit_t ounit){

  int                n, count=0;
  unsigned           OutRows=0;
  AspHistoCompCol_t  indxcol;
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

  AddColumn(&newhead, &table, CLNM_X_BIN, CARD_COMM_X_BIN, "1J", TNONE);
  AddColumn(&newhead, &table, CLNM_Y_BIN, CARD_COMM_Y_BIN, "1J", TNONE);
  AddComment(&newhead,"The columns X_BIN and Y_BIN give the bin number according to the following definition:");
  AddComment(&newhead,"X_BIN =1 for 0<=X_OFFSET<1*pixbin");
  AddComment(&newhead,"X_BIN =2 for 1*pixbin<=X_OFFSET<2*pixbin");
  AddComment(&newhead,"etc.");
  AddColumn(&newhead, &table, CLNM_DURATION, "Total temporal duration of the bin", "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);
  AddColumn(&newhead, &table, CLNM_REF_TIME, CARD_COMM_REF_TIME, "1D", TUNIT, UNIT_SEC, CARD_COMM_PHYSUNIT);

  /* Add keywords */

  AddCard(&newhead, KWNM_EXTNAME, S, KWVL_EXTNAME_ASP_HIST_COMP, CARD_COMM_EXTNAME);

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

  if ((indxcol.X_BIN = GetColNameIndx(&table, CLNM_X_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_BIN);
      goto WriteAspectHistoCompExt_end;
    }

  if ((indxcol.Y_BIN = GetColNameIndx(&table, CLNM_Y_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_BIN);
      goto WriteAspectHistoCompExt_end;
    }

  if ((indxcol.DURATION = GetColNameIndx(&table, CLNM_DURATION)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DURATION);
      goto WriteAspectHistoCompExt_end;
    }

  if ((indxcol.REF_TIME = GetColNameIndx(&table, CLNM_REF_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_REF_TIME);
      goto WriteAspectHistoCompExt_end;
    }


  OutRows = 0;
  count = 0;
  
  while(count<histocount){
    
    for(n=0; (n<BINTAB_ROWS)&&(count<histocount); n++){

      /* NOTE! Bin number in input file start from 1 while in histoinfo start from 0 */
      JVEC(table, n, indxcol.X_BIN) = histoinfo[count].x_bin +1;
      JVEC(table, n, indxcol.Y_BIN) = histoinfo[count].y_bin +1;      
      DVEC(table, n, indxcol.DURATION) = histoinfo[count].duration;  
      DVEC(table, n, indxcol.REF_TIME) = histoinfo[count].ref_time;

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
  
 WriteAspectHistoCompExt_end:
  if (newhead.first)
    ReleaseBintable(&newhead, &table);
  return NOT_OK;


} /* WriteAspectHistoCompExt */


int ComputeAspHisto(OffsetInfo_t *offinfo, int offcount, struct gti_struct *gti, AspHistoInfo_t **histoinfo, int *histocount){

  int       count=0, n=0, status=OK;
  int       x_bin, y_bin, firstrow=1;
  int       min_bin=MIN_BIN, max_bin=MAX_BIN;
  double    tstart=0, tstop=0, dtime=0, x_offset, y_offset;

  
  if(!(offcount>0)){
    headas_chat(CHATTY,"%s: Warning: ComputeAspHisto: empty input data.\n", global.taskname);
    *histocount = 0;
    return OK;
  }

  /* Allocate memory to storage all data */
  *histoinfo = (AspHistoInfo_t *)calloc(offcount, sizeof(AspHistoInfo_t));
  if(*histoinfo==NULL){
    headas_chat(CHATTY,"%s: Error: ComputeAspHisto: memory allocation failure.\n", global.taskname);
    goto ComputeAspHisto_end;
  }
  
  for(n=0; n<offcount; n++){

    tstart = offinfo[n].time;
    tstop  = (n<offcount-1) ? offinfo[n+1].time : ( n>0 ? (tstart+offinfo[n].time-offinfo[n-1].time) : tstart+0.25 );

    x_offset = offinfo[n].x_offset;
    y_offset = offinfo[n].y_offset;

    x_bin = GetBinIndex((double)global.par.pixbin, min_bin, max_bin, x_offset);
    y_bin = GetBinIndex((double)global.par.pixbin, min_bin, max_bin, y_offset);


    /* Compute overlap exposure of the time bin with GTI */
    dtime = HDgti_exp(tstart, tstop, gti, &status);
    if(status!=OK){
      headas_chat(NORMAL, "%s: Error: unable to compute overlap exposure of the time bin with GTI\n", global.taskname);
      goto ComputeAspHisto_end;
    }

    if( firstrow==0 && (*histoinfo)[count].x_bin==x_bin && (*histoinfo)[count].y_bin==y_bin ){

      (*histoinfo)[count].duration += dtime;
      (*histoinfo)[count].tstop = tstop;
    }
    else{

      if(firstrow==1)
	firstrow=0;
      else
	count++;

      (*histoinfo)[count].x_bin = x_bin;
      (*histoinfo)[count].y_bin = y_bin;
      (*histoinfo)[count].duration = dtime;
      (*histoinfo)[count].tstart = tstart;
      (*histoinfo)[count].tstop = tstop;
      (*histoinfo)[count].ref_time = tstart;
    }

  }

  /* update histocount value */
  *histocount = count+1;


  return OK;
  
 ComputeAspHisto_end:
  return NOT_OK;
  
} /* ComputeAspHisto */


int ComputeAspHistoComp(AspHistoInfo_t *histoinfo, int histocount, AspHistoCompInfo_t **histocmpinfo, int *histocmpcount){

  int                         i,j,n, count=0;
  int                         x_bin, y_bin;
  static AspHistoCompInfo_t   tmp[MAX_BIN][MAX_BIN];


  /* Allocate memory to storage all data */
  *histocmpinfo = (AspHistoCompInfo_t *)calloc(histocount, sizeof(AspHistoCompInfo_t));
  if(*histocmpinfo==NULL){
    headas_chat(CHATTY,"%s: Error: ComputeAspHistoCmp: memory allocation failure.\n", global.taskname);
    goto ComputeAspHistoCmp_end;
  }


  for(i=0; i<MAX_BIN; i++){
    for(j=0; j<MAX_BIN; j++){
      tmp[i][j].ref_time = -1;
    }
  }

  for(n=0; n<histocount; n++){

    x_bin = histoinfo[n].x_bin;
    y_bin = histoinfo[n].y_bin;

    if(x_bin>=MAX_BIN || y_bin>=MAX_BIN )
      continue;
      

    if(tmp[x_bin][y_bin].ref_time<0)
      {
	tmp[x_bin][y_bin].ref_time = histoinfo[n].ref_time;
	tmp[x_bin][y_bin].duration = histoinfo[n].duration;
      }
    else
      {
	tmp[x_bin][y_bin].duration += histoinfo[n].duration;
      }
  }


  for(i=0; i<MAX_BIN; i++){
    for(j=0; j<MAX_BIN; j++){

      if(tmp[i][j].ref_time>0){
	
	(*histocmpinfo)[count].x_bin = i;
	(*histocmpinfo)[count].y_bin = j;
	(*histocmpinfo)[count].duration = tmp[i][j].duration;
	(*histocmpinfo)[count].ref_time = tmp[i][j].ref_time;

	count++;
      }
    }
  }

  *histocmpcount = count;


  return OK;

 ComputeAspHistoCmp_end:
  return NOT_OK;

} /* ComputeAspHistoComp */


int ReadAspectHistoCompExt(char *filename, AspHistoCompInfo_t **histocmpinfo, int *histocmpcount){

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

  if ((indxcol.DURATION = GetColNameIndx(&table, CLNM_DURATION)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DURATION);
      goto ReadAspectHistoCompExt_end;
    }

  if ((indxcol.REF_TIME = GetColNameIndx(&table, CLNM_REF_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_REF_TIME);
      goto ReadAspectHistoCompExt_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *histocmpcount = table.MaxRows;
 *histocmpinfo = (AspHistoCompInfo_t *)calloc(*histocmpcount, sizeof(AspHistoCompInfo_t));
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
	 /* NOTE! Bin number in input file start from 1 while in histoinfo start from 0 */
	 (*histocmpinfo)[count].x_bin = JVEC(table,n,indxcol.X_BIN) -1;
	 (*histocmpinfo)[count].y_bin = JVEC(table,n,indxcol.Y_BIN) -1;
	 (*histocmpinfo)[count].duration = DVEC(table,n,indxcol.DURATION);
	 (*histocmpinfo)[count].ref_time = DVEC(table,n,indxcol.REF_TIME);

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


/*
NOTE: index=0 =>  (imin) < value < (imin+binsize)
 */
int GetBinIndex(double binsize, int imin, int imax, double value){
  
  int i;
  i = (value<imin*binsize) ? imin : ( value>imax*binsize ? imax : ((value-imin*binsize)/binsize) );
  
  return i;

} /* GetBinIndex */


/*
 *
 *      ReadInstrProbMapFile
 *
 *	DESCRIPTION:
 *           Routine to read input instrprobmapfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadInstrProbMapFile(ObsInfo_t *obsinfo, float instrmap[4][DET1_ROWS][DET1_PIXS]){
  

  char        filename0[PIL_LINESIZE], filename1[PIL_LINESIZE], filename2[PIL_LINESIZE], filename3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;


  /* Derive CALDB instrprobmapfile filename */  
  if ( !strcasecmp(global.par.instrprobmapfile,DF_CALDB) )
    {
      /* Retrieve DET0 instrprobmap filename */
      if (CalGetFileName(HD_MAXRET, obsinfo->dateobs, obsinfo->timeobs, obsinfo->dateend, obsinfo->timeend, KWVL_INSTRPROBMAP_DSET, filename0, HD_EXPR, &extfile0, obsinfo->instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for instrprobmapfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadInstrProbMapFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 instrprobmap filename */
      if (CalGetFileName(HD_MAXRET, obsinfo->dateobs, obsinfo->timeobs, obsinfo->dateend, obsinfo->timeend, KWVL_INSTRPROBMAP_DSET, filename1, HD_EXPR, &extfile1, obsinfo->instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for instrprobmapfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadInstrProbMapFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 instrprobmap filename */
      if (CalGetFileName(HD_MAXRET, obsinfo->dateobs, obsinfo->timeobs, obsinfo->dateend, obsinfo->timeend, KWVL_INSTRPROBMAP_DSET, filename2, HD_EXPR, &extfile2, obsinfo->instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for instrprobmapfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadInstrProbMapFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 instrprobmap filename */
      if (CalGetFileName(HD_MAXRET, obsinfo->dateobs, obsinfo->timeobs, obsinfo->dateend, obsinfo->timeend, KWVL_INSTRPROBMAP_DSET, filename3, HD_EXPR, &extfile3, obsinfo->instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for instrprobmapfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadInstrProbMapFile_end;
      	}
      extfile3++;

    }
  else{
    strcpy(filename0, global.par.instrprobmapfile);
    strcpy(filename1, global.par.instrprobmapfile);
    strcpy(filename2, global.par.instrprobmapfile);
    strcpy(filename3, global.par.instrprobmapfile);
  }

  /* Retrieve DET0 instrument probability map */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for instrument probability map of detector %s.\n", global.taskname, filename0, KWVL_DETNAM_DET0);
  if( ReadInstrProbMapInfo(filename0, extfile0, KWVL_DETNAM_DET0, instrmap[0]) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read instrument probability map\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input instrprobmapfile: %s.\n", global.taskname, filename0);
      goto ReadInstrProbMapFile_end;
    }

  /* Retrieve DET1 instrument probability map */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for instrument probability map of detector %s.\n", global.taskname, filename1, KWVL_DETNAM_DET1);
  if( ReadInstrProbMapInfo(filename1 ,extfile1, KWVL_DETNAM_DET1, instrmap[1]) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read instrument probability map\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input instrprobmapfile: %s.\n", global.taskname, filename1);
      goto ReadInstrProbMapFile_end;
    }

  /* Retrieve DET2 instrument probability map */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for instrument probability map of detector %s.\n", global.taskname, filename2, KWVL_DETNAM_DET2);
  if( ReadInstrProbMapInfo(filename2, extfile2, KWVL_DETNAM_DET2, instrmap[2]) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read instrument probability map\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input instrprobmapfile: %s.\n", global.taskname, filename2);
      goto ReadInstrProbMapFile_end;
    }

  /* Retrieve DET3 instrument probability map */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for instrument probability map of detector %s.\n", global.taskname, filename3, KWVL_DETNAM_DET3);
  if( ReadInstrProbMapInfo(filename3, extfile3, KWVL_DETNAM_DET3, instrmap[3]) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read instrument probability map\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input instrprobmapfile: %s.\n", global.taskname, filename3);
      goto ReadInstrProbMapFile_end;
    }



  return OK;
  
 ReadInstrProbMapFile_end:
  
  return NOT_OK;

} /* ReadInstrProbMapFile */


int ReadInstrProbMapInfo(char *filename, long extno, char *detnam, float instrmap[DET1_ROWS][DET1_PIXS]){

  int                   status=OK, anynull=0, found=NOT_OK;
  int                   inExt, totExt;
  char		        r_extname[FLEN_KEYWORD];
  char		        r_detnam[FLEN_VALUE];
  long                  dimen; 
  FitsCard_t            *card;
  FitsHeader_t	        head;
  FitsFileUnit_t        inunit=NULL; 
  

  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadInstrProbMapInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(inunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadInstrProbMapInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input instrprobmapfile */
      if (fits_get_num_hdus(inunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadInstrProbMapInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to INSTRPROBMAP extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( inunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadInstrProbMapInfo_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(inunit);
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	    strcpy(r_detnam, card->u.SVal);
	  else
	    strcpy(r_detnam, "-");

	  if( !strcmp(r_extname,KWVL_EXTNAME_INSTRPROBMAP) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", global.taskname,KWVL_EXTNAME_INSTRPROBMAP,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadInstrProbMapInfo_end;
	}
    }

  
  dimen=DET1_ROWS*DET1_PIXS;
  if( fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, instrmap[0], &anynull, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to read image\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto ReadInstrProbMapInfo_end;
    }

  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadInstrProbMapInfo_end;
    }


  return OK;

 ReadInstrProbMapInfo_end:
  return NOT_OK;

} /* ReadInstrProbMapInfo */


int WriteDET1InstrFile(float instrmap[4][DET1_ROWS][DET1_PIXS], AspHistoCompInfo_t *histocmpinfo, int histocmpcount, char *aspecthistofile, char *evtfile, ObsInfo_t *obsinfo, Det1InstrMapInfo_t *det1info, char *filename){

  int                 status=OK, hdu_count=1, count, xx, yy, kk, small_width;
  int                 ndeltaT[4], pix_on;
  /* int                 ii, jj; */
  int                 bitpix=0, naxis=0;
  long                naxes[2], group, firstelem, nelements;
  ImageInfo_t         imginfo;
  FitsFileUnit_t      inunit=NULL;
  static float        img[DET1_ROWS][DET1_PIXS];
  static float        map_bad_tot[DET1_ROWS][DET1_PIXS];
  double              *time_bad[4];
  struct gti_struct   gti;
  struct gti_struct   bpgti[4][DET_ROWS][DET_PIXS];
  struct gti_struct   merged_gti[4][DET_ROWS][DET_PIXS];
  PixPosInfo_t        pixpos[4][DET_ROWS][DET_PIXS][PIXPOS_MAXGRADE];
  float               *small_img;
  DeltaTimeInfo_t     *deltaT[4];


  /* Read GTI info from <evtfile> GTI extension */
  if(HDgti_read(evtfile, &gti, KWVL_EXTNAME_GTI, 0, 0, 0, 0, &status)){
    headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' file.\n", global.taskname, evtfile);
    goto WriteDET1InstrFile_end;
  }

  /* Read BADPIX info from input event file */
  if( ReadEvtBPdataAsGTI(evtfile, obsinfo->mjdref, bpgti) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to read BADPIX info\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: from '%s' file.\n", global.taskname, evtfile);
      goto WriteDET1InstrFile_end;
    }

  /* Merge time info from GTI and BADPIX */
  for(kk=0; kk<4; kk++){
    for(yy=0; yy<DET_ROWS; yy++){
      for(xx=0; xx<DET_PIXS; xx++){

	status = OK;
	if( HDgti_merge(GTI_AND, &merged_gti[kk][yy][xx], &bpgti[kk][yy][xx], &gti, &status) )
	  {
	    headas_chat(NORMAL,"%s: Error: Unable to get BADPIX and GTI info (%d)\n", global.taskname, status);
	    goto WriteDET1InstrFile_end;
	  }

      }
    }
  }


  /* Compute bad pixel time intervals */
  if( ComputeDeltaTimeInterval(merged_gti, deltaT, ndeltaT) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to compute bad pixel time intervals\n", global.taskname);
      goto WriteDET1InstrFile_end;
    }

/*   for(kk=0; kk<4; kk++){ */
/*     headas_chat(CHATTY,"DETID=%d\n",kk); */
/*     for(ii=0; ii<ndeltaT[kk]; ii++){ */
/*       headas_chat(CHATTY,"DELTATIME START=%f STOP=%f\n", deltaT[kk][ii].tstart, deltaT[kk][ii].tstop); */

/*       if(deltaT[kk][ii].nbp>0){ */
/* 	for(jj=0; jj<deltaT[kk][ii].nbp; jj++) */
/* 	  headas_chat(CHATTY,"BADPIX RAWX=%d RAWY=%d\n", deltaT[kk][ii].bp[jj].rawx, deltaT[kk][ii].bp[jj].rawy); */
/*       } */
/*     } */
/*   } */


  /* Read Pixel Location info from input pixposfile */
  if( ReadPixPosFile(obsinfo, pixpos) )
    {
      headas_chat(NORMAL, " %s: Error: Unable to read pixel location info\n", global.taskname);
      headas_chat(NORMAL, " %s: Error: in input pixpos file: %s.\n", global.taskname, global.par.pixposfile);
      goto WriteDET1InstrFile_end;
    }


  group=0;
  firstelem=1;

  /* Create output primary array */
  if(fits_create_file(&inunit, filename, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto WriteDET1InstrFile_end;
    }

  bitpix=8;
  naxis=0;
  naxes[0]=0;
  naxes[1]=0;

  if(fits_create_img(inunit, bitpix, naxis, naxes, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto WriteDET1InstrFile_end;
    }
  hdu_count=1;

  if(AddEvtKeywords(inunit, &global.obsinfo))
    {
      headas_chat(NORMAL,"%s: Error: Unable to update keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file (ext=%d).\n", global.taskname, filename, hdu_count);
      goto WriteDET1InstrFile_end;
    }

  /* write history */
  if(HDpar_stamp(inunit, 1, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto WriteDET1InstrFile_end;
    }

  hdu_count++;

    
  for(count=0; count<histocmpcount; count++){

    /* Compute overlap time with bad pixels */
    if( ComputeTimeBad(aspecthistofile, histocmpinfo[count].x_bin, histocmpinfo[count].y_bin, deltaT, ndeltaT, time_bad) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to compute overlap time with bad pixels for xbin=%d ybin=%d.\n",
		  global.taskname, histocmpinfo[count].x_bin, histocmpinfo[count].y_bin);
      goto WriteDET1InstrFile_end;
    }

    /* Compute total map bad */
    if(ComputeMapBadTot(deltaT, ndeltaT, time_bad, instrmap, pixpos, map_bad_tot))
      {
	headas_chat(NORMAL, "%s: Error: Unable to compute map bad values.\n", global.taskname);
	goto WriteDET1InstrFile_end;
      }


    /* compute map */
    for(yy=0; yy<DET1_ROWS; yy++){
      for(xx=0; xx<DET1_PIXS; xx++){
	
	pix_on = (instrmap[0][yy][xx]>=global.par.percent||instrmap[1][yy][xx]>=global.par.percent||instrmap[2][yy][xx]>=global.par.percent||instrmap[3][yy][xx]>=global.par.percent) ? 1 : 0 ;
	img[yy][xx] = ( pix_on * (double)histocmpinfo[count].duration ) - map_bad_tot[yy][xx] ;

	if(img[yy][xx]<0){
	  headas_chat(CHATTY, "%s: Warning: det1x=%d det1y=%d negative DET1 instrument map value (%f), setting it to zero\n", global.taskname, xx, yy, img[yy][xx]);
	  img[yy][xx] = 0;
	}
      }
    }

     
    /* Create small image to store in output file */

    small_width = det1info[count].det1size ;
    small_img = (float*)malloc(sizeof(float)*small_width*small_width);

    for(yy=0; yy<small_width; yy++){
      for(xx=0; xx<small_width; xx++){

	if( yy+det1info[count].det1y-1<0 || yy+det1info[count].det1y-1>DET1_ROWS-1 || xx+det1info[count].det1x-1<0 || xx+det1info[count].det1x-1>DET1_PIXS-1 )
	  small_img[yy*small_width+xx] = 0;
	else
	  small_img[yy*small_width+xx] = img[yy+det1info[count].det1y-1][xx+det1info[count].det1x-1] ;

      }
    }

    naxis = 2;
    naxes[0] = small_width;
    naxes[1] = small_width;
    bitpix=-32;
    nelements = small_width*small_width;

    if(fits_create_img(inunit, bitpix, naxis, naxes, &status))
      {
	headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
	headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
	goto WriteDET1InstrFile_end;
      }

    if(fits_write_img_flt(inunit, group, firstelem, nelements, small_img, &status))
      {
	headas_chat(NORMAL,"%s: Error: Unable to write image extension\n", global.taskname);
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
	goto WriteDET1InstrFile_end;
      }
    headas_chat(NORMAL, "%s: Info: Appended %d image in %s file\n", global.taskname, hdu_count-1, filename);

    if(AddEvtKeywords(inunit, &global.obsinfo))
      {
	headas_chat(NORMAL,"%s: Error: Unable to update keywords\n", global.taskname);
	headas_chat(NORMAL,"%s: Error: in '%s' file (ext=%d).\n", global.taskname, filename, hdu_count);
	goto WriteDET1InstrFile_end;
      }

    /* update current image info */
    imginfo.duration = histocmpinfo[count].duration;
    imginfo.ref_time = histocmpinfo[count].ref_time;
    imginfo.crval1 = det1info[count].det1x-1;
    imginfo.crval2 = det1info[count].det1y-1;

    if(AddImageKeyword(inunit, hdu_count, &imginfo))
      {
	headas_chat(NORMAL,"%s: Error: Unable to add keywords\n", global.taskname);
	headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
	goto WriteDET1InstrFile_end;
      }

    /* write history is parameter history set */
    if(HDpar_stamp(inunit, hdu_count, &status))
      {
	headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	goto WriteDET1InstrFile_end;
      }
    hdu_count++;


    /* Free memory */
    for(kk=0; kk<4; kk++)
      free(time_bad[kk]);

  }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto WriteDET1InstrFile_end;
    }
  

  if(fits_close_file(inunit, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto WriteDET1InstrFile_end;
    }


  return OK;

 WriteDET1InstrFile_end:
  return NOT_OK;

} /* WriteDET1InstrFile */


int AddImageKeyword(FitsFileUnit_t inunit, int hducount, ImageInfo_t *imginfo){

  int    status=OK, ival;

  if(fits_update_key(inunit, TDOUBLE, KWNM_REF_TIME, &(imginfo->ref_time), CARD_COMM_REF_TIME, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_REF_TIME, hducount);
      goto AddImageKeyword_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_DURATION, &(imginfo->duration), CARD_COMM_DURATION, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_DURATION, hducount);
      goto AddImageKeyword_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_CTYPE1, "DET1X", "X Coordinate type", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CTYPE1, hducount);
      goto AddImageKeyword_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_CUNIT1, "pixel", "WCS axis unit", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CUNIT1, hducount);
      goto AddImageKeyword_end;
    }

  ival = 0;
  if(fits_update_key(inunit, TINT, KWNM_CRPIX1, &ival, "X axis reference pixel", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CRPIX1, hducount);
      goto AddImageKeyword_end;
    }

  if(fits_update_key(inunit, TINT, KWNM_CRVAL1, &(imginfo->crval1), "coord of X ref pixel in original image", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CRVAL1, hducount);
      goto AddImageKeyword_end;
    }

  ival = 1;
  if(fits_update_key(inunit, TINT, KWNM_CDELT1, &ival, "X axis increment", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CDELT1, hducount);
      goto AddImageKeyword_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_CTYPE2, "DET1Y", "Y Coordinate type", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CTYPE2, hducount);
      goto AddImageKeyword_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_CUNIT2, "pixel", "WCS axis unit", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CUNIT2, hducount);
      goto AddImageKeyword_end;
    }

  ival = 0;
  if(fits_update_key(inunit, TINT, KWNM_CRPIX2, &ival, "Y axis reference pixel", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CRPIX2, hducount);
      goto AddImageKeyword_end;
    }

  if(fits_update_key(inunit, TINT, KWNM_CRVAL2, &(imginfo->crval2), "coord of Y ref pixel in original image", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CRVAL2, hducount);
      goto AddImageKeyword_end;
    }

  ival = 1;
  if(fits_update_key(inunit, TINT, KWNM_CDELT2, &ival, "Y axis increment", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CDELT2, hducount);
      goto AddImageKeyword_end;
    }

  
  return OK;
  
 AddImageKeyword_end:
  return NOT_OK;
  
} /* AddImageKeyword */


int ReadAlignFile(AlignInfo_t *aligninfo){

  long         extfile=-1;

  /* Derive CALDB alignment filename */  
  if ( !strcasecmp(global.par.alignfile,DF_CALDB) )
    {
      if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_ALIGN_DSET, global.par.alignfile, "type.eq.systems", &extfile, KWVL_INSTRUME_FPM, HD_DETNAM))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for alignfile parameter.\n", global.taskname);
      	  goto ReadAlignFile_end;
      	}
      extfile++;
    }

  /* Retrieve alignment info */
  if( ReadAlignInfo(global.par.alignfile, aligninfo) )
    {
      headas_chat(NORMAL, " %s: Error: unable to read alignment file.\n", global.taskname);
      goto ReadAlignFile_end;
    }


  return OK;
  
 ReadAlignFile_end:
  return NOT_OK;

} /* ReadAlignFile */


int ReadAlignInfo(char *filename, AlignInfo_t *aligninfo){

  unsigned           FromRow, ReadRows, nCols;
  int                status=OK;
  AlignCol_t         col;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

 
  headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, filename);
  /* Open readonly input align file */
  if ((unit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }
 
  /* Move in SYSTEM_ALIGNMENT extension in input align file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_SYSTEM_ALIGNMENT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_SYSTEM_ALIGNMENT);
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

/*   if ((col.Q_FB_OB=ColNameMatch(CLNM_Q_FB_OB, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_OB); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.V_FB_OB=ColNameMatch(CLNM_V_FB_OB, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_OB); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.Q_FPMA_DET1=ColNameMatch(CLNM_Q_FPMA_DET1, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FPMA_DET1); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.Q_FPMB_DET1=ColNameMatch(CLNM_Q_FPMB_DET1, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FPMB_DET1); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

  if ((col.V_FPMA_DET1=ColNameMatch(CLNM_V_FPMA_DET1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FPMA_DET1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FPMB_DET1=ColNameMatch(CLNM_V_FPMB_DET1, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FPMB_DET1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_FB_FPMA=ColNameMatch(CLNM_Q_FB_FPMA, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_FPMA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.Q_FB_FPMB=ColNameMatch(CLNM_Q_FB_FPMB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_FPMB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FB_FPMA=ColNameMatch(CLNM_V_FB_FPMA, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_FPMA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_FB_FPMB=ColNameMatch(CLNM_V_FB_FPMB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_FPMB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

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

  if ((col.V_DET2A_OB=ColNameMatch(CLNM_V_DET2A_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_DET2A_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }

  if ((col.V_DET2B_OB=ColNameMatch(CLNM_V_DET2B_OB, &table)) == -1)
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_DET2B_OB);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadAlignInfo_end;
    }


  EndBintableHeader(&head, &table);


  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;

  if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {
/*     DVECVEC_ARRAY_READ(aligninfo->Qfbob, 4, table, 0, col.Q_FB_OB); */
/*     DVECVEC_ARRAY_READ(aligninfo->Tfbob, 3, table, 0, col.V_FB_OB); */
    DVECVEC_ARRAY_READ(aligninfo->Qfbfpm0, 4, table, 0, col.Q_FB_FPMA);
    DVECVEC_ARRAY_READ(aligninfo->Qfbfpm1, 4, table, 0, col.Q_FB_FPMB);
    DVECVEC_ARRAY_READ(aligninfo->Tfbfpm0, 3, table, 0, col.V_FB_FPMA);
    DVECVEC_ARRAY_READ(aligninfo->Tfbfpm1, 3, table, 0, col.V_FB_FPMB);
    DVECVEC_ARRAY_READ(aligninfo->Qdet2Aob, 4, table, 0, col.Q_DET2A_OB);
    DVECVEC_ARRAY_READ(aligninfo->Qdet2Bob, 4, table, 0, col.Q_DET2B_OB);
    DVECVEC_ARRAY_READ(aligninfo->Tdet2Aob, 3, table, 0, col.V_DET2A_OB);
    DVECVEC_ARRAY_READ(aligninfo->Tdet2Bob, 3, table, 0, col.V_DET2B_OB);
/*     DVECVEC_ARRAY_READ(aligninfo->Qfpm0det1, 4, table, 0, col.Q_FPMA_DET1); */
    DVECVEC_ARRAY_READ(aligninfo->Tfpm0det1, 3, table, 0, col.V_FPMA_DET1);
/*     DVECVEC_ARRAY_READ(aligninfo->Qfpm1det1, 4, table, 0, col.Q_FPMB_DET1); */
    DVECVEC_ARRAY_READ(aligninfo->Tfpm1det1, 3, table, 0, col.V_FPMB_DET1);
  }

  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  

/*   /\* Move in METROLOGY_ALIGNMENT extension in input align file *\/ */
/*   if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_METROLOGY_ALIGNMENT, 0, &status)) */
/*     {  */
/*       headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_METROLOGY_ALIGNMENT); */
/*       headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); */
/*       if( CloseFitsFile(unit)) */
/* 	{ */
/* 	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname); */
/* 	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename); */
/* 	} */
/*       goto ReadAlignInfo_end; */
/*     } */
  
/*   head=RetrieveFitsHeader(unit); */

/*   GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL); */
/*   if(!table.MaxRows) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   /\* Get needed columns number from name *\/ */

/*   if ((col.Q_FB_MD0=ColNameMatch(CLNM_Q_FB_MD0, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_MD0); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.Q_FB_MD1=ColNameMatch(CLNM_Q_FB_MD1, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_FB_MD1); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.V_FB_MD0=ColNameMatch(CLNM_V_FB_MD0, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_MD0); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.V_FB_MD1=ColNameMatch(CLNM_V_FB_MD1, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_FB_MD1); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.Q_OB_ML0=ColNameMatch(CLNM_Q_OB_ML0, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_OB_ML0); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.Q_OB_ML1=ColNameMatch(CLNM_Q_OB_ML1, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Q_OB_ML1); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.V_OB_ML0=ColNameMatch(CLNM_V_OB_ML0, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_OB_ML0); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.V_OB_ML1=ColNameMatch(CLNM_V_OB_ML1, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_V_OB_ML1); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.L0_ORIG=ColNameMatch(CLNM_L0_ORIG, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_L0_ORIG); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.L1_ORIG=ColNameMatch(CLNM_L1_ORIG, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_L1_ORIG); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.L0_POINT=ColNameMatch(CLNM_L0_POINT, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_L0_POINT); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */

/*   if ((col.L1_POINT=ColNameMatch(CLNM_L1_POINT, &table)) == -1) */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_L1_POINT); */
/*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
/*       goto ReadAlignInfo_end; */
/*     } */


/*   EndBintableHeader(&head, &table); */

/*   /\* Read Bintable *\/ */
/*   FromRow = 1; */
/*   ReadRows=table.nBlockRows; */
/*   nCols=table.nColumns; */

/*   if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) { */
/*     DVECVEC_ARRAY_READ(aligninfo->Qfbmd0, 4, table, 0, col.Q_FB_MD0); */
/*     DVECVEC_ARRAY_READ(aligninfo->Qfbmd1, 4, table, 0, col.Q_FB_MD1); */
/*     DVECVEC_ARRAY_READ(aligninfo->Tfbmd0, 3, table, 0, col.V_FB_MD0); */
/*     DVECVEC_ARRAY_READ(aligninfo->Tfbmd1, 3, table, 0, col.V_FB_MD1); */
/*     DVECVEC_ARRAY_READ(aligninfo->Qobml0, 4, table, 0, col.Q_OB_ML0); */
/*     DVECVEC_ARRAY_READ(aligninfo->Qobml1, 4, table, 0, col.Q_OB_ML1); */
/*     DVECVEC_ARRAY_READ(aligninfo->Tobml0, 3, table, 0, col.V_OB_ML0); */
/*     DVECVEC_ARRAY_READ(aligninfo->Tobml1, 3, table, 0, col.V_OB_ML1); */
/*     DVECVEC_ARRAY_READ(aligninfo->L0ml, 3, table, 0, col.L0_ORIG); */
/*     DVECVEC_ARRAY_READ(aligninfo->L1ml, 3, table, 0, col.L1_ORIG); */
/*     DVECVEC_ARRAY_READ(aligninfo->D0ob, 3, table, 0, col.L0_POINT); */
/*     DVECVEC_ARRAY_READ(aligninfo->D1ob, 3, table, 0, col.L1_POINT); */
/*   } */

/*   /\* Free memory allocated with bintable data *\/ */
/*   ReleaseBintable(&head, &table); */


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
  return NOT_OK;

} /* ReadAlignInfo */


int GetMastInfo(char *filename, double time, MastInfo_t *info){

  unsigned           FromRow, ReadRows, n, nCols;
  int                status=OK, found=-1;
  double             masttstart=0, masttstop=0;
  MastExtCol_t       indxcol;
  Bintable_t	     table;
  FitsCard_t         *card;
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
 
  /* Move in MAST_ASPECT extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_MAST_ASPECT, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_MAST_ASPECT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto GetMastInfo_end;
    }
  

  head=RetrieveFitsHeader(unit);

  /* Retrieve TSTART */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      masttstart = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  /* Retrieve TSTOP */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      masttstop = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  /* Check if mastaspectfile is appropriate for input event file */

  if( global.obsinfo.tstart<masttstart || global.obsinfo.tstop>masttstop )
    headas_chat(NORMAL, "%s: Warning: event file time range not covered by %s time range (check TSTART and/or TSTOP)\n", global.taskname, filename);


  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto GetMastInfo_end;
    }


  /* Get needed columns number from name */
  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto GetMastInfo_end;
    }

  if ((indxcol.T_FBOB = GetColNameIndx(&table, CLNM_T_FBOB)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_T_FBOB);
      goto GetMastInfo_end;
    }

  if ((indxcol.Q_FBOB = GetColNameIndx(&table, CLNM_Q_FBOB)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Q_FBOB);
      goto GetMastInfo_end;
    }

 EndBintableHeader(&head, &table);

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((found<0) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows && found<0 ; ++n)
       {
 
	 if( DVEC(table,n,indxcol.TIME) >= time-TIME_SENS ){

	   info->time =  DVEC(table,n,indxcol.TIME);
	   DVECVEC_ARRAY_READ( info->Tfbob, 3, table, n, indxcol.T_FBOB);
	   DVECVEC_ARRAY_READ( info->Qfbob, 4, table, n, indxcol.Q_FBOB);

	   found = 1;
	 }
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS;
   }/* while */ 


  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      return NOT_OK;
    }


  if(found<0){
    headas_chat(NORMAL,"%s: Error: Unable to get mast info for TIME=%f\n",global.taskname,time);
    return NOT_OK;
  }


  return OK;

  
 GetMastInfo_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

} /* GetMastInfo */


int ComputeAngle1(double val[4], double *outval){

  int      i=0;
  double   angle, radians;
  AtRotMat rm;  /* rotation matrix */
  AtRotMat rm2;	/* inversed rotation matrix */
  AtQuat   Quat;

  for(i=0; i<4; i++)
    Quat[i] = val[i];
  
  if(atQuatToRM(Quat, rm)){
    headas_chat(CHATTY,"Error: ComputeAngle1: atQuatToRM() error.\n");
    return NOT_OK;
  }
  
  if(atInvRotMat(rm, rm2)){
    headas_chat(CHATTY,"Error: ComputeAngle1: atInvRotMat() error.\n");
    return NOT_OK;
  }
  
  radians = atan2(rm2[1][0],rm2[0][0]);
  angle = -radians *(180/M_PI);
  
  *outval = angle;
  
  return OK;
  
} /* ComputeAngle1 */


int ComputeAngle2(double val[4], double *outval){

  int      i=0;
  double   angle, radians;
  AtRotMat rm;  /* rotation matrix */
  AtQuat   Quat;

  for(i=0; i<4; i++)
    Quat[i] = val[i];
  
  if(atQuatToRM(Quat, rm)){
    headas_chat(CHATTY,"Error: ComputeAngle2: atQuatToRM() error.\n");
    return NOT_OK;
  }
  
  radians = atan2(rm[1][0],rm[0][0]);
  angle = -radians *(180/M_PI);
  
  *outval = angle;
  
  return OK;
  
} /* ComputeAngle2 */

int WriteCombineXformInFile(char *filename, MastInfo_t *mast, AlignInfo_t *align){

  FILE	   *file;
  int      i;
  double   v_fpm_det1[2], q_fb_fpm_angle, v_fb_fpm[2];
  double   t_fbob[2], q_fbob_angle, q_det2_ob_angle, v_det2_ob[2];
  double   q_fb_fpm[4], q_det2_ob[4], q_fbob[4];


    /* FPMA or FPMB*/
    if(!strcasecmp(global.obsinfo.instrume,KWVL_INSTRUME_FPMA)){
      
      v_fpm_det1[0] = align->Tfpm0det1[0] ;
      v_fpm_det1[1] = align->Tfpm0det1[1] ;

      v_fb_fpm[0] = align->Tfbfpm0[0] ;
      v_fb_fpm[1] = align->Tfbfpm0[1] ;

      v_det2_ob[0]  = align->Tdet2Aob[0] ;
      v_det2_ob[1]  = align->Tdet2Aob[1] ;

      for(i=0; i<4; i++)
	q_fb_fpm[i] = align->Qfbfpm0[i];

      for(i=0; i<4; i++)
	q_det2_ob[i] = align->Qdet2Aob[i];

    }
    else{

      v_fpm_det1[0] = align->Tfpm1det1[0] ;
      v_fpm_det1[1] = align->Tfpm1det1[1] ;

      v_fb_fpm[0] = align->Tfbfpm1[0] ;
      v_fb_fpm[1] = align->Tfbfpm1[1] ;

      v_det2_ob[0]  = align->Tdet2Bob[0] ;
      v_det2_ob[1]  = align->Tdet2Bob[1] ;

      for(i=0; i<4; i++)
	q_fb_fpm[i] = align->Qfbfpm1[i];

      for(i=0; i<4; i++)
	q_det2_ob[i] = align->Qdet2Bob[i];

    }

    t_fbob[0] = - mast->Tfbob[0] ;
    t_fbob[1] = - mast->Tfbob[1] ;

    for(i=0; i<4; i++)
      q_fbob[i] = mast->Qfbob[i];


    if(ComputeAngle1(q_fb_fpm, &q_fb_fpm_angle))
      goto WriteCombineXformInFile_end;

    if(ComputeAngle1(q_det2_ob, &q_det2_ob_angle))
      goto WriteCombineXformInFile_end;

    if(ComputeAngle2(q_fbob, &q_fbob_angle))
      goto WriteCombineXformInFile_end;


    if (!(file = fopen(filename, "w"))){
      headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,filename);
      goto WriteCombineXformInFile_end;
    }

    fprintf(file, "trans(%f,%f)\n", v_fpm_det1[0]/SUBPIX_SIZE_MM, v_fpm_det1[1]/SUBPIX_SIZE_MM );
    fprintf(file, "rot(%f)\n", q_fb_fpm_angle);
    fprintf(file, "trans(%f,%f)\n", v_fb_fpm[0]/SUBPIX_SIZE_MM, v_fb_fpm[1]/SUBPIX_SIZE_MM );
    fprintf(file, "trans(%f,%f)\n", t_fbob[0]/SUBPIX_SIZE_MM, t_fbob[1]/SUBPIX_SIZE_MM );
    fprintf(file, "rot(%f)\n", q_fbob_angle);
    fprintf(file, "rot(%f)\n", q_det2_ob_angle);
    fprintf(file, "trans(%f,%f)\n", v_det2_ob[0]/SUBPIX_SIZE_MM, v_det2_ob[1]/SUBPIX_SIZE_MM );

    fclose(file);


    return OK;
    
 WriteCombineXformInFile_end:
    return NOT_OK;

}  /* WriteCombineXformInFile */


int WriteSkyCombineXformInFile(char *filename, int xmin_sky, int ymin_sky, Det2InstrMapInfo_t *det2info, char *getxformfile){
  
  FILE	   *file;
  
  if (!(file = fopen(filename, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,filename);
    goto WriteSkyCombineXformInFile_end;
  }
  
  fprintf(file, "trans(%d,%d) file(%s) trans(%d,%d)\n",det2info->crval1,det2info->crval2,getxformfile, -xmin_sky ,-ymin_sky);
  
  fclose(file);
  
  
  return OK;
  
 WriteSkyCombineXformInFile_end:
  return NOT_OK;
  
}  /* WriteSkyCombineXformInFile */


int UpdateCombineXformInFile(char *filename, int det1xx, int det1yy, int det2xx, int det2yy){

  FILE	             *filein, *fileout;
  pid_t              pid;
  char               tmpfile[256];
  unsigned char      buffer[BUF_SIZE];
  int                count;

  /* Get pid */
  pid=getpid();

  if (!(filein = fopen(filename, "r"))){
    headas_chat(NORMAL, "%s: Error: Unable to open file %s\n",global.taskname,filename);
    goto UpdateCombineXformInFile_end;
  }

  sprintf(tmpfile, "%dtmp_updated_trasform.txt", (int)pid);
  if (!(fileout = fopen(tmpfile, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,tmpfile);
    goto UpdateCombineXformInFile_end;
  }

  fprintf(fileout, "trans(%d,%d)\n", det1xx, det1yy);

  while( (count=fread(buffer, sizeof(char), BUF_SIZE, filein))!=0 )
    fwrite(buffer, sizeof(char), count, fileout);

  fprintf(fileout, "trans(%d,%d)\n", det2xx, det2yy);

  
  fclose(filein);
  fclose(fileout);
  
  if ( RenameFile(tmpfile, filename) == -1 ){
    headas_chat(NORMAL, "%s: Error: Unable to update file %s\n",global.taskname, filename);
    goto UpdateCombineXformInFile_end;
  }
  

  return OK;
    
 UpdateCombineXformInFile_end:
    return NOT_OK;

} /* UpdateCombineXformInFile */


int WriteDet1ToDet2ApplyXformInFile(char *filename, int det1xx, int det1yy){
  
  FILE	   *file;

  if (!(file = fopen(filename, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,filename);
    goto WriteDet1ToDet2ApplyXformInFile_end;
  }
  
  fprintf(file, "%d %d\n", det1xx, det1yy);
  fclose(file);

  return OK;
  
 WriteDet1ToDet2ApplyXformInFile_end:
  return NOT_OK;
  
}  /* WriteDet1ToDet2ApplyXformInFile */


int ReadDet1ToDet2ApplyXformOutFile(char *filename, int *det2x, int *det2y){

  FILE	      *file;
  int         sresult;
  double      d_det2x, d_det2y, d_det1x, d_det1y;
  char        buffer[STR_LEN];     /* buffer for fgets() */


  if (!(file = fopen(filename, "r"))){
    headas_chat(NORMAL, "%s: Error: Unable to open file %s\n",global.taskname,filename);
    goto ReadDet1ToDet2ApplyXformOutFile_end;
  }

  if(fgets( buffer, STR_LEN, file))
    {
      sresult = sscanf(buffer, "%lf %lf %lf %lf",&d_det1x, &d_det1y, &d_det2x, &d_det2y);
      if(sresult!=4){
	headas_chat(NORMAL, "%s: Error: unexpected format of data in file %s\n",global.taskname,filename);
	goto ReadDet1ToDet2ApplyXformOutFile_end;
      }
      
      *det2x = (int)(10.0-d_det2x-0.5);
      *det2y = (int)(10.0-d_det2y-0.5);
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: unable to read data in file %s\n",global.taskname,filename);
      goto ReadDet1ToDet2ApplyXformOutFile_end; 
    }

  fclose(file);

  return OK;
  
 ReadDet1ToDet2ApplyXformOutFile_end:
  return NOT_OK;

} /* ReadDet1ToDet2ApplyXformOutFile */


int WriteDet2ToSkyApplyXformInFile(char *filename, Det2InstrMapInfo_t *det2info){
  
  FILE	   *file;

  if (!(file = fopen(filename, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,filename);
    goto WriteDet2ToSkyApplyXformInFile_end;
  }
  
  fprintf(file, "%d %d\n", det2info->crval1, det2info->crval2);
  fprintf(file, "%d %d\n", det2info->crval1 + det2info->naxis1 - 1, det2info->crval2);
  fprintf(file, "%d %d\n", det2info->crval1 + det2info->naxis1 - 1, det2info->crval2 + det2info->naxis2 - 1);
  fprintf(file, "%d %d\n", det2info->crval1, det2info->crval2 + det2info->naxis2 - 1);


  fclose(file);

  return OK;
  
 WriteDet2ToSkyApplyXformInFile_end:
  return NOT_OK;
  
}  /* WriteDet2ToSkyApplyXformInFile */


int ReadDet2ToSkyApplyXformOutFile(char *filename, int *xmin_sky, int *ymin_sky){

  FILE	      *file;
  int         sresult, i=0;
  double      d_det2x, d_det2y, d_skyx, d_skyy;
  double      min_skyx=1000, min_skyy=1000;
  char        buffer[STR_LEN];     /* buffer for fgets() */


  if (!(file = fopen(filename, "r"))){
    headas_chat(NORMAL, "%s: Error: Unable to open file %s\n",global.taskname,filename);
    goto ReadDet2ToSkyApplyXformOutFile_end;
  }


  while( !feof(file) && !ferror(file) && i<4 ) 
    {
      
      if(fgets( buffer, STR_LEN, file))
	{
	  sresult = sscanf(buffer, "%lf %lf %lf %lf",&d_det2x, &d_det2y, &d_skyx, &d_skyy);
	  if(sresult!=4){
	    headas_chat(NORMAL, "%s: Error: unexpected format of data in file %s\n",global.taskname,filename);
	    goto ReadDet2ToSkyApplyXformOutFile_end;
	  }
	  
	  min_skyx = MIN(min_skyx, d_skyx);
	  min_skyy = MIN(min_skyy, d_skyy);
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: unable to read data in file %s\n",global.taskname,filename);
	  goto ReadDet2ToSkyApplyXformOutFile_end; 
	}
      
      i++;
    }


  if(i!=4) {
    headas_chat(NORMAL, "%s: Error: unexpected format of data in file %s\n",global.taskname,filename);
    goto ReadDet2ToSkyApplyXformOutFile_end; 
  }

  fclose(file);
  
  *xmin_sky = (int)(min_skyx+0.5-2.0);
  *ymin_sky = (int)(min_skyy+0.5-2.0);
  

  return OK;
  
 ReadDet2ToSkyApplyXformOutFile_end:
  return NOT_OK;

} /* ReadDet2ToSkyApplyXformOutFile */


int WriteXimageInFile(char *skyinstrfile, char *expofile, int imgnum, Det1InstrMapInfo_t *det1info, char *outfile){

  int      i=0;
  FILE	   *file;

    if (!(file = fopen(outfile, "w"))){
      headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,outfile);
      goto WriteXimageInFile_end;
    }

    /* skip not good images */
    while(i<imgnum && !det1info[i].isgood)
      i++;

    /* this is the first good image */
    if(i<imgnum)
      fprintf(file, "read %s+%d;save_ima\n", skyinstrfile, i+1);


    i++;
    while(i<imgnum)
      {
	if(det1info[i].isgood)
	  fprintf(file, "read %s+%d;sum_ima;save_ima\n", skyinstrfile, i+1);

	i++;
      }

    if(global.par.skysize<SKY_MAX)
      fprintf(file, "crop/xp=%f/yp=%f/size=%d\n",global.par.skyx, global.par.skyy, global.par.skysize+10);

    fprintf(file, "write_ima/file=\"%s\"\n", expofile);
    fprintf(file, "quit\n");

    fclose(file);

    return OK;
    
 WriteXimageInFile_end:

    return NOT_OK;

}  /* WriteXimageInFile */


int CreateSkyWCS(char *outfile, int xmin_sky, int ymin_sky, int skysize){
  
  double   crpix1p=0., crpix2p=0.;
  FILE	   *file;
  
  if (!(file = fopen(outfile, "w"))){
    headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,outfile);
    goto CreateSkyWCS_end;
  }

  if(skysize<SKY_MAX){
    crpix1p = 0.0;
    crpix2p = 0.0;
  }
  else{
    crpix1p = 1.0;
    crpix2p = 1.0;
  }

  
  fprintf(file, "WCSNAMEP= 'PHYSICAL'\n");
  fprintf(file, "CTYPE1P = 'X       '           / Source of X-axis\n");
  fprintf(file, "CRPIX1P = %f / X axis reference pixel\n", crpix1p);
  fprintf(file, "CRVAL1P = %f / coord of X ref pixel in original image\n", (double)xmin_sky);
  fprintf(file, "CDELT1P = 1.000000000000000E+00 / X axis increment\n");
  fprintf(file, "CTYPE2P = 'Y       '           / Source of Y-axis\n");
  fprintf(file, "CRPIX2P = %f / Y axis reference pixel\n", crpix2p);
  fprintf(file, "CRVAL2P = %f / coord of Y ref pixel in original image\n", (double)ymin_sky);
  fprintf(file, "CDELT2P = 1.000000000000000E+00 / Y axis increment\n");
  
  fprintf(file, "CRPIX1 = %f / X axis reference pixel\n", (double)(500.5-xmin_sky+crpix1p));
  fprintf(file, "CRPIX2 = %f / Y axis reference pixel\n", (double)(500.5-ymin_sky+crpix2p));

  fclose(file);
  
  return OK;
  
 CreateSkyWCS_end:
  return NOT_OK;
  
} /* CreateSkyWCS */


int UpdateExposureKeyword(char *filename, ExposureInfo_t *info){

  int             status=OK;
  double          dvalue;
  fitsfile        *fptr=NULL;  

  /* Open input file */  
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateExposureKeyword_end;
    }
  
  if(fits_movabs_hdu(fptr, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateExposureKeyword_end;
    }

  if(AddEvtKeywords(fptr, &global.obsinfo))
    {
      headas_chat(NORMAL,"%s: Error: Unable to update keywords\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateExposureKeyword_end;
    }
  
  dvalue = info->ontime;
  if(fits_update_key(fptr, TDOUBLE, KWNM_ONTIME, &dvalue, CARD_COMM_ONTIME, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_ONTIME);
      goto UpdateExposureKeyword_end;
    }
  
  dvalue = info->livetime;
  if(fits_update_key(fptr, TDOUBLE, KWNM_LIVETIME, &dvalue, CARD_COMM_LIVETIME, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_LIVETIME);
      goto UpdateExposureKeyword_end;
    }
  
  dvalue = info->livetime;
  if(fits_update_key(fptr, TDOUBLE, KWNM_EXPOSURE, &dvalue, CARD_COMM_EXPOSURE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_EXPOSURE);
      goto UpdateExposureKeyword_end;
    }

  dvalue = info->deadc;
  if(fits_update_key(fptr, TDOUBLE, KWNM_DEADC, &dvalue, CARD_COMM_DEADC, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEADC);
      goto UpdateExposureKeyword_end;
    }
  
  /* VIGNAPP */
  if(fits_update_key(fptr, TLOGICAL, KWNM_VIGNAPP, &global.par.vignflag, CARD_COMM_VIGNAPP, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_VIGNAPP);
      headas_chat(NORMAL, "%s: Error: in '%s' temporary file.\n", global.taskname, filename);
      goto UpdateExposureKeyword_end;
    }

  /* VIGNEN */
  if(global.par.vignflag)
    {
      if(fits_update_key(fptr, TDOUBLE, KWNM_VIGNEN, &global.par.energy, CARD_COMM_VIGNEN, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_VIGNEN);
	  headas_chat(NORMAL, "%s: Error: in '%s' temporary file.\n", global.taskname, filename);
	  goto UpdateExposureKeyword_end;
	}
    }

  if(fits_update_key(fptr, TINT, KWNM_PIXBIN, &global.par.pixbin, CARD_COMM_PIXBIN, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_PIXBIN);
      goto UpdateExposureKeyword_end;
    }

  if(fits_update_key(fptr, TDOUBLE, KWNM_PERCENT, &global.par.percent, CARD_COMM_PERCENT, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_PERCENT);
      goto UpdateExposureKeyword_end;
    }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdateExposureKeyword_end;
    }
  

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdateExposureKeyword_end;
    }


  return OK;

 UpdateExposureKeyword_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);

  return NOT_OK; 
  
} /* UpdateExposureKeyword */


int AddEvtKeywords(FitsFileUnit_t inunit, ObsInfo_t *obsinfo){

  int    status=OK;

  if(fits_update_key(inunit, TSTRING, KWNM_TELESCOP, obsinfo->telescop, obsinfo->telescop_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TELESCOP);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_INSTRUME, obsinfo->instrume, obsinfo->instrume_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_INSTRUME);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_OBS_ID, obsinfo->obs_id, obsinfo->obs_id_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBS_ID);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TINT, KWNM_TARG_ID, &obsinfo->targ_id, obsinfo->targ_id_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TARG_ID);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_OBJECT, obsinfo->object, obsinfo->object_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_OBJECT);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_RA_OBJ, &obsinfo->raobj, obsinfo->raobj_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_OBJ);
      goto addevt_end;
    }
 
  if(fits_update_key(inunit, TDOUBLE, KWNM_DEC_OBJ, &obsinfo->decobj, obsinfo->decobj_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_OBJ);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_RA_NOM, &obsinfo->ranom, obsinfo->ranom_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_NOM);
      goto addevt_end;
    }
 
  if(fits_update_key(inunit, TDOUBLE, KWNM_DEC_NOM, &obsinfo->decnom, obsinfo->decnom_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_NOM);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_RA_PNT, &obsinfo->rapnt, obsinfo->rapnt_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RA_PNT);
      goto addevt_end;
    }
 
  if(fits_update_key(inunit, TDOUBLE, KWNM_DEC_PNT, &obsinfo->decpnt, obsinfo->decpnt_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DEC_PNT);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TDOUBLE, KWNM_EQUINOX, &obsinfo->equinox, obsinfo->equinox_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_EQUINOX);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_TIMESYS, obsinfo->timesys, obsinfo->timesys_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TIMESYS);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TINT, KWNM_MJDREFI, &obsinfo->mjdrefi, obsinfo->mjdrefi_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFI);
      goto addevt_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_MJDREFF, &obsinfo->mjdreff, obsinfo->mjdreff_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_MJDREFF);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_TIMEUNIT, obsinfo->timeunit, obsinfo->timeunit_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TIMEUNIT);
      goto addevt_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_TSTART, &obsinfo->tstart, obsinfo->tstart_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTART);
      goto addevt_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_TSTOP, &obsinfo->tstop, obsinfo->tstop_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TSTOP);
      goto addevt_end;
    }
  
  if(fits_update_key(inunit, TDOUBLE, KWNM_TELAPSE, &obsinfo->telapse, obsinfo->telapse_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TELAPSE);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_DATEOBS, obsinfo->dateobs, obsinfo->dateobs_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEOBS);
      goto addevt_end;
    }

  if(fits_update_key(inunit, TSTRING, KWNM_DATEEND, obsinfo->dateend, obsinfo->dateend_comm, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_DATEEND);
      goto addevt_end;
    }


  return OK;
  
 addevt_end:
  return NOT_OK;
  
} /* AddEvtKeywords */


int UpdateExposureMapWithDEADC(char *filename, double deadc){

  int                   status=OK, anynull=0, nfound;
  int                   xx, yy, xwidth, ywidth;
  long                  naxes[2], dimen;
  float                 *img;
  double                imgcorr;
  FitsFileUnit_t        inunit=NULL; 
  

  /* Open read/write file */
  if ((inunit=OpenReadWriteFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateExposureMapWithDEADC_end;
    }
  
  
  
  if (fits_read_keys_lng(inunit, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateExposureMapWithDEADC_end;
    } 
  
  xwidth = (int)naxes[0];
  ywidth = (int)naxes[1];
  
  dimen = xwidth*ywidth;
  img = (float*)malloc(sizeof(float)*dimen);
  if(img==NULL){
    headas_chat(CHATTY,"%s: Error: UpdateExposureMapWithDEADC: memory allocation failure.\n", global.taskname);
    goto UpdateExposureMapWithDEADC_end;
  }
  
  if( fits_read_img_flt(inunit, 0l, 1l, dimen,0.0, img, &anynull, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to read image\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateExposureMapWithDEADC_end;
    }
  
  
  for(yy=0; yy<ywidth; yy++){
    for(xx=0; xx<xwidth; xx++){
      imgcorr = img[yy*xwidth+xx] * deadc;
      img[yy*xwidth+xx] = (float)imgcorr; 
    }
  }

  
  if(fits_write_img_flt(inunit, 0l, 1l, dimen, img, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to update image\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateExposureMapWithDEADC_end;
    }
  
  
  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateExposureMapWithDEADC_end;
    }

  
  return OK;
  
 UpdateExposureMapWithDEADC_end:
  return NOT_OK;
  
} /* UpdateExposureMapWithDEADC */


int UpdateInstrMapKeys(char *filename){

  int             status=OK, inExt, outExt;
  char            str[FLEN_VALUE];
  fitsfile        *fptr=NULL;  


  /* Open input file */  
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateInstrMapKeys_end;
    }
  
  /* Get number of hdus in input file */
  if (fits_get_num_hdus(fptr, &inExt, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateInstrMapKeys_end;
    }
  
  /* Update all extension */
  outExt=1;

  while ( status == OK && outExt <= inExt)
    {
      if(fits_movabs_hdu(fptr, outExt, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto UpdateInstrMapKeys_end;
	}

      strcpy(str, "s");
      if(fits_update_key(fptr, TSTRING, KWNM_BUNIT, str, CARD_COMM_BUNIT, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_BUNIT);
	  goto UpdateInstrMapKeys_end;
	}

      outExt++;
    }
 
  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdateInstrMapKeys_end;
    }

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdateInstrMapKeys_end;
    }


  return OK;
  
 UpdateInstrMapKeys_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
  
  return NOT_OK; 

} /* UpdateInstrMapKeys */


int UpdateDet2InstrMapKeys(char *filename, int extnum, Det2InstrMapInfo_t *info){

  int             status=OK, ival;
  fitsfile        *fptr=NULL;  


  /* Open input file */  
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateDet2InstrMapKeys_end;
    }
   
  /* Move in extnum extension */
  if(fits_movabs_hdu(fptr, extnum, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extnum);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  
  if(fits_update_key(fptr, TSTRING, KWNM_CTYPE1, "DET2X", "X Coordinate type", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CTYPE1, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  if(fits_update_key(fptr, TSTRING, KWNM_CUNIT1, "pixel", "WCS axis unit", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CUNIT1, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  ival = 0;
  if(fits_update_key(fptr, TINT, KWNM_CRPIX1, &ival, "X axis reference pixel", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CRPIX1, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  if(fits_update_key(fptr, TINT, KWNM_CRVAL1, &(info->crval1), "coord of X ref pixel in original image", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CRVAL1, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  ival = 1;
  if(fits_update_key(fptr, TINT, KWNM_CDELT1, &ival, "X axis increment", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CDELT1, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  if(fits_update_key(fptr, TSTRING, KWNM_CTYPE2, "DET2Y", "Y Coordinate type", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CTYPE2, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  if(fits_update_key(fptr, TSTRING, KWNM_CUNIT2, "pixel", "WCS axis unit", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CUNIT2, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  ival = 0;
  if(fits_update_key(fptr, TINT, KWNM_CRPIX2, &ival, "Y axis reference pixel", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CRPIX2, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  if(fits_update_key(fptr, TINT, KWNM_CRVAL2, &(info->crval2), "coord of Y ref pixel in original image", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CRVAL2, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  ival = 1;
  if(fits_update_key(fptr, TINT, KWNM_CDELT2, &ival, "Y axis increment", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword (ext=%d)\n", global.taskname, KWNM_CDELT2, extnum);
      goto UpdateDet2InstrMapKeys_end;
    }
  
  
  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdateDet2InstrMapKeys_end;
    }

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdateDet2InstrMapKeys_end;
    }


  return OK;
  
 UpdateDet2InstrMapKeys_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
  
  return NOT_OK; 

} /* UpdateDet2InstrMapKeys */


int UpdateVignettingKeys(char *filename){

  int             status=OK, inExt, outExt;
  fitsfile        *fptr=NULL;  


  /* Open input file */  
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateVignettingKeys_end;
    }
  
  /* Get number of hdus in input file */
  if (fits_get_num_hdus(fptr, &inExt, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, filename);
      goto UpdateVignettingKeys_end;
    }
  
  /* Update all extension */
  outExt=1;

  while ( status == OK && outExt <= inExt)
    {
      if(fits_movabs_hdu(fptr, outExt, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto UpdateVignettingKeys_end;
	}
       
      /* VIGNAPP */
      if(fits_update_key(fptr, TLOGICAL, KWNM_VIGNAPP, &global.par.vignflag, CARD_COMM_VIGNAPP, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_VIGNAPP);
	  headas_chat(NORMAL, "%s: Error: in '%s' temporary file.\n", global.taskname, filename);
	  goto UpdateVignettingKeys_end;
	}

      /* VIGNEN */
      if(global.par.vignflag)
	{
	  if(fits_update_key(fptr, TDOUBLE, KWNM_VIGNEN, &global.par.energy, CARD_COMM_VIGNEN, &status))
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, KWNM_VIGNEN);
	      headas_chat(NORMAL, "%s: Error: in '%s' temporary file.\n", global.taskname, filename);
	      goto UpdateVignettingKeys_end;
	    }
	}

      outExt++;
    }
 
  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto UpdateVignettingKeys_end;
    }

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto UpdateVignettingKeys_end;
    }


  return OK;
  
 UpdateVignettingKeys_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
  
  return NOT_OK; 

} /* UpdateVignettingKeys */


int ReadVignFile(char *filename, VignInfo_t *info){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  VignCol_t          indxcol;
  Bintable_t	     table;
/*   FitsCard_t         *card; */
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

/*   if(ExistsKeyWord(&head, "THETABIN", &card)) */
/*     { */
/*       info->thetabin = card->u.DVal; */
/*       /\* from arcsec to arcmin *\/ */
/*       info->thetabin = info->thetabin/60; */
/*     } */
/*   else */
/*     { */
/*       headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, "THETABIN"); */
/*       headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename); */
/*       goto ReadVignFile_end; */
/*     } */


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
  info->energ_lo_dim = table.Multiplicity[indxcol.ENERG_LO];


  if ((indxcol.THETA = GetColNameIndx(&table, CLNM_THETA)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_THETA);
      goto ReadVignFile_end;
    }
  info->theta_dim = table.Multiplicity[indxcol.THETA];


  if ((indxcol.VIGNET = GetColNameIndx(&table, CLNM_VIGNET)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_VIGNET);
      goto ReadVignFile_end;
    }
  info->vignet_dim = table.Multiplicity[indxcol.VIGNET];


  if(info->vignet_dim != (info->energ_lo_dim*info->theta_dim))
    {
      headas_chat(NORMAL, "%s: Error: bad multiplicity of vignetting file columns.\n", global.taskname);
      goto ReadVignFile_end;
    }


 EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 info->nrows = table.MaxRows;
 info->row = (VignRow_t *)calloc(info->nrows, sizeof(VignRow_t));
 if(info==NULL){
   headas_chat(CHATTY,"%s: Error: ReadVignFile: memory allocation failure.\n", global.taskname);
   goto ReadVignFile_end;
 }

 /* Read Bintable */
 FromRow = 1;
 ReadRows=table.nBlockRows;
 nCols=table.nColumns;

 while((count<info->nrows) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
   {
     for(n=0; n<ReadRows ; ++n)
       {
	 info->row[count].azimuth = EVEC(table, n, indxcol.AZIMUTH);

	 info->row[count].energ_lo = (float*)calloc(info->energ_lo_dim, sizeof(float));
	 EVECVEC_ARRAY_READ( info->row[count].energ_lo, info->energ_lo_dim, table, n, indxcol.ENERG_LO);

	 info->row[count].theta = (float*)calloc(info->theta_dim, sizeof(float));
	 EVECVEC_ARRAY_READ( info->row[count].theta, info->theta_dim, table, n, indxcol.THETA);

	 info->row[count].vignet = (float*)calloc(info->vignet_dim, sizeof(float));
	 EVECVEC_ARRAY_READ( info->row[count].vignet, info->vignet_dim, table, n, indxcol.VIGNET);

	 count++;
       }

     FromRow += ReadRows;
     ReadRows = BINTAB_ROWS_SMALL;
   }/* while */
   
  info->nrows = count;


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


int ApplyVignetting(char *filename, VignInfo_t *vigninfo, AlignInfo_t *aligninfo){

  int             status=OK, inExt, outExt, anynull=0, nfound;
  int             xx, yy, xwidth, ywidth, xx_det2, yy_det2;
  int             idx1, idx2;
  int             crval1=0, crval2=0;
  double          x_det2, y_det2, dist_arcmin, phi;
  double          vignet_1, vignet_2, vignet;
  long            naxes[2], dimen;
  float           *img;
  FitsCard_t      *card;
  FitsHeader_t    head;
  fitsfile        *fptr=NULL;  


  /* Check instrument (FPMA or FPMB) */
  if(!strcasecmp(global.obsinfo.instrume, KWVL_INSTRUME_FPMA)){
    x_det2 = aligninfo->x_det2a;
    y_det2 = aligninfo->y_det2a;
  }
  else{
    x_det2 = aligninfo->x_det2b;
    y_det2 = aligninfo->y_det2b;    
  }


  /* Open input file */  
  if ( fits_open_file(&fptr, filename, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ApplyVignetting_end;
    }
  
  /* Get number of hdus in input file */
  if (fits_get_num_hdus(fptr, &inExt, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, filename);
      goto ApplyVignetting_end;
    }
  
  /* Update all instrument map extension */
  outExt=2;

  while ( status == OK && outExt <= inExt)
    {
      if(fits_movabs_hdu(fptr, outExt, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto ApplyVignetting_end;
	}
      
      /* Retrieve header pointer */    
      head=RetrieveFitsHeader(fptr);

      /* CRVAL1 */
      if((ExistsKeyWord(&head, KWNM_CRVAL1, &card))){
	crval1 = card->u.JVal;
      }
      else{
	headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_CRVAL1);
	headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      }

      /* CRVAL2 */
      if((ExistsKeyWord(&head, KWNM_CRVAL2, &card))){
	crval2 = card->u.JVal;
      }
      else{
	headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_CRVAL2);
	headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
      }

      /* NAXIS */
      if (fits_read_keys_lng(fptr, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto ApplyVignetting_end;
	}
      
      xwidth = (int)naxes[0];
      ywidth = (int)naxes[1];
      
      dimen = xwidth*ywidth;
      img = (float*)malloc(sizeof(float)*dimen);
      if(img==NULL){
	headas_chat(CHATTY,"%s: Error: ApplyVignetting: memory allocation failure.\n", global.taskname);
	goto ApplyVignetting_end;
      }
  
      if( fits_read_img_flt(fptr, 0l, 1l, dimen,0.0, img, &anynull, &status) )
	{
	  headas_chat(NORMAL,"%s: Error: Unable to read image\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto ApplyVignetting_end;
	}
  
      /* Apply vignetting correction to instrument map */
      for(yy=0; yy<ywidth; yy++){
	for(xx=0; xx<xwidth; xx++){

	  xx_det2 = xx + crval1 ;
	  yy_det2 = yy + crval2 ;	  

	  dist_arcmin = sqrt( pow((xx_det2 - x_det2),2) + pow((yy_det2 - y_det2),2) ) * SUBPIX_SIZE_ARCMIN;

	  phi = atan2( yy_det2-y_det2, xx_det2-x_det2 )*(180.0/M_PI) +120.;
	  phi = NormalaziedAt360Angle(phi);


	  /* Find appropriate vignetting row index ( index 'idx1' indicates the first azimuth value over 'phi' ) */

	  for(idx1=0; idx1<vigninfo->nrows; idx1++)
	    if( vigninfo->row[idx1].azimuth > phi )
	      break;
	  
	  if(idx1==vigninfo->nrows)
	    {
	      idx1 = vigninfo->nrows -1;
	      idx2 = 0;
	    }
	  else
	    {
	      idx2 = (idx1-1)>=0 ? (idx1-1) : idx1;
	    }

	  vignet_1 = ComputeVignValue(vigninfo, idx1, dist_arcmin);
	  vignet_2 = ComputeVignValue(vigninfo, idx2, dist_arcmin);	  

	  InterpolateValues((double)vigninfo->row[idx1].azimuth, (double)vigninfo->row[idx2].azimuth, phi, vignet_1, vignet_2, &vignet);

	  
	  /* Apply vignetting correction */
	  img[yy*xwidth+xx] = img[yy*xwidth+xx] * vignet; 

	}
      }

      
      if(fits_write_img_flt(fptr, 0l, 1l, dimen, img, &status))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to update image\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto ApplyVignetting_end;
	}


      outExt++;
    }
 
  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, filename);
      goto ApplyVignetting_end;
    }

  /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      goto ApplyVignetting_end;
    }


  return OK;
  
 ApplyVignetting_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
  
  return NOT_OK; 


} /* ApplyVignetting */


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


/*
 *
 *      ReadEvtBPdataAsGTI
 *
 *	DESCRIPTION:
 *           Routine to read the bad pixel extensions of the input event file
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadEvtBPdataAsGTI(char *filename, double mjdref, struct gti_struct gti[4][DET_ROWS][DET_PIXS]){

  int                    bpExt=-1;
  static BadPixExtData_t inbp[DET_PIXS][DET_ROWS];
  BOOL                   extfound;
  short int              nobadflag=0;


  /* 'nobadflag' may be used to exclude BADPIX rows with the following BADFLAG bit set to 1 */
  nobadflag = 0;


  /* Read BADPIX ext for DET0 */
  if(ReadEvtBadPixExt(inbp,filename, KWVL_DETNAM_DET0, nobadflag, &extfound, &bpExt))
    return NOT_OK;

  /* Sort by TIME values */
  SortBadPixExtDatabyTIME(inbp);

  if( SetGTIfromBADPIX(inbp, mjdref, gti[0]) )
    return NOT_OK;


  /* Read BADPIX ext for DET1 */
  if(ReadEvtBadPixExt(inbp,filename, KWVL_DETNAM_DET1, nobadflag, &extfound, &bpExt))
    return NOT_OK;

  /* Sort by TIME values */
  SortBadPixExtDatabyTIME(inbp);

  if( SetGTIfromBADPIX(inbp, mjdref, gti[1]) )
    return NOT_OK;


  /* Read BADPIX ext for DET2 */
  if(ReadEvtBadPixExt(inbp,filename, KWVL_DETNAM_DET2, nobadflag, &extfound, &bpExt))
    return NOT_OK;

  /* Sort by TIME values */
  SortBadPixExtDatabyTIME(inbp);

  if( SetGTIfromBADPIX(inbp, mjdref, gti[2]) )
    return NOT_OK;


  /* Read BADPIX ext for DET3 */
  if(ReadEvtBadPixExt(inbp,filename, KWVL_DETNAM_DET3, nobadflag, &extfound, &bpExt))
    return NOT_OK;

  /* Sort by TIME values */
  SortBadPixExtDatabyTIME(inbp);

  if( SetGTIfromBADPIX(inbp, mjdref, gti[3]) )
    return NOT_OK;


  return OK;

} /* ReadEvtBPdataAsGTI */


int CompareBPInfobyTIME(const void *aa, const void *bb){

   const BadPixExtInfo_t *a, *b;

   a = (BadPixExtInfo_t *) aa;
   b = (BadPixExtInfo_t *) bb;
   
   if(a->time > b->time)
     return 1;
   else if (a->time < b->time)
     return -1;
   else
     return 0;
   
 } /* CompareBPInfobyTIME  */


/*
 *  SortBadPixExtData
 *
 */
void SortBadPixExtDatabyTIME(BadPixExtData_t bp[DET_PIXS][DET_ROWS]){

  int      i,j;

  for(i=0; i<DET_PIXS; i++){
    for(j=0; j<DET_ROWS; j++){
      if(bp[i][j].info!=NULL && bp[i][j].ninfo>0)
	qsort( bp[i][j].info, bp[i][j].ninfo, sizeof(BadPixExtInfo_t), (int(*)(const void *, const void *))CompareBPInfobyTIME );
    }
  }

} /* SortBadPixExtDatabyTIME  */


int SetGTIfromBADPIX(BadPixExtData_t inbp[DET_PIXS][DET_ROWS], double mjdref, struct gti_struct gti[DET_ROWS][DET_PIXS]){

  int                i, j, k;
  int                gtinum, gtitot;


  for(i=0; i<DET_ROWS; i++){
    for(j=0; j<DET_PIXS; j++){

      gtitot = inbp[j][i].ninfo;

      /* Initialize GTI */
      HDgti_init(&gti[i][j]);
      
      gti[i][j].mjdref = mjdref;
      gti[i][j].timezero = 0;
      gti[i][j].ngti = gtitot;
      gti[i][j].maxgti = gtitot;

      if(gtitot<=0)
	continue;
      
      /* Start filling in output GTI structure */
      gti[i][j].start = (double *)malloc(2*sizeof(double)*gtitot);
      if (gti[i][j].start == 0) 
	return NOT_OK;

      gti[i][j].dptr = (void *) gti[i][j].start;
      gti[i][j].stop = gti[i][j].start + gtitot;


      /* Write gti values in gti struct */
      gtinum = 1;
      gti[i][j].start[0] = inbp[j][i].info[0].time;
      gti[i][j].stop[0] = inbp[j][i].info[0].time_stop;

      /* headas_chat(NORMAL,"DEBUG RAWX=%d RAWY=%d k=%d gtinum=%d TIME=%f TIME_STOP=%f\n",i,j,0,gtinum,gti[i][j].start[gtinum-1],gti[i][j].stop[gtinum-1]); */

      for(k=1; k<gtitot; k++){

	if(inbp[j][i].info[k].time > gti[i][j].stop[gtinum-1])
	  {
	    gti[i][j].start[gtinum] = inbp[j][i].info[k].time;
	    gti[i][j].stop[gtinum] = inbp[j][i].info[k].time_stop;
	    
	    gtinum++;
	  }
	else
	  {
	    if(inbp[j][i].info[k].time_stop > gti[i][j].stop[gtinum-1])
	      gti[i][j].stop[gtinum-1] = inbp[j][i].info[k].time_stop;
	  }

	/* headas_chat(NORMAL,"DEBUG RAWX=%d RAWY=%d k=%d gtinum=%d TIME=%f TIME_STOP=%f\n",i,j,k,gtinum,gti[i][j].start[gtinum-1],gti[i][j].stop[gtinum-1]); */
      }

      /* Update GTI structure ending info */      
      gti[i][j].ngti = gtinum;
      gti[i][j].maxgti = gtinum;
      
    }
  }

  return OK;

} /* SetGTIfromBADPIX */


int ComputeTimeBad(char *aspecthistofile, int this_xbin, int this_ybin, DeltaTimeInfo_t *deltaT[4], int ndeltaT[4], double *time_bad[4]){

  unsigned           FromRow, ReadRows, n, nCols;
  int                status=OK, kk, ideltaT;
  int                x_bin, y_bin;
  double             tstart, tstop, dtime=0.0;
  AspHistoCol_t      indxcol;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     unit=NULL;
  
  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );


  /* Allocate memory and initialize time_bad array */
  for(kk=0; kk<4; kk++){
    time_bad[kk] = (double*)calloc(ndeltaT[kk], sizeof(double)); /* allocate memory and set values to zero */
    if(time_bad[kk]==NULL){
      headas_chat(CHATTY,"%s: Error: ComputeTimeBad: memory allocation failure.\n", global.taskname);
      goto ComputeTimeBad_end;
    }
  }


  /* Open readonly input file */
  if ((unit=OpenReadFitsFile(aspecthistofile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, aspecthistofile);
      return NOT_OK;
    }
  
  /* Move in ASP_HIST extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_ASP_HIST, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_ASP_HIST);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, aspecthistofile); 
      goto ComputeTimeBad_end;
    }
  
  /* Retrieve header pointer */
  head=RetrieveFitsHeader(unit);
  
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, aspecthistofile);
    }
  
  /* Get needed columns number from name */
  if ((indxcol.X_BIN = GetColNameIndx(&table, CLNM_X_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_X_BIN);
      goto ComputeTimeBad_end;
    }
  
  if ((indxcol.Y_BIN = GetColNameIndx(&table, CLNM_Y_BIN)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_Y_BIN);
      goto ComputeTimeBad_end;
    }

  if ((indxcol.TSTART = GetColNameIndx(&table, CLNM_TSTART)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TSTART);
      goto ComputeTimeBad_end;
    }

  if ((indxcol.TSTOP = GetColNameIndx(&table, CLNM_TSTOP)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TSTOP);
      goto ComputeTimeBad_end;
    }
  
  EndBintableHeader(&head, &table);
  
  
  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;
  
  while(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for(n=0; n<ReadRows ; n++)
	{
	  /* NOTE! Bin number in input file start from 1 while in histoinfo start from 0 */
	  x_bin = JVEC(table,n,indxcol.X_BIN)-1;
	  y_bin = JVEC(table,n,indxcol.Y_BIN)-1;
	  
	  if( x_bin==this_xbin && y_bin==this_ybin )
	    {
	      
	      tstart = DVEC(table,n,indxcol.TSTART);
	      tstop = DVEC(table,n,indxcol.TSTOP);
	      

	      for(kk=0; kk<4; kk++){

		if(ndeltaT[kk]>0){

		  FindDeltaTimeIndex( deltaT[kk], ndeltaT[kk], tstart, &ideltaT );

		  while(deltaT[kk][ideltaT].tstart<tstop && ideltaT<ndeltaT[kk]){    

		    if(deltaT[kk][ideltaT].tstart<=tstart)
		      dtime = MIN(deltaT[kk][ideltaT].tstop-tstart,tstop-tstart) ;
		    else if(deltaT[kk][ideltaT].tstop>tstop)
		      dtime = tstop - deltaT[kk][ideltaT].tstart ;
		    else
		      dtime = deltaT[kk][ideltaT].tstop - deltaT[kk][ideltaT].tstart ;
	
		    if(dtime>0)
		      time_bad[kk][ideltaT] += dtime;


		    ideltaT++;
		  }

		}

	      }

	  
	    } /* if( x_bin==this_xbin && y_bin==this_ybin ) */
      
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);
  
  /* Close file */
  if (CloseFitsFile(unit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, aspecthistofile);
      return NOT_OK;
    }


  return OK;
  
 ComputeTimeBad_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

} /* ComputeTimeBad */


/*
 *
 *      ReadPixPosFile
 *
 *	DESCRIPTION:
 *           Routine to read input pixposfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadPixPosFile(ObsInfo_t *obsinfo, PixPosInfo_t pixpos[4][DET_ROWS][DET_PIXS][PIXPOS_MAXGRADE]){
  

  char        pixposfile0[PIL_LINESIZE], pixposfile1[PIL_LINESIZE], pixposfile2[PIL_LINESIZE], pixposfile3[PIL_LINESIZE];
  long        extfile0=-1, extfile1=-1, extfile2=-1, extfile3=-1;


  /* Derive CALDB gain filename */  
  if ( !strcasecmp(global.par.pixposfile,DF_CALDB) )
    {
      /* Retrieve DET0 pixpos filename */
      if (CalGetFileName(HD_MAXRET, obsinfo->dateobs, obsinfo->timeobs, obsinfo->dateend, obsinfo->timeend, KWVL_PIXPOS_DSET, pixposfile0, HD_EXPR, &extfile0, obsinfo->instrume, KWVL_DETNAM_DET0))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for pixposfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET0);
      	  goto ReadPixPosFile_end;
      	}
      extfile0++;

      /* Retrieve DET1 pixpos filename */
      if (CalGetFileName(HD_MAXRET, obsinfo->dateobs, obsinfo->timeobs, obsinfo->dateend, obsinfo->timeend, KWVL_PIXPOS_DSET, pixposfile1, HD_EXPR, &extfile1, obsinfo->instrume, KWVL_DETNAM_DET1))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for pixposfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET1);
      	  goto ReadPixPosFile_end;
      	}
      extfile1++;

      /* Retrieve DET2 pixpos filename */
      if (CalGetFileName(HD_MAXRET, obsinfo->dateobs, obsinfo->timeobs, obsinfo->dateend, obsinfo->timeend, KWVL_PIXPOS_DSET, pixposfile2, HD_EXPR, &extfile2, obsinfo->instrume, KWVL_DETNAM_DET2))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for pixposfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET2);
      	  goto ReadPixPosFile_end;
      	}
      extfile2++;

      /* Retrieve DET3 pixpos filename */
      if (CalGetFileName(HD_MAXRET, obsinfo->dateobs, obsinfo->timeobs, obsinfo->dateend, obsinfo->timeend, KWVL_PIXPOS_DSET, pixposfile3, HD_EXPR, &extfile3, obsinfo->instrume, KWVL_DETNAM_DET3))
      	{
      	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB for pixposfile parameter (detector %s).\n", global.taskname, KWVL_DETNAM_DET3);
      	  goto ReadPixPosFile_end;
      	}
      extfile3++;

    }
  else{
    strcpy(pixposfile0, global.par.pixposfile);
    strcpy(pixposfile1, global.par.pixposfile);
    strcpy(pixposfile2, global.par.pixposfile);
    strcpy(pixposfile3, global.par.pixposfile);
  }

  /* Retrieve DET0 pixpos info */    
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for pixpos data of detector %s.\n", global.taskname, pixposfile0, KWVL_DETNAM_DET0);
  if( ReadPixPosInfo(pixpos[0], pixposfile0, extfile0, KWVL_DETNAM_DET0) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read pixpos info\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input pixpos file: %s.\n", global.taskname, pixposfile0);
      goto ReadPixPosFile_end;
    }

  /* Retrieve DET1 pixpos info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for pixpos data of detector %s.\n", global.taskname, pixposfile1, KWVL_DETNAM_DET1);
  if( ReadPixPosInfo(pixpos[1], pixposfile1 ,extfile1, KWVL_DETNAM_DET1) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read pixpos info\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input pixpos file: %s.\n", global.taskname, pixposfile1);
      goto ReadPixPosFile_end;
    }

  /* Retrieve DET2 pixpos info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for pixpos data of detector %s.\n", global.taskname, pixposfile2, KWVL_DETNAM_DET2);
  if( ReadPixPosInfo(pixpos[2], pixposfile2, extfile2, KWVL_DETNAM_DET2) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read pixpos info\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input pixpos file: %s.\n", global.taskname, pixposfile2);
      goto ReadPixPosFile_end;
    }

  /* Retrieve DET3 pixpos info */ 
  headas_chat(NORMAL, "%s: Info: Processing '%s' file for pixpos data of detector %s.\n", global.taskname, pixposfile3, KWVL_DETNAM_DET3);
  if( ReadPixPosInfo(pixpos[3], pixposfile3, extfile3, KWVL_DETNAM_DET3) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to read pixpos info\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in input pixpos file: %s.\n", global.taskname, pixposfile3);
      goto ReadPixPosFile_end;
    }


  return OK;
  
 ReadPixPosFile_end:
  
  return NOT_OK;

} /* ReadPixPosFile */


/*
 *
 *      ReadPixPosInfo
 *
 *	DESCRIPTION:
 *           Routine to get pixpos info from input pixposfile
 *               
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ReadPixPosInfo(PixPosInfo_t pixpos[DET_ROWS][DET_PIXS][PIXPOS_MAXGRADE], char *filename, long extno, char *detnam){

  int                n, i=0, j=0, g=0, status=OK, found=NOT_OK;
  int                inExt, totExt, rawx, rawy, grade;
  char		     r_extname[FLEN_KEYWORD];
  char		     r_detnam[FLEN_VALUE];
  PixPosCol_t        pixposcol;
  unsigned           FromRow, ReadRows, nCols;
  FitsCard_t         *card;
  Bintable_t	     table;
  FitsHeader_t	     head;
  FitsFileUnit_t     gunit=NULL;

  TMEMSET0( &table, Bintable_t );
  TMEMSET0( &head, FitsHeader_t );

  for(i=0; i<DET_ROWS; i++){
    for(j=0; j<DET_PIXS; j++){
      for(g=0; g<PIXPOS_MAXGRADE; g++){
	pixpos[i][j][g].ref_det1x = -1;
	pixpos[i][j][g].ref_det1y = -1;
	pixpos[i][j][g].pdf = NULL;
      }
    }
  }


  /* Open read only pixpos file */
  if ((gunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }


  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(gunit,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 
	  goto ReadPixPosInfo_end;
	}
    }
  else
    {
      /* Get number of hdus in input pixposfile */
      if (fits_get_num_hdus(gunit, &totExt, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
	  headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, filename);
	  goto ReadPixPosInfo_end;
	}

      inExt=1;
      status=OK;
      found=NOT_OK;
     /* Move to PIXPOS extension with DETNAM=<detnam> */
      while ( found==NOT_OK && status==OK && inExt<=totExt ) 
	{
	  if(fits_movabs_hdu( gunit, inExt, NULL, &status ))
	    { 
	      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	      goto ReadPixPosInfo_end;
	    }
      
	  /* Retrieve header pointer */    
	  head=RetrieveFitsHeader(gunit);    
	  
	  if(ExistsKeyWord(&head, KWNM_EXTNAME, &card))
	    strcpy(r_extname, card->u.SVal);
	  else
	    strcpy(r_extname, "NONAME");
	  
	  if(ExistsKeyWord(&head, KWNM_DETNAM, &card))
	    strcpy(r_detnam, card->u.SVal);
	  else
	    strcpy(r_detnam, "-");

	  if( !strcmp(r_extname,KWVL_EXTNAME_PIXPOS) && !strcmp(r_detnam,detnam) ){
	    found=OK;
	  }

	  inExt++;
	}

      if(found == NOT_OK)
	{
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension with DETNAM='%s'\n", global.taskname,KWVL_EXTNAME_PIXPOS,detnam);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, filename); 	  
	  goto ReadPixPosInfo_end;
	}
    }


  head = RetrieveFitsHeader(gunit);
  
  /* Read pixpos bintable */
  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  nCols=table.nColumns;
  
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }


  /* Get columns index from name */


  if ((pixposcol.RAWX = GetColNameIndx(&table, CLNM_RAWX)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.RAWY = GetColNameIndx(&table, CLNM_RAWY)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.GRADE = GetColNameIndx(&table, CLNM_GRADE)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GRADE);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.REF_DET1X = GetColNameIndx(&table, CLNM_REF_DET1X)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_REF_DET1X);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.REF_DET1Y = GetColNameIndx(&table, CLNM_REF_DET1Y)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_REF_DET1Y);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }

  if ((pixposcol.PDF = GetColNameIndx(&table, CLNM_PDF)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PDF);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
      goto ReadPixPosInfo_end;
    }


  /* Read blocks of bintable rows */
  FromRow=1; 
  ReadRows = table.nBlockRows;
  while (ReadBintable(gunit, &table, nCols, NULL, FromRow, &ReadRows) == 0)
    {
      for (n=0; n<ReadRows; ++n)
	{
	  rawx = BVEC(table,n,pixposcol.RAWX);
	  rawy = BVEC(table,n,pixposcol.RAWY);
	  grade = IVEC(table,n,pixposcol.GRADE);

	  if(rawx<RAWX_MIN||rawx>RAWX_MAX||rawy<RAWY_MIN||rawy>RAWY_MAX){
	    headas_chat(NORMAL,"%s: Error: RAWX=%d RAWY=%d out of range values.\n", global.taskname, rawx, rawy);
	    goto ReadPixPosInfo_end;
	  }

	  if(grade>=PIXPOS_MAXGRADE){
	    headas_chat(NORMAL,"%s: Error: GRADE=%d out of range values.\n", global.taskname, grade);
	    goto ReadPixPosInfo_end;
	  }

	  pixpos[rawy][rawx][grade].ref_det1x = IVEC(table,n,pixposcol.REF_DET1X);	  
	  pixpos[rawy][rawx][grade].ref_det1y = IVEC(table,n,pixposcol.REF_DET1Y);

	  /* exclude pdf with REF_DET1X=-1 or REF_DET1Y=-1 */
	  if( pixpos[rawy][rawx][grade].ref_det1x>=0 && pixpos[rawy][rawx][grade].ref_det1y>=0){

	    /* pixpos[rawy][rawx][grade].pdf = (float*)malloc(PIXPOS_PDF_DIM*sizeof(float)); */
	    pixpos[rawy][rawx][grade].pdf = realloc( pixpos[rawy][rawx][grade].pdf, PIXPOS_PDF_DIM*sizeof(float) );

	    if(pixpos[rawy][rawx][grade].pdf==NULL){
	      headas_chat(CHATTY,"%s: Error: ReadPixPosInfo: memory allocation failure.\n", global.taskname);
	      goto ReadPixPosInfo_end;
	    }

	    for (j=0; j< PIXPOS_PDF_DIM; j++)
	      pixpos[rawy][rawx][grade].pdf[j] = EVECVEC(table,n,pixposcol.PDF,j);
	  }

	}
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }
  /* Free memory allocated with bintable data */
  ReleaseBintable(&head, &table);


  /* Close file */
  if (CloseFitsFile(gunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadPixPosInfo_end;
    }


  /* Check reference pixel values */
  for(i=0; i<DET_ROWS; i++){
    for(j=0; j<DET_PIXS; j++){
      for(g=0; g<PIXPOS_MAXGRADE; g++){      

	if(pixpos[i][j][g].ref_det1x-1<DET1X_MIN||pixpos[i][j][g].ref_det1x-1>DET1X_MAX||pixpos[i][j][g].ref_det1y-1<DET1Y_MIN||pixpos[i][j][g].ref_det1y-1>DET1Y_MAX)
	  {

	    if(pixpos[i][j][g].ref_det1x!=-1&&pixpos[i][j][g].ref_det1y!=-1)
	      headas_chat(NORMAL,"%s: Warning: RAWX=%d RAWY=%d DETNAM='%s' GRADE=%d - DET1X=%d DET1Y=%d out of range in pixel location file.\n",
			  global.taskname, j, i, detnam, g, pixpos[i][j][g].ref_det1x, pixpos[i][j][g].ref_det1y);
	  }

      }
    }
  }


  return OK;
  
 ReadPixPosInfo_end:
  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;

 
}  /* ReadPixPosInfo */


int CheckAttitude(char *filename){

  int                status=OK;
  double             tstart=0, tstop=0;
  FitsHeader_t	     head;
  FitsCard_t         *card;
  FitsFileUnit_t     inunit=NULL;

  TMEMSET0( &head, FitsHeader_t );


  /* Open readonly input file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
 
  /* Move in ATTITUDE extension in input file */
  if (fits_movnam_hdu(inunit, ANY_HDU, KWVL_EXTNAME_ATTITUDE, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_ATTITUDE);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto CheckAttitude_end;
    }
  
  
  head=RetrieveFitsHeader(inunit);

  /* Retrieve TSTART */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      tstart = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  /* Retrieve TSTOP */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      tstop = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }


  /* Check if attitude file is appropriate for input event file */

  if( global.obsinfo.tstart<tstart || global.obsinfo.tstop>tstop )
    headas_chat(NORMAL, "%s: Warning: event file time range not covered by %s time range (check TSTART and/or TSTOP)\n", global.taskname, filename);


  /* close input file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      return NOT_OK;
    }

  return OK;
  
 CheckAttitude_end:

  CloseFitsFile(inunit);
  return NOT_OK;

} /* CheckAttitude */


int CheckDet1Ref(char *filename){

  int                status=OK;
  double             tstart=0, tstop=0;
  char               instrume[FLEN_VALUE];
  FitsHeader_t	     head;
  FitsCard_t         *card;
  FitsFileUnit_t     inunit=NULL;

  TMEMSET0( &head, FitsHeader_t );


  /* Open readonly input file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      return NOT_OK;
    }
 
  /* Move in DET1_REFPOINT extension in input file */
  if (fits_movnam_hdu(inunit, ANY_HDU, KWVL_EXTNAME_DET1_REFPOINT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, KWVL_EXTNAME_DET1_REFPOINT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename); 
      goto CheckDet1Ref_end;
    }
  
  
  head=RetrieveFitsHeader(inunit);

  /* Retrieve TSTART */
  if((ExistsKeyWord(&head, KWNM_TSTART, &card)))
    {
      tstart = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTART);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  /* Retrieve TSTOP */
  if((ExistsKeyWord(&head, KWNM_TSTOP, &card)))
    {
      tstop = card->u.DVal;
    }
  else
    {
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_TSTOP);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }

  
  /* Retrieve INSTRUME */
  if((ExistsKeyWord(&head, KWNM_INSTRUME, &card)))
    {
      strcpy(instrume, card->u.SVal);
    }
  else
    {
      strcpy(instrume, "\0");
      headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname,KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, filename);
    }


  /* Check if det1ref file is appropriate for input event file */

  if( global.obsinfo.tstart<tstart || global.obsinfo.tstop>tstop )
    headas_chat(NORMAL, "%s: Warning: event file time range not covered by %s time range (check TSTART and/or TSTOP)\n", global.taskname, filename);

  if( strcasecmp(global.obsinfo.instrume,instrume) )
    headas_chat(NORMAL, "%s: Warning: INSTRUME keywords of %s file and event file are not consistent\n", global.taskname, filename);


  /* close input file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, filename);
      return NOT_OK;
    }

  return OK;
  
 CheckDet1Ref_end:

  CloseFitsFile(inunit);
  return NOT_OK;

} /* CheckDet1Ref */


double NormalaziedAt360Angle(double angle){

  double outangle;
  
  outangle = fmod(angle,360.0);
  outangle = outangle>=0 ? outangle : outangle+360;

  return outangle;

} /* NormalaziedAt360Angle */


double ComputeVignValue(VignInfo_t *vigninfo, int vignrow, double dist_arcmin){

  int             theta_imax=0, theta_imin=0, en_i=0;
  double          vignet_1=0., vignet_2=0., vignet=0.;


  /* Find appropriate theta index ( index 'theta_imax' indicates the first theta value over 'dist_arcmin' ) */
  for(theta_imax=0; theta_imax<vigninfo->theta_dim; theta_imax++){
    if( vigninfo->row[vignrow].theta[theta_imax] > dist_arcmin )
      break;
  }
  theta_imin = theta_imax-1>0 ? theta_imax-1 : 0 ;
  
  /* Case dist_arcmin > theta[vigninfo->theta_dim-1]  */
  theta_imax = MIN(theta_imax, vigninfo->theta_dim-1);
  
  
  /* Find appropriate energ_lo index ( index 'en_i' indicates the first energ_lo value over energy input parameter value ) */
  for(en_i=0; en_i<vigninfo->energ_lo_dim; en_i++){
    if( vigninfo->row[vignrow].energ_lo[en_i] > global.par.energy )
      break;
  }
  en_i = en_i-1>0 ? en_i-1 : 0;
  
  vignet_1 = vigninfo->row[vignrow].vignet[ theta_imin*vigninfo->energ_lo_dim + en_i ];
  vignet_2 = vigninfo->row[vignrow].vignet[ theta_imax*vigninfo->energ_lo_dim + en_i ];	  
  
  InterpolateValues((double)vigninfo->row[vignrow].theta[theta_imin], (double)vigninfo->row[vignrow].theta[theta_imax], dist_arcmin, vignet_1, vignet_2, &vignet);
  
  return vignet;

} /* ComputeVignValue */


int ReadSkyDetFile(char *filename, SkyDetInfo_t **info, int *ninfo){

  unsigned           FromRow, ReadRows, n, nCols;
  int                count=0, status=OK;
  SkyDetCol_t        indxcol;
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
  
  /* Move in DET_COORD extension in input file */
  if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_DET_COORD, 0, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_DET_COORD);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
      goto ReadSkyDetFile_end;
    }

  /* Retrieve header pointer */  
  head=RetrieveFitsHeader(unit);

  GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
  if(!table.MaxRows)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto ReadSkyDetFile_end;
    }

  /* Get needed columns number from name */
  
  if ((indxcol.TIME = GetColNameIndx(&table, CLNM_TIME)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_TIME);
      goto ReadSkyDetFile_end;
    }
  
  if ((indxcol.DET1X = GetColNameIndx(&table, CLNM_DET1X)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DET1X);
      goto ReadSkyDetFile_end;
    }
  
  if ((indxcol.DET1Y = GetColNameIndx(&table, CLNM_DET1Y)) == -1 )
    {
      headas_chat(NORMAL, "%s: Error: unable to find '%s' column\n", global.taskname, CLNM_DET1Y);
      goto ReadSkyDetFile_end;
    }


  EndBintableHeader(&head, &table);


 /* Allocate memory to storage all data */
 *ninfo = table.MaxRows;
 *info = (SkyDetInfo_t *)calloc(*ninfo, sizeof(SkyDetInfo_t));
 if(*info==NULL){
   headas_chat(CHATTY,"%s: Error: ReadSkyDetFile: memory allocation failure.\n", global.taskname);
   goto ReadSkyDetFile_end;
 }

  
  /* Read Bintable */
  FromRow = 1;
  ReadRows=table.nBlockRows;
  nCols=table.nColumns;
  
  while((count<*ninfo) && (ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0))
    {
      for(n=0; n<ReadRows ; ++n)
	{
	  (*info)[count].time = DVEC(table,n,indxcol.TIME);
	  (*info)[count].det1x = IVEC(table,n,indxcol.DET1X);
	  (*info)[count].det1y = IVEC(table,n,indxcol.DET1Y);
	 
	  count++;
	}
      
      FromRow += ReadRows;
      ReadRows = BINTAB_ROWS;
    }/* while */ 
  
  *ninfo = count;
  
  
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

  
 ReadSkyDetFile_end:

  CloseFitsFile(unit);

  if (head.first)
    ReleaseBintable(&head, &table);
  
  return NOT_OK;


} /* ReadSkyDetFile */



void FindClosestSkyDetIndex(SkyDetInfo_t *info, int nrows, double time, int *index){
  
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
   
} /* FindClosestSkyDetIndex */


void FindDeltaTimeIndex(DeltaTimeInfo_t *info, int nrows, double time, int *index){
  
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
   
} /* FindDeltaTimeIndex */

int CompareTimes(const void *aa, const void *bb){

   const double *a, *b;

   a = (double *) aa;
   b = (double *) bb;
   
   if(*a > *b)
     return 1;
   else if (*a < *b)
     return -1;
   else
     return 0;
   
 } /* CompareTimes  */


int ComputeDeltaTimeInterval(struct gti_struct bpgti[4][DET_ROWS][DET_PIXS], DeltaTimeInfo_t *deltaT[4], int *ndeltaT){

  int                xx, yy, kk, i, ideltaT;
  int                count, totbp, ntimes;
  double             *times;


  for(kk=0; kk<4; kk++){
    
    totbp=0;
    
    for(yy=0; yy<DET_ROWS; yy++){
      for(xx=0; xx<DET_PIXS; xx++){
	
	if(bpgti[kk][yy][xx].ngti>0)
	  totbp += bpgti[kk][yy][xx].ngti ;
      }
    }


    ntimes = totbp*2;  /* note: 2 time values for each bp */
    times = (double*)malloc(sizeof(double)*ntimes);
    if(times==NULL){
      headas_chat(CHATTY,"%s: Error: ComputeDeltaTimeInterval: memory allocation failure.\n", global.taskname);
      goto ComputeDeltaTimeInterval_end;
    }

    count=0;

    for(yy=0; yy<DET_ROWS; yy++){
      for(xx=0; xx<DET_PIXS; xx++){
	
	for(i=0; i<bpgti[kk][yy][xx].ngti; i++){

	  times[count] = bpgti[kk][yy][xx].start[i];
	  times[count+1] = bpgti[kk][yy][xx].stop[i];
	  count +=2;
	}
	
      }
    }

    qsort( times, ntimes, sizeof(double), (int(*)(const void *, const void *))CompareTimes );
   

    ndeltaT[kk] = ntimes-1>0 ?  ntimes-1 : 0; 
    deltaT[kk] = (DeltaTimeInfo_t *)malloc( ndeltaT[kk] * sizeof(DeltaTimeInfo_t) );
    if(deltaT[kk]==NULL){
      headas_chat(CHATTY,"%s: Error: ComputeDeltaTimeInterval: memory allocation failure.\n", global.taskname);
      goto ComputeDeltaTimeInterval_end;
    }


    for(i=0; i<ntimes-1; i++){
      deltaT[kk][i].tstart = times[i];
      deltaT[kk][i].tstop = times[i+1];
      deltaT[kk][i].nbp = 0;
      deltaT[kk][i].bp = NULL;
    }


    for(yy=0; yy<DET_ROWS; yy++){
      for(xx=0; xx<DET_PIXS; xx++){
	
	for(i=0; i<bpgti[kk][yy][xx].ngti; i++){

	  FindDeltaTimeIndex( deltaT[kk], ndeltaT[kk], bpgti[kk][yy][xx].start[i], &ideltaT );


	  /* each badpix may be included in more than one deltaT  */
	  while( deltaT[kk][ideltaT].tstart<bpgti[kk][yy][xx].stop[i] && ideltaT<ndeltaT[kk]){

	  deltaT[kk][ideltaT].nbp +=1;
	  deltaT[kk][ideltaT].bp = (DeltaTimeBPInfo_t*) realloc( deltaT[kk][ideltaT].bp, ( deltaT[kk][ideltaT].nbp * sizeof(DeltaTimeBPInfo_t)) );

	  deltaT[kk][ideltaT].bp[ deltaT[kk][ideltaT].nbp - 1 ].rawx = xx ;
	  deltaT[kk][ideltaT].bp[ deltaT[kk][ideltaT].nbp - 1 ].rawy = yy ;

	  ideltaT++;
	  }

	}
	
      }
    }


    /* Free memory */
    free(times);

  }


  return OK;

 ComputeDeltaTimeInterval_end:
  return NOT_OK;
  
}  /* ComputeDeltaTimeInterval */


int ComputeMapBadTot(DeltaTimeInfo_t *deltaT[4], int ndeltaT[4], double *time_bad[4], float probmap[4][DET1_ROWS][DET1_PIXS], PixPosInfo_t pixpos[4][DET_ROWS][DET_PIXS][PIXPOS_MAXGRADE], float map_bad_tot[DET1_ROWS][DET1_PIXS]){

  int                 itime, ibp, idetid, xx ,yy, igrade, idet1x, idet1y, ipdf, loc_bad_on;
  static float        pdf_bad[DET1_ROWS][DET1_PIXS];
  static float        loc_map_bad_tot[4][DET1_ROWS][DET1_PIXS];


  for(idetid=0; idetid<4; idetid++){
    for(yy=0; yy<DET1_ROWS; yy++){
      for(xx=0; xx<DET1_PIXS; xx++){
	loc_map_bad_tot[idetid][yy][xx] = 0;
      }
    }
  }


  /* Loop on DET ID */
  for(idetid=0; idetid<4; idetid++){

    /* Loop on time interval */
    for(itime=0; itime<ndeltaT[idetid]; itime++){

      /* skip time intervals without bad pixels */
      if(deltaT[idetid][itime].nbp==0)
	continue;

      /* skip time intervals with time_bad=0 */
      if(time_bad[idetid][itime]<=0)
	continue;
       

      /* reset pdf_bad[][] array */
      for(yy=0; yy<DET1_ROWS; yy++){
	for(xx=0; xx<DET1_PIXS; xx++){
	  pdf_bad[yy][xx] = 0;
	}
      }


      /* Loop on bad pixels of the selected time interval */
      for(ibp=0; ibp<deltaT[idetid][itime].nbp; ibp++){

	xx = deltaT[idetid][itime].bp[ibp].rawx;
	yy = deltaT[idetid][itime].bp[ibp].rawy;


	for(igrade=0; igrade<PIXPOS_MAXGRADE; igrade++){

	  /* Skip pixels with no 'ref_det1x' or 'ref_det1y' data available */
	  /* (NOTE: for these pixels no PDF values are loaded from input pixposfile) */
	  if( pixpos[idetid][yy][xx][igrade].ref_det1x-1<DET1X_MIN || pixpos[idetid][yy][xx][igrade].ref_det1x-1>DET1X_MAX ||
	      pixpos[idetid][yy][xx][igrade].ref_det1y-1<DET1Y_MIN || pixpos[idetid][yy][xx][igrade].ref_det1y-1>DET1Y_MAX )
	    continue;

	  ipdf = 0;
	  for(idet1y=pixpos[idetid][yy][xx][igrade].ref_det1y-1; idet1y<pixpos[idetid][yy][xx][igrade].ref_det1y-1+PIXPOS_PDF_HEIGHT; idet1y++){
	    for(idet1x=pixpos[idetid][yy][xx][igrade].ref_det1x-1; idet1x<pixpos[idetid][yy][xx][igrade].ref_det1x-1+PIXPOS_PDF_WIDTH; idet1x++){

	      if( idet1x>=0 && idet1x<DET1_PIXS && idet1y>=0 && idet1y<DET1_ROWS){

		if(!isnan(pixpos[idetid][yy][xx][igrade].pdf[ipdf]))
		  pdf_bad[idet1y][idet1x] += pixpos[idetid][yy][xx][igrade].pdf[ipdf];

	      }

	      ipdf++;
	    }
	  }
	  
	} /* End loop on grade */

      } /* End loop on bad pixels */


      /* Update LOC_MAP_BAD_TOT array */
      for(yy=0; yy<DET1_ROWS; yy++){
	for(xx=0; xx<DET1_PIXS; xx++){

	  if( ((probmap[idetid][yy][xx]-pdf_bad[yy][xx])<global.par.percent) && (probmap[idetid][yy][xx]>=global.par.percent) )
	    loc_map_bad_tot[idetid][yy][xx] += time_bad[idetid][itime];
	}
      }

    } /* End loop on time interval */

  } /* End loop on DET ID */


  for(yy=0; yy<DET1_ROWS; yy++){
    for(xx=0; xx<DET1_PIXS; xx++){

      loc_bad_on = 0;
      for(idetid=0; idetid<4; idetid++){
	if(loc_map_bad_tot[idetid][yy][xx]>0)
	  loc_bad_on++;
      }

      if(loc_bad_on>1)
	headas_chat(CHATTY,"%s: Info: pixel det1x=%d det1y=%d bad for more than one detector.\n", global.taskname, xx, yy);

      map_bad_tot[yy][xx] = MAX(loc_map_bad_tot[0][yy][xx], MAX(loc_map_bad_tot[1][yy][xx], MAX(loc_map_bad_tot[2][yy][xx], loc_map_bad_tot[3][yy][xx]) ) );
    }
  }
  

  return OK;

} /* ComputeMapBadTot */


int GetInfoFromDet2InstrMap(char *filename, AspHistoCompInfo_t **histocmpinfo, int *histocmpcount){

  int                   status=OK, count=0 ,nfound;
  int                   inExt, totExt;
  long                  naxes[2];
  FitsCard_t            *card;
  FitsHeader_t	        head;
  FitsFileUnit_t        inunit=NULL;


  /* Open read only file */
  if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetInfoFromDet2InstrMap_end;
    }

  /* Get number of hdus */
  if (fits_get_num_hdus(inunit, &totExt, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetInfoFromDet2InstrMap_end;
    }

  if(totExt<=1)
    {
      headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
      goto GetInfoFromDet2InstrMap_end;
    }

  /* Allocate memory to storage all data */
  *histocmpinfo = (AspHistoCompInfo_t *)calloc(totExt-1, sizeof(AspHistoCompInfo_t));
  if(*histocmpinfo==NULL){
    headas_chat(CHATTY,"%s: Error: ComputeAspHistoCmp: memory allocation failure.\n", global.taskname);
    goto GetInfoFromDet2InstrMap_end;
  }
  *histocmpcount = totExt-1;


  inExt=1; /* primary ext */
  if(fits_movabs_hdu( inunit, inExt, NULL, &status ))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
      goto GetInfoFromDet2InstrMap_end;
    }

  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(inunit);

  if( (ExistsKeyWord(&head, KWNM_VIGNAPP, NULLNULLCARD)) && (GetLVal(&head, KWNM_VIGNAPP)) )
    {
      headas_chat(NORMAL, "%s: Error: Input DET2 Instrument Map is corrected for vignetting.\n", global.taskname);
      goto GetInfoFromDet2InstrMap_end;
    }


  inExt=2; /* skip primary ext */
  status=OK;
  while ( status==OK && inExt<=totExt ) 
    {
      if(fits_movabs_hdu( inunit, inExt, NULL, &status ))
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,inExt);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto GetInfoFromDet2InstrMap_end;
	}

      /* Retrieve header pointer */    
      head=RetrieveFitsHeader(inunit);

      if (fits_read_keys_lng(inunit, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, filename);
	  goto GetInfoFromDet2InstrMap_end;
	}

      if( (naxes[0]!=600) || (naxes[1]!=600) )
	{
	  headas_chat(NORMAL, "%s: Error: Input DET2 Instrument Map size < 600 pixels.\n", global.taskname);
	  goto GetInfoFromDet2InstrMap_end;
	}

      if(ExistsKeyWord(&head, KWNM_DURATION, &card))
	{
	  (*histocmpinfo)[count].duration = card->u.DVal;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DURATION);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	  goto GetInfoFromDet2InstrMap_end;
	}

      if(ExistsKeyWord(&head, KWNM_REF_TIME, &card))
	{
	  (*histocmpinfo)[count].ref_time = card->u.DVal;
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_REF_TIME);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, filename);
	  goto GetInfoFromDet2InstrMap_end;
	}

      (*histocmpinfo)[count].x_bin = 0; /* not used */
      (*histocmpinfo)[count].y_bin = 0; /* not used */


      count++;
      inExt++;
    }


  /* Close file */
  if (CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
      goto GetInfoFromDet2InstrMap_end;
    }


  return OK;

 GetInfoFromDet2InstrMap_end:
  return NOT_OK;

} /* GetInfoFromDet2InstrMap */


int CreateFilteredDet1RefFile(char *infile, char *evtfile, char *outfile){

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


  if(FilterTimesByGTI(outfile, KWVL_EXTNAME_DET1_REFPOINT, evtfile, KWVL_EXTNAME_GTI))
    {
      headas_chat(NORMAL, "%s: Error: unable to create temporary file '%s'\n", global.taskname, outfile);
      goto CreateFilteredDet1RefFile_end;
    }


  /* Copy GTI extension from evtfile to DET1 Reference Point filtered file */
  if( CopyHDUExt(evtfile, outfile, KWVL_EXTNAME_GTI) ){
    headas_chat(NORMAL, "%s: Error: Unable to copy %s extension from '%s' to '%s' file.\n", global.taskname, KWVL_EXTNAME_GTI, evtfile, outfile);
    goto CreateFilteredDet1RefFile_end;
  }


  return OK;

 CreateFilteredDet1RefFile_end:
  return NOT_OK;

} /* CreateFilteredDet1RefFile */
