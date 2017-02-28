
/*
 * 
 *	xrtmkarf.c: --- Swift/XRT ---
 *
 *	INVOCATION:
 *
 *		xrtmkarf [parameter=value ...]
 *
 *	DESCRIPTION:
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY
 *
 *
 *      Version 0.1.0 written by Alberto Moretti, INAF/OAB, January 2003
 *
 *        0.1.1 - BS 03/02/2003 - Modified to work under HEADAS
 *        0.1.2 -    14/02/2003 - Modified to get RMF file from CALDB
 *        0.2.0 -    14/05/2003 - Added DATASUM and CHECKSUM keywords 
 *                                in output file
 *                              - Changed check on 'clobber' position
 *        0.2.1      07/07/2003 - bug fixed on 'xrtmkarf_getpar' 
 *                                failure 
 *        0.3.0 - AM 29/07/2003 - Added new input files
 *        0.3.1 - BS 01/08/2003 - Added parameters for input files
 *                              - Added routine to query CALDB
 *        0.3.2 - BS 05/08/2003 - Added check on supported readout mode
 *                                temporary only PHOTON COUNTING is supported.
 *        0.3.4 - AM 31/10/2003 - Version for build 7:
 *                                - Added new input parameter 'psfflag'
 *                                - Modified 'read_pha' prototype
 *                                - Deleted 'geome' function
 *                                - Added handling of timing mode
 *        0.3.5 - BS 10/11/2003 - Deleted 'cntrdetx' and 'cntrdety' input parameters
 *        0.3.6 -    14/11/2003 - Replaced call to 'headas_parstamp' with 'HDpar_stamp'
 *        0.3.7 -    19/11/2003 - Bug fixed
 *        0.4.0 - AM 23/02/2004 - Build 8 Version
 *        0.4.1 - AM 19/03/2004 - Added check on source coordinates range
 *                                Handled new PSF file format 
 *        0.4.2 - BS 23/03/2004 - Bugs fixed
 *        0.4.3 -    09/04/2004 - Bug fixed to avoid segmentation fault 
 *                                on Mac
 *        0.4.4 -    20/04/2004 - Deleted "HDUCLAS2" keyword. (Decided during 
 *                                the 2004-04-20 telecon )
 *        0.4.5 -    06/05/2004 - Modified input parameters description
 *        0.4.6 -    18/05/2004 - Messages displayed revision
 *                              - Added TELESCOP, INSTRUME and DATE keywords in the primary
 *                                of the Arf file
 *        0.4.7 -    23/08/2004 - Bug fixed: Handled Piled Up read out mode
 *                   26/08/2004 - Exit with error if input file datamode is not allowed
 *        0.4.8 - RP 05/10/2004 - Write keyword RESPFILE without path
 *                                Get RMF Caldb file with grade expression (read keyword from spectrum file)
 *        0.4.9 - BS 21/10/2004 - Erased not needed routine
 *        0.4.10- AM 10/02/2005 - Modified to work with WMAP in sky coordinates (only for WT and PC)
 *        0.4.11- FT 11/02/2005 - Bug fixed, divided by zero in read_pha function
 *        0.4.12- BS 14/02/2005 - bug fixed
 *        0.4.13- AM 17/02/2005 - Added routine to read input ARF file
 *        0.4.14- AM 16/03/2005 - Minor changes 
 *        0.4.15- BS 01/07/2005 - Added HDUVERS1 keyword needed to use addarf task
 *                              - Added keywords XRTSX/XRTSY instead of XRTSDETX/XRTSDETY
 *                                for WT and PC 
 *                                  
 *        0.4.16- BS 04/07/2005 - Added HDUVERS and HDUVERS2 keywords in the output arf file 
 *
 *        0.4.17- AM 12/10/2005 - AM modify read_pha routine to read and use the  exposure map
 *        0.4.18- AM 19/10/2005 - Added check on 'XRTVIGCO' keyword in expofile
 *                              - Changed 'srcdetx' and 'srcdety' input parameters datatype  
 *        0.4.19- BS 26/10/2005 - replaced XRTVIGCO with VIGNAPP
 *        0.4.20- BS 27/10/2005 - Set vig_flag to 0 when 'expofile' parameter set to NONE
 *                              - Check 'expofile' parameter when datamode is LR or PU 
 *                              - Filled vcorr array if exposure map is vignetting corrected
 *        0.5.0-                - Bug fixed as reported in the M. Tripicco mail
 *                                (09/11/2005)
 *        0.5.1- BS 15/11/2005  - Changed declaration of the 'expo' array
 *                                to avoid crashing on OS-X (Panther) as 
 *                                reported in the M. Tripicco mail  (15/11/2005)
 *
 *        0.5.2 -AM 03/03/2006  - Changed the WT part of read_pha to account for 
 *                                different geometries of the extraction region. 
 *                                Added the possibility of reading and using the WT
 *                                exposure map.
 *        0.5.3 -NS 29/05/2007  - Changed the WT part of read_pha to improve
 *                                the computation of exposure map rotation
 *        0.5.4 -NS 09/07/2007  - Added read_pha_keywords function
 *                              - Query to CALDB for arf and rmf files depending on time
 *                                and 'XRTVSUB' of input pha file
 *        0.5.5 -NS 21/08/2007  - Read 'XRTVSUB' keyword in primary hdu or SPECTRUM ext of input pha file
 *        0.5.6 -NS 15/11/2007  - Check that expofile=NONE if psfflag set to "no"
 *        0.5.7 -NS 07/06/2010  - Modified off-axis angle computation.
 *        0.5.8 -NS 25/06/2010  - Handle extended sources while creating arf file
 *                              - Added 'extended', 'boxsize' and 'cleanup' input parameters
 *                              - Added 'offaxis' input parameter
 *        0.5.9 -NS 28/03/2011  - Handle new psf CALDB file format
 *        0.6.0 -NS 20/07/2011  - Added datamode selection in PSF CALDB file query
 *        0.6.1 -NS 30/05/2014  - Fixed memory bug for the 64-bit architecture on Mac OS X 10.9.x.
 *        0.6.2 -NS 16/10/2014  - Fixed memory bug when the number of sub-boxes used to divide the extraction region is larger than 100
 *        0.6.3 -NS 16/07/2015  - Fixed bug in exposure map pixels numbering
 *
 *
 *
 *
 *	AUTHORS:
 *
 *       ISAC Italian Swift Archive Center (FRASCATI)
 */

#define TOOLSUB xrtmkarf          /* headas_main() requires that TOOLSUB 
				       be defined first */
#define XRTMKARF_C
#define XRTMKARF_VERSION  "0.6.3"
#define PRG_NAME          "xrtmkarf"

/******************************
 *        header files        *
 ******************************/
#include "headas_main.c"
#include "xrtmkarf.h"

/******************************
 *       definitions          *
 ******************************/
Global_t global;

/*
 *	xrtmkarf_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from 
 *                 xrtmkarf.par    
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result); 
 *           int PILGetReal(char *name, int *result);  
 *           int headas_chat(int, char * , ...);
 *           void disp_info(void);
 *	
 *      CHANGE HISTORY:
 *        0.1.0: - BS 03/02/2003 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

int xrtmkarf_getpar(void)
{
  /************************
   *   Get parameters.    *
   ************************/
  
  /* Get input RMF file name */
  if(PILGetFname(PAR_RMFFILE, global.par.rmf_file))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_RMFFILE);
      goto Error;
    }

  /* Get input INARF file name */
  if(PILGetFname(PAR_INARFFILE, global.par.inarf_file))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INARFFILE);
      goto Error;
    }

  /* Get input EXPO file name */
  if(PILGetFname(PAR_EXPOFILE, global.par.expo_file))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EXPOFILE);
      goto Error;
    }

  /* Get input MIR file name */
  if(PILGetFname(PAR_MIRFILE, global.par.mir_file))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_MIRFILE);
      goto Error;
    }

  /* Get input FLT file name */
  if(PILGetFname(PAR_TRANSMFILE, global.par.flt_file))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TRANSMFILE);
      goto Error;
    }

  /* Get input VIG file name */
  if(PILGetFname(PAR_VIGFILE, global.par.vig_file))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_VIGFILE);
      goto Error;
    }

 /* Get input PHA file name */
  if(PILGetFname(PAR_PHAFILE, global.par.pha_file))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PHAFILE);
      goto Error;
    }

 /* Get input PSF file name */
  if(PILGetFname(PAR_PSFFILE, global.par.psf_file))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSFFILE);
      goto Error;
    }

 /* Get input PSF correction (yes or no) */
  if(PILGetBool(PAR_PSFFLAG, &global.par.psf_flag))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_PSFFLAG);
      goto Error;
    }

  /* Get output ARF fits file name */ 
  if(PILGetFname(PAR_OUTFILE, global.par.outfile))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
      goto Error;
    }


  /* source detx */
  if(PILGetReal(PAR_SRCDETX, &global.par.srcdetx)) 
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDETX);
      goto Error;	
    }

  /* source dety */
  if(PILGetReal(PAR_SRCDETY, &global.par.srcdety)) 
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SRCDETY);
      goto Error;	
    }

  /* offaxis */
  if(PILGetReal(PAR_OFFAXIS, &global.par.offaxis)) 
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OFFAXIS);
      goto Error;	
    }

  /* boxsize */
  if(PILGetInt(PAR_BOXSIZE, &global.par.boxsize)) 
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_BOXSIZE);
      goto Error;	
    }

 /* Extended source (yes or no) */
  if(PILGetBool(PAR_EXTENDED, &global.par.extended))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EXTENDED);
      goto Error;
    }

 /* Cleanup (yes or no) */
  if(PILGetBool(PAR_CLEANUP, &global.par.cleanup))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CLEANUP);
      goto Error;
    }


   get_history(&global.hist);

   /* Display parameters value */
   disp_info();

   
   return OK;

 Error:
   return NOT_OK;
}/*xrtmkarf_getpar*/


/*
 *	disp_info 
 *
 *
 *	DESCRIPTION:
 *         Display information about input parameters.
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *          get_toolnamev(char *);
 *          headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 03/02/2003 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

void disp_info(void)
{
  headas_chat(MUTE, "\n---------------------------------------------------------------\n");
  headas_chat(MUTE,"\t\tRunning '%s'\n", global.taskname);
  headas_chat(MUTE, "---------------------------------------------------------------\n");
  headas_chat(NORMAL,"\t\t Input Parameters List: \n");
  headas_chat(NORMAL,"Name of the input RMF file                    :'%s'\n", global.par.rmf_file);
  headas_chat(NORMAL,"Name of the input mirror effective area file  :'%s'\n", global.par.mir_file);
  headas_chat(NORMAL,"Name of the input filter transmission file    :'%s'\n", global.par.flt_file);
  headas_chat(NORMAL,"Name of the input arf file                    :'%s'\n", global.par.inarf_file);
  headas_chat(NORMAL,"Name of the input exposure map file           :'%s'\n", global.par.expo_file);
  headas_chat(NORMAL,"Name of the input vignetting file             :'%s'\n", global.par.vig_file);
  headas_chat(NORMAL,"Name of the input spectrum file               :'%s'\n", global.par.pha_file);
  headas_chat(NORMAL,"Name of the input PSF file                    :'%s'\n", global.par.psf_file);
  headas_chat(NORMAL,"Name of the output ARF file                   :'%s'\n", global.par.outfile);
  headas_chat(NORMAL,"Source SKYX                                   :'%f'\n", global.par.srcdetx);
  headas_chat(NORMAL,"Source SKYY                                   :'%f'\n", global.par.srcdety);
  headas_chat(NORMAL,"Source off-axis angle (arcmin)                :'%f'\n", global.par.offaxis);
  if (global.par.extended){
    headas_chat(NORMAL,"Extended source?                              : yes\n");
    headas_chat(NORMAL,"Size in pixel for the subimage boxes          : %d\n", global.par.boxsize);
  }else
    headas_chat(NORMAL,"Extended source?                              : no\n");
  if (headas_clobpar)
    headas_chat(CHATTY,"Overwrite output file?                        : yes\n");
  else
    headas_chat(CHATTY,"Overwrite output file?                        : no\n");
  if (global.hist)
    headas_chat(CHATTY,"Write HISTORY keywords?                       : yes\n");
  else
    headas_chat(CHATTY,"Write HISTORY keywords?                       : no\n");
  if (global.par.cleanup)
    headas_chat(CHATTY,"Delete temporary files?                       : yes\n");
  else
    headas_chat(CHATTY,"Delete temporary files?                       : no\n");
  headas_chat(NORMAL, "---------------------------------------------------------------\n\n");
  return;

}/* disp_info */

/*
 *	xrtmkarf
 *
 *
 *	DESCRIPTION:
 *        
 *              
 *
 *	DOCUMENTATION:
 *
 *
 *      FUNCTION CALL:
 *             void set_toolversion(char *);
 *             void set_toolname(char *);   
 *             void get_toolname(char *);
 *             void get_toolnamev(char *);
 *             int headas_chat(int ,char *, ...);
 *
 *             void xrtmkarf_getpar(void);
 * 	       void xrtmkarf_work(void);
 *
 *          
 *	CHANGE HISTORY:
 *        0.1.0: - BS 03/02/2003 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 */
int xrtmkarf(void)
{
  
  /********************** 
   * set HEADAS globals *
   **********************/
  set_toolname(PRG_NAME);
  set_toolversion(XRTMKARF_VERSION);

  get_toolnamev(global.taskname);
  
  GetSWXRTDASVersion(global.swxrtdas_v);

  /*************************
   * Read input parameters *
   *************************/
  if ( xrtmkarf_getpar() == OK ) {

    if ( xrtmkarf_work() ) {
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      headas_chat(MUTE, "%s: Exit with Error.\n", global.taskname);
      headas_chat(MUTE,"---------------------------------------------------------------------\n");
      goto mkarf_end;
    }
  }
  else
    goto mkarf_end;

  return OK;

 mkarf_end:
  return NOT_OK;
  
} /* xrtmkarf */

/*
 *	xrtmkarf_work
 *
 *
 *	DESCRIPTION:
 *           
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *          
 *           int headas_chat(int , char *, ...);
 *           int read_rmf(long*,float**,float**);
 *           int read_pha(float*,float*,float*,float*,float*,int*);
 *           void off_calc(float*,float*,float*,float*);
 *           int vig_corr(float*,float*);
 *           void psf_corr(float*,float*);
 *           int write_arf(long,float*,float*,float*);
 *
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *        0.1.1: - BS 03/02/2003 - Adapted for HEADAS enviroment 
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */


int xrtmkarf_work()
{
  long      nenerg;
  float     *e_hig, *e_low;
  float     off,*arf, *vcorr, *pcorr, *ene, *mir, *flt, *wei, *inarf;
  int       maxrad,cntbox=0,status=0;
  float     countstot=0.;
  register int i;
  float    cx={-1.},cy={-1.};
  int      vig_flag;
  char     **arffile;
  pid_t    pid;
  char     tmpext[20];
  char     inaddarf[PIL_LINESIZE+20];
  char     cmd[2*PIL_LINESIZE+50];
  FILE	   *file;

  VigInfo_t  viginfo;

  WmapBox_t * wmapbox;
  wmapbox = (WmapBox_t *) malloc(sizeof(WmapBox_t));


  /* CENTER of the SRC (SKY) */
  global.srcx = (float)global.par.srcdetx;
  global.srcy = (float)global.par.srcdety;


  /*  psfflag="no" and expofile!=NONE not allowed */
  if(!global.par.psf_flag && strcasecmp(global.par.expo_file,DF_NONE) && !global.par.extended)
    {
      headas_chat(NORMAL, "%s: Error: Exposure map should be set to NONE\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: with psfflag set to 'no'\n", global.taskname);
      goto end_func;
    }


  /* Check if output file exists */  
  if (FileExists(global.par.outfile))
    {
      headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, global.par.outfile);
      if (headas_clobpar)
	{
	  headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set,\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: the '%s' file will be overwritten.\n", global.taskname, global.par.outfile);
	  if(remove (global.par.outfile) == -1)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
	      headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
	      goto end_func;
	    }  
	}
      else
	{
	  headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, global.par.outfile);
	  headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
	  goto end_func;
	}
    }
  
  /* Read input spectrum file */
  if(read_pha(&cx, &cy, &wei, &maxrad, &vig_flag, wmapbox))
    {
      headas_chat(NORMAL, "%s: Error: Unable to process\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.pha_file);
      goto end_func;
    }

  /* Read keywords from input spectrum file */
  if(read_pha_keywords())
    {
      headas_chat(NORMAL, "%s: Error: Unable to read keywords\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: from %s file.\n", global.taskname, global.par.pha_file);
      goto end_func;
    }

  /* Read input RMF file */
  if(read_rmf(&nenerg,&e_low,&e_hig))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.rmf_file);
      goto end_func;
    }
  headas_chat(CHATTY,"%s: Info: energy grid elements  = %ld \n", global.taskname, nenerg);

  ene=malloc(sizeof(float)*nenerg);
  inarf=malloc(sizeof(float)*nenerg);
  mir=malloc(sizeof(float)*nenerg);
  flt=malloc(sizeof(float)*nenerg);
  vcorr=malloc(sizeof(float)*nenerg);  
  pcorr=malloc(sizeof(float)*nenerg);  
  arf=malloc(sizeof(float)*nenerg);


  for (i=0; i < nenerg; i++)
    {
      ene[i]=(e_hig[i]+e_low[i])/2.;
    }


  if (!strcasecmp(global.par.inarf_file,DF_NONE) )
    {
      /* Read input effective area file */
      if(read_mir(&nenerg,ene,mir))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.mir_file);
	  goto end_func;
	}
      
      /* Read input  filter transmission file */
      if(read_flt(&nenerg,ene,flt))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.flt_file);
	  goto end_func;
	}
    }
  else{
    
    /* Read input empirical arf file */
    if(read_inarf(&nenerg,ene,inarf))
      {
	headas_chat(NORMAL, "%s: Error: Unable to read\n", global.taskname);
	headas_chat(NORMAL, "%s: Error: %s file.\n", global.taskname, global.par.inarf_file);
	goto end_func;
      }
  }

  /* Generation of ARF for extended sources sopported only for PC data */
  if(global.par.extended && strcmp(KWVL_DATAMODE_PH,global.datamode)){
    headas_chat(NORMAL, "%s: Error: Generation of ARF for extended sources not supported for datamode=%s\n", global.taskname,global.datamode);
    goto end_func;
  }
 

  if ((vig_flag != 0)&&(global.par.offaxis>=0)){
    headas_chat(NORMAL, "%s: Error: Vignetting is applied to exposure map, parameter offaxis must be set to -99.\n",global.taskname);
    goto end_func;
  }


  /* Extended source = no */
  if(!global.par.extended){

    if ( strcmp(global.datamode,KWVL_DATAMODE_PD ) && strcmp(global.datamode,KWVL_DATAMODE_PDPU) ){
      if(opt2sky_calc()){
	goto end_func;
      }
    }

    if(global.par.offaxis>=0){
      off = global.par.offaxis;
      headas_chat(NORMAL, "%s: Info: Off-axis angle used for vignetting correction: %f (arcmin).\n",global.taskname,off);
    }
    else{
      if(off_calc(&global.srcx, &global.srcy, &off))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to calculate off-axis angle.\n", global.taskname);
	  goto end_func;
	}
    }

    headas_chat(CHATTY,"%s: Info: cen x   = %f \n", global.taskname, cx);
    headas_chat(CHATTY,"%s: Info: cen y   = %f \n", global.taskname, cy);
    headas_chat(CHATTY,"%s: Info: data mode  = %s \n", global.taskname, global.datamode);
    headas_chat(CHATTY,"%s: Info: off-axis angle[arcmin] =  %lf \n", global.taskname, off ); 
    
    headas_chat(NORMAL, "vig_flag   : %d \n", vig_flag);
    if (vig_flag == 0 ) 
      { 

	if(ReadVigInfo(&viginfo)){
	  headas_chat(NORMAL, "%s: Error: Unable to read coefficients from vignetting CALDB file\n",  global.taskname);
	  goto end_func;
	}

	if(vig_corr(&off,ene,&nenerg,vcorr,&viginfo))
	  {
	    headas_chat(NORMAL, "%s: Error: Unable to calculate vignetting correction.\n", global.taskname);
	    goto end_func;
	  } 
      } 
    else
      { 
	headas_chat(NORMAL, ">>> using a vignetted exposure map \n");
	for (i=0; i < nenerg; i++) 
	  vcorr[i]=1.;
      }
    
    
    if (!strcasecmp(global.par.inarf_file,DF_NONE) )
      {      
	if (global.par.psf_flag)
	  {
	    if(psf_corr(ene,&nenerg,&off,&maxrad,wei,pcorr))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to calculate PSF correction.\n", global.taskname);
		goto end_func;
	      }
	    
	    for (i=0; i < nenerg; i++) arf[i] = mir[i]*flt[i]*vcorr[i]*pcorr[i];   
	    
	  }
	else     
	  {
	    for (i=0; i < nenerg; i++) arf[i] = mir[i]*flt[i]*vcorr[i];   
	  }
      }
    else
      { 
	if (global.par.psf_flag)
	  {
	    if(psf_corr(ene,&nenerg,&off,&maxrad,wei,pcorr))
	      {
		headas_chat(NORMAL, "%s: Error: Unable to calculate PSF correction.\n", global.taskname);
		goto end_func;
	      }
	    
	    for (i=0; i < nenerg; i++) arf[i] = inarf[i]*vcorr[i]*pcorr[i];   
	    
	  } 
	else     
	  {
	    for (i=0; i < nenerg; i++) arf[i] = inarf[i]*vcorr[i];   
	  }
      }
    
    /* Write ARF file */
    if(write_arf((int)cx, (int)cy, nenerg, e_low, e_hig, arf, global.par.outfile))
      {
	headas_chat(NORMAL, "%s: Error: Unable to create \n",global.taskname);
	headas_chat(NORMAL, "%s: Error: %s file.\n",global.taskname, global.par.outfile);
	goto end_func;
      }
    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outfile);

  }
  /* Extended source = yes */
  else{
   
    if (global.par.offaxis>=0){
      headas_chat(NORMAL, "%s: Error: Parameter offaxis must be set to -99 for extended sources.\n",global.taskname);
      goto end_func;
    }
 
    if(vig_flag != 0){
      headas_chat(NORMAL, ">>> using a vignetted exposure map \n");
      for (i=0; i < nenerg; i++) 
	vcorr[i]=1.;
    }
    else{
      if(ReadVigInfo(&viginfo)){
	headas_chat(NORMAL, "%s: Error: Unable to read coefficients from vignetting CALDB file\n",  global.taskname);
	goto end_func;
      }
    }

    if ( strcmp(global.datamode,KWVL_DATAMODE_PD ) && strcmp(global.datamode,KWVL_DATAMODE_PDPU) ){
      if(opt2sky_calc()){
	goto end_func;
      }
    }

    arffile = (char**)malloc(wmapbox->nbox * sizeof(char*));

    /* Derive arf temporary filename */
    pid = getpid();
    
    for(cntbox=0; cntbox<wmapbox->nbox; cntbox++){
      arffile[cntbox] = (char*)malloc((PIL_LINESIZE+10)*sizeof(char));
      
      sprintf(tmpext, "_%d_%d", (int)pid, cntbox);
      DeriveFileName(global.par.outfile, arffile[cntbox], tmpext);
    }

    /* Derive addarf input filename */
    strcpy(inaddarf,global.par.outfile);
    sprintf(tmpext, "_%d.txt", (int)pid);
    StripExtension(inaddarf);
    strcat(inaddarf, tmpext);


    for(cntbox=0; cntbox<wmapbox->nbox; cntbox++){

      if(off_calc(&wmapbox->box[cntbox].skycx, &wmapbox->box[cntbox].skycy, &off))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to calculate off-axis angle (box %d).\n", global.taskname,cntbox);
	  goto end_func;
	}
      
      headas_chat(CHATTY,"%s: Info: (box %d) cen x   = %f \n", global.taskname,cntbox, wmapbox->box[cntbox].skycx);
      headas_chat(CHATTY,"%s: Info: (box %d) cen y   = %f \n", global.taskname,cntbox, wmapbox->box[cntbox].skycy);
      headas_chat(CHATTY,"%s: Info: (box %d) off-axis angle[arcmin] =  %lf \n", global.taskname, cntbox, off); 
      
      if (vig_flag == 0 ){
	if(vig_corr(&off,ene,&nenerg,vcorr,&viginfo))
	  {
	    headas_chat(NORMAL, "%s: Error: (box %d) Unable to calculate vignetting correction.\n", global.taskname,cntbox);
	    goto end_func;
	  }
      }

    if (!strcasecmp(global.par.inarf_file,DF_NONE) ){
      for (i=0; i < nenerg; i++) arf[i] = mir[i]*flt[i]*vcorr[i]*wmapbox->box[cntbox].expocorr;
    }
    else{
      for (i=0; i < nenerg; i++) arf[i] = inarf[i]*vcorr[i]*wmapbox->box[cntbox].expocorr;
    }


    /* Write ARF file */
    if(write_arf(wmapbox->box[cntbox].skycx, wmapbox->box[cntbox].skycy, nenerg, e_low, e_hig, arf, arffile[cntbox]))
      {
	headas_chat(NORMAL, "%s: Error: Unable to create \n",global.taskname);
	headas_chat(NORMAL, "%s: Error: %s file.\n",global.taskname, arffile[cntbox]);
	goto end_func;
      }
     
    } /* for(cntbox=0; cntbox<wmapbox->nbox; cntbox++) */


    /* compute tot counts value  */
    countstot=0.0;
    for(cntbox=0; cntbox<wmapbox->nbox; cntbox++){
      countstot += wmapbox->box[cntbox].countsbox;
    }
    countstot = countstot!=0 ? countstot : 1.;



    /* Write input file for addarf task */
    if (!(file = fopen(inaddarf, "w"))){
      headas_chat(NORMAL, "%s: Error: Unable to open temporary file %s\n",global.taskname,inaddarf);
      goto end_func;
    }

    for(cntbox=0; cntbox<wmapbox->nbox; cntbox++){
      if(wmapbox->box[cntbox].countsbox>0){
	fprintf(file, "%s %f\n",arffile[cntbox],wmapbox->box[cntbox].countsbox/countstot);
      }
    }
  
    fclose(file);


    /* Execute addarf task to add arf files  */
    sprintf(cmd, "addarf list=@%s out_ARF=%s", inaddarf, global.par.outfile);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);

    status = system(cmd);
    if(status!=0){
      headas_chat(NORMAL, "%s: Error: unable to add ARF files\n", global.taskname);
      goto end_func;
    }

    /* Update keywords in the output ARF FITS file  */
    if(update_arf_keywords(global.par.outfile)){
      headas_chat(NORMAL, "%s: Error: unable to update keywords in %s file.\n", global.taskname,global.par.outfile);
      goto end_func;
    }
    
    /* cleanup temporary file if cleanup=yes  */
    if(global.par.cleanup){

      for(cntbox=0; cntbox<wmapbox->nbox; cntbox++){
	if(remove(arffile[cntbox]) == -1){
	  headas_chat(NORMAL, "%s: Warning: Unable to remove temporary file\n", global.taskname);
	  headas_chat(NORMAL, "%s: Warning: '%s' file.\n", global.taskname, arffile[cntbox]);
	}
      }
      
      if(remove(inaddarf) == -1){
	headas_chat(NORMAL, "%s: Warning: Unable to remove temporary file\n", global.taskname);
	headas_chat(NORMAL, "%s: Warning: '%s' file.\n", global.taskname, inaddarf);
      }
    }
    
  } /* end else Extended source = yes */



  free(ene)   ;
  free(mir)   ;
  free(inarf) ;
  free(flt)   ;
  free(vcorr) ;  
  free(pcorr) ;  
  free(arf) ;
  free(wei);
  free(e_low);
  free(e_hig);
  free(wmapbox);
  
  headas_chat(MUTE,"---------------------------------------------------------------------\n");
  headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
  headas_chat(MUTE,"---------------------------------------------------------------------\n");

  return OK ;

 end_func:
  return NOT_OK;
}/* xrtmkarf_work*/


/*
 *	read_rmf
 *
 *
 *	DESCRIPTION:
 *            This routine reads the input RMF FITS file 
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           
 *           int fits_open_file(fitsfile **fptr, char *filename, int iomode,
 *                              int status);
 *           int fits_movabs_hdu(fitsfile *fptr,int hdunum, int *hdutype, 
 *                               int *status);
 *           int fits_get_num_rows(fitsfile *fptr, long *mir_nrows, int *status);
 *           int fits_get_colnum(fitsfile *fptr, int casesen, char *templt, char *colnum,
 *                               int *status);
 *           int fits_get_coltype(fitsfile *fptr, int colnum, int *typecode, long *repeat,
 *                                long *width, int *status);
 *           int fits_read_col(fitsfile *fptr, int datatype, int colnum, long firstrow, 
 *                             long firstelem, long nelements, DTYPE *nulval,
 *                             DTYPE *array, int *anynul, int *status);
 *           int fits_close_file(fitsfile *fptr, int *status);           			      
 *           void printerror (int);
 *
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *        0.1.1: - BS 03/02/2003 - Adapted for HEADAS enviroment
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int read_rmf(long *nenerg, float **col1, float **col2)

{
  
  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  int    status = OK , hdutype, anynull, colnum_lo, colnum_hi;
  long   extno;
  float  floatnull=0.;
  char expr[PIL_LINESIZE];


  extno=-1;
  /* Derive CALDB gain filename */ 
  if (!(strcasecmp (global.par.rmf_file, DF_CALDB)))
    {
     
      sprintf(expr, "datamode.eq.%s.and.grade.eq.G%s.and.xrtvsub.eq.%d", global.datamode, global.pha.grade, global.pha.xrtvsub);
      if (CalGetFileName( HD_MAXRET, global.pha.dateobs, global.pha.timeobs, DF_NOW, DF_NOW, KWVL_MATRIX_DSET, global.par.rmf_file, expr, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto rmf_end;
	}
      extno++;
    }
  
  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.rmf_file);

  /* Open input RMF file */  
  if ( fits_open_file(&fptr, global.par.rmf_file, READONLY, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.rmf_file);
      goto rmf_end;
    }

  if(extno != -1)
    {
      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to 2nd HDU */

	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.rmf_file);
	  goto rmf_end;    
	}
    }
  else
    {
      if (fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_MATRIX,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_MATRIX);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.rmf_file);
	  goto rmf_end;
	} 
    }
  
  /* Get rows number */
  if (fits_get_num_rows(fptr, nenerg, &status) )
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of rows in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.rmf_file);
      goto rmf_end;    
    }
  
  /* Get ENERG_LO column index */
  if ( fits_get_colnum(fptr, CASEINSEN, CLNM_ENERG_LO, &colnum_lo, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENERG_LO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.rmf_file);
      goto rmf_end;    
    }
  /* Get ENERG_HI column index */
  if ( fits_get_colnum(fptr, CASEINSEN, CLNM_ENERG_HI, &colnum_hi, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENERG_HI);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.rmf_file);
      goto rmf_end;    
    }
  
 
  (*col1)=(float*)calloc((*nenerg),sizeof(float));
  (*col2)=(float*)calloc((*nenerg),sizeof(float)); 

  /* Get ENERG_LO values */
  if  (fits_read_col(fptr,TFLOAT,colnum_lo,1,1, *nenerg, &floatnull,*col1,
		     &anynull, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname, CLNM_ENERG_LO);
      headas_chat(NORMAL, "%s: Error: in %s file.\n",global.taskname, global.par.rmf_file); 
      goto rmf_end;
    }
  /* Get ENERG_HI values */
  if  (fits_read_col(fptr,TFLOAT,colnum_hi,1,1, *nenerg, &floatnull,*col2,
		     &anynull, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname, CLNM_ENERG_HI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n",global.taskname, global.par.rmf_file); 
      goto rmf_end;
    }   

  /* Close RMF file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.rmf_file);
      goto rmf_end;
    }
  
  return OK;

 rmf_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
   printerror( status ); 
   return NOT_OK;
}/* read_rmf */
/*
 *	read_pha
 *
 *
 *	DESCRIPTION:
 *            This routine reads the input pha FITS file 
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           
 *           int fits_open_file(fitsfile **fptr, char *filename, int iomode,
 *                              int status);
 *           int fits_movabs_hdu(fitsfile *fptr,int hdunum, int *hdutype, 
 *                               int *status);
 *           int fits_close_file(fitsfile *fptr, int *status);           			      
 *           void printerror (int);
 *
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *        0.1.1: - BS 03/02/2003 - Adapted for HEADAS enviroment
 *        0.1.2: - FT 11/02/2005 - bug fixed, divided by 0 when
 *                                 calculate dist1 sign
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int read_pha(float* cx, float* cy, float **wei,int*maxrad, int*vig_flag, WmapBox_t* wmapbox)
     
{
  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  int    status = OK , anynull,nfound,hdutype,cntbox;
  float *wmap,nullval;
  double dblnull=0.;
  register int i,j,k,m;
  long naxes[2],fpixel,npixels;
  static float mask[SX_MAX][SY_MAX];
  float *x0,*y0;
  long iwx1=0,iwx2=0,iwy1=0,iwy2=0,ir,ix0,iy0;
  float wx1={0.},wx2={0.},wy1={0.},wy2={0.};
  float mask_sum=0.,mask_num=0.;

  int limx1,limx2,limy1,limy2;
  float maxd,d[4],dd,*circ;

  fitsfile *fptr2=NULL;        
  int    status2 = OK;
  float expmin,expmax;
  float rexpmin,rexpmax;
  static float expo[SX_MAX][SY_MAX];
  int imin,imax,jmin,jmax;
  long extno;
  double ttcx,ttcy,prcx,prcy,*rcx,*rcy;
  float *tang,pang;
  
  long ixi=0;
  float cexp[SX_MAX];
  int ixmin=1000,ixmax=0;


  x0=malloc(sizeof(float));
  y0=malloc(sizeof(float));
  tang=malloc(sizeof(float));
  
  for (i=0; i<SX_MAX; i++) cexp[i]=-1;
  
  imin=0;
  imax=0;
  jmin=0;
  jmax=0;
  

  /*******************************************************************/
  /* EXPOSURE MAP read and inizialize  */ 

  if (!strcasecmp(global.par.expo_file,DF_NONE) )
    {
      
      *vig_flag=0;
      
      for (i=0; i<SX_MAX; i++) 
	{
	  for (j=0; j<SY_MAX; j++)
	    { 
	      expo[i][j]=1.;
	    }
	}
    }
  else
    {
      if ( fits_open_file(&fptr2, global.par.expo_file, READONLY, &status2) )
	{
	  headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.expo_file);

	  goto pha_end;
	}
  
      if (fits_read_key(fptr2,TLOGICAL,KWNM_VIGNAPP,vig_flag, NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_VIGNAPP);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.expo_file);
	  goto pha_end;
	}
    
      headas_chat(NORMAL, "vig_flag (in)  : %d \n", *vig_flag);

      
      nullval  = 0;
      npixels  = SX_MAX*SY_MAX;         /* number of pixels in the image */
      fpixel   = 1;
      
      /*  tmap=malloc(sizeof(float)*SX_MAX*SY_MAX);*/
      if ( fits_read_img(fptr2, TFLOAT, fpixel, npixels, &nullval,expo, &anynull, &status2) ) 
	printerror( status2 );
      
      
      /* Close expo file */
      if ( fits_close_file(fptr2, &status2) )
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.expo_file);
	  goto pha_end;
	}

    }
  
  expmax=-1000.;
  expmin=100000.;
  
  for (i=0; i<SX_MAX; i++) 
    {
      for (j=0; j<SY_MAX; j++)
	{ 
	  if (expo[i][j] < expmin) 
	    {
	      expmin=expo[i][j] ;
	      imin=i;
	      jmin=j;
	    }
	  if (expo[i][j] > expmax) 
	    {
	      expmax=expo[i][j] ;
	      imax=i;
	      jmax=j;
	    }
	}
    }
  
  headas_chat(NORMAL, "  min exposure : %f in %d %d \n", expmin,jmin,imin);
  headas_chat(NORMAL, "  max exposure : %f in %d %d  \n", expmax,jmax,imax);
  

  /********************************************************************/
  /* PHA file open input */  
 
  if ( fits_open_file(&fptr, global.par.pha_file, READONLY, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.pha_file);
      goto pha_end;
    }
  
  if (fits_read_key(fptr,TSTRING,KWNM_DATAMODE, global.datamode, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_DATAMODE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.pha_file);
      goto pha_end;
    }
  
  *cx=(float)global.par.srcdetx;
  *cy=(float)global.par.srcdety;

  for (i=0; i<SX_MAX; i++) 
    {
      for (j=0; j<SY_MAX; j++)
	{ 
	  mask[i][j]=expo[j][i]/expmax;
	}
    }

    headas_chat(NORMAL, " MAXIMUM EXPOSURE : %f  \n", mask[jmax][imax] );


  /* <<<<<<<<<<<<<<<<<<<  PHOTON COUNTING >>>>>>>>>>>>>>>>>>>> */  

  if(!strcmp(KWVL_DATAMODE_PH,global.datamode))   
    {
      if (fits_read_keys_lng(fptr, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.pha_file);
	  goto pha_end;
	} 
      if (fits_read_key(fptr,TFLOAT,KWNM_X_OFFSET,x0,NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_X_OFFSET);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.pha_file);
	  goto pha_end;
	  
	} 
      if (fits_read_key(fptr,TFLOAT,KWNM_Y_OFFSET,y0,NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_Y_OFFSET);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.pha_file);
	  goto pha_end;
	}


      /* ----------------------------------------------------------- */
      extno=4;
      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to #3 HDU */
	
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: The source coordinates should be provided explicitly \n");
	  goto pha_end;    
	}
      rcx=(double*)malloc(sizeof(double));
      rcy=(double*)malloc(sizeof(double));
      
      if  (fits_read_col(fptr,TDOUBLE,1,1,1,1, &dblnull,rcx,&anynull, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname);
	  headas_chat(NORMAL, "%s: The source coordinates should be provided explicitly \n");
	  goto pha_end;
	}
      if  (fits_read_col(fptr,TDOUBLE,2,1,1,1, &dblnull,rcy,&anynull, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname);
	  headas_chat(NORMAL, "%s: The source coordinates should be provided explicitly \n");
	  goto pha_end;
	}
      
      if (*cx <= 0){
	*cx=(float)(*rcx);
	global.srcx = (float)(*rcx);
      }
      if (*cy <= 0){
	*cy=(float)(*rcy);
	global.srcy = (float)(*rcy);	
      }
      
      headas_chat(NORMAL, " CENTER of the ROI (SKY)  %f %f \n",*rcx,*rcy);
      headas_chat(NORMAL, " CENTER of the SRC (SKY)  %f %f \n", global.srcx,global.srcy);
      
      if (*cx > SX_MAX || *cy > SY_MAX)
	{
	  headas_chat(NORMAL, "%s: Error: Source coordinates are out of range.\n", global.taskname);
	  goto pha_end;
	}
      
      extno=1;

      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to #3 HDU */
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  goto pha_end;    
	}
      /* ----------------------------------------------------------- */


      iwx1=*x0;
      iwx2=*x0+naxes[0]-1;
      iwy1=*y0;
      iwy2=*y0+naxes[1]-1;	
      wx1=iwx1*1.;
      wx2=iwx2*1.;
      wy1=iwy1*1.;
      wy2=iwy2*1.;

      nullval  = 0; 
      npixels  = naxes[0]*naxes[1];         /* number of pixels in the image */
      fpixel   = 1;
      wmap=malloc(sizeof(float)*npixels);
      if ( fits_read_img(fptr, TFLOAT, fpixel, npixels, &nullval,wmap, &anynull, &status) ) 
	printerror( status );
      

      k=0;
      ix0=floor(*x0);
      iy0=floor(*y0);
      
      for (j=0; j<naxes[1]; j++) 
	{
	  for (i=0; i<naxes[0]; i++)
	    { 
	      if (wmap[k] < 0) mask[i+ix0-1][j+iy0-1]=-1.;
	      k++;
	    }
	}

      for  (j=0; j<SY_MAX; j++)
	{ 
	  for (i=0; i<SX_MAX; i++)
	    {
	      if( i < (iwx1-1) || i > (iwx2-1) || j < (iwy1-1) || j > (iwy2-1) ) {
		mask[i][j]=-1;
	      }
	    }
	}


      /* Extended source = yes */
      if(global.par.extended){
	
	wmapbox->x_offset = (int)*x0;
	wmapbox->y_offset = (int)*y0;
	wmapbox->x_width = (int)naxes[0];   
	wmapbox->y_width = (int)naxes[1];
	wmapbox->nbox = 0;
	
	if(ComputeWmapBox(wmapbox)){
	  headas_chat(NORMAL, "%s: Error: Unable to compute subimages boxes\n",  global.taskname);
	  goto pha_end;
	}

	for(cntbox=0; cntbox<wmapbox->nbox; cntbox++){

	  mask_sum=0.;
	  mask_num=0.;
	  wmapbox->box[cntbox].countsbox=0.;

	  for (j=wmapbox->box[cntbox].y_min; j<wmapbox->box[cntbox].y_max; j++)
	    {
	      for (i=wmapbox->box[cntbox].x_min; i<wmapbox->box[cntbox].x_max; i++)
		{
		  wmapbox->box[cntbox].countsbox += (wmap[j*wmapbox->x_width+i]>0) ? (wmap[j*wmapbox->x_width+i]) : 0;

		  mask_num += (mask[i+ix0-1][j+iy0-1]>=0) ? 1 : 0;
		  mask_sum += (mask[i+ix0-1][j+iy0-1]>=0) ? mask[i+ix0-1][j+iy0-1] : 0;
		}
	    }

	  wmapbox->box[cntbox].expocorr = (mask_num>0) ? (mask_sum/mask_num) : 0;

	}
	
      }

    }
  
  /*  <<<<<<<<<<<<<<<<< WINDOWED TIMING >>>>>>>>>>>>>>>>>>>>> */
  
  else if (!strcmp(global.datamode,KWVL_DATAMODE_TM ))
    {
      if (fits_read_keys_lng(fptr, KWNM_NAXIS, 1, 2, naxes, &nfound, &status) )
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_NAXIS);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.pha_file);
	  goto pha_end;
	} 
      if (fits_read_key(fptr,TFLOAT,KWNM_X_OFFSET,x0,NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_X_OFFSET);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.pha_file);
	  goto pha_end;
	}
      if (fits_read_key(fptr,TFLOAT,KWNM_Y_OFFSET,y0,NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_Y_OFFSET);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.pha_file);
	  goto pha_end;
	}

      if (fits_read_key(fptr,TFLOAT,"PA_PNT",tang,NULL, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, "PA_PNT");
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.pha_file);
	  goto pha_end;
	}
      nullval  = 0; 
      npixels  = naxes[0]*naxes[1];         /* number of pixels in the image */
      fpixel   = 1;
      wmap=malloc(sizeof(float)*npixels);
      if ( fits_read_img(fptr, TFLOAT, fpixel, npixels, &nullval,wmap, &anynull, &status) ) 
	printerror( status );
      

      /* ----------------------------------------------------------- */
      extno=4;
      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to #3 HDU */
	
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: The source coordinates should be provided explicitly \n");
	  goto pha_end;    
	}
      rcx=(double*)malloc(sizeof(double));
      rcy=(double*)malloc(sizeof(double));
      
      if  (fits_read_col(fptr,TDOUBLE,1,1,1,1, &dblnull,rcx,&anynull, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname);
	  headas_chat(NORMAL, "%s: The source coordinates should be provided explicitly \n");
	  goto pha_end;
	}
      if  (fits_read_col(fptr,TDOUBLE,2,1,1,1, &dblnull,rcy,&anynull, &status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname);
	  headas_chat(NORMAL, "%s: The source coordinates should be provided explicitly \n");
	  goto pha_end;
	}
      

      if (*cx <= 0){
	*cx=(float)(*rcx);
	global.srcx = (float)(*rcx);
      }
      if (*cy <= 0){
	*cy=(float)(*rcy);
	global.srcy = (float)(*rcy);	
      }
      
      headas_chat(NORMAL, " CENTER of the ROI (SKY)  %f %f \n",*rcx,*rcy);
      headas_chat(NORMAL, " CENTER of the SRC (SKY)  %f %f \n", global.srcx,global.srcy);
      
      if (*cx > SX_MAX || *cy > SY_MAX)
	{
	  headas_chat(NORMAL, "%s: Error: Source coordinates are out of range.\n", global.taskname);
	  goto pha_end;
	}
      
      extno=1;

      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to #3 HDU */
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  goto pha_end;    
	}
      /* ----------------------------------------------------------- */


      pang=(*tang)*3.1415927/180.+3.1415927/2.;
      prcx= cos(pang)*(*rcx-CSKYX) + sin(pang)*(*rcy-CSKYY)+CSKYX ;      
      prcy=-sin(pang)*(*rcx-CSKYX) + cos(pang)*(*rcy-CSKYY)+CSKYY ;
      
      ttcx=(*cx);
      ttcy=(*cy);
      *cx= cos(pang)*(ttcx-CSKYX) + sin(pang)*(ttcy-CSKYY)+CSKYX ;
      *cy=-sin(pang)*(ttcx-CSKYX) + cos(pang)*(ttcy-CSKYY)+CSKYY ;

      ix0=floor(*x0-1);
      iy0=floor(*y0-1);
      k=0;
      for (j=0; j<naxes[1]; j++) 
	{
	  for (i=0; i<naxes[0]; i++)
	    { 
	      if (wmap[k] > 0)
		{
		  ttcx= cos(pang)*((i+ix0)*1.-CSKYX) + sin(pang)*((j+iy0)*1.-CSKYY)+CSKYX;
		  ttcy=-sin(pang)*((i+ix0)*1.-CSKYX) + cos(pang)*((j+iy0)*1.-CSKYY)+CSKYY;
		  ixi=floor(ttcx+0.5);
		  cexp[ixi]=mask[i+ix0][j+iy0];
		  /*  headas_chat(NORMAL, "%d %d %d  %f %f \n",i+ix0,j+iy0,ixi,wmap[k],mask[i+ix0][j+iy0]*expmax);  */
		}
	      k++;
	    }
	}

      headas_chat(CHATTY, " cexp[] array:\n");
      for(i=0; i<SX_MAX; i++)
	headas_chat(CHATTY, "(%d) %f | ", i, cexp[i]);
      headas_chat(CHATTY,"\n");

      /* For pixels unknown values compute mean of neighbor pix */
      for (i=1; i<SX_MAX-1; i++) 
	{
	  if(cexp[i]==-1 && cexp[i-1]>=0 && cexp[i+1]>=0)
	    cexp[i]=(cexp[i-1]+cexp[i+1])/2.0;
	}

      headas_chat(CHATTY, " cexp[] array (after evaluation of unknown values):\n");
      for(i=0; i<SX_MAX; i++)
	headas_chat(CHATTY, "(%d) %f | ", i, cexp[i]);
      headas_chat(CHATTY,"\n");


      ixmin=1000;
      ixmax=0;
      for (i=0; i<SX_MAX; i++) 
	{
	  if (cexp[i]> 0 && i < ixmin) ixmin=i;
	  if (cexp[i]> 0 && i > ixmax) ixmax=i;
	  /* if (cexp[i]> 0)  headas_chat(NORMAL, "%d  %f  \n", i, cexp[i]);  */
	}

      wy1=1;
      wy2=SY_MAX;	
      wx1=ixmin+1; 
      wx2=ixmax+1;
      iwx1=floor(wx1);
      iwx2=floor(wx2); 
      iwy1=floor(wy1);
      iwy2=floor(wy2);

      headas_chat(NORMAL, " POSITION ANGLE [degrees]: %f  \n", *tang);
      headas_chat(NORMAL, " Source position (PSEUDODETX,PSEUDODETY): [%f , %f] \n",*cx,*cy);
      headas_chat(NORMAL, " Region Center   (PSEUDODETX,PSEUDODETY): [%f , %f] \n",prcx,prcy);
      headas_chat(NORMAL, " Region boundaries (PSEUDODETX1:PSEUDODETX2,PSEUDODETY1:PSEUDODETY2):[ %d : %d , %d : %d ] \n",iwx1,iwx2,iwy1,iwy2); 

      for (i=0; i<SX_MAX; i++)
	{ 
	  for  (m=0; m<SY_MAX; m++)
	    
	    {
	      mask[i][m]= cexp[i] ;
	    }
	}
    }  

  /*  <<<<<<<<<<<<<<<<<  PHOTO DIODE >>>>>>>>>>>>>>>>>>>>>>>>>>>  */
  
  else if ((!strcmp(global.datamode,KWVL_DATAMODE_PD )) ||(!strcmp(global.datamode,KWVL_DATAMODE_PDPU)))
    {
      if (strcasecmp(global.par.expo_file,DF_NONE) )
	{
	  headas_chat(NORMAL, "%s: Error: '%s' should be set to 'NONE' if datamode is %s.\n",global.taskname, PAR_EXPOFILE, global.datamode  );
	  goto pha_end;
	}
      iwx1=1;
      iwx2=DETX_MAX;
      iwy1=1;
      iwy2=DETY_MAX;	
      wx1=iwx1*1.;
      wx2=iwx2*1.;
      wy1=iwy1*1.;
      wy2=iwy2*1.;
      if (*cx < DETX_MIN || *cy < DETY_MIN || *cx > DETX_MAX || *cy > DETY_MAX)
	{
	  headas_chat(NORMAL, "%s: Error: Source coordinates are out of range.\n", global.taskname);
	  goto pha_end;
	}
    }
  
  else
    {
      headas_chat(NORMAL, "%s: Error: %s datamode not allowed.\n", global.taskname, global.datamode);
      headas_chat(NORMAL, "%s: Error: in %s file.\n",global.taskname, global.par.pha_file);
      goto pha_end;
    }


  /************************************************************************************************************/
  
  if (!strcmp(global.datamode,KWVL_DATAMODE_TM )){
    headas_chat(NORMAL, "%s: Info: WMAP region boundaries (PSEUDODETX1:PSEUDODETX2,PSEUDODETY1:PSEUDODETY2):[ %d : %d , %d : %d ] \n",global.taskname,iwx1,iwx2,iwy1,iwy2); 
    headas_chat(NORMAL, "%s: Info: Source position (PSEUDODETX,PSEUDODETY): [%f , %f] \n",global.taskname,*cx,*cy);
  }
  else if(!strcmp(KWVL_DATAMODE_PH,global.datamode)){
    headas_chat(NORMAL, "%s: Info: WMAP region boundaries (X1:X2,Y1:Y2):[ %d : %d , %d : %d ] \n",global.taskname,iwx1,iwx2,iwy1,iwy2); 
    headas_chat(NORMAL, "%s: Info: Source position (X,Y): [%f , %f] \n",global.taskname,*cx,*cy);
  }
  else{
    headas_chat(NORMAL, "%s: Info: WMAP region boundaries (DETX1:DETX2,DETY1:DETY2):[ %d : %d , %d : %d ] \n",global.taskname,iwx1,iwx2,iwy1,iwy2); 
    headas_chat(NORMAL, "%s: Info: Source position (DETX,DETY): [%f , %f] \n",global.taskname,*cx,*cy);
  }
  
  d[0]=sqrt((*cx-wx1)*(*cx-wx1) + (*cy-wy1)*(*cy-wy1));
  d[1]=sqrt((*cx-wx1)*(*cx-wx1) + (*cy-wy2)*(*cy-wy2));
  d[2]=sqrt((*cx-wx2)*(*cx-wx2) + (*cy-wy1)*(*cy-wy1));
  d[3]=sqrt((*cx-wx2)*(*cx-wx2) + (*cy-wy2)*(*cy-wy2));
  maxd=-1.;
  for (i=0; i<4; i++)
    {
      if (d[i] > maxd) maxd=d[i];
    }
  *maxrad=floor(maxd+0.5);
  *wei=malloc(sizeof(float)*(*maxrad+1));
  circ=malloc(sizeof(float)*(*maxrad+1));
  for (i=0; i<*maxrad+1; i++) (*wei)[i]=0.;
  for (i=0; i<*maxrad+1; i++) circ[i]=0.;
  limx1=floor(*cx-maxd);
  limx2=floor(*cx+maxd);
  limy1=floor(*cy-maxd);
  limy2=floor(*cy+maxd);
  
  rexpmax=-1000.;
  rexpmin=10000000.;
  for (i=limx1; i<=limx2; i++) 
    {
      for (j=limy1; j<=limy2; j++)
	{ 
	  dd=sqrt((*cx-i)*(*cx-i)+(*cy-j)*(*cy-j))+0.5;
	  ir=floor(dd);
	  if (ir < *maxrad) circ[ir]=circ[ir]+1; 
	  if (i >= iwx1 && i <= iwx2 && j >= iwy1 && j <= iwy2 && mask[i-1][j-1] > 0)
	    {
	      (*wei)[ir]=(*wei)[ir]+mask[i-1][j-1]; 
	      if (mask[i-1][j-1] < rexpmin)
		{
		  rexpmin=mask[i-1][j-1] ;
		  imin=i;
		  jmin=j;  
		}
	      if (mask[i-1][j-1] > rexpmax) 
		{
		  rexpmax=mask[i-1][j-1] ;
		  imax=i;
		  jmax=j;
		}
	    }
	}
    }
  
  headas_chat(NORMAL, "  min ROI exposure : %f in %d %d \n", rexpmin*expmax,imin,jmin);
  headas_chat(NORMAL, "  max ROI exposure : %f in %d %d \n", rexpmax*expmax,imax,jmax);

  if (circ[0] == 0.) circ[0]=1.; 
  for (i=0; i<*maxrad; i++)
    {
      /*      headas_chat(NORMAL, " circ %i %f \n",i,circ[i]);*/
      if(circ[i] == 0)
	{
	  headas_chat(NORMAL, "%s: Error: Unable to read WMAP\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n",global.taskname, global.par.pha_file);
	  goto pha_end;
	}
      (*wei)[i]=(*wei)[i]/circ[i];
    }
  

  /* Close PHA file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.pha_file);
      goto pha_end;
    }
  return OK;
  
 pha_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
  printerror( status ); 
  return NOT_OK;
}/* read_pha */


/*
 *	read_pha_keywords
 *
 *
 *	DESCRIPTION:
 *            This routine reads the needed keyword to query CALDB
 *            from the input pha FITS file 
 *
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int   read_pha_keywords(){

  int    status = OK, n=0, found=0, vsub_found=0;
  char	 KeyWord[FLEN_KEYWORD];
  FitsHeader_t   head;
  FitsFileUnit_t inunit=NULL;
  FitsCard_t         *card;

  /* Open readonly input event file */
  if ((inunit=OpenReadFitsFile(global.par.pha_file)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.pha_file);
      goto pha_keywords_end;
    }
     
   head=RetrieveFitsHeader(inunit); 

  /* Retrieve XRTVSUB from primay hdu */
  if((ExistsKeyWord(&head, KWNM_XRTVSUB, &card)))
    {
      global.pha.xrtvsub=card->u.JVal;
      vsub_found=1;
    }

  /* Retrieve RA_PNT from primay hdu */
  if((ExistsKeyWord(&head, KWNM_RA_PNT, &card)))
    {
      global.pha.ranom=card->u.DVal;
    }

  /* Retrieve DEC_PNT from primay hdu */
  if((ExistsKeyWord(&head, KWNM_DEC_PNT, &card)))
    {
      global.pha.decnom=card->u.DVal;
    }

  /* Retrieve CRVAL1 from primay hdu */
  if((ExistsKeyWord(&head, KWNM_CRVAL1, &card)))
    {
      global.pha.crval1=card->u.DVal;
    }

  /* Retrieve CRVAL2 from primay hdu */
  if((ExistsKeyWord(&head, KWNM_CRVAL2, &card)))
    {
      global.pha.crval2=card->u.DVal;
    }


  /* Move in SPECTRUM extension in input file */
  if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_SPEC, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in \n", global.taskname,KWVL_EXTNAME_SPEC,status);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.pha_file); 
      
      if( CloseFitsFile(inunit))
	{
	  headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
	  headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.pha_file);
	}
      goto pha_keywords_end;
      
    }
  
  head=RetrieveFitsHeader(inunit); 
  

  /* Retrieve XRTVSUB from SPECTRUM ext */
  if(!vsub_found && (ExistsKeyWord(&head, KWNM_XRTVSUB, &card)))
    {
      global.pha.xrtvsub=card->u.JVal;
      vsub_found=1;
    }

  if(!vsub_found)
    {
      global.pha.xrtvsub=DEFAULT_XRTVSUB;
      headas_chat(NORMAL,"%s: Warning: '%s' keyword not found in %s\n", global.taskname, KWNM_XRTVSUB, global.par.pha_file);
      headas_chat(NORMAL,"%s: Warning: using default value '%d'\n", global.taskname, DEFAULT_XRTVSUB);
    }


  /* Retrieve DATEOBS  */
  if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
      global.pha.dateobs=card->u.SVal;
      if(!(strchr(global.pha.dateobs, 'T')))
	{
	  if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
	    global.pha.timeobs=card->u.SVal;
	  else
	    {
	      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_TIMEOBS);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.pha_file);
	      goto pha_keywords_end;
	    }
	}
    }
  else
    {
      headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_DATEOBS);
      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.pha_file);
      goto pha_keywords_end;
      
    }

  /* Retrieve GRADE selection */
  for (n=1; !found ; n++)
    {
      sprintf(KeyWord, "%s%d",KWNM_DSTYP,n);
      if(ExistsKeyWord(&head, KeyWord,&card))
	{ 
	  if(!(strcasecmp (card->u.SVal, KWVL_DSTYP_GRADE))){
  
	    sprintf(KeyWord, "%s%d",KWNM_DSVAL,n);
	    if(ExistsKeyWord(&head, KeyWord,&card)) {
	      sprintf(global.pha.grade, "%s",card->u.SVal);
	      found=1;	    
	    } 
	    else {
	      headas_chat(NORMAL, "%s: Error: Unable to get value DSVAL%d.\n", global.taskname,n);
	      headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.pha_file);
	      goto pha_keywords_end;
	    }
	  }
	}
      else
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to get value DSTYP%d.\n", global.taskname,n);
	  headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.pha_file);
	  goto pha_keywords_end;
	  
	}
    }


  if( CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.pha_file);
      
      goto pha_keywords_end;
    }

  
  return OK;

 pha_keywords_end:

  if (inunit)
    CloseFitsFile(inunit);
  
  return NOT_OK;
  
} /* read_pha_keywords */


/*
 *	read_inarf
 *
 *
 *	DESCRIPTION:
 *            This routine reads the input INARF FITS file 
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           
 *           int fits_open_file(fitsfile **fptr, char *filename, int iomode,
 *                              int status);
 *           int fits_movabs_hdu(fitsfile *fptr,int hdunum, int *hdutype, 
 *                               int *status);
 *           int fits_get_num_rows(fitsfile *fptr, long *mir_nrows, int *status);
 *           int fits_get_colnum(fitsfile *fptr, int casesen, char *templt, char *colnum,
 *                               int *status);
 *           int fits_get_coltype(fitsfile *fptr, int colnum, int *typecode, long *repeat,
 *                                long *width, int *status);
 *           int fits_read_col(fitsfile *fptr, int datatype, int colnum, long firstrow, 
 *                             long firstelem, long nelements, DTYPE *nulval,
 *                             DTYPE *array, int *anynul, int *status);
 *           int fits_close_file(fitsfile *fptr, int *status);           			      
 *           void printerror (int);
 *
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    02/2005 - First version
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
 int read_inarf(long *nenerg, float *ene, float *inarf)
{
  
  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  int    status = OK , hdutype, anynull, colnum_lo, colnum_hi, colnum_sp;
  long   extno;

  float  *a_ene,*tarf,*col1,*col2;
  float  dx,dy;
  long *inarf_nrow;  
  register int i,k;
  float  floatnull=0.;
  char expr[PIL_LINESIZE];


  inarf_nrow=malloc(sizeof(long));

  extno=-1;
  /* Derive CALDB gain filename */ 
  if (!(strcasecmp (global.par.inarf_file, DF_CALDB)))
    {

      sprintf(expr, "datamode.eq.%s.and.grade.eq.G%s.and.xrtvsub.eq.%d", global.datamode, global.pha.grade, global.pha.xrtvsub);
      if (CalGetFileName( HD_MAXRET, global.pha.dateobs, global.pha.timeobs, DF_NOW, DF_NOW, KWVL_SPECRESP_DSET, global.par.inarf_file, expr, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto inarf_end;
	}
      extno++;
    }
  
  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.inarf_file);

  /* Open input inarf file */  
  if ( fits_open_file(&fptr, global.par.inarf_file, READONLY, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.inarf_file);
      goto inarf_end;
    }

  if(extno != -1)
    {
      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to 2nd HDU */

	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.inarf_file);
	  goto inarf_end;    
	}
    }
  else
    {
      if (fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_SPECRESP,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_SPECRESP);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.inarf_file);
	  goto inarf_end;
	} 
    }
  
  /* Get rows number */
  if (fits_get_num_rows(fptr, inarf_nrow, &status) )
    {
      
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of rows in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.inarf_file);
      goto inarf_end;    
    }

  col1=(float*)malloc((*inarf_nrow)*sizeof(float));
  col2=(float*)malloc((*inarf_nrow)*sizeof(float));
  a_ene=(float*)malloc((*inarf_nrow)*sizeof(float));
  tarf=(float*)malloc((*inarf_nrow)*sizeof(float)); 



  /* Get ENERG_LO column index */
  if ( fits_get_colnum(fptr, CASEINSEN, CLNM_ENERG_LO, &colnum_lo, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENERG_LO);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.inarf_file);
      goto inarf_end;    
    }
  /* Get ENERG_HI column index */
  if ( fits_get_colnum(fptr, CASEINSEN, CLNM_ENERG_HI, &colnum_hi, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENERG_HI);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.inarf_file);
      goto inarf_end;    
    }
 
  /* Get SPECRESP column index */
  if ( fits_get_colnum(fptr, CASEINSEN, CLNM_SPECRESP, &colnum_sp, &status) )
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SPECRESP);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.inarf_file);
      goto inarf_end;    
    }

 
   /* Get ENERG_LO values */
  if  (fits_read_col(fptr,TFLOAT,colnum_lo,1,1, *nenerg, &floatnull,col1,
		     &anynull, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname, CLNM_ENERG_LO);
      headas_chat(NORMAL, "%s: Error: in %s file.\n",global.taskname, global.par.inarf_file); 
      goto inarf_end;
    }
  /* Get ENERG_HI values */
  if  (fits_read_col(fptr,TFLOAT,colnum_hi,1,1, *nenerg, &floatnull,col2,
		     &anynull, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname, CLNM_ENERG_HI);
      headas_chat(NORMAL, "%s: Error: in %s file.\n",global.taskname, global.par.inarf_file); 
      goto inarf_end;
    }   

  /* Get SPECRESP values */
  if  (fits_read_col(fptr,TFLOAT,colnum_sp,1,1, *nenerg, &floatnull,tarf,
		     &anynull, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to read %s column\n",global.taskname, CLNM_SPECRESP);
      headas_chat(NORMAL, "%s: Error: in %s file.\n",global.taskname, global.par.inarf_file); 
      goto inarf_end;
    }   


  for (i=0; i < *nenerg; i++)
    {
      a_ene[i]=(col1[i]+col2[i])/2.;
    }

  for (i=0; i<(*nenerg); i++)
    {
      
      if (ene[i] <= a_ene[0]) inarf[i]=tarf[0];
      else if (ene[i] > a_ene[*inarf_nrow-1]) inarf[i]=tarf[*inarf_nrow-1];
      else
	{
	  k=0;
	  while(a_ene[k] < ene[i]) k++;
	  
	  if ((a_ene[k]-a_ene[k-1]) == 0)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to  correct input arf Area .\n", global.taskname);
	      goto inarf_end;
	    }
	  
	  dx=(ene[i]-a_ene[k-1])/(a_ene[k]-a_ene[k-1]);
	  dy=tarf[k]-tarf[k-1];
	  inarf[i]=tarf[k-1]+dy*dx;
	}
    }




  /* Close INARF file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.inarf_file);
      goto inarf_end;
    }
  
  return OK;

 inarf_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
   printerror( status ); 
   return NOT_OK;
}/* read_inarf */







/*
 *	read_mir
 *
 *
 *	DESCRIPTION:
 *            This routine reads the input MIR FITS file 
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           
 *           int fits_open_file(fitsfile **fptr, char *filename, int iomode,
 *                              int status);
 *           int fits_movabs_hdu(fitsfile *fptr,int hdunum, int *hdutype, 
 *                               int *status);
 *           int fits_get_num_rows(fitsfile *fptr, long *nrows, int *status);
 *           int fits_get_colnum(fitsfile *fptr, int casesen, char *templt, char *colnum,
 *                               int *status);
 *           int fits_get_coltype(fitsfile *fptr, int colnum, int *typecode, long *repeat,
 *                                long *width, int *status);
 *           int fits_read_col(fitsfile *fptr, int datatype, int colnum, long firstrow, 
 *                             long firstelem, long nelements, DTYPE *nulval,
 *                             DTYPE *array, int *anynul, int *status);
 *           int fits_close_file(fitsfile *fptr, int *status);           			      
 *           void printerror (int);
 *
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *        0.1.1: - BS 03/02/2003 - Adapted for HEADAS enviroment
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int read_mir(long *nenerg, float *ene, float *mir)

{
  
  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  int    status = OK , hdutype,anynull;
  float  floatnull={0.};
  float  *m_ene,*area;
  float  dx,dy;
  long *mir_nrow, extno;
  register int i,k;

  mir_nrow=malloc(sizeof(long));

  extno=-1;
  /* Derive CALDB gain filename */ 
  if (!(strcasecmp (global.par.mir_file, DF_CALDB)))
    {
      if (CalGetFileName( HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_MIRROR_DSET, global.par.mir_file, HD_EXPR, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto mir_end;
	}
      extno++;
    }
  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.mir_file);

  /* Open input MIR file */  
  if ( fits_open_file(&fptr, global.par.mir_file, READONLY, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.mir_file);
      goto mir_end;
    }

  if(extno != -1)
    {
      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to 2nd HDU */
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.mir_file);
	  goto mir_end;    
	}
    }
  else
  {
    if (fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_MIRROR,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n",global.taskname, KWVL_EXTNAME_MIRROR);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.mir_file);
	  goto mir_end;
	} 
  }
    
  /* Get rows number */
  if(fits_get_num_rows(fptr, mir_nrow, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of rows in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.mir_file);
      goto mir_end;
    } 
  
  m_ene=(float*)malloc((*mir_nrow)*sizeof(float));
  area=(float*)malloc((*mir_nrow)*sizeof(float)); 

  /* Get ENERG_LO values */
  if  (fits_read_col(fptr,TFLOAT,1,1,1, *mir_nrow, &floatnull,m_ene,&anynull, &status)) 
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENERGY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.mir_file);
      goto mir_end;
    }
  /* Get area values */
  if  (fits_read_col(fptr,TFLOAT,2,1,1, *mir_nrow, &floatnull,area,&anynull, &status))
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AREA);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.mir_file);
      goto mir_end;
    }
  /* Close MIR file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.mir_file);
      goto mir_end;
    }
 
  for(i=0; i<(*mir_nrow); i++) m_ene[i]=m_ene[i]/1000.;

  for (i=0; i<(*nenerg); i++)
    {
      
      if (ene[i] <= m_ene[0]) mir[i]=area[0];
      else if (ene[i] > m_ene[*mir_nrow-1]) mir[i]=area[*mir_nrow-1];
      else
	{
	  k=0;
	  while(m_ene[k] < ene[i]) k++;
	  
	  if ((m_ene[k]-m_ene[k-1]) == 0)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to  correct Mirror Effective Area .\n", global.taskname);
	      goto mir_end;
	    }
	  
	  dx=(ene[i]-m_ene[k-1])/(m_ene[k]-m_ene[k-1]);
	  dy=area[k]-area[k-1];
	  mir[i]=area[k-1]+dy*dx;
	}
    }
  return OK;
  
 mir_end:
  if (fptr != NULL) fits_close_file(fptr, &status);
  printerror( status ); 
  return NOT_OK;
}/* read_mir */

/*
 *	read_flt
 *
 *
 *	DESCRIPTION:
 *            This routine reads the input FLT FITS file 
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *           
 *           int fits_open_file(fitsfile **fptr, char *filename, int iomode,
 *                              int status);
 *           int fits_movabs_hdu(fitsfile *fptr,int hdunum, int *hdutype, 
 *                               int *status);
 *           int fits_get_num_rows(fitsfile *fptr, long *nrows, int *status);
 *           int fits_get_colnum(fitsfile *fptr, int casesen, char *templt, char *colnum,
 *                               int *status);
 *           int fits_get_coltype(fitsfile *fptr, int colnum, int *typecode, long *repeat,
 *                                long *width, int *status);
 *           int fits_read_col(fitsfile *fptr, int datatype, int colnum, long firstrow, 
 *                             long firstelem, long nelements, DTYPE *nulval,
 *                             DTYPE *array, int *anynul, int *status);
 *           int fits_close_file(fitsfile *fptr, int *status);           			      
 *           void printerror (int);
 * 
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *        0.1.1: - BS 03/02/2003 - Adapted for HEADAS enviroment
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int read_flt(long *nenerg, float *ene, float *flt)

{
  
  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  int    status = OK , hdutype,anynull;
  float  floatnull={0.};
  float  *f_ene,*filt;
  float  dx,dy;
  long  *flt_nrow, extno;
  register int i,k;

  flt_nrow=malloc(sizeof(long));

  extno=-1;
  /* Derive CALDB gain filename */ 
  if (!(strcasecmp (global.par.flt_file, DF_CALDB)))
    {
      if (CalGetFileName( HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_FILTER_DSET, global.par.flt_file, HD_EXPR, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto flt_end;
	}
      extno++;
    }
  
  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.flt_file);

  /* Open input FLT file */  
  if ( fits_open_file(&fptr, global.par.flt_file, READONLY, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.flt_file);
      goto flt_end;
    }
  if (extno != -1)
    {
      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to 2nd HDU */
	{
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.flt_file);
	   goto flt_end;
	}
    }
  else
    {
      if (fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_TRANS,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_TRANS);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.flt_file);
	  goto flt_end;
	} 
    }

  /* Get rows number */
  if(fits_get_num_rows(fptr, flt_nrow, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to get the total number of rows in\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, global.par.flt_file);
      goto flt_end;
    }

 
  f_ene=(float*)malloc((*flt_nrow)*sizeof(float));
  filt=(float*)malloc((*flt_nrow)*sizeof(float)); 

  /* Get ENERGY values */
  if  (fits_read_col(fptr,TFLOAT,1,1,1, *flt_nrow, &floatnull,f_ene,&anynull, &status))
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_ENERGY);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.flt_file);
      goto flt_end;
    } 
    
  /* Get TRANSMIS values */
  if  (fits_read_col(fptr,TFLOAT,2,1,1, *flt_nrow, &floatnull,filt,&anynull, &status))
    {
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TRANSMIS);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.flt_file);
      goto flt_end;
    }
    
  /* Close FLT file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.flt_file);
      goto flt_end;
    }
   
 
  for(i=0; i<(*flt_nrow); i++) f_ene[i]=f_ene[i]/1000.;

  for (i=0; i<(*nenerg); i++)
    {
      
      if (ene[i] <= f_ene[0]) flt[i]=filt[0];
      else if (ene[i] > f_ene[*flt_nrow-1]) flt[i]=filt[*flt_nrow-1];
      else
	{
	  k=0;
	  while(f_ene[k] < ene[i]) k++;
	  
	  if ((f_ene[k]-f_ene[k-1]) == 0)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to calculate filter transmission value.\n", global.taskname);
	      goto flt_end;
	    }
	  dx=(ene[i]-f_ene[k-1])/(f_ene[k]-f_ene[k-1]);
	  dy=filt[k]-filt[k-1];
	  flt[i]=filt[k-1]+dy*dx;
	}
    }


  return OK;
  
 flt_end:
  if (fptr != NULL) fits_close_file(fptr, &status);
  printerror( status ); 
  return NOT_OK;
}/* read_flt */


/*
 *
 *  opt2sky_calc
 *
 *  DESCRIPTION:
 *    
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int opt2sky_calc(){

  int    sizex, sizey;
  double roll, pixsize;

  /* parameters for radec2sky coordinates conversion */
  sizex=SIZE_X;
  sizey=SIZE_Y;
  roll=270.;
  pixsize=PIX_SIZE_DEG;

  headas_chat(NORMAL, "%f %f %f %f\n",global.pha.ranom, global.pha.decnom,global.pha.crval1, global.pha.crval2);
  if(RADec2XY(global.pha.ranom, global.pha.decnom, sizex, sizey, global.pha.crval1, global.pha.crval2, roll, pixsize, &global.optaxis.skyx, &global.optaxis.skyy))
    {
      headas_chat(NORMAL, "%s: Error: Unable to calculate the SKY coordinates for the center of optical axis.\n",global.taskname);
      return NOT_OK;
    }

  
  headas_chat(NORMAL, " Optical Axis SKY coordinate X: %f\n",global.optaxis.skyx);
  headas_chat(NORMAL, " Optical Axis SKY coordinate Y: %f\n",global.optaxis.skyy);

  return OK;
}


/*
 *	off_calc
 *
 *
 *	DESCRIPTION:
 *           
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *    
 *         double sqrt(double x);
 *         double pow(double x, double y);  
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *       
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 */
int  off_calc(float *cx, float *cy, float *off) 
     
{
  double dx=0.,dy=0.;  

  dx=*cx-global.optaxis.skyx;
  dy=*cy-global.optaxis.skyy;

  *off=sqrt(dx*dx+dy*dy)*(PXL_S)/60.;

  if ((!strcmp(global.datamode,KWVL_DATAMODE_PD )) ||(!strcmp(global.datamode,KWVL_DATAMODE_PDPU)))
    {
      dx=*cx-CDETX;
      dy=*cy-CDETY;
      *off=sqrt(dx*dx+dy*dy)*(PXL_S)/60.;
    }
  else
    {
      dx=*cx-global.optaxis.skyx;
      dy=*cy-global.optaxis.skyy;
      *off=sqrt(dx*dx+dy*dy)*(PXL_S)/60.;
    }

  headas_chat(NORMAL, "CX: %f CY: %f Off-axis Angle (arcmin): %f \n",*cx,*cy,*off);


  return OK;

  /* off_end:  */

   return NOT_OK;


} /* off_calc */

/*
 *	vig_corr
 *
 *
 *	DESCRIPTION:
 *           
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *    
 *         double pow(double x, double y);  
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *       
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 */

int vig_corr(float *off, float *ene, long* nenerg, float *vcorr, VigInfo_t* viginfo)
{    
  register int i;
  float  vcoeff;

  for (i=0; i < *nenerg; i++)
    {
      vcoeff= viginfo->par0 * pow(viginfo->par1,ene[i]) + viginfo->par2;
      vcorr[i] = 1.-vcoeff*(*off)*(*off);
      if (vcorr[i] < 0)  vcorr[i]=0.;
    }
  
  return OK;

} /* vig_corr */



/*
 *	psf_corr
 *
 *
 *	DESCRIPTION:
 *           
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *    
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *       
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 */

int psf_corr( float *ene, long* nenerg, float *off,int *rx,float *wei, float *pcorr)
{     
  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  float c0,c1,c2,c3,jx,tcorr,icorr;
  float *par0,*par1,*par2,*par3;
  register int i,j;
  char  expr[256];
  int   status = OK , anynull ;
  float floatnull={0.};
  float pi={3.1415927},fk,fg,totflux;
  int   ncoef={4};
  long  extno;
  float ioff,iene;
  float stene={0.1},stoff={0.1};
  float minp,maxp,mane={0.},mine={0.};

  extno=-1;
  par0=malloc(sizeof(float)* ncoef);
  par1=malloc(sizeof(float)* ncoef);
  par2=malloc(sizeof(float)* ncoef);
  par3=malloc(sizeof(float)* ncoef);
  

  /* Set expression for CALDB query */
  if(!strcmp(KWVL_DATAMODE_PH,global.datamode) || !strcmp(global.datamode,KWVL_DATAMODE_TM ))
    sprintf(expr, "datamode.eq.%s", global.datamode);
  else
    sprintf(expr, "%s", HD_EXPR);


  /* read the coefficient  from psfne CALDB file */
  extno=-1;
  /* Derive CALDB gain filename */ 
  if (!(strcasecmp (global.par.psf_file, DF_CALDB)))
    {
      if (CalGetFileName( HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_PSF_DSET, global.par.psf_file, expr, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto psf_end;
	}
      extno++;
    }
 
  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.psf_file);
  

  /* Open input PSF file */  
  if ( fits_open_file(&fptr, global.par.psf_file, READONLY, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.psf_file);
      goto psf_end;
    }

  if(extno != -1)
    {
      /* move to extension number 'extno' */
      if (fits_movabs_hdu(fptr,(int)(extno), NULL,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find extension number '%d'\n", global.taskname, extno);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psf_file); 
	  goto psf_end;
	}
    }
  else
    { 
      if (!strcmp(global.datamode,KWVL_DATAMODE_TM ))
	{
	  if (fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_WTPSFCOEF,0,&status))
	    { 
	      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_WTPSFCOEF);
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psf_file);
	      goto psf_end;
	    }
	}
      else
	{
	  if (fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_PCPSFCOEF,0,&status))
	    { 
	      headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_PCPSFCOEF);
	      headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.psf_file);
	      goto psf_end;
	    }
	}
    }

  /* Get parameters values */
  if  (fits_read_col(fptr,TFLOAT,2,1,1,ncoef, &floatnull,par0,&anynull,&status))
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_COEF0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.psf_file);
      goto psf_end;
    } 
  if  (fits_read_col(fptr,TFLOAT,3,1,1,ncoef, &floatnull,par1,&anynull,&status))
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_COEF1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.psf_file);
      goto psf_end;
    }   
  if  (fits_read_col(fptr,TFLOAT,4,1,1,ncoef, &floatnull,par2,&anynull,&status))
    {  
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_COEF2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.psf_file);
      goto psf_end;
    }   
  if  (fits_read_col(fptr,TFLOAT,5,1,1,ncoef, &floatnull,par3,&anynull,&status))
    {  
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_COEF3);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.psf_file);
      goto psf_end;
    } 
  /* Close PSF file */
  if ( fits_close_file(fptr, &status) )
    { 
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.psf_file);
      goto psf_end;
    } 

  if(stoff == 0 || stene == 0)
    {
      headas_chat(NORMAL, "%s: Error: Unable to apply PSF correction.\n", global.taskname);
      goto psf_end;
    }

  ioff=*off/stoff;

  for (i=0; i < *nenerg; i++)
    {
      iene=ene[i]/stene;
      
      c0=par0[0]+ioff*par1[0]+iene*par2[0]+ioff*iene*par3[0];
      c1=par0[1]+ioff*par1[1]+iene*par2[1]+ioff*iene*par3[1];
      c2=par0[2]+ioff*par1[2]+iene*par2[2]+ioff*iene*par3[2];
      c3=par0[3]+ioff*par1[3]+iene*par2[3]+ioff*iene*par3[3];

      if((c3-1.) == 0)
	{
	  headas_chat(NORMAL, "%s: Error: Unable to apply PSF correction.\n", global.taskname);
	  goto psf_end;
	}

      totflux=2.*pi*c0*c1*c1+pi*c2*c2*(1.-c0)/(c3-1.);
      tcorr=0.;
      icorr=0.;
      for (j=0; j <= *rx; j++)
	{ 
	  
	  if(c2 == 0 || c1 == 0)
	    {
	      headas_chat(NORMAL, "%s: Error: Unable to apply PSF correction.\n", global.taskname);
	      goto psf_end;
	    }
	  
	  jx=1.*j+0.5;
	  fk=pi*c2*c2*(1.-c0)/(1.-c3)*(pow((1.+(jx/c2)*(jx/c2)),(1.-c3))-1.);
	  fg=2.*pi*c0*c1*c1*(1.-exp(-1.*jx*jx/2./(c1*c1)));
	  
	  tcorr=tcorr+((fg+fk)-icorr)*wei[j];
	  icorr=fg+fk;
	}
      
      if(totflux == 0)
	{
	  headas_chat(NORMAL, "%s: Error: Unable to apply PSF correction.\n", global.taskname);
	  goto psf_end;
	}
      pcorr[i]=tcorr/totflux;
    }
  
  minp=1000;
  maxp=-1000;
  tcorr=0;
   for (i=0; i < *nenerg; i++)
     {
       if (pcorr[i] > maxp )
	 { 
	   maxp=pcorr[i];
	   mane=ene[i];
	 }
       if (pcorr[i] < minp )
	 {
	   minp=pcorr[i];
	   mine=ene[i];
	 }
       tcorr=pcorr[i]+tcorr;
     }  
   tcorr=tcorr/(1.*(*nenerg));

   /*   headas_chat(NORMAL, " PSF CORRECTION  MIN: %f at %f \n", minp,mine);
	headas_chat(NORMAL, " PSF CORRECTION  MAX: %f at %f \n", maxp,mane);*/
   headas_chat(NORMAL, " PERC. OF FLUENCE WITHIN THE ROI:  %f %  (AT %f keV )  \n",pcorr[200]*100.,ene[200]);
   headas_chat(NORMAL, " PERC. OF FLUENCE WITHIN THE ROI:  %f %  (ON AVERAGE) \n",tcorr*100.);
  
  return OK;
  
 psf_end:   
  return NOT_OK;
  
} /* psf_corr */

/*
 *	write_arf
 *
 *
 *	DESCRIPTION:
 *            This routine writes output ARF fits file 
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 * 
 *           char *strcpy(char *dest, const char *src);
 *           char *strncat(char *dest, const char *src, size_t n);
 *           int fits_create_file(fitsfile **fptr, char *filename, int *status);
 *           int fits_create_tbl(fitsfile *fptr, int tbltype, long naxis2, int tfields,
 *                               char *ttype[], char *tform[], char *tunit[], 
 *                               char *extname, int *status);
 *           int fits_write_key_str(fitsfile *fptr, char *keyname, char *value,
 *                                  char *comment, int *status);
 *           int fits_write_col(fitsfile *fptr, int datatype, int colnum, long firstrow,
 *                              long firstelem, long nelements, DTYPE *array, int *status);
 *           int fits_close_file(fitsfile *fptr, int *status);           			      
 *           void printerror (int);
 *
 * 
 *	
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *        0.1.1: - BS 03/02/2003 - Adapted for HEADAS enviroment
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int write_arf (int cx, int cy, long nenerg, float *col1,float *col2,float *arf,char *arffile)
{
  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  int status;
  long firstrow, firstelem;
  /* primary header (null data) */
  /* table extension */
  int tfields=3;                              /* table will have 3 columns */
  char *ttype[] = { CLNM_ENERG_LO, CLNM_ENERG_HI,CLNM_SPECRESP}; /*define the name,  */
  char *tform[] = { "1E","1E","1E"};                   /* datatype,        */
  char *tunit[] = { "keV","keV","cm**2"};              /* physical units   */
  char strhist[256], date[25];

  char BaseName[MAXFNAME_LEN];
 
  status=OK;
 

  if (fits_create_file(&fptr, arffile, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, arffile);
      goto arf_end;
    }
 
  if ( fits_create_tbl( fptr, BINARY_TBL, nenerg, tfields, ttype, tform, tunit, KWVL_EXTNAME_SR, &status) )  
    {
      headas_chat(NORMAL,"%s: Error: Unable to create %s extension\n", global.taskname,KWVL_EXTNAME_SR );
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, arffile);
      goto arf_end;
    }
  
  /* Add keyword */

  if(fits_write_key_str(fptr, KWNM_TELESCOP, KWVL_TELESCOP, CARD_COMM_TELESCOP, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_TELESCOP);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_key_str(fptr, KWNM_INSTRUME,KWVL_INSTRUME, CARD_COMM_INSTRUME, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }


  if(fits_write_key_str(fptr, KWNM_HDUCLASS, KWVL_HDUCLASS, CARD_COMM_HDUCLASS, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_HDUCLASS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_key_str(fptr, "HDUVERS", "1.1.0", "Version of format (OGIP memo CAL/GEN/92-002a)", &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_HDUCLASS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_key_str(fptr, "HDUVERS1", "1.0.0", "Obsolete - included for backwards compatibility", &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_HDUCLASS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_key_str(fptr, "HDUVERS2", "1.1.0", "Obsolete - included for backwards compatibility", &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_HDUCLASS);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_key_str(fptr, KWNM_HDUCLAS1, KWVL_HDUCLAS1_R,CARD_COMM_HDUCLAS1_R, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_HDUCLAS1);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_key_str(fptr, KWNM_HDUCLAS2, KWVL_HDUCLAS2_S,CARD_COMM_HDUCLAS2_S, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_HDUCLAS2);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }


  SplitFilePath(global.par.rmf_file, NULL, BaseName);

  if(fits_write_key_str(fptr, KWNM_RESPFILE , BaseName  ,CARD_COMM_RESPFILE,&status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_RESPFILE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(!strcmp(KWVL_DATAMODE_PH,global.datamode) || !strcmp(global.datamode,KWVL_DATAMODE_TM ))
    {
      
      if(fits_write_key(fptr, TINT, "XRTSX", &cx, "source detector x coordinate",&status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to add XRTSX keyword\n", global.taskname);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
	  goto arf_end;
	}

 
      if(fits_write_key(fptr, TINT, "XRTSY", &cy, "source detector y coordinate",&status))
      {
	headas_chat(NORMAL, "%s: Error: Unable to add XRTSY keyword\n", global.taskname);
	headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
	goto arf_end;
      }
    }
  else
    { 
      
      if(fits_write_key(fptr, TINT, KWNM_XRTSDETX, &cx, CARD_COMM_XRTSDETX,&status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_XRTSDETX);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
	  goto arf_end;
	}
 
      if(fits_write_key(fptr, TINT, KWNM_XRTSDETY, &cy, CARD_COMM_XRTSDETY,&status))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_XRTSDETY);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
	  goto arf_end;
	}
    }
  
  if(fits_write_date(fptr, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_DATE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }
  /*CALDB KEYWORDS */
/*    if(fits_write_key_str(fptr,"CCLS0001","CPF"       ,"Dataset is Calibration Product File"      ,&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CCLS0001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

/*    if(fits_write_key_str(fptr,"CDTP0001","DATA"      ,"Dataset consists of real data"            ,&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CDTP0001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

/*    if(fits_write_key_str(fptr,"CCNM0001","SPECRESP"    ,"Type of Calibration data     "          ,&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CCNM0001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

/*    if(fits_write_key_str(fptr,"CVSD0001","2001-01-01","Date when this file should first be used ",&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CVSD0001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

/*    if(fits_write_key_str(fptr,"CVST0001","00:00:00"  ,"when this file should first be used"      ,&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CVST0001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

/*    if(fits_write_key_str(fptr,"CDES0001","XRT SPECTRAL RESPONSE"    ,"   "                       ,&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CVST0001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

/*    if(fits_write_key_str(fptr,"CBD10001","THETA(0) arcmin"  ,"dataset parameter boundary"     ,&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CBD10001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

/*    if(fits_write_key_str(fptr,"CBD20001","PHI(0) arcmin"  ,"dataset parameter boundary"     ,&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CBD20001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

/*    if(fits_write_key_str(fptr,"CBD30001","ENERG(0.1-17.0)keV"  ,"dataset parameter boundary"     ,&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CBD30001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

 
/*    sprintf(mode, "DATAMODE(%s)", global.datamode); */
  
/*    if(fits_write_key_str(fptr, "CBD40001" ,mode ,"dataset parameter boundary",&status)) */
/*      { */
/*        headas_chat(NORMAL, "write_arf: Error writing CBD40001 keyword in %s output file.\n", global.par.arf_file); */
/*        goto arf_end; */
/*      } */

  /* Write into output file computed values */ 
  firstrow  = 1;  /* first row in table to write   */
  firstelem = 1;  /* first element in row  (ignored in ASCII tables) */
  
  if(fits_write_col(fptr, TFLOAT, 1, firstrow, firstelem, nenerg, col1, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to fill %s column\n", global.taskname, CLNM_ENERG_LO);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_col(fptr, TFLOAT, 2, firstrow, firstelem, nenerg, col2, &status))
    { 
      headas_chat(NORMAL, "%s: Error: Unable to fill %s column.\n", global.taskname, CLNM_ENERG_HI);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_col(fptr, TFLOAT, 3, firstrow, firstelem, nenerg, arf, &status))
    { 
      headas_chat(CHATTY, "%s: Error: Unable to fill %s column\n", global.taskname, CLNM_SPECRESP);
      headas_chat(CHATTY, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  /* If history parameter is set write history records */ 
  if(HDpar_stamp(fptr, 2, &status))
    {
      headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
      goto arf_end;
    }
  
  if(global.hist)
    {

      GetGMTDateTime(date);

      sprintf(strhist, "File created  by %s (%s) at %s", global.taskname, global.swxrtdas_v,date );
      if(fits_write_history(fptr, strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto arf_end;
	}

      sprintf(strhist, "Used following files:");
      if(fits_write_history(fptr, strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto arf_end;
	}

      sprintf(strhist, "%s CALDB RMF file", global.par.rmf_file);
      if(fits_write_history(fptr, strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto arf_end;
	}

      sprintf(strhist, "%s CALDB mirror effective area file", global.par.mir_file);
      if(fits_write_history(fptr, strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto arf_end;
	}
      sprintf(strhist, "%s CALDB filter transmission file", global.par.flt_file);
      if(fits_write_history(fptr, strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto arf_end;
	}
      sprintf(strhist, "%s CALDB vignetting file", global.par.vig_file);
      if(fits_write_history(fptr, strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto arf_end;
	}
      sprintf(strhist, "%s CALDB PSF file", global.par.psf_file);
      if(fits_write_history(fptr, strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto arf_end;
	}
      sprintf(strhist, "%s Spectrum file", global.par.pha_file);
      if(fits_write_history(fptr, strhist, &status))
	{
	  headas_chat(CHATTY, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
	  goto arf_end;
	}


    }/* If (global.hist)*/
  

  if(fits_movabs_hdu(fptr, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }
  
  if(fits_write_key_str(fptr, KWNM_TELESCOP, KWVL_TELESCOP, CARD_COMM_TELESCOP, &status))
    {
      
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_TELESCOP);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }
  
  if(fits_write_key_str(fptr, KWNM_INSTRUME,KWVL_INSTRUME, CARD_COMM_INSTRUME, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }

  if(fits_write_date(fptr, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to add %s keyword\n", global.taskname, KWNM_DATE);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto arf_end;
    }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, arffile);
      goto arf_end;
    }



   /* close output arf file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, arffile);
      goto arf_end;
    }
  
  return OK;
  
 arf_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
  printerror( status );
  return NOT_OK;

}/* write_arf */


/*
 *
 *  update_arf_keywords
 *
 *  DESCRIPTION:
 *    
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int   update_arf_keywords(char* arffile){

  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  int  status = OK, bitpix=16;
  char BaseName[MAXFNAME_LEN];
  FitsCard_t     *card;
  FitsHeader_t   head;

  /* Open input file */  
  if ( fits_open_file(&fptr, arffile, READWRITE, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }
  
  /* Primary HDU */
  if(fits_movabs_hdu(fptr, 1, NULL, &status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to move in primary HDU\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }


  if(fits_update_key(fptr, TSTRING, KWNM_TELESCOP,  KWVL_TELESCOP,CARD_COMM_TELESCOP, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_TELESCOP);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }
      
  if(fits_update_key(fptr, TSTRING, KWNM_INSTRUME,  KWVL_INSTRUME,CARD_COMM_INSTRUME, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_INSTRUME);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }

  if(fits_update_key(fptr, TINT, "BITPIX", &bitpix, "number of bits per data pixel", &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, "BITPIX");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }

  /* 'SPECRESP' HDU */
  if(fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_SPECRESP,0,&status))
    {
      headas_chat(NORMAL, "%s: Error: Unable to find  '%s' extension\n", global.taskname, KWVL_EXTNAME_SPECRESP);
      headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }


  SplitFilePath(global.par.rmf_file, NULL, BaseName);
  if(fits_update_key(fptr, TSTRING, KWNM_RESPFILE, BaseName, CARD_COMM_RESPFILE, &status))
    {
      headas_chat(NORMAL,"%s: Error: Unable to write %s keyword\n", global.taskname, KWNM_RESPFILE);
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }

  /* Retrieve header pointer */    
  head=RetrieveFitsHeader(fptr);
  
  if(ExistsKeyWord(&head, "ARFVERSN", &card)){
    if(fits_delete_key(fptr, "ARFVERSN", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "ARFVERSN");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }
  }

  if(ExistsKeyWord(&head, "DETNAM", &card)){
    if(fits_delete_key(fptr, "DETNAM", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "DETNAM");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }
  }

  if(ExistsKeyWord(&head, "FILTER", &card)){
    if(fits_delete_key(fptr, "FILTER", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "FILTER");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }
  }

  if(ExistsKeyWord(&head, "HDUDOC", &card)){
    if(fits_delete_key(fptr, "HDUDOC", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "HDUDOC");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }
  }

  if(ExistsKeyWord(&head, "PHAFILE", &card)){
    if(fits_delete_key(fptr, "PHAFILE", &status )){
      headas_chat(NORMAL,"%s: Error: Unable to delete %s keyword\n", global.taskname, "PHAFILE");
      headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, arffile);
      goto upd_arf_key;
    }
  }


  /* Calculate checksum and add it in file */
  if (ChecksumCalc(fptr))
    {
      headas_chat(CHATTY, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
      headas_chat(CHATTY, "%s: Error: in '%s' file. \n ", global.taskname, arffile);
      goto upd_arf_key;
    }


   /* close output file */
  if ( fits_close_file(fptr, &status) )
    {
      headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, arffile);
      goto upd_arf_key;
    }


  return OK;

 upd_arf_key:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
  printerror( status );
  return NOT_OK;

} /* update_arf_keywords  */


/*
 *
 *  ComputeWmapBox
 *
 *  DESCRIPTION:
 *    
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int ComputeWmapBox(WmapBox_t * wmapbox){

  int nx=0,ny=0,i=0,j=0,cnt=0;

  headas_chat(CHATTY, "%s: Info: Computing SubImages boxes\n",  global.taskname);

  if(global.par.boxsize> wmapbox->x_width || global.par.boxsize> wmapbox->y_width){
    
    headas_chat(NORMAL, "%s: Warning: Subimage box size is larger than the spectrum extraction region,", global.taskname);
    headas_chat(NORMAL, "%s: Warning: resetting boxsize to the WMAP size\n", global.taskname);

    wmapbox->nbox = 1;
    wmapbox->box = (WmapBoxInfo_t*) malloc(sizeof(WmapBoxInfo_t));
    wmapbox->box[0].x_min = 0;
    wmapbox->box[0].x_max = wmapbox->x_width;
    wmapbox->box[0].y_min = 0;
    wmapbox->box[0].y_max = wmapbox->y_width;
    
    wmapbox->box[0].skycx = wmapbox->x_offset + wmapbox->box[0].x_min + (float)(wmapbox->box[0].x_max - wmapbox->box[0].x_min)/2.;
    wmapbox->box[0].skycy = wmapbox->y_offset + wmapbox->box[0].y_min + (float)(wmapbox->box[0].y_max - wmapbox->box[0].y_min)/2.;

    return OK;
  }

  nx = (wmapbox->x_width%global.par.boxsize)>(global.par.boxsize/2) ? (wmapbox->x_width/global.par.boxsize +1) : (wmapbox->x_width/global.par.boxsize);
  ny = (wmapbox->y_width%global.par.boxsize)>(global.par.boxsize/2) ? (wmapbox->y_width/global.par.boxsize +1) : (wmapbox->y_width/global.par.boxsize);

  wmapbox->nbox = nx*ny;
  wmapbox->box = (WmapBoxInfo_t*) malloc(wmapbox->nbox * sizeof(WmapBoxInfo_t));
  cnt=0;

  for(i=0; i<nx; i++){
    for(j=0; j<ny; j++){
    
      wmapbox->box[cnt].x_min = i*global.par.boxsize;
      wmapbox->box[cnt].y_min = j*global.par.boxsize;

      if(i==nx-1){ wmapbox->box[cnt].x_max = wmapbox->x_width; }
      else{ wmapbox->box[cnt].x_max = (i+1)*global.par.boxsize; }

      if(j==ny-1){ wmapbox->box[cnt].y_max = wmapbox->y_width; }
      else{ wmapbox->box[cnt].y_max = (j+1)*global.par.boxsize; }
      
      wmapbox->box[cnt].skycx = wmapbox->x_offset + wmapbox->box[cnt].x_min + (float)(wmapbox->box[cnt].x_max - wmapbox->box[cnt].x_min)/2.;
      wmapbox->box[cnt].skycy = wmapbox->y_offset + wmapbox->box[cnt].y_min + (float)(wmapbox->box[cnt].y_max - wmapbox->box[cnt].y_min)/2.;
      
      headas_chat(CHATTY, "%s: Info: Box %d: x_min %d x_max %d y_min %d y_max %d\n", 
		  global.taskname, cnt, wmapbox->box[cnt].x_min, wmapbox->box[cnt].x_max, wmapbox->box[cnt].y_min, wmapbox->box[cnt].y_max);

      cnt++;
    }
  }


  return OK;
} /*  ComputeWmapBox  */


/*
 *
 *  ReadVigInfo
 *
 *  DESCRIPTION:
 *    
 *
 *      Return Status: 
 *        0: OK
 *        1: NOT_OK
 */
int   ReadVigInfo(VigInfo_t* viginfo){
    
  fitsfile *fptr=NULL;       /* pointer to the FITS file, defined in fitsio.h */
  int    status = OK , hdutype, anynull ;
  long   extno;
  float  floatnull=0.;

  /* read the coefficient  from vigne CALDB file */
  extno=-1;
  /* Derive CALDB gain filename */ 
  if (!(strcasecmp (global.par.vig_file, DF_CALDB)))
    {
      if (CalGetFileName( HD_MAXRET, DF_NOW, DF_NOW, DF_NOW, DF_NOW, KWVL_VIGNET_DSET, global.par.vig_file, HD_EXPR, &extno))
	{
	  headas_chat(NORMAL, "%s: Error: Unable to query CALDB.\n", global.taskname);
	  goto viginfo_end;
	}
      extno++;
    }
  if(strcasecmp(global.par.expo_file,DF_NONE))
    headas_chat(NORMAL, ">>> using a not vignetted exposure map \n ");

  headas_chat(NORMAL, "%s: Info: Processing '%s' CALDB file.\n", global.taskname, global.par.vig_file);
  

  /* Open input VIG file */  
  if ( fits_open_file(&fptr, global.par.vig_file, READONLY, &status) )
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.vig_file);
      goto viginfo_end;
    }
  if (extno != -1)
    {
      /* Move into right extension */
      if ( fits_movabs_hdu(fptr, extno, &hdutype, &status) ) /* move to 2nd HDU */
	{ 
	  headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,extno);
	  headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.vig_file);
	  goto viginfo_end;
	}
    }
  else
    {
      if (fits_movnam_hdu(fptr,ANY_HDU,KWVL_EXTNAME_VIGCOEF,0,&status))
	{ 
	  headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_VIGCOEF);
	  headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, global.par.vig_file);
	  goto viginfo_end;
	} 
    }

  /* Get parameters values */
  if  (fits_read_col(fptr,TFLOAT,1,1,1,1, &floatnull,&viginfo->par0,&anynull,&status))
    { 
      headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR0);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vig_file);
      goto viginfo_end;
    }  

  if  (fits_read_col(fptr,TFLOAT,2,1,1,1, &floatnull,&viginfo->par1,&anynull,&status)) 
    { headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR1);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vig_file);
      goto viginfo_end;
    }  
  if (fits_read_col(fptr,TFLOAT,3,1,1,1, &floatnull,&viginfo->par2,&anynull,&status))
    { headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PAR2);
      headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.par.vig_file);
      goto viginfo_end;
    }  
  /* Close VIG file */
  if ( fits_close_file(fptr, &status) )
    { 
      headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.vig_file);
      goto viginfo_end;
    }

  return OK;

 viginfo_end:
  if (fptr != NULL)
    fits_close_file(fptr, &status);
   printerror( status ); 
   return NOT_OK;

} /* ReadVigInfo()  */




/*
 *	print_error
 *
 *
 *	DESCRIPTION:
 *           
 * 		  
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *    
 *        void fits_report_error (FILE *stream, int *status);
 *
 *      CHANGE HISTORY:
 *
 *        0.1.0: - AM    01/2003 - First version
 *       
 *
 *	AUTHORS:
 *
 *        ISAC Italian Swift Archive Center (FRASCATI)
 *
 */

void printerror( int status)
{
  /*****************************************************/
  /*          Print out cfitsio error messages         */
  /*****************************************************/
  if (status)
    fits_report_error(stderr, status); /* print error report */
  
  return;
}/* printerror */


