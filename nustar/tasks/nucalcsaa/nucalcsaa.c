/*
 *
 *	nucalcsaa.c
 *
 *	INVOCATION:
 *
 *		nucalcsaa [parameter=value ...]
 *
 *	DESCRIPTION:
 *
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 28/09/12 - First version
 *        0.1.1 - NS 06/11/12 - Replaced 'rename' call with 'RenameFile' routine
 *        0.1.2 - NS 15/11/12 - Bug fixed in 'FindClosestOrbitIndex' routine
 *        0.1.3 - RF 16/03/16 - New Strict/Optimized SAA filtering, new Tentacle SAA filtering routine 
 *        0.1.4 - RF 06/10/16 - Modified parameter file, optimizing call to fselect
 *
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nucalcsaa  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nucalcsaa.h"


/********************************/
/*         definitions          */
/********************************/

#define NUCALCSAA_C
#define NUCALCSAA_VERSION	"0.1.4"
#define PRG_NAME		"nucalcsaa"

/********************************/
/*           globals            */
/********************************/

Global_t global;
GlobalCallDB_t globalCalDB;

/*
 *	nucalcsaa_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from
 *                 nucalcsaa.par
 *
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result);
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nucalcsaa_info(void);
 *           void get_history(int *);
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 28/09/12 - First version
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int nucalcsaa_getpar()
{

    /* Input Housekeeping File Name */
    if(PILGetFname(PAR_HKFILE, global.par.hkfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HKFILE);
        goto Error;
    }

    /* Input Orbit File Name */
    if(PILGetFname(PAR_ORBITFILE, global.par.orbitfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ORBITFILE);
        goto Error;
    }

    /* Input Event File Name */
    if(PILGetFname(PAR_EVTFILE, global.par.evtfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EVTFILE);
        goto Error;
    }

    /* Output Housekeeping File Name */
    if(PILGetFname(PAR_OUTFILE, global.par.outfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTFILE);
        goto Error;
    }

    /* Input SAAPAR File Name */
    if(PILGetFname(PAR_SAAPARFILE, global.par.saaparfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SAAPARFILE);
        goto Error;
    }

    /* SAA type */
    if(PILGetInt(PAR_SAACALC, &global.par.saacalc))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SAACALC);
        goto Error;
    }

    /* SAA Cut mode */
    if(PILGetString(PAR_SAAMODE, global.par.saamode))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SAAMODE);
        goto Error;
    }
    
    /* Calculate SAA tentacle flag?*/
    if(PILGetBool(PAR_TENTACLE, &global.par.tentacle))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TENTACLE);
        goto Error;
    }

    /* Expression to select good events or NONE to do not make the selection */
    if(PILGetString(PAR_EVTEXPR, global.par.evtexpr))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_EVTEXPR);
        goto Error;
    }
    
    /* New Optimized SAA filtering RMS threshold */
    if(PILGetReal(PAR_OPTIMIZEDRMS, &global.par.MaxAllowedRMS))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OPTIMIZEDRMS);
        goto Error;
    }
    
    /* New Tentacle SAA filtering RMS threshold */
    if(PILGetReal(PAR_TENTACLERMS, &global.par.TentacleCutRMSThreshold))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TENTACLERMS);
        goto Error;
    }
    
    /* Eliminate the source for events rate estimation */
    if(PILGetBool(PAR_ELIMINATESOURCE, &global.par.ElimiateSource))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_ELIMINATESOURCE);
        goto Error;
    }
    
    /* Sigma upper limit cut-off for source elimination */
    if(PILGetReal(PAR_SOURCETHR, &global.par.sourcethr))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SOURCETHR);
        goto Error;
    }
    
    /* Apply Longitude restriction for new tentacle calculation? */
    if(PILGetBool(PAR_TENTACLEREGCUT, &global.par.TentacleCutRMSRegionRestriction))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TENTACLEREGCUT);
        goto Error;
    }
    
    /* Perform Sanity Checks for new optimized SAA calculation?  */
    if(PILGetBool(PAR_OPTIMIZEDSANITYCHECK, &global.par.SanityChecks))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OPTIMIZEDSANITYCHECK);
        goto Error;
    }
    
    /* Perform Sanity Checks for new Tentacle SAA calculation?  */
    if(PILGetBool(PAR_TENTACLESANITYCHECK, &global.par.TentacleCutRMSSanityChecks))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TENTACLESANITYCHECK);
        goto Error;
    }

    nucalcsaa_info();

    return OK;

Error:
    return NOT_OK;

} /* nucalcsaa_getpar */


/*
 *	nucalcsaa_work
 *
 *
 *	DESCRIPTION:
 *
 *
 *
 *      FUNCTION CALL:
 *             int nucalcsaa_checkinput();
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
 *        0.1.0 - NS 28/09/12 - First version
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int nucalcsaa_work()
{
    int                status = OK, i=0;
    char               cmd[BUF_SIZE];
    long               extfile=-1;
    HKRow_t            *hkinfo=NULL;
    EVTRow_t           *evtinfo=NULL;
    OrbitRow_t         *orbitinfo=NULL;
    double             *FilteredRate=NULL, *FilteredRateTentacle=NULL;
    BTYPE              *SoftSAA=NULL, *SoftTentacled=NULL;
    int                hknrows=0, evtnrows=0, orbitnrows=0;
    struct gti_struct  gti;


    if(nucalcsaa_checkinput())
        goto Error;

    /* Create the task temporary directory */
    if(mkdir(global.tmp.dirname,0777)) {
        headas_chat(NORMAL, "%s: Error: Unable to create the temporary directory '%s'\n", global.taskname, global.tmp.dirname);
        goto Error;
    }


    /* Get Observation Info from input file */
    if( GetObsInfo(global.par.evtfile, KWVL_EXTNAME_EVT, &global.obsinfo) ) {
        headas_chat(NORMAL, "%s: Error: Unable to get info from input '%s' file.\n", global.taskname, global.par.evtfile);
        goto Error;
    }


    /* Derive CALDB SAAPAR filename */
    extfile=-1;
    if ( !strcasecmp(global.par.saaparfile,DF_CALDB) )
    {
        if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_SAAPAR_DSET, global.par.saaparfile, "type.eq.saa", &extfile, KWVL_INSTRUME_FPM, HD_DETNAM))
        {

            headas_chat(NORMAL, "%s: Error: Unable to query CALDB for saaparfile parameter.\n", global.taskname);
            goto Error;
        }
        extfile++;
    }

    headas_chat(NORMAL, "%s: Info: global.par.saaparfile = '%s'.\n", global.taskname,global.par.saaparfile);
    headas_chat(NORMAL, "%s: Info: global.par.hkfile     = '%s'.\n", global.taskname,global.par.hkfile);
    

    /* Get SAAPAR Info from saapar input file */
    if ( ReadSAAParInfo(global.par.saaparfile, &global.saaparinfo) ) {
        headas_chat(NORMAL, "%s: Error: Unable to get data from input '%s' saaparfile.\n", global.taskname, global.par.saaparfile);
        goto Error;
    }

#ifdef WRITE_DEBUG
    PrintCallDBParam(&globalCalDB);
#endif
       
    if( strcasecmp(global.par.evtexpr,"NONE")!=0  && ( strcasecmp(global.par.saamode,"NONE")!=0 || global.par.tentacle!=0 ) ) 
    {
	  headas_chat(NORMAL, "%s: Info: Creating '%s' symbolic link to input event file.\n", global.taskname, global.tmp.fsel_infile);
	  if( CreateAbsSymbolicLink(global.par.evtfile, global.tmp.fsel_infile) )
	  {
	      headas_chat(NORMAL, "%s: Error: unable to create symbolic link '%s' to '%s'\n", global.taskname, global.tmp.fsel_infile, global.par.evtfile);
	      goto Error;
	  }

	  /* Execute fselect to screen events */
	  headas_chat(NORMAL, "%s: Info: Running fselect, creating filtered '%s' temporary event file.\n", global.taskname, global.tmp.fsel_outfile);
	  /*   sprintf(expr, "STATUS==b00000000xx0xx000"); */
	  sprintf(cmd, "fselect infile=%s outfile=%s copyall=yes expr=\"%s\"", global.tmp.fsel_infile, global.tmp.fsel_outfile, global.par.evtexpr);
	
	  fflush(stdout);	
	  status = system(cmd);
	  if(status!=0) {
	      headas_chat(NORMAL, "%s: Error: Unable to create '%s' file.\n", global.taskname, global.tmp.fsel_outfile);
	      goto Error;
	  }

	  strcpy(global.tmp.loc_evtfile,global.tmp.fsel_outfile);
	  
	  if( unlink(global.tmp.fsel_infile) )
	  {
	      headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary link.\n", global.taskname, global.tmp.fsel_infile);
	  }
    }
    else
    {
        headas_chat(NORMAL, "%s: Info: Creating '%s' symbolic link to input event file.\n", global.taskname, global.tmp.loc_evtfile);
        if( CreateAbsSymbolicLink(global.par.evtfile, global.tmp.loc_evtfile) )
        {
            headas_chat(NORMAL, "%s: Error: unable to create symbolic link '%s' to '%s'\n", global.taskname, global.tmp.loc_evtfile, global.par.evtfile);
            goto Error;
        }
    }


    /* Read Housekeeping input file */
    if( ReadHouseKeeping(global.par.hkfile, &hkinfo, &hknrows) )
    {
        headas_chat(NORMAL, "%s: Error: Unable to read %s file\n", global.taskname, global.par.hkfile);
        goto Error;
    }

    
    /* Read EventFillered input file */
    if( ReadEvent(global.tmp.loc_evtfile, &evtinfo, &evtnrows) )
    {
        headas_chat(NORMAL, "%s: Error: Unable to read %s file\n", global.taskname, global.tmp.loc_evtfile);
        goto Error;
    }

    /* Read Orbit input file */
    if( ReadOrbit(global.par.orbitfile, &orbitinfo, &orbitnrows) )
    {
        headas_chat(NORMAL, "%s: Error: Unable to read %s file\n", global.taskname, global.par.orbitfile);
        goto Error;
    }


    /* Compute Filtered Rate */
    if( ComputeFilteredRate(global.tmp.loc_evtfile, hkinfo, hknrows, &FilteredRate) ) {
        goto Error;
    }
    
    headas_chat(NORMAL, "%s: Info: hknrows                 = %d\n", global.taskname, hknrows);
    headas_chat(NORMAL, "%s: Info: evtnrows                = %d.\n", global.taskname, evtnrows);
    headas_chat(NORMAL, "%s: Info: orbitnrows              = %d\n", global.taskname, orbitnrows);

    /* Create a copy of FilteredRate */
    FilteredRateTentacle = (double*)malloc(hknrows*sizeof(double));
    if(FilteredRateTentacle==NULL) {
        headas_chat(CHATTY,"%s: Error: memory allocation failure.\n", global.taskname);
        goto Error;
    }
    for(i=0; i<hknrows; i++)
    {
        FilteredRateTentacle[i] = FilteredRate[i];
    }


    /* Read GTI info from GTI extension of input event file */
    if(HDgti_read(global.tmp.loc_evtfile, &gti, KWVL_EXTNAME_GTI, 0, 0, 0, 0, &status)) {
        headas_chat(NORMAL, "%s: Error: Unable to read GTI info from '%s' file.\n", global.taskname, global.tmp.loc_evtfile);
        goto Error;
    }

    /*if( CopyCallDBParam(&globalCalDB) )
    {
	headas_chat(NORMAL, "%s: Error: Unable to read CalDB param.\n", global.taskname);
	goto Error;
    }*/
    
    if( global.par.saacalc == SAACALC_OLD ) 
    {
	/* Allocate memory to storage SoftSAA info */
	SoftSAA = (BTYPE*)calloc(hknrows, sizeof(BTYPE));  /* array initialized to zero */
	if(SoftSAA==NULL) {
	    headas_chat(CHATTY,"%s: Error: memory allocation failure.\n", global.taskname);
	    goto Error;
	}

	/* Compute SoftSAA flag */
	if( FindSAAs(hkinfo, hknrows, FilteredRate, orbitinfo, orbitnrows, &gti, SoftSAA) ) {
	    headas_chat(NORMAL, "%s: Error: Unable to compute SoftSAA flag.\n", global.taskname);
	    goto Error;
	}
	

	/* Allocate memory to storage SoftTentacled info */
	SoftTentacled = (BTYPE*)calloc(hknrows, sizeof(BTYPE));  /* array initialized to zero */
	if(SoftTentacled==NULL) {
	    headas_chat(CHATTY,"%s: Error: memory allocation failure.\n", global.taskname);
	    goto Error;
	}

	/* Compute SoftTentacled flag */
	if(global.par.tentacle) {
	    if( FindSAATentacle(hkinfo, hknrows, FilteredRateTentacle, orbitinfo, orbitnrows, &gti, SoftSAA, SoftTentacled) ) {
		headas_chat(NORMAL, "%s: Error: Unable to compute SoftTentacled flag.\n", global.taskname);
		goto Error;
	    }
	}
    }
    else if( global.par.saacalc == SAACALC_NEW ) 
    {
	/* Allocate memory to storage SoftSAA info */
	SoftSAA = (BTYPE*)calloc(hknrows, sizeof(BTYPE));  /* array initialized to zero */
	if(SoftSAA==NULL) {
	    headas_chat(CHATTY,"%s: Error: memory allocation failure.\n", global.taskname);
	    goto Error;
	}

	/* Compute SoftSAA flag */
	if( FindSAAs_new(hkinfo, hknrows, evtinfo, evtnrows, FilteredRate, orbitinfo, orbitnrows, &gti, SoftSAA) ) {
	    headas_chat(NORMAL, "%s: Error: Unable to compute SoftSAA flag.\n", global.taskname);
	    goto Error;
	}
	
	/* Allocate memory to storage SoftTentacled info */
	SoftTentacled = (BTYPE*)calloc(hknrows, sizeof(BTYPE));  /* array initialized to zero */
	if(SoftTentacled==NULL) {
	    headas_chat(CHATTY,"%s: Error: memory allocation failure.\n", global.taskname);
	    goto Error;
	}

	/* Compute SoftTentacled flag */
	if(global.par.tentacle) {
	    if( FindSAATentacle_new(hkinfo, hknrows, evtinfo, evtnrows, FilteredRateTentacle, orbitinfo, orbitnrows, &gti, SoftSAA, SoftTentacled) ) {
		headas_chat(NORMAL, "%s: Error: Unable to compute SoftTentacled flag.\n", global.taskname);
		goto Error;
	    }
	}
	
    }
    /* Write output file */
    if( WriteOutFile(global.par.hkfile, global.tmp.loc_outfile, hknrows, SoftSAA, SoftTentacled) ) {
        headas_chat(NORMAL, "%s: Error: Unable to write output file.\n", global.taskname);
        goto Error;
    }
    

    /* rename temporary file into output file */
    if (RenameFile(global.tmp.loc_outfile,global.par.outfile) == -1)
    {
	headas_chat(NORMAL, "%s: Error: Unable to rename temporary file.\n", global.taskname);
	goto Error;
    }

    /* Remove local temporary input evtfile  */
    if( unlink(global.tmp.loc_evtfile) )
    {
	headas_chat(NORMAL, "%s: Warning: Unable to remove '%s' temporary file.\n", global.taskname, global.tmp.loc_evtfile);
    }


    /* Delete the task temporary directory */
    if(rmdir(global.tmp.dirname))
    {
	perror("rmdir");
	headas_chat(NORMAL, "%s: Warning: Unable to delete the temporary directory '%s'\n", global.taskname, global.tmp.dirname);
    }

    headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, global.par.outfile);

    headas_chat(MUTE,"---------------------------------------------------------------------\n");
    headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
    headas_chat(MUTE,"---------------------------------------------------------------------\n");

    return OK;

Error:

    return NOT_OK;
} /* nucalcsaa_work */


/*
 *	nucalcsaa
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
 *             void nucalcsaa_getpar(void);
 * 	       void nucalcsaa_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *
 *      CHANGE HISTORY:
 *        0.1.0 - NS 28/09/12 - First version
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int nucalcsaa()
{
    /* set HEADAS globals */
    set_toolname(PRG_NAME);
    set_toolversion(NUCALCSAA_VERSION);

    get_toolnamev(global.taskname);

    GetNuSTARDASVersion(global.nustardas_v);
    global.warning=0;

    /* Get parameter values */
    if ( nucalcsaa_getpar() == OK)
    {

        if ( nucalcsaa_work())
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
            if (FileExists(global.tmp.loc_outfile))
                remove (global.tmp.loc_outfile);
        }

    }

    return OK;

pdcorr_end:

    if (FileExists(global.tmp.loc_outfile))
        remove (global.tmp.loc_outfile);
    return NOT_OK;

} /* nucalcsaa */


/*
 *	nucalcsaa_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - NS 28/09/12 - First version
 *
 */
void nucalcsaa_info(void)
{
    headas_chat(NORMAL,"---------------------------------------------------------------------\n");
    headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
    headas_chat(NORMAL,"---------------------------------------------------------------------\n");
    headas_chat(NORMAL,"\t\t Input Parameters List: \n");
    headas_chat(NORMAL,"Name of the input Housekeeping file                      :'%s'\n",global.par.hkfile);
    headas_chat(NORMAL,"Name of the input Orbit file                             :'%s'\n",global.par.orbitfile);
    headas_chat(NORMAL,"Name of the input Event file                             :'%s'\n",global.par.evtfile);
    headas_chat(NORMAL,"Name of the output Housekeeping file                     :'%s'\n",global.par.outfile);
    headas_chat(NORMAL,"SAA calculation algorithm ('1'=OLD; '2'=NEW)             :'%d'\n",global.par.saacalc);
    headas_chat(NORMAL,"SAA removal function mode                                :'%s'\n",global.par.saamode);
    if (global.hist)
        headas_chat(CHATTY,"Write HISTORY keywords in output file                    : yes\n");
    else
        headas_chat(CHATTY,"Write HISTORY keywords in output file                    : no\n");
    
    if (headas_clobpar)
        headas_chat(CHATTY,"Overwrite existing output file                           : yes\n");
    else
        headas_chat(CHATTY,"Overwrite existing output file                           : no\n");
            
    headas_chat(NORMAL,"New Optimized SAA filtering RMS threshold                :'%f'\n",global.par.MaxAllowedRMS);
    headas_chat(NORMAL,"New Tentacle SAA filtering RMS threshold                 :'%f'\n",global.par.TentacleCutRMSThreshold);

    if (global.par.ElimiateSource)
        headas_chat(NORMAL,"Eliminate the source for events rate estimation          : yes\n");
    else
        headas_chat(NORMAL,"Eliminate the source for events rate estimation          : no\n");

    headas_chat(NORMAL,"Sigma upper limit cut-off for source elimination         :'%f'\n",global.par.sourcethr);
    
    if (global.par.TentacleCutRMSRegionRestriction)
        headas_chat(NORMAL,"Apply Longitude restriction for new tentacle calculation : yes\n");
    else
        headas_chat(NORMAL,"Apply Longitude restriction for new tentacle calculation : no\n");
    if (global.par.SanityChecks)
        headas_chat(NORMAL,"Perform Sanity Checks for new optimized SAA calculation  : yes\n");
    else
        headas_chat(NORMAL,"Perform Sanity Checks for new optimized SAA calculation  : no\n");
    if (global.par.TentacleCutRMSSanityChecks)
        headas_chat(NORMAL,"Perform Sanity Checks for new Tentacle SAA calculation   : yes\n");
    else
        headas_chat(NORMAL,"Perform Sanity Checks for new Tentacle SAA calculation   : no\n");
    
    headas_chat(NORMAL,"---------------------------------------------------------------------\n");

} /* nucalcsaa_info */


/*
 *	nucalcsaa_checkinput
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
 *        0.1.0: - NS 28/09/12 - First version
 *
 */
int nucalcsaa_checkinput(void)
{
    char           BaseName[MAXFNAME_LEN], DirName[MAXFNAME_LEN];
    pid_t          tmp;

    tmp=getpid();


    if(strcasecmp(global.par.saamode,"NONE") && strcasecmp(global.par.saamode,"STRICT") && strcasecmp(global.par.saamode,"OPTIMIZED") )
    {
        headas_chat(NORMAL, "%s: Error: input value '%s' not allowed for input parameter 'saamode'.\n", global.taskname, global.par.saamode);
        goto check_end;
    }


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
            if( strcmp(global.par.outfile,global.par.hkfile) ) {
                if(remove (global.par.outfile) == -1)
                {
                    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
                    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.outfile);
                    goto check_end;
                }
            }
        }
    }


    /* Set temporary task directory name */

    sprintf(global.tmp.dirname, "%d_tmp_nucalcsaa", (int)tmp);


    /* Derive temporary filename */

    SplitFilePath(global.par.outfile, DirName, BaseName);
    sprintf(global.tmp.loc_outfile, "%s/%dout_%s", global.tmp.dirname, (int)tmp, BaseName);

    SplitFilePath(global.par.evtfile, DirName, BaseName);
    sprintf(global.tmp.fsel_infile, "%s/%dfselin_%s", global.tmp.dirname, (int)tmp, BaseName);
    sprintf(global.tmp.fsel_outfile, "%dfselout_%s", (int)tmp, BaseName);
    sprintf(global.tmp.loc_evtfile, "%s/%s", global.tmp.dirname, BaseName);


    return OK;

check_end:
    return NOT_OK;
}


int ReadEvent(char *filename, EVTRow_t **evtinfo, int *evtnrows)
{
    unsigned		FromRow, ReadRows, n, nCols;
    int			evtcount=0, status=OK;
    int			PI=0;
    EvtCol_t		evtcol;
    Bintable_t		evttable;
    FitsHeader_t	evthead;
    FitsFileUnit_t	evtunit=NULL;

    TMEMSET0( &evttable, Bintable_t );
    TMEMSET0( &evthead, FitsHeader_t );
    
    headas_chat(NORMAL, "%s: Info: Processing %s file.\n", global.taskname, filename);

    /* Open readonly input evt file */
    if ((evtunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
        goto ReadEvent_end;
    }

    /* Move in EVENTS extension in input hk file */
    if (fits_movnam_hdu(evtunit, ANY_HDU, KWVL_EXTNAME_EVT, 0, &status))
    {
        headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
        if( CloseFitsFile(evtunit))
        {
            headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
        }
        goto ReadEvent_end;
    }

    /* Retrieve header pointer */
    evthead=RetrieveFitsHeader(evtunit);

    /* Read bintable */
    GetBintableStructure(&evthead, &evttable, BINTAB_ROWS, 0, NULL);
    nCols=evttable.nColumns;

    if(!evttable.MaxRows)
    {
        headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
        goto ReadEvent_end;
    }


    /* Get needed columns number from name */

    /* Time */
    if ((evtcol.TIME=ColNameMatch(CLNM_TIME, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }

    /* PI */
    if ((evtcol.PI=ColNameMatch(CLNM_PI, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PI);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }
    
    /* DET_ID */
    if ((evtcol.DET_ID=ColNameMatch(CLNM_DET_ID, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DET_ID);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }
    
    /* RAWX */
    if ((evtcol.RAWX=ColNameMatch(CLNM_RAWX, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWX);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }
    
    /* RAWY */
    if ((evtcol.RAWY=ColNameMatch(CLNM_RAWY, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RAWY);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }
    
    /* DET1X */
    if ((evtcol.DET1X=ColNameMatch(CLNM_DET1X, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DET1X);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }
    
    /* DET1Y */
    if ((evtcol.DET1Y=ColNameMatch(CLNM_DET1Y, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DET1Y);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }
    
    
    /* X */
    if ((evtcol.X=ColNameMatch(CLNM_X, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_X);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }
    
    /* Y */
    if ((evtcol.Y=ColNameMatch(CLNM_Y, &evttable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_Y);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadEvent_end;
    }
    
    

    EndBintableHeader(&evthead, &evttable);


    /* Allocate memory to storage all data */
    *evtnrows = evttable.MaxRows;
    *evtinfo = (EVTRow_t *)calloc(*evtnrows, sizeof(EVTRow_t));
    if(*evtinfo==NULL) {
        headas_chat(CHATTY,"%s: Error: ReadEvent: evtinfo memory allocation failure.\n", global.taskname);
        goto ReadEvent_end;
    }


    /* Read blocks of bintable rows */
    FromRow = 1;
    ReadRows=evttable.nBlockRows;
    while(ReadBintable(evtunit, &evttable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
        for(n=0; n<ReadRows ; ++n)
        {
            (*evtinfo)[evtcount].Time = DVEC(evttable,n,evtcol.TIME);
            PI = JVEC(evttable,n,evtcol.PI);
            (*evtinfo)[evtcount].energy = 0.04*(double)(PI)+1.6; /*valdbl/10.0 + 3.0;  */
	    (*evtinfo)[evtcount].DetectorID = BVEC(evttable,n,evtcol.DET_ID);
	    (*evtinfo)[evtcount].RawX = BVEC(evttable,n,evtcol.RAWX);
	    (*evtinfo)[evtcount].RawY = BVEC(evttable,n,evtcol.RAWY);
	    
	    (*evtinfo)[evtcount].Det1X = IVEC(evttable,n,evtcol.DET1X);
	    (*evtinfo)[evtcount].Det1Y = IVEC(evttable,n,evtcol.DET1Y);
	    (*evtinfo)[evtcount].X = IVEC(evttable,n,evtcol.X);
	    (*evtinfo)[evtcount].Y = IVEC(evttable,n,evtcol.Y);
	    
	    /* headas_chat(NORMAL,"%s: Info: Time[%d]=%f, DetID=%d, Rawx=%d, RawY=%d\n", global.taskname,evtcount,(*evtinfo)[evtcount].Time,(*evtinfo)[evtcount].DetectorID,(*evtinfo)[evtcount].RawX,(*evtinfo)[evtcount].RawY);*/
	    
            evtcount++;
        }

        FromRow += ReadRows;
        ReadRows = BINTAB_ROWS;

    }/* while */


    return OK;

ReadEvent_end:
    if (evthead.first)
        ReleaseBintable(&evthead, &evttable);

    return NOT_OK;
    
}/*ReadEvent */

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


    headas_chat(CHATTY, "%s: Info: Processing %s file.\n", global.taskname, filename);

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

    /* SHLDLO */
    if ((hkcol.SHLDLO=ColNameMatch(CLNM_SHLDLO, &hktable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SHLDLO);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadHouseKeeping_end;
    }

    /* SHLDHI */
    if ((hkcol.SHLDHI=ColNameMatch(CLNM_SHLDHI, &hktable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SHLDHI);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadHouseKeeping_end;
    }

    /* NACCEPT */
    if ((hkcol.NACCEPT=ColNameMatch(CLNM_NACCEPT, &hktable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_NACCEPT);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadHouseKeeping_end;
    }

    /* NREJECT */
    if ((hkcol.NREJECT=ColNameMatch(CLNM_NREJECT, &hktable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_NREJECT);
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
    if(*hkinfo==NULL) {
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
            (*hkinfo)[hkcount].ShieldRateLow = JVEC(hktable,n,hkcol.SHLDLO);
            (*hkinfo)[hkcount].ShieldRateHigh = JVEC(hktable,n,hkcol.SHLDHI);
            (*hkinfo)[hkcount].NAcceptedEvents = IVEC(hktable,n,hkcol.NACCEPT);
            (*hkinfo)[hkcount].NRejectedEvents = IVEC(hktable,n,hkcol.NREJECT);
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
 *      ReadOrbit
 *
 *	DESCRIPTION:
 *           Routine to read Orbit input file
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int ReadOrbit(char *filename, OrbitRow_t **info, int *nrows)
{
    unsigned           FromRow, ReadRows, n, nCols;
    int                	count=0, status=OK;
    OrbitCol_t         	incol;
    Bintable_t		intable;
    FitsHeader_t	inhead;
    FitsFileUnit_t	inunit=NULL;
    char 		fpm[2];
    
    checkFMP(global.par.evtfile,fpm);
    
    TMEMSET0( &intable, Bintable_t );
    TMEMSET0( &inhead, FitsHeader_t );


    headas_chat(CHATTY, "%s: Info: Processing %s file.\n", global.taskname, filename);

    /* Open readonly input orbit file */
    if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
        goto ReadOrbit_end;
    }

    /* Move in ORBIT extension in input orbit file */
    if (fits_movnam_hdu(inunit, ANY_HDU, KWVL_EXTNAME_ORBIT, 0, &status))
    {
        headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_ORBIT);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
        if( CloseFitsFile(inunit))
        {
            headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
        }
        goto ReadOrbit_end;
    }

    /* Retrieve header pointer */
    inhead=RetrieveFitsHeader(inunit);

    /* Read bintable */
    GetBintableStructure(&inhead, &intable, BINTAB_ROWS, 0, NULL);
    nCols=intable.nColumns;

    if(!intable.MaxRows)
    {
        headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
        goto ReadOrbit_end;
    }


    /* Get needed columns number from name */

    if ((incol.TIME=ColNameMatch(CLNM_TIME, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadOrbit_end;
    }

    if ((incol.SAA=ColNameMatch(CLNM_SAA, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SAA);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadOrbit_end;
    }
    
    if ((incol.SAA_A=ColNameMatch(CLNM_SAA_A, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SAA_A);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadOrbit_end;
    }
    
    if ((incol.SAA_B=ColNameMatch(CLNM_SAA_B, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SAA_B);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadOrbit_end;
    }

    if ((incol.OCCULTED=ColNameMatch(CLNM_OCCULTED, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OCCULTED);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadOrbit_end;
    }

    /*   if ((incol.DAY=ColNameMatch(CLNM_DAY, &intable)) == -1) */
    /*     { */
    /*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_DAY); */
    /*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
    /*       goto ReadOrbit_end; */
    /*     } */

    if ((incol.SLEW=ColNameMatch(CLNM_SLEW, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SLEW);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadOrbit_end;
    }

    /*   if ((incol.GEOCOR=ColNameMatch(CLNM_GEOCOR, &intable)) == -1) */
    /*     { */
    /*       headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GEOCOR); */
    /*       headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename); */
    /*       goto ReadOrbit_end; */
    /*     } */

    if ((incol.GEODETIC=ColNameMatch(CLNM_GEODETIC, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GEODETIC);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadOrbit_end;
    }

    /* Check columns multiplicity */
    if(intable.Multiplicity[incol.GEODETIC]!=3)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column with bad multiplicity\n", global.taskname, CLNM_GEODETIC);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ReadOrbit_end;
    }


    EndBintableHeader(&inhead, &intable);


    /* Allocate memory to storage all data */
    *nrows = intable.MaxRows;
    *info = (OrbitRow_t *)calloc(*nrows, sizeof(OrbitRow_t));
    if(*info==NULL) {
        headas_chat(CHATTY,"%s: Error: ReadOrbit: memory allocation failure.\n", global.taskname);
        goto ReadOrbit_end;
    }


    /* Read blocks of bintable rows */
    FromRow = 1;
    ReadRows=intable.nBlockRows;
    while(ReadBintable(inunit, &intable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
        for(n=0; n<ReadRows ; ++n)
        {
            (*info)[count].Time = DVEC(intable,n,incol.TIME);
	    (*info)[count].SAAFlag = BVEC(intable,n,incol.SAA);
	    (*info)[count].SAA_A = BVEC(intable,n,incol.SAA_A);
	    (*info)[count].SAA_B = BVEC(intable,n,incol.SAA_B);
	    if(!strcasecmp(fpm,"A"))
	    {
		  (*info)[count].SAAFlagTentacle = BVEC(intable,n,incol.SAA);
	    }
	    else if(!strcasecmp(fpm,"B"))
	    {
		  (*info)[count].SAAFlagTentacle = BVEC(intable,n,incol.SAA);
	    }
	    else
	    {
		  (*info)[count].SAAFlagTentacle = BVEC(intable,n,incol.SAA);
	    }
            (*info)[count].occulted = BVEC(intable,n,incol.OCCULTED);
            (*info)[count].slew = BVEC(intable,n,incol.SLEW);
            (*info)[count].Latitude = EVECVEC(intable,n,incol.GEODETIC,0);
            (*info)[count].Longitude = EVECVEC(intable,n,incol.GEODETIC,1);

            count++;
        }

        FromRow += ReadRows;
        ReadRows = BINTAB_ROWS;

    }/* while */


    return OK;

ReadOrbit_end:
    if (inhead.first)
        ReleaseBintable(&inhead, &intable);

    return NOT_OK;

} /* ReadOrbit */

DTYPE GetMinTimeEvt(EVTRow_t *evtinfo,int evtnrows)
{
    return evtinfo[0].Time;
}

DTYPE GetMaxTimeEvt(EVTRow_t *evtinfo,int evtnrows)
{
    return evtinfo[evtnrows-1].Time;
}

DTYPE GetMinTime(HKRow_t *hkinfo,int hknrows)
{
    return hkinfo[0].Time;
} /* GetMinTime */

DTYPE GetMaxTime(HKRow_t *hkinfo,int hknrows)
{
    return hkinfo[hknrows-1].Time;
} /* GetMaxTime */


/* GetIndex using the number of bins */
unsigned int GetIndex(double MinTime, double MaxTime, unsigned int NBins, double TestTime)
{
    double RelativeTime = 0.0;
    double BinWidth = 0.0;
    
    RelativeTime = TestTime - MinTime;
    BinWidth = (MaxTime - MinTime) / NBins;

    return (unsigned int) (RelativeTime / BinWidth);
} /* GetIndex */

int WithinSpecialGTI(DTYPE in)
{
    return 1;
} /* WithinSpecialGTI */

int FindClosestIndex(int m_LastClostestIndex,DTYPE Time,HKRow_t *hkinfo,int hknrows) 
{
  
  unsigned int Size = hknrows;
  unsigned int last = hknrows-1;
  int i;
  /* Binary search: */
  
  unsigned int upper = Size;
  unsigned int center = 1;
  unsigned int lower = 0;
  int ClosestFast = -1;
  int ClosestSlow = -1;
  
  if (Size == 0) 
  {
	headas_chat(NORMAL, "%s: Info: Find closest: The size of this data set is zero!\n",global.taskname);  
	return -1;
  }
  
  if (Time <= hkinfo[0].Time) 
  {
	return 0;
  }
  
  if (Time >= hkinfo[last].Time) 
  {
	return Size - 1; 
  }
  
  
  if (m_LastClostestIndex > 0 && Time > hkinfo[m_LastClostestIndex-1].Time && Time - hkinfo[m_LastClostestIndex-1].Time < 20) 
  {
    ClosestSlow = -1;
    for (i = m_LastClostestIndex-1; i < Size; ++i) 
    {
      if (hkinfo[i].Time > Time) 
      {
        ClosestSlow = i;
        break;
      }
    }
    if (hkinfo[ClosestSlow].Time - Time < Time - hkinfo[ClosestSlow-1].Time) 
    {
      m_LastClostestIndex = ClosestSlow;
      return ClosestSlow; 
    } 
    else 
    {
      m_LastClostestIndex = ClosestSlow - 1;      
      return ClosestSlow - 1; 
    }
  }
  
 
  
  while (upper-lower > 1) 
  {
    center = (upper+lower) >> 1;
    if (Time == hkinfo[center].Time) 
    {
      ClosestFast = (int)(center+1);
      break;
    }
    if (Time < hkinfo[center].Time) 
    {
      upper = center;
    } 
    else 
    {
      lower = center;
    }
  }
  
  if (ClosestFast == -1) 
  {
	ClosestFast = (int)(lower+1);
  }
  
  /*
  *    if (ClosestFast != ClosestSlow) {
  *      cerr<<"Closest fast "<<ClosestFast<<" vs. Closest slow "<<ClosestSlow<<endl;
  *      cout<<Time<<" vs fast: "<<hkinfo[ClosestFast-1].Time<<" - "<<hkinfo[ClosestFast].Time<<"  slow: "<<hkinfo[ClosestSlow-1].Time<<" - "<<hkinfo[ClosestSlow].Time<<endl;
  }
  */
  
  if (hkinfo[ClosestFast].Time - Time < Time - hkinfo[ClosestFast-1].Time) 
  {
    m_LastClostestIndex = ClosestFast;
    return ClosestFast; 
  } 
  else 
  {
    m_LastClostestIndex = ClosestFast - 1;
    return ClosestFast - 1; 
  }
}  

void PrintCallDBParam(GlobalCallDB_t *globalCallDB)
{
    headas_chat(NORMAL, "%s: Info: ************ CalDB Parameter **************\n",global.taskname); 
    headas_chat(NORMAL, "%s: Info: SuperStrictOffTimeInterval               = %d\n",global.taskname,global.saaparinfo.SN_SuperStrictOffTimeInterval); 
    headas_chat(NORMAL, "%s: Info: Prior                                    = %d\n",global.taskname,global.saaparinfo.SN_Prior);  
    headas_chat(NORMAL, "%s: Info: SmoothSHLDLO                             = %d\n",global.taskname,global.saaparinfo.SN_SmoothSHLDLO);  
    headas_chat(NORMAL, "%s: Info: SmoothSOURCE                             = %d\n",global.taskname,global.saaparinfo.SN_SmoothSOURCE);  
    headas_chat(NORMAL, "%s: Info: MinAllowedVolatility                     = %f\n",global.taskname,global.saaparinfo.SN_MinAllowedVolatility);  
    headas_chat(NORMAL, "%s: Info: MinMaximum                               = %f\n",global.taskname,global.saaparinfo.SN_MinMaximum);  
    headas_chat(NORMAL, "%s: Info: MaxMinimum                               = %f\n",global.taskname,global.saaparinfo.SN_MaxMinimum);  
    headas_chat(NORMAL, "%s: Info: MinVariationLeft                         = %f\n",global.taskname,global.saaparinfo.SN_MinVariationLeft);  
    headas_chat(NORMAL, "%s: Info: MinVariationRight                        = %f\n",global.taskname,global.saaparinfo.SN_MinVariationRight);  
    headas_chat(NORMAL, "%s: Info: NonSAAMinLongOpt                         = %f\n",global.taskname,global.saaparinfo.SN_NonSAAMinLongOpt);  
    headas_chat(NORMAL, "%s: Info: NonSAAMaxLongOpt                         = %f\n",global.taskname,global.saaparinfo.SN_NonSAAMaxLongOpt); 
    headas_chat(NORMAL, "%s: Info: SpectrumMin                              = %f\n",global.taskname,global.saaparinfo.SN_SpectrumMin);  
    headas_chat(NORMAL, "%s: Info: SpectrumMax                              = %f\n",global.taskname,global.saaparinfo.SN_SpectrumMax);  
    headas_chat(NORMAL, "%s: Info: TimeOneSigma                             = %f\n",global.taskname,global.saaparinfo.SN_TimeOneSigma);  
    headas_chat(NORMAL, "%s: Info: MaxSearchDistance                        = %d\n",global.taskname,global.saaparinfo.SN_MaxSearchDistance);  
    headas_chat(NORMAL, "%s: Info: HintOfVariabilityCutOff                  = %f\n",global.taskname,global.saaparinfo.SN_HintOfVariabilityCutOff);  
    headas_chat(NORMAL, "%s: Info: LogBinningBins                           = %d\n",global.taskname,global.saaparinfo.SN_LogBinningBins);  
    headas_chat(NORMAL, "%s: Info: PercentageEliminatedPixels               = %f\n",global.taskname,global.saaparinfo.SN_PercentageEliminatedPixels); 
    headas_chat(NORMAL, "%s: Info: SuspiciousnessThreshold                  = %d\n",global.taskname,global.saaparinfo.SN_SuspiciousnessThreshold);  
    
    headas_chat(NORMAL, "%s: Info: TentacleTimeInterval                     = %d\n",global.taskname,global.saaparinfo.TN_TentacleTimeInterval); 
    headas_chat(NORMAL, "%s: Info: TentacleNonSAAMinLong                    = %f\n",global.taskname,global.saaparinfo.TN_TentacleNonSAAMinLong);  
    headas_chat(NORMAL, "%s: Info: TentacleNonSAAMaxLong                    = %f\n",global.taskname,global.saaparinfo.TN_TentacleNonSAAMaxLong); 
    headas_chat(NORMAL, "%s: Info: TentacleLongitudeMin                     = %f\n",global.taskname,global.saaparinfo.TN_TentacleLongitudeMin);  
    headas_chat(NORMAL, "%s: Info: TentacleLongitudeMax                     = %f\n",global.taskname,global.saaparinfo.TN_TentacleLongitudeMax);  
    headas_chat(NORMAL, "%s: Info: TentacleMinLatitude                      = %f\n",global.taskname,global.saaparinfo.TN_TentacleMinLatitude);  
    headas_chat(NORMAL, "%s: Info: TentacleAvgLongitude                     = %f\n",global.taskname,global.saaparinfo.TN_TentacleAvgLongitude); 
    headas_chat(NORMAL, "%s: Info: TentacleVsRegionCrossingsBadCutoff       = %f\n",global.taskname,global.saaparinfo.TN_TentacleVsRegionCrossingsBadCutoff);  
    headas_chat(NORMAL, "%s: Info: TentacleVsRegionCrossingsReallyBadCutoff = %f\n",global.taskname,global.saaparinfo.TN_TentacleVsRegionCrossingsReallyBadCutoff);  
    headas_chat(NORMAL, "%s: Info: TentacleDurationCutOff                   = %f\n",global.taskname,global.saaparinfo.TN_TentacleDurationCutOff);  
    headas_chat(NORMAL, "%s: Info: TentacleAdjacentOrNotCutOff              = %f\n",global.taskname,global.saaparinfo.TN_TentacleAdjacentOrNotCutOff);  
    headas_chat(NORMAL, "%s: Info: TentacleLongDuration                     = %f\n",global.taskname,global.saaparinfo.TN_TentacleLongDuration);  
    headas_chat(NORMAL, "%s: Info: TentacleLongThreshold                    = %f\n",global.taskname,global.saaparinfo.TN_TentacleLongThreshold);  
    headas_chat(NORMAL, "%s: Info: TentacleFluxThreshold              = %f\n",global.taskname,global.saaparinfo.TN_TentacleFluxThreshold);  
    headas_chat(NORMAL, "%s: Info: TentacleSuspiciousnessThreshold          = %d\n",global.taskname,global.saaparinfo.TN_TentacleSuspiciousnessThreshold);  
    headas_chat(NORMAL, "%s: Info: *******************************************\n",global.taskname); 
} /* PrintCallDBParam */

int CopyCallDBParam(GlobalCallDB_t *globalCallDB)
{
    /* Into CalDB */
    globalCallDB->SuperStrictOffTimeInterval 			= global.saaparinfo.SN_SuperStrictOffTimeInterval;
    globalCallDB->Prior 					= global.saaparinfo.SN_Prior;
    globalCallDB->SmoothSHLDLO 					= global.saaparinfo.SN_SmoothSHLDLO;
    globalCallDB->SmoothSOURCE 					= global.saaparinfo.SN_SmoothSOURCE;
    globalCallDB->MinAllowedVolatility				= global.saaparinfo.SN_MinAllowedVolatility;
    globalCallDB->MinMaximum					= global.saaparinfo.SN_MinMaximum;
    globalCallDB->MaxMinimum					= global.saaparinfo.SN_MaxMinimum;
    globalCallDB->MinVariationLeft				= global.saaparinfo.SN_MinVariationLeft;
    globalCallDB->MinVariationRight				= global.saaparinfo.SN_MinVariationRight;
    globalCallDB->NonSAAMinLongOpt				= global.saaparinfo.SN_NonSAAMinLongOpt;
    globalCallDB->NonSAAMaxLongOpt				= global.saaparinfo.SN_NonSAAMaxLongOpt;
    globalCallDB->SpectrumMin 					= global.saaparinfo.SN_SpectrumMin;
    globalCallDB->SpectrumMax 					= global.saaparinfo.SN_SpectrumMax;
    globalCallDB->TimeOneSigma					= global.saaparinfo.SN_TimeOneSigma;
    globalCallDB->MaxSearchDistance				= global.saaparinfo.SN_MaxSearchDistance;
    globalCallDB->HintOfVariabilityCutOff			= global.saaparinfo.SN_HintOfVariabilityCutOff;
    globalCallDB->LogBinningBins				= global.saaparinfo.SN_LogBinningBins;
    globalCallDB->PercentageEliminatedPixels			= global.saaparinfo.SN_PercentageEliminatedPixels;
    globalCallDB->SuspiciousnessThreshold			= global.saaparinfo.SN_SuspiciousnessThreshold;
    
    globalCallDB->TentacleTimeInterval				= global.saaparinfo.TN_TentacleTimeInterval;
    globalCallDB->TentacleNonSAAMinLong				= global.saaparinfo.TN_TentacleNonSAAMinLong;
    globalCallDB->TentacleNonSAAMaxLong				= global.saaparinfo.TN_TentacleNonSAAMaxLong;
    globalCallDB->TentacleLongitudeMin				= global.saaparinfo.TN_TentacleLongitudeMin;
    globalCallDB->TentacleLongitudeMax				= global.saaparinfo.TN_TentacleLongitudeMax;
    globalCallDB->TentacleMinLatitude 				= global.saaparinfo.TN_TentacleMinLatitude;
    globalCallDB->TentacleAvgLongitude 				= global.saaparinfo.TN_TentacleAvgLongitude;
    globalCallDB->TentacleVsRegionCrossingsBadCutoff		= global.saaparinfo.TN_TentacleVsRegionCrossingsBadCutoff;
    globalCallDB->TentacleVsRegionCrossingsReallyBadCutoff	= global.saaparinfo.TN_TentacleVsRegionCrossingsReallyBadCutoff;
    globalCallDB->TentacleDurationCutOff				= global.saaparinfo.TN_TentacleDurationCutOff;
    globalCallDB->TentacleAdjacentOrNotCutOff			= global.saaparinfo.TN_TentacleAdjacentOrNotCutOff;
    globalCallDB->TentacleLongDuration				= global.saaparinfo.TN_TentacleLongDuration;
    globalCallDB->TentacleLongThreshold				= global.saaparinfo.TN_TentacleLongThreshold;
    globalCallDB->TentacleFluxThreshold				= global.saaparinfo.TN_TentacleFluxThreshold;
    globalCallDB->TentacleSuspiciousnessThreshold 		= global.saaparinfo.TN_TentacleSuspiciousnessThreshold;
    
    return OK;
} /* CopyCallDBParam */


double GetRMS(DTYPE *vett,int minVal,int maxVal)
{
    double media=0;
    double somma=0;
    double num_elem=0;
    double deviazione=0;
    double scarto=0;
    double scarto_quad=0;
    int i;
    
    media=GetMean(vett,minVal,maxVal);
    
    for(i=minVal;i<maxVal;i++)
    {
	if(vett[i]>0)
	{
	    num_elem++;
	    scarto=(vett[i]-media);
	    scarto_quad=scarto*scarto;
	    somma=somma+scarto_quad;
	}
    }
    
    deviazione=sqrt(somma/num_elem);
    
    return deviazione;
} /* GetRMS */

double GetMean(DTYPE *vett,int minVal,int maxVal)
{
    double somma=0;
    double num_elem=0;
    double media=0;
    int i;
    /*il  +1 è stato aggiunto per considerare anche l'ultimo elemento del vettor, inizialmente escluso. */
    
    for(i=minVal;i<maxVal;i++)
    {
	if(vett[i]>0)
	{
	    num_elem++;
	    somma=somma+vett[i];
	  
	    /*headas_chat(NORMAL, "vett[%d]=%d, num_elem=%f, somma=%f\n",i,vett[i],num_elem,somma);*/
	}
    }
    
    media=somma/num_elem;
    
    return media;
} /* GetMean */

double GetRMS_ROOT(BTYPE *vett,int nBins,int minVal,int maxVal)
{
    double stats[4],x,w,fattore;
    int i;
    int axm           = 2;
    double stddev2    = 0;
    double deviazione = 0;
   
    int numElem=(maxVal-minVal)+1;
    
    for(i=0;i<4;i++)
    {
	stats[i]=0;
    }
    
    for(i=0;i<numElem;i++)
    {
	fattore=( (((double)maxVal-(double)minVal)/(double)nBins))/2;
	x = i+( fattore );
	w = vett[i];
	
	if(w==0)
	{
	    continue;
	}
	
	stats[0] += w;
	stats[1] += 0;
	stats[2] += w*x;
	stats[3] += w*x*x;
    }

    x= stats[axm]/stats[0];
    stddev2 = fabs(stats[axm+1]/stats[0] - x*x);
    deviazione=sqrt(stddev2);
    
    return deviazione;
} /* GetRMS_ROOT */

double GetMean_ROOT(BTYPE *vett,int minVal,int maxVal)
{
    double somma    = 0;
    double prod     = 0;
    double num_elem = 0;
    double media    = 0;
    int i;
    int numElem;
    
    numElem=(maxVal-minVal)+1; 
    
    for(i=minVal;i<numElem;i++)
    {
	if(vett[i]>0)
	{
	    num_elem=num_elem+vett[i];
	    prod=i*vett[i];
	    somma=somma+prod;
	}
    }
    
    media=somma/num_elem;
    
    return media;
} /* GetMean_ROOT */

void TestMostlyEliminateSource(EVTRow_t *evtinfo, int evtnrows,struct gti_struct *gti,int *ExcludedDetRawXY, int ExcludedDetRawXYrows)
{
    int cont1=0,i=0;
    char buffer[200];
    int PosX,PosY;
    int DetectorID,RawX,RawY,ID,lower_bound;    
    FILE *fp,*fp1;
    
    fp = fopen( "discarded_events.txt" , "w" );
    fp1 = fopen( "events_not_discarded.txt" , "w" );
    /*headas_chat(NORMAL,"************************************************* Coordinates list to exclude *************************************************.\n", global.taskname);*/
    
    for (i = 0; i < evtnrows; ++i) 
	{
	
	    if (WithinSpecialGTI(evtinfo[i].Time) == 0) 
	    {
		continue;
	    }
	    
	    if (evtinfo[i].energy < global.saaparinfo.SN_SpectrumMin || evtinfo[i].energy > global.saaparinfo.SN_SpectrumMax) 
	    {
		continue;    
	    }

	    if (IsGTI(gti, evtinfo[i].Time) == 0) 
	    {		
		continue;
	    }
	    
	    DetectorID = evtinfo[i].DetectorID;
	    RawX = evtinfo[i].RawX;
	    RawY = evtinfo[i].RawY;
	    ID = 10000*DetectorID + 100*RawX + RawY;
	    
	    lower_bound=MyLowerBound (ExcludedDetRawXY, ExcludedDetRawXYrows, ID);  
	    
	    ConvertRawPos(RawX, RawY, DetectorID, &PosX, &PosY);
	    
	    if(lower_bound!=LOWER_BOUND_NOT_FOUND && lower_bound==ID)
	    {
		cont1++;
		sprintf(buffer,"%5d %5d %5d %5d %5d %5d %5d %5d %5d",evtinfo[i].Det1X,evtinfo[i].Det1Y,evtinfo[i].X,evtinfo[i].Y,PosX,PosY,evtinfo[i].RawX,evtinfo[i].RawY,evtinfo[i].DetectorID);
		fprintf(fp,"%5d %5d %5d %5d %5d %5d %5d %5d %5d\n",evtinfo[i].Det1X,evtinfo[i].Det1Y,evtinfo[i].X,evtinfo[i].Y,PosX,PosY,evtinfo[i].RawX,evtinfo[i].RawY,evtinfo[i].DetectorID);
		/*headas_chat(NORMAL,"%d) %s \n",cont1,buffer);*/
		continue;
	    }	    
	    fprintf(fp1,"%5d %5d %5d %5d %5d %5d %5d %5d %5d\n",evtinfo[i].Det1X,evtinfo[i].Det1Y,evtinfo[i].X,evtinfo[i].Y,PosX,PosY,evtinfo[i].RawX,evtinfo[i].RawY,evtinfo[i].DetectorID);	        
	}
	/*headas_chat(NORMAL,"**********************************************************************************************************************************.\n", global.taskname);*/
	
	/*headas_chat(NORMAL,"+++++++++++++++++++++++++++++++++ List for energy values [= 0.04*(double)(PI)+1.6 ] ++++++++++++++++++++++++++++++++++++++++.\n", global.taskname);*/
	for (i = 0; i < evtnrows; ++i) 
	{
	
	    if (evtinfo[i].energy < global.saaparinfo.SN_SpectrumMin || evtinfo[i].energy > global.saaparinfo.SN_SpectrumMax) 
	    {
		cont1++;
		/*headas_chat(NORMAL,"%d) %f %f %f \n",cont1,evtinfo[i].energy,global.saaparinfo.SN_SpectrumMin,global.saaparinfo.SN_SpectrumMax );*/
		continue;    
	    }

	    if (IsGTI(gti, evtinfo[i].Time) == 0) 
	    {
		continue;
	    }
	       
	}
	/*headas_chat(NORMAL,"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.\n", global.taskname);*/

 	fclose(fp);
 	fclose(fp1);
	
} /* TestMostlyEliminateSource */


void writeSAA(char *nameFile,HKRow_t *hkinfo,BTYPE *SoftSAA,int SoftSAA_size)
{
    int b;
    FILE *fp=NULL;

    fp = fopen( nameFile , "w" );

    for (b = 0; b < SoftSAA_size; ++b)
    {
        fprintf(fp,"Time[%d] %d   SoftSAA[%d] %d\n", b,(int)hkinfo[b].Time,b,SoftSAA[b]);
    }

    if(fp!=NULL)
    {
        fclose(fp);
    }

} /* writeSAA */

void writeVetD(char *nome_vett,DTYPE *vett,int vett_size)
{
    FILE *fp;
    int i;
    char nome_file[200];
    
    sprintf(nome_file,"%s.txt",nome_vett);
    
    fp = fopen( nome_file , "w" );
    
    for(i=0;i<vett_size;i++)
    {
	fprintf(fp,"%s %d %f\n",nome_vett,i,vett[i]);
    }
    
    if(fp!=NULL)
    {
        fclose(fp);
    }
} /* writeVetD */

void writeVetI(char *nome_vett,int *vett,int vett_size)
{
    FILE *fp;
    int i;
    char nome_file[200];
    
    sprintf(nome_file,"%s.txt",nome_vett);
    
    fp = fopen( nome_file , "w" );
    
    for(i=0;i<vett_size;i++)
    {
	fprintf(fp,"%s %d %d\n",nome_vett,i,vett[i]);
    }
    
    if(fp!=NULL)
    {
        fclose(fp);
    }
} /* writeVetI */

void writeVetB(char *nome_vett,BTYPE *vett,int vett_size)
{
    FILE *fp;
    int i;
    char nome_file[200];
    
    sprintf(nome_file,"%s.txt",nome_vett);
    
    fp = fopen( nome_file , "w" );
    
    for(i=0;i<vett_size;i++)
    {
	fprintf(fp,"%s %d %f\n",nome_vett,i,(double)vett[i]);
    }
    
    if(fp!=NULL)
    {
        fclose(fp);
    }
} /* writeVetB */

void writeVetJ(char *nome_vett,JTYPE *vett,int vett_size)
{
    FILE *fp;
    int i;
    char nome_file[200];
    
    sprintf(nome_file,"%s.txt",nome_vett);
    
    fp = fopen( nome_file , "w" );
    
    for(i=0;i<vett_size;i++)
    {
	fprintf(fp,"%s %d %f\n",nome_vett,i,(double)vett[i]);
    }
    
    if(fp!=NULL)
    {
        fclose(fp);
    }
} /* writeVetJ */

int FindSAAs_new(HKRow_t *hkinfo, int hknrows,EVTRow_t *evtinfo, int evtnrows, double *FilteredRate, OrbitRow_t *orbitinfo, int orbitnrows, struct gti_struct *gti, BTYPE *SoftSAA)
{
    /* For Debugging */
#ifdef WRITE_DEBUG
    FILE *fp					= NULL;
    FILE *fp1					= NULL;
#endif
    int h					= 0;		
    int i					= 0;
    int OIndex					= 0;
    int index_bin				= 0;
    int lt_index				= 0;
    int s					= 0;
    int Index					=-1;
    double NonSAARateMax			= 0;
    int ExposureTimeInternalSAA			= 0;
    int ExposureTimeStrictLSR			= 0;
    int ContentBins				= 0;
    int Bin;
    int ExcludedDetRawXYrows;
    int NAverageFluxes;
    int Suspiciousness;
    int OneSecBins, SuperStrictBins;
    int PeakLowEdge, PeakHighEdge;
    int MinimumBin, MaximumBin, b, bb, bx;
    int DetectorID,RawX,RawY,ID,lower_bound;       
    double NonSAARateMean;
    double NonSAARateRMS;
    double NonSAARateSigma;
    double Threshold;
    
    double val;
    double Average;
    double numerator;
    double denominator;
    double MaxFlux;
    double AverageFlux;
    double Content 	= 0.0;
    double MinTime, MaxTime;
    double MinTimeEvt, MaxTimeEvt;
    double Minimum, Maximum, Volatility;
    double binSize_1, binSize_2;
    unsigned int ShieldRateLowBB_size;
    Baesian_t baesian;
    double MaxAllowedRMS;
    
    /* Define rejection flags*/
    BTYPE RejectionOn, RejectionOff;
    
   
    int   *ExcludedDetRawXY		= NULL;

    DTYPE *NonSAARate			= NULL;
    DTYPE *NonSAARateHistogram		= NULL;
    
    BTYPE *SoftSAAOptimizedLSRRMS	= NULL;
    BTYPE *RawRate			= NULL;
    BTYPE *OnOffOptimizedRMSLeftToRight	= NULL;
    BTYPE *OnOffOptimizedRMSRightToLeft	= NULL;
    BTYPE *OnOffOptimizedRMS		= NULL;
    BTYPE *OnOffInternalSAA		= NULL;
    BTYPE *OnOffStrictLSR		= NULL;
    
    DTYPE *GradientHistogram		= NULL; 
    DTYPE *EvaluationRate		= NULL;
    DTYPE *EvaluationRateNOLT		= NULL;
    DTYPE *LiveTime			= NULL;
    DTYPE *ShieldRateLowBB		= NULL;
    DTYPE *ShieldRateLow		= NULL;
    baesian.m_BinEdges			= NULL;
    baesian.m_BinnedData		= NULL;
    
    

    
    if( !strcasecmp(global.par.saamode,"NONE") )
    {
	return OK;
    }
    
    if (evtnrows == 0) 
    {
	headas_chat(NORMAL, "%s: Info: No filtered events = no SAA cut\n",global.taskname);
	return OK;
    }
    
    MaxAllowedRMS = global.par.MaxAllowedRMS;
    
    /* Number eliminated source */
    ExcludedDetRawXYrows = 0;
    
    /* Define rejection flags */
    RejectionOn  = 0;
    RejectionOff = 1;
    
    /* One second binning */
    OneSecBins  = GetMaxTime(hkinfo,hknrows) - GetMinTime(hkinfo,hknrows) + 1;
    MinTime     = GetMinTime(hkinfo,hknrows);
    MaxTime     = GetMaxTime(hkinfo,hknrows);
    MinTimeEvt  = GetMinTimeEvt(evtinfo, evtnrows); 
    MaxTimeEvt  = GetMaxTimeEvt(evtinfo, evtnrows); 
    
    /* 60-second binning -- good compromise for having enough statistics per bin, and small enough bins for not cutting out too much  */
    SuperStrictBins = OneSecBins/global.saaparinfo.SN_SuperStrictOffTimeInterval;
    
    headas_chat(NORMAL, "%s: ********** Find SAANew Strict **************\n",global.taskname);
    headas_chat(NORMAL, "%s: Info: OneSecBins              = %d\n",global.taskname,OneSecBins);
    headas_chat(NORMAL, "%s: Info: Evt MinTime             = %f\n",global.taskname,MinTimeEvt);
    headas_chat(NORMAL, "%s: Info: Evt MaxTime             = %f\n",global.taskname,MaxTimeEvt);
    headas_chat(NORMAL, "%s: Info: hk MinTime              = %f\n",global.taskname,MinTime);
    headas_chat(NORMAL, "%s: Info: hk MaxTime              = %f\n",global.taskname,MaxTime);
    headas_chat(NORMAL, "%s: Info: hkMaxTime - hkMinTime   = %f\n",global.taskname,MaxTime-MinTime);
    headas_chat(NORMAL, "%s: Info: SuperStrictBins         = %d\n",global.taskname,SuperStrictBins);
    
    
    /* MALLOC and CALLOC */
    
    SoftSAAOptimizedLSRRMS = (BTYPE*)calloc(hknrows, sizeof(BTYPE));  /* array initialized to zero */
    if(SoftSAAOptimizedLSRRMS==NULL) 
    {
	headas_chat(CHATTY,"%s: Error: SoftSAAOptimizedLSRRMS memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    OnOffInternalSAA = (BTYPE*)malloc(hknrows*sizeof(BTYPE));
    if(OnOffInternalSAA==NULL) 
    {
        headas_chat(CHATTY,"%s: Error: FindSAAs_new: OnOffInternalSAA memory allocation failure.\n", global.taskname);
        goto FindSAAs_new_end;
    }
    
    OnOffStrictLSR = (BTYPE*)malloc(hknrows*sizeof(BTYPE));
    if(OnOffStrictLSR==NULL) 
    {
        headas_chat(CHATTY,"%s: Error: FindSAAs_new: OnOffStrictLSR memory allocation failure.\n", global.taskname);
        goto FindSAAs_new_end;
    }
    
    ShieldRateLow = (DTYPE*)calloc(SuperStrictBins,sizeof(DTYPE));
    if(ShieldRateLow==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: ShieldRateLow memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
        
    LiveTime = (DTYPE*)malloc(hknrows*sizeof(DTYPE));
    if(LiveTime==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: LiveTime memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    RawRate = (BTYPE*)calloc(evtnrows,sizeof(BTYPE));
    if(RawRate==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: RawRate memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    OnOffOptimizedRMSLeftToRight = (BTYPE*)calloc(SuperStrictBins,sizeof(BTYPE));
    if(OnOffOptimizedRMSLeftToRight==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: OnOffOptimizedRMSLeftToRight memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    OnOffOptimizedRMSRightToLeft = (BTYPE*)calloc(SuperStrictBins,sizeof(BTYPE));
    if(OnOffOptimizedRMSRightToLeft==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: OnOffOptimizedRMSRightToLeft memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    OnOffOptimizedRMS = (BTYPE*)calloc(SuperStrictBins,sizeof(BTYPE));
    if(OnOffOptimizedRMS==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: OnOffOptimizedRMS memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    NonSAARate = (DTYPE*)calloc(SuperStrictBins,sizeof(DTYPE));
    if(NonSAARate==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: NonSAARate memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    EvaluationRate = (DTYPE*)calloc(SuperStrictBins,sizeof(DTYPE));
    if(EvaluationRate==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: EvaluationRate memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    EvaluationRateNOLT = (DTYPE*)calloc(SuperStrictBins,sizeof(DTYPE));
    if(EvaluationRateNOLT==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: EvaluationRateNOLT memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
       
    /* END MALLOC and CALLOC */
    
    for (i = 0; i < hknrows; i++) 
    {
	LiveTime[i] = hkinfo[i].LiveTime ;
    }
    
#ifdef WRITE_DEBUG     
    writeVetD("LiveTime",LiveTime,hknrows);
#endif
    
    if (global.par.ElimiateSource == TRUE) 
    {
	/* Create a list of pixels with high source count for elimination */
	MostlyEliminateSource(evtinfo,evtnrows,orbitinfo,orbitnrows,gti,&ExcludedDetRawXY,&ExcludedDetRawXYrows,FALSE);

#ifdef WRITE_DEBUG      
	writeVetI("ExcludedDetRawXY",ExcludedDetRawXY,ExcludedDetRawXYrows);
	/* Test eliminated source*/
	TestMostlyEliminateSource(evtinfo,evtnrows,gti,ExcludedDetRawXY,ExcludedDetRawXYrows);
#endif
    }
    
    /* (B) Fill the histograms */
    if (global.par.ElimiateSource == TRUE) 
    {
	for (i = 0; i < evtnrows; ++i) 
	{
	
	    if (WithinSpecialGTI(evtinfo[i].Time) == 0) 
	    {
		continue;
	    }
	    
	    if (evtinfo[i].energy < global.saaparinfo.SN_SpectrumMin || evtinfo[i].energy > global.saaparinfo.SN_SpectrumMax) 
	    {
		continue;    
	    }

	    if (IsGTI(gti, evtinfo[i].Time) == 0) 
	    {		
		continue;
	    }
	    
	    DetectorID = evtinfo[i].DetectorID;
	    RawX = evtinfo[i].RawX;
	    RawY = evtinfo[i].RawY;
	    ID = 10000*DetectorID + 100*RawX + RawY;
	    
	    lower_bound=MyLowerBound (ExcludedDetRawXY, ExcludedDetRawXYrows, ID);  
	    
	    if(lower_bound!=LOWER_BOUND_NOT_FOUND && lower_bound==ID)
	    {		
		continue;
	    }
	    
	    RawRate[i]++;
	        
	}
	
	if( MyIntegral(RawRate,evtnrows)==0 ) 
	{
	    headas_chat(NORMAL, "%s: Error: Something went wrong! No data!\n",global.taskname);
	    goto FindSAAs_new_end;
	}

    }
    
    /* (B) Fill the histograms */
    for (i = 0; i < hknrows; i++) 
    {
	if (WithinSpecialGTI(hkinfo[i].Time) == 0) 
	{
	    continue;
	}
	
	FindClosestOrbitIndex(orbitinfo, orbitnrows, hkinfo[i].Time, &OIndex);

	
	index_bin=GetIndex(MinTime - 0.5, MaxTime + 0.5, SuperStrictBins, hkinfo[i].Time);

	if ( orbitinfo[OIndex].SAAFlag == 0 )
	{
	    ShieldRateLow[index_bin] = ShieldRateLow[index_bin]+hkinfo[i].ShieldRateLow;
	    OnOffInternalSAA[i]=RejectionOff;
	    ++ExposureTimeInternalSAA;
	} 
	else 
	{
	    OnOffInternalSAA[i]=RejectionOn; 
	}
    }
    MySmoothArray(SuperStrictBins,ShieldRateLow,global.saaparinfo.SN_SmoothSHLDLO);

#ifdef WRITE_DEBUG
    writeVetB("OnOffInternalSAA",OnOffInternalSAA,hknrows);
    writeVetD("ShieldRateLowSmoothed",ShieldRateLow,SuperStrictBins);
#endif
    /* (C) Create a Bayesian Block binned histogram of the shield data     
       Step 1: Create a histogram with Bayesian block binning end get normalize ShieldRateLowBB*/
    MyBayesianBlocks(0, MaxTime - MinTime + 1,
                     global.saaparinfo.SN_SuperStrictOffTimeInterval,
                     global.saaparinfo.SN_Prior,
                     ShieldRateLow,
                     SuperStrictBins,&baesian); 

    /* (D) Calculate a simple (slightly) shifted, *normalized* gradient histogram */
    GetNormalized(&baesian,&ShieldRateLowBB,&ShieldRateLowBB_size);
    
#ifdef WRITE_DEBUG
    writeVetB("OnOffInternalSAA",OnOffInternalSAA,hknrows);
    writeVetD("ShieldRateLowBB",ShieldRateLowBB,ShieldRateLowBB_size);
#endif
    
    GradientHistogram = (DTYPE*)calloc(ShieldRateLowBB_size,sizeof(DTYPE));
    if(GradientHistogram==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: GradientHistogram memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
        
    for (b = 1; b < ShieldRateLowBB_size; ++b) 
    {
	Average = 0.5 * (ShieldRateLowBB[b] + ShieldRateLowBB[b-1]);
	numerator=ShieldRateLowBB[b]-ShieldRateLowBB[b-1];
	
	binSize_1=baesian.m_BinEdges[b]-baesian.m_BinEdges[b-1];
	binSize_2=baesian.m_BinEdges[b+1]-baesian.m_BinEdges[b];
	denominator=(binSize_2/2) + (binSize_1/2);
	
	val=numerator/denominator/Average;
	GradientHistogram[b]=val;
    }
    
    /* (E) Find zero passages + --> - and regions around zero passages for SAA exclusion
         i.e. find the peaks */
    
    for (i = 0; i < hknrows; i++) 
    {
            OnOffStrictLSR[i]=OnOffInternalSAA[i];
    }
    
    for (b = 0; b < ShieldRateLowBB_size - 1; ++b) 
    {
        if (GradientHistogram[b] > 0 && GradientHistogram[b+1] < 0) 
        {
            /* Find edges: */
            MaximumBin = b;
	    for (bb = MaximumBin-1; bb >= 0; --bb) 
            {
                if (GradientHistogram[bb] > GradientHistogram[MaximumBin]) 
                {
                    MaximumBin = bb;
                } 
                else 
		{
                    break;
                }
            }
                        
            MinimumBin = b+1;
            for (bb = MinimumBin+1; bb < ShieldRateLowBB_size; ++bb) 
	    {
                if (GradientHistogram[bb] < GradientHistogram[MinimumBin]) 
		{
                    MinimumBin = bb;
                } 
                else 
		{
                    break;
                }
            }

            Minimum = GradientHistogram[MinimumBin];
            Maximum = GradientHistogram[MaximumBin];
            Volatility = Maximum - Minimum;
	    	    
	    if (!(Volatility > global.saaparinfo.SN_MinAllowedVolatility &&
                    ((Maximum > + global.saaparinfo.SN_MinMaximum && Minimum < -global.saaparinfo.SN_MaxMinimum) ||
		    (Minimum < - global.saaparinfo.SN_MinMaximum && Maximum > +global.saaparinfo.SN_MaxMinimum)))) 
	    {
                continue;
            }
            
        while (MaximumBin > 0 && fabs(GradientHistogram[MaximumBin-1]) > global.saaparinfo.SN_MinVariationLeft) /* Protection added */
            {
                --MaximumBin;
            }
        while (MinimumBin < ShieldRateLowBB_size - 1 && fabs(GradientHistogram[MinimumBin+1]) > global.saaparinfo.SN_MinVariationRight) /* Protection added */
            { 
                /* Relaxed for delayed decay */
                ++MinimumBin;
            }
                       
        
        PeakLowEdge = GetPeakLowEdge(&baesian,MaximumBin);
        PeakHighEdge = GetPeakHighEdge(&baesian,MinimumBin);
            
            /* Add to the on-off-histo: */
        
	for (bx = PeakLowEdge; bx <= PeakHighEdge; ++bx) 
	{
	    if (bx >= 0 && bx < hknrows) { 
		OnOffStrictLSR[bx] = RejectionOn;
	    }
	}
      }
    }

    /* (F) Add the internal/hardware SAA rejection regions */
    for (b = 0; b < hknrows; ++b) 
    {
	if (OnOffInternalSAA[b] == RejectionOn) 
	{
	    OnOffStrictLSR[b]=RejectionOn;
	}
    }
    
  
    /* (G) Store the preliminary cut */
    ExposureTimeStrictLSR = 0;
    
    for (b = 0; b < hknrows; ++b) 
    {	
	Index = FindClosestIndex(Index,b+MinTime,hkinfo,hknrows);
	if( abs(b-Index) <= 1)
	{
	
	    if (OnOffStrictLSR[b] == RejectionOn) 
	    {
		SoftSAA[b] = 1;
	    } 
	    else 
	    {
		SoftSAA[b] = 0;
		++ExposureTimeStrictLSR;
	    }	
	}
	else
	{
	    headas_chat(NORMAL,"%s: Error: Something is wrong with the times as I cannot find housekeeping data... \n", global.taskname,b+MinTime);
	}
    }

#ifdef WRITE_DEBUG    
    writeVetB("OnOffStrictLSRFinal",OnOffStrictLSR,hknrows);
    writeSAA("SoftSAA_STRICT.txt",hkinfo,SoftSAA,hknrows);
#endif    

    headas_chat(NORMAL,"%s: Info: ExposureTimeStrictLSR   = %d\n", global.taskname,ExposureTimeStrictLSR);
    headas_chat(NORMAL,"%s: Info: ExposureTimeInternalSAA = %d\n", global.taskname,ExposureTimeInternalSAA);
    headas_chat(NORMAL,"%s: Info: Exposure time strict    = %f %\n", global.taskname,100 * (double)(ExposureTimeStrictLSR/ExposureTimeInternalSAA));
    
    if( !strcasecmp(global.par.saamode,"STRICT") ) 
    {
	goto FindSAAs_new_end_ok;
    }
    
    /* Step 2: Check for rate increases and only cut those regions
     (A) Create and fill all required histograms */
#ifdef WRITE_DEBUG     
    fp = fopen( "input_EvaluationRate.txt" , "w" );
    fp1 = fopen( "input_EvaluationRate_LiveTime.txt" , "w" );
    fprintf(fp,"mintime=%f\n",MinTime-0.5);
    fprintf(fp,"maxtime=%f\n",MaxTime+0.5);
    fprintf(fp,"SpectrumMin=%f\n", (double)global.saaparinfo.SN_SpectrumMin);
    fprintf(fp,"SpectrumMax=%f\n",(double)global.saaparinfo.SN_SpectrumMax);
    fprintf(fp1,"mintime=%f\n",MinTime-0.5);
    fprintf(fp1,"maxtime=%f\n",MaxTime+0.5);
    fprintf(fp1,"SpectrumMin=%f\n", (double)global.saaparinfo.SN_SpectrumMin);
    fprintf(fp1,"SpectrumMax=%f\n",(double)global.saaparinfo.SN_SpectrumMax);
#endif
    
    for (i = 0; i < evtnrows; ++i) 
    {
      
        if (WithinSpecialGTI(evtinfo[i].Time) == 0) 
        {
            continue;
        }
        
        if (evtinfo[i].energy < global.saaparinfo.SN_SpectrumMin || evtinfo[i].energy > global.saaparinfo.SN_SpectrumMax) 
        {
            continue;    
        }

        if (IsGTI(gti, evtinfo[i].Time) == 0) 
        {
            continue;
        }
        
        if (global.par.ElimiateSource == TRUE) 
        {
                DetectorID = evtinfo[i].DetectorID;
		RawX = evtinfo[i].RawX;
		RawY = evtinfo[i].RawY;
		ID = 10000*DetectorID + 100*RawX + RawY;
		
		lower_bound=MyLowerBound (ExcludedDetRawXY, ExcludedDetRawXYrows, ID);  
                
                if(lower_bound!=LOWER_BOUND_NOT_FOUND && lower_bound==ID)
                {
                    continue;
                }
        }
	    
  
	index_bin=GetIndex(MinTime-0.5, MaxTime+0.5, SuperStrictBins, evtinfo[i].Time);
	FindClosestOrbitIndex(orbitinfo, orbitnrows, evtinfo[i].Time, &OIndex);
	FindHkIndex(hkinfo, hknrows, evtinfo[i].Time, &lt_index);
	
	/*printf("evtinfo[%d].Time=%f, lt_index=%d,  hkinfo[%d]=%f  ,hkinfo[%d]=%f,  hkinfo[%d]=%f\n",
		i,
		evtinfo[i].Time,
		lt_index,
		lt_index-1,hkinfo[lt_index-1].Time,
		lt_index,hkinfo[lt_index].Time,
		lt_index+1,hkinfo[lt_index+1].Time);*/
	
	if (orbitinfo[OIndex].slew == 0 && orbitinfo[OIndex].occulted==0 && orbitinfo[OIndex].SAA_A == 0 && orbitinfo[OIndex].SAA_B == 0) 
	{
		EvaluationRateNOLT[index_bin]++;
		
		if(LiveTime[lt_index]>0)
		{
		    EvaluationRate[index_bin]=EvaluationRate[index_bin]+(1/LiveTime[lt_index]);
		}
		else
		{
      EvaluationRate[index_bin]++;
    }
#ifdef WRITE_DEBUG              
      fprintf(fp,"ID %d Time %f orbitindex %d bin %d  EvaluationRate[%d] %f\n",i, evtinfo[i].Time,OIndex,index_bin,index_bin,EvaluationRateNOLT[index_bin]);
                fprintf(fp1,"ID %d Time %f  bin %d  EvaluationRate[%d] %f\n",i, evtinfo[i].Time,index_bin,index_bin,EvaluationRate[index_bin]);
  } else {
    fprintf(fp,"ID %d --> Orbit reject: %d\n", i, OIndex);
    fprintf(fp1,"ID %d --> Orbit reject: %d\n", i, OIndex);
#endif
  }
        
        if (orbitinfo[OIndex].Longitude >= global.saaparinfo.SN_NonSAAMinLongOpt && orbitinfo[OIndex].Longitude <= global.saaparinfo.SN_NonSAAMaxLongOpt) 
	{
	    if (orbitinfo[OIndex].slew == 0 && orbitinfo[OIndex].occulted==0 && orbitinfo[OIndex].SAA_A == 0 && orbitinfo[OIndex].SAA_B == 0) 
	    {
		if(LiveTime[lt_index]>0)
		{
			NonSAARate[index_bin]=NonSAARate[index_bin]+(1/LiveTime[lt_index]);
		}
		else
		{
			NonSAARate[index_bin]++;
		}
	    }
	}
    }

#ifdef WRITE_DEBUG     
    fclose(fp);
    fclose(fp1);
#endif    
    /* (B) Determine the mean and rms detector count rate in the regions away from the SAA as well as the threshold
       (a) Create a rate histogram */
    NonSAARateMax = GetMax(NonSAARate,SuperStrictBins);
    
    NonSAARateHistogram = (DTYPE*)calloc(SuperStrictBins,sizeof(DTYPE)); /* il vettore va da 0 a NonSAARateMax con 1000 bins */
    if(NonSAARateHistogram==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: NonSAARateHistogram memory allocation failure.\n", global.taskname);
	goto FindSAAs_new_end;
    }
    
    for (b = 1; b < SuperStrictBins-1; ++b)
    {
	if (NonSAARate[b] > 0 && NonSAARate[b-1] > 0 && NonSAARate[b+1] > 0)
	{   
	    NonSAARateHistogram[b]=NonSAARate[b];
	}
    }

#ifdef WRITE_DEBUG    
    writeVetD("EvaluationRateNOLT",EvaluationRateNOLT,SuperStrictBins);
    writeVetD("EvaluationRate",EvaluationRate,SuperStrictBins);
    writeVetD("NonSAARate",NonSAARate,SuperStrictBins);
    writeVetD("NonSAARateHistogram",NonSAARateHistogram,SuperStrictBins);
#endif    
    /* (b) Calculate Media and RMS */
    NonSAARateMean	= GetMean(NonSAARateHistogram,0,SuperStrictBins);
    NonSAARateRMS	= GetRMS(NonSAARateHistogram,0,SuperStrictBins);
    NonSAARateSigma	= sqrt(NonSAARateMean);
    Threshold		= MaxAllowedRMS*NonSAARateRMS + NonSAARateMean;
    
       
    if (NonSAARateRMS > global.saaparinfo.SN_TimeOneSigma * NonSAARateSigma) 
    {
	Threshold = NonSAARateMax + MaxAllowedRMS*NonSAARateRMS;	
	headas_chat(NORMAL,"%s: Info: Bursts detected! Threshold is max + %f *rms.\n", global.taskname,MaxAllowedRMS);
    }
    
    /* When we have no data in the NonSAA region our threshold will be zero,
       Set it ti the maximum */
    if (Threshold == 0) 
    {
	headas_chat(NORMAL,"%s: Info: Threshold would be zero since we have no data, setting it to max.\n", global.taskname);
	Threshold = FLT_MAX / 100;
    }

    headas_chat(NORMAL,"%s: ********** Find SAANew Optimized **************\n",global.taskname);
    headas_chat(NORMAL,"%s: Info: Threshold calculation diagnostics based on non-SAA regions:\n", global.taskname);
    headas_chat(NORMAL,"%s: Info: Average rate in a %d-sec interval: %f \n", global.taskname,global.saaparinfo.SN_SuperStrictOffTimeInterval,NonSAARateMean);
    headas_chat(NORMAL,"%s: Info: Maximum rate: %f\n", global.taskname,NonSAARateMax);
    headas_chat(NORMAL,"%s: Info: RMS: %f  (vs. 1-sigma: %f)\n", global.taskname,NonSAARateRMS,sqrt(NonSAARateMean));
    headas_chat(NORMAL,"%s: Info: --> Threshold rate: %f\n", global.taskname,Threshold);

    headas_chat(NORMAL,"%s: Info: NonSAARateMax           = %f\n", global.taskname,NonSAARateMax); 
    headas_chat(NORMAL,"%s: Info: NonSAARateMean          = %f\n", global.taskname,NonSAARateMean);   
    headas_chat(NORMAL,"%s: Info: NonSAARateRMS           = %f\n", global.taskname,NonSAARateRMS);
    headas_chat(NORMAL,"%s: Info: NonSAARateSigma         = %f\n", global.taskname,NonSAARateSigma);
    headas_chat(NORMAL,"%s: Info: MaxAllowedRMS           = %f\n", global.taskname,MaxAllowedRMS);
    headas_chat(NORMAL,"%s: Info: Threshold               = %f\n", global.taskname,Threshold);
    
    
    /* (C) Determine the regions to reject
    Approach:
    Assuming we have a threshold of 5 RMS
    Then we look for single bins with an increase of 5 RMS, two-bins with an increase of 5/sqrt(2), 
    three bins with an increase of 5/sqrt(3), etc. until we reach our max search distance MaxSearchDistance
    Since this approach has a tail (e.g. we have one gigantic increase and the two following bins normal, 
    then a search distance of 3 would catch all three although we do not have an increase in the latter 2),
    We do it once from the left and once from the right and then "AND" those two together --> no more tails
    Finally "AND" / mask it with the previously dtermined regions of low-shield rate increases
    */
    
    /* The maximum amount of adjacent bins we serach for a significant rate increase
       (a) left to right*/
    for (b = 0; b < SuperStrictBins; ++b) 
    {
	OnOffOptimizedRMSLeftToRight[b] = RejectionOff;
    }
	    
    /* First seach for one-bin X-rms increases, then two-bin X/sqrt(2) increases, etc. */
    for (s = 1; s <= global.saaparinfo.SN_MaxSearchDistance; ++s) 
    {
	for (b = 0; b < SuperStrictBins; ++b) 
	{
	    ContentBins = 0;
	    Content = 0.0;

	    for (bb = b; bb < b+s && b < SuperStrictBins; ++bb) 
	    {
		if (EvaluationRate[bb] > NonSAARateMean) 
		{ 
		    /* stop immediately if we are below mean */
		    Content += EvaluationRate[bb];
		    ContentBins++;
		} 
		else 
		{
		    break; 
		}
	    }
	    if (ContentBins > 0 && NonSAARateMean > 0) 
	    { 
		/* NonSAARateMean > 0 means we do have threshold values, i.e. data i the non-SAA region */
		if ( Content/ContentBins > NonSAARateRMS * MaxAllowedRMS/sqrt(ContentBins) + NonSAARateMean) 
		{
		    /* We are above our threshold -> reject */
		    for (bb = b; bb < b+ContentBins; ++bb) 
		    {
			OnOffOptimizedRMSLeftToRight[bb] = RejectionOn; 
		    }
		}
	    }
	}
    }

    /* (b) Search right to left */
    for (b = 0; b < SuperStrictBins; ++b) 
    {
	OnOffOptimizedRMSRightToLeft[b] = RejectionOff;
    }
    
    for (s = 1; s <= global.saaparinfo.SN_MaxSearchDistance; ++s) 
    {
	for (b = SuperStrictBins-1; b >= 0; --b) 
	{
	    ContentBins = 0;
	    Content = 0.0;
	    
	    for (bb = b; bb > b-s && b >= 0; --bb) 
	    {
		if (EvaluationRate[bb] > NonSAARateMean) 
		{
		    Content += EvaluationRate[bb];
		    ContentBins++;
		} 
		else 
		{
		    break; 
		}
	    }
	    
	    if (ContentBins > 0 && NonSAARateMean > 0) 
	    { 
	        /* NonSAARateMean > 0 means we do have threshold values, i.e. data i the non-SAA region */
		if ( Content/ContentBins > NonSAARateRMS * MaxAllowedRMS/sqrt(ContentBins) + NonSAARateMean) 
		{
		    for (bb = b; bb > b-ContentBins; --bb) 
		    {
			OnOffOptimizedRMSRightToLeft[bb] = RejectionOn; 
		    }
		}
	    }
	}   
    }

    /* (c) "AND" them together: Only when both are ON set the combined to ON */
    for (b = 0; b < SuperStrictBins; ++b) 
    {
	if (OnOffOptimizedRMSRightToLeft[b] == RejectionOn && OnOffOptimizedRMSLeftToRight[b] == RejectionOn) 
	{
	    OnOffOptimizedRMS[b] = RejectionOn;
	} 
	else 
	{
	    OnOffOptimizedRMS[b] = RejectionOff;
	}
    }
    
#ifdef WRITE_DEBUG
    writeVetB("OnOffOptimizedRMSRightToLeft",OnOffOptimizedRMSRightToLeft,SuperStrictBins);
    writeVetB("OnOffOptimizedRMSLeftToRight",OnOffOptimizedRMSLeftToRight,SuperStrictBins);
    writeVetB("OnOffOptimizedRMS",OnOffOptimizedRMS,SuperStrictBins);
#endif 
    

    /* (d) Make sure we include edge bins in the rejection: */
    for ( b = 2; b < SuperStrictBins-2; ++b)
    {
	if (OnOffOptimizedRMS[b] == RejectionOn)
	{
	    if (EvaluationRate[b-2] == 0) 
	    {
		OnOffOptimizedRMS[b-1] = RejectionOn;
	    }
	    
	    if (EvaluationRate[b+2] == 0)
	    {
		OnOffOptimizedRMS[b+1] = RejectionOn;
	    }
	}
    }

#ifdef WRITE_DEBUG
    writeVetB("OnOffOptimizedRMS_NextStep",OnOffOptimizedRMS,SuperStrictBins);
#endif   
    /* (e) Mask it with the low-shield rate cut */
    for ( b = 0; b < hknrows; ++b) 
    {
	if (OnOffStrictLSR[b]== RejectionOff) 
	{	    
	    index_bin=GetIndex(MinTime - 0.5, MaxTime + 0.5, SuperStrictBins, hkinfo[b].Time);
	    OnOffOptimizedRMS[index_bin]=RejectionOff;
	}
    }

#ifdef WRITE_DEBUG
    writeVetB("OnOffOptimizedRMS_BeforeSanityChecks",OnOffOptimizedRMS,SuperStrictBins);
#endif   
    
    /* (D) Sanity checks: */
    /* A normal SAA passage should give us regions which are way above the threshold.
       Thus if the average rate in our region is overall below the threshold, 
       and we either never exceed the threshold or we have a slightly variable source
       then we reject the SAA detection */
    if (global.par.SanityChecks == TRUE)
    {
	MaxFlux = 0.0;
	AverageFlux = 0.0;
	NAverageFluxes = 0;
	
	for (i = 0; i < SuperStrictBins; ++i)
	{
	    if (OnOffOptimizedRMS[i] == RejectionOn)
	    {
		if (EvaluationRate[i] > 0)
		{
		    AverageFlux += EvaluationRate[i];
		    NAverageFluxes++;
		    if (EvaluationRate[i] > MaxFlux)
		    {
			MaxFlux = EvaluationRate[i];
		    }
		}
	    }
	}
	

	if (NAverageFluxes > 0)
	{
	    AverageFlux = AverageFlux / NAverageFluxes;
	    Suspiciousness = 0;
	   
	    /* Usually the average flux is way high */
	    if (AverageFlux < Threshold && MaxFlux <= (MaxAllowedRMS+2.0)*NonSAARateRMS + NonSAARateMean)
	    {
		headas_chat(NORMAL,"%s: Info: Suspicious: The average flux value (%f  vs. max: %f) is smaller than the threshold (%f)\n", global.taskname,AverageFlux,MaxFlux,Threshold);
		Suspiciousness++;

		if (MaxFlux < Threshold)
		{
		    headas_chat(NORMAL,"%s: Info:             and the maximum flux value (%f) is smaller than the threshold (%f)\n", global.taskname,MaxFlux,Threshold);
		    Suspiciousness++;
		}

		if (NonSAARateRMS > global.saaparinfo.SN_HintOfVariabilityCutOff*NonSAARateSigma) 
		{
		    headas_chat(NORMAL,"%s: Info:             and we have a hint of a variable source\n", global.taskname);
		    ++Suspiciousness;
		}
	    }

	    /* More than 2 suspicousness points? No SAA! */
	    if (Suspiciousness >= global.saaparinfo.SN_SuspiciousnessThreshold)
	    {
		headas_chat(NORMAL,"%s: Info: Attention: The characteristics of the given SAA region is not in agreement with a typical one.\n", global.taskname);
		headas_chat(NORMAL,"%s: Info:            I ignore them all!\n", global.taskname);

		for (b = 0; b < SuperStrictBins; ++b) 
		{
		    OnOffOptimizedRMS[b] = RejectionOff;
		}
	    }
	}
    } 

#ifdef WRITE_DEBUG
    writeVetB("OnOffOptimizedRMSFinal",OnOffOptimizedRMS,SuperStrictBins);
#endif   
    
    /* (E) Now set it (slow...): */
    for (h = 0; h < hknrows; ++h)
    {	
      Bin=GetIndex(MinTime - 0.5, MaxTime + 0.5, SuperStrictBins, hkinfo[h].Time);
        if (Bin < 0 || Bin >= SuperStrictBins)
        {
	    headas_chat(NORMAL,"%s: Error: Something is wrong with the times as I cannot find a rejection value for time %.\n", global.taskname,hkinfo[h].Time);
	    SoftSAA[h] = 0;
        }
        else
        {
	    if (OnOffOptimizedRMS[Bin] == RejectionOn)
            {
		SoftSAA[h] = 1;
            }
            else
            {
		SoftSAA[h] = 0;	
            }
        }
    }

#ifdef WRITE_DEBUG  
    writeSAA("SoftSAA_OPTIMIZED.txt",hkinfo,SoftSAA,hknrows);
#endif
   

FindSAAs_new_end_ok:    

    /* Try free memory allocated */
    if(ExcludedDetRawXY!=NULL)
	free (ExcludedDetRawXY);
    if(SoftSAAOptimizedLSRRMS!=NULL)
	free (SoftSAAOptimizedLSRRMS);
    if(RawRate!=NULL)
	free (RawRate);
    if(NonSAARate!=NULL)
	free (NonSAARate);
    if(OnOffOptimizedRMSLeftToRight!=NULL)
	free (OnOffOptimizedRMSLeftToRight);
    if(OnOffOptimizedRMSRightToLeft!=NULL)
	free (OnOffOptimizedRMSRightToLeft);
    if(OnOffOptimizedRMS!=NULL)
	free (OnOffOptimizedRMS);
    if(NonSAARateHistogram!=NULL)
	free (NonSAARateHistogram);
    if(EvaluationRate!=NULL)
	free (EvaluationRate);
    if(LiveTime!=NULL)
	free (LiveTime);
    if(ShieldRateLow!=NULL)
	free (ShieldRateLow);
    if(ShieldRateLowBB!=NULL)
	free (ShieldRateLowBB);
    if(GradientHistogram!=NULL)
	free (GradientHistogram);
    if(OnOffInternalSAA!=NULL)
	free (OnOffInternalSAA);
    if(OnOffStrictLSR!=NULL)
	free (OnOffStrictLSR);
    if(baesian.m_BinEdges!=NULL)
	free (baesian.m_BinEdges);
    if(baesian.m_BinnedData!=NULL)
	free (baesian.m_BinnedData);
    
    return OK;

FindSAAs_new_end:
    
    /* Try free memory allocated */
    if(ExcludedDetRawXY!=NULL)
	free (ExcludedDetRawXY);
    if(SoftSAAOptimizedLSRRMS!=NULL)
	free (SoftSAAOptimizedLSRRMS);
    if(RawRate!=NULL)
	free (RawRate);
    if(NonSAARate!=NULL)
	free (NonSAARate);
    if(OnOffOptimizedRMSLeftToRight!=NULL)
	free (OnOffOptimizedRMSLeftToRight);
    if(OnOffOptimizedRMSRightToLeft!=NULL)
	free (OnOffOptimizedRMSRightToLeft);
    if(OnOffOptimizedRMS!=NULL)
	free (OnOffOptimizedRMS);
    if(NonSAARateHistogram!=NULL)
	free (NonSAARateHistogram);
    if(EvaluationRate!=NULL)
	free (EvaluationRate);
    if(LiveTime!=NULL)
	free (LiveTime);
    if(ShieldRateLow!=NULL)
	free (ShieldRateLow);
    if(ShieldRateLowBB!=NULL)
	free (ShieldRateLowBB);
    if(GradientHistogram!=NULL)
	free (GradientHistogram);
    if(OnOffInternalSAA!=NULL)
	free (OnOffInternalSAA);
    if(OnOffStrictLSR!=NULL)
	free (OnOffStrictLSR);
    if(baesian.m_BinEdges!=NULL)
	free (baesian.m_BinEdges);
    if(baesian.m_BinnedData!=NULL)
	free (baesian.m_BinnedData);
    
    return NOT_OK;
} /* FindSAAs_new */

DTYPE GetMax(DTYPE *vett,int size_vett)
{
    int i;
    DTYPE max=0;
    
    max=vett[0];
    
    for(i=1; i<size_vett; i++) 
    {
	if(vett[i]>max)
	{
	    max=vett[i];
	}
    }
    return max;
} /* GetMax */

int GetIndexMax(double *vett,int size_vett,int index_start)
{
    int i;
    double max;
    int maxInd=index_start;
    
    max=vett[index_start];
    
    for(i=index_start; i<size_vett; i++) 
    {
	if(vett[i]>max)
	{
	    max=vett[i];
	    maxInd=i;
	}
    }
    return maxInd;
} /* GetIndexMax */

int FindSAATentacle_new(HKRow_t *hkinfo, int hknrows, EVTRow_t *evtinfo, int evtnrows,double *FilteredRate, OrbitRow_t *orbitinfo, int orbitnrows, struct gti_struct *gti, BTYPE *SoftSAA, BTYPE *SoftTentacled)
{
    /* For Debugging */
#ifdef WRITE_DEBUG
    FILE *fp          = NULL;
    FILE *fp1         = NULL;
#endif
    int i					= 0;
    int h					= 0;
    int index_bin				= 0;
    int OIndex					= 0;
    int lt_index				= 0;
    double NonSAARateMax			= 0; 
    int ContentBins				= 0;
    double Content 				= 0.0;
    double OrbitDuration 			= 6240; /* sec  --- no need at all to be perfect here */
    double NTentacles         			= 0;
    double NLongTentacles         		= 0; 
    BOOL LastAdjacentCounted 			= FALSE;
    int LastAdjacentBin 			= 0; 
    double NAdjacentTentacles 			= 0;
    double LastDuration 			= 0;
    double DurationOfTentacles 			= 0;
    double MaxFlux 				= 0;
    double AverageFlux 				= 0;
    int NAverageFluxes 				= 0;
    int NCrossings				= 0;
    int Suspiciousness				= 0;
    BOOL Started 				= FALSE;
    int Bin					= 0;
    
    int Index1;
    int Index2;
    int b, bb, s;
    double NonSAARateMean;
    double NonSAARateRMS;
    double NonSAARateSigma;
    double Threshold;
    double MaxAllowedRMS;
    BOOL DoSourceElimination;
    BOOL DoRegionRestriction;
    BOOL DoSanityChecks;
    int OneSecBins;
    double MinTime, MaxTime;
    int TimeInterval;
    int EvaluationBins;
    int ExcludedDetRawXYrows;
    int DetectorID,RawX,RawY,ID,lower_bound;
    double TimeGap;
    double time;
   
    

    /* Define rejection flags */
    BTYPE RejectionOn, RejectionOff;
    
    int   *ExcludedDetRawXY		= NULL;
    DTYPE *EvaluationRate		= NULL;
    DTYPE *EvaluationRateNOLT		= NULL;
    
    DTYPE *NonSAAComparisonRate		= NULL;
    DTYPE *NonSAARateHistogram		= NULL;
    
    BTYPE *OnOffInternalSAA		= NULL;
    BTYPE *OnOffLeftToRight		= NULL;
    BTYPE *OnOffRightToLeft		= NULL;
    BTYPE *OnOff			= NULL;
    DTYPE *LiveTime			= NULL;
    
    /* (A) Initializations... */
    MaxAllowedRMS = global.par.TentacleCutRMSThreshold;            
    DoSourceElimination = global.par.ElimiateSource;
    DoRegionRestriction = global.par.TentacleCutRMSRegionRestriction;  
    DoSanityChecks = global.par.TentacleCutRMSSanityChecks;

    
    /* Number eliminated source */
    ExcludedDetRawXYrows = 0;
    
    /* The rejection flags */
    RejectionOn  = 0;
    RejectionOff = 1;

    
    /* One second binning */
    OneSecBins 	= GetMaxTime(hkinfo,hknrows) - GetMinTime(hkinfo,hknrows) + 1;
    MinTime 	= GetMinTime(hkinfo,hknrows);
    MaxTime 	= GetMaxTime(hkinfo,hknrows);
    
    /* 60-second binning -- good compromise for having enough statistics per bin, and small enough bins for not cutting out too much */
    TimeInterval = global.saaparinfo.TN_TentacleTimeInterval;             
    EvaluationBins = OneSecBins/TimeInterval;  /* Almost 60 second bins (but not perfectly) */
    
    
    
    headas_chat(NORMAL, "%s: ************* Find Tentacle New **************\n", global.taskname);
    headas_chat(NORMAL, "%s: Info: OneSecBins              = %d\n", global.taskname, OneSecBins);
    headas_chat(NORMAL, "%s: Info: EvaluationBins          = %d\n", global.taskname, EvaluationBins);
    headas_chat(NORMAL, "%s: Info: evtnrows                = %d\n", global.taskname, evtnrows);
    
    
    /* MALLOC and CALLOC */
    OnOffLeftToRight = (BTYPE*)calloc(EvaluationBins,sizeof(BTYPE));
    if(OnOffLeftToRight==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAATentacle_new: OnOffLeftToRight memory allocation failure.\n", global.taskname);
	goto FindSAATentacle_new_end;
    }
    
    OnOffRightToLeft = (BTYPE*)calloc(EvaluationBins,sizeof(BTYPE));
    if(OnOffRightToLeft==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAATentacle_new: OnOffRightToLeft memory allocation failure.\n", global.taskname);
	goto FindSAATentacle_new_end;
    }
    
    OnOff = (BTYPE*)calloc(EvaluationBins,sizeof(BTYPE));
    if(OnOff==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAATentacle_new: OnOff memory allocation failure.\n", global.taskname);
	goto FindSAATentacle_new_end;
    }
    
    OnOffInternalSAA = (BTYPE*)malloc(EvaluationBins*sizeof(BTYPE));
    if(OnOffInternalSAA==NULL) 
    {
        headas_chat(CHATTY,"%s: Error: FindSAATentacle_new: OnOffInternalSAA memory allocation failure.\n", global.taskname);
        goto FindSAATentacle_new_end;
    }
    
    EvaluationRate = (DTYPE*)calloc(EvaluationBins,sizeof(DTYPE));
    if(EvaluationRate==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAATentacle_new: EvaluationRate memory allocation failure.\n", global.taskname);
	goto FindSAATentacle_new_end;
    }
    
    EvaluationRateNOLT = (DTYPE*)calloc(EvaluationBins,sizeof(DTYPE));
    if(EvaluationRateNOLT==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAATentacle_new: EvaluationRateNOLT memory allocation failure.\n", global.taskname);
	goto FindSAATentacle_new_end;
    }
    
    NonSAAComparisonRate = (DTYPE*)calloc(EvaluationBins,sizeof(DTYPE));
    if(NonSAAComparisonRate==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAATentacle_new: NonSAAComparisonRate memory allocation failure.\n", global.taskname);
	goto FindSAATentacle_new_end;
    }
    
    LiveTime = (DTYPE*)malloc(hknrows*sizeof(DTYPE));
    if(LiveTime==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAAs_new: LiveTime memory allocation failure.\n", global.taskname);
	goto FindSAATentacle_new_end;
    }
       
    
    /* END MALLOC and CALLOC */
    
    for (i = 0; i < hknrows; i++) 
    {
	LiveTime[i] = hkinfo[i].LiveTime ;
    }
    
    if (hknrows == 0) 
    {
	headas_chat(NORMAL, "%s: Info: Something went wrong! No data!\n",global.taskname);
	return OK;
    }
    
    if (DoSourceElimination == TRUE)
    {
	MostlyEliminateSource(evtinfo,evtnrows,orbitinfo,orbitnrows,gti,&ExcludedDetRawXY,&ExcludedDetRawXYrows,FALSE);
    }
    
    
    for (b = 0; b < EvaluationBins; ++b) 
    {
	OnOffInternalSAA[b] = RejectionOff;
    }
    
#ifdef WRITE_DEBUG     
    fp = fopen( "input_TentacleEvaluationRate.txt" , "w" );
    fp1 = fopen( "input_TentacleEvaluationRate_LiveTime.txt" , "w" );
    fprintf(fp,"mintime=%f\n",MinTime-0.5);
    fprintf(fp,"maxtime=%f\n",MaxTime+0.5);
    fprintf(fp,"SpectrumMin=%f\n", (double)global.saaparinfo.SN_SpectrumMin);
    fprintf(fp,"SpectrumMax=%f\n",(double)global.saaparinfo.SN_SpectrumMax);
    fprintf(fp1,"mintime=%f\n",MinTime-0.5);
    fprintf(fp1,"maxtime=%f\n",MaxTime+0.5);
    fprintf(fp1,"SpectrumMin=%f\n", (double)global.saaparinfo.SN_SpectrumMin);
    fprintf(fp1,"SpectrumMax=%f\n",(double)global.saaparinfo.SN_SpectrumMax);
#endif
    
    /* (B) Create and fill the histograms */
    for (i = 0; i < evtnrows; ++i) 
    {
	if (WithinSpecialGTI(evtinfo[i].Time) == 0) 
	{
#ifdef WRITE_DEBUG     
    fprintf(fp,"ID %d --> Special GTI reject\n",i);
    fprintf(fp1,"ID %d --> Special GTI  reject\n",i);    
#endif
	    continue;
	}
	
	if (evtinfo[i].energy < global.saaparinfo.SN_SpectrumMin || evtinfo[i].energy > global.saaparinfo.SN_SpectrumMax) 
	{
#ifdef WRITE_DEBUG     
    fprintf(fp,"ID %d --> Energy reject\n",i);
    fprintf(fp1,"ID %d --> Energy reject\n",i);    
#endif
	    continue;    
	}

	if (IsGTI(gti, evtinfo[i].Time) == 0) 
	{		
#ifdef WRITE_DEBUG     
    fprintf(fp,"ID %d --> GTI reject\n",i);
    fprintf(fp1,"ID %d --> GTI reject\n",i);    
#endif
	    continue;
	}
	
	if (DoSourceElimination == TRUE)
	{
		DetectorID = evtinfo[i].DetectorID;
		RawX = evtinfo[i].RawX;
		RawY = evtinfo[i].RawY;
		ID = 10000*DetectorID + 100*RawX + RawY;
		
		lower_bound=MyLowerBound (ExcludedDetRawXY, ExcludedDetRawXYrows, ID);  
		
		if(lower_bound!=LOWER_BOUND_NOT_FOUND && lower_bound==ID)
		{		
#ifdef WRITE_DEBUG     
      fprintf(fp,"ID %d --> Source elim reject\n",i);
      fprintf(fp1,"ID %d --> Source elim reject\n",i);
#endif
		    continue;
		}
	}
	
	index_bin=GetIndex(MinTime-0.5, MaxTime+0.5, EvaluationBins, evtinfo[i].Time);
	
	FindClosestOrbitIndex(orbitinfo, orbitnrows, evtinfo[i].Time, &OIndex);
	FindHkIndex(hkinfo, hknrows, evtinfo[i].Time, &lt_index);
	/* headas_chat(NORMAL,"OIndex=%d Longitude=%f F.m_Time[%d]=%f  bin=%d\n",OIndex,orbitinfo[OIndex].Longitude,i,evtinfo[i].Time,index_bin); */
	
	
	if (orbitinfo[OIndex].slew == 0 && orbitinfo[OIndex].occulted==0 && orbitinfo[OIndex].SAA_A == 0 && orbitinfo[OIndex].SAA_B == 0)  
	{
		if(LiveTime[lt_index]>0)
		{
		    EvaluationRate[index_bin]=EvaluationRate[index_bin]+(1/LiveTime[lt_index]);
		}
		else
		{
		    EvaluationRate[index_bin]++;
		}
		
		EvaluationRateNOLT[index_bin]++;
#ifdef WRITE_DEBUG    
    fprintf(fp,"ID %d Time %f orbitindex %d bin %d  EvaluationRate[%d] %f\n",i, evtinfo[i].Time,OIndex,index_bin,index_bin,EvaluationRateNOLT[index_bin]);
    fprintf(fp1,"ID %d Time %f  bin %d  EvaluationRate[%d] %f\n",i, evtinfo[i].Time,index_bin,index_bin,EvaluationRate[index_bin]);
  } else {
    fprintf(fp,"ID %d --> Orbit reject: %d\n", i, OIndex);
    fprintf(fp1,"ID %d --> Orbit reject: %d\n", i, OIndex);
#endif
	}
	
	if (orbitinfo[OIndex].Longitude >= global.saaparinfo.TN_TentacleNonSAAMinLong && orbitinfo[OIndex].Longitude <= global.saaparinfo.TN_TentacleNonSAAMaxLong) 
	{
	    if (orbitinfo[OIndex].slew == 0 && orbitinfo[OIndex].occulted==0 && orbitinfo[OIndex].SAA_A == 0 && orbitinfo[OIndex].SAA_B == 0)  
	    {
		if(LiveTime[lt_index]>0)
		{
		    NonSAAComparisonRate[index_bin]=NonSAAComparisonRate[index_bin]+(1/LiveTime[lt_index]);
		}
		else
		{
		    NonSAAComparisonRate[index_bin]++;
		}
		
	    }
	} 
    }

#ifdef WRITE_DEBUG     
    fclose(fp);
    fclose(fp1);
#endif    
    
      
    for (i = 0; i < hknrows; i++) 
    {
	FindClosestOrbitIndex(orbitinfo, orbitnrows, hkinfo[i].Time, &OIndex);
	
	index_bin=GetIndex(MinTime-0.5, MaxTime+0.5, EvaluationBins, hkinfo[i].Time);
	
	if ( orbitinfo[OIndex].SAAFlag == 1 )
	{
	    OnOffInternalSAA[index_bin]=RejectionOn;
	}
    }

#ifdef WRITE_DEBUG
    writeVetD("TentacleEvaluationRateNOLT",EvaluationRateNOLT,EvaluationBins);
    writeVetD("TentacleEvaluationRate",EvaluationRate,EvaluationBins);
    writeVetD("TentacleNonSAAComparisonRate",NonSAAComparisonRate,EvaluationBins);
    writeVetB("TentacleOnOffInternalSAA",OnOffInternalSAA,EvaluationBins);
#endif  
    
    NonSAARateMax = GetMax(NonSAAComparisonRate,EvaluationBins);
    
    /* (a) Create a rate histogram  */
    NonSAARateHistogram = (DTYPE*)calloc(EvaluationBins,sizeof(DTYPE)); /* il vettore va da 0 a NonSAARateMax con 1000 bins */
    if(NonSAARateHistogram==NULL)
    {
	headas_chat(CHATTY,"%s: Error: FindSAATentacle_new: NonSAARateHistogram memory allocation failure.\n", global.taskname);
	goto FindSAATentacle_new_end;
    }
    
    for (b = 1; b < EvaluationBins-1; ++b)
    {
	if (NonSAAComparisonRate[b-1] > 0  && NonSAAComparisonRate[b] > 0 && NonSAAComparisonRate[b+1] > 0)
	{   
	    NonSAARateHistogram[b]=NonSAAComparisonRate[b];
	}
    }
    
    
    /* (b) Calculate Media and RMS */
    NonSAARateMean	= GetMean(NonSAARateHistogram,0,EvaluationBins);
    NonSAARateRMS	= GetRMS(NonSAARateHistogram,0,EvaluationBins);
    NonSAARateSigma	= sqrt(NonSAARateMean);
    Threshold		= MaxAllowedRMS*NonSAARateRMS + NonSAARateMean;
    
    
    /* Take care of the rare case when we have observations with strong outbursts
       Normal outburst should increase the RMS thus we are automatically in a higher threshold regime
       Then we set the threshold to the maximum + 1 rms
       There is only one case where this ever happened "RapidBurster"

       If the rms is more than 3 times the expected 1-sigma value, then we have a burster 
       Thus set the threshold to maximum + MaxAllowedRMS*rms  */
    if (NonSAARateRMS > global.saaparinfo.SN_TimeOneSigma * NonSAARateSigma) 
    {
	Threshold = NonSAARateMax + MaxAllowedRMS*NonSAARateRMS;	
	headas_chat(NORMAL,"%s: Info: Bursts detected! Threshold is max + %f *rms.\n", global.taskname,MaxAllowedRMS);
    }
    
    /* When we have no data in the NonSAA region our threshold will be zero,
       Set it ti the maximum */
    if (Threshold == 0) 
    {
	headas_chat(NORMAL,"%s: Info: Threshold would be zero since we have no data, setting it to max.\n", global.taskname);
	Threshold = FLT_MAX / 100;
    }
    
    headas_chat(NORMAL,"%s: Info: Threshold calculation diagnostics based on non-SAA regions:\n", global.taskname);
    headas_chat(NORMAL,"%s: Info: Average rate in a %d-sec interval: %f \n", global.taskname,global.saaparinfo.SN_SuperStrictOffTimeInterval,NonSAARateMean);
    headas_chat(NORMAL,"%s: Info: Maximum rate: %f\n", global.taskname,NonSAARateMax);
    headas_chat(NORMAL,"%s: Info: RMS: %f  (vs. 1-sigma: %f)\n", global.taskname,NonSAARateRMS,sqrt(NonSAARateMean));
    headas_chat(NORMAL,"%s: Info: --> Threshold rate: %f\n", global.taskname,Threshold);

    headas_chat(NORMAL,"%s: Info: NonSAARateMax           = %f\n", global.taskname,NonSAARateMax);   
    headas_chat(NORMAL,"%s: Info: NonSAARateMean          = %f\n", global.taskname,NonSAARateMean);   
    headas_chat(NORMAL,"%s: Info: NonSAARateRMS           = %f\n", global.taskname,NonSAARateRMS);
    headas_chat(NORMAL,"%s: Info: NonSAARateSigma         = %f\n", global.taskname,NonSAARateSigma);
    headas_chat(NORMAL,"%s: Info: MaxAllowedRMS           = %f\n", global.taskname,MaxAllowedRMS);
    headas_chat(NORMAL,"%s: Info: Threshold               = %f\n", global.taskname,Threshold);
    
    
    /* (D) Determine the regions to reject */
  
    /* Approach:
       Assuming we have a threshold of 5 RMS
       Then we look for single bins with an increase of 5 RMS, two-bins with an increase of 5/sqrt(2), 
       three bins with an increase of 5/sqrt(3), etc. until we reach our max search distance MaxSearchDistance
       Since this approach has a tail (e.g. we have one gigantic increase and the two following bins normal, 
       then a search distance of 3 would catch all three although we do not have an increase in the latter 2),
       We do it once from the left and once from the right and then "AND" those two together --> no more tails
       Finally "AND" / mask it with the previously dtermined regions of low-shield rate increases
       The maximum amount of adjacent bins we serach for a significant rate increase
       (a) left to right */
    for (b = 0; b < EvaluationBins; ++b) 
    {
	OnOffLeftToRight[b] = RejectionOff;
    }
    
   
  
    /* First seach for one-bin X-rms increases, then two-bin X/sqrt(2) increases, etc. */
    for (s = 1; s <= global.saaparinfo.SN_MaxSearchDistance; ++s) 
    {
	for (b = 0; b < EvaluationBins; ++b) 
	{
	    ContentBins = 0;
	    Content = 0.0;

	    for (bb = b; bb < b+s && b < EvaluationBins; ++bb) 
	    {
		if (EvaluationRate[bb] > NonSAARateMean) 
		{ 
		    /* stop immediately if we are below mean */
		    Content += EvaluationRate[bb];
		    ContentBins++;
		} 
		else
		{
		    break; 
		}
	    }
	    if (ContentBins > 0 && NonSAARateMean > 0) 
	    { /* NonSAARateMean > 0 means we do have threshold values, i.e. data i the non-SAA region */
		if ( Content/ContentBins > NonSAARateRMS * MaxAllowedRMS/sqrt(ContentBins) + NonSAARateMean) 
		{
		    /* We are above our threshold -> reject */
		    for (bb = b; bb < b+ContentBins; ++bb) 
		    {
			OnOffLeftToRight[bb] = RejectionOn; 
		    }
		}
	    }
	}
    }

    /* (b) Search right to left */
    for (b = 0; b < EvaluationBins; ++b) 
    {
	OnOffRightToLeft[b] = RejectionOff;
    }

    for (s = 1; s <= global.saaparinfo.SN_MaxSearchDistance; ++s) 
    {
	for (b = EvaluationBins-1; b >= 0; --b) 
	{
	    ContentBins = 0;
	    Content = 0.0;
	    
	    for (bb = b; bb > b-s && b >= 0; --bb) 
	    {
		if (EvaluationRate[bb] > NonSAARateMean) 
		{
		    Content += EvaluationRate[bb];
		    ContentBins++;
		} 
		else 
		{
		    break; 
		}
	    }
	    
	    if (ContentBins > 0 && NonSAARateMean > 0) 
	    { 
		/* NonSAARateMean > 0 means we do have threshold values, i.e. data i the non-SAA region */
		if ( Content/ContentBins > NonSAARateRMS * MaxAllowedRMS/sqrt(ContentBins) + NonSAARateMean) 
		{
		    for (bb = b; bb > b-ContentBins; --bb) 
		    {
			OnOffRightToLeft[bb] = RejectionOn; 
		    }
		}
	    }
	}   
    }
    
    /* (c) "AND" them together: Only when both are ON set the combined to ON */
    for (b = 0; b < EvaluationBins; ++b) 
    {
	if (OnOffRightToLeft[b] == RejectionOn && OnOffLeftToRight[b] == RejectionOn) 
	{
	    OnOff[b] = RejectionOn;
	} 
	else 
	{
	    OnOff[b] = RejectionOff;
	}
    }

    /* (d) Make sure we include edge bins in the rejection: */
    for ( b = 2; b < EvaluationBins-2; ++b)
    {
	if (OnOff[b] == RejectionOn)
	{
	    if (EvaluationRate[b-2] == 0) 
	    {
		OnOff[b-1] = RejectionOn;
	    }
	    
	    if (EvaluationRate[b+2] == 0)
	    {
		OnOff[b+1] = RejectionOn;
	    }
	}
    }
    
    /* (E) Restriction to tentacle region */
    time=MinTime+(global.saaparinfo.TN_TentacleTimeInterval/2)-0.5;
    
    if (DoRegionRestriction == TRUE) 
    {
	for (b = 0; b < EvaluationBins; ++b) 
	{
	    if (OnOff[b] == RejectionOn) 
	    {
		FindClosestOrbitIndex(orbitinfo, orbitnrows, time+(b*global.saaparinfo.TN_TentacleTimeInterval), &OIndex);
		
		if (OIndex == -1) 
		{
			headas_chat(NORMAL,"%s: Error: No suiting orbit data found!\n", global.taskname);
			continue;
		}
		
		if ( fabs(orbitinfo[OIndex].Time-(time+(b*global.saaparinfo.TN_TentacleTimeInterval))) > 60 ) 
		{
			headas_chat(NORMAL,"%s: Error: Orbits is %d seconds from data\n", global.taskname, fabs((orbitinfo[OIndex].Time-MinTime) - b));
			continue;
		}
		if (orbitinfo[OIndex].Longitude < global.saaparinfo.TN_TentacleLongitudeMin || orbitinfo[OIndex].Longitude > global.saaparinfo.TN_TentacleLongitudeMax) 
		{
			OnOff[b]=RejectionOff;
		}
	    }
	}
    }
    
    /* (F) Sanity checks */
#ifdef WRITE_DEBUG         
    writeVetB("TentacleOnOffRMSRightToLeft",OnOffRightToLeft,EvaluationBins);
    writeVetB("TentacleOnOffRMSLeftToRight",OnOffLeftToRight,EvaluationBins);
    writeVetB("TentacleOnOffRMS",OnOff,EvaluationBins);
#endif
    
    
    if (DoRegionRestriction == TRUE && DoSanityChecks == TRUE) 
    {
	for (i = 0; i < EvaluationBins; ++i) 
	{
	    if (OnOff[i] == RejectionOn) 
	    {
		Started = TRUE;
		LastDuration++;
		if (EvaluationRate[i] > 0) 
		{
			AverageFlux += EvaluationRate[i];
			if (EvaluationRate[i] > MaxFlux) 
			{
				MaxFlux = EvaluationRate[i];
			}
			NAverageFluxes++;
		}
		if (OnOffInternalSAA[i] == RejectionOn) 
		{ 
			/* Skip those here since we have an SAA and not a tentacle */
			while (OnOff[i] == RejectionOn) 
			{
			    ++i;
			}
			Started = FALSE;
			LastDuration = 0;
		}
	    } 
	    else 
	    {
		if (Started == TRUE) 
		{
		      headas_chat(NORMAL,"%s: Info: Tentacle %f: Duration: %f\n", global.taskname,i-LastDuration,LastDuration);
		      DurationOfTentacles += LastDuration;
		      NTentacles++;
			if (LastDuration >= global.saaparinfo.TN_TentacleLongDuration) NLongTentacles++;
		      Started = FALSE;
		      LastDuration = 0;
		      if (LastAdjacentBin != 0) 
		      {
			  TimeGap = (time+(i*global.saaparinfo.TN_TentacleTimeInterval)) - (time+(LastAdjacentBin*global.saaparinfo.TN_TentacleTimeInterval));
			  /*printf("TimeGap=%f OnOff->GetBinCenter(i)=%f OnOff->GetBinCenter(LastAdjacentBin)=%f  LastAdjacentBin=%d\n",
				 TimeGap,
				 (double)(time+(i*global.saaparinfo.TN_TentacleTimeInterval)),
				 (double)(time+(LastAdjacentBin*global.saaparinfo.TN_TentacleTimeInterval)),
					  LastAdjacentBin);*/
			  if ((int)((TimeGap + OrbitDuration/2)/OrbitDuration) == 1) 
			  {
			      ++NAdjacentTentacles;
			      if (LastAdjacentCounted == FALSE) 
			      {
				++NAdjacentTentacles;
				LastAdjacentCounted = TRUE; 
			      }
			  } 
			  else 
			  {
			      LastAdjacentCounted = FALSE; 
			  }
		      } 
		      LastAdjacentBin = i;
		}	
	    }
	}
	
	time=MinTime+(global.saaparinfo.TN_TentacleTimeInterval/2)-0.5;

	if (NTentacles > 0) 
	{
		DurationOfTentacles /= NTentacles;
		AverageFlux /= NAverageFluxes;
		
		for (b = 0; b < EvaluationBins-1; ++b) 
		{
		      if (EvaluationRate[b] > 0 && EvaluationRate[b+1] > 0) 
		      {

				FindClosestOrbitIndex(orbitinfo, orbitnrows, time+(b*global.saaparinfo.TN_TentacleTimeInterval), &Index1);
				FindClosestOrbitIndex(orbitinfo, orbitnrows, time+((b+1)*global.saaparinfo.TN_TentacleTimeInterval), &Index2);
				if (orbitinfo[Index1].Latitude >= global.saaparinfo.TN_TentacleMinLatitude && 
				  orbitinfo[Index2].Latitude >= global.saaparinfo.TN_TentacleMinLatitude &&
				  orbitinfo[Index1].Longitude <= global.saaparinfo.TN_TentacleAvgLongitude &&
				  orbitinfo[Index2].Longitude > global.saaparinfo.TN_TentacleAvgLongitude)
				{
					NCrossings++;
				}
		      }
		}
		
		headas_chat(NORMAL,"%s: Info: Number of tentacles: %f  (vs. %d orbits) with average duration: %f\n", global.taskname,NTentacles,NCrossings,DurationOfTentacles);
		headas_chat(NORMAL,"%s: Info: Number of adjacent tentacles: %f\n", global.taskname,NAdjacentTentacles);
		headas_chat(NORMAL,"%s: Info: Average flux: %f  vs. %f\n", global.taskname,AverageFlux,NonSAARateMean);

		Suspiciousness = 0;
		
		/* Sanity check 1:
		   If we really have a tentacle most of the tentacle region crossings should produce a signal */
		if (NTentacles < global.saaparinfo.TN_TentacleVsRegionCrossingsBadCutoff * NCrossings) 
		{
			headas_chat(NORMAL,"%s: Info: Suspicious: Only %f tentacles in %d crossings of the tentacle zone.\n", global.taskname,NTentacles,NCrossings);
			++Suspiciousness;
			if (NTentacles < global.saaparinfo.TN_TentacleVsRegionCrossingsReallyBadCutoff * NCrossings) 
			{
			  ++Suspiciousness;
			}
		}
		
		/* Sanity check 2:
		   The average tentacle cut duration is 4-5, i.e. 240-300 seconds
		   = 150 sec<-- into caldb (60 is from default Timeintervall) */
		if (DurationOfTentacles <= global.saaparinfo.TN_TentacleDurationCutOff) 
		{
			headas_chat(NORMAL,"%s: Info: Suspicious: The average duration of a tentacle passing is only %f instead of ~5.\n", global.taskname,DurationOfTentacles);
			++Suspiciousness;
		}
		
		/* Sanity check 3:
		   Tentacle follows after tentacle follows after tentacle follows...  */
		if (NTentacles > 2 && NAdjacentTentacles < global.saaparinfo.TN_TentacleAdjacentOrNotCutOff * NTentacles) 
		{
			headas_chat(NORMAL,"%s: Info: Suspicious: Only %f out of %f are in sequential (or same) orbit.\n", global.taskname,NAdjacentTentacles,NTentacles);
			++Suspiciousness;
		}
		
		/* Sanity check 4:
		   If the average flux in the tentacles is below the threshold then we have mostly likely just random flux increases
		   It is even worse, if there is some slight source variability  */
		if (AverageFlux < Threshold && MaxFlux <= 2.0*MaxAllowedRMS*NonSAARateRMS + NonSAARateMean)
		{
			headas_chat(NORMAL,"%s: Info: Suspicious: The average OVERALL tentacle flux is below the threshold: %f vs. %f\n", global.taskname,AverageFlux,Threshold);
			++Suspiciousness;
			if (NonSAARateRMS > global.saaparinfo.SN_HintOfVariabilityCutOff * NonSAARateSigma) 
			{
				headas_chat(NORMAL,"%s: Info:            and we have a hint of a variable source.\n", global.taskname);
				++Suspiciousness;
			}
		}
		
		/* Sanity check 5:
		   If we have a few tentacles with above average length, then we likely have a tentacle */
		if (NLongTentacles > global.saaparinfo.TN_TentacleLongThreshold) {
			headas_chat(NORMAL,"%s: Info: Positive: We have a few long duration tentacles: %d out of %d\n",global.taskname, NLongTentacles, NTentacles);    
		    --Suspiciousness;
		}
      
		/* Sanity check 6:
		   If we have a tentacle with at least 5 times the average flux, then we likely have a tentacle  */
		if (MaxFlux > global.saaparinfo.TN_TentacleFluxThreshold*AverageFlux) {
		    headas_chat(NORMAL,"%s: Info: Positive: We have at least one strong tentacles: %f vs. average %f\n", global.taskname, MaxFlux, AverageFlux);    
		    --Suspiciousness;
		}
		
		
		
		/* More than 2 suspicousness points? No Tentacle! */
		if (Suspiciousness >= global.saaparinfo.TN_TentacleSuspiciousnessThreshold) 
		{
			headas_chat(NORMAL,"%s: Info: Attention: The characteristics of the given tentacle regions is not in agreement with a typical tentacle.\n", global.taskname);
			headas_chat(NORMAL,"%s: Info:           I ignore them all!.\n", global.taskname);
			for (b = 0; b < EvaluationBins; ++b) 
			{
				OnOff[b]=RejectionOff;
			}
		}
      
	}
    }

#ifdef WRITE_DEBUG
    writeVetB("TentacleOnOffRMSFinal",OnOff,EvaluationBins);
#endif    
    
    /* (G) Now set it: */
    for (h = 0; h <hknrows ; ++h)
    {
        Bin=GetIndex(MinTime-0.5, MaxTime + 0.5, EvaluationBins, hkinfo[h].Time);
        if (Bin < 0 || Bin >= EvaluationBins)
        {
            headas_chat(NORMAL,"%s: Error: Something is wrong with the times as I cannot find a rejection value for time %f.\n", global.taskname,hkinfo[h].Time);
            SoftTentacled[h] = 1;
        }
        else
        {
            if (OnOff[Bin] == RejectionOn)
            {
                SoftTentacled[h] = 1;
            }
            else
            {
                SoftTentacled[h] = 0;
            }
        }
    }
    
#ifdef WRITE_DEBUG
    writeVetB("TentacleOnOffAll", SoftTentacled, hknrows);
    writeSAA("SoftSAA_TENTACLE.txt",hkinfo,SoftTentacled,hknrows);
#endif    
   
    headas_chat(NORMAL,"%s: Info: Done with tentacle flag identification - RMS mode.\n", global.taskname);
    
    if(LiveTime!=NULL)
	free (LiveTime);
    if(ExcludedDetRawXY!=NULL)
	free (ExcludedDetRawXY);
    if(EvaluationRate!=NULL)
	free (EvaluationRate);
    if(NonSAAComparisonRate!=NULL)
	free (NonSAAComparisonRate);
    if(OnOffInternalSAA!=NULL)
	free (OnOffInternalSAA);
    if(NonSAARateHistogram!=NULL)
	free (NonSAARateHistogram);
    if(OnOffLeftToRight!=NULL)
	free (OnOffLeftToRight);
    if(OnOffRightToLeft!=NULL)
	free (OnOffRightToLeft);
    if(OnOff!=NULL)
	free (OnOff);
        
    return OK;
    
FindSAATentacle_new_end:

    if(LiveTime!=NULL)
	free (LiveTime);
    if(ExcludedDetRawXY!=NULL)
	free (ExcludedDetRawXY);
    if(EvaluationRate!=NULL)
	free (EvaluationRate);
    if(NonSAAComparisonRate!=NULL)
	free (NonSAAComparisonRate);
    if(OnOffInternalSAA!=NULL)
	free (OnOffInternalSAA);
    if(NonSAARateHistogram!=NULL)
	free (NonSAARateHistogram);
    if(OnOffLeftToRight!=NULL)
	free (OnOffLeftToRight);
    if(OnOffRightToLeft!=NULL)
	free (OnOffRightToLeft);
    if(OnOff!=NULL)
	free (OnOff);

    return NOT_OK;    
} /* FindSAATentacle_new */

int FindSAAs(HKRow_t *hkinfo, int hknrows, double *FilteredRate, OrbitRow_t *orbitinfo, int orbitnrows, struct gti_struct *gti, BTYPE *SoftSAA) 
{

    int                i=0, j=0, jmin, jmax, OIndex=0;
    BTYPE              *RejectionOnOff ;
    double             *LiveTime, *ShieldRateHighAverage, *ShieldRateHighCut, *NAcceptedEvents, *NAcceptedEventsShieldHighCut;
    int                *ShieldRateHigh;
    double             *CleanRate, *FilteredRateAverageLarge, *FilteredRateAverageSmall;
    double             LiveTimeNoCuts=0, LiveTimeStrictCuts=0, LiveTimeOptimizedCuts=0;


    /* Step 1 */
    /* Half length of the interval over which we average: */
    int                HalfAverageLength = global.saaparinfo.S_HalfAverageLength ; /* good values: 30-60 */

    int                NBins=0, Min, Max, Dist;
    double             Average=0;

    /* Step 2 : We decide based on a few key numbers of the average high veto rate: */
    /* This is the distance between two data points on which we determine that we have a significant increase  */
    int                DecisionDistance = HalfAverageLength * global.saaparinfo.S_DecisionDistanceFactor ; /* 2 for 30 and 60 */
    /* This is the threshold of increase we require for starting to skip */
    /* There is a slight difference between left and right to account for delayed activation */
    double             DecisionThresholdLeft = global.saaparinfo.S_DecisionThrLeft ; /* good: 30+1.15, 60+1.35 */
    double             DecisionThresholdRight = DecisionThresholdLeft * global.saaparinfo.S_DecisionThrRightFactor ; /* good: 30+1.15+0.95, 60+1.3+0.95 */

    double             EnteringValueLeft=0, EnteringValueRight=0 ;
    int                DecisionPointLeft, DecisionPointRight ;
    unsigned           Cut = 0;

    int                SAARampingTime = global.saaparinfo.S_SAARampingTime ;
    unsigned           InSAAorFreakEvent = 0;

    double             MinLong=global.saaparinfo.S_MinLong, MaxLong=global.saaparinfo.S_MaxLong ;

    int                CutOffCount = HalfAverageLength * global.saaparinfo.S_CutOffCountFactor ; /* good: 2x */
    int                OffCount=0, OnCount=0;

    int                FilteredEventsHalfAverageLarge=global.saaparinfo.S_FltEvtHalfAverageLarge , FilteredEventsHalfAverageSmall=global.saaparinfo.S_FltEvtHalfAverageSmall ;
    double             FilteredDecisionThresholdLeft=global.saaparinfo.S_FltDecisionThrLeft , FilteredDecisionThresholdRight=global.saaparinfo.S_FltDecisionThrRight, SingleRateDecisionThreshold=global.saaparinfo.S_SingleRateDecisionThr ;
    unsigned           Previous;


    if( !strcasecmp(global.par.saamode,"NONE") ) {
        return OK;
    }

    
    /* Allocate memory to storage all data */

    ShieldRateHighAverage = (double*)calloc(hknrows, sizeof(double));  /* array initialized to zero */
    if(ShieldRateHighAverage==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }

    RejectionOnOff = (BTYPE*)malloc(hknrows*sizeof(BTYPE));
    if(RejectionOnOff==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }

    ShieldRateHigh = (int*)malloc(hknrows*sizeof(int));
    if(ShieldRateHigh==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }

    ShieldRateHighCut = (double*)malloc(hknrows*sizeof(double));
    if(ShieldRateHighCut==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }

    NAcceptedEvents = (double*)malloc(hknrows*sizeof(double));
    if(NAcceptedEvents==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }

    NAcceptedEventsShieldHighCut = (double*)malloc(hknrows*sizeof(double));
    if(NAcceptedEventsShieldHighCut==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }

    LiveTime = (double*)malloc(hknrows*sizeof(double));
    if(LiveTime==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }


    /*for ( i = 0; i < hknrows; i++) 
    {
	    headas_chat(NORMAL,"%s: INFO_R: FilteredRate[%d]=%f.\n", global.taskname,i,FilteredRate[i]);
    }
    exit(1);*/
    
    for(i=0; i<hknrows; i++) {

        ShieldRateHigh[i] = hkinfo[i].ShieldRateHigh ;
        ShieldRateHighCut[i] = hkinfo[i].ShieldRateHigh ;

        NAcceptedEvents[i] = hkinfo[i].NAcceptedEvents ;
        NAcceptedEventsShieldHighCut[i] = hkinfo[i].NAcceptedEvents ;

        LiveTime[i] = hkinfo[i].LiveTime ;

        FindClosestOrbitIndex(orbitinfo, orbitnrows, hkinfo[i].Time, &OIndex);

        /* Some rare events happen even during known SAA passages (super-high energy protons?) - skip those */
        if ( orbitinfo[OIndex].SAAFlag == 0 && hkinfo[i].ShieldRateHigh > 0 ) {
            RejectionOnOff[i] = 1;
        } else {
            RejectionOnOff[i] = 0;
        }

    }

    /* Exclude occultation and slew time intervals */
    for(i=0; i<hknrows; i++) {

        FindClosestOrbitIndex(orbitinfo, orbitnrows, hkinfo[i].Time, &OIndex);

        if( orbitinfo[OIndex].occulted == 1 || orbitinfo[OIndex].slew == 1 )
            FilteredRate[i] = 0;
    }

    /* Apply livetime correction to event rates  */
    for(i=0; i<hknrows; i++) {
        FilteredRate[i] = (LiveTime[i]>0) ? FilteredRate[i]/LiveTime[i] : FilteredRate[i];
    }


    /* Step 1: Calculate averages */

    for ( i = 0; i < hknrows; i++) {

        Average = 0;
        NBins = 0;

        /* Determine the interval over which we average dynamically to take care of the edges */
        Min = (i-HalfAverageLength<0) ? 0 : i-HalfAverageLength ;
        Max = (i+HalfAverageLength>=hknrows) ? hknrows-1 : i+HalfAverageLength ;

        for ( j=Min; j<=Max; j++) {
            /* We need to jump over bins where we don't have shield or HK data!!! Why do we have gaps in HK data?? */
            if (ShieldRateHigh[j] > 1) {
                NBins++;
                Average += ShieldRateHigh[j];
            }
        }
        Average /= NBins;

        if ( ShieldRateHigh[i] > 1 ) {  /* we have some stray "1" during SAA passages... */
            ShieldRateHighAverage[i]= Average ;
        }
    }

    /* Step 2: Decide which regions to skip: */

    /* We also store the shield-rate values when we started skipping from the left (lower time values) and the right (higher time values) */
    for ( i = 0; i < hknrows; i++) {

        /* We always make a decision from the left and from the rights */

        /* Here we take care of the boarders of the distribution */
        DecisionPointLeft = i-DecisionDistance<0 ? 0 : i-DecisionDistance ;
        DecisionPointRight = i+DecisionDistance>=hknrows ? hknrows-1 : i+DecisionDistance ;


        /* We cut this bin if: */

        Cut = 0;

        /* ... we have an increase above our decision threshold from left to right */
        if ( ShieldRateHighAverage[DecisionPointLeft] > 1 &&
                ShieldRateHighAverage[i] > DecisionThresholdLeft*ShieldRateHighAverage[DecisionPointLeft] ) {
            Cut = 1;
        }

        /* ... we have an increase above our decision threshold from right to left */
        if ( ShieldRateHighAverage[DecisionPointRight] > 1 &&
                ShieldRateHighAverage[i] > DecisionThresholdRight*ShieldRateHighAverage[DecisionPointRight] ) {
            Cut = 1;
        }

        /* Next we take care of the case if the increase is no longer above the decision threshold, */
        /* but we are still above the initial increase value: */

        /* If we have both a left and a right value we cut if we are still above the higher values */
        if (EnteringValueLeft > 0 && EnteringValueRight  > 0) {
            if (EnteringValueLeft > EnteringValueRight) {
                if (ShieldRateHighAverage[i] > EnteringValueLeft) {
                    Cut = 1;
                }
            }
            else {
                if (ShieldRateHighAverage[i] > EnteringValueRight) {
                    Cut = 1;
                }
            }
        }
        /* If we have just one value, we only cut on it */
        else {
            if ((ShieldRateHighAverage[i] > EnteringValueLeft && EnteringValueLeft > 0) ||
                    (ShieldRateHighAverage[i] > EnteringValueRight && EnteringValueRight  > 0)) {
                Cut = 1;
            }
        }


        if ( Cut == 1 ) {
            RejectionOnOff[i] = 0;
            /* Update the entering values */
            if ( ShieldRateHighAverage[i] > DecisionThresholdLeft*ShieldRateHighAverage[DecisionPointLeft] &&
                    (EnteringValueLeft == 0 || ShieldRateHighAverage[i] < EnteringValueLeft) ) {
                EnteringValueLeft = ShieldRateHighAverage[i];
            }
            if (ShieldRateHighAverage[i] > DecisionThresholdRight*ShieldRateHighAverage[DecisionPointRight] &&
                    (EnteringValueRight == 0 || ShieldRateHighAverage[i] < EnteringValueRight)) {
                EnteringValueRight = ShieldRateHighAverage[i];
            }
        }
        else {
            EnteringValueLeft = 0;
            EnteringValueRight = 0;
        }
    }


    /* Clean the periods immediately after SAA passages and in SAA passages - since we are still ramping up the voltage and don't get all vetoes */

    InSAAorFreakEvent = 0;
    for ( i = 0; i<hknrows; i++) {
        FindClosestOrbitIndex(orbitinfo, orbitnrows, hkinfo[i].Time, &OIndex);

        if ( orbitinfo[OIndex].SAAFlag  == 1 ) {
            InSAAorFreakEvent = 1;
            RejectionOnOff[i] = 0;
        }
        else {
            if ( InSAAorFreakEvent == 1 ) {
                /* Flag the next "SAARampingTime" seconds as still bad, since the high voltage is ramping up slowly */
                for ( j = i; j < hknrows && j-i < SAARampingTime; j++) {
                    RejectionOnOff[j] = 0;
                }
            }
            InSAAorFreakEvent = 0;
        }
    }


    /* Clean up - all SAA passages are confinend between MinLong and MaxLong */
    /* We might replace this one day with a real outer SAA contour... */
    for ( i = 0; i < hknrows; i++) {
        FindClosestOrbitIndex(orbitinfo, orbitnrows, hkinfo[i].Time, &OIndex);

        if ( fabs( orbitinfo[OIndex].Time - hkinfo[i].Time ) < 1.0 ) {
            if ( orbitinfo[i].Longitude < MinLong || orbitinfo[i].Longitude > MaxLong) {
                RejectionOnOff[i] = 1;
            }
        }
        else {
            headas_chat(NORMAL, "%s: Warning: no orbit information found for HK TIME = %f \n",global.taskname, hkinfo[i].Time);
        }
    }


    /* Clean up - remove off periods < X*HalfAverageLength  */

    OffCount = 0;
    for ( i = 0; i < hknrows; i++) {
        if (RejectionOnOff[i] == 1) {
            if (OffCount > 0 && OffCount < CutOffCount) {
                jmax = i-1<0 ? 0 : i-1 ;
                jmin = i-OffCount<0 ? 0 : i-OffCount ;
                for ( j = jmax; j >= jmin; j--) {
                    if ( RejectionOnOff[j] == 0 ) {
                        RejectionOnOff[j] = 1;
                    }
                    else {
                        headas_chat(NORMAL, "%s: Warning: On remove error...\n",global.taskname);
                    }
                }
            }
            OffCount = 0;
        }
        else {
            OffCount++;
        }
    }

    /* Remove on periods < X*HalfAverageLength */

    OnCount = 0;
    for ( i = 0; i < hknrows; i++) {
        if (RejectionOnOff[i] == 0) {
            if (OnCount > 0 && OnCount < CutOffCount) {
                jmax = i-1<0 ? 0 : i-1 ;
                jmin = i-OnCount<0 ? 0 : i-OnCount ;
                for ( j = jmax; j >= jmin; j--) {
                    if ( RejectionOnOff[j] == 1 ) {
                        RejectionOnOff[j] = 0;
                    }
                    else {
                        headas_chat(NORMAL, "%s: Warning: On remove error...\n",global.taskname);
                    }
                }
            }
            OnCount = 0;
        }
        else {
            OnCount++;
        }
    }


    for ( i = 0; i < hknrows; i++) {

        if( IsGTI(gti, hkinfo[i].Time) )
            LiveTimeNoCuts += LiveTime[i];

        if ( ( RejectionOnOff[i] == 1 ) && IsGTI(gti, hkinfo[i].Time) )
            LiveTimeStrictCuts += LiveTime[i];
    }


    if( !strcasecmp(global.par.saamode,"STRICT") ) {

        for ( i = 0; i < hknrows; i++) {

            if( RejectionOnOff[i]==0 )
                SoftSAA[i] = 1;
            else
                SoftSAA[i] = 0;

        }

        headas_chat(NORMAL,"%s: Info: Live times: \n", global.taskname );
        headas_chat(NORMAL,"%s: Info: No cut: %f\n", global.taskname, LiveTimeNoCuts );
        headas_chat(NORMAL,"%s: Info: Strict cut: %f (Loss: %f %)\n", global.taskname, LiveTimeStrictCuts, 100.0*(LiveTimeNoCuts-LiveTimeStrictCuts)/LiveTimeNoCuts);

        return OK;
    }


    /* Final stage: back track - make sure to only exclude regions where we actually see an increase in the final count rate */


    /* Allocate memory to storage all data */

    CleanRate = (double*)malloc(hknrows*sizeof(double));
    if(CleanRate==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }

    /*   FilteredRateCutOriginal = (double*)malloc(hknrows*sizeof(double)); */
    /*   if(FilteredRateCutOriginal==NULL){ */
    /*     headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname); */
    /*     goto FindSAAs_end; */
    /*   } */

    /*   FilteredRateCut = (double*)malloc(hknrows*sizeof(double)); */
    /*   if(FilteredRateCut==NULL){ */
    /*     headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname); */
    /*     goto FindSAAs_end; */
    /*   } */

    FilteredRateAverageLarge = (double*)malloc(hknrows*sizeof(double));
    if(FilteredRateAverageLarge==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }

    FilteredRateAverageSmall = (double*)malloc(hknrows*sizeof(double));
    if(FilteredRateAverageSmall==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAAs: memory allocation failure.\n", global.taskname);
        goto FindSAAs_end;
    }


    /* Create average again: */
    for ( i = 0; i < hknrows; i++) {
        Average = 0;
        NBins = 0;

        /* Determine the interval over which we average dynamically to take care of the edges */
        Min = (i-FilteredEventsHalfAverageLarge<0) ? 0 : i-FilteredEventsHalfAverageLarge ;
        Max = (i+FilteredEventsHalfAverageLarge>=hknrows) ? hknrows-1 : i+FilteredEventsHalfAverageLarge  ;

        for ( j=Min; j<=Max; j++) {
            /* We need to jump over bins where we don't have HK data */
            if (FilteredRate[j] > 0) {
                NBins++;
                Average += FilteredRate[j];
            }
        }

        Average /= NBins;
        FilteredRateAverageLarge[i] = Average;
    }


    for ( i = 0; i < hknrows; i++) {
        Average = 0;
        NBins = 0;

        /* Determine the interval over which we average dynamically to take care of the edges */
        Min = (i-FilteredEventsHalfAverageSmall<0) ? 0 : i-FilteredEventsHalfAverageSmall ;
        Max = (i+FilteredEventsHalfAverageSmall>=hknrows) ? hknrows-1 : i+FilteredEventsHalfAverageSmall ;

        for ( j=Min; j<=Max; j++) {
            /* We need to jump over bins where we don't have HK data */
            if (FilteredRate[j] > 0) {
                NBins++;
                Average += FilteredRate[j];
            }
        }

        Average /= NBins;
        FilteredRateAverageSmall[i] = Average;
    }


    /* Track from the left and right */

    Previous = 1;
    for ( i = 0; i < hknrows; i++) {

        /* Switch off -> on */
        if ( RejectionOnOff[i] == 0 && Previous == 1 ) {
            Previous = 0;
            Min = (i-FilteredEventsHalfAverageSmall<0) ? 0 : i-FilteredEventsHalfAverageSmall ;
            Dist = i-Min;

            while (FilteredRateAverageSmall[i] < FilteredDecisionThresholdLeft*FilteredRateAverageLarge[Min] &&
                    FilteredRate[i] < SingleRateDecisionThreshold*FilteredRateAverageLarge[Min] + global.saaparinfo.S_FltEvtSingleRateSigma *sqrt(FilteredRateAverageLarge[Min]) ) {

                for ( j=i-Dist; j<=i; j++)
                    RejectionOnOff[j] = 1;

                i++;
                if ( i >= hknrows ) break;
                if ( RejectionOnOff[i] == 1 ) break;
            }
        }
        /* Switch on -> off */
        else if ( RejectionOnOff[i] == 1 && Previous == 0 ) {
            Previous = 1;
        }
    }

    /* Track from right to left */
    Previous = 1;

    for ( i = hknrows-1; i >= 0 ; i--) {
        /* Switch off -> on */
        if (RejectionOnOff[i] == 0 && Previous == 1 ) {
            /* headas_chat(NORMAL,"%s: Info: Found new rejection on at TIME=%f\n", global.taskname, hkinfo[i].Time); */
            Previous = 0;
            Max = (i+FilteredEventsHalfAverageSmall>=hknrows) ? hknrows-1 : i+FilteredEventsHalfAverageSmall ;
            Dist = Max-i;

            while (FilteredRateAverageSmall[i] < FilteredDecisionThresholdRight*FilteredRateAverageLarge[Max] &&
                    FilteredRate[i] < SingleRateDecisionThreshold*FilteredRateAverageLarge[Max] + global.saaparinfo.S_FltEvtSingleRateSigma * sqrt(FilteredRateAverageLarge[Max]) ) {

                for ( j = i; j <= i+Dist; j++)
                    RejectionOnOff[j] = 1;

                i--;
                if (i < 0) break;
                if ( RejectionOnOff[i] == 1 ) break;
            }
        }
        /* Switch on -> off */
        else if ( RejectionOnOff[i] == 1 && Previous == 0 ) {
            Previous = 1;
        }
    }

    for ( i = 0; i < hknrows; i++) {
        if (RejectionOnOff[i] == 0 ) NAcceptedEventsShieldHighCut[i] = 0;
        if (RejectionOnOff[i] == 1 ) ShieldRateHighCut[i] = 0;
        if (RejectionOnOff[i] == 1 ) {
            CleanRate[i] = FilteredRate[i];
            /* FilteredRateCut[i] = 0; */
            if( IsGTI(gti, hkinfo[i].Time) )
                LiveTimeOptimizedCuts += LiveTime[i];
        }
    }

    for ( i = 0; i < hknrows; i++) {

        if (RejectionOnOff[i] == 0 )
            SoftSAA[i] = 1;
        else
            SoftSAA[i] = 0;
    }

    headas_chat(NORMAL,"%s: Info: Live times: \n", global.taskname );
    headas_chat(NORMAL,"%s: Info: No cut: %f\n", global.taskname, LiveTimeNoCuts );
    headas_chat(NORMAL,"%s: Info: Strict cut: %f (Loss: %f %)\n", global.taskname, LiveTimeStrictCuts, 100.0*(LiveTimeNoCuts-LiveTimeStrictCuts)/LiveTimeNoCuts);
    headas_chat(NORMAL,"%s: Info: Optimized cut: %f (Loss: %f %)\n", global.taskname, LiveTimeOptimizedCuts, 100.0*(LiveTimeNoCuts-LiveTimeOptimizedCuts)/LiveTimeNoCuts);


    return OK;

FindSAAs_end:

    return NOT_OK;

}  /* FindSAAs */


int FindSAATentacle(HKRow_t *hkinfo, int hknrows, double *FilteredRate, OrbitRow_t *orbitinfo, int orbitnrows, struct gti_struct *gti, BTYPE *SoftSAA, BTYPE *SoftTentacled) {

    /* For each orbit check for each module the count rate in the tentacle region -  */
    /* if there is an increase flag the region                                       */

    int                i, j, jmin, jmax, OIndex=0;
    double             *LiveTime, *CleanRate, *FilteredRateAverageLarge, *FilteredRateAverageSmall ;
    /*   double              *FilteredRateCut ; */
    BTYPE              *RejectionOnOff ;
    double             LiveTimeNoCuts=0, LiveTimeWithCuts=0;

    int                NBins=0, Min, Max;
    double             GlobalAverage = 0.0, Average=0;
    int                FilteredEventsHalfAverageLarge=global.saaparinfo.T_FltEvtHalfAverageLarge, FilteredEventsHalfAverageSmall=global.saaparinfo.T_FltEvtHalfAverageSmall;

    /* This is the distance between two data points on which we determine that we have a significant increase */
    int                DecisionDistance = global.saaparinfo.T_DecisionDistanceFactor * FilteredEventsHalfAverageLarge; /* 2 for 30 and 60 */
    /* This is the threshold of increase we require for starting to skip */
    /* There is a slight difference between left and right to account for delayed activation */
    double             DecisionThresholdLeft = global.saaparinfo.T_DecisionThrLeft ; /* good: 30+1.15, 60+1.35 */
    double             DecisionThresholdRight = global.saaparinfo.T_DecisionThrRightFactor * DecisionThresholdLeft; /* good: 30+1.15+0.95, 60+1.3+0.95 */

    double             EnteringValueLeft=0, EnteringValueRight=0;
    int                DecisionPointLeft, DecisionPointRight ;
    unsigned           Cut = 0;

    unsigned           Previous;
    double             LongitudeMin=global.saaparinfo.T_LongitudeMin, LongitudeMax=global.saaparinfo.T_LongitudeMax;
    double             AboveGlobalAverageThreshold=global.saaparinfo.T_AboveGlobalAverageThr, LocalAverage;

    int                CutOffCount = global.saaparinfo.T_CutOffCountFactor * FilteredEventsHalfAverageLarge; /* good: 2x */
    int                OffCount;


    /* Allocate memory to storage all data */

    LiveTime = (double*)malloc(hknrows*sizeof(double));
    if(LiveTime==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAATentacle: memory allocation failure.\n", global.taskname);
        goto FindSAATentacle_end;
    }

    CleanRate = (double*)malloc(hknrows*sizeof(double));
    if(CleanRate==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAATentacle: memory allocation failure.\n", global.taskname);
        goto FindSAATentacle_end;
    }

    /*   FilteredRateCut = (double*)malloc(hknrows*sizeof(double)); */
    /*   if(FilteredRateCut==NULL){ */
    /*     headas_chat(CHATTY,"%s: Error: FindSAATentacle: memory allocation failure.\n", global.taskname); */
    /*     goto FindSAATentacle_end; */
    /*   } */

    FilteredRateAverageLarge = (double*)malloc(hknrows*sizeof(double));
    if(FilteredRateAverageLarge==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAATentacle: memory allocation failure.\n", global.taskname);
        goto FindSAATentacle_end;
    }

    FilteredRateAverageSmall = (double*)malloc(hknrows*sizeof(double));
    if(FilteredRateAverageSmall==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAATentacle: memory allocation failure.\n", global.taskname);
        goto FindSAATentacle_end;
    }

    RejectionOnOff = (BTYPE*)malloc(hknrows*sizeof(BTYPE));
    if(RejectionOnOff==NULL) {
        headas_chat(CHATTY,"%s: Error: FindSAATentacle: memory allocation failure.\n", global.taskname);
        goto FindSAATentacle_end;
    }

    for(i=0; i<hknrows; i++) {

        LiveTime[i] = hkinfo[i].LiveTime ;
        RejectionOnOff[i] = 1 ;

        if( LiveTime[i]>0 && SoftSAA[i]==0 ) {
            /*       FilteredRateCut[i] = FilteredRate[i] ; */
        }
        else {
            FilteredRate[i] = 0 ;
            /*       FilteredRateCut[i] = FilteredRate[i] ;  */
        }

        if( IsGTI(gti, hkinfo[i].Time) )
            LiveTimeNoCuts += LiveTime[i];
    }


    /* Exclude occultation and slew time intervals */
    for(i=0; i<hknrows; i++) {

        FindClosestOrbitIndex(orbitinfo, orbitnrows, hkinfo[i].Time, &OIndex);

        if( orbitinfo[OIndex].occulted == 1 || orbitinfo[OIndex].slew == 1 )
            FilteredRate[i] = 0;
    }

    /* Apply livetime correction to event rates  */
    for(i=0; i<hknrows; i++) {
        FilteredRate[i] = (LiveTime[i]>0) ? FilteredRate[i]/LiveTime[i] : FilteredRate[i];
    }


    NBins = 0;
    GlobalAverage = 0.0;
    for(i=0; i<hknrows; i++) {

        if( FilteredRate[i]>0 ) {
            GlobalAverage += FilteredRate[i];
            NBins++;
        }

    }
    if (NBins > 0) GlobalAverage /= NBins;


    /* Create average again: */
    for ( i = 0; i < hknrows; i++) {
        Average = 0;
        NBins = 0;

        /* Determine the interval over which we average dynamically to take care of the edges */
        Min = (i-FilteredEventsHalfAverageLarge<0) ? 0 : i-FilteredEventsHalfAverageLarge ;
        Max = (i+FilteredEventsHalfAverageLarge>=hknrows) ? hknrows-1 : i+FilteredEventsHalfAverageLarge  ;

        for ( j=Min; j<=Max; j++) {
            /* We need to jump over bins where we don't have HK data */
            if (FilteredRate[j] > 0) {
                NBins++;
                Average += FilteredRate[j];
            }
        }

        Average /= NBins;
        FilteredRateAverageLarge[i] = Average;
    }


    for ( i = 0; i < hknrows; i++) {
        Average = 0;
        NBins = 0;

        /* Determine the interval over which we average dynamically to take care of the edges */
        Min = (i-FilteredEventsHalfAverageSmall<0) ? 0 : i-FilteredEventsHalfAverageSmall ;
        Max = (i+FilteredEventsHalfAverageSmall>=hknrows) ? hknrows-1 : i+FilteredEventsHalfAverageSmall ;

        for ( j=Min; j<=Max; j++) {
            /* We need to jump over bins where we don't have HK data */
            if (FilteredRate[j] > 0) {
                NBins++;
                Average += FilteredRate[j];
            }
        }

        Average /= NBins;
        FilteredRateAverageSmall[i] = Average;
    }


    /* We also store the shield-rate values when we started skipping from the left (lower time values) and the right (higher time values) */
    EnteringValueLeft = 0;
    EnteringValueRight = 0;
    for ( i = 0; i < hknrows; i++) {

        /* We always make a decision from the left and from the rights */

        /* Here we take care of the boarders of the distribution */
        DecisionPointLeft = i-DecisionDistance<0 ? 0 : i-DecisionDistance ;
        DecisionPointRight = i+DecisionDistance>=hknrows ? hknrows-1 : i+DecisionDistance ;


        /* We cut this bin if: */

        Cut = 0;

        /* ... we have an increase above our decision threshold from left to right */
        if ( FilteredRateAverageLarge[DecisionPointLeft] > 1 &&
                FilteredRateAverageLarge[i] > DecisionThresholdLeft*FilteredRateAverageLarge[DecisionPointLeft] ) {
            Cut = 1;
        }

        /* ... we have an increase above our decision threshold from right to left */
        if ( FilteredRateAverageLarge[DecisionPointRight] > 1 &&
                FilteredRateAverageLarge[i] > DecisionThresholdRight*FilteredRateAverageLarge[DecisionPointRight] ) {
            Cut = 1;
        }

        /* Next we take care of the case if the increase is no longer above the decision threshold, */
        /* but we are still above the initial increase value: */

        /* If we have both a left and a right value we cut if we are still above the average values */
        if (EnteringValueLeft > 0 && EnteringValueRight  > 0) {
            if ( FilteredRateAverageLarge[i] > 0.5*(EnteringValueLeft+EnteringValueRight) ) {
                Cut = 1;
            }
        }
        /* If we have just one value, we only cut on it */
        else {
            if ((FilteredRateAverageLarge[i] > EnteringValueLeft && EnteringValueLeft > 0) ||
                    (FilteredRateAverageLarge[i] > EnteringValueRight && EnteringValueRight  > 0)) {
                Cut = 1;
            }
        }


        if ( Cut == 1 ) {
            RejectionOnOff[i] = 0;
            /* Update the entering values */
            if ( FilteredRateAverageLarge[i] > DecisionThresholdLeft*FilteredRateAverageLarge[DecisionPointLeft] &&
                    (EnteringValueLeft == 0 || FilteredRateAverageLarge[i] < EnteringValueLeft) ) {
                EnteringValueLeft = FilteredRateAverageLarge[i];
            }
            if (FilteredRateAverageLarge[i] > DecisionThresholdRight*FilteredRateAverageLarge[DecisionPointRight] &&
                    (EnteringValueRight == 0 || FilteredRateAverageLarge[i] < EnteringValueRight)) {
                EnteringValueRight = FilteredRateAverageLarge[i];
            }
        }
        else {
            EnteringValueLeft = 0;
            EnteringValueRight = 0;
        }
    }


    /* Filters: */

    /* (A) If it start or ends outside the tantacle region filter out: */
    Previous = 1;
    for ( i = 0; i < hknrows; i++) {
        if ( RejectionOnOff[i] == 0 ) {

            /* Find the orbit position */
            FindClosestOrbitIndex(orbitinfo, orbitnrows, hkinfo[i].Time, &OIndex);
            if ( fabs( orbitinfo[OIndex].Time - hkinfo[i].Time ) > 1 ) {
                headas_chat(NORMAL, "%s: Info: Orbit is %f seconds from data\n",global.taskname, (orbitinfo[OIndex].Time-hkinfo[i].Time) );
                continue;
            }

            if ( orbitinfo[OIndex].Longitude < LongitudeMin || orbitinfo[OIndex].Longitude > LongitudeMax ) {
                RejectionOnOff[i] = 1;
            }

        }
    }

    /* (B) The average off must be at least X percent above average value: */
    Previous = 1;
    for ( i = 0; i < hknrows; i++) {
        /* Switch on -> off */
        if ( RejectionOnOff[i] == 0 && Previous == 1 ) {
            Previous = 0;
            LocalAverage = 0.0;
            NBins = 0;

            j=i;
            do {
                if (FilteredRate[j] > 0) {
                    LocalAverage += FilteredRate[j];
                    NBins++;
                }
                j++;
            } while ( j < hknrows && RejectionOnOff[j] == 0 );

            if (NBins > 0) LocalAverage /= NBins;
            if (LocalAverage < AboveGlobalAverageThreshold*GlobalAverage) {
                do {
                    RejectionOnOff[i] = 1;
                    i++;
                } while ( i < hknrows && RejectionOnOff[i] == 0 );
            }
        }
        /* Switch on -> off */
        else if ( RejectionOnOff[i] == 1 && Previous == 0 ) {
            Previous = 1;
        }
    }

    /* (C) Cutoffs require a certain length to be reasonable */
    OffCount = 0;
    for ( i = 0; i < hknrows; i++) {
        if (RejectionOnOff[i] == 1) {
            if (OffCount > 0 && OffCount < CutOffCount) {
                jmax = i-1<0 ? 0 : i-1 ;
                jmin = i-OffCount<0 ? 0 : i-OffCount ;
                for ( j = jmax; j >= jmin; j--) {
                    if ( RejectionOnOff[j] == 0 ) {
                        RejectionOnOff[j] = 1;
                    }
                    else {
                        headas_chat(NORMAL, "%s: Warning: On remove error...\n",global.taskname);
                    }
                }
            }
            OffCount = 0;
        }
        else {
            OffCount++;
        }
    }

    for ( i = 0; i < hknrows; i++) {
        if ( RejectionOnOff[i] == 1 ) {
            CleanRate[i] = FilteredRate[i];
            /*       FilteredRateCut[i] = 0; */
        }
    }

    for ( i = 0; i < hknrows; i++) {

        if (RejectionOnOff[i] == 0 )
            SoftTentacled[i] = 1;
        else
            SoftTentacled[i] = 0;
    }

    for ( i = 0; i < hknrows; i++) {
        if ( RejectionOnOff[i] == 1 && IsGTI(gti, hkinfo[i].Time ) ) {
            LiveTimeWithCuts += LiveTime[i];
        }
    }


    headas_chat(NORMAL,"%s: Info: Live times: \n", global.taskname );
    headas_chat(NORMAL,"%s: Info: No cut: %f\n", global.taskname, LiveTimeNoCuts );
    headas_chat(NORMAL,"%s: Info: tentacle cut: %f (Loss: %f %)\n", global.taskname, LiveTimeWithCuts, 100.0*(LiveTimeNoCuts-LiveTimeWithCuts)/LiveTimeNoCuts);


    return OK;

FindSAATentacle_end:

    return NOT_OK;

}  /*  FindSAATentacle */


void FindClosestOrbitIndex(OrbitRow_t *info, int nrows, double time, int *index) {

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
    if( (i>0) && (info[i].Time>time) ) {
        i--;
    }

    *index = i;

} /* FindClosestOrbitIndex */


/*
 *
 *      ComputeFilteredRate
 *
 *	DESCRIPTION:
 *           Routine to compute filtered rate
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int ComputeFilteredRate(char *filename, HKRow_t *hkinfo, int hknrows, double **rate) {
    unsigned           	FromRow, ReadRows, n, nCols, timecol;
    int                	status=OK, index=0;
    double             	time;
    Bintable_t	    	intable;
    FitsHeader_t	inhead;
    FitsFileUnit_t	inunit=NULL;

    TMEMSET0( &intable, Bintable_t );
    TMEMSET0( &inhead, FitsHeader_t );


    /* Allocate memory for the output array ( initialized to zero) */
    *rate = (double*)calloc(hknrows, sizeof(double));
    if(*rate==NULL) {
        headas_chat(CHATTY,"%s: Error: ComputeFilteredRate: memory allocation failure.\n", global.taskname);
        goto ComputeFilteredRate_end;
    }

    /* Open readonly input file */
    if ((inunit=OpenReadFitsFile(filename)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, filename);
        goto ComputeFilteredRate_end;
    }

    /* Move in EVENTS extension in input file */
    if (fits_movnam_hdu(inunit, ANY_HDU, KWVL_EXTNAME_EVT, 0, &status))
    {
        headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname, KWVL_EXTNAME_EVT);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
        if( CloseFitsFile(inunit))
        {
            headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
        }
        goto ComputeFilteredRate_end;
    }

    /* Retrieve header pointer */
    inhead=RetrieveFitsHeader(inunit);

    /* Read bintable */
    GetBintableStructure(&inhead, &intable, BINTAB_ROWS, 0, NULL);
    nCols=intable.nColumns;
    

    if(!intable.MaxRows)
    {
        headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
        goto ComputeFilteredRate_end;
    }
    

    /* Get needed columns number from name */

    if ((timecol=ColNameMatch(CLNM_TIME, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIME);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
        goto ComputeFilteredRate_end;
    }

    EndBintableHeader(&inhead, &intable);


    /* Read blocks of bintable rows */
    FromRow = 1;
    ReadRows=intable.nBlockRows;
    while(ReadBintable(inunit, &intable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
        for(n=0; n<ReadRows ; n++)
        {
            time = DVEC(intable,n,timecol);

            if( FindHkIndex(hkinfo, hknrows, time, &index) ) 
	    {  
                (*rate)[index]++;
            }
        }

        FromRow += ReadRows;
        ReadRows = BINTAB_ROWS;

    }/* while */


    return OK;

ComputeFilteredRate_end:
    if (inhead.first)
        ReleaseBintable(&inhead, &intable);

    return NOT_OK;

} /* ComputeFilteredRate */


/*
 *
 *      FindHkIndex
 *
 *	DESCRIPTION:
 *           Routine to search the index of the housekeeping row apporpriate for the input time value
 *           ( i.e. hktime <= time < hktime+1 )
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int FindHkIndex(HKRow_t *hkinfo, int hknrows, double time, int *index) {

    int        i, imin, imax, ioffset;
    double     binsize;

    ioffset = (int)hkinfo[0].Time ;
    binsize = 1 ;                    /* housekeeping bin time is 1 sec */
    imin = ioffset + 0 ;
    imax = ioffset + hknrows-1 ;

    i = (time<=imin*binsize) ? imin : ( time>imax*binsize ? imax : ((time-imin*binsize)/binsize +imin) );
    i -= ioffset ;


    while(  i>=0 &&  time < hkinfo[i].Time ) i--;
    i = i>=0 ? i : 0 ;

    while(  i<hknrows &&  time >= (hkinfo[i].Time + binsize) ) i++;
    i = i<hknrows ? i : hknrows-1 ;


    if( time >= hkinfo[i].Time && time < (hkinfo[i].Time + binsize) )
    {
        *index = i;
        return TRUE;
    }
    else
    {
        return FALSE;
    }

}  /* FindHkIndex */


int IsGTI(struct gti_struct  *gti, double time) {

    int     segs;
    int     status=OK;

    HDgti_where(gti, 1, &time, &segs, &status);
    if(status!=OK) {
        headas_chat(NORMAL, "%s: Warning: unable to check if time %f is included in GTI intervals\n", global.taskname, time);
        return 0 ;
    }

    if( segs<0 )
        return FALSE;
    else
        return TRUE;

}


int WriteOutFile(char *infile, char *outfile, int nrows, BTYPE *SoftSAA, BTYPE *SoftTentacled) {

    int                 status=OK, inExt, outExt, hkExt;
    FitsFileUnit_t      inunit=NULL, outunit=NULL;   /* Input and Output fits file pointer */

    /* Open readonly input file */
    if ((inunit=OpenReadFitsFile(infile)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, infile);
        goto WriteOutFile_end;
    }
    

    /* Move in HK1FPM extension in input file */
    if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_HK1FPM, 0, &status))
    {
        headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_HK1FPM);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, infile);
        if( CloseFitsFile(inunit))
        {
            headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, infile);
        }
        goto WriteOutFile_end;
    }

    
     
    /* Get HK1FPM ext number */
    if (!fits_get_hdu_num(inunit, &hkExt))
    {
        headas_chat(NORMAL,"%s: Error: Unable to find  '%s' extension\n",global.taskname, KWVL_EXTNAME_HK1FPM);
        headas_chat(NORMAL,"%s: Error: in '%s' file.\n",global.taskname, infile);
        goto WriteOutFile_end;
    }

    /* Create output file */
    if ((outunit = OpenWriteFitsFile(outfile)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL, "%s: Error: Unable to create\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' temporary file.\n", global.taskname, outfile);
        goto WriteOutFile_end;
    }

    /* Get number of hdus in input file */
    if (fits_get_num_hdus(inunit, &inExt, &status))
    {
        headas_chat(CHATTY, "%s: Error: Unable to get the total number of HDUs in\n", global.taskname);
        headas_chat(CHATTY, "%s: Error: '%s' file.\n", global.taskname, infile);
        goto WriteOutFile_end;
    }

    /* Copy all extension before HK1FPM extension from input to output file */
    outExt=1;

    while(outExt<hkExt && status == OK)
    {
        if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
        {
            headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
            headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, infile);
            goto WriteOutFile_end;
        }
        if(fits_copy_hdu( inunit, outunit, 0, &status ))
        {
            headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
            headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, infile);
            headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, outfile);
            goto WriteOutFile_end;
        }
        outExt++;
    }

    /* make sure get specified header by using absolute location */
    if(fits_movabs_hdu( inunit, hkExt, NULL, &status ))
    {
        headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,hkExt);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, infile);
        goto WriteOutFile_end;
    }

    /* Update HK1FPM extension */
    if (UpdateHK1FPMExt(inunit, outunit, nrows, SoftSAA, SoftTentacled))
    {
        headas_chat(NORMAL, "%s: Error: Unable to update HK1FPM extension in '%s' file.\n", global.taskname, outfile);
        goto WriteOutFile_end;
    }
    outExt++;


    /* copy any extension after the extension to be operated on */
    while ( status == OK && outExt <= inExt)
    {
        if(fits_movabs_hdu( inunit, outExt, NULL, &status ))
        {
            headas_chat(NORMAL, "%s: Error: Unable to move in %d HDU\n", global.taskname,outExt);
            headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, infile);
            goto WriteOutFile_end;
        }
        if(fits_copy_hdu ( inunit, outunit, 0, &status ))
        {
            headas_chat(CHATTY,"%s: Error: Unable to copy %d HDU\n",global.taskname,outExt);
            headas_chat(CHATTY,"%s: Error: from %s input file\n", global.taskname, infile);
            headas_chat(CHATTY,"%s: Error: to %s temporary output file.\n",global.taskname, outfile);
            goto WriteOutFile_end;
        }
        outExt++;
    }


    /* Add history if parameter history set */
    if(HDpar_stamp(outunit, hkExt, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to add HISTORY keywords.\n", global.taskname);
        goto WriteOutFile_end;
    }


    /* Update checksum and datasum keywords in all extensions */
    if (ChecksumCalc(outunit))
    {
        headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, outfile);
        goto WriteOutFile_end;
    }

    /* close input and output files */
    if (CloseFitsFile(inunit))
    {
        headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, infile);
        goto WriteOutFile_end;
    }
    if (CloseFitsFile(outunit))
    {
        headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, outfile);
        goto WriteOutFile_end;
    }


    return OK;

WriteOutFile_end:

    return NOT_OK;

}


int UpdateHK1FPMExt(FitsFileUnit_t inunit, FitsFileUnit_t ounit, int nrows, BTYPE *SoftSAA, BTYPE *SoftTentacled) {

    unsigned           FromRow, ReadRows, n,nCols, OutRows=0;
    int                newsaa, newtentacle, count=0;
    char               comm[256];
    unsigned           sw_saa, sw_tentacle;
    Bintable_t	     outtable;
    FitsHeader_t	     head;


    TMEMSET0( &outtable, Bintable_t );
    TMEMSET0( &head, FitsHeader_t );

    head=RetrieveFitsHeader(inunit);

    GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);

    if(!outtable.MaxRows)
    {
        headas_chat(NORMAL, "%s: Warning: Housekeeping file is empty.\n", global.taskname);
        return OK;
    }

    nCols=outtable.nColumns;


    /* SW_SAA (added if needed) */
    newsaa=0;
    if((sw_saa=ColNameMatch(CLNM_SW_SAA, &outtable)) == -1)
    {
        newsaa=1;
        AddColumn(&head, &outtable,CLNM_SW_SAA,CARD_COMM_SW_SAA , "1B",TNONE);
        headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_SW_SAA);
        headas_chat(NORMAL, "%s: Info: in output file.\n", global.taskname);
        sw_saa=ColNameMatch(CLNM_SW_SAA, &outtable);
    }

    /* SW_TENTACLE (added if needed) */
    newtentacle=0;
    if((sw_tentacle=ColNameMatch(CLNM_SW_TENTACLE, &outtable)) == -1)
    {
        newtentacle=1;
        AddColumn(&head, &outtable,CLNM_SW_TENTACLE,CARD_COMM_SW_TENTACLE , "1B",TNONE);
        headas_chat(NORMAL, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_SW_TENTACLE);
        headas_chat(NORMAL, "%s: Info: in output file.\n", global.taskname);
        sw_tentacle=ColNameMatch(CLNM_SW_TENTACLE, &outtable);
    }
    
    
    /* Add history */
    GetGMTDateTime(global.date);
    if(global.hist)
    {
        sprintf(global.strhist, "File modified by '%s' (%s) at %s: computed SW_SAA and SW_TENTACLE.", global.taskname, global.nustardas_v,global.date );
        AddHistory(&head, global.strhist);

        if(newsaa)
        {
            sprintf(comm, "Added %s column", CLNM_SW_SAA);
            AddHistory(&head, comm);
        }

        if(newtentacle)
        {
            sprintf(comm, "Added %s column", CLNM_SW_TENTACLE);
            AddHistory(&head, comm);
        }
    }

    EndBintableHeader(&head, &outtable);

    /* write out new header to new file */
    FinishBintableHeader(ounit, &head, &outtable);



    FromRow = 1;
    ReadRows=outtable.nBlockRows;
    OutRows = 0;

    /* Read input bintable */
    while( ReadBintable(inunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
        for(n=0; n<ReadRows; ++n)
        {

            if(count>nrows) {
                headas_chat(CHATTY, "%s: Error: input rows (%d) not match computed SAA values (%d)\n", global.taskname, outtable.MaxRows, count);
                goto UpdateHK1FPMExt_end;
            }

            BVEC(outtable,n,sw_saa) = SoftSAA[count];
            BVEC(outtable,n,sw_tentacle) = SoftTentacled[count];

            count++;


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

UpdateHK1FPMExt_end:
    if (head.first)
        ReleaseBintable(&head, &outtable);

    return NOT_OK;

}


int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo) {

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


int ReadSAAParInfo(char *filename, SAAParInfo_t *saaparinfo) {

    unsigned           FromRow, ReadRows, nCols;
    int                status=OK;
    SAAParCol_t        col;
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
        goto ReadSAAParInfo_end;
    }


    { /* SAA Extension */
	/* Move in SAA extension in input file */
	if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_SAA, 0, &status))
	{
	    headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_SAA);
	    headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
	    if( CloseFitsFile(unit))
	    {
		headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
		headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	    }
	    goto ReadSAAParInfo_end;
	}
    
	head=RetrieveFitsHeader(unit);

	GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
	if(!table.MaxRows)
	{
	    headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	/* Get needed columns number from name */

	if ((col.S_HalfAverageLength=ColNameMatch(CLNM_S_HalfAverageLength, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_HalfAverageLength);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_DecisionDistanceFactor=ColNameMatch(CLNM_S_DecisionDistanceFactor, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_DecisionDistanceFactor);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_DecisionThrLeft=ColNameMatch(CLNM_S_DecisionThrLeft, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_DecisionThrLeft);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_DecisionThrRightFactor=ColNameMatch(CLNM_S_DecisionThrRightFactor, &table)) == -1)
	{
	    headas_chat(NORMAL,"DEBUG 1\n");
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_DecisionThrRightFactor);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_SAARampingTime=ColNameMatch(CLNM_S_SAARampingTime, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_SAARampingTime);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_MinLong=ColNameMatch(CLNM_S_MinLong, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_MinLong);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_MaxLong=ColNameMatch(CLNM_S_MaxLong, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_MaxLong);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_CutOffCountFactor=ColNameMatch(CLNM_S_CutOffCountFactor, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_CutOffCountFactor);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_FltEvtHalfAverageLarge=ColNameMatch(CLNM_S_FltEvtHalfAverageLarge, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_FltEvtHalfAverageLarge);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_FltEvtHalfAverageSmall=ColNameMatch(CLNM_S_FltEvtHalfAverageSmall, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_FltEvtHalfAverageSmall);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_FltDecisionThrLeft=ColNameMatch(CLNM_S_FltDecisionThrLeft, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_FltDecisionThrLeft);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_FltDecisionThrRight=ColNameMatch(CLNM_S_FltDecisionThrRight, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_FltDecisionThrRight);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_SingleRateDecisionThr=ColNameMatch(CLNM_S_SingleRateDecisionThr, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_SingleRateDecisionThr);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.S_FltEvtSingleRateSigma=ColNameMatch(CLNM_S_FltEvtSingleRateSigma, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_S_FltEvtSingleRateSigma);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
      
	EndBintableHeader(&head, &table);


	/* Read Bintable */
	FromRow = 1;
	ReadRows=table.nBlockRows;
	nCols=table.nColumns;

	/* NOTE: read only one row! */
	if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {

	    /* Value of SAA Ext*/
	    saaparinfo->S_DecisionThrLeft = DVEC(table,0,col.S_DecisionThrLeft);
	    saaparinfo->S_DecisionThrRightFactor = DVEC(table,0,col.S_DecisionThrRightFactor);
	    saaparinfo->S_MinLong = DVEC(table,0,col.S_MinLong);
	    saaparinfo->S_MaxLong = DVEC(table,0,col.S_MaxLong);
	    saaparinfo->S_CutOffCountFactor = DVEC(table,0,col.S_CutOffCountFactor);
	    saaparinfo->S_FltDecisionThrLeft = DVEC(table,0,col.S_FltDecisionThrLeft);
	    saaparinfo->S_FltDecisionThrRight = DVEC(table,0,col.S_FltDecisionThrRight);
	    saaparinfo->S_SingleRateDecisionThr = DVEC(table,0,col.S_SingleRateDecisionThr);
	    saaparinfo->S_FltEvtSingleRateSigma = DVEC(table,0,col.S_FltEvtSingleRateSigma);

	    /* Value of TENTACLE Ext*/
	    saaparinfo->S_HalfAverageLength = JVEC(table,0,col.S_HalfAverageLength);
	    saaparinfo->S_DecisionDistanceFactor = JVEC(table,0,col.S_DecisionDistanceFactor);
	    saaparinfo->S_SAARampingTime = JVEC(table,0,col.S_SAARampingTime);
	    saaparinfo->S_FltEvtHalfAverageLarge = JVEC(table,0,col.S_FltEvtHalfAverageLarge);
	    saaparinfo->S_FltEvtHalfAverageSmall = JVEC(table,0,col.S_FltEvtHalfAverageSmall);
	    
	}

	/* Free memory allocated with bintable data */
	ReleaseBintable(&head, &table);
    } /* End SAA Extension */
    
    { /* TENTACLE Extension */
	/* Move in TENTACLE extension in input file */
	if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_TENTACLE, 0, &status))
	{
	    headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_TENTACLE);
	    headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
	    if( CloseFitsFile(unit))
	    {
		headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
		headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	    }
	    goto ReadSAAParInfo_end;
	}

	head=RetrieveFitsHeader(unit);

	GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
	if(!table.MaxRows)
	{
	    headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	/* Get needed columns number from name */

	if ((col.T_FltEvtHalfAverageLarge=ColNameMatch(CLNM_T_FltEvtHalfAverageLarge, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_FltEvtHalfAverageLarge);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.T_FltEvtHalfAverageSmall=ColNameMatch(CLNM_T_FltEvtHalfAverageSmall, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_FltEvtHalfAverageSmall);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.T_DecisionDistanceFactor=ColNameMatch(CLNM_T_DecisionDistanceFactor, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_DecisionDistanceFactor);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.T_DecisionThrLeft=ColNameMatch(CLNM_T_DecisionThrLeft, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_DecisionThrLeft);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.T_DecisionThrRightFactor=ColNameMatch(CLNM_T_DecisionThrRightFactor, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_DecisionThrRightFactor);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.T_LongitudeMin=ColNameMatch(CLNM_T_LongitudeMin, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_LongitudeMin);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.T_LongitudeMax=ColNameMatch(CLNM_T_LongitudeMax, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_LongitudeMax);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.T_AboveGlobalAverageThr=ColNameMatch(CLNM_T_AboveGlobalAverageThr, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_AboveGlobalAverageThr);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.T_CutOffCountFactor=ColNameMatch(CLNM_T_CutOffCountFactor, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_T_CutOffCountFactor);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	EndBintableHeader(&head, &table);

	/* Read Bintable */
	FromRow = 1;
	ReadRows=table.nBlockRows;
	nCols=table.nColumns;

	if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {

	    saaparinfo->T_DecisionThrLeft = DVEC(table,0,col.T_DecisionThrLeft);
	    saaparinfo->T_DecisionThrRightFactor = DVEC(table,0,col.T_DecisionThrRightFactor);
	    saaparinfo->T_LongitudeMin = DVEC(table,0,col.T_LongitudeMin);
	    saaparinfo->T_LongitudeMax = DVEC(table,0,col.T_LongitudeMax);
	    saaparinfo->T_AboveGlobalAverageThr = DVEC(table,0,col.T_AboveGlobalAverageThr);

	    saaparinfo->T_FltEvtHalfAverageLarge = JVEC(table,0,col.T_FltEvtHalfAverageLarge);
	    saaparinfo->T_FltEvtHalfAverageSmall = JVEC(table,0,col.T_FltEvtHalfAverageSmall);
	    saaparinfo->T_DecisionDistanceFactor = JVEC(table,0,col.T_DecisionDistanceFactor);
	    saaparinfo->T_CutOffCountFactor = JVEC(table,0,col.T_CutOffCountFactor);

	}

	/* Free memory allocated with bintable data */
	ReleaseBintable(&head, &table);
    
    } /* End TENTACLE Extension */
    
    { /* SAANEW Extension */
      headas_chat(NORMAL,"%s: Info: SAANEW Extension '%s'\n", global.taskname,KWVL_EXTNAME_SAANEW);
	/* Move in SAANEW extension in input file */
	if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_SAANEW, 0, &status))
	{
	    headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_SAANEW);
	    headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
	    if( CloseFitsFile(unit))
	    {
		headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
		headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	    }
	    goto ReadSAAParInfo_end;
	}

	head=RetrieveFitsHeader(unit);

	GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
	if(!table.MaxRows)
	{
	    headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	/* Get index column of SAANEW Ext */
	
	if ((col.SN_SuperStrictOffTimeInterval=ColNameMatch(CLNM_SN_SuperStrictOffTimeInterval, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_SuperStrictOffTimeInterval);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.SN_Prior=ColNameMatch(CLNM_SN_Prior, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_Prior);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.SN_SmoothSHLDLO=ColNameMatch(CLNM_SN_SmoothSHLDLO, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_SmoothSHLDLO);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}    
	
	if ((col.SN_SmoothSOURCE=ColNameMatch(CLNM_SN_SmoothSOURCE, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_SmoothSOURCE);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}    

	if ((col.SN_MinAllowedVolatility=ColNameMatch(CLNM_SN_MinAllowedVolatility, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_MinAllowedVolatility);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}    

	if ((col.SN_MinMaximum=ColNameMatch(CLNM_SN_MinMaximum, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_MinMaximum);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}    

	if ((col.SN_MaxMinimum=ColNameMatch(CLNM_SN_MaxMinimum, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_MaxMinimum);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}   

	if ((col.SN_MinVariationLeft=ColNameMatch(CLNM_SN_MinVariationLeft, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_MinVariationLeft);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}      

	if ((col.SN_MinVariationRight=ColNameMatch(CLNM_SN_MinVariationRight, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_MinVariationRight);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}    

	if ((col.SN_NonSAAMinLongOpt=ColNameMatch(CLNM_SN_NonSAAMinLongOpt, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_NonSAAMinLongOpt);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}                     

	if ((col.SN_NonSAAMaxLongOpt=ColNameMatch(CLNM_SN_NonSAAMaxLongOpt, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_NonSAAMaxLongOpt);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.SN_SpectrumMin=ColNameMatch(CLNM_SN_SpectrumMin, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_SpectrumMin);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	if ((col.SN_SpectrumMax=ColNameMatch(CLNM_SN_SpectrumMax, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_SpectrumMax);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	} 
	
	if ((col.SN_TimeOneSigma=ColNameMatch(CLNM_SN_TimeOneSigma, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_TimeOneSigma);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	} 
	
	if ((col.SN_MaxSearchDistance=ColNameMatch(CLNM_SN_MaxSearchDistance, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_MaxSearchDistance);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}     
	
	if ((col.SN_HintOfVariabilityCutOff=ColNameMatch(CLNM_SN_HintOfVariabilityCutOff, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_HintOfVariabilityCutOff);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}   

	if ((col.SN_LogBinningBins=ColNameMatch(CLNM_SN_LogBinningBins, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_LogBinningBins);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.SN_PercentageEliminatedPixels=ColNameMatch(CLNM_SN_PercentageEliminatedPixels, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_PercentageEliminatedPixels);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}   

	if ((col.SN_SuspiciousnessThreshold=ColNameMatch(CLNM_SN_SuspiciousnessThreshold, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SN_SuspiciousnessThreshold);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	/* Read Bintable */
	FromRow = 1;
	ReadRows=table.nBlockRows;
	nCols=table.nColumns;

	if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {

	    /* Value of SAANEW Ext*/
	    saaparinfo->SN_SuperStrictOffTimeInterval = JVEC(table,0,col.SN_SuperStrictOffTimeInterval);
	    saaparinfo->SN_Prior                      = JVEC(table,0,col.SN_Prior);
	    saaparinfo->SN_SmoothSHLDLO               = JVEC(table,0,col.SN_SmoothSHLDLO);
	    saaparinfo->SN_SmoothSOURCE               = JVEC(table,0,col.SN_SmoothSOURCE);
	    saaparinfo->SN_MinAllowedVolatility       = DVEC(table,0,col.SN_MinAllowedVolatility);
	    saaparinfo->SN_MinMaximum                 = DVEC(table,0,col.SN_MinMaximum);
	    saaparinfo->SN_MaxMinimum                 = DVEC(table,0,col.SN_MaxMinimum);
	    saaparinfo->SN_MinVariationLeft           = DVEC(table,0,col.SN_MinVariationLeft);
	    saaparinfo->SN_MinVariationRight          = DVEC(table,0,col.SN_MinVariationRight);
	    saaparinfo->SN_NonSAAMinLongOpt           = DVEC(table,0,col.SN_NonSAAMinLongOpt);
	    saaparinfo->SN_NonSAAMaxLongOpt           = DVEC(table,0,col.SN_NonSAAMaxLongOpt);
	    saaparinfo->SN_SpectrumMin                = DVEC(table,0,col.SN_SpectrumMin);
	    saaparinfo->SN_SpectrumMax                = DVEC(table,0,col.SN_SpectrumMax);
	    saaparinfo->SN_TimeOneSigma               = DVEC(table,0,col.SN_TimeOneSigma);
	    saaparinfo->SN_MaxSearchDistance          = JVEC(table,0,col.SN_MaxSearchDistance);
	    saaparinfo->SN_HintOfVariabilityCutOff    = DVEC(table,0,col.SN_HintOfVariabilityCutOff);
	    saaparinfo->SN_LogBinningBins             = JVEC(table,0,col.SN_LogBinningBins);
	    saaparinfo->SN_PercentageEliminatedPixels = DVEC(table,0,col.SN_PercentageEliminatedPixels);
	    saaparinfo->SN_SuspiciousnessThreshold    = JVEC(table,0,col.SN_SuspiciousnessThreshold);

	}

	/* Free memory allocated with bintable data */
	ReleaseBintable(&head, &table);

    } /* End SAANEW Extension */
    
    { /* TENTACLENEW Extension */
	/* Move in SAANEW extension in input file */
	if (fits_movnam_hdu(unit, ANY_HDU, KWVL_EXTNAME_TENTACLENEW, 0, &status))
	{
	    headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_TENTACLE);
	    headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
	    if( CloseFitsFile(unit))
	    {
		headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
		headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, filename);
	    }
	    goto ReadSAAParInfo_end;
	}

	head=RetrieveFitsHeader(unit);

	GetBintableStructure(&head, &table, BINTAB_ROWS, 0, NULL);
	if(!table.MaxRows)
	{
	    headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}

	/* Get index column of TENTACLENEW Ext */
	if ((col.TN_TentacleTimeInterval=ColNameMatch(CLNM_TN_TentacleTimeInterval, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleTimeInterval);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleNonSAAMinLong=ColNameMatch(CLNM_TN_TentacleNonSAAMinLong, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleNonSAAMinLong);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleNonSAAMaxLong=ColNameMatch(CLNM_TN_TentacleNonSAAMaxLong, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleNonSAAMaxLong);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleLongitudeMin=ColNameMatch(CLNM_TN_TentacleLongitudeMin, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleLongitudeMin);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleLongitudeMax=ColNameMatch(CLNM_TN_TentacleLongitudeMax, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleLongitudeMax);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleMinLatitude=ColNameMatch(CLNM_TN_TentacleMinLatitude, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleMinLatitude);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleAvgLongitude=ColNameMatch(CLNM_TN_TentacleAvgLongitude, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleAvgLongitude);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	    
	    if ((col.TN_TentacleVsRegionCrossingsBadCutoff=ColNameMatch(CLNM_TN_TentacleVsRegionCrossingsBadCutoff, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleVsRegionCrossingsBadCutoff);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleVsRegionCrossingsReallyBadCutoff=ColNameMatch(CLNM_TN_TentacleVsRegionCrossingsReallyBadCutoff, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleVsRegionCrossingsReallyBadCutoff);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleDurationCutOff=ColNameMatch(CLNM_TN_TentacleDurationCutOff, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleDurationCutOff);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleAdjacentOrNotCutOff=ColNameMatch(CLNM_TN_TentacleAdjacentOrNotCutOff, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleAdjacentOrNotCutOff);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleLongDuration=ColNameMatch(CLNM_TN_TentacleLongDuration, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleLongDuration);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleLongThreshold=ColNameMatch(CLNM_TN_TentacleLongThreshold, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleLongThreshold);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleFluxThreshold=ColNameMatch(CLNM_TN_TentacleFluxThreshold, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleFluxThreshold);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	if ((col.TN_TentacleSuspiciousnessThreshold=ColNameMatch(CLNM_TN_TentacleSuspiciousnessThreshold, &table)) == -1)
	{
	    headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TN_TentacleSuspiciousnessThreshold);
	    headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, filename);
	    goto ReadSAAParInfo_end;
	}
	
	EndBintableHeader(&head, &table);

	/* Read Bintable */
	FromRow = 1;
	ReadRows=table.nBlockRows;
	nCols=table.nColumns;

	if(ReadBintable(unit, &table, nCols, NULL, FromRow, &ReadRows) == 0 ) {

	    /* Value of TENTACLENEW Ext*/
	    saaparinfo->TN_TentacleTimeInterval                     = JVEC(table,0,col.TN_TentacleTimeInterval);
	    saaparinfo->TN_TentacleNonSAAMinLong                    = DVEC(table,0,col.TN_TentacleNonSAAMinLong);
	    saaparinfo->TN_TentacleNonSAAMaxLong                    = DVEC(table,0,col.TN_TentacleNonSAAMaxLong);
	    saaparinfo->TN_TentacleLongitudeMin                     = DVEC(table,0,col.TN_TentacleLongitudeMin);
	    saaparinfo->TN_TentacleLongitudeMax                     = DVEC(table,0,col.TN_TentacleLongitudeMax);
	    saaparinfo->TN_TentacleMinLatitude                      = DVEC(table,0,col.TN_TentacleMinLatitude);
	    saaparinfo->TN_TentacleAvgLongitude                     = DVEC(table,0,col.TN_TentacleAvgLongitude);
	    saaparinfo->TN_TentacleVsRegionCrossingsBadCutoff       = DVEC(table,0,col.TN_TentacleVsRegionCrossingsBadCutoff);
	    saaparinfo->TN_TentacleVsRegionCrossingsReallyBadCutoff = DVEC(table,0,col.TN_TentacleVsRegionCrossingsReallyBadCutoff);
	    saaparinfo->TN_TentacleDurationCutOff                   = DVEC(table,0,col.TN_TentacleDurationCutOff);
	    saaparinfo->TN_TentacleAdjacentOrNotCutOff              = DVEC(table,0,col.TN_TentacleAdjacentOrNotCutOff);
	    saaparinfo->TN_TentacleLongDuration                     = DVEC(table,0,col.TN_TentacleLongDuration);
	    saaparinfo->TN_TentacleLongThreshold                    = DVEC(table,0,col.TN_TentacleLongThreshold);
	    saaparinfo->TN_TentacleFluxThreshold                    = DVEC(table,0,col.TN_TentacleFluxThreshold);
	    saaparinfo->TN_TentacleSuspiciousnessThreshold          = JVEC(table,0,col.TN_TentacleSuspiciousnessThreshold);
	}

	/* Free memory allocated with bintable data */
	ReleaseBintable(&head, &table);
	
    } /* End TENTACLENEW Extension */
    
    /* Close file */
    if (CloseFitsFile(unit))
    {
        headas_chat(NORMAL,"%s: Error: Unable to close\n",global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, filename);
        goto ReadSAAParInfo_end;
    }


    return OK;

ReadSAAParInfo_end:
    return NOT_OK;

} /* ReadSAAParInfo */


void Mycopy(int vet_size,double *vet_sorg,double *vet_dest)
{
	int i;
	for(i=0;i<vet_size;i++)
	{
	 	vet_dest[i]=vet_sorg[i];
	}
}/* Mycopy */

double MyMedian(int n,double *valori)
{
	int i = 0,j;
	double val, mediana = -1.0;
	int minori, maggiori;
	
	while (i < n && mediana == -1.0) 
	{
	    minori = 0;
	    maggiori = 0;
	    val = valori[i];
	    for (j = 0 ; j < n ; j++) 
	    {
		if (valori[j] <= val) 
		{
		    minori++;
		}
		if (valori[j] >= val)
		{
		    maggiori++;
		}
	    }
	    if (minori >= (n+1)/2 && maggiori >= (n+1)/2)
	    {
		mediana = val;
	    }
	    i++;
	}
	return mediana;
}/* MyMedian */

double MyMinElement(int vet_size,double *vet_sorg)
{
	double min;
	int i;
	
	min=vet_sorg[0];
	
	for(i=1;i<vet_size;i++)
	{
	 	if(vet_sorg[i]<min)
	 	{
	 		min=vet_sorg[i];
	 	}
	}
	
	return min;
} /* MyMinElement */

double MyMax(double a, double b)
{ 
	return a >= b ? a : b; 
} /* MyMax */

void  MySmoothArray(int nn, double *xx, int ntimes)
{

	int jj, jk, ii, pass, noent, kk, medianType, ifirst, ilast;
	double hh[6] = {0,0,0,0,0,0};
	double xmin;
	double *yy = NULL,*zz = NULL,*rr = NULL;
	
	if (nn < 3 ) 
	{
		headas_chat(NORMAL,"%s: Error: SmoothArray->Need at least 3 points for smoothing: n = %d\n",global.taskname,nn);
		return;
	}
	
	yy = (double*)malloc(nn*sizeof(double));
	zz = (double*)malloc(nn*sizeof(double));
	rr = (double*)malloc(nn*sizeof(double));
	
	for (pass=0;pass<ntimes;pass++) 
	{
		/* first copy original data into temp array */
		Mycopy(nn, xx, zz );
      	
		for (noent = 0; noent < 2; ++noent) 
		{ /* run algorithm two times */

			/*  do 353 i.e. running median 3, 5, and 3 in a single loop */
			for  (kk = 0; kk < 3; kk++)  
			{
				Mycopy(nn, zz, yy);
				medianType	= (kk != 1)  ?  3 : 5;
				ifirst		= (kk != 1 ) ?  1 : 2;
				ilast		= (kk != 1 ) ? nn-1 : nn -2;
		        	
				/* nn2 = nn - ik - 1;
				   do all elements beside the first and last point for median 3
				   and first two and last 2 for median 5 */
				for  ( ii = ifirst; ii < ilast; ii++)  
				{
					/* assert(ii - ifirst >= 0); */
					if (ii - ifirst < 0) break;
					
					for  (jj = 0; jj < medianType; jj++)   
					{
						hh[jj] = yy[ii - ifirst + jj ];
					}
					zz[ii] = MyMedian(medianType, hh);
				}
				
				if  (kk == 0)  
				{   /* first median 3 */
					/* first point */
					hh[0] = zz[1];
					hh[1] = zz[0];
					hh[2] = 3*zz[1] - 2*zz[2];
					zz[0] = MyMedian(3, hh);
					/* last point */
					hh[0] = zz[nn - 2];
					hh[1] = zz[nn - 1];
					hh[2] = 3*zz[nn - 2] - 2*zz[nn - 3];
					zz[nn - 1] = MyMedian(3, hh);
				}
            	
				if  (kk == 1)  
				{   /*  median 5 */
					for  (ii = 0; ii < 3; ii++) 
					{
						hh[ii] = yy[ii];
					}
					zz[1] = MyMedian(3, hh);
					/* last two points */
					for  (ii = 0; ii < 3; ii++) 
					{
						hh[ii] = yy[nn - 3 + ii];
					}
					zz[nn - 2] = MyMedian(3, hh);
				}
			}
			
			Mycopy ( nn, zz, yy );
			
			/* quadratic interpolation for flat segments */
			for (ii = 2; ii < (nn - 2); ii++)
			{
				if  (zz[ii - 1] != zz[ii]) 
				{
					continue;
				}
				if  (zz[ii] != zz[ii + 1])
				{
					continue;
				}
				hh[0] = zz[ii - 2] - zz[ii];
				hh[1] = zz[ii + 2] - zz[ii];
				if  (hh[0] * hh[1] <= 0)
				{
					continue;
				}
				jk = 1;
				if  ( fabs(hh[1]) > fabs(hh[0]) )
				{
					jk = -1;
				}
				yy[ii] = -0.5*zz[ii - 2*jk] + zz[ii]/0.75 + zz[ii + 2*jk] /6.;
				yy[ii + jk] = 0.5*(zz[ii + 2*jk] - zz[ii - 2*jk]) + zz[ii];
			}
			
			/* running means */
			for  (ii = 1; ii < nn - 1; ii++) 
			{
				zz[ii] = 0.25*yy[ii - 1] + 0.5*yy[ii] + 0.25*yy[ii + 1];
			}
			zz[0] = yy[0];
			zz[nn - 1] = yy[nn - 1];
			
			if (noent == 0) 
			{
				/* save computed values */
				Mycopy(nn, zz, rr);
		    
				/* COMPUTE  residuals */
				for  (ii = 0; ii < nn; ii++)
				{
				    zz[ii] = xx[ii] - zz[ii];
				}
			}
		} /* end loop on noent */
		
		xmin = MyMinElement(nn,xx);
		for  (ii = 0; ii < nn; ii++) 
		{
			if (xmin < 0) 
			{
				xx[ii] = rr[ii] + zz[ii];
			}
			/* make smoothing defined positive - not better using 0 ? */
			else
			{
				xx[ii] = MyMax((rr[ii] + zz[ii]),0.0 );
			}
		}
	}
	
	if(yy!=NULL) free(yy);
	if(zz!=NULL) free(zz);
	if(rr!=NULL) free(rr);
} /* MySmoothArray */


void MySort(int *vet,int vet_size)
{
    int i,j,swap;
    for(i = 0; i < vet_size; ++i)
    {
        for (j = i + 1; j < vet_size; ++j)
        {
            if (vet[i] > vet[j])
            {

                swap =  vet[i];
                vet[i] = vet[j];
                vet[j] = swap;
            }
        }
    }
} /* MySort */

int MyLowerBound(int *vet,int vet_size,int lb)
{
    int i=0,lower_bound=LOWER_BOUND_NOT_FOUND;
    
    for(i=0;i<vet_size;i++)
    {
	    if(lb<=vet[i])
	    {
		lower_bound = vet[i];
		break;
	    }
    }
    return lower_bound;
} /* MyLowerBound */

void ShowCCD(int PositionsOnSource[N_POSITION_BIN][N_POSITION_BIN])
{
    int by,bx;
    
    for (by = 0; by < N_POSITION_BIN; ++by) 
    {
	for (bx = 0; bx < N_POSITION_BIN; ++bx)    
	{
	    if(by!=32&&bx!=32) 
	    {
		headas_chat(NORMAL,"%02d ", PositionsOnSource[bx][by]);
	    }
	}
	headas_chat(NORMAL,"\n");
    }
} /* MyLowerBound */

int MostlyEliminateSource(EVTRow_t *evtinfo, int evtnrows,OrbitRow_t *orbitinfo, int orbitnrows,struct gti_struct *gti,int **ExcludedDetRawXY, int *nEliminateSource,BOOL Show)
{
    int PositionsOnSource[N_POSITION_BIN][N_POSITION_BIN];
    unsigned int e;
    int i;
    int DetectorID,RawX,RawY;
    int PosX = 0;
    int PosY = 0;
    int bx,by;
    int cont=0;
    int DXY;
    double numEliminatedPixels;
    double LogBinningMin = INT_MAX;
    double LogBinningMax = 0;
    double Dist;
    double Candidate;
    double NotEliminatedPixels = 0;
    double GetBinCenter;
    int b;
    int GoodPixels = 0;
    double PositionRatesMaximum;
    double PositionRatesCutoff;
    int OIndex;
    
    mostly_t mostlyData;
    
    mostlyData.LogBinning	= NULL;
    mostlyData.delta		= NULL;
    mostlyData.PositionRates	= NULL;
    
    for (by = 0; by < N_POSITION_BIN; ++by) 
    {
	for (bx = 0; bx < N_POSITION_BIN; ++bx)    
	{
	    PositionsOnSource[bx][by]=0;
	}
    }
    
    /* Fill histograms which require filling by event */
    for (e = 0; e < evtnrows; ++e) 
    {
      
	if (evtinfo[e].energy < global.saaparinfo.SN_SpectrumMin || evtinfo[e].energy > global.saaparinfo.SN_SpectrumMax) 
	{	    
	    continue;    
	}

	if (IsGTI(gti, evtinfo[e].Time) == 0) 
	{
	    continue;
	}
	
	FindClosestOrbitIndex(orbitinfo, orbitnrows, evtinfo[e].Time, &OIndex);
	
	/*if (orbitinfo[OIndex].Longitude >= global.saaparinfo.SN_NonSAAMinLongOpt && orbitinfo[OIndex].Longitude <= global.saaparinfo.SN_NonSAAMaxLongOpt) */
	{
	    if (orbitinfo[OIndex].slew == 0 && orbitinfo[OIndex].occulted==0 && orbitinfo[OIndex].SAA_A == 0 && orbitinfo[OIndex].SAA_B == 0) 
	    {
		      DetectorID = evtinfo[e].DetectorID;
		      RawX = evtinfo[e].RawX;
		      RawY = evtinfo[e].RawY;
		      
		      if (RawX >= 0 && RawX <= 31 && RawY >= 0 && RawY <= 31) 
		      {
			  ConvertRawPos(RawX, RawY, DetectorID, &PosX, &PosY);
			  
			  /*
			  int New_RawX=0,New_RawY=0,New_DetectorID=0;
			  New_DetectorID = 0;
			  New_RawX = 0;
			  New_RawY = 0;
			  ConvertPosRaw(PosX, PosY, &New_DetectorID, &New_RawX, &New_RawY);
			  
			  
			  
			  if(RawX!=New_RawX || New_RawY!=RawY)
				  headas_chat(NORMAL,"%s: Rawx=%4d, RawY=%4d, New_Rawx=%4d, New_RawY=%4d, New_DetectorID=%2d\n", global.taskname,RawX,RawY,New_RawX,New_RawY,New_DetectorID);
			  */
			  PositionsOnSource[PosX][PosY]++;
		      }
	    }
	}
	
    }
        
    /*ShowCCD(PositionsOnSource); */
    
    /* Callcolo il Min ed il Max*/
    for (bx = 0; bx < N_POSITION_BIN; ++bx) 
    {
	for (by = 0; by < N_POSITION_BIN; ++by) 
	{
	    if (PositionsOnSource[bx][by]> 1) 
	    {
		if (PositionsOnSource[bx][by] > LogBinningMax) 
		{
		    LogBinningMax = PositionsOnSource[bx][by];
		}
		if (PositionsOnSource[bx][by] < LogBinningMin) 
		{
		    LogBinningMin = PositionsOnSource[bx][by];
		}
	    }
	}
    }
    
    /*
    headas_chat(NORMAL,"%s: Info: LogBinningMin=%f\n", global.taskname,LogBinningMin);
    headas_chat(NORMAL,"%s: Info: LogBinningMax=%f\n", global.taskname,LogBinningMax);
    */
    
    mostlyData.RealLogBinningBins = 0; 
    mostlyData.LogBinningBins = global.saaparinfo.SN_LogBinningBins; 
    
    mostlyData.LogBinning = (double *)calloc((mostlyData.LogBinningBins), sizeof(double));
    if(mostlyData.LogBinning==NULL) 
    {
        headas_chat(CHATTY,"%s: Error: mostlyData.LogBinning memory allocation failure.\n", global.taskname);
        goto MostlyEliminateSource_end;
    }
    mostlyData.delta = (double *)calloc((mostlyData.LogBinningBins), sizeof(double)); 
    if(mostlyData.delta==NULL) 
    {
        headas_chat(CHATTY,"%s: Error: mostlyData.delta memory allocation failure.\n", global.taskname);
        goto MostlyEliminateSource_end;
    }
    mostlyData.PositionRates=(double *)calloc((mostlyData.LogBinningBins), sizeof(double));
    if(mostlyData.PositionRates==NULL) 
    {
        headas_chat(CHATTY,"%s: Error: mostlyData.PositionRates memory allocation failure.\n", global.taskname);
        goto MostlyEliminateSource_end;
    }
    
    LogBinningMax += 1;
    
    LogBinningMin = log(LogBinningMin);
    LogBinningMax = log(LogBinningMax);
    Dist = (LogBinningMax-LogBinningMin)/(mostlyData.LogBinningBins - 1.0);
    
    
    /*
    headas_chat(NORMAL,"%s: Info: mostlyData.LogBinningBins=%d\n", global.taskname,mostlyData.LogBinningBins);
    headas_chat(NORMAL,"%s: Info: LogBinningMin=%f\n", global.taskname,LogBinningMin);
    headas_chat(NORMAL,"%s: Info: LogBinningMax=%f\n", global.taskname,LogBinningMax);
    headas_chat(NORMAL,"%s: Info: Dist=%f\n", global.taskname,Dist);
    */
    
    
    mostlyData.LogBinning[0] = (int) (exp(LogBinningMin) + 0.5) - 0.5; 
    for (i = 1; i < mostlyData.LogBinningBins; ++i) 
    {
      Candidate = (int) (exp(LogBinningMin+i*Dist) + 0.5) - 0.5;
      /* Skip bins < 1 count */ 
      if (Candidate == mostlyData.LogBinning[mostlyData.RealLogBinningBins]) 
      {
        continue;
      }
      mostlyData.LogBinning[++mostlyData.RealLogBinningBins] = Candidate;
    } 
    
    
    
    for (i = 0; i < mostlyData.LogBinningBins - 1; ++i) 
    {
		  mostlyData.delta[i] = mostlyData.LogBinning[i+1] - mostlyData.LogBinning[i];
      if (mostlyData.delta[i] < 0) mostlyData.delta[i] = 0;
	 
	    /*headas_chat(NORMAL,"%s: Info: mostlyData.LogBinning[%d]=%d DELTA=%d %d-%d\n", global.taskname,i,mostlyData.LogBinning[i],mostlyData.delta[i],i,i-1);*/
    }

       
    GoodPixels = 0;
    for ( bx = 0; bx < N_POSITION_BIN; ++bx) 
    {
	for ( by = 0; by < N_POSITION_BIN; ++by) 
	{
	    if (PositionsOnSource[bx][by] > 0) 
	    {
		/*printf("GoodPixels=%f PositionsOnSource[%d][%d]=%f\n",(double)GoodPixels,bx,by,(double)PositionsOnSource[bx][by]);*/
		MyFill(PositionsOnSource[bx][by],&mostlyData);
		GoodPixels++;
	    }
	}
    }
    
    /* Normalize */
    for (i = 0; i < mostlyData.LogBinningBins - 1; ++i) {
      double Width = mostlyData.LogBinning[i+1] - mostlyData.LogBinning[i];
      if (Width != 0) mostlyData.PositionRates[i] /= Width;
    }
    
#ifdef WRITE_DEBUG     
    writeVetD("PositionRatesBeforeSmooth",mostlyData.PositionRates,mostlyData.RealLogBinningBins);
#endif
    
    
    MySmoothArray(mostlyData.RealLogBinningBins,&mostlyData.PositionRates[0],global.saaparinfo.SN_SmoothSOURCE); 
   
#ifdef WRITE_DEBUG     
    writeVetD("PositionRatesAfterSmooth",mostlyData.PositionRates,mostlyData.RealLogBinningBins);
#endif

    headas_chat(NORMAL,"%s: Info: GoodPixels              = %d \n", global.taskname,GoodPixels);
    headas_chat(NORMAL,"%s: Info: RealLogBinningBins      = %d \n", global.taskname,mostlyData.RealLogBinningBins);
    
    PositionRatesMaximum = GetPositionMax(&mostlyData);
    PositionRatesCutoff = PositionRatesMaximum + global.par.sourcethr * sqrt(PositionRatesMaximum);
    
    
    NotEliminatedPixels = 0;
      
   
    /* This task should just eliminate the worst offenders, we do not want to cut more than ~25% of the data*/
    
    for (b = 0; b < mostlyData.RealLogBinningBins-1; ++b) 
    {
	    GetBinCenter = 0.5*(mostlyData.LogBinning[b] + mostlyData.LogBinning[b+1]);
	/*printf("GoodPixels=%f PositionRates->GetBinCenter(%d)=%f PositionRates->GetBinContent(%d)=%f PositionRates->GetBinWidth(%d)=%f\n", 
	   (double)GoodPixels,
	   b,
	   GetBinCenter,
	   b,
	   mostlyData.PositionRates[b],
	   b,
	   (double)mostlyData.delta[b]
	  );*/
		
	if (GetBinCenter < PositionRatesCutoff) 
	{
	    NotEliminatedPixels += mostlyData.PositionRates[b] * mostlyData.delta[b];
	} 
    }

    headas_chat(NORMAL,"%s: Info: CutOff=%f  with %f of %d used pixels \n", global.taskname,PositionRatesCutoff,NotEliminatedPixels,GoodPixels);
    
    /* Number of eliminated pixel */
    (*nEliminateSource)=0;
    
    for (bx = 0; bx < N_POSITION_BIN; ++bx) 
    {
	for (by = 0; by < N_POSITION_BIN; ++by) 
	{
	    if (PositionsOnSource[bx][by] > PositionRatesCutoff) 
	    {
		(*nEliminateSource)++;
	    }
	}
    }
    
    /* Create a map of excluded bins: Notation 1000*Det + 100*RawX + RawY (for faster lookup later) */
    headas_chat(NORMAL,"%s: Info: nEliminateSource        = %d \n", global.taskname,*nEliminateSource);
    
    *ExcludedDetRawXY = (int *)malloc(*nEliminateSource * sizeof(int));
    if(*ExcludedDetRawXY==NULL) 
    {
	headas_chat(CHATTY,"%s: Error: MostlyEliminateSource: ExcludedDetRawXY memory allocation failure.\n", global.taskname);
	goto MostlyEliminateSource_end;
    }
    
    cont=0;
   
    for (bx = 0; bx < N_POSITION_BIN; ++bx) 
    {
	for (by = 0; by < N_POSITION_BIN; ++by) 
	{
	    if (PositionsOnSource[bx][by] > PositionRatesCutoff) 
	    {
		DetectorID = 0;
		RawX = 0;
		RawY = 0;
		ConvertPosRaw(bx, by, &DetectorID, &RawX, &RawY);
		
		DXY = 10000*DetectorID + 100*RawX + RawY;
		(*ExcludedDetRawXY)[cont]=DXY;
		
		/*headas_chat(NORMAL,"%s: Info: Eliminated[%03d]--> Det_ID=%2d, RawX=%2d, RawY=%2d,  DXY=%d\n", global.taskname,cont,DetectorID,RawX,RawY,DXY);*/
		cont++;
	    }
	}
    }
    MySort(*ExcludedDetRawXY,*nEliminateSource);
  
    numEliminatedPixels=100.0 * (*nEliminateSource) / (64*64);
    headas_chat(NORMAL,"%s: Info: Eliminated pixels       = %f %\n", global.taskname,numEliminatedPixels);

    if(mostlyData.LogBinning!=NULL)
	free(mostlyData.LogBinning);
    if(mostlyData.delta!=NULL)
	free(mostlyData.delta);
    if(mostlyData.PositionRates!=NULL)
	free(mostlyData.PositionRates);
    
    return OK;
    
MostlyEliminateSource_end:
    
    if(mostlyData.LogBinning!=NULL)
	free(mostlyData.LogBinning);
    if(mostlyData.delta!=NULL)
	free(mostlyData.delta);
    if(mostlyData.PositionRates!=NULL)
	free(mostlyData.PositionRates);
    
    return NOT_OK;
} /* MostlyEliminateSource */


double GetPositionMax(mostly_t * ms)
{
    int id;
    double PositionRatesMaximum;
    
    id=GetIndexMax(ms->PositionRates,ms->RealLogBinningBins,0);
    
    if (id < ms->RealLogBinningBins) {
      PositionRatesMaximum = 0.5*(ms->LogBinning[id] + ms->LogBinning[id+1]);
    } else {
      PositionRatesMaximum = ms->LogBinning[id];
    }
    
    /*headas_chat(NORMAL,"%s: Info: ms->LogBinning[%d]=%f \n", global.taskname,id,ms->LogBinning[id]);
    headas_chat(NORMAL,"%s: Info: indice del valore massimo=%d \n", global.taskname,id);
    headas_chat(NORMAL,"%s: Info: PositionRatesMaximum=%f \n", global.taskname,PositionRatesMaximum);*/
    
    return PositionRatesMaximum;
} /* GetPositionMax */

int MyCount(double *vet_sorg,int vet_size)
{
    int i;
    int count=0;
    
    for(i=0;i<vet_size-1;i++)
    {
	if(vet_sorg[i]>0)
	    count++;
    }
    
    return count;
} /* MyCount */

void MyFill(int val,mostly_t * ms)
{
    int i;
        
    if(val < ms->LogBinning[0]) return;
    
	  for(i=0; i < ms->LogBinningBins-1; i++)
    {
		  if(val>=ms->LogBinning[i] && val<ms->LogBinning[i+1])
		  {
		  
		    ms->PositionRates[i]++;
		    /*printf("Val=%f     bin=%d     PositionRates[%d]=%f\n",
		      (double)val,
		      i+1,
		      i+1,
		      ms->PositionRates[i+1]);*/
		    break;
		  }
	  }
} /* MyFill */

int MyIntegral(BTYPE *vet,int vet_size)
{
    int i=0,sum=0;
    
    for(i=0;i<vet_size;i++)
    {
	  sum=sum+vet[i];
    }
    
    return sum;
} /* MyIntegral */

double MyIntegrald(DTYPE *vet,int vet_size)
{
    int i=0;
    double sum=0;
    
    for(i=0;i<vet_size;i++)
    {
	  sum=sum+vet[i];
    }
    
    return sum;
} /* MyIntegrald */

void ConvertRawPos(int RawX, int RawY, int DetectorID, int *PosX, int *PosY)
{ 
    if (DetectorID == 0) 
    {
	*PosX = +RawY + 1; 
	*PosY = +RawX + 1; 
    } 
    else if (DetectorID == 1) 
    {
	*PosX = -RawX - 1; 
	*PosY = +RawY + 1;
    } 
    else if (DetectorID == 2) 
    {
	*PosX = -RawY - 1;
	*PosY = -RawX - 1;
    }
    else if (DetectorID == 3) 
    {
	*PosX = +RawX + 1;
	*PosY = -RawY - 1;
    }
    
    *PosX += 32;
    *PosY += 32;
} /* ConvertRawPos */

void ConvertPosRaw(int PosX, int PosY, int *DetectorID, int *RawX, int *RawY)
{ 
    PosX -= 32;
    PosY -= 32;
    
    if (PosX > 0 && PosY > 0) 
    {
	*DetectorID = 0;
	*RawX = (PosY)-1;
	*RawY = (PosX)-1;
    } 
    else if (PosX < 0 && PosY > 0) 
    {
	*DetectorID = 1;
	*RawX = (-PosX)-1;
	*RawY = (PosY)-1;
    } 
    else if (PosX < 0 && PosY < 0) 
    {
	*DetectorID = 2;
	*RawX = (-PosY)-1;
	*RawY = (-PosX)-1;
    } 
    else if (PosX > 0 && PosY < 0) 
    {
	*DetectorID = 3;
	*RawX = (PosX)-1;
	*RawY = (-PosY)-1;
    }
} /* ConvertPosRaw */

int GetPeakLowEdge(Baesian_t *baesian, int bin)
{
  if (bin < 0 || bin >= baesian->m_BinnedData_size) {
    headas_chat(NORMAL, "%s: Error: Bin size out of bounds - the following results will be wrong\n", global.taskname);    
    return 0;
  }
  return baesian->m_BinEdges[bin];
} /* GetPeakEdge */
    
int GetPeakHighEdge(Baesian_t *baesian, int bin)
    {
  if (bin < 0 || bin >= baesian->m_BinnedData_size) {
    headas_chat(NORMAL, "%s: Error: Bin size out of bounds - the following results will be wrong\n", global.taskname);    
    return 0;
    } 
  return baesian->m_BinEdges[bin+1]; /* Edges is one larger than data */
} /* GetPeakEdge */

int GetNormalized(Baesian_t *baesian,DTYPE **vett_dest, unsigned int *vett_dest_size)
{
	unsigned int index;
        double binSize;
        
        /* Allocate memory to storage all data */
        *vett_dest_size = baesian->m_BinnedData_size; 
        *vett_dest = (DTYPE *)calloc(*vett_dest_size, sizeof(DTYPE));
        if(*vett_dest==NULL) 
        {
            goto GetNormalized_end;
        }
        
        /* Double
        for(index=0;index<(*vett_dest_size);index++)
        {
            binSize=baesian->m_BinEdges[index+1]-baesian->m_BinEdges[index];
        }
        */
        
        for(index=0;index < baesian->m_BinnedData_size;index++)
        {
            binSize=baesian->m_BinEdges[index+1]-baesian->m_BinEdges[index];
      if (binSize > 0) { 
            (*vett_dest)[index]= baesian->m_BinnedData[index]/binSize;
      } else {
        (*vett_dest)[index] = 0;
      }
        }
        
        return OK;
	
GetNormalized_end:
	return NOT_OK;
    
} /* GetNormalized */

int MyBayesianBlocks(int m_Minimum,int m_Maximum,int interval,double prior, DTYPE *vett_sorg,unsigned int vett_sorg_size,Baesian_t *baesian)
{
	/* ! The minimum bin width */
	double m_MinimumBinWidth;
	
	double *Edges = NULL;
	unsigned int Edges_size = 0;
	
	float* BlockLength = NULL;
	unsigned int BlockLength_size = 0;
	
	float* CountsPerBin = NULL;
	unsigned int CountsPerBin_size = 0;
	
	double* Best = NULL;
	unsigned int Best_size = 0;
	
	unsigned int* Last = NULL;
	unsigned int Last_size = 0;
	
	float* Width = NULL;
	unsigned int Width_size = 0;
	
	float* BlockCounts = NULL;
	unsigned int BlockCounts_size = 0;
		
	float* Fits = NULL;
	unsigned int Fits_size = 0;

	unsigned int* ChangePoints = NULL;
	unsigned int ChangePoints_size = 0;
	
	double *m_AxisValue = NULL;
	unsigned int m_AxisValue_size = 0;
	
	double *m_DataValue = NULL;
	unsigned int m_DataValue_size = 0;
	
	unsigned int m_BinEdges_index;	
	unsigned int e;
	unsigned int i;
	unsigned int s;
 	unsigned int Size;
	double appo;
	
	unsigned int index;
	int ChangePointsIndex;
	int CurrentIndex;
	int LastCounts;
	double Fit;
	double Value;
	unsigned int Maximum;
	
	m_MinimumBinWidth	= interval;
	Size			= vett_sorg_size;
	m_AxisValue_size	= Size;
	m_DataValue_size	= Size;
	ChangePointsIndex 	= Size;
	CurrentIndex 		= Size;
	
	
	/* Step 1: Fill m_AxisValue */
	m_AxisValue = (double*)malloc(m_AxisValue_size*sizeof(double));
	if(m_AxisValue==NULL)
	{
	    headas_chat(CHATTY,"%s: Error: m_AxisValue memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	m_DataValue = (double*)malloc(m_DataValue_size*sizeof(double));
	if(m_DataValue==NULL)
	{
	    headas_chat(CHATTY,"%s: Error: m_DataValue memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	for(index=0;index<Size;index++)
	{
	    m_AxisValue[index]=30+(index*60);
            m_DataValue[index]=vett_sorg[index];
        }
        
        Edges_size=Size+1; 
        Edges = (double*)malloc(Edges_size*sizeof(double));
        if(Edges==NULL)
        {
	    headas_chat(CHATTY,"%s: Error: Edges memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	
	for(index=0;index<Edges_size;index++)
	{
	    Edges[index]=index*m_MinimumBinWidth;
	}

	
	/* Step 3: Create Block length: */
	BlockLength_size=Edges_size;
	BlockLength = (float*)malloc(BlockLength_size*sizeof(float));
	if(BlockLength==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: BlockLength: Edges memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	for (index = 0; index < BlockLength_size; ++index) 
	{
	    appo=Edges[BlockLength_size-1];
	    BlockLength[index] = (appo - Edges[index]);
	}
	
	

	/* Step 4: Prepare for iterations */
	CountsPerBin_size=Edges_size-1;
	CountsPerBin = (float*)calloc(CountsPerBin_size, sizeof(float));
	if(CountsPerBin==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: CountsPerBin memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	for (index=0;index<CountsPerBin_size;index++)
	{
	    Value = m_AxisValue[index];
	    for (e = 0; e < Edges_size - 1; ++e) 
	    { 
		/* Speed improvement possible */
		if (Edges[e] <= Value && Edges[e+1] > Value) 
		{
		    CountsPerBin[e] += m_DataValue[index];
		    break;     
		}
	    }
	}
	

	Best_size=Size;
	Best = (double*)calloc(Best_size, sizeof(double));
	if(Best==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: Best memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	Last_size=Size;
	Last = (unsigned int*)calloc(Last_size, sizeof(unsigned int));
	if(Last==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: Last memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	Width_size=Size;
	Width = (float*)malloc(Width_size * sizeof(float));
	if(Width==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: Width memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	BlockCounts_size=Size+1;
	BlockCounts = (float*)calloc (BlockCounts_size,sizeof(float));
	if(BlockCounts==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: BlockCounts memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
		
	Fits_size=Size;
	Fits = (float*)calloc (Fits_size, sizeof(float));
	if(Fits==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: Fits memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	
	/* Step 5: Iterate */
	for (s=0; s<Size; ++s) 
	{
	    /* Calculate the width of the blocks */
	    for (i = 0; i<= s; ++i) 
	    {
		Width[i]=(BlockLength[i] - BlockLength[s+1]);
	    }
	    
	    /* Calculate the block count */
	    LastCounts = 0;
	    for (i = s; i <= s; i--) 
	    {
		BlockCounts[i] = LastCounts + CountsPerBin[i];
                LastCounts = BlockCounts[i];
            }

            /* Calculate the fits block */
            for (i = 0; i <= s; ++i) 
            {
                double appo1=(log(BlockCounts[i]) - log(Width[i]));
		double appo2=( (double)BlockCounts[i] * appo1 );
		Fit = (double) appo2;
 		/* headas_chat(NORMAL,"1) s=%d appo1=%f appo2=%f Fit=%f  BlockCounts[%d]=%f  log(BlockCounts[%d])=%f log(Width[%d])=%f\n", s, appo1,appo2, (float)Fit,i,BlockCounts[i],i,(float)log(BlockCounts[i]),i,(float)log(Width[i])); */
		Fit = Fit - prior;
 		/* headas_chat(NORMAL,"1) s=%d Fit-prior=%f\n", s, (float)Fit); */
		Fits[i]=(Fit);
		/* headas_chat(NORMAL,"1) s=%d Fits[%d]=%f  BlockCounts[%d]=%f  log(BlockCounts[%d])=%f log(Width[%d])=%f\n", s, i, (float)Fits[i],i,BlockCounts[i],i,(float)log(BlockCounts[i]),i,(float)log(Width[i])); */
	    }
	    
	    for (i = 1; i <= s; ++i) 
	    {
		Fits[i] += Best[i-1];
	    }
	
	    Maximum = 0;
	    for (i = 0; i <= s; ++i) 
	    {
		if (Fits[i] > Fits[Maximum]) Maximum = i;
	    }
	    
	    Last[s] = Maximum;
	    Best[s] = Fits[Maximum];
	}
	
	/* Step 6: Find the change points: */
	ChangePoints_size=Size;
	ChangePoints = (unsigned int*)calloc(ChangePoints_size, sizeof(unsigned int));
	if(ChangePoints==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: ChangePoints memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}

  	while (TRUE) 
	{
	    ChangePointsIndex -=1;
	    ChangePoints[ChangePointsIndex] = CurrentIndex;
	    if (CurrentIndex == 0) 
	    {
		break;
	    }
	    CurrentIndex = Last[CurrentIndex - 1];
	}
	
	baesian->m_BinEdges_size=Size-ChangePointsIndex;
	baesian->m_BinEdges = (double *)calloc(baesian->m_BinEdges_size, sizeof(double));
	if(baesian->m_BinEdges==NULL)
	{	
	    headas_chat(CHATTY,"%s: Error: baesian->m_BinEdges memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	
	m_BinEdges_index=0;
	
	for (i = ChangePointsIndex; i < Size; ++i) 
	{
	    baesian->m_BinEdges[m_BinEdges_index]=(Edges[ChangePoints[i]]);
	    m_BinEdges_index++;
	} 
	
	
	/* Step 7: Do some sanity checks:
           NOT IMPLEMENTED */
        
        /* Step 8: Finally fill the data array */
        baesian->m_BinnedData_size=m_BinEdges_index - 1; 
        baesian->m_BinnedData = (double *)calloc(baesian->m_BinnedData_size, sizeof(double));
        if(baesian->m_BinnedData==NULL)
        {       
	    headas_chat(CHATTY,"%s: Error: baesian->m_BinnedData memory allocation failure.\n", global.taskname);
	    goto MyBayesianBlocks_end;
	}
	
	for (index = 0; index<m_AxisValue_size; ++index) 
	{
	    for (e = 0; e < baesian->m_BinEdges_size; ++e) 
	    {
		if (baesian->m_BinEdges[e] > m_AxisValue[index]) 
		{
		    if (e > 0) 
		    {
			baesian->m_BinnedData[e-1] += m_DataValue[index];
		    }
		    break;
		}
	    }
	}
	
	
	/* Step 9: Reject bins with less than X elements
	   NOT IMPLEMENTED */
	
	if(Edges!=NULL)
		free(Edges);
	if(BlockLength!=NULL)
		free(BlockLength);
	if(CountsPerBin!=NULL)
		free(CountsPerBin);
	if(Best!=NULL)
		free(Best);
	if(Last!=NULL)
		free(Last);
	if(Width!=NULL)
		free(Width);
	if(BlockCounts!=NULL)
		free(BlockCounts);
	if(Fits!=NULL)
		free(Fits);	
	if(ChangePoints!=NULL)
		free(ChangePoints);
	if(m_AxisValue!=NULL)
		free(m_AxisValue);
	if(m_DataValue!=NULL)
		free(m_DataValue);
	
	return OK;
	
MyBayesianBlocks_end:

	if(Edges!=NULL)
		free(Edges);
	if(BlockLength!=NULL)
		free(BlockLength);
	if(CountsPerBin!=NULL)
		free(CountsPerBin);
	if(Best!=NULL)
		free(Best);
	if(Last!=NULL)
		free(Last);
	if(Width!=NULL)
		free(Width);
	if(BlockCounts!=NULL)
		free(BlockCounts);
	if(Fits!=NULL)
		free(Fits);	
	if(ChangePoints!=NULL)
		free(ChangePoints);
	if(m_AxisValue!=NULL)
		free(m_AxisValue);
	if(m_DataValue!=NULL)
		free(m_DataValue);

	return NOT_OK;
  
} /* MyBayesianBlocks */


void checkFMP(const char* input,char* dest)
{
  subString(input,LEN_STEMOUT,1,dest);
  headas_chat(NORMAL, "%s: Info: FPM'%s'\n", global.taskname,dest);
} /* checkFMP */


void subString (const char* input, int offset, int len, char* dest)
{
    int input_len = strlen (input);

    if (offset + len > input_len)
    {
        strcpy (dest,"");
    }

    strncpy (dest, input + offset, len);
    dest[len]='\0';
} /* subString */
