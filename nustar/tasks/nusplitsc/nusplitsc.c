/*
 *
 *	nusplitsc.c
 *
 *	INVOCATION:
 *
 *		nusplitsc [parameter=value ...]
 *
 *	DESCRIPTION:
 *              Routine for the conversion of the charge of each event from electronic units ("Pulse Height Amplitude", PHA)
 *              into energy ("Pulse Invariant", PI) units
 *
 *	DOCUMENTATION:
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - RF 14/12/15 - First version
 *
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */


#define TOOLSUB nusplitsc  /* headas_main() requires that TOOLSUB be defined first */

/********************************/
/*        header files          */
/********************************/
#include "headas_main.c"
#include "nusplitsc.h"

/********************************/
/*         definitions          */
/********************************/

#define NUSPLITSC_C
#define NUSPLITSC_VERSION      "0.1.1"
#define PRG_NAME               "nusplitsc"

/********************************/
/*           globals            */
/********************************/

Global_t global;


/*
 *	nusplitsc_getpar
 *
 *
 *	DESCRIPTION:
 *                 Routine to read input parameters from
 *                 nusplitsc.par
 *
 *
 *      FUNCTION CALL:
 *           int PILGetFname(char *name, char *result);
 * 	     int PILGetInt(char *name, int *result);
 *           int PILGetReal(char *name, int *result);
 *           int headas_chat(int , char *, ...);
 *           void nusplitsc_info(void);
 *           void get_history(int *);
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - RF 14/12/15 - First version
 *        0.1.1 - RF 05/04/16 - Add 'outdir' parameter
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int nusplitsc_getpar()
{

    /* Input SCIENCE_SC FITS Event File Name */
    if(PILGetFname(PAR_INFILE, global.par.infile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_INFILE);
        goto Error;
    }

    /* Input CHU123 Housekeeping FITS File */
    if(PILGetFname(PAR_CHU123HKILE, global.par.chu123hkfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CHU123HKILE);
        goto Error;
    }

    /* Input Housekeeping FITS filee */
    if(PILGetFname(PAR_HFILE, global.par.hkfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_HFILE);
        goto Error;
    }


    /* Input fits CHU123RANGE file or CALDB  */
    if(PILGetFname(PAR_CHU123RANGEFILE, global.par.chu123rangefile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CHU123RANGEFILE);
        goto Error;
    }
    
    /* Input fits OUTDIR file or CALDB  */
    if(PILGetFname(PAR_OUTDIR, global.par.outdir))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_OUTDIR);
        goto Error;
    }
    
	
    /* Create the task 'global.par.outdir' directory */
    if(strcasecmp(global.par.outdir,"./")) 
    {
	if( (mkdir(global.par.outdir,0777)) && (errno != EEXIST) )
	{
	    headas_chat(NORMAL, "%s: Error: Unable to create the directory '%s'\n", global.taskname,global.par.outdir);
	    goto Error;
	}
    }
    /* CHU123 splitting mode (NORMAL,STRICT)  */
    if(PILGetFname(PAR_SPLITMODE, global.par.splitmode))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_SPLITMODE);
        goto Error;
    }
    else
    {
        if(strcmp(global.par.splitmode,"NORMAL")==STR_EQUAL)
        {
            strcpy(global.postfr,"0.999999");
        }
        else
        {
            strcpy(global.postfr,"0");
        }
    }

    /* Cut non simultaneous CHU time intervals (yes/no)?  */
    if(PILGetBool(PAR_TIMECUT, &global.par.timecut))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_TIMECUT);
        goto Error;
    }

    /* Stem for the output files or DEFAULT to use the stem of input files  */
    if(PILGetFname(PAR_STEMOUT, global.par.stemout))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_STEMOUT);
        goto Error;
    }

    /* Delete temporary file? (yes/no)  */
    if(PILGetBool(PAR_CLEANUP, &global.par.cleanup))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get '%s' parameter.\n",global.taskname,PAR_CLEANUP);
        goto Error;
    }

    get_history(&global.hist);
    nusplitsc_info();

    return OK;

Error:
    return NOT_OK;

} /* nusplitsc_getpar */


/*
 *	nusplitsc_work
 *
 *
 *	DESCRIPTION:
 *
 *
 *
 *      FUNCTION CALL:
 *             int nusplitsc_checkinput();
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
 *             int rename (char *, char *);
 *             int CopyFile(char *source, char *destination);
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - RF 14/12/15 - First version
 *
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int nusplitsc_work()
{
    int            	status = OK, i=0;
    int 			evExt=0;
    CmdNuSplitSC_t	cmd;
    FitsFileUnit_t 	inunit=NULL;

	if(nusplitsc_checkinput())
	{
        goto Error;
    }
    
    if(getInstrument())
    {
        goto Error;
    }

    if(GetObsInfo(global.par.infile, KWVL_EXTNAME_EVT))
    {
        headas_chat(NORMAL, "%s: Error: Unable to get info from %s file.\n", global.taskname, global.par.chu123rangefile);
        goto Error;
    }
    
    if(loadCALDBfile(KWVL_CHU123RANGE_EXT1))
    {
			goto Error;
    }
    

    /* Make Evt Output file name */
    if(makeOutputEvtFileName(&cmd))
    {
			goto Error;
    }

	/* Check Evt Output file name clobber=yes/no*/
	if(nusplitsc_checkoutput(&cmd))
    {
			goto Error;
    }
    
    /* Make command line for maketime,xselect,nulivetime */
    if(makeCMD(&cmd))
    {
			goto Error;
    }

    /* Split CHU File in CHU1,CHU2,CHU3 */
    if(splitChuFile())
    {
        goto Error;
    }
    
    
    /* Create CHU Merge file */
    if(mergeChuFile())
    {
        goto Error;
    }
    
	
    /* Execute maketime command */
    if(execMaketime(&cmd))
    {
        goto Error;
    }

#ifdef CHECKEMPTY
    /* Check STDGTI empty in input STDGTI file */
    if(checkEmptyfile(&cmd))
    {
        goto Error;
    }
#endif

    
    /* Execute xselect command */
    if(execXselect(&cmd))
    {
        goto Error;
    }
    
    /* Check GTI empty in output evt file */
    if(checkOutputEmptyfile(&cmd))
    {
        goto Error;
    }

    /* Execute nulivetime command */
    if(execNulivetime(&cmd))
    {
        goto Error;
    }
    
    if(global.par.cleanup==TRUE)
    {
        if(cleanNuSplitScTempFile(&cmd))
        {
            goto Error;
        }
    }
	
    /* get evt HDU, checksum, HDpar_stamp */
    for (i=0; i<NUM_EXPRSCREENING; i++)
    {
		if(strcmp(cmd.xselectCmd[i],"")==STR_EQUAL) /* il file eventi non è stato generato */
		{
			continue;
		}
		
        if ((inunit=OpenReadWriteFitsFile(cmd.output_evtFile[i])) <= (FitsFileUnit_t )0)
        {
            headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, cmd.output_evtFile[i]);
            goto Error;
        }

        /* Move in events extension in input file */
        if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
        {
            headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
            goto Error;
        }

        if (!fits_get_hdu_num(inunit,&evExt))
        {
            headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension\n", global.taskname,KWVL_EXTNAME_EVT);
            headas_chat(NORMAL,"%s: Error: in '%s' file.\n", global.taskname, cmd.output_evtFile[i]);
            goto Error;
        }

        if(HDpar_stamp(inunit,evExt,&status))
        {
            headas_chat(NORMAL,"%s: Error: unable to add HISTORY keywords.\n", global.taskname);
            goto Error;
        }
        else
        {
            headas_chat(NORMAL,"%s: Info: Add HISTORY keywords to file '%s' in '%s' HDU.\n", global.taskname,cmd.output_evtFile[i],KWVL_EXTNAME_EVT);
        }

        if (ChecksumCalc(inunit))
        {
            headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, cmd.output_evtFile[i]);
            goto Error;
        }
        else
        {
            headas_chat(NORMAL,"%s: Info: Update CHECKSUM and DATASUM '%s'.\n", global.taskname,cmd.output_evtFile[i]);
        }

        if (CloseFitsFile(inunit))
        {
            headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, cmd.output_evtFile[i]);
            goto Error;
        }

    }

    
    for (i=0; i<NUM_EXPRSCREENING; i++)
    {
        if(strcmp(cmd.xselectCmd[i],"")==STR_EQUAL)
        {
				continue;
        }
        else
        {
				headas_chat(NORMAL, "%s: Info: '%s' file successfully written.\n", global.taskname, cmd.output_evtFile[i]);
        }
    }
    
    nusplitsc_report(&cmd);

    return OK;
Error:

    return NOT_OK;
} /* nusplitsc_work */

int nusplitsc_report(CmdNuSplitSC_t *_cmd)
{
	int 			status = OK, i = 0;
	double 			exposure_chu = 0.0;
	FitsFileUnit_t	inunit=NULL; 
	
	headas_chat(NORMAL,"---------------------------------------------------------------------\n");
    headas_chat(NORMAL,"NuSTAR '%s' Report\n",global.taskname);
    headas_chat(NORMAL,"---------------------------------------------------------------------\n");
	
	for (i=0; i<NUM_EXPRSCREENING; i++)
    {
			if(strcmp(_cmd->xselectCmd[i],"")==STR_EQUAL)
			{
					exposure_chu=0.0;
					headas_chat(NORMAL,"Info: Exposure time of '%s' = 0 seconds\n",_cmd->output_evtFile[i]);
			}
			else /* Get Exposure TIME from Evt Output file*/
			{
					/* Open read only file */
					if ((inunit=OpenReadFitsFile(_cmd->output_evtFile[i])) <= (FitsFileUnit_t )0)
					{
							headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
							headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, _cmd->output_evtFile[i]);
							goto Error_report;
					}

					if (fits_read_key(inunit,TDOUBLE,KWNM_EXPOSURE, &exposure_chu,NULL, &status))
					{
							headas_chat(NORMAL, "%s: Error: Unable to read %s keyword \n", global.taskname, KWNM_EXPOSURE);
							headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname,  _cmd->output_evtFile[i]);
							goto Error_report;
					}
					
					/* Close file */
					if (CloseFitsFile(inunit))
					{
							headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
							headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, _cmd->output_evtFile[i]);
							goto Error_report;
					}
					headas_chat(NORMAL,"Info: Exposure time of '%s' = %.11f seconds\n",_cmd->output_evtFile[i],exposure_chu);
			}
			
			
	}

	return OK;
	
Error_report:
	return NOT_OK;
}

int getInstrument()
{
    int            	status = OK;
    FitsHeader_t   	head;
    FitsCard_t     	*card;
    FitsFileUnit_t 	inunit=NULL;   /* Input and Output fits file pointer */

    /* Open readonly input event file */
    if ((inunit=OpenReadFitsFile(global.par.infile)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, global.par.infile);
        goto Error_end;
    }

    /* Move in events extension in input file */
    if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_EVT, 0, &status))
    {
        headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,KWVL_EXTNAME_EVT);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, global.par.infile);
        goto Error;
    }


    /* Retrieve header pointer */
    head=RetrieveFitsHeader(inunit);


    /* Retrieve INSTRUME from input event file */
    if(ExistsKeyWord(&head, KWNM_INSTRUME, &card))
    {
        strcpy(global.evt.instrume, card->u.SVal);
    }
    else
    {
        headas_chat(NORMAL, "%s: Error: %s keyword not found\n", global.taskname, KWNM_INSTRUME);
        headas_chat(NORMAL, "%s: Error: in %s file.\n", global.taskname, global.par.infile);
        goto Error;
    }

    /* close input and output files */
    if (CloseFitsFile(inunit))
    {
        headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
        goto Error_end;
    }

    return OK;

Error:
    if( CloseFitsFile(inunit))
    {
        headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, global.par.infile);
    }
    return NOT_OK;

Error_end:
    return NOT_OK;
} /* getInstrument */

/*
 *	nusplitsc
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
 *             void nusplitsc_getpar(void);
 * 	       void nusplitsc_work(void);
 *             FileExists(char *)
 *             remove(char *)
 *
 *
 *	CHANGE HISTORY:
 *        0.1.0 - RF 14/12/15 - First version
 *
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int nusplitsc()
{
    /* set HEADAS globals */
    set_toolname(PRG_NAME);
    set_toolversion(NUSPLITSC_VERSION);

    get_toolnamev(global.taskname);

    GetNuSTARDASVersion(global.nustardas_v);
    global.warning=0;

    /* Get parameter values */
    if ( nusplitsc_getpar() == OK)
    {

        if ( nusplitsc_work())
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
        else
        {
            headas_chat(MUTE,"---------------------------------------------------------------------\n");
            headas_chat(MUTE, "%s: Exit with success.\n", global.taskname);
            headas_chat(MUTE,"---------------------------------------------------------------------\n");
        }

    }

    return OK;

pdcorr_end:

    return NOT_OK;

} /* nusplitsc */


/*
 *	nusplitsc_info
 *
 *	DESCRIPTION:
 *         Display information about input parameters
 *
 *
 *      FUNCTION CALL:
 *               int headas_chat(int, char *, ...);
 *
 *	CHANGE HISTORY:
 *        0.1.0 - RF 14/12/15 - First version
 *
 *
 */

void nusplitsc_info(void)
{

    headas_chat(NORMAL,"---------------------------------------------------------------------\n");
    headas_chat(NORMAL," \t\tRunning '%s'\n",global.taskname);
    headas_chat(NORMAL,"---------------------------------------------------------------------\n");
    headas_chat(NORMAL,"\t\t Input Parameters List: \n");
    headas_chat(NORMAL,"Name of input SCIENCE_SC(06) FITS Event File                        :'%s'\n",global.par.infile);
    headas_chat(NORMAL,"Name of input CHU123 Housekeeping FITS File                         :'%s'\n",global.par.chu123hkfile);
    headas_chat(NORMAL,"Name of input Housekeeping FITS File                                :'%s'\n",global.par.hkfile);
    headas_chat(NORMAL,"Name of the input fits CHU123RANGE file or CALDB                    :'%s'\n",global.par.chu123rangefile);
    headas_chat(NORMAL,"Name of the output directory for output files                       :'%s'\n",global.par.outdir);
    headas_chat(NORMAL,"CHU123 splitting mode (NORMAL,STRICT)                               :'%s'\n",global.par.splitmode);
    if(global.par.timecut==TRUE)
        headas_chat(NORMAL,"Cut non simultaneous CHU time intervals (yes/no)?                   :'yes'\n");
    else
        headas_chat(NORMAL,"Cut non simultaneous CHU time intervals (yes/no)?                   :'no'\n");
    headas_chat(NORMAL,"Stem for the output files or DEFAULT to use the stem of input files :'%s'\n",global.par.stemout);
    if(global.par.cleanup==TRUE)
        headas_chat(NORMAL,"Delete temporary file? (yes/no)                                     :'yes'\n");
    else
        headas_chat(NORMAL,"Delete temporary file? (yes/no)                                     :'no'\n");

    if (global.hist)
        headas_chat(CHATTY,"Write HISTORY keywords in output file                  : yes\n");
    else
        headas_chat(CHATTY,"Write HISTORY keywords in output file                  : no\n");
    if (headas_clobpar)
        headas_chat(CHATTY,"Overwrite existing output file                         : yes\n");
    else
        headas_chat(CHATTY,"Overwrite existing output file                         : no\n");

    headas_chat(NORMAL,"---------------------------------------------------------------------\n");


} /* nusplitsc_info */


/*
 *	nusplitsc_checkinput
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
 *        0.1.0 - RF 14/12/15 - First version
 * 
 *
 */
int nusplitsc_checkoutput(CmdNuSplitSC_t *_cmd)
{
    int			i=0;

    for(i=0; i<NUM_EXPRSCREENING; i++)
    {
        if(FileExists(_cmd->output_evtFile[i]))
        {
            headas_chat(NORMAL, "%s: Info: '%s' file exists,\n", global.taskname, _cmd->output_evtFile[i]);
            if(!headas_clobpar) /* clobber=no */
            {
                headas_chat(NORMAL, "%s: Error: cannot overwrite '%s' file.\n", global.taskname, _cmd->output_evtFile[i]);
                headas_chat(NORMAL, "%s: Error: Please set the parameter 'clobber' to yes to overwrite it.\n", global.taskname);
                goto check_end;
            }
            else /* clobber=yes */
            {
                headas_chat(NORMAL, "%s: Warning: parameter 'clobber' set, the\n", global.taskname);
                headas_chat(NORMAL, "%s: Warning: '%s' file will be overwritten.\n", global.taskname, _cmd->output_evtFile[i]);
                if(remove (_cmd->output_evtFile[i]) == -1)
                {
                    headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
                    headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, _cmd->output_evtFile[i]);
                    goto check_end;
                }
            }
        }
    }

    return OK;

check_end:
    return NOT_OK;
} /* nusplitsc_checkoutput */

int nusplitsc_checkinput()
{
    if(!FileExists(global.par.infile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to open input SCIENCE_SC(06) FITS Event File\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.infile);
        goto check_end;
    }
    
    if(!FileExists(global.par.chu123hkfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to open input CHU123 Housekeeping FITS File\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.chu123hkfile);
        goto check_end;
    }
    
    if(!FileExists(global.par.hkfile))
    {
        headas_chat(NORMAL, "%s: Error: Unable to open input Housekeeping FITS File\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.hkfile);
        goto check_end;
    }
		
		if ( strcasecmp(global.par.chu123rangefile,DF_CALDB) )
		{    
		    if(!FileExists(global.par.chu123rangefile))
		    {
		        headas_chat(NORMAL, "%s: Error: Unable to open input fits CHU123RANGE file\n", global.taskname);
		        headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.par.chu123rangefile);
		        goto check_end;
		    }
		}
		return OK;

check_end:
    return NOT_OK;
} /* nusplitsc_checkinput */

/*
 *
 *      ComputeDETCoords
 *
 *	DESCRIPTION:
 *           Routine to compute DET coordinates and put them into EVENTS bintable.
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
 *	CHANGE HISTORY:
 *        0.1.0 - RF 14/12/15 - First version
 *
 *
 *      Return Status:
 *        0: OK
 *        1: NOT_OK
 */
int ComputeDETCoords(FitsFileUnit_t evunit, FitsFileUnit_t ounit)
{
    unsigned           	FromRow, ReadRows, n,nCols, OutRows=0;
    /*
	int                	rawx, rawy;
    double             	time;
    */
	EvtCol_t           	indxcol;
    Bintable_t			outtable;
    FitsHeader_t		head;


    TMEMSET0( &outtable, Bintable_t );
    TMEMSET0( &head, FitsHeader_t );

    head=RetrieveFitsHeader(evunit);

    GetBintableStructure(&head, &outtable, BINTAB_ROWS, 0, NULL);

    if(!outtable.MaxRows)
    {
        headas_chat(NORMAL, "%s: Warning: %s file is empty.\n", global.taskname, global.par.infile);
        global.warning=1;
        return OK;
    }

    nCols=outtable.nColumns;



    /* RAWX */
    if ((indxcol.RAWX=ColNameMatch(CLNM_RAWX, &outtable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWX);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
        goto reco_end;
    }

    /* RAWY */
    if ((indxcol.RAWY=ColNameMatch(CLNM_RAWY, &outtable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_RAWY);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
        goto reco_end;
    }

    /* TIME */
    if ((indxcol.TIME=ColNameMatch(CLNM_TIME, &outtable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_TIME);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
        goto reco_end;
    }

    /* DET_ID */
    if ((indxcol.DET_ID=ColNameMatch(CLNM_DET_ID, &outtable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_DET_ID);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
        goto reco_end;
    }

    /* GRADE */
    if ((indxcol.GRADE=ColNameMatch(CLNM_GRADE, &outtable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: %s column does not exist\n",  global.taskname, CLNM_GRADE);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.infile);
        goto reco_end;
    }


    /* Add new columns if needed */

    if((indxcol.DET1X=ColNameMatch(CLNM_DET1X, &outtable)) == -1)
    {
        headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_DET1X);
        /*headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);*/
        AddColumn(&head, &outtable, CLNM_DET1X, CARD_COMM_DET1X, "1D", TNONE);
        indxcol.DET1X=ColNameMatch(CLNM_DET1X, &outtable);
    }

    if((indxcol.DET1Y=ColNameMatch(CLNM_DET1Y, &outtable)) == -1)
    {
        headas_chat(CHATTY, "%s: Info: %s column will be added and filled\n",  global.taskname, CLNM_DET1Y);
        /*headas_chat(CHATTY, "%s: Info: in '%s' file.\n", global.taskname, global.par.outfile);*/
        AddColumn(&head, &outtable, CLNM_DET1Y, CARD_COMM_DET1Y, "1D", TNONE);
        indxcol.DET1Y=ColNameMatch(CLNM_DET1Y, &outtable);
    }


    /* Add history */
    GetGMTDateTime(global.date);
    if(global.hist)
    {
        sprintf(global.strhist, "File modified by '%s' (%s) at %s: computed PHA.", global.taskname, global.nustardas_v,global.date );
        AddHistory(&head, global.strhist);

    }

    EndBintableHeader(&head, &outtable);

    /* write out new header to new file */
    FinishBintableHeader(ounit, &head, &outtable);



    FromRow = 1;
    ReadRows= outtable.nBlockRows;
    OutRows = 0;

    /* Read input bintable */
    while(ReadBintable(evunit, &outtable, nCols, NULL,FromRow,&ReadRows) == 0 )
    {
        for(n=0; n<ReadRows; ++n)
        {
			/*
            time = DVEC(outtable,n,indxcol.TIME);
            rawx = BVEC(outtable,n,indxcol.RAWX);
            rawy = BVEC(outtable,n,indxcol.RAWY);
			*/
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

reco_end:
    if (head.first)
        ReleaseBintable(&head, &outtable);

    return NOT_OK;

} /* ComputeDETCoords */



void createNameChuTempFile(const char *nameFileSorg,int num_extension,char *sliptChuFileName)
{
    char 	_preSteam[PATH_MAX + 1],_steamout[PATH_MAX];
    int		num_pid;

    num_pid=getpid();
    num_extension=num_extension+1;

    refreshStemOut(nameFileSorg,global.par.stemout,sliptChuFileName);

    preSteamOut(_preSteam);
    getStemOut(sliptChuFileName,_steamout);

    sprintf(sliptChuFileName,"%s%s_chu%d_%d.fits",_preSteam,_steamout,num_extension,num_pid);

    /*
    headas_chat(NORMAL, "CHU File sorg: %s\n",global.par.chu123hkfile);
    headas_chat(NORMAL, "CHU File dest: %s\n",sliptChuFileName);
    */
} /* createNameChuTempFile */

void createNameChuMergeFile(const char *nameFileSorg,char *sliptChuFileName)
{
    char 	_preSteam[PATH_MAX + 1],_steamout[PATH_MAX];
    int		num_pid;

    num_pid=getpid();
    refreshStemOut(nameFileSorg,global.par.stemout,sliptChuFileName);

    preSteamOut(_preSteam);
    getStemOut(sliptChuFileName,_steamout);

    sprintf(sliptChuFileName,"%s%s_chu123_merge_%d.fits",_preSteam,_steamout,num_pid);

    /*
    headas_chat(NORMAL, "File sorg: %s\n",nameFileSorg);
    headas_chat(NORMAL, "File dest: %s\n",sliptChuFileName);
    headas_chat(NORMAL, "_preSteam: %s\n",_preSteam);
    headas_chat(NORMAL, "_steamout: %s\n",_steamout);
    */

} /* createNameChuMergeFile */

void createNameChuGTIFile(const char *nameFileSorg,int N,char *gtiChuFileName)
{
		char 	_preSteam[PATH_MAX + 1],_steamout[PATH_MAX];
		int		num_pid;
		
		num_pid=getpid();
		refreshStemOut(nameFileSorg,global.par.stemout,gtiChuFileName);

		preSteamOut(_preSteam);
		getStemOut(gtiChuFileName,_steamout);

		sprintf(gtiChuFileName,"%s%s_chu%d_gti_%d.fits",_preSteam,_steamout,N,num_pid);

		
		/*
		headas_chat(NORMAL, "File sorg         : %s\n",nameFileSorg);
		headas_chat(NORMAL, "global.par.stemout: %s\n",global.par.stemout);
		headas_chat(NORMAL, "_preSteam         : %s\n",_preSteam);
		headas_chat(NORMAL, "_steamout         : %s\n",_steamout);
		headas_chat(NORMAL, "File dest         : %s\n",gtiChuFileName);
		*/
} /* createNameChuGTIFile */

void createNameEVTFile(const char *nameFileSorg,int N,char *evtFileName)
{
    char 	_preSteam[PATH_MAX + 1],_steamout[PATH_MAX],instr[4],eventStrictMode[3];

    if ( !strcasecmp(global.par.splitmode,SPLITMODE_NORMAL) )
    {
        strcpy(eventStrictMode,"_N");
    }
    else if ( !strcasecmp(global.par.splitmode,SPLITMODE_STRICT) )
    {
        strcpy(eventStrictMode,"_S");
    }

    if ( !strcasecmp(global.evt.instrume,KWVL_INSTRUME_FPMA) )
    {
        strcpy(instr,"A");
    }
    else if ( !strcasecmp(global.evt.instrume,KWVL_INSTRUME_FPMB) )
    {
        strcpy(instr,"B");
    }

    refreshStemOut(nameFileSorg,global.par.stemout,evtFileName);

    preSteamOut(_preSteam);
    getStemOut(evtFileName,_steamout);
	/* headas_chat(NORMAL, "%s: Info: _steamout='%s'\n", global.taskname, _steamout); */
    sprintf(evtFileName,"%s%s%s06_chu%d%s_cl.evt",_preSteam,_steamout,instr,N,eventStrictMode);
} /* createNameEVTFile */

int splitChuFile()
{
    char	cmd[BUF_SIZE],nameChuOutputFile[BUF_SIZE];
    int	status = OK;

    /* SPLIT File */
    createNameChuTempFile(global.par.chu123hkfile,ID_CHU1,nameChuOutputFile);

    sprintf(cmd, "ftsort infile='%s[CHU1]' outfile=%s columns=TIME method=heap memory=YES unique=YES copyall=NO clobber=YES chatter=1 history=NO",
            global.par.chu123hkfile,
            nameChuOutputFile);
	
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    fflush(stdout);
    status = system(cmd);

    if(status!=0)
    {
        headas_chat(NORMAL, "%s: Error: unable to create temporary CHU File for extension '%s'\n", global.taskname,EXTNAME_CHU1);
        goto Error;
    }


    createNameChuTempFile(global.par.chu123hkfile,ID_CHU2,nameChuOutputFile);
    sprintf(cmd, "ftsort infile='%s[CHU2]' outfile=%s columns=TIME method=heap memory=YES unique=YES copyall=NO clobber=YES chatter=1 history=NO",
            global.par.chu123hkfile,
            nameChuOutputFile);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    fflush(stdout);
    status = system(cmd);

    if(status!=0)
    {
        headas_chat(NORMAL, "%s: Error: unable to create temporary CHU File for extension '%s'\n", global.taskname,EXTNAME_CHU2);
        goto Error;
    }

    createNameChuTempFile(global.par.chu123hkfile,ID_CHU3,nameChuOutputFile);
    sprintf(cmd, "ftsort infile='%s[CHU3]' outfile=%s columns=TIME method=heap memory=YES unique=YES copyall=NO clobber=YES chatter=1 history=NO",
            global.par.chu123hkfile,
            nameChuOutputFile);
    headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);
    fflush(stdout);
    status = system(cmd);

    if(status!=0)
    {
        headas_chat(NORMAL, "%s: Error: unable to create temporary CHU File for extension '%s'\n", global.taskname,EXTNAME_CHU3);
        goto Error;
    }

    return OK;

Error:

    return NOT_OK;
} /* splitChuFile */

int getIndexMergeCol(ChuMerge_t *_chuObj)
{
    if ((_chuObj->indxcolMerge.TIME = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_TIME)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error MERGE: '%s' column does not exist\n", global.taskname, CLNM_TIME);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CHUQ1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CHUQ1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CHUQ1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.SEQUENCE1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_SEQUENCE1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SEQUENCE1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CORR1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CORR1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CORR1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CHUID1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CHUID1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CHUID1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.HIRATE1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_HIRATE1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_HIRATE1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.BBO1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_BBO1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BBO1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.TIMEREF1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_TIMEREF1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIMEREF1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.VALID1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_VALID1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_VALID1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.RESIDUAL1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_RESIDUAL1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RESIDUAL1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.LOCKS1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_LOCKS1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LOCKS1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.OBJECTS1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_OBJECTS1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OBJECTS1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.STARSFAIL1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_STARSFAIL1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STARSFAIL1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.AGCFLOOR1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_AGCFLOOR1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AGCFLOOR1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.AGCCEIL1 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_AGCCEIL1)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AGCCEIL1);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CHUQ2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CHUQ2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CHUQ2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.SEQUENCE2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_SEQUENCE2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SEQUENCE2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CORR2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CORR2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CORR2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CHUID2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CHUID2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CHUID2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.HIRATE2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_HIRATE2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_HIRATE2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.BBO2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_BBO2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BBO2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.TIMEREF2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_TIMEREF2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIMEREF2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.VALID2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_VALID2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_VALID2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.RESIDUAL2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_RESIDUAL2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RESIDUAL2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.LOCKS2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_LOCKS2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LOCKS2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.OBJECTS2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_OBJECTS2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OBJECTS2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.STARSFAIL2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_STARSFAIL2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STARSFAIL2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.AGCFLOOR2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_AGCFLOOR2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AGCFLOOR2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.AGCCEIL2 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_AGCCEIL2)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AGCCEIL2);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CHUQ3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CHUQ3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CHUQ3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.SEQUENCE3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_SEQUENCE3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SEQUENCE3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CORR3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CORR3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CORR3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.CHUID3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_CHUID3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CHUID3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.HIRATE3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_HIRATE3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_HIRATE3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.BBO3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_BBO3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BBO3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.TIMEREF3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_TIMEREF3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIMEREF3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.VALID3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_VALID3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_VALID3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.RESIDUAL3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_RESIDUAL3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RESIDUAL3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.LOCKS3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_LOCKS3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LOCKS3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.OBJECTS3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_OBJECTS3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OBJECTS3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.STARSFAIL3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_STARSFAIL3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STARSFAIL3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.AGCFLOOR3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_AGCFLOOR3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AGCFLOOR3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    if ((_chuObj->indxcolMerge.AGCCEIL3 = GetColNameIndx(&(_chuObj->chuTableMerge), CLNM_AGCCEIL3)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AGCCEIL3);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }


    return OK;

Error:

    return NOT_OK;
} /* getIndexMergeCol */

int getIndexCol(ChuMerge_t *_chuObj)
{
    if ((_chuObj->indxcol.TIME = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_TIME)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error CHU: '%s' column does not exist\n", global.taskname, CLNM_TIME);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.CHUQ = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_CHUQ)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CHUQ);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.SEQUENCE = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_SEQUENCE)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SEQUENCE);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.CORR = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_CORR)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CORR);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.CHUID = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_CHUID)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_CHUID);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.HIRATE = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_HIRATE)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_HIRATE);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.BBO = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_BBO)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_BBO);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.TIMEREF = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_TIMEREF)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_TIMEREF);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.VALID = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_VALID)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_VALID);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.RESIDUAL = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_RESIDUAL)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RESIDUAL);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.LOCKS = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_LOCKS)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_LOCKS);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.OBJECTS = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_OBJECTS)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_OBJECTS);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.STARSFAIL = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_STARSFAIL)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_STARSFAIL);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.AGCFLOOR = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_AGCFLOOR)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AGCFLOOR);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    if ((_chuObj->indxcol.AGCCEIL = GetColNameIndx(&(_chuObj->chuTable[ID_CHU1]), CLNM_AGCCEIL)) == -1 )
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_AGCCEIL);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, _chuObj->tempChuFileName[ID_CHU1]);
        goto Error;
    }

    return OK;

Error:

    return NOT_OK;
}  /* getIndexCol */

void iGetBintableStructure(ChuMerge_t *_chuObj, int chuId)
{
    FitsCard_t   *card;

    TMEMSET0( &(_chuObj->chuTable[chuId]), Bintable_t );
    TMEMSET0( &(_chuObj->chuHead[chuId]), FitsHeader_t );

    _chuObj->chuHead[chuId]=RetrieveFitsHeader(_chuObj->chuFileFU[chuId]);
    GetBintableStructure(&(_chuObj->chuHead[chuId]), &(_chuObj->chuTable[chuId]), 1, 0, NULL);

    _chuObj->nCols[chuId]=_chuObj->chuTable[chuId].nColumns;
    _chuObj->maxRow_chu[chuId]=_chuObj->chuTable[chuId].MaxRows;

    /*
    headas_chat(NORMAL,"%s: Info: lette %d righe sul file fits: %s\n", global.taskname,_chuObj->maxRow_chu[chuId], _chuObj->tempChuFileName[chuId]);
    */

    _chuObj->rowValue[chuId]=malloc(_chuObj->nCols[chuId]*sizeof(unsigned));

    if (ExistsKeyWord(&(_chuObj->chuHead[chuId]), KWNM_MJDREFI, &card))
    {
        _chuObj->mjdrefi[chuId]=card->u.JVal;
    }
    else
    {
        headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_MJDREFI);
        headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, _chuObj->tempChuFileName[chuId]);
        _chuObj->mjdrefi[chuId]=0;
    }

    if (ExistsKeyWord(&(_chuObj->chuHead[chuId]), KWNM_MJDREFF, &card))
    {
        _chuObj->mjdreff[chuId]=card->u.DVal;
    }
    else
    {
        headas_chat(NORMAL, "%s: Warning: %s keyword not found\n", global.taskname, KWNM_MJDREFF);
        headas_chat(NORMAL, "%s: Warning: in %s file.\n", global.taskname, _chuObj->tempChuFileName[chuId]);
        _chuObj->mjdreff[chuId]=0.0;
    }

}/* iGetBintableStructure */

void iGetBintableMergeStructure(ChuMerge_t *_chuObj)
{

    TMEMSET0( &(_chuObj->chuTableMerge), Bintable_t );
    TMEMSET0( &(_chuObj->chuHeadMerge), FitsHeader_t );

    _chuObj->chuHeadMerge=RetrieveFitsHeader(_chuObj->chuFileMergeFU);

    GetBintableStructure(&(_chuObj->chuHeadMerge), &(_chuObj->chuTableMerge), 1, 0, NULL);

}/* iGetBintableMergeStructure */

int readRowChuFile(ChuMerge_t *_chuObj,int chuId)
{
    unsigned 	ReadRows;

    if(!_chuObj->flagRead[chuId])
    {
        return OK;
    }

    if(!_chuObj->chuTable[chuId].MaxRows)
    {
        headas_chat(NORMAL, "%s: Error: %s file is empty.\n", global.taskname, _chuObj->tempChuFileName[chuId]);
        goto Error;
    }
    else
    {
        ReadRows=1;
        ReadBintable(_chuObj->chuFileFU[chuId], &(_chuObj->chuTable[chuId]),_chuObj->nCols[chuId], NULL,_chuObj->rowChu[chuId],&ReadRows);

        _chuObj->time[chuId] 					= DVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.TIME);
        DVECVEC_ARRAY_READ(_chuObj->chuq[chuId], 4, _chuObj->chuTable[chuId], 0, _chuObj->indxcol.CHUQ);
        _chuObj->rowValue[chuId][_chuObj->indxcol.SEQUENCE]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.SEQUENCE);
        _chuObj->rowValue[chuId][_chuObj->indxcol.CORR]		= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.CORR);
        _chuObj->rowValue[chuId][_chuObj->indxcol.CHUID]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.CHUID);
        _chuObj->rowValue[chuId][_chuObj->indxcol.HIRATE]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.HIRATE);
        _chuObj->rowValue[chuId][_chuObj->indxcol.BBO]		= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.BBO);
        _chuObj->rowValue[chuId][_chuObj->indxcol.TIMEREF]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.TIMEREF);
        _chuObj->rowValue[chuId][_chuObj->indxcol.VALID]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.VALID);
        _chuObj->rowValue[chuId][_chuObj->indxcol.RESIDUAL]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.RESIDUAL);
        _chuObj->rowValue[chuId][_chuObj->indxcol.LOCKS]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.LOCKS);
        _chuObj->rowValue[chuId][_chuObj->indxcol.OBJECTS]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.OBJECTS);
        _chuObj->rowValue[chuId][_chuObj->indxcol.STARSFAIL]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.STARSFAIL);
        _chuObj->rowValue[chuId][_chuObj->indxcol.AGCFLOOR]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.AGCFLOOR);
        _chuObj->rowValue[chuId][_chuObj->indxcol.AGCCEIL]	= BVEC(_chuObj->chuTable[chuId],0,_chuObj->indxcol.AGCCEIL);

        /*printRow(_chuObj,chuId);*/

    }

    /* EndBintableHeader(&(_chuObj->chuHead[chuId]), &(_chuObj->chuTable[chuId])); */

    return OK;

Error:
    return NOT_OK;
} /* readRowChuFile */

void printRow(ChuMerge_t *_chuObj,int chuId)
{
    headas_chat(NORMAL,"%s [row=%d,chuid=%d] --> TIME=%f;CHUQ[%f,%f,%f,%f];SEQUENCE=%d;CORR=%d;CHUID=%d;HIRATE=%d;BBO=%d\n",
                global.taskname,
                _chuObj->rowChu[chuId],
                chuId,
                _chuObj->time[chuId],
                _chuObj->chuq[chuId][0],
                _chuObj->chuq[chuId][1],
                _chuObj->chuq[chuId][2],
                _chuObj->chuq[chuId][3],
                _chuObj->rowValue[chuId][_chuObj->indxcol.SEQUENCE],
                _chuObj->rowValue[chuId][_chuObj->indxcol.CORR],
                _chuObj->rowValue[chuId][_chuObj->indxcol.CHUID],
                _chuObj->rowValue[chuId][_chuObj->indxcol.HIRATE],
                _chuObj->rowValue[chuId][_chuObj->indxcol.BBO]);


} /* printRow */

int initChuFileTemp(ChuMerge_t *_chuObj)
{
    int 	status=OK;
    char	nameChuOutputFile[BUF_SIZE];

    /* Read CHU Temporary File for merge */
    createNameChuTempFile(global.par.chu123hkfile,ID_CHU1,nameChuOutputFile);
    sprintf(_chuObj->tempChuFileName[ID_CHU1],"%s",nameChuOutputFile);
    if ((_chuObj->chuFileFU[ID_CHU1]=OpenReadFitsFile(_chuObj->tempChuFileName[ID_CHU1])) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL, "%s:  Couldn't open files '%s'\n",global.taskname,nameChuOutputFile);
        goto Error;
    }
    else
    {
        /* Move in EXTNAME_CHU1 extension in input chuFileFU_1 file */
        if (fits_movnam_hdu(_chuObj->chuFileFU[ID_CHU1], ANY_HDU, EXTNAME_CHU1, 0, &status))
        {
            headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,EXTNAME_CHU1);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, nameChuOutputFile);
            if( CloseFitsFile(_chuObj->chuFileFU[ID_CHU1]))
            {
                headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
                headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, nameChuOutputFile);
                goto Error;
            }
            goto Error;
        }
        else
        {
            headas_chat(NORMAL,"%s: Info: Find '%s' extension in\n", global.taskname,EXTNAME_CHU1);
        }
    }

    createNameChuTempFile(global.par.chu123hkfile,ID_CHU2,nameChuOutputFile);
    sprintf(_chuObj->tempChuFileName[ID_CHU2],"%s",nameChuOutputFile);
    if ((_chuObj->chuFileFU[ID_CHU2]=OpenReadFitsFile(_chuObj->tempChuFileName[ID_CHU2])) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL, "%s:  Couldn't open files '%s'\n",global.taskname,nameChuOutputFile);
        goto Error;
    }
    else
    {
        /* Move in EXTNAME_CHU2 extension in input chuFileFU_2 file */
        if (fits_movnam_hdu(_chuObj->chuFileFU[ID_CHU2], ANY_HDU, EXTNAME_CHU2, 0, &status))
        {
            headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,EXTNAME_CHU2);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, nameChuOutputFile);
            if( CloseFitsFile(_chuObj->chuFileFU[ID_CHU2]))
            {
                headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
                headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, nameChuOutputFile);
                goto Error;
            }
            goto Error;
        }
        else
        {
            headas_chat(NORMAL,"%s: Info: Find '%s' extension in\n", global.taskname,EXTNAME_CHU2);
        }
    }

    createNameChuTempFile(global.par.chu123hkfile,ID_CHU3,nameChuOutputFile);
    sprintf(_chuObj->tempChuFileName[ID_CHU3],"%s",nameChuOutputFile);
    if ((_chuObj->chuFileFU[ID_CHU3]=OpenReadFitsFile(_chuObj->tempChuFileName[ID_CHU3])) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL, "%s:  Couldn't open files '%s'\n",global.taskname,nameChuOutputFile);
        goto Error;
    }
    else
    {
        /* Move in EXTNAME_CHU3 extension in input chuFileFU_3 file */
        if (fits_movnam_hdu(_chuObj->chuFileFU[ID_CHU3], ANY_HDU, EXTNAME_CHU3, 0, &status))
        {
            headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", global.taskname,EXTNAME_CHU3);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n",global.taskname, nameChuOutputFile);
            if( CloseFitsFile(_chuObj->chuFileFU[ID_CHU3]))
            {
                headas_chat(NORMAL,"%s: Error: Unable to close\n", global.taskname);
                headas_chat(NORMAL,"%s: Error: '%s' file.\n ", global.taskname, nameChuOutputFile);
                goto Error;
            }
            goto Error;
        }
        else
        {
            headas_chat(NORMAL,"%s: Info: Find '%s' extension in\n", global.taskname,EXTNAME_CHU3);
        }
    }

    return OK;
Error:

    return NOT_OK;

} /* initChuFileTemp */

int writeChuMerge(ChuMerge_t *_chuObj)
{
    int chuId;



    DVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.TIME) = _chuObj->time[_chuObj->indexMinTime];

    chuId=ID_CHU1;
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.SEQUENCE1) = _chuObj->rowValue[chuId][_chuObj->indxcol.SEQUENCE];
    DVECVEC_ARRAY_WRITE(_chuObj->chuq[chuId], 4, _chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CHUQ1);
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CORR1) = _chuObj->rowValue[chuId][_chuObj->indxcol.CORR];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CHUID1) = _chuObj->rowValue[chuId][_chuObj->indxcol.CHUID];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.HIRATE1) = _chuObj->rowValue[chuId][_chuObj->indxcol.HIRATE];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.BBO1) = _chuObj->rowValue[chuId][_chuObj->indxcol.BBO];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.TIMEREF1) = _chuObj->rowValue[chuId][_chuObj->indxcol.TIMEREF];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.VALID1) = _chuObj->rowValue[chuId][_chuObj->indxcol.VALID];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.RESIDUAL1) = _chuObj->rowValue[chuId][_chuObj->indxcol.RESIDUAL];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.LOCKS1) = _chuObj->rowValue[chuId][_chuObj->indxcol.LOCKS];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.OBJECTS1) = _chuObj->rowValue[chuId][_chuObj->indxcol.OBJECTS];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.STARSFAIL1) = _chuObj->rowValue[chuId][_chuObj->indxcol.STARSFAIL];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.AGCFLOOR1) = _chuObj->rowValue[chuId][_chuObj->indxcol.AGCFLOOR];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.AGCCEIL1) = _chuObj->rowValue[chuId][_chuObj->indxcol.AGCCEIL];

    chuId=ID_CHU2;
    DVECVEC_ARRAY_WRITE(_chuObj->chuq[chuId], 4, _chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CHUQ2);
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.SEQUENCE2) = _chuObj->rowValue[chuId][_chuObj->indxcol.SEQUENCE];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CORR2) = _chuObj->rowValue[chuId][_chuObj->indxcol.CORR];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CHUID2) = _chuObj->rowValue[chuId][_chuObj->indxcol.CHUID];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.HIRATE2) = _chuObj->rowValue[chuId][_chuObj->indxcol.HIRATE];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.BBO2) = _chuObj->rowValue[chuId][_chuObj->indxcol.BBO];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.TIMEREF2) = _chuObj->rowValue[chuId][_chuObj->indxcol.TIMEREF];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.VALID2) = _chuObj->rowValue[chuId][_chuObj->indxcol.VALID];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.RESIDUAL2) = _chuObj->rowValue[chuId][_chuObj->indxcol.RESIDUAL];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.LOCKS2) = _chuObj->rowValue[chuId][_chuObj->indxcol.LOCKS];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.OBJECTS2) = _chuObj->rowValue[chuId][_chuObj->indxcol.OBJECTS];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.STARSFAIL2) = _chuObj->rowValue[chuId][_chuObj->indxcol.STARSFAIL];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.AGCFLOOR2) = _chuObj->rowValue[chuId][_chuObj->indxcol.AGCFLOOR];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.AGCCEIL2) = _chuObj->rowValue[chuId][_chuObj->indxcol.AGCCEIL];

    chuId=ID_CHU3;
    DVECVEC_ARRAY_WRITE(_chuObj->chuq[chuId], 4, _chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CHUQ3);
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.SEQUENCE3) = _chuObj->rowValue[chuId][_chuObj->indxcol.SEQUENCE];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CORR3) = _chuObj->rowValue[chuId][_chuObj->indxcol.CORR];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.CHUID3) = _chuObj->rowValue[chuId][_chuObj->indxcol.CHUID];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.HIRATE3) = _chuObj->rowValue[chuId][_chuObj->indxcol.HIRATE];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.BBO3) = _chuObj->rowValue[chuId][_chuObj->indxcol.BBO];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.TIMEREF3) = _chuObj->rowValue[chuId][_chuObj->indxcol.TIMEREF];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.VALID3) = _chuObj->rowValue[chuId][_chuObj->indxcol.VALID];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.RESIDUAL3) = _chuObj->rowValue[chuId][_chuObj->indxcol.RESIDUAL];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.LOCKS3) = _chuObj->rowValue[chuId][_chuObj->indxcol.LOCKS];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.OBJECTS3) = _chuObj->rowValue[chuId][_chuObj->indxcol.OBJECTS];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.STARSFAIL3) = _chuObj->rowValue[chuId][_chuObj->indxcol.STARSFAIL];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.AGCFLOOR3) = _chuObj->rowValue[chuId][_chuObj->indxcol.AGCFLOOR];
    BVEC(_chuObj->chuTableMerge, 0, _chuObj->indxcolMerge.AGCCEIL3) = _chuObj->rowValue[chuId][_chuObj->indxcol.AGCCEIL];

    _chuObj->rowWrite_chu123++;

    WriteFastBintable(_chuObj->chuFileMergeFU, &(_chuObj->chuTableMerge), 1, FALSE);

    return OK;

} /* writeChuMerge */

int mergeChuFile()
{
    int            	status = OK,cont = 0,i = 0;
    double 		minore = 0.0;
    ChuMerge_t 		chuObj;

    status=initChuFileTemp(&chuObj);

    initIndex(&chuObj);
	

    iGetBintableStructure(&chuObj,ID_CHU1);
    iGetBintableStructure(&chuObj,ID_CHU2);
    iGetBintableStructure(&chuObj,ID_CHU3);

    status=getIndexCol(&chuObj);
    status=updateKeyChuMerge(&chuObj);
    iGetBintableMergeStructure(&chuObj);
	
    status=getIndexMergeCol(&chuObj);

    if(global.par.timecut==TRUE)
    {
        while(checkIndex(&chuObj)==OK)
        {
            /*
            printIndex(&chuObj,0);
            */
            status=readRowChuFile(&chuObj,ID_CHU1);
            status=readRowChuFile(&chuObj,ID_CHU2);
            status=readRowChuFile(&chuObj,ID_CHU3);

            minore=checkTime(&chuObj);

            if( minore==TIME_EQUAL )
            {
                cont++;
                /*printIndex(&chuObj,1);*/
                status=writeChuMerge(&chuObj);
                incAll(&chuObj);
            }
            else
            {
                incMinor(&chuObj,minore);
            }

        }
    }
    else if(global.par.timecut==FALSE)
    {
        while(checkIndex(&chuObj)==OK)
        {

            status=readRowChuFile(&chuObj,ID_CHU1);
            status=readRowChuFile(&chuObj,ID_CHU2);
            status=readRowChuFile(&chuObj,ID_CHU3);
	    
			/* printIndex(&chuObj,4); */

            minore=checkTime(&chuObj);

            if(cont==0) /* First row */
            {
                if( minore == TIME_EQUAL) /* First row && CHU1==CHU2==CHU3 */
                {
                    /*printIndex(&chuObj,3);*/
                    status=writeChuMerge(&chuObj);
                }
                cont++;
                incAll(&chuObj);
            }
            else /* Next row */
            {

                if( minore!=TIME_EQUAL )
                {
                    /* Fix minor & decrese others index */
                    decOthers(&chuObj,minore);
                    status=readRowChuFile(&chuObj,ID_CHU1);
                    status=readRowChuFile(&chuObj,ID_CHU2);
                    status=readRowChuFile(&chuObj,ID_CHU3);
                    /*printIndex(&chuObj,2);*/
                    status=writeChuMerge(&chuObj);
                }
                else /* CHU1==CHU2==CHU3*/
                {
                    /*printIndex(&chuObj,1);*/
                    status=writeChuMerge(&chuObj);
                }
                cont++;
                incAll(&chuObj);
            }
        }
    }

    headas_chat(NORMAL, "%s: Info: Write '%d' rows in file '%s'.\n", global.taskname,chuObj.rowWrite_chu123,global.merge_chu123hkfile);

    /* Update checksum and datasum keywords */
    if (ChecksumCalc(chuObj.chuFileMergeFU))
    {
        headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }


    /* close  output files */
    if ( CloseFitsFile(chuObj.chuFileMergeFU) )
    {
        headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }


    for(i=0; i<NUM_EXTENSION; i++)
    {
        if( CloseFitsFile(chuObj.chuFileFU[i]))
        {
            headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, chuObj.tempChuFileName[i]);
            goto Error;
        }
    }

    if(global.par.cleanup==FALSE)
    {
        /* Add Key MJDREFI,MJDREFF at CHU temporary file CHU1,CHU2,CHU3 */
        status=AddKeyPrimatyExtensionChuFile(&chuObj);
        if(status!=0)
        {
            goto Error;
        }
    }


    EndBintableHeader(&chuObj.chuHead[ID_CHU1], &chuObj.chuTable[ID_CHU1]);
    EndBintableHeader(&chuObj.chuHead[ID_CHU2], &chuObj.chuTable[ID_CHU2]);
    EndBintableHeader(&chuObj.chuHead[ID_CHU3], &chuObj.chuTable[ID_CHU3]);
    EndBintableHeader(&chuObj.chuHeadMerge, &chuObj.chuTableMerge);


    ReleaseBintable(&chuObj.chuHead[ID_CHU1], &chuObj.chuTable[ID_CHU1]);
    ReleaseBintable(&chuObj.chuHead[ID_CHU2], &chuObj.chuTable[ID_CHU2]);
    ReleaseBintable(&chuObj.chuHead[ID_CHU3], &chuObj.chuTable[ID_CHU3]);


    return OK;


Error:

    return NOT_OK;
} /* mergeChuFile */

void  readChuColType(ChuColDef_t *_chuColDef)
{
    strcpy(_chuColDef->colNameOrig[0],CLNM_TIME);
    strcpy(_chuColDef->colNameOrig[1],CLNM_CHUQ);
    strcpy(_chuColDef->colNameOrig[2],CLNM_SEQUENCE);
    strcpy(_chuColDef->colNameOrig[3],CLNM_CORR);
    strcpy(_chuColDef->colNameOrig[4],CLNM_CHUID);
    strcpy(_chuColDef->colNameOrig[5],CLNM_HIRATE);
    strcpy(_chuColDef->colNameOrig[6],CLNM_BBO);
    strcpy(_chuColDef->colNameOrig[7],CLNM_TIMEREF);
    strcpy(_chuColDef->colNameOrig[8],CLNM_VALID);
    strcpy(_chuColDef->colNameOrig[9],CLNM_RESIDUAL);
    strcpy(_chuColDef->colNameOrig[10],CLNM_LOCKS);
    strcpy(_chuColDef->colNameOrig[11],CLNM_OBJECTS);
    strcpy(_chuColDef->colNameOrig[12],CLNM_STARSFAIL);
    strcpy(_chuColDef->colNameOrig[13],CLNM_AGCFLOOR);
    strcpy(_chuColDef->colNameOrig[14],CLNM_AGCCEIL);

    strcpy(_chuColDef->colType[0],"D");
    strcpy(_chuColDef->colType[1],"4D");
    strcpy(_chuColDef->colType[2],"B");
    strcpy(_chuColDef->colType[3],"B");
    strcpy(_chuColDef->colType[4],"B");
    strcpy(_chuColDef->colType[5],"B");
    strcpy(_chuColDef->colType[6],"B");
    strcpy(_chuColDef->colType[7],"B");
    strcpy(_chuColDef->colType[8],"B");
    strcpy(_chuColDef->colType[9],"B");
    strcpy(_chuColDef->colType[10],"B");
    strcpy(_chuColDef->colType[11],"B");
    strcpy(_chuColDef->colType[12],"B");
    strcpy(_chuColDef->colType[13],"I");
    strcpy(_chuColDef->colType[14],"I");

    strcpy(_chuColDef->colUnit[0],UNIT_SEC);
    strcpy(_chuColDef->colUnit[1],UNIT_NA);
    strcpy(_chuColDef->colUnit[2],UNIT_NA);
    strcpy(_chuColDef->colUnit[3],UNIT_NA);
    strcpy(_chuColDef->colUnit[4],UNIT_NA);
    strcpy(_chuColDef->colUnit[5],UNIT_NA);
    strcpy(_chuColDef->colUnit[6],UNIT_NA);
    strcpy(_chuColDef->colUnit[7],UNIT_NA);
    strcpy(_chuColDef->colUnit[8],UNIT_NA);
    strcpy(_chuColDef->colUnit[9],UNIT_ARCSEC);
    strcpy(_chuColDef->colUnit[10],UNIT_CT);
    strcpy(_chuColDef->colUnit[11],UNIT_CT);
    strcpy(_chuColDef->colUnit[12],UNIT_CT);
    strcpy(_chuColDef->colUnit[13],UNIT_NA);
    strcpy(_chuColDef->colUnit[14],UNIT_NA);

} /* readChuColType */

int updateKeyChuMerge(ChuMerge_t *_chuObj)
{
    int 						nkeys;
    int 						ii,k;
    char 						card_key[FLEN_CARD];
    int 						status=OK;
    char 						colName[BUF_SIZE];
    char 						colKeyName[BUF_SIZE];
    int 						naxis2=0;
    char 						extname[FLEN_CARD];

    ChuColDef_t			chuColDef;

    Bintable_t			table;
    FitsHeader_t		head,head_input;
    FitsCard_t    	*card;
    FitsFileUnit_t  inunit=NULL;

    TMEMSET0( &table, Bintable_t );
    TMEMSET0( &head, FitsHeader_t );
    TMEMSET0( &head_input, FitsHeader_t );

    readChuColType(&chuColDef);
    /* createNameChuMergeFile(global.par.chu123hkfile,global.merge_chu123hkfile); */


    /* Open chu123hkfile input file */
    if ((inunit=OpenReadFitsFile(global.par.chu123hkfile)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, global.par.chu123hkfile);
        goto Error;
    }

    /* Open chu123hkfile merge output file  */
    if ((_chuObj->chuFileMergeFU=OpenWriteFitsFile(global.merge_chu123hkfile)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    /* Move to input file primary header to copy it in new file */
    if(fits_movabs_hdu(inunit, 1, NULL, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to move in primary header\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.chu123hkfile);
        goto Error;
    }

    if(fits_copy_hdu(inunit, _chuObj->chuFileMergeFU, 0, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to copy primary header to\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.merge_chu123hkfile);
        goto Error;
    }

    /* Create a new bintable header and get pointer to it */
    head = NewBintableHeader(BINTAB_ROWS, &table);

    /* Move to input file primary header to copy it in new file */


    if(fits_movabs_hdu(inunit, 2, NULL, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to move in primary header\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.chu123hkfile);
        goto Error;
    }

    head_input 	= RetrieveFitsHeader(inunit);

    fits_get_hdrspace(inunit, &nkeys, NULL, &status);


    /* Get all comment CHU1 extension for TTYPEn */

    if(ExistsKeyWord(&head_input,"TTYPE1", &card))
    {
        sprintf(chuColDef.colComment[0],"%s %s %s ",card->Comment,card->Comment,card->Comment);
    }


    for(k=1; k<NUM_COLUMNS; k++)
    {
        sprintf(colKeyName,"%s%d","TTYPE",k+1);

        if(ExistsKeyWord(&head_input,colKeyName, &card))
        {
            /*
            headas_chat(NORMAL, "%s: colKeyName=%s colComment[%d]=%s\n", global.taskname,colKeyName,k,card->Comment);
            */
            sprintf(chuColDef.colComment[k],"%s",card->Comment);
        }
        else
        {
            headas_chat(NORMAL, "%s: NOT FOUND colKeyName=%s \n", global.taskname,colKeyName);
        }
    }


    AddColumn(&head, &table, chuColDef.colNameOrig[0], chuColDef.colComment[0], chuColDef.colType[0], TUNIT,  chuColDef.colUnit[0], CARD_COMM_PHYSUNIT);
    AddComment(&head,"TIME refers to the midpoint of the starlight integration-interval.");
    AddComment(&head,"Refer to the MicroASC User Manual and TM/TC ICD");
    AddComment(&head,"'for detailed documentation: ASC-DTU-MA-3001 and ASC-DTU-ICD-3004.");

    for(k=1; k<NUM_COLUMNS; k++)
    {
        sprintf(colName,"%s%d",chuColDef.colNameOrig[k],ID_CHU1+1);
        AddColumn(&head, &table, colName, chuColDef.colComment[k], chuColDef.colType[k], TUNIT, chuColDef.colUnit[k], CARD_COMM_PHYSUNIT);

    }

    for(k=1; k<NUM_COLUMNS; k++)
    {
        sprintf(colName,"%s%d",chuColDef.colNameOrig[k],ID_CHU2+1);
        AddColumn(&head, &table, colName, chuColDef.colComment[k], chuColDef.colType[k], TUNIT, chuColDef.colUnit[k], CARD_COMM_PHYSUNIT);

    }

    for(k=1; k<NUM_COLUMNS; k++)
    {
        sprintf(colName,"%s%d",chuColDef.colNameOrig[k],ID_CHU3+1);
        AddColumn(&head, &table, colName, chuColDef.colComment[k], chuColDef.colType[k], TUNIT, chuColDef.colUnit[k], CARD_COMM_PHYSUNIT);

    }


    /* Finish bintable header */
    EndBintableHeader(&head, &table);

    /* Write bintable in file */
    FinishBintableHeader(_chuObj->chuFileMergeFU, &head, &table);

    /* Move to output merge file to CHU123 extension */
    if(fits_movabs_hdu(_chuObj->chuFileMergeFU, 2, NULL, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to move in primary header\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.chu123hkfile);
        goto Error;
    }


    /* Move to input file CHU1 header to copy it in new file */
    if(fits_movabs_hdu(inunit, 2, NULL, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to move in primary header\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, global.par.chu123hkfile);
        goto Error;
    }

    /* fits_get_hdrspace(inunit, &nkeys, NULL, &status); */

    /*
    headas_chat(NORMAL, "%s: Info %d key in CHU1.\n", global.taskname, nkeys);
    */

    /* Copy header keywords for CHU1 extension*/
    for (ii = 62; ii < nkeys ; ii++)
    {
        fits_read_record(inunit, ii, card_key, &status); /* read keyword */
        /*
        headas_chat(NORMAL, "%s: [KEY #=%d] %s\n", global.taskname, ii, card_key);
        */
        fits_write_record(_chuObj->chuFileMergeFU, card_key, &status); /* write keyword */
    }

    /* Modifay key standard NAXIS2 */
    if(fits_update_key(_chuObj->chuFileMergeFU, TINT, KWNM_NAXIS2, &naxis2, CARD_COMM_NAXIS2, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, CARD_COMM_NAXIS2);
        goto Error;
    }

    /* Modifay key standard EXTNAME */
    strcpy(extname,"CHU123");
    if(fits_update_key(_chuObj->chuFileMergeFU, TSTRING, KWNM_EXTNAME, &extname, CARD_COMM_EXTNAME, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to update %s keyword\n", global.taskname, CARD_COMM_EXTNAME);
        goto Error;
    }



    return OK;

Error:
    return NOT_OK;
} /* updateKeyChuMerge */

double checkTime(ChuMerge_t *_chuObj)
{
    int i;
    double t1,t2,t3,minTime=0.0,sens=0.0000001;

    t1=_chuObj->time[ID_CHU1];
    t2=_chuObj->time[ID_CHU2];
    t3=_chuObj->time[ID_CHU3];


    /*
    headas_chat(NORMAL,"%s: Info t0: %f\n", global.taskname,t1);
    headas_chat(NORMAL,"%s: Info: t1: %f\n", global.taskname,t2);
    headas_chat(NORMAL,"%s: Info: t2: %f\n", global.taskname,t3);
    */


    if ( ( (t1>t2-sens) && (t1<t2+sens) ) && ( (t2>t3-sens) && (t2<t3+sens) ) )
    {
	_chuObj->indexMinTime=ID_CHU1;
        return TIME_EQUAL;
    }
    else
    {
        for(i=0; i<NUM_EXTENSION; i++)
        {
            if(_chuObj->flagRead[i]==TRUE)
            {
                minTime=_chuObj->time[i];
		_chuObj->indexMinTime=i;
                break;
            }
        }

        for(i=0; i<NUM_EXTENSION; i++)
        {
            if(_chuObj->flagRead[i]==FALSE)
            {
                continue;
            }
            if(_chuObj->time[i]<minTime )
            {
                minTime=_chuObj->time[i];
		_chuObj->indexMinTime=i;
            }
        }
        return minTime;
    }
} /* checkTime */

void initIndex(ChuMerge_t *_chuObj)
{
    _chuObj->rowChu[ID_CHU1]=1;
    _chuObj->rowChu[ID_CHU2]=1;
    _chuObj->rowChu[ID_CHU3]=1;

    _chuObj->rowWrite_chu123=0;
    _chuObj->flagRead[ID_CHU1]=TRUE;
    _chuObj->flagRead[ID_CHU2]=TRUE;
    _chuObj->flagRead[ID_CHU3]=TRUE;

} /* initIndex */

void incMinor(ChuMerge_t *_chuObj,double minore)
{
    int i;
    double sens=0.0000001;

    for(i=0; i<NUM_EXTENSION; i++)
    {
        if((minore > _chuObj->time[i]-sens) && (minore < _chuObj->time[i]+sens) )
        {
            _chuObj->rowChu[i]++;
        }
    }
} /* incMinor */

void decOthers(ChuMerge_t *_chuObj,double minore)
{
    int i;
    double sens=0.0000001;

    for(i=0; i<NUM_EXTENSION; i++)
    {
        if((minore > _chuObj->time[i]-sens) && (minore < _chuObj->time[i]+sens) )
        {
            /* conserva l'indice */
        } else
        {
            _chuObj->rowChu[i]--;
        }
    }
} /* decOthers */

void printIndex(ChuMerge_t *_chuObj,int flag)
{
    double t1,t2,t3;

    t1=_chuObj->time[ID_CHU1];
    t2=_chuObj->time[ID_CHU2];
    t3=_chuObj->time[ID_CHU3];
    switch(flag)
    {
      case 0:
      {
	  headas_chat(NORMAL,"%s: GENERAL index[row='%d'][%d, %d, %d][%f,%f,%f][min time(%d)=%f]\n\n", global.taskname,_chuObj->rowWrite_chu123+1,_chuObj->rowChu[ID_CHU1],_chuObj->rowChu[ID_CHU2],_chuObj->rowChu[ID_CHU3],t1,t2,t3,_chuObj->indexMinTime,_chuObj->time[_chuObj->indexMinTime]);
      }break;
      
      case 1:
      {
	  headas_chat(NORMAL,"%s: EQUAL index[row='%d'][%d, %d, %d][%f,%f,%f][min time(%d)=%f]\n\n", global.taskname,_chuObj->rowWrite_chu123+1,_chuObj->rowChu[ID_CHU1],_chuObj->rowChu[ID_CHU2],_chuObj->rowChu[ID_CHU3],t1,t2,t3,_chuObj->indexMinTime,_chuObj->time[_chuObj->indexMinTime]);
      }break;
      
      case 2:
      {
	  headas_chat(NORMAL,"%s: MINOR index[row='%d'][%d, %d, %d][%f,%f,%f][min time(%d)=%f]\n\n", global.taskname,_chuObj->rowWrite_chu123+1,_chuObj->rowChu[ID_CHU1],_chuObj->rowChu[ID_CHU2],_chuObj->rowChu[ID_CHU3],t1,t2,t3,_chuObj->indexMinTime,_chuObj->time[_chuObj->indexMinTime]);
      }break;
      
      case 3:
      {
	  headas_chat(NORMAL,"%s: FIRST EQUAL index[row='%d'][%d, %d, %d][%f,%f,%f][min time(%d)=%f]\n\n", global.taskname,_chuObj->rowWrite_chu123+1,_chuObj->rowChu[ID_CHU1],_chuObj->rowChu[ID_CHU2],_chuObj->rowChu[ID_CHU3],t1,t2,t3,_chuObj->indexMinTime,_chuObj->time[_chuObj->indexMinTime]);
      }break;
      
      case 4:
      {
	  headas_chat(NORMAL,"%s: READ  index[row='%d'][%d, %d, %d][%f,%f,%f]\n", global.taskname,_chuObj->rowWrite_chu123+1,_chuObj->rowChu[ID_CHU1],_chuObj->rowChu[ID_CHU2],_chuObj->rowChu[ID_CHU3],t1,t2,t3);
      }break;
    }
} /* printIndex */

void incAll(ChuMerge_t *_chuObj)
{
    _chuObj->rowChu[ID_CHU1]++;
    _chuObj->rowChu[ID_CHU2]++;
    _chuObj->rowChu[ID_CHU3]++;
    /*
     *
        headas_chat(NORMAL,"%s: rowChu[%d, %d, %d]\n", global.taskname,_chuObj->rowChu[ID_CHU1],_chuObj->rowChu[ID_CHU2],_chuObj->rowChu[ID_CHU3]);
        headas_chat(NORMAL,"%s: max_rowChu[%d, %d, %d]\n", global.taskname,_chuObj->maxRow_chu[ID_CHU1],_chuObj->maxRow_chu[ID_CHU2],_chuObj->maxRow_chu[ID_CHU3]);
    */

} /* incAll */

int cleanNuSplitScTempFile(CmdNuSplitSC_t *_cmd)
{
    unsigned	extNumber[NUM_EXTENSION];
    char	extName[NUM_EXTENSION][20];
    int 	i = 0;
    char 	tempChuFileName[BUF_SIZE];

    extNumber[0]=ID_CHU1;
    extNumber[1]=ID_CHU2;
    extNumber[2]=ID_CHU3;
	
    sprintf(extName[0],"%s",EXTNAME_CHU1);
    sprintf(extName[1],"%s",EXTNAME_CHU2);
    sprintf(extName[2],"%s",EXTNAME_CHU3);

    /* Remove split chu file */
    for (i=0; i<NUM_EXTENSION; i++)
    {
        createNameChuTempFile(global.par.chu123hkfile,extNumber[i],tempChuFileName);

        if (FileExists(tempChuFileName))
        {
            headas_chat(NORMAL, "%s: Info: Remove split chu file '%s'\n", global.taskname, tempChuFileName);
            if(remove (tempChuFileName) == -1)
            {
                headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
                headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, tempChuFileName);
                /* goto check_end; */
            }
        }

    }

    /* Remove merge chu file */
    if (FileExists(global.merge_chu123hkfile))
    {
        headas_chat(NORMAL, "%s: Info: Remove merge chu file '%s'\n", global.taskname, global.merge_chu123hkfile);
        if(remove (global.merge_chu123hkfile) == -1)
        {
            headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, global.merge_chu123hkfile);
            /* goto check_end; */
        }
    }

    /* Remove gti chu file */
    for (i=0; i<NUM_EXPRSCREENING; i++)
    {
        if (FileExists(_cmd->gti_chu123hkfile[i]))
        {
            headas_chat(NORMAL, "%s: Info: Remove gti file '%s'\n", global.taskname,_cmd->gti_chu123hkfile[i]);
            if(remove (_cmd->gti_chu123hkfile[i]) == -1)
            {
                headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
                headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, _cmd->gti_chu123hkfile[i]);
                /* goto check_end; */
            }
        }
    }

    /* Remove xco file */
    for (i=0; i<NUM_EXPRSCREENING; i++)
    {
        if (FileExists(_cmd->xselectFile[i]))
        {
            headas_chat(NORMAL, "%s: Info: Remove xco file '%s'\n", global.taskname,_cmd->xselectFile[i]);
            if(remove (_cmd->xselectFile[i]) == -1)
            {
                headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
                headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, _cmd->xselectFile[i]);
                /* goto check_end; */
            }

        }
    }
    /* Remove xco log file */
    if (FileExists(_cmd->xselectLogFile))
    {
        headas_chat(NORMAL, "%s: Info: Remove xco log file '%s'\n", global.taskname,_cmd->xselectLogFile);
        if(remove (_cmd->xselectLogFile) == -1)
        {
            headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, _cmd->xselectLogFile);
            /* goto check_end; */
        }

    }

    /* Remove xco time file */
    if (FileExists(_cmd->xselectTimeFile))
    {
        headas_chat(NORMAL, "%s: Info: Remove xco time file '%s'\n", global.taskname,_cmd->xselectTimeFile);
        if(remove (_cmd->xselectTimeFile) == -1)
        {
            headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, _cmd->xselectTimeFile);
            /* goto check_end; */
        }

    }

    return OK;
} /* cleanNuSplitScTempFile */

int checkIndex(ChuMerge_t *_chuObj)
{
    int i, status = OK;


    if(global.par.timecut==TRUE)
    {
        for(i=0; i<NUM_EXTENSION; i++)
        {
            if(_chuObj->rowChu[i]>_chuObj->maxRow_chu[i])
            {
                return NOT_OK;
            }
        }

        return OK;
    }
    else if(global.par.timecut==FALSE)
    {

        for(i=0; i<NUM_EXTENSION; i++)
        {
            if(_chuObj->rowChu[i]>_chuObj->maxRow_chu[i])
            {
                _chuObj->flagRead[i]=FALSE;
            }
        }

        status = NOT_OK;
        for(i=0; i<NUM_EXTENSION; i++)
        {
            if(_chuObj->rowChu[i]<=_chuObj->maxRow_chu[i])
            {
                return OK;
            }
        }

        return status;

    }

    return NOT_OK;
} /* checkIndex */

int AddKeyPrimatyExtensionChuFile(ChuMerge_t *chuObj)
{
    FitsFileUnit_t      outunit=NULL;
    FitsHeader_t	      head;
    int                 status = OK,i;

    for(i=0; i<NUM_EXTENSION; i++)
    {
        /* Open chu output file */
        if ((outunit=OpenReadWriteFitsFile(chuObj->tempChuFileName[i])) <= (FitsFileUnit_t )0)
        {
            headas_chat(NORMAL,"%s: Error: Unable to create\n", global.taskname);
            headas_chat(NORMAL,"%s: Error: '%s' file. \n", global.taskname, chuObj->tempChuFileName[i] );
            goto Error;
        }

        /* Move to input file primary header to copy it in new file */
        if(fits_movabs_hdu(outunit, 1, NULL, &status))
        {
            headas_chat(NORMAL, "%s: Error: Unable to move in primary header\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, chuObj->tempChuFileName[i]);
            goto Error;
        }

        /* Retrieve header pointer */
        head=RetrieveFitsHeader(outunit);

        AddCard(&head, KWNM_MJDREFI, J, &(chuObj->mjdrefi[i]), CARD_COMM_MJDREFI);
        AddCard(&head, KWNM_MJDREFF, D, &(chuObj->mjdreff[i]), CARD_COMM_MJDREFF);

        if(WriteUpdatedHeader(outunit, &head))
        {
            headas_chat(NORMAL, "%s: Error: Unable to update primary HDU\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, chuObj->tempChuFileName[i] );
            goto Error;
        }


        /* Update checksum and datasum keywords in all extensions */
        if (ChecksumCalc(outunit))
        {
            headas_chat(NORMAL, "%s: Error: Unable to update CHECKSUM and DATASUM\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: in '%s' file. \n ", global.taskname, chuObj->tempChuFileName[i] );
            goto Error;
        }

        if (CloseFitsFile(outunit))
        {
            headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: '%s' file.\n ", chuObj->tempChuFileName[i] );
            goto Error;
        }
    }

    return OK;

Error:
    return NOT_OK;

} /* AddKeyPrimatyExtensionChuFile */

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

void preSteamOut(char* dest)
{
		
      sprintf(dest,"%s/",global.par.outdir);
		
} /* preSteamOut */

void preSteam(const char *path,char* dest)
{
    char *pch; 
    int pos;
    pch=strrchr(path,'/');
    if(pch!=NULL)
    {
        pos=pch-path+1;
        strncpy(dest,path,pos);
        dest[pos]='\0';
    }
    else
    {
        strcpy(dest,"");
    }  
} /* preSteam */

void getStemOut(const char *path,char* dest)
{
    char * pch;
    int pos1;
    pch=strrchr(path,'/');

    if(pch!=NULL)
    {
        pos1=pch-path+1;
    }
    else
    {
        pos1=0;
    }
	
	if(!strcasecmp(global.par.stemout,DF_STEAMOUT))
    {
        subString(path,pos1,LEN_STEMOUT,dest);
    }
    else
    {
        subString(path,pos1,strlen(global.par.stemout),dest);
    }
		
} /* getStemOut */

void postSteam(const char *path,char* dest)
{
    char *pch;
    int pos;
    pch=strrchr(path,'_');
    if(pch!=NULL)
    {
        pos=pch-path;
        subString(path,pos,strlen(path)-pos,dest);
    }
    else
    {
        strcpy(dest,"");
    }
}/* postSteam */

void refreshStemOut(const char *path,const char* par_stemout,char* dest)
{
    char _preSteam[PATH_MAX+1],_steamout[PATH_MAX],_postSteam[PATH_MAX];

    preSteam(path,_preSteam);
    getStemOut(path,_steamout);
    postSteam(path,_postSteam);

	sprintf(dest,"%s%s%s",_preSteam,_steamout,_postSteam);
	
    if(!strcasecmp(par_stemout,DF_STEAMOUT)) /* DEFAULT */
    {
        sprintf(dest,"%s%s%s",_preSteam,_steamout,_postSteam);
    }
    else /* Passato da parametro */
    {
        sprintf(dest,"%s%s%s",_preSteam,par_stemout,_postSteam);
    }
} /* refreshStemOut */


int makeExprScreening(char *name_ExprScreening,char *expr)
{
    Bintable_t		intable;
    FitsHeader_t	head;
    FitsFileUnit_t     	inunit=NULL;
    Chu123rangefile_T	col;
    int 		status = OK, n;
    char 		nome_file[BUF_SIZE];
    unsigned		FromRow, ReadRows;

    char			parName[50];		/* CLNM_PARNAME */
    char			parRange[50];		/* CLNM_RANGE */
    char 			parSyntax[50];		/* CLNM_SYNTAX*/
    unsigned	parGroup,parGroup_pre;	/* CLNM_GROUP */
    char			parGroupSyntax[50];	/* CLNM_GROUP_SYNTAX */


    TMEMSET0( &intable, Bintable_t );
    TMEMSET0( &head, FitsHeader_t );

    strcpy(nome_file,global.par.chu123rangefile);
    
    /* Open readonly file */
    if ((inunit=OpenReadFitsFile(nome_file)) <= (FitsFileUnit_t )0)
    {
        headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
        headas_chat(NORMAL,"%s: Error: eccolo '%s' file.\n", global.taskname, nome_file);
        return NOT_OK;
    }
    else
    {
        headas_chat(NORMAL,"%s: Info: Open '%s' file and move in HDU '%s'.\n", global.taskname, nome_file,name_ExprScreening);
    }



    /* Move to input file numExprScreening */
    if(fits_movnam_hdu(inunit, ANY_HDU, name_ExprScreening, 0, &status))
    {
        headas_chat(NORMAL, "%s: Error: Unable to move in HDU '%s' .\n", global.taskname,name_ExprScreening);
        headas_chat(NORMAL, "%s: Error: on '%s' file.\n", global.taskname, nome_file);
        goto Error;
    }

    head=RetrieveFitsHeader(inunit);
    GetBintableStructure(&head, &intable, BINTAB_ROWS, 0, NULL);


    if ((col.PARNAME=ColNameMatch(CLNM_PARNAME, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_PARNAME);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, nome_file);
        goto Error;
    }

    if ((col.RANGE=ColNameMatch(CLNM_RANGE, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_RANGE);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, nome_file);
        goto Error;
    }

    if ((col.SYNTAX=ColNameMatch(CLNM_SYNTAX_PAR, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SYNTAX_PAR);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, nome_file);
        goto Error;
    }

    if ((col.GROUP=ColNameMatch(CLNM_GROUP, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_GROUP);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, nome_file);
        goto Error;
    }

    if ((col.GROUP_SYNTAX=ColNameMatch(CLNM_SYNTAX_GROUP, &intable)) == -1)
    {
        headas_chat(NORMAL, "%s: Error: '%s' column does not exist\n", global.taskname, CLNM_SYNTAX_GROUP);
        headas_chat(NORMAL, "%s: Error: in '%s' file. \n", global.taskname, nome_file);
        goto Error;
    }

    EndBintableHeader(&head, &intable);

    FromRow = 1;
    ReadRows=intable.nBlockRows;

    /* Read input bintable */
    parGroup_pre = -1;
    strcpy(expr,"(");

    /* headas_chat(NORMAL,"%s: Info: try load expression screening num rows=%d.\n", global.taskname,intable.MaxRows); */


    while(ReadBintable(inunit, &intable, NUM_COLUMNS_CHU123RANGEFILE, NULL,FromRow,&ReadRows) == 0 )
    {
        /* headas_chat(NORMAL,"%s: Info: Read %d rows.\n", global.taskname,ReadRows); */

        for(n=0; n<ReadRows; ++n)
        {
            strcpy(parName, SVEC(intable, n, col.PARNAME));
            strcpy(parRange, SVEC(intable, n, col.RANGE));
            strcpy(parSyntax, SVEC(intable, n, col.SYNTAX));
            parGroup = BVEC(intable, n, col.GROUP);             
            strcpy(parGroupSyntax, SVEC(intable, n, col.GROUP_SYNTAX));

/*
            headas_chat(NORMAL,"%s: Info: parName='%s'\n", global.taskname,parName); 
            headas_chat(NORMAL,"%s: Info: parRange='%s'\n", global.taskname,parRange); 
            headas_chat(NORMAL,"%s: Info: parSyntax='%s'\n", global.taskname,parSyntax); 
            headas_chat(NORMAL,"%s: Info: parGroup='%d'\n", global.taskname,parGroup); 
            headas_chat(NORMAL,"%s: Info: parGroupSyntax='%s'\n", global.taskname,parGroupSyntax); 
            headas_chat(NORMAL,"%s: Info: parGroup %d .\n", global.taskname,parGroup);
            headas_chat(NORMAL,"%s: Info: parGroup_pre %d .\n", global.taskname,parGroup_pre);
*/            

            if (parGroup!=parGroup_pre)
            {
                if(parGroup_pre!=-1)
                {
                    strcat(expr,")");
                    strcat(expr,SVEC(intable, n-1, col.GROUP_SYNTAX));
                    strcat(expr,"(");
                    strcat(expr,parName);
                    strcat(expr,parRange);
                    strcat(expr,parSyntax);

                }
                else
                {
                    strcat(expr,parName);
                    strcat(expr,parRange);
                    strcat(expr,parSyntax);
                    /* headas_chat(NORMAL,"%s: Info: parGroup successivo %d .\n", global.taskname,BVEC(intable, n+1, col.GROUP)); */
                    /*
                    if(BVEC(intable, n+1, col.GROUP)!=parGroup)
                    {
                        strcat(expr,parSyntax);
                    }
                    */
                }
                parGroup_pre=parGroup;
            }
            else
            {
                strcat(expr,parName);
                strcat(expr,parRange);

                if(BVEC(intable, n+1, col.GROUP)==parGroup)
                {
                    /* headas_chat(NORMAL,"%s: Info: parGroup_next %d .\n", global.taskname,BVEC(intable, n+1, col.GROUP)); */
                    strcat(expr,parSyntax);
                }
            }


            /* headas_chat(NORMAL,"%s: Info: expr parziale =%s\n\n\n", global.taskname,expr); */

        }

        FromRow += ReadRows;
        ReadRows = BINTAB_ROWS;
    }

    strcat(expr,")");

    /* headas_chat(NORMAL,"%s: Info: expr=%s\n", global.taskname,expr); */


    ReleaseBintable(&head, &intable);

    /* close input file */
    if (CloseFitsFile(inunit))
    {
        headas_chat(NORMAL, "%s: Error: Unable to close\n", global.taskname);
        headas_chat(NORMAL, "%s: Error: '%s' file.\n ", global.taskname, nome_file);
        return NOT_OK;
    }

    return OK;

Error:
    CloseFitsFile(inunit);
    return NOT_OK;
} /* makeExprScreening */

int makeOutputEvtFileName(CmdNuSplitSC_t *_cmd)
{
	int i = 0;
	/* Expression screening number setup */
    _cmd->N[0]=1;
    _cmd->N[1]=2;
    _cmd->N[2]=3;
    _cmd->N[3]=12;
    _cmd->N[4]=13;
    _cmd->N[5]=23;
    _cmd->N[6]=123;
	
	for(i=0; i<NUM_EXPRSCREENING; i++)
    {
        createNameEVTFile(global.par.infile,_cmd->N[i],_cmd->output_evtFile[i]);
		/* headas_chat(NORMAL, "%s: Info: _cmd->output_evtFile[%d]='%s'.\n", global.taskname,i,_cmd->output_evtFile[i]); */ 
	}
	
	return OK;

} /* makeOutputEvtFileName */

void extractNameFile(char *sorg,char *dest)
{
		int pos = 0, len = 0;
		char path_infile[PATH_MAX];
		
		preSteam(sorg,path_infile);
		pos=strlen(path_infile);
		
		len=strlen(sorg);
		
		if(pos>=len)
		{
			pos=0;
		}
		
		subString(sorg,pos,len-pos,dest);
} /* extractNameFile */

int makeCMD(CmdNuSplitSC_t *_cmd)
{
    int i = 0;
	int		num_pid;
	char postrf[20],name_chu_ext[10],path_infile[PATH_MAX],name_infile[PATH_MAX];
    FILE *fp;

    createNameChuMergeFile(global.par.chu123hkfile,global.merge_chu123hkfile);

    /* Expression screening number setup */
    /*
	_cmd->N[0]=1;
    _cmd->N[1]=2;
    _cmd->N[2]=3;
    _cmd->N[3]=12;
    _cmd->N[4]=13;
    _cmd->N[5]=23;
    _cmd->N[6]=123;
	*/

    /* Expression screening */
    for(i=0; i<NUM_EXPRSCREENING; i++)
    {
    		sprintf(name_chu_ext,"CHU%d",_cmd->N[i]);
    		makeExprScreening(name_chu_ext,_cmd->exprScreening[i]);
    }
    
    /* headas_chat(NORMAL, "%s: Info: splitmode='%s'.\n", global.taskname,global.par.splitmode); */
	
    /* Create tmp var for command line*/
    if ( !strcasecmp(global.par.splitmode,SPLITMODE_NORMAL) )
    {
        strcpy(postrf,"0.999999");
    }
    else if ( !strcasecmp(global.par.splitmode,SPLITMODE_STRICT) )
    {
        strcpy(postrf,"0");
    }

    /* Generate maketime command */
    for(i=0; i<NUM_EXPRSCREENING; i++)
    {
        createNameChuGTIFile(global.par.chu123hkfile,
                             _cmd->N[i],
                             _cmd->gti_chu123hkfile[i]);


        sprintf(_cmd->makeTimeCmd[i],"maketime time=TIME prefr=0 postfr=%s expr='%s' infile='%s[1]' outfile='%s' clobber=no compact=no",
                postrf,
                _cmd->exprScreening[i],
                global.merge_chu123hkfile,
                _cmd->gti_chu123hkfile[i]);

        /*headas_chat(NORMAL, "%s: Info: maketime[%d]=%s.\n", global.taskname,i,_cmd->makeTimeCmd[i]);*/
    }
	
    extractNameFile(global.par.infile,name_infile);	
	preSteam(global.par.infile,path_infile);
	
	if(strcmp(path_infile,"")==STR_EQUAL)
	{
		strcpy(path_infile,global.par.outdir);
	}
    
    /* Generate xselect_chuN.xco file and xselect command */
	num_pid=getpid();
    sprintf(_cmd->xselectLogFile,"%s","xselect.log");
    sprintf(_cmd->xselectTimeFile,"%s","xsel_timefile.asc");
      
	/*headas_chat(NORMAL, "%s: Info: name_infile='%s'.\n", global.taskname,name_infile);
	headas_chat(NORMAL, "%s: Info: path_infile='%s'.\n", global.taskname,path_infile);*/
	
	
    for(i=0; i<NUM_EXPRSCREENING; i++)
    {
        sprintf(_cmd->xselectFile[i],"%s/xselect_chu%d_%d.xco",global.par.outdir,_cmd->N[i],num_pid);    
        sprintf(_cmd->xselectCmd[i],"xselect @%s",_cmd->xselectFile[i]);
		
		/*headas_chat(NORMAL, "%s: Info: xselectFile='%s'.\n", global.taskname,_cmd->xselectFile[i]);
		headas_chat(NORMAL, "%s: Info: xselectCmd='%s'.\n", global.taskname,_cmd->xselectCmd[i]);*/


        fp=fopen(_cmd->xselectFile[i], "w");
        if(fp!=NULL)
        {
				fprintf(fp,"xsel%d\n",num_pid);
				fprintf(fp,"%s\n","read event");
				fprintf(fp,"%s\n",path_infile);
				fprintf(fp,"%s\n",name_infile);
				fprintf(fp,"%s\n","yes");
				fprintf(fp,"filter time file %s\n",_cmd->gti_chu123hkfile[i]);
				fprintf(fp,"%s\n","extract events copyall=yes");
				fprintf(fp,"save events %s\n",_cmd->output_evtFile[i]);
				fprintf(fp,"%s\n","no");
				fprintf(fp,"%s\n","quit");
				fprintf(fp,"%s","no");
				fflush(fp);
				fclose(fp);
        } 
        else
        {
				goto Error;
        }

        /* headas_chat(NORMAL, "%s: Info: xselect[%d]='%s'\n", global.taskname,i,_cmd->xselectCmd[i]);*/
    }

    /* Generate nulivetime command */
    for(i=0; i<NUM_EXPRSCREENING; i++)
    {
			sprintf(_cmd->nulivetimeCmd[i],"nulivetime clobber=no outfile=NONE chatter=3 hkfile='%s' infile='%s' history=yes",
			global.par.hkfile,
			_cmd->output_evtFile[i]);
        
			/* headas_chat(NORMAL, "%s: Info: nulivetime[%d]=%s.\n", global.taskname,i,_cmd->nulivetimeCmd[i]); */
    }

    return OK;

Error:
    return NOT_OK;
}/* makeCMD */

int execMaketime(CmdNuSplitSC_t *_cmd)
{
    int 	status = OK,i = 0;
    char 	cmd[BUF_SIZE];

    for (i=0; i<NUM_EXPRSCREENING; i++)
    {

        sprintf(cmd, "%s",_cmd->makeTimeCmd[i]);
        headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);

        fflush(stdout);
        status = status | system(cmd);

        if(status!=0)
        {
            headas_chat(NORMAL, "%s: Error: exec '%s' \n", global.taskname,cmd);
			goto Error;
        }

    }

    return OK;
Error:
	return NOT_OK;
}/* execMaketime */

int execXselect(CmdNuSplitSC_t *_cmd)
{
    int 	status = OK,i = 0,len = 0;
    char 	cmd[BUF_SIZE];

    for (i=0; i<NUM_EXPRSCREENING; i++)
    {

        len=strlen(_cmd->xselectCmd[i]);
        if(len>0)
        {
            sprintf(cmd, "%s",_cmd->xselectCmd[i]);
            headas_chat(NORMAL, "%s: Info: executing '%s'.\n", global.taskname, cmd);

            fflush(stdout);
            status = status | system(cmd);

            if(status!=0)
            {
                headas_chat(NORMAL, "%s: Error: exec '%s'.\n", global.taskname,cmd);
				goto Error;
            }
        }
        else
        {
            headas_chat(NORMAL, "%s: Info: file '%s' is empty.\n", global.taskname,_cmd->gti_chu123hkfile[i]);
        }
    }

    return OK;
Error:
	return NOT_OK;
}/* execXselect */

int execNulivetime(CmdNuSplitSC_t *_cmd)
{
    int 	status = OK,i = 0,len = 0;
    char 	cmd[BUF_SIZE];

    for (i=0; i<NUM_EXPRSCREENING; i++)
    {
        len=strlen(_cmd->nulivetimeCmd[i]);
        if(len>0)
        {
            sprintf(cmd, "%s",_cmd->nulivetimeCmd[i]);
            headas_chat(NORMAL, "%s: Info: executing '%s'\n", global.taskname, cmd);

            fflush(stdout);
            status = status | system(cmd);

            if(status!=0)
            {
                headas_chat(NORMAL, "%s: Error: exec '%s' \n", global.taskname,cmd);
				goto Error;
            }
        }
    }

    return OK;
Error:
	return NOT_OK;
}/* execNulivetime */


int loadCALDBfile(char *ext_name)
{
    long	extfile=-1;

    /* Derive CALDB alignment filename */
    if ( !strcasecmp(global.par.chu123rangefile,DF_CALDB) )
    {
    		if (CalGetFileName(HD_MAXRET, global.obsinfo.dateobs, global.obsinfo.timeobs, global.obsinfo.dateend, global.obsinfo.timeend, KWVL_CHU123RANGE, global.par.chu123rangefile, ext_name , &extfile, KWVL_INSTRUME_FPM, HD_DETNAM))
        {
            headas_chat(NORMAL, "%s: Error: Unable to query CALDB for alignfile parameter.\n", global.taskname);
            goto Error;
        }
    }

    return OK;

Error:
    return NOT_OK;
}/* loadCALDBfile */

int GetObsInfo(char *filename, char *extname)
{

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

    /* Retrieve date-obs and time-obs from input file  */
    if (ExistsKeyWord(&head, KWNM_DATEOBS, &card))
    {
        global.obsinfo.dateobs=card->u.SVal;
        if(!(strchr(global.obsinfo.dateobs, 'T')))
        {
            if (ExistsKeyWord(&head, KWNM_TIMEOBS, &card))
            {
                global.obsinfo.timeobs=card->u.SVal;
            }
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
        global.obsinfo.dateend=card->u.SVal;
        if(!(strchr(global.obsinfo.dateend, 'T')))
        {
            if (ExistsKeyWord(&head, KWNM_TIMEEND, &card))
            {
                global.obsinfo.timeend=card->u.SVal;
            }
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

int checkOutputEmptyfile(CmdNuSplitSC_t *_cmd)
{	
	int            	status = OK,i = 0, totrows = 0;
    FitsHeader_t   	head;
    FitsFileUnit_t 	inunit=NULL;
    Bintable_t		intable;


    TMEMSET0( &intable, Bintable_t );
    TMEMSET0( &head, FitsHeader_t );

    for (i=0; i<NUM_EXPRSCREENING; i++)
    {
	if(strcmp(_cmd->xselectCmd[i],"")==STR_EQUAL) /* il file eventi non è stato generato */
	{
		continue;
	}
		
        /* Open readonly file */
        if ((inunit=OpenReadFitsFile(_cmd->output_evtFile[i])) <= (FitsFileUnit_t )0)
        {
			headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, _cmd->output_evtFile[i]);
            return NOT_OK;
        }



        /* Move to input file ID_GTI */
        if(fits_movabs_hdu(inunit, ID_GTI, NULL, &status))
        {
            headas_chat(NORMAL, "%s: Error: Unable to move in GTI.\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, _cmd->output_evtFile[i]);
            goto Error;
        }

        head=RetrieveFitsHeader(inunit);
        GetBintableStructure(&head, &intable, 1, 0, NULL);
        
		totrows=intable.MaxRows;
	
        EndBintableHeader(&head, &intable);
        CloseFitsFile(inunit);

        if(totrows==0)
        {
	    strcpy(_cmd->xselectCmd[i],"");
	    strcpy(_cmd->nulivetimeCmd[i],"");
            headas_chat(NORMAL, "%s: Info: Remove empty file '%s'.\n", global.taskname,_cmd->output_evtFile[i]);

            /* Remove evt output  file */
            if(remove (_cmd->output_evtFile[i]) == -1)
            {
                headas_chat(NORMAL, "%s: Error: Unable to remove\n", global.taskname);
                headas_chat(NORMAL, "%s: Error: '%s' file.\n", global.taskname, _cmd->output_evtFile[i]);

                goto Error;
            }

        }
        else
	{
		headas_chat(CHATTY, "%s: Info: File '%s' is not empty.\n", global.taskname,_cmd->output_evtFile[i]);
	}

    }


    return OK;

Error:
    CloseFitsFile(inunit);
    return NOT_OK;
} /* checkOutputEmptyfile */

int checkEmptyfile(CmdNuSplitSC_t *_cmd)
{	
	int            	status = OK,i = 0, totrows = 0;
    FitsHeader_t   	head;
    FitsFileUnit_t 	inunit=NULL;
    Bintable_t		intable;


    TMEMSET0( &intable, Bintable_t );
    TMEMSET0( &head, FitsHeader_t );
	
	

    for (i=0; i<NUM_EXPRSCREENING; i++)
    {
        /* Open readonly file */
		
        if ((inunit=OpenReadFitsFile(_cmd->gti_chu123hkfile[i])) <= (FitsFileUnit_t )0)
        {
            headas_chat(NORMAL,"%s: Error: Unable to open\n", global.taskname);
            headas_chat(NORMAL,"%s: Error: '%s' file.\n", global.taskname, _cmd->gti_chu123hkfile[i]);
            return NOT_OK;
        }



        /* Move to input file ID_STDGTI */
        if(fits_movabs_hdu(inunit, ID_STDGTI, NULL, &status))
        {
            headas_chat(NORMAL, "%s: Error: Unable to move in STDGTI.\n", global.taskname);
            headas_chat(NORMAL, "%s: Error: in '%s' file.\n", global.taskname, _cmd->gti_chu123hkfile[i]);
            goto Error;
        }

        head=RetrieveFitsHeader(inunit);
        GetBintableStructure(&head, &intable, 1, 0, NULL);

        totrows=intable.MaxRows;

        /* Set empty file */

        if(totrows==0)
        {
            strcpy(_cmd->xselectCmd[i],"");
            strcpy(_cmd->nulivetimeCmd[i],"");
            headas_chat(NORMAL, "%s: Info: rows number=%d in file '%s'.\n", global.taskname,totrows,_cmd->gti_chu123hkfile[i]);
        }

        EndBintableHeader(&head, &intable);


        CloseFitsFile(inunit);

    }


    return OK;

Error:
    CloseFitsFile(inunit);
    return NOT_OK;
} /* checkEmptyfile */
