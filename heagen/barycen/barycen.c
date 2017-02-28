/**
   \file barycen.c
   \brief Calculate and apply barycenter correction to TIME column.
   \author David Riethmiller, Hans Krimm
   \date $Date: 2016/06/03 19:57:45 $  */

#include "barycen.h"
#include <string.h>
#define REVISION "$Revision: 1.37 $"

int main(int argc, char ** argv){
    PARAM param;
    BARYCORR_DATA bcorr;
    DPLEPH_DATA dpl;
    char version[NEW_FLEN_VALUE];
    get_version(version);

    /* Report the version of barycen */
    printf("  \nBarycenter Correction ('barycen'), v. %s \n\n",version);

    /* Initialize all possible structure variables to zero */
    initialize_structures(&param, &bcorr, &dpl);

    /* Read the parameter file */
    if ( getPars(argc, argv, &param) != 0 ){
        info(&param,ALWAYS,"\nERROR: there was a problem getting the parameters.\n");
        return -1;
    }
        
    /* Set up for barycen to run */
    if ( initialize(&param) != 0 ){
        info(&param,ALWAYS,"\nERROR: there was a problem with the initialization stage.\n");
        return -1;
    }

    /* Report all parameters - some were modified during initialize(). */
    info(&param,LOW,"\n INPUT PARAMETERS AFTER initialize():\n\n");
    info(&param,LOW,"  infile         = %s\n",param.infile);
    info(&param,LOW,"  outfile        = %s\n",param.outfile);
    info(&param,LOW,"  orbfile        = %s\n",param.orbfile);
    info(&param,LOW,"  ra             = %f\n",param.ra);
    info(&param,LOW,"  dec            = %f\n",param.dec);
    info(&param,LOW,"  orbext         = %s\n",param.orbext);
    info(&param,LOW,"  orbform        = %s\n",param.orbform);
    info(&param,LOW,"  orbcol         = %s\n",param.orbcol);
    info(&param,LOW,"  refframe       = %s\n",param.refframe);
    info(&param,LOW,"  orbinterp      = %s\n",param.orbinterp);
    info(&param,LOW,"  timecol        = %s\n",param.timecol);
    info(&param,LOW,"  startcol       = %s\n",param.startcol);
    info(&param,LOW,"  stopcol        = %s\n",param.stopcol);
    info(&param,LOW,"  useweights     = %d\n",param.useweights);
    info(&param,LOW,"  clobber        = %d\n",param.clobber);
    info(&param,LOW,"  mode           = %s\n",param.mode);
    info(&param,LOW,"  chatter        = %d\n",param.chatter);
    info(&param,LOW,"  debug          = %d\n",param.debug);
    info(&param,LOW,"  logfile        = %s\n",param.logfile);
    info(&param,LOW,"  num_orb_cols   = %d\n",param.num_orb_cols);
    
    switch(param.interp_method){
    case 0:
        info(&param,LOW,"  interp_method  = 0: ORBI_UNKNOWN\n");
        break;
    case 1:
        info(&param,LOW,"  interp_method  = 1: ORBI_WEIGHTED\n");
        break;
    case 2:
        info(&param,LOW,"  interp_method  = 2: ORBI_TAYLOR\n");
        break;
    case 3:
        info(&param,LOW,"  interp_method  = 3: ORBI_NEAREST\n");
        break;
    }
    
    info(&param,LOW,"\n");

    /* Execute main workhorse of the barycen tool */
    if ( doWork(&param, &bcorr, &dpl) != 0 ){
        info(&param,1,"\nERROR: there was a problem during the doWork stage.\n");
        return -1;
    }

    /* Clean up memory allocations */
    if ( finalize(&param, &bcorr, &dpl) != 0){
        info(&param,1,"\nERROR: there was a problem during the finalize stage.\n");
        return -1;
    }

    /* Report that barycen has finished */
    info(&param,ALWAYS,"\nFinished with barycen program.\n\n");

    return 0;
}


/* FUNCTION: getPars() 
 * 
 * PURPOSE: read parameter file
 *
 * INPUT: argc, argv:  grab command line input
 *             param:  structure containing parameters
 *
 * OUTPUT: returns 0 for success or -1 for failure
 */ 
int getPars(int argc, char ** argv, PARAM * param){
    int errstatus = 0;
    int num_params = 0;
    int * status_array = 0;
    int ii = 0;
    char * temp = 0;
    char local_clobber_bool;
    char local_debug_bool;
    char local_history_bool;
    char local_useweights_bool;

    char * param_list[] = {"infile",
                           "outfile",
                           "orbfile",
                           "ra",
                           "dec",
                           "orbext",
                           "orbform",
                           "orbcol",
                           "refframe",
                           "orbinterp",
                           "timecol",
                           "startcol",
                           "stopcol",
                           "useweights",
                           "clobber",
                           "chatter",
                           "logfile",
                           "debug",
                           "history",
                           "mode",
                           NULL};

    /* Get the number of non-NULL variables in the param list */
    for (num_params = 0; param_list[num_params] != NULL; num_params++);

    /* Initialize APE */
    errstatus = ape_trad_init(argc, argv);
    if (errstatus != 0) {
        printf("\nERROR: could not initialize APE.  Please check that barycen.par is correctly installed.\n");
        return -1;
    }

    status_array = calloc(num_params, sizeof(int));

    /* APE read parameter infile */
    status_array[ii++] = ape_trad_query_file_name("infile",&temp);
    strcpy(param->infile,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter outfile */
    status_array[ii++] = ape_trad_query_string("outfile",&temp);
    strcpy(param->outfile,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter orbfile */
    status_array[ii++] = ape_trad_query_file_name("orbfile",&temp);
    strcpy(param->orbfile,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter ra */
    status_array[ii++] = ape_trad_query_double("ra",&param->ra);

    /* APE read parameter dec */
    status_array[ii++] = ape_trad_query_double("dec",&param->dec);

    /* APE read orbext */
    status_array[ii++] = ape_trad_query_string("orbext",&temp);
    strcpy(param->orbext,temp);
    free(temp);  /* ape allocates temp, must free! */

    /* APE read orbform */
    status_array[ii++] = ape_trad_query_string("orbform",&temp);
    strcpy(param->orbform,temp);
    free(temp);  /* ape allocates temp, must free! */

    /* APE read orbcol */
    status_array[ii++] = ape_trad_query_string("orbcol",&temp);
    strcpy(param->orbcol,temp);
    free(temp);  /* ape allocates temp, must free! */

    /* APE read parameter refframe */
    status_array[ii++] = ape_trad_query_string("refframe",&temp);
    strcpy(param->refframe,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter orbinterp */
    status_array[ii++] = ape_trad_query_string("orbinterp",&temp);
    strcpy(param->orbinterp,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter timecol */
    status_array[ii++] = ape_trad_query_string("timecol",&temp);
    strcpy(param->timecol,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter startcol */
    status_array[ii++] = ape_trad_query_string("startcol",&temp);
    strcpy(param->startcol,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter orbinterp */
    status_array[ii++] = ape_trad_query_string("stopcol",&temp);
    strcpy(param->stopcol,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter useweights */
    status_array[ii++] = ape_trad_query_bool("useweights", &local_useweights_bool);
    if (local_useweights_bool == 1) {param->useweights = 1;} else {param->useweights = 0;}

    /* APE read parameter clobber */
    status_array[ii++] = ape_trad_query_bool("clobber", &local_clobber_bool);
    if (local_clobber_bool == 1) {param->clobber = 1;} else {param->clobber = 0;}
    
    /* APE read parameter chatter */
    status_array[ii++] = ape_trad_query_int("chatter",&param->chatter);

    /* APE read parameter logfile */
    status_array[ii++] = ape_trad_query_string("logfile",&temp);
    strcpy(param->logfile,temp);
    free(temp); /* ape allocates temp, must free! */

    /* APE read parameter debug */
    status_array[ii++] = ape_trad_query_bool("debug", &local_debug_bool);
    if (local_debug_bool == 1) {param->debug = 1;} else {param->debug = 0;}

    /* APE read parameter history */
    status_array[ii++] = ape_trad_query_bool("history", &local_history_bool);
    if (local_history_bool == 1) {
        param->history = 1;
    } else {
        param->history = 0;
    }

    /* APE read parameter mode */
    status_array[ii++] = ape_trad_query_string("mode",&temp);
    strcpy(param->mode,temp);
    free(temp); /* ape allocates temp, must free! */

    /* Check that we read everything ok */
    for (ii=0; ii<num_params; ii++){
        if (status_array[ii] != eOK){
            printf("problem with entry number %d: %s!\n",ii, param_list[ii]);
            errstatus=-1;
        }
    }

    /* clean up */
    free(status_array);

    return errstatus;
}


/* FUNCTION: initialize_structures()
 *
 * PURPOSE:  initialize all possible structure variables to zero - protects against memory leaks
 *
 * IN/OUTPUT:   param - Data structure containing tool parameters
 *              bcorr - Data structure containing former heagen/barycorr/bary.c global variables
 *              dpl - Data structure containing former heagen/barycorr/dpleph.c global variables
 */
void initialize_structures(PARAM * param, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){
    /* Any memory-checking utility will complain about "uninitialized variables" if we don't do this. */

    /* Read directly from par file */
    strcpy(param->infile,"");        /* Name of input file with time to correct */
    strcpy(param->outfile,"");       /* Name of output file corrected */
    strcpy(param->orbfile,"");       /* Name of satellite orbit file */
    param->ra = 0.0;                 /* Right ascension of position used in the corrction (deg) */
    param->dec = 0.0;                /* Declination of position used in the correction (deg) */
    strcpy(param->orbext,"");        /* Orbit extension */
    strcpy(param->orbform,"");       /* Orbit velocity format (VECTOR|COMPONENTS|KEPLERIAN) */
    strcpy(param->orbcol,"");        /* Orbital columns */
    strcpy(param->refframe,"");      /* Ephemeris reference frame */
    strcpy(param->orbinterp,"");     /* Interpolation method */
    strcpy(param->timecol,"");       /* Name of time column in file */
    strcpy(param->startcol,"");      /* Name of start column in GTI extension */
    strcpy(param->stopcol,"");       /* Name of stop column in GTI extension */
    param->clobber = 0;              /* Overwrite existing output file */
    strcpy(param->mode,"");          /* Mode of automatic parameters */
    param->chatter = 0;              /* Chatter level for output */
    param->debug = 0;                /* Debug messages on */
    strcpy(param->logfile,"");       /* Name of log file, DEFAULT, or NONE; '!' to clobber */
    param->useweights = 0;           /* Use weights to combine duplicate rows in orbit file */


    /* Derived, not in par file */
    param->tstarti = 0;
    param->tstartf = 0.0;
    param->tstopi = 0;
    param->tstopf = 0.0;

    param->num_orb_cols = 0;
    /* char ** orb_col_name; */    /* can't initialize arrays yet */
    /* ORBINTERP interp_method; */  /* can't initialize yet */
    param->get_ra_from_infile = 0;
    param->get_dec_from_infile = 0;
    param->history_string = 0;

    /* Initialize data from the BARYCORR_DATA structure */
    bcorr->c = 0.0;
    bcorr->msol = 0.0;
    bcorr->radsol = 0.0;
    bcorr->denum = 0;
    bcorr->met2day = 0.0;
    bcorr->day2met = 0.0;
    bcorr->met2sec = 0.0;
    bcorr->mjdRefi = 49353;
    bcorr->mjdReff = 6.965740740000E-04;
    bcorr->numleaps = 0;

    /* Initialize data from the DPLEPH_DATA structure */
    dpl->irecsz = 0;
    dpl->ss1 = 0.0;
    dpl->ss2 = 0.0;
    dpl->ss3 = 0.0;
    dpl->ss3inv = 0.0;
    dpl->currec = 0;
    dpl->emratinv = 0.0;
    dpl->buffer = NULL;
    dpl->buflen = 0;
    dpl->nrecs = 0;
    dpl->clight = 0.0;
    dpl->au = 0.0;
    dpl->aufac = 0.0;
    dpl->velfac = 0.0;


}

/* FUNCTION: initialize()
 *
 * PURPOSE:  set up for barycen tool to run
 *
 * IN/OUTPUT: param - Data structure containing tool parameters
 *
 * OUTPUT: return 0 if successful, -1 if failure
 */
int initialize(PARAM * param){
    int errstatus = 0;
    char orbcolbuffer[NEW_FLEN_VALUE];
    int fstat = 0;
    char version[NEW_FLEN_VALUE];
    get_version(version);

    /* Set up the logfile, if not set to NONE */
    if (strcasecmp(param->logfile,"none") != 0){
        int overwrite_log = 0;

        /* If logfile name is preceded by '!', then remove the '!' and flag to overwrite logfile */
        if (param->logfile[0] == '!'){
            memmove(&param->logfile[0], &param->logfile[0 + 1], strlen(param->logfile) - 0);
            overwrite_log = 1;
        }

        /* If the remaining logfile name is 'DEFAULT', then assign a more meaningful log name */
        if (strcasecmp(param->logfile,"DEFAULT") == 0)
            strcpy(param->logfile,"barycen.log");
        
        /* Remove the old log file if flagged to do so */
        if (overwrite_log == 1){
            int file_found = 0;
            fits_file_exists(param->logfile, &file_found, &fstat);
            if ( (file_found == 1) || (file_found == 2) )  remove(param->logfile);
        }

        /* Write a header to the log file */
        FILE *fp = fopen(param->logfile,"a");
        fprintf(fp,"  Barycenter Correction (\"barycen\"), v. %s \n",version);
        fclose(fp);
    }


    /* orbform is a string parameter.  orb_format is an enumeration. */
    param->orb_format = ORBF_UNKNOWN;

    /* Set the number of orbit columns, according to orb format */
    if (strcasecmp(param->orbform,"COMPONENTS") == 0){
        param->num_orb_cols = 6;
        param->orb_format = ORBF_COMPONENTS;
    } else if (strcasecmp(param->orbform,"VECTOR") == 0){
        param->num_orb_cols = 2;
        param->orb_format = ORBF_VECTOR;
    } else if (strcasecmp(param->orbform,"KEPLERIAN") == 0){
        param->num_orb_cols = 6;
        if (param->useweights == 1){
            param->orb_format = ORBF_KEPLERIAN;
        } else {
            param->orb_format = ORBF_KEPLERIAN_NODUP;
        }
    } else {
        info(param,ALWAYS,"Invalid value of orbform: %s\n",param->orbform);
        errstatus = -1;
        return errstatus;
    }

    info(param,LOW,"Orbit format is %s, allocating %d orbit columns.\n",param->orbform,param->num_orb_cols); 
    
    /* Now we can allocate and assign orbit column names */
    /* If COMPONENT, the input column names will probably be X,Y,Z,VX,VY,Z */
    /* If KEPLERIAN, the input column names will probably be A,E,I,AN,AP,MA */
    param->orb_col_name = (char **) calloc(param->num_orb_cols, sizeof(char*));
    
    /* The orbcol string contains a comma-delimited set of sub-strings */
    strcpy(orbcolbuffer,param->orbcol);

    for (int icol=0; icol<param->num_orb_cols; icol++){
        char * token = 0;
        param->orb_col_name[icol] = (char *) calloc(NEW_FLEN_VALUE, sizeof(char));
        if (icol == 0){
            token = strtok(orbcolbuffer,",");
        } else {
            token = strtok(NULL,",");
        }
        strcpy(param->orb_col_name[icol],token);

        info(param,LOW,"param->orb_col_name[%d] = %s\n",icol,param->orb_col_name[icol]);
    }

    /* Set the interpolation method */
    param->interp_method = ORBI_UNKNOWN;

    if (strcasecmp(param->orbinterp,"NEAREST") == 0){
        param->interp_method = ORBI_NEAREST;

    } else if (strcasecmp(param->orbinterp,"WEIGHTED") == 0){
        param->interp_method = ORBI_WEIGHTED;

    } else if (strcasecmp(param->orbinterp,"TAYLOR") == 0){
        
        if (strcasecmp(param->orbform,"KEPLERIAN") != 0){
            param->interp_method = ORBI_TAYLOR;
        } else {
            info(param,MED,"WARNING: Interpolation method TAYLOR is not supported for orbform %s.\n",param->orbform);
            info(param,MED,"         Setting to WEIGHTED instead.\n");
            param->interp_method = ORBI_WEIGHTED;
        }
    } else {
        /* No interpolation method could be matched. */
        info(param,ALWAYS,"\nERROR: Unknown interpolation method: %s\n",param->orbinterp);
        errstatus = -1;
        return errstatus;
    }

    
    /* Check if input RA & Dec is within range; otherwise flag to read from infile */
    if ( (param->ra < 0.0) || (param->ra > 360.0) ){
        param->get_ra_from_infile = 1;
    } else {
        param->get_ra_from_infile = 0;
    }

    if ( (param->dec < -90.0) || (param->dec > 90.0) ){
        param->get_dec_from_infile = 1;
    } else {
        param->get_dec_from_infile = 0;
    }

    /* If history=yes, then populate history string with parameters */
    if (param->history){
        param->history_string = calloc(10000, sizeof(char));
        sprintf(param->history_string,
                "infile=%s\n"
                "outfile=%s\n"
                "orbfile=%s\n"
                "ra=%f\n"
                "dec=%f\n"
                "orbext=%s\n"
                "orbform=%s\n"
                "orbcol=%s\n"
                "refframe=%s\n"
                "orbinterp=%s\n"
                "timecol=%s\n"
                "stopcol=%s\n"
                "clobber=%d\n"
                "logfile=%s\n"
                "debug=%d\n"
                "history=%d\n"
                "mode=%s\n",
                param->infile,
                param->outfile,
                param->orbfile,
                param->ra,
                param->dec,
                param->orbext,
                param->orbform,
                param->orbcol,
                param->refframe,
                param->orbinterp,
                param->timecol,
                param->stopcol,
                param->clobber,
                param->logfile,
                param->debug,
                param->history,
                param->mode);
    }

    return errstatus;
}


/* FUNCTION: doWork()
 *
 * PURPOSE:  execute main body of barycen tool
 *
 * INPUT:   param - Data structure containing tool parameters
 *          bcorr - Data structure containing former heagen/barycorr/bary.c global variables
 *            dpl - Data structure containing former heagen/barycorr/dpleph.c global variables
 *
 * OUTPUT:  return 0 if successful, -1 if failure
 */
int doWork(PARAM * param, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){
    int errstatus = 0;
    double src_dir[3];
    double DEG2RAD = PI/180.0;
    char radecsys[NEW_FLEN_VALUE];
    char radecsys_save[NEW_FLEN_VALUE] = "NULL";
    char fromts[NEW_FLEN_VALUE];
    char fromts_save[NEW_FLEN_VALUE] = "NULL";
    char * tots = 0;
    char timeref[NEW_FLEN_VALUE];
    char timeref_save[NEW_FLEN_VALUE] = "NULL";
    char timesys[NEW_FLEN_VALUE];
    char timesys_save[NEW_FLEN_VALUE] = "NULL";
    char telescop[NEW_FLEN_VALUE];
    char telescop_save[NEW_FLEN_VALUE] = "NULL";
    char timeunit[NEW_FLEN_VALUE];
    char timeunit_save[NEW_FLEN_VALUE] = "NULL";
    long mjdrefi = 0;
    long mjdrefi_save = -99999;
    double mjdreff = 0.0;
    double mjdreff_save = -99999.0;
    double mjdrefWHOLE = 0.0;
    double ra = 0.0;
    double ra_save = -99999.0;
    double dec = 0.0;
    double dec_save = -99999.0;
    double equinox = 0.0;
    double equinox_save = -99999.0;

    EXTENSION_TYPE exttype = 0;

    int N_infiles = 1;
    int N_orbfiles = 1;
    int N_outfiles = 1;
    char ** infile_names;
    char ** orbfile_names;
    char ** outfile_names;

    COLUMN_INFO cp;

    /* Check for multiple input files */
    if (param->infile[0] == '@'){
        /* Remove the '@' character preceding the file list */
        memmove(&param->infile[0], &param->infile[0 + 1], strlen(param->infile) - 0);

        /* Get the number of lines in the file */
        N_infiles = get_nlines(param->infile);
        info(param,MED,"Reading list of %d input file names.\n",N_infiles);

        /* Allocate and populate the infile_names list */
        infile_names = calloc(N_infiles, sizeof(char *));
        FILE *fp = fopen(param->infile,"r");
        for (int ii=0; ii<N_infiles; ii++){
            infile_names[ii] = calloc(NEW_FLEN_VALUE, sizeof(char));
            fscanf(fp,"%s",infile_names[ii]);
            info(param,MED,"   %s\n",infile_names[ii]);
        }
        fclose(fp);

    } else {
        /* Otherwise copy the single infile name to the infile_names list */
        infile_names = calloc(1,sizeof(char *));
        infile_names[0] = calloc(NEW_FLEN_VALUE,sizeof(char));
        strcpy(infile_names[0],param->infile);
        info(param,MED,"Preparing for single input file: %s\n",infile_names[0]);
    }


    /* Check for multiple orbit files */
    if (param->orbfile[0] == '@'){
        /* Remove the '@' character preceding the file list */
        memmove(&param->orbfile[0], &param->orbfile[0 + 1], strlen(param->orbfile) - 0);

        /* Get the number of lines in the file */
        N_orbfiles = get_nlines(param->orbfile);
        info(param,MED,"Reading list of %d orbit file names.\n",N_orbfiles);

        /* Allocate and populate the orbfile_names list */
        orbfile_names = calloc(N_orbfiles, sizeof(char *));
        FILE *fp = fopen(param->orbfile,"r");
        for (int ii=0; ii<N_orbfiles; ii++){
            orbfile_names[ii] = calloc(NEW_FLEN_VALUE, sizeof(char));
            fscanf(fp,"%s",orbfile_names[ii]);
            info(param,MED,"   %s\n",orbfile_names[ii]);
        }
        fclose(fp);

    } else {
        /* Otherwise copy the single orbfile name to the orbfile_names list */
        orbfile_names = calloc(1,sizeof(char *));
        orbfile_names[0] = calloc(NEW_FLEN_VALUE,sizeof(char));
        strcpy(orbfile_names[0],param->orbfile);
        info(param,MED,"Preparing for single orbit file: %s\n",orbfile_names[0]);
    }

    /* Check that we have the same number of infiles as orbit files */
    if (N_infiles != N_orbfiles){
        /* Complain and exit if file numbers mismatch */
        info(param,ALWAYS,"\nERROR: Must have exactly one orbit file for each input file!\n");
        errstatus = -1;
        return errstatus;
    } else {
        /* Otherwise go ahead and set up output file names */
        char fname[NEW_FLEN_VALUE];
        char buffer[NEW_FLEN_VALUE];
        char root[NEW_FLEN_VALUE];
        char suffix[NEW_FLEN_VALUE];

        N_outfiles = N_infiles;

        strcpy(buffer,param->outfile);
        get_root_suffix(buffer, root, suffix);

        outfile_names = calloc(N_outfiles,sizeof(char *));

        if (N_infiles > 1){
            /* If multiple outfiles, insert an index number before suffix */
            info(param,MED,"Setting up %d output files names.\n",N_infiles);
            for (int ii=0; ii<N_outfiles; ii++){
                outfile_names[ii] = calloc(NEW_FLEN_VALUE,sizeof(char));
                sprintf(fname,"%s%d.%s",root,ii+1,suffix);
                strcpy(outfile_names[ii],fname);
                info(param,MED,"   %s\n",outfile_names[ii]);
            }
        } else {
            /* Otherwise, set the single entry in the outfile_names array as root.suffix */
            outfile_names[0] = calloc(NEW_FLEN_VALUE,sizeof(char));
            sprintf(fname,"%s.%s",root,suffix);
            strcpy(outfile_names[0],fname);
            info(param,MED,"Setting up single output file: %s\n",outfile_names[0]);
        }
    }

    /* This should not be possible, but check anyway: */
    if (N_infiles != N_outfiles){
        info(param,ALWAYS,"\nERROR: Number of outfiles (%s) does not equal number of infiles (%s)!\n",N_outfiles,N_infiles);
        errstatus = -1;
        return errstatus;
    } else {
        info(param,LOW,"N_infiles: %d   N_outfiles: %d\n",N_infiles,N_outfiles);
    }

    /* #################################
       At this point, we have three string arrays (though possibly only one element long each), giving 
       the input file names, the orbit file names, and the output file names, and all three arrays
       are gauranteed the same number of entries.  Now ready to loop over those file name entries.
       ################################# */

    /* TOP LEVEL LOOP "A" */
    for (int file_loop=0; file_loop<N_infiles; file_loop++){
        int writeorigcols = 0;     /* flag that input file should be overwritten */
        GENORBFILE * GenOrbFile;   /* GENORBFILE structure */
        char vunit[NEW_FLEN_VALUE];        /* Unit for orbit file, default should be meters */
        int colnum = 0;
        char keyname[NEW_FLEN_VALUE];
        char * comment = 0;
        fitsfile * inputFP = 0;
        fitsfile * outputFP = 0;
        int time_unit = 0;
        int found = 0;

        info(param,MED,"\n");

        fits_file_exists(outfile_names[file_loop], &found, &errstatus);
        /* If the outfile has the same name as the infile, check clobber status */
        if ( strcasecmp(infile_names[file_loop],outfile_names[file_loop]) == 0){
            if (param->clobber == 0){
                info(param,ALWAYS,"\nERROR: Request to overwrite %s, but clobber is set to NO.\n",infile_names[file_loop]);
                errstatus = -1;
                return errstatus;
            } else {
                /* flag to acknowledge that we will overwrite input file */
                writeorigcols = 1;
                sprintf(outfile_names[file_loop],"%s_tmp",outfile_names[file_loop]);
            }
        } else if ( (found==1) || (found==2) ){
            /* The output file already exists, but is different from the input file */
            if (param->clobber == 0){
                info(param,ALWAYS,"\nERROR: Output file %s already exists, but clobber is set to NO.\n",outfile_names[file_loop]);
                errstatus = -1;
                return errstatus;
            } else {
                remove(outfile_names[file_loop]);
            }
        }

        /* Open the orbit file */
        GenOrbFile = openGenOrbFile(orbfile_names[file_loop], param->orbext, param->orb_col_name, param->orb_format, param->interp_method);

        if (GenOrbFile == NULL){
            info(param,ALWAYS,"\nERROR: could not open or read orbit file %s\n",orbfile_names[file_loop]);
            return -1;
        } else {
            info(param,MED,"Successfully opened and read orbit file %s\n",orbfile_names[file_loop]);
        }

        /* Read the units from the orbit file */
        if ( (param->orb_format == ORBF_KEPLERIAN) || (param->orb_format == ORBF_KEPLERIAN_NODUP) ){
            if (fits_get_colnum(GenOrbFile->fp, CASEINSEN, "A", &colnum, &errstatus) != 0){
                info(param,ALWAYS,"  ERROR: Orbit file %s: column name 'A' does not exist.\n",orbfile_names[file_loop]);
                return -1;
            }

            sprintf(keyname,"TUNIT%d",colnum);
            if (fits_read_key_str(GenOrbFile->fp, keyname, vunit, comment, &errstatus) != 0){
                info(param,ALWAYS,"  ERROR: Orbit file %s: failed to read %s value, errstatus = %d\n",keyname,errstatus);
                return -1;
            }

        } else if ( (param->orb_format == ORBF_VECTOR) || (param->orb_format == ORBF_COMPONENTS) ){
            if (fits_get_colnum(GenOrbFile->fp, CASEINSEN, param->orb_col_name[0], &colnum, &errstatus) != 0){
                info(param,ALWAYS,"  ERROR: Orbit file %s: column name %s does not exist.\n",orbfile_names[file_loop],param->orb_col_name[0]);
                return -1;
            }

            sprintf(keyname,"TUNIT%d",colnum);
            if (fits_read_key_str(GenOrbFile->fp, keyname, vunit, comment, &errstatus) != 0){
                info(param,ALWAYS,"  ERROR: Orbit file %s: failed to read %s value, errstatus = %d\n",keyname,errstatus);
                return -1;
            }

        } else {
            info(param,MED,"  Orbit file %s: Setting vunit to meters.\n",orbfile_names[file_loop]);
            strcpy(vunit,"m");
        }

        /* Get the first and the last time in the orbit file */
        fits_read_key_dbl(GenOrbFile->fp, "TSTART", &param->torbstart, comment, &errstatus);
        fits_read_key_dbl(GenOrbFile->fp, "TSTOP", &param->torbstop, comment, &errstatus);
        info(param,MED,"  Orbit file %s: [torbstart,torbstop] = [%f, %f]\n",orbfile_names[file_loop],param->torbstart,param->torbstop); 


        /* Open the input and output files */
        if (fits_open_file(&inputFP, infile_names[file_loop], READONLY, &errstatus) == 0){
                
            if (fits_create_file(&outputFP, outfile_names[file_loop], &errstatus) == 0) {
                int N_HDUs = 0;         /* Number of HDUs in this input file */
                int * correct_HDU = 0;  /* Array of flags specifiying to correct this HDU */
                
                info(param,MED,"Successfully opened input file %s\n",infile_names[file_loop]);
                
                /* Get the number of HDUs in the input file */
                if (fits_get_num_hdus(inputFP, &N_HDUs, &errstatus) != 0){
                    info(param,ALWAYS,"  ERROR: Problem getting number of HDUs in file %s.\n",infile_names[file_loop]);
                    return -1;
                }
                
                /* Allocate the array of HDU correction flags, and set each to zero.  We will cycle through
                   the list of HDUs, and if that HDU is to be barycenter corrected, we set correct_HDU[i] to 1. */
                correct_HDU = calloc(N_HDUs, sizeof(int));
                
                /* Two possibilities exist: 
                   1) Only the input file name was given; we'll loop over all extensions and correct GTIs.
                   2) Input file was given with extension specified; we'll correct only this extension.  However,
                   if the extension specified is an EVENTS extension, then also correct any GTIs.
                */
                
                /* Parse the input filename to see if HDU extension was given */
                int extension_specified = -1;  
                char tempstring[NEW_FLEN_VALUE];
                strcpy(tempstring,infile_names[file_loop]);
                char * extnum = 0;
                extnum = strtok(tempstring,"[");
                extnum = strtok(NULL,"]");
                int refframe_ephnum = 0;
                int radecsys_ephnum = 0;

                if (extnum != NULL){
                    extension_specified = atoi(extnum);
                    info(param,LOW,"  User has requested correction to HDU %d in file %s.\n",extension_specified,infile_names[file_loop]);
                }
                
                /* LOOP OVER HDU EXTENSIONS - LOOP "B" */
                for (int iHDU = 0; iHDU < N_HDUs; iHDU++){
                    char HDU_NAME[NEW_FLEN_VALUE];
                    
                    /* Move to the next HDU, remember cfitsio HDU numbering starts at 1, not 0 */
                    if ( fits_movabs_hdu(inputFP, iHDU+1, NULL, &errstatus) != 0){
                        info(param,ALWAYS,"  ERROR: Problem moving to HDU number %d of file %s: errstatus %d\n",
                             iHDU, infile_names[file_loop], errstatus);
                        return -1;
                    }
                    
                    /* Get the name of this HDU extension, or Primary */
                    if (fits_read_key_str(inputFP,"EXTNAME", HDU_NAME, NULL, &errstatus)){
                        strcpy(HDU_NAME,"Primary");
                        errstatus = 0;
                    }
                    
                    correct_HDU[iHDU] = check_extension(inputFP, iHDU, extension_specified, param, &cp, &exttype);
                    
                    /* Problem with this HDU extension */
                    if (correct_HDU[iHDU] == -1){
                        info(param,ALWAYS,"  ERROR: Problem in HDU %d of file %s, check orbit file time bounds.\n",
                             iHDU,infile_names[file_loop]);
                        
                    } else if (correct_HDU[iHDU] == 1){ /* This HDU is good, use it. */
                        int apply_timezero = 0;
                        double timezero = 0.0;
                        
                        /* Read the RADECSYS keyword from the input fits file. */
                        fits_read_key_str(inputFP,"RADECSYS",radecsys,comment,&errstatus);

                        /* If we couldn't find it, see if we have it saved from a previous extension */
                        if (errstatus){
                            if ( strcasecmp(radecsys_save,"") != 0 ){
                                /* We have it saved, use it! */
                                strcpy(radecsys,radecsys_save);
                                errstatus = 0;
                            } else {
                                /* We didn't find it and we don't have it saved. */
                                /* errstatus is still nonzero, but things may yet be ok. */
                            }
                        } else { /* If we DID find it, save it for next time. */
                            strcpy(radecsys_save,radecsys);
                        }

                        /* At this point, we either have a value for radecsys, or we have a nonzero errstatus */

                        /* Case 1: The user set refframe to "FILE", but we can't find the RADECSYS keyword in the file.
                           Complain and exit. */
                        if ( (strcasecmp(param->refframe,"FILE") == 0) && (errstatus) ){
                            info(param,ALWAYS,"  ERROR: refframe set to FILE, but RADECSYS keyword not found.\n");
                            return -1;
                        }

                        /* Case 2: We failed to find the RADECSYS keyword, but the user set refframe to something
                           other than "FILE".  Use the user's refframe value. */
                        else if ( (strcasecmp(param->refframe,"FILE") != 0) && (errstatus) ){
                            info(param,LOW,"  Setting ephemeris system from user's refframe = %s.\n",param->refframe);
                            strcpy(radecsys,param->refframe);
                            errstatus = 0;
                        }
                        
                        /* Case 3: The user set refframe to "FILE", and we found RADECSYS successfully in the file.
                           Set refframe to the radecsys value that we found. */
                        else if ( (strcasecmp(param->refframe,"FILE") == 0) && (!errstatus) ){
                            info(param,LOW,"  refframe set to FILE, using RADECSYS = %s from input file as ephemeris system.\n",
                                 radecsys);
                            strcpy(param->refframe,radecsys);
                        }
                        
                        /* Case 4: User's refframe and input file's RADECSYS are different, but may yet be OK. 
                           If refframe=FK5 and RADECSYS=ICRS, then c200to405 will take care of this later. */
                        else if ( strcasecmp(param->refframe,radecsys) != 0){
                            info(param,LOW,"  RADECSYS and refframe do not match, will attempt to continue.\n");
                        }

                        /* Case 5: radecsys and refframe match, and all is well. */
                        else {
                            info(param,LOW,"  Ephemeris systems match: radecsys = %s, refframe = %s\n",
                                 radecsys,param->refframe);
                        }


                        /* Set the refframe_ephnum */
                        if (strcasecmp(param->refframe,"FK5") == 0){
                            refframe_ephnum = 200; /* PLEPHEM='JPL-DE200' */
                            
                        } else if (strcasecmp(param->refframe,"ICRS") == 0){
                            refframe_ephnum = 405; /* PLEPHEM='JPL-DE405' */
                        } else {
                            refframe_ephnum = 0;
                        }

                        /* Set the radecsys_ephnum */
                        if (strcasecmp(radecsys,"FK5") == 0){
                            radecsys_ephnum = 200; /* PLEPHEM='JPL-DE200' */

                        } else if (strcasecmp(radecsys,"ICRS") == 0){
                            radecsys_ephnum = 405; /* PLEPHEM='JPL-DE405' */
                        } else {
                            radecsys_ephnum = 0;
                        }

                        /* Read the TIMESYS keyword */
                        fits_read_key_str(inputFP,"TIMESYS",fromts,comment,&errstatus);
                        
                        if (errstatus){ /* Problem finding TIMESYS keyword */
                            if ( strcasecmp(fromts_save,"") == 0){
                                strcpy(fromts,fromts_save);
                            } else {
                                info(param,ALWAYS,"  ERROR: TIMESYS keyword not found.\n");
                                return -1;
                            }
                        } else {
                            strcpy(fromts_save,fromts);
                        }
                        tots = "TDB";
                        
                        /* based on subroutine suzaku/com/src/module/aebarycen/aebarycen_body: aebarycen_check_time_header */
                        fits_read_key_str(inputFP, "TIMEREF", timeref, comment, &errstatus) ; 
                        
                        if (errstatus) {
                            if ( strcasecmp(timeref_save,"") != 0) {  /* Use saved value */
                                strcpy(timeref,timeref_save);
                            } else {
                                info(param,ALWAYS,"  ERROR: TIMEREF keyword not found.\n");
                                return -1;
                            }
                        } else {
                            strcpy(timeref_save,timeref);
                        }
                        
                        if ( strcasecmp(timeref,"SOLARSYSTEM") == 0 ) {
                            info(param,ALWAYS,"  ERROR: TIMEREF is already SOLARSYSTEM\n");
                            return -1;
                        }
                        
                        fits_read_key_str(inputFP,"TIMESYS", timesys, comment, &errstatus) ; 
                        if (errstatus) {
                            if ( strcasecmp(timesys_save,"") != 0) {  /* Use saved value */
                                strcpy(timesys,timesys_save);
                            } else {
                                info(param,ALWAYS,"  ERROR: TIMESYS keyword not found.\n");
                                return -1;
                            }
                        } else {
                            strcpy(timesys_save,timesys);
                        }
                        
                        if ( 0 != strcasecmp(timesys, "TT") &&
                             0 != strcasecmp(timesys, "TDT") &&
                             0 != strcasecmp(timesys, "TAI") &&
                             0 != strcasecmp(timesys, "ET") &&
                             0 != strcasecmp(timesys, "UTC") ) {
                            info(param,ALWAYS,"  ERROR: TIMESYS is unknown, '%s'\n",timesys);
                            return -1;
                        }
                        
                        if ( 0 == strcmp(timesys, "TDB") ||
                             0 == strcmp(timesys, "TCB") ) {
                            info(param,ALWAYS,"WARNING: TIMESYS is already %s\n",timesys);
                            return -1;
                        }
                        
                        /* If any of these keywords does not exist, exit with an error message */
                        fits_read_key_str(inputFP, "TELESCOP", telescop, comment, &errstatus) ; 
                        if (errstatus) {
                            if ( strcasecmp(telescop_save,"") != 0) {  /* Use saved value */
                                strcpy(telescop,telescop_save);
                            } else {
                                info(param,ALWAYS,"  ERROR: TELESCOP keyword not found.\n");
                                return -1;
                            }
                        } else {
                            strcpy(telescop_save,telescop);
                        }
                        
                        fits_read_key_str(inputFP, "TIMEUNIT", timeunit, comment, &errstatus) ;
                        if (errstatus) {
                            if ( strcasecmp(timeunit_save,"") != 0) {  /* Use saved value */
                                strcpy(timeunit,timeunit_save);
                            } else {
                                info(param,ALWAYS,"  ERROR: TIMEUNIT keyword not found.\n");
                                return -1;
                            }
                        } else {
                            strcpy(timeunit_save,timeunit);
                        }
                        

                        /* MJD may exist as a single keyword MJDREF, or as integer and fractional
                           components MJDREFI and MJDREFF. */
                        if ( fits_read_key_dbl(inputFP,"MJDREF", &mjdrefWHOLE, comment, &errstatus) == 0 ){
                            info(param,LOW,"  Parsing MJDREF keyword into MJDREFI and MJDREFF.\n");
                            mjdrefi = (int)floor(mjdrefWHOLE);
                            mjdreff = mjdrefWHOLE - mjdrefi;
                            mjdrefi_save = mjdrefi;
                            mjdreff_save = mjdreff;
                        } else {
                            errstatus = 0;
                            fits_read_key_lng(inputFP,"MJDREFI", &mjdrefi, comment, &errstatus) ; 
                            if (errstatus) {
                                if (mjdrefi_save != -99999) {  /* Use saved value */
                                    mjdrefi = mjdrefi_save;
                                } else {
                                    info(param,ALWAYS,"  ERROR: MJDREFI keyword not found.\n");
                                    return -1;
                                }
                            } else {
                                mjdrefi_save = mjdrefi;
                            }
                            
                            fits_read_key_dbl(inputFP,"MJDREFF", &mjdreff, comment, &errstatus) ;
                            if (errstatus) {
                                if (mjdreff_save != -99999.0) {  /* Use saved value */
                                    mjdreff = mjdreff_save;
                                } else {
                                    info(param,ALWAYS,"  ERROR: MJDREFF keyword not found.\n");
                                    return -1;
                                }
                            } else {
                                mjdreff_save = mjdreff;
                            }
                        }

                        /* Check for a TIMEZERO keyword in the current HDU header.  If found, set flag to zero-correct time. */
                        errstatus = 0;
                        fits_read_key_dbl(inputFP, "TIMEZERO", &timezero, comment, &errstatus);

                        if (errstatus == 0) {

                            if (timezero == 0.0) {
                                /* If timezero is 0, no need to correct. */
                                info(param,ALWAYS,"  Warning: TIMEZERO keyword exists, but is zero.\n");
                                apply_timezero = 0;

                            } else { /* timezero is not zero, we have work to do. */
                                int timecolnum = 0;
                                double time_one[1]; /* CFITSIO idiosyncracy - this is a one-element array */

                                /* Figure out the time column number */
                                fits_get_colnum(inputFP, CASEINSEN, param->timecol, &timecolnum, &errstatus);

                                /* Read the first entry in the time column */
                                time_one[0] = 0.0;
                                fits_read_col_dbl(inputFP, timecolnum, 1, 1, 1, 0, time_one, NULL, &errstatus); 
                                
                                if (time_one[0] == 0.0) { 
                                    /* timezero is nonzero, time_one is zero, then set flag to correct. */
                                    info(param,MED,"  Flagging this HDU for zero-correction.\n");
                                    apply_timezero = 1;
                                } else {
                                    /* If both timezero and time_one are non-zero, then something is wrong. */
                                    info(param,ALWAYS,"  Incompatability: TIMEZERO keyword is %f, but first TIME value is nonzero.\n",timezero);
                                    return -1;
                                }
                            }

                        } else if (errstatus == KEY_NO_EXIST){
                            /* If the TIMEZERO keyword isn't present at all, then proceed normally. */
                            info(param,MED,"  No TIMEZERO keyword exists, skipping zero-correction.\n");
                            apply_timezero = 0;
                            errstatus = 0;

                        } else {
                            /* If the TIMEZERO keyword read fails for some other reason, throw an error. */
                            info(param,ALWAYS,"  ERROR: Problem reading TIMEZERO keyword, fits error status %d.\n",errstatus);
                            return -1;
                        }


                        /* ################################################################
                           Step 2e.  Determine the source position if not given by the input parameters.  
                           Partially based on code in the subroutine suzaku/com/src/module/aebarycen/aebarycen_body: 
                           aebarycen_read_header */
                        
                        
                        
                        if(param->get_ra_from_infile) {
                            /* coordevt.cxx CVS v1.61 Lines 880-960 */
                            /* Try RA_NOM first. */ 
                            fits_read_key_dbl(inputFP, "RA_NOM", &ra, NULL, &errstatus);
                            if(errstatus) {
                                /* RA_NOM wasn't found, so try RA_PNT. */ 
                                errstatus = 0;
                                fits_read_key_dbl(inputFP, "RA_PNT", &ra, NULL, &errstatus);
                                if(errstatus) { 
                                    if  (ra_save != -99999.0){
                                        ra = ra_save;
                                    } else { /*RA_PNT also wasn't found. Complain and quit. */
                                        info(param,ALWAYS,"  ERROR: RA_NOM or RA_PNT keyword not found.\n");
                                        return -1;
                                    }
                                }
                            }
                            param->ra = ra;
                            ra_save = ra;
                        }
                        
                        if(param->get_dec_from_infile) {
                            /* coordevt.cxx CVS v1.61 Lines 880-960 */
                            /* Try DEC_NOM first. */
                            fits_read_key_dbl(inputFP, "DEC_NOM", &dec, NULL, &errstatus);
                            if(errstatus) {
                                /* DEC_NOM wasn't found, so try DEC_PNT. */
                                errstatus = 0;
                                fits_read_key_dbl(inputFP, "DEC_PNT", &dec, NULL, &errstatus);
                                if(errstatus) {
                                    if  (dec_save != -99999.0){
                                        dec = dec_save;
                                    } else { /*DEC_PNT also wasn't found. Complain and quit. */
                                        info(param,ALWAYS,"  ERROR: DEC_NOM or DEC_PNT keyword not found.\n");
                                        return -1;
                                    }
                                }
                            }
                            param->dec = dec;
                            dec_save = dec;
                        }
                        
                        /* At this point, also check that the EQUINOX keyword exist.  If not, use J2000 */ 
                        fits_read_key_dbl(inputFP, "EQUINOX", &equinox, NULL, &errstatus);
                        
                        if (errstatus){
                            if (equinox_save != -99999.0){
                                equinox = equinox_save;
                                errstatus = 0;
                            } else {
                                info(param,ALWAYS,"  NOTE: Bad or missing EQUINOX keyword, using J2000.\n");
                                equinox = 2000.0;
                                errstatus = 0;
                            }
                        } else {
                            /* Only set equinox_save if we've read equinox succesfully. */
                            equinox_save = equinox;
                        }
                        
                        /* Fill the src_dir vector */
                        src_dir[0] = cos(param->ra*DEG2RAD) * cos(param->dec*DEG2RAD) ;
                        src_dir[1] = sin(param->ra*DEG2RAD) * cos(param->dec*DEG2RAD) ;
                        src_dir[2] = sin(param->dec*DEG2RAD) ;

                        if (0 != param->debug) {
                            info(param,LOW,"  Cartesian source coords: (%.15g, %.15g, %.15g)\n",
                              src_dir[0], src_dir[1], src_dir[2]);
                        }

                        /* If the RA and DEC did not come from the infile, and the user has specified
                           FK5 system, but the infile is in ICRS, the correct user's request to ICRS. */
                        if ( (radecsys_ephnum != refframe_ephnum) &&
                             (param->get_ra_from_infile != 1) &&
                             (param->get_dec_from_infile != 1) ){

                            if ( (radecsys_ephnum == 405) && (refframe_ephnum == 200) ){
                                /* Correct the direction from FK5 to ICRS. */
                                c200to405(src_dir,param->debug);
                                refframe_ephnum = 405;
                            } else {
                                info(param,ALWAYS,"ERROR: Cannot resolve differences between FK5 and ICRS.\n");
                                return -1;
                            }
                        }
                        
                        
                        /* Step 2f.  Determine the time unit.  This based on code in the subroutine 
                           suzaku/com/src/module/aebarycen/aebarycen_body: process_hdu */
                        
                        /* aebarycen_body.c  Lines 821-830 */
                        if ( 0 == strcasecmp(timeunit, "s") ) {
                            time_unit = BARYCEN_TIME_UNIT_SEC;
                        } else if ( 0 == strcasecmp(timeunit,"d") ) {
                            time_unit = BARYCEN_TIME_UNIT_DAY;
                        } else {
                            info(param,ALWAYS,"  ERROR: TIMEUNIT is undefined (%s)\n", timeunit);
                            return -1;
                        }
                        
                        /* Step 2g.  Initialize barycentric correction function. This is in the heagen/barycorr library.  
                           This routine could be used as-is, but includes code to read the refdata file `tai-utc.dat' 
                           to determine the number of leap seconds. From what I can figure, the leap seconds are only 
                           used when TIMESYS == `UTC'.*/
                        
                        int mission_enum = ObsString2Enum(telescop);

                        bcorr->denum = baryinit(mission_enum, fromts, tots, mjdrefi, mjdreff, timeunit, refframe_ephnum, bcorr, dpl);
                        if (!bcorr->denum) {
                            info(param,ALWAYS, "  ERROR in baryinit()\n");
                            return -1;
                        }
                        
                        /* Step 3a-e. Loop over the times in the event file and determine the barycenter correction.  
                           This is the main working part of the code. */
                        
                        int ncols=0, cols[3], icol=0;
                        long irow=0, nrow=0;
                        long ignored_null_times = 0;
                        
                        cols[0] = cp.t_col;  /* timecol:  number of the column called TIME  */
                        cols[1] = cp.s_col;  /* startcol: number of the column called START */
                        cols[2] = cp.e_col;  /* stopcol:  number of the column called STOP  */
                        nrow = cp.nrow; /* Number of rows */
                        fits_get_num_cols(inputFP, &ncols, &errstatus); /* Get number of columns */
                        
                        /* Create a new HDU in the output file, and copy header from input file */
                        fits_copy_hdu(inputFP, outputFP, 0, &errstatus);

                        /* LEVEL LOOP C: Loop over columns in the current input HDU */
                        for (icol=1; icol<=ncols; icol++){
                            
                            /* If this column is the START, STOP, or TIME column */
                            if ( (icol == cols[0]) || (icol == cols[1]) || (icol == cols[2]) ){
                                
                                /* Report what column we're operating on */
                                char colname[NEW_FLEN_VALUE];
                                char keyname[NEW_FLEN_VALUE];
                                sprintf(keyname,"TTYPE%d",icol);
                                fits_read_key_str(inputFP, keyname, colname, NULL, &errstatus);
                                info(param,LOW,"  Applying barycentric correction to column %d: %s.\n",icol,colname);

                                /* Read the column data */
                                double * time_array = calloc(nrow, sizeof(double));
                                fits_read_col_dbl(inputFP, icol, 1, 1, nrow, 0, time_array, NULL, &errstatus);
                                
                                /* If we're going to overwrite the input file, then add a new column with the uncorrected times. */
                                if (writeorigcols == 1){
                                    char colname_ori[NEW_FLEN_VALUE];
                                    sprintf(colname_ori,"%s%s",colname,"ORI");
                                    info(param,MED,"  Overwriting infile, adding column %s\n",colname_ori);
                                    fits_insert_col(outputFP, ncols+1, colname_ori, "1D", &errstatus);
                                    fits_write_col_dbl(outputFP, ncols+1, 1, 1, nrow, time_array, &errstatus);
                                    ncols++;
                                }

                                /* Loop over rows - note that time_array starts at 0, but fits row numbering starts at 1*/
                                if (0 != param->debug) {
                                    info(param,LOW,"%s%s\n", "         TIME             BARYTIME       VEARTH     VEARTHX    VEARTHY    VEARTHZ", 
                                      "    ERADVEL      TOTCORR    TTtoTDB       S/C_SSBC   S/C_EARTH       GRAV\n");
                                }

                                for (irow=0; irow<nrow; irow++){
                                    
                                    /* Apply barycenter correction to the time value */
                                    double timeori = time_array[irow];
                                    
                                    /* Make sure timeori is not NaN, otherwise apply bary correction */
                                    if (timeori != timeori){
                                        ignored_null_times++;
                                    } else {
                                        if (apply_timezero) timeori += timezero;
                                        time_array[irow] = correct_bary(GenOrbFile, timeori, mjdrefi, mjdreff, src_dir, vunit, 
                                          param, bcorr, dpl);
                                    }
                                    
                                } /* end loop over rows */
                                
                                /* Write newly-corrected array to output fits column and clean up time_array memory allocation */
                                fits_write_col_dbl(outputFP, icol, 1, 1, nrow, time_array, &errstatus);
                                free(time_array);
                                
                            } else { /* Otherwise, just copy the column from input to output fits */
                                fits_copy_col(inputFP, outputFP, icol, icol, 0, &errstatus);
                                
                            } /* end column name conditional */
                            
                        } /* END LEVEL LOOP C: loop over columns */
                        
                        /* Barycenter-correct TSTART/TSTOP keywords */
                        correct_bary_keyword("TSTART", outputFP, GenOrbFile, mjdrefi, mjdreff, src_dir, vunit, param, bcorr, dpl);
                        correct_bary_keyword("TSTOP", outputFP, GenOrbFile, mjdrefi, mjdreff, src_dir, vunit, param, bcorr, dpl); 
                        
                        if (ignored_null_times)
                          info(param,MED,"  Ignored %ld NULL times in this HDU, wrote as-is.\n",ignored_null_times);

                        /* If modify keyword fails, assume keyword not found, and write keyword. */
                        if (fits_modify_key_str(outputFP, "TIMESYS", "TDB", "Time System (TDB: Barycentric Dynamical Time)", &errstatus)){
                            errstatus = 0;
                            fits_write_key_str(outputFP,  "TIMESYS", "TDB", "Time System (TDB: Barycentric Dynamical Time)", &errstatus);
                        }

                        if (fits_modify_key_str(outputFP, "TIMEREF", "SOLARSYSTEM", "Times are pathlength-corrected to barycenter", &errstatus)){
                            errstatus = 0;
                            fits_write_key_str(outputFP, "TIMEREF", "SOLARSYSTEM", "Times are pathlength-corrected to barycenter", &errstatus);
                        }

                        if (apply_timezero){
                            if (fits_modify_key_dbl(outputFP, "TIMEZERO", 0.0, -10, "Time zero", &errstatus)){
                                errstatus = 0;
                                fits_write_key_dbl(outputFP, "TIMEZERO",  0.0, -10, "Time zero", &errstatus);
                            }
                        }

                        if (fits_modify_key_str(outputFP, "PLEPHEM", "JPL-DE200", "Solar System ephemeris used for baryctr corr", &errstatus)){
                            errstatus = 0;
                            fits_write_key_str(outputFP, "PLEPHEM", "JPL-DE200", "Solar System ephemeris used for baryctr corr", &errstatus);
                        }

                        if (fits_modify_key_dbl(outputFP, "RA_TDB", param->ra, -10, "Target RA of Barycentric Dynamical Time corr", &errstatus)){
                            errstatus = 0;
                            fits_write_key_dbl(outputFP, "RA_TDB",  param->ra, -10, "Target RA of Barycentric Dynamical Time corr", &errstatus);
                        }

                        if (fits_modify_key_dbl(outputFP, "DEC_TDB",  param->dec, -10, "Target DEC of Barycentric Dynamical Time corr", &errstatus)){
                            errstatus = 0;
                            fits_write_key_dbl(outputFP, "DEC_TDB",  param->dec, -10, "Target DEC of Barycentric Dynamical Time corr", &errstatus);
                        }                       

                        if (errstatus){
                            info(param,ALWAYS,"  ERROR: Problem writing/updating keywords.  errstatus %d\n",errstatus);
                            return -1;
                        }


                        /* Write date / checksums / datasums / history */
                        fits_write_date(outputFP, &errstatus);
                        fits_write_chksum(outputFP, &errstatus);
                        if (param->history){
                            fits_write_history(outputFP, param->history_string, &errstatus);
                        }
                        
                        info(param,HIGH,"Created HDU %d: %s.\n",iHDU,HDU_NAME);
                    } else {
                        fits_copy_hdu(inputFP, outputFP, 0, &errstatus);
                        info(param,HIGH,"Copied HDU %d: %s as-is.\n",iHDU,HDU_NAME);
                    }
                    
                } /* END OF LOOP B */
                
                free(correct_HDU);
                if ( fits_close_file(inputFP, &errstatus) != 0){
                    info(param,ALWAYS,"  ERROR: Problem closing input file %s\n",infile_names[file_loop]);
                    return -1;
                }
                
                if ( fits_close_file(outputFP, &errstatus) != 0){
                    info(param,ALWAYS,"  ERROR: Problem closing output file %s\n",outfile_names[file_loop]);
                    return -1;
                }


            } else {
                info(param,ALWAYS,"ERROR, could not create output file %s\n",outfile_names[file_loop]);
                return -1;
            }   
            
        } else {
            info(param,ALWAYS,"ERROR, could not open input file %s\n",infile_names[file_loop]);
            return -1;
        }
        
        
        closeGenOrbFile(GenOrbFile);


        /* if we've flagged to overwrite input file, then rename output */
        if (writeorigcols == 1){
            char finalname[NEW_FLEN_VALUE];
            strcpy(finalname,outfile_names[file_loop]);
            remove_substring(finalname,"_tmp");
            rename(outfile_names[file_loop],finalname);
        }



    } /* END LEVEL LOOP A (over input/orbit/output files) */

       


            
    /* Clean up */
    for (int ii=0; ii<N_infiles; ii++){
        free(orbfile_names[ii]);
        free(infile_names[ii]);
        free(outfile_names[ii]);
    }

    free(orbfile_names);
    free(infile_names);
    free(outfile_names);


    return errstatus;
}



/* FUNCTION:  correct_bary()
 *
 * PURPOSE:   perform the actual barycenter correction on the time given
 *
 * INPUTS:  GenOrbFile  -  Data structure containing orbit information
 *                time  -  Time to be corrected
 *             mjdrefi  -  Integer component of MJD time
 *             mjdreff  -  Fractional component of MJD time
 *             src_dir  -  3-vector source direction
 *               vunit  -  Velocity unit
 *               param  -  Run parameters
 *               bcorr  -  Data structure containing former heagen/barycorr/bary.c global variables
 *                 dpl  -  Data structure containing former heagen/barycorr/dpleph.c global variables
 *
 * OUTPUTS:  returns barycenter-corrected time
 */
double correct_bary (GENORBFILE * GenOrbFile, double time, int mjdrefi, double mjdreff, double * src_dir, char * vunit, 
                     PARAM * param, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){

    //([Function equivalent of correct_bary() in aebarycen])

    ORBPOSITION orbpos;
    MJDTime mjd_time;
    PRINT_DATA pdata = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    int found = 0;
    double barytime = 0.0;
    double mjdtime = 0.0;
    
    
    //# RSH050618 new version of old subroutine (not renamed).
    found = findOrbPositionInGenOrbFile(GenOrbFile, &orbpos, time);
    
    //# This is needed for both Suzaku and Swift and presumably for Astro-H
    //# aebarycen_body.c  Lines 275-287 
    
    /* findOrbPositioninGenOrbFile() always returns orbpos in units of km; convert to m. */
    orbpos.p[0] *= 1000.0; /** km --> m **/
    orbpos.p[1] *= 1000.0;
    orbpos.p[2] *= 1000.0;

    
    /*- calc TIME -*/
    //# Here might be able to replace this with something in the coordlib
    mjdtime = aste2mjdtt(time, mjdrefi, mjdreff);
    mjd_time.ts = TT; /** time is defined as TT. **/
    mjd_time.MJDint = (long) (mjdtime / 1.0);
    mjd_time.MJDfr  = mjdtime - (double) mjd_time.MJDint;
    
    barytime = time;
    //# This is a call to the library routine barycorr in heagen/barycorr/bary.c
    barytime += barycorr(&mjd_time, src_dir, orbpos.p, bcorr, dpl, &pdata);

    if (0 != param->debug) {
      info(param, LOW, "%18.6f %18.6f %10.5f %10.5f %10.5f %10.5f %10.5f %12g %12g %12g %12g %12g\n", 
        time, barytime,
        pdata.vearth, pdata.vearthx, pdata.vearthy, pdata.vearthz, pdata.eradvel,
        pdata.total, pdata.t2b, pdata.scssbc, pdata.scearth, pdata.grav);
    }

    return barytime;     
}


/* FUNCTION: correct_bary_keyword()
 *
 * PURPOSE:  Execute barycenter correction on a time-related keyword value
 *
 * INPUTS:  keyword  -  Keyword to be corrected
 *         fits_ptr  -  Pointer to fits file containing keyword to be corrected
 *       GenOrbFile  -  Data structure containing orbit information
 *          mjdrefi  -  Integer component of MJD time
 *          mjdreff  -  Fractional component of MJD time
 *          src_dir  -  3-vector source direction
 *            vunit  -  Velocity unit
 *            param  -  Data structure containing tool parameters
 *            bcorr  -  Data structure containing former heagen/barycorr/bary.c global variables
 *              dpl  -  Data structure containing former heagen/barycorr/dpleph.c global variables
 *
 * OUTPUTS: writes updated keyword value to fits_ptr 
 */
void correct_bary_keyword(char * keyword, fitsfile * fits_ptr, GENORBFILE * GenOrbFile, int mjdrefi, 
                          double mjdreff, double * src_dir, char * vunit, PARAM * param, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){
    
    double value = 0.0, corrvalue = 0.0;
    int stat = 0;

    info(param, LOW, "  Applying barycentric correction to keyword %s\n", keyword);
    
    /* correct(tstart) using Function correct_bary() above. */
    if ( fits_read_key_dbl(fits_ptr, keyword, &value, NULL, &stat) == 0 ){
        if (value == 0.0){
            info(param,ALWAYS,"  ERROR: keyword %s = 0.0.\n",keyword);
        } else {
            if (0 != param->debug) {
                info(param,LOW,"%s%s\n", "         TIME             BARYTIME       VEARTH     VEARTHX    VEARTHY    VEARTHZ", 
                  "    ERADVEL      TOTCORR    TTtoTDB       S/C_SSBC   S/C_EARTH       GRAV\n");
            }
            corrvalue= correct_bary (GenOrbFile, value, mjdrefi, mjdreff, src_dir, vunit, param, bcorr, dpl);  /* This is `Step 7' as a function call */
            fits_modify_key_dbl(fits_ptr, keyword, corrvalue, -15, "Barycen corrected", &stat) ;
        }
        info(param, LOW, "  Input value=%.15g   Output value=%.15g\n", value, corrvalue);
    } else {
        info(param,ALWAYS,"  ERROR: keyword %s not found.\n",keyword);
    }
}        


/* FUNCTION: info()
 *
 * PURPOSE: print information both to terminal screen and to log file, depending on chatter level
 *
 * INPUTS:          param  -  Data structure containing tool parameters
 *          chat_priority  -  Level at which statement should be printed to screen
 *                 format  -  String to be printed
 *                    ...  -  Variables to be inserted into string
 *
 * OUTPUTS:  returns 0 if successful
 */
int info(PARAM * param, int chat_priority, const char * format, ...){
    int errstatus = 0;
    va_list arg;

    /* Always write to log file unless set to "none" */
    if (strcasecmp(param->logfile,"none") != 0){
        FILE * fp = fopen(param->logfile,"a");
        va_start(arg, format);
        errstatus = vfprintf(fp, format, arg);
        va_end(arg);
        fclose(fp);
    }

    /* We have defined ALWAYS=0, HIGH=1, MED=2, LOW=3, so that a chatter setting will print:
       chatter=0    prints only message priority   ALWAYS
       chatter=1    prints only message priority   ALWAYS, HIGH
       chatter=2    prints only message priority   ALWAYS, HIGH, MED
       chatter=3    prints only message priority   ALWAYS, HIGH, MED, LOW

       Setting debug=yes automatically mimics chatter setting 3.
    */

    if ( (param->debug > 0) || (param->chatter >= chat_priority) ){
        va_start(arg, format);
        errstatus = vfprintf(stdout, format, arg);
        va_end(arg);
    }

    return errstatus;
}


/* FUNCTION: get_nlines()
 *
 * PURPOSE:  get the number of lines contained within an ASCII file
 *
 * INPUTS:  filename  -  the name of the file to be checked
 *
 * OUTPUTS: returns the number of valid lines
 */
int get_nlines(char * filename){  /* source input filename */
    FILE *fp;                     /* The file to be opened*/
    char line[NEW_FLEN_VALUE];       /* string line in file */
    int n_commentlines=0;         /* number of comment lines or spaces in file */
    int numlines=0;               /* number of valid source lines in file */
    int len;                      /* string length */
    int j=0, k=0;                 /* loop index */
    char temp_line[NEW_FLEN_VALUE];  /* temporary string buffer */

    fp = fopen(filename,"r");
    while(fgets(line,sizeof(line),fp) != NULL){

        /* strip trailing '\n' if it exists */
        len = strlen(line)-1;  if(line[len] == '\n') line[len] = 0;

        /* remove spaces in line */
        k=0;
        for (j=0; j<=len; j++)  if (line[j] != ' ')  temp_line[k++] = line[j];
        temp_line[k] = 0;
        strcpy(line,temp_line);

        if ( (line[0] == '#') ||
             (line[0] == '!') ||
             (line[0] == '@') ||
             (strlen(line) == 0) ||
             (line[0] == ' ') ){
            n_commentlines++;
        }else
            numlines++;
    }

    fclose(fp);
    return numlines;
}



/* FUNCTION: check_extension()
 *
 * PURPOSE:  Determine if the given FITS extension should be scheduled for barycenter correction
 *
 * INPUTS:     fp  -  pointer to fits file
 *           iHDU  -  Current HDU number
 *      given_HDU  -  User-specified HDU to correct
 *          param  -  Structure containing parameter info 
 *             cp  -  Structure containing start/stop column info
 *        exttype  -  Enumerated type of HDU extention
 *
 * OUTPUTS:
 *
 *      Return one of three possible outcomes:
 *      return 1;  - Go ahead and apply barycenter correction to the current HDU.
 *      return 0;  - Skip the current HDU in the barycenter correction.
 *      return -1; - There is something wrong with the given information; inform user and exit.
 */
int check_extension(fitsfile * fp,              /* Pointer to (already opened) fits file */
                    int iHDU,                   /* Current HDU number */
                    int given_HDU,              /* User-specified HDU to correct */
                    PARAM * param,              /* Structure containing parameter info */
                    COLUMN_INFO * cp,           /* Structure containing start/stop column info */
                    EXTENSION_TYPE * exttype){  /* Enumerated type of HDU extention */

    /* This function should return one of three possible outcomes: 
       return 1;  - Go ahead and apply barycenter correction to the current HDU.
       return 0;  - Skip the current HDU in the barycenter correction.
       return -1; - There is something wrong with the given information; inform user and exit.
    */

    char * comment = 0;
    int errstatus = 0;
    char HDUTYPE[NEW_FLEN_VALUE];
    int hdutype = 0;
    double textstart=0, textstop=0;
    double nullval = 0;
    int anynul = 0;

    cp->nrow = 0;
    cp->t_col = 0;
    cp->s_col = 0;
    cp->e_col = 0;


    info(param,LOW,"\nChecking HDU %d...\n",iHDU);

    /* hdutype should be BINARY_TABL */
    fits_get_hdu_type(fp, &hdutype, &errstatus);
    if (hdutype != BINARY_TBL){
        info(param,LOW,"  HDU %d is not a table, skipping.\n",iHDU);
        return 0;
    }
    

    /**** Regardless of EVENTS or GTI HDUTYPE, should have nrows > 0 in table. *****/
    if (fits_read_key_lng(fp, "NAXIS2", &cp->nrow, comment, &errstatus)){
        info(param,LOW,"  Could not read NAXIS2 keyword; skipping HDU %d.\n",iHDU);
        return 0;
    }
    if (cp->nrow <= 0L){
        info(param,LOW,"  No table rows in this HDU, skipping HDU %d.\n",iHDU);
        return 0;   /* Don't correct if HDU is not a table */
    }

    /**** Determine what type of extension */
    if (fits_read_key_str(fp, "EXTNAME", HDUTYPE, comment, &errstatus)){  /* Look for EXTNAME */
        errstatus = 0;
        if (fits_read_key_str(fp, "HDUCLAS1", HDUTYPE, comment, &errstatus)){ /* If no EXTNAME, look for HDUCLAS1 */
            info(param,LOW,"  Could not find EXTNAME or HDUCLAS1 keyword, skipping HDU %d.\n",iHDU);
            return 0;
        }
    }

    /* If this HDU is an EVENTS extension */
    if (strcasecmp(HDUTYPE,"EVENTS") == 0){ 
        (*exttype) = EXTTYPE_EVENT;

        /* Get the TIME column number - this must exist, otherwise nothing to correct */
        if (fits_get_colnum(fp, CASESEN, param->timecol, &cp->t_col, &errstatus)){
            info(param,LOW,"  Could not find %s column number, skipping HDU %d.\n",param->timecol,iHDU);
            return 0;  /* Don't correct if can't read time column */
        }

        /* Attempt to read TSTART/TSTOP keywords - for EVENTS, these are assumed to exist. */
        errstatus = 0;
        fits_read_key_dbl(fp, "TSTART", &textstart, comment, &errstatus);
        fits_read_key_dbl(fp, "TSTOP", &textstop, comment, &errstatus);


    } else if (strcasecmp(HDUTYPE,"GTI") == 0){   /* If this HDU is a GTI extension */
        (*exttype) = EXTTYPE_GTI;
        fits_read_key_dbl(fp, "TSTART", &textstart, comment, &errstatus);
        fits_read_key_dbl(fp, "TSTOP", &textstop, comment, &errstatus);

        /**** search START and STOP ****/
        if (fits_get_colnum(fp, CASESEN, param->startcol, &cp->s_col, &errstatus)){
            info(param,LOW,"  Problem getting %s column number, skipping HDU %d.\n",param->startcol,iHDU);
            return 0;  /* Don't correct if can't read start column */
        }
        if (fits_get_colnum(fp, CASESEN, param->stopcol, &cp->t_col, &errstatus)){
            info(param,LOW,"  Problem getting %s column number, skipping HDU %d.\n",param->stopcol,iHDU);
            return 0;  /* Don't correct if can't read stop column */
        }

    } else {  /* If this HDU is has some other name */
        errstatus = 0;
        (*exttype) = EXTTYPE_OTHER;

        /* Get the TIME column number - this must exist, otherwise nothing to correct */
        if (fits_get_colnum(fp, CASESEN, param->timecol, &cp->t_col, &errstatus)){
            info(param,LOW,"  Could not find %s column number, HDU %d will not be corrected.\n",param->timecol,iHDU);
            (*exttype) = EXTTYPE_SKIP;
            return 0;  /* Don't correct if can't read time column */
        }
        
        /* Attempt to read TSTART/TSTOP keywords - may or may not exist */
        fits_read_key_dbl(fp, "TSTART", &textstart, comment, &errstatus);
        fits_read_key_dbl(fp, "TSTOP", &textstop, comment, &errstatus);

        /* If we couldn't get start/stop times from keywords, read first and last TIME entries. */
        if (errstatus){
            info(param,LOW,"  Searching TIME column for first/last values.\n");
            errstatus = 0;
            double * timevals = calloc(cp->nrow, sizeof(double));

            fits_read_col_dbl(fp, cp->t_col, 1, 1, cp->nrow, nullval, timevals, &anynul, &errstatus);
            if (errstatus){
                info(param,LOW,"  Could not get start/stop values for HDU %d, skipping.",iHDU);
                free(timevals);
                return 0;
            } else {
                textstart = timevals[0];
                textstop = timevals[cp->nrow-1];
                free(timevals);
            }
        }
    }
    info(param,LOW,"  TSTART = %f    TSTOP = %f\n",textstart,textstop);

    /* Start/Stop times must be in range of orbit file's start/stop times */
    if ((textstart < param->torbstart) || (textstop > param->torbstop)){
        info(param,ALWAYS,"  ERROR: Orbit file times do not match %s times.\n",HDUTYPE);
        info(param,ALWAYS,"    Choose a different orbit file or increase the extrapolation time value.\n");
        return -1;
    }
    
    /* If:
          1. The user specified an HDU to correct (default value is -1, indicating no choice).
          2. The current HDU is not the one the user specfied.
          3. The current HDU is not a GTI.
       then skip correction.
    */
    if ( (given_HDU > -1) && (given_HDU != iHDU) && ( (*exttype) != EXTTYPE_GTI) ){
        return 0;
    } else {
        /* Otherwise, we've passed all the above checks, so schedule the current HDU for correction. */
        info(param,LOW,"  Scheduling HDU %d for barycenter correction.\n",iHDU);
        return 1;
    }
}


/* FUNCTION:  ObsString2Enum()
 *
 * PURPOSE:  convert the string name of a mission to its corresponding enumerated type
 *
 * INPUTS:  input_string  -  the string name of the mission
 *
 * OUTPUTS:  returns the enumerated type from enum Observatory defined in barycen.h
 */
int ObsString2Enum(char * input_string){

    /* Convert an input string to its corresponding enum in the Observatory enum */

    if (strcasecmp(input_string,"Geocenter")){
        return Geocenter;
    } else if (strcasecmp(input_string,"XTE")){
        return XTE;
    } else if (strcasecmp(input_string,"AXAF")){
        return AXAF;
    } else if (strcasecmp(input_string,"SWIFT")){
        return SWIFT;
    } else if (strcasecmp(input_string,"NuSTAR")){
        return NuSTAR;
    } else if (strcasecmp(input_string,"HITOMI")){
        return HITOMI;
    } else {
        return Unknown;
    }
}

/* FUNCTION:  aste2mjdtt
 *
 * PURPOSE:  convert aste mission time to mjd
 *
 * INPUTS:  mission_time  - the mission time
 *               mjdrefi  - Integer component of mjd
 *               mjdreff  - Fractional component of mjd
 *
 * OUTPUTS: returns mjdtt mission time
 */
double aste2mjdtt(double mission_time, int mjdrefi, double mjdreff){
    static double DAYSEC = 24 * 60 * 60;
    double mjd_tt;

    mjd_tt = mission_time / DAYSEC + mjdreff; /* must be calculated first */
    mjd_tt += mjdrefi;

    return mjd_tt;
}


/* FUNCTION: get_version()
 *
 * PURPOSE: Report the most recent version number of this file (barycen.c)
 *
 * IN/OUTPUTS:  version - the version number
 */
void get_version(char * version){
    char ver[NEW_FLEN_VALUE];
    char * delimiter = " ";
    char * token = 0;

    strcpy(ver,REVISION);

    token = strtok(ver,delimiter);
    token = strtok(NULL,delimiter);

    strcpy(version,token);

    return;
}

/* FUNCTION: remove_substring()
 *
 * PURPOSE: remove a substring from a string
 *
 * INPUT:       s  -  input string
 *        toremove - substring to remove
 *
 * OUTPUT:     s  -  the newly modified input string
*/
void remove_substring(char *s, const char *toremove){
    while ( ( s=strstr(s,toremove) ) )
        memmove(s,s+strlen(toremove),1+strlen(s+strlen(toremove)));
}


/* FUNCTION: finalize()
 *
 * PURPOSE: Clean up any data still sitting in memory
 *
 * INPUTS:  param - Data structure containing tool parameters
 *          bcorr - Data structure containing former heagen/barycorr/bary.c global variables
 *            dpl - Data structure containing former heagen/barycorr/dpleph.c global variables
 *
 * OUTPUTS: none
 */
int finalize(PARAM * param, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){

    for (int ii=0; ii<param->num_orb_cols; ii++)
        free(param->orb_col_name[ii]);
    free(param->orb_col_name);
    
    if (dpl->buffer) free(dpl->buffer);

    if (param->history) free(param->history_string);

    free(bcorr->leapsMJD);
    free(bcorr->leapsecs);
    free(bcorr->MJDoffset);
    free(bcorr->leapcoeff);

    return 0;
}



/* FUNCTION: get_root_suffix()
 *
 * PURPOSE: isolate the root and suffix of a filename
 *
 * INPUTS: filename  - the name of the file
 *
 * OUTPUTS:    root  - the root of the filename
 *           suffix  - the suffix of the filename
 *
 */
void get_root_suffix(char * filename, char * root, char * suffix){

    /* Get the LAST occurance of the period dot in the filename string. */
    char *dot = strrchr(filename, '.');

    if (dot) {
        strcpy(root,filename);
        strcpy(suffix,dot+1);
        remove_substring(root,dot);
    } else {
        strcpy(root,filename);
        strcpy(suffix,"fits");
    }
}


/* Revision Log
   $Log: barycen.c,v $
   Revision 1.37  2016/06/03 19:57:45  rshill
   Added cartesian coordinates of source to debug output.

   Revision 1.36  2016/06/02 21:54:52  rshill
   Added radial velocity diagnostic printout; expanded tab characters in code.

   Revision 1.35  2016/06/02 20:57:56  rshill
   Corrected argument lists and cleaned up output.

   Revision 1.34  2016/06/02 01:36:50  rshill
   Implemented printout of Earth velocity and barycentric correction components for debug=yes.

   Revision 1.33  2016/03/23 15:31:28  driethmi
   Changed NaN/NULL check on original time value so that zero values are valid, i.e.

       if ( (!timeori) || (timeori != timeori) ){

   to just:

      if (timeori != timeori){

   Revision 1.32  2016/03/22 20:14:13  driethmi
   1. Modified code such that other extensions besides EVENTS or GTI may be
   barycenter-corrected.

   2. Added check for TIMEZERO keyword.  If this keyword exists and is non-zero,
   then correct the time values (in addition to barycen correction) by this value.
   Then reset TIMEZERO keyword to zero in output file.

   Revision 1.31  2016/02/18 23:57:39  klrutkow
   changed ah mission to HITOMI

   Revision 1.30  2015/12/29 18:36:50  driethmi
   Removed unnecessary parameter "orbdt" from code and par files.

   Revision 1.29  2015/11/18 21:35:57  driethmi
   Changed fits_copy_header to fits_copy_hdu.  When copying variable-length
   array columns, need data to populate in order to preservce the data type.
   Copy HDU copies this data along with header info.

   Revision 1.28  2015/11/18 16:20:25  driethmi
   Corrected wrong type declaration - ignore_null_times should be long, not int.

   Revision 1.27  2015/11/18 16:00:48  driethmi
   NULL or NaN times in the event file are now skipped for bary correction,
   and written to the output file as-is.  The number of times this happens
   for each HDU is reported.

   Revision 1.26  2015/11/17 20:35:35  driethmi
   If using KEPLERIAN orbit format, and useweights is set to "yes", then keep
   using KEPLERIAN.  If useweights is set to "no", then use KEPLERIAN_NODUP.

   Revision 1.25  2015/09/28 17:48:25  driethmi
   Removed conditional around orbpos conversion from km to m; genorbfile
   always returns orbpos in units of km, so this check is unnecessary.

   Revision 1.24  2015/09/25 19:05:26  driethmi
   We now check for keyword MJDREF (i.e., MJD as a whole), and parse into
   MJDREFI and MJDREFF.  If MJDREF not found, then check for MJDREFI and MJDREFF
   individually.

   Revision 1.23  2015/09/24 20:27:44  driethmi
   If EQUINOX keyword is bad or missing, first check to see if we can used a saved
   keyword value, or else set to J2000 and warn the user.

   Revision 1.22  2015/09/24 19:38:56  driethmi
   TSTART, TSTOP, RA_TDB, and DEC_TDB keywords now written out to higher precision,
   and not truncated with exponential notation.

   Revision 1.21  2015/09/24 19:30:43  driethmi
   Changed ra and dec written to outfile keywords RA_TDB and DEC_TDB to be
   param->ra and param->dec.

   Revision 1.20  2015/09/15 15:42:58  driethmi
   Correction to user-specified extension block, to avoid "uninitialized value"
   errors in memory checking utility.  Specifically, set char * extnum = 0;

   Revision 1.19  2015/09/14 15:38:14  driethmi
   Corrected logic in block were barycen decides whether to use TAYLOR
   interpolation method.

   Revision 1.18  2015/09/11 20:28:06  driethmi
   Added function to handle filename root and suffix strings in a more
   intelligent way.

   Revision 1.17  2015/09/10 18:13:34  driethmi
   Added NEW_FLEN_VALUE as the default string length.

   Revision 1.16  2015/09/09 18:47:04  driethmi
   Minor cosmetic changes to comments.

   Revision 1.15  2015/09/08 19:41:17  driethmi
   If we're going to overwrite the input event file, add new columns containing
   the original uncorrected time data.

   Revision 1.14  2015/09/04 18:18:52  driethmi
   Corrected typo in fits_read_key_str() where we read the column name, to
   dereference colname variable.

   Revision 1.13  2015/09/04 15:34:50  driethmi
   Made message reporting more explicit and organized, for each level of chatter.

   Revision 1.12  2015/09/03 19:19:01  driethmi
   Removed leapsecond and ephemeris files from parameters; these are hard-coded
   into the barycorr routines.

   Revision 1.11  2015/08/31 18:20:54  driethmi
   Added capability to write history keyword paramters at end of fits header.

   Revision 1.10  2015/08/31 15:00:37  driethmi
   Minor cosmetic changes in comment blocks.

   Revision 1.9  2015/08/27 21:48:58  driethmi
   Updated comment blocks preceding function definitions.

   Revision 1.8  2015/08/27 19:15:03  driethmi
   Eliminated global variables related to dpleph.c, in favor of structure
   which is passed to all functions which require it.  Code now passes
   Valgrind memory test cleanly.

   Revision 1.7  2015/08/27 17:21:28  driethmi
   Eliminated global variables copied from bary.c to from_barycorr.c with a structure
   which is passed to functions which need it.

   Revision 1.6  2015/08/27 14:54:21  driethmi
   Combined several conversion functions in from_barycorr.c, reworked "ephfile"
   so it is no longer a global variable.

   Revision 1.5  2015/08/26 19:18:52  driethmi
   Added definition for finalize().

   Revision 1.4  2015/08/25 18:13:44  driethmi
   Moved call to c200to405() until after src_dir vector is populated.

   Revision 1.3  2015/08/25 17:28:30  driethmi
   Changes to correct errstatus functionality, and rename output file in the case
   where input=output file name.

   Revision 1.2  2015/08/24 15:46:25  driethmi
   Added revision log at bottom of files.

*/

