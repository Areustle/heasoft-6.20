/**
 * \file initialize.c
 * \brief Holds functions in the "initialize" stage of the code
 * \author David Riethmiller
 * \date $Date: 2016/08/26 20:38:41 $
 */


#include "fitsio.h"       /* cfitsio defined constants */
#include "heasim.h"
#ifndef use_legacy_heasp
#include "Cheasp.h"       /* C-interface to C++ HEASP library */
#endif

#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>



 /*                                                                                                       */
 /* FUNCTION NAME: getPars                                                                                */
 /*                                                                                                       */
 /* CALLING SEQUENCE:                                                                                     */
 /*   errstatus = getPars(argc, argv, insrcdeffile, outfile, par_mode,                   */
 /*        &debug, &clobber, &getinfile, &s_calfiles, &s_back, &s_obs, mdbfile, &seed);     */
 /*                                                                                                       */
 /* PURPOSE: read in the simulation parameters from heasim.par                                            */
 /*                                                                                                       */
 /* INPUTS: argc, argv                                                                                    */
 /*                                                                                                       */
 /* OUTPUTS: infile, outfile, par_mode, debug, clobber, getinfile s_calfiles,                     */
 /*             s_back, s_obs, mdbfile, seed                                                        */
 /*                                                                                                       */
 /* EXTERNAL DEPENDENCIES                                                                                 */
 /*   ape libraries                                                                                       */
 /*                                                                                                       */ 
 /* CALLED BY                                                                                             */
 /*   main()                                                                                              */
 /*                                                                                                       */


int getPars(int argc, char ** argv,         /* string inputs */
            char * infile,                  /* Name of source input filename */
            char * outfile,                 /* Output event filename */
            char * par_mode,                /* Mode for parameter file */
            int * debug,                    /* Flag for setting debuggin mode (BOOLEAN) */
            int * clobber,                  /* Overwrite existing output file? (BOOLEAN) */
            int * getinfile,        /* Copy sample source def file to current working dir?  (BOOLEAN) */
            CalFiles * s_calfiles,          /* Structure containing cal filenames (see definition in main) */
            GeneralBackground * s_back,     /* Structure containing background filenames (see def in main) */
            ObsParams * s_obs,              /* Structure containing observation parameters (see def in main) */
            char * mdbfile,                 /* Name of mission database file */
            long * seed){             /* Seed for RNG */

    
    /* boolean input - will return as int */
    char local_getinfile;     /* draw info from user-file? */         
    char local_debug;                 /* debug mode? */
    char local_clobber;               /* overwite output files? */
    char bool_flagsubex;
    char bool_resample;
    char bool_skipfov;
    
    /* internal variables */
    int status= eOK;            /* status tracker */
    int num_vars;               /* number of parameters we expect to find */
    int * status_array;         /* array of flags indicating whether we found that parameter */
    int ii;                     /* generic loop index */
    char * temp = 0;                /* a temporary buffer string */
    char * temp2 = 0;               /* temporary string filename */

    /* Make a list of parameters to be read.  Truncate list with NULL. Make sure params are read in same order below. */
    char * param_list[] = {"mission",
                           "instrume",
                           "filter",
                           "instmode",
                           "rapoint",
                           "decpoint",
                           "roll",
                           "exposure",
                           "flagsubex",
                           "subexposure",
                           "resample",
                           "skipfov",
                           "insrcdeffile",
                           "outfile",
                           "psffile",
                           "vigfile",
                           "rmffile",
                           "arffile",
                           "arfrmftol",
                           "intbackfile",
                           "psbackfile",
                           "difbackfile",
                           "pszbackfile",
                           "dtpileup",
                           "getinfile",
                           "seed",
                           "mdbfile",
                           "clobber",
                           "debug",
                           "mode",
                           NULL};

    /* Get the number of non-NULL variables in the param list */
    for (num_vars = 0; param_list[num_vars] != NULL; num_vars++);

    status_array = calloc(num_vars, sizeof(int));

    status = ape_trad_init(argc, argv);
    if (eOK != status) {
        printf("heasim: ape_trad_init failed\n");
        printf("heasim: check PFILES paths for heasimc.par\n");
        return -1;
    }
    
    ii=0;

    /* APE populate observatory structure.  String values must be strcpy'ed. */
    status_array[ii++] = ape_trad_query_string("mission",&temp);
    strcpy(s_obs->mission,temp);
    free(temp);  /* ape allocates temp, must free! */
    
    status_array[ii++] = ape_trad_query_string("instrume",&temp);
    strcpy(s_obs->instrume,temp);
    free(temp);  /* ape allocates temp, must free! */
    
    status_array[ii++] = ape_trad_query_string("filter",&temp);
    strcpy(s_obs->filter,temp);
    free(temp);  /* ape allocates temp, must free! */
    
    status_array[ii++] = ape_trad_query_string("instmode",&temp);
    strcpy(s_obs->instmode,temp);
    free(temp);  /* ape allocates temp, must free! */
    
    status_array[ii++] = ape_trad_query_double("rapoint", &s_obs->rapoint);
    status_array[ii++] = ape_trad_query_double("decpoint", &s_obs->decpoint);
    status_array[ii++] = ape_trad_query_double("roll", &s_obs->roll);
    status_array[ii++] = ape_trad_query_double("exposure", &s_obs->exposure);
    
    status_array[ii++] = ape_trad_query_bool("flagsubex", &bool_flagsubex);
    if (1 == bool_flagsubex){ s_obs->flagsubex = 1; } else { s_obs->flagsubex = 0;}

    if (s_obs->flagsubex){
        status_array[ii++] = ape_trad_query_double("subexposure", &s_obs->subexposure);
    } else {
        status_array[ii++] = eOK;
    }

    status_array[ii++] = ape_trad_query_bool("resample", &bool_resample);
    if (1 == bool_resample){ s_obs->resample = 1; } else { s_obs->resample = 0;}

    status_array[ii++] = ape_trad_query_bool("skipfov", &bool_skipfov);
    if (1 == bool_skipfov){ s_obs->skipfov = 1; } else { s_obs->skipfov = 0;}

    /* APE populate loose parameters.  String values must be strcpy'ed. */
    status_array[ii++] = ape_trad_query_file_name("insrcdeffile", &temp);
    strcpy(infile,temp);
    free(temp);  /* ape allocates temp, must free! */
    
    status_array[ii++] = ape_trad_query_file_name("outfile", &temp);
    strcpy(outfile,temp);
    free(temp);  /* ape allocates temp, must free! */    

    /* APE populate calibration file structure. String values must be strcpy'ed. */
    status_array[ii++] = ape_trad_query_file_name("psffile", &temp);
    strcpy(s_calfiles->psffile,temp);
    temp2 = resolve_pathname(s_calfiles->psffile);
    strcpy(s_calfiles->psffile,temp2);
    free(temp);  /* ape allocates temp, must free! */
    if (0 != strcasecmp(temp2,"none")) free(temp2);

    status_array[ii++] = ape_trad_query_file_name("vigfile", &temp);
    strcpy(s_calfiles->vigfile,temp);
    temp2 = resolve_pathname(s_calfiles->vigfile);
    strcpy(s_calfiles->vigfile,temp2);
    free(temp);  /* ape allocates temp, must free! */
    if (0 != strcasecmp(temp2,"none")) free(temp2);
    
    status_array[ii++] = ape_trad_query_file_name("rmffile", &temp);
    strcpy(s_calfiles->rmffile,temp);
    temp2 = resolve_pathname(s_calfiles->rmffile);
    strcpy(s_calfiles->rmffile,temp2);
    if (0 != strcasecmp(temp2,"none")) free(temp2);
    free(temp);  /* ape allocates temp, must free! */

    status_array[ii++] = ape_trad_query_file_name("arffile", &temp);
    strcpy(s_calfiles->arffile,temp);
    temp2 = resolve_pathname(s_calfiles->arffile);
    strcpy(s_calfiles->arffile,temp2);
    free(temp);  /* ape allocates temp, must free! */
    if (0 != strcasecmp(temp2,"none")) free(temp2);

    status_array[ii++] = ape_trad_query_double("arfrmftol", &s_calfiles->arfrmftol);
    
    /* APE populate background file structure. String values must be strcpy'ed. */
    status_array[ii++] = ape_trad_query_file_name("intbackfile",&temp);
    strcpy(s_calfiles->intbackfile,temp);
    temp2 = resolve_pathname(s_calfiles->intbackfile);
    strcpy(s_calfiles->intbackfile,temp2);
    free(temp);  /* ape allocates temp, must free! */
    if (0 != strcasecmp(temp2,"none")) free(temp2);

    status_array[ii++] = ape_trad_query_file_name("psbackfile",&temp);
    strcpy(s_back->psbackfile,temp);
    temp2 = resolve_pathname(s_back->psbackfile);
    strcpy(s_back->psbackfile,temp2);
    free(temp);  /* ape allocates temp, must free! */
    if (0 != strcasecmp(temp2,"none")) free(temp2);

    status_array[ii++] = ape_trad_query_file_name("difbackfile",&temp);
    strcpy(s_back->difbackfile,temp);
    temp2 = resolve_pathname(s_back->difbackfile);
    strcpy(s_back->difbackfile,temp2);
    free(temp);  /* ape allocates temp, must free! */
    if (0 != strcasecmp(temp2,"none")) free(temp2);

    status_array[ii++] = ape_trad_query_file_name("pszbackfile",&temp);
    strcpy(s_back->pszbackfile,temp);
    temp2 = resolve_pathname(s_back->pszbackfile);
    strcpy(s_back->pszbackfile,temp2);
    free(temp);  /* ape allocates temp, must free! */
    if (0 != strcasecmp(temp2,"none")) free(temp2);

    /* Get the pileup time scale */
    status_array[ii++] = ape_trad_query_double("dtpileup",&s_obs->dtpileup);

    status_array[ii++] = ape_trad_query_bool("getinfile", &local_getinfile);

    /* Get the user input seed */
    status_array[ii++] = ape_trad_query_long("seed",seed);

    /* Get the mission database file name */
    status_array[ii++] = ape_trad_query_file_name("mdbfile", &temp);
    strcpy(mdbfile,temp);
    temp2 = resolve_pathname(mdbfile);
    strcpy(mdbfile,temp2);
    free(temp);  /* ape allocates temp, must free! */    
    if (0 != strcasecmp(temp2,"none")) free(temp2);

    status_array[ii++] = ape_trad_query_bool("clobber", &local_clobber);
    status_array[ii++] = ape_trad_query_bool("debug", &local_debug);

    status_array[ii++] = ape_trad_query_string("mode", &temp);
    strcpy(par_mode,temp);
    free(temp);  /* ape allocates temp, must free! */


    /* Check that we read everything ok */
    for (ii=0; ii<num_vars; ii++)
        if (status_array[ii] != eOK){
            printf("problem with entry number %d: %s!\n",ii, param_list[ii]);
            status=-1;
        }
    
    
    /* analyze the boolean APE inputs, assign as ints */
    if (1 == local_debug){
        *debug = 1;
    } else *debug = 0;
    if (1 == local_clobber){
        *clobber = 1;
    } else *clobber = 0;

    /* Copy heasim_source_sample.txt to the local working directory if specified,
       and use it as the source file instead of the previously set infile */
    if (1 == local_getinfile){
        char *sample_infile=0;     /* name of sample input file */
        char ch=0;                 /* individual character to be written */
        char line[STR_MAX_LEN];    /* line in file to be read */
        char *junk=0;              /* dummy string variable */
        FILE *source=0;            /* file to be copied */
        FILE *target=0;            /* file to be written */
        char * relpath = calloc(STR_MAX_LEN,sizeof(char));  /* relative path to sample input file */
        

        /* Get the absolute file path name */
        strcpy(relpath,"$LHEA_DATA/heasim_source_sample.txt");
        sample_infile = resolve_pathname(relpath);
        printf("Copying %s to current directory.\n",sample_infile);

        /* Read and copy the file to local directory */
        source = fopen(sample_infile, "r");
        target = fopen("./heasim_source_sample.txt", "w");
        while( ( ch = fgetc(source) ) != EOF )
            fputc(ch, target);
        fclose(source);
        fclose(target);

        /* Reset the output fits file name */
        strcpy(infile,"heasim_source_sample.txt");
        temp = calloc(STR_MAX_LEN,sizeof(char));
        sprintf(temp,"output/%s_%s_source_sample.fits",s_obs->mission,s_obs->instrume);
        strcpy(outfile,temp);
        free(temp);

        /* Parse the sample input file for the correct strings to overwrite pointing RA and DEC */
        target = fopen("./heasim_source_sample.txt", "r");
        while(fgets(line,sizeof(line),target) != NULL){
            if (strncasecmp("# !!! RA",line,8) == 0){
                junk = strtok(line,"=");
                s_obs->rapoint = atof(strtok(NULL,"="));
            } else if (strncasecmp("# !!! DEC",line,9) == 0){
                junk = strtok(line,"=");
                s_obs->decpoint = atof(strtok(NULL,"="));
            }
        }
        fclose(target);
        printf("Overwriting pointing RA and Dec to [%f,%f].\n",s_obs->rapoint,s_obs->decpoint);

        *getinfile = 1;
        free(sample_infile);  /* allocated by resolve_pathname */
        free(relpath);

    } else *getinfile = 0;
    
    free(status_array);

    return status;
}



/*                                                                                                      */
/* FUNCTION NAME:  read_mdb                                                                             */
/*                                                                                                      */
/* CALLING SEQUENCE:                                                                                    */
/*     errstatus = read_mdb(mdbfile, mission, instrume, filter, instmode, &s_mdbm debug);  */
/*                                                                                                      */
/* PURPOSE: Read the mission database file and parse the necessary parameters                           */
/*                                                                                                      */
/* INPUTS: mdbfile, mission, instrume, filter, mode                                               */
/*                                                                                                      */
/* OUTPUTS: the s_mdb structure                                                                         */
/*                                                                                                      */
/* CALLED BY:                                                                                           */
/*    main()                                                                                            */
/*                                                                                                      */
/*                                                                                                      */


int read_mdb(char * mdbfile,                /* MDB filename */
             const char * mission,      /* Observatory name */
             const char * instrume,       /* Instrument name */
             const char * filter,           /* Filter name */
             const char * mode,             /* Mode (CCD read mode?) */
             MDB_params * s_mdb,
             int debug){

    /* MDB variables */
    double FOV_RADIUS = 0.0; 
    double FOCALLEN = 0.0;
    double PSF_FWHM = 0.0;
    double DET_XSCL = 0.0;
    double DET_YSCL = 0.0;
    int DET_XNPIX = 0;  
    int DET_YNPIX = 0; 
    double HEA_XPIXSIZ = 0.0;    
    double HEA_YPIXSIZ = 0.0;
    int HEA_XNPIX = 0;   
    int HEA_YNPIX = 0;   
    double SKY_XPIXSIZ = 0.0;
    double SKY_YPIXSIZ = 0.0;
    int SKY_XNPIX = 0;
    int SKY_YNPIX = 0;
    double FOC_XPIXSIZ = 0.0;
    double FOC_YPIXSIZ = 0.0;
    int FOC_XNPIX = 0;
    int FOC_YNPIX = 0;
    double OPTAXIS_FOCX = 0.0;
    double OPTAXIS_FOCY = 0.0;
    double AIM_FOCX = 0.0;
    double AIM_FOCY = 0.0;
    double FOC_ROTD = 0.0;
    double FOC_XOFF = 0.0;
    double FOC_YOFF = 0.0;
    char INSTMAP[STR_MAX_LEN];
    
    /* local variables */
    FILE *fp;              /* The file buffer */
    int numlines=0;        /* Number of valid lines found */
    int  n_commentlines=0; /* Number of comment or empty lines */
    int i=0, thisi=0;          /* generic loop index */ 
    int count=0;             /* number of ":" found in the line */    
    int  len=0;              /* string length */
    int errstatus=0;       /* error status tracker */
    char str[STR_MAX_LEN];         /* string line read from file */

    /* string segments broken off from file line via strtok(), to be converted to appropriate data types */
    char *tel=0, *det=0, *fil=0, *mod=0, *key=0, *val=0;

    /* mission, instrume, filter, mode may need special case consideration, make sure we don't overwrite */
    char mission1[STR_MAX_LEN], instrume1[STR_MAX_LEN], filter1[STR_MAX_LEN], mode1[STR_MAX_LEN];
    
    int combofound=0;       /* does a matching combination of mission/detector/filter/mode exist? */
    
    int numflags=27;        /* number of keyword flags to fill */
    int * flagvals=0;         /* int array telling whether we've found each keyword */
    char ** flagnames=0;      /* names of each keyword we expect to find */
    
    flagvals = calloc(numflags, sizeof(int));
    flagnames = calloc(numflags, sizeof(char *));

    flagnames[0] = "FOV_RADIUS";
    flagnames[1] = "FOCALLEN";
    flagnames[2] = "PSF_FWHM";
    flagnames[3] = "DET_XSCL";
    flagnames[4] = "DET_YSCL";
    flagnames[5] = "DET_XNPIX";
    flagnames[6] = "DET_YNPIX";
    flagnames[7] = "HEA_XPIXSIZ";
    flagnames[8] = "HEA_YPIXSIZ";
    flagnames[9] = "HEA_XNPIX";
    flagnames[10] = "HEA_YNPIX";
    flagnames[11] = "SKY_XPIXSIZ";
    flagnames[12] = "SKY_YPIXSIZ";
    flagnames[13] = "SKY_XNPIX";
    flagnames[14] = "SKY_YNPIX";
    flagnames[15] = "FOC_XPIXSIZ";
    flagnames[16] = "FOC_YPIXSIZ";
    flagnames[17] = "FOC_XNPIX";
    flagnames[18] = "FOC_YNPIX";
    flagnames[19] = "OPTAXIS_FOCX";
    flagnames[20] = "OPTAXIS_FOCY";
    flagnames[21] = "AIM_FOCX";
    flagnames[22] = "AIM_FOCY";
    flagnames[23] = "FOC_ROTD";
    flagnames[24] = "FOC_XOFF";
    flagnames[25] = "FOC_YOFF";
    flagnames[26] = "INSTMAP";

    /* set all flagvals to zero; will increment if flagname is found. */
    for (i=0; i<numflags; i++)  flagvals[i] = 0;
    

    /* Handle a few special cases, where detector truncations may be appropriate. For example,
     instrume might contain a string "emos1", meaning the mdb file EMOS entries are the ones
     we want.  We've created the strings mission1, instrume1, filter1, and mode1 to ensure that we don't
     accidentally overwrite mission, instrume, filter, and mode. */

    if ( 0 == strcasecmp(mission,"XMM") ){
        strcpy(mission1,"XMM");
        if ( (0 == strcasecmp(instrume,"MOS")) ||
             (0 == strcasecmp(instrume,"MOS1")) ||
             (0 == strcasecmp(instrume,"EMOS1")) ||
             (0 == strcasecmp(instrume,"MOS2")) ||
             (0 == strcasecmp(instrume,"EMOS2")) ){
            strcpy(instrume1,"EMOS");

        } else if ( (0 == strcasecmp(instrume,"PN")) ||
                    (0 == strcasecmp(instrume,"EPN")) ){
            strcpy(instrume1,"EPN");

        } else{
            strcpy(instrume1,instrume);
        }

        strcpy(filter1,filter);
        strcpy(mode1,mode);

    } else{
        strcpy(mission1,mission);
        strcpy(instrume1,instrume);
        strcpy(filter1,filter);
        strcpy(mode1,mode);
    }
    
    /* As of now, the hard-coded obsveratory parameters did not bother to match
     filter or mode inputs.  We will also set them to zero-length strings
     for now as well.  */
    if (debug == 1) printf("Ignoring filter %s for mdb purposes.\n",filter1);
    strcpy(filter1,"");
    if (debug == 1) printf("Ignoring mode %s for mdb purposes.\n",mode1);
    strcpy(mode1,"");
    
    printf("Reading mission database file %s ...\n",mdbfile);
    if ((fp = fopen(mdbfile,"r"))){
        
        while(fgets(str,sizeof(str),fp) != NULL){
            
            /* strip trailing '\n' if it exists */
            len = strlen(str)-1;
            if(str[len] == '\n')
                str[len] = 0;
            
            if ( (str[0] == '!') || (strlen(str) == 0) ){/* ignore commented lines denoted by !*/
                n_commentlines++;
            }else{
                numlines++;
                /* Count the number of ":" in the string to determine how many data
                 fields exist.*/
                for (i=0, count=0; str[i]; i++)  count +=(str[i] == ':');
                /* printf("str: %s, count = %d\n",str,count); */
                
                /* Parse the line into the relevent fields */
                switch(count){
                    case 0:
                        printf("ERROR: too few fields in mdb line format.\n");
                        printf("  Fields should be delimited by ':' marker.\n");
                        printf("  %s\n",str);
                        errstatus=1;
                        break;
                        
                    case 1:
                        tel=strtok(str,":");
                        det = "";
                        fil = "";
                        mod = "";
                        key = strtok(NULL,":");
                        /* The actual value is delimited by a space or tab rather than : */
                        key = strtok(key," \t");
                        val = strtok(NULL," \t");
                        break;
                        
                    case 2:
                        tel=strtok(str,":");
                        det = strtok(NULL,":");
                        fil = "";
                        mod = "";
                        key = strtok(NULL,":");
                        key = strtok(key," \t ");
                        val = strtok(NULL," \t");
                        break;
                        
                    case 3:
                        tel=strtok(str,":");
                        det = strtok(NULL,":");
                        fil = strtok(NULL,":");
                        mod = "";
                        key = strtok(NULL,":");
                        key = strtok(key," \t");
                        val = strtok(NULL," \t");
                        break;
                        
                    case 4:
                        tel=strtok(str,":");
                        det = strtok(NULL,":");
                        fil = strtok(NULL,":");
                        mod = strtok(NULL,":");
                        key = strtok(NULL,":");
                        key = strtok(key," \t");
                        val = strtok(NULL," \t");
                        break;
                        
                    default:
                        printf("ERROR: too many fields in mdb line format:\n");
                        printf("  %s\n",str);
                        errstatus=1;
                        break;
                }
                
                if (errstatus == 0){
                    /* Populate the correct tags for the instrument, by matching case-independent strings */
                    if (0 == strcasecmp(tel,mission1)){
                        if (0 == strcasecmp(det,instrume1)){
                            if (0 == strcasecmp(fil,filter1)){
                                if (0 == strcasecmp(mod,mode1)){
                                    combofound=1;
                                    if (0 == strcasecmp(key,flagnames[0])){  /* FOV_RADIUS */
                                        FOV_RADIUS = atof(val);
                                        flagvals[0]++;
                                        thisi = 0;
                                    }else if (0 == strcasecmp(key, flagnames[1])){ /* FOCALLEN */
                                        FOCALLEN = atof(val);
                                        flagvals[1]++;
                                        thisi = 1;
                                    }else if (0 == strcasecmp(key,flagnames[2])){  /* PSF_FWHM */
                                        PSF_FWHM = atof(val);
                                        flagvals[2]++;
                                        thisi = 2;
                                    }else if (0 == strcasecmp(key,flagnames[3])){ /* DET_XSCL */
                                        DET_XSCL = atof(val);
                                        flagvals[3]++;
                                        thisi = 3;
                                    }else if (0 == strcasecmp(key,flagnames[4])){ /* DET_YSCL */
                                        DET_YSCL = atof(val);
                                        flagvals[4]++;
                                        thisi = 4;
                                    }else if (0 == strcasecmp(key,flagnames[5])){ /* DET_XNPIX */
                                        DET_XNPIX = atoi(val);
                                        flagvals[5]++;
                                        thisi = 5;
                                    }else if (0 == strcasecmp(key,flagnames[6])){ /* DET_YNPIX */
                                        DET_YNPIX = atoi(val);
                                        flagvals[6]++;
                                        thisi = 6;
                                    }else if (0 == strcasecmp(key,flagnames[7])){ /* HEA_XPIXSIZ */
                                        HEA_XPIXSIZ = atof(val);
                                        flagvals[7]++;
                                        thisi = 7;
                                    }else if (0 == strcasecmp(key,flagnames[8])){ /* HEA_YPIXSIZ */
                                        HEA_YPIXSIZ = atof(val);
                                        flagvals[8]++;
                                        thisi = 8;
                                    }else if (0 == strcasecmp(key,flagnames[9])){ /* HEA_XNPIX */
                                        HEA_XNPIX = atoi(val);
                                        flagvals[9]++;
                                        thisi = 9;
                                    }else if (0 == strcasecmp(key,flagnames[10])){ /* HEA_YNPIX */
                                        HEA_YNPIX = atoi(val);
                                        flagvals[10]++;
                                        thisi = 10;
                                    }else if (0 == strcasecmp(key,flagnames[11])){ /* SKY_XPIXSIZ */
                                        SKY_XPIXSIZ = atof(val);
                                        flagvals[11]++;
                                        thisi = 11;
                                    }else if (0 == strcasecmp(key,flagnames[12])){ /* SKY_YPIXSIZ */
                                        SKY_YPIXSIZ = atof(val);
                                        flagvals[12]++;
                                        thisi = 12;
                                    }else if (0 == strcasecmp(key,flagnames[13])){ /* SKY_XNPIX */
                                        SKY_XNPIX = atoi(val);
                                        flagvals[13]++;
                                        thisi = 13;
                                    }else if (0 == strcasecmp(key,flagnames[14])){ /* SKY_YNPIX */
                                        SKY_YNPIX = atoi(val);
                                        flagvals[14]++;
                                        thisi = 14;
                                    }else if (0 == strcasecmp(key,flagnames[15])){ /* FOC_XPIXSIZ */
                                        FOC_XPIXSIZ = atof(val);
                                        flagvals[15]++;
                                        thisi = 15;
                                    }else if (0 == strcasecmp(key,flagnames[16])){ /* FOC_YPIXSIZ */
                                        FOC_YPIXSIZ = atof(val);
                                        flagvals[16]++;
                                        thisi = 16;
                                    }else if (0 == strcasecmp(key,flagnames[17])){ /* FOC_XNPIX */
                                        FOC_XNPIX = atoi(val);
                                        flagvals[17]++;
                                        thisi = 17;
                                    }else if (0 == strcasecmp(key,flagnames[18])){ /* FOC_YNPIX */
                                        FOC_YNPIX = atoi(val);
                                        flagvals[18]++;
                                        thisi = 18;
                                    }else if (0 == strcasecmp(key,flagnames[19])){ /* OPTAXIS_FOCX */
                                        OPTAXIS_FOCX = atof(val);
                                        flagvals[19]++;
                                        thisi = 19;
                                    }else if (0 == strcasecmp(key,flagnames[20])){ /* OPTAXIS_FOCY */
                                        OPTAXIS_FOCY = atof(val);
                                        flagvals[20]++;
                                        thisi = 20;
                                    }else if (0 == strcasecmp(key,flagnames[21])){ /* AIM_FOCX */
                                        AIM_FOCX = atof(val);
                                        flagvals[21]++;
                                        thisi = 21;
                                    }else if (0 == strcasecmp(key,flagnames[22])){ /* AIM_FOCY */
                                        AIM_FOCY = atof(val);
                                        flagvals[22]++;
                                        thisi = 22;
                                    }else if (0 == strcasecmp(key,flagnames[23])){ /* FOC_ROTD */
                                        FOC_ROTD = atof(val);
                                        flagvals[23]++;
                                        thisi = 23;
                                    }else if (0 == strcasecmp(key,flagnames[24])){ /* FOC_XOFF */
                                        FOC_XOFF = atof(val);
                                        flagvals[24]++;
                                        thisi = 24;
                                    }else if (0 == strcasecmp(key,flagnames[25])){ /* FOC_YOFF */
                                        FOC_YOFF = atof(val);
                                        flagvals[25]++;
                                        thisi = 25;
                                    }else if (0 == strcasecmp(key,flagnames[26])){ /* INSTMAP */
                                        strcpy(INSTMAP,val);
                                        flagvals[26]++;
                                        thisi = 26;
                                }

                                    /* for debugging purposes */
                                    /* printf("Found %s %d.\n",flagnames[thisi], thisi); */
                                }
                            }
                        }
                    }
                }
            }
        }
        fclose(fp);
        
        if (errstatus == 0){
            /* Check to see if we found the relevent fields in mdb file. */
            if ( combofound < 1){
                printf("\nError: this combination of mission/detector/filter/mode\n");
                printf("does not exist in the mission database.\n");
                printf("%s %s %s %s\n",mission1,instrume1,filter1,mode1);
                errstatus=1;
            }
            
            /* Check to ensure we found exactly one value for each keyword in this combination */
            for (i=0; i<numflags; i++){
                if (flagvals[i] < 1){
                    printf("\nERROR: Missing mdb entry for %s:%s:%s !!!\n",
                           mission1,instrume1,flagnames[i]);
                    errstatus=1;
                }
                if (flagvals[i] > 1){
                    printf("\nERROR: Multiple mdb entries for %s:%s:%s !!!\n",
                           mission1,instrume1,flagnames[i]);
                    errstatus=1;
                }
                
            }
        }
        if (debug == 1){
            printf("mdb: Read %d entry lines, %d comments/spaces.\n",numlines,n_commentlines);
        } else {
            printf("...done.\n");
        }

    }else{
        printf("Error: Mission database file not found.\n");
        errstatus=1;
    }
    
    free(flagvals);
    free(flagnames);

    if (debug == 1){
        printf("\nFrom inside read_mdb:\n\n");
        printf("Observatory: %s  Detector: %s  Filter: %s  Mode:%s\n",
               mission1,instrume1,filter1,mode1);
        printf("    FOV_RADIUS:    %f\n",  FOV_RADIUS);
        printf("    FOCALLEN:      %f\n",  FOCALLEN);
        printf("    PSF_FWHM:      %f\n",  PSF_FWHM);
        printf("    DET_XSCL:      %f\n",  DET_XSCL);
        printf("    DET_YSCL:      %f\n",  DET_YSCL);
        printf("    DET_XNPIX:     %d\n",  DET_XNPIX);
        printf("    DET_YNPIX:     %d\n",  DET_YNPIX);
        printf("    HEA_XPIXSIZ:   %.15f\n",  HEA_XPIXSIZ);
        printf("    HEA_YPIXSIZ:   %.15f\n",  HEA_YPIXSIZ);
        printf("    HEA_XNPIX:     %d\n",  HEA_XNPIX);
        printf("    HEA_YNPIX:     %d\n",  HEA_YNPIX);
        printf("    SKY_XPIXSIZ    %.15f\n",  SKY_XPIXSIZ);
        printf("    SKY_YPIXSIZ    %.15f\n",  SKY_YPIXSIZ);
        printf("    SKY_XNPIX:     %d\n",  SKY_XNPIX);
        printf("    SKY_YNPIX:     %d\n",  SKY_YNPIX);
        printf("    FOC_XPIXSIZ:   %.15f\n",  FOC_XPIXSIZ);
        printf("    FOC_YPIXSIZ:   %.15f\n",  FOC_YPIXSIZ);
        printf("    FOC_XNPIX:     %d\n",  FOC_XNPIX);
        printf("    FOC_YNPIX:     %d\n",  FOC_YNPIX);
        printf("    OPTAXIS_FOCX:  %f\n",  OPTAXIS_FOCX);
        printf("    OPTAXIS_FOCY:  %f\n",  OPTAXIS_FOCY);
        printf("    AIM_FOCX:      %f\n",  AIM_FOCX);
        printf("    AIM_FOCY:      %f\n",  AIM_FOCY);
        printf("    FOC_ROTD:      %f\n",  FOC_ROTD);
        printf("    FOC_XOFF:      %f\n",  FOC_XOFF);
        printf("    FOC_YOFF:      %f\n",  FOC_YOFF);
        printf("    INSTMAP:       %s\n",  INSTMAP);
    }

    /* populate s_mdb direct parameters */
    s_mdb->FOV_RADIUS = FOV_RADIUS;
    s_mdb->FOCALLEN = FOCALLEN;
    s_mdb->PSF_FWHM = PSF_FWHM;
    s_mdb->DET_XSCL = DET_XSCL;
    s_mdb->DET_YSCL = DET_YSCL;
    s_mdb->DET_XNPIX = DET_XNPIX;
    s_mdb->DET_YNPIX = DET_YNPIX;
    s_mdb->HEA_XPIXSIZ = HEA_XPIXSIZ;
    s_mdb->HEA_YPIXSIZ = HEA_YPIXSIZ;
    s_mdb->HEA_XNPIX = HEA_XNPIX;
    s_mdb->HEA_YNPIX = HEA_YNPIX;
    s_mdb->SKY_XNPIX = SKY_XNPIX;
    s_mdb->SKY_YNPIX = SKY_YNPIX;
    s_mdb->FOC_XPIXSIZ = FOC_XPIXSIZ;
    s_mdb->FOC_YPIXSIZ = FOC_YPIXSIZ;
    s_mdb->FOC_XNPIX = FOC_XNPIX;
    s_mdb->FOC_YNPIX = FOC_YNPIX;
    s_mdb->OPTAXIS_FOCX = OPTAXIS_FOCX;
    s_mdb->OPTAXIS_FOCY = OPTAXIS_FOCY;
    s_mdb->AIM_FOCX = AIM_FOCX;
    s_mdb->AIM_FOCY = AIM_FOCY;
    s_mdb->FOC_ROTD = FOC_ROTD;
    s_mdb->FOC_XOFF = FOC_XOFF;
    s_mdb->FOC_YOFF = FOC_YOFF;
    strcpy(s_mdb->instmap_name,INSTMAP);

    /* populate s_mdb derived parameters (some are redundant...) */
    s_mdb->plate_scale = (360.0 / TWOPI) / (FOCALLEN);
    s_mdb->psf_fwhm = PSF_FWHM;
    s_mdb->cdltx = HEA_XPIXSIZ; /*x decrement*/
    s_mdb->cdlty = HEA_YPIXSIZ; /*y decrement*/
    s_mdb->cdltx_sky = -SKY_XPIXSIZ;  /*sky-x decrement*/
    s_mdb->cdlty_sky = SKY_YPIXSIZ;  /*sky-y decrement*/
    s_mdb->cdltx_det = -s_mdb->plate_scale * DET_XSCL; /*det-x decrement*/
    s_mdb->cdlty_det =  s_mdb->plate_scale * DET_YSCL; /*det-y decrement*/
    s_mdb->crpxx_sky = (SKY_XNPIX + 1.) / 2.0; /*sky-x reference pixel:*/
    s_mdb->crpxy_sky = (SKY_YNPIX + 1.) / 2.0; /*sky-y reference pixel:*/
    s_mdb->xmin_sky = 1;
    s_mdb->xmax_sky = SKY_XNPIX;
    s_mdb->ymin_sky = 1;
    s_mdb->ymax_sky = SKY_YNPIX;
    s_mdb->cdltx_foc = -FOC_XPIXSIZ; /*foc-x decrement*/
    s_mdb->cdlty_foc =  FOC_YPIXSIZ; /*foc-y decrement*/
    s_mdb->resampx = s_mdb->cdltx_det / s_mdb->cdltx_foc; /*resampling factor x*/
    s_mdb->resampy = s_mdb->cdlty_det / s_mdb->cdlty_foc; /*resampling factor y*/
    s_mdb->xmin = -0.5 * HEA_XNPIX;
    s_mdb->xmax =  0.5 * HEA_XNPIX;
    s_mdb->ymin = -0.5 * HEA_YNPIX;
    s_mdb->ymax =  0.5 * HEA_YNPIX;
    s_mdb->xmin_foc = 1;
    s_mdb->xmax_foc = FOC_XNPIX;
    s_mdb->ymin_foc = 1;
    s_mdb->ymax_foc = FOC_YNPIX;
    s_mdb->xmin_det = 1;
    s_mdb->xmax_det = (s_mdb->cdltx_det/s_mdb->cdltx_foc) * DET_XNPIX;
    s_mdb->ymin_det = 1;
    s_mdb->ymax_det = (s_mdb->cdlty_det/s_mdb->cdlty_foc) * DET_YNPIX;
    s_mdb->crpxx_det = (s_mdb->xmax_det + 1.)/2.0; /*resampled det-x ref. pix.*/
    s_mdb->crpxy_det = (s_mdb->ymax_det + 1.)/2.0; /*resampled det-y ref. pix.*/
    s_mdb->FOC_ROTD_sin = sin(D2R*FOC_ROTD);
    s_mdb->FOC_ROTD_cos = cos(D2R*FOC_ROTD);
    s_mdb->crpxx_foc = (FOC_XNPIX + 1.) / 2.0; /*foc-x reference pixel:*/
    s_mdb->crpxy_foc = (FOC_YNPIX + 1.) / 2.0; /*foc-y reference pixel:*/
    strcpy(s_mdb->instmap_name,INSTMAP);
    if (0 == strcasecmp(s_mdb->instmap_name,"none")){
        s_mdb->instmap_flag=0;
    } else {
        s_mdb->instmap_flag=1;
    }
    s_mdb->FOV_RADIUS = FOV_RADIUS;

    if (debug == 1){
        FILE * fp1 = fopen("output/debug_mdb.txt","w");
        printf("Writing mdb debug file output/debug_mdb.txt.\n");

        fprintf(fp1,"MISSION DATABASE PARAMETERS, AS READ:\n");
        fprintf(fp1,"FOV_RADIUS    = %f\n",s_mdb->FOV_RADIUS);
        fprintf(fp1,"FOCALLEN      = %f\n",s_mdb->FOCALLEN);
        fprintf(fp1,"PSF_FWHM      = %f\n",s_mdb->PSF_FWHM);
        fprintf(fp1,"DET_XSCL      = %f\n",s_mdb->DET_XSCL);
        fprintf(fp1,"DET_YSCL      = %f\n",s_mdb->DET_YSCL);
        fprintf(fp1,"DET_XNPIX     = %f\n",s_mdb->DET_XNPIX);
        fprintf(fp1,"DET_YNPIX     = %f\n",s_mdb->DET_YNPIX);
        fprintf(fp1,"HEA_XPIXSIZ   = %f\n",s_mdb->HEA_XPIXSIZ);
        fprintf(fp1,"HEA_YPIXSIZ   = %f\n",s_mdb->HEA_YPIXSIZ);
        fprintf(fp1,"HEA_XNPIX     = %f\n",s_mdb->HEA_XNPIX);
        fprintf(fp1,"HEA_YNPIX     = %f\n",s_mdb->HEA_YNPIX);
        fprintf(fp1,"SKY_XNPIX     = %f\n",s_mdb->SKY_XNPIX);
        fprintf(fp1,"SKY_YNPIX     = %f\n",s_mdb->SKY_YNPIX);
        fprintf(fp1,"FOC_XPIXSIZ   = %f\n",s_mdb->FOC_XPIXSIZ);
        fprintf(fp1,"FOC_YPIXSIZ   = %f\n",s_mdb->FOC_YPIXSIZ);
        fprintf(fp1,"FOC_XNPIX     = %f\n",s_mdb->FOC_XNPIX);
        fprintf(fp1,"FOC_YNPIX     = %f\n",s_mdb->FOC_YNPIX);
        fprintf(fp1,"OPTAXIS_FOCX  = %f\n",s_mdb->OPTAXIS_FOCX);
        fprintf(fp1,"OPTAXIS_FOCY  = %f\n",s_mdb->OPTAXIS_FOCY);
        fprintf(fp1,"AIM_FOCX      = %f\n",s_mdb->AIM_FOCX);
        fprintf(fp1,"AIM_FOCY      = %f\n",s_mdb->AIM_FOCY);
        fprintf(fp1,"FOC_ROTD      = %f\n",s_mdb->FOC_ROTD);
        fprintf(fp1,"FOC_XOFF      = %f\n",s_mdb->FOC_XOFF);
        fprintf(fp1,"FOC_YOFF      = %f\n",s_mdb->FOC_YOFF);
        fprintf(fp1,"\n");

        fprintf(fp1,"MISSION DATABASE PARAMETERS, AS DERIVED:\n");
        fprintf(fp1,"plate_scale   = %f\n",s_mdb->plate_scale);
        fprintf(fp1,"cdltx         = %f\n",s_mdb->cdltx);
        fprintf(fp1,"cdlty         = %f\n",s_mdb->cdlty);
        fprintf(fp1,"crpxx         = %f\n",s_mdb->crpxx);
        fprintf(fp1,"crpxy         = %f\n",s_mdb->crpxy);
        fprintf(fp1,"xmin          = %f\n",s_mdb->xmin);
        fprintf(fp1,"xmax          = %f\n",s_mdb->xmax);
        fprintf(fp1,"ymin          = %f\n",s_mdb->ymin);
        fprintf(fp1,"ymax          = %f\n",s_mdb->ymax);
        fprintf(fp1,"psf_fwhm      = %f\n",s_mdb->psf_fwhm);
        fprintf(fp1,"cdltx_sky     = %f\n",s_mdb->cdltx_sky);
        fprintf(fp1,"cdlty_sky     = %f\n",s_mdb->cdlty_sky);
        fprintf(fp1,"crpxx_sky     = %f\n",s_mdb->crpxx_sky);
        fprintf(fp1,"crpxy_sky     = %f\n",s_mdb->crpxy_sky);
        fprintf(fp1,"xmin_sky      = %f\n",s_mdb->xmin_sky);
        fprintf(fp1,"xmax_sky      = %f\n",s_mdb->xmax_sky);
        fprintf(fp1,"ymin_sky      = %f\n",s_mdb->ymin_sky);
        fprintf(fp1,"ymax_sky      = %f\n",s_mdb->ymax_sky);
        fprintf(fp1,"resampx       = %f\n",s_mdb->resampx);
        fprintf(fp1,"resampy       = %f\n",s_mdb->resampy);
        fprintf(fp1,"cdltx_foc     = %f\n",s_mdb->cdltx_foc);
        fprintf(fp1,"cdlty_foc     = %f\n",s_mdb->cdlty_foc);
        fprintf(fp1,"crpxx_foc     = %f\n",s_mdb->crpxx_foc);
        fprintf(fp1,"crpxy_foc     = %f\n",s_mdb->crpxy_foc);
        fprintf(fp1,"xmin_foc      = %f\n",s_mdb->xmin_foc);
        fprintf(fp1,"xmax_foc      = %f\n",s_mdb->xmax_foc);
        fprintf(fp1,"ymin_foc      = %f\n",s_mdb->ymin_foc);
        fprintf(fp1,"ymax_foc      = %f\n",s_mdb->ymax_foc);
        fprintf(fp1,"FOC_ROTD_sin  = %f\n",s_mdb->FOC_ROTD_sin);
        fprintf(fp1,"FOC_ROTD_cos  = %f\n",s_mdb->FOC_ROTD_cos);
        fprintf(fp1,"crpxx_det     = %f\n",s_mdb->crpxx_det);
        fprintf(fp1,"crpxy_det     = %f\n",s_mdb->crpxy_det);
        fprintf(fp1,"cdltx_det     = %f\n",s_mdb->cdltx_det);
        fprintf(fp1,"cdlty_det     = %f\n",s_mdb->cdlty_det);
        fprintf(fp1,"xmin_det      = %f\n",s_mdb->xmin_det);
        fprintf(fp1,"xmax_det      = %f\n",s_mdb->xmax_det);
        fprintf(fp1,"ymin_det      = %f\n",s_mdb->ymin_det);
        fprintf(fp1,"ymax_det      = %f\n",s_mdb->ymax_det);
        fprintf(fp1,"instmap_name  = %s\n",s_mdb->instmap_name);
        fprintf(fp1,"instmap_flag  = %d\n",s_mdb->instmap_flag);
        fprintf(fp1,"nx_imap       = %ld\n",s_mdb->nx_imap);
        fprintf(fp1,"ny_imap       = %ld\n",s_mdb->ny_imap);

        fclose(fp1);
    }

    
    return errstatus;
}


/* FUNCTION NAME: check_calfiles_exist                                             */
/*                                                                                 */ 
/* CALLING SEQUENCE:                                                               */
/*    errstatus = check_calfiles_exist(s_calfiles);                                */
/*                                                                                 */
/* PURPOSE:                                                                        */
/*    If not "none" or "caldb" ensure that the given filenames actually exist.     */
/*    Does not attempt to read files, only check that they exist.                  */
/*                                                                                 */
/* INPUTS: s_cal                                                                   */
/*                                                                                 */
/* OUTPUTS: status                                                                 */
/*                                                                                 */
/* CALLED BY:                                                                      */
/*    main()                                                                       */
/*                                                                                 */

int check_calfiles_exist(CalFiles * s_cal){
    int status = 0;    /* tracks the status, 0 to continue, anything else to stop */
    int found = 0;     /* tracks the found status of the file: 2=compressed, 1=found, 0=does not exist, -1=not a disk file */
    int fstat = 0;     /* fits status */

    /* verify calibration files exist */
    if (eOK == status) { 
        fits_file_exists(s_cal->arffile, &found, &fstat);
        if ( (found == 2) || (found == 1) ){
            status = 0;
        } else {
            status = -1;
            printf("heasim.c: problem accessing ARF file %s\n",s_cal->arffile);
        }
    }
    
    if (eOK == status) { 
        /* +++ 2016-08-24 RSH ***new */
        if (0 == strcasecmp(s_cal->rmffile, "none")) {
            status = 0;  // rmffile == "none"
        } else {
            fits_file_exists(s_cal->rmffile, &found, &fstat);
            if ( (found == 2) || (found == 1) ){
                status = 0;
            } else {
                status = -1;
                printf("heasim.c: problem accessing RMF file %s\n",s_cal->rmffile);
            }
        }
    }
    
    if (eOK == status) {
        if (0 != strncasecmp("NONE",s_cal->intbackfile,4)) {
            fits_file_exists(s_cal->intbackfile, &found, &fstat);
            if ( (found == 2) || (found == 1) ){
                status = 0;
            } else {
                status = -1;
                printf("heasim.c: problem accessing PHA file %s\n",s_cal->intbackfile);
            }
        }
    }

    if (eOK == status) { 
        if ( (0 != strncasecmp("NONE",s_cal->psffile,4)) &&   /* psf file can be "none" */
             (0 != strncasecmp("GAUS",s_cal->psffile,4)) ){   /* psf file can be set to "gaussian" */
            fits_file_exists(s_cal->psffile, &found, &fstat);
            if ( (found == 2) || (found == 1) ){
                status = 0;
            } else {
                status = -1;
                printf("heasim.c: problem accessing PSF file %s\n",s_cal->psffile);
            }
        }
    }
    
    if (eOK == status) { 
        if (0 != strncasecmp("NONE",s_cal->vigfile,4)) {
            fits_file_exists(s_cal->vigfile, &found, &fstat);
            if ( (found == 2) || (found == 1) ){
                status = 0;
            } else {
                status = -1;
                printf("heasim.c: problem accessing Vignette file %s\n",s_cal->vigfile);
            }
        }
    }
    
    if (status == 0)
        printf("All calibration files located successfully.\n\n");
    
    return status;
}


/* *************** HEASP-RELATED FUNCTIONS ********************/

/* FUNCTION NAME: read_ARF                                          */
/*                                                                  */
/* CALLING SEQUENCE:                                                */
/*     arf_struct = read_ARF(&s_calfiles, &nebin, debug);           */
/*                                                                  */
/* PURPOSE: read the ARF file into a useable data structure         */
/*                                                                  */
/* INPUTS: s_calfiles, debug                                        */
/*                                                                  */
/* OUTPUTS: nebin, returns arf_struct                               */
/*                                                                  */
/* CALLED BY:                                                       */
/*    main()                                                        */
/*                                                                  */
/* EXTERNAL DEPENDENCIES:                                           */
/*    HEASP library: headas/heacore/heasp                           */
/*                                                                  */

ARF read_ARF(CalFiles * s_cal,  /* structure containing calibration filenames */
             int *nebin,        /* number of arf input energy bins */
             int debug){        /* a debugging flag */

    ARF arf_struct;      /* The ARF structure to return */
    int errstatus = 0;   /* error status.  0 to continue */
    long ARFnumber=0;    /* Required by HEASP read.  If there are multiple SPECRESP extensions will read the ARFnumber one. */
    int fstat = 0;       /* status of FITS read.  Required by HEASP read */
    fitsfile * ARFfits;  /* The fits file structure */
    int i=0;               /* loop index */
    int arfcheck1=0, arfcheck2=0, arfcheck3=0, arfcheck4=0, arfcheck5=0;
    int colnum_lo=0, colnum_hi=0, colnum_area=0;
    int ncols=0;

    fits_open_file(&ARFfits, s_cal->arffile, READONLY, &fstat);

    /* Perform some basic checks to ensure that this is actually an ARF file */
    fits_movnam_hdu(ARFfits, BINARY_TBL, "SPECRESP", ARFnumber, &arfcheck1);
    fits_get_num_cols(ARFfits, &ncols, &arfcheck2);
    fits_get_colnum(ARFfits, CASEINSEN, "ENERG_LO", &colnum_lo, &arfcheck3);
    fits_get_colnum(ARFfits, CASEINSEN, "ENERG_HI", &colnum_hi, &arfcheck4);
    fits_get_colnum(ARFfits, CASEINSEN, "SPECRESP", &colnum_area, &arfcheck5);

    if ( (arfcheck1 != 0) ||
         (ncols != 3) ||
         (arfcheck2 != 0) ||
         (arfcheck3 != 0) ||
         (arfcheck4 != 0) ||
         (arfcheck5 != 0) ){
        printf("\n##### ERROR: #####  File %s is not a valid ARF file!\n\n",s_cal->arffile);
    } else {
        printf("Confirmed that supplied ARF file is a valid ARF file.\n");
    }

#ifdef use_legacy_heasp
    printf("Legacy HEASP reading ARF file into arf_struct.......");
    errstatus = C_ReadARF(ARFfits, ARFnumber, &arf_struct);
#else
    printf("Active HEASP reading ARF file into arf_struct.......");
    errstatus = ReadARF(s_cal->arffile, ARFnumber, &arf_struct);
#endif

    printf("done.\n\n");

    fits_close_file(ARFfits, &fstat);
    *nebin = arf_struct.NumberEnergyBins;

    /* If Area entries are NULL, set them to zero. */
    for (i=0; i<*nebin; i++)
        if (arf_struct.EffArea[i] != arf_struct.EffArea[i]){
            if (debug == 1) printf("ARF element %d: Area is NULL, setting to 0.\n",i);
            arf_struct.EffArea[i] = 0.0;
        }

    /* If we're debugging, print out some values. */
    if (debug == 1){
        printf("\nARF File Debug:\n");
        printf("   ENERG_LO  ENERG_HI  SPECRESP\n");
        for (i=0; i<10; i++)
            printf("%d  %f  %f  %f\n",
                   i,arf_struct.LowEnergy[i],arf_struct.HighEnergy[i],arf_struct.EffArea[i]);
        printf("...\n");
        for (i=*nebin-10; i<*nebin; i++)
            printf("%d  %f  %f  %f\n",
                   i,arf_struct.LowEnergy[i],arf_struct.HighEnergy[i],arf_struct.EffArea[i]);
        printf("\n");
    }

    return arf_struct;
}




/* FUNCTION NAME: read_RMF                                      */
/*                                                              */
/* CALLING SEQUENCE:                                            */
/* rmf_struct = read_RMF(&s_calfiles, &nebin_rmf, &nchan,       */
/*         &ebounds_TLMIN1, &ebounds_TLMAX1, debug);            */
/*                                                              */
/* PURPOSE: read the RMF file into a useable data structure     */
/*                                                              */
/* INPUTS: s_calfiles                                           */
/*                                                              */
/* OUTPUTS: nebin_rmf, nchan, ebounds_TLMIN1, ebounds_TLMIN2,   */
/*           returns rmf_struct                                 */
/*                                                              */
/* CALLED BY:                                                   */
/*    main()                                                    */
/*                                                              */
/* EXTERNAL DEPENDENCIES:                                       */
/*    HEASP library: headas/heacore/heasp                       */
/*                                                              */


RMF read_RMF(CalFiles * s_cal,       /* structure containing calibration filenames */
             int *nebin_rmf,         /* number of rmf input energy bins */
             int *nchan,             /* number of output energy bins [PI channels] from rmf ebounds ext */
             long *ebounds_TLMIN1,   /* EBOUNDS minimum allowed PI channel value */
             long *ebounds_TLMAX1,   /* EBOUNDS maximum allowed PI channel value */
             int debug){

    RMF rmf_struct;     /* The RMF structure to return */
    int errstatus = 0;  /* error status.  0 to continue */
    long RMFnumber=0;   /* Required by HEASP read.  If there are multiple SPECRESP extensions will read the RMFnumber one. */
    int fstat = 0;      /* status of FITS read.  Required by HEASP read */
    fitsfile * RMFfits; /* The fits file structure */
    int i=0;              /* loop index */
    char * comment = NULL;
    int chancol=0;
    long nrows=0;
    int anynul=0;
    long * chanarr=0;

    fits_open_file(&RMFfits, s_cal->rmffile, READONLY, &fstat);

    if (fstat == 0){
        /* Perform some basic checks to ensure that this is actually an RMF file */
        
        int rmfcheck1=0, rmfcheck2=0, rmfcheck3=0, rmfcheck4=0, rmfcheck5=0;
        int rmfcheck6=0, rmfcheck7=0, rmfcheck8=0, rmfcheck9=0, rmfcheck10=0;
        int rmfcheck11=0, rmfcheck12=0, rmfcheck13=0;
        int colnum_lo=0, colnum_hi=0, colnum_n_grp=0, colnum_f_chan=0, colnum_n_chan=0, colnum_matrix=0;
        int colnum_chan=0, colnum_emin=0, colnum_emax=0;
        int n_matrix_cols=6, n_ebounds_cols=3;

        fits_movnam_hdu(RMFfits, BINARY_TBL, "MATRIX", RMFnumber, &rmfcheck1);
        fits_get_num_cols(RMFfits, &n_matrix_cols, &rmfcheck2);
        fits_get_colnum(RMFfits, CASEINSEN, "ENERG_LO", &colnum_lo, &rmfcheck3);
        fits_get_colnum(RMFfits, CASEINSEN, "ENERG_HI", &colnum_hi, &rmfcheck4);
        fits_get_colnum(RMFfits, CASEINSEN, "N_GRP", &colnum_n_grp, &rmfcheck5);
        fits_get_colnum(RMFfits, CASEINSEN, "F_CHAN", &colnum_f_chan, &rmfcheck6);
        fits_get_colnum(RMFfits, CASEINSEN, "N_CHAN", &colnum_n_chan, &rmfcheck7);
        fits_get_colnum(RMFfits, CASEINSEN, "MATRIX", &colnum_matrix, &rmfcheck8);
        
        fits_movnam_hdu(RMFfits, BINARY_TBL, "EBOUNDS", RMFnumber, &rmfcheck9);
        fits_get_num_cols(RMFfits, &n_ebounds_cols, &rmfcheck10);
        fits_get_colnum(RMFfits, CASEINSEN, "CHANNEL", &colnum_chan, &rmfcheck11);
        fits_get_colnum(RMFfits, CASEINSEN, "E_MIN", &colnum_emin, &rmfcheck12);
        fits_get_colnum(RMFfits, CASEINSEN, "E_MAX", &colnum_emax, &rmfcheck13);

        if ( (rmfcheck1 != 0) || (rmfcheck2 != 0) || (rmfcheck3 != 0) || (rmfcheck4 != 0) || 
             (rmfcheck5 != 0) || (rmfcheck6 != 0) || (rmfcheck7 != 0) || (rmfcheck8 != 0) ||
             (rmfcheck9 != 0) || (rmfcheck10 != 0) || (rmfcheck11 != 0) || (rmfcheck12 != 0) ||
             (rmfcheck13 != 0) || (n_matrix_cols != 6) || (n_ebounds_cols != 3) ){
            printf("\n##### ERROR: #####  File %s is not a valid RMF file!\n\n",s_cal->rmffile);
            fits_close_file(RMFfits, &fstat);
            return rmf_struct;
        } else {
            printf("Confirmed that supplied RMF file is a valid RMF file.\n");
        }
    }

#ifdef use_legacy_heasp
    printf("Legacy HEASP reading RMF file into rmf_struct.......");
    errstatus = C_ReadRMFMatrix(RMFfits, RMFnumber, &rmf_struct);
    errstatus = C_ReadRMFEbounds(RMFfits, RMFnumber, &rmf_struct);
#else
    printf("Active HEASP reading RMF file into rmf_struct.......");
    errstatus = ReadRMFMatrix(s_cal->rmffile, RMFnumber, &rmf_struct);
    errstatus = ReadRMFEbounds(s_cal->rmffile, RMFnumber, &rmf_struct);
#endif

    printf("done.\n\n");

    /* pick up the EBOUNDS TLMIN1 and TLMAX1, we'll pass them to setup_output() */
    fits_movnam_hdu(RMFfits,BINARY_TBL,"EBOUNDS",0,&fstat);
    fits_read_key_lng(RMFfits, "TLMIN1", ebounds_TLMIN1, comment, &fstat);
    fits_read_key_lng(RMFfits, "TLMAX1", ebounds_TLMAX1, comment, &fstat);

    /* If there was a problem picking up TLMIN1 and TLMAX1, get them from first and last rows in CHANNEL */
    if (fstat != 0){
        fstat = 0;
        fits_get_colnum(RMFfits, 0, "CHANNEL", &chancol, &fstat);
        fits_get_num_rows(RMFfits, &nrows, &fstat);
        chanarr = calloc(nrows, sizeof(long));
        fits_read_col(RMFfits, TLONG, chancol, 1, 1, nrows, NULL, chanarr, &anynul, &fstat);
        *ebounds_TLMIN1 = chanarr[0];
        *ebounds_TLMAX1 = chanarr[nrows-1];
        free(chanarr);
    }

    fits_close_file(RMFfits, &fstat);
    *nebin_rmf = rmf_struct.NumberEnergyBins;
    *nchan = rmf_struct.NumberChannels;

    /* Set NULL values to zero. */
    for (i=0; i<*nebin_rmf; i++){
        if (rmf_struct.LowEnergy[i] != rmf_struct.LowEnergy[i]){
            printf("RMF element %d: LowEnergy is NULL, setting to 0.\n",i);
            rmf_struct.LowEnergy[i] = 0.0;
        }
        if (rmf_struct.HighEnergy[i] != rmf_struct.HighEnergy[i]){
            printf("RMF element %d: HighEnergy is NULL, setting to 0.\n",i);
            rmf_struct.LowEnergy[i] = 0.0;
        }
    }

    if (debug == 1){
        printf("   RMF->FirstChannel = %ld\n",rmf_struct.FirstChannel);
        printf("   RMF->NumberChannels = %ld\n",rmf_struct.NumberChannels);
        printf("   ebounds_TLMIN1 = %ld\n",*ebounds_TLMIN1);
        printf("   ebounds_TLMAX1 = %ld\n\n",*ebounds_TLMAX1);
    }

    return rmf_struct;
}




/* FUNCTION NAME: check_arf_rmf                                                             */
/*                                                                                          */
/* CALLING SEQUENCE:                                                                        */
/*      errstatus = check_arf_rmf(&s_calfiles, &rmf_struct, &arf_struct                     */
/*                 nebin, nebin_rmf, nchan,                                                 */
/*                 &ebin_lo, &ebin_hi, &ebin_mid, &ebin_del, &area,                         */
/*                 &ebin_lo_rmf, &ebin_hi_rmf,                                              */
/*                 &emin, &emax, &ecen, &edelt);                                            */
/*                                                                                          */
/* PURPOSE: check compatibility of ARF and RMF                                              */
/*                                                                                          */
/* INPUTS: s_cal, rmf_struct, arf_struct, nebin, nebin_rmf, nchan                           */
/*                                                                                          */
/* OUTPUTS: ebin_lo, ebin_hi, ebin_mid, ebin_del, area, ebin_lo_rmf, ebin_hi_rmf,           */
/*              emin, emax, ecen, edelt, errstatus                                          */
/*                                                                                          */
/* CALLED BY:                                                                               */
/*   main()                                                                                 */
/*                                                                                          */

int check_arf_rmf(CalFiles * s_cal,       /* structure containing calibration filenames */   
                  RMF * rmf_struct,       /* RMF structure */
                  ARF * arf_struct,       /* ARF structure */
                  int nebin,              /* number of arf input energy bins */          
                  int nebin_rmf,          /* number of rmf input energy bins */
                  int nchan,              /* number of output energy bins [PI channels] from rmf ebounds ext */
                  double ** ebin_lo,       /* lower boundary of arf energy grid bins */
                  double ** ebin_hi,       /* upper boundary of arf energy grid bins */
                  double ** ebin_mid,      /* midpoint of arf energy grid bins */
                  double ** ebin_del,      /* grid spacing of arf energy grid bins */
                  double ** area,          /* effective area from arf */
                  double ** ebin_lo_rmf,   /* lower boundary of rmf energy grid bins */
                  double ** ebin_hi_rmf,   /* upper boundary of rmf energy grid bins */
                  double ** emin,          /* lower boundary of output energy grid bins */
                  double ** emax,          /* upper boundary of output energy grid bins */
                  double ** ecen,          /* midpoint of output energy grid bins */
                  double ** edelt){        /* grid spacing output energy grid bins */

    int errstatus=0;         /* error status.  0 to continue */
    int i=0;                   /* generic loop index */
    double bin_tolerance=0;     /* how close must ARF and RMF match? */
    double delta_energy_hi=0;   /* difference in ebin_hi */
    double delta_energy_lo=0;   /* difference in ebin_lo */
    double delE=0;              /* minimum of delta_energy_hi and _lo */

    /* allocate needed arrays (REMEMBER TO FREE AT END OF MAIN !! ) */
    *ebin_lo = calloc(nebin, sizeof(double));
    *ebin_hi = calloc(nebin, sizeof(double));
    *ebin_mid = calloc(nebin, sizeof(double));
    *ebin_del = calloc(nebin, sizeof(double));
    *area = calloc(nebin, sizeof(double));
    *ebin_lo_rmf = calloc(nebin_rmf, sizeof(double));
    *ebin_hi_rmf = calloc(nebin_rmf, sizeof(double));
    *emin = calloc(nchan, sizeof(double));
    *emax = calloc(nchan, sizeof(double));
    *ecen = calloc(nchan, sizeof(double));
    *edelt = calloc(nchan, sizeof(double));

    for (i=0; i<nebin; i++){
        (*ebin_lo)[i] = (double)arf_struct->LowEnergy[i];
        (*ebin_hi)[i] = (double)arf_struct->HighEnergy[i];
        /* printf("i=%d ebin_lo=%g ebin_hi=%g\n", i, (*ebin_lo)[i], (*ebin_hi)[i]); */
        (*ebin_mid)[i] = 0.5 * ( (*ebin_lo)[i] + (*ebin_hi)[i]);
        (*area)[i] = (double)arf_struct->EffArea[i];
    }
    printf("nebin=%d nebin_rmf=%d\n", nebin, nebin_rmf);

    /* +++ 2016-08-24 RSH ***new */
    if (0 == strcasecmp(s_cal->rmffile, "none")) {
        /* rmffile == "none" */
        printf("rmffile==none\n");
        for (i=0; i<nebin_rmf; i++) {
            (*ebin_lo_rmf)[i] = (*ebin_lo)[i];
            (*ebin_hi_rmf)[i] = (*ebin_hi)[i];
        }
    } else {
        /* rmffile != "none" */
        printf("rmffile!=none\n");
        for (i=0; i<nebin_rmf; i++){
            (*ebin_lo_rmf)[i] = (double)rmf_struct->LowEnergy[i];
            (*ebin_hi_rmf)[i] = (double)rmf_struct->HighEnergy[i];
        }
    }

    printf("***** ebin_lo[0]==%g\n", (*ebin_lo)[0]);

    printf("Checking ARF/RMF compatibility...\n");
    /* make sure arf and rmf have the same number of bins */
    if (nebin != nebin_rmf){
        printf("ERROR: ARF and RMF have different number of energy bins!");
        printf("ARF: %d   RMF: %d\n",nebin,nebin_rmf);
        return -1;
    }else{
        for (i=0; i<nebin; i++){
            /* only perform this test of ebin_lo is not zero for ARF or RMF */
            if ( ( (*ebin_lo)[i] != 0.) && ( (*ebin_lo_rmf)[i] != 0.)) {
                bin_tolerance = s_cal->arfrmftol * ( (*ebin_hi)[i] - (*ebin_lo)[i]);
                delta_energy_hi = fabs( (*ebin_hi)[i] - (*ebin_hi_rmf)[i]);
                delta_energy_lo = fabs( (*ebin_lo)[i] - (*ebin_lo_rmf)[i]);
                /* make sure arf and rmf energy grids match to within tolerance */
                /* delE = MIN(delta_energy_lo,delta_energy_hi); */
                delE = delta_energy_lo;
                if (delta_energy_lo < delta_energy_hi)
                    delE = delta_energy_hi;
                if (delE > bin_tolerance){
                    printf("ERROR: ARF and RMF energy grid rance exceeds tolerance!\n");
                    printf("   delta_energy: %e    tolerance: %e\n", delE, bin_tolerance);
                    printf("   ebin_lo[%d]: %.10e     ebin_lo_rmf[%d]: %.10e\n",
                            i,(*ebin_lo)[i],i,(*ebin_lo_rmf)[i]);
                    printf("   ebin_hi[%d]: %.10e     ebin_hi_rmf[%d]: %.10e\n",
                            i,(*ebin_hi)[i],i,(*ebin_hi_rmf)[i]);
                    return -1;
                }
            }
        }
    }
    printf("ARF/RMF compatibiilty check passed successfully.\n\n");

    /* +++ 2016-08-24 RSH ***new */
    if (0 == strcasecmp(s_cal->rmffile, "none")) {
        for (i=0; i<nchan; i++) {
            (*emin)[i] = (*ebin_lo_rmf)[i];
            (*emax)[i] = (*ebin_hi_rmf)[i];
            (*ecen)[i] = 0.5 * ( (*emin)[i]+(*emax)[i]);
            (*edelt)[i] = (*emax)[i] - (*emin)[i];
        }
    } else {
        for (i=0; i<nchan; i++){
            (*emin)[i] = (double)rmf_struct->ChannelLowEnergy[i];
            (*emax)[i] = (double)rmf_struct->ChannelHighEnergy[i];
            (*ecen)[i] = 0.5 * ( (*emin)[i]+(*emax)[i]);
            (*edelt)[i] = (*emax)[i] - (*emin)[i];
        }
    }
    
    return errstatus;
}




/* FUNCTION NAME: read_internal_background                                                          */
/*                                                                                                  */
/* CALLING SEQUENCE:                                                                                */
/*   errstatus = read_internal_background(&s_calfiles, nchan, &ibspec, &ib_backscal, &ib_expose);   */
/*                                                                                                  */
/* PURPOSE: read internal background                                                                */
/*                                                                                                  */
/* INPUTS: s_back, nchan                                                                            */
/*                                                                                                  */
/* OUTPUTS: ibspec, ib_backscal, ib_expose, errstatus                                               */
/*                                                                                                  */
/* SUBROUTINES: deallocate_ph_data()                                                                */
/*                                                                                                  */
/* CALLED BY:                                                                                       */
/*   main()                                                                                         */
/*                                                                                                  */
/* EXTERNAL DEPENDENCIES:                                                                           */
/*    HEASP library: headas/heacore/heasp                                                           */
/*                                                                                                  */

int read_internal_background(CalFiles * s_cal,     /* structure containing background filenames */
                             MDB_params * s_mdb,   /* mdb parameters */
                             int nchan,            /* number of output energy bins [PI channels] from rmf ebounds ext */
                             int ebounds_TLMIN,  /* lower limit on energy spectrum */
                             double ** ibspec,      /* internal background spectrum */
                             double ** ib_backscal, /* internal background scaling factor */
                             double * ib_expose,    /* internal background exposure time */
                             long * ib_firstchan){   /* first channel in background spectrum */

    PHA iback_pha_struct;   /* the background structure */
    int errstatus = 0;      /* error status. 0 to continue. */
    long PHAnumber=0;       /* required by HEASP read. */
    int fstat=0;            /* FITS read status */
    int ibflag = 0;         /* Internal background status.  0 to continue */
    int i=0;                  /* generic loop index */
    fitsfile * PHAfits;     /* The fits file structure */
    char mission[STR_MAX_LEN];  /* mission string */

    /* allocate here, FREE AT END OF MAIN!! */
    *ibspec = calloc(nchan, sizeof(double));
    *ib_backscal = calloc(nchan, sizeof(double));

    fits_open_file(&PHAfits, s_cal->intbackfile, READONLY, &fstat);

#ifdef use_legacy_heasp
    printf("Legacy HEASP reading PHA (internal background) file...");
    errstatus = C_ReadPHAtypeI(PHAfits, PHAnumber, &iback_pha_struct);
#else
    printf("Active HEASP reading PHA (internal background) file...");
    errstatus = ReadPHAtypeI(s_cal->intbackfile, PHAnumber, &iback_pha_struct);
#endif

    printf("done.\n");

    /* Read the TELESCOP keyword */
    fits_read_key_str(PHAfits,"TELESCOP",mission,NULL,&fstat);
    if (fstat != 0){
        fstat = 0;
        strcpy(mission,"UNKNOWN");
    }

    /* Read the TLMIN1 keyword */
    fits_read_key_lng(PHAfits,"TLMIN1", &(*ib_firstchan), NULL, &fstat);
    if (fstat != 0){
        /* If there was a problem picking up TLMIN1, get it from first row in CHANNEL */
        int chancol = 0;
        long nrows = 0;
        long * chanarr = 0;

        fstat = 0;
        fits_get_colnum(PHAfits, 0, "CHANNEL", &chancol, &fstat);
        fits_get_num_rows(PHAfits, &nrows, &fstat);
        chanarr = calloc(nrows, sizeof(long));
        fits_read_col(PHAfits, TLONG, chancol, 1, 1, nrows, NULL, chanarr, NULL, &fstat);
        (*ib_firstchan) = iback_pha_struct.FirstChannel = chanarr[0];
        free(chanarr);
    }

    fits_close_file(PHAfits, &fstat);

    if (iback_pha_struct.NumberChannels != (long)nchan){
        ibflag = 1;
        printf("ERROR: incorrect number of channels in background struct.\n");
        printf("  Try setting intbackfile = none\n");
        printf("  nchan: %d  iback_pha_struct.NumberChannels: %ld\n",nchan,iback_pha_struct.NumberChannels);
        return -1;

    } else if ( (*ib_firstchan) != ebounds_TLMIN ){
        ibflag = 1;
        printf("ERROR: ib_firstchan does not match ebounds_TLMIN.\n");
        printf("  Try setting intbackfile = none\n");
        printf("  ib_firstchan = %ld,  ebounds_TLMIN = %d\n",(*ib_firstchan),ebounds_TLMIN);
        return -1;

    } else {
        *ib_expose = iback_pha_struct.Exposure; 
        printf("Populating ibspec and ib_backscal...");
        for (i=0; i<nchan; i++){
            (*ib_backscal)[i] = iback_pha_struct.BackScaling[i];
            (*ibspec)[i] = iback_pha_struct.Pha[i];
        }
        printf("done.\n\n");
    }

    if (strcasecmp(mission,"xmm") == 0){

        /* If these conditions are met, it is likely that the XMM background rescaling needs modification
           to match heasim units. */

        if ((*ib_backscal)[0] > 100.0){
            double rescale_factor = pow( (s_mdb->FOV_RADIUS * 60.0),2 ) * EULER_PI / 
                ( (s_mdb->FOC_XPIXSIZ * 3600) * (s_mdb->FOC_YPIXSIZ * 3600) );
            printf("Rescaling XMM BACKSCAL by factor = %e\n",rescale_factor);
            
            for (i=0; i<nchan; i++){
                (*ib_backscal)[i] /= rescale_factor;
            }
        } else if ((*ib_backscal)[0] == 0.0){
            printf("Resetting XMM BACKSCAL from 0.0 to 1.0.\n");
            for (i=0; i<nchan; i++){
                (*ib_backscal)[i] = 1.0;
            }
        }
    }
    
    /* we pass back the arrays, done with the structure */
    deallocate_pha_data(&iback_pha_struct);

    return errstatus;
}


/******************* SOURCE DATA RELATED FUNCTIONS ***********************/


/* FUNCTION NAME: read_source_data                                                                                         */
/*                                                                                                                         */
/* CALLING SEQUENCE:                                                                                                       */
/*    errstatus = read_source_data(insrcdeffile, &nsource, &ras, &decs, &colden, &spectype, &specpar, &fluxpar,   */
/*                           &band_lo, &band_hi, &sfilename, &sformat, &sunits, &period, &pulse_fraction,                  */
/*                           &tburst, &trise, &tdecay, &burst_rat,                                                         */
/*                           &ifilename, &sd_matrix_size, &sd_param_matrix, debug);                                        */
/*                                                                                                                         */
/* PURPOSE: read and parse the source parameters from the source definition file, calculate some secondary                 */
/*             quantities needed for the simulation, and pass what is required back to the "initialize"                    */
/*            section of main().                                                                                           */
/*                                                                                                                         */
/* INPUTS: infile, debug                                                                                                   */
/*                                                                                                                         */
/* OUTPUTS: nsource, ras, decs, colden, spectype, specpar, fluxpar, band_lo, band_hi,                                      */
/*             sfilename, sformat, sunits, period, pulse_fraction, ifilename, sd_matrix_size,                              */
/*            sd_param_matrix                                                                                              */
/*                                                                                                                         */
/* CALLED BY:                                                                                                              */
/*   main()                                                                                                                */
/*                                                                                                                         */



int read_source_data(char * infile,                /* The source input filename */
                     int * nsource,                /* number of sources found in input source file */
                     double ** ras,                /* array of source RAs */
                     double ** decs,               /* array of source DECs */
                     double ** colden,             /* array of source column densities */
                     int ** spectype,              /* array of source spectral type */
                     double ** specpar,            /* value of spectral param for all sources */
                     double ** fluxpar,            /* flux for all sources, can be 0 for spectral file */
                     double ** band_lo,            /* flux lower bandpass for all sources */
                     double ** band_hi,            /* flux upper bandpass for all sources */
                     char *** sfilename,           /* name of spectral user input file for all sources, "none" for spectral model */
                     int ** sformat,               /* spectral sformat file format flag 1 or 2 for all sources; can be 0 for spectral model */
                     int ** sunits,                /* spectral file flux unit tag 1-9 for all sources, can be 0 for spectral model */
                     double ** period,             /* period for all sources, 0 for constant */
                     double ** pulse_fraction,     /* pulse fraction for all sources, 0 for constant */
                     double ** tburst,             /* burst start time for all original sources, 0 for constant */
                     double ** trise,              /* burst risetime for all original sources, 0 for constant */
                     double ** tdecay,             /* burst decay time for all original sources, 0 for constant */
                     double ** burst_rat,          /* burst ratio for all original sources, 0 for constant */
                     int ** burst_tabidx,          /* array of index identifying lookup table for burst */
                     int * nburst,                 /* number of burst sources */
                     char *** ifilename,           /* name of image user input file for all sources; "none" for point source or spatial model */
                     int * sd_matrix_size,         /* maximum number of spatial distribution quantities needed */
                     double *** sd_param_matrix,   /* matrix of spatial distribution quantities needed to apply source distribution */
                     int debug){                   /* flag to debug */

    int src_num = 0;                    /* the current source number of nsource contained within infile */
    FILE *fp=NULL;                      /* The file to open */
    char line[STR_MAX_LEN]="\0";        /* string line read from infile */
    char temp_line[STR_MAX_LEN]="\0";   /* temporary string buffer */
    int n_commentlines=0;               /* number of comment lines or spaces in infile */
    int len=0;                          /* string length */
    int i=0, j=0, k=0;                  /* generic loop index */

    char * spec_model=NULL;             /* spectral model type */
    char * band=NULL;                   /* bandpass (as a string lo-hi) */
    char * field = NULL;                /* string buffer holding last field of source line, may or may not be present */
    char * source_specs_type=NULL;      /* holds description of spec parameters, e.g. extmod, image, or pulse */
    char * spec_string=NULL;            /* a temporary string buffer */

    /* The meaning of c[0-7] and p[1-7] differ depending on the value of source_type, see below. */
    /* c[1-7] is the string value, p[1-7] is the string cast as a double. */
    char * c[8];                   /* spec_type parameters, as strings */
    double p[8] = {0,0,0,0,0,0,0,0};  /* spec_type parameters, as doubles.  TRF refers to as source_specs_par  */

    double rat_min=0.;  /* ratio of rmin/r_core (in TRF - not sure what this means) */
    double rat_max=0.;  /* ratio of rmax/r_core (in TRF - not sure what this means) */
    double bslope=0.;   /* beta model slope (in TRF - not sure what this means) */

    int status=0;        /* error status, continue if 0. */
    int fstat = 0;       /* fits status */
    int comcount=0;      /* count the number of commas in a string */

    printf("Reading input file...\n");

    *sd_matrix_size = 8;

    /* Check that the source input file exists - fits_file_exists() will return 1 or 2 if successful, even if not fits file. */
    fits_file_exists(infile, &status, &fstat);
    if ( (status != 1) && (status != 2) ){
        printf("Error: cannot find source file %s, exiting.\n",infile);
        return -1;
    }else {
        status = 0;
        
        /* Get the number of valid source lines in the input file. */
        *nsource = get_nsource(infile);
        
        *nburst = 0;

        /* allocate here, call deallocate at end of main! */
        *tburst = calloc(*nsource, sizeof(double));
        *trise= calloc(*nsource, sizeof(double));
        *tdecay = calloc(*nsource, sizeof(double));
        *burst_rat = calloc(*nsource, sizeof(double));
        *burst_tabidx = calloc(*nsource, sizeof(int));

        *ras = calloc(*nsource, sizeof(double));
        *decs = calloc(*nsource, sizeof(double));
        *colden = calloc(*nsource, sizeof(double));
        *spectype = calloc(*nsource, sizeof(int));
        *specpar = calloc(*nsource, sizeof(double));
        *fluxpar = calloc(*nsource, sizeof(double));
        *band_lo = calloc(*nsource, sizeof(double));
        *band_hi = calloc(*nsource, sizeof(double));
        *sformat = calloc(*nsource, sizeof(int));
        *sunits = calloc(*nsource, sizeof(int));
        *period = calloc(*nsource, sizeof(double));
        *pulse_fraction = calloc(*nsource, sizeof(double));
        
        *sd_param_matrix = (double **)calloc(*nsource, sizeof(double *));
        (*ifilename) = calloc(*nsource,  sizeof(char*));
        *sfilename = calloc(*nsource, sizeof(char *));
        for (i=0; i<*nsource; i++){
            (*sd_param_matrix)[i] = (double *)calloc(*sd_matrix_size, sizeof(double));
            (*ifilename)[i] = calloc(STR_MAX_LEN, sizeof(char));
            (*sfilename)[i] = calloc(STR_MAX_LEN, sizeof(char));
        }

        /* initialize matrix with zeroes */
        for (i=0; i<*nsource; i++)
            for (j=0; j<*sd_matrix_size; j++){
                (*sd_param_matrix)[i][j] = 0.0;
            }
        
        /* open the file and begin reading each line */
        fp = fopen(infile,"r");
        while(fgets(line,sizeof(line),fp) != NULL){
            
            /* strip trailing '\n' if it exists */
            len = strlen(line)-1;
            if(line[len] == '\n')
                line[len] = 0;
            
            /* remove spaces in line */
            k=0;
            for (j=0; j<=len; j++)
                if (line[j] != ' ')
                    temp_line[k++] = line[j];
            temp_line[k] = 0;
            strcpy(line,temp_line);
            
            /* ignore lines beginning with spaces or # */
            if ( (line[0] == '#') || (strlen(line) == 0) || (line[0] == ' ') ){
                n_commentlines++;
            }else{
                
                /* The strtok command breaks off parts of a longer string
                   into, delimited by a given character.  The original string
                   is modified, so subsequent operations on the same string
                   may be called as NULL. */
                
                /* populate the data fields  - ORDER OF STRTOK CALLS MATTERS!!! */
                (*ras)[src_num] = atof(strtok(line,","));
                (*decs)[src_num] = atof(strtok(NULL,","));
                (*colden)[src_num] = atof(strtok(NULL,","));
                
                spec_model = strtok(NULL,",");
                
                (*specpar)[src_num] = atof(strtok(NULL,","));
                (*fluxpar)[src_num] = atof(strtok(NULL,","));
                band = strtok(NULL,",");
                if (debug == 1) printf("band = %s\n",band);
                strcpy((*sfilename)[src_num],strtok(NULL,","));
                if (debug == 1) printf("sfilename = %s\n",(*sfilename)[src_num]);
                (*sformat)[src_num] = atoi(strtok(NULL,","));
                if (0 != strcasecmp((*sfilename)[src_num],"none")) printf("sformat = %d\n",(*sformat)[src_num]);
                (*sunits)[src_num] = atoi(strtok(NULL,","));
                field = strtok(NULL,"|");  /* this may or may not exist on the given line */
                
                (*band_lo)[src_num] = atof(strtok(band,"-"));
                (*band_hi)[src_num] = atof(strtok(NULL,"-"));
                
                /* match spec_model string to spectype int */
                if ( (0 == strcasecmp(spec_model,"pl")) ||
                     (0 == strcasecmp(spec_model,"plaw")) ||
                     (0 == strcasecmp(spec_model,"pow")) ||
                     (0 == strcasecmp(spec_model,"power")) ){
                    (*spectype)[src_num] = 1;
                }else if ( (0 == strcasecmp(spec_model,"raymond-smith")) ||
                           (0 == strcasecmp(spec_model,"raymond")) ||
                           (0 == strcasecmp(spec_model,"ray")) ||
                           (0 == strcasecmp(spec_model,"rs")) ){
                    (*spectype)[src_num] = 2;
                }else if ( (0 == strcasecmp(spec_model,"blackbody")) ||  
                           (0 == strcasecmp(spec_model,"black")) ||
                           (0 == strcasecmp(spec_model,"blac")) ||
                           (0 == strcasecmp(spec_model,"bl")) ||
                           (0 == strcasecmp(spec_model,"bbody")) ||
                           (0 == strcasecmp(spec_model,"bb")) ){
                    (*spectype)[src_num] = 3;
                }else if ( (0 == strcasecmp(spec_model,"bremsstrahlung")) ||
                           (0 == strcasecmp(spec_model,"brems")) ||
                           (0 == strcasecmp(spec_model,"brem")) ||
                           (0 == strcasecmp(spec_model,"br")) ){
                    (*spectype)[src_num] = 4;
                }else if ( (0 == strcasecmp(spec_model,"mono-energetic")) ||
                           (0 == strcasecmp(spec_model,"mono")) ){
                    (*spectype)[src_num] = 5;
                }else if  (0 == strncasecmp(spec_model,"user",4)){
                    (*spectype)[src_num] = 6;
                } else if ( (0 == strcasecmp(spec_model,"torus")) ||
                            (0 == strcasecmp(spec_model,"tor")) ){
                    (*spectype)[src_num]  = 7;
                } else if (0 == strcasecmp(spec_model,"gilli_thick")) {
                    (*spectype)[src_num]  = 8;
                } else if (0 == strcasecmp(spec_model,"gilli_mild")) {
                    (*spectype)[src_num]  = 9;
                } 
    
                
                
                /* Do some validation checks: */
                if ( (0 > (*ras)[src_num]) || ((*ras)[src_num] > 360) ){
                    printf("Error: Source %d RA out of bounds (0 <= RA <= 360): %f\n",1+src_num,(*ras)[src_num]);
                    status = -1;
                }
                if ( (-90 > (*decs)[src_num]) || ((*decs)[src_num] > 90) ){
                    printf("Error: Source %d DEC out of bounds (-90 <= DEC <= 90): %f\n",1+src_num,(*decs)[src_num]);
                    status = -1;
                }
                if ( (*colden)[src_num] < 0 ){
                    printf("Error: Source %d column density out of bounds (NH >= 0): %f\n",1+src_num,(*colden)[src_num]);
                    status = -1;
                }
                if ( ((*spectype)[src_num] <= 0) || ((*spectype)[src_num] > 9) ){
                    printf("Error: Did not recognize source %d spectral model: %s\n",1+src_num,spec_model);
                    status = -1;
                }
                if ((*spectype)[src_num] != 6){ /* if not user-specified */
                    if ( (*specpar)[src_num] < 0 ){
                        printf("Error: Source %d spectral parameter (specpar) must be >= 0: %f\n",1+src_num,(*specpar)[src_num]);
                        status = -1;
                    }
                    if ( (*fluxpar)[src_num] <= 0 ){
                        printf("Error: Source %d flux parameter (fluxpar) must be > 0: %f\n",1+src_num,(*fluxpar)[src_num]);
                        status = -1;
                    }
                    if ( ((*band_lo)[src_num] < 0) || ( (*band_hi)[src_num] <= 0) || 
                         ( (*band_lo)[src_num] >= (*band_hi)[src_num]) ){
                        printf("Error: Source %d bandpass must be positve and have band_lo < band_hi: %f-%f\n",
                               1+src_num, (*band_lo)[src_num],(*band_hi)[src_num]);
                        status = -1;
                    }
                }
                if ( ((*sformat)[src_num] != 1) && ((*sformat)[src_num] != 2) ){
                    printf("Error: Source %d spectral format flag must be either 1 or 2: %d\n",1+src_num,(*sformat)[src_num]);
                    status = -1;
                }
                if ( ((*sunits)[src_num] < 1) || ((*sunits)[src_num] > 9) ){
                    printf("Error: Source %d spectral flux unit flag must be in range [1,9]: %d\n",1+src_num,(*sunits)[src_num]);
                    status = -1;
                }


                if (status != 0) return status;


                if (debug == 1){
                    /* print out the source info */
                    printf("Source Number =      %d\n",1+src_num);
                    printf("Source RA =          %f\n",(*ras)[src_num]);
                    printf("Source DEC =         %f\n",(*decs)[src_num]);
                    printf("Source NH =          %e\n",(*colden)[src_num]);
                    printf("Source spec_type =   %d\n",(*spectype)[src_num]);
                    printf("Source spec_par =    %e\n",(*specpar)[src_num]);
                    printf("Source flux_par =    %e\n",(*fluxpar)[src_num]);
                    printf("Source band_lower =  %f\n",(*band_lo)[src_num]);
                    printf("Source band_upper =  %f\n",(*band_hi)[src_num]);
                    printf("Source spec_file =   %s\n",(*sfilename)[src_num]);
                    printf("Source spec_format = %d\n",(*sformat)[src_num]);
                    printf("Source units =       %d\n",(*sunits)[src_num]);
                    if (field != NULL)
                        printf("Source extra =       %s\n",field); 
                    printf("\n");
                }
                
                /* printf("Source spec field =    %s\n",field); */
                source_specs_type = strtok(field,"(");  /* holds description of field parameters, e.g. extmod or pulse */
                spec_string = strtok(NULL,")");   /* holds the string inside parentheses */
                /* printf("%s field processed = %s\n\n",source_specs_type,spec_string); */
                
                if (spec_string != NULL){
                    /* Count the number of "," in the string to determine how many data fields exist.*/
                    for (i=0, comcount=0; spec_string[i]; i++)  comcount +=(spec_string[i] == ',');
                }

                /* match spec type params timing */
                
                /************** IF NO LAST FIELD IS GIVEN **************************/
                if (source_specs_type == NULL){ 
                    (*sd_param_matrix)[src_num][0] = 1.;   /* type 1 means point source */
                    strcpy((*ifilename)[src_num],"none");
                    /* include constant timing info */
                    (*period)[src_num] = 0.;
                    (*pulse_fraction)[src_num] = 0.;
                    
                    /***************************** IF SOURCE_SPECS_TYPE IS PULSE *******************************/    
                } else if (0 == strcasecmp(source_specs_type,"pulse")){ 
                    if (debug == 1) printf("Processing pulse info.\n");

                    p[1] = atof(strtok(spec_string,","));
                    p[2] = atof(strtok(NULL,","));
                    /* In this case, period and pulse fraction are not constant! */
                    (*period)[src_num] = p[1];
                    (*pulse_fraction)[src_num] = p[2];
                    strcpy((*ifilename)[src_num],"none");
                    (*sd_param_matrix)[src_num][0] = 1.;    /* this is still a point source */
                    
                    /* Pulse validation checks: */
                    if ( (*period)[src_num] < 0){
                        printf("Error: Source %d pulse period must be >= 0: %f\n",1+src_num,(*period)[src_num]);
                        status = -1;
                    }
                    if ( ((*pulse_fraction)[src_num] < 0) || ((*pulse_fraction)[src_num] > 1)){
                        printf("Error: Source pulse fraction must be between zero and one.  Current value: %f\n",
                               (*pulse_fraction)[src_num]);
                        status = -1;
                    }
                    if (comcount != 1){
                        printf("Error: Source %d pulse should have exactly 2 arguments: %d\n",1+src_num,comcount+1);
                        status = -1;
                    }

                    /***************************** IF SOURCE_SPECS_TYPE IS BURST *******************************/                   

                } else if (0 == strcasecmp(source_specs_type,"burst")){ /* if source_specs_type is BURST */
                    if (debug == 1) printf("Processing burst info.\n");

                    (*nburst)++;
                    (*burst_tabidx)[src_num] = (*nburst);

                    /* the burst field in the source file line looks like:  burst(par1,par2,par3,par4) */
                    (*tburst)[src_num] = atof(strtok(spec_string,","));
                    (*trise)[src_num] = atof(strtok(NULL,","));
                    (*tdecay)[src_num] = atof(strtok(NULL,","));
                    (*burst_rat)[src_num] = atof(strtok(NULL,","));
                    strcpy((*ifilename)[src_num],"none");
                    (*sd_param_matrix)[src_num][0] = 1.;    /* this is still a point source */

                    if (debug == 1) printf("  Burst params: tburst = %f, trise = %f, tdecay = %f, burst_rat = %f\n",
                                           (*tburst)[src_num], (*trise)[src_num], (*tdecay)[src_num], (*burst_rat)[src_num]);

                    /* Burst validation checks: */
                    if ( (*trise)[src_num] < 0){
                        printf("Error: Source %d trise must be >= 0: %f\n",1+src_num,(*trise)[src_num]);
                        status = -1;
                    }
                    if ( (*tdecay)[src_num] < 0){
                        printf("Error: Source %d tdecay must be >= 0: %f\n",1+src_num,(*tdecay)[src_num]);
                        status = -1;
                    }
                    if ( (*burst_rat)[src_num] < 0){
                        printf("Error: Source %d burst_rat must be >= 0: %f\n",1+src_num,(*burst_rat)[src_num]);
                        status = -1;
                    }
                    if (comcount != 3){
                        printf("Error: Source %d burst should have exactly 4 arguments: %d\n",1+src_num,comcount+1);
                        status = -1;
                    }
                    if ( (*tburst)[src_num] < 0){
                        printf("Error: Source %d tburst should be >= 0: %f\n",1+src_num,(*tburst)[src_num]);
                        status = -1;
                    }


                    /***************************** IF SOURCE_SPECS_TYPE IS EXTMOD *******************************/
                } else if (0 == strcasecmp(source_specs_type,"extmod")){ /* if source_specs_type is EXTMOD */
                    if (debug == 1) printf("Processing external model info.\n");

                    
                    /* parse the parameter string first into character arrays (c) and then doubles (p) */
                    /* We're going to skip c0 and p0, for consistency with TRF. */
                    /* c1,c2,c3,c4,c5,c6,c7 */
                    /* p1 p2 p3 p4 p5 p6 p7 */
                    /* c1 should be a string name.  The others can be converted to doubles. */
                    
                    c[1] = strtok(spec_string,",");
                    for (i=2; i<=7; i++){  /* start at p[2] */
                        c[i] = strtok(NULL,",");
                        if (c[i] != NULL)
                            p[i] = atof(c[i]);
                    }
                    
                    if (0 == strncasecmp(c[1],"gauss",5)){ /* c[1] is GAUSS */
                        strcpy((*ifilename)[src_num],"none");
                        (*sd_param_matrix)[src_num][0] = 2.;              /* type 2 means GAUSS */
                        (*sd_param_matrix)[src_num][1] = p[2] / 2.35482;  /* stdev along major axis */
                        (*sd_param_matrix)[src_num][2] = p[3] / 2.35482;  /* stdev along minor axis */
                        (*sd_param_matrix)[src_num][3] = cos(D2R * p[4]); /* cosine of position angle */
                        (*sd_param_matrix)[src_num][4] = sin(D2R * p[4]); /* sine of position angle */
                        /* include constant timing info */
                        (*period)[src_num] = 0.;
                        (*pulse_fraction)[src_num] = 0.;
                        
                        /* Gauss validation checks: */
                        if ( (*sd_param_matrix)[src_num][1] < 0){
                            printf("Source %d stdev_maj must be >= 0: %f\n",1+src_num,(*sd_param_matrix)[src_num][1]);
                            status = -1;
                        }
                        if ( (*sd_param_matrix)[src_num][2] < 0){
                            printf("Source %d stdev_min must be >= 0: %f\n",1+src_num,(*sd_param_matrix)[src_num][2]);
                            status = -1;
                        }
                        if ( (0 > p[4]) || (p[4] > 360) ){
                            printf("Source %d position angle is out of bounds (0 <= p4 <= 360): %f\n",1+src_num,p[4]);
                            status = -1;
                        }
                        if (comcount != 3){
                            printf("Error: Source %d gauss should have exactly 4 arguments: %d\n",1+src_num,comcount+1);
                            status = -1;
                        }

                        
                    }else if (0 == strncasecmp(c[1],"pow",3)){ /* c[1] is POWER LAW */
                        strcpy((*ifilename)[src_num],"none");
                        (*sd_param_matrix)[src_num][0] = 3.;            /* type 3 means POWER LAW */
                        (*sd_param_matrix)[src_num][1] = p[2];         /* slope */
                        (*sd_param_matrix)[src_num][2] = p[3];         /* rmin */
                        (*sd_param_matrix)[src_num][3] = p[4];         /* rmax */
                        (*sd_param_matrix)[src_num][4] = p[3] / p[4];  /* rmin/rmax */
                        /* include constant timing info */
                        (*period)[src_num] = 0.;
                        (*pulse_fraction)[src_num] = 0.;
                        
                        /* Powerlaw validation checks: */
                        if ( (*sd_param_matrix)[src_num][2] < 0 ){
                            printf("Error: Source %d powerlaw rmin must be >= 0: %f\n",1+src_num,(*sd_param_matrix)[src_num][2]);
                            status = -1;
                        }
                        if ( (*sd_param_matrix)[src_num][3] <= 0 ){
                            printf("Error: Source %d powerlaw rmax must be > 0: %f\n",1+src_num,(*sd_param_matrix)[src_num][3]);
                            status = -1;
                        }
                        if ( (*sd_param_matrix)[src_num][2] >=  (*sd_param_matrix)[src_num][3]  ){
                            printf("Error: Source %d powerlaw must have rmin < rmax: %f, %f\n",
                                   1+src_num,(*sd_param_matrix)[src_num][2], (*sd_param_matrix)[src_num][3]);
                            status = -1;
                        }
                        if (comcount != 3){
                            printf("Error: Source %d powerlaw should have exactly 4 arguments: %d\n",1+src_num,comcount+1);
                            status = -1;
                        }


                    }else if (0 == strncasecmp(c[1],"ellip",5)){ /* c[1] is ELLIPSE */
                        strcpy((*ifilename)[src_num],"none");
                        (*sd_param_matrix)[src_num][0] = 4.;               /* type 4 means ELLIPSE */
                        (*sd_param_matrix)[src_num][1] = p[2];             /* ellipticity */
                        (*sd_param_matrix)[src_num][2] = cos(D2R * p[3]);  /* cosine of position angle */
                        (*sd_param_matrix)[src_num][3] = sin(D2R * p[3]);  /* sine of position angle */
                        (*sd_param_matrix)[src_num][4] = p[4] * p[4];      /* rmin^2 */
                        (*sd_param_matrix)[src_num][5] = (p[5]*p[5]) - (p[4]*p[4]);   /* rmax^2 - rmin^2 */
                        /* include constant timing info */
                        (*period)[src_num] = 0.;
                        (*pulse_fraction)[src_num] = 0.;
                        
                        /* Elliptical validation checks: */
                        if ( (*sd_param_matrix)[src_num][1] <0 ){
                            printf("Error: Source %d ellipticity must be >= 0: %f\n",1+src_num,(*sd_param_matrix)[src_num][1]);
                            status = -1;
                        }
                        if (p[4] < 0){
                            printf("Error: Source %d rmin must be >= 0: %f\n",1+src_num,p[4]);
                            status = -1;
                        }
                        if (p[5] <= 0){
                            printf("Error: Source %d rmax must be > 0: %f\n",1+src_num,p[5]);
                            status = -1;
                        }
                        if (p[4] > p[5]){
                            printf("Error: Source %d rmax must be > rmin: %f, %f\n",1+src_num,p[4],p[5]);
                            status = -1;
                        }
                        if ( (0 > p[3]) || (p[3] > 360) ){
                            printf("Source %d position angle is out of bounds (0 <= p3 <= 360): %f\n",1+src_num,p[3]);
                            status = -1;
                        }
                        if (comcount != 4){
                            printf("Error: Source %d ellipse should have exactly 5 arguments: %d\n",1+src_num,comcount+1);
                            status = -1;
                        }


                    }else if (0 == strncasecmp(c[1],"beta",4)){ /* c[1] is BETA MODEL */
                        strcpy((*ifilename)[src_num],"none");
                        bslope = 1.5 - (3.0 * p[2]);
                        rat_min = p[6] / p[3];
                        rat_max = p[7] / p[3];
                        (*sd_param_matrix)[src_num][0] = 5.;                /* type 5 means BETA MODEL */

                        if (bslope != 0){
                            (*sd_param_matrix)[src_num][1] = 1.0/bslope;    /* 1.0 / (1.5-(3.0 * beta)) */
                        } else {
                            /* special case of beta = 0.5 */
                            (*sd_param_matrix)[src_num][1] = 0.0;
                        }
                        (*sd_param_matrix)[src_num][2] = p[3];              /* r_core */
                        (*sd_param_matrix)[src_num][3] = p[4];              /* ellipticity */
                        (*sd_param_matrix)[src_num][4] = cos(D2R * p[5]);   /* cosine of position angle */
                        (*sd_param_matrix)[src_num][5] = sin(D2R * p[5]);   /* sine of position angle */
                        
                        if (bslope != 0){
                            (*sd_param_matrix)[src_num][6] = pow( (1.0 + rat_min*rat_min), bslope);
                            (*sd_param_matrix)[src_num][7] = pow( (1.0 + rat_max*rat_max), bslope);
                        } else {
                            /* special case of beta = 0.5 */
                            /* note: log() is base e, log10() is base 10 */
                            (*sd_param_matrix)[src_num][6] = log(1.0 + rat_min*rat_min);
                            (*sd_param_matrix)[src_num][7] = log(1.0 + rat_max*rat_max);
                        }

                        /* include constant timing info */
                        (*period)[src_num] = 0.;
                        (*pulse_fraction)[src_num] = 0.;
                        
                        /* Beta model validation checks: */
                        if (p[2] < 0){
                            printf("Error: Source %d beta must be >= 0: %f\n",1+src_num,p[2]);
                            status = -1;
                        }
                        if (p[3] <= 0){
                            printf("Error Source %d r_core must be > 0: %f\n",1+src_num,p[3]);
                            status = -1;
                        }
                        if (p[4] <0){
                            printf("Error: Source %d ellipticity must be >= 0: %f\n",1+src_num,p[4]);
                            status = -1;
                        }
                        if ( (0 > p[5]) || (p[5] > 360) ){
                            printf("Source %d position angle is out of bounds (0 <= p5 <= 360): %f\n",1+src_num,p[5]);
                            status = -1;
                        }
                        if (rat_min < 0){
                            printf("Error: Source %d rat_min must be >= 0: %f\n",1+src_num,rat_min);
                            status = -1;
                        }
                        if (rat_max <= 0){
                            printf("Error: Source %d rat_max must be > 0: %f\n",1+src_num,rat_max);
                            status = -1;
                        }
                        if (rat_min > rat_max){
                            printf("Error: Source %d rat_max must be > rat_min: %f, %f\n",1+src_num,rat_min,rat_max);
                            status = -1;
                        }
                        if (comcount != 6){
                            printf("Error: Source %d beta should have exactly 7 arguments: %d\n",1+src_num,comcount+1);
                            status = -1;
                        }

                        

                    }else if (0 == strncasecmp(c[1],"flat",4)){ /* c[1] is FLAT */
                        strcpy((*ifilename)[src_num],"none");
                        (*sd_param_matrix)[src_num][0] = 6.;                    /* type 6 means FLAT */
                        (*sd_param_matrix)[src_num][1] = p[2] * p[2];           /* rmin^2 */
                        (*sd_param_matrix)[src_num][2] = p[3]*p[3] - p[2]*p[2]; /* rmax^2-rmin^2 */
                        /* include constant timing info */
                        (*period)[src_num] = 0.;
                        (*pulse_fraction)[src_num] = 0.;
                        
                        /* Flat validation checks: */
                        if (p[2] < 0){
                            printf("Error: Source %d rmin must be >= 0: %f\n",1+src_num,p[2]);
                            status = -1;
                        }
                        if (p[3] <= 0){
                            printf("Error: Source %d rmax must be > 0: %f\n",1+src_num,p[3]);
                            status = -1;
                        }
                        if (p[2] > p[3]){
                            printf("Error: Source %d rmax must be > rmin: %f, %f\n",1+src_num,p[2],p[3]);
                            status = -1;
                        }
                        if (comcount != 2){
                            printf("Error: Source %d should have exactly 3 arguments: %d\n",1+src_num,comcount+1);
                            status = -1;
                        }

                    }

                    /* General extmod validation checks: */
                    if ( ((*sd_param_matrix)[src_num][0] < 1) || ((*sd_param_matrix)[src_num][0] > 6)){
                        printf("Error: Source %d unrecognized extmod argument: %s\n",1+src_num,c[1]);
                        status = -1;
                    }

                } else if (0 == strcasecmp(source_specs_type,"image")){ /* source_specs_type is IMAGE */
                    c[1] = strtok(spec_string,",");
                    for (i=2; i<=5; i++){  /* start at p[2] */
                        c[i] = strtok(NULL,",");
                        if (c[i] != NULL)
                            p[i] = atof(c[i]);
                    }
                    
                    strcpy((*ifilename)[src_num], c[1]);
                    (*sd_param_matrix)[src_num][0] = 7.;      /* type 7 means IMAGE */
                    (*sd_param_matrix)[src_num][1] = p[2];
                    (*sd_param_matrix)[src_num][2] = p[3];
                    (*sd_param_matrix)[src_num][3] = p[4];
                    (*sd_param_matrix)[src_num][4] = p[5];
                    /* include constant timing info */
                    (*period)[src_num] = 0.;
                    (*pulse_fraction)[src_num] = 0.;
                    
                    if (debug == 1){
                        printf("Reading image info...\n");
                        printf("Image file: %s\n",c[1]);
                        printf("Image pixel xmin: %d\n",(int)p[2]);
                        printf("Image pixel xmax: %d\n",(int)p[3]);
                        printf("Image pixel ymin: %d\n",(int)p[4]);
                        printf("Image pixel ymax: %d\n",(int)p[5]);
                        printf("\n");
                    }

                } else{ /* if last field given is something other than extmod, pulse, or image */
                    printf("Error: Could not recognize source %d type (extmod, pulse, or image): %s \n",1+src_num,source_specs_type);
                    status = -1;
                }
                
                /* Make sure that the first argument of extmod, pulse, or image is also valid */
                if ( ((*sd_param_matrix)[src_num][0] < 1) || ((*sd_param_matrix)[src_num][0] > 7)){
                    printf("Error: Could not recognize source %d extmod/pulse/image argument: %s\n",
                           1+src_num,c[1]);
                    status = -1;
                }

                if (0 != status) return status;

                src_num++;
            }
        }
        
        if (debug == 1){
            printf("SD MATRIX:\n");
            for (i=0; i<*nsource; i++)
                printf("%f %f %f %f %f %f %f %f\n",(*sd_param_matrix)[i][0],
                       (*sd_param_matrix)[i][1],
                       (*sd_param_matrix)[i][2],
                       (*sd_param_matrix)[i][3],
                       (*sd_param_matrix)[i][4],
                       (*sd_param_matrix)[i][5],
                       (*sd_param_matrix)[i][6],
                       (*sd_param_matrix)[i][7]);
            
            printf("\n");
        } else {
            printf("...done.\n\n");
        }
    }
    fclose(fp);
    return status;
}



/* FUNCTION NAME: get_nsource                                                      */
/*                                                                                 */
/* CALLING SEQUENCE:                                                               */
/*   nsource = get_nsource(infile);                                                */
/*                                                                                 */
/* PURPOSE: Return the number of valid sources within the source input file        */
/*                                                                                 */
/* INPUTS: filename                                                                */
/*                                                                                 */
/* OUTPUTS: numlines                                                               */
/*                                                                                 */
/* CALLED BY:                                                                      */
/*   read_source_data()                                                            */
/*                                                                                 */


int get_nsource(char * filename){  /* source input filename */

    FILE *fp;                     /* The file to be opened*/
    char line[STR_MAX_LEN];       /* string line in file */
    int n_commentlines=0;         /* number of comment lines or spaces in file */
    int numlines=0;               /* number of valid source lines in file */
    int len=0;                      /* string length */
    int j=0, k=0;                 /* loop index */
    char temp_line[STR_MAX_LEN];  /* temporary string buffer */

    fp = fopen(filename,"r");
    while(fgets(line,sizeof(line),fp) != NULL){

        /* strip trailing '\n' if it exists */
        len = strlen(line)-1;
        if(line[len] == '\n')
            line[len] = 0;

        /* remove spaces in line */
        k=0;
        for (j=0; j<=len; j++)
            if (line[j] != ' ')
                temp_line[k++] = line[j];
        temp_line[k] = 0;
        strcpy(line,temp_line);
       
        if ( (line[0] == '#') || (strlen(line) == 0) || (line[0] == ' ') ){/* ignore commented lines denoted by # */
            n_commentlines++;
        }else
            numlines++;
    }

    fclose(fp);
    printf("Found %d valid sources in file %s\n",numlines,filename);

    return numlines;
}


        
/******************* OUTPUT RELATED FUNCTIONS *******************/


/* FUNCTION NAME: setup_output                                                                          */
/*                                                                                                      */
/* CALLING SEQUENCE:                                                                                    */
/*  errstatus = setup_output(outfile, clobber, version, &s_obs, &s_mdb, &rmf_struct, ebounds_TLMIN1,    */ 
/*    ebounds_TLMAX1, debug);                                                                           */
/*                                                                                                      */
/* PURPOSE: Initialize the output FITS event file, and populate its header info.                        */
/*                                                                                                      */
/* INPUTS: outfile, clobber, version, s_obs, s_mdb, rmf_struct, ebounds_TLMIN1, ebounds_TLMAX1          */
/*                                                                                                      */
/* OUTPUTS: errstatus, actual fits file specified by "outfile"                                          */
/*                                                                                                      */
/* CALLED BY:                                                                                           */
/*   main()                                                                                             */
/*                                                                                                      */


int setup_output(char * outfile,        /* Output event filename */
                 int clobber,           /* Overwrite existing output file? */
                 char * version,        /* Last known date of simulator update */
                 ObsParams * s_obs,    /* Structure containing observation parameters */
                 MDB_params * s_mdb,    /* Structure containing derived MDB parameters */
                 RMF * rmf_struct,      /* Structure containing rmf and ebounds information */
                 long ebounds_TLMIN1,   /* EBOUNDS minimum allowed PI channel value */
                 long ebounds_TLMAX1,  /* EBOUNDS maximum allowed PI channel value */
                 int debug){

    char * base = 0;           /* the base name of the output fits file */
    int fstat=0;               /* fits read status */
    fitsfile * ounit = NULL;   /* output fits file structure */
    int filenum=0;               /* number to append to base, if filename already exists */

    time_t now=0;                             /* The current time. */
    time_t later=0;                           /* The time after exposure has passed. */
    struct tm * time_ptr;                   /* tm structure containing time tags.  In time.h */
    char datec[20] = "";                    /* UT date of file creation,     YYYY-MM-DDThh:mm:ss */
    char dates[20] = "";                    /* UT date of observation start, YYYY-MM-DDThh:mm:ss */
    char datee[20] = "";                    /* UT date of observation end,   YYYY-MM-DDThh:mm:ss */
    int mjdrefi = 0;                        /* Modified Julian Day */
    float mjdreff = 0.0;                    /* Modified Julian Time */

    char * ttype[] = {"TIME","X   ","Y   ", "PI  ", "PILEUP", "Pixel_ID"};          /* FITS header column titles */
    char * tform[] = {"1D","1J","1J","1J","1I","1J"};                                    /* FITS header column format */
    char * tunit[]  = {"s      ","       ","       ","       ","       ","       "};     /* FITS header column units  */

    char create[18] = "";         /* String buffer, holds current version name of sim code */ 
    int errstatus = 0;            /* error status.  0 to continue. */

    printf("\nInitializing output FITS file...\n");

    errstatus = string2upper(s_obs->mission);
    errstatus = string2upper(s_obs->instrume);
    errstatus = string2upper(s_obs->filter);

    /* Initialize the output fits file */
    
    /* If the output file name was specified, then initialize the file */
    if (0 != strncasecmp("   ",outfile,3)) {
        /* if the output filename is not blank, strip off the suffix and append ".fits",  */
        /*   then create the file */
        
        base = calloc(STR_MAX_LEN, sizeof(char));
        strcpy(base,outfile);
        remove_substring(base,".fits");
        sprintf(outfile,"%s.fits",base);
        
        if (clobber == 1){
            if (debug == 1) printf("Clobber engaged; removing %s.\n",outfile);
            remove(outfile);
        }
        
        fits_create_file(&ounit, outfile, &fstat);
    }

    /* If the output file name was not specified or a file with the same name already  */
    /*    existed, then create a name with base "quicksim" + <a version number>   */
    if ( (0 == strncasecmp("   ",outfile,3)) || (fstat != 0) ) {
        strcpy(outfile,"sim_output.fits");
        
        fstat = 1;
        filenum = 0;
        
        while ( (0 != fstat) && (filenum < 99) ) {
            fstat = 0;
            fits_create_file(&ounit, outfile, &fstat);
            if (0 != fstat) {
                filenum++;
                sprintf(outfile,"sim_output%02d.fits",filenum);
            }
        }
    }
    
    /* print the output filenames */
    if (debug == 1){
        printf("Check output filenames:\n");
        printf("  base = %s\n",base);
        printf("  outfile = %s\n",outfile);
    }

    if (fstat > 0) {
        printf("Failure on photon event file open statement\n");
        printf("CFITSIO status: %d", fstat);
        fits_report_error(stderr, fstat);
        return(fstat);
    }

    /* populate the output event file FITS header */
    
    sprintf(create,"HEASim %s",version);
    
    fits_write_imghdr(ounit,8,0,0,&fstat);                        /* ftphps */
    fits_write_key_str(ounit,"ORIGIN",create," ",&fstat);     /* ftpkys */
    
    if (fstat > 0){
        if (debug == 1) printf("Create fits output file: ");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }

    /* Get the current time. */
    now = time(NULL);
    //time_ptr = localtime(&now);  /* Use for local time */
    time_ptr = gmtime(&now);   /* Use for UTC */
     
    /* Set the MJD values */
    double MJD = (double)now / 86400. + 40587;
    mjdrefi = (int)MJD;
    mjdreff = MJD - mjdrefi;
    

    /* Format the time pointer into YYYY-MM-DDThh:mm:ss, and store to dates and datec */
    strftime(datec, 100, "%Y-%m-%dT%H:%M:%S", time_ptr);
    strftime(dates, 100, "%Y-%m-%dT%H:%M:%S", time_ptr);

    /* Write the start-time related keywords to header */
    fits_write_key_str(ounit,"DATE",datec,"Date and time of creation",&fstat);
    fits_write_key_str(ounit,"CREATOR",create, "Name of code that generated this file",&fstat);
    fits_write_key_str(ounit,"TELESCOP",s_obs->mission,"Name of the Mission ***  ",&fstat);
    fits_write_key_str(ounit,"INSTRUME",s_obs->instrume,"Name of the instrument ***  ",&fstat);
    fits_write_key_str(ounit,"FILTER",s_obs->filter, "Filter", &fstat);
    fits_write_key_str(ounit,"DATE-OBS",dates, "UT date and time of observation start",&fstat);

    if (fstat > 0){
        if (debug == 1) printf("Format date: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
    }

    /* Reset date after exposure time has passed */
    later = now + s_obs->exposure;
    //time_ptr = localtime(&later);  /* Use for local time */
    time_ptr = gmtime(&later);  /* Use for UTC */

    /* Format the time pointer into YYYY-MM-DDThh:mm:ss, and store to datee */
    strftime(datee, 100, "%Y-%m-%dT%H:%M:%S", time_ptr);

    /* Write the end-time related keywords to header */
    fits_write_key_str(ounit,"DATE-END",datee, "UT date and time of observation end",&fstat);
    fits_write_key_str(ounit,"DATAMODE","STANDARD","Datamode all set to STANDARD ***  ",&fstat);
    fits_write_key_str(ounit,"OBS_MODE","POINTING","Pointing or Slew ***  ",&fstat);
    fits_write_chksum(ounit,&fstat);
    
    if (debug == 1){
        printf("Formatting current date:\n");
        printf("  Exposure start date  = %s\n",dates);
        printf("  Exposure end date    = %s\n",datee);
        printf("  File creation date   = %s\n",datec);
    }

    if (fstat > 0){
        if (debug == 1) printf("Split exposure time: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }
    
    /*  Create binary table extension to accept events */
    
    fits_insert_btbl(ounit,0,6,ttype,tform,tunit,"EVENTS",0,&fstat);
    fits_write_key_str(ounit,"HDUCLASS","OGIP", "format conforms to OGIP standard",&fstat);
    fits_write_key_str(ounit,"HDUCLAS1","EVENTS", "Photon event file (OGIP memo OGIP-92-007)",&fstat);
    fits_write_key_str(ounit,"CREATOR",create, "Name of code that generated this file",&fstat);
    fits_write_key_str(ounit,"TELESCOP",s_obs->mission,"Name of the Mission ***  ",&fstat);
    fits_write_key_str(ounit,"INSTRUME",s_obs->instrume,"Name of the instrument ***  ",&fstat);
    fits_write_key_str(ounit,"FILTER",s_obs->filter, "Filter", &fstat);
    fits_write_key_str(ounit,"DATAMODE","STANDARD","Datamode all set to STANDARD ***  ",&fstat);
    fits_write_key_str(ounit,"OBS_MODE","POINTING","Pointing or Slew ***  ",&fstat);

    
    if ( (0 == strcasecmp("epn",s_obs->instrume)) || (0 == strcasecmp("pn",s_obs->instrume)) ){
        fits_write_key_str(ounit,"PN-MODE", s_obs->instmode, "PN mode",&fstat);
    }

    fits_write_key_str(ounit,"RADECSYS","FK5", "Celestrial coord system",&fstat);
    fits_write_key_lng(ounit,"EQUINOX",2000,"Equinox of celestial coord system",&fstat);
    fits_write_key_lng(ounit,"MJDREFI",mjdrefi, "MJD reference day",&fstat);
    fits_write_key_fixflt(ounit,"MJDREFF",mjdreff,6, "MJD reference fraction of day",&fstat);
    
    fits_write_key_str(ounit,"TIMESYS","TT", "Fundamental time system: Terrestrial Time",&fstat);
    fits_write_key_str(ounit,"TIMEUNIT","s", "Unit for TSTART, TSTOP, TIMEZERO, TELPASE",&fstat);
    fits_write_key_str(ounit,"TIMEREF","LOCAL", "spatial reference frame",&fstat);
    fits_write_key_str(ounit,"TASSIGN","SATELLITE", "location of time assignment",&fstat);
    fits_write_key_fixflt(ounit,"TIMEZERO",0.0,6,"Offset for TIME column",&fstat);
    fits_write_key_fixdbl(ounit,"TSTART",0.0,6,"Start time ***",&fstat);
    fits_write_key_fixdbl(ounit,"TSTOP",s_obs->exposure,6,"Stop Time ***",&fstat);
        
    fits_write_key_fixflt(ounit,"RA_NOM",s_obs->rapoint,6, "Nominal Right Ascension (deg)",&fstat);
    fits_write_key_fixflt(ounit,"DEC_NOM",s_obs->decpoint,6, "Nominal Declination (deg)",&fstat);
    fits_write_key_fixflt(ounit,"PA_NOM",s_obs->roll,6, "Nominal ROLL (deg CCW North)",&fstat);
    fits_write_key_str(ounit,"DATE",datec, "Date and time of file creation",&fstat);
    fits_write_key_str(ounit,"DATE-OBS",dates, "UT date and time of observation start",&fstat);
    fits_write_key_str(ounit,"DATE-END",datee, "UT date and time of observation end",&fstat);
    fits_write_key_fixflt(ounit,"EXPOSURE",s_obs->exposure,6,"Simulated exposure time",&fstat);

    if (fstat > 0){
        if (debug == 1) printf("Create binary table: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }

    fits_modify_comment(ounit,"NAXIS2","Number of events",&fstat);

    /* TIME Header Keywords (column 1) */
    fits_modify_comment(ounit,"TFORM1","data format of field: 8-byte DOUBLE",&fstat);
    fits_modify_comment(ounit,"TTYPE1","Time of event arrival",&fstat);
    fits_modify_comment(ounit,"TUNIT1","physical unit of field",&fstat);

    if (fstat > 0){
        printf("Column 1 TIME: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }

    /* X Header Keywords (column 2) */
    fits_write_key_str(ounit,"TCTYP2","RA---TAN","Coord type: RA tangent plane projection",&fstat);
    fits_write_key_str(ounit,"TCUNI2","deg","Units for RA tanget plane projection",&fstat);
    fits_write_key_fixflt(ounit,"TCRPX2",s_mdb->crpxx_sky,6,"X-axis reference pixel of projected image",&fstat);
    fits_write_key_fixflt(ounit,"TCRVL2",s_obs->rapoint,6,"Sky coord (deg) at X-axis ref pixel",&fstat);
    fits_write_key_flt(ounit,"TCROT2",0.0,6,"Rotation angle (degrees)",&fstat);
    fits_write_key_lng(ounit,"TLMIN2",s_mdb->xmin_sky,"Minimum legal image X axis value",&fstat);
    fits_write_key_lng(ounit,"TLMAX2",s_mdb->xmax_sky,"Maximum legal image X axis value",&fstat);
    fits_write_key_flt(ounit,"TCDLT2",s_mdb->cdltx_sky,6,"X increment (deg/pixel) at ref pixel",&fstat);
    fits_modify_comment(ounit,"TFORM2","data format of field: 4-byte INTEGER",&fstat);
    fits_modify_comment(ounit,"TTYPE2", "Sky X", &fstat);
    fits_modify_comment(ounit,"TUNIT2","physical unit of field",&fstat);


    if (fstat > 0){
        printf("Column 2 X: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }

    /* Y Header Keywords (column 3) */
    fits_write_key_str(ounit,"TCTYP3","DEC--TAN","Coord type: Dec tangent plane projection",&fstat);
    fits_write_key_str(ounit,"TCUNI3","deg","Units for Dec tangent plane projection",&fstat);
    fits_write_key_fixflt(ounit,"TCRPX3",s_mdb->crpxy_sky,6,"Y-axis reference pixel of projected image",&fstat);
    fits_write_key_fixflt(ounit,"TCRVL3",s_obs->decpoint,6,"Sky coord (deg) at Y-axis ref pixel",&fstat);
    fits_write_key_flt(ounit,"TCDLT3",s_mdb->cdlty_sky,6,"Y increment (deg/pixel) at ref pixel",&fstat);
    fits_write_key_flt(ounit,"TCROT3",0.0,6,"Rotation angle (degrees)",&fstat);
    fits_write_key_lng(ounit,"TLMIN3",s_mdb->ymin_sky,"Minimum legal image Y axis value",&fstat);
    fits_write_key_lng(ounit,"TLMAX3",s_mdb->ymax_sky,"Maximum legal image Y axis value",&fstat);
    fits_modify_comment(ounit,"TFORM3","data format of field: 4-byte INTEGER",&fstat);
    fits_modify_comment(ounit,"TTYPE3", "Sky Y", &fstat);
    fits_modify_comment(ounit,"TUNIT3","physical unit of field",&fstat);

    /* +++ 2016-08-24 RSH ***new */
    /* deletions only */

    if (fstat > 0){
        printf("Column 3 Y: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }

    /* PI Header Keywords (column 4) */
    fits_write_key_lng(ounit,"TLMIN4",ebounds_TLMIN1,"Minimum legal PI channel value",&fstat);
    fits_write_key_lng(ounit,"TLMAX4",ebounds_TLMAX1,"Maximum legal PI channel value",&fstat);
    fits_modify_comment(ounit,"TTYPE4","Pulse invariant event energy",&fstat);
    fits_modify_comment(ounit,"TFORM4","data format of field: 4-byte INTEGER",&fstat);    
    fits_modify_comment(ounit,"TUNIT4","physical unit of field",&fstat);

    if (fstat > 0){
        printf("Column 4 PI: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }
    
    /* PILEUP Header Keywords (column 5) */
    fits_write_key_lng(ounit,"TLMIN5",0,"Minimum legal pileup flag value",&fstat);
    fits_write_key_lng(ounit,"TLMAX5",1,"Maximum legal pileup flag value",&fstat);
    fits_modify_comment(ounit,"TTYPE5","Pileup flag",&fstat);
    fits_modify_comment(ounit,"TUNIT5","physical unit of field",&fstat);    

    if (fstat > 0){
        printf("Column 5 PileUp Flag: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }

    /* Pixel ID Header Keywords (column 6) - needed for pileup flagging, but will be deleted */
    fits_modify_comment(ounit,"TTYPE6","Pixel ID",&fstat);
    fits_modify_comment(ounit,"TFORM6","data format of field: 4-byte INTEGER",&fstat);
    fits_modify_comment(ounit,"TUNIT6","physical unit of field",&fstat);

    /* We will write CHECKSUM and DATASUM keywords at the end of doWork(). */

    if (fstat > 0){
        printf("End state: \n");
        fits_report_error(stderr,fstat);              /* fitsioerr */
        return fstat;
    }


    

    /* Close the fits file as is; we'll need to open it again later to write the simulated data. */
    fits_close_file(ounit,&fstat);

    /* clean up */
    free(base);

    printf("...done.\n\n");
    
    return 0;

}





/* FUNCTION NAME: read_vignette                                                         */
/*                                                                                      */
/* PURPOSE:                                                                             */
/*   Read the vignette file and store data to structure.                                */
/*                                                                                      */
/* CALLING SEQUENCE: result = read_vignette(s_calfiles->vigfile, &s_vig, debug);  */
/*                                                                                      */
/* INPUTS:                                                                              */
/*   vigfile, s_vig, debug                                                        */
/*                                                                                      */
/* OUTPUTS:                                                                             */
/*   the populated s_vig structure, and the int vignette type                           */
/*                  type = 0 for no vignette file                                       */
/*                  type = 1 for VIG_FITS_1  (enabled, but unused)                      */
/*                  type = 2 for VIG_FITS_2  (not enabled)                              */
/*                  type = 3 for VIG_IMAGE   (not enabled)                              */
/*                  type = 4 for VIG_TEXT_1                                             */
/*                  type = -1 for problem with read                                     */
/*                                                                                      */
/* CALLED BY: main(), in "initialize" section.                                          */
/*                                                                                      */

int read_vignette(char * vigfile,  /* vignette file name */
                  Vig_struct * s_vig,    /* Structure that will hold vig data */
                  int debug){            /* flag to display debug info */

    fitsfile * ounit = NULL;       /* fits file unit */
    FILE *fp = NULL;               /* ascii file unit */
    char line[STR_LONG_MAX_LEN];   /* string holder for file line */
    char *key=NULL, *val=NULL;     /* string holder for keyword and value */
    char * comment = NULL;         /* string holder for comment */
    int fstat = 0;                 /* status flag for success/failure of fits operations */
    int nfound=0, anynull=0;       /* unused ints needed for fits operations */
    int len=0, ii=0;               /* loop index */
    int mat_counter=0;             /* vig_matrix index */
    int n1flag=0;                  /* flag for keyword NAXIS1 found */
    int n2flag=0;                  /* flag for keyword NAXIS2 found */
    int extflag=0;                 /* flag for keyword EXTNAME found */
    int tfflag=0;                  /* flag for keyword TFIELDS found */
    int tf_holder=0, n1_holder=0;  /* temporary int holders for TFIELDS and NAXIS1 values */
    int n_hdu=0;                   /* number of HDU extensions found */
    int hdutype=0;                 /* HDU type (BINARY_TBL, IMAGE_HDU) */
    int n_vig_hdu=0;               /* number of vignette HDUs found */
    int n_img_hdu=0;               /* number of vignette image HDUs found */
    int n_bin_hdu=0;               /* number of vignette binary HDUs found */
    int n_energy=0;                  /* number of energies */
    int nx=0, ny=0;                    /* dimensions of vignette image table */
    int colnum = 0;

    int HDU_num = 1;
    int HDU_TYPE = -1;
    char tempstring[STR_MAX_LEN];
    char img_telescope[STR_MAX_LEN];
    char * extnum=0;

    if (0 == strcasecmp(vigfile,"none")){
        s_vig->vigtype = 0;
        strcpy(s_vig->type_desc,"none");

    } else {

        /* parse the filename, see if HDU extension is given, otherwise = 1 */
        HDU_num = 1;
        strcpy(tempstring,vigfile);
        vigfile = strtok(tempstring,"[");
        extnum = strtok(NULL,"]");
        if (extnum != NULL)
            HDU_num = atoi(extnum);
        printf("Reading vignette file...\n"); 
        if (debug == 1) printf("starting from HDU header %d\n",HDU_num);
        
        /* try to open as if fits file... */
        fits_open_file(&ounit, vigfile, READONLY, &fstat);

        if (fstat == 0){

            /* get the number of HDU entries contained in fits file */
            fits_get_num_hdus(ounit,&n_hdu,&fstat);

            /* how many of those entries are VIGNETTE extentions? */
            for (ii=1; ii<=n_hdu; ii++){
                fits_movabs_hdu(ounit,ii,&hdutype,&fstat);
                fits_read_key_str(ounit, "EXTNAME", line, comment, &fstat);
                if (0 == strcasecmp(line,"VIGNETTE")){
                    n_vig_hdu++;
                    fits_get_hdu_type(ounit, &hdutype, &fstat);
                    if (hdutype == IMAGE_HDU) n_img_hdu++;
                    if (hdutype == BINARY_TBL) n_bin_hdu++;
                }
                fstat=0;
            }

            if (debug == 1){
                printf("\n%d of %d HDU entries is VIGNETTE.\n",n_vig_hdu,n_hdu);
                printf("%d of those VIG HDUs are BINARY_TBL.\n",n_bin_hdu);
                printf("%d of those VIG HDUs are IMAGE_HDU.\n",n_img_hdu);
            }

            if ( n_vig_hdu < 1){ /* if no EXTNAME = VIGNETTE keywords are found */

                printf("ERROR: At least one fits extention must be VIGNETTE.\n");
                printf("Please set header keyword EXTNAME to VIGNETTE in vignette file.\n");
                return -1;

            } else if ( (n_bin_hdu > 0) && (n_img_hdu > 0) ){  /* if both binary table and image are present in vig file */

                printf("ERROR: Cannot use both IMAGE_HDU and BINARY_TBL as vignette.\n");
                return -1;

            } else if ( (n_bin_hdu == 1) && (n_img_hdu == 0) ){ /* if the only VIG extention is a non-image */
                /* ################################### */
                /* ############ VIG_FITS_1 ########### */
                /* ################################### */
                s_vig->vigtype = 1;
                strcpy(s_vig->type_desc,"VIG_FITS_1");

                /* move to the VIGNETTE extention and get the matrix dimensions */
                if (fits_movnam_hdu(ounit, BINARY_TBL, "VIGNETTE", 0, &fstat))
                    printf("Could not move to HDU extension called VIGNETTE.\n");
                if (fits_read_key_lng(ounit, "TFIELDS", &s_vig->ncol, comment, &fstat))
                    printf("Could not find TFIELDS keyword.\n");
                if (fits_read_key_lng(ounit, "NAXIS2", &s_vig->nrow, comment, &fstat))
                    printf("Could not find NAXIS2 keyword.\n");
                if (fits_read_key_str(ounit, "EXTNAME", s_vig->extname, comment, &fstat))
                    printf("Could not find EXTNAME keyword.\n");

                if (fstat) return -1;

                /* What if this is actually a type 2 fits file, but only has one extension? */
                int TREAT_TYPE2_AS_TYPE1 = 0;

                /* Allocate the vignette matrix and arrays */
                s_vig->vig_matrix = (double **)calloc(s_vig->ncol-1, sizeof(double *));
                for (ii=0; ii<s_vig->ncol-1; ii++)
                    s_vig->vig_matrix[ii] = (double *)calloc(s_vig->nrow,sizeof(double));
                s_vig->energy_vec = calloc(s_vig->ncol-1, sizeof(double));
                s_vig->angle_vec = calloc(s_vig->nrow, sizeof(double));

                /* read the array of energy_vec values */
                fits_read_keys_dbl(ounit, "ENG", 1, s_vig->ncol-1, s_vig->energy_vec, &nfound, &fstat);

                if (nfound == 0){
                    /* If this didn't work, then this may be a TYPE2 file. */
                    printf("Treating VIG_FITS_TYPE2 as VIG_FITS_TYPE1.\n");
                    TREAT_TYPE2_AS_TYPE1 = 1;
                    fstat = 0;
                    fits_read_key_dbl(ounit, "ENERGY", &s_vig->energy_vec[0], comment, &fstat);

                    if (fstat){
                        /* Something still is wrong. */
                        printf("ERROR: read_vignette ENERGY keywords could not be found.\n");
                        return -1;
                    }
                }

                /* read the column of angle_vec values */
                fits_get_colnum(ounit, CASEINSEN, "OFFAXIS", &colnum, &fstat);
                fits_read_col_dbl(ounit, colnum, 1, 1, s_vig->nrow, 0, s_vig->angle_vec, &nfound, &fstat);

                /* read the matrix of vig data */
                for (ii=1; ii<s_vig->ncol; ii++){
                    char colname[10];
                    if (TREAT_TYPE2_AS_TYPE1){
                        sprintf(colname,"VIG");
                        fits_get_colnum(ounit, CASEINSEN, colname, &colnum, &fstat);
                        fits_read_col_dbl(ounit, colnum, 1, 1, s_vig->nrow, 0, s_vig->vig_matrix[ii-1], NULL, &fstat);
                        break;
                    } else if (ii < 10){
                        sprintf(colname,"VIG00%d",ii);
                    } else if (ii < 100){
                        sprintf(colname,"VIG0%d",ii);
                    } else {
                        sprintf(colname,"VIG%d",ii);
                    }
                    trim_string(colname);
                    fits_get_colnum(ounit, CASEINSEN, colname, &colnum, &fstat);
                    fits_read_col_dbl(ounit, colnum, 1, 1, s_vig->nrow, 0, s_vig->vig_matrix[ii-1], NULL, &fstat);
                }
                
                if (debug == 1){
                    /* If debug is set, output an ASCII version of the Vignette FITS file. */
                    int  jj=0;
                    FILE *fp = fopen("output/debug_vignette.txt","w");
                    printf("  Outputting debug file output/debug_vignette.txt.\n");
                    fprintf(fp,"%-15s","OFFAXIS");
                    for (jj=1; jj<s_vig->ncol; jj++){
                        char colname[10];
                        if (TREAT_TYPE2_AS_TYPE1){
                            sprintf(colname,"VIG (HDU 1)");
                        } else if (jj < 10){
                            sprintf(colname,"VIG00%d",jj);
                        } else if (jj < 100){
                            sprintf(colname,"VIG0%d",jj);
                        } else {
                            sprintf(colname,"VIG%d",jj);
                        }
                        fprintf(fp,"%-15s",colname);
                    }
                    fprintf(fp,"\n");
                    for (int kk=0; kk<s_vig->nrow; kk++){
                        fprintf(fp,"%-15f",s_vig->angle_vec[kk]);

                        for (int ll=0; ll<s_vig->ncol-1; ll++){
                            fprintf(fp,"%-15f",s_vig->vig_matrix[ll][kk]);
                        }
                        fprintf(fp,"\n");
                    }
                    fclose(fp);
                }


                fits_close_file(ounit, &fstat);

            } else if ( n_img_hdu > 0 ){ /* if vig file is an image */
                /* ################################### */
                /* ############ VIG_IMAGE ########### */   /* THIS SECTION HAS NOT BEEN TESTED - NO IMAGE FILE AVAILABLE! */
                /* ################################### */
                s_vig->vigtype = 3;
                strcpy(s_vig->type_desc,"VIG_IMAGE  (untested)");

                n_energy = s_vig->n_energy = n_img_hdu;

                /* Allocate the arrays */
                s_vig->energy_vec = calloc(s_vig->n_energy, sizeof(double));
                s_vig->angle_vec = calloc(s_vig->n_energy, sizeof(double));
              
                if (HDU_num > 1){ /* if specified, move to HDU header */
                    fits_movabs_hdu(ounit, HDU_num, &HDU_TYPE, &fstat);
                } else { /* otherwise move to the first VIGNETTE extention  */
                    fits_movnam_hdu(ounit, IMAGE_HDU, "VIGNETTE", 0, &fstat);
                }

                /* get the telescope name */
                fits_read_key_str(ounit, "TELESCOP", img_telescope, comment, &fstat);
                printf("Using a PSF image from %s.\n",img_telescope);

                /* get the matrix dimensions */   /* +++ ARE THESE KEYWORDS THE SAME IN ALL MISSIONS? */
                fits_read_key_dbl(ounit, "ENERGY", &s_vig->energy_vec[0], comment, &fstat);
                fits_read_key_lng(ounit, "NAXIS1", &s_vig->img_nx, comment, &fstat);
                fits_read_key_lng(ounit, "NAXIS2", &s_vig->img_ny, comment, &fstat);
                fits_read_key_dbl(ounit, "CRVAL1", &s_vig->crvl1, comment, &fstat);
                fits_read_key_dbl(ounit, "CRVAL2", &s_vig->crvl2, comment, &fstat);
                fits_read_key_dbl(ounit, "CRPIX1", &s_vig->crpx1, comment, &fstat);
                fits_read_key_dbl(ounit, "CRPIX2", &s_vig->crpx2, comment, &fstat);
                fits_read_key_dbl(ounit, "CDELT1", &s_vig->cdlt1, comment, &fstat);
                fits_read_key_dbl(ounit, "CDELT2", &s_vig->cdlt2, comment, &fstat);

                nx = s_vig->img_nx;
                ny = s_vig->img_ny;

                /* allocate the matrix */
                s_vig->vig_immat = calloc(n_energy, sizeof(double));
                for (ii=0; ii<n_energy; ii++)
                    s_vig->vig_immat[ii] = calloc(nx*ny, sizeof(double));

                /* read entire image into the first column of the 2D, [n_energy x nx*ny] element array */
                fits_read_2d_dbl(ounit, 0, 0, nx, nx, ny, s_vig->vig_immat[0], NULL, &fstat);

                for (ii=1; ii<n_energy; ii++){
                    fits_movrel_hdu(ounit, 1, &hdutype, &fstat); /* +++ PROBLEM IF VIGS ARE NON-CONSECUTIVE!!!*/
                    fits_read_key_str(ounit, "EXTNAME", line, comment, &fstat);
                    if (0 != strcasecmp(line,"VIGNETTE")){
                        printf("ERROR: VIGNETTE extensions must appear consecutively in FITS input file.\n");
                        return -1;
                    }
                    fits_read_key_dbl(ounit, "ENERGY", &s_vig->energy_vec[ii], comment, &fstat);
                    fits_read_2d_dbl(ounit, 0, 0, nx, nx, ny, s_vig->vig_immat[ii], NULL, &fstat);
                }

                fits_close_file(ounit, &fstat);

            } else if (n_bin_hdu > 1){
                /* ################################### */
                /* ############ VIG_FITS_2 ########### */
                /* ################################### */
                s_vig->vigtype = 2;
                strcpy(s_vig->type_desc,"VIG_FITS_2");

                n_energy = n_bin_hdu;
                s_vig->ncol = (long)n_energy + 1;  /* number of energies + angle column */

                /* move to the first VIGNETTE extention and get the matrix dimensions */
                fits_movnam_hdu(ounit, BINARY_TBL, "VIGNETTE", 0, &fstat);
                fits_read_key_lng(ounit, "NAXIS2", &s_vig->nrow, comment, &fstat);

                /* Allocate the vignette matrix and arrays */
                s_vig->vig_matrix = (double **)calloc(s_vig->ncol-1, sizeof(double *));
                for (ii=0; ii<s_vig->ncol-1; ii++)
                    s_vig->vig_matrix[ii] = (double *)calloc(s_vig->nrow,sizeof(double));
                s_vig->energy_vec = calloc(s_vig->ncol-1, sizeof(double));
                s_vig->angle_vec = calloc(s_vig->nrow, sizeof(double));

                /* assign the first entry in energy_vec */
                fits_read_key_dbl(ounit, "ENERGY", &s_vig->energy_vec[0], comment, &fstat);

                /* read the angle_vec column */
                fits_get_colnum(ounit, CASEINSEN, "OFFAXIS", &colnum, &fstat);
                fits_read_col_dbl(ounit, colnum, 1, 1, s_vig->nrow, 0, s_vig->angle_vec, &anynull, &fstat);

                /* read the vig_matrix column */
                fits_get_colnum(ounit, CASEINSEN, "VIG", &colnum, &fstat);
                fits_read_col_dbl(ounit, colnum, 1, 1, s_vig->nrow, 0, s_vig->vig_matrix[0], &anynull, &fstat);

                /* Now do the same for the remainder of the VIGNETTE extensions */
                for (ii=1; ii<n_energy; ii++){
                    /* move to NEXT vig extension */
                    fits_movrel_hdu(ounit, 1, &hdutype, &fstat); /* +++ PROBLEM IF VIGS ARE NON-CONSECUTIVE!!!*/
                    fits_read_key_str(ounit, "EXTNAME", line, comment, &fstat);
                    if (0 != strcasecmp(line,"VIGNETTE")){
                        printf("ERROR: VIGNETTE extensions must appear consecutively in FITS input file.\n");
                        return -1;
                    }

                    /* read the vig_matrix column */
                    fits_get_colnum(ounit, CASEINSEN, "VIG", &colnum, &fstat);
                    fits_read_col_dbl(ounit, colnum, 1, 1, s_vig->nrow, 0, s_vig->vig_matrix[ii], &anynull, &fstat);

                    /* read the energy_vec keyword value */
                    fits_read_key_dbl(ounit, "ENERGY", &s_vig->energy_vec[ii], comment, &fstat);
                }

                if (debug == 1){
                    /* If debug is set, output an ASCII version of the Vignette FITS file. */
                    int  jj=0;
                    FILE *fp = fopen("output/debug_vignette.txt","w");
                    printf("  Outputting debug file output/debug_vignette.txt.\n");
                    for (jj=1; jj<=n_energy; jj++){
                        char colname[10];  /* We assume there will not be more than 999 columns. */
                        sprintf(colname,"VIG (HDU %d)",jj);
                        fprintf(fp,"%-15s %-15s %s: %-15f\n","OFFAXIS",colname,"Energy",s_vig->energy_vec[jj-1]);

                        for (int kk=0; kk<s_vig->nrow; kk++){
                            fprintf(fp,"%-15f%-15f\n",s_vig->angle_vec[kk],s_vig->vig_matrix[jj-1][kk]);
                        }
                        fprintf(fp,"\n\n");
                    }
                    fclose(fp);
                }

                fits_close_file(ounit, &fstat);

            } else { /* Some other problem has occurred */
                printf("ERROR: Vig file is a FITS file, but does not match vignette requirements.\n");
                return -1;
            }

        } else {
            /* We know this is NOT a FITS file...likely ASCII.  Check for keywords.  */
            fstat = 0;
            fp = fopen(vigfile,"r");
            /* first search only for NAXIS1, NAXIS2, and EXTNAME.  Then close the file again. */
            do{
                fgets(line, sizeof(line), fp);
                len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;
                remove_substring(line," ");
                if (0 == strcasecmp(line,"END")) break; /* don't go beyond end of keywords */

                /* populate NAXIS1, NAXIS2, and EXTNAME keywords */
                if (strlen(line) != 0){
                    key = strtok(line,"=");
                    val = strtok(NULL,"/");

                    if (0 == strcasecmp(key,"NAXIS1")){
                        n1_holder = atoi(val);
                        n1flag = 1;
                    } else if (0 == strcasecmp(key,"TFIELDS")){
                        tf_holder = atoi(val);
                        tfflag = 1;
                    } else if (0 == strcasecmp(key,"NAXIS2")){
                        s_vig->nrow = atoi(val);
                        n2flag = 1;
                    } else if (0 == strcasecmp(key,"EXTNAME")){
                        remove_substring(val,"'");
                        strcpy(s_vig->extname, val);
                        extflag = 1;
                    }
                }
            }  while(0 != strcasecmp(val,"END"));
            fclose(fp);

            /* If we haven't found naxis1, naxis2, and extname, then something is wrong */
            if ( (n1flag != 1) || (n2flag != 1) || (extflag != 1) ){
                printf("VIG ERROR: could not find correct matrix dimension keywords!\n");
                s_vig->vigtype = -1;
                return -1;
            } else {
                /* otherwise, proceed */

                /* Some ascii vig files mistakenly write the TFIELDS keyword value in the NAXIS1 slot */
                if (tfflag == 1){
                    s_vig->ncol = tf_holder;
                } else
                    s_vig->ncol = n1_holder;

                /* ################################### */
                /* ############ VIG_TEXT_1 ########### */
                /* ################################### */
                s_vig->vigtype = 4;
                strcpy(s_vig->type_desc,"VIG_TEXT_1");
                if (debug == 1) printf("NCOL: %ld  NROW: %ld  EXTNAME: %s\n",s_vig->nrow,s_vig->ncol,s_vig->extname);

                /* Allocate the vignette matrix and arrays */
                s_vig->vig_matrix = (double **)calloc(s_vig->ncol-1, sizeof(double *));
                for (ii=0; ii<s_vig->ncol-1; ii++)
                    s_vig->vig_matrix[ii] = (double *)calloc(s_vig->nrow,sizeof(double));

                s_vig->energy_vec = calloc(s_vig->ncol-1, sizeof(double));
                s_vig->angle_vec = calloc(s_vig->nrow, sizeof(double));


                /* open the file again to start reading */
                fp = fopen(vigfile,"r");

                /* now search the keywords for the energy values, denoted by "ENG001, ENG002", etc. */
                while ( ( fgets(line, sizeof(line), fp) != NULL) ){
                    len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;
                    remove_substring(line," ");
                    if (0 == strcasecmp(line,"END")) break; /* don't go beyond end of keywords */

                    if (strlen(line) != 0){
                        key = strtok(line,"=");
                        val = strtok(NULL,"/");

                        if (0 == strncasecmp(key,"ENG",3)){
                            remove_substring(key,"ENG");
                            s_vig->energy_vec[atoi(key)-1] = (double)atof(val);
                        }
                    }
                }

                /* now read in the matrix data */
                mat_counter=0;  /* this is the current ROW */
                while ( fgets(line, sizeof(line), fp) != NULL){
                    len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;

                    /* if we're not dealing with an empty line... */
                    if (strlen(line) > 0){

                        /* first column is angle data */
                        val = strtok(line," ");
                        s_vig->angle_vec[mat_counter] = (double)atof(val);

                        for (ii=0; ii<s_vig->ncol-1; ii++){
                            val = strtok(NULL," ");
                            s_vig->vig_matrix[ii][mat_counter] = (double)atof(val);
                        }
                        mat_counter++;
                    }
                }
                fclose(fp);
            }
        }
    }

    /* Return:                         */
    /* type = 0 for no vignette file   */
    /* type = 1 for VIG_FITS_1         */
    /* type = 2 for VIG_FITS_2         */
    /* type = 3 for VIG_IMAGE          */
    /* type = 4 for VIG_TEXT_1         */
    /* type = -1 for problem with read */
    if (debug == 1) printf("\nVignette file is type %d: %s\n",s_vig->vigtype,s_vig->type_desc);
    printf("...done.\n");

    return s_vig->vigtype;

}



/* FUNCTION:  read_psf                                                      */
/*                                                                          */
/* PURPOSE: read the psf file and store data to PSF structure               */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */ 
/*   psftype = read_psf(s_calfiles.psffile, &s_psf, &s_psf_image, debug);  */
/*                                                                          */
/* INPUTS: psffile, debug                                                  */
/*                                                                          */
/* OUTPUTS: s_psf, s_psf_image, also returns psftype:                       */
/*    type = 0 for no psf file                                              */
/*    type = 1 for EEF_FITS_1                                               */
/*    type = 2 for EEF_FITS_2                                               */
/*    type = 3 for EEF_IMAGE                                                */
/*    type = 4 for EEF_TEXT_1                                               */
/*    type = 5 for GAUSSIAN                                                 */
/*    type = -1 for problem with read                                       */
/*                                                                          */
/* CALLED BY:  main()                                                       */
/*                                                                          */


int read_psf(char * psffile,          /* the PSF file name */
             PSF_struct * s_psf,       /* Structure that will contain PSF data */
             PSF_image * s_psf_image,  /* Structure that will contain PSF image data */
             int debug){               /* flag to report debugging data */

    fitsfile * ounit = NULL;        /* the FITS file unit */
    FILE *fp=NULL;                  /* ascii file unit */
    char *key=NULL, *val=NULL;      /* keyword and value string holders */
    char line[STR_LONG_MAX_LEN];    /* ascii file line string holder */
    char * comment = NULL;          /* fits file string comment holder */
    int fstat = 0;                  /* status flag for success/failure of fits operation */
    int tf_holder=0, n1_holder=0;   /* temporary int holder for TFIELDS and NAXIS1 values */
    int n1flag=0;                   /* flag indicating NAXIS1 keyword found */
    int n2flag=0;                   /* flag indicating NAXIS2 keyword found */
    int extflag=0;                  /* flag indicating EXTNAME keyword found */
    int tfflag=0;                   /* flag indicating TFIELDS keyword found */
    int len=0;                      /* string length */
    int ii=0;                       /* loop index */
    int mat_counter=0;              /* index for s_psf->eef_matrix[][] */
    int n_hdu=0;                    /* number of HDU extensions found in fits file */
    int hdutype=0;                  /* type of HDU (BINARY_TBL or IMAGE_HDU) */
    int n_eef_hdu=0;                /* number of HDUs that are extname EEF */
    int n_img_hdu=0;                /* number of EEF HDUs that are type IMAGE_HDU */
    int n_bin_hdu=0;                /* number of EEF HDUs that are type BINARY_TBL */
    int anynull=0, nfound=0;        /* unused integer inputs required for some fits operations */
    int colnum=0;                   /* column number */

    int HDU_num = 1;
    char tempstring[STR_MAX_LEN];
    char * extnum=0;    

    if (0 == strncasecmp(psffile,"GAUS",4)){
        /* don't read a file; just set Gaussian devation */
        s_psf->psftype = 5;
        strcpy(s_psf->type_desc,"GAUSSIAN");

    } else if (0 == strcasecmp(psffile,"none")){
        s_psf->psftype = 0;
        strcpy(s_psf->type_desc,"none");

    } else {

        /* parse the filename, see if HDU extension is given, otherwise = 1 */
        HDU_num = 1;
        strcpy(tempstring,psffile);
        psffile = strtok(tempstring,"[");
        extnum = strtok(NULL,"]");
        if (extnum != NULL)
            HDU_num = atoi(extnum);
        printf("Reading PSF file...\n");
        if (debug == 1) printf("starting from HDU header %d\n",HDU_num);


        /* try to open as fits file */
        fits_open_file(&ounit, psffile, READONLY, &fstat);

        if (fstat == 0){

            /* get the number of HDU entries contained in fits file */
            fits_get_num_hdus(ounit,&n_hdu,&fstat);

            /* how many of those entries are EEF extentions? */
            for (ii=1; ii<=n_hdu; ii++){
                fits_movabs_hdu(ounit,ii,&hdutype,&fstat);
                fits_read_key_str(ounit, "EXTNAME", line, comment, &fstat);
                if ( (0 == strcasecmp(line,"EEF")) ||
                     (0 == strcasecmp(line,"PSF")) ||
                     (0 == strcasecmp(line,"IMAGE_PSF")) ){
                    n_eef_hdu++;
                    fits_get_hdu_type(ounit, &hdutype, &fstat);
                    if (hdutype == IMAGE_HDU) n_img_hdu++;
                    if (hdutype == BINARY_TBL) n_bin_hdu++;
                }
                
                fstat=0;
            }
            if (debug == 1){
                printf("%d of %d HDU entries is EEF, PSF, or PSF_IMAGE\n",n_eef_hdu,n_hdu);
                printf("%d of those PSF HDUs are BINARY_TBL.\n",n_bin_hdu);
                printf("%d of those PSF HDUs are IMAGE_HDU.\n\n",n_img_hdu);
            }

            if ( n_eef_hdu < 1){ /* if no EXTNAME = EEF keywords are found */

                printf("ERROR: At least one fits extention must be EEF.\n");
                printf("Please set header keyword EXTNAME to EEF in psf file.\n");
                return -1;

            } else if ( (n_bin_hdu == 1) && (n_img_hdu == 0) ){ /* if the only EEF extention is a non-image */
                /* ################################### */
                /* ############ EEF_FITS_1 ########### */
                /* ################################### */
                s_psf->psftype = 1;
                strcpy(s_psf->type_desc,"EEF_FITS_1");

                /* move to the EEF extention and get the matrix dimensions */
                if (fits_movnam_hdu(ounit, BINARY_TBL, "EEF", 0, &fstat))
                    printf("Could not find HDU extension called EEF.\n");
                if (fits_read_key_lng(ounit, "TFIELDS", &s_psf->ncol, comment, &fstat))
                    printf("Could not find keyword TFIELDS.\n");
                if (fits_read_key_lng(ounit, "NAXIS2", &s_psf->nrow, comment, &fstat))
                    printf("Could not find keyword NAXIS2.\n");

                if (fstat) return -1;

                /* What if this is actually a type 2 fits file, but only has one extension? */
                int TREAT_TYPE2_AS_TYPE1 = 0;

                s_psf->n_eef = s_psf->ncol-1;
                s_psf->n_radii = s_psf->nrow;

                /* Allocate the psf matrix and arrays */
                s_psf->eef_matrix = (double **)calloc(s_psf->n_eef, sizeof(double *));
                for (ii=0; ii<s_psf->n_eef; ii++)
                    s_psf->eef_matrix[ii] = (double *)calloc(s_psf->nrow,sizeof(double));

                s_psf->energy_vec = calloc(s_psf->n_eef, sizeof(double));
                s_psf->angle_vec = calloc(s_psf->n_eef, sizeof(double));
                s_psf->radii = calloc(s_psf->n_radii, sizeof(double));

                /* read the array of energy_vec and angle_vec values */
                fits_read_keys_dbl(ounit, "ENG", 1, s_psf->n_eef, s_psf->energy_vec, &nfound, &fstat);
                fits_read_keys_dbl(ounit, "OFF", 1, s_psf->n_eef, s_psf->angle_vec, &nfound, &fstat);

                if (nfound == 0){
                    /* If this didn't work, then this may be a TYPE2 file. */
                    printf("Treating FITS_EEF_TYPE2 as FITS_EEF_TYPE1.\n");
                    TREAT_TYPE2_AS_TYPE1 = 1;
                    fstat = 0;
                    fits_read_key_dbl(ounit, "ENERGY", &s_psf->energy_vec[0], comment, &fstat);
                    fits_read_key_dbl(ounit, "OFFAXIS", &s_psf->angle_vec[0], comment, &fstat);

                    if (fstat){
                        /* Something still is wrong. */
                        printf("ERROR: read_psf ENERGY/OFFAXIS keywords could not be found.\n");
                        return -1;
                    }
                }

                /* read the column of radii values */
                fits_get_colnum(ounit, CASEINSEN, "PSFRAD", &colnum, &fstat);           
                fits_read_col_dbl(ounit, colnum, 1, 1, s_psf->n_radii, 0, s_psf->radii, NULL, &fstat);

                /* read the matrix of psf data */
                for (ii=1; ii<s_psf->ncol; ii++){
                    char colname[10];
                    if (TREAT_TYPE2_AS_TYPE1){
                        sprintf(colname,"EEF");
                        fits_get_colnum(ounit, CASEINSEN, colname, &colnum, &fstat);
                        fits_read_col_dbl(ounit, colnum, 1, 1, s_psf->nrow, 0, s_psf->eef_matrix[ii-1], NULL, &fstat);
                        break;
                    } else if (ii < 10){
                        sprintf(colname,"EEF00%d",ii);
                    } else if (ii < 100){
                        sprintf(colname,"EEF0%d",ii);
                    } else {
                        sprintf(colname,"EEF%d",ii);
                    }
                    trim_string(colname);
                    if (fits_get_colnum(ounit, CASEINSEN, colname, &colnum, &fstat))
                        printf("Could not get column number for column name %s\n",colname);
                    fits_read_col_dbl(ounit, colnum, 1, 1, s_psf->nrow, 0, s_psf->eef_matrix[ii-1], NULL, &fstat);
                }

                if (debug == 1){
                    /* If debug is set, output an ASCII version of the Vignette FITS file. */
                    int  jj=0;
                    FILE *fp = fopen("output/debug_psf.txt","w");
                    printf("  Outputting debug file output/debug_psf.txt.\n");
                    fprintf(fp,"%-15s","PSFRAD");
                    for (jj=1; jj<s_psf->ncol; jj++){
                        char colname[10];
                        if (TREAT_TYPE2_AS_TYPE1){
                            sprintf(colname,"ENERGY (HDU 1)");
                        } else if (jj < 10){
                            sprintf(colname,"EEF00%d",jj);
                        } else if (jj < 100){
                            sprintf(colname,"EEF0%d",jj);
                        } else {
                            sprintf(colname,"EEF%d",jj);
                        }
                        fprintf(fp,"%-15s",colname);
                    }
                    fprintf(fp,"\n");
                    for (int kk=0; kk<s_psf->nrow; kk++){
                        fprintf(fp,"%-15f",s_psf->radii[kk]);

                        for (int ll=0; ll<s_psf->ncol-1; ll++){
                            fprintf(fp,"%-15f",s_psf->eef_matrix[ll][kk]);
                        }
                        fprintf(fp,"\n");
                    }
                    fclose(fp);
                }


                fits_close_file(ounit, &fstat);

            } else if (n_img_hdu > 0){  /* if the psf file is an image */
                /* ################################### */
                /* ############ PSF_IMAGE ############ */
                /* ################################### */
                s_psf->psftype = 3;
                strcpy(s_psf->type_desc,"PSF_IMAGE");
                s_psf->n_eef = n_img_hdu;

                /* if we have an image, the read_psf_image routine will read it.  Close the file. */
                fits_close_file(ounit, &fstat);
                fstat = read_psf_image(psffile, s_psf_image, debug);

            } else if ( n_bin_hdu > 1){
                /* ################################### */
                /* ############ EEF_FITS_2 ########### */
                /* ################################### */
                s_psf->psftype = 2;
                strcpy(s_psf->type_desc,"EEF_FITS_2");

                s_psf->n_eef = n_bin_hdu;
                s_psf->ncol = (long)s_psf->n_eef + 1;  /* number of energies + angle column */

                /* move to the first EEF extention and get the matrix dimensions */
                fits_movnam_hdu(ounit, BINARY_TBL, "EEF", 0, &fstat);
                fits_read_key_lng(ounit, "NAXIS2", &s_psf->nrow, comment, &fstat);
                s_psf->n_radii = s_psf->nrow;

                /* Allocate the psf matrix and arrays */
                s_psf->eef_matrix = (double **)calloc(s_psf->n_eef, sizeof(double *));
                for (ii=0; ii<s_psf->n_eef; ii++)
                    s_psf->eef_matrix[ii] = (double *)calloc(s_psf->nrow,sizeof(double));

                s_psf->energy_vec = calloc(s_psf->n_eef, sizeof(double));
                s_psf->angle_vec = calloc(s_psf->n_eef, sizeof(double));
                s_psf->radii = calloc(s_psf->n_radii, sizeof(double));

                /* Grab the first extension's keywords for energy and angle vectors */
                fits_read_key_dbl(ounit, "ENERGY", &s_psf->energy_vec[0], comment, &fstat);
                fits_read_key_dbl(ounit, "OFFAXIS", &s_psf->angle_vec[0], comment, &fstat);

                /* read the radii column */
                fits_get_colnum(ounit, CASEINSEN, "PSFRAD", &colnum, &fstat);           
                fits_read_col_dbl(ounit, colnum, 1, 1, s_psf->n_radii, 0, s_psf->radii, &anynull, &fstat);

                /* read the eef_matrix column */
                fits_get_colnum(ounit, CASEINSEN, "EEF", &colnum, &fstat);
                fits_read_col_dbl(ounit, colnum, 1, 1, s_psf->nrow, 0, s_psf->eef_matrix[0], &anynull, &fstat);

                /* Do the same for the remaining psf extensions */
                for (ii=1; ii<s_psf->n_eef; ii++){
                    /* move to next psf extension */
                    fits_movrel_hdu(ounit, 1, &hdutype, &fstat); /* +++ PROBLEM IF PSFS ARE NON-CONSECUTIVE!!!*/
                    fits_read_key_str(ounit, "EXTNAME", line, comment, &fstat);
                    if (0 != strcasecmp(line,"EEF")){
                        printf("ERROR: EEF extensions must appear consecutively in FITS input file.\n");
                        return -1;
                    }

                    /* read the eef_matrix column */
                    fits_get_colnum(ounit, CASEINSEN, "EEF", &colnum, &fstat);
                    fits_read_col_dbl(ounit, colnum, 1, 1, s_psf->nrow, 0, s_psf->eef_matrix[ii], &anynull, &fstat);

                    /* Grab this extension's keywords for energy and angle vectors */
                    fits_read_key_dbl(ounit, "ENERGY", &s_psf->energy_vec[ii], comment, &fstat);
                    fits_read_key_dbl(ounit, "OFFAXIS", &s_psf->angle_vec[ii], comment, &fstat);
                }

                if (debug == 1){
                    /* If debug is set, output an ASCII version of the Vignette FITS file. */
                    int  jj=0;
                    FILE *fp = fopen("output/debug_psf.txt","w");
                    printf("  Outputting debug file output/debug_psf.txt.\n");
                    for (jj=1; jj<s_psf->ncol; jj++){
                        char colname[20];
                        sprintf(colname,"EEF (HDU %d)",jj);
                        fprintf(fp,"%-15s %-15s %s: %-15f %s: %-15f\n",
                                "PSFRAD",colname,"Energy",s_psf->energy_vec[jj-1],"Offaxis",s_psf->angle_vec[jj-1]);

                        for (int kk=0; kk<s_psf->nrow; kk++){
                            fprintf(fp,"%-15f%-15f\n",s_psf->radii[kk],s_psf->eef_matrix[jj-1][kk]);
                        }
                        fprintf(fp,"\n\n");
                    }
                    fclose(fp);
                }


                fits_close_file(ounit, &fstat);
            }


        } else {
            /* ASCII file...probably.  Perform some checks. */

            fp = fopen(psffile,"r");

            /* first search only for NAXIS1, NAXIS2, and EXTNAME.  Then close the file again. */
            do {
                fgets(line, sizeof(line), fp);
                len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;
                remove_substring(line," ");
                if (0 == strcasecmp(line,"END")) break;

                /* populate NCOL, NAXIS2, and EXTNAME keywords */
                if (strlen(line) != 0){
                    key = strtok(line,"=");
                    val = strtok(NULL,"/");

                    remove_substring(key," ");
                    remove_substring(val," ");
                    remove_substring(val,"'");

                    if (0 == strcasecmp(key,"NAXIS1")){
                        n1_holder = atoi(val);
                        n1flag = 1;
                    } else if (0 == strcasecmp(key,"NAXIS2")){
                        s_psf->nrow = atoi(val);
                        n2flag = 1;
                    } else if ( (0 == strcasecmp(key,"EXTNAME")) && (0 == strcasecmp(val,"EEF"))  ){
                        extflag = 1;
                    } else if (0 == strcasecmp(key,"TFIELDS")){
                        tf_holder = atoi(val);
                        tfflag = 1;
                    }
                }
            } while(0 != strcasecmp(val,"END"));
            fclose(fp);

            /* If we haven't found naxis1, naxis2, and extname, then something is wrong */
            if ( (n1flag != 1) || (n2flag != 1) || (extflag != 1) ){
                printf("PSF ERROR: could not find correct matrix dimension keywords!\n");
                s_psf->psftype = -1;
                return -1;
            } else {
                /* otherwise, proceed */

                /* Some ascii psf files mistakenly write the TFIELDS keyword value in the NAXIS1 slot */
                if (tfflag == 1){
                    s_psf->ncol = tf_holder;
                } else
                    s_psf->ncol = n1_holder;

                s_psf->n_eef = s_psf->ncol-1;
                s_psf->n_radii = s_psf->nrow;

                /* ################################### */
                /* ############ EEF_TEXT_1 ########### */
                /* ################################### */
                s_psf->psftype = 4;
                strcpy(s_psf->type_desc,"EEF_TEXT_1");

                /* Allocate the psf matrix and arrays */
                s_psf->eef_matrix = (double **)calloc(s_psf->n_eef, sizeof(double *));
                for (ii=0; ii<s_psf->n_eef; ii++)
                    s_psf->eef_matrix[ii] = (double *)calloc(s_psf->nrow,sizeof(double));

                s_psf->energy_vec = calloc(s_psf->n_eef, sizeof(double));
                s_psf->angle_vec = calloc(s_psf->n_eef, sizeof(double));
                s_psf->radii = calloc(s_psf->n_radii, sizeof(double));

                /* open the file again to start reading */
                fp = fopen(psffile,"r");

                /* now search the keywords for the energy values, denoted by "ENG001, ENG002", etc. */
                while ( ( fgets(line, sizeof(line), fp) != NULL) ){
                    len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;
                    remove_substring(line," ");
                    if (0 == strcasecmp(line,"END")) break; /* don't go beyond end of keywords */

                    if (strlen(line) != 0){
                        key = strtok(line,"=");
                        val = strtok(NULL,"/");

                        if (0 == strncasecmp(key,"ENG",3)){
                            remove_substring(key,"ENG");
                            s_psf->energy_vec[atoi(key)-1] = (double)atof(val);
                        }

                        if (0 == strncasecmp(key,"OFF",3)){
                            remove_substring(key,"OFF");
                            s_psf->angle_vec[atoi(key)-1] = (double)atof(val);
                        }

                    }
                }
                /* now read in the matrix data */
                mat_counter=0;  /* this is the current ROW */
                while ( fgets(line, sizeof(line), fp) != NULL){
                    len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;

                    /* if we're not dealing with an empty line... */
                    if (strlen(line) > 0){

                        /* first column is angle data */
                        val = strtok(line," ");
                        s_psf->radii[mat_counter] = (double)atof(val);

                        for (ii=0; ii<s_psf->n_eef; ii++){
                            val = strtok(NULL," ");
                            s_psf->eef_matrix[ii][mat_counter] = (double)atof(val);
                        }
                        mat_counter++;
                    }
                }
                fclose(fp);
            }
        }
    }

    /* Return:                         */
    /* type = 0 for no psf file        */
    /* type = 1 for EEF_FITS_1         */
    /* type = 2 for EEF_FITS_2         */
    /* type = 3 for EEF_IMAGE          */
    /* type = 4 for EEF_TEXT_1         */
    /* type = 5 for GAUSSIAN           */
    /* type = -1 for problem with read */
    if (debug == 1) printf("\nPSF file is type %d: %s\n",s_psf->psftype,s_psf->type_desc);

    printf("...done.\n");

    return s_psf->psftype;
}




/* FUNCTION:  read_psf_image                                                */
/*                                                                          */
/* PURPOSE: handle the case for read_psf when the psf file is an image      */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   fstat = read_psf_image(psffile, s_psf_image, debug);                  */
/*                                                                          */
/* INPUTS: psffile, debug                                                  */
/*                                                                          */
/* OUTPUTS: s_psf_image                                                     */
/*                                                                          */
/* CALLED BY:  read_psf()                                                   */
/*                                                                          */


int read_psf_image(char * psffile,
                   PSF_image * s_psf_image,
                   int debug){

    fitsfile * ounit = NULL;                      /* FITS file unit */
    int ii=0, im=0;                                     /* loop index */
    int fstat=0;                                  /* status flag for success/failure of fits operations */
    char * comment = NULL;
    int nimages=0;
    int xdim=0, ydim=0;
    double imtot=0.;
    int PSF_extnum=0;

    /* open the fits file */
    fits_open_file(&ounit, psffile, READONLY, &fstat);

    /* We assume the structure of the fits file is such that a binary extension called
       "PSF" is followed by nimages PSF image extensions, and that nimages can be read
       from the NAXIS2 keyword in the "PSF" extension */
    fits_movnam_hdu(ounit, BINARY_TBL, "PSF", 0, &fstat);
    fits_get_hdu_num(ounit, &PSF_extnum);
    fits_read_key(ounit, TINT, "NAXIS2", &s_psf_image->nimages, comment, &fstat);

    /* Save struct data to shorter name for convenience */
    nimages = s_psf_image->nimages;
    
    /* Print out some diagnostic info if debug */
    if (debug == 1){
        printf("In file %s:\n",psffile);
        printf("   There are %d image extensions.\n\n",nimages);
    }

    /* Now that we have nimages, we can begin allocating... */
    s_psf_image->xdim = calloc(nimages, sizeof(int));
    s_psf_image->ydim = calloc(nimages, sizeof(int));
    s_psf_image->crvl1 = calloc(nimages, sizeof(double));
    s_psf_image->crvl2 = calloc(nimages, sizeof(double));
    s_psf_image->crpx1 = calloc(nimages, sizeof(double));
    s_psf_image->crpx2 = calloc(nimages, sizeof(double));
    s_psf_image->cdlt1 = calloc(nimages, sizeof(double));
    s_psf_image->cdlt2 = calloc(nimages, sizeof(double));
    s_psf_image->azim_vec = calloc(nimages, sizeof(double));
    s_psf_image->energy_vec = calloc(nimages, sizeof(double));
    s_psf_image->angle_vec = calloc(nimages, sizeof(double));
    s_psf_image->prob_array = calloc(nimages, sizeof(double *));
    
    /* Now loop over the images */
    for (ii=0; ii<nimages; ii++){
        printf("   Reading image %d...\n",ii);

        fits_movrel_hdu(ounit, 1, IMAGE_HDU, &fstat);
        fits_read_key(ounit, TINT, "NAXIS1", &s_psf_image->xdim[ii], comment, &fstat);
        fits_read_key(ounit, TINT, "NAXIS2", &s_psf_image->ydim[ii], comment, &fstat);

        /* Save struct data to shorter name for convenience */
        xdim = s_psf_image->xdim[ii];
        ydim = s_psf_image->ydim[ii];

        /* Read keywords for this image */
        fits_read_key_dbl(ounit, "CRVAL1", &s_psf_image->crvl1[ii], comment, &fstat);
        fits_read_key_dbl(ounit, "CRVAL2", &s_psf_image->crvl2[ii], comment, &fstat);
        fits_read_key_dbl(ounit, "CRPIX1", &s_psf_image->crpx1[ii], comment, &fstat);
        fits_read_key_dbl(ounit, "CRPIX2", &s_psf_image->crpx2[ii], comment, &fstat);
        fits_read_key_dbl(ounit, "CDELT1", &s_psf_image->cdlt1[ii], comment, &fstat);
        fits_read_key_dbl(ounit, "CDELT2", &s_psf_image->cdlt2[ii], comment, &fstat);
        fits_read_key_dbl(ounit, "AZIMUTH", &s_psf_image->azim_vec[ii], comment, &fstat);
        fits_read_key_dbl(ounit, "ENERGY", &s_psf_image->energy_vec[ii], comment, &fstat);
        fits_read_key_dbl(ounit, "OFFAXIS", &s_psf_image->angle_vec[ii], comment, &fstat);
        
        /* Allocate and read the 2D image into 1D probability array */
        s_psf_image->prob_array[ii] = calloc(xdim*ydim, sizeof(double));
        fits_read_2d_dbl(ounit, 0, 0, xdim, xdim, ydim, s_psf_image->prob_array[ii], NULL, &fstat);

        /* Get the total counts in the prob_array */
        imtot = 0.0;
        for (im=0; im<xdim*ydim; im++)
            imtot += s_psf_image->prob_array[ii][im];
            
        /* Normalize the prob_array */
        for (im=0; im<xdim*ydim; im++)
            s_psf_image->prob_array[ii][im] /= imtot;

        /* Accumulate the prob_array (start loop at 1; can't accumulate first entry!) */
        for (im=1; im<xdim*ydim; im++)
            s_psf_image->prob_array[ii][im] +=  s_psf_image->prob_array[ii][im-1];

    } /* end loop over images */

    fits_close_file(ounit,&fstat);

    return fstat;
}




/* FUNCTION NAME: read_instmap                                           */
/*                                                                       */
/* PURPOSE:                                                              */
/*    Transfer the contents of an instrument map into an array to be     */
/*    to determine whether a photon lands on the live part of the        */
/*    detector.  Map is assumed to be in simulator "foc" coords.         */
/*                                                                       */
/* CALLING SEQUENCE:                                                     */
/*    errstatus = read_instmap(&s_mdb, debug);                           */
/*                                                                       */
/* INPUTS: s_mdb structure                                               */
/*                                                                       */
/* OUTPUTS: populates several parameters in the s_mdb structure          */
/*                                                                       */
/* CALLED BY:  main()                                                    */
/*                                                                       */

int read_instmap(MDB_params * s_mdb, int debug){


    fitsfile * iunit = NULL;
    int fstat = 0;
    long naxis1, naxis2;

    int HDU_num=1;
    int HDU_TYPE=-1;
    char * HDU=0;
    char tempstring[STR_MAX_LEN];
    char * extnum=0;
    char * xten=0;
    char * com = NULL;
    long num_AXIS=0;
    int status = 0;
    char * full_instmap_path=0;

    printf("\nReading instrument map file...\n");
    if (debug == 1)
        printf("    Relative instmap path:\n      %s\n",s_mdb->instmap_name);

    full_instmap_path = resolve_pathname(s_mdb->instmap_name);
    strcpy(s_mdb->instmap_name,full_instmap_path);
    free(full_instmap_path);
    if (debug == 1) printf("    Absolute instmap path:\n      %s\n",s_mdb->instmap_name);

    /* make sure file exists */
    fits_file_exists(s_mdb->instmap_name, &status, &fstat);
    switch(status){
    case 2:
        if (debug > 0) printf("  Confirmed: compressed version of instrument map file exists on disk.\n");
        break;
    case 1:
        if (debug > 0) printf("  Confirmed: instrument map file exists on disk.\n");
        break;
    case 0:
        printf("  ERROR: instrument map file does not exist on disk:\n   %s\n\n",s_mdb->instmap_name);
        return -1;
        break;
    case -1:
        printf("  WARNING: instrument map file is not a disk file, could be on remote server.\n");
        break;
    default:
        printf("  ERROR: could not execute search for instrument map file.\n");
        return -1;
        break;
    }


    /* parse the filename, see if HDU extension is given, otherwise = 1 */
    HDU_num = 1;
    strcpy(tempstring,s_mdb->instmap_name);
    strcpy(s_mdb->instmap_name,strtok(tempstring,"["));
    extnum = strtok(NULL,"]");
    if (extnum != NULL)
        HDU_num = atoi(extnum);

    /* try to open fits file */
    fits_open_file(&iunit, s_mdb->instmap_name, READONLY, &fstat);
    if (fstat != 0) {
        printf("ERROR: Failed to open imap file %s.\n",s_mdb->instmap_name);
        return(fstat);
    }

    /* move to HDU_num */
    fits_movabs_hdu(iunit, HDU_num, &HDU_TYPE, &fstat);
    if (HDU_TYPE == IMAGE_HDU){ HDU="IMAGE_HDU";}
    else if (HDU_TYPE == ASCII_TBL) { HDU="ASCII_TBL"; }
    else if (HDU_TYPE == BINARY_TBL) {HDU="BINARY_TBL"; }
    else { HDU="unknown"; }
    if (debug == 1) printf("  Moving to HDU %d, type %s.\n",HDU_num,HDU);
    if (fstat > 0) {
        printf("  ERROR: Failed to locate IMAGE HDU in imap file %s.\n",s_mdb->instmap_name);
        printf("       Are you certain this is a FITS image file?\n");
        return(fstat);
    }

    /* If not in the primary HDU, check that XTENSION keyword is "IMAGE" */
    if (HDU_num > 1){
        fits_read_key_longstr(iunit, "XTENSION", &xten, com, &fstat);
        if (debug == 1) printf("  Checking XTENSION keyword: %s\n",xten);
        if (0 != strcasecmp(xten,"IMAGE")){
            printf("  ERROR! Image XTENSION is not IMAGE, cannot read!\n");
            fstat = -1;
            free(xten);  /* fits_read_key_longstr allocates this, need to free. */
            return(fstat);
        } else {
            free(xten);
        }
    }

    /* Check that NAXIS keyword is 2.  Otherwise it's impossible that this is an image. */
    fits_read_key_lng(iunit, "NAXIS", &num_AXIS, com, &fstat);
    if (num_AXIS != 2){
        printf("  ERROR! NAXIS keyword indicates dimensions other than 2!  Not an image!\n");
        fstat = -1;
        return(fstat);
    } else {
        if (debug == 1) printf("  instmap NAXIS keyword confirmed at 2 dimensions.\n");
    }


    fits_read_key_lng(iunit, "NAXIS1", &naxis1, com, &fstat);
    fits_read_key_lng(iunit, "NAXIS2", &naxis2, com, &fstat);

    s_mdb->nx_imap = naxis1;
    s_mdb->ny_imap = naxis2;

    s_mdb->array_imap = calloc(naxis1*naxis2, sizeof(long));

    fits_read_2d_lng(iunit, 0, 0, s_mdb->nx_imap, s_mdb->nx_imap, s_mdb->ny_imap, s_mdb->array_imap, NULL, &fstat);
    printf("...done.\n\n");
    
    fits_close_file(iunit,&fstat);
    return fstat;
}


/* FUNCTION NAME: initialize_structures                                        */
/*                                                                             */
/* PURPOSE:                                                                    */
/*    Assign values to all members of structures (with exception               */
/*    of not-yet-allocated arrays), such that Valgrind won't complain          */
/*    about uninitialized values.                                              */
/*                                                                             */
/* CALLING SEQUENCE:                                                           */
/*    initialize_structures(&s_obs, &s_cal, &s_back, &s_mdb, &s_vig,           */
/*                          &s_psf, &s_psf_image);                             */
/*                                                                             */
/* INPUTS: &s_obs, &s_cal, &s_back, &s_mdb, &s_vig, &s_psf, &s_psf_image       */
/*         &s_tpar, &s_tspec                                                   */
/*                                                                             */
/* OUTPUTS: none                                                               */
/*                                                                             */
/* CALLED BY:  main()                                                          */
/*                                                                             */

void initialize_structures(ObsParams * s_obs,
                           CalFiles * s_cal,
                           GeneralBackground * s_back,
                           MDB_params *s_mdb,
                           Vig_struct * s_vig,
                           PSF_struct * s_psf,
                           PSF_image * s_psf_image,
                           burst_table_struct * btab){

    /* Obs Params */
    strcpy(s_obs->mission,"");        /* Observatory, e.g. Astro-H, XMM, Suzaku... */
    strcpy(s_obs->instrume,"");         /* Instrument, e.g. SXI, SXS, EMOS, etc. */
    strcpy(s_obs->filter,"");             /* Don't know if this will be used */
    strcpy(s_obs->instmode,"");    /* Instrument Mode - will be used? */
    s_obs->rapoint = 0.0;       /* Pointing RA, decimal degrees J2000 */
    s_obs->decpoint = 0.0;      /* Pointing DEC, decimal degrees J2000 */
    s_obs->roll = 0.0;              /* Roll angle, decimal degrees */
    s_obs->exposure = 0.0;          /* Simulation Exposure Time */
    s_obs->flagsubex = 0;                        /* Flag to use subexposure time */
    s_obs->subexposure = 0.0;                   /* Subdivision of exposure time - will co-add at end */
    s_obs->dtpileup = 0.0;                      /* Time range delta T under which pileup may occur */
    s_obs->resample = 0;                         /* Flag to resample sky coordinates within detector pixel */
    s_obs->skipfov = 0;                          /* 1 to skip discarding of  out-of-fov events, otherwise 0 */


    /* Cal Params */
    strcpy(s_cal->psffile,"");                  /* PSF filename */
    strcpy(s_cal->vigfile,"");             /* Vignette filename */
    strcpy(s_cal->arffile,"");                  /* Ancillary Response filename */
    strcpy(s_cal->rmffile,"");                  /* Response Matrix filename */
    strcpy(s_cal->intbackfile,"");  /* Internal Background filename */
    s_cal->arfrmftol = 0.0;        /* Tolerance for RMF & ARF comparison */
    
    /* Background Params */
    strcpy(s_back->psbackfile,"");         /* Point source catalog from background tool */
    strcpy(s_back->difbackfile,"");        /* Diffuse spectrum catalog from background tool */
    strcpy(s_back->pszbackfile,"");        /* Pt-src redshift/abs list from bkg tool */

    /* MDB direct parameters: */
    s_mdb->FOV_RADIUS = 0.0;
    s_mdb->FOCALLEN = 0.0;
    s_mdb->PSF_FWHM = 0.0;
    s_mdb->DET_XSCL = 0.0;
    s_mdb->DET_YSCL = 0.0;
    s_mdb->DET_XNPIX = 0.0;
    s_mdb->DET_YNPIX = 0.0;
    s_mdb->HEA_XPIXSIZ = 0.0;
    s_mdb->HEA_YPIXSIZ = 0.0;
    s_mdb->HEA_XNPIX = 0.0;
    s_mdb->HEA_YNPIX = 0.0;
    s_mdb->SKY_XNPIX = 0.0;
    s_mdb->SKY_YNPIX = 0.0;
    s_mdb->FOC_XPIXSIZ = 0.0;
    s_mdb->FOC_YPIXSIZ = 0.0;
    s_mdb->FOC_XNPIX = 0.0;
    s_mdb->FOC_YNPIX = 0.0;
    s_mdb->OPTAXIS_FOCX = 0.0;
    s_mdb->OPTAXIS_FOCY = 0.0;
    s_mdb->AIM_FOCX = 0.0;
    s_mdb->AIM_FOCY = 0.0;
    s_mdb->FOC_ROTD = 0.0;
    s_mdb->FOC_XOFF = 0.0;
    s_mdb->FOC_YOFF = 0.0;

    /* MDB derived parameters: */
    s_mdb->plate_scale = 0.0;                  /* plate scale in degree/mm */
    s_mdb->cdltx = 0.0;                        /* x-pixel size in degrees */
    s_mdb->cdlty = 0.0;                        /* y-pixel size in degress */
    s_mdb->crpxx = 0.0;                        /* reference pixel in x */
    s_mdb->crpxy = 0.0;                        /* reference pixel in y */
    s_mdb->xmin = 0.0;
    s_mdb->xmax = 0.0;
    s_mdb->ymin = 0.0;
    s_mdb->ymax = 0.0;       /* s_mdb->number of pixels max/min */
    s_mdb->psf_fwhm = 0.0;                     /* Gaussian approx PSF FWHM in arcsec */
    s_mdb->cdltx_sky = 0.0;
    s_mdb->cdlty_sky = 0.0;
    s_mdb->crpxx_sky = 0.0;
    s_mdb->crpxy_sky = 0.0;
    s_mdb->resampx = 0.0;
    s_mdb->resampy = 0.0;
    s_mdb->cdltx_foc = 0.0;
    s_mdb->cdlty_foc = 0.0;
    s_mdb->crpxx_foc = 0.0;
    s_mdb->crpxy_foc = 0.0;
    s_mdb->xmin_foc = 0.0;
    s_mdb->xmax_foc = 0.0;
    s_mdb->ymin_foc = 0.0;
    s_mdb->ymax_foc = 0.0;
    s_mdb->FOC_ROTD_sin = 0.0;
    s_mdb->FOC_ROTD_cos = 0.0;
    s_mdb->crpxx_det = 0.0;
    s_mdb->crpxy_det = 0.0;
    s_mdb->xmin_det = 0.0;
    s_mdb->xmax_det = 0.0;
    s_mdb->ymin_det = 0.0;
    s_mdb->ymax_det = 0.0;
    s_mdb->xmin_sky = 0.0;
    s_mdb->xmax_sky = 0.0;
    s_mdb->ymin_sky = 0.0;
    s_mdb->ymax_sky = 0.0;
    s_mdb->cdltx_det = 0.0;
    s_mdb->cdlty_det = 0.0;
    strcpy(s_mdb->instmap_name,"");
    s_mdb->instmap_flag = 0;
    s_mdb->nx_imap = 0;
    s_mdb->ny_imap = 0;
    /*long * array_imap; */      /* can't initialize arrays yet */

    /* Vignette Params */
    strcpy(s_vig->extname,"");                /* extension name */
    s_vig->vigtype = 0;                 /* vignette file type */
    strcpy(s_vig->type_desc,"");              /* character type description */
    s_vig->nrow = 0;                    /* number of rows in table */
    s_vig->ncol = 0;                    /* number of columns in table */
    s_vig->n_energy = 0;                /* number of energies (usually = ncol-1) */
    /* double ** vig_matrix; */         /* the ncol-1 x nrow matrix of vignette data */
    /* double * energy_vec; */          /* array of energies, in keV */
    /* double * angle_vec; */           /* array of angles, in arcmin */
    /* double ** vig_immat; */          /* image matrix */
    s_vig->img_nx = 0;                  /* x dimension of image matrix */
    s_vig->img_ny = 0;                  /* y dimension of image matrix */
    s_vig->crvl1 = 0.0;                 /* image x-axis reference pixel */
    s_vig->crvl2 = 0.0;                 /* image y-axis reference pixel */
    s_vig->crpx1 = 0.0;                 /* image x-axis reference pixel coord */
    s_vig->crpx2 = 0.0;                 /* image y-axis reference pixel coord */
    s_vig->cdlt1 = 0.0;                 /* x pixel increment */
    s_vig->cdlt2 = 0.0;                 /* y pixel increment */

    /* PSF Params */
    s_psf->psftype = 0;                  /* psf file type */
    strcpy(s_psf->type_desc,"");  /* character type description */
    s_psf->ncol = 0;                    /* number of columns in table */
    s_psf->nrow = 0;                    /* number of rows in table */
    s_psf->n_eef = 0;                   /* number of energies */
    s_psf->n_radii = 0;                 /* number of radii */
    /* double * energy_vec; */          /* array of energies, in keV */
    /* double * angle_vec; */          /* array of angles, in arcmin */
    /* double * radii; */              /* array of radii */
    /* double * azim_vec; */            /* array of azimuth data */
    /* double ** eef_matrix; */         /* ncol-1 x nrow matrix of psf data */

    s_psf_image->nimages = 0;

    /* Burst params */
    btab->xdim = 1;   /* there exists a xdim-1 as a possible array index */
    btab->ydim = 0;
    btab->zdim = 0; 

}

int make_burst_tabarray(double exposure,
                        int nsource_new,
                        double * ratburst_new,
                        double * tburst_new,
                        double * trise_new,
                        double * tdecay_new,
                        int * burst_tabidx_new,
                        int nburst_new,
                        int ntable,
                        burst_table_struct * btab){

    int errstatus = 0;

    btab->xdim = ntable+1;
    btab->ydim = 2;
    btab->zdim = nburst_new;

    /* allocate the table */
    btab->burst_table = (double ***)calloc(btab->xdim, sizeof(double **));
    for (int ii=0; ii<btab->xdim; ii++){
        btab->burst_table[ii] = (double **)calloc(btab->ydim, sizeof(double *));
        for (int jj=0; jj<btab->ydim; jj++){
            btab->burst_table[ii][jj] = (double *)calloc(btab->zdim, sizeof(double));
        }
    }

    /* We'll also save the burst_tabidx as part of the structure */
    btab->burst_tabidx = calloc(nsource_new,sizeof(int));

    for (int isource=0; isource<nsource_new; isource++){

        int iz = burst_tabidx_new[isource];
        btab->burst_tabidx[isource] = iz;

        if (iz > 0){
            
            printf("iz = %d   nburst_new = %d\n",iz,nburst_new);
            
            double * table_time = calloc( (ntable+1), sizeof(double));
            double * table_x = calloc( (ntable+1), sizeof(double));

            errstatus = burst_table(exposure, ratburst_new[isource], tburst_new[isource], trise_new[isource], tdecay_new[isource],
                                    ntable, table_time, table_x);

            for (int itable=0; itable<=ntable; itable++){
                btab->burst_table[itable][0][iz-1] = table_time[itable];
                btab->burst_table[itable][1][iz-1] = table_x[itable];
            } 

            free(table_time);
            free(table_x);

        } /* endif (iz > 0) */
    } /* endfor  (isource = 0; ii < nsource_new; isource++) */

    return 0;
}


int burst_table(double expose,
                double ratburst,
                double tburst,
                double trise,
                double tdecay,
                int ntable,
                double * table_time,
                double * table_x){

    double tpeak = tburst + trise; /* time of peak = beginning plus rise time */

    /* normalize timescales to exposure time to simplify computation */
    double tb_norm = tburst / expose;
    double tr_norm = trise / expose;
    double td_norm = tdecay / expose;
    double tp_norm = tpeak / expose;
    double tm_norm = 0.0;

    /* normalizations and factors */
    double norm0 = 1.0;
    double fac1 = 0.5 * ratburst  / tr_norm;
    double norm1 = 1.0 + fac1 * (1.0 - tb_norm) * (1.0 - tb_norm); 
    double fac2a = 0.5 * ratburst  * tr_norm;
    double fac2b = ratburst  * td_norm; 
    double norm2 = (1.0 + fac2a) + fac2b * (1.0 - exp(-((1-tp_norm)/td_norm)));

    double dtn0 = 1.0 / (double)ntable; /* mean (normalized) interval*/

    /* initialize: constant intervals*/
    int ibeg1 = 0;
    int iend1 = ntable;
    int ibeg2 = ntable;
    int iend2 = ntable;
    int ibeg3 = ntable;
    int iend3 = ntable;
    int ibeg4 = ntable;
    int iend4 = ntable;

    double dtn1 = dtn0;
    double dtn2 = 0.0;
    double dtn3 = 0.0;
    double dtn4 = 0.0;
    double dtn10 = 0.0;
    double dtn20 = 0.0;
    double dtn30 = 0.0;

    double tfac = 0.0;

    double n10 = 0.0;
    double n20 = 0.0;
    double n30 = 0.0;
    double n40 = 0.0;

    int n1 = 0;
    int n2 = 0;
    int n3 = 0;
    int n4 = 0;

    double norm = norm0;
    double * dt_table = 0;
 
    if (tp_norm >= 1.0)  {
        norm = norm1; /* decay outside of exposure; resolve risetime */
        tfac = (1.0-tb_norm) * dtn0;
        dtn10 = tb_norm * dtn0 + tfac * dtn0 / tr_norm;
        dtn20 = tb_norm * tr_norm + tfac;
        if (dtn20 > dtn0) dtn20 = dtn0;
        n10 = tb_norm / dtn10;
        n20 = (1.0-tb_norm) / dtn20;
        n1 = floor(n10);
        n2 = ntable - n1;
        dtn2 = dtn20;
        dtn1 = (1 - (double)n2*dtn20)/(double)n1;
        ibeg1 = 0;
        iend1 = n1;
        ibeg2 = n1;
        iend2 = ntable; 
    }  else {
        norm = norm2;
        tm_norm = tp_norm + td_norm * log(ratburst); /* natural log*/
        if (tm_norm < tp_norm) tm_norm = tp_norm;
        if (tm_norm > 1) { /* slow decay; resolve risetime */
            tfac = dtn0 + (1.0-tp_norm);
            dtn10 = tb_norm * dtn0 + dtn0 * dtn0 + tfac * dtn0 / td_norm;
            dtn20 = tb_norm * tr_norm + dtn0 * tr_norm + tfac * tr_norm / td_norm;
            dtn30 = tb_norm * td_norm + dtn0 * td_norm + tfac;
            if (dtn20 > dtn0) dtn20 = dtn0;
            if (dtn30 > dtn0) dtn30 = dtn0;
            n10 = tb_norm / dtn10;
            n20 = tr_norm / dtn20;
            n30 = (1.0-tp_norm) / dtn30;
            n1 = floor(n10);
            n3 = floor(n30);
            n2 = ntable - n1 - n3;
            dtn2 = dtn20;
            dtn3 = dtn30;
            dtn1 = (1 - (double)n2 * dtn20 - (double)n3 * dtn30) / (double)n1;
            ibeg1 = 0;
            iend1 = n1;
            ibeg2 = n1;
            iend2 = n1+n2;
            ibeg3 = n1+n2;
            iend3 = ntable;
            
        } else { /* resolve risetime and decay*/
            double ttemp = tb_norm + 1.0 - tm_norm;
            tfac = (tm_norm-tp_norm) * dtn0;
            dtn10 = ttemp * dtn0 + dtn0 * dtn0 + tfac * dtn0 / td_norm;
            dtn20 = ttemp * tr_norm + dtn0 * tr_norm + tfac * tr_norm / td_norm;
            dtn30 = ttemp * td_norm + dtn0 * td_norm + tfac;
            if (dtn20 > dtn0) dtn20 = dtn0;
            if (dtn30 > dtn0) dtn30 = dtn0;
            n10 = tb_norm / dtn10;
            n20 = tr_norm / dtn20;
            n30 = (tm_norm-tp_norm) / dtn30;
            n40 = (1.0-tm_norm) / dtn10;
            n1 = floor(n10);
            n4 = floor(n40);
            n3 = floor(n30);
            n2 = ntable - n1 - n3 - n4;
            dtn2 = dtn20;
            dtn3 = dtn30;
            dtn1 = (1 - (double)n2 * dtn20 - (double)n3 * dtn30) / (double)(n1+n4);
            dtn4 = dtn1;
            ibeg1 = 0;
            iend1 = n1;
            ibeg2 = n1;
            iend2 = n1+n2;
            ibeg3 = n1+n2;
            iend3 = n1+n2+n3;
            ibeg4 = n1+n2+n3;
            iend4 = ntable;

        } /* endif (tm_norm > 1) */
    } /* endif (tp_norm >= 1.0) */
    

    /* Populate dt_table */
    dt_table = calloc(ntable, sizeof(double));
    for (int ii=0; ii<ntable; ii++){
        if ( (ii >= ibeg1) && (ii < iend1) ){
            dt_table[ii] = dtn1;
        } else if ( (ii >= ibeg2) && (ii < iend2) ){
            dt_table[ii] = dtn2;
        } else if ( (ii >= ibeg3) && (ii < iend3) ){
            dt_table[ii] = dtn3;
        } else if ( (ii >= ibeg4) && (ii < iend4) ){
            dt_table[ii] = dtn4;
        } else {
            printf("ERROR: this case of dt_table population should not happen!\n");
            free(dt_table);
            return -1;
        }
    }

    /* lookup table, mapping uniform distribution (table_x) onto lightcurve (table_time) */
    /* table size is ntable+1 - have already been allocated and passed in */

    table_time[0] = 0.0;
    for (int itable=1; itable<=ntable; itable++){
        table_time[itable] = table_time[itable-1] + dt_table[itable-1];
        double tnorm = table_time[itable];
        if ((tnorm <= tb_norm) || (ratburst == 0.0)) { /* before the burst*/
            table_x[itable] = tnorm;
        } else {
            if (tnorm <= tp_norm) { /* before the peak*/
                table_x[itable] = tnorm + fac1*(tnorm - tb_norm)*(tnorm - tb_norm);
            } else {
                double facx = 1.0 - exp(-((tnorm-tp_norm)/td_norm));
                table_x[itable] = (tnorm+fac2a) + fac2b *facx;
            }
        }
        table_x[itable] /= norm;
    } /* endfor itable=0,ntable */

    free(dt_table);
    return 0;
}
    



/*
  ! $Log: initialize.c,v $
  ! Revision 1.92  2016/08/26 20:38:41  rshill
  ! (1) Deleted skiprmf parameter (use rmffile=none instead)
  ! (2) inserted setting ebounds_TLMIN1, ebounds_TLMAX2, and nebin_rmf at main
  ! level for rmffile=none.
  ! (3) smaller corrections from comparison to TRF.
  !
  ! Revision 1.91  2016/08/25 22:07:38  rshill
  ! Implemented skiprmf=yes and rmffile=none.
  !
  ! Revision 1.90  2016/03/31 21:41:40  driethmi
  ! Impemented suite of changes to handle burst time assignment.  (There will
  ! likely be more bug fixes to this implementation soon.)
  !
  ! Revision 1.89  2016/03/30 17:42:45  driethmi
  ! pulse_fraction should now be between zero and one.
  !
  ! Revision 1.88  2015/12/29 18:15:51  driethmi
  ! Added ability to skip field of view restriction and/or RMF implementation.
  ! User sets parameters skipfov and/or skiprmf, respectively, to yes/no.
  !
  ! Revision 1.87  2015/11/12 18:46:43  driethmi
  ! Added keywords TCUNI2 and TCUNI3, set both to 'deg'.
  !
  ! Revision 1.86  2015/11/12 18:42:09  driethmi
  ! Changed keywords TLMIN2, TLMAX2, TLMIN3, TLMAX3 from float to long.
  !
  ! Revision 1.85  2015/11/12 18:38:22  driethmi
  ! Removed "pixel" and "chan" from TUNIT{2,3,4} header keywords; these are not
  ! real unit values.
  !
  ! Revision 1.84  2015/11/12 18:13:54  driethmi
  ! MDB resampling ratios are redefinted with respect to FOC instead of internal
  ! heasim coordinates.
  !
  ! Revision 1.83  2015/11/12 18:07:42  driethmi
  ! If TLMIN1 is not found in background PHA file, then get it by reading the
  ! first CHANNEL column entry and assign it to ib_firstchan.
  !
  ! Revision 1.82  2015/11/09 14:44:07  driethmi
  ! Now check for first channel number in background file - usually 0, but
  ! use ib_firstchan to be safe.  If ib_firstchan != ebounds_TLMIN1, then
  ! exit with error.
  !
  ! Revision 1.81  2015/11/03 18:22:25  driethmi
  ! Added capability for resampling pixel size.
  !
  ! Revision 1.80  2015/09/30 15:45:24  driethmi
  ! Modified read_internal_background to process XMM BACKSCAL values in order to
  ! match units that heasim expects.
  !
  ! Revision 1.79  2015/07/01 18:46:34  driethmi
  ! Cleaned up checksum writing, so this happens at the end of the code.  Heasim
  ! output now passes ftverify check.
  !
  ! Revision 1.78  2015/06/30 14:02:39  driethmi
  ! Inserted flag flagsubex to trigger subexposure divisions - hidden, "no"
  ! by default.
  !
  ! Revision 1.77  2015/06/25 18:52:29  driethmi
  ! Changes to FITS keywords.
  !
  ! Revision 1.76  2015/06/17 19:15:50  driethmi
  ! Corrected a counting typo in read_psf and read_vig, related to the fact
  ! that HDUs start numbering at 1, not 0.
  !
  ! Revision 1.75  2015/06/05 21:21:19  driethmi
  ! Added "subexposure" as a parameter to the par file, and modified code to use
  ! it.  Now user specifies the total exposure time ("exposure"), and the sub-
  ! exposure time ("subexposure"), and the code divides the simulation into
  ! the appropriate sub-blocks, adding the final time assignment from the previous
  ! block to the time events in the next block.
  !
  ! Revision 1.74  2015/06/03 17:26:16  driethmi
  ! Made vignette and psf column name reading more robust.
  !
  ! Revision 1.73  2015/06/02 17:01:14  driethmi
  ! On some architectures, strings have a termination character, so allocated
  ! strings should account for this.  Changed colname[6] to colname[10], to be
  ! safe.
  !
  ! Revision 1.72  2015/05/29 17:45:38  driethmi
  ! Corrected loop in psf_read, ii<s_psf->ncol, instead of ii<=s_psf->ncol.  Also
  ! added block that writes the entire s_mdb structure to ascii file in the case
  ! debug=yes.
  !
  ! Revision 1.71  2015/05/27 14:56:32  driethmi
  ! Applied same TYPE2/TYPE1 catch as read_psf into read_vignette; now type 2
  ! FITS files with only one HDU extension will be treated as type 1.
  !
  ! Revision 1.70  2015/05/27 14:16:06  driethmi
  ! Changes to read_psf.  In FITS1 and FITS2 format, we look for PSFRAD and EEF
  ! keywords, and also dump out ASCII file mimicing FITS info, if debug is set.
  ! Also found case where FITS2 should be treated as FITS1, in the event where
  ! only one HDU extension is present.
  !
  ! Revision 1.69  2015/05/26 18:55:27  driethmi
  ! Changes to read_vignette.  In FITS1 and FITS2 format, we look for OFFAXIS
  ! and VIG### keywords, and also dump out ASCII file mimicing FITS info, if
  ! debug is set.
  !
  ! Revision 1.68  2015/05/22 17:24:36  driethmi
  ! Updated parameter names throughout code, changed header keywords/comments
  ! to match standard values.  Changed input user seed so that seed=0 triggers
  ! seeding from the system time.
  !
  ! Revision 1.67  2015/05/20 20:12:20  driethmi
  ! Modified mdb parameter names, and instances of these names in the code.  Also
  ! have updated values for mdb parameters.
  !
  ! Revision 1.66  2015/05/07 14:17:55  driethmi
  ! Modified block for copy sample input file to local directory, such that
  ! heasim_source_sample.txt is retrieved from the $LHEA_DATA refdata directory.
  !
  ! Revision 1.65  2015/05/04 17:07:33  driethmi
  ! Reverted back one version to fix bugs.
  !
  ! Revision 1.63  2015/05/04 16:16:33  driethmi
  ! Implemented better variable initializations.
  !
  ! Revision 1.62  2015/04/28 15:42:51  driethmi
  ! Fixed some variable declarations to be initialized immediately, i.e.
  ! instead of "double xyz;" now have "double xyz=0.0".
  !
  ! Revision 1.61  2015/04/08 19:09:12  driethmi
  ! Replaced calls to stat() with fits_file_exists(), which is more stable.
  ! This routine will report successful location, even in the file is not a fits
  ! file.
  !
  ! Revision 1.60  2015/04/07 15:47:49  driethmi
  ! Corrected non-critical compilations warnings; unused variables, etc.
  !
  ! Revision 1.59  2015/04/06 14:42:49  driethmi
  ! TSTART and TSTOP should be 0 and exposure time, respectively.  Also, we now
  ! set MJDREFI with the current Modified Julian Day and MJDREFF with the current
  ! Modified Julian time fraction.
  !
  ! Revision 1.58  2015/04/01 17:57:51  driethmi
  ! Modified setup_output() to set TSTART and TSTOP according to the current
  ! date & time.
  !
  ! Revision 1.57  2015/04/01 17:33:46  driethmi
  ! Modified setup_output() to use the current time as the observation start time,
  ! and set the exposure end date X number of seconds after the current time.
  ! Changed local time stamping to use UTC instead.
  !
  ! Revision 1.56  2015/03/26 19:41:03  driethmi
  ! Removed output fits file column header units for energy_in, flag_pileup,
  ! changed pi channel from "channel" to "chan".
  !
  ! Revision 1.55  2015/03/25 21:10:03  driethmi
  ! Changed read_instmap() to only report confirmation of existing map files
  ! if debug = 1.
  !
  ! Revision 1.54  2015/03/25 20:59:54  driethmi
  ! Corrected error in read_instmap() routine - check for map file existance was
  ! not being executed correctly.
  !
  ! Revision 1.53  2015/03/24 19:09:05  driethmi
  ! Added a check in read_instmap to ensure that the instrument map file actually
  ! exists before proceeding.
  !
  ! Revision 1.52  2015/03/24 14:16:21  driethmi
  ! Corrected mistake in timeset_burst - dtn4 = dtn1 should happen AFTER dtn1
  ! is redefined.  Also added validation test to ensure that tburst >= 0.
  !
  ! Revision 1.51  2015/03/19 20:35:30  driethmi
  ! Added burst capability, and updated function comment blocks.
  !
  ! Revision 1.50  2015/02/18 21:34:07  driethmi
  ! Corrected bug in process_image, needed to use fits_read_key_dbl instead of
  ! fits_read_key_flt - lack of precision was causing zeroes to be read.  Also
  ! improved verbosity of debug statements.
  !
  ! Revision 1.49  2015/02/18 18:52:51  driethmi
  ! Improved diagnostic output, restricted more output to debug == 1.
  !
  ! Revision 1.48  2015/02/18 15:53:41  driethmi
  ! Preliminary redshift changes, and cleaned up output chatter so debug = 1
  ! is more useful.
  !
  ! Revision 1.47  2015/02/02 19:48:37  driethmi
  ! Changed > to >= since type 0 is now valid.
  !
  ! Revision 1.46  2015/01/29 19:53:31  driethmi
  ! Removed difspec_file_background parameter.
  !
  ! Revision 1.45  2015/01/29 00:49:57  driethmi
  ! Heasim now complains and exists if it can't find input source files or
  ! skyback files.
  !
  ! Revision 1.44  2015/01/28 21:32:20  driethmi
  ! Enabled torus spectral model for background point sources.
  !
  ! Revision 1.43  2015/01/21 17:25:03  driethmi
  ! Modified heasim to accept data files from sky background tool.
  !
  ! Revision 1.42  2014/12/05 19:41:56  driethmi
  ! Added checks to confirm that user-supplied ARF and RMF files actually
  ! have the correct format.
  !
  ! Revision 1.41  2014/12/02 19:57:40  driethmi
  ! Changed floats to doubles for consistency, except where float is required.
  !
  ! Revision 1.40  2014/10/29 18:53:35  driethmi
  ! Corrected bug that passed user input seed out of initialize incorrectly.
  !
  ! Revision 1.39  2014/10/28 20:55:40  driethmi
  ! Replaced system call to remove clobbered files with native C routine "remove".
  !
  ! Revision 1.38  2014/09/30 20:46:14  driethmi
  ! Removed external system calls in getPars routine.  Now copy the sample input
  ! file via native C functions.
  !
  ! Revision 1.37  2014/09/05 19:34:36  driethmi
  ! Updated comment blocks before each function to reflect most recent versions.
  !
  ! Revision 1.36  2014/08/27 14:40:59  driethmi
  ! Changed order of getPars parameters to be read in the same order as listed
  ! in the par file.  Also changed hardwired array numbering to incremented
  ! index as read.
  !
  ! Revision 1.35  2014/08/19 18:30:41  driethmi
  ! Added initial capability to read PSF images correctly.
  !
  ! Revision 1.34  2014/07/30 19:03:31  driethmi
  ! Promoted s_mdb pixelsize_* and cdlt* from double to double precision, to
  ! accomodate values in the mdb file.
  !
  ! Revision 1.33  2014/07/28 18:51:02  driethmi
  ! In setup_output, needed longer string length for tmp_comment, etc.
  !
  ! Revision 1.32  2014/07/25 18:10:41  driethmi
  ! Minor changes.
  !
  ! Revision 1.31  2014/07/24 20:08:01  driethmi
  ! Modified fits output file creation to have 7 columns instead of five: added
  ! column for pixel ID and for Pileup status flag.
  !
  ! Revision 1.30  2014/07/10 22:35:27  driethmi
  ! Minor changes, to make things more general.
  !
  ! Revision 1.29  2014/07/07 15:36:55  driethmi
  ! Added initialization for several more s_mdb parameters.
  !
  ! Revision 1.28  2014/07/01 13:47:16  driethmi
  ! Current state - added roll angle and image HDU capability.  Still some
  ! issues to work out with response file and background read.  Also, imagedis
  ! seems to have execution bottleneck.
  !
  ! Revision 1.27  2014/06/25 18:52:47  driethmi
  ! Added capability to specify HDU header for psf and vig image files.
  !
  ! Revision 1.26  2014/06/25 18:05:51  driethmi
  ! Type in comments only - no change in active code.
  !
  ! Revision 1.25  2014/06/02 18:54:41  driethmi
  ! Added switch to use legacy C heasp library instead of active C++ library
  ! from heacore/heasp.
  !
  ! Revision 1.24  2014/05/23 21:51:38  driethmi
  ! Changed random number generator such that MTstate is passed down through
  ! each function using it.  Now, user must specify seed.  If seed > 0, it is
  ! used to seed the RNG.  If seed <=0, we seed using the system time.
  !
  ! Revision 1.23  2014/04/30 17:03:44  driethmi
  ! Changed the getinfile option such that it copies
  ! source_data/heasim_source_sample.txt to the current working directory,
  ! uses it as the source infile, and overwrites the previously set
  ! RA and Dec, to ensure that the sample sources are within the field of view.
  !
  ! Revision 1.22  2014/04/08 13:59:06  driethmi
  ! Corrected typo - ENG001 to ENERGY
  !
  ! Revision 1.21  2014/04/03 20:41:06  driethmi
  ! Updated commenting and minor changes to attempt to comply more with coding
  ! standards.  Added doxygen tags.
  !
  ! Revision 1.20  2014/04/02 20:47:43  driethmi
  ! Finished psf and vignetting functions, moved read_vig and read_psf into
  ! initialize.c, moved apply_vig and apply_psf into doWork.c.  Removed
  ! vig.c and psf.c from repository.  Edited Makefile to not build
  ! vig.c and psf.c
  !
  ! Revision 1.19  2014/04/02 14:54:18  driethmi
  ! PSF filename is allowed to be "GAUSSIAN" and will still pass the check
  ! for files exist.
  !
  ! Revision 1.18  2014/03/27 14:20:04  driethmi
  ! Added handling for special case in beta model where beta = 0.5
  !
  ! Revision 1.17  2014/03/19 14:19:45  driethmi
  ! Removed all instances of idum and random_seed, which are now obsolete.
  ! Also reworked the delE block of code so that the MIN macro is no longer
  ! necessary - makes the compiler happy.
  !
  ! Revision 1.16  2014/03/10 21:53:25  driethmi
  ! Changed mallocs to callocs, misc print statements.
  !
  ! Revision 1.15  2014/02/28 19:08:21  driethmi
  ! 1) Fixed many small memory leaks in C code; however, Valgrind still reports some
  ! "still reachable" blocks in C++ interface code.
  !
  ! 2) Added parentheses around (*ifilename) and (*sfilename) in doWork.c, that avoids
  ! a bus error.
  !
  ! 3) doWork() and spectra() now take a debug flag as an argument.
  !
  ! 4) doWork() now sorts the avenergy array, and then de_half and inspec are reordered
  ! to match, before populating binlow and binhigh.
  !
  ! Revision 1.14  2014/02/21 21:19:56  driethmi
  ! If EBOUNDS has no TLMIN1 or TLMAX1 keywords, get them from the CHANNEL first
  ! and last values.
  !
  ! Revision 1.13  2014/02/21 15:54:42  driethmi
  ! Fixed bug to pass ebounds_TLMIN1 and TLMAX1 correctly.
  !
  ! Revision 1.12  2014/02/20 21:12:35  driethmi
  ! Fixed issues with the TLMIN5/TLMAX5 header keyword.  Now duplicates from the
  ! EBOUNDS TLMIN1/TLMAX1 keyword.
  !
  ! Revision 1.11  2014/02/20 18:30:48  driethmi
  ! Moved the doWork section in heasim.c main to its own function in doWork.c.
  ! Also moved C_ReturnChannel() to doWork.c and removed the ReturnChannel.c
  ! file from the repository.
  !
  ! Revision 1.10  2014/02/07 15:58:58  driethmi
  ! Algorithm differences, comments.
  !
  ! Revision 1.9  2014/01/16 22:46:41  driethmi
  ! Inserted a debug option into read_ARF, defined D2R (degrees to radian)
  ! in heasim.h instead of defining locally.
  !
  ! Revision 1.8  2014/01/14 21:43:19  driethmi
  ! Moved deallocate_source_data() to finalize.c, which is newly added.
  !
  ! Revision 1.7  2014/01/14 16:47:53  driethmi
  ! Validation check on ellipse and beta models: ellipticity required to
  ! be >= 0, not > 0.
  !
  ! Revision 1.6  2014/01/13 22:08:56  driethmi
  ! Minor changes for consistency - made all "return" values which signify
  ! a failure to -1.
  !
  ! Revision 1.5  2014/01/13 22:01:59  driethmi
  ! Made skycoords_per_arcsec_x and _y.  Code now also exits on failure
  ! of ARF/RMF compatibility check.
  !
  ! Revision 1.4  2014/01/13 14:49:38  driethmi
  ! Fixed some bugs in print statements, correcting the number of arguments.
  !
  ! Revision 1.3  2014/01/10 22:29:36  driethmi
  ! Began adding a battery of validation tests and other behavioral changes
  ! based on list of bugs found.
  !
  ! Revision 1.2  2014/01/09 16:16:34  driethmi
  ! Forgot to delete duplicate blocks of "include" statements after concatentation.
  !
  ! Revision 1.1  2014/01/09 16:11:34  driethmi
  ! Concatenated heasp_read.c, read_source_data.c, setup_output.c, and several
  ! other "initialize" functions previously in heasim.c into a new file called
  ! initialize.c.  Figured this would be more consistent with TRF.
  !
*/

