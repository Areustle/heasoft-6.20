/**
 * \file psftool.c
 * \brief Main file for vigtools
 * \author David Riethmiller
 * \date $Date: 2014/04/04 15:41:15 $
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>
#include "Cheasp.h"       /* C-interface to C++ HEASP library */
#include "fitsio.h"       /* cfitsio defined constants */
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "psftool.h"

int main(int argc, char *argv[]){
    fitsfile * ounit = NULL;                      /* FITS file unit */
    FILE *fp = NULL;                              /* ASCII file unit */
    char file_psf[STR_MAX_LEN];                   /* ASCII file name */
    char file_psf_fits[STR_MAX_LEN];              /* FITS file name */
    char line[STR_MAX_LEN];                       /* string line buffer */
    char templine[STR_MAX_LEN];                   /* temporary string line */
    char *key=NULL, *val=NULL, *comment=NULL;     /* string keyword, value, and comment */
    PSF_struct s_psf;                             /* structure to hold psf data */
    int len=0;                                    /* string length */
    int ii=0;                                     /* loop index */
    int mat_counter=0;                            /* matrix dimension */
    int n1flag=0, n2flag=0, extflag=0, tfflag=0;  /* int flags that keywords are found */
    int tf_holder=0, n1_holder=0;                 /* temp holders for NAXIS1 and TFIELDS vals */
    int fitstype=0;                               /* type of fits file to create */
    int fstat=0;                                  /* status flag for success/failure of fits operations */


    /* User must enter correct number of arguments */
    if (argc != 3){
	printf("\nCALLING SEQUENCE:  psf_ascii2fits <ascii filename> <fits type (1 or 2)>\n\n");
	return 0;
    }

    /* get the input file name */
    strcpy(file_psf,argv[1]);
    fitstype = atoi(argv[2]);
    printf("\nInput ASCII file = %s\n",file_psf);

    /* set the output file name */
    strcpy(file_psf_fits,file_psf);
    remove_substring(file_psf_fits,".txt");
    remove_substring(file_psf_fits,".dat");
    sprintf(file_psf_fits,"%s_type%d.fits",file_psf_fits,fitstype);
    printf("Output FITS file (type %d) = %s\n\n",fitstype, file_psf_fits);

    /* if the output file already exists, delete it */

    fits_open_file(&ounit,file_psf_fits, READONLY, &fstat);
    if (fstat == 0){
        fits_close_file(ounit,&fstat);
	remove(file_psf_fits);
    } else
	fstat = 0;

    fp = fopen(file_psf,"r");

    s_psf.n_asciilines=0;

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

	    if (0 != strcasecmp(key,"END")) 
		(s_psf.n_asciilines)++;
	    
            if (0 == strcasecmp(key,"NAXIS1")){
                n1_holder = atoi(val);
                n1flag = 1;
            } else if (0 == strcasecmp(key,"NAXIS2")){
                s_psf.nrow = atoi(val);
                n2flag = 1;
            } else if (0 == strcasecmp(key,"EXTNAME")){
                remove_substring(val,"'");
                strcpy(s_psf.extname, val);
                extflag = 1;
            } else if (0 == strcasecmp(key,"TFIELDS")){
                tf_holder = atoi(val);
                tfflag = 1;
	    }
        }
    } while(0 != strcasecmp(val,"END"));
    fclose(fp);

    if ( (n1flag != 1) || (n2flag != 1) || (extflag != 1) ){
        printf("PSF ERROR: could not find correct matrix dimension keywords!\n");
        return -1;
    } else {
	if (tfflag == 1){
	    s_psf.ncol = tf_holder;
	} else
	    s_psf.ncol = n1_holder;
	
        s_psf.psftype = 4;
        strcpy(s_psf.type_desc,"PSF_TEXT_1");
        printf("NCOL: %d  NROW: %d  EXTNAME: %s\n",s_psf.ncol,s_psf.nrow,s_psf.extname);

	s_psf.n_eef = s_psf.ncol-1;
	s_psf.n_radii = s_psf.nrow;

        /* Allocate the psf matrix and arrays */
        s_psf.eef_matrix = (double **)calloc(s_psf.n_eef, sizeof(double *));
        for (ii=0; ii<s_psf.n_eef; ii++)
            s_psf.eef_matrix[ii] = (double *)calloc(s_psf.n_radii,sizeof(double));

	s_psf.radii = calloc(s_psf.nrow, sizeof(double));

	s_psf.col_unit = calloc(s_psf.ncol, sizeof(char *));
	s_psf.col_type = calloc(s_psf.ncol, sizeof(char *));
	s_psf.col_form = calloc(s_psf.ncol, sizeof(char *));
	for (ii=0; ii<s_psf.ncol; ii++){
	    s_psf.col_unit[ii] = calloc(20, sizeof(char *));
	    s_psf.col_type[ii] = calloc(20, sizeof(char *));
	    s_psf.col_form[ii] = calloc(20, sizeof(char *));
	}

        s_psf.energy_vec = calloc(s_psf.n_eef, sizeof(double));
        s_psf.angle_vec = calloc(s_psf.n_eef, sizeof(double));

	s_psf.key = calloc(s_psf.n_asciilines, sizeof(char *));
	s_psf.val = calloc(s_psf.n_asciilines, sizeof(char *));
	s_psf.comm = calloc(s_psf.n_asciilines, sizeof(char *));
	for (ii=0; ii<s_psf.n_asciilines; ii++){
	    s_psf.key[ii] = calloc(STR_MAX_LEN, sizeof(char));
	    s_psf.val[ii] = calloc(STR_MAX_LEN, sizeof(char));
	    s_psf.comm[ii] = calloc(STR_MAX_LEN, sizeof(char));
	}
	
        /* open the file again to start reading */
        fp = fopen(file_psf,"r");
	
	ii=0;
	
        /* read all keywords, values, and comments */
        while ( ( fgets(line, sizeof(line), fp) != NULL) ){
            len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;
            if (0 == strcasecmp(line,"END")) break;

            if (strlen(line) != 0){
                key = strtok(line,"=");
                val = strtok(NULL,"/");
		comment = strtok(NULL,"/");

		remove_substring(val,"'");
		remove_substring(val," ");
		remove_substring(key," ");

		strcpy(s_psf.key[ii],key);
		strcpy(s_psf.val[ii],val);
		strcpy(s_psf.comm[ii],comment);
		
		ii++;
	    }
	}
	
	for (ii=0; ii<s_psf.n_asciilines; ii++){
	    strcpy(templine,s_psf.key[ii]);

	    if (0 == strncasecmp(templine,"ENG",3)){
		remove_substring(templine,"ENG");
		s_psf.energy_vec[atoi(templine)-1] = (double)atof(s_psf.val[ii]);

	    } else if (0 == strncasecmp(templine,"OFF",3)){
                remove_substring(templine,"OFF");
                s_psf.angle_vec[atoi(templine)-1] = (double)atof(s_psf.val[ii]);

	    } else if (0 == strncasecmp(templine,"TUNIT",5)){
		remove_substring(templine,"TUNIT");
		strcpy(s_psf.col_unit[atoi(templine)-1], s_psf.val[ii]);

	    } else if (0 == strncasecmp(templine,"TFORM",5)){
                remove_substring(templine,"TFORM");
                strcpy(s_psf.col_form[atoi(templine)-1], s_psf.val[ii]);
		
            } else if (0 == strncasecmp(templine,"TTYPE",5)){
                remove_substring(templine,"TTYPE");
                strcpy(s_psf.col_type[atoi(templine)-1], s_psf.val[ii]);
	    }

	}

        for (ii=0; ii<s_psf.ncol; ii++)
            if (0 == strcasecmp(s_psf.col_unit[ii],"")){
                free(s_psf.col_unit[ii]);
                s_psf.col_unit[ii] = "unitless";
            }


	/* now read in the matrix data */
	mat_counter=0;
	while ( fgets(line, sizeof(line), fp) != NULL){
	    len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;
	    
	    /* if we're not dealing with an empty line... */
	    if (strlen(line) > 0){
		
		/* first column is radius data */
		val = strtok(line," ");
		s_psf.radii[mat_counter] = (double)atof(val);
		
		for (ii=0; ii<s_psf.ncol-1; ii++){
		    val = strtok(NULL," ");
		    s_psf.eef_matrix[ii][mat_counter] = (double)atof(val);
		}
		mat_counter++;
	    }
	}
	
        fclose(fp);
    }


    if (fitstype == 1) {
	write_fits1(&s_psf, file_psf_fits);
    } else if (fitstype == 2) {
	write_fits2(&s_psf, file_psf_fits);
    } else
	printf("ERROR: FITS type must be 1 or 2\n");


    /* clean up */
    for (ii=0; ii<s_psf.n_eef; ii++)
	free(s_psf.eef_matrix[ii]);
    free(s_psf.eef_matrix);
    
    free(s_psf.radii);

    for (ii=0; ii<s_psf.ncol; ii++){
	if (0 != strcasecmp(s_psf.col_unit[ii],"unitless")) free(s_psf.col_unit[ii]);
	free(s_psf.col_type[ii]);
	free(s_psf.col_form[ii]);
    }
    free(s_psf.col_unit);
    free(s_psf.col_type);
    free(s_psf.col_form);

    free(s_psf.energy_vec);
    free(s_psf.angle_vec);

    for (ii=0; ii<s_psf.n_asciilines; ii++){
	free(s_psf.key[ii]);
	free(s_psf.val[ii]);
	free(s_psf.comm[ii]);
    }
    free(s_psf.key);
    free(s_psf.val);
    free(s_psf.comm);

    return 0;
}


/* FUNCTION NAME:  write_fits1              */
/*                                          */
/* PURPOSE: write fits type 1 file          */
/*                                          */
/* INPUTS: s_psf, file_psf_fits             */
/*                                          */
/* OUTPUTS: writes fits file                */
/*                                          */

void write_fits1(PSF_struct * s_psf,     /* structure that holds psf data */
		 char * file_psf_fits){  /* fits file name */

    int fstat = 0;               /* status flag for success/failure of fits operations */
    int ii=0, jj=0;              /* loop index */
    fitsfile * ounit = NULL;     /* fits file unit */
    int colnum;                  /* column number */
    int firstrow;                /* first row in fits file */
    int firstelem;               /* first element in fits file */
    double * tempvec;            /* temporary vector holder */

    printf("Creating type PSF_FITS_1 file.\n");

    /* create the file, insert a binary table, and move to the correct extension name */
    fits_create_file(&ounit, file_psf_fits, &fstat);
    fits_insert_btbl(ounit, s_psf->nrow, s_psf->ncol, s_psf->col_type, s_psf->col_form, s_psf->col_unit,
		     "EEF", 0, &fstat);
    fits_movnam_hdu(ounit, BINARY_TBL, "EEF", 0, &fstat);


    /* For each line in the ASCII file write/modify keywords in output fits file */
    for (ii=0; ii<s_psf->n_asciilines; ii++){
	
	/* first try to modify existing keyword */
	if (0 == strncasecmp(s_psf->key[ii],"ENG",3)){
	    fits_modify_key_flt(ounit,s_psf->key[ii],(float)atof(s_psf->val[ii]),3,s_psf->comm[ii],&fstat);
	    
	} else if (0 == strncasecmp(s_psf->key[ii],"OFF",3)){
	    fits_modify_key_flt(ounit, s_psf->key[ii], (float)atof(s_psf->val[ii]), 3, s_psf->comm[ii], &fstat);
	    
	} else if (0 == strncasecmp(s_psf->key[ii],"TUNIT",5)){
	    fits_modify_key_str(ounit,s_psf->key[ii],s_psf->val[ii],s_psf->comm[ii],&fstat);
	    
	} else if (0 == strncasecmp(s_psf->key[ii],"TELES",5)){
            fits_modify_key_str(ounit,s_psf->key[ii],s_psf->val[ii],s_psf->comm[ii],&fstat);

	} else if (0 == strncasecmp(s_psf->key[ii],"INSTR",5)){
            fits_modify_key_str(ounit,s_psf->key[ii],s_psf->val[ii],s_psf->comm[ii],&fstat);

        } else if (0 == strncasecmp(s_psf->key[ii],"EXTNA",5)){
            fits_modify_key_str(ounit,s_psf->key[ii],s_psf->val[ii],s_psf->comm[ii],&fstat);

	}

	if (fstat != 0){  /* if keyword doesn't yet exist, create it */
	    fstat=0;
	    if (0 == strncasecmp(s_psf->key[ii],"ENG",3)){
		fits_write_key_flt(ounit,s_psf->key[ii],(float)atof(s_psf->val[ii]),3,s_psf->comm[ii],&fstat);
		
	    } else if (0 == strncasecmp(s_psf->key[ii],"OFF",3)){
		fits_write_key_flt(ounit,s_psf->key[ii],(float)atof(s_psf->val[ii]),3,s_psf->comm[ii],&fstat);
		
	    }  else if (0 == strncasecmp(s_psf->key[ii],"TUNIT",5)){
		fits_write_key_str(ounit,s_psf->key[ii],s_psf->val[ii],s_psf->comm[ii],&fstat);

	    } else if (0 == strncasecmp(s_psf->key[ii],"TELES",5)){
		fits_write_key_str(ounit,s_psf->key[ii],s_psf->val[ii],s_psf->comm[ii],&fstat);

	    } else if (0 == strncasecmp(s_psf->key[ii],"INSTR",5)){
		fits_write_key_str(ounit,s_psf->key[ii],s_psf->val[ii],s_psf->comm[ii],&fstat);

	    } else if (0 == strncasecmp(s_psf->key[ii],"EXTNA",5)){
		fits_write_key_str(ounit,s_psf->key[ii],s_psf->val[ii],s_psf->comm[ii],&fstat);
	    }
	    
	}
    }
    
    colnum=1;
    firstrow=1;
    firstelem=1;
    
    /* first column is the radius vector */
    fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, s_psf->nrow, s_psf->radii, &fstat);

    /* remaining columns are from psf matrix */
    for (ii=0; ii<s_psf->ncol-1; ii++){
	colnum++;
	tempvec = calloc(s_psf->nrow, sizeof(double));
	for (jj=0; jj<s_psf->nrow; jj++)
	    tempvec[jj] = s_psf->eef_matrix[ii][jj];
	fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, s_psf->nrow, tempvec, &fstat);
	free(tempvec);
    }
    
    /* close the fits file */
    fits_close_file(ounit,&fstat);


}


/* FUNCTION NAME:  write_fits2              */
/*                                          */
/* PURPOSE: write fits type 2 file          */
/*                                          */
/* INPUTS: s_psf, file_psf_fits             */
/*                                          */
/* OUTPUTS: writes fits file                */
/*                                          */


void write_fits2(PSF_struct * s_psf,     /* structure that contains psf data */
		 char * file_psf_fits){  /* fits file name */

    int fstat = 0;            /* status flag for success/failure of fits operations */
    int ii=0, jj=0;           /* loop index */
    fitsfile * ounit = NULL;  /* fits file unit */
    double * tempvec;         /* temporary holding vector */
    char * telescope;         /* telescope name */
    char * instrument;        /* instrument name */
    char ** col_type;         /* column name */
    char ** col_form;         /* column format */
    char ** col_unit;         /* column units */
    int n_hdu=0;              /* number of fits HDUs */
    int n_energy_cols;        /* number of energy columns */


    n_energy_cols = s_psf->ncol-1;

    col_type = calloc(2, sizeof(char *));
    col_form = calloc(2, sizeof(char *));
    col_unit = calloc(2, sizeof(char *));
    for (ii=0; ii<2; ii++){
	col_type[ii] = calloc(STR_MAX_LEN, sizeof(char));
	col_form[ii] = calloc(STR_MAX_LEN, sizeof(char));
	col_unit[ii] = calloc(STR_MAX_LEN, sizeof(char));
    }

    for (ii=0; ii<s_psf->n_asciilines; ii++){
	if (0 == strncasecmp(s_psf->key[ii],"TELES",5))
	    telescope = s_psf->val[ii];
	if (0 == strncasecmp(s_psf->key[ii],"INSTR",5))
            instrument = s_psf->val[ii];
    }

    strcpy(col_type[0],s_psf->col_type[0]);
    strcpy(col_form[0],s_psf->col_form[0]);
    strcpy(col_unit[0],s_psf->col_unit[0]);

    printf("Creating type PSF_FITS_2 file.\n");

    /* create the file, insert a binary table, and move to the correct extension name */
    fits_create_file(&ounit, file_psf_fits, &fstat);

    for (ii=1; ii<s_psf->ncol; ii++){
	strcpy(col_type[1],"ENGOFF");
	strcpy(col_form[1],s_psf->col_form[ii]);
	strcpy(col_unit[1],s_psf->col_unit[ii]);

	fits_insert_btbl(ounit, 1, 2, col_type, col_form, col_unit, "EEF", 0, &fstat);
	fits_write_key_str(ounit, "TELESCOP", telescope, "TELESCOPE NAME", &fstat);
	fits_write_key_str(ounit, "INSTRUME", instrument, "INSTRUMENT NAME", &fstat);
	fits_write_key_dbl(ounit, "ENERGY", s_psf->energy_vec[ii-1], 3, "ENERGY in keV", &fstat);
	fits_write_key_dbl(ounit, "OFFAXIS", s_psf->angle_vec[ii-1], 3, "OFF-AXIS ANGLE IN ARCMIN", &fstat);

	/* write the angle vector */
	fits_write_col(ounit, TDOUBLE, 1, 1, 1, s_psf->nrow, s_psf->radii, &fstat);
	tempvec = calloc(s_psf->nrow, sizeof(double));
	for (jj=0; jj<s_psf->nrow; jj++)
	    tempvec[jj] = s_psf->eef_matrix[ii-1][jj];
	fits_write_col(ounit, TDOUBLE, 2, 1, 1, s_psf->nrow, tempvec, &fstat);
	free(tempvec);


    }
    fits_close_file(ounit,&fstat);

    fits_open_file(&ounit, file_psf_fits, READONLY, &fstat);
    fits_get_num_hdus(ounit, &n_hdu, &fstat);
    printf("This file has %d HDU sections.\n",n_hdu);
    fits_close_file(ounit,&fstat);

    for (ii=0; ii<2; ii++){
        free(col_type[ii]);
        free(col_form[ii]);
        free(col_unit[ii]);
    }
    free(col_type);
    free(col_form);
    free(col_unit);
}



/* FUNCTION NAME: remove_substring                                   */
/*                                                                   */
/* CALLING SEQUENCE:                                                 */
/*    remove_substring(base,".fits");                                */
/*                                                                   */
/* PURPOSE: remove a substring from a string                         */
/*                                                                   */
/* INPUT: a string s, and a substring toremove                       */
/*                                                                   */
/* OUTPUT: the same string, without the substring                    */
/*                                                                   */
/* CALLED BY:                                                        */
/*   setup_output()                                                  */
/*                                                                   */

void remove_substring(char *s,                 /* the total input string */
                      const char *toremove){   /* substring to remove */
    while ( ( s=strstr(s,toremove) ) )
        memmove(s,s+strlen(toremove),1+strlen(s+strlen(toremove)));
}
