/**
 * \file vigtool.c
 * \brief Main file for vigtools
 * \author David Riethmiller
 * \date $Date: 2014/04/04 15:41:37 $
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
#include "vigtool.h"



int main(int argc, char *argv[]){
    fitsfile * ounit = NULL;                      /* FITS file unit */
    FILE *fp = NULL;                              /* ASCII file unit */
    char line[STR_MAX_LEN];                       /* string line buffer */
    char templine[STR_MAX_LEN];                   /* temporary string line */
    char *key=NULL, *val=NULL, *comment=NULL;     /* string keyword, value, and comment */
    char file_vignette[STR_MAX_LEN];              /* ASCII file name */
    char file_vig_fits[STR_MAX_LEN];              /* FITS file name */ 
    Vig_struct s_vig;                             /* structure to hold vig data */
    int len=0;                                    /* string length */
    int ii=0;                                     /* loop index */
    int mat_counter=0;                            /* matrix dimension */
    int n1flag=0, n2flag=0, extflag=0, tfflag=0;  /* int flags that keywords are found */
    int tf_holder=0, n1_holder=0;                 /* temp holders for NAXIS1 and TFIELDS vals */
    int fitstype=0;                               /* type of fits file to create */
    int fstat=0;                                  /* status flag for success/failure of fits operations */

    /* User must enter correct number of arguments */
    if (argc != 3){
	printf("\nCALLING SEQUENCE:  vig_ascii2fits <ascii filename> <fits type (1 or 2)>\n\n");
	return 0;
    }

    /* get the input file name */
    strcpy(file_vignette,argv[1]);
    fitstype = atoi(argv[2]);
    printf("\nInput ASCII file = %s\n",file_vignette);

    /* set the output file name */
    strcpy(file_vig_fits,file_vignette);
    remove_substring(file_vig_fits,".txt");
    remove_substring(file_vig_fits,".dat");
    sprintf(file_vig_fits,"%s_type%d.fits",file_vig_fits,fitstype);
    printf("Output FITS file (type %d) = %s\n\n",fitstype, file_vig_fits);

    /* if the output file already exists, delete it */

    fits_open_file(&ounit,file_vig_fits, READONLY, &fstat);
    if (fstat == 0){
        fits_close_file(ounit,&fstat);
	remove(file_vig_fits);
    } else
	fstat = 0;

    fp = fopen(file_vignette,"r");

    s_vig.n_asciilines=0;

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
		(s_vig.n_asciilines)++;
	    
            if (0 == strcasecmp(key,"NAXIS1")){
                n1_holder = atoi(val);
                n1flag = 1;
            } else if (0 == strcasecmp(key,"NAXIS2")){
                s_vig.nrow = atoi(val);
                n2flag = 1;
            } else if (0 == strcasecmp(key,"EXTNAME")){
                remove_substring(val,"'");
                strcpy(s_vig.extname, val);
                extflag = 1;
            } else if (0 == strcasecmp(key,"TFIELDS")){
                tf_holder = atoi(val);
                tfflag = 1;
	    }
        }
    } while(0 != strcasecmp(val,"END"));
    fclose(fp);

    if ( (n1flag != 1) || (n2flag != 1) || (extflag != 1) ){
        printf("VIG ERROR: could not find correct matrix dimension keywords!\n");
        return -1;
    } else {
	if (tfflag == 1){
	    s_vig.ncol = tf_holder;
	} else
	    s_vig.ncol = n1_holder;
	
        s_vig.vigtype = 4;
        strcpy(s_vig.type_desc,"VIG_TEXT_1");
        printf("NCOL: %d  NROW: %d  EXTNAME: %s\n",s_vig.ncol,s_vig.nrow,s_vig.extname);

        /* Allocate the vignette matrix and arrays */
        s_vig.vig_matrix = (double **)calloc(s_vig.ncol-1, sizeof(double *));
        for (ii=0; ii<s_vig.ncol-1; ii++)
            s_vig.vig_matrix[ii] = (double *)calloc(s_vig.nrow,sizeof(double));

	s_vig.col_unit = calloc(s_vig.ncol, sizeof(char *));
	s_vig.col_type = calloc(s_vig.ncol, sizeof(char *));
	s_vig.col_form = calloc(s_vig.ncol, sizeof(char *));
	for (ii=0; ii<s_vig.ncol; ii++){
	    s_vig.col_unit[ii] = calloc(20, sizeof(char *));
	    s_vig.col_type[ii] = calloc(20, sizeof(char *));
	    s_vig.col_form[ii] = calloc(20, sizeof(char *));
	}

        s_vig.energy_vec = calloc(s_vig.ncol-1, sizeof(double));
        s_vig.angle_vec = calloc(s_vig.nrow, sizeof(double));

	s_vig.key = calloc(s_vig.n_asciilines, sizeof(char *));
	s_vig.val = calloc(s_vig.n_asciilines, sizeof(char *));
	s_vig.comm = calloc(s_vig.n_asciilines, sizeof(char *));
	for (ii=0; ii<s_vig.n_asciilines; ii++){
	    s_vig.key[ii] = calloc(STR_MAX_LEN, sizeof(char));
	    s_vig.val[ii] = calloc(STR_MAX_LEN, sizeof(char));
	    s_vig.comm[ii] = calloc(STR_MAX_LEN, sizeof(char));
	}
	

        /* open the file again to start reading */
        fp = fopen(file_vignette,"r");
	
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

		strcpy(s_vig.key[ii],key);
		strcpy(s_vig.val[ii],val);
		strcpy(s_vig.comm[ii],comment);
		
		ii++;
	    }
	}
	
	for (ii=0; ii<s_vig.n_asciilines; ii++){
	    strcpy(templine,s_vig.key[ii]);

	    if (0 == strncasecmp(templine,"ENG",3)){
		remove_substring(templine,"ENG");
		s_vig.energy_vec[atoi(templine)-1] = (double)atof(s_vig.val[ii]);

	    } else if (0 == strncasecmp(templine,"TUNIT",5)){
		remove_substring(templine,"TUNIT");
		strcpy(s_vig.col_unit[atoi(templine)-1], s_vig.val[ii]);

	    } else if (0 == strncasecmp(templine,"TFORM",5)){
                remove_substring(templine,"TFORM");
                strcpy(s_vig.col_form[atoi(templine)-1], s_vig.val[ii]);
		
            } else if (0 == strncasecmp(templine,"TTYPE",5)){
                remove_substring(templine,"TTYPE");
                strcpy(s_vig.col_type[atoi(templine)-1], s_vig.val[ii]);
	    }

	}

        for (ii=0; ii<s_vig.ncol; ii++)
            if (0 == strcasecmp(s_vig.col_unit[ii],"")){
                free(s_vig.col_unit[ii]);
                s_vig.col_unit[ii] = "unitless";
            }


	/* now read in the matrix data */
	mat_counter=0;
	while ( fgets(line, sizeof(line), fp) != NULL){
	    len = strlen(line)-1; if(line[len] == '\n') line[len] = 0;
	    
	    /* if we're not dealing with an empty line... */
	    if (strlen(line) > 0){
		
		/* first column is angle data */
		val = strtok(line," ");
		s_vig.angle_vec[mat_counter] = (double)atof(val);
		
		for (ii=0; ii<s_vig.ncol-1; ii++){
		    val = strtok(NULL," ");
		    s_vig.vig_matrix[ii][mat_counter] = (double)atof(val);
		}
		mat_counter++;
	    }
	}
	
        fclose(fp);
    }


    if (fitstype == 1) {
	write_fits1(&s_vig, file_vig_fits);
    } else if (fitstype == 2) {
	write_fits2(&s_vig, file_vig_fits);
    } else
	printf("ERROR: FITS type must be 1 or 2\n");


    /* clean up */
    for (ii=0; ii<s_vig.ncol-1; ii++)
	free(s_vig.vig_matrix[ii]);
    free(s_vig.vig_matrix);

    for (ii=0; ii<s_vig.ncol; ii++){
	if (0 != strcasecmp(s_vig.col_unit[ii],"unitless")) free(s_vig.col_unit[ii]);
	free(s_vig.col_type[ii]);
	free(s_vig.col_form[ii]);
    }
    free(s_vig.col_unit);
    free(s_vig.col_type);
    free(s_vig.col_form);

    free(s_vig.energy_vec);
    free(s_vig.angle_vec);

    for (ii=0; ii<s_vig.n_asciilines; ii++){
	free(s_vig.key[ii]);
	free(s_vig.val[ii]);
	free(s_vig.comm[ii]);
    }
    free(s_vig.key);
    free(s_vig.val);
    free(s_vig.comm);


    return 0;
}


/* FUNCTION NAME:  write_fits1              */
/*                                          */
/* PURPOSE: write fits type 1 file          */
/*                                          */
/* INPUTS: s_vig, file_vig_fits             */
/*                                          */
/* OUTPUTS: writes fits file                */
/*                                          */

void write_fits1(Vig_struct * s_vig,      /* structure that holds vig data */
		 char * file_vig_fits){   /* fits file name */

    int fstat = 0;               /* status flag for success/failure of fits operations */
    int ii=0, jj=0;              /* loop index */
    fitsfile * ounit = NULL;     /* fits file unit */
    int colnum;                  /* column number */
    int firstrow;                /* first row in fits file */
    int firstelem;               /* first element in fits file */
    double * tempvec;            /* temporary vector holder */            

    /* create the file, insert a binary table, and move to the correct extension name */
    fits_create_file(&ounit, file_vig_fits, &fstat);
    fits_insert_btbl(ounit, s_vig->nrow, s_vig->ncol, s_vig->col_type, s_vig->col_form, s_vig->col_unit,
		     "VIGNETTE", 0, &fstat);
    fits_movnam_hdu(ounit, BINARY_TBL, "VIGNETTE", 0, &fstat);


    /* For each line in the ASCII file write/modify keywords in output fits file */
    for (ii=0; ii<s_vig->n_asciilines; ii++){
	
	/* first try to modify existing keyword */
	if (0 == strncasecmp(s_vig->key[ii],"ENG",3)){
	    fits_modify_key_flt(ounit,s_vig->key[ii],(float)atof(s_vig->val[ii]),3,s_vig->comm[ii],&fstat);
	    
	} else if (0 == strncasecmp(s_vig->key[ii],"TUNIT",5)){
	    fits_modify_key_str(ounit,s_vig->key[ii],s_vig->val[ii],s_vig->comm[ii],&fstat);

	} else if (0 == strncasecmp(s_vig->key[ii],"TELES",5)){
            fits_modify_key_str(ounit,s_vig->key[ii],s_vig->val[ii],s_vig->comm[ii],&fstat);

	} else if (0 == strncasecmp(s_vig->key[ii],"INSTR",5)){
            fits_modify_key_str(ounit,s_vig->key[ii],s_vig->val[ii],s_vig->comm[ii],&fstat);

        } else if (0 == strncasecmp(s_vig->key[ii],"EXTNA",5)){
            fits_modify_key_str(ounit,s_vig->key[ii],s_vig->val[ii],s_vig->comm[ii],&fstat);

	}

	if (fstat != 0){  /* if keyword doesn't yet exist, create it */
	    fstat=0;
	    if (0 == strncasecmp(s_vig->key[ii],"ENG",3)){
		fits_write_key_flt(ounit,s_vig->key[ii],(float)atof(s_vig->val[ii]),3,s_vig->comm[ii],&fstat);
		
	    }  else if (0 == strncasecmp(s_vig->key[ii],"TUNIT",5)){
		fits_write_key_str(ounit,s_vig->key[ii],s_vig->val[ii],s_vig->comm[ii],&fstat);

	    } else if (0 == strncasecmp(s_vig->key[ii],"TELES",5)){
		fits_write_key_str(ounit,s_vig->key[ii],s_vig->val[ii],s_vig->comm[ii],&fstat);

	    } else if (0 == strncasecmp(s_vig->key[ii],"INSTR",5)){
		fits_write_key_str(ounit,s_vig->key[ii],s_vig->val[ii],s_vig->comm[ii],&fstat);

	    } else if (0 == strncasecmp(s_vig->key[ii],"EXTNA",5)){
		fits_write_key_str(ounit,s_vig->key[ii],s_vig->val[ii],s_vig->comm[ii],&fstat);
	    }
	    
	}
    }
    
    colnum=1;
    firstrow=1;
    firstelem=1;
    
    /* first column is the angle vector */
    fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, s_vig->nrow, s_vig->angle_vec, &fstat);

    /* remaining columns are from vig matrix */
    for (ii=0; ii<s_vig->ncol-1; ii++){
	colnum++;
	tempvec = calloc(s_vig->nrow, sizeof(double));
	for (jj=0; jj<s_vig->nrow; jj++)
	    tempvec[jj] = s_vig->vig_matrix[ii][jj];
	fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, s_vig->nrow, tempvec, &fstat);
	free(tempvec);
    }
    
    /* close the fits file */
    fits_close_file(ounit,&fstat);


}

/* FUNCTION NAME:  write_fits2              */
/*                                          */
/* PURPOSE: write fits type 2 file          */
/*                                          */
/* INPUTS: s_vig, file_vig_fits             */
/*                                          */
/* OUTPUTS: writes fits file                */
/*                                          */

void write_fits2(Vig_struct * s_vig,      /* structure that holds vig data */
		 char * file_vig_fits){   /* fits file name */

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

    n_energy_cols = s_vig->ncol-1;

    col_type = calloc(2, sizeof(char *));
    col_form = calloc(2, sizeof(char *));
    col_unit = calloc(2, sizeof(char *));
    for (ii=0; ii<2; ii++){
	col_type[ii] = calloc(STR_MAX_LEN, sizeof(char));
	col_form[ii] = calloc(STR_MAX_LEN, sizeof(char));
	col_unit[ii] = calloc(STR_MAX_LEN, sizeof(char));
    }

    for (ii=0; ii<s_vig->n_asciilines; ii++){
	if (0 == strncasecmp(s_vig->key[ii],"TELES",5))
	    telescope = s_vig->val[ii];
	if (0 == strncasecmp(s_vig->key[ii],"INSTR",5))
            instrument = s_vig->val[ii];
    }

    strcpy(col_type[0],s_vig->col_type[0]);
    strcpy(col_form[0],s_vig->col_form[0]);
    strcpy(col_unit[0],s_vig->col_unit[0]);

    printf("Creating type VIG_FITS_2 file.\n");

    /* create the file, insert a binary table, and move to the correct extension name */
    fits_create_file(&ounit, file_vig_fits, &fstat);

    for (ii=1; ii<s_vig->ncol; ii++){
	strcpy(col_type[1],s_vig->col_type[ii]);
	strcpy(col_form[1],s_vig->col_form[ii]);
	strcpy(col_unit[1],s_vig->col_unit[ii]);

	fits_insert_btbl(ounit, 1, 2, col_type, col_form, col_unit, "VIGNETTE", 0, &fstat);
	fits_write_key_str(ounit, "TELESCOP", telescope, "TELESCOPE NAME", &fstat);
	fits_write_key_str(ounit, "INSTRUME", instrument, "INSTRUMENT NAME", &fstat);
	fits_write_key_dbl(ounit, "ENERGY", s_vig->energy_vec[ii-1], 3, "ENERGY in keV", &fstat);
	
	/* write the angle vector */
	fits_write_col(ounit, TDOUBLE, 1, 1, 1, s_vig->nrow, s_vig->angle_vec, &fstat);
	tempvec = calloc(s_vig->nrow, sizeof(double));
	for (jj=0; jj<s_vig->nrow; jj++)
	    tempvec[jj] = s_vig->vig_matrix[ii-1][jj];
	fits_write_col(ounit, TDOUBLE, 2, 1, 1, s_vig->nrow, tempvec, &fstat);
	free(tempvec);


    }
    fits_close_file(ounit,&fstat);

    fits_open_file(&ounit, file_vig_fits, READONLY, &fstat);
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
