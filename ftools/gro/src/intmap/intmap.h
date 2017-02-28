/* intmap.h */


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "xpi.h"
/* #include <sys/param.h> */
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>



int checkreal(float min, char msg[], char *strval, float *realval);

int listlevel(char *pFits, char *fname, int *LEVELS, float *fact, float *specen, 
	      char **energy);

int namelist(char *pFits, char *countFileName, char *exphst, float *specin, 
	     float *fact);

void readinput(char *data_dir, char *cmapfile, char *exphst, int *expflag, 
	       float *specin, float *sfact, char *misc_dir, char *calib_dir, 
	       char *output_dir, char *cal_bin_dir, int *status);


