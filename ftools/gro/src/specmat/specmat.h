/* specmat.h */

#define  FILESZ  80

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "xpi.h"

void readinput(char *misc_dir, char *data_dir, char *calib_dir, char *cal_bin_dir, 
	       char *output_dir, char *exphistfilebase, char *outputfile,
	       char *selfile, char *scalefacfile, char *rmffile, char *objname, 
	       int *evclass, int *clobber, int *status);


