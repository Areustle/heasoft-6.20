/* cfitsio needs to be used directly to process variable-length
columns for a test file.  RSH, 2013-03-04 */

#include <stdio.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"

int main (int argc, char **argv);

int main (int argc, char **argv)
{
    fitsfile *infptr = 0;
    int status = 0, hdutype = 0;

    char* filename = "types_with_nulls.fits";
    char errmsg[31];
    
    unsigned char bytedata[5] = {0, 66, 67, 0, 69};
    unsigned char bytenull = 0;

    double doubledata[5] = {1024.0, 2.0, 3.0, 1024.0, 5.0};
    double doublenull = 1024.0;
    
    printf ("%s\n", filename);

    fits_open_file (&infptr, filename, 1, &status);
    if (status != 0) {
        fits_get_errstatus(status, errmsg);
	printf("%s %s\n", "cftisio error:", errmsg);
	return 1;
    }
    
    fits_movabs_hdu (infptr, 2, &hdutype, &status);
    if (status != 0) {
        fits_get_errstatus(status, errmsg);
	printf("%s %s\n", "cftisio error:", errmsg);
	return 1;
    }
    
    fits_write_colnull(infptr, TDOUBLE, 18, 2, 1, 5, doubledata, &doublenull, &status);
    if (status != 0) {
        fits_get_errstatus(status, errmsg);
	printf("%s %s\n", "cftisio error:", errmsg);
	return 1;
    }

    fits_write_colnull(infptr, TBYTE, 19, 2, 1, 5, bytedata, &bytenull, &status);
    if (status != 0) {
        fits_get_errstatus(status, errmsg);
	printf("%s %s\n", "cftisio error:", errmsg);
	return 1;
    }
 
    fits_close_file (infptr, &status);
    if (status != 0) {
        fits_get_errstatus(status, errmsg);
	printf("%s %s\n", "cftisio error:", errmsg);
	return 1;
    }
   
    

}
        
