#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <fitsio.h>
#include <xpi.h>
#include <cftools.h>
/******************************************************************************
* SELECTOR TASK:
*      fcollen
*
* FILE:
*      fcollen.c
*
* DESCRIPTION:
*      
*     Modify the vector length of an existing column in a binary table. 
* The data type of that column is not changed.
*
* Usage:
*     fcollen infil colname collen 
* where infil = filetype://filename[ext]
*       colname = column number or column name, but the wildcard is not
*                 allowed.
*       collen  = new column vector length.
*
* AUTHOR:
*      Ning Gan  6/21/98
*
* MODIFICATION HISTORY:
*      6/21/98 v1.0 Ning Gan.
*      8/10/98 v1.1 Ning Gan 
*              Added the time stamp in history and the default extension(1). 
*               
*
*
*******************************************************************************/

/*================= Function prototypes =====================*/
int init_fcollen(char *infil, char *colname,  int *collen, int *status);

/*================= Global variables ========================*/
static char errmes[FLEN_ERRMSG];	
static char comm[FLEN_COMMENT];
void fcollen()
{ 
    char infil[FLEN_FILENAME];  	/* Input Fits files */
    char colname[FLEN_VALUE];  		/* Column name or number*/
    int colnum = 0;			/* Column number */
    int collen = 0;			/* Desired column length */
    int exttype;     			/* extension type */
    int hdunum;

    fitsfile *infits;

    int status = 0;

    c_ptaskn("fcollen v1.1");

    /* initialization */ 

    memset(infil, 0, FLEN_FILENAME); 


    /* get the input file and output file from the parameter */

    if(init_fcollen(infil, colname,  &collen, &status) ) {
        strcpy(errmes,"Errors in init_fcollen");
        c_fcerr(errmes);
	return ;
    }


    /* open The input fitsfile  */  
    if(fits_open_file(&infits, infil, READWRITE, &status)) {
	fits_report_error(stderr,status); 
        return ;
    } 
    fits_get_hdu_num(infits, &hdunum);

    /* set the default extension to 1 */
    if (hdunum == 1) {
        if (fits_movabs_hdu(infits, 2, &exttype, &status)){
           fits_report_error(stderr,status);
           return ;
        }
    }


    /* make sure it is binary table */
    if(fits_get_hdu_type(infits, &exttype, &status)) {
	fits_report_error(stderr,status); 
        return ;
    } 
    
    if(exttype != BINARY_TBL) {
	strcpy(errmes,"This program can only be applied to the binary table.");         c_fcerr(errmes);
        return;
    }

    /* find the right column number */
    if(fits_get_colnum(infits, CASEINSEN, colname, &colnum, &status)) {
	fits_report_error(stderr,status); 
        return ;
    } 
    
    if(collen < 0) {
       sprintf(errmes,
  "Illegal desired colunm vector length: %d < 0. Column length is not changed.",
   collen);
    c_fcerr(errmes);
    return;
   }
		 

    /* reset the colum vec length */
    status = 0;
    if(fits_modify_vector_len(infits, colnum, (long) collen, &status)) {
	fits_report_error(stderr,status); 
        return ;
    } 

    /* write the history  time stamp */
    c_timestamp(infits);

    /* close the input fitsfile  */ 
    fits_close_file(infits, &status);

    return ;
}

/******************************************************************************
* Function
*      init_fcollen
*
*
* DESCRIPTION:
*      Get the parameters from the par file
*
*******************************************************************************/
int init_fcollen(char *infil, 		/* input filename+filter */
		 char *colname,		/* Column name or number */ 
		 int  *collen,		/* desired Column length */
		 int *status
		)
{

    int BufLen_2;
    /* get name of the input fits file */
    *status = 0;
    BufLen_2  =  FLEN_FILENAME-1 ;
    Uclgst("infile", infil, status);
    if(*status) {
        strcpy(errmes,"could not get infile parameter");
        c_fcerr(errmes);
        return *status;
    }


    /* get name of the output fits file */
    *status = 0;
    BufLen_2  =  FLEN_VALUE-1 ;
    Uclgst("colname",colname,status);
    if(*status) {
        strcpy(errmes,"could not get colname parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* copy the scale keywords? */
    Uclgsi("collen",collen,status);
    if(*status) {
        strcpy(errmes,"could not get the collen keyword");
        c_fcerr(errmes);
        return *status;
    }
 
    return 0;
}
/* The following code is needed to allow IRAF to call fcollen.
This extra subroutine layer is needed because of differences in
linkers between vms and unix. */
#ifdef vms
#define F77CALL ifcollen
#endif
#ifdef unix
#define F77CALL ifcollen_
#endif

void F77CALL()
{
        void fcollen();
        fcollen();
}


