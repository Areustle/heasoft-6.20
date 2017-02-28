#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <fitsio.h>
#include <xpi.h>
#include <cftools.h>
#include "cphead.h"
#define  BufLen_2  FLEN_FILENAME-1 
/******************************************************************************
* SELECTOR TASK:
*      cphead
*
* FILE:
*      cphead.c
*
* DESCRIPTION:
*      
*     Copy keywords from a HDU  of a input file to a HDU of a output
*     file.
*     If the extension is not given, the default extension will be 
*     number 0 ( like cfitsio).
*
* Usage:
*     cphead infil outfil 
* where infil = filetype://filename[ext]
*       outfil = filename[ext]
*
* AUTHOR:
*      Ning Gan  6/2/98 v1.0
*
* MODIFICATION HISTORY:
*      6/2/98 v1.0 Ning Gan.
*      8/10/98 v1.1 Ning Gan 
*              Added the default extension.
*      8/10/98 v1.2 Ning Gan 
*              undo the revision on v1.1.
*
*
*******************************************************************************/

/*================= Function prototypes =====================*/
int init_cphead(char *infil, char *outfil, char *keyfil,
		int *scale, int *comment, int *history, int *status);

/*================= Global variables ========================*/
static char errmes[FLEN_ERRMSG];	
static char comm[FLEN_COMMENT];
static char task[] = "cphead v1.2";
void cphead()
{ 
    char infil[FLEN_FILENAME] = "";  	/* Input Fits files */
    char outfil[FLEN_FILENAME] = "";  	/* output fits file */
    char keyfil[FLEN_FILENAME] = "";
    char *keys[MAXCOPYKEYS];

    fitsfile *infits;
    fitsfile *outfits;
    FILE *fp;


    int status = 0;
    int i;
    char *p;

    int hdutype;
    int hdunum;


    int scale = 0;			/* copy the scale keyword ?*/  
    int comment = 0;			/* copy the comment keyword ? */  
    int history = 0;			/* copy the history keyword */ 
    char buf[FLEN_CARD];
    int nkeys = 0;

    c_ptaskn(task);

    /* get the input file and output file from the parameter */

    if(init_cphead(infil, outfil, keyfil,
	     &scale, &comment, &history, &status) ) {
        strcpy(errmes,"Errors in init_cphead");
        c_fcerr(errmes);
	return ;
    }


    /* open the key list file and prepare the keylist */ 
    if( ( ( fp = fopen(keyfil,"r") )== NULL) && (strlen(keyfil) != 0) ) {
        sprintf(errmes,"key list file %s does not exist. Quit!",keyfil);
        c_fcerr(errmes);
	return;
    }
    if(strlen(keyfil) != 0)  { 
	nkeys = 0;
	while( fgets(buf, FLEN_CARD, fp) != NULL) {
	    keys[nkeys] = (char *) malloc( strlen(buf)+1);
	    strcpy(keys[nkeys],buf);
	    if(( p = strchr(keys[nkeys],'\n')) !=NULL) *p = '\0';
	    if(( p = strchr(keys[nkeys],' '))  !=NULL) *p = '\0';
	    nkeys++;
        }
    }
    if(comment == 0) {
	    keys[nkeys] = (char *) malloc(FLEN_KEYWORD+1);
	    strcpy(keys[nkeys],"!COMMENT");
	    nkeys++;
    }

    if(history == 0) {
	    keys[nkeys] = (char *) malloc(FLEN_KEYWORD+1);
	    strcpy(keys[nkeys],"!HISTORY");
	    nkeys++;
    }


    /* open The input fitsfile  */  
    if(fits_open_file(&infits, infil, READONLY, &status)) {
	fits_report_error(stderr,status); 
        return ;
    } 
    


    /* open the output fitsfile */
    if(fits_open_file(&outfits, outfil, READWRITE, &status)) {
	fits_report_error(stderr,status); 
	return;
    }


    /* copy the keywords */
    if(nkeys == 0) {
       c_copyhead(infits, outfits, scale, "0");
    } 
    else {
       c_copyheadn(infits, outfits, scale, nkeys, keys);
    }
	
    /* close the output fitsfile  */ 
    fits_close_file(outfits, &status);

    /* close the input fitsfile  */ 
    fits_close_file(infits, &status);

    for(i=0; i<nkeys; i++) free(keys[i]);
    return ;
}

/******************************************************************************
* Function
*      init_cphead
*
*
* DESCRIPTION:
*      Get the parameters from the par file
*
*******************************************************************************/
int init_cphead(char *infil, 		/* input filename+filter */
		char *outfil, 		/* output filename */
		char *keyfil, 		/* key list file  */
		int  *scale,		/* scale-related  keywords flag */ 
		int  *comment,		/* comment  keywords flag */ 
		int  *history,		/* comment  keywords flag */ 
		int *status
		)
{

    /* get name of the input fits file */
    *status = 0;
    Uclgst("infile", infil, status);
    if(*status) {
        strcpy(errmes,"could not get infile parameter");
        c_fcerr(errmes);
        return *status;
    }


    /* get name of the output fits file */
    *status = 0;
    Uclgst("outfile",outfil,status);
    if(*status) {
        strcpy(errmes,"could not get outfile parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* get name of the key list  file */
    *status = 0;
    Uclgst("keyfil",keyfil,status);
    if(*status) {
        strcpy(errmes,"could not get keyfil parameter");
        c_fcerr(errmes);
        return *status;
    } 
    
    /* copy the scale keywords? */
    Uclgsb("scale",scale,status);
    if(*status) {
        strcpy(errmes,"could not get the scale keyword");
        c_fcerr(errmes);
        return *status;
    }
 
    /* copy the comment keywords? */
    Uclgsb("comment",comment,status);
    if(*status) {
        strcpy(errmes,"could not get the comment keyword");
        c_fcerr(errmes);
        return *status;
    }

    /* copy the history keywords? */
    Uclgsb("history",history,status);
    if(*status) {
        strcpy(errmes,"could not get the history keyword");
        c_fcerr(errmes);
        return *status;
    }
    return 0;
}
/* The following code is needed to allow IRAF to call cphead.
This extra subroutine layer is needed because of differences in
linkers between vms and unix. */
#ifdef vms
#define F77CALL icphead
#endif
#ifdef unix
#define F77CALL icphead_
#endif

void F77CALL()
{
        void cphead();
        cphead();
}


