#include <string.h>
#include "attout.h"
#include "attconvert.h"
#include "align.h"

/*
#define DEBUG
*/

/*****************************************************************************
******************************************************************************
* open an attitude file of any supported format.
* The standard format is the ASCA attitude file format which is a FITS
* bintable with extension name "ATTITUDE" and two columns TIME, and QPARAM.
* QPARAM is an array giving the four components of a quaternion.
* The TELESCOP keyword in the primary header is also read to get the mission
* name, but this is optional and set to "UNKNOWN" if it does not exist.
*
* If an attempt is made to read any other format, then this routine will open
* a new temporary file containing a copy of the file converted to the standard
* format. In general the temporary file will be a memory resident
* "mem://" file.
*
* currently supported formats are the standard format and ASCII RA Dec Roll
* format.
*****************************************************************************/
fitsfile* open_any_attfile(char* filename) {

int status=0;

char* tmpname;
fitsfile* fp;
fitsfile* fp2;

/*******************************************************************
* if we have to convert this file file into the regular format
* then we will need a temporary file name. By default this will
* be a memory file with the same name as the original file.
* At some point we may get clever and use the extended file name 
* parsing to allow the user to specify that the temporary file
* be written on disk
******************************************************************/
tmpname=(char*)malloc(sizeof(char)*(strlen(filename)+1+7) );
strcpy(tmpname,"mem://");
strcat(tmpname,filename);

/*********************
* open the FITS file *
*********************/
fits_open_file(&fp,filename,READONLY,&status);
fp2=fp;

if(status==READ_ERROR || status==UNKNOWN_REC) {
    /*********************************************************
    * this is the sort of error you get then you try to
    * open something which isn't a FITS file. 
    * In this case we assume that the file is actually 
    * in ASCII RA, DEC ROLL format and convert accordingly
    *********************************************************/
    status=0;
    fp2=convert_ascii_ra_dec_roll(filename,tmpname);

    #ifdef DEBUG
    printf("open_any_attfile: done converting\n");
    #endif

}


/*****************************
* check for any stray errors *
*****************************/
if(status) {
   fprintf(stderr,"FITSIO error while opening attitude file %s\n",filename);
   fits_report_error(stderr,status);

   exit(status);
}

#ifdef DEBUG
printf("open_any_attfile: returning fp2@%d\n",(int)fp2);
#endif

free(tmpname);

return(fp2);

} /* end of open_any_attfile */


/****************************************************************************
*****************************************************************************
* convert an a file with the following format to a standard 
* quaternion-based attitude file:
* A plane ASCII text file with each row containing four numbers:
* time ra dec roll
* where time is in seconds from some arbitrary reference time,
* and ra, dec are the celestial coordinates of the spacecraft Z axis
* in decimal degrees, and roll is the roll angle, also in degrees.
* any row beginning with "#" will be ignored. In fact any row 
* from which four separate numbers cannot be read will be ignored.
* This routine does not actually close the fitsfile when done
* and returns a pointer to the fitsfile structure. This way it can be
* used to write memory resident files which are deleted on closing.
****************************************************************************/
fitsfile* convert_ascii_ra_dec_roll(char* infile, char* outfile) {

FILE* fp;
ATTOUT* att;

int nread;

double time, ra, dec, roll;
ALIGN* align;
QUAT* q;

/**************
* open infile *
**************/
fp=fopen(infile,"r");
if(fp==NULL) {
    fprintf(stderr,"Can't open Euler ASCII attitude file %s\n",infile);
    exit(1);
}

/***************
* open outfile *
***************/
att=createAttOut(outfile);


/*************************************
* allocate euler and quat structures *
*************************************/
align = allocateDefaultAlign();
q    =allocateQuat();

/***********************************
* read all the lines in the infile *
***********************************/
while((nread=fscanf(fp,"%lf %lf %lf %lf",&time,&ra,&dec,&roll))>=0) {

    #ifdef DEBUG
    printf("nread=%d time=%g ra=%g dec=%g roll=%g\n",nread,time,ra,dec,roll);
    #endif

    if(nread==4) {
        /***********************
        * this is a valid line *
        ***********************/
        convertRADecRollToQuat(align, q, ra, dec, roll);

        #ifdef DEBUG
        printf("about to write\n");
        #endif

        addAttOutRow(att,time,q);
    }

} /* end of loop over rows */

/**********
* cleanup *
**********/
destroyQuat(q);

/**********************************************************
* flush the buffers, etc. and return the fitsfile pointer *
**********************************************************/
return(finishAttOut(att));

} /* end of convertRADecRollASCIIAttFile function */
