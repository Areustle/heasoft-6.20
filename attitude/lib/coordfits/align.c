#include "align.h"
#include <string.h>
#include <math.h>

/****************************************************************************
* Allocate storage for an ALIGN object and set it's values to the defaults
* Specificly an identity rotation, positive roll angle with zero offset.
*****************************************************************************/
ALIGN* allocateDefaultAlign() {

    ALIGN* align;

    align = (ALIGN*)malloc(sizeof(ALIGN));

    align->q = allocateQuat();
    align->q_inverse = allocateQuat();

    setQuatToIdentity(align->q);
    setQuatToIdentity(align->q_inverse);
    
    align->roll_offset = 0.0;
    align->roll_sign   = 1;

    return align;


} /* end of allocateAlign function */

/************************************************************************
* destructor
************************************************************************/
void destroyAlign(ALIGN* align) {

    destroyQuat(align->q);
    destroyQuat(align->q_inverse);
    free(align);

} /* end of destroyAlign function */

/* Copy an ALIGN structure. */
void copyAlign(ALIGN* dest, ALIGN* source)
{
  copyQuat(dest->q, source->q);
  copyQuat(dest->q_inverse, source->q_inverse);
  dest->roll_sign = source->roll_sign;
  dest->roll_offset = source->roll_offset;
}

/******************************************************************************
* read alignment information from the current HDU of an already opened FITS
* file. This function is useful for reading the alignment embedded in a
* teldef file.
******************************************************************************/
ALIGN* readAlignFromFITSfile(fitsfile* fp) {


    ALIGN* align;

    ROTMATRIX* rot;
    int status=0;

    char key[FLEN_KEYWORD];
    int i,j;
    
    long long_value;

    /*****************************
    * create and align structure *
    *****************************/
    align = allocateDefaultAlign();


    /***********************************
    * allocate ROTMATRIX and open file *
    ***********************************/
    rot=allocateRotMatrix();

    /******************
    * read the matrix *
    ******************/
    for(j=0;j<3;++j) {
        for(i=0;i<3;++i) {

            sprintf(key,"ALIGNM%d%d",i+1,j+1);
            fits_read_key_dbl(fp,key,&(rot->m[j][i]),NULL,&status);
        }
    }

    /************************
    * convert to quaternion *
    ************************/
    convertRotMatrixToQuat(align->q,rot);

    if(fabs(normOfQuat(align->q) -1.0) > ALIGN_ROUNDOFF_ERROR) {
        fprintf(stderr, "Ill-formed alignment matrix\n");
        return NULL;
    }

    /*********
    * invert *
    *********/
    invertQuat(align->q_inverse, align->q);

    /*******************************
    * cleanup and check for errors *
    *******************************/
    destroyRotMatrix(rot);


    if(status) {
        fprintf(stderr,"Error reading alignment file\n");
        fits_report_error(stderr,status);
        return NULL;
    }

    /*************************************************
    * now read the roll angle specification keywords *
    *************************************************/
    fits_read_key_dbl(fp, "ROLLOFF", &(align->roll_offset), NULL, &status);
    if(status == KEY_NO_EXIST) {
        /***********************************
        * this is OK, just use the default *
        ***********************************/
        status=0;
    }
    
    fits_read_key_lng(fp, "ROLLSIGN", &long_value, NULL, &status);
    if(status == KEY_NO_EXIST) {
        /***********************
        * again, not a problem *
        ***********************/
        status=0;
    } else {
        if(long_value == 1) align->roll_sign=1;
        else if(long_value == -1) align->roll_sign=-1;
        else {
            fprintf(stderr, "Invalid ROLLSIGN=%ld\n",long_value);
            return NULL;
        }
    }

    /*************************
    * check for stray errors *
    *************************/
    if(status) {
        fprintf(stderr,"Error reading alignment file\n");
        fits_report_error(stderr,status);
        return NULL;
    }

    /*************************************
    * if we get here, everything is fine *
    *************************************/
    return align;

} /* end of  readAlignFromFITSfile function */

/***************************************************************************
* read a standalone alignment file. If the filename is "none" (case
* insensitive) or empty, then no file will be read and a default ALIGN
* structure will be returned.
***************************************************************************/
ALIGN* readAlign(char* filename) {

    ALIGN* align;
    int status=0;
    fitsfile* fp;

    /*********************************************
    * check if the filename indiecates "no file" *
    *********************************************/
    if(!strcasecmp(filename,"none") || filename[0]=='\0' ) {
        /************************************************************
        * the filename is "none" or empty, so just return a default *
        ************************************************************/
        return allocateDefaultAlign();
    }


    /*********************
    * open the FITS file *
    *********************/
    fits_open_file(&fp,filename,READONLY,&status);
    if(status) {
        fprintf(stderr,"Could not open alignment file  %s\n",filename);
        fits_report_error(stderr,status);
        return NULL;
    }
    
    /*********************
    * read the alignment *
    *********************/
    align = readAlignFromFITSfile(fp);
    
    /**********************
    * close the FITS file *
    **********************/
    fits_close_file(fp, &status);
    if(status) {
        fprintf(stderr,"Error closing alignment file  %s\n",filename);
        fits_report_error(stderr,status);
        return NULL;
    }
    
    /********************************************
    * if we get here, everything should be fine *
    ********************************************/
    return align;

} /* end of readAlign function */

/************************************************************************
* Given a quaternion, calculate the corresponding RA Dec and Roll,
* applying the given misalignment and roll angle convention.
************************************************************************/
void convertQuatToRADecRoll(ALIGN* align, QUAT* q,
                       double* ra, double* dec, double* roll) {
    QUAT* realigned;
    EULER* e;

    /***********************************************
    * apply the alignment matrix to the quaternion *
    ***********************************************/
    realigned = allocateQuat();
    productOfQuats(realigned,q,align->q_inverse);

    /**************************
    * convert to Euler angles *
    **************************/
    e = allocateEuler();
    convertQuatToEuler(e, realigned);

    /**********************************************************
    * now convert to RA, Dec, and Roll
    * Note that we apply the roll angle convention in the
    * ALIGN structure
    *********************************************************/
    *ra=180./M_PI*e->phi;
    while(*ra<0.) *ra+=360.;

    *dec=90.-180./M_PI*e->theta;

    *roll=180./M_PI*e->psi - 90.;
    *roll = (*roll + align->roll_offset) * align->roll_sign;
    while(*roll<0.) *roll+=360.;

    /**********
    * cleanup *
    **********/
    destroyQuat(realigned);
    destroyEuler(e);

} /* end of convertQuatToRADecRoll function */


/************************************************************************
* Given RA Dec and roll, calculate the corresponding quaternion,
* applying the given misalignment and roll angle convention.
************************************************************************/
void convertRADecRollToQuat(ALIGN* align, QUAT* q,
                       double ra, double dec, double roll) {
                       

    QUAT* realigned;
    EULER* e;
    

    /**********************************************************
    * convert to Euler angles, applying roll angle convention *
    **********************************************************/
    e = allocateEuler();
    setEulerAngles(e, ra*M_PI/180., 
                      (90.-dec)*M_PI/180., 
                      (roll*align->roll_sign+90 - align->roll_offset)*M_PI/180. );

    /**************************
    * convert to a quaternion *
    **************************/
    realigned = allocateQuat();
    convertEulerToQuat(realigned, e);

    /*****************************
    * now apply the misalignment *
    *****************************/
    productOfQuats(q, realigned, align->q);
    
    /**********
    * cleanup *
    **********/
    destroyQuat(realigned);
    destroyEuler(e);
    
} /*end of convertRADecRollToQuat function */


/****************************************
 * Print an Align structure to a stream.
 ****************************************/
void printAlign(ALIGN* align, FILE* stream)
{
  fprintf(stream, "        ALIGN structure contents:\n");
  fprintf(stream, "          q: ");
  printQuat(align->q, stream);
  fprintf(stream, "\n          q_inverse: ");
  printQuat(align->q_inverse, stream);
  fprintf(stream, "\n          roll_sign: %d\n", align->roll_sign);
  fprintf(stream, "          roll_offset: %f\n", align->roll_offset);
}
