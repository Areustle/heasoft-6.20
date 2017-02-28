#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "attfile.h"
#include "attconvert.h"

/*
#define DEBUG
*/


#define EPSILON (1e-3)


/***********************************************************************
*************************************************************************
* handle FITSIO errors while reading attitude files
* prints error messages and exits if there is an error
************************************************************************/
void checkAttFileFITSerrors(int status, char* doing, ATTFILE* file) {
 
   if(!status) return;
 
   fprintf(stderr,"FITSIO error while %s file %s:\n",doing,file->name);
   fits_report_error(stderr,status);

   exit(status);
 
}


/**************************************************************************
***************************************************************************
* create an attfile structure and open an actual attitude file
**************************************************************************/
ATTFILE* openAttFile(char* filename ) {
ATTFILE* file;
int name_length;
int status=0;
char comment[FLEN_COMMENT];


/*******************************
* allocate space for structure *
*******************************/
file=(ATTFILE*)calloc(1, sizeof(ATTFILE));


/************************************************************
* allocate space for file name and copy name into structure *
************************************************************/
name_length=strlen(filename)+1;
file->name=(char*)malloc(name_length*sizeof(char));
strncpy(file->name,filename,name_length);

/******************************************************************************
* open the attitude file 
* This routine will open arbitrary (supported) format files.
* standard format files are simply opened as-is, but other (supported) formats
* are first converted to a standard format temporary file. The temporary
* file is memory resident by default
******************************************************************************/
file->fp = open_any_attfile(filename);


/*********************************************
* read the mission from the TELESCOP keyword *
*********************************************/
fits_read_key_str(file->fp,"TELESCOP",file->mission,comment,&status);
if(status==KEY_NO_EXIST) {
    /************************************
    * set mission to UNKNOWN by default *
    ************************************/
    status=0;
    strncpy(file->mission,"UNKNOWN",FLEN_VALUE);
}
checkAttFileFITSerrors(status,"reading TELESCOP from",file);

/***************************
* go to ATTITUDE extension *
***************************/
fits_movnam_hdu(file->fp,BINARY_TBL,"ATTITUDE",0/* ignore version*/,&status);
checkAttFileFITSerrors(status,"finding ATTITUDE extension in",file);

/*******************************
* read number of rows in table *
*******************************/
fits_read_key_lng(file->fp,"NAXIS2",&(file->nrows),comment,&status);
checkAttFileFITSerrors(status,"reading NAXIS2 from",file);

/**********************
* read column numbers *
**********************/
fits_get_colnum(file->fp,CASESEN,"TIME"  ,&(file->time_col)  ,&status);
fits_get_colnum(file->fp,CASESEN,"QPARAM",&(file->qparam_col),&status);

/*
fits_get_colnum(file->fp,CASESEN,"SIGMA" ,&(file->sigma_col) ,&status);
fits_get_colnum(file->fp,CASESEN,"SENSOR",&(file->sensor_col),&status);
*/

checkAttFileFITSerrors(status,"locating columns in",file);

/******************************************************************
* mark the last search result as uninitialized
* since we haven't done any searches yet
* also allocate space for the resident quaternian structures
* which are used to make repeated interpolations more efficient.
*****************************************************************/
file->search_row=UNINITIALIZED_ATTFILE_ROW;
file->search_qs_reliable=0;

file->search_q0    =allocateQuat();
file->search_q1    =allocateQuat();
file->search_deltaq=allocateQuat();
file->hatq         =allocateQuat();



/*********************************************************
* read last and first times and set extrapolation limits 
* we read the first time last to leave the FITSIO buffers  
* holding the beginning of the file
*********************************************************/
file->tstop =readTimeFromAttFile(file,file->nrows);
file->tstart=readTimeFromAttFile(file,1L         );

file->duration=file->tstop - file->tstart;
if (file->duration <= 0) {
	printf("warning: ATTFILE %s does not have positive duration [%f]\n",
			file->name, file->duration);
	file->duration = 1;
}

resetAttFileExtrapolationLimits(file,DEFAULT_ATTFILE_EXTRAPOLATION);

file->interpolation = ATTFILE_LINEAR;
file->extrapolation = ATTFILE_CONSTANT;

return(file);

}


/**************************************************************************
***************************************************************************
* close an attitude file and destroy the ATTFILE structure
**************************************************************************/
void closeAttFile(ATTFILE* file) {
int status=0;

fits_close_file(file->fp,&status);
checkAttFileFITSerrors(status,"closing",file);

destroyQuat(file->search_q0);
destroyQuat(file->search_q1);
destroyQuat(file->search_deltaq);
destroyQuat(file->hatq);

free(file->name);
free(file);

}



/**************************************************************************
***************************************************************************
* Read a generic quantity from the attitude file.
* This routine makes the FITSIO read call, checks for errors and 
* resets the search flags if needed.
* This routine should not be used by the general user. Instead, the
* column-specific functions
* readQuatFromAttFile
* readTimeFromAttFile, etc should be used.
**************************************************************************/
void readThingFromAttFile(ATTFILE* file, void* data, void* nullvalue,
                          int datatype,
                          long row, int col, long nelements)
{
int anynull=0;
int status=0;

/********************
* read the value(s) *
********************/
fits_read_col(file->fp,datatype,col,row,1L,nelements,nullvalue,data,
              &anynull,&status);

/*******************
* check for errors *
*******************/
checkAttFileFITSerrors(status,"reading",file);

if(anynull) {
    fprintf(stderr,"Null values in column %d row %ld of %s\n",
            col,row,file->name);

    exit(anynull);
}

/******************************************************************
* reset the search row pointer if we have just read from a wildly
* different part of the file
*****************************************************************/
if(file->search_row !=UNINITIALIZED_ATTFILE_ROW &&
   abs(row-file->search_row)>DIST_FOR_ATTFILE_RESET ) {
    file->search_row=UNINITIALIZED_ATTFILE_ROW;
    file->search_qs_reliable=0;
}


}

/**************************************************************************
***************************************************************************
* Read the Q parameter from a given row of the table
**************************************************************************/
void readQuatFromAttFile(ATTFILE* file, QUAT* q,long row)
{
double nullvalue=0.;

readThingFromAttFile(file,q->p,&nullvalue,TDOUBLE,row,file->qparam_col,4l);
renormalizeQuat(q);

#ifdef DEBUG
printf("readQuatFromAttFile: row=%ld, row, norm-1=%g\n",
       row, normOfQuat(q)-1.0 );

#endif 

}


/**************************************************************************
***************************************************************************
* Read the TIME from a given row of the table
**************************************************************************/
double readTimeFromAttFile(ATTFILE* file,long row)
{
double nullvalue=0.;
double time;

readThingFromAttFile(file,&time,&nullvalue,TDOUBLE,row,file->time_col,1L);

return(time);

}

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file
* returns 1 if time is withing the first and last rows
* returns 0 otherwise
**************************************************************************/
int isInAttFile(ATTFILE* file, double time)
{
return(time>=file->tstart && time<=file->tstop);
}

/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file, or its 
* reasdonable extrapolation
* returns 1 if time is within extrrapolation limits
* returns 0 otherwise
* See also resetAttFileExtrapolationLimits.
* Note there are no internal checks for whether a time is covered by the 
* attfile, these must be supplied by the calling program.
**************************************************************************/
int isInExtrapolatedAttFile(ATTFILE* file, double time)
{
return(time>=file->min_extrapolated && time<=file->max_extrapolated);
}

/**************************************************************************
***************************************************************************
* Changes the valid extrapolation time limits for the attitude file.
* The default values when the file is opened is 
* DEFAULT_ATTFILE_EXTRAPOLATION.
* See also sInExtrapolatedAttFile(ATTFILE* file, double time).
**************************************************************************/
void resetAttFileExtrapolationLimits(ATTFILE* file, double margin ) {

file->min_extrapolated=file->tstart-margin;
file->max_extrapolated=file->tstop +margin;

}

/**************************************************************************
***************************************************************************
* Locates a given time value in the attitude file
* the row will be the last row in the file which has a time value 
* less than or equal to the specified time. 
* if the time is before the table the first row in the table is returned.
* if the time is at or beyond the end of the table the second to last row
* will be returned.
* In other words, this routine returns the first of the pair of rows 
* which should be used to interpolate to the specified time.
**************************************************************************/
long findTimeInAttFile(ATTFILE* file, double time)
{
long row;
double current_time,last_time=0.0;
long row0,row1;
double time0,time1;

#ifdef DEBUG
printf("findTimeInAttFile: start\n");
#endif

/************************
* are we extrapolating? *
************************/
if(!isInAttFile(file,time) || time == file->tstop) {


#   ifdef DEBUG
    printf("extrapolating time=%.14g tstart=%.14g tstop=%.14g\n",
            time,file->tstart,file->tstop);
#   endif

    /**********************
    * need to extrapolate *
    **********************/
    if (time < file->tstart || file->nrows == 1L)
        row = 1L;                   /* before beginning */
    else
        row = file->nrows - 1L;     /* after end */


    if (file->search_row != row) {
        /*********************************************************
        * this is different from the result of the last search
        * so we need to reset some things 
        *********************************************************/
        file->search_time0 = readTimeFromAttFile(file, row);
        if (file->nrows > 1)
            file->search_time1 = readTimeFromAttFile(file, row+1);
        file->search_qs_reliable = 0;

        file->search_row = row;
    }

#   ifdef DEBUG
    printf("returning extrapolated\n");
#   endif


    /********************
    * return the answer *
    ********************/
    return(file->search_row);

} 

/**************************************************************
* are we still in the same place we found in the last search? *
**************************************************************/
if (file->search_row != UNINITIALIZED_ATTFILE_ROW &&
   time >= file->search_time0 && 
   time < file->search_time1    )   return(file->search_row);


/************************************************************
* none of the easy answers worked, so we have to go looking 
* through the table. The new search location is guaranteed to 
* be different from the last one, so we need to mark the
* last found quaternions as unreliable. 
************************************************************/
file->search_qs_reliable = 0;

/*********************************************************** 
* determine where we should start an initial linear search *
***********************************************************/
if (file->search_row == UNINITIALIZED_ATTFILE_ROW) {
    /***************************************************************************
    * we haven't searched before, or the last read was far away from the 
    * last row found, so calculate the start position based on tstart and tstop 
    ***************************************************************************/
    row = (time - file->tstart) / file->duration * file->nrows + 1;
} else {
    /*********************************************************************
    * calculate the start position from the current position. This will
    * work better if the current search is near the last search 
    * which we hope should usually be the case
    **********************************************************************/
    double dt = file->search_time1 - file->search_time0;
    if (dt <= EPSILON)
        dt = file->duration / file->nrows;
    row = (time - file->search_time0) / dt + file->search_row;
}    

/*******************************************************
* make sure we haven't gone beyond the end of the file *
*******************************************************/
if (row < 1) row = 1;
if (row > file->nrows - 1) row = file->nrows - 1;
  
/******************************************************************
* try a linear search for the correct time value
******************************************************************/
current_time = readTimeFromAttFile(file, row);

if (time == current_time) {
    /***************************
    * it's right, exactly here *
    ***************************/
    file->search_time0 = current_time;
    file->search_time1 = readTimeFromAttFile(file, row+1);

    file->search_row = row;

    return(file->search_row);

} else if (time > current_time) {
    /*****************
    * search forward *
    *****************/
    int row_limit;

    row_limit = row + ATTFILE_LOCAL_SEARCH_LIMIT;

    /*********
    * search *
    *********/
    while (time >= current_time && row < row_limit) {
        ++row;
        last_time = current_time;
        current_time = readTimeFromAttFile(file, row);
    }

    /***********************
    * check if we found it *
    ***********************/
    if (time < current_time) {
        /******************************
        * we passed the point we want *
        ******************************/
        file->search_row = row - 1;
        file->search_time0 = last_time;
        file->search_time1 = current_time;
        return(file->search_row);
    }

} else if (time < current_time) {
    /*******************
    * search backwards *
    *******************/
    int row_limit;

    row_limit = row - ATTFILE_LOCAL_SEARCH_LIMIT;

    /*********
    * search *
    *********/
    while (time < current_time && row > row_limit) {
        --row;
        last_time = current_time;
        current_time = readTimeFromAttFile(file,row);
    }

    /***********************
    * check if we found it *
    ***********************/
    if (time >= current_time) {
        /*****************************
        * we're at the point we want *
        *****************************/
        file->search_row = row;
        file->search_time0 = current_time;
        file->search_time1 = last_time;
        return(file->search_row);
    }
}

/********************************************************************
* now if we get here, it means we haven't found the point we want
* by a linear search so we use bisection to hunt it down.
********************************************************************/
if (time > current_time) {
    /***************************************************
    * search between current position and end of table *
    ***************************************************/
    row0 = row;
    time0 = current_time;

    row1 = file->nrows;
    time1 = file->tstop;
} else {
    /*********************************************************
    * search between current position and beginning of table *
    *********************************************************/
    row0 = 1L;
    time0 = file->tstart;

    row1 = row;
    time1 = current_time;
}

while (row1 - row0 > 1) {
    row = (row1 + row0) / 2;
    current_time = readTimeFromAttFile(file, row);

    if (current_time == time) {
        file->search_row = row;
        file->search_time0 = current_time;
        file->search_time1 = readTimeFromAttFile(file, row + 1);
        return(file->search_row);

    } else if (current_time > time) {
        row1 = row;
        time1 = current_time;
    } else {
        row0 = row;
        time0 = current_time;
    }

}

/****************************************************************************
* when we get here we have found the point we were looking for by bisection *
****************************************************************************/
file->search_row = row0;
file->search_time0 = time0;
file->search_time1 = time1;
return(file->search_row);

}




/**************************************************************************
***************************************************************************
* determines the quaternion at an arbitrary time
* values are interpolated if necessary and things are optimized
* for the case of repeated calls to this routine with similar time values
**************************************************************************/
int findQuatInAttFile(ATTFILE* file, QUAT* q, double time)
{
int found = 1;
int doLinear;
double hat;

/********************************************
* search for the correct place in the table *
********************************************/
findTimeInAttFile(file, time);

/*****************************************************
* check if we need to read a new pair of quaternions *
*****************************************************/
if (!file->search_qs_reliable) {
    /*********************************************************
    * read two values and calculate the "delta" between them *
    *********************************************************/

    readQuatFromAttFile(file, file->search_q0, file->search_row);

    if (file->nrows > 1) {
        readQuatFromAttFile(file, file->search_q1, file->search_row + 1L);

        getQuatOfChange(file->search_deltaq, file->search_q0, file->search_q1);
    }

    file->search_qs_reliable = 1;
}

/*******************************************************
* if there's only one row we don't need to interpolate *
*******************************************************/
if(file->nrows == 1) {

    *q = *(file->search_q0);
    if (file->limit_nearby >= 0)
        found = fabs(file->search_time0 - time) <= file->limit_nearby;
    return found;
}


doLinear = 0;

if (time <= file->search_time0 || time >= file->search_time1) {
    /* outside the normal time bounds */
    if (file->extrapolation == ATTFILE_CONSTANT) {
        if (time <= file->search_time0)
            *q = *(file->search_q0);
        else
            *q = *(file->search_q1);
    }
    else
        doLinear = 1;
}
else if (file->interpolation == ATTFILE_CONSTANT) {

    if (fabs(file->search_time1 - time) < fabs(file->search_time0 - time))
        *q = *(file->search_q1);
    else
        *q = *(file->search_q0);

}
else if (file->interpolation == ATTFILE_LINEAR_NEARBY) {

	if (fabs(file->search_time1 - time) <= file->limit_nearby
			&& fabs(file->search_time0 - time) <= file->limit_nearby)
		doLinear = 1;
	else if (fabs(file->search_time1 - time) < fabs(file->search_time0 - time))
		*q = *(file->search_q1);
	else
		*q = *(file->search_q0);
}
else
    doLinear = 1;


if (doLinear) {
/**************************
* interpolate quaternions *
**************************/
    double dt = file->search_time1 - file->search_time0;
    if (dt > 0) {
        hat = (time - file->search_time0) / dt;
        multiplyQuatByScalar(file->hatq, file->search_deltaq, hat);

#   ifdef DEBUG
    printf("findQuatInAttFile:   hatq=(%g %g %g %g)\n",     
           file->hatq->p[0],
           file->hatq->p[1],
           file->hatq->p[2],
           file->hatq->p[3]);
    printf("findQuatInAttFile: hat=%g\n",hat);
    printf("findQuatInAttFile: time=%.14g time0=%.14g time1=%.14g\n",
           time, file->search_time0, file->search_time1);
    printf("findQuatInAttFile: search_row=%ld\n", file->search_row);
#   endif

        productOfQuats(q, file->search_q0, file->hatq);
    }
    else
        *q = *(file->search_q0);

#ifdef DEBUG
printf("findQuatInAttFile: norm(q)-1 = %g\n", normOfQuat(q)-1.0);
#endif
}


if (file->limit_nearby >= 0)
    found = fabs(file->search_time1 - time) <= file->limit_nearby
            || fabs(file->search_time0 - time) <= file->limit_nearby
            ;

return found;
}


void requireTimeNearAttFileRecord (ATTFILE * file, double limit)
{
	file->limit_nearby = limit;
}


void setAttFileInterpolation (ATTFILE * file, int mode)
{
	file->interpolation = mode;
}


void setAttFileExtrapolation (ATTFILE * file, int mode)
{
	file->extrapolation = mode;
}


/**************************************************************************
***************************************************************************
* Move the current search row ahead by one row in the attitude file.
* Note that a search (findTimeInAttFile or findQuatInAttFile) must
* be done first to initialize the current search row.
* This routine will not advance beyound the end of the file, but will
* leave the search row unchanged if asked to do so.
**************************************************************************/
void incrementAttFileSearchRow(ATTFILE* file) {

#ifdef DEBUG
printf("incrementAttFileSearchRow: start\n");
#endif

/**********************************************************
* check a bunch of things before doing the actual advance
**********************************************************/

if(file->search_row==UNINITIALIZED_ATTFILE_ROW) {
    /*********************************************************
    * search row uninitialized - must be a programming error *
    *********************************************************/
    fprintf(stderr,"Search row uninitialized in incrementAttFileSearchRow\n");
    exit(1);
}

if(file->search_row==file->nrows-1) {
    /**************************************
    * don't go beyond the end of the file *
    **************************************/
    return;
}


if(!file->search_qs_reliable) {
    /*******************************************************************
    * q's haven't been read for the current search row, so read q1 now *
    *******************************************************************/
    readQuatFromAttFile(file,file->search_q1,file->search_row+1L);
    file->search_qs_reliable=1;
}


#ifdef DEBUG
printf("incrementAttFileSearchRow: about to increment\n");
#endif


/******************************************
* copy the "1" values to the "0" position *
******************************************/
file->search_time0=file->search_time1;
*(file->search_q0)=*(file->search_q1);

++(file->search_row);

/***************************
* read the new "1" values" *
***************************/
file->search_time1=readTimeFromAttFile(file,file->search_row+1L);
readQuatFromAttFile(file,file->search_q1,file->search_row+1L);

getQuatOfChange(file->search_deltaq,file->search_q0,file->search_q1);

}


/* Print the contents of an ATTFILE structure to a stream. */
void printAttFile(ATTFILE* attfile, FILE* stream)
{
  fprintf(stream, "    name: %s\n", attfile->name);
  fprintf(stream, "    fp: %ld\n", (long) attfile->fp);
  fprintf(stream, "    nrows: %ld\n", attfile->nrows);
  fprintf(stream, "    mission: %s\n", attfile->mission);
  fprintf(stream, "    time_col: %d\n", attfile->time_col);
  fprintf(stream, "    qparam_col: %d\n", attfile->qparam_col);
  fprintf(stream, "    sigma_col: %d\n", attfile->sigma_col);
  fprintf(stream, "    sensor_col: %d\n", attfile->sensor_col);
  fprintf(stream, "    tstart: %g\n", attfile->tstart);
  fprintf(stream, "    tstop: %g\n", attfile->tstop);
  fprintf(stream, "    duration: %g\n", attfile->duration);
  fprintf(stream, "    min_extrapolated: %g\n", attfile->min_extrapolated);
  fprintf(stream, "    max_extrapolated: %g\n", attfile->max_extrapolated);
  fprintf(stream, "    search_row: %ld\n", attfile->search_row);
  fprintf(stream, "    search_time0: %g\n", attfile->search_time0);
  fprintf(stream, "    search_time1: %g\n", attfile->search_time1);
  fprintf(stream, "    search_q0: "); printQuat(attfile->search_q0, stream); fprintf(stream, "\n");
  fprintf(stream, "    search_q1: "); printQuat(attfile->search_q0, stream); fprintf(stream, "\n");
  fprintf(stream, "    search_deltaq: "); printQuat(attfile->search_deltaq, stream); fprintf(stream, "\n");
  fprintf(stream, "    hatq: "); printQuat(attfile->hatq, stream); fprintf(stream, "\n");
  fprintf(stream, "    search_qs_reliable: %d\n", attfile->search_qs_reliable);
  fprintf(stream, "    interpolation: %d\n", attfile->interpolation);
  fprintf(stream, "    extrapolation: %d\n", attfile->extrapolation);
  fprintf(stream, "    limit_nearby: %g\n", attfile->limit_nearby);
}

