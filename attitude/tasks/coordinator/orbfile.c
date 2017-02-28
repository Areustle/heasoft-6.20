/*
 * $Source: /headas/headas/attitude/tasks/coordinator/orbfile.c,v $
 * $Revision: 1.1 $
 * $Date: 2005/01/27 20:53:36 $
 *
 * $Log: orbfile.c,v $
 * Revision 1.1  2005/01/27 20:53:36  rwiegand
 * Added capability to correct for spacecraft about earth velocity portion
 * aberration.
 *
 */

#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "orbfile.h"
#include "fitsio.h"
#include "att_fatal.h"

/*
#define DEBUG
*/

/**************************************************************************
***************************************************************************
* Locates a given time value in the orbit file
* the row will be the last row in the file which has a time value 
* less than or equal to the specified time. 
* if the time is before the table the first row in the table is returned.
* if the time is at or beyond the end of the table the second to last row
* will be returned.
* In other words, this routine returns the first of the pair of rows 
* which should be used to interpolate to the specified time.
**************************************************************************/
long findTimeInOrbitFile(ORBFILE* file, double time);

/**************************************************************************
***************************************************************************
* Move the current search row ahead by one row in the orbit file.
* Note that a search (findTimeInOrbitFile or findQuatInOrbitFile) must
* be done first to initialize the current search row.
* This routine will not advance beyound the end of the file, but will
* leave the search row unchanged if asked to do so.
**************************************************************************/
void incrementOrbitFileSearchRow(ORBFILE* file);

/**************************************************************************
***************************************************************************
* Read a generic quantity from the orbit file.
* This routine makes the FITSIO read call, checks for errors and 
* resets the search flags if needed.
* This routine should not be used by the general user. Instead, the
* column-specific functions
* readQuatFromOrbitFile
* readTimeFromOrbitFile, etc should be used.
**************************************************************************/
void readThingFromOrbitFile(ORBFILE* file, void* data, void* nullvalue,
                          int datatype, long row, int col, long nelements);

/**************************************************************************
***************************************************************************
* Read the position/velocity from a given row of the table
**************************************************************************/
void readRecordFromOrbitFile(ORBFILE* file, PVrecord * pv,long row);

/**************************************************************************
***************************************************************************
* Read the TIME from a given row of the table
**************************************************************************/
double readTimeFromOrbitFile(ORBFILE* file,long row);



/***********************************************************************
*************************************************************************
* handle FITSIO errors while reading orbit files
* prints error messages and aborts if there is an error
************************************************************************/
void checkOrbitFileFITSerrors (int status, const char* doing, ORBFILE* file)
{
   if (!status) return;
 
   fprintf(stderr, "FITSIO error while %s file %s:\n", doing, file->name);
   fits_report_error(stderr, status);

   att_fatal(status);
}


/**************************************************************************
***************************************************************************
* create an attfile structure and open an actual attitude file
**************************************************************************/
ORBFILE* openOrbitFile (const char* filename)
{
	ORBFILE* file;
	int status = 0;
	char comment[FLEN_COMMENT];


	/*******************************
	* allocate space for structure *
	*******************************/
	file = (ORBFILE*) calloc(1, sizeof(ORBFILE));

	{
	/************************************************************
	* allocate space for file name and copy name into structure *
	************************************************************/
    	int name_length = strlen(filename)+1;
    	file->name = (char*) malloc(name_length * sizeof(char));
    	strncpy(file->name, filename, name_length);
	}

	/**************************************************************************
	* open the orbit file 
	* This routine will open arbitrary (supported) format files.
	* standard format files are simply opened as-is, but other (supported) formats
	* are first converted to a standard format temporary file. The temporary
	* file is memory resident by default
	**************************************************************************/
	fits_open_file(&file->fptr, filename, READONLY, &status);
	checkOrbitFileFITSerrors(status, "opening FITS file", file);


	/*********************************************
	* read the mission from the TELESCOP keyword *
	*********************************************/
	fits_read_key_str(file->fptr, "TELESCOP", file->mission, comment, &status);
	if (status == KEY_NO_EXIST) {
    	/************************************
    	* set mission to UNKNOWN by default *
    	************************************/
    	status=0;
    	strncpy(file->mission, "UNKNOWN", FLEN_VALUE);
	}
	checkOrbitFileFITSerrors(status, "reading TELESCOP from", file);

	/***************************
	* go to PREFILTER extension *
	***************************/
	fits_movnam_hdu(file->fptr, BINARY_TBL, "PREFILTER", 0, &status);
	checkOrbitFileFITSerrors(status, "finding PREFILTER extension in", file);

	/*******************************
	* read number of rows in table *
	*******************************/
	fits_read_key_lng(file->fptr, "NAXIS2", &file->nrows, comment, &status);
	checkOrbitFileFITSerrors(status, "reading NAXIS2 from", file);

	/**********************
	* read column numbers *
	**********************/
	fits_get_colnum(file->fptr, CASESEN, "TIME",     &file->time_col, &status);
	fits_get_colnum(file->fptr, CASESEN, "POSITION", &file->pos_col,  &status);
	fits_get_colnum(file->fptr, CASESEN, "VELOCITY", &file->vel_col,  &status);
	checkOrbitFileFITSerrors(status, "locating columns in", file);

	/******************************************************************
	* mark the last search result as uninitialized
	* since we haven't done any searches yet
	* also allocate space for the resident quaternian structures
	* which are used to make repeated interpolations more efficient.
	*****************************************************************/
	file->search_row = UNINITIALIZED_ORBFILE_ROW;
	file->search_pvs_reliable = 0;


	/*********************************************************
	* read first and last times and set extrapolation limits 
	* we read the first time last to leave the FITSIO buffers  
	* holding the beginning of the file
	*********************************************************/
	file->tstop  = readTimeFromOrbitFile(file, file->nrows);
	file->tstart = readTimeFromOrbitFile(file, 1);

	file->duration = file->tstart - file->tstop;

	resetOrbitFileExtrapolationLimits(file, DEFAULT_ORBFILE_EXTRAPOLATION);

	return file;
}


/**************************************************************************
***************************************************************************
* close an attitude file and destroy the ORBFILE structure
**************************************************************************/
void closeOrbitFile (ORBFILE* file)
{
	int status=0;

	fits_close_file(file->fptr, &status);
	checkOrbitFileFITSerrors(status, "closing", file);

	free(file->name);
	free(file);
}



/**************************************************************************
***************************************************************************
* Read a generic quantity from the attitude file.
* This routine makes the FITSIO read call, checks for errors and 
* resets the search flags if needed.
**************************************************************************/
void readThingFromOrbitFile(ORBFILE* file, void* data, void* nullvalue,
                          int datatype,
                          long row, int col, long nelements)
{
	int anynull = 0;
	int status = 0;

	/********************
	* read the value(s) *
	********************/
	fits_read_col(file->fptr, datatype, col, row, 1, nelements, nullvalue,
					data, &anynull, &status);

	/*******************
	* check for errors *
	*******************/
	checkOrbitFileFITSerrors(status, "reading", file);

	if(anynull) {
    	fprintf(stderr,"Null values in column %d row %ld of %s\n",
            	col, row, file->name);
    	att_fatal(anynull);
	}


	/******************************************************************
	* reset the search row pointer if we have just read from a wildly
	* different part of the file
	*****************************************************************/
	if (file->search_row != UNINITIALIZED_ORBFILE_ROW
			&& abs(row - file->search_row) > DIST_FOR_ORBFILE_RESET) {
		file->search_row = UNINITIALIZED_ORBFILE_ROW;
		file->search_pvs_reliable = 0;
	}
}


/**************************************************************************
***************************************************************************
* Read the position/velocity from a given row of the table
**************************************************************************/
void readRecordFromOrbitFile(ORBFILE* file, PVrecord* pv, long row)
{
	double nullvalue = 0.;

	readThingFromOrbitFile(file, pv->pos, &nullvalue, TDOUBLE,
					row, file->pos_col, 3);

	readThingFromOrbitFile(file, pv->vel, &nullvalue, TDOUBLE,
					row, file->vel_col, 3);
}


/**************************************************************************
***************************************************************************
* Read the TIME from a given row of the table
**************************************************************************/
double readTimeFromOrbitFile (ORBFILE* file, long row)
{
	double nullvalue = 0.;
	double time;

	readThingFromOrbitFile(file, &time, &nullvalue, TDOUBLE,
					row, file->time_col, 1);

	return time;
}


/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file
* returns 1 if time is withing the first and last rows
* returns 0 otherwise
**************************************************************************/
int isInOrbitFile(ORBFILE* file, double time)
{
	return time >= file->tstart && time <= file->tstop;
}


/**************************************************************************
***************************************************************************
* checks if a given time is covered by an attitude file, or its 
* reasdonable extrapolation
* returns 1 if time is within extrrapolation limits
* returns 0 otherwise
* See also resetOrbitFileExtrapolationLimits.
* Note there are no internal checks for whether a time is covered by the 
* orbfile, these must be supplied by the calling program.
**************************************************************************/
int isInExtrapolatedOrbitFile (ORBFILE* file, double time)
{
	return time >= file->min_extrapolated && time <= file->max_extrapolated;
}


/**************************************************************************
***************************************************************************
* Changes the valid extrapolation time limits for the orbit file.
* The default values when the file is opened is 
* DEFAULT_ORBFILE_EXTRAPOLATION.
* See also isInExtrapolatedOrbitFile(ORBFILE* file, double time).
**************************************************************************/
void resetOrbitFileExtrapolationLimits (ORBFILE* file, double margin)
{
	file->min_extrapolated = file->tstart - margin;
	file->max_extrapolated = file->tstop  + margin;
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
long findTimeInOrbitFile (ORBFILE* file, double time)
{
	long row;
	double current_time;
	double last_time = 0.0;
	long row0, row1;
	double time0, time1;

#ifdef DEBUG
printf("findTimeInOrbitFile(%.3f)\n", time);
#endif

/************************
* are we extrapolating? *
************************/
	if(!isInOrbitFile(file, time) || time == file->tstop) {

#   ifdef DEBUG
    printf("extrapolating time=%.14g tstart=%.14g tstop=%.14g\n",
            time, file->tstart, file->tstop);
#   endif

    	/**********************
    	* need to extrapolate *
    	**********************/
    	if (time < file->tstart || file->nrows == 1)
			row = 1; /* before beginning */
    	else
			row = file->nrows - 1; /* after end */

		if (file->search_row != row) {
        	/*********************************************************
        	* this is different from the result of the last search
        	* so we need to reset some things 
        	*********************************************************/
			file->search_time0 = readTimeFromOrbitFile(file, row);
			if (file->nrows > 1)
				file->search_time1 = readTimeFromOrbitFile(file, row + 1);

			file->search_pvs_reliable = 0;
			file->search_row = row;
		}

#   ifdef DEBUG
printf("returning extrapolated\n");
#   endif

    	/********************
    	* return the answer *
    	********************/
    	return file->search_row;
	} 

	/**************************************************************
	* are we still in the same place we found in the last search? *
	**************************************************************/
	if (file->search_row != UNINITIALIZED_ORBFILE_ROW
			&& time >= file->search_time0
			&& time < file->search_time1)
		return file->search_row;


	/************************************************************
	* none of the easy answers worked, so we have to go looking 
	* through the table. The new search location is guaranteed to 
	* be different from the last one, so we need to mark the
	* last found quaternions as unreliable. 
	************************************************************/
	file->search_pvs_reliable = 0;

	/*********************************************************** 
	* determine where we should start an initial linear search *
	***********************************************************/
	row = (time - file->tstart) / file->duration * file->nrows + 1;

	/*******************************************************
	* make sure we haven't gone beyond the end of the file *
	*******************************************************/
	if (row < 1)
		row = 1;
	else if (row > file->nrows - 1)
		row = file->nrows - 1;
  
	/******************************************************************
	* try a linear search for the correct time value
	******************************************************************/
	current_time = readTimeFromOrbitFile(file,row);

	if(time == current_time) {
    	/***************************
    	* it's right, exactly here *
    	***************************/
    	file->search_time0 = current_time;
    	file->search_time1 = readTimeFromOrbitFile(file, row + 1);

    	file->search_row = row;

    	return file->search_row;

	}
	else if (time > current_time) {
		/*****************
		* search forward *
		*****************/
		long row_limit = row + ORBFILE_LOCAL_SEARCH_LIMIT;

		/*********
		* search *
		*********/
		while (time >= current_time && row < row_limit) {
			++row;
			last_time = current_time;
			current_time = readTimeFromOrbitFile(file, row);
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
			return file->search_row;
		}

	}
	else if (time < current_time) {
		/*******************
		* search backwards *
		*******************/
		long row_limit = row - ORBFILE_LOCAL_SEARCH_LIMIT;

		/*********
		* search *
		*********/
		while (time < current_time && row > row_limit) {
			--row;
			last_time = current_time;
			current_time = readTimeFromOrbitFile(file, row);
		}

		/***********************
		* check if we found it *
		***********************/
		if(time >= current_time) {
			/*****************************
			* we're at the point we want *
			*****************************/
			file->search_row = row;
			file->search_time0 = current_time;
			file->search_time1 = last_time;
			return file->search_row;
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
	}
	else {
    	/*********************************************************
    	* search between current position and beginning of table *
		*********************************************************/
		row0 = 1;
		time0 = file->tstart;

		row1 = row;
		time1 = current_time;
	}

	while (row1 - row0 > 1) {
		row = (row1 + row0) / 2;
		current_time = readTimeFromOrbitFile(file, row);

		if (current_time == time) {
			file->search_row = row;
			file->search_time0 = current_time;
			file->search_time1 = readTimeFromOrbitFile(file, row + 1);
			return file->search_row;
		}
		else if (current_time > time) {
			row1 = row;
			time1 = current_time;
		}
		else {
			row0 = row;
			time0 = current_time;
		}
	}

	/************************************************************************
	* when we get here we have found the point we were looking for by bisection
	************************************************************************/
	file->search_row = row0;
	file->search_time0 = time0;
	file->search_time1 = time1;

	return file->search_row;
}




/**************************************************************************
***************************************************************************
* Determines the position/velocity at an arbitrary time.
* Values are interpolated if necessary and things are optimized
* for the case of repeated calls to this routine with similar time values
**************************************************************************/
int findRecordInOrbitFile (ORBFILE* file, PVrecord* pv, double time)
{
	int found = 1;
	double hat;

	/********************************************
	* search for the correct place in the table *
	********************************************/
	findTimeInOrbitFile(file, time);

	/*****************************************************
	* check if we need to read a new pair of records *
	*****************************************************/
	if (!file->search_pvs_reliable) {
		/*********************************************************
		* read two values and calculate the "delta" between them *
		*********************************************************/

		readRecordFromOrbitFile(file, &file->search_pv0, file->search_row);

		if (file->nrows > 1)
			readRecordFromOrbitFile(file, &file->search_pv1, file->search_row + 1);

		file->search_pvs_reliable = 1;
	}


	/*******************************************************
	* if there's only one row we don't need to interpolate *
	*******************************************************/
	if (file->nrows == 1) {
		*pv = file->search_pv0;
		return found;
	}


	/***********************************************************
	* interpolate records
	* TODO: provide more sophisticated interpolation option(s)
	***********************************************************/
	hat = (time - file->search_time0)
			/ (file->search_time1 - file->search_time0);

	{
		int i;
		PVrecord * pv0 = &file->search_pv0;
		PVrecord * pv1 = &file->search_pv1;

		for (i = 0; i < 3; ++i) {
			pv->pos[i] = pv0->pos[i] + hat * (pv1->pos[i] - pv0->pos[i]);
			pv->vel[i] = pv0->vel[i] + hat * (pv1->vel[i] - pv0->vel[i]);
		}
	}

	return found;
}


void requireTimeNearOrbitFileRecord (ORBFILE * file, double limit)
{
    if (limit < 0) {
        /* turn off restriction */
        file->search_nearby = 0;
    }
    else {
        file->search_nearby = 1;
        file->limit_nearby = limit;
    }
}

