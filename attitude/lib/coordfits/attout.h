#ifndef ATTOUT_INCLUDED

#include "fitsio.h"
#include "longnam.h"
#include "coord.h"

/***************************************************************************
* The ATTOUT structure is used for creating new attitude files.
***************************************************************************/
typedef struct {

char* name;
fitsfile* fp;

int time_col;
int q_col;

long nbuffer;
double* time;
double** q;

int row;
long file_row;

} ATTOUT;


/*****************************************************************************
******************************************************************************
* Create a new ATTOUT output attitude file structure
* This functions creates a structure and opens a corresponding FITS file
* for writing
*****************************************************************************/
ATTOUT* createAttOut(char* filename);

/*****************************************************************************
******************************************************************************
* write the TELESCOP keyword in the current and primary HDUs of the file.
* This is not a terribly efficient routine, but that doesn't matter 
* if it is called only once or twice.
*****************************************************************************/
void setAttOutMission(ATTOUT* att, char* mission);

/*****************************************************************************
******************************************************************************
* write a single row to the attitude file via the buffers
*****************************************************************************/
void addAttOutRow(ATTOUT* att, double time, QUAT* q );

/*****************************************************************************
******************************************************************************
* write the data in the ATTOUT buffers to the file
*****************************************************************************/
void flushAttOut(ATTOUT* att);

/*****************************************************************************
******************************************************************************
* do everything to close an output attitude except actually closing the
* FITS file. The ATTOUT buffers are flushed, the FITSIO buffers are flushed,
* the NASXIS2 keyword is updated, and the att structure is destroyed.
* A pointer to the fitsfile structurte is returned so that
* another application can read this file. This is useful for memory
* resident files where closing the fistfile deletes the "file".
* it can also save some overhead of closing and reopening a regular disk file.
*****************************************************************************/
fitsfile* finishAttOut(ATTOUT* att);

/*****************************************************************************
******************************************************************************
* close an output attitude file and destroy the corresponding ATTOUT structure
* note this function must be called or the FITSIO buffers may not
* be flushed and the file may not be complete
*****************************************************************************/
void closeAttOut(ATTOUT* att);

/*****************************************************************************
******************************************************************************
* Handle CFITSIO errors
*****************************************************************************/
void checkAttOutFITSerrors(int status, char* doing, ATTOUT* att);


#define ATTOUT_INCLUDED
#endif /* ATTOUT_INCLUDED */
