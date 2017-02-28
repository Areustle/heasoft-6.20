/****************************************************************************
* Function:
*      c_timestamp
*
* DESCRIPTION:
*      Write a history date/time stamp in the current hdu.
*
* AUTHOR:
*       Ning Gan
*       Raytheon STX
*       July, 1998
*
* MODIFICATION HISTORY:
*
* NOTES:
*     If the task common block is populated, the name of the task is
*     included in the history record. The time is in UTC.  It is a 
*     c version of the "timestamp" fortran routine.
*
******************************************************************************/
#include <string.h>
#include <fitsio.h>
#include <cftools.h>
     int  c_timestamp(fitsfile *fptr /* fitsfile  */)
{
    
    char history[FLEN_VALUE];
    char task[C_TASKNAME];
    int timeref;
    char date_str[FLEN_VALUE]; 
    int status = 0;


    /* get the task name */
    c_gtaskn(task);

    /* get the current date */
    if(fits_get_system_time(date_str, &timeref, &status )) { 
	fits_report_error(stderr,status);
        return status;
    }

    /* get the history string */
    strcpy(history, task);
    strcat(history, " at ");
    strcat(history, date_str); 
    if(timeref == 0) strcat(history,"UTC");

    /*write out the history */
    if(fits_write_history(fptr, history, &status )) { 
	fits_report_error(stderr,status);
        return status;
    }
    return 0;
}
