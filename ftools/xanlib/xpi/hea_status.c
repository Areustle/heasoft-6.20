/*
 *   Description: Manager the global status variable in HEASOFT
 *                package. 
 *
 *
 *   Author/Date: Ning Gan,  RSTX,  April 24, 2000
 */
#include "cfortran.h"
#include "hea_status.h"

    static int HEAStatus = 0;      /* HEASoft error status */

/*  Reset the error status */
    void ResetHEAStatus()      
    {
        HEAStatus = 0;
        return;
    }       

/*  Set the non-zero error status. For the zero status, do nothing.    
    If you try to set the status to zero, call the ResetHEAStatus
    instead. */

    void SetHEAStatus(int status)      
    {
        if(!status) return;    /* prevent the HEAStatus to be reset to 0*/
        HEAStatus = status;
        return;
    }       

/*  Get the error status */   
    int GetHEAStatus(int *status)      
    {
        if (0 != status) *status = HEAStatus;
        return HEAStatus;
    }       

/*  Check the error status. If it is an error, then exit it imediately. */   
    void HEATermination()      
    {
        if(HEAStatus) exit(HEAStatus);
        return;
    } 


FCALLSCSUB0 (ResetHEAStatus,RESETHEASTATUS,resetheastatus)
FCALLSCSUB1 (SetHEAStatus,SETHEASTATUS,setheastatus,INT)
FCALLSCSUB1 (GetHEAStatus,GETHEASTATUS,getheastatus,PINT)
FCALLSCSUB0 (HEATermination,HEATermination,heatermination)
       
