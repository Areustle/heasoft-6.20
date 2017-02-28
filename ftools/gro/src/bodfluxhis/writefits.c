#include <string.h>
#include <stdio.h>
#include "bodfluxhis.h"

void  writefits(int *nrows, float *StartTime,  float *Flux, 
      float *FluxErr, char *filename, int *binsz)
{

    fitsfile *fptr;       /* pointer to the FITS file*/
    int status = 0, hdutype,ii, slen;
    long firstrow, firstelem, *time=NULL;
    int tfields   = 3;       /* table will have 3 columns */
    char extname[] = "BATSE_SRC_HIS";           /* extension name */
    char modeKyw[10];
    

    /* define the name, datatype, and physical units for the 3 columns */
    char *ttype[] = { "Time", "RATE",    "ERROR"      };
    char *tform[] = { "1D",        "1E",        "1E"         };
    char *tunit[] = { "D",       "SEC",    "ph/cm2-sec"    };
  
    modeKyw[10]='\0'; 
    if (*binsz == 1)
      {
	slen=strlen("EVENT");
       strcpy(modeKyw,"EVENT");
       modeKyw[slen] ='\0'; 
      }
    else
      { 
	slen=strlen("BINNED");
       strcpy(modeKyw, "BINNED");
       modeKyw[slen] ='\0' ;
      }

    /* Delete old file if it already exists */
    remove(filename);    
           
    /* open the FITS file containing a primary array and an ASCII table */
    if ( fits_create_file(&fptr, filename, &status)) 
	{
          printf ("Problem creating FITs file!");
          exit (1);
	}

    /* append a new empty binary table onto the FITS file */
    if ( fits_create_tbl( fptr, BINARY_TBL, *nrows, tfields, ttype, 
         tform,tunit, extname, &status) )
        {
          printf ("Problem creating binary table!");
          exit (1);
	}
    /*update keywords */
    ffukys(fptr, "TTYPE1","TIME", "Barycenter corrected time of measurement",
           &status);
    if (status != 0)
     {
          printf ("Problem updating keyword TTYPE1!");
          exit (1);
	}

    ffukys(fptr, "TFORM1","1D", "data format: double precision float",
           &status);
    if (status != 0)
     {
          printf ("Problem updating keyword TFORM1!");
          exit (1);
	}

     ffukys(fptr, "TUNIT1","d", "physical unit of field",
           &status);
    if (status != 0)
     {
          printf ("Problem updating keyword TUNIT1!");
          exit (1);
	}
  
    /*write keywords */
    ffpkyf(fptr,"MJDREF", 2440000.5,1,"MJD reference", &status);
    if (status != 0)
     {
          printf ("Problem writing keyword MJDREF!");
          exit (1);
	}
  
    ffpkys(fptr,"TIMESYS","TJD", "The time system is Julian Days", 
        &status);
    if (status != 0)
      {
          printf ("Problem writing keyword TIMESYS!");
          exit (1);
	  }

    ffpkys(fptr,"TIMEUNIT","d", "physical unit for TSTART, TSTOP",
     &status);
    if (status != 0) 
     {
          printf ("Problem writing keyword TIMEUNIT!");
          exit (1);
	  }
    
    ffpkys(fptr,"HDUCLAS1","BINNED", "Data mode (event or binned)", 
    &status);
    if (status != 0) 
      {
          printf ("Problem writing keyword HDUCLAS1!");
          exit (1);
	  }

    ffpkyj(fptr,"TSTART", (int)(StartTime[0]), "observation start time", 
           &status); 
    if (status != 0)
        {
          printf ("Problem writing keyword TSTART!");
          exit (1);
	  }
     
    ffpkyj(fptr,"TSTOP", (int)(StartTime[*nrows-1]), 
       "observation end time",  &status);
    if (status != 0)
     {
          printf ("Problem writing keyword TSTOP!");
          exit (1);
	  }
     
    status = 0;
    firstrow  = 1;  /* first row in table to write   */
    firstelem = 1;  /* first element in row  (ignored in ASCII tables) */
    
    time = (long *) malloc((long)(*nrows) * sizeof(long));
    for (ii=0;ii<*nrows;ii++) time[ii] = (int)StartTime[ii];
    
    /* write binary table */
    fits_write_col(fptr, TFLOAT, 1, firstrow, firstelem, *nrows,StartTime,
                   &status);
    fits_write_col(fptr, TFLOAT, 2, firstrow, firstelem, *nrows, Flux,
                   &status);
    fits_write_col(fptr, TFLOAT, 3, firstrow, firstelem, *nrows, FluxErr,
                   &status);

    status = 0;
    if ( fits_close_file(fptr, &status) )       /* close the FITS file */
        {
          printf ("Problem closing created FITs file!");
          exit (1);
	}
    fits_report_error(stderr, status);
    finish: return;
}
