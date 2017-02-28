/* ---------------------------------------------------------------------------------------
Function:
     FileExists
 
Description:
 
     This routine checks the output file for writing
     If iit already exists && clobber is true
         Then it deletes the pre existing file
     else
         Asks user whether to delete the existing file

Functions called:
 
    system functions:
      access                -- to check if the file exists
      remove                -- delete the pre-existing file
 
Author and modification history:
    Sandhia Bansal      Jan 2002      Modified to be used with GRO ftools 
    Sandhia Bansal      (Jan,  1999)  Deleted fileRef (new RIL library)
    Sandhia Bansal      (Dec,  1998)  Changed name from CheckFile to CheckFile to keep it
                                      separate from CheckFile routine which is in CALLIB. 
				      The CheckFile routine should not be needed once all
				      programs are using DAL.
    Banashree M Seifert (July, 1997)
 
Usage:
    int FileExists(outputParsPtr outputPtr);
 
----------------------------------------------------------------------------------------*/
#include <unistd.h>
#include <stdio.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "xpi.h"


void FileExists(char *fname, int *clobber, int *status);


 
void FileExists(char *fname, int *clobber, int *status)
{
    int exist=0;

    char msg[128];
    char str[4];


    if ((fname[0] == '-') || 
	(fname[0] == ' ') ||(strcasecmp(fname, "none") == 0))
    {
       strcpy(fname, "\0");
    }

    else
    {
       exist=access(fname,F_OK);
       if(exist == 0) 
       {
          if(*clobber)
          {
	     remove(fname);
          }

          else
          {
             sprintf(msg, "file %s exists.  Erase it?[n]: ", fname);
	     Fcerr(msg);
             fgets(str, sizeof(str), stdin);
             if(toupper(*str) == 'Y')
             {
	        remove(fname);
             }

	     else
             {
		strcpy(msg, "Try another filename!");
	        Fcerr(msg);
  	        *status = 1;  
             }
          }
       }
    }
}
 
FCALLSCSUB3(FileExists, FILEEXISTS, fileexists, PSTRING, PINT, PINT)
