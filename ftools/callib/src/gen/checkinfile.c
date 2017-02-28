/* -------------------------------------------------------------------
Function:
     CheckInFile
 
Description:
 
     This routine checks the input file 
       If it exists, then return to calling function
       else,
          exits the program with error

Functions called:
    StatusChk 
 
    system functions:
      access  -- to check if the file exists
 
Author and modification history:
    Banashree M Seifert (July, 1997)
    Banashree M Seifert (September, 1997)
        . introduced for '-' in filename
        . if filename has extension no. then check is made 

 
Usage:
    CheckInFile(char *filename);
 
-----------------------------------------------------------------*/
#include <unistd.h>
#define FLEN_BUFFER 257
 
void CheckInFile(char *filename)
{
    char subinfo[100];
    char tfile[FLEN_BUFFER];

    int status=0, exist=0;
    int ExtnNo=0;

    if(filename[0] == '-') return;
    if(c_fcpars(filename, tfile, &ExtnNo, &status)) Printerror(status);

    if(exist=access(tfile,F_OK)){
      sprintf(subinfo,"....Error: file \"%s\" doesn't exist \n",tfile); 
      status=1;
      StatusChk(status,subinfo);
    }
    return ;

}
 
