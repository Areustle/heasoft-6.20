/* -------------------------------------------------------------------
Function:
     CheckFile
 
Description:
 
     This routine checks the output file for writing
     If iit already exists && clobber is true
         Then it deletes the pre existing file
     else
         Asks user whether to delete the existing file

Functions called:
 
    general utility functions:
      DispMsg    -- displays messages
      Printerror -- prints FITSIO error messages
 
    system functions:
      access  -- to check if the file exists
      remove  -- delete the pre-existing file
 
Author and modification history:
    Banashree M Seifert (July, 1997)
 
Usage:
    CheckFile(char *filename, int chatter, int clobber);
 
-----------------------------------------------------------------*/
#include <unistd.h>
#include "general.h"
 
void CheckFile(char *filename, int chatter, int clobber)
{
    char str[4], subinfo[100];
    int exist=0;

    if(filename[0] == '-') return ;
    exist=access(filename,F_OK);
    if(exist == 0) {
       if(clobber){
          if(remove(filename)){
             sprintf(subinfo," cannot erase file \"%s\"\n", filename);
             DispMsg(chatter, 0, subinfo);
             exit(1);
          }
       }else{
          printf("\n outfile \"%s\" exists. Erase it?[n]: ", filename);
          gets(str);
          if(toupper(*str) == 'Y'){
             if(remove(filename)){
               sprintf(subinfo," cannot erase file \"%s\"\n", filename);
               DispMsg(chatter, 0,subinfo);
               exit(1);
             }
          }else{
             sprintf(subinfo," Try another filename!\n");
             DispMsg(chatter, 0,subinfo);
             exit(1);
          }
       }
     }

    return ;

}
 
