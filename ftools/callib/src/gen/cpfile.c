#include <string.h>

#include "caltools.h"

cpfile(file1,file2) 
char *file1,*file2;
{

    char command[200];
    int errstat;

#ifdef VMS

    strcpy(command,"copy\0");
#endif

#ifdef unix

    strcpy(command,"cp\0");
#endif

    strcat(command," \0");
    strcat(command,file1);
    strcat(command," \0");
    strcat(command,file2);
    Cspawn(command,strlen(command),&errstat);
}
