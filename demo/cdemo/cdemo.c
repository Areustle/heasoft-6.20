#include <stdio.h>
#include <string.h>
#include "pil.h"

int printsub(const char *);

#define TOOLSUB cdemo
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

int	cdemo(void){
    int status=0;
    char msg[80];

    static char taskname[80] = "cdemo";
    static char version[8] = "6.66";

    /* Register taskname and version. If not set 
       explicitly a default value (= executable name) 
       is used. Retrieve these via get_toolname(),
       get_toolversion(), and/or get_toolnamev() */

    set_toolname(taskname);
    set_toolversion(version);
    
    /* XPI equivalent: Uclgst("text", msg, &status); */
    status=PILGetString("text", msg);
    if(status != 0){
	fprintf(stderr,"\nCould not get text parameter\n");
	return status;
    }
    
    printf("main: this text written via printf()\n");
    fprintf(stderr,"main: this text written via fprintf(stderr)\n");
    fprintf(heaout,"main: this text written via fprintf(heaout)\n");
    HD_printf("main: this text written via HD_printf()\n");
    
    printsub(msg);

    HD_printf("main: msg is %s, chatter is %d, clobber is %d\n", msg, headas_chatpar, headas_clobpar);

/* Note that while headas_chatpar is a global variable there is normally 
   no need for a task to access it directly (it's used internally by the 
   _chat() routines, see the associated file printsub.c for usage examples). 
   
   While developers do not need to read the "clobber" parameter themselves
   they do need to decide what to do about preexisting file(s) based on
   the value of the headas_clobpar global */

    return status;
}
