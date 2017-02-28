#include <stdio.h>
#include "headas.h"

int printsub(const char *msg){
    int status=0;
     
    printf("printsub using printf(): %s\n", msg);
    fprintf(stderr,"printsub using fprintf(stderr): %s\n", msg);
    fprintf(heaout,"printsub using fprintf(heaout): %s\n", msg);
    HD_printf("printsub using HD_printf(): %s\n", msg);

    headas_chat(0,"This should be printed at chatter >= 0\n");
    headas_chat(1,"This should be printed at chatter >= 1\n");
    headas_chat(2,"This should be printed at chatter >= 2\n");
    headas_chat(3,"msg is %s (This should be printed at chatter >= 3)\n", msg);
    headas_chat(4,"This should be printed at chatter >= 4\n");
    headas_chat(5,"This should be printed at chatter==5 only\n");
    
    return status;
}
