#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "headas_error.h"

int parseLong (char * string, int* n, long * array, char * tokens,int * status)

{

    char * ptr;
    char msg[256];
   

    if (*status ) return *status;

    *n =0;

    ptr = string;

    while(*ptr != '\0')
    { 
      if( (*ptr > '9' || *ptr < '0') && (*ptr != ',') ) { 
        sprintf(msg, "ERROR: naxes parameter contains non-numeric characters");
        *status = 1;
        HD_ERROR_THROW(msg,*status);
        return *status;
      }
      ptr++;
    }

    ptr = strtok(string,tokens);

    if( ptr ) {
         array[*n] = atol(ptr);
         (*n)++;
    }
    else  {
         return *status;
    }
  
    while ( (ptr=strtok(NULL,tokens))) 
    {
         array[*n] = atol(ptr);
         (*n)++;
    }

    return *status;

}
