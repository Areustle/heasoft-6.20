#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int parseDouble (char * string, int* n, double * array, char * tokens,int * status)

{

    char * ptr;
    int llen;

    if (*status ) return *status;

          /* replace CR and newline chars at end of line with nulls */
    llen = strlen(string);
    if ((llen > 0) && (string[llen-1]=='\n' || string[llen-1] == '\r')) {
          string[--llen] = '\0';

          if ((llen > 0) && (string[llen-1]=='\n' || string[llen-1] == '\r')) {
                 string[--llen] = '\0';
          }
      }

    *n =0;

    ptr = strtok(string,tokens);

    if( ptr ) {

         array[*n] = atof(ptr);
         (*n)++;
    }
    else  {
         return *status;
    }
  

    while ( (ptr=strtok(NULL,tokens)) != NULL) 
    {
         array[*n] = atof(ptr);
         (*n)++;
    }
 	

    return *status;

}
     
