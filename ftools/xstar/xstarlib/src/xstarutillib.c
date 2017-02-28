/*
   File:   xstarutillib.c
   Author: W.T. Bridgman, RITSS
   Date:   January 1999

   Description:


   Caveats & other potential gotchas:

   Known Platform-Dependent Issues:
   --------------------------------
   * sprintf() is implemented as the standard on OSF compiler (returns an
     int) but NOT on the Sun compiler (returns char*).  Therefore, do not
     use the return value from this function.
   * Always assume that string literals are being written to read-only
     memory.  Do not try to modify them.

-----------------------------------------------------------------------

   To Do:
   ------

-----------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio2.h"     /* cfitsio defined constants             */
#include "fitsio.h" 

#include "xstarutillib.h"

#define ERRMSG 255

/********************************************************
 *  matrix_alloc:                                       *
 *                                                      *
 *  Abstract:                                           *
 *     Allocate a 2-dimensional matrix.                 *
 *                                                      *
 *  Input Parameters:                                   *
 *     nrows    = number of rows                        *
 *     ncolumns = number of columns                     *
 *                                                      *
 *  Returns:                                            *
 *     Pointer to matrix                                *
 ********************************************************/
float **matrix_alloc(int nrows, int ncolumns){
  float** matrix;
  int     ir,ic;

  matrix=(float**)malloc((unsigned)nrows*sizeof(float*));
  if(!matrix){
    c_fcerr("matrix_alloc: Matrix allocation failure 1");
    exit(EXIT_FAILURE);
  } else {
    for(ir=0;ir<nrows;ir++){
      matrix[ir]=(float*)malloc((unsigned)ncolumns*sizeof(float));
      if(!matrix[ir]){
        c_fcerr("matrix_alloc: Matrix allocation failure 2");
        exit(EXIT_FAILURE);
      } else {
        /* initialize the matrix */
        for(ic=0;ic<ncolumns;ic++) { 
	  matrix[ir][ic]=0.0;
	}
      }
    }
  }
  return(matrix);
}
/********************************************************
 *  matrix_dealloc:                                     *
 *                                                      *
 *  Abstract:                                           *
 *     De-allocate a 2-dimensional matrix               *
 *                                                      *
 *  Input Parameters:                                   *
 *     nrows    = number of rows                        *
 *     ncolumns = number of columns                     *
 *                                                      *
 ********************************************************/
void matrix_dealloc(float** m, int nrows, int ncolumns) {
   int ir;
   char msg[ERRMSG];

   for(ir=0; ir<nrows; ir++){ 
      if(m[ir]!=(float*)NULL) free(m[ir]);
      else {
	sprintf(msg,"matrix_dealloc: NULL pointer at index %5d of %5d.",ir,nrows);
	c_fcerr(msg);
      }
   }
   if(m!=(float**)NULL) free(m);
   else c_fcerr("matrix_dealloc: NULL pointer at top level.");
}

/********************************************************
 *  Flag2String:                                        *
 *                                                      *
 *  Abstract:                                           *
 *    Map (0,1) to ("No","Yes")                         *
 *    Note this routine does not allocate memory for    *
 *    the output string.                                *
 ********************************************************/
void Flag2String(char* outstr, int value){
  char msg[ERRMSG];

  switch(value){
  case 0:
    strcpy(outstr,"No");
    break;
  case 1:
    strcpy(outstr,"Yes");
    break;
  default:
    sprintf(msg,"Flag2String: Flag value %d is not 0 or 1.",value);
    c_fcerr(msg);
    strcpy(outstr,"Undefined");
  }
}
/********************************************************
 *  PrintError:                                         *
 *                                                      *
 *  Abstract:                                           *
 *     Print the error message from CFITSIO & exit      *
 *                                                      *
 *  Input Parameters:                                   *
 *     status = status code returned by CFITSIO         *
 *                                                      *
 ********************************************************/
void PrintError( int status)
{
    char status_str[FLEN_STATUS], errmsg[FLEN_ERRMSG];
  
    if (status)
      fprintf(stderr, "\n*** Error occurred during program execution ***\n");

    fits_get_errstatus(status, status_str);   /* get the error description */
    fprintf(stderr, "\nstatus = %d: %s\n", status, status_str);

    /* get first message; null if stack is empty */
    if ( fits_read_errmsg(errmsg) ) 
    {
         fprintf(stderr, "\nError message stack:\n");
         fprintf(stderr, " %s\n", errmsg);

         while ( fits_read_errmsg(errmsg) )  /* get remaining messages */
             fprintf(stderr, " %s\n", errmsg);
    }

    exit(EXIT_FAILURE);       /* terminate the program, returning error status */
}
