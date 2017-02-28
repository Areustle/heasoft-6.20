#include "xpi.h"
#include "xpi_wrappers.h"

/****************************************************************************
* This file contains a number of routines for readging and writing to
* the parameter file. These are basicly just wrappers for the 
* "xpi" routines found in the xanlib FTOOLS library.
* These wrappers are used for several reasons:
*
* - if someone wanted to replace the xpi routines with private routines
*   the would only have to re-write the routines in this file.
*
* - the wrappers have less cryptic C-like names
*
* - they conveniently include error checking.
*
* - the xpi.h header file is very sensitive to namespcae collisions with
*   other defines. These can have mysterious and unleasant side-effects.
*   We avoid this problem by separating the xpi calls into this file
*   which has no pre-processor "includes" other than xpi.h
*
*****************************************************************************/

/*************************************************************************
***************************************************************************
* error handler for xpi routines
***************************************************************************/
void check_for_xpi_errors(int status,char* doing, char* name) 
{

   if(!status) return;

   fprintf(stderr,"Couldn't %s paramater %s status=%d\n",
           doing,name,status);

   exit(status);

}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
void read_string_param(char* name, char* value, int dimen) 
{
   int status=0;
   int BufLen_2; /* xpi C - FORTRAN interface */

   BufLen_2=dimen-1;

   Uclgst(name,value,&status);
   check_for_xpi_errors(status,"read",name);

}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
double read_double_param(char* name) 
{
   int status=0;
   double value=0.;

   Uclgsd(name,&value,&status);
   check_for_xpi_errors(status,"read",name);

   return(value);
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
int read_int_param(char* name) 
{
   int status=0;
   int value;

   Uclgsi(name,&value,&status);
   check_for_xpi_errors(status,"read",name);


   return(value);
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
int read_boolean_param(char* name) 
{
   int status=0;
   int value;

   Uclgsb(name,&value,&status);
   check_for_xpi_errors(status,"read",name);


   return(value);
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
void write_int_param(char* name, int value) 
{
   int status=0;

   Uclpsi(name,value,&status);
   check_for_xpi_errors(status,"write",name);


}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
void write_double_param(char* name, double value) 
{
   int status=0;

   Uclpsd(name,value,&status);
   check_for_xpi_errors(status,"write",name);


}






