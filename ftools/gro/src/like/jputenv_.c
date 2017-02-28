/*  int jputenv_(char *string,int jstrlen) 				*/
/*					 				*/
/*					 				*/
/*  $Id: jputenv_.c,v 1.1 2002/04/16 20:27:35 irby Exp $		*/
/* 					 				*/
/*  Purpose: interface between FORTRAN and C function putenv(char*)	*/
/* 					 				*/
/*  Effect:  Adds or replaces contents of string in the program 	*/
/*	     environment						*/
/*  Programmer: J.A.Esposito delivered 29 AUG 1995			*/
/* 					 				*/
/* 					 				*/
/*  $Log: jputenv_.c,v $
/*  Revision 1.1  2002/04/16 20:27:35  irby
/*  New GRO tool 'like'.  Submitted by S.Bansal.
/*
 * Revision 5.5  1998/06/10  15:32:23  jae
 * Fixed minor non-code typo
 *
 * Revision 5.4  1996/03/07  16:37:30  jae
 * The code to change the environment matrix
 * has been commented out for a test of
 * why cnfcal.f doesn't see the CALIB_DIR
 * environment variable
 *
 * Revision 5.3  1996/03/07  16:01:30  jae
 * added realloc(environ[j],tsize) to make sure
 * new string fits in the environment array.
 *
 * Revision 5.2  1996/03/05  18:07:21  jae
 * Function has been changed to directly affect the char **environ variable
 * The character string is placed with both putchar AND by direct insertion
 * in *environ[k].
 *
 * Revision 5.1  1996/02/29  20:54:18  jae
 * *** empty log message ***
 *
 * Revision 5.0  1996/02/13  21:57:03  jae
 * Subroutine Module for like V5.00
 *                					 	*/
/* 					 				*/
/* 					 				*/
#include "like.h"

static char *Id;
extern char **environ;

int jputenv_(char *string,int jstrlen)
{
   int i=-1,j,k,l;
   char *p;

   Id = "$Id: jputenv_.c,v 1.1 2002/04/16 20:27:35 irby Exp $";


   string[jstrlen-1]='\0';
   i=putenv(string);

   /*j=-1;
     while(++j < 1000){
     p = environ[j];
     k=0;
     l = 0;
     if(p[k] = '\0')break;
     while(p[k++] != '='){
     if(p[k] != string[k])l=1;
     }
     if(l == 0){
     realloc(environ[j],jstrlen);
     environ[j]=string;
     return(i);
     }
     }*/

   return(i);
}
