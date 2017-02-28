#ifndef PILL_H
#define PILL_H

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/pill.h,v $
 * $Revision: 1.2 $
 * $Date: 2002/12/06 20:14:21 $
 */

int PILLGetInteger (const char * parameter, int * output);
int PILLGetReal (const char * parameter, double * output);
int PILLGetBool (const char * parameter, int * output);
int PILLGetString (const char * parameter, char * output);
int PILLGetFname (const char * parameter, char * output);

int PILLGetRealVector (const char * parameter, int length, double * output);

#endif
