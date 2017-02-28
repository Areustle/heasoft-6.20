/*
 * $Source: /headas/headas/attitude/tasks/tristarid/SUtil.h,v $
 * $Revision: 1.1 $
 * $Date: 2005/08/27 12:52:23 $
 *
 * $Log: SUtil.h,v $
 * Revision 1.1  2005/08/27 12:52:23  wiegand
 * Initial revision
 *
 */


#ifndef TRISTARID_STRING_H
#define TRISTARID_STRING_H


char * string_copy (const char * s);

int string_split (char * s, char * fields[], int nfields, const char * sep);

#endif
