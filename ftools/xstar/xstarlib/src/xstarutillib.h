/*************************************************/
/* File:         xstarutillib.h                  */
/* Description:  Header file for xstarutillib.c  */
/*                                               */
/*************************************************/

#ifndef XSTARUTILLIB_H
#define XSTARUTILLIB_H

/* Generic Utility functions */
float** matrix_alloc(int, int);
void matrix_dealloc(float**, int, int);
void Flag2String(char*,int);
void PrintError(int status);
#endif
