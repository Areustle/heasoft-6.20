/*
 *	CalReadPsfFile.h: definitons and declarations with 
 *                              global relevance
 *
 *
 *	AUTHOR:
 *        Italian Swift Archive Center (FRASCATI)
 *     
 */

#ifndef CALREADPSFFILE_H
#define CALREADPSFFILE_H


#include "xrtcaldb.h"
#include "CalGetFileName.h"


			/********************************/
			/*      defines / typedefs      */
			/********************************/
extern int CalReadPsfFile(int FromCaldb,          
			  const char *CalFileName,
			  PsfPar_t *PsfParm,
			  int OnAxis);

#endif

