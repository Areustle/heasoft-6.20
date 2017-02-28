/*
 *	xrt_termio.h: --- terminal input/output module ---
 *
 *	DESCRIPTION:
 *		A module comprising functions for general terminal I/O, e.g. issuing
 *		messages to <stderr>, user-friendly reading of values from <stdin>,
 *		and things like that ...
 *
 *	AUTHOR:
 *	 	UL, Mon Jul 11 20:09:12 METDST 1994
 *
 */

#ifndef XRT_TERMIO_H
#define XRT_TERMIO_H

		    /********************************/
                    /*        header files          */
                    /********************************/

#include <ctype.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fitsio.h>
#include <basic.h>
#include <headas_utils.h>
#include <headas_stdio.h>
#define OK 0
#define NOT_OK 1
#define BUF_SIZE 4096 
					/********************************/
					/*      defines / typedefs      */
					/********************************/

#define MAX_FILES      50
#define BOOL int

typedef enum { MUTE=0, NORMAL=2, CHATTY=4 } Verbosity_t;

typedef enum {	NO_ERR=0,			/* fine							*/
				FOPEN_ERR,		/* fopen failed					*/
				FILE_SYS_ERR,	/* general file system failure	*/
				MALLOC_ERR,		/* [mc]alloc failed				*/
				LIBR_ERR,		/* library error occured		*/
				MATCH_ERR,		/* matching error occured		*/
				ILL_ERR,		/* illegal condition occured	*/
				EXCEED_ERR,		/* limit exceeded				*/
				INPUT_ERR,		/* input error					*/
				SIGNAL_ERR,		/* external signal occured		*/
				MISC_ERR		/* miscellaneous				*/
			 } Error_t;

					/********************************/
					/*           globals            */
					/********************************/

					/********************************/
					/*      function prototypes     */
					/********************************/

extern void SetVerboseLevel	(const Verbosity_t VerboseLevel);

extern void Error		(const char *info,
				 const char *fmt,
				 ...);

extern void SysError		(const char *info,
				 const char *fmt,
				 ...);

extern void Info		(const Verbosity_t VerboseLevel,
				 const char *fmt,
				 ...);

extern void Infolines		(const Verbosity_t VerboseLevel,
				 const char *fmt,
				 ...);

extern void Message		(const char *fmt,
				 ...);

extern void SetVerboseLevel	(const Verbosity_t VerboseLevel);

extern unsigned SplitLines	(const char *Line,
				 unsigned length,
				 char *(**StrVec)[]);
extern void ReadInFileNamesFile(char *InFileNames);
extern void FitsioError(int status);
extern void PrgExit(const Error_t error);
extern int CopyFile(char *InFileNames,char *OutFileNames);
#endif



