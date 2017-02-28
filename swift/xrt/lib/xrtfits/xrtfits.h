/*
 *   xrtfits.h:   definitons and declarations with global relevance
 *
 *  
 *   EXPORTED FUNCTIONS:
 *     IsXRT_Header(const FitsHeader_t *Header );
 *     IsXRT_File(FitsFileUnit_t unit);
 *	     
 *   MACROS:
 *     ISSTANDARDFNAME(char *fname);
 *
 *   AUTHOR:     
 *            ISAC 
 *   
 * 
 */   
                 
#ifndef XRTFITS_H
#define XRTFITS_H

#include "basic.h"
#include <fitsio.h>
#include <longnam.h>
#include "highfits.h"
#include "xrtdefs.h"
#include "xrt_termio.h"
#include "headas_stdio.h"

#define XRTSUFFIXFLEN 14

#define ISSTANDARDFNAME(a) \
          ( strlen(a)>XRTSUFFIXFLEN && \
            strncmp(a,"sw",2)==0    && \
            a[XRTSUFFIXFLEN-1]=='x' )

#define ISXRTFILE_WARNING(d,a,b,c) \
/*                                        \
 * d - int              ChattyNumber      \
 * a - FitsFileUnit_t   *FitsFileUnit	  \
 * b - char             *FileName	  \
 * c - char             *TaskName	  \
 */                                       \
if ( !IsXRT_File( a ) ) {                 \
  headas_chat(d,"%s: Warning: `%s' is not a Swift/XRT file, task could work not properly\n",c,b);  \
}

#define ISXRTFILE_ERROR(a,b,c,e) \
/*                                        \
 * a - FitsFileUnit_t   *FitsFileUnit	  \
 * b - char             *FileName	  \
 * c - char             *TaskName	  \
 * e - int              status            \
 */                                       \
if ( !IsXRT_File( a ) ) {                 \
  headas_chat(MUTE,"%s: Error: `%s' is not a Swift/XRT file\n",c,b);  \
  e = 1;            \
}

BOOL IsXRT_Header(const FitsHeader_t *Header );
BOOL IsXRT_File(FitsFileUnit_t unit);
BOOL ObsIdCkeck(const FitsHeader_t *Header, char *Obs_Id);
BOOL BitSet ( Bintable_t table, unsigned RawNb, unsigned ColNb, int bitnumb);


/*-----------------------------------------------------------------------------
 *	SYNOPSIS:
 *             void AddDate(fitsfile *);
 *             void Addlongstr(fitsfile *, char * , char * , char * );
 *
 *	DESCRIPTION:
 *	      Add Keywords in a FITS File
 *
 *	PARAMETERS:
 *		
 *		
 *
 *	RETURNS:
 *		
 *---------------------------------------------------------------------------*/
void AddDate(fitsfile *);
void Addlongstr(fitsfile *, char * , char * , char * );
void ModifyColCom(fitsfile *, char *, char *);
BOOL IsThisEvDatamode(char *filename, char *datamode, BOOL *ret);
BOOL ReadDatamode(char *filename, char *datamode);

#endif

