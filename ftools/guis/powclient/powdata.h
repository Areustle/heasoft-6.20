#define POWDATA

#ifndef _POWDATA_H
#define _POWDATA_H


#include "tcl.h"
#include "tk.h"



#include <stdio.h>
#include <ctype.h>
#ifndef MAC_TCL
#   include <fcntl.h>
#endif
#   include <limits.h>
#include <math.h>
#   include <stdlib.h>
#include <string.h>

#if !(defined(__WIN32__) || defined(macintosh))
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#   include <unistd.h>
#endif

#define BYTE_DATA 0 /* unsigned char */
#define SHORTINT_DATA 1
#define INT_DATA 2
#define REAL_DATA 3
#define DOUBLE_DATA 4  
#define STRING_DATA 5

/* on some system , e.g. linux, SUNs DBL_MAX is in float.h */
#ifndef DBL_MAX
#include <float.h>
#endif

#ifndef DBL_MIN
#include <float.h>
#endif

/*  Sun4s do not support %p, so switch to %lx  */

#ifdef HEX_PTRFORMAT
#define PTRFORMAT "%lx"
#else
#define PTRFORMAT "%p"
#endif

extern int pixelSizes[5];

/* Typedef for a PowData structure.  This is the main way of getting
   data into TCL */

typedef struct PowData {
  char *data_name;  /* The identifier for this data known to TCL and the
                       calling program.  Also the hash key.                 */
  void *data_array; /* The array full of data.                              */
  int  data_type;  /* The actual type of the data Byte-0,2 Bytes-1,4 Bytes-2,
		      4 Bytes Real- 3, 8 Bytes Real- 4 (not fully 
		      supported for images), String - 5 (currently supported
                      as the "z-vector" for a curve only.   */
  int copy;    /*if non-zero, indicates that the data pointer "belongs" to
                 POW (i.e. the data was copied at creation time) and may
                 thus be 'ckfree'd */
  int length;       /* The number of elements in the array.                 */
}  PowData;


extern int pixelSizes[5];
extern int Pow_Done;
extern int tty;
/* extern int isatty _ANSI_ARGS_((int fd)); */


extern Tcl_Interp *interp;		/* Interpreter for application. */


extern Tcl_HashTable PowDataTable;

void PowCreateData(char *, void *, int *, int *, int *, int *);
void PowRegisterData(PowData *,int *);
void PowDestroyData(char *, int *);

PowData * PowFindData(char *);

int PowCreateDataFromChannel(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowCreateDataFromPtr(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowCreateStrFromPtr(ClientData, Tcl_Interp *, int, Tcl_Obj *const []);
int PowCreateDataFromList(ClientData, Tcl_Interp *, int, char **);

#endif

