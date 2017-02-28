/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/faint/faint.h,v 3.8 1996/07/18 15:40:51 miket Exp $   */
/*                   */
/* Define the Fortran compiler for cfortran */

/* #define sunFortran */
/* #define DECFortran */
/* #define vmsFortran */
/* #define mipsFortran */

#include "cfortran.h"

/* The VMS Linker is case insensitive so must change the names 
 * of FORTRAN calls C routines */

#ifdef vms
#define faint_v31 faint_v31_
#define faint_v40 faint_v40_
#endif
