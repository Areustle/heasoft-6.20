/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/timeconv/timeconv.h,v 3.8 1996/07/18 15:41:24 miket Exp $   */
/*                   */
/* Define the Fortran compiler for cfortran */

/* #define sunFortran */
/* #define DECFortran */
/* #define vmsFortran */
/* #define mipsFortran */

#include "cfortran.h"

/* The VMS Linker is case insensative so must change the names 
 * of FORTRAN calls C routines */

#ifdef vms
#define asca_geocen asca_geocen_
#define asca_geocen_init asca_geocen_init_
#endif
