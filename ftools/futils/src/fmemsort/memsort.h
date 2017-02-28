/* Define the Fortran compiler for cfortran */

/* #define sunFortran */
/* #define DECFortran */
/* #define vmsFortran */
/* #define mipsFortran */

#include "cfitsio.h"
#include "ftools.h"

/* The VMS Linker is case insensative so must change the names 
 * of FORTRAN calls C routines */

#ifdef vms
#define memsort memsort_
#define setload setload_
#endif

/* Define some macros */

#define myTRUE           1
#define myFALSE          0 

#define FITS_LOGICAL    14
#define FITS_ASCII      16
#define FITS_BIT         1
#define FITS_BYTE       11
#define FITS_SHORT      21
#define FITS_LONG       41
#define FITS_SINGLE     42
#define FITS_DOUBLE     82
#define FITS_COMPLEX    83
#define FITS_DCOMPLEX  163

/* Define a useful data type for generic FITS data fields */

typedef struct complex_data_types {
                                    double real;
                                    double imag;
                                  } complex;

typedef union fits_data_types {
                                char **sptr;
                                int *iptr;
                                double *dptr;
                                complex *cptr;
                              } fits_array;

typedef struct fits_column_data {
                                  fits_array data;
                                  int *undef_flags;
                                  int dtype;
                                } column;
