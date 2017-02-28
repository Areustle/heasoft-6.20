/*
   This routine returns one record from an HK file of the format TIME,
   NAME, VALUE.  If the return value is non-zero, an error occured.

   Author: Emily A. Greene
           Hughes STX/NASA/GSFC
	   April, 1994

   Modified by Jeff Guerber, RSTX/GSFC, Jan. 1998.  Replaced cfortran FCECHO
   with native-C c_fcecho.
*/

#include "mkfilter.h"
#include "cftools.h"
#define DEBUG1

int mkf2GetHK (unit, row, tcolno, ncolno, vcolno, time, name, value)
     int unit;    /* The unit number of the HK file being read */
     int row;     /* The number of the row to read */
     int tcolno;  /* The time column number */
     int ncolno;  /* The name column number */
     int vcolno;  /* The value column number */
     double *time; /* The returned time in the requested row */
     char *name;   /* The returned name of the HK parameter in this row */
/*     char name[15]; /*   The returned name of the HK parameter in this row */
     int *value;   /* The returned value of the HK parameter in this row */

{
  int flag[1];
  int anyf;
  int status;
  int nelem;
  int felem;
  char context[80], strval[1026];

/* Get the time for this row */

  nelem = 1;
  felem = 1;
  status = 0;
  flag[0] = 0;
  anyf = 0;
  FTGCFD(unit, tcolno, row, nelem, felem, time, flag, anyf, status);

  if (status != 0)
    {

#ifdef DEBUG
      fprintf ( stderr, "time= %lf, flag = %d, anyf=%d, status = %d \n",
		*time, flag[0], anyf, status );
#endif

      if      ( unit == 50 ) sprintf( context, " Undefined TIME: S0_HK, row=%d", row );
      else if ( unit == 51 ) sprintf( context, " Undefined TIME: S1_HK, row=%d", row );
      else if ( unit == 52 ) sprintf( context, " Undefined TIME: G2_HK, row=%d", row );
      else if ( unit == 53 ) sprintf( context, " Undefined TIME: G3_HK, row=%d", row );
      c_fcecho( context );

#ifdef DEBUG
      fprintf ( stderr, "time=%lf, anyf=%d\n", time, anyf );
#endif

      status = 11;
      return(status);
    }
  if (status != 0)
    return(status);

/* Get the name for this row */
/*
  fprintf ( stderr, "unit=%d, row=%d, nelem=%d, felem=%d\n", unit, ncolno, row, nelem, felem );
 */
  FTGCFS(unit, ncolno, row, nelem, felem, strval, flag, anyf, status); 
  strcpy( name, strval );    /* (is this OK???) */

#ifdef DEBUG
  fprintf ( stderr, "name=%s, iret = %d\n", name, status );
#endif

  if (status != 0)
    {
      if      ( unit == 50 ) sprintf( context, " Undefined NAME: S0_HK, row=%d", row );
      else if ( unit == 51 ) sprintf( context, " Undefined NAME: S1_HK, row=%d", row );
      else if ( unit == 52 ) sprintf( context, " Undefined NAME: G2_HK, row=%d", row );
      else if ( unit == 53 ) sprintf( context, " Undefined NAME: G3_HK, row=%d", row );
      c_fcecho( context );
      status = 12;
      return(status);
    }

/* Get the *value for this row */
  FTGCFJ(unit, vcolno, row, nelem, felem, value, flag, anyf, status);

  if ( status != 0)
    {
      if      ( unit == 50 ) sprintf( context, " Undefined VALUE: S0_HK, row=%d", row );
      else if ( unit == 51 ) sprintf( context, " Undefined VALUE: S1_HK, row=%d", row );
      else if ( unit == 52 ) sprintf( context, " Undefined VALUE: G2_HK, row=%d", row );
      else if ( unit == 53 ) sprintf( context, " Undefined VALUE: G3_HK, row=%d", row );
      c_fcecho( context );
      status = 13;
      return(status);
    }
     return(status);

}
