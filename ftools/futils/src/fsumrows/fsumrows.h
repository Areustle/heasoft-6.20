/******************************************************************************

Filename:
     fsumrows.h

Description:
      Header file containing common definitions and function prototypes used
      in the functions comprising the tool fsumrows.

Author/Date: toliver / April, 1999

Modification History:

$Log: fsumrows.h,v $
Revision 1.3  1999/10/27 14:55:07  peachey
Use new boolean parameter "sametype" (default false) to allow user
to write the output with the same data type as the input.

Revision 1.2  1999/06/03 20:53:41  toliver
removed ^M characters that were hosing IRIX cc build

 * Revision 1.1  1999/05/19  19:30:31  toliver
 * initial version
 *

Notes:	

Usage:
     #include "fsumrows.h"

******************************************************************************/

#include <cftools.h>
#include <fitsio.h>

#define MAXRANGES 15
#define BUFSIZE 128
#define CBUFSIZE BUFSIZE+1
#define MAXDIM 999

/*
** Function prototypes.
*/

int fsumrows (void);

int fsumrows_get_params (char *,
                         char *,
                         char *,
                         char *,
                         int *,
                         int *,
                         int *,
                         int *);

int fsumrows_process_rows (char *,
                           char *,
                           char *,
                           char *,
                           int,
                           int,
                           int,
                           int);

void fsumrows_build_selected_columns_list (fitsfile *,
                                           char *,
                                           int *,
                                           int,
                                           int *);

void fsumrows_wrt_col_result (fitsfile *,
                              int,
                              int,
                              long,
                              void *,
                              char *,
                              int,
                              int *);

/*
** Global declarations.
*/

enum {SUM_OP, AVG_OP};
