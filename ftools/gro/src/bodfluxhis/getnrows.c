#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

 getnrows(long *nrows, float *startTime, float *stopTime,long *noutrows,
 float *tjdsta,float *tjdstp, long *firstrow, long *lastrow)

{
  int   ii;
 
    for (ii = 0; ii <*nrows; ii++)
      {
      if (startTime[ii] >= *tjdsta)
         {
           *firstrow = ii;
           break;
         }
      }
    /*    *firstrow = *firstrow +1;*/

    /* Find out the number of last row for the stoptime */
 step1:for (ii = 0; ii <*nrows; ii++)
        {
         if (stopTime[ii] >= *tjdstp)
          {
           *lastrow = ii;
           break;
          }
        }
    /**lastrow = *lastrow + 1 ;*/
    *noutrows = *lastrow - *firstrow +2;
}
