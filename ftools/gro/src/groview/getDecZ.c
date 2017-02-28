
#include"groview.h"
#define AMIN   60.0
#define ASEC  3600.0
#define HASEC 360000.0

void getDecZ(float *deg, float *minu, float *sec, float *dec)
{
  int status = 0;
  float idegp;

  idegp = fabs(*deg*1.0);
  *dec = idegp + fabs(*minu/AMIN ) + fabs( *sec/ASEC );
 
    if (idegp != 0)                     /*FIX TO HANDLE "-0'S"*/
    *dec = *dec*(*deg)*1.0/idegp;           /*CASE( *,*,*)  (DEFAULT)*/
    else if (*minu !=0 )
      *dec = *minu/fabs(*minu) * (*dec);    /*CASE (0,*,*) */
    else if ( *sec!=0 )
      *dec = *sec/fabs(*sec ) * (*dec);     /*CASE (0,0,*) */

}
   
