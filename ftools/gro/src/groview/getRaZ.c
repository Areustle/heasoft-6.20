
#include"groview.h"
#define AMIN   60.0

void getRaZ(float *hour, float *minu, float *sec, float *ra)
{

  *ra= 15.0 * (*hour) + 0.25 * (*minu) + 0.25 /AMIN *(*sec);
}
   
