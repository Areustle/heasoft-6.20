/***************************************************************************
* this is a wrapper for a random number generator which is seeded with 
* an int and returns a double between -.5 and +.5
****************************************************************************/
#include "random.h"


/****************************************************************
* Use the numerical recipies random number generator by default *
****************************************************************/
#ifndef USE_SYSTEM_RANDOM
#    define USE_NUMREC2_RANDOM
#endif

#ifdef USE_NUMREC2_RANDOM
/********************************************************************
* use the Numerical Recipies "ran2" random number generator
* at least that's the official Headas random number generator was
* last I checked.
********************************************************************/
#include "headas_utils.h"

static long idum=1;


double get_random(void) {

return (hd_ran2(&idum) - 0.5);
}

void seed_random(int idum_set) {

idum=idum_set;

}

#endif /* USE_NUMREC2_RANDOM */



#ifdef USE_SYSTEM_RANDOM
/***************************************************************************
* use the system drand48 random number generator.
* This generator is supposed to be credible (unlike rand() which isn't).
**************************************************************************/
#include <stdlib.h>
double get_random(void) {

return(drand48()-0.5);

}


void seed_random(int idum_set) {

srand48((long)idum_set);

}

#endif /* USE_SYSTEM_RANDOM */





