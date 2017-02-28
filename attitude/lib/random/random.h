/*****************************************************************************
* this header file gives function prototypes for the random number generator
* to be used here. These functions are actually wrappers to make it easy to
* switch between the FTOOLS standard generator and some other generator
*
* random returns a number between -.5 and + .5
* seed_random is used to seed the random number generator. It should be called
* before the first call to random.
****************************************************************************/

double get_random(void);
void seed_random(int idum_set);

