/*****************************************************************************
 * 
 * File name: xrt_ran2.h
 * 
 *
 * Description:
 *          this header file gives function prototypes for the 
 *          random number generator
 *
 * Author/Date: Barbara Saija October 2001
 *
 * History:
 *         v. 0.1.0 - adapted from NUMERICAL RECIPES
 *
 *  Functions implemented:
 *           xrt_ran2 returns a number between 0.0 and  1.0  (exclusive of the 
 *                    endpoint values)
 *           get_ran2seed is used to seed the random number generator. 
 *                        It should be called before the first call to random.
 ****************************************************************************/

float xrt_ran2(long *idum);
void get_ran2seed(long *seed);
