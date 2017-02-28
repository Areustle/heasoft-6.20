/// \file ahrandom.h
/// \brief Functions to generate random numbers
/// \author James Peachey
/// \date $Date: 2014/09/23 04:00:05 $

/// \addtogroup mod_ahgen
/// \section ahgen_ahrandom Random Numbers - ahrandom
///
/// This library provides wrapper routines for the Marsenne Twister random
/// number generator provided by HEADAS.
///

#ifndef AHGEN_AHRANDOM_H
#define AHGEN_AHRANDOM_H 

#include "ahgen/ahversion.h"
AHVERSION(AHGEN_AHRANDOM,"$Id: ahrandom.h,v 1.1 2014/09/23 04:00:05 mwitthoe Exp $")

#include "ahlog/ahlog.h"

/// \ingroup mod_ahgen
namespace ahgen {

/** \addtogroup mod_ahgen
 *  @{
 */

/// \brief seed random number generator (Mersenne Twister)
/// \param[in] seed value of seed to use (=0 to use current time)
void seedRandom(unsigned long int seed);

/// \brief free the object that is created in seedRandom (Mersenne Twister)
///
/// Caller must use this function when they're done with a seed, to free
/// the object that seedRandom() creates.
void freeRandom(void);

/// \brief return random number between [0:1)
/// \return next random number
double getRandom(void);

/// \brief return random integer in given range
/// \param[in] minimum value of random integer
/// \param[in] maximum value of random integer
/// \return random integer
int getRandomInt(int minval, int maxval);

} // namespace ahgen

/** @} */

#endif   /* AHGEN_AHRANDOM_H */

/* Revision Log
 $Log: ahrandom.h,v $
 Revision 1.1  2014/09/23 04:00:05  mwitthoe
 ahgen: split some functions from main ahgen file into ahfile and ahrandom files; see issue 437


*/
