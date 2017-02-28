/// \file ahrandom.cxx
/// \brief Implementation of public members of the libahgen:ahrandom library.
/// \author James Peachey
/// \date $Date: 2014/09/23 04:00:05 $

#define AHLABEL ahgen_ahrandom
#define AHCVSID "$Id: ahrandom.cxx,v 1.1 2014/09/23 04:00:05 mwitthoe Exp $"

#include "ahgen/ahrandom.h"
#include "ahlog/ahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "headas_rand.h"

#include "sys/time.h"       // to seed random number generator


namespace ahgen {

// -----------------------------------------------------------------------------

HDmt_state* & getRandSeedState() {
  static HDmt_state *state=0;
  return state;
}

// -----------------------------------------------------------------------------

void seedRandom(unsigned long int seedIn) {
  unsigned long int seed = seedIn;
  if (0 == seedIn) {
    struct timeval tt;      // time structure
    unsigned int time_s;    // time in seconds
    unsigned int time_us;   // time in micro seconds

    gettimeofday(&tt, NULL);   // get the system time
    time_s = tt.tv_sec;        // time in seconds
    time_us = tt.tv_usec;      // time in micro seconds
    seed = time_s + time_us;   // combine into powerfully random seed
  }
  getRandSeedState() = HDmt_srand(seed);
}

// -----------------------------------------------------------------------------

void freeRandom(void) {
  HDmt_destroy_state(getRandSeedState());
}

// -----------------------------------------------------------------------------

double getRandom(void) {
  return HDmt_drand(getRandSeedState());
}

// -----------------------------------------------------------------------------

int getRandomInt(int minval, int maxval) {
  if (minval > maxval) AH_THROW_LOGIC("minimum value cannot exceed maximum value");
  if (minval == maxval) return minval;
  return minval+(int)(getRandom()*(maxval-minval+1));
}

// -----------------------------------------------------------------------------

} // namespace ahgen

/* Revision Log
 $Log: ahrandom.cxx,v $
 Revision 1.1  2014/09/23 04:00:05  mwitthoe
 ahgen: split some functions from main ahgen file into ahfile and ahrandom files; see issue 437


*/
