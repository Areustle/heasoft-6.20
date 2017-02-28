#include <XSUtil/Numerics/RandomGenerator.h>

#include <cmath>
#include <algorithm>

#include <iostream>

namespace Numerics {

void GaussRand(RealArray& gaussSamples)
{
   size_t N (gaussSamples.size());  
   int iset (0);
   float random[2];
   Real gset(0);
   const DefaultRandomGenerator& randGen = DefaultRandomGenerator::instance();
   for ( size_t i = 0; i < N; ++i)
   {
      if ( iset == 0 )
      {
         float v1(0);
         float v2(0);
         float rsq (0);
         while ( rsq >= 1 || rsq == 0)
         {
            randGen.getRandom(random,2);
            v1 = 2.*random[0] - 1;
            v2 = 2.*random[1] - 1;
            rsq = v1*v1 + v2*v2;
         }
         float fac (sqrt(-2.*log(rsq)/rsq));
         gset = v1*fac;
         gaussSamples[i] = v2*fac;
         iset = 1;

      }
      else
      {
         gaussSamples[i] = gset;
         iset = 0;
      }

   }      
}                

}
