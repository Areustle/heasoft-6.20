#include <XSUtil/Numerics/RandomGenerator.h>

namespace Numerics {

void UniformRand(RealArray& randomNumbers)
{
   int nRands = randomNumbers.size();
   float *rands = new float[nRands];   
   const DefaultRandomGenerator& randGen = DefaultRandomGenerator::instance();
   randGen.getRandom(rands, nRands);

   for (int i=0; i<nRands; ++i)
   {
      randomNumbers[i] = rands[i];
   }
   delete [] rands;
}



}
