#include <XSUtil/Numerics/RandomGenerator.h>

namespace Numerics {

void CauchyRand(RealArray& randomNumbers)
{
   // Analogous to the GaussRand function, this does not apply
   // a scaling factor or additive constant to the random
   // numbers.  Leave it up to the calling function to do that.
   static const Real PI = 4.*std::atan(1.); 
   Numerics::UniformRand(randomNumbers);
   randomNumbers *= PI;
   randomNumbers = std::tan(randomNumbers); 
}

}
