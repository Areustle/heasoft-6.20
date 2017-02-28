
#include <XSUtil/Numerics/RandomGenerator.h>
#include <XSUtil/Numerics/LnBang.h>

#include <cmath>
#include <algorithm>

#include <iostream>
#include <fstream>


namespace Numerics {

void PoissonRand(RealArray& poissonMeans)
{
   size_t N (poissonMeans.size());  
   static const Real PI = 4.*std::atan(1.);  
   const DefaultRandomGenerator& randGen = DefaultRandomGenerator::instance();

// Function to return NM random Poisson numbers whose means are
// given by XM(NM). Uses Knuth algorith for means < 12 and a rejection
// method using the normal distribution as a covering function for larger means.
// The rejection method is "Rejection method PA" from "The Computer Generation of 
// Poisson Random Variables" by A. C. Atkinson, Journal of the Royal Statistical 
// Society Series C (Applied Statistics) Vol. 28, No. 1. (1979) The article is on 
// pages 29-35. The algorithm given here is on page 32.

// The uniform random number generator used is from CERN.

   float oldm(-1); 
   float g(0);
   float alpha(0);
   float beta(0);
   LnBang logFactorial;

   for (size_t i = 0; i < N; ++i) {

     float xm (poissonMeans[i]);
     float em(0);
     float random(0);

     if ( xm < 12. ) {

       // if the mean is small then generate independent exponential deviates

       if (xm != oldm) {
	 oldm = xm;
	 g = exp(-xm);       
       } 
       em = 0;
       randGen.getRandom(&random,1);
       float t (random);
       while ( t > g ) {
	 ++em;
	 randGen.getRandom(&random,1);
	 t *= random;
       }

     } else {

       // for a large mean use rejection method

       if (xm != oldm) {
	 oldm = xm;
	 beta = PI/sqrt(3.0*xm);
	 alpha = beta*xm;
	 g = log(0.767 - 3.36/xm) - xm - log(beta);
       } 

       bool done (false);
       while ( !done ) {
	 em = -1;
	 float x;
	 while ( em < 0 ) {
	   randGen.getRandom(&random,1);
	   x = (alpha - log((1.0 - random)/random))/beta;
	   em = floor(x + 0.5);
	 }
	 float rhs = g + em*log(xm) - logFactorial((long)em);
	 float y = alpha - beta*x;
	 float temp = 1.0 + exp(y);
	 randGen.getRandom(&random,1);
	 float lhs = y + log(random/(temp*temp));
	 done = (lhs <= rhs);
       }

     }

     poissonMeans[i] = em;
     oldm = xm;

   }

}                

}
