//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <ExpInt.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Numerics/Gamma.h>
#include <cfloat>
#include <climits>
#include <cmath>

// IncGamma
#include <XSUtil/Numerics/IncGamma.h>

namespace Numerics {

    // Class Numerics::IncGamma 

    IncGamma::IncGamma()
    {
    }


    Real IncGamma::operator () (const Real a, const Real x) const
    {
      if (x <= 0.0)
      {
         throw YellowAlert("Incomplete Gamma routine is only valid for x > 0.0\n");
      }

      Real val=0.0;

      if (a < 0.0)
      {
         // Gsl uses .25 as the cutoff for the continued fraction region
         // when 'a' is negative.
         val = (x > .25) ? continuousFraction(a,x) :
                           integrateByParts(a,x);         
      }
      else if (a == 0.0)
      {
         E1 e1;
         val = e1(x);
      }
      else
      {
         // For positive 'a'. Numerical Recipes advocates x = a + 1
         // as the dividing region between these two methods.
         // When 'a' falls below ~10^-6 (yet remains positive)
         // this technique seems to diverge from the more
         // complicated algorithms used in gsl.


         val = (a+1.0 <= x) ? continuousFraction(a,x) :
                          seriesSolution(a,x);
      }
      return val;
    }

    Real IncGamma::continuousFraction (const Real a, const Real x)
    {
      // Evaluate the continued fraction using the modified Lentz algorithm, which
      // is described in Numerical Recipes ch. 5.2 and 6.2.  This uses the even part
      // of the continued fraction to determine the ai and b coefficients.

      // This should place FPMIN roughly around 10^-35, though the actual value 
      // isn't too important.
      const Real FPMIN = 1000.0*FLT_MIN; 
      const Real EPS = 1e-7;
      const int nTerms = 10000;

      Real b = x + 1.0 - a;
      Real c = 1.0/FPMIN;
      Real d = (fabs(b) < FPMIN) ? 1.0/FPMIN : 1.0/b;
      Real f = d;
      for (int i=1; i<nTerms; ++i)
      {
         Real ival = static_cast<Real>(i);
         Real ai = -ival*(ival - a);
         b += 2.0;
         c = b + ai/c;
         if (fabs(c) < FPMIN)
            c = FPMIN;
         d = b + ai*d;
         if (fabs(d) < FPMIN)
            d = FPMIN;
         d = 1.0/d;
         Real delta = c*d;
         f *= delta;
         if (fabs(delta - 1.0) < EPS)
            break;         
      }

      Real val = f*exp(-x + a*log(x));
      return val;
    }

    Real IncGamma::seriesSolution (const Real a, const Real x)
    {
       // Calculate the series:
       // gamma(a,x) = pow(x,a)*exp(-x)/a * M(1, 1+a, x) for a>0, x>=0
       //   where M(a,b,x) is Kummer's Function (see Abramowitz and Stegun,
       //   "Handbook of Mathematical Functions," chapter 13).
       // Return Gamma(a,x) = Gamma(a) - gamma(a,x)

      const int nTerms = 1000;
      Real term = 1.0;
      Real sum = term;
      Real aPlus = a;
      for (int i=0; i<nTerms; ++i)
      {
         aPlus += 1.0;
         term *= x/aPlus;
         sum += term;
      }
      sum *= exp(-x + a*log(x))/a;

      GammaLN lnGamma;
      Real val = exp(lnGamma(a)) - sum;
      return val;
    }

    Real IncGamma::integrateByParts (const Real a, const Real x)
    {
      // Assume 'a' is a negative value.  Perform a series of
      // integration-by-parts which effectively raises 'a' by +1 
      // each iteration.  Must pay special attention to case of 'a'
      // being an integer.  When this happens, 'remainder' will be 0
      // and continuousFraction/seriesSolution functions can't be used.

      if (a >= 0.0)
         throw RedAlert("Positive or zero 'a' sent to IncGamma::integrateByParts");
      if (-a >= static_cast<Real>(INT_MAX))
         throw YellowAlert("Abs value of 'a' is too large in IncGamma::integrateByParts");

      Real val=0.0;
      Real aFloor = floor(a);
      Real remainder = a - aFloor;
      // nParts must be >= 1
      int nParts = -static_cast<int>(aFloor);
      Real numer = -exp(-x)*pow(x,a-1);
      Real denom = 1.0;
      for (int i=0; i<nParts; ++i)
      {
         numer *= x;
         denom *= (a+static_cast<Real>(i));
         val += numer/denom;
      }
      Real integral=.0;
      if (remainder == 0.0)
      {
         E1 e1;
         integral = e1(x);         
      }
      else
      {
         integral = (remainder <= x) ? continuousFraction(remainder,x) :
                          seriesSolution(remainder,x);
      }
      integral /= denom;
      val += integral;

      return val;
    }

    // Additional Declarations

} // namespace Numerics
