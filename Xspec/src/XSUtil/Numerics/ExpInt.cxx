//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <cmath>

// ExpInt
#include <XSUtil/Numerics/ExpInt.h>

namespace Numerics {
     // All coefficients are listed in chapter 5 of:
     // M. Abramowitz, I.A. Stegun, "Handbook of Mathematical Functions
     // with Formulas, Graphs, and Mathematical Tables," 
     // U.S. Department of Commerce, 2002

     // For 0 <= x <= 1, from E.E. Allen, Mathematical Tables and other 
     // Aids to Computation, vol.8, 240 (1954)
     const Real E1::a[] = {-.57721566,  .99999193, -.24991055,
                            .05519968, -.00976004,  .00107857};
     // For 1 <= x < inf, from C. Hastings, Jr., Approximations for
     // digital computers, Princeton Univ. Press, (1955)
     const Real E1::b[] = { 9.5733223454, 25.6329561486,
                           21.0996530827,  3.9584969228};
     const Real E1::c[] = { 8.5733287401, 18.0590169730,
                            8.6347608925,   .2677737343};

    // Class Numerics::E1 

    E1::E1()
    {
    }


    Real E1::operator () (const Real x) const
    {
       Real val=0.0;
       const Real x2 = x*x;
       const Real x3 = x2*x;
       const Real x4 = x2*x2;
       if (x <= 1.0)
       {
          val = a[0] + a[1]*x + a[2]*x2 + a[3]*x3 + a[4]*x4 
                + a[5]*x4*x - log(x);
       }
       else
       {
          val = (x4 + c[0]*x3 + c[1]*x2 + c[2]*x + c[3])/
                (x4 + b[0]*x3 + b[1]*x2 + b[2]*x + b[3]);
          val /= x*exp(x);
       }
       return val;
    }

    // Additional Declarations

} // namespace Numerics
