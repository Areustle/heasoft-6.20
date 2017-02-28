//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// EDFVariants
#include <XSFit/StatMethod/EDF/EDFVariants.h>
#include <XSUtil/Error/Error.h>
#include <cmath>
#include <XSstreams.h>


// Class K_S 
const string K_S::cmdName = string("ks");
const string K_S::fullName = string("log(Kolmogorov-Smirnov)");
const string K_S::scriptName = string("log(Kolmogorov-Smirnov)");

Real K_S::calculate (const RealArray& EDF, const RealArray& modelDF, const Real totalCounts)
{
   const RealArray difference(std::abs(EDF-modelDF));
   Real result = difference.max();
   return result;
}

// Class C_vM 
const string C_vM::cmdName = string("cvm");
const string C_vM::fullName = string("log(Cramer-von Mises)");
const string C_vM::scriptName = string("log(Cramer-von Mises)");

Real C_vM::calculate (const RealArray& EDF, const RealArray& modelDF, const Real totalCounts)
{
   // Calculate (F_n(x) - F(x))^2*dF(x)
   RealArray difference(EDF);
   difference -= modelDF;
   difference *= difference;
   // And now the dF(x) term...
   Real prev = 0.0;
   const size_t sz = modelDF.size();
   for (size_t i=0; i<sz; ++i)
   {
      Real dF = modelDF[i] - prev;
      difference[i] *= dF;
      prev = modelDF[i]; 
   }
   return difference.sum();
}

// Class A_D 
const string A_D::cmdName = string("ad");
const string A_D::fullName = string("log(Anderson-Darling)");
const string A_D::scriptName = string("log(Anderson-Darling)");

Real A_D::calculate (const RealArray& EDF, const RealArray& modelDF, const Real totalCounts)
{
   // Calculate (F_n(x) - F(x))^2*dF(x)/(F(x)*|(1 - F(x))|)
   RealArray difference(EDF);
   difference -= modelDF;
   difference *= difference;
   Real prev = 0.0;
   const size_t sz = modelDF.size();
   for (size_t i=0; i<sz; ++i)
   {
      Real dF = modelDF[i] - prev;
      difference[i] *= dF;
      prev = modelDF[i]; 
   }
   RealArray weight(modelDF);
   weight -= 1.0;
   weight = abs(weight);
   weight *= modelDF;
   Real result=0.0;
   const Real* pDiff = &difference[0];
   const Real* pWeight = &weight[0];
   for (size_t i=0; i<sz; ++i)
   {
      if (pWeight[i] != 0.0)
      {
         result += pDiff[i]/pWeight[i];
      }
   }
   return result;
}
