//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Error/Error.h>
#include <cmath>

// MathOperator
#include <XSUtil/Numerics/MathOperator.h>
#include <algorithm>
#include <XSUtil/Numerics/Numerics.h>

namespace Numerics {

    // Class Numerics::MathOperator 

    void MathOperator::operator () (RealArray& firstAndNew, const RealArray& second) const
    {
       throw RedAlert("MathOperator (binary) called with wrong number of arguments.");
    }

    void MathOperator::operator () (RealArray& firstAndNew) const
    {
       throw RedAlert("MathOperator (unary) called with wrong number of arguments.");
    }

    // Additional Declarations

    // Even though destructor is pure virtual, a definition is still needed.
    // (See item 12.4.7 in the Standard)
    MathOperator::~MathOperator() {}  


    void PlusOp::operator () (RealArray& firstAndNew, const RealArray& second) const
    {
       firstAndNew += second;
    }

    void MinusOp::operator () (RealArray& firstAndNew, const RealArray& second) const
    {
       firstAndNew -= second;
    }

    void MultOp::operator () (RealArray& firstAndNew, const RealArray& second) const
    {
       firstAndNew *= second;
    }

    void DivideOp::operator () (RealArray& firstAndNew, const RealArray& second) const
    {
       firstAndNew /= second;
    }

    void PowOp::operator () (RealArray& firstAndNew, const RealArray& second) const
    {
       firstAndNew = pow(firstAndNew, second);
    }

    void MaxOp::operator () (RealArray& firstAndNew, const RealArray& second) const
    {
       Real* pFirst = &firstAndNew[0];
       const size_t sz = firstAndNew.size();
       for (size_t i=0; i<sz; ++i)
       {
          pFirst[i] = std::max(pFirst[i],second[i]);
       }
    }

    void MinOp::operator () (RealArray& firstAndNew, const RealArray& second) const
    {
       Real* pFirst = &firstAndNew[0];
       const size_t sz = firstAndNew.size();
       for (size_t i=0; i<sz; ++i)
       {
          pFirst[i] = std::min(pFirst[i],second[i]);
       }       
    }



    void UnaryMinusOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew *= -1.;
    }

    void ExpOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = exp(firstAndNew);
    }

    void SinOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = sin(firstAndNew);
    }

    void SinDOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew *= DEGTORAD;
       firstAndNew = sin(firstAndNew);
    }

    void CosOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = cos(firstAndNew);
    }

    void CosDOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew *= DEGTORAD;
       firstAndNew = cos(firstAndNew);
    }

    void TanOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = tan(firstAndNew);
    }

    void TanDOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew *= DEGTORAD;
       firstAndNew = tan(firstAndNew);
    }

    void LogOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = log10(firstAndNew);
    }

    void LnOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = log(firstAndNew);
    }

    void SqrtOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = sqrt(firstAndNew);
    }

    void AbsOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = abs(firstAndNew);
    }

    void IntOp::operator () (RealArray& firstAndNew) const
    {
       const size_t sz = firstAndNew.size();
       Real* pVals = &firstAndNew[0];
       for (size_t i=0; i<sz; ++i)
       {
          // deliberate truncation
          int tmp = static_cast<int>(pVals[i]);
          pVals[i] = static_cast<Real>(tmp);
       }
    }

    void ASinOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = asin(firstAndNew);
    }

    void ACosOp::operator () (RealArray& firstAndNew) const
    {
       firstAndNew = acos(firstAndNew);
    }

    void MeanOp::operator () (RealArray& firstAndNew) const
    {
       Real mean = firstAndNew.sum()/firstAndNew.size();
       firstAndNew = mean;
    }

    void DimOp::operator () (RealArray& firstAndNew) const
    {
       Real dim = static_cast<Real>(firstAndNew.size());
       firstAndNew = dim;
    }

    void SMinOp::operator () (RealArray& firstAndNew) const
    {
       Real smin = firstAndNew.min();
       firstAndNew = smin;
    }

    void SMaxOp::operator () (RealArray& firstAndNew) const
    {
       Real smax = firstAndNew.max();
       firstAndNew = smax;
    }

} // namespace Numerics
