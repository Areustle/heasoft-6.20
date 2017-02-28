//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// RandomLux
#include <XSUtil/Numerics/RandomLux.h>
// RandomLuxAdapter
#include <XSUtil/Numerics/RandomLuxAdapter.h>


namespace Numerics {

    // Class Numerics::RandomLuxAdapter 

    RandomLuxAdapter::RandomLuxAdapter()
    {
       m_generator = new RandomLux();
    }


    RandomLuxAdapter::~RandomLuxAdapter()
    {
       delete m_generator;
    }


    void RandomLuxAdapter::initialize (int seed, const std::vector<Real>& extraPars)
    {
       int luxLevel = 3;

       if (extraPars.size())
          luxLevel = static_cast<int>(extraPars[0]);
       m_generator->rluxgo(luxLevel, seed, 0, 0);
    }

    void RandomLuxAdapter::getRandom (std::vector<Real>& randNumbers, const std::vector<Real>& extraPars)
    {
       int length = static_cast<int>(randNumbers.size());
       float *tmpFloats = new float[length];
       m_generator->ranlux(tmpFloats, length);
       for (int i=0; i<length; ++i)
       {
          randNumbers[i] = static_cast<Real>(tmpFloats[i]);
       }
       delete [] tmpFloats;
    }

    void RandomLuxAdapter::getRandom (float* randNumbers, int length, const std::vector<Real>& extraPars)
    {
       m_generator->ranlux(randNumbers, length);
    }

    void RandomLuxAdapter::getRandom (Real* randNumbers, int length, const std::vector<Real>& extraPars)
    {
       float *tmpFloats = new float[length];
       m_generator->ranlux(tmpFloats, length);
       for (int i=0; i<length; ++i)
       {
          randNumbers[i] = static_cast<Real>(tmpFloats[i]);
       }
       delete [] tmpFloats;
    }

    // Additional Declarations

} // namespace Numerics
