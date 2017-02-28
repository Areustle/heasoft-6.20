//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include "xsTypes.h"

// Cosmology
#include <XSModel/GlobalContainer/Cosmology.h>

namespace XSContainer {

    // Class XSContainer::Cosmology 

    Cosmology::Cosmology (Real H, Real q, Real lambda)
          : H0(H),
            q0(q),
            lambda0(lambda)
    {
    }


} // namespace XSContainer
