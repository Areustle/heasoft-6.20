/******************************************************************************
 *   File name: hoops_ape_factory.h                                           *
 *                                                                            *
 * Description: Declaration of factory to create Ape-based parameter objects. *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_APE_FACTORY_H
#define HOOPS_APE_FACTORY_H
////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include "hoops/hoops.h"
#include <string>
////////////////////////////////////////////////////////////////////////////////

#ifndef EXPSYM
#ifdef WIN32
#define EXPSYM __declspec(dllexport)
#else
#define EXPSYM
#endif
#endif

namespace hoops {

  //////////////////////////////////////////////////////////////////////////////
  // Constants.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type declarations/definitions.
  //////////////////////////////////////////////////////////////////////////////
  class EXPSYM HoopsApeFileFactory : public IParFileFactory {
    public:
      virtual ~HoopsApeFileFactory() {}

      virtual IParFile * NewIParFile(const IParFile & p);
      virtual IParFile * NewIParFile(const std::string & comp_name);
  };

  class EXPSYM HoopsApePromptFactory : public IParPromptFactory {
    public:
      virtual ~HoopsApePromptFactory() {}

      virtual IParPrompt * NewIParPrompt(const IParPrompt & p);
      virtual IParPrompt * NewIParPrompt(int argc, char * argv[], const std::string & comp_name = std::string());
  };
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Global variable forward declarations.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Function declarations.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

}
#endif

/******************************************************************************
 * $Log: hoops_ape_factory.h,v $
 * Revision 1.2  2009/12/23 20:27:19  peachey
 * Add some needed missing header files.
 *
 * Revision 1.1  2007/02/15 21:33:58  peachey
 * Initial version of Ape-based implementation.
 *
 ******************************************************************************/
