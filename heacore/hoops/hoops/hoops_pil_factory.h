/******************************************************************************
 *   File name: hoops_pil_factory.h                                           *
 *                                                                            *
 * Description: Declaration of factory to create PIL-based parameter objects. *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_PIL_FACTORY_H
#define HOOPS_PIL_FACTORY_H
////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include <string>
#include "hoops/hoops.h"
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
  class EXPSYM PILParFileFactory : public IParFileFactory {
    public:
      virtual ~PILParFileFactory() {}

      virtual IParFile * NewIParFile(const IParFile & p);
      virtual IParFile * NewIParFile(const std::string & comp_name);
  };

  class EXPSYM PILParPromptFactory : public IParPromptFactory {
    public:
      virtual ~PILParPromptFactory() {}

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
 * $Log: hoops_pil_factory.h,v $
 * Revision 1.8  2004/03/30 21:14:46  peachey
 * Allow PILParPrompt and ParPromptGroup constructors to accept optional
 * component name which is then used in lieu of argv[0]
 *
 * Revision 1.7  2004/03/16 14:36:58  peachey
 * Remove default construction option for ParFile and ParPrompt.
 *
 * Revision 1.6  2003/11/26 18:50:03  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.5  2003/11/13 19:29:29  peachey
 * Add preprocessor macro needed on Windows to export symbols.
 *
 * Revision 1.4  2003/11/10 18:19:45  peachey
 * Moved header files into hoops subdirectory.
 *
 * Revision 1.3  2003/07/17 13:58:51  peachey
 * PILParPromptFactory should publically inherit from IParPromptFactory.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:30  jchiang
 * First import
 *
 * Revision 1.2  2003/06/06 20:51:20  peachey
 * Add new virtual constructor to PILParPromptFactory, to be
 * consistent with PILParPrompt constructors. Similar change
 * also to PILParFileFactory.
 *
 * Revision 1.1  2003/06/05 15:46:08  peachey
 * Move PIL-related factories into a separate header.
 *
 ******************************************************************************/
