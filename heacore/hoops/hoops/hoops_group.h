/******************************************************************************
 *   File name: hoops_group.h                                                 *
 *                                                                            *
 * Description: Declaration for ParGroup class.                               *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_GROUP_H
#define HOOPS_GROUP_H
////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include "hoops/hoops.h"
#include <string>
#include <vector>
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
  class EXPSYM ParGroup : public IParGroup {
    public:
      typedef std::vector<IPar *> Container_t;
      typedef BiDirItor<IPar *, Container_t::iterator> Itor_t;
      typedef ConstBiDirItor<IPar *, Container_t::const_iterator> ConstItor_t;

      ParGroup(const std::string & comp_name);
      ParGroup(const ParGroup & g);

      virtual ~ParGroup();

      virtual ParGroup & operator =(const IParGroup & g);

      virtual ParGroup & operator =(const ParGroup & g);

      virtual IPar & operator [](const std::string & pname) const
        { return Find(pname); }

      virtual IPar & Find(const std::string & pname) const;
      virtual ParGroup & Clear();
      virtual ParGroup & Add(IPar * p);
      virtual ParGroup & Remove(IPar * p);
      virtual ParGroup & Remove(const std::string & pname);
      virtual GenParItor begin()
        { return GenParItor(Itor_t(mPars.begin())); }
      virtual ConstGenParItor begin() const
        { return ConstGenParItor(ConstItor_t(mPars.begin())); }
      virtual GenParItor end()
        { return GenParItor(Itor_t(mPars.end())); }
      virtual ConstGenParItor end() const
        { return ConstGenParItor(ConstItor_t(mPars.end())); }

      virtual ParGroup * Clone() const { return new ParGroup(*this); }

    private:
      Container_t mPars;
      std::string mGroupName;
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
 * $Log: hoops_group.h,v $
 * Revision 1.8  2009/12/23 20:27:19  peachey
 * Add some needed missing header files.
 *
 * Revision 1.7  2004/09/21 16:47:04  peachey
 * Add name of group field to class, and use it in error messages.
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
 * Revision 1.1.1.1  2003/11/04 01:48:30  jchiang
 * First import
 *
 * Revision 1.3  2003/06/06 20:44:05  peachey
 * Add assignment operator from IParGroup reference.
 *
 * Revision 1.2  2003/06/06 13:25:37  peachey
 * Restructure the header files. The files hoops_exception.h, hoops.h,
 * hoops_itor.h, hoops_pil_factory.h, and hoops_prim.h are now the
 * only public (installed) files.
 *
 * Revision 1.1  2003/04/23 17:41:33  peachey
 * ParGroup class now has its own header.
 *
 ******************************************************************************/
