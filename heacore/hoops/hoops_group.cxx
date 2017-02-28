/******************************************************************************
 *   File name: hoops_group.cxx                                               *
 *                                                                            *
 * Description: Implementation of standard parameter group.                   *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/

////////////////////////////////////////////////////////////////////////////////
// Header files.
////////////////////////////////////////////////////////////////////////////////
#include "hoops/hoops_group.h"
#include <string>
#include <vector>
////////////////////////////////////////////////////////////////////////////////
namespace hoops {

  //////////////////////////////////////////////////////////////////////////////
  // Constants.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type definitions.
  //////////////////////////////////////////////////////////////////////////////
  ParGroup::ParGroup(const std::string & name): IParGroup(), mPars(),
    mGroupName(name) {}
  ParGroup::ParGroup(const ParGroup & g): IParGroup(), mPars(),
    mGroupName(g.mGroupName) {
    std::vector<IPar *>::const_iterator it;
    for (it = g.mPars.begin(); it != g.mPars.end(); ++it) 
      mPars.push_back((*it)->Clone());
  }

  ParGroup::~ParGroup() { Clear(); }

  ParGroup & ParGroup::operator =(const IParGroup & g) {
    ConstGenParItor it;
    Clear();
    for (it = g.begin(); it != g.end(); ++it) 
      mPars.push_back((*it)->Clone());
    return *this;
  }

  ParGroup & ParGroup::operator =(const ParGroup & g) {
    std::vector<IPar *>::const_iterator it;
    Clear();
    for (it = g.mPars.begin(); it != g.mPars.end(); ++it) 
      mPars.push_back((*it)->Clone());
    return *this;
  }

  IPar & ParGroup::Find(const std::string & pname) const {
    std::vector<IPar *>::const_iterator it;

    // Look for a parameter with the given name.
    for (it = mPars.begin(); it != mPars.end(); ++it)
      if (!pname.compare((*it)->Name())) break;

    // If not found, throw an exception to indicate this fact.
    if (it == mPars.end()) throw Hexception(PAR_NOT_FOUND,
      "Parameter " + pname + " not found in parameter group " + mGroupName,
      __FILE__, __LINE__);

    // Otherwise, return the found parameter.
    return *(*it);
  }

  ParGroup & ParGroup::Clear() {
    std::vector<IPar *>::iterator it;
    for (it = mPars.begin(); it != mPars.end(); ++it) delete *it;
    mPars.clear();
    return *this;
  }

  ParGroup & ParGroup::Add(IPar * p)
    { if (p) mPars.push_back(p); return *this; }
  
  ParGroup & ParGroup::Remove(IPar * p) { Remove(p->Name()); return *this; }

  ParGroup & ParGroup::Remove(const std::string & pname) {
    std::vector<IPar *>::iterator it;
    for (it = mPars.begin(); it != mPars.end(); ++it) {
      if (!pname.compare((*it)->Name())) {
        delete (*it);
        mPars.erase(it);
      }
    }
    return *this;
  }
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Global variable definitions.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Static variable definitions.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Static function definitions.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Function definitions.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

}

/******************************************************************************
 * $Log: hoops_group.cxx,v $
 * Revision 1.11  2009/12/23 20:26:10  peachey
 * Add some needed missing header files.
 *
 * Revision 1.10  2004/09/21 16:47:03  peachey
 * Add name of group field to class, and use it in error messages.
 *
 * Revision 1.9  2004/03/24 16:59:19  peachey
 * Improve messages in thrown exceptions.
 *
 * Revision 1.8  2004/03/16 20:50:57  peachey
 * Explicitly invoke constructors for base classes to shut up compiler
 * warnings in the SLAC build.
 *
 * Revision 1.7  2004/03/12 15:40:42  peachey
 * When throwing exceptions, include the file name and
 * line number where the exception was thrown.
 *
 * Revision 1.6  2003/11/26 18:50:02  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.5  2003/11/10 18:16:12  peachey
 * Moved header files into hoops subdirectory.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:29  jchiang
 * First import
 *
 * Revision 1.4  2003/06/06 20:44:05  peachey
 * Add assignment operator from IParGroup reference.
 *
 * Revision 1.3  2003/06/06 13:25:37  peachey
 * Restructure the header files. The files hoops_exception.h, hoops.h,
 * hoops_itor.h, hoops_pil_factory.h, and hoops_prim.h are now the
 * only public (installed) files.
 *
 * Revision 1.2  2003/04/23 17:43:29  peachey
 * Use new iterator classes.
 *
 * Revision 1.1  2003/04/11 19:20:38  peachey
 * New component HOOPS, an object oriented parameter interface. Low
 * level access currently uses PIL, but this can be changed.
 *
 ******************************************************************************/
