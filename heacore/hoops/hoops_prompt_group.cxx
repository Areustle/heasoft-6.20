/******************************************************************************
 *   File name: hoops_ballistic.h                                             *
 *                                                                            *
 * Description: Conveniece wrappers for simple parameter file access.         *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_BALLISTIC_H
#define HOOPS_BALLISTIC_H

////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include "hoops/hoops.h"
#include "hoops/hoops_ape_factory.h"
#include "hoops/hoops_group.h"
#include "hoops/hoops_prompt_group.h"
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
  ParPromptGroup::ParPromptGroup(int argc, char * argv[],
    const std::string & comp_name): IParGroup(), mFile(0),
    mPrompter(0) {
    // Use comp_name if one was supplied to find the parameter file, otherwise
    // use argv[0].
    if (comp_name.empty()) mFile = HoopsApeFileFactory().NewIParFile(argv[0]);
    else mFile = HoopsApeFileFactory().NewIParFile(comp_name);

    // Create a new prompter object:
    mPrompter = HoopsApePromptFactory().NewIParPrompt(argc, argv, comp_name);

    // Get a reference to the prompter's group:
    mGroup = &mPrompter->Group();
  }

  ParPromptGroup::ParPromptGroup(const ParPromptGroup & group): IParGroup(),
    mFile(0), mPrompter(0) {
    mFile = group.mFile->Clone();
    mPrompter = group.mPrompter->Clone();
    mGroup = &mPrompter->Group();
  }

  ParPromptGroup::~ParPromptGroup() {
    // DON'T delete mGroup! It is owned by mPrompter!
    delete mPrompter;
    delete mFile;
  }

  IParGroup & ParPromptGroup::operator =(const ParPromptGroup & g) {
    *mGroup = *g.mGroup;
    return *this;
  }

  IParGroup & ParPromptGroup::operator =(const IParGroup & g) {
    *mGroup = g;
    return *this;
  }

  IPar & ParPromptGroup::operator [](const std::string & pname) const {
    return mGroup->operator [](pname);
  }

  IPar & ParPromptGroup::Find(const std::string & pname) const {
    return mGroup->Find(pname);
  }

  IParGroup & ParPromptGroup::Clear() {
    mGroup->Clear();
    return *this;
  }

  IParGroup & ParPromptGroup::Add(IPar * p) {
    throw Hexception(PAR_UNSUPPORTED,
      "Editing Ape-based prompt group not supported", __FILE__, __LINE__);
    mGroup->Add(p);
    return *this;
  }

  IParGroup & ParPromptGroup::Remove(IPar * p) {
    throw Hexception(PAR_UNSUPPORTED,
      "Editing Ape-based prompt group not supported", __FILE__, __LINE__);
    mGroup->Remove(p);
    return *this;
  }

  IParGroup & ParPromptGroup::Remove(const std::string & pname) {
    throw Hexception(PAR_UNSUPPORTED,
      "Editing Ape-based prompt group not supported", __FILE__, __LINE__);
    mGroup->Remove(pname);
    return *this;
  }

  GenParItor ParPromptGroup::begin() {
    return mGroup->begin();
  }

  ConstGenParItor ParPromptGroup::begin() const {
    return (const_cast<const IParGroup *>(mGroup))->begin();
  }

  GenParItor ParPromptGroup::end() {
    return mGroup->end();
  }

  ConstGenParItor ParPromptGroup::end() const {
    return (const_cast<const IParGroup *>(mGroup))->end();
  }

  IParGroup * ParPromptGroup::Clone() const { return new ParPromptGroup(*this); }

  ParPromptGroup & ParPromptGroup::Prompt() {
    mPrompter->Prompt();
    return *this;
  }

  ParPromptGroup & ParPromptGroup::Prompt(const std::string & pname) {
    mPrompter->Prompt(pname);
    return *this;
  }

  void ParPromptGroup::Load() {
    mFile->Load();
    mPrompter->Group() = mFile->Group();
  }

  void ParPromptGroup::Save() const {
    mFile->Group() = mPrompter->Group();
    mFile->Save();
  }
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
 * $Log: hoops_prompt_group.cxx,v $
 * Revision 1.6  2007/02/15 21:46:21  peachey
 * Changes to use Ape implementation instead of PIL.
 *
 * Revision 1.5  2004/09/21 16:48:06  peachey
 * Shorten line lengths to conform to package convention.
 *
 * Revision 1.4  2004/03/30 21:14:46  peachey
 * Allow PILParPrompt and ParPromptGroup constructors to accept optional
 * component name which is then used in lieu of argv[0]
 *
 * Revision 1.3  2004/03/26 22:31:54  peachey
 * Prevent client from shooting self in the foot: don't allow
 * them to add/remove parameters, because PIL can't handle that. What's more,
 * don't call Save() from the destructor: Save() can throw!
 *
 * Revision 1.2  2004/03/16 20:50:57  peachey
 * Explicitly invoke constructors for base classes to shut up compiler
 * warnings in the SLAC build.
 *
 * Revision 1.1  2004/03/15 15:00:52  peachey
 * New wrapper class which handles simplest use case for getting parameters.
 *
 ******************************************************************************/
