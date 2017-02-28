/******************************************************************************
 *   File name: hoops_prompt_group.h                                          *
 *                                                                            *
 * Description: Convenience prompter for simple parameter file needs.         *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_PROMPT_GROUP_H
#define HOOPS_PROMPT_GROUP_H

////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include "hoops/hoops.h"
#include "hoops/hoops_group.h"
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
  class EXPSYM ParPromptGroup : public IParGroup {
    public:
      // If the comp_name argument is supplied, argv[0] is ignored.
      ParPromptGroup(int argc, char * argv[], const std::string & comp_name = std::string());
      ParPromptGroup(const ParPromptGroup & group);

      virtual ~ParPromptGroup();

      virtual IParGroup & operator =(const ParPromptGroup & g);
      virtual IParGroup & operator =(const IParGroup & g);

      virtual IPar & operator [](const std::string & pname) const;

      virtual IPar & Find(const std::string & pname) const;
      virtual IParGroup & Clear();
      virtual IParGroup & Add(IPar * p);
      virtual IParGroup & Remove(IPar * p);
      virtual IParGroup & Remove(const std::string & pname);

      virtual GenParItor begin();
      virtual ConstGenParItor begin() const;
      virtual GenParItor end();
      virtual ConstGenParItor end() const;

      virtual IParGroup * Clone() const;

      // File-like methods:
      void Load();
      void Save() const;

      // Prompt-like methods:
      ParPromptGroup & Prompt();
      ParPromptGroup & Prompt(const std::string & pname);

    private:
      IParFile * mFile;
      IParPrompt * mPrompter;
      IParGroup * mGroup;
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
 * $Log: hoops_prompt_group.h,v $
 * Revision 1.2  2004/03/30 21:14:46  peachey
 * Allow PILParPrompt and ParPromptGroup constructors to accept optional
 * component name which is then used in lieu of argv[0]
 *
 * Revision 1.1  2004/03/15 15:00:52  peachey
 * New wrapper class which handles simplest use case for getting parameters.
 *
 ******************************************************************************/
