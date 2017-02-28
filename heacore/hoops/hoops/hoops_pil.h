/******************************************************************************
 *   File name: hoops_pil.h                                                   *
 *                                                                            *
 * Description: Definition of PIL-based interface.                            *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_PIL_H
#define HOOPS_PIL_H
////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include <string>
#include <vector>
#include "hoops/hoops.h"
#include "hoops/hoops_exception.h"
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
  class PILException : public Hexception {
    public:
      PILException(const int & code, const std::string & msg = std::string(),
                   const std::string & filename = std::string(), int line = 0);

    protected:
      virtual void format(const std::string & msg);
  };

  class EXPSYM PILParFile : public IParFile {
    public:
      PILParFile(const PILParFile & pf);
      PILParFile(const IParFile & pf);
      PILParFile(const std::string & comp, int argc = 0, char ** argv = 0);

      virtual ~PILParFile();

      virtual PILParFile & operator =(const PILParFile & pf);
      virtual PILParFile & operator =(const IParFile & pf);

      // Synchronize memory image with parameter file and vice versa.
      virtual void Load();
      virtual void Save() const;

      // Read member access.
      virtual const std::string & Component() const { return mComponent; }
      virtual IParGroup & Group();
      virtual const IParGroup & Group() const;

      // Write member access.
      virtual PILParFile & SetComponent(const std::string & comp);
      virtual IParGroup * SetGroup(IParGroup * group = 0);

      virtual GenParItor begin();
      virtual ConstGenParItor begin() const;
      virtual GenParItor end();
      virtual ConstGenParItor end() const;

      virtual IParFile * Clone() const;

      void OpenParFile() const;
      void CloseParFile(int status = 0) const;
      void SetArgs(int argc, char ** argv);
      int Argc() const { return mArgc; }
      char ** const Argv() const { return mArgv; }

    protected:
      std::string mComponent;
      mutable IParGroup * mGroup;
      int mArgc;
      char ** mArgv;
      void CleanComponent(const std::string & comp, std::string & clean) const;
  };

  class EXPSYM PILParPrompt : public IParPrompt {
    public:
      PILParPrompt(const PILParPrompt & prompt);
      PILParPrompt(const IParPrompt & prompt);
      PILParPrompt(int argc, char ** argv, const std::string & comp_name = std::string());

      virtual ~PILParPrompt();

      virtual PILParPrompt & operator =(const PILParPrompt & p);
      virtual PILParPrompt & operator =(const IParPrompt & p);

      virtual PILParPrompt & Prompt();
      virtual PILParPrompt & Prompt(const std::string & pname);
      virtual PILParPrompt & Prompt(const std::vector<std::string> & pnames);

      virtual int Argc() const { return mFile->Argc(); }
      virtual char ** const Argv() const { return mFile->Argv(); }
      virtual IParGroup & Group();
      virtual const IParGroup & Group() const;

      virtual IParGroup * SetGroup(IParGroup * group = 0);

      virtual IParPrompt * Clone() const;

    protected:
      void Init(int argc, char ** argv, const std::string & comp_name = std::string());
      mutable PILParFile * mFile;
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
 * $Log: hoops_pil.h,v $
 * Revision 1.14  2004/11/09 18:55:45  peachey
 * Refactor PILParFile to include a constructor which accepts command line
 * arguments. Remove SetArgc, SetArgv from PILParPrompt. Refactor
 * PILParPrompt so that it uses PILParFile to manage its command line
 * arguments.
 *
 * Revision 1.13  2004/03/30 21:14:46  peachey
 * Allow PILParPrompt and ParPromptGroup constructors to accept optional
 * component name which is then used in lieu of argv[0]
 *
 * Revision 1.12  2004/03/16 20:11:20  peachey
 * Change PILParFile so that it can be used by PILParPrompt to open/
 * close the parameter file excplicitly. Change PILParPrompt so that it
 * uses its member PILParFile to manage calls to PILInit. All this is
 * so that PILParFile's code which handles stripping path and extension
 * can be used by PILParPrompt.
 *
 * Revision 1.11  2004/03/16 14:36:58  peachey
 * Remove default construction option for ParFile and ParPrompt.
 *
 * Revision 1.10  2004/03/15 13:58:31  peachey
 * Add Clone method to IParFile and IParPrompt and subclasses.
 * Have PILParFile::Group() create a group if it doesn't have
 * one rather than throw an exception.
 *
 * Revision 1.9  2004/03/12 15:40:42  peachey
 * When throwing exceptions, include the file name and
 * line number where the exception was thrown.
 *
 * Revision 1.8  2004/03/10 19:35:19  peachey
 * Remove throw specifications.
 *
 * Revision 1.7  2003/11/26 18:50:03  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.6  2003/11/13 19:29:29  peachey
 * Add preprocessor macro needed on Windows to export symbols.
 *
 * Revision 1.5  2003/11/10 18:19:45  peachey
 * Moved header files into hoops subdirectory.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:30  jchiang
 * First import
 *
 * Revision 1.4  2003/06/06 20:50:21  peachey
 * From PILParFile, remove Open() and Close(), which are actually
 * redundant with Load() and Save(). Change PILParPrompt::SetGroup
 * so that it returns the current group, thus overriding the base
 * class method. Add similar SetGroup method to PILParFile.
 *
 * These changes prompted some changes in the memory management.
 * The member parameter group object of PILParFile is now
 * deleted only by the destructor. This allows calling code to
 * cause of group object to be "adopted" by the IParFile object.
 * Also, by default, the parameters prompted for are *not* saved,
 * allowing the developer to choose when saving occurs.
 *
 * Add new virtual constructor to PILParPromptFactory, to be
 * consistent with PILParPrompt constructors.
 *
 * Revision 1.3  2003/06/05 15:46:08  peachey
 * Move PIL-related factories into a separate header.
 *
 * Revision 1.2  2003/04/23 17:44:38  peachey
 * Use new abstract base class IParGroup instead of ParGroup.
 *
 * Revision 1.1  2003/04/11 19:20:38  peachey
 * New component HOOPS, an object oriented parameter interface. Low
 * level access currently uses PIL, but this can be changed.
 *
 ******************************************************************************/
