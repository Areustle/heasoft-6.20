/******************************************************************************
 *   File name: hoops_pil.cxx                                                 *
 *                                                                            *
 * Description: Implementation of PIL-based interface.                        *
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
// stdio.h is needed only because pil.h neglects to include it.
#include <sstream>
#include <stdio.h>
#include "hoops/hoops.h"
#include "hoops/hoops_group.h"
#include "hoops/hoops_par.h"
#include "hoops/hoops_pil.h"
#include "hoops/hoops_pil_factory.h"
#include "hoops/hoops_prim.h"

// Suppress generation of unused static pil version string variable.
#ifndef NO_PIL_VERSION_STRING
#define NO_PIL_VERSION_STRING 1
#endif
#include "pil.h"

// PIL uses a global for holding the prompt mode, but doesn't have a
// read accessor method for it.
extern int PILQueryMode;

////////////////////////////////////////////////////////////////////////////////
namespace hoops {

  //////////////////////////////////////////////////////////////////////////////
  // Constants.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Static function declarations.
  //////////////////////////////////////////////////////////////////////////////
  static char *CpyStr(const char *s);
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type definitions.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin PILException implementation.
  //////////////////////////////////////////////////////////////////////////////
  PILException::PILException(const int & code, const std::string & msg,
    const std::string & filename, int line):
    Hexception(code, filename, line) { format(msg); }

  void PILException::format(const std::string & msg) {
    const char * pil_msg = PIL_err_handler(mCode);
    if (msg.empty() && 0 != pil_msg) {
      Hexception::format(std::string("pil error: ") + pil_msg);
    } else Hexception::format(msg);
  }
  //////////////////////////////////////////////////////////////////////////////
  // End PILException implementation.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin PILParFile implementation.
  //////////////////////////////////////////////////////////////////////////////
  PILParFile::PILParFile(const PILParFile & pf): IParFile(),
    mComponent(pf.mComponent), mGroup(0), mArgc(0), mArgv(0) {
    if (pf.mGroup) mGroup = pf.mGroup->Clone();
    SetArgs(pf.mArgc, pf.mArgv);
  }

  PILParFile::PILParFile(const IParFile & pf): IParFile(),
    mComponent(pf.Component()), mGroup(0), mArgc(0), mArgv(0) {
    mGroup = pf.Group().Clone();
    SetArgs(0, 0);
    Load();
  }

  PILParFile::PILParFile(const std::string & comp, int argc, char ** argv):
    IParFile(), mComponent(), mGroup(0), mArgc(0), mArgv(0) {
    if (comp.empty()) {
      SetComponent(argv[0]);
      SetArgs(argc, argv);
    } else {
      SetComponent(comp);
      ++argc;
      char ** new_argv = new char *[argc];
      new_argv[0] = CpyStr(comp.c_str());
      for (int ii = 1; ii < argc; ++ii) {
        new_argv[ii] = argv[ii - 1];
      }
      SetArgs(argc, new_argv);
      delete [] new_argv[0];
      delete [] new_argv;
    }
    Load();
  }

  PILParFile::~PILParFile() { SetArgs(0, 0); delete mGroup; }

  PILParFile & PILParFile::operator =(const PILParFile & pf) {
    mComponent = pf.mComponent;
    if (mGroup) {
      if (pf.mGroup) *mGroup = *pf.mGroup;
      else mGroup->Clear();
    } else {
      if (pf.mGroup) mGroup = pf.mGroup->Clone();
    }
    SetArgs(pf.mArgc, pf.mArgv);
    return *this;
  }

  PILParFile & PILParFile::operator =(const IParFile & pf) {
    mComponent = pf.Component();
    if (mGroup) {
      *mGroup = pf.Group();
    } else {
      mGroup = pf.Group().Clone();
    }
    SetArgs(0, 0);
    return *this;
  }

  // Synchronize memory image with parameter file and vice versa.
  void PILParFile::Load() {
    PIL_PARAM * pp = 0;

    try {
      int status = PIL_OK;

      // Use PIL to open the par file specified by the component and
      // path fields.
      OpenParFile();

      // Get number of parameters.
      int nPar = 0;
      status = PILGetNumParameters(&nPar);
      if (PIL_OK != status)
        throw PILException(status,
        "Could not get the number of parameters for component " + mComponent,
        __FILE__, __LINE__);

      // Make space to store parameter array.
      pp = new PIL_PARAM[nPar];

      // Loop over parameters in file, reading them all into local memory.
      PIL_VALUE vmin;
      PIL_VALUE vmax;
      int minmaxok;
      for (int ii = 0; ii < nPar; ++ii) {
        memset(pp + ii, 0, sizeof(PIL_PARAM));
        status = PILGetParameter(ii, pp + ii, &minmaxok, &vmin, &vmax);
        if (PIL_OK != status) {
          std::ostringstream s;
          s << "Problem reading the " << ii + 1 <<
            "th line in the parameter file for component " << mComponent;
          throw PILException(status, s.str(), __FILE__, __LINE__);
        }

        // Make sure parameters are valid:
        if (!IPrim::IsBlank(pp[ii].strline) &&
            PIL_FORMAT_BLANK != pp[ii].format &&
            PIL_FORMAT_COMMENT != pp[ii].format &&
            PIL_FORMAT_OK != pp[ii].format) {
          status = PAR_FILE_CORRUPT;
          std::ostringstream s;
          s << "The " << ii + 1 << "th line in the parameter file for component "
            << mComponent << " is invalid";
          throw PILException(status, s.str(), __FILE__, __LINE__);
        }
      }

      // At this point, no further problems _should_ happen, so go
      // ahead and clear the current parameter group.
      if (mGroup) mGroup->Clear(); else mGroup = new ParGroup(mComponent);

      // Loop over local copy of parameters, and use them to create
      // the parameter group.
      bool inquote;
      const char * comment;
      for (int ii = 0; ii < nPar; ++ii) {
        // Handle comments. They start with an unquoted #.
        inquote = false;
        if (pp[ii].strline) {
          comment = pp[ii].strline;
          while (*comment) {
            if (inquote) {
              if ('"' == *comment) inquote = false;
            } else {
              if ('"' == *comment) inquote = true;
              else if ('#' == *comment) break;
            }
            ++comment;
          }
          // Back up to include whitespace before the comment.
          while (comment > pp[ii].strline && isspace(*(comment - 1))) --comment;
        } else comment = "";

        // There is a check above to make sure the line is valid.
        // At this point, we can therefore assume that the 7 standard
        // fields are either all present, or all absent (blank line or
        // comment line.)
        if (pp[ii].strname)
          mGroup->Add(new Par(pp[ii].strname, pp[ii].strtype, pp[ii].strmode,
              pp[ii].strvalue, pp[ii].strmin, pp[ii].strmax, pp[ii].strprompt,
              comment));
        else
          mGroup->Add(new Par("", "", "", "", "", "", "", comment));
      }
    } catch (...) {
      delete [] pp;
      CloseParFile(-1);
      throw;
    }

    delete [] pp;
    CloseParFile();
  }

  void PILParFile::Save() const {
    try {
      if (mGroup) {
        int status = PIL_OK;
        const IParGroup * constGroup = mGroup;

        OpenParFile();

        // Loop over parameter group in memory, and save each parameter.
        const IPar * par;
        ConstGenParItor it;
        std::ostringstream err_stream;
        for (it = constGroup->begin(); it != constGroup->end(); ++it) {
          par = *it;
          if (par->Name().empty()) continue;
          const std::string & type = par->Type();
          if (std::string::npos != type.find("b")) {
            bool p = *par;
            status = PILPutBool(par->Name().c_str(), p);
            if (PIL_OK != status) {
              err_stream << "Could not write boolean parameter " << par->Name() <<
                " for component " << mComponent;
              throw PILException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("f")) {
            status = PILPutFname(par->Name().c_str(), *par);
            if (PIL_OK != status) {
              err_stream << "Could not write file name parameter " << par->Name() <<
                " for component " << mComponent;
              throw PILException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("i")) {
            long p = *par;
            status = PILPutInt(par->Name().c_str(), p);
            if (PIL_OK != status) {
              err_stream << "Could not write int parameter " << par->Name() <<
                " for component " << mComponent;
              throw PILException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("r")) {
            status = PILPutReal(par->Name().c_str(), *par);
            if (PIL_OK != status) {
              err_stream << "Could not write real parameter " << par->Name() <<
                " for component " << mComponent;
              throw PILException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("s")) {
            status = PILPutString(par->Name().c_str(), *par);
            if (PIL_OK != status) {
              err_stream << "Could not write string parameter " << par->Name() <<
                " for component " << mComponent;
              throw PILException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else {
            status = PAR_INVALID_TYPE;
            err_stream << "Parameter " << par->Name() << " in component " << mComponent
              << " has invalid type " << type;
            throw PILException(status, err_stream.str(), __FILE__, __LINE__);
          }
        }
      } else {
        throw PILException(PAR_NULL_PTR, "Attempt to save a NULL group of parameters", __FILE__, __LINE__);
      }
    } catch (...) {
      CloseParFile(-1);
      throw;
    }
    CloseParFile();
  }

  IParGroup & PILParFile::Group() {
    if (!mGroup) mGroup = new ParGroup(mComponent);
    return *mGroup;
  }

  const IParGroup & PILParFile::Group() const {
    if (!mGroup) mGroup = new ParGroup(mComponent);
    return *mGroup;
  }

  PILParFile & PILParFile::SetComponent(const std::string & comp) {
    CleanComponent(comp, mComponent);
    return *this;
  }

  IParGroup * PILParFile::SetGroup(IParGroup * group)
    { IParGroup * retval = mGroup; mGroup = group; return retval; }

  GenParItor PILParFile::begin() {
    if (!mGroup) {
      std::ostringstream s;
      s << "Attempt to find the beginning of a NULL group of parameters for " <<
        mComponent;
      throw PILException(PAR_NULL_PTR, s.str(), __FILE__, __LINE__);
    }
    return mGroup->begin();
  }

  ConstGenParItor PILParFile::begin() const {
    if (!mGroup) {
      std::ostringstream s;
      s << "Attempt to find the beginning of a NULL group of parameters for " <<
        mComponent << " (const)";
      throw PILException(PAR_NULL_PTR, s.str(), __FILE__, __LINE__);
    }
    return static_cast<const IParGroup *>(mGroup)->begin();
  }

  GenParItor PILParFile::end() {
    if (!mGroup) {
      std::ostringstream s;
      s << "Attempt to find the end of a NULL group of parameters for " <<
        mComponent;
      throw PILException(PAR_NULL_PTR, s.str(), __FILE__, __LINE__);
    }
    return mGroup->end();
  }

  ConstGenParItor PILParFile::end() const {
    if (!mGroup) {
      std::ostringstream s;
      s << "Attempt to find the end of a NULL group of parameters for " <<
        mComponent << " (const)";
      throw PILException(PAR_NULL_PTR, s.str(), __FILE__, __LINE__);
    }
    return static_cast<const IParGroup *>(mGroup)->end();
  }

  IParFile * PILParFile::Clone() const { return new PILParFile(*this); }

  void PILParFile::CleanComponent(const std::string & comp, std::string & clean)
    const {
    clean = comp;
    // Trim path name.
    std::string::size_type pos = clean.rfind("/");
    if (std::string::npos != pos) clean.erase(0, pos + 1);

    // Trim extension.
    pos = clean.rfind(".");
    if (std::string::npos != pos) clean.erase(pos);
  }

  // Note: in this method, argc and argv must *not* include
  // the name of the tool (traditionally argv[0])
  void PILParFile::OpenParFile() const {
    int status = PIL_OK;

    // Set the component/module name.
    PILSetModuleName(mComponent.c_str());

    // Use PIL to open the par file with the new arguments.
    // Don't throw an exception (if necessary) until after the
    // clean up block.
    status = PILInit(mArgc, mArgv);

    if (PIL_OK != status) {
      std::ostringstream s;
      s << "Could not open parameter file for " << mComponent;
      throw PILException(status, s.str(), __FILE__, __LINE__);
    }

    status = PILVerifyCmdLine();
    if (PIL_OK != status) {
      std::ostringstream s;
      s << "Invalid command line for " << mComponent;
      throw PILException(status, s.str(), __FILE__, __LINE__);
    }
  }

  void PILParFile::CloseParFile(int status) const {
    PILClose(status);
  }

  void PILParFile::SetArgs(int argc, char ** argv) {
    // Check new arguments.
    if (0 > argc)
      throw Hexception(PAR_NULL_PTR, "Number of arguments cannot be negative", __FILE__, __LINE__);

    // Delete current set of arguments.
    if (0 != mArgc) {
      for (int ii = mArgc - 1; ii >= 0; --ii)
        delete [] mArgv[ii];
      delete [] mArgv;
      mArgv = 0;
    }

    // Copy new arguments.
    if (0 != argc) {
      mArgc = argc;
      if (0 != mArgc) {
        mArgv = new char *[mArgc];
        for (int ii = 0; ii < mArgc; ++ii)
          mArgv[ii] = CpyStr(argv[ii]);
      }
    }
  }
  //////////////////////////////////////////////////////////////////////////////
  // End PILParFile implementation.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin PILParPrompt implementation.
  //////////////////////////////////////////////////////////////////////////////
  PILParPrompt::PILParPrompt(const PILParPrompt & prompt):
    IParPrompt(), mFile(0) {
    mFile = new PILParFile(*prompt.mFile);
  }

  PILParPrompt::PILParPrompt(const IParPrompt & prompt):
    IParPrompt(), mFile(0) {
    mFile = new PILParFile("", prompt.Argc(), prompt.Argv());
    mFile->Group() = prompt.Group();
  }

  PILParPrompt::PILParPrompt(int argc, char ** argv,
    const std::string & comp_name):
    IParPrompt(), mFile(0) {
    mFile = new PILParFile(comp_name, argc, argv);
  }

  PILParPrompt::~PILParPrompt() { delete mFile; }

  PILParPrompt & PILParPrompt::operator =(const PILParPrompt & p) {
    *mFile = *p.mFile;
    return *this;
  }

  PILParPrompt & PILParPrompt::operator =(const IParPrompt & p) {
    delete mFile;
    mFile = new PILParFile("", p.Argc(), p.Argv());
    return *this;
  }

  PILParPrompt & PILParPrompt::Prompt() {
    const IParGroup & g = mFile->Group();

    // Make a list of all the parameters which may need prompting.
    std::vector<std::string> plist;
    ConstGenParItor it;
    for (it = g.begin(); it != g.end(); ++it) {
      const std::string & name = (*it)->Name();
      if (!name.empty()) plist.push_back(name);
    }

    // Pass list to overloaded Prompt().
    return Prompt(plist);
  }

  PILParPrompt & PILParPrompt::Prompt(const std::string & pname) {
    // Make a list of one parameter name: pname.
    std::vector<std::string> plist;
    plist.push_back(pname);

    // Pass list to overloaded Prompt().
    return Prompt(plist);
  }

  PILParPrompt & PILParPrompt::Prompt(const std::vector<std::string> & pnames) {
    int status = PIL_OK;

    try {
      mFile->OpenParFile();

      std::string type;
      std::vector<std::string>::const_iterator it;

      // Loop over parameter names in the list.
      for (it = pnames.begin(); it != pnames.end(); ++it) {
        if(it->empty()) continue; // Skip blank/comment lines.

        // Find the corresponding parameter in the group.
        IPar & par = mFile->Group().Find(*it);

        // Prompt using the appropriate PILGet function.
        type = par.Type();

        std::ostringstream err_stream;
        if (std::string::npos != type.find("b")) {
          int r = 0;
          status = PILGetBool(it->c_str(), &r);
          if (PIL_OK != status) {
            err_stream << "Cannot get boolean parameter " << *it <<
              " for component " << mFile->Component();
            throw PILException(status, err_stream.str(), __FILE__, __LINE__);
          }
          par = 0 != r;
        } else if (std::string::npos != type.find("f")) {
          char r[PIL_LINESIZE] = "";
          status = PILGetFname(it->c_str(), r);
          if (PIL_OK != status) {
            err_stream << "Cannot get file name parameter " << *it <<
              " for component " << mFile->Component();
            throw PILException(status, err_stream.str(), __FILE__, __LINE__);
          }
          par = r;
        } else if (std::string::npos != type.find("i")) {
          int r = 0;
          status = PILGetInt(it->c_str(), &r);
          if (PIL_OK != status) {
            err_stream << "Cannot get int parameter " << *it <<
              " for component " << mFile->Component();
            throw PILException(status, err_stream.str(), __FILE__, __LINE__);
          }
          par = r;
        } else if (std::string::npos != type.find("r")) {
          double r = 0.;
          status = PILGetReal(it->c_str(), &r);
          if (PIL_OK != status) {
            err_stream << "Cannot get real parameter " << *it <<
              " for component " << mFile->Component();
            throw PILException(status, err_stream.str(), __FILE__, __LINE__);
          }
          par = r;
        } else if (std::string::npos != type.find("s")) {
          char r[PIL_LINESIZE] = "";
          status = PILGetString(it->c_str(), r);
          if (PIL_OK != status) {
            err_stream << "Cannot get string parameter " << *it <<
              " for component " << mFile->Component();
            throw PILException(status, err_stream.str(), __FILE__, __LINE__);
          }
          par = r;
        } else {
          status = PAR_INVALID_TYPE;
          err_stream << "Cannot prompt for parameter of unknown type " << type <<
            " for component " << mFile->Component();
          throw PILException(status, err_stream.str(), __FILE__, __LINE__);
        }

      }
    } catch (...) {
      // Clean up: make sure PIL is closed, and file object cleaned.
      mFile->CloseParFile(-1); // Call with non-0 argument so pars wont be saved.
      throw;
    }

    // Clean up.
    mFile->CloseParFile(-1); // Don't save parameters at this point.

    // This non-specific error message is just in case something didn't
    // get handled more specifically above.
    if (PIL_OK != status) throw PILException(status, "", __FILE__, __LINE__);
    return *this;
  }

  IParGroup & PILParPrompt::Group() {
    return mFile->Group();
  }

  const IParGroup & PILParPrompt::Group() const {
    return mFile->Group();
  }

  IParGroup * PILParPrompt::SetGroup(IParGroup * group)
    { IParGroup * retval = &mFile->Group(); mFile->SetGroup(group); return retval; }

  IParPrompt * PILParPrompt::Clone() const { return new PILParPrompt(*this); }
  //////////////////////////////////////////////////////////////////////////////
  // End PILParPrompt implementation.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin PILParFileFactory implementation.
  //////////////////////////////////////////////////////////////////////////////
  IParFile * PILParFileFactory::NewIParFile(const IParFile & p)
    { return new PILParFile(p); }

  IParFile * PILParFileFactory::NewIParFile(const std::string & comp_name)
    { return new PILParFile(comp_name); }
  //////////////////////////////////////////////////////////////////////////////
  // End PILParFileFactory implementation.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin PILParPromptFactory implementation.
  //////////////////////////////////////////////////////////////////////////////
  IParPrompt * PILParPromptFactory::NewIParPrompt(const IParPrompt & p)
    { return new PILParPrompt(p); }

  IParPrompt * PILParPromptFactory::NewIParPrompt(int argc, char ** argv, const std::string & comp_name)
    { return new PILParPrompt(argc, argv, comp_name); }
  //////////////////////////////////////////////////////////////////////////////
  // End PILParPromptFactory implementation.
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
  // Utility CpyStr: copy a string, allocating a new string
  // just large enough to hold the string being copied.
  static char *CpyStr(const char *s) {
    char *r = 0;
    if (s) {
      char *ptr;
      const char *c_ptr = s;
      while (*c_ptr) ++c_ptr;
      r = new char[c_ptr - s + 1];
      ptr = r;
      while (0 != (*ptr = *s)) { ++s; ++ptr; }
    }
    return r;
  }
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Function definitions.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

}

/******************************************************************************
 * $Log: hoops_pil.cxx,v $
 * Revision 1.22  2006/03/30 14:22:05  peachey
 * Suppress ISDC's static version string.
 *
 * Revision 1.21  2004/11/09 18:55:45  peachey
 * Refactor PILParFile to include a constructor which accepts command line
 * arguments. Remove SetArgc, SetArgv from PILParPrompt. Refactor
 * PILParPrompt so that it uses PILParFile to manage its command line
 * arguments.
 *
 * Revision 1.20  2004/09/21 16:48:46  peachey
 * Report name of group whenever errors are thrown.
 *
 * Revision 1.19  2004/03/31 16:20:33  peachey
 * Make proper boolean expressions instead of using implicit conversion to
 * bool, to silence VC7 performance warnings on Windows.
 *
 * Revision 1.18  2004/03/30 21:14:46  peachey
 * Allow PILParPrompt and ParPromptGroup constructors to accept optional
 * component name which is then used in lieu of argv[0]
 *
 * Revision 1.17  2004/03/26 22:30:37  peachey
 * Improve (make more specific) exception messages.
 *
 * Revision 1.16  2004/03/16 20:50:57  peachey
 * Explicitly invoke constructors for base classes to shut up compiler
 * warnings in the SLAC build.
 *
 * Revision 1.15  2004/03/16 20:11:20  peachey
 * Change PILParFile so that it can be used by PILParPrompt to open/
 * close the parameter file excplicitly. Change PILParPrompt so that it
 * uses its member PILParFile to manage calls to PILInit. All this is
 * so that PILParFile's code which handles stripping path and extension
 * can be used by PILParPrompt.
 *
 * Revision 1.14  2004/03/16 14:39:25  peachey
 * Remove default constructor option for PILParFile and PILParPrompt.
 * Restructure PILParPrompt so that it owns a PILParFile object and
 * uses that for access to its group. Streamline the main Prompt method.
 *
 * Revision 1.13  2004/03/15 13:58:34  peachey
 * Add Clone method to IParFile and IParPrompt and subclasses.
 * Have PILParFile::Group() create a group if it doesn't have
 * one rather than throw an exception.
 *
 * Revision 1.12  2004/03/12 15:40:42  peachey
 * When throwing exceptions, include the file name and
 * line number where the exception was thrown.
 *
 * Revision 1.11  2004/03/10 19:35:29  peachey
 * Remove throw specifications.
 *
 * Revision 1.10  2004/02/25 18:37:29  peachey
 * Remove the extension (.*) from the component name, so that parameter files
 * can be named <application>.par instead of <application>.exe.par.
 *
 * Revision 1.9  2003/11/26 18:50:02  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.8  2003/11/26 17:54:09  peachey
 * Explicitly zero-initialize all pointers, as VS does not adhere to
 * the ISO standard for default-initialized pointers.
 *
 * Revision 1.7  2003/11/10 18:16:12  peachey
 * Moved header files into hoops subdirectory.
 *
 * Revision 1.6  2003/06/18 18:19:59  peachey
 * CpyStr facility moved to IPrim.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:29  jchiang
 * First import
 *
 * Revision 1.5  2003/06/06 20:50:21  peachey
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
 * Revision 1.4  2003/06/06 13:51:21  peachey
 * Default for internal closes should be to forget the parameters.
 *
 * Revision 1.3  2003/06/05 15:46:08  peachey
 * Move PIL-related factories into a separate header.
 *
 * Revision 1.2  2003/04/23 17:45:06  peachey
 * Use new abstract base class IParGroup instead of ParGroup, and new
 * iterator classes.
 *
 * Revision 1.1  2003/04/11 19:20:38  peachey
 * New component HOOPS, an object oriented parameter interface. Low
 * level access currently uses PIL, but this can be changed.
 *
 ******************************************************************************/
