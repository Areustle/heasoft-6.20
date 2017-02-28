/******************************************************************************
 *   File name: hoops_ape.cxx                                                 *
 *                                                                            *
 * Description: Implementation of Ape-based interface.                        *
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
#include "hoops/hoops.h"
#include "hoops/hoops_group.h"
#include "hoops/hoops_par.h"
#include "hoops/hoops_ape.h"
#include "hoops/hoops_ape_factory.h"
#include "hoops/hoops_prim.h"

#include "ape/ape_error.h"
#include "ape/ape_list.h"
#include "ape/ape_io.h"
#include "ape/ape_par.h"
#include "ape/ape_trad.h"

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

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
  // Begin ApeException implementation.
  //////////////////////////////////////////////////////////////////////////////
  ApeException::ApeException(const int & code, const std::string & msg,
    const std::string & filename, int line):
    Hexception(ape2Hoops(code), filename, line), mApeCode(code) {
      std::ostringstream os;
      os << msg << "; Ape exception code " << code;
      format(os.str());
    }

  int ApeException::ApeCode() const { return mApeCode; }

  int ApeException::ape2Hoops(int status) {
    // Convert ape error codes into hoops error codes in the cases where
    // there is a correspondence.
    // Note these are placed in order of the Hoops exceptions rather than
    // the order of Ape exceptions.
    // Note also that Hoops has two different sets of codes. The ones that
    // start with P_ are parameter-related, i.e. they are concerned with
    // flagging conversion/parsing problems of interpreting parameter values.
    // The ones that start with PAR_ are in a different numerical range, and
    // are concerned with code problems such as null pointers and
    // unsupported operations.
    switch (status) {
      // Begin P_ Hoops exception codes:
      case eOK:
        status = P_OK; break;
      // The following has no Ape equivalent:
      //  status = P_ILLEGAL; break;
      case eOverflow:
        status = P_OVERFLOW; break;
      case eUnderflow:
        status = P_UNDERFLOW; break;
      // The following has no Ape equivalent:
      //  status = P_BADSIZE; break;
      case eTypeMismatch:
        status = P_PRECISION; break;
      // The following has no Ape equivalent:
      //  status = P_SIGNEDNESS; break;
      case eStringRemainder:
        status = P_STR_OVERFLOW; break;
      case eConversionError:
        status = P_STR_INVALID; break;
      // The following has no Ape equivalent:
      //  status = P_STR_NULL; break;
      case eNan:
        status = P_INFINITE; break;
      case eUndefinedValue:
        status = P_UNDEFINED; break;
      // DO NOT CONVERT ANYTHING TO P_UNEXPECTED, which is used just by test code.
      //  status = P_UNEXPECTED; break;
      // End P_ Hoops exception codes:

      // Begin PAR_ Hoops exception codes:
      // The following is redundant with P_OK:
      //  status = PAR_NONE; break;
      // The following has no Ape equivalent:
      //  status = PAR_UNSUPPORTED; break;
      case eUnknownType:
        status = PAR_INVALID_TYPE; break;
      // The following has no Ape equivalent:
      //  status = PAR_ILLEGAL_CONVERSION; break;
      case eParNotFound:
        status = PAR_NOT_FOUND; break;
      // The following has no Ape equivalent:
      //  status = PAR_FILE_CORRUPT; break;
      case eFileWriteError:
        status = PAR_FILE_WRITE_ERROR; break;
      case eNullPointer:
        status = PAR_NULL_PTR; break;
      // The following has no Ape equivalent:
      //  status = PAR_COMP_UNDEF; break;
      // End PAR_ Hoops exception codes:

      default:
        status = P_CODE_ERROR; break;
    }
    return status;
  }

  //////////////////////////////////////////////////////////////////////////////
  // End ApeException implementation.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin HoopsApeFile implementation.
  //////////////////////////////////////////////////////////////////////////////
  HoopsApeFile::HoopsApeFile(const HoopsApeFile & pf): IParFile(),
    mComponent(pf.mComponent), mGroup(0), mArgc(0), mArgv(0) {
    if (pf.mGroup) mGroup = pf.mGroup->Clone();
    SetArgs(pf.mArgc, pf.mArgv);
  }

  HoopsApeFile::HoopsApeFile(const IParFile & pf): IParFile(),
    mComponent(pf.Component()), mGroup(0), mArgc(0), mArgv(0) {
    mGroup = pf.Group().Clone();
    SetArgs(0, 0);
    Load();
  }

  HoopsApeFile::HoopsApeFile(const std::string & comp, int argc, char ** argv):
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

  HoopsApeFile::~HoopsApeFile() { SetArgs(0, 0); delete mGroup; }

  HoopsApeFile & HoopsApeFile::operator =(const HoopsApeFile & pf) {
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

  HoopsApeFile & HoopsApeFile::operator =(const IParFile & pf) {
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
  void HoopsApeFile::Load() {
    try {
      int status = eOK;

      // Open the par file specified by the component and path fields.
      OpenParFile();

      // Get current parameter file object.
      ApeParFile * current = 0;
      status = ape_trad_get_current(&current);
      if (eOK != status)
        throw ApeException(status,
        "Could not get parameter file object for " + mComponent,
        __FILE__, __LINE__);

      // Get container of parameters in file.
      ApeList * par_cont = 0;
      status = ape_io_get_par_cont(current, &par_cont);
      if (eOK != status)
        throw ApeException(status,
        "Could not get parameter container for " + mComponent,
        __FILE__, __LINE__);

      // At this point, no further problems _should_ happen, so go
      // ahead and clear the current parameter group.
      if (mGroup) mGroup->Clear(); else mGroup = new ParGroup(mComponent);

      // Iterate through container.
      for (ApeListIterator par_itor = ape_list_begin(par_cont);
        eOK == status && par_itor != ape_list_end(par_cont); par_itor = ape_list_next(par_itor)) {
        // Get each Ape parameter.
        ApePar * par = (ApePar *) ape_list_get(par_itor);
        if (0 != par) {
          static const ParFieldId field_id [] = { eName, eType, eMode, eValue, eMin, eMax, ePrompt };
          char * field[eEndOfField] = { 0, 0, 0, 0, 0, 0, 0 };
          char * comment = 0;
          for (std::size_t ii = eName; eOK == status && eEndOfField != ii; ++ii) {
            status = ape_par_get_field(par, field_id[ii], field + ii);
          }
          // Tolerate missing fields in the hopes that the parameter format is OK.
          if (eFieldNotFound == status) status = eOK;
          if (eOK == status) {
            status = ape_par_get_comment(par, &comment);

          }
#define SAFE(A) (0!=A?A:"")
          if (eOK == status) {
            if ('\0' != *field[eName])
              mGroup->Add(new Par(SAFE(field[eName]), SAFE(field[eType]), SAFE(field[eMode]),
                SAFE(field[eValue]), SAFE(field[eMin]), SAFE(field[eMax]), SAFE(field[ePrompt]), SAFE(comment)));
            else
              mGroup->Add(new Par("", "", "", "", "", "", "", SAFE(comment)));
#undef SAFE
          }
          free(comment); comment = 0;
          for (std::size_t ii = eEndOfField; eName != ii; --ii) {
            std::free(field[ii - 1]);
          }
        }
        if (eOK != status)
          throw ApeException(status, "Problem loading parameters for " + mComponent,
          __FILE__, __LINE__);
      }
      
    } catch (...) {
      CloseParFile(-1);
      throw;
    }

    CloseParFile();
  }

  void HoopsApeFile::Save() const {
    try {
      if (mGroup) {
        int status = eOK;
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

          if (P_INFINITE == par->Status() || P_UNDEFINED == par->Status()) {
            status = ape_trad_set_string(par->Name().c_str(), par->Value().c_str());
            if (eOK != status) {
              err_stream << "Problem setting parameter " << par->Name() <<
                  " for component " << mComponent;
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("b")) {
            bool p = *par;
            status = ape_trad_set_bool(par->Name().c_str(), p);
            if (eOK != status) {
              err_stream << "Problem setting boolean parameter " << par->Name() <<
                " for component " << mComponent;
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("f")) {
            status = ape_trad_set_file_name(par->Name().c_str(), *par);
            if (eOK != status) {
              err_stream << "Problem setting file parameter " << par->Name() <<
                " for component " << mComponent;
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("i")) {
            long p = *par;
            status = ape_trad_set_long(par->Name().c_str(), p);
            if (eOK != status) {
              err_stream << "Problem setting int parameter " << par->Name() <<
                " for component " << mComponent;
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("r")) {
            status = ape_trad_set_double(par->Name().c_str(), *par);
            if (eOK != status) {
              err_stream << "Problem setting real parameter " << par->Name() <<
                " for component " << mComponent;
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else if (std::string::npos != type.find("s")) {
            status = ape_trad_set_string(par->Name().c_str(), *par);
            if (eOK != status) {
              err_stream << "Problem setting string parameter " << par->Name() <<
                " for component " << mComponent;
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
          } else {
            status = PAR_INVALID_TYPE;
            err_stream << "Parameter " << par->Name() << " in component " << mComponent
              << " has invalid type \"" << type << "\"";
            throw Hexception(status, err_stream.str(), __FILE__, __LINE__);
          }
        }
      } else {
        throw Hexception(PAR_NULL_PTR, "Attempt to save a NULL group of parameters", __FILE__, __LINE__);
      }
    } catch (...) {
      CloseParFile(-1);
      throw;
    }
    CloseParFile();
  }

  IParGroup & HoopsApeFile::Group() {
    if (!mGroup) mGroup = new ParGroup(mComponent);
    return *mGroup;
  }

  const IParGroup & HoopsApeFile::Group() const {
    if (!mGroup) mGroup = new ParGroup(mComponent);
    return *mGroup;
  }

  HoopsApeFile & HoopsApeFile::SetComponent(const std::string & comp) {
    CleanComponent(comp, mComponent);
    return *this;
  }

  IParGroup * HoopsApeFile::SetGroup(IParGroup * group)
    { IParGroup * retval = mGroup; mGroup = group; return retval; }

  GenParItor HoopsApeFile::begin() {
    if (!mGroup) {
      std::ostringstream s;
      s << "Attempt to find the beginning of a NULL group of parameters for " <<
        mComponent;
      throw Hexception(PAR_NULL_PTR, s.str(), __FILE__, __LINE__);
    }
    return mGroup->begin();
  }

  ConstGenParItor HoopsApeFile::begin() const {
    if (!mGroup) {
      std::ostringstream s;
      s << "Attempt to find the beginning of a NULL group of parameters for " <<
        mComponent << " (const)";
      throw Hexception(PAR_NULL_PTR, s.str(), __FILE__, __LINE__);
    }
    return static_cast<const IParGroup *>(mGroup)->begin();
  }

  GenParItor HoopsApeFile::end() {
    if (!mGroup) {
      std::ostringstream s;
      s << "Attempt to find the end of a NULL group of parameters for " <<
        mComponent;
      throw Hexception(PAR_NULL_PTR, s.str(), __FILE__, __LINE__);
    }
    return mGroup->end();
  }

  ConstGenParItor HoopsApeFile::end() const {
    if (!mGroup) {
      std::ostringstream s;
      s << "Attempt to find the end of a NULL group of parameters for " <<
        mComponent << " (const)";
      throw Hexception(PAR_NULL_PTR, s.str(), __FILE__, __LINE__);
    }
    return static_cast<const IParGroup *>(mGroup)->end();
  }

  IParFile * HoopsApeFile::Clone() const { return new HoopsApeFile(*this); }

  void HoopsApeFile::CleanComponent(const std::string & comp, std::string & clean)
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
  void HoopsApeFile::OpenParFile() const {
    int status = eOK;

    // Open the par file with the new arguments.
    // Don't throw an exception (if necessary) until after the
    // clean up block.
    status = ape_trad_init(mArgc, mArgv);

    if (eOK != status) {
      std::ostringstream s;
      s << "Cannot open parameter file for " << mComponent;
      throw ApeException(status, s.str(), __FILE__, __LINE__);
    }

  }

  void HoopsApeFile::CloseParFile(int status) const {
    ape_trad_close(0 == status ? 1 : 0);
  }

  void HoopsApeFile::SetArgs(int argc, char ** argv) {
    // Check new arguments.
    if (0 > argc)
      throw Hexception(PAR_NULL_PTR, "Number of arguments cannot be negative", __FILE__, __LINE__);

    // Delete current set of arguments.
    if (0 != mArgc) {
      for (int ii = mArgc - 1; ii >= 0; --ii)
        delete [] mArgv[ii];
      delete [] mArgv;
      mArgv = 0;
      mArgc = 0;
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
  // End HoopsApeFile implementation.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin HoopsApePrompt implementation.
  //////////////////////////////////////////////////////////////////////////////
  HoopsApePrompt::HoopsApePrompt(const HoopsApePrompt & prompt):
    IParPrompt(), mFile(0) {
    mFile = new HoopsApeFile(*prompt.mFile);
  }

  HoopsApePrompt::HoopsApePrompt(const IParPrompt & prompt):
    IParPrompt(), mFile(0) {
    mFile = new HoopsApeFile("", prompt.Argc(), prompt.Argv());
    mFile->Group() = prompt.Group();
  }

  HoopsApePrompt::HoopsApePrompt(int argc, char ** argv,
    const std::string & comp_name):
    IParPrompt(), mFile(0) {
    mFile = new HoopsApeFile(comp_name, argc, argv);
  }

  HoopsApePrompt::~HoopsApePrompt() { delete mFile; }

  HoopsApePrompt & HoopsApePrompt::operator =(const HoopsApePrompt & p) {
    *mFile = *p.mFile;
    return *this;
  }

  HoopsApePrompt & HoopsApePrompt::operator =(const IParPrompt & p) {
    delete mFile;
    mFile = new HoopsApeFile("", p.Argc(), p.Argv());
    return *this;
  }

  HoopsApePrompt & HoopsApePrompt::Prompt() {
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

  HoopsApePrompt & HoopsApePrompt::Prompt(const std::string & pname) {
    // Make a list of one parameter name: pname.
    std::vector<std::string> plist;
    plist.push_back(pname);

    // Pass list to overloaded Prompt().
    return Prompt(plist);
  }

  HoopsApePrompt & HoopsApePrompt::Prompt(const std::vector<std::string> & pnames) {
    int status = eOK;

    try {
      mFile->OpenParFile();

      std::string type;
      std::vector<std::string>::const_iterator it;

      // Loop over parameter names in the list.
      for (it = pnames.begin(); it != pnames.end(); ++it) {
        if(it->empty()) continue; // Skip blank/comment lines.

        // Find the corresponding parameter in the group.
        IPar & par = mFile->Group().Find(*it);

        // Prompt using the appropriate function.
        type = par.Type();

        std::ostringstream err_stream;
        try {
          if (std::string::npos != type.find("b")) {
            char r = 0;
            status = ape_trad_query_bool(it->c_str(), &r);
            if (eOK != status) {
              err_stream << "Exception while querying for boolean parameter " << *it <<
                " for component " << mFile->Component();
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
            par = 0 != r;
          } else if (std::string::npos != type.find("f")) {
            char * r = 0;
            status = ape_trad_query_file_name(it->c_str(), &r);
            if (eOK != status) {
              free(r); r = 0;
              err_stream << "Exception while querying for file name parameter " << *it <<
                " for component " << mFile->Component();
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
            par = r;
            free(r); r = 0;
          } else if (std::string::npos != type.find("i")) {
            long r = 0;
            status = ape_trad_query_long(it->c_str(), &r);
            if (eOK != status) {
              err_stream << "Exception while querying for int parameter " << *it <<
                " for component " << mFile->Component();
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
            par = r;
          } else if (std::string::npos != type.find("r")) {
            double r = 0.;
            status = ape_trad_query_double(it->c_str(), &r);
            if (eOK != status) {
              err_stream << "Exception while querying for real parameter " << *it <<
                " for component " << mFile->Component();
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
            par = r;
          } else if (std::string::npos != type.find("s")) {
            char * r = 0;
            status = ape_trad_query_string(it->c_str(), &r);
            if (eOK != status) {
              free(r); r = 0;
              err_stream << "Exception while querying for string parameter " << *it <<
                " for component " << mFile->Component();
              throw ApeException(status, err_stream.str(), __FILE__, __LINE__);
            }
            par = r;
            free(r); r = 0;
          } else {
            status = PAR_INVALID_TYPE;
            err_stream << "Unable to query for parameter of type \"" << type <<
              "\" for component " << mFile->Component();
            throw Hexception(status, err_stream.str(), __FILE__, __LINE__);
          }
        } catch (const ApeException & x) {
          if (P_INFINITE == x.Code() || P_UNDEFINED == x.Code()) {
            char * r = 0;
            status = ape_trad_get_string(it->c_str(), &r);
            par = r;
            free(r); r = 0;
          } else {
            throw;
          }
        }

      }
    } catch (...) {
      // Clean up: make sure parameter file is closed, and file object cleaned.
      mFile->CloseParFile(-1); // Call with non-0 argument so pars wont be saved.
      throw;
    }

    // Clean up.
    mFile->CloseParFile(-1); // Don't save parameters at this point.

    // This non-specific error message is just in case something didn't
    // get handled more specifically above.
    if (eOK != status) throw ApeException(status, "Exception during prompting", __FILE__, __LINE__);
    return *this;
  }

  IParGroup & HoopsApePrompt::Group() {
    return mFile->Group();
  }

  const IParGroup & HoopsApePrompt::Group() const {
    return mFile->Group();
  }

  IParGroup * HoopsApePrompt::SetGroup(IParGroup * group)
    { IParGroup * retval = &mFile->Group(); mFile->SetGroup(group); return retval; }

  IParPrompt * HoopsApePrompt::Clone() const { return new HoopsApePrompt(*this); }
  //////////////////////////////////////////////////////////////////////////////
  // End HoopsApePrompt implementation.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin HoopsApeFileFactory implementation.
  //////////////////////////////////////////////////////////////////////////////
  IParFile * HoopsApeFileFactory::NewIParFile(const IParFile & p)
    { return new HoopsApeFile(p); }

  IParFile * HoopsApeFileFactory::NewIParFile(const std::string & comp_name)
    { return new HoopsApeFile(comp_name); }
  //////////////////////////////////////////////////////////////////////////////
  // End HoopsApeFileFactory implementation.
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Begin HoopsApePromptFactory implementation.
  //////////////////////////////////////////////////////////////////////////////
  IParPrompt * HoopsApePromptFactory::NewIParPrompt(const IParPrompt & p)
    { return new HoopsApePrompt(p); }

  IParPrompt * HoopsApePromptFactory::NewIParPrompt(int argc, char ** argv, const std::string & comp_name)
    { return new HoopsApePrompt(argc, argv, comp_name); }
  //////////////////////////////////////////////////////////////////////////////
  // End HoopsApePromptFactory implementation.
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
 * $Log: hoops_ape.cxx,v $
 * Revision 1.9  2013/04/19 18:23:43  peachey
 * When resetting arguments, reset the count as well as the pointer to arguments.
 *
 * Revision 1.8  2010/01/15 21:11:27  peachey
 * Ensure that undefined and infinite values work when supplied in
 * response to a prompt.
 *
 * Revision 1.7  2010/01/07 19:21:39  peachey
 * Add ApeCode() method and mApeCode member for getting Ape's status.
 * Be more careful about not passing null char ptrs to std::string constructor.
 *
 * Revision 1.6  2010/01/07 17:56:50  peachey
 * Improve and make universal the conversion of ape error codes to hoops codes.
 *
 * Revision 1.5  2010/01/06 20:04:36  peachey
 * When saving parameter file, handle special case of infinite or undefined
 * parameters by writing them as a string regardless of the underlying
 * parameter type.
 *
 * Revision 1.4  2009/12/23 20:31:32  peachey
 * Convert ape error codes into proper hoops error codes.
 *
 * Revision 1.3  2009/12/03 18:21:30  elwinter
 * Fixed code to compile under Ubuntu 9.10.
 *
 * Revision 1.2  2008/11/21 22:03:05  peachey
 * Use base class implementation of error message facility.
 *
 * Revision 1.1  2007/02/15 21:33:58  peachey
 * Initial version of Ape-based implementation.
 *
 ******************************************************************************/
