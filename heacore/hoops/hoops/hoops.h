/******************************************************************************
 *   File name: hoops.h                                                       *
 *                                                                            *
 * Description: Public declarations for the HOOPS library.                    *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_H
#define HOOPS_H

////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include "hoops/hoops_exception.h"
#include "hoops/hoops_itor.h"
#include "hoops/hoops_prim.h"
#include <iosfwd>
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
  enum ParErrorCode_e {
    PAR_NONE = 0,
    PAR_UNSUPPORTED = 101,
    PAR_INVALID_TYPE = 102,
    PAR_ILLEGAL_CONVERSION = 103,
    PAR_NOT_FOUND = 104,
    PAR_FILE_CORRUPT = 105,
    PAR_FILE_WRITE_ERROR = 106,
    PAR_NULL_PTR = 107,
    PAR_COMP_UNDEF = 108
  };

  enum ParFileOption_e {
    PF_NONE = 0,
    PF_SAVE = 1,
    PF_FORGET = 2,
    PF_DEFAULT = PF_SAVE
  };

  enum ParPromptOption_e {
    PP_NONE = 0,
    PP_FORCE_PROMPT = 1,
    PP_DEFAULT = PP_NONE
  };
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type declarations/definitions.
  //////////////////////////////////////////////////////////////////////////////
  class EXPSYM IPar {
    public:
      // Destructor.
      virtual ~IPar() {}

      // Assignment operators.
      virtual IPar & operator =(const IPar & p) = 0;
      virtual IPar & operator =(const IPrim & p) = 0;
      virtual IPar & operator =(const bool & p) = 0;
      virtual IPar & operator =(const char & p) = 0;
      virtual IPar & operator =(const signed char & p) = 0;
      virtual IPar & operator =(const short & p) = 0;
      virtual IPar & operator =(const int & p) = 0;
      virtual IPar & operator =(const long & p) = 0;
      virtual IPar & operator =(const unsigned char & p) = 0;
      virtual IPar & operator =(const unsigned short & p) = 0;
      virtual IPar & operator =(const unsigned int & p) = 0;
      virtual IPar & operator =(const unsigned long & p) = 0;
      virtual IPar & operator =(const float & p) = 0;
      virtual IPar & operator =(const double & p) = 0;
      virtual IPar & operator =(const long double & p) = 0;
      virtual IPar & operator =(const char * p) = 0;
      virtual IPar & operator =(const std::string & p) = 0;

      // Conversion operators.
      virtual operator bool () const = 0;
      virtual operator char () const = 0;
      virtual operator signed char () const = 0;
      virtual operator short () const = 0;
      virtual operator int () const = 0;
      virtual operator long () const = 0;
      virtual operator unsigned char () const = 0;
      virtual operator unsigned short () const = 0;
      virtual operator unsigned int () const = 0;
      virtual operator unsigned long () const = 0;
      virtual operator float () const = 0;
      virtual operator double () const = 0;
      virtual operator long double () const = 0;
      virtual operator const char *() const = 0;
      virtual operator const std::string &() const = 0;

      // Conversions from other objects.
      virtual void From(const IPar & p) = 0;
      virtual void From(const IPrim & p) = 0;
      virtual void From(const bool & p) = 0;
      virtual void From(const char & p) = 0;
      virtual void From(const signed char & p) = 0;
      virtual void From(const short & p) = 0;
      virtual void From(const int & p) = 0;
      virtual void From(const long & p) = 0;
      virtual void From(const unsigned char & p) = 0;
      virtual void From(const unsigned short & p) = 0;
      virtual void From(const unsigned int & p) = 0;
      virtual void From(const unsigned long & p) = 0;
      virtual void From(const float & p) = 0;
      virtual void From(const double & p) = 0;
      virtual void From(const long double & p) = 0;
      virtual void From(const char * p) = 0;
      virtual void From(const std::string & p) = 0;

      // Conversions to other objects.
      virtual void To(bool & p) const = 0;
      virtual void To(char & p) const = 0;
      virtual void To(signed char & p) const = 0;
      virtual void To(short & p) const = 0;
      virtual void To(int & p) const = 0;
      virtual void To(long & p) const = 0;
      virtual void To(unsigned char & p) const = 0;
      virtual void To(unsigned short & p) const = 0;
      virtual void To(unsigned int & p) const = 0;
      virtual void To(unsigned long & p) const = 0;
      virtual void To(float & p) const = 0;
      virtual void To(double & p) const = 0;
      virtual void To(long double & p) const = 0;
      virtual void To(std::string & p) const = 0;

      virtual IPar * Clone() const = 0;

      // Member access.
      virtual const std::string & Name() const = 0;
      virtual const std::string & Type() const = 0;
      virtual const std::string & Mode() const = 0;
      virtual const std::string & Value() const = 0;
      virtual const std::string & Min() const = 0;
      virtual const std::string & Max() const = 0;
      virtual const std::string & Prompt() const = 0;
      virtual const std::string & Comment() const = 0;
      virtual const IPrim * PrimValue() const = 0;
      virtual int Status() const = 0;

      virtual IPar & SetName(const std::string & s) = 0;
      virtual IPar & SetType(const std::string & s) = 0;
      virtual IPar & SetMode(const std::string & s) = 0;
      virtual IPar & SetValue(const std::string & s) = 0;
      virtual IPar & SetMin(const std::string & s) = 0;
      virtual IPar & SetMax(const std::string & s) = 0;
      virtual IPar & SetPrompt(const std::string & s) = 0;
      virtual IPar & SetComment(const std::string & s) = 0;
  };

  EXPSYM typedef IBiDirItor<IPar *> IParItor;
  EXPSYM typedef IConstBiDirItor<IPar *> IConstParItor;
  EXPSYM typedef GenBiDirItor<IPar *> GenParItor;
  EXPSYM typedef ConstGenBiDirItor<IPar *> ConstGenParItor;

  class EXPSYM IParGroup {
    public:
      virtual ~IParGroup() {}

      virtual IParGroup & operator =(const IParGroup & g) = 0;

      virtual IPar & operator [](const std::string & pname) const = 0;

      virtual IPar & Find(const std::string & pname) const = 0;
      virtual IParGroup & Clear() = 0;
      virtual IParGroup & Add(IPar * p) = 0;
      virtual IParGroup & Remove(IPar * p) = 0;
      virtual IParGroup & Remove(const std::string & pname) = 0;

      virtual GenParItor begin() = 0;
      virtual ConstGenParItor begin() const = 0;
      virtual GenParItor end() = 0;
      virtual ConstGenParItor end() const = 0;

      virtual IParGroup * Clone() const = 0;
  };

  class EXPSYM IParFile {
    public:
      virtual ~IParFile() {}

      virtual IParFile & operator =(const IParFile & pf) = 0;

      // Synchronize memory image with parameter file and vice versa.
      virtual void Load() = 0;
      virtual void Save() const = 0;

      // Read member access.
      virtual const std::string & Component() const = 0;
      virtual IParGroup & Group() = 0;
      virtual const IParGroup & Group() const = 0;

      // Write member access.
      virtual IParFile & SetComponent(const std::string & comp) = 0;
      virtual IParGroup * SetGroup(IParGroup * group) = 0;

      // Iterative access.
      virtual GenParItor begin() = 0;
      virtual ConstGenParItor begin() const = 0;
      virtual GenParItor end() = 0;
      virtual ConstGenParItor end() const = 0;

      virtual IParFile * Clone() const = 0;
  };

  class EXPSYM IParPrompt {
    public:
      virtual ~IParPrompt() {}

      virtual IParPrompt & operator =(const IParPrompt & p) = 0;

      virtual IParPrompt & Prompt() = 0;
      virtual IParPrompt & Prompt(const std::string & pname) = 0;
      virtual IParPrompt & Prompt(const std::vector<std::string> & pnames) = 0;

      virtual int Argc() const = 0;
      virtual char ** const Argv() const = 0;
      virtual IParGroup & Group() = 0;
      virtual const IParGroup & Group() const = 0;

      virtual IParGroup * SetGroup(IParGroup * group) = 0;

      virtual IParPrompt * Clone() const = 0;
  };

  class EXPSYM IParFactory {
    public:
      virtual ~IParFactory() {}

      virtual IPar * NewIPar() = 0;
      virtual IPar * NewIPar(const IPar & p) = 0;
      virtual IPar * NewIPar(const std::string & name, const std::string & type,
        const std::string & mode, const std::string & value,
        const std::string & min, const std::string & max,
        const std::string & prompt, const std::string & comment) = 0;
  };

  class EXPSYM IIParGroupFactory {
    public:
      virtual ~IIParGroupFactory() {}

      virtual IParGroup * NewIIParGroup() = 0;
      virtual IParGroup * NewIIParGroup(const IParGroup & p) = 0;
  };

  class EXPSYM IParFileFactory {
    public:
      virtual ~IParFileFactory() {}

      virtual IParFile * NewIParFile(const IParFile & p) = 0;
  };

  class EXPSYM IParPromptFactory {
    public:
      virtual ~IParPromptFactory() {}

      virtual IParPrompt * NewIParPrompt(const IParPrompt & p) = 0;
  };

  // Placeholders for ParFile and ParPrompt
  class ParFile : public IParFile {
  };

  class ParPrompt : public IParPrompt {
  };
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Global variable forward declarations.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Function declarations.
  //////////////////////////////////////////////////////////////////////////////
  EXPSYM std::ostream & operator <<(std::ostream & os, const IPar & p);
  //////////////////////////////////////////////////////////////////////////////

}
#endif

/******************************************************************************
 * $Log: hoops.h,v $
 * Revision 1.17  2010/01/06 20:00:10  peachey
 * Add Status() method for getting status of parameter value.
 * This is used to flag e.g. infinite and undefined values of numeric parameters.
 *
 * Revision 1.16  2009/12/23 20:27:18  peachey
 * Add some needed missing header files.
 *
 * Revision 1.15  2004/11/09 18:28:17  peachey
 * Remove SetArgc/SetArgv from IParPrompt interface.
 *
 * Revision 1.14  2004/03/26 22:32:15  peachey
 * Add new error code for unsupported features.
 *
 * Revision 1.13  2004/03/16 14:36:58  peachey
 * Remove default construction option for ParFile and ParPrompt.
 *
 * Revision 1.12  2004/03/15 13:58:31  peachey
 * Add Clone method to IParFile and IParPrompt and subclasses.
 * Have PILParFile::Group() create a group if it doesn't have
 * one rather than throw an exception.
 *
 * Revision 1.11  2004/03/11 17:35:11  peachey
 * Remove an unneccessary error condition.
 *
 * Revision 1.10  2004/03/10 19:35:19  peachey
 * Remove throw specifications.
 *
 * Revision 1.9  2003/11/26 18:50:03  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.8  2003/11/13 19:29:29  peachey
 * Add preprocessor macro needed on Windows to export symbols.
 *
 * Revision 1.7  2003/11/10 18:19:45  peachey
 * Moved header files into hoops subdirectory.
 *
 * Revision 1.6  2003/06/18 18:10:03  peachey
 * Remove method to return char * because IPrim no longer supports
 * char * as a specializable type.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:30  jchiang
 * First import
 *
 * Revision 1.5  2003/06/06 20:44:23  peachey
 * From IParFile, remove Open() and Close(), which are actually
 * redundant with Load() and Save(). Add assignment operator
 * to IParGroup. Change IParPrompt::SetGroup so that it returns
 * the current group. Add similar SetGroup method to IParFile.
 *
 * Revision 1.4  2003/06/06 13:25:13  peachey
 * Restructure the header files. The files hoops_exception.h, hoops.h,
 * hoops_itor.h, hoops_pil_factory.h, and hoops_prim.h are now the
 * only public (installed) files.
 *
 * Revision 1.3  2003/04/24 14:50:00  peachey
 * Add placeholders for ParFile and ParPrompt.
 *
 * Revision 1.2  2003/04/23 17:42:56  peachey
 * Add virtual base class IParGroup. This requires also using new iterator
 * classes.
 *
 * Revision 1.1  2003/04/11 19:20:38  peachey
 * New component HOOPS, an object oriented parameter interface. Low
 * level access currently uses PIL, but this can be changed.
 *
 ******************************************************************************/
