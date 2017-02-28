/******************************************************************************
 *   File name: hoops_par.cxx                                                 *
 *                                                                            *
 * Description: Implementation of standard parameter type.                    *
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
#include <cstring>
#include <iostream>
#include "hoops/hoops_par.h"
////////////////////////////////////////////////////////////////////////////////
namespace hoops {

  //////////////////////////////////////////////////////////////////////////////
  // Constants.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Static function declarations.
  //////////////////////////////////////////////////////////////////////////////
#ifdef WIN32
  static int strcasecmp(const char *s1, const char *s2);
#endif
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type definitions.
  //////////////////////////////////////////////////////////////////////////////
  Par::Par(): IPar(), mName(), mType(), mMode(), mValue(0), mMin(), mMax(),
    mPrompt(), mComment(), mValString(), mStatus(P_OK) {}

  Par::Par(const Par & p): IPar(), mName(p.mName),
    mType(p.mType), mMode(p.mMode), mValue(0), mMin(), mMax(),
    mPrompt(p.mPrompt), mComment(p.mComment), mValString(p.mValString),
    mStatus(p.mStatus) {
    if (p.mValue) mValue = p.mValue->Clone();
  }

  Par::Par(const IPar & p): IPar(), mName(p.Name()),
    mType(p.Type()), mMode(p.Mode()), mValue(0), mMin(), mMax(),
    mPrompt(p.Prompt()), mComment(p.Comment()), mValString(p.Value()),
    mStatus(p.Status()) {
    if (!p.Value().empty()) From(p.Value());
  }

  Par::Par(const std::string & name, const std::string & type,
    const std::string & mode, const std::string & value,
    const std::string & min, const std::string & max,
    const std::string & prompt, const std::string & comment):
    IPar(), mName(name), mType(type), mMode(mode),
    mValue(0), mMin(min), mMax(max), mPrompt(prompt),
    mComment(comment), mValString(), mStatus(P_OK) {
    if (!value.empty()) From(value);
  }
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Destructor.
  //////////////////////////////////////////////////////////////////////////////
  Par::~Par() {
    delete mValue;
  }
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Assignments.
  //////////////////////////////////////////////////////////////////////////////
  void Par::From(const IPar & p) {
    if (!p.Value().empty()) From(p.Value());
    else if (!p.Type().empty() && !Type().empty()) {
      // Allow conversion from a "null" parameter if dest and source
      // are well defined parameter type.
      delete mValue;
      mValue = 0;
      mValString.clear();
    } else {
      // At least one parameter is of undefined type. This is illegal.
      throw Hexception(PAR_ILLEGAL_CONVERSION, "", __FILE__, __LINE__);
    }
  }

  void Par::From(const IPrim & p)
    { ConvertFrom<const IPrim &>(p, mValue, mType); }

  void Par::From(const bool & p)
    { ConvertFrom<const bool &>(p, mValue, mType); }

  void Par::From(const char & p)
    { ConvertFrom<const char &>(p, mValue, mType); }

  void Par::From(const signed char & p)
    { ConvertFrom<const signed char &>(p, mValue, mType); }

  void Par::From(const short & p)
    { ConvertFrom<const short &>(p, mValue, mType); }

  void Par::From(const int & p)
    { ConvertFrom<const int &>(p, mValue, mType); }

  void Par::From(const long & p)
    { ConvertFrom<const long &>(p, mValue, mType); }

  void Par::From(const unsigned char & p)
    { ConvertFrom<const unsigned char &>(p, mValue, mType); }

  void Par::From(const unsigned short & p)
    { ConvertFrom<const unsigned short &>(p, mValue, mType); }

  void Par::From(const unsigned int & p)
    { ConvertFrom<const unsigned int &>(p, mValue, mType); }

  void Par::From(const unsigned long & p)
    { ConvertFrom<const unsigned long &>(p, mValue, mType); }

  void Par::From(const float & p)
    { ConvertFrom<const float &>(p, mValue, mType); }

  void Par::From(const double & p)
    { ConvertFrom<const double &>(p, mValue, mType); }

  void Par::From(const long double & p)
    { ConvertFrom<const long double &>(p, mValue, mType); }

  void Par::From(const char * p)
    { From(std::string(p)); }

  void Par::From(const std::string & p) {
    try {
      ConvertFrom<const std::string &>(p, mValue, mType);
      mStatus = P_OK;
    } catch (const Hexception & x) {
      int status = x.Code();
      if (P_INFINITE == status || P_UNDEFINED == status) mStatus = status;
      else throw;
    }
  }
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Conversions.
  //////////////////////////////////////////////////////////////////////////////
  Par::operator bool () const
    { bool r; ConvertTo<bool>(mValue, r); return r; }
  Par::operator char () const
    { char r; ConvertTo<char>(mValue, r); return r; }
  Par::operator signed char () const
    { signed char r; ConvertTo<signed char>(mValue, r); return r; }
  Par::operator short () const
    { short r; ConvertTo<short>(mValue, r); return r; }
  Par::operator int () const
    { int r; ConvertTo<int>(mValue, r); return r; }
  Par::operator long () const
    { long r; ConvertTo<long>(mValue, r); return r; }
  Par::operator unsigned char () const
    { unsigned char r; ConvertTo<unsigned char>(mValue, r); return r; }
  Par::operator unsigned short () const
    { unsigned short r; ConvertTo<unsigned short>(mValue, r); return r; }
  Par::operator unsigned int () const
    { unsigned int r; ConvertTo<unsigned int>(mValue, r); return r; }
  Par::operator unsigned long () const
    { unsigned long r; ConvertTo<unsigned long>(mValue, r); return r; }
  Par::operator float () const
    { float r; ConvertTo<float>(mValue, r); return r; }
  Par::operator double () const
    { double r; ConvertTo<double>(mValue, r); return r; }
  Par::operator long double () const
    { long double r; ConvertTo<long double>(mValue, r); return r; }

  // Difference between this and Value() is that the latter handles exceptions.
  Par::operator const char *() const {
    ConvertTo<std::string>(mValue, mValString);
    return mValString.c_str();
  }

  Par::operator const std::string &() const {
    ConvertTo<std::string>(mValue, mValString);
    return mValString;
  }

  void Par::To(bool & p) const
    { ConvertTo<bool>(mValue, p); }

  void Par::To(char & p) const
    { ConvertTo<char>(mValue, p); }

  void Par::To(signed char & p) const
    { ConvertTo<signed char>(mValue, p); }

  void Par::To(short & p) const
    { ConvertTo<short>(mValue, p); }

  void Par::To(int & p) const
    { ConvertTo<int>(mValue, p); }

  void Par::To(long & p) const
    { ConvertTo<long>(mValue, p); }

  void Par::To(unsigned char & p) const
    { ConvertTo<unsigned char>(mValue, p); }

  void Par::To(unsigned short & p) const
    { ConvertTo<unsigned short>(mValue, p); }

  void Par::To(unsigned int & p) const
    { ConvertTo<unsigned int>(mValue, p); }

  void Par::To(unsigned long & p) const
    { ConvertTo<unsigned long>(mValue, p); }

  void Par::To(float & p) const
    { ConvertTo<float>(mValue, p); }

  void Par::To(double & p) const
    { ConvertTo<double>(mValue, p); }

  void Par::To(long double & p) const
    { ConvertTo<long double>(mValue, p); }

  void Par::To(std::string & p) const {
    if (P_INFINITE == mStatus) p = mValString;
    else if (P_UNDEFINED == mStatus) p = mValString;
    // Call ConvertTo even if p was already assigned so that the proper
    // exception is thrown.
    ConvertTo<std::string>(mValue, p);
  }
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Member access.
  //////////////////////////////////////////////////////////////////////////////
  const std::string & Par::Value() const {
    try { To(mValString); }
    catch (const Hexception &) {}
    return mValString;
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
#ifdef WIN32
// Windows has no strcasecmp function.
static int strcasecmp(const char *s1, const char *s2) {
  const char *p1 = s1;
  const char *p2 = s2;
  int diff = 0;
  while (!diff) {
    diff = toupper(*p1) - toupper(*p2);
    if (*p1 && *p2) { ++p1; ++p2; }
    else { diff = *p1 - *p2; break; }
  }
  return diff;
}
#endif

  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Function definitions.
  //////////////////////////////////////////////////////////////////////////////
  std::ostream & operator <<(std::ostream & os, const IPar & p) {
    if (!p.Name().empty()) {
      os << p.Name() << "," << p.Type() << "," << p.Mode() << ",";
      if (!p.Type().compare("f") || !p.Type().compare("s")) {
        os << '"' << p.Value() << '"' << ",";
        if (!p.Min().empty()) os << '"' << p.Min() << '"';
        os << ",";
        if (!p.Max().empty()) os << '"' << p.Max() << '"';
      } else if (!p.Type().compare("b")) {
        const char * value = p.Value().c_str();
        if (!strcasecmp(value, "true"))
          os << "\"yes\"," << p.Min() << "," << p.Max();
        else if (!strcasecmp(value, "false"))
          os << "\"no\"," << p.Min() << "," << p.Max();
        else
          os << p.Value() << "," << p.Min() << "," << p.Max();
      } else {
        os << p.Value() << "," << p.Min() << "," << p.Max();
      }
      os << "," << '"' << p.Prompt() << '"';
    }
    if (!p.Comment().empty()) os << p.Comment();
    return os;
  }

  //////////////////////////////////////////////////////////////////////////////

}

/******************************************************************************
 * $Log: hoops_par.cxx,v $
 * Revision 1.13  2010/01/06 20:03:18  peachey
 * 1. Add Status() method for flagging special values such as infinite and
 * undefined numeric parameters.
 * 2. Copy the status when copy constructing parameters.
 * 3. Whenever assigning to a parameter, make exact duplicate of value
 * in mValString. This is returned whenever INDEF or INF etc. are used
 * to indicate special values.
 *
 * Revision 1.12  2008/07/29 15:49:30  peachey
 * Add a status member variable and use it to track
 * whether the parameter was last assigned from an undefined or
 * infinite value.
 *
 * Revision 1.11  2004/03/16 20:50:57  peachey
 * Explicitly invoke constructors for base classes to shut up compiler
 * warnings in the SLAC build.
 *
 * Revision 1.10  2004/03/12 15:40:42  peachey
 * When throwing exceptions, include the file name and
 * line number where the exception was thrown.
 *
 * Revision 1.9  2004/03/10 19:35:29  peachey
 * Remove throw specifications.
 *
 * Revision 1.8  2003/11/26 18:50:02  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.7  2003/11/26 17:54:09  peachey
 * Explicitly zero-initialize all pointers, as VS does not adhere to
 * the ISO standard for default-initialized pointers.
 *
 * Revision 1.6  2003/11/13 20:53:21  peachey
 * Remove dummy exception variable to silence warning on Windows.
 *
 * Revision 1.5  2003/11/10 18:16:12  peachey
 * Moved header files into hoops subdirectory.
 *
 * Revision 1.4  2003/11/07 18:12:05  peachey
 * Do not use fully qualified names for static functions. This confuses
 * Visual Studio 7
 *
 * Revision 1.3  2003/06/18 18:17:42  peachey
 * Remove method to return char * because IPrim no longer supports
 * char * as a specializable type.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:29  jchiang
 * First import
 *
 * Revision 1.2  2003/05/14 15:28:00  peachey
 * Add strcasecmp for Windows.
 *
 * Revision 1.1  2003/04/11 19:20:38  peachey
 * New component HOOPS, an object oriented parameter interface. Low
 * level access currently uses PIL, but this can be changed.
 *
 ******************************************************************************/
