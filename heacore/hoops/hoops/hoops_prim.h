/******************************************************************************
 *   File name: hoops_prim.h                                                  *
 *                                                                            *
 * Description: Type-safe primitive wrapper class IPrim, plus factory         *
 *     classes IPrimFactory and PrimFactory (concrete).                       *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_PRIM_H
#define HOOPS_PRIM_H

////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include <cctype>
#include <iosfwd>
#include <string>
////////////////////////////////////////////////////////////////////////////////

#ifndef EXPSYM
#ifdef WIN32
#define EXPSYM __declspec(dllexport)
#else
#define EXPSYM
#endif
#endif

namespace hoops {
  class EXPSYM IPrim {
    public:
      virtual ~IPrim() {}

      virtual void From(const IPrim & x) = 0;
      virtual void From(const bool & x) = 0;
      virtual void From(const char & x) = 0;
      virtual void From(const signed char & x) = 0;
      virtual void From(const signed short & x) = 0;
      virtual void From(const signed int & x) = 0;
      virtual void From(const signed long & x) = 0;
      virtual void From(const unsigned char & x) = 0;
      virtual void From(const unsigned short & x) = 0;
      virtual void From(const unsigned int & x) = 0;
      virtual void From(const unsigned long & x) = 0;
      virtual void From(const float & x) = 0;
      virtual void From(const double & x) = 0;
      virtual void From(const long double & x) = 0;
      virtual void From(const std::string & x) = 0;

      virtual void To(IPrim & x) const = 0;
      virtual void To(bool & x) const = 0;
      virtual void To(char & x) const = 0;
      virtual void To(signed char & x) const = 0;
      virtual void To(signed short & x) const = 0;
      virtual void To(signed int & x) const = 0;
      virtual void To(signed long & x) const = 0;
      virtual void To(unsigned char & x) const = 0;
      virtual void To(unsigned short & x) const = 0;
      virtual void To(unsigned int & x) const = 0;
      virtual void To(unsigned long & x) const = 0;
      virtual void To(float & x) const = 0;
      virtual void To(double & x) const = 0;
      virtual void To(long double & x) const = 0;
      virtual void To(std::string & x) const = 0;

      virtual std::string StringData() const = 0;

      virtual IPrim * Clone() const = 0;

      // Utility IsBlank: determine whether a given string has
      // any non-blank characters.
      static bool IsBlank(const char *s) {
        if (s) while (*s) if(!isspace(*s++)) return false;
        return true;
      }
  };

  class EXPSYM IPrimFactory {
    public:
      virtual ~IPrimFactory() {}

      virtual IPrim * NewIPrim(const bool & p) const = 0;
      virtual IPrim * NewIPrim(const char & p) const = 0;
      virtual IPrim * NewIPrim(const signed char & p) const = 0;
      virtual IPrim * NewIPrim(const signed short & p) const = 0;
      virtual IPrim * NewIPrim(const signed int & p) const = 0;
      virtual IPrim * NewIPrim(const signed long & p) const = 0;
      virtual IPrim * NewIPrim(const unsigned char & p) const = 0;
      virtual IPrim * NewIPrim(const unsigned short & p) const = 0;
      virtual IPrim * NewIPrim(const unsigned int & p) const = 0;
      virtual IPrim * NewIPrim(const unsigned long & p) const = 0;
      virtual IPrim * NewIPrim(const float & p) const = 0;
      virtual IPrim * NewIPrim(const double & p) const = 0;
      virtual IPrim * NewIPrim(const long double & p) const = 0;
      virtual IPrim * NewIPrim(const std::string & p) const = 0;
  };

  class EXPSYM PrimFactory: public IPrimFactory {
    public:
      virtual ~PrimFactory() {}

      virtual IPrim * NewIPrim(const bool & p) const;
      virtual IPrim * NewIPrim(const char & p) const;
      virtual IPrim * NewIPrim(const signed char & p) const;
      virtual IPrim * NewIPrim(const signed short & p) const;
      virtual IPrim * NewIPrim(const signed int & p) const;
      virtual IPrim * NewIPrim(const signed long & p) const;
      virtual IPrim * NewIPrim(const unsigned char & p) const;
      virtual IPrim * NewIPrim(const unsigned short & p) const;
      virtual IPrim * NewIPrim(const unsigned int & p) const;
      virtual IPrim * NewIPrim(const unsigned long & p) const;
      virtual IPrim * NewIPrim(const float & p) const;
      virtual IPrim * NewIPrim(const double & p) const;
      virtual IPrim * NewIPrim(const long double & p) const;
      virtual IPrim * NewIPrim(const std::string & p) const;
  };

  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Global variable forward declarations.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Function declarations.
  //////////////////////////////////////////////////////////////////////////////

  // Stream output.
  //////////////////////////////////////////////////////////////////////////////
  EXPSYM std::ostream & operator <<(std::ostream & os, const IPrim & p);
  //////////////////////////////////////////////////////////////////////////////
}
#endif

/******************************************************************************
 * $Log: hoops_prim.h,v $
 * Revision 1.7  2004/03/10 19:35:19  peachey
 * Remove throw specifications.
 *
 * Revision 1.6  2003/11/26 18:50:03  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.5  2003/11/13 20:51:24  peachey
 * Add preprocessor macro needed on Windows to export symbols.
 *
 * Revision 1.4  2003/06/18 18:12:27  peachey
 * Cosmetic differences, plus addition of CpyStr function.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:30  jchiang
 * First import
 *
 * Revision 1.3  2003/05/15 04:03:11  peachey
 * Include iosfwd.
 *
 * Revision 1.2  2003/05/14 15:17:07  peachey
 * Further encapsulate Prim class by hiding the templates in hoops_prim.cxx
 * and using a factory class.
 *
 * Revision 1.1  2003/04/11 19:20:38  peachey
 * New component HOOPS, an object oriented parameter interface. Low
 * level access currently uses PIL, but this can be changed.
 *
 ******************************************************************************/
