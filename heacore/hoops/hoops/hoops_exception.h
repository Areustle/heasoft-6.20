/******************************************************************************
 *   File name: hoops_exception.h                                             *
 *                                                                            *
 * Description: Exception class for use by hoops.                             *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_EXCEPTION_H
#define HOOPS_EXCEPTION_H

////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include <exception>
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
  // Enumerated codes to describe each type of Hexception.
  enum HexceptionCode_e {
    P_OK = 0,              // No exception, normal execution.
    P_ILLEGAL = 1,         // No rules exist to perform the operation.
    P_OVERFLOW = 2,        // Source value > maximum destination value.
    P_UNDERFLOW = 3,       // Source value < minimum destination value.
    P_BADSIZE = 4,         // Destination may be smaller than the source.
    P_PRECISION = 5,       // Conversion between integral and floating types.
    P_SIGNEDNESS = 6,      // Conversion between signed and unsigned int. types.
    P_STR_OVERFLOW = 7,    // Attempt to convert a string which contained
                           // non-space characters after the number.
    P_STR_INVALID = 8,     // Attempt to convert a string which did not
                           // contain a number.
    P_STR_NULL = 9,        // Attempt to convert from a null string.
    P_INFINITE = 10,       // Converted an infinite string to a numeric value.
    P_UNDEFINED = 11,      // Converted an "undefined" string to a numeric value.
    P_UNEXPECTED = 12,     // (Not thrown) An error occurred which does
                           // not fit into one of the other categories.
    P_CODE_ERROR = 13      // Internal code/logic/runtime error, such as a bad
                           // pointer, dynamic allocation failed, etc.
  };
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type declarations/definitions.
  //////////////////////////////////////////////////////////////////////////////
  class EXPSYM Hexception: public std::exception {
    public:
      Hexception(const int & code, const std::string & msg = std::string(),
                 const std::string & filename = std::string(), int line = 0);
      virtual ~Hexception() throw() {}

      int Code() const { return mCode; }
      const std::string & Msg() const { return mMsg; }
      virtual const char * what() const throw();

    protected:
      Hexception(const int & code, const std::string & filename = std::string(),
                 int line = 0);
      virtual void format(const std::string & msg);
      std::string mMsg;
      std::string mFileName;
      int mCode;
      int mLine;
  };
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type declarations/definitions.
  //////////////////////////////////////////////////////////////////////////////
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
 * $Log: hoops_exception.h,v $
 * Revision 1.9  2009/12/23 20:28:27  peachey
 * Add code P_CODE_ERROR, for converting unknown errors from ape etc. into hoops errors.
 *
 * Revision 1.8  2008/07/29 15:41:58  peachey
 * Add codes to cover infinite and undefined values.
 *
 * Revision 1.7  2004/03/12 15:39:38  peachey
 * Add more flexible formatting, to allow subclasses to
 * change the way the message is constructed. Construct the message during
 * construction of the exception object.
 *
 * Revision 1.6  2004/03/11 17:36:58  peachey
 * Change implementation of Hexception::what() so that it determines
 * a message from the error code.
 *
 * Revision 1.5  2004/03/10 19:34:40  peachey
 * Add what() method.
 *
 * Revision 1.4  2003/11/26 18:50:03  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.3  2003/11/13 20:52:33  peachey
 * Remove unnecessary export of enum.
 *
 * Revision 1.2  2003/11/13 19:29:29  peachey
 * Add preprocessor macro needed on Windows to export symbols.
 *
 * Revision 1.2  2003/11/10 19:24:02  jchiang
 * add mFileName and mLine data members and initialize in constructor to avoid
 * warnings on linux
 *
 * Revision 1.1.1.1  2003/11/04 01:48:30  jchiang
 * First import
 *
 * Revision 1.1  2003/04/11 19:20:38  peachey
 * New component HOOPS, an object oriented parameter interface. Low
 * level access currently uses PIL, but this can be changed.
 *
 ******************************************************************************/
