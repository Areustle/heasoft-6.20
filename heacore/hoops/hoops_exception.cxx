/******************************************************************************
 *   File name: hoops_exception.cxx                                           *
 *                                                                            *
 * Description: Implementation of Hexception and related classes.             *
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
#include <sstream>
#include <string>

#include "hoops/hoops.h"
#include "hoops/hoops_exception.h"
////////////////////////////////////////////////////////////////////////////////
namespace hoops {

  //////////////////////////////////////////////////////////////////////////////
  // Member function definitions.
  //////////////////////////////////////////////////////////////////////////////
  Hexception::Hexception(const int & code, const std::string & msg,
    const std::string & filename, int line): mMsg(), mFileName(filename),
    mCode(code), mLine(line) { format(msg); }

  const char * Hexception::what() const throw() { return mMsg.c_str(); }

  Hexception::Hexception(const int & code, const std::string & filename,
    int line): mMsg(), mFileName(filename), mCode(code), mLine(line) {}

  void Hexception::format(const std::string & msg) {
    if (!msg.empty()) mMsg = msg;
    else {
      mMsg = "hoops: ";
      switch (mCode) {
        case P_ILLEGAL:
          mMsg += "attempt to convert between incompatible types";
          break;
        case P_OVERFLOW:
          mMsg += "mathematical overflow occurred on conversion";
          break;
        case P_UNDERFLOW:
          mMsg += "mathematical underflow occurred on conversion";
          break;
        case P_BADSIZE:
          mMsg += "attempt to convert to a potentially smaller type";
          break;
        case P_PRECISION:
          mMsg += "attempt to convert from integral to floating type or vice versa";
          break;
        case P_SIGNEDNESS:
          mMsg += "attempt to convert from signed to unsigned type or vice versa";
          break;
        case P_STR_OVERFLOW:
          mMsg += "attempt to convert a string which had trailing characters";
          break;
        case P_STR_INVALID:
          mMsg += "attempt to convert a string which did not contain a number";
          break;
        case P_STR_NULL:
          mMsg += "attempt to convert a null string";
          break;
        case P_INFINITE:
          mMsg += "converted a string meaning infinity";
          break;
        case P_UNDEFINED:
          mMsg += "converted a string meaning undefined";
          break;
        case PAR_INVALID_TYPE:
          mMsg += "parameter type field (f, r, b, etc.) is invalid";
          break;
        case PAR_ILLEGAL_CONVERSION:
          mMsg += "attempt to convert between defined and undefined parameter objects";
          break;
        case PAR_NOT_FOUND:
          mMsg += "parameter was not found in group of parameters";
          break;
        case PAR_FILE_CORRUPT:
          mMsg += "one or more parameters in the file are invalid";
          break;
        case PAR_FILE_WRITE_ERROR:
          mMsg += "error writing parameter file";
          break;
        case PAR_NULL_PTR:
          mMsg += "null pointer passed";
          break;
        case PAR_COMP_UNDEF:
          mMsg += "the component name (base name of the parameter file) is undefined";
          break;
        default: 
          mMsg += "unknown error condition";
          break;
      }
    }

    if (!mFileName.empty()) {
      mMsg += " (at ";
      mMsg += mFileName;
      if (0 < mLine) {
        mMsg += ": ";
        std::ostringstream s;
        s << mLine;
        mMsg += s.str();
        mMsg += ")";
      }
    }
  }
  //////////////////////////////////////////////////////////////////////////////

}

/******************************************************************************
 * $Log: hoops_exception.cxx,v $
 * Revision 1.4  2009/12/23 20:26:10  peachey
 * Add some needed missing header files.
 *
 * Revision 1.3  2008/07/29 15:41:59  peachey
 * Add codes to cover infinite and undefined values.
 *
 * Revision 1.2  2004/03/12 15:39:47  peachey
 * Add more flexible formatting, to allow subclasses to
 * change the way the message is constructed. Construct the message during
 * construction of the exception object.
 *
 * Revision 1.1  2004/03/11 17:36:58  peachey
 * Change implementation of Hexception::what() so that it determines
 * a message from the error code.
 *
 ******************************************************************************/
