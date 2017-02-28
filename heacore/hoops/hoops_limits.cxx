/******************************************************************************
 *   File name:                                                               *
 *                                                                            *
 * Description:                                                               *
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
#include "hoops/hoops_limits.h"
#include "hoops/hoops_numeric_limits.h"
////////////////////////////////////////////////////////////////////////////////

namespace hoops {

#ifdef HAVE_LIMITS
  using std::numeric_limits;
#endif

  //////////////////////////////////////////////////////////////////////////////
  // Constants.
  //////////////////////////////////////////////////////////////////////////////
  // Lim<T>::code static initializations.
  //////////////////////////////////////////////////////////////////////////////
  template <>
  PrimTypeCode_e Lim<bool>::code = P_BOOL;
  template <>
  PrimTypeCode_e Lim<char>::code = P_CHAR;
  template <>
  PrimTypeCode_e Lim<signed char>::code = P_SCHAR;
  template <>
  PrimTypeCode_e Lim<short>::code = P_SHORT;
  template <>
  PrimTypeCode_e Lim<int>::code = P_INT;
  template <>
  PrimTypeCode_e Lim<long>::code = P_LONG;
  template <>
  PrimTypeCode_e Lim<unsigned char>::code = P_UCHAR;
  template <>
  PrimTypeCode_e Lim<unsigned short>::code = P_USHORT;
  template <>
  PrimTypeCode_e Lim<unsigned int>::code = P_UINT;
  template <>
  PrimTypeCode_e Lim<unsigned long>::code = P_ULONG;
  template <>
  PrimTypeCode_e Lim<float>::code = P_FLOAT;
  template <>
  PrimTypeCode_e Lim<double>::code = P_DOUBLE;
  template <>
  PrimTypeCode_e Lim<long double>::code = P_LONGDOUBLE;
  //////////////////////////////////////////////////////////////////////////////

#ifdef OLD_LIMITS_IMPLEMENTATION
  // Lim<T>::digits10 static initializations.
  //////////////////////////////////////////////////////////////////////////////
  template <>
  const int Lim<bool>::digits10 = numeric_limits<bool>::digits10;
  template <>
  const int Lim<char>::digits10 = numeric_limits<char>::digits10;
  template <>
  const int Lim<signed char>::digits10 =
      numeric_limits<signed char>::digits10;
  template <>
  const int Lim<short>::digits10 = numeric_limits<short>::digits10;
  template <>
  const int Lim<int>::digits10 = numeric_limits<int>::digits10;
  template <>
  const int Lim<long>::digits10 = numeric_limits<long>::digits10;
  template <>
  const int Lim<unsigned char>::digits10 =
      numeric_limits<unsigned char>::digits10;
  template <>
  const int Lim<unsigned short>::digits10 =
      numeric_limits<unsigned short>::digits10;
  template <>
  const int Lim<unsigned int>::digits10 =
      numeric_limits<unsigned int>::digits10;
  template <>
  const int Lim<unsigned long>::digits10 =
      numeric_limits<unsigned long>::digits10;
  template <>
  const int Lim<float>::digits10 = numeric_limits<float>::digits10;
  template <>
  const int Lim<double>::digits10 = numeric_limits<double>::digits10;
  // On *expletive* Solaris, numeric_limits<long double> causes the 
  // compiler to exit with assertion failures.
  template <>
  const int Lim<long double>::digits10 =
      numeric_limits<double>::digits10;
  //////////////////////////////////////////////////////////////////////////////

  // Lim<T>::epsilon static initializations.
  //////////////////////////////////////////////////////////////////////////////
  template <>
  const bool Lim<bool>::epsilon = numeric_limits<bool>::epsilon();
  template <>
  const char Lim<char>::epsilon = numeric_limits<char>::epsilon();
  template <>
  const signed char Lim<signed char>::epsilon =
      numeric_limits<signed char>::epsilon();
  template <>
  const short Lim<short>::epsilon = numeric_limits<short>::epsilon();
  template <>
  const int Lim<int>::epsilon = numeric_limits<int>::epsilon();
  template <>
  const long Lim<long>::epsilon = numeric_limits<long>::epsilon();
  template <>
  const unsigned char Lim<unsigned char>::epsilon =
      numeric_limits<unsigned char>::epsilon();
  template <>
  const unsigned short Lim<unsigned short>::epsilon =
      numeric_limits<unsigned short>::epsilon();
  template <>
  const unsigned int Lim<unsigned int>::epsilon =
      numeric_limits<unsigned int>::epsilon();
  template <>
  const unsigned long Lim<unsigned long>::epsilon =
      numeric_limits<unsigned long>::epsilon();
  template <>
  const float Lim<float>::epsilon = numeric_limits<float>::epsilon();
  template <>
  const double Lim<double>::epsilon = numeric_limits<double>::epsilon();
  // On *expletive* Solaris, numeric_limits<long double> causes the 
  // compiler to exit with assertion failures.
  template <>
  const long double Lim<long double>::epsilon =
      numeric_limits<double>::epsilon();
  //////////////////////////////////////////////////////////////////////////////

  // Lim<T>::is_integer static initializations.
  //////////////////////////////////////////////////////////////////////////////
  template <>
  const bool Lim<bool>::is_integer = numeric_limits<bool>::is_integer;
  template <>
  const bool Lim<char>::is_integer = numeric_limits<char>::is_integer;
  template <>
  const bool Lim<signed char>::is_integer =
      numeric_limits<signed char>::is_integer;
  template <>
  const bool Lim<short>::is_integer = numeric_limits<short>::is_integer;
  template <>
  const bool Lim<int>::is_integer = numeric_limits<int>::is_integer;
  template <>
  const bool Lim<long>::is_integer = numeric_limits<long>::is_integer;
  template <>
  const bool Lim<unsigned char>::is_integer =
      numeric_limits<unsigned char>::is_integer;
  template <>
  const bool Lim<unsigned short>::is_integer =
      numeric_limits<unsigned short>::is_integer;
  template <>
  const bool Lim<unsigned int>::is_integer =
      numeric_limits<unsigned int>::is_integer;
  template <>
  const bool Lim<unsigned long>::is_integer =
      numeric_limits<unsigned long>::is_integer;
  template <>
  const bool Lim<float>::is_integer = numeric_limits<float>::is_integer;
  template <>
  const bool Lim<double>::is_integer = numeric_limits<double>::is_integer;
  // On *expletive* Solaris, numeric_limits<long double> causes the 
  // compiler to exit with assertion failures.
  template <>
  const bool Lim<long double>::is_integer =
      numeric_limits<double>::is_integer;
  //////////////////////////////////////////////////////////////////////////////

  // Lim<T>::is_signed static initializations.
  //////////////////////////////////////////////////////////////////////////////
  template <>
  const bool Lim<bool>::is_signed = numeric_limits<bool>::is_signed;
  template <>
  const bool Lim<char>::is_signed = numeric_limits<char>::is_signed;
  template <>
  const bool Lim<signed char>::is_signed =
      numeric_limits<signed char>::is_signed;
  template <>
  const bool Lim<short>::is_signed = numeric_limits<short>::is_signed;
  template <>
  const bool Lim<int>::is_signed = numeric_limits<int>::is_signed;
  template <>
  const bool Lim<long>::is_signed = numeric_limits<long>::is_signed;
  template <>
  const bool Lim<unsigned char>::is_signed =
      numeric_limits<unsigned char>::is_signed;
  template <>
  const bool Lim<unsigned short>::is_signed =
      numeric_limits<unsigned short>::is_signed;
  template <>
  const bool Lim<unsigned int>::is_signed =
      numeric_limits<unsigned int>::is_signed;
  template <>
  const bool Lim<unsigned long>::is_signed =
      numeric_limits<unsigned long>::is_signed;
  template <>
  const bool Lim<float>::is_signed = numeric_limits<float>::is_signed;
  template <>
  const bool Lim<double>::is_signed = numeric_limits<double>::is_signed;
  // On *expletive* Solaris, numeric_limits<long double> causes the 
  // compiler to exit with assertion failures.
  template <>
  const bool Lim<long double>::is_signed =
      numeric_limits<double>::is_signed;
  //////////////////////////////////////////////////////////////////////////////

  // Lim<T>::round_error static initializations.
  //////////////////////////////////////////////////////////////////////////////
  template <>
  const bool Lim<bool>::round_error = numeric_limits<bool>::round_error();
  template <>
  const char Lim<char>::round_error = numeric_limits<char>::round_error();
  template <>
  const signed char Lim<signed char>::round_error =
      numeric_limits<signed char>::round_error();
  template <>
  const short Lim<short>::round_error = numeric_limits<short>::round_error();
  template <>
  const int Lim<int>::round_error = numeric_limits<int>::round_error();
  template <>
  const long Lim<long>::round_error = numeric_limits<long>::round_error();
  template <>
  const unsigned char Lim<unsigned char>::round_error =
      numeric_limits<unsigned char>::round_error();
  template <>
  const unsigned short Lim<unsigned short>::round_error =
      numeric_limits<unsigned short>::round_error();
  template <>
  const unsigned int Lim<unsigned int>::round_error =
      numeric_limits<unsigned int>::round_error();
  template <>
  const unsigned long Lim<unsigned long>::round_error =
      numeric_limits<unsigned long>::round_error();
  template <>
  const float Lim<float>::round_error = numeric_limits<float>::round_error();
  template <>
  const double Lim<double>::round_error = numeric_limits<double>::round_error();
  // On *expletive* Solaris, numeric_limits<long double> causes the 
  // compiler to exit with assertion failures.
  template <>
  const long double Lim<long double>::round_error =
      numeric_limits<double>::round_error();
  //////////////////////////////////////////////////////////////////////////////

  // Lim<T>::max static initializations.
  //////////////////////////////////////////////////////////////////////////////
  template <>
  const bool Lim<bool>::max = numeric_limits<bool>::max();
  template <>
  const char Lim<char>::max = numeric_limits<char>::max();
  template <>
  const signed char Lim<signed char>::max =
      numeric_limits<signed char>::max();
  template <>
  const short Lim<short>::max = numeric_limits<short>::max();
  template <>
  const int Lim<int>::max = numeric_limits<int>::max();
  template <>
  const long Lim<long>::max = numeric_limits<long>::max();
  template <>
  const unsigned char Lim<unsigned char>::max =
      numeric_limits<unsigned char>::max();
  template <>
  const unsigned short Lim<unsigned short>::max =
      numeric_limits<unsigned short>::max();
  template <>
  const unsigned int Lim<unsigned int>::max =
      numeric_limits<unsigned int>::max();
  template <>
  const unsigned long Lim<unsigned long>::max =
      numeric_limits<unsigned long>::max();
  template <>
  const float Lim<float>::max = numeric_limits<float>::max();
  template <>
  const double Lim<double>::max = numeric_limits<double>::max();
  // On *expletive* Solaris, numeric_limits<long double> causes the 
  // compiler to exit with assertion failures.
  template <>
  const long double Lim<long double>::max =
      numeric_limits<double>::max();
  //////////////////////////////////////////////////////////////////////////////

  // Lim<T>::min static initializations.
  //////////////////////////////////////////////////////////////////////////////
  template <>
  const bool Lim<bool>::min = numeric_limits<bool>::min();
  template <>
  const char Lim<char>::min = numeric_limits<char>::min();
  template <>
  const signed char Lim<signed char>::min =
      numeric_limits<signed char>::min();
  template <>
  const short Lim<short>::min = numeric_limits<short>::min();
  template <>
  const int Lim<int>::min = numeric_limits<int>::min();
  template <>
  const long Lim<long>::min = numeric_limits<long>::min();
  template <>
  const unsigned char Lim<unsigned char>::min =
      numeric_limits<unsigned char>::min();
  template <>
  const unsigned short Lim<unsigned short>::min =
      numeric_limits<unsigned short>::min();
  template <>
  const unsigned int Lim<unsigned int>::min =
      numeric_limits<unsigned int>::min();
  template <>
  const unsigned long Lim<unsigned long>::min =
      numeric_limits<unsigned long>::min();
  // Note: for all floating points, min is -max, _not_ min in the
  // sense of numeric_limits::min().
  template <>
  const float Lim<float>::min = -numeric_limits<float>::max();
  template <>
  const double Lim<double>::min = -numeric_limits<double>::max();
  // On *expletive* Solaris, numeric_limits<long double> causes the 
  // compiler to exit with assertion failures.
  template <>
  const long double Lim<long double>::min =
      -numeric_limits<double>::max();
  //////////////////////////////////////////////////////////////////////////////

  // Support of all the above for long long.
  //////////////////////////////////////////////////////////////////////////////
#ifdef HAVE_LONGLONG
  // Utility to determine maximum value for type long long.
  // On Solaris, numeric_limits does not contain meaningful
  // specializations for long long, so the maximum value is
  // computed.
  static long long LongLongMax() {
    long long max = numeric_limits<long long>::max();
    if (0 == max) {
      long long bitmax = 1;

      // Find highest positive power of 2.
      while (0 < bitmax << 1) bitmax <<= 1;

      // Fill lower bits, always testing for overflow.
      while (0 < bitmax) {
        if (0 < max | bitmax) max |= bitmax;
        bitmax >>= 1;
      }
    }
    return max;
  }

  // Utility to determine minimum value for type long long.
  // On Solaris, numeric_limits does not contain meaningful
  // specializations for long long, so the minimum value is
  // computed using LongLongMax, and then taking the 2s complement.
  static long long LongLongMin() {
    static long long min = 0;
    if (0 == min) {
      min = numeric_limits<long long>::min();
      if (0 == min) {
        // Assume 2s complement
        min = LongLongMax() + 1;
      }
    }
    return min;
  }
// the following #endif is for HAVE_LONGLONG
#endif
// the following #endif is for OLD_LIMITS_IMPLEMENTATION
#endif

#ifdef HAVE_LONGLONG
  // Specializations of all static members of Lim for type long long.
  template <>
  PrimTypeCode_e Lim<long long>::code = P_LONGLONG;

#ifdef OLD_LIMITS_IMPLEMENTATION
  template <>
  const long long Lim<long long>::epsilon =
      numeric_limits<long long>::epsilon();

  template <>
  const bool Lim<long long>::is_signed = numeric_limits<long>::is_signed;

  template <>
  const bool Lim<long long>::is_integer = numeric_limits<long>::is_integer;

  template <>
  const long long Lim<long long>::max = LongLongMax();

  template <>
  const long long Lim<long long>::min = LongLongMin();
#endif
#endif
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Static function declarations.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type definitions.
  //////////////////////////////////////////////////////////////////////////////
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
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Function definitions.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
}

/******************************************************************************
 * $Log: hoops_limits.cxx,v $
 * Revision 1.4  2004/06/24 20:26:39  peachey
 * Correct a violation of ISO standard; static const int/enum types
 * must be initialized where they are declared. The Lim class's "code"
 * static member variable was therefore changed to be non-const. To
 * protect its value, it was made private and an accessor, GetCode()
 * was added.
 *
 * In addition, all Lim's other const static members which shadow
 * numeric_limits members were removed, and instead Lim derives
 * directly from std::numeric_limits (or hoops::numeric_limits if
 * std::numeric_limits is not available.)
 *
 * Revision 1.3  2003/12/02 14:39:44  peachey
 * To support compilers which do not have limits, such as g++ 2.95.x,
 * add round_error field to Lim class. This allows the test code to
 * be compiled without the limits header.
 *
 * Revision 1.2  2003/11/10 18:16:12  peachey
 * Moved header files into hoops subdirectory.
 *
 * Revision 1.1  2003/06/18 18:06:36  peachey
 * New source file to hold static initializers for Lim class support.
 *
 * Revision 1.4  2003/05/15 04:02:16  peachey
 * Use standard <limits> header only if HAVE_LIMITS is defined. Otherwise,
 * use hoops_limits.h.
 *
 * Revision 1.3  2003/05/14 15:28:00  peachey
 * Add strcasecmp for Windows.
 *
 * Revision 1.2  2003/05/14 15:17:06  peachey
 * Further encapsulate Prim class by hiding the templates in hoops_prim.cxx
 * and using a factory class.
 *
 * Revision 1.1  2003/04/11 19:20:38  peachey
 * New component HOOPS, an object oriented parameter interface. Low
 * level access currently uses PIL, but this can be changed.
 *
 ******************************************************************************/
