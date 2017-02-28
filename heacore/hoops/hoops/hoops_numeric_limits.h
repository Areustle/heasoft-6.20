/******************************************************************************
 *   File name: hoops_numeric_limits.h                                        *
 *                                                                            *
 * Description: Replacement for <limits> to be used only for compilers        *
 *     which do not yet have <limits>                                         *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOP_NUMERIC_LIMITS_H
#define HOOP_NUMERIC_LIMITS_H

////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#ifdef HAVE_LIMITS
#include <limits>
#else
#include <limits.h>
#endif
#include <float.h>
////////////////////////////////////////////////////////////////////////////////

#ifndef HAVE_LIMITS
namespace hoops {

  //////////////////////////////////////////////////////////////////////////////
  // Constants.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type declarations/definitions.
  //////////////////////////////////////////////////////////////////////////////
  template<typename T> class numeric_limits;
//  enum float_round_style;
//  enum float_denorm_style;
  template<> class numeric_limits<bool>;
  template<> class numeric_limits<char>;
  template<> class numeric_limits<signed char>;
  template<> class numeric_limits<unsigned char>;
//  template<> class numeric_limits<wchar_t>;
  template<> class numeric_limits<short>;
  template<> class numeric_limits<int>;
  template<> class numeric_limits<long>;
  template<> class numeric_limits<unsigned short>;
  template<> class numeric_limits<unsigned int>;
  template<> class numeric_limits<unsigned long>;
  template<> class numeric_limits<float>;
  template<> class numeric_limits<double>;
//  template<> class numeric_limits<long double>;

  template <typename T>
  class numeric_limits {
    public:
      static const bool is_specialized = false;
      static T min() throw();
      static T max() throw();
//      static const int digits = 0;
      static const int digits10 = 0;
      static const bool is_signed = false;
      static const bool is_integer = false;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw();
      static T round_error() throw();
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw();
//      static T quiet_NaN() throw();
//      static T signaling_NaN() throw();
//      static T denorm_min() throw();
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<bool> {
    public:
      typedef bool T;
      static const bool is_specialized = true;
      static T min() throw() { return false; }
      static T max() throw() { return true; }
//      static const int digits = 0;
      static const int digits10 = 0;
      static const bool is_signed = false;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return false; }
      static T round_error() throw() { return false; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<char> {
    public:
      typedef char T;
      static const bool is_specialized = true;
      static T min() throw() { return CHAR_MIN; }
      static T max() throw() { return CHAR_MAX; }
//      static const int digits = 0;
      static const int digits10 = 2;
      static const bool is_signed = (CHAR_MIN != 0);
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<signed char> {
    public:
      typedef signed char T;
      static const bool is_specialized = true;
      static T min() throw() { return SCHAR_MIN; }
      static T max() throw() { return SCHAR_MAX; }
//      static const int digits = 0;
      static const int digits10 = 2;
      static const bool is_signed = true;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<unsigned char> {
    public:
      typedef unsigned char T;
      static const bool is_specialized = true;
      static T min() throw() { return 0; }
      static T max() throw() { return UCHAR_MAX; }
//      static const int digits = 0;
      static const int digits10 = 2;
      static const bool is_signed = false;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<short> {
    public:
      typedef short T;
      static const bool is_specialized = true;
      static T min() throw() { return SHRT_MIN; }
      static T max() throw() { return SHRT_MAX; }
//      static const int digits = 0;
      static const int digits10 = 4;
      static const bool is_signed = true;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<int> {
    public:
      typedef int T;
      static const bool is_specialized = true;
      static T min() throw() { return INT_MIN; }
      static T max() throw() { return INT_MAX; }
//      static const int digits = 0;
      static const int digits10 = 9;
      static const bool is_signed = true;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<long> {
    public:
      typedef long T;
      static const bool is_specialized = true;
      static T min() throw() { return LONG_MIN; }
      static T max() throw() { return LONG_MAX; }
//      static const int digits = 0;
      static const int digits10 = 9;
      static const bool is_signed = true;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<unsigned short> {
    public:
      typedef unsigned short T;
      static const bool is_specialized = true;
      static T min() throw() { return 0; }
      static T max() throw() { return USHRT_MAX; }
//      static const int digits = 0;
      static const int digits10 = 4;
      static const bool is_signed = false;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<unsigned int> {
    public:
      typedef unsigned int T;
      static const bool is_specialized = true;
      static T min() throw() { return 0; }
      static T max() throw() { return UINT_MAX; }
//      static const int digits = 0;
      static const int digits10 = 9;
      static const bool is_signed = false;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<unsigned long> {
    public:
      typedef unsigned long T;
      static const bool is_specialized = true;
      static T min() throw() { return 0; }
      static T max() throw() { return ULONG_MAX; }
//      static const int digits = 0;
      static const int digits10 = 9;
      static const bool is_signed = false;
      static const bool is_integer = true;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return 0; }
      static T round_error() throw() { return 0; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<float> {
    public:
      typedef float T;
      static const bool is_specialized = true;
      static T min() throw() { return FLT_MIN; }
      static T max() throw() { return FLT_MAX; }
//      static const int digits = 0;
      static const int digits10 = FLT_DIG;
      static const bool is_signed = true;
      static const bool is_integer = false;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return FLT_EPSILON; }
      static T round_error() throw() { return .5; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<double> {
    public:
      typedef double T;
      static const bool is_specialized = true;
      static T min() throw() { return DBL_MIN; }
      static T max() throw() { return DBL_MAX; }
//      static const int digits = 0;
      static const int digits10 = DBL_DIG;
      static const bool is_signed = true;
      static const bool is_integer = false;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return DBL_EPSILON; }
      static T round_error() throw() { return .5; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
  };

  template <>
  class numeric_limits<long double> {
    public:
      typedef long double T;
      static const bool is_specialized = true;
      static T min() throw() { return LDBL_MIN; }
      static T max() throw() { return LDBL_MAX; }
//      static const int digits = 0;
      static const int digits10 = LDBL_DIG;
      static const bool is_signed = true;
      static const bool is_integer = false;
//      static const bool is_exact = false;
//      static const int radix = 0;
      static T epsilon() throw() { return LDBL_EPSILON; }
      static T round_error() throw() { return .5; }
//      static const int min_exponent = 0;
//      static const int min_exponent10 = 0;
//      static const int max_exponent = 0;
//      static const int max_exponent10 = 0;
//      static const bool has_infinity = false;
//      static const bool has_quiet_NaN = false;
//      static const bool has_signaling_NaN = false;
//      static const float_denorm_style has_denorm = denorm_absent;
//      static const bool has_denorm_loss = false;
//      static T infinity() throw() { return false; }
//      static T quiet_NaN() throw() { return false; }
//      static T signaling_NaN() throw() { return false; }
//      static T denorm_min() throw() { return false; }
//      static const bool is_iec559 = false;
//      static const bool is_bounded = false;
//      static const bool is_modulo = false;
//      static const bool traps = false;
//      static const bool tinyness_before = false;
//      static const float_round_style round_style = round_toward_zero;
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
#endif

/******************************************************************************
 * $Log: hoops_numeric_limits.h,v $
 * Revision 1.2  2005/09/13 20:51:36  peachey
 * Use .5 for all rounding errors.
 *
 * Revision 1.1  2003/06/18 18:07:29  peachey
 * Replacements for numeric_limits class for compilers which don't
 * yet have it.
 *
 ******************************************************************************/
