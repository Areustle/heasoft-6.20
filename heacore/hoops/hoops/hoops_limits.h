/******************************************************************************
 *   File name: hoops_limits.h                                                *
 *                                                                            *
 * Description: Encapsulation of machine-dependent numeric limits,            *
 *     needed because of variations in the implementations of <limits>.       *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOP_LIMITS_H
#define HOOP_LIMITS_H

////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include "hoops/hoops_numeric_limits.h"
////////////////////////////////////////////////////////////////////////////////

namespace hoops {

  //////////////////////////////////////////////////////////////////////////////
  // Constants.
  //////////////////////////////////////////////////////////////////////////////
  // Enumerated codes to describe all primitive types. These are combined
  // using binary logic to facilitate more compact logic during conversions.
  enum PrimTypeCode_e {
    BOOL_TYPE       = 1 << 0,
    CHAR_TYPE       = 1 << 1,
    SHORT_TYPE      = 1 << 2,
    INT_TYPE        = 1 << 3,
    LONG_TYPE       = 1 << 4,
    SIGNED_TYPE     = 1 << 5,
    UNSIGNED_TYPE   = 1 << 6,
    FP_TYPE         = 1 << 7,
    EXTENDED_TYPE   = 1 << 8,
    POINTER_TYPE    = 1 << 9,
    P_BOOL          = BOOL_TYPE | UNSIGNED_TYPE,
    P_CHAR          = CHAR_TYPE,
    P_SCHAR         = CHAR_TYPE | SIGNED_TYPE,
    P_SHORT         = SHORT_TYPE | SIGNED_TYPE,
    P_INT           = INT_TYPE | SIGNED_TYPE,
    P_LONG          = LONG_TYPE | SIGNED_TYPE,
    P_UCHAR         = CHAR_TYPE | UNSIGNED_TYPE,
    P_USHORT        = SHORT_TYPE | UNSIGNED_TYPE,
    P_UINT          = INT_TYPE | UNSIGNED_TYPE,
    P_ULONG         = LONG_TYPE | UNSIGNED_TYPE,
    P_WCHAR         = CHAR_TYPE | EXTENDED_TYPE,
    P_FLOAT         = FP_TYPE | SHORT_TYPE,
    P_DOUBLE        = FP_TYPE | LONG_TYPE,
    P_LONGDOUBLE    = FP_TYPE | EXTENDED_TYPE,
    P_BOOL_P        = BOOL_TYPE | POINTER_TYPE,
    P_CHAR_P        = CHAR_TYPE | POINTER_TYPE,
    P_SCHAR_P       = CHAR_TYPE | SIGNED_TYPE | POINTER_TYPE,
    P_SHORT_P       = SHORT_TYPE | SIGNED_TYPE | POINTER_TYPE,
    P_INT_P         = INT_TYPE | SIGNED_TYPE | POINTER_TYPE,
    P_LONG_P        = LONG_TYPE | SIGNED_TYPE | POINTER_TYPE,
    P_UCHAR_P       = CHAR_TYPE | UNSIGNED_TYPE | POINTER_TYPE,
    P_USHORT_P      = SHORT_TYPE | UNSIGNED_TYPE | POINTER_TYPE,
    P_UINT_P        = INT_TYPE | UNSIGNED_TYPE | POINTER_TYPE,
    P_ULONG_P       = LONG_TYPE | UNSIGNED_TYPE | POINTER_TYPE,
    P_WCHAR_P       = CHAR_TYPE | EXTENDED_TYPE | POINTER_TYPE,
    P_FLOAT_P       = FP_TYPE | SHORT_TYPE | POINTER_TYPE,
    P_DOUBLE_P      = FP_TYPE | LONG_TYPE | POINTER_TYPE,
    P_LONGDOUBLE_P  = FP_TYPE | EXTENDED_TYPE | POINTER_TYPE,
    P_VOID_P        = POINTER_TYPE,
    P_UNKNOWN       = 0,
    P_LONGLONG      = SIGNED_TYPE | EXTENDED_TYPE,
    P_LONGLONG_P    = SIGNED_TYPE | EXTENDED_TYPE | POINTER_TYPE
  };
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type declarations/definitions.
  //////////////////////////////////////////////////////////////////////////////
  // Template class Lim, which contains the necessary information to
  // assess compatibility of different numeric types. Note that T
  // must be a fundamental primitive type: bool, char, etc.
  //////////////////////////////////////////////////////////////////////////////
  template <typename T>
#ifdef HAVE_LIMITS
  class Lim : public std::numeric_limits<T> {
#else
  class Lim : public hoops::numeric_limits<T> {
#endif
    public:
      inline static bool is_smaller_than(PrimTypeCode_e typecode) throw () {
        switch (typecode) {
          case P_BOOL: return sizeof(T) < sizeof(bool); break;
          case P_CHAR: return sizeof(T) < sizeof(char); break;
          case P_SCHAR: return sizeof(T) < sizeof(signed char); break;
          case P_SHORT: return sizeof(T) < sizeof(short); break;
          case P_INT: return sizeof(T) < sizeof(int); break;
          case P_LONG: return sizeof(T) < sizeof(long); break;
          case P_UCHAR: return sizeof(T) < sizeof(unsigned char); break;
          case P_USHORT: return sizeof(T) < sizeof(unsigned short); break;
          case P_UINT: return sizeof(T) < sizeof(unsigned int); break;
          case P_ULONG: return sizeof(T) < sizeof(unsigned long); break;
          case P_FLOAT: return sizeof(T) < sizeof(float); break;
          case P_DOUBLE: return sizeof(T) < sizeof(double); break;
          case P_LONGDOUBLE: return sizeof(T) < sizeof(long double); break;
          default: break;
        }
        return false;
      }

      // Note that maybe_smaller_than is defined for all types,
      // but it is only valid when applied to integral types.
      static bool maybe_smaller_than(PrimTypeCode_e typecode) throw ()
        { return false; }

      static PrimTypeCode_e GetCode() { return code; }

    private:
      static PrimTypeCode_e code;
  };

  // Template class Lim specializations.
  //////////////////////////////////////////////////////////////////////////////

  // Type bool is considered smaller than all other types, regardless
  // of how the actual sizes compare.
  template <>
  inline bool Lim<bool>::is_smaller_than(PrimTypeCode_e typecode) throw ()
    { return P_BOOL != typecode; }

  // Type bool is considered smaller than all other types, regardless
  // of how the actual sizes compare.
  template <>
  inline bool Lim<bool>::maybe_smaller_than(PrimTypeCode_e typecode) throw ()
    { return P_BOOL != typecode; }

  template <>
  inline bool Lim<char>::maybe_smaller_than(PrimTypeCode_e typecode) throw ()
    { return 0 == ((BOOL_TYPE | CHAR_TYPE) & typecode); }

  template <>
  inline bool Lim<signed char>::maybe_smaller_than(PrimTypeCode_e typecode)
      throw ()
    { return 0 == ((BOOL_TYPE | CHAR_TYPE) & typecode); }

  template <>
  inline bool Lim<short>::maybe_smaller_than(PrimTypeCode_e typecode) throw ()
    { return 0 != ((INT_TYPE | LONG_TYPE) & typecode); }

  template <>
  inline bool Lim<int>::maybe_smaller_than(PrimTypeCode_e typecode) throw ()
    { return 0 != (LONG_TYPE & typecode); }

  template <>
  inline bool Lim<unsigned char>::maybe_smaller_than(PrimTypeCode_e typecode)
      throw ()
    { return 0 == ((BOOL_TYPE | CHAR_TYPE) & typecode); }

  template <>
  inline bool Lim<unsigned short>::maybe_smaller_than(PrimTypeCode_e typecode)
      throw ()
    { return 0 != ((INT_TYPE | LONG_TYPE) & typecode); }

  template <>
  inline bool Lim<unsigned int>::maybe_smaller_than(PrimTypeCode_e typecode)
      throw ()
    { return 0 != (LONG_TYPE & typecode); }

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
 * $Log: hoops_limits.h,v $
 * Revision 1.7  2004/06/24 20:48:39  peachey
 * Remove dead code which had been left in with an #ifdef until
 * it was certain it was not needed.
 *
 * Revision 1.6  2004/06/24 20:26:38  peachey
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
 * Revision 1.5  2004/03/31 16:20:52  peachey
 * Make proper boolean expressions instead of using implicit conversion to
 * bool, to silence VC7 performance warnings on Windows.
 *
 * Revision 1.4  2003/12/02 14:39:43  peachey
 * To support compilers which do not have limits, such as g++ 2.95.x,
 * add round_error field to Lim class. This allows the test code to
 * be compiled without the limits header.
 *
 * Revision 1.3  2003/11/26 18:50:03  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.2  2003/06/18 18:16:52  peachey
 * Original purpose of this file is now met by hoops_numeric_limits.h.
 * This file now contains only items related to Lim class.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:30  jchiang
 * First import
 *
 * Revision 1.1  2003/05/15 03:55:47  peachey
 * Replacement for <limits> for systems/compilers which don't yet have <limits>.
 *
 ******************************************************************************/
