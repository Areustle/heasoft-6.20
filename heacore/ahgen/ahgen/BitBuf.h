/// \file BitBuf.h
/// \brief Facilities for packing/unpacking streams of bits to/from arrays of unsigned integers.
/// \author James Peachey
/// \date $Date: 2013/03/29 14:49:41 $

/// \addtogroup mod_ahgen
/// \section ahgen_BitBuf Bit Packing/Unpacking - BitBuf
///
/// Facilities for packing/unpacking streams of bits to/from arrays of 
/// unsigned integers.
///

#ifndef AHGEN_BITBUF_H
#define AHGEN_BITBUF_H

#include "ahgen/ahversion.h"
AHVERSION(AHGEN_BITBUF,"$Id: BitBuf.h,v 1.11 2013/03/29 14:49:41 rshill Exp $")

#include <cstddef>
#include <limits>
#include <string>

namespace ahgen {

/** \addtogroup mod_ahgen
 *  @{
 */

typedef std::size_t SizeType;
typedef unsigned char UByteType;
typedef unsigned short UShortType;
typedef unsigned int UIntType;
typedef unsigned long ULongType;
typedef unsigned long long ULongestType;
typedef char ByteType;
typedef short ShortType;
typedef int IntType;
typedef long LongType;
typedef long long LongestType;

class BitBuf {
public:
  /// \brief Simple flags to switch between writing bits into the array or reading bits from the array.
  enum BitBufIOEnum {
    e_UNPACK = 0,
    e_PACK = 1
  };

  /// \brief Simple flags to switch between two bit-orderings in the array.
  enum BitBufOrderEnum {
    e_ORDER_NORMAL = 0,
    e_ORDER_REVERSED = 1
  };

  /// \brief Construct a new BitBuf object with the given number of elements and size. Note the similarity to the arguments to calloc.
  /// \param[in] num_chunks The number of chunks of arbitrary bit length that this BitBuf will need to store.
  /// \param[in] bits_per_chunk The bit length of each chunk that this BitBuf will need to store.
  BitBuf(SizeType num_chunks, SizeType bits_per_chunk);

  /// \brief Destroy the object, freeing up the allocated buffer.
  ~BitBuf();
  
  /// \brief Find word architecture of machine at execution time.
  /// \return TRUE if bytes in a word are ordered most-significan to least-significant; FALSE otherwise
  bool big_endian_machine();

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned byte
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(UByteType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned short
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(UShortType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned int
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(UIntType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned long
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(ULongType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned long long
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(ULongestType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned byte
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(ByteType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned short
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(ShortType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned int
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(IntType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned long
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(LongType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Read/write some number of bits from/to this BitBuf, and store them in an unsigned long long
  /// \param[out] elem Gets the result
  /// \param[in] num_bits Number of bits to process
  /// \param[in] io_flag Transfer direction, into or out of the bit buffer
  /// \param[in] bit_order Whether or not to reverse the bits during the transfer
  void io(LongestType & elem, SizeType num_bits, BitBufIOEnum io_flag, BitBufOrderEnum bit_order);

  /// \brief Stop processing the bit buffer and prepare to reprocess it
  void rewind();

  /// \brief Return the value of the internal element index for debugging
  ULongestType get_elem_index() const;

  /// \brief Return the value of the internal bit index for debugging
  ULongestType get_bit_index() const;

  /// \brief Return the value of the internal current element for debugging
  ULongestType get_curr_elem() const;

  /// \brief Return the value of the internal next element for debugging
  ULongestType get_next_elem() const;

  /// \brief Byte swap one long long
  void byte_swap(ULongestType & elem);

  /// \brief Swap the bits internal to each byte of a long long without changing byte order
  void bit_swap_by_bytes(ULongestType & elem);

  /// \brief Swap the bits internal to each byte of the bit buffer without changing byte order
  void bufBitSwapB();

  /// \brief Treat the bit buffer as 32-bit words and byte-swap within each word
  void bufByteSwap32(long long dataLength);

  /// \brief Constant giving the largest number of bits that may be shifted from the largest type of integer.
  static const SizeType s_max_bits = std::numeric_limits<ULongestType>::digits;

  /// \brief Internal bit buffer:  exposed so it can be connected by ahfits.
  ULongestType * m_buf;
  
private:
  SizeType m_num_chunks, m_bits_per_chunk;
  SizeType m_num_elements;
  SizeType m_elem_index, m_bit_index;
  ULongestType m_curr_elem, m_next_elem;

};

/// \brief Convert a long long to a character string of '1' and '0'.
/// \param[in] element long long to convert
/// \param[in] num_bits number of bits in element
/// \return string containing '1' and '0'
std::string bitsToStr(const ULongestType & element, SizeType num_bits = BitBuf::s_max_bits);

/// \brief Convert a character string of '1' and '0' to a long long.
/// \param[in] elem_string string containing '1' and '0'
/// \return value equivalent to elem_string interpreted as a binary number
ULongestType strToBits(const std::string & elem_string);

/** @} */

}

// #endif AHGEN_BITBUF_H definition
#endif

/* Revision Log
 $Log: BitBuf.h,v $
 Revision 1.11  2013/03/29 14:49:41  rshill
 Added signed types as output.

 Revision 1.10  2013/01/24 18:18:42  mwitthoe
 list Doxygen group in BitBuf.h

 Revision 1.9  2013/01/09 20:42:10  rshill
 Augmented doxygen markup.

 Revision 1.8  2012/12/18 01:40:01  rshill
 Capabilities augmented to accommodate SGD FFF/SFF format of
 2012-12-04.  Done with "pre/post conditioner" routines to
 do global swaps of bits and/or bytes on the telemetry buffer
 before/after using the BitBuf::io capability.

 Revision 1.7  2012/12/14 21:13:20  rshill
 Corrected initialization bug, changed some parameter names for clarity

 Revision 1.6  2012/10/25 22:55:23  rshill
 Added ULongType

 Revision 1.5  2012/10/18 23:00:21  rshill
 Added revison log substitution.

*/
