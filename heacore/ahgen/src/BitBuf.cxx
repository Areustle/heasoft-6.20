/// \file BitBuf.cxx
/// \brief General bit reading/writing functions.
/// \author James Peachey
/// \date $Date: 2014/03/05 01:39:51 $
 
#define AHLABEL ahgen_bitbuf
#define AHCVSID "$Id: BitBuf.cxx,v 1.12 2014/03/05 01:39:51 rshill Exp $"

#include "ahlog/ahlog.h"
#include "ahgen/BitBuf.h"

#include <cstring>

namespace ahgen {

BitBuf::BitBuf(SizeType num_chunks, SizeType bits_per_chunk):
    m_buf(0u), m_num_elements(0u), m_elem_index(0u), m_bit_index(0u), m_curr_elem(0u), m_next_elem(0u) {
  // Round up size of buffer to nearest ULongestType buffer.
  m_num_elements = (num_chunks*bits_per_chunk)/s_max_bits;
  if (m_num_elements*s_max_bits < num_chunks*bits_per_chunk) {
    ++m_num_elements;
  }
  m_buf = new ULongestType[m_num_elements];
  std::memset(m_buf, '\0', m_num_elements*sizeof(ULongestType));
  m_num_chunks = num_chunks;
  m_bits_per_chunk = bits_per_chunk;
}

BitBuf::~BitBuf() { delete [] m_buf; }

bool BitBuf::big_endian_machine () {
// Returns true if big-endian (Sun, MIPS, old Mac), false if little-endian (Intel, Alpha).
// Slightly adapted from Doug Mink's C routine imswapped in WCSLIB.
  char * ctest;
  int itest;

  itest = 1;
  ctest = (char *)&itest;
  if (*ctest) {
    return false;
  } else {
    return true;
  }
}

void BitBuf::byte_swap(ULongestType & elem) {
  // Brute force byte swap.
  char * byte_ptr;
  char byte;
  SizeType target;
  SizeType n_bytes=sizeof(ULongestType);

  byte_ptr = (char *)&elem;
  for (SizeType i=0; i<n_bytes/2; i++) {
    target = n_bytes-1-i;
    byte = byte_ptr[target];
    byte_ptr[target] = byte_ptr[i];
    byte_ptr[i] = byte;
  }
}

void BitBuf::bit_swap_by_bytes(ULongestType & elem) {
  char * b;
  SizeType n_bytes=sizeof(ULongestType);

  b = (char *)&elem;
  for (SizeType i=0; i<n_bytes; i++) {
    // Bit reversal fragment found on stackoverflow.com
    b[i] = (b[i] & 0xF0) >> 4 | (b[i] & 0x0F) << 4;
    b[i] = (b[i] & 0xCC) >> 2 | (b[i] & 0x33) << 2;
    b[i] = (b[i] & 0xAA) >> 1 | (b[i] & 0x55) << 1;
  }
}

void BitBuf::bufBitSwapB() {
  SizeType j;
  ULongestType elem;
  for (j=0; j<m_num_elements; ++j) {
    elem = m_buf[j];
    bit_swap_by_bytes(elem);
    m_buf[j] = elem;
  }
}

void BitBuf::bufByteSwap32(long long dataLength) {

  unsigned char b;
  long long j, remainder;

  unsigned char * m_buf_char_ptr = (unsigned char *)m_buf;

  remainder = dataLength;
  for (j=0; j<dataLength; j+=4) {

    b = m_buf_char_ptr[j];
    m_buf_char_ptr[j] = m_buf_char_ptr[j+3];
    m_buf_char_ptr[j+3] = b;

    b = m_buf_char_ptr[j+1];
    m_buf_char_ptr[j+1] = m_buf_char_ptr[j+2];
    m_buf_char_ptr[j+2] = b;

    remainder -= 4;
  }
  //  Need to define behavior for left-over pieces.
  //  Nothing official as of 2012-12-17.
  
  //  Other non-zero values of remainder are pathological and
  //  should be caught before this point.  Behavior for
  //  them is undefined.
  //  if (remainder == 2) {
  //    b = m_buf_char_ptr[dataLength-2];
  //    m_buf_char_ptr[dataLength-2] = m_buf_char_ptr[dataLength-1];
  //    m_buf_char_ptr[dataLength-1] = b;
  //  }

  // As of 2012-12-18 it looks as if each row of ASIC data is padded
  // to 32 bits, but that individual ASICs padded to 16 bits are
  // stored contiguously.  It *appears* from the chart as if the
  // 32-bit words are stored big-endian in the FFF raw_asic_data
  // column, but this has not been confirmed.
  if (remainder != 0) {
    AH_THROW_RUNTIME("bad buffer length found when byte-swapping 32-bit words");
  }
}

void BitBuf::io(ByteType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io(ShortType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io(IntType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io(LongType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io(LongestType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io(UByteType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io(UShortType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io(UIntType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io(ULongType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  ULongestType longest = elem;
  io(longest, num_bits, io_flag, bit_order);
  elem = longest;
}

void BitBuf::io (ULongestType & elem, SizeType num_bits, BitBufIOEnum io_flag,
                BitBufOrderEnum bit_order) {
  // Create local variables to store the source and destination of this operation.
  ULongestType src = 0u, elem_sh_1 = 0u, elem_sh_2 = 0u;

  // "Unpacking" means you are peeling stuff out of the
  //   internal buffer, which has unpredictable bit
  //   ordering and alignment of data items, into a long long variable, 
  //   right justified.
  // "Packing" means the opposite:  you have a well-behaved
  //   variable and want to store it into the next num_bits bits of
  //   a long long buffer.
  //
  // There is one big assumption encoded in the fancy chart that I am
  // counting on: when bytes are arranged in little-endian order, all bits
  // that should be logically contiguous are contiguous, even if the
  // order has to be reversed at some point to interpret the integer. The
  // variable-length arrays containing telemetry blocks are byte arrays
  // in the FITS files, so this should be true...right?...right?
  //
  // References to big-endian machines below only have to do with the
  // parsing of bytes into long data types in an internal buffer, not with
  // external data sets, such as the FFF and SFF.
  //

  //AH_DEBUG << "m_bit_index = " << m_bit_index << std::endl;
  //AH_DEBUG << "m_elem_index = " << m_elem_index << std::endl;
  if (e_UNPACK == io_flag) {
    // Source is the buffer, bit-shifted to align with the output element.
    if (m_elem_index > (m_num_elements-1)) {
      AH_THROW_LOGIC("buffer overrun in unpacking operation for current element");
    } else {
      m_curr_elem = m_buf[m_elem_index];
      if (m_elem_index >= (m_num_elements-1)) {
        m_next_elem = 0u;
      } else {
        m_next_elem = m_buf[m_elem_index+1];
      }
    }
    if (big_endian_machine()) {
      // Big-endian example: MIPS.
      // Little-endian example: Intel.
      byte_swap(m_curr_elem);
      byte_swap(m_next_elem);
    }
    // Note that in C++ a shift by the number of bits in the variable has an
    // undefined result.  Extra-cautious special handling here.
    elem_sh_1 = (m_bit_index == s_max_bits ? 0u : m_curr_elem >> m_bit_index);
    elem_sh_2 = (m_bit_index == 0          ? 0u : m_next_elem << (s_max_bits - m_bit_index));
    src = elem_sh_1 | elem_sh_2;
    //AH_DEBUG << "m_curr_elem = " << std::hex << m_curr_elem << std::dec << std::endl;
    //AH_DEBUG << "m_next_elem = " << std::hex << m_next_elem << std::dec << std::endl;
    //AH_DEBUG << "elem_sh_1 = " << std::hex << elem_sh_1 << std::dec << std::endl;
    //AH_DEBUG << "elem_sh_2 = " << std::hex << elem_sh_2 << std::dec << std::endl;
  } else if (e_PACK == io_flag) {
    src = elem;
  }

  if (bit_order == e_ORDER_REVERSED) {
    //AH_DEBUG << "src before swap = " << std::hex << src << std::dec << std::endl;
    byte_swap(src);
    bit_swap_by_bytes(src);
    //AH_DEBUG << "src  after swap = " << std::hex << src << std::dec << std::endl;
    src >>= (s_max_bits - num_bits);
    //AH_DEBUG << "s_max_bits - num_bits = " << s_max_bits - num_bits << std::endl;
    //AH_DEBUG << "src  after shift = " << std::hex << src << std::dec << std::endl;
  } else {
    // Clear out all but the requested bits.
    src <<= (s_max_bits - num_bits);
    src >>= (s_max_bits - num_bits);
  }
  
  if (e_UNPACK == io_flag) {
    elem = src;
  } else if (e_PACK == io_flag) {
    // Note that in C++ a shift by the number of bits in the variable has an
    // undefined result.  Extra-cautious special handling here.
    m_curr_elem = (m_bit_index == 0 ? src : src << m_bit_index);   
    m_next_elem = (m_bit_index == 0 ? 0u : src >> (s_max_bits - m_bit_index));
    //AH_DEBUG << std::hex << m_curr_elem << std::dec << std::endl;
    //AH_DEBUG << std::hex << m_next_elem << std::dec << std::endl;
    if (big_endian_machine()) {
        byte_swap(m_curr_elem);
        byte_swap(m_next_elem);
    }
    if (m_elem_index > (m_num_elements-1)) {
      AH_THROW_LOGIC("buffer overrun in packing operation for current element");
    } else {
      //AH_DEBUG << "m_elem_index = " << m_elem_index << std::endl;
      m_buf[m_elem_index] |= m_curr_elem;
      if (m_elem_index >= (m_num_elements-1)) {
        if (m_bit_index + num_bits > s_max_bits) 
          AH_THROW_LOGIC("buffer overrun in packing operation for next element");
      } else {
        m_buf[m_elem_index+1] |= m_next_elem;
      }
    }
  }

  // Advance pointers to next unused portion of buffer.
  m_bit_index += num_bits;
  m_elem_index += m_bit_index / s_max_bits;
  m_bit_index %= s_max_bits;
}

void BitBuf::rewind() {
  m_elem_index = 0u;
  m_bit_index = 0u;
  std::memset(m_buf, '\0', m_num_elements*sizeof(ULongestType));
}

ULongestType BitBuf::get_elem_index() const {
  return m_elem_index;
}

ULongestType BitBuf::get_bit_index() const {
  return m_bit_index;
}

ULongestType BitBuf::get_curr_elem() const {
  return m_curr_elem;
}

ULongestType BitBuf::get_next_elem() const {
  return m_next_elem;
}

std::string bitsToStr(const ULongestType & element, SizeType num_bits) {
  if (BitBuf::s_max_bits < num_bits) AH_THROW_LOGIC("attempt to print too many bits");
  char elem_string[BitBuf::s_max_bits + 1] = "";
  SizeType ii = 0u;
  for (ii = 0u; ii != num_bits; ++ii) {
    elem_string[num_bits - ii - 1] = '0' + (1 & (element >> ii));
  }
  elem_string[ii] = '\0';
  return elem_string;
}

ULongestType strToBits(const std::string & elem_string) {
  SizeType num_bits = elem_string.size();
  ULongestType element = 0u;
  for (SizeType ii = 0u; ii != num_bits; ++ii) {
    element <<= 1u; // 0th shift has no effect, since element == 0. Subsequent shifts make room for more bits.
    element |= ULongestType(elem_string[ii] - '0'); // Put the next bit into the element.
  }
  return element;
}

}
/* Revision Log
 $Log: BitBuf.cxx,v $
 Revision 1.12  2014/03/05 01:39:51  rshill
 Commented out AH_DEBUG statements to improve timing.

 Revision 1.11  2013/03/29 14:48:59  rshill
 Added signed types as output.

 Revision 1.10  2012/12/18 19:36:32  rshill
 Fixed 32-bit byte swap according to state of HXI/SGD telemetry charts
 ("fancy charts") as of 2012-12-18 per TAKAHASHI Hiromitsu.

 Revision 1.9  2012/12/18 15:37:17  rshill
 Corrected bug in 32-bit byte swap.

 Revision 1.8  2012/12/18 01:40:01  rshill
 Capabilities augmented to accommodate SGD FFF/SFF format of
 2012-12-04.  Done with "pre/post conditioner" routines to
 do global swaps of bits and/or bytes on the telemetry buffer
 before/after using the BitBuf::io capability.

 Revision 1.7  2012/12/14 21:13:21  rshill
 Corrected initialization bug, changed some parameter names for clarity

 Revision 1.6  2012/10/25 22:56:01  rshill
 Debugged the UNPACK side.

 Revision 1.5  2012/10/18 22:57:53  rshill
 Added revision log substitution.

*/
