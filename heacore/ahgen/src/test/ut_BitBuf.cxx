/// \file ut_BitBuf.cxx
/// \brief unit test for BitBuf
/// \author Bob S. Hill
/// \date $Date: 2014/12/16 18:45:04 $
 
#define AHLABEL ahgen_ut_bitbuf
#define AHCVSID "$Id: ut_BitBuf.cxx,v 1.3 2014/12/16 18:45:04 mwitthoe Exp $"

#include "ahgen/BitBuf.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"

using namespace ahgen;

// ---------------------------------------------------------------------------

void ut_BitBuf(void) {

  LABEL_TEST("Test bit packing and unpacking functions");

  ahlog::set_debug(false);

  START_TEST("unpacking with series of forward unsigned 10-bit integers") {

    UByteType * bptr;
    BitBuf * bb = new BitBuf(16,8);
    bptr = (UByteType *)bb->m_buf;
    ULongestType elem_out;
    bool failure;
    ULongestType * uintval = new ULongestType[10];

    bptr[0] = 0x00;  //  00000000
    bptr[1] = 0x02;  //  00000010
    bptr[2] = 0x0c;  //  00001100
    bptr[3] = 0x38;  //  00111000
    bptr[4] = 0xf0;  //  11110000
    bptr[5] = 0xe0;  //  11100000
    bptr[6] = 0xc3;  //  11000011
    bptr[7] = 0x8f;  //  10001111
    bptr[8] = 0x3f;  //  00111111
    bptr[9] = 0xff;  //  11111111
    bptr[10] = 0xfe; //  11111110
    bptr[11] = 0xff; //  11111111
    bptr[12] = 0x0f; //  00001111
    bptr[13] = 0x0;
    bptr[14] = 0x0;
    bptr[15] = 0x0;

    uintval[0] = 0x0200;  // 0010 0000 0000
    uintval[1] = 0x0300;  // 0011 0000 0000
    uintval[2] = 0x0380;  // 0011 1000 0000
    uintval[3] = 0x03c0;  // 0011 1100 0000
    uintval[4] = 0x03e0;  // 0011 1110 0000
    uintval[5] = 0x03f0;  // 0011 1111 0000
    uintval[6] = 0x03f8;  // 0011 1111 1000
    uintval[7] = 0x03fc;  // 0011 1111 1100
    uintval[8] = 0x03fe;  // 0011 1111 1110
    uintval[9] = 0x03ff;  // 0011 1111 1111
    
    failure = false;
    for (int i=0; i<10; i++) {
      AH_INFO(ahlog::LOW) << std::dec << bb->get_elem_index() << " " << bb->get_bit_index() 
        << " " << std::hex << bb->get_curr_elem() << " " << bb->get_next_elem() << std::endl;
      bb->io(elem_out, (SizeType)10, BitBuf::e_UNPACK, BitBuf::e_ORDER_NORMAL);
      AH_INFO(ahlog::LOW) << std::hex << "Expected: " << uintval[i] << "     Obtained: " << elem_out << std::endl;
      if (elem_out != uintval[i]) failure = true;
    }
    if (failure) FAIL;
    delete [] uintval;
    delete bb;
  } END_TEST

  ahlog::set_debug(false);

  START_TEST("unpacking with series of reversed unsigned 10-bit integers") {
    UByteType * bptr;
    BitBuf * bb = new BitBuf(16,8);
    bptr = (UByteType *)bb->m_buf;
    ULongestType elem_out;
    bool failure;
    ULongestType * uintval = new ULongestType[10];

    bptr[0] = 0x01;  // 00000001.
    bptr[1] = 0x0c;  // 000011.00
    bptr[2] = 0x70;  // 0111.0000
    bptr[3] = 0xc0;  // 11.000000
    bptr[4] = 0x03;  // 00000011.
    bptr[5] = 0x1f;  // 00011111.
    bptr[6] = 0xfc;  // 111111.00
    bptr[7] = 0xf0;  // 1111.0000
    bptr[8] = 0xc7;  // 11.000111
    bptr[9] = 0x3f;  // 00111111.
    bptr[10] = 0xff; // 11111111.
    bptr[11] = 0xfd; // 111111.01
    bptr[12] = 0x0f; // 00001111.
    bptr[13] = 0x0;
    bptr[14] = 0x0;
    bptr[15] = 0x0;

    uintval[0] = 0x0200;  // 0010 0000 0000
    uintval[1] = 0x0300;  // 0011 0000 0000
    uintval[2] = 0x0380;  // 0011 1000 0000
    uintval[3] = 0x03c0;  // 0011 1100 0000
    uintval[4] = 0x03e0;  // 0011 1110 0000
    uintval[5] = 0x03f0;  // 0011 1111 0000
    uintval[6] = 0x03f8;  // 0011 1111 1000
    uintval[7] = 0x03fc;  // 0011 1111 1100
    uintval[8] = 0x03fe;  // 0011 1111 1110
    uintval[9] = 0x03ff;  // 0011 1111 1111
    
    failure = false;
    for (int i=0; i<10; i++) {
      AH_INFO(ahlog::LOW) << std::dec << bb->get_elem_index() << " " << bb->get_bit_index() 
        << " " << std::hex << bb->get_curr_elem() << " " << bb->get_next_elem() << std::endl;
      bb->io(elem_out, (SizeType)10, BitBuf::e_UNPACK, BitBuf::e_ORDER_REVERSED);
      AH_INFO(ahlog::LOW) << std::hex << "Expected: " << uintval[i] << "     Obtained: " << elem_out << std::endl;
      if (elem_out != uintval[i]) failure = true;
    }
    if (failure) FAIL;
    delete [] uintval;
    delete bb;
  } END_TEST

  ahlog::set_debug(false);

  START_TEST("unpacking with series of unsigned 10-bit integers, alternating senses 1") {
    UByteType * bptr;
    BitBuf * bb = new BitBuf(16,8);
    bptr = (UByteType *)bb->m_buf;
    ULongestType elem_out;
    bool failure;
    BitBuf::BitBufOrderEnum curr_bit_order;
    ULongestType * uintval = new ULongestType[10];

    bptr[0] = 0x01;  // 00000001.
    bptr[1] = 0x00;  // 000000.00 
    bptr[2] = 0x7c;  // 0111.1100
    bptr[3] = 0x00;  // 00.000000 
    bptr[4] = 0xf0;  // .11110000
    bptr[5] = 0x1f;  // 00011111.
    bptr[6] = 0xc0;  // 110000.00
    bptr[7] = 0xff;  // 1111.1111 
    bptr[8] = 0x07;  // 00.000111
    bptr[9] = 0xff;  // .11111111
    bptr[10] = 0xff; // 11111111.
    bptr[11] = 0xfd; // 111111.01 
    bptr[12] = 0x0f; // 0000.1111
    bptr[13] = 0x0;
    bptr[14] = 0x0;
    bptr[15] = 0x0;

    uintval[0] = 0x0200;  // 0010 0000 0000
    uintval[1] = 0x0300;  // 0011 0000 0000
    uintval[2] = 0x0380;  // 0011 1000 0000
    uintval[3] = 0x03c0;  // 0011 1100 0000
    uintval[4] = 0x03e0;  // 0011 1110 0000
    uintval[5] = 0x03f0;  // 0011 1111 0000
    uintval[6] = 0x03f8;  // 0011 1111 1000
    uintval[7] = 0x03fc;  // 0011 1111 1100
    uintval[8] = 0x03fe;  // 0011 1111 1110
    uintval[9] = 0x03ff;  // 0011 1111 1111
    
    failure = false;
    for (int i=0; i<10; i++) {
      AH_INFO(ahlog::LOW) << std::dec << bb->get_elem_index() << " " << bb->get_bit_index() 
        << " " << std::hex << bb->get_curr_elem() << " " << bb->get_next_elem() << std::dec << std::endl;
      curr_bit_order = (i % 2 == 0 ? BitBuf::e_ORDER_REVERSED : BitBuf::e_ORDER_NORMAL);
      bb->io(elem_out, (SizeType)10, BitBuf::e_UNPACK, curr_bit_order);
      AH_INFO(ahlog::LOW) << std::hex << "Expected: " << uintval[i] << "     Obtained: " << elem_out << std::dec << std::endl;
      if (elem_out != uintval[i]) {
          AH_INFO(ahlog::LOW) << "DIFF = " << std::hex << elem_out-uintval[i] << std::dec << std::endl;
          failure = true;
      }
    }
    if (failure) FAIL;
    delete [] uintval;
    delete bb;
  } END_TEST

  ahlog::set_debug(false);

  START_TEST("unpacking with series of unsigned 10-bit integers, alternating senses 2") {
    UByteType * bptr;
    BitBuf * bb = new BitBuf(16,8);
    bptr = (UByteType *)bb->m_buf;
    ULongestType elem_out;
    bool failure;
    BitBuf::BitBufOrderEnum curr_bit_order;
    ULongestType * uintval = new ULongestType[10];

    bptr[0] = 0x00;   // 00000000.
    bptr[1] = 0x0e;   // 000011.10 
    bptr[2] = 0x00;   // 0000.0000
    bptr[3] = 0xf8;   // 11.111000 
    bptr[4] = 0x03;   // .00000011
    bptr[5] = 0xe0;   // 11100000.
    bptr[6] = 0xff;   // 111111.11
    bptr[7] = 0x80;   // 1000.0000 
    bptr[8] = 0xff;   // 11.111111
    bptr[9] = 0x3f;   // .00111111
    bptr[10] = 0xfe;  // 11111110.
    bptr[11] = 0xff;  // 111111.11 
    bptr[12] = 0x0f;  // 0000.1111
    bptr[13] = 0x0;
    bptr[14] = 0x0;
    bptr[15] = 0x0;

    uintval[0] = 0x0200;  // 0010 0000 0000
    uintval[1] = 0x0300;  // 0011 0000 0000
    uintval[2] = 0x0380;  // 0011 1000 0000
    uintval[3] = 0x03c0;  // 0011 1100 0000
    uintval[4] = 0x03e0;  // 0011 1110 0000
    uintval[5] = 0x03f0;  // 0011 1111 0000
    uintval[6] = 0x03f8;  // 0011 1111 1000
    uintval[7] = 0x03fc;  // 0011 1111 1100
    uintval[8] = 0x03fe;  // 0011 1111 1110
    uintval[9] = 0x03ff;  // 0011 1111 1111
    
    failure = false;
    for (int i=0; i<10; i++) {
      AH_INFO(ahlog::LOW) << std::dec << bb->get_elem_index() << " " << bb->get_bit_index() 
        << " " << std::hex << bb->get_curr_elem() << " " << bb->get_next_elem() << std::dec << std::endl;
      curr_bit_order = (i % 2 == 0 ? BitBuf::e_ORDER_NORMAL : BitBuf::e_ORDER_REVERSED);
      bb->io(elem_out, (SizeType)10, BitBuf::e_UNPACK, curr_bit_order);
      AH_INFO(ahlog::LOW) << std::hex << "Expected: " << uintval[i] << "     Obtained: " << elem_out << std::dec << std::endl;
      if (elem_out != uintval[i]) {
          AH_INFO(ahlog::LOW) << "DIFF = " << std::hex << elem_out-uintval[i] << std::dec << std::endl;
          failure = true;
      }
    }
    if (failure) FAIL;
    delete [] uintval;
    delete bb;
  } END_TEST

} 

/* Revision Log
 $Log: ut_BitBuf.cxx,v $
 Revision 1.3  2014/12/16 18:45:04  mwitthoe
 ahgen: change the filePathsEquivalent() function so that no errors are thrown if the stat() call fails on either function; in those cases, false is returned (see issue 469; change AH_OUT messages to AH_INFO in ut_BitBuf

 Revision 1.2  2013/01/23 20:04:11  rshill
 Updated for new definition of forward bit order.

 Revision 1.1  2012/10/18 22:56:08  rshill
 Unit test for BitBuf; only does unpacking side at present.


*/
