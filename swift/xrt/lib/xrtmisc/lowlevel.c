/*
 *	lowlevel.c: provide certain utility routines
 *		    designation: 1 halfword = 2 bytes
 *				 1 word = 2 halfwords = 4 bytes
 *
 *      CHANGES:
 *          FT 11/04/2002: SPR_XRT_001 - fatal compilation error on Solaris
 *                         Use 'BYTESWAPPED' defined by fitsio2.h to check
 *                         MACHINE byte order.
 *          BS 06/07/2004: Modified "PutDoubleWord" routine to fix gcc3.3 bug
 * 
 *        
 *	AUTHOR:
 *
 *	UL, Wed Apr 13 12:50:52 MESZ 1994
 */

#include <string.h>

#include "basic.h"
#include "lowlevel.h"

BYTE GetByte(const BYTE vec[])
{
	return vec[0];
} /* GetByte */


unsigned GetBytes(BYTE vec[], const BYTE ByteVec[], const unsigned n)
{
	register unsigned	i;
	register unsigned	CheckSum = 0;

	memcpy(vec, ByteVec, n*sizeof(BYTE));
	for (i=0; i<n; CheckSum+=vec[i++]);

	return CheckSum;
} /* GetBytes */


HWORD GetHalfWord(const BYTE vec[])
{
	return (HWORD)(vec[1]<<8|vec[0]);
} /* GetHalfWord */



unsigned GetHalfWords(HWORD HalfWordVec[], const BYTE ByteVec[],
					  unsigned n)
{
	register unsigned	i;
	register unsigned	CheckSum = 0;

	for(i=0; i<n; ++i)
		CheckSum += HalfWordVec[i] = GetHalfWord(ByteVec+HWSIZE(i));

	return CheckSum;
} /* GetWords */



WORD GetWord(const BYTE vec[])
{
	return (WORD)(vec[3]<<24|vec[2]<<16|vec[1]<<8|vec[0]);
} /* GetWord */



unsigned GetWords(WORD WordVec[], const BYTE ByteVec[], unsigned n)
{
	register unsigned	i;
	register unsigned	CheckSum = 0;

	for(i=0; i<n; ++i)
		CheckSum += WordVec[i] = GetWord(ByteVec+WSIZE(i));

	return CheckSum;
} /* GetWords */



WORD I3toI4(HWORD hw, BYTE byt)
{
	return (WORD)(hw<<16|byt<<8);
} /* I3toI4 */



void PutHalfWord(HWORD hw, BYTE ByteVec[2])
{
	ByteVec[0] = hw >> 8;
	ByteVec[1] = hw & 0xFF;
} /* PutHalfWord */



void PutWord(int w, BYTE ByteVec[4])
{
#if BYTESWAPPED
        PutHalfWord(w >> 16, ByteVec+0);
	PutHalfWord(w & 0xFFFF, ByteVec+2);
#else
	memcpy(ByteVec, &w, sizeof(WORD));
#endif
} /* PutWord */



void PutDoubleWord(DWORD dw, BYTE ByteVec[8])
{
#if BYTESWAPPED
  union {
    WORD wp[2];    
    DWORD dwp;

  }U; 
  U.dwp=dw;

  /* WORD	*wp = WDP(&dw);*/
	PutWord(U.wp[0], ByteVec+4);
	PutWord(U.wp[1], ByteVec+0);
#else
	memcpy(ByteVec, &dw, sizeof(DWORD));
#endif

} /* PutDoubleWord */




