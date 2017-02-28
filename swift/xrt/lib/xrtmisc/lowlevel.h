/*
 *	lowlevel.h: --- low-level utility routines for various purposes ---
 *
 *	DESCRIPTION:
 *		The module provides several functions for performing various low-level
 *		tasks, e.g. reversing byte order in halfword data with wrong bytesex
 *		etc. Designation used:
 *				1 half-word = 2 Bytes
 *				1 word = 2 half-words = 4 Bytes
 *				1 double-word = 2 words = 8 Bytes
 *
 *	EXPORTED FUNCTIONS
 *		GetByte			--- get a single byte
 *		GetBytes		--- get a byte vector
 *		GetHalfWord		--- get a halfword from two consecutive bytes
 *		GetHalfWords	--- get a vector of halfwords from byte vector
 *		GetWord			--- get a word from four consecutive bytes
 *		GetWords		--- get a vector of words from byte vector
 *		I3toI4			--- get a word from three consecutive bytes
 *		GetBits			--- extract a bit-subfield from a word
 *		PutHalfWord		--- pack a half-word into a byte vector in (BE order)
 *		PutWord			--- pack a word into a byte vector (BE order)
 *		PutDoubleWord	--- pack a double word into a byte vector (BE order)
 *
 *	AUTHOR:
 *		UL, Mon Jul 11 20:09:12 METDST 1994
 */

#ifndef LOWLEVEL_H
#define LOWLEVEL_H

                    /********************************/
                    /*        header files          */
                    /********************************/

#include "basic.h"
#include "fitsio2.h"


		/********************************/
		/*      function prototypes     */
		/********************************/

/*
 *	SYNOPSIS:
 *		BYTE GetByte(const BYTE vec[])
 *
 *	DESCRIPTION:
 *	
 *
 *	DOCUMENTATION:
 *
 *	PARAMETERS:
 *
 *	 I	:	<none>
 *	 O	:	<none> 
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	<none>
 */
extern BYTE GetByte			(const BYTE vec[]);

extern unsigned GetBytes	(BYTE vec[],
							 const BYTE ByteVec[],
							 const unsigned n);

extern HWORD GetHalfWord	(const BYTE vec[]);

extern unsigned GetHalfWords(HWORD HalfWordVec[],
							 const BYTE ByteVec[],
							 unsigned n);

extern WORD GetWord			(const BYTE vec[]);

extern unsigned GetWords	(WORD WordVec[],
							 const BYTE ByteVec[],
							 unsigned n);

extern WORD I3toI4			(HWORD hw,
							 BYTE byt);

extern WORD GetBits			(WORD a,
							 WORD b,
							 WORD c);

extern void PutHalfWord		(HWORD hw,
							 BYTE ByteVec[2]);

extern void PutWord			(int w,
							 BYTE ByteVec[4]);

extern void PutDoubleWord	(DWORD dw,
							 BYTE ByteVec[8]);

#endif
