/*
 *	basic.h: --- general definitions and useful macros ---
 *
 *	AUTHOR:
 *
 *	UL, Mon Apr 11 14:26:37 MESZ 1994
 *      FT, 18/02/00 - MAXFNAME_LEN changed for Linux system
 *      BS, 08/07/02 - added macros from 'cfitsio.h' and 'cfitsio_ftools.h'
 *      FT, 11/01/03 - changed XTYPE type from unsigned int to unsigned char
 *      FT, 27/01/03 - aded UTYPE (unsigned short integer) and 
 *                     VTYPE (unsigned integer)
 */

#ifndef BASIC_H
#define BASIC_H

#include <limits.h>
#include <stdio.h>
#include <stddef.h>
#include "fitsio.h"
#include "cfortran.h"

/* From cfitsio_ftools.h */

#define FITS_CLEN_HDEFKWDS  25

/* From cfitsio.h */

#define XTYPE				unsigned char	/* the "X" type (1 Byte)		*/
#define XTYPE_NULL			(XTYPE)0		/* the 0 of XTYPE				*/
#define XTYPE_MAX			(XTYPE)UCHAR_MAX/* max. positive XTYPE value	*/
#define BTYPE				unsigned char	/* the "B" type (1 Byte)		*/
#define BTYPE_NULL			(BTYPE)0		/* the 0 of BTYPE				*/
#define BTYPE_MAX			(BTYPE)UCHAR_MAX/* max. positive BTYPE value	*/


#define ITYPE				short			/* the "I" type	(2 Bytes)	   */
#define ITYPE_NULL			(ITYPE)0		/* the 0 of ITYPE		   */
#define ITYPE_MAX			(ITYPE)SHRT_MAX	        /* max. positive ITYPE value	   */
#define UTYPE				unsigned short			/* the "U" type	(2 Bytes)  */
#define UTYPE_NULL			(UTYPE)0		/* the 0 of UTYPE		   */
#define UTYPE_MAX			(UTYPE)USHRT_MAX	        /* max. positive UTYPE value	   */
#define JTYPE				long			/* the "J" type (4 Bytes)	   */
/* changed from int to long to fix problems on Alpha, shouldn't make a
difference on Solaris, since SIZEOF(long)==SIZEOF(int) */
#define JTYPE_NULL			(JTYPE)0		/* the 0 of JTYPE		   */
#define JTYPE_MAX			(JTYPE)INT_MAX	        /* max. positive JTYPE value	   */
#define VTYPE				unsigned long		/* the "J" type (4 Bytes)	   */
#define VTYPE_NULL			(VTYPE)0		/* the 0 of JTYPE		   */
#define VTYPE_MAX			(VTYPE)UINT_MAX	/* max. positive JTYPE value	*/
#define ETYPE				float			/* the "E" type (4 Bytes)		*/
#define DTYPE				double			/* the "D" type (8 Bytes)		*/
#define FTYPE				ETYPE			/* the "G" type (4 Bytes)		*/
#define GTYPE				DTYPE			/* the "G" type (8 Bytes)		*/
#define LTYPE				BTYPE			/* the "L" type (4 Bytes)		*/
#define STYPE				char			/* the "S" type (strings)		*/
#define SPTYPE				STYPE *			/* the "S" type (str args)		*/


#define Status_t			int				/* type of status variable		*/


typedef fitsfile *FitsFileUnit_t ; 

							/* errors */


							/* miscellaneous */
#define FITSIO_RDONLY		0
#define FITSIO_RDWR			1

typedef enum { HDU_UNKNOWN = -1, HDU_PRIM_ARRAY, HDU_ASCIITABLE, HDU_BINTABLE }
		HDU_t;









				/* some common data type declarations */

#define BYTE			unsigned char	/* this must be a 8 bit quantity	*/
#define HWORD			unsigned short	/* this must be a 16 bit quantity	*/
#define WORD			unsigned int	/* this must be a 32 bit quantity	*/
#define DWORD			double			/* this must be a 64 bit quantity	*/
/*#define TDOUBLE			double removed due to conflict with fitsio.h and
it is not used anywhere */
#define S_BYTE			char
#define S_HWORD			short
#define S_WORD			int

#define BOOL			int
#if defined(TRUE)
	#undef TRUE
#endif
#define TRUE			(1)
#if defined(FALSE)
	#undef FALSE
#endif
#define	FALSE			(0)
#define HWBITS			(CHAR_BIT*sizeof(HWORD))
#define WBITS			(CHAR_BIT*sizeof(WORD))
#define NBMIN			(HWORD)0
#define NBMAX			(HWORD)15
#define BMIN			(BYTE)0
#define BMAX			(BYTE)255
#define HWMIN			(HWORD)0
#define HWMAX			(HWORD)USHRT_MAX
#define WMIN			(WORD)0
#define WMAX			(WORD)UINT_MAX
#define NULLPTR(a)		((a *)NULL)
#define VPNULL			NULLPTR(void)
#define CPNULL			NULLPTR(char)
#define UPNULL			NULLPTR(unsigned)
#define IPNULL			NULLPTR(int)
#define SPNULL			NULLPTR(short)
#define DPNULL			NULLPTR(double)
#define MAXINFILES               10
#if defined(LINUX)
        #define MAXFNAME_LEN FILENAME_MAX
#else
        #define MAXFNAME_LEN    8*FILENAME_MAX
#endif
#define MAXEXT_LEN		FILENAME_MAX

#define	ONEKB			1024
#define ONEMB			(1024*ONEKB)

							/*****************/
							/* useful macros */
							/*****************/

#define TMEMSET0(a,b)         memset(a,'\0',sizeof(b))

#define TOSTR(a)			#a
#define CSTR(a)				TOSTR(a)

#define YCAT(a, b)			a ## b
#define CAT(a, b)			YCAT(a, b)
#define CCAT(a, b, c)		CAT(CAT(a, b), c)

#define NUM_OF(x)			(sizeof(x)/sizeof(x[0]))
#define STREQ(a, b)			(strcmp((a), (b)) == 0)
#define STRIEQ(a, b)		(stricmp((a), (b)) == 0)
#define STRNCPY(a, b)		(strncpy(a, b, sizeof(a)))
#define TMALLOC(a)			((a *)malloc(sizeof(a)))
#define TSMALLOC(a, n)		((a *)malloc((n) * sizeof(a)))

#ifdef MIN
	#undef MIN
#endif
#define MIN(a, b)			((a) <= (b) ? (a) : (b))
#ifdef MAX
	#undef MAX
#endif
#define MAX(a, b)			((a) >= (b) ? (a) : (b))

#define MINMAX(a, b, c)		(MIN((c), MAX((a), (b))))

#define PERCENTAGE(a, b)	((b) ? (100. * (double)(a)/(double)(b)) : 100.)

#ifdef BSIZE
	#undef BSIZE
#endif
#define BSIZE(a)		((a)*sizeof(BYTE))
#define HWSIZE(a)		((a)*sizeof(HWORD))
#define WSIZE(a)		((a)*sizeof(WORD))

#define BYP(a)			((BYTE *)(a))
#define HWP(a)			((HWORD *)(a))
#define WDP(a)			((WORD *)(a))

#ifndef LITTLE_ENDIAN
	#define SWAP_FIELDS(a, b) a b
#else
	#define SWAP_FIELDS(a, b) b a
#endif


#define HWORD_BITFIELD_LIST_02(a,na,b,nb)									\
	SWAP_FIELDS(HWORD a : na;, HWORD b : nb;)
#define HWORD_BITFIELD_LIST_03(a,na,b,nb,c,nc)								\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_02(a,na,b,nb), HWORD c : nc;)
#define HWORD_BITFIELD_LIST_04(a,na,b,nb,c,nc,d,nd)							\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_02(a,na,b,nb),							\
				HWORD_BITFIELD_LIST_02(c,nc,d,nd))
#define HWORD_BITFIELD_LIST_05(a,na,b,nb,c,nc,d,nd,e,ne)					\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_04(a,na,b,nb,c,nc,d,nd),				\
				HWORD e : ne;)
#define HWORD_BITFIELD_LIST_06(a,na,b,nb,c,nc,d,nd,e,ne,f,nf)				\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_03(a,na,b,nb,c,nc),						\
				HWORD_BITFIELD_LIST_03(d,nd,e,ne,f,nf))
#define HWORD_BITFIELD_LIST_07(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng)			\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_06(a,na,b,nb,c,nc,d,nd,e,ne,f,nf),		\
				HWORD g : ng;)
#define HWORD_BITFIELD_LIST_08(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh)		\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_04(a,na,b,nb,c,nc,d,nd),				\
				HWORD_BITFIELD_LIST_04(e,ne,f,nf,g,ng,h,nh))
#define HWORD_BITFIELD_LIST_09(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh,		\
				i,ni)														\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_08(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,	\
				h,nh), HWORD i : ni;)
#define HWORD_BITFIELD_LIST_10(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh,		\
				i,ni,j,nj)													\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_05(a,na,b,nb,c,nc,d,nd,e,ne),			\
				HWORD_BITFIELD_LIST_05(f,nf,g,ng,h,nh,i,ni,j,nj))
#define HWORD_BITFIELD_LIST_11(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh,		\
				i,ni,j,nj,k,nk)												\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_10(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,	\
				h,nh,i,ni,j,nj), HWORD k : nk;)
#define HWORD_BITFIELD_LIST_12(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh,		\
				i,ni,j,nj,k,nk,l,nl)										\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_06(a,na,b,nb,c,nc,d,nd,e,ne,f,nf),		\
				HWORD_BITFIELD_LIST_06(g,ng,h,nh,i,ni,j,nj,k,nk,l,nl))
#define HWORD_BITFIELD_LIST_13(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh,		\
				i,ni,j,nj,k,nk,l,nl,m,nm)									\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_12(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,	\
				h,nh,i,ni,j,nj,k,nk,l,nl), HWORD m : nm;)
#define HWORD_BITFIELD_LIST_14(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh,		\
				i,ni,j,nj,k,nk,l,nl,m,nm,n,nn)								\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_07(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng),	\
				HWORD_BITFIELD_LIST_07(h,nh,i,ni,j,nj,k,nk,l,nl,m,nm,n,nn))
#define HWORD_BITFIELD_LIST_15(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh,		\
				i,ni,j,nj,k,nk,l,nl,m,nm,n,nn,o,no)							\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_14(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,	\
				h,nh,i,ni,j,nj,k,nk,l,nl,m,nm,n,nn), HWORD o : no;)
#define HWORD_BITFIELD_LIST_16(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,h,nh,		\
				i,ni,j,nj,k,nk,l,nl,m,nm,n,nn,o,no,p,np)					\
	SWAP_FIELDS(HWORD_BITFIELD_LIST_08(a,na,b,nb,c,nc,d,nd,e,ne,f,nf,g,ng,	\
				h,nh), HWORD_BITFIELD_LIST_08(i,ni,j,nj,k,nk,l,nl,m,nm,n,nn,\
				o,no,p,np))

#define MAINFUN			int main(int argc, char **argv)

#if defined(SUNOS)
	#define MAIN				\
		void MAIN_(void) {};	\
		MAINFUN
#else
	#define MAIN	MAINFUN
#endif

#define GETMEM(var, type, size, name, retval)							\
	if (!(var = TSMALLOC(type, size))) {								\
		Error(name, "Failed to allocate %4.1lf Mb for data vector in "	\
			  "file \"" __FILE__ "\" line #" CSTR(__LINE__),			\
			  ((size) * sizeof(type))/(double)ONEMB);					\
		return (retval);												\
	}

#define SWAP_VALS(type, val_a, val_b)									\
	{																	\
		type	tmp = (val_a);	   										\
		val_a	= (val_b);												\
		val_b	= tmp;													\
	}

#endif



