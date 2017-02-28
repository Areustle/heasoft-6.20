/*
 *	highfits.h: --- a collection of high-level interface routines to
 *					the FITSIO library ---
 *
 *	DESCRIPTION:
 *		This module provides a collection of high-level routines to the FITSIO
 *		library for creating, reading, and modifying FITS files.
 *
 *	EXPORTED FUNCTIONS:
 *
 *	Functions that use the FITSIO library:
 *
 *		OpenFitsFile		--- open existing FITS file in R/O or R/W mode
 *		OpenWriteFitsFile	--- create new FITS file
 *		CloseFitsFile		--- close FITS file
 *		WriteComment		--- write comment record(s) to FITS file
 *		WriteHistory		--- write history record(s) to FITS file
 *		FinishPrimaryHeader	--- write primary header out to disk
 *		FinishBintableHeader--- write Bintable header out to disk
 *		WriteHeader			--- write header to disk
 *		WriteUpdatedHeader	--- re-write header to disk
 *		WriteBintable		--- write block of Bintable rows to disk
 *		WriteFastBintable	--- same as `WriteBintable' but faster
 *		ReadBintable		--- read block of Bintable rows from disk
 *		WriteWholeBintable	--- write Bintable to disk
 *		KeyWordMatch		--- check if header keyword has certain value
 *		RetrieveFitsHeader	--- read FITS header from disk into memory
 *		ReadImage			--- read FITS primary array into memory
 *
 *	Function that work independently from FITSIO
 *
 *		EmpytHeader			--- create an empty FITS header
 *		ExistsKeyWord		--- check wether header contains certain keyword
 *		GetLVal				--- retrieve boolean value from header
 *		GetSVal				--- retrieve string value from header
 *		GetJVal				--- retrieve integer value from header
 *		GetDVal				--- retrieve double value from header
 *		GetComment			--- retrieve comment fields of header card
 *		SetLVal				--- set boolean value in header
 *		SetSVal				--- set string value in header
 *		SetJVal				--- set integer value in header
 *		SetDVal				--- set double value in header
 *		SetComment			--- define comment field of header card
 *		InsertCard			--- insert card in FITS header at specified place
 *		MoveCards			--- move header cards
 *		AddCard				--- add a new card to FITS header
 *		AddComHis			--- add comment or history card to FITS header
 *		AddComment			--- add comment record(s) to FITS header
 *		AddHistory			--- add history record(s) to FITS header
 *		AddColumn			--- add new column to Bintable
 *		ChangeColumn		--- modify column in Bintable
 *		DeleteCard			--- delete card from FITS header
 *		DeleteComment		--- delete comment record(s) from FITS header
 *		NewPrimaryHeader	--- create empty FITS primary header in memory
 *		EndPrimaryHeader	--- finish building primary FITS header
 *		NewBintableHeader	--- create empty Bintable header in memory
 *		GetBintableStructure--- figure out structure of Bintable
 *		EndBintableHeader	--- finish building Bintable header
 *		ColNameMatch		--- check if column with certain name exists
 *		RowCp				--- copy Bintable rows (obsolete)
 *              CopyRows                --- copy Bintable rows
 *		RowIllegal			--- fill current row with 'illegal' values
 *		GetColNameIndx		--- get Bintable column index
 *		ColIndices			--- `GetColNameIndx' for multiple columns
 *		ReleaseBintable		--- free memory allocated with Bintable data
 *		ReleaseImage		--- free memory allocated by previous ReadImage
 *              ChecksumCalc            --- update checksum of all fits file extensions
 *              ManageDSKeywords        --- write or update DS keywords
 *	DOCUMENTATION:
 *		[1] Definition of the Flexible Image Transport System, NOST 100-1.0
 *		[2] FITSIO User Manual
 *
 *	AUTHOR:
 *	 	UL, Mon Jul 11 20:09:12 METDST 1994
 *              BS, 06/03/2006 - Modified maximum number of columns 
 *                               in FITS Bintable to handle new header 
 *                               fits file format
 */ 

#ifndef HIGHFITS_H
#define HIGHFITS_H

					/********************************/
                    /*        header files          */
                    /********************************/


#include "basic.h"


					/********************************/
					/*      defines / typedefs      */
					/********************************/

#define MAX_COLUMNS		110				/* maximum number of columns in
										   FITS Bintable					*/



/* defines below copied from CFITSIO source code */

/* #define FNANMASK   0x7F800000 */ /* mask bits 1 - 8; all set on NaNs */
                                     /* all 0 on underflow  or 0. */
 
/* #define DNANMASK   0x7FF00000 */ /* mask bits 1 - 11; all set on NaNs */
                                     /* all 0 on underflow  or 0. */



						/* set data item to "illegal" */

#define SET_E_NULL(a)   (*(unsigned *)&(a) = FNANMASK)

#ifdef BIG_ENDIAN
   #define SET_D_NULL(a)  *(unsigned *)&(a) = DNANMASK, *((unsigned *)&(a)+1) = 1;
#else
   #define SET_D_NULL(a)  *((unsigned *)&(a) + 1) = DNANMASK, *(unsigned *)&(a) = 1;
#endif


/*
#define SET_E_NULL(a)	(*(int *)&(a) = -1)
#define SET_D_NULL(a)	(*(int *)&(a) = *((int *)&(a) + 1) = -1)
*/
										/* macros to set float/double values
										 * to NaN; uses NaN definition of
										 * FITSIO: NaN = all bits set to 1
										 */

#define IJ_NULL_VAL		-1L				/* d/f illegal value for integer	*/
#define SET_IJ_NULL(a)	((a) = IJ_NULL_VAL)

#define L_NULL_VAL		0				/* d/f illegal value for logical	*/
#define SET_L_NULL(a)	((a) = L_NULL_VAL)

						/* is data item "illegal" */
/*
#define IS_E_NULL(a)	(*(int *)&(a) == -1)
#define IS_D_NULL(a)	(*(int *)&(a) == -1 && *((int *)&(a) + 1) == -1)
*/
#define IS_E_NULL(a)  ((*(unsigned *)&(a) & FNANMASK) == FNANMASK)

#ifdef BIG_ENDIAN
    #define IS_D_NULL(a)  ((*(unsigned *)&(a) & DNANMASK) == DNANMASK)
#else
    #define IS_D_NULL(a)  ((*((unsigned *)&(a) + 1) & DNANMASK) == DNANMASK)
#endif


#define IS_IJ_NULL(a)	((a) == IJ_NULL_VAL)
#define IS_L_NULL(a)	((a) == L_NULL_VAL)

						/* transform illegal values */
#define ILL_EVAL2_REAL(a, b)	(!IS_E_NULL(a) ? (a) : (b))
#define ILL_DVAL2_REAL(a, b)	(!IS_D_NULL(a) ? (a) : (b))
#define ILL_IJVAL2_REAL(a, b)	(!IS_IJ_NULL(a) ? (a) : (b))
#define ILL_LVAL2_REAL(a, b)	(!IS_L_NULL(a) ? (a) : (b))

#define REAS_E_VAL(a)	ILL_EVAL2_REAL(a, 0.f)
#define REAS_D_VAL(a)	ILL_DVAL2_REAL(a, 0.)
#define REAS_IJ_VAL(a)	ILL_IJVAL2_REAL(a, 0)
#define REAS_L_VAL(a)	ILL_LVAL2_REAL(a, FALSE)

/*
 *	symbolic FITS keyword value type:
 *	B		: Bit
 *	C		: Complex
 *	E, F	: Floating point (single precision)
 *	D, G	: Floating point (double precision)
 *	I		: Integer (16 bit)
 *	J		: Integer (32 Bit)
 *	U		: Unsigned Integer (16 bit)
 *	V		: Unsigned Integer (32 Bit)
 *	L		: Logical
 *	S		: String
 *	NC		: None (Comment keyword)
 *	NH		: None (History keyword)
 */
typedef enum { X, B, C, E, F, D, G, I, J, U, V, L, S, NC, NH } ValueTag_t;

typedef struct FitsCard_t {
	char				KeyWord[FLEN_KEYWORD];
	ValueTag_t			ValueTag;
	union {
		ETYPE	EVal;
		DTYPE	DVal;
		ITYPE	IVal;
		JTYPE	JVal;
		LTYPE	LVal;
		STYPE	SVal[FLEN_VALUE];
	}					u;
	/*
	 *	FLEN_COMMENT is defined as 73 in cfitsio.h, however,
	 *	FITSIO routine FTPCOM splits comments in fragments of 70 characters,
	 *	i.e. length of Comment field should be 70+1 = FLEN_COMMENT-2
	 */
	char				Comment[FLEN_COMMENT-2];
	struct FitsCard_t	*next;
} FitsCard_t;

#define NULLCARD		NULLPTR(FitsCard_t)
#define NULLNULLCARD	NULLPTR(FitsCard_t *)

typedef enum { PRIMARY=0, ASCIITABLE, BINTABLE } FitsHeaderType_t;

typedef struct {
	unsigned	nColumns;				/* # of columns						*/
    long		MaxRows;				/* max. # of rows					*/
	unsigned	nRows,					/* # of rows written/read			*/
				nBlockRows,				/* # of rows in one BINTABLE block	*/
				ByteWidth,				/* width of table in bytes			*/
				DataColSize[MAX_COLUMNS],
										/* width of individual column data
										   items in FITS file on disk; note
										   that in case of vector columns,
										   this field is _not_ equal to the
										   total width of the column which
										   is given by
										   DataColSize[n] * Multiplcity[n]
										   for a particular column <n>		*/
				Multiplicity[MAX_COLUMNS];
							/* multiplicity of data item, in
							   case of vector columns, e.g.
							   9J -> Multiplicty = 9	*/
        double          Tzero[MAX_COLUMNS],
                        Tscal[MAX_COLUMNS];
	char		ColNames[MAX_COLUMNS][FITS_CLEN_HDEFKWDS],
										/* vector with column names			*/
				tforms[MAX_COLUMNS][FITS_CLEN_HDEFKWDS];
										/* vector to TFORMS					*/
	ValueTag_t	TagVec[MAX_COLUMNS];	/* type vector						*/
	void		*cols[MAX_COLUMNS];		/* the value vector					*/
} Bintable_t;
#define NULLTABLE	NULLPTR(Bintable_t)

typedef struct {
	FitsHeaderType_t	type;			/* type of FITS header				*/
	int					nAxes;			/* number of axes of prim. array	*/
 	JTYPE				*AxesSizes;		/* sizes of axes					*/
	unsigned			nCards;			/* # of cards in header				*/
	unsigned			nKeyWords;		/* # of keywords in header			*/
	FitsCard_t			*first;			/* pointer to first header card		*/
	FitsCard_t			*actual;		/* pointer to actual header card	*/
} FitsHeader_t;

#define NULLHEAD	NULLPTR(FitsHeader_t)

/*
 *	"access" macros for Bintable column of simple integral type, e.g.
 *	DVEC(table, 10, 3) expands to element in row #10/column #3 of the Bintable
 *	`table' which is a double value
 */

#define VECACCESS(type, a, b, c)		\
			((type *)(a).cols[c])[b]
#define VECVECACCESS(type, a, b, c, d)	\
			((type *)(a).cols[c])[b*(a).Multiplicity[c]+d]
#define XVEC(a, b, c)	        VECACCESS(UTYPE, a, b, c) /*same as XVEC2BYTE*/
#define XVECVEC(a, b, c, d)	VECVECACCESS(BTYPE, a, b, c, d)
#define XVEC1BYTE(a, b, c)	VECACCESS(XTYPE, a, b, c)
#define XVEC2BYTE(a, b, c)	VECACCESS(UTYPE, a, b, c)
#define XVEC4BYTE(a, b, c)	VECACCESS(VTYPE, a, b, c)
#define BVEC(a, b, c)	        VECACCESS(BTYPE, a, b, c)
#define BVECVEC(a, b, c, d)	VECVECACCESS(BTYPE, a, b, c, d)
#define EVEC(a, b, c)		VECACCESS(ETYPE, a, b, c)
#define EVECVEC(a, b, c, d)	VECVECACCESS(ETYPE, a, b, c, d)
#define DVEC(a, b, c)		VECACCESS(DTYPE, a, b, c)
#define DVECVEC(a, b, c, d)	VECVECACCESS(DTYPE, a, b, c, d)
#define IVEC(a, b, c)		VECACCESS(ITYPE, a, b, c)
#define IVECVEC(a, b, c, d)	VECVECACCESS(ITYPE, a, b, c, d)
#define UVEC(a, b, c)		VECACCESS(UTYPE, a, b, c)
#define UVECVEC(a, b, c, d)	VECVECACCESS(UTYPE, a, b, c, d)
#define JVEC(a, b, c)		VECACCESS(JTYPE, a, b, c)
#define JVECVEC(a, b, c, d)	VECVECACCESS(JTYPE, a, b, c, d)
#define VVEC(a, b, c)		VECACCESS(VTYPE, a, b, c)
#define VVECVEC(a, b, c, d)	VECVECACCESS(VTYPE, a, b, c, d)
#define LVEC(a, b, c)		VECACCESS(LTYPE, a, b, c)
#define LVECVEC(a, b, c, d)	VECVECACCESS(LTYPE, a, b, c, d)
#define SVEC(a, b, c)		(((char **)(a).cols[c])[b])
#define SVASSGN(a, b, c, d)	(strncpy(SVEC(a, b, c), d, (a).Multiplicity[c]),\
							 *(SVEC(a, b, c) + (a).Multiplicity[c]) = '\0')


					/********************************/
					/*      function prototypes     */
					/********************************/

/*
 *	SYNOPSIS:
 *		OpenWriteFitsFile
 *
 *	DESCRIPTION:
 *		Creates a new FITS file with name <name> in the current directory;
 *		already existing file with the same name is overwritten;
 *
 *	PARAMETERS:
 *		I	:	name	: the name of the FITS file to create/open
 *
 *	RETURNS:
 *		OpenWriteFitsFile returns a positive file unit (to be used in
 *		subsequent calls to other functions) if the operations succeeds; if it
 *		fails, an error message is issued and the return value is negative
 *		(absolut value is the error code from the FITSIO library).
 */
extern FitsFileUnit_t OpenWriteFitsFile(char *name);
extern FitsFileUnit_t OpenFitsFile(char *name, int rwmode);

#define OpenReadFitsFile(a)			OpenFitsFile(a, FITSIO_RDONLY)
#define OpenReadWriteFitsFile(a)	        OpenFitsFile(a, FITSIO_RDWR)

/*
 *	SYNOPSIS:
 *		CloseFitsFile(FitsFileUnit_t file);
 *
 *	DESCRIPTION:
 *		Closes a FITS file previously opened with OpenWriteFitsFile.
 *
 *	PARAMETERS:
 *		I	:	file		: the unit of the FITS file to close
 *
 *	RETURNS:
 *		0 on successful completion, otherwise -1
 */
extern int			  CloseFitsFile(FitsFileUnit_t file);



/*
 *	NAME:
 *		NewPrimaryHeader
 *
 *	DESCRIPTION:
 *		Create a new FITS primary header with all mandatory keywords.
 *
 *	PARAMETERS:
 *		I	:	DataType	: type of data of primary array values
 *				nAxes		: # of data axes of the primary array
 *				AxesSizes	: the "sizes" of the data axes
 *				AxesComments: the comment fields of AXIS? keywords
 *				extend		: will the FITS file contain extensions?
 *
 *	RETURNS:
 *		A structure describing the FITS header to be used in subsequent calls
 *		to other functions.
 */
extern FitsHeader_t	  NewPrimaryHeader		(ValueTag_t DataType,
											 int nAxes,
											 const JTYPE AxesSizes[],
											 const char *AxesComments[],
											 BOOL extend);

/*
 *	NAME:
 *		EndPrimaryHeader
 *
 *	DESCRIPTION:
 *		Completes a FITS header obtained with NewPrimaryHeader.
 *
 *	PARAMETERS:
 *		 I	:	header	: a pointer to the FITS header to complete
 *
 */
extern void			  	EndPrimaryHeader	(FitsHeader_t *header);



extern int				WriteHeader			(const FitsFileUnit_t unit,
											 const FitsHeader_t *header);



extern int				WriteUpdatedHeader	(const FitsFileUnit_t unit,
											 const FitsHeader_t *header);



/*
 *	NAME:
 *		FinishPrimaryHeader
 *
 *	DESCRIPTION:
 *		Writes out a FITS primary header obtained with NewPrimaryHeader and
 *		completed with EndPrimaryHeader to a FITS file.
 *
 *	PARAMETERS:
 *		I	: 	unit	: the FITS file unit obtained from OpenWriteFitsFile
 *				header	: pointer to the FITS header to write
 */
extern void			  FinishPrimaryHeader	(FitsFileUnit_t unit,
											 const FitsHeader_t *header);



/*
 *	NAME:
 *		NewBintableHeader
 *
 *	DESCRIPTION:
 *		Creates a new FITS Bintable header with all mandatory keywords.
 *
 *	PARAMETERS:
 *		 I	: 	MaxBlockRows	: the number of rows in a Bintable row block;
 *				table			: a pointer to a Bintable_t data structure;
 *
 *	RETURNS:
 *		A structure describing the FITS header to be used in subsequent calls
 *		to other functions.
 */
extern FitsHeader_t	  NewBintableHeader		(const unsigned MaxBlockRows,
											 Bintable_t *table);



/*
 *	NAME:
 *		EndBintableHeader
 *
 *	DESCRIPTION:
 *		Completes a FITS header obtained with NewBintableHeader.
 * 
 *	PARAMETERS:
 *		 I/O	: 	header	: a pointer to the FITS header to complete
 */
extern void			  EndBintableHeader		(FitsHeader_t *header,
											 const Bintable_t *table);



/*
 *	SYNOPSIS:
 *		GetBintableStructure(FitsHeader_t *header, Bintable_t *table,
 *							 const int MaxBlockRows, const unsigned nColumns,
 *							 const unsigned ActCols[])
 *
 *	DESCRIPTION:
 *		Function to retrieve information about the column structure of
 *		a Bintable from an existing FITS file.
 *
 *	PARAMETERS:
 *		I	:	header			: pointer to FITS header corresponding to
 *								  Bintable
 *				MaxBlockRows	: number of rows in a block;
 *									<0 : allocate (via malloc) that many rows
 *										 for each column that the Bintable
 *										 consists of
 *									=0 : do not allocate any space for column
 *										 data
 *									>0 : allocate space to store <MaxBlockRows>
 *										 data items in each column
 *				nColumns		: the number of columns for which space is to
 *								  be allocated via "malloc"; <nColumns> = 0
 *								  signifies that all columns should be
 *								  considered
 *				ActCols[]		: a vector with column indices; only for the
 *								  columns ActCols[0] ... ActCols[nColumns]
 *								  space will be allocated; unused for
 *								  <nColumns> = 0
 *		 O	:	*table			: structure describing the current Bintable; if
 *								  <MaxBlockRows> != 0, the vector table->cols
 *								  contains pointers to store column data
 */
extern void GetBintableStructure	(FitsHeader_t *header,
									 Bintable_t *table,
									 const int MaxBlockRows,
									 const unsigned nColumns,
									 const unsigned ActCols[]);



/*-----------------------------------------------------------------------------
 *	NAME:
 *		FinishBintableHeader
 *
 *	DESCRIPTION:
 *
 *	Writes out a FITS Bintable header obtained with NewBintableHeader and
 *	completed with EndBintableHeader to a FITS file.
 *
 *	PARAMETERS:
 *
 *	 I	: 	unit	: the FITS file unit obtained from OpenWriteFitsFile
 *			header	: pointer to the FITS header to write
 *	 O	: <none> 
 *	 I/O: <none>
 *---------------------------------------------------------------------------*/
extern void			  FinishBintableHeader	(const FitsFileUnit_t unit,
											 FitsHeader_t *header,
											 Bintable_t *table);



/*
 *	NAME:
 *		RetrieveFitsHeader
 *
 *	DESCRIPTION:
 *
 *	Reads a FITS header from a FITS file into memory for subsequent
 *	manipulation, i.e. adding, deleting and/or modifying card images.
 *	Note that in case of a Bintable header, no memory is allocated for
 *	storing table column information.
 *
 *	PARAMETERS:
 *
 *	 I	: 	unit	: the FITS file unit as obtained from OpenWriteFitsFile;
 *			header	: pointer to the header structure to be filled
 *	 O	:	*header	: structure is filled with information as obtained from
 *					  FITS file
 */
extern FitsHeader_t		RetrieveFitsHeader	(FitsFileUnit_t unit);



/*
 *	NAME:
 *		AddCard
 *
 *	DESCRIPTION:
 *		Adds a card image (keyword - value - comment) to a FITS header.
 *
 *	PARAMETERS:
 *		I	:	header	:	pointer to FITS header
 *				KeyWord	:	the name of the keyword to add
 *				vtag	:	a tag describing the data type of the keyword
 *				value	:	a pointer to the value of the keyword
 *				comment :	comment field for that card image
 */
extern void			  AddCard				(FitsHeader_t *header,
											 const char *KeyWord,
							  				 const ValueTag_t vtag,
											 const void *value,
											 const char *comment);

extern void			  InsertCard			(FitsHeader_t *header,
											 const char *PosKeyWord,
											 const char *KeyWord,
											 const ValueTag_t vtag,
											 const void *value,
											 const char *comment);

extern void			  MoveCards				(FitsHeader_t *header,
											 const char *StartKeyWord,
											 const char *EndKeyWord,
											 const char *MoveKeyWord);

/*
 *	NAME:
 *		AddComment/AddHistory
 *
 *	DESCRIPTION:
 *		Add a comment/history card image to a FITS header.
 *
 *	PARAMETERS:
 *		I	:	header	: pointer to the header structure
 *				Comment	: the comment to add
 */
extern void				AddComment			(FitsHeader_t *header,
											 const char *Comment);
extern void				AddHistory			(FitsHeader_t *header,
											 const char *History);

	/* possible values for argument <present> of function AddColumn	*/
#define TNONE			(0)				/* nothing present				*/
#define TUNIT			(1)				/* flag: TUNIT field present	*/
#define TMIN			(1<<2)			/* flag: TLMIN field present	*/
#define TMAX			(1<<3)			/* flag: TLMAX field present	*/
#define TSCAL			(1<<4)			/* flag: TSCALE field present	*/
#define TNULL			(1<<5)			/* flag: TNULL field present	*/
#define TZERO			(1<<6)			/* flag: TZERO field present	*/
#define TMMS			TMIN|TMAX|TSCAL
#define TALL			TUNIT|TMMS

/* 
   
   For each argument <present> in function AddColumn 
   call shall be specified following fields in this
   order:
   
   TUNIT          value         comment
   TMIN           value
   TMAX           value
   TSCAL   (DTYPE)value         comment
   TNULL   (JTYPE)value
   TZERO   (DTYPE)value         comment

   'value' should be always present and 'comment'
   could be "" .

*/


/*
 *	NAME:
 *		AddColumn
 *
 *	DESCRIPTION:
 *		Add a column to a Bintable.
 *
 *	PARAMETERS:
 *		I	: header		: pointer to FITS header
 *			  table			: pointer to Bintable
 *			  ttype			: mandatory TTYPE field for column definition
 *			  TypeComment	: comment for TTYPE field
 *			  tform			: mandatory TFORM field for column definition
 *			  present		: tag field to specify the optional arguments that
 *							  follow; for a list of allowed values, see #define
 *							  section above
 */
extern void			  AddColumn				(FitsHeader_t *header,
											 Bintable_t *table,
											 const char *ttype, 
											 const char *TypeComment,
											 const char *tform,
											 const unsigned present,
											 ...);



/*
 *	NAME:
 *		ChangeColumn
 *
 *	DESCRIPTION:
 *		Change type of column in a Bintable.
 *
 *	PARAMETERS:
 *		I	: header		: pointer to FITS header
 *			  table			: pointer to Bintable
 *			  ttype			: mandatory TTYPE field for column definition
 *			  TypeComment	: comment for TTYPE field
 *			  tform			: mandatory TFORM field for column definition
 *			  present		: tag field to specify the optional arguments that
 *							  follow; for a list of allowed values, see #define
 *							  section above
 */
extern void			  ChangeColumn			(const unsigned nCol,
											 FitsHeader_t *header,
											 Bintable_t *table,
											 const char *ttype, 
											 const char *TypeComment,
											 const char *tform,
											 const unsigned present,
											 ...);



/*
 *	NAME:
 *		DeleteCard
 *
 *	DESCRIPTION:
 *
 *	Delete a card image from a FITS header.
 *
 *	PARAMETERS:
 *		I	: header	: pointer to FITS header
 *			  KeyWord	: keyword of card image to delete
 */
extern void			  DeleteCard			(FitsHeader_t *header,
											 const char *KeyWord);



/*
 *	NAME:
 *		DeleteComment
 *
 *	DESCRIPTION:
 *		Delete an comment card image from FITS header.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to FITS header
 *						: keyword of card image preceding the comment card
 *						  to delete
 */
extern void			  DeleteComment			(FitsHeader_t *header,
											 const char *KeyWord);



/*
 *	SYNOPSIS:
 *		BOOL ExistsKeyWord(const FitsHeader_t *header, const char *KeyWord,
 *						   FitsCard_t **card);
 *
 *	DESCRIPTION:
 *		Tests if the keyword <KeyWord> exists in the FITS header pointed at by
 *		<header>.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header to inspect
 *			  KeyWord	: the keyword to look for
 *		O	: *card		: pointer to FITS card containing searched keyword or
 *						  NULL if keyword was not found in current header
 *
 *	RETURNS:
 *		TRUE if keyword was found, FALSE otherwise.
 */
extern BOOL			ExistsKeyWord			(const FitsHeader_t *header,
											 const char *KeyWord,
											 FitsCard_t **card);



/*
 *	NAME:
 *		GetLVal
 *
 *	DESCRIPTION:
 *		Get a boolean value from a FITS header card image.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: the keyword whose value to retrieve
 *
 *	RETURNS:
 *		The value of <KeyWord> - if the keyword is not found in the header an
 *		error message is issued and FALSE is returned
 */	
extern LTYPE		  GetLVal				(const FitsHeader_t *header,
											 const char *KeyWord);



/*
 *	NAME:
 *		GetSVal
 *
 *	DESCRIPTION:
 *		Get a string value from a FITS header card image.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: the keyword whose value to retrieve
 *
 *	RETURNS:
 *		A pointer to the value of keyword <KeyWord>. If no card image with
 *		keyword <KeyWord> is found in header an error message is issued an
 *		NULL pointer is returned.
 *	
 */
extern SPTYPE		  GetSVal				(const FitsHeader_t *header,
											 const char *KeyWord);



/*
 *	NAME:
 *		GetJVal
 *
 *	DESCRIPTION:
 *		Get an integer value from a FITS header card image.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: the keyword whose value to retrieve
 *
 *	RETURNS:
 *		The value of keyword <KeyWord>. If no card image with keyword <KeyWord>
 *		is found in header an error message is issued and 0 is returned.
 *
 */
extern JTYPE		  GetJVal				(const FitsHeader_t *header,
											 const char *KeyWord);



/*
 *	NAME:
 *		GetDVal
 *
 *	DESCRIPTION:
 *		Get a double value from a FITS header card image.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: the keyword whose value to retrieve
 *	RETURNS:
 *		The value of keyword <KeyWord>. If no card image with keyword <KeyWord>
 *		is found in header an error message is issued and 0. is returned.
 *
 */
extern DTYPE		  GetDVal				(const FitsHeader_t *header,
											 const char *KeyWord);



/*
 *	NAME:
 *		GetComment
 *
 *	DESCRIPTION:
 *		Retrieve the comment field of a FITS card image.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: keyword of card image whose comment field is
 *						  requested
 *
 *	RETURNS:
 *		A pointer to the comment field of card image with keyword <KeyWord>.
 *		If the FITS header does not contain an card image with keyword
 *		<KeyWord> a NULL pointer is returned.
 *
 */
extern SPTYPE		  GetComment			(const FitsHeader_t *header,
											 const char *KeyWord);



/*
 *	NAME:
 *		SetLVal
 *
 *	DESCRIPTION:
 *		Sets the value of a boolean keyword.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: keyword whose value is to be altered
 *			  value		: new value for keyword <KeyWord>
 */
extern void			  SetLVal				(const FitsHeader_t *header,
											 const char *KeyWord,
											 const LTYPE value);



/*
 *	NAME:
 *		SetSVal
 *
 *	DESCRIPTION:
 *		Sets the value of a string keyword.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: keyword whose value is to be altered
 *			  value		: new value for keyword <KeyWord>
 */
extern void			  SetSVal				(const FitsHeader_t *header,
											 const char *KeyWord,
											 const SPTYPE value);



/*
 *	NAME:
 *		SetJVal
 *
 *	DESCRIPTION:
 *		Sets the value of an integer keyword.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: keyword whose value is to be altered
 *			  value		: new value for keyword <KeyWord>
 */
extern void			  SetJVal				(const FitsHeader_t *header,
											 const char *KeyWord,
											 const JTYPE value);



/*
 *	NAME:
 *		SetDVal
 *
 *	DESCRIPTION:
 *		Sets the value of a double keyword.
 *
 *	PARAMETERS:
 *		I	: header	: a pointer to the FITS header
 *			  KeyWord	: keyword whose value is to be altered
 *			  value		: new value for keyword <KeyWord>
 */
extern void			  SetDVal				(const FitsHeader_t *header,
											 const char *KeyWord,
											 const DTYPE value);



/*
 *	NAME:
 *		SetComment
 *
 *	DESCRIPTION:
 *		Set the comment field of a FITS header card image.
 *
 *	PARAMETERS:
 *		I	: header		: a pointer to the FITS header
 *			  KeyWord		: keyword of card image whose comment field is to
 *							  be altered
 *			  NewComment	: new value for comment field
 */
extern void			  SetComment			(const FitsHeader_t *header,
											 const char *KeyWord,
											 const char *NewComment);



/*-----------------------------------------------------------------------------
 *	SYNOPSIS:
 *		int Write[Fast]Bintable(FitsFileUnit_t unit, Bintable_t *table,
 *								unsigned nRows, BOOL last);
 *
 *	DESCRIPTION:
 *		These two functions do exactly the same, namely, write a block
 *		of complete FITS Bintable rows which are kept in memory to disk.
 *		The last boolean argument indicates whether the current row block
 *		is the last one, i.e., the one that completes the Bintable.
 *		The proper usage of `WriteBintable' is exemplified in the following
 *		code fragment:
 *
 *			FitsFileUnit_t	unit;
 *			Bintable_t		table;
 *
 *			...
 *			nRows = 1000		    // # of rows in a row block
 *			for (i=0; i<nBlocks; ++i) {
 *				FillRows(table, i, nRows);
 *									// occupy the rows i*nRows + 1 to
 *									//   (i+1)*nRows in memory
 *				WriteBintable(unit, table, nRows, FALSE);
 *			}
 *			WriteBintable(unit, table, 0, TRUE);
 *
 *	    In this example, the first call to `WriteBintable' writes the
 *		rows 1 - 1000 which have been occupied in the preceding call
 *		to `FillRows' (or whatever). Every time `WriteBintable' is called,
 *		an internal	row counter is advanced keeping track of what
 *	    portion of the Bintable has been dumped to disk already. This
 *		means that the second call to `WriteBintable' will actually write
 *		the rows 1001 - 2000, the third the rows 2001 - 3000, etc.
 *		The very last call must be made with `last=TRUE'. This then
 *		triggers that the Bintable on disk is completed and the
 *		total numbers of rows, i.e., the value of the NAXIS1 keywords,
 *		gets calculated.
 *
 *		`WriteFastBintable' is an incarnation, of `WriteBintable' which
 *		proves to be _MUCH_ faster due to the usage of a different interface
 *		to the underlying FITSIO library. However, use it with caution!
 *              The value written in the outut file from `WriteFastBintable'
 *              is: (<colval> - tzero)/tscal  (FT 15/10/2002)
 *
 *	PARAMETERS:
 *		I	:	unit	: file unit of FITS file to write to
 *				table	: pointer to Bintable containing data
 *				nRows	: number of rows to write; must be less or equal to
 *						  the <MaxBlockRows> value specified in a previous call
 *						  to `NewBintableHeader'
 *				last	: boolean value specifiying whether this is the last
 *						  call to `Write[Fast]Bintable' (so all data have been
 *						  written)
 *
 *	RETURNS:
 *		depending on last = F/T: the number of rows successfully written in
 *								 this call/total number of Bintable rows
 *								 written
 *---------------------------------------------------------------------------*/
extern int			WriteBintable			(FitsFileUnit_t unit,
											 Bintable_t *table,
											 unsigned nRows,
											 BOOL last);

extern int			WriteFastBintable		(FitsFileUnit_t unit,
											 Bintable_t *table,
											 unsigned nRows,
											 BOOL last);


/*
 *	NAME:
 *		ReadBintable
 *
 *	DESCRIPTION:
 *		Read rows of data from a FITS Bintable extension.
 *
 *	PARAMETERS:
 *		I	: unit		: unit of FITS file to read from
 *			  ActCols	: vector containing the column numbers that shall be
 *						  read (starting index: 0)
 *			  nColumns	: number of colunms to read; the actual column numbers
 *						  are given in vector <ActColumns>; a value of 0
 *						  signifies that all columns shall be read - contents
 *						  of <ActColumns> is unused in this case
 *			  FromRow	: start reading from row #<FromRow>, NB: first row in
 *						  Bintable is row #1 not #0!	
 *
 *		O	: table		: on exit, the data have been read into data area
 *						  pointed to by table
 *
 *		I/O : nRows		: on entry, <*nRows> specifies the number of rows to
 *						  read from the Bintable; on exit, it contains the
 *						  actual number of lines read which may be less than
 *						  the requested number
 *
 *	RETURNS:
 *		Upon successful completion 0 is returned; -1 signals EOF condition
 *		occured during reading.
 */
extern int			  ReadBintable			(const FitsFileUnit_t unit,
											 Bintable_t *table,
											 const unsigned nColumns,
											 const unsigned ActCols[],
											 const unsigned FromRow,
											 unsigned *nRows);


extern int			  ReadBintableWithNULL			(const FitsFileUnit_t unit,
											 Bintable_t *table,
											 const unsigned nColumns,
											 const unsigned ActCols[],
											 const unsigned FromRow,
											 unsigned *nRows);



extern void			  WriteComment			(const FitsFileUnit_t unit,
											 const char *Comment);



extern void			  WriteHistory			(const FitsFileUnit_t unit,
											 const char *History);



/*
 *	NAME:
 *		WriteWholeBintable
 *
 *	DESCRIPTION:
 *		Write the data section of a Bintable as a whole.
 *
 *	PARAMETERS:
 *		I	: unit	: unit of FITS file to write to
 *			  table	: pointer to Bintable data area
 */
extern void				WriteWholeBintable	(FitsFileUnit_t unit,
											 Bintable_t *table);



/*
 *	NAME:
 *		KeyWordMatch
 *
 *	DESCRIPTION:
 *		Checks if a FITS header keyword has a certain value.
 *
 *	PARAMETERS:
 *		I	: unit			: unit of FITS file to read from
 *			  KeyWord 		: name of keyword to check
 *			  vtag			: value tag of keyword <KeyWord>
 *			  NominalValue	: pointer to variable holding the nominal value
 *							  of keyword <KeyWord>
 *		O	: ActValue		: the actual value of keyword <KeyWord>
 *
 *	RETURNS:
 *		1 if value of keyword <KeyWord> coincides with <NominalValue>, 0
 *		otherwise or if <KeyWord> is not contained in current FITS file
 *		extension.
 */
extern BOOL			  KeyWordMatch			(FitsFileUnit_t unit,
											 const char *KeyWord,
							  				 ValueTag_t vtag,
											 const void *NominalValue,
											 void *ActValue);



/*
 *	NAME:
 *		ColNameMatch
 *
 *	DESCRIPTION:
 *		Checks if Bintable contains column with certain name
 *
 *	PARAMETERS:
 *		I	: ColName		: check if column <ColName> exists
 *			  table 		: pointer to Bintable that is to be checked
 *							  for existence of column <ColName>
 *
 *	RETURNS:
 *		Index of column name <ColName> (starting from 0) or -1 if a column
 *		with name <ColName> does not exist.
 */
extern int			  ColNameMatch			(const char *ColName,
											 const Bintable_t *table);



extern void			  RowCp					(Bintable_t *table,
											 const unsigned ToRow,
											 const unsigned FromRow);

extern int                        CopyRows                      (Bintable_t *table, const unsigned ToRow, 
								 const unsigned FromRow);

extern void			  RowIllegal			(Bintable_t *table,
											 const unsigned RowNr);

extern void			  ElimIllegalFRows		(FTYPE Vec[], unsigned nRows,
											 unsigned *NewRows);

extern int			  GetColNameIndx		(const Bintable_t *table,
											 const char *ColName);

extern int			  ColIndices			(const Bintable_t *table,
											 unsigned n,
											 ...);

extern void			  **ReadImage			(FitsFileUnit_t unit,
											 ValueTag_t type,
											 int NullValue,
											 unsigned xmin,
											 unsigned xmax,
											 unsigned ymin,
											 unsigned ymax);

extern unsigned		  ReleaseImage			(void **Image,
											 ValueTag_t type,
											 unsigned xmin,
											 unsigned xmax,
											 unsigned ymin,
											 unsigned ymax);

extern unsigned		  ReleaseBintable		(FitsHeader_t *head,
											 Bintable_t	  *table);

extern FitsHeader_t	  *EmptyHeader			(void);

/*
 *	SYNOPSIS:
 *		ChecksumCalc(FitsFileUnit_t unit);
 *
 *	DESCRIPTION:
 *              Routine to calculate checksum and datasum and insert it
 *              in file.
 *		
 *	PARAMETERS:
 *		I	:	unit		: the unit of the FITS file to close
 *
 *	RETURNS:
 *		0 on successful completion, otherwise 1
 */
extern int ChecksumCalc(FitsFileUnit_t unit);
/*
 *	SYNOPSIS:
 *	extern void ManageDSKeywords(FitsHeader_t *header, const char *Typ, const char *Form,const char *value);
 *
 *	DESCRIPTION:
 *              Routine to write or update DS keywords
 *              
 *		
 *	PARAMETERS:
 *	
 *
 *	RETURNS:
 *	
 */





extern void ManageDSKeywords(FitsHeader_t *header, const char *Typ, const char *Form,const char *value);
extern void ManageDMKeywords(FitsHeader_t *header, const char *Typ, const char *Form,const char *comment);
extern int CopyHdu(FitsFileUnit_t evunit, FitsFileUnit_t outunit, int hducount);

extern int GetAttitudeATTFLAG(char* attfile, char* attflag);



#endif
