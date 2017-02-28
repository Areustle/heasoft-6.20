/*
 *	highfits.c: set of high-level interface routines to the CFITSIO library
 *
 *	DESCRIPTION:
 *
 *	CHANGE HISTORY:
 *		FT, 01/02/02: 0.1.0: - working version from SAX (Uwe Lammer)
 *              FT, 01/02/02: 0.1.1: - added column Bit Type handle (8/16/32X)
 *                                     WriteFastBintable
 *                                   - Little change OpenWriteFitsFile
 *                                   - Added  ChecksumCalc
 *                                   - Do not add default extension in 
 *                                     OpenWriteFitsFile function 
 *              BS, 08/07/02: 0.2.0: - Changed calls to fortran wrapper
 *                                     with direct call to cfitsio functions
 *                                     and substituted local variables with
 *                                     common defines from cfitsio.h.
 *                                   - minor change into CloseFitsFile set 
 *                                     FitsFileUnit_t file pointer to 0 before
 *                                     'return'.
 *              BS, 20/09/02: 0.2.1: - Added column unsigned 1 byte  integer 
 *                                     Type handle
 *              FT, 15/10/02: 0.3.0: - Added TZERO and TSCAL handle routines
 *                                     affected: 'WriteFastBintable', 
 *                                     'RetrieveFitsHeader', 
 *                                     'FinishBintableHeader', 
 *                                     'WriteUpdatedHeader',
 *                                     'WriteHeader',
 *                                     'GetBintableStructure',
 *                                     'ChangeColumn',
 *                                     'AddColumn'
 *                                   - 'highfits.h' added 'Tzero[MAX_COLUMNS]'
 *                                     and 'Tscal[MAX_COLUMNS]' to 'Bintable_t'
 *                                     structure. New macros: BVEC, BVECVEC,
 *                                     TZERO 
 *                                   - 'WriteFastBintable' minor change 
 *                                     for empty tables handle
 *                                   - Bug Fixed "AddCommHist"               
 *              FT, 20/01/02: 0.3.1: - Extended handle of Bit Type Column of 
 *                                     all X multiplicity (nX)
 *              FT, 27/01/02: 0.3.2: - Extended ReadBintable to read unsigned
 *                                     short integer and unsigned integer
 *                                     columns
 *              FT, 06/05/03: 0.3.3: - 'WriteFastBintable' Solaris Bug fixed
 *                                     (X type columns swap)
 *
 *              BS, 26/05/03: 0.3.4: - 'AddColumn" and 'ChangeColumn' added handle
 *                                     of 'TNULL' value for column of integer data types I, 
 *                                     and J
 *                  02/07/03: 0.3.5: - Added handling of B column in 'RowCp' routine
 *                  04/07/03: 0.3.6: - Added 'CopyRows routine to use instead of 'RowCp' routine
 *                  23/10/03: 0.3.7  - Added 'ManageDSKeywords' routine
 *                  22/03/04: 0.3.8  - Added 'CopyHdu' routine
 *                  21/04/04: 0.3.9  - Modified "AddCard" to update keyword if it exists 
 *                  06/05/04: 0.3.10 - Added 'ManageDMKeywords' routine\
 *                  18/05/04: 0.3.11 - Messages displayed revision 
 *              PM, 21/05/04: 0.3.12 - Change case J and I in WriteFastBintable for unsigned column
 *              BS, 21/05/04: 0.3.13 - Change in 'WritefastBintable' case J : 'PutWord' with 'PutHalfWord'
 *              CS, 09/07/04: 0.3.14 - Minor changes (eliminate warning)
 *              BS, 09/07/04: 0.3.15 - Deleted 'Cffempty' dummy routine calls 
 *                                     to avoid compiler warning
 *              BS, 13/07/05: 0.3.16 - Added ReadBintableWithNULL routine to perform
 *                                     check on undefined value
 *              NS, 16/01/14: 0.3.17 - Added GetAttitudeATTFLAG routine
 *              NS, 17/02/14: 0.3.18 - Modified GetAttitudeATTFLAG routine
 *
 *     
 *
 *                                     
 *
 *	AUTHOR:
 *		ISAC - Italian Swift Archive Center
 */	

#define HIGHFITS_C
#define HIGHFITS_VERSION		0.3.18

					/********************************/
					/*        header files          */
					/********************************/

#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "basic.h"
#include "highfits.h"
#include "lowlevel.h"
#include "misc.h"
#include "xrt_termio.h"
#include "headas_utils.h"
#include "headas_stdio.h"
#include "xrtdefs.h"
					/********************************/
	       				/*           globals            */
	       				/********************************/

#define FITSEXT		  .fits	      /* d/f extension for FITS files  	  */
#define EDECIMALS	  8           /* # of decimals for E type data	  */
#define DDECIMALS	  13          /* # of decimals for D/G type data  */

#define FITSERR(a, b)					      	\
		if (status) {					\
				char errtxt[FLEN_STATUS];	\
				ffgerr(status, errtxt);		\
				Error(CSTR(a), "FITSIO routine " CSTR(b)       \
				  " returned error #%d: %s", status, errtxt);  \
				status = 0;		       		       \
			    }

	  #define SWAPBYTES(a,b) \
		for(j=0;j<(a)->Multiplicity[b]/2;++j){ \
		  index = (unsigned )((a)->Multiplicity[b]-1-j);    \
		  if ( j !=  index ) {                     \
		    tmpbuff = ((char *)(a)->cols[b])[i*(a)->Multiplicity[b]+j];    \
		    ((char *)(a)->cols[b])[i*(a)->Multiplicity[b]+j] =             \
		      ((char *)(a)->cols[b])[i*(a)->Multiplicity[b]+index];        \
		    ((char *)(a)->cols[b])[i*(a)->Multiplicity[b]+index] = tmpbuff;\
		  } }

					/********************************/
					/*          functions           */
					/********************************/

static FitsCard_t *EmptyCard(void)
{
	FitsCard_t *card;

	if (!(card = TMALLOC(FitsCard_t))) {
		Error("EmptyCard", "Unable to allocate memory for new card.");
		PrgExit(MALLOC_ERR);
	}
	card->next = (FitsCard_t *)NULL;

	return card;
} /* EmptyCard */



FitsHeader_t *EmptyHeader(void)
{
	FitsHeader_t *header;

	if (!(header = TMALLOC(FitsHeader_t))) {
		Error("EmptyHeader", "Unable to allocate memory for new header.");
		PrgExit(MALLOC_ERR);
	}

	return header;
} /* EmptyHeader */



static void SetColumnIllegal(void *ColPtr, ValueTag_t tag, unsigned nRows,
							 unsigned ElementSize)
{
	int	c;

	if (tag == L || tag == S)
		c = L_NULL_VAL;
	else
		c = IJ_NULL_VAL;

	if (tag == S) {
		unsigned n;
		char **p = (char **)ColPtr;
		for (n=0; n<nRows; *p[n++] = '\0');
	} else
	memset(ColPtr, c, nRows * ElementSize);
} /* SetColumnIllegal */



static const char *TformScan(const char *tform, unsigned *AllocSize,
							 ValueTag_t *tag, unsigned *FieldSize,
							 unsigned *nElem)
{
	static const char		FieldStr[] = "XBLIJUVAED";
	static const ValueTag_t	TagVec[]   =     { X, B, L, I, J, U, V, S, E, D};
	static const unsigned	FITSFieldLen[] = { 1, 1, 1, 2, 4, 2, 4, 1, 4, 8};
	static const unsigned	MemFieldLen[]  = { 
	                                           sizeof(XTYPE),
						   sizeof(BTYPE),
						   sizeof(BTYPE),
						   /* LOGICAL coded as single
						    * character ('T'/'F') in
						    * FITS file
						    */ 
						   sizeof(ITYPE),
						   sizeof(JTYPE),
						   sizeof(UTYPE),
						   sizeof(VTYPE),
						   sizeof(STYPE),
						   sizeof(ETYPE),
						   sizeof(DTYPE)
                            };

	static const char		*comment[] = {
	                                               "Array of BIT",
						       "Unsigned 1-byte INTEGER",
	                                               "LOGICAL", 
						       "2-byte INTEGER", 
						       "4-byte INTEGER",
						       "2-byte UNSIGNSD INTEGER", 
						       "4-byte UNSIGNED INTEGER",
						       "ASCII character", 
						       "REAL", 
						       "DOUBLE PRECISION",
	                                };
	const char				*pos, *tmp = tform;
	unsigned				index;

	*nElem = isdigit(*tmp) ? *tmp++ -'0' : 1;
	while (isdigit(*tmp))
		*nElem = 10* *nElem + *tmp++ -'0';
	if ((pos=strchr(FieldStr, *tmp))) {
		index		= pos-FieldStr;
		*tag		= TagVec[index];
		*FieldSize	= FITSFieldLen[index];
		*AllocSize	= *nElem*MemFieldLen[index];
		if (*tag == S)
			/*
			 *	in case of string column, add 1 to AllocSize to account for
			 *	'\0' at the end
			 */
			++*AllocSize;
		else if (*tag == L)
			/*
			 *	logicals are stored as LTYPE
			 */
		        *AllocSize = *nElem * sizeof(LTYPE);
		else if (*tag == B)
		  *AllocSize = *nElem * sizeof(BTYPE);
		     
		else if (*tag == X) {
			/*
			 *	bits are stored as BYTES
			 */
		        *nElem = (int)((*nElem-1)/8) + 1;
			*AllocSize = *nElem * sizeof(XTYPE);
		}
		return comment[index];
	} else {
		Error("TformScan", 
		      "`%c' is an illegal type field identifier", *pos);
		PrgExit(ILL_ERR);
	}

	return 0;
} /* TformScan */



static FitsCard_t *LookupCard(const FitsHeader_t *header, const char *KeyWord,
							  FitsCard_t **PredCard)
{
	FitsCard_t *card, *pred;

	pred = (FitsCard_t *)NULL;

	for(card=header->first; card; pred=card, card=card->next)
	  if (STREQ(card->KeyWord, KeyWord)) {
	    if (PredCard)
	      *PredCard = pred;
	    return card;
	  }

	return (FitsCard_t *)NULL;
} /* LookupCard */



BOOL ExistsKeyWord(const FitsHeader_t *header, const char *KeyWord,
				   FitsCard_t **card)
{
	FitsCard_t	*c;

	c = LookupCard(header, KeyWord, NULL);
	if (card)
		*card = c;

	return c != (FitsCard_t *)NULL;
} /* ExistsKeyWord */



LTYPE GetLVal(const FitsHeader_t *header, const char *KeyWord)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL)))
		if (card->ValueTag == L)
			return card->u.LVal;
		else
			Error("GetLVal", "Value of Keyword `%s' is not of type `logical'",
				  KeyWord);
	else
		Error("GetLVal", "Keyword `%s' not contained in header", KeyWord);

	return (LTYPE)FALSE;
} /* GetLVal */



SPTYPE GetSVal(const FitsHeader_t *header, const char *KeyWord)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL)))
		if (card->ValueTag == S)
			return card->u.SVal;
		else
			Error("GetSVal", "Value of Keyword `%s' is not of type `string'",
				  KeyWord);
	else
		Error("GetSVal", "Keyword `%s' not contained in header", KeyWord);

	return (SPTYPE)0;
} /* GetSVal */



JTYPE GetJVal(const FitsHeader_t *header, const char *KeyWord)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL)))
		if (card->ValueTag == J)
			return card->u.JVal;
		else
			Error("GetJVal", "Value of Keyword `%s' is not of type `integer'",
				  KeyWord);
	else
		Error("GetJVal", "Keyword `%s' not contained in header", KeyWord);

	return (JTYPE)0;
} /* GetJVal */



DTYPE GetDVal(const FitsHeader_t *header, const char *KeyWord)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL))) {
		if (card->ValueTag == D || card->ValueTag == G)
			return card->u.DVal;
		else if (card->ValueTag == E || card->ValueTag == F)
			return (DTYPE)card->u.EVal;
		else
			Error("GetDVal", "Value of Keyword `%s' is not of type `floating ", KeyWord);
	} else
		Error("GetDVal", "Keyword `%s' not contained in header", KeyWord);

	return (DTYPE)0;
} /* GetDVal */



SPTYPE GetComment(const FitsHeader_t *header, const char *KeyWord)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL)))
		return card->Comment;
	else
		Error("GetComment", "Keyword `%s' not contained in header", KeyWord);

	return (SPTYPE)0;
} /* GetComment */



void SetLVal(const FitsHeader_t *header, const char *KeyWord,
			 const LTYPE value)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL))) {
		card->ValueTag = L;
		card->u.LVal = value;
	} else
		Error("SetLVal", "Keyword `%s' not contained in header", KeyWord);
} /* SetLVal */



void SetSVal(const FitsHeader_t *header, const char *KeyWord,
			 const SPTYPE value)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL))) {
		card->ValueTag = S;
		STRNCPY(card->u.SVal, value);
	} else
		Error("SetSVal", "Keyword `%s' not contained in header", KeyWord);
} /* SetSVal */



void SetJVal(const FitsHeader_t *header, const char *KeyWord,
			 const JTYPE value)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL))) {
		card->ValueTag = J;
		card->u.JVal = value;
	} else
		Error("SetJVal", "Keyword `%s' not contained in header", KeyWord);
} /* SetJVal */



void SetDVal(const FitsHeader_t *header, const char *KeyWord,
			 const DTYPE value)
{
	FitsCard_t	*card;

	if ((card = LookupCard(header, KeyWord, NULL))) {
		card->ValueTag = D;
		card->u.DVal = value;
	} else
		Error("SetDVal", "Keyword `%s' not contained in header", KeyWord);
} /* SetDVal */



void SetComment(const FitsHeader_t *header, const char *KeyWord,
				const char *NewComment)
{
	FitsCard_t	*card;

	if (!NewComment)
		return;
	if ((card = LookupCard(header, KeyWord, NULL)))
		STRNCPY(card->Comment, *NewComment ? NewComment : " ");
	else
		Error("SetComment", "Keyword `%s' not contained in header", KeyWord);
} /* SetComment */



FitsFileUnit_t OpenWriteFitsFile(char *name)
{
	char		tmp[MAXFNAME_LEN+1];
	FitsFileUnit_t unit;
	int			success;
	Status_t	status;

	STRNCPY(tmp, name);
	/*
	if (!(DotPos = strrchr(tmp, '.')) )
		strncat(STRNCPY(tmp, name), CSTR(FITSEXT), MAXFNAME_LEN-strlen(name));
	*/
	remove(tmp);
	status = 0;
	ffinit(&unit, tmp,&status);
	success = status == 0;

	FITSERR(OpenWriteFitsFile, ffinit);

	return success ? unit : (FitsFileUnit_t)0;
} /* OpenWriteFitsFile */



FitsFileUnit_t OpenFitsFile(char *name, int rwmode)
{
	FitsFileUnit_t unit;
	int			success;
	Status_t	status;

	status = 0;
	ffopen(&unit, name, rwmode, &status);
	success = status == 0;
	FITSERR(OpenFitsFile, ffopen);

	return success ? unit : (FitsFileUnit_t)0;
} /* OpenFitsFile */


	
int CloseFitsFile(FitsFileUnit_t file)
{
	Status_t	status;

	status = 0;
	/*
	 *	close file
	 */
	ffclos(file, &status);
	FITSERR(CloseFitsFile, ffclos);

	/*
	 *	free unit number
	 */
	file = (FitsFileUnit_t)0;
	return 0;
} /* CloseFitsFile */



void InsertCard(FitsHeader_t *header, const char *PosKeyWord,
				const char *KeyWord, const ValueTag_t vtag,
				const void *valuep, const char *comment)
{
	FitsCard_t	*tmp, *card = EmptyCard();

	tmp = PosKeyWord ? LookupCard(header, PosKeyWord, NULL) : header->actual;
	if (tmp) {
		card->next = tmp->next;
		tmp->next = card;
		if (tmp == header->actual)
			header->actual = card;
	} else
		/*
		 *	positional keyword not in header or this is the first entry
		 */
		if (!PosKeyWord)
			header->first = header->actual = card;
		else {
			free(card);
			Error("InsertCard", "Keyword `%s' not contained in header",
				  PosKeyWord);
			return;
		}

	++header->nCards;
	++header->nKeyWords;
	STRNCPY(card->KeyWord, KeyWord);
	card->ValueTag = vtag;
	switch(vtag) {
		case S:	/* string */
				STRNCPY(card->u.SVal, valuep ? (SPTYPE)valuep : " ");
				break;
		case L: /* logical */
				card->u.LVal = valuep ? *(LTYPE *)valuep : TRUE;
				break;
		case J: /* integer */
				card->u.JVal = valuep ? *(JTYPE *)valuep : 0;
				break;
		case E:
		case F:	/* float */
				card->u.EVal = valuep ? *(ETYPE *)valuep : 0.f;
				break;
		case D:
		case G: /* double */
				card->u.DVal = valuep ? *(DTYPE *)valuep : 0.;
				break;
		default:
				Error("InsertCard", "Unkown tag field detected");
				PrgExit(MISC_ERR);
	}
	STRNCPY(card->Comment, comment);
} /* InsertCard */



void MoveCards(FitsHeader_t *header, const char *StartKeyWord,
			   const char *EndKeyWord, const char *MoveKeyWord)
{
	FitsCard_t	*StartCard, *EndCard, *MoveCard, *PredCard;

	if (!StartKeyWord)
		return;
	if (!(StartCard = LookupCard(header, StartKeyWord, &PredCard))) {
		Error("MoveCards", "Keyword `%s' not contained in header",
			  StartKeyWord);
		return;
	}
	EndCard = EndKeyWord ? LookupCard(header, EndKeyWord, NULL) :
															header->actual;
	if (!EndCard) {
		Error("MoveCards", "Keyword `%s' not contained in header",
			  EndCard);
		return;
	}
	MoveCard = MoveKeyWord ? LookupCard(header, MoveKeyWord, NULL) :
															header->actual;
	if (!MoveCard) {
		Error("MoveCards", "Keyword `%s' not contained in header",
			  MoveKeyWord);
		return;
	}
	if (StartCard == MoveCard || EndCard == MoveCard)
		/*
		 *	nothing to move
		 */
		return;
	if (header->actual == MoveCard)
		header->actual = EndCard;
	else if (header->actual == EndCard)
		header->actual = PredCard;
	if (header->first == StartCard)
		header->first = PredCard;
	PredCard->next = EndCard->next;
	EndCard->next = MoveCard->next;
	MoveCard->next = StartCard;
} /* MoveCards */



void AddCard(FitsHeader_t *header, const char *KeyWord, const ValueTag_t vtag,
			 const void *valuep, const char *comment)
{

  if(ExistsKeyWord(header, KeyWord, NULL))
    DeleteCard(header, KeyWord);

  InsertCard(header, CPNULL, KeyWord, vtag, valuep, comment);


} /* AddCard */



void AddComHis(FitsHeader_t *header, const char *Comment, const ValueTag_t tag)
{
	FitsCard_t	*card = EmptyCard();

	if (header->first)
		header->actual->next = card;
	else
		/*
		 * this is the first header card
		 */
		header->first = card;
	header->actual = card;
	card->next = NULL;
	++header->nCards;
	STRNCPY(card->Comment, *Comment ? Comment : " ");
	card->ValueTag = tag;
	strcpy(card->KeyWord, " ");
} /* AddComHis */



void AddComment(FitsHeader_t *header, const char *Comment)
{
	char		*(*StrPtr)[];
	unsigned	nLines;
	
	if (Comment && strlen(Comment) > 1) {
		unsigned n;

		nLines = SplitLines(Comment, FLEN_VALUE, &StrPtr);
		for (n=0; n<nLines; ++n)
			AddComHis(header, (*StrPtr)[n], NC);
	} else
		AddComHis(header, " ", NC);
} /* AddComment */



void AddHistory(FitsHeader_t *header, const char *Comment)
{
	char		*(*StrPtr)[];
	unsigned	nLines;

	if (Comment && strlen(Comment) > 1) {
		unsigned n;

		nLines = SplitLines(Comment, FLEN_VALUE, &StrPtr);
		for (n=0; n<nLines; ++n)
			AddComHis(header, (*StrPtr)[n], NH);
	} else
		AddComHis(header, " ", NH);
}



void WriteComment(const FitsFileUnit_t unit, const char *Comment)
{

	if (Comment) {
		char		*(*StrPtr)[];
		unsigned	nLines, n;
		Status_t	status;

		nLines = SplitLines(Comment, FLEN_VALUE, &StrPtr);
		for (n=0; n<nLines; ++n) {
			status = 0;
			ffpcom(unit, (*StrPtr)[n], &status);
			FITSERR(WriteComment, ffpcom);
		}
	}
} /* WriteComment */



void WriteHistory(const FitsFileUnit_t unit, const char *History)
{

	if (History) {
		char		*(*StrPtr)[];
		unsigned	nLines, n;
		Status_t	status;

		nLines = SplitLines(History, FLEN_VALUE, &StrPtr);
		for (n=0; n<nLines; ++n) {
			status = 0;
			ffphis(unit, (*StrPtr)[n], &status);
			FITSERR(WriteHistory, ffphis);
		}
	}
} /* WriteHistory */



void AddColumn(FitsHeader_t *header, Bintable_t *table,
			   const char *ttype, const char *TypeComment,
			   const char *tform, const unsigned present, ...)
{
	va_list		ap;
	JTYPE		jdum;
	DTYPE		ddum;
	ETYPE		edum;
	XTYPE		xdum;
	char		KeyWord[FLEN_KEYWORD];
	char		Comment[FLEN_COMMENT];
	unsigned	FieldSize, AllocSize, nElem;
	ValueTag_t	tag;

	#define IS_PRESENT(a)	(present & a)
	#define COL				table->nColumns-1

	va_start(ap, present);
	if (++COL >= MAX_COLUMNS) {
		Error("AddColumn", "Too many columns - increase MAX_COLUMNS in header "
			  "file to " __FILE__);
		PrgExit(EXCEED_ERR);
	}
	sprintf(KeyWord, "TTYPE%d", COL+1);
	AddCard(header, KeyWord, S, ttype, TypeComment);
	strcpy(table->ColNames[COL], ttype);
	sprintf(Comment, "data format of the field: %s",
			TformScan(tform, &AllocSize, &tag, &FieldSize, &nElem));
	sprintf(KeyWord, "TFORM%d", COL+1);
	AddCard(header, KeyWord, S, tform, Comment);
	strcpy(table->tforms[COL], header->actual->u.SVal);
	table->DataColSize[COL]		= FieldSize;
	table->Tzero[COL]	        = 0.;
	table->Tscal[COL]	        = 1.;
	table->ByteWidth	       += FieldSize * nElem;
	table->TagVec[COL]		= tag;
	table->Multiplicity[COL]	= nElem;

	/*
	 * try to allocate space for the actual table column
	 */
	if (table->nBlockRows)
    {
		if (table->TagVec[COL] != S) {
			if (!(table->cols[COL]=TSMALLOC(BYTE,
											table->nBlockRows*AllocSize))) {
				Error("AddColumn", "attempt to allocate %d bytes for BINTABLE "
					  "column #%u failed\n", AllocSize*table->nBlockRows,
					  COL+1);
				PrgExit(MALLOC_ERR);
			}
		} else {
			unsigned char **p;
			unsigned n;
			if (!(p = TSMALLOC(SPTYPE, table->nBlockRows))) {
				Error("AddColumn", "attempt to allocate %d bytes for BINTABLE "
					  "column #%u (char * vector) failed\n",
					  sizeof(SPTYPE) * table->nBlockRows);
				PrgExit(MALLOC_ERR);
			}
			table->cols[COL] = p;
			for (n=0; n<table->nBlockRows; ++n)
				if (!(p[n] = TSMALLOC(BYTE, AllocSize))) {
					Error("AddColumn", "attempt to allocate %d bytes for "
						  "row #%u of string column failed\n",
						  AllocSize, n);
					PrgExit(MALLOC_ERR);
				}
		}
     }
	/*
	 *	set all elements in new colum as undefined
	 */
	SetColumnIllegal(table->cols[COL], tag, table->nBlockRows, AllocSize);

	/*
	 * now, process optional fields
	 */
	if (IS_PRESENT(TUNIT)) {
		char	*tmp;

		tmp = va_arg(ap, SPTYPE);
		sprintf(KeyWord, "TUNIT%d", table->nColumns);
		AddCard(header, KeyWord, S, tmp, va_arg(ap, char *));
	}
	if (IS_PRESENT(TMIN)) {
		sprintf(Comment, "minimum legal value for parameter %d",
				table->nColumns);
		sprintf(KeyWord, "TLMIN%d", table->nColumns);
		switch (tag) {
			case I:
			case J: jdum = va_arg(ap, JTYPE);
					AddCard(header, KeyWord, J, &jdum, Comment);
					break;
			case E:
			case F:
			case D:
					edum = (float)va_arg(ap, DTYPE);
					AddCard(header, KeyWord, E, &edum, Comment);
					break;
			default:
					Error("AddColumn", "wrong type for TLMIN");
					break;
		}
	}
	if (IS_PRESENT(TMAX)) {
		sprintf(Comment, "maximum legal value for parameter %d",
				table->nColumns);
		sprintf(KeyWord, "TLMAX%d", table->nColumns);
		switch (tag) {
			case I:
			case J: jdum = va_arg(ap, JTYPE);
					AddCard(header, KeyWord, J, &jdum, Comment);
					break;
			case E:
			case F:
			case D:
					edum = (float)va_arg(ap, DTYPE);
					AddCard(header, KeyWord, E, &edum, Comment);
					break;
			default:
					Error("AddColumn", "wrong type for TLMAX");
					break;
		}
	}
	if (IS_PRESENT(TSCAL)) {
		sprintf(KeyWord, "TSCAL%d", table->nColumns);
		ddum = va_arg(ap, DTYPE);
		AddCard(header, KeyWord, G, &ddum, va_arg(ap, SPTYPE));
		table->Tscal[COL] = ddum;
	}
	if (IS_PRESENT(TNULL)) {
		sprintf(Comment, "illegal value for column %d", table->nColumns);
		sprintf(KeyWord, "TNULL%d", table->nColumns);
		switch (tag) {
		        case I:
			case J: jdum = va_arg(ap, JTYPE);
					AddCard(header, KeyWord, J, &jdum, Comment);
					break;
		
			default:
					Error("AddColumn", "wrong type for TNULL");
					break;
		}
	
	}
	if (IS_PRESENT(TZERO)) {
		sprintf(KeyWord, "TZERO%d", table->nColumns);
		ddum = va_arg(ap, DTYPE);
		AddCard(header, KeyWord, G, &ddum, va_arg(ap, SPTYPE));
		table->Tzero[COL] = ddum;
	}
	va_end(ap);

	#undef IS_PRESENT
	#undef COL
} /* AddColumn */



void ChangeColumn(const unsigned nCol, FitsHeader_t *header, Bintable_t *table,
				  const char *ttype, const char *TypeComment,
				  const char *tform, const unsigned present, ...)
{
	va_list		ap;
	char		KeyWord[FLEN_KEYWORD], LastKeyWord[FLEN_KEYWORD];
	char		Comment[FLEN_COMMENT];
	unsigned	FieldSize, AllocSize, nElem;
	JTYPE		jdum;
	DTYPE		ddum;
	ValueTag_t	tag;

	#define IS_PRESENT(a)	(present & a)

	va_start(ap, present);

	if (nCol >= table->nColumns) {
		Error("ChangeColumn", "Column #%u does not exist", nCol);
		return;
	}

	/*
	 *	correct table width
	 */
	TformScan(table->tforms[nCol], &AllocSize, &tag, &FieldSize, &nElem);
	if (tag == S) {
		Error("ChangeColumn", "Sorry - string column cannot be changed\n");
		PrgExit(MISC_ERR);
	}
	table->ByteWidth -= FieldSize * nElem;

	/*
	 *	change name of column and comment field
	 */
	sprintf(LastKeyWord, "TTYPE%d", nCol+1);
	SetSVal(header, LastKeyWord, ttype);
	SetComment(header, LastKeyWord, TypeComment);
	strcpy(table->ColNames[nCol], ttype);
	sprintf(Comment, "data format of the field: %s",
			TformScan(tform, &AllocSize, &tag, &FieldSize, &nElem));

	/*
	 *	change TFORM field and comment field
	 */
	sprintf(KeyWord, "TFORM%d", nCol+1);
	SetSVal(header, KeyWord, tform);
	SetComment(header, KeyWord, Comment);

	strcpy(table->tforms[nCol], GetSVal(header, KeyWord));
	table->TagVec[nCol]			= tag;
	table->Tzero[nCol]	        = 0.;
	table->Tscal[nCol]	        = 1.;
	table->Multiplicity[nCol]	= nElem;
	table->DataColSize[nCol]	= FieldSize;
	table->ByteWidth			+= FieldSize * nElem;

	/*
	 *	adjust space for table column
	 */
	if (!(table->cols[nCol] = realloc(table->cols[nCol],
									  table->nBlockRows * AllocSize))) {
		Error("ChangeColumn", "attempt to re-allocate %d bytes for BINTABLE "
			  "column #%u failed\n", AllocSize*table->nBlockRows, nCol+1);
		PrgExit(MALLOC_ERR);
	}

	/*
	 *	figure out if TUNIT, TLMIN, TLMAX, TSCAL, or TNULL cards exist for
	 *	column to change, if so, delete them
	 */
	{
		char		KeyWord[FLEN_KEYWORD],
					*Fields[] = { "TUNIT", "TLMIN", "TLMAX", "TSCAL", "TNULL" , "TZERO"};
		unsigned	n;

		for (n=0; n<NUM_OF(Fields); ++n) {
			sprintf(KeyWord, "%s%u", Fields[n], nCol+1);
			if (LookupCard(header, KeyWord, (FitsCard_t **)NULL))
				DeleteCard(header, KeyWord);
		}
	}

	/*
	 * now, process optional fields
	 */
	if (IS_PRESENT(TUNIT)) {
		char	*tmp;

		tmp = va_arg(ap, SPTYPE);
		sprintf(KeyWord, "TUNIT%d", nCol+1);
		InsertCard(header, LastKeyWord, KeyWord, S, tmp, va_arg(ap, char *));
		strcpy(LastKeyWord, KeyWord);
	}
	if (IS_PRESENT(TMIN)) {
		sprintf(Comment, "minimum legal value for parameter %d",
				table->nColumns);
		sprintf(KeyWord, "TLMIN%d", nCol+1);
		jdum = va_arg(ap, JTYPE);
		InsertCard(header, LastKeyWord, KeyWord, J, &jdum, Comment);
		strcpy(LastKeyWord, KeyWord);
	}
	if (IS_PRESENT(TMAX)) {
		sprintf(Comment, "maximum legal value for parameter %d",
				nCol+1);
		sprintf(KeyWord, "TLMAX%d", nCol+1);
		jdum = va_arg(ap, JTYPE);
		InsertCard(header, LastKeyWord, KeyWord, J, &jdum, Comment);
		strcpy(LastKeyWord, KeyWord);
	}
	if (IS_PRESENT(TSCAL)) {
		sprintf(KeyWord, "TSCAL%d", nCol+1);
		ddum = va_arg(ap, DTYPE);
		InsertCard(header, LastKeyWord, KeyWord, G, &ddum, va_arg(ap, SPTYPE));
		table->Tscal[nCol] = ddum;
	}
	if (IS_PRESENT(TNULL)) {
		sprintf(Comment, "illegal value for column %d", nCol+1);
		sprintf(KeyWord, "TNULL%d", nCol+1);
		jdum = va_arg(ap, JTYPE);
		InsertCard(header, LastKeyWord, KeyWord, J, &jdum, Comment);
	}
	if (IS_PRESENT(TZERO)) {
		sprintf(KeyWord, "TZERO%d", nCol+1);
		ddum = va_arg(ap, DTYPE);
		InsertCard(header, LastKeyWord, KeyWord, G, &ddum, va_arg(ap, SPTYPE));
		table->Tzero[nCol] = ddum;
	}

 	va_end(ap);

	#undef IS_PRESENT
} /* ChangeColumn */



void DeleteCard(FitsHeader_t *header, const char *KeyWord)
{
	FitsCard_t	*card, *pred;

	if ((card = LookupCard(header, KeyWord, &pred))) {
		if (pred)
			pred->next = card->next;
		else
			/*
			 * no predecessor, i.e. we are deleting the very first card
			 */
			header->first = card->next;
		if (header->actual == card)
			header->actual = pred;
		--header->nKeyWords;
		--header->nCards;
		free(card);
	} else
		Error("DeleteCard", "Keyword `%s' not contained in header", KeyWord);
} /* DeleteCard */



void DeleteComment(FitsHeader_t *header, const char *KeyWord)
{
	FitsCard_t	*card, *tmp, *tmp1;
	unsigned	deleted;

	deleted = 0;
	if ((card = LookupCard(header, KeyWord, NULL))) {
		tmp = card->next;
		while (tmp && tmp->ValueTag == NC) {
			if (tmp == header->actual)
				header->actual = card;
			tmp1 = tmp;
			tmp = tmp->next;
			deleted++;
			free(tmp1);
		}
		header->nCards -= deleted;
		card->next = tmp;
	} else
		Error("DeleteComment", "Keyword `%s' not contained in header", KeyWord);
} /* DeleteComment */


FitsHeader_t NewPrimaryHeader(ValueTag_t DataType, int nAxes,
							  const JTYPE AxesSizes[],
							  const char *AxesComments[],
							  BOOL extend)
{
	FitsHeader_t	*header = EmptyHeader();
	LTYPE			ldum;
	JTYPE			jdum;
	int				i;
	char			KeyWord[FLEN_KEYWORD];
	char			ExtComment[FLEN_COMMENT];

	header->type = PRIMARY;
	header->nCards = header->nKeyWords = 0;
	header->first = header->actual = NULL;

	/*
	 * now, define the mandatory keywords for FITS primary header
	 */
	ldum = TRUE;
	AddCard(header, "SIMPLE", L, &ldum, "file does conform to FITS standard");
	switch (DataType) {
		case B: jdum = 1; break;
		case I: jdum = 2; break;
		case J: jdum = 4; break;
		case E:
		case F: jdum = -4; break;
		case D:
		case G: jdum = -8; break;
		default:
				Error("NewPrimaryHeader", "Improper data type");
				PrgExit(MISC_ERR);
	}
	jdum *= CHAR_BIT;
	AddCard(header, "BITPIX", J, &jdum, "number of bits per data pixel");
	header->nAxes = nAxes;
	jdum = nAxes;
	AddCard(header, "NAXIS", J, &jdum, "number of data axes");
	header->AxesSizes = (JTYPE *)calloc(nAxes, sizeof(JTYPE));
	for (i=0; i<nAxes; ++i) {
		header->AxesSizes[i] = AxesSizes[i];
		sprintf(KeyWord, "NAXIS%1u", i+1);
		AddCard(header, KeyWord, J, &AxesSizes[i], AxesComments[i]);
	}
	ldum = (LTYPE)extend;
	strcat(strcat(strcpy(ExtComment, "FITS dataset "),
				  extend ? "may" : "does not"),
		   " contain extensions");
	AddCard(header, "EXTEND", L, &ldum, ExtComment);
	return *header;
} /* NewPrimaryHeader */



void EndPrimaryHeader(FitsHeader_t *header)
{
	/* nothing to do for the moment */
} /* EndPrimaryHeader */



FitsHeader_t NewBintableHeader(const unsigned MaxBlockRows,
							   Bintable_t *table)
{
	FitsHeader_t	*header = EmptyHeader();
	JTYPE			jdum;

	header->type		= BINTABLE;
	header->nCards		= header->nKeyWords = 0;
	header->first		= header->actual = NULL;
	table->nColumns 	= table->ByteWidth = 0;
	table->nRows		= 0;
	table->MaxRows		= 0;
	table->nBlockRows 	= MaxBlockRows;
	/*
	 * now, define the mandatory keywords for FITS BINTABLE header
	 */
	AddCard(header, "XTENSION", S, "BINTABLE", "binary table extension");
	jdum = CHAR_BIT;
	AddCard(header, "BITPIX", J, &jdum, "8-bit bytes");
	jdum = 2;
	AddCard(header, "NAXIS", J, &jdum, "2-dimensional binary table");
	jdum = 0;
	AddCard(header, "NAXIS1", J, &jdum, "width of table in bytes");
	AddCard(header, "NAXIS2", J, &jdum, "number of rows in table");
	jdum = 0;
	AddCard(header, "PCOUNT", J, &jdum, "size of special data area");
	jdum = 1;
	AddCard(header, "GCOUNT", J, &jdum,"one data group (required keyword)");
	jdum = 0;
	AddCard(header, "TFIELDS", J, &jdum, "number of fields in each row");

	return *header;
} /* NewBintableHeader */



void GetBintableStructure(FitsHeader_t *header, Bintable_t *table,
						  const int MaxBlockRows, const unsigned nColumns,
						  const unsigned ActCols[])
{
	unsigned	n, TableSize, AllocRows, ColCounter;
	FitsCard_t      *card;

	if (header->type != BINTABLE) {
		Error("GetBintableStructure", "not a Bintable FITS header");
		PrgExit(ILL_ERR);
	}
	table->nColumns = GetJVal(header, "TFIELDS");
	table->nRows = 0;
	table->MaxRows = GetJVal(header, "NAXIS2");
	table->ByteWidth = GetJVal(header, "NAXIS1");
	AllocRows = 0;
	if (MaxBlockRows)
		AllocRows = MaxBlockRows > 0 ? MaxBlockRows : table->MaxRows;
	table->nBlockRows = AllocRows;

	/*
	 *	now, for each column get name (TTYPE), form (TFORM)
	 */
	TableSize = 0;
	ColCounter = 0;
	for (n=0; n<table->nColumns; ++n) {
		unsigned	FieldSize, AllocSize, nElem;
		char		tmp[FLEN_KEYWORD];

		if (n >= MAX_COLUMNS) {
			Error("GetBintableStructure", "too many columns in Bintable");
			PrgExit(EXCEED_ERR);
		}
		sprintf(tmp, "TTYPE%u", n+1);
		strcpy(table->ColNames[n], GetSVal(header, tmp));
		sprintf(tmp, "TFORM%u", n+1);
		strcpy(table->tforms[n], GetSVal(header, tmp));
		TformScan(table->tforms[n], &AllocSize, table->TagVec+n, &FieldSize,
				  &nElem);

		sprintf(tmp, "TZERO%u", n+1);
		if (ExistsKeyWord(header, tmp, &card )) {
		  table->Tzero[n] = card->u.DVal;
		} else {
		  table->Tzero[n] = 0;
		}
		sprintf(tmp, "TSCAL%u", n+1);
		if (ExistsKeyWord(header, tmp, &card )) {
		  table->Tscal[n] = card->u.DVal;
		} else {
		  table->Tscal[n] = 1;
		}

		table->Multiplicity[n]	= nElem;
		table->DataColSize[n]	= FieldSize;
		TableSize	        += FieldSize * nElem;
		/*
		 *	logic: we allocate space for column #n, if
		 *		1. AllowRows != 0, and
		 *		2. nColumns == 0 or nColumns != 0 and n is contained in ActCols
		 */
		if (AllocRows && (!nColumns || UVecMatch(n, ActCols, nColumns) >= 0)) {
			if (table->TagVec[n] != S) {
				if (!(table->cols[n] = TSMALLOC(BYTE, AllocRows * AllocSize))){
					Error("GetBintableStructure", "attempt to allocate %d "
						  "bytes for BINTABLE column #%u failed\n",
						  AllocSize*AllocRows, n+1);
					PrgExit(MALLOC_ERR);
				}
			} else {
				unsigned char **p;
				unsigned nrow;
				if (!(p = TSMALLOC(SPTYPE, AllocRows))) {
					Error("GetBintableStructure", "attempt to allocate %d "
						  "bytes for BINTABLE column #%u (char * vector) "
						  "failed\n", sizeof(SPTYPE) * AllocRows);
					PrgExit(MALLOC_ERR);
				}
				table->cols[n] = p;
				for (nrow=0; nrow<AllocRows; ++nrow)
					if (!(p[nrow] = TSMALLOC(BYTE, AllocSize))) {
						Error("GetBintableStructure", "attempt to allocate %d "
							  "bytes for row #%u of string column failed\n",
							  AllocSize, nrow);
						PrgExit(MALLOC_ERR);
					}
			}
		} else
			table->cols[n] = VPNULL;
	}
	table->ByteWidth = TableSize;
} /* GetBintableStructure */



void EndBintableHeader(FitsHeader_t *header, const Bintable_t *table)
{
  /*
   * we have to complete the mandatory fields: NAXIS1 and TFIELDS
   */
  SetJVal(header, "NAXIS1",  table->ByteWidth);
  SetJVal(header, "TFIELDS", table->nColumns);
} /* EndBintableHeader */



int WriteHeader(const FitsFileUnit_t unit, const FitsHeader_t *header)
{
	FitsCard_t	*act;
	int			status;

	for (status=0, act=header->first; act!=NULL; act=act->next)
		switch(act->ValueTag) {
			case S: /* string */
					ffpkys(unit, act->KeyWord, act->u.SVal, act->Comment,
						   &status);
					FITSERR(WriteHeader, ffpkys);
					break;
			case L: /* logical */
					ffpkyl(unit, act->KeyWord, act->u.LVal, act->Comment,
						   &status);
					FITSERR(WriteHeader, ffpkyl);
					break;
			case J: /* integer */
					ffpkyj(unit, act->KeyWord, act->u.JVal, act->Comment,
						   &status);
					FITSERR(WriteHeader, ffpkyj);
					break;
			case E:	/* float */
					ffpkye(unit, act->KeyWord, act->u.EVal, EDECIMALS,
						   act->Comment, &status);
					FITSERR(WriteHeader, ffpkye);
					break;
			case F: /* float - fixed point format */
					ffpkyf(unit, act->KeyWord, act->u.EVal, EDECIMALS,
						   act->Comment, &status);
					FITSERR(WriteHeader, ffpkyf);
					break;
			case D: /* double - floating point format */
					ffpkyd(unit, act->KeyWord, act->u.DVal, DDECIMALS,
						   act->Comment, &status);
					FITSERR(WriteHeader, ffpkyd);
					break;
			case G: /* double - fixed point format */
				        if( !strncmp(act->KeyWord,"TZERO",5) ||
					    !strncmp(act->KeyWord,"TSCAL",5) ) {
					  ffpkyg(unit, act->KeyWord, act->u.DVal, 0,
						 act->Comment, &status);
					  FITSERR(WriteHeader, ffpkyg);
					}
					else {
					  ffpkyg(unit, act->KeyWord, act->u.DVal, DDECIMALS,
						 act->Comment, &status);
					  FITSERR(WriteHeader, ffpkyg);
					}
					break;
			case NC: /* comment */
					ffpcom(unit, act->Comment, &status);
					FITSERR(WriteHeader, ffpcom);
					break;
			case NH: /* history */
					ffphis(unit, act->Comment, &status);
					FITSERR(WriteHeader, ffphis);
					break;
			default:
					Error("WriteHeader", "Unkown tag field detected");
					return 1;
		}

	return 0;
} /* WriteHeader */



int WriteUpdatedHeader(const FitsFileUnit_t unit, const FitsHeader_t *header)
{
	FitsCard_t	*act;
	int			status;

	for (status=0, act=header->first; act!=NULL; act=act->next)
		switch(act->ValueTag) {
			case S: /* string */
					ffukys(unit, act->KeyWord, act->u.SVal, act->Comment,
						   &status);
					FITSERR(WriteUpdatedHeader, ffukys);
					break;
			case L: /* logical */
					ffukyl(unit, act->KeyWord, act->u.LVal, act->Comment,
						   &status);
					FITSERR(WriteUpdatedHeader, ffukyl);
					break;
			case J: /* integer */
					ffukyj(unit, act->KeyWord, act->u.JVal, act->Comment,
						   &status);
					FITSERR(WriteUpdatedHeader,ffukyj );
					break;
			case E:	/* float */
					ffukye(unit, act->KeyWord, act->u.EVal, EDECIMALS,
						   act->Comment, &status);
					FITSERR(WriteUpdatedHeader, ffukye);
					break;
			case F: /* float - fixed point format */
					ffukyf(unit, act->KeyWord, act->u.EVal, EDECIMALS,
						   act->Comment, &status);
					FITSERR(WriteUpdatedHeader,ffukyf );
					break;
			case D: /* double - floating point format */
					ffukyd(unit, act->KeyWord, act->u.DVal, DDECIMALS,
						   act->Comment, &status);
					FITSERR(WriteUpdatedHeader, ffukyd);
					break;
			case G: /* double - fixed point format */
				        if( !strncmp(act->KeyWord,"TZERO",5) ||
					    !strncmp(act->KeyWord,"TSCAL",5) ) {
					  ffpkyg(unit, act->KeyWord, act->u.DVal, DDECIMALS,
						 act->Comment, &status);
					  FITSERR(WriteHeader, ffpkyg);
					}
					else {
					  ffpkyg(unit, act->KeyWord, act->u.DVal, 0,
						 act->Comment, &status);
					  FITSERR(WriteHeader, ffpkyg);
					}
					break;
			case NC: /* comment */
			case NH: /* history */
					break;
			default:
					Error("WriteUpdatedHeader", "Unkown tag field detected");
					return 1;
		}

	return 0;
} /* WriteUpdatedHeader */


/*  void Cffempty(void); */

void FinishPrimaryHeader(FitsFileUnit_t unit, const FitsHeader_t *header)
{
	Status_t	status;
	int			pcount, gcount;

	if (header->type != PRIMARY) {
		Error("FinishPrimaryHeader", "FITS header is not of type PRIMARY");
		PrgExit(MATCH_ERR);
	}
	/*
	 * write out the primary header
	 */
	WriteHeader(unit, header);

	/*
	 * define structure of primary array
	 */
	pcount = 0;
	gcount = 1;
	status = 0;
	/*  Cffempty(unit, GetJVal(header, "BITPIX"), header->nAxes, */
/*  		   header->AxesSizes, pcount, gcount, &status); */
/*  	FITSERR(FinishPrimaryHeader, Cffempty); */
} /* FinishPrimaryHeader */



void FinishBintableHeader(const FitsFileUnit_t unit, FitsHeader_t *header,
						  Bintable_t *table)
{
	int			varidat, dummynrows;
	/* unsigned	cols; */
	/*	double		tscal, tzero; */
	Status_t	status;

	if (header->type != BINTABLE) {
		Error("FinishBintableHeader", "FITS header is not of type BINTABLE");
		PrgExit(MATCH_ERR);
	}
	/*
	 * create new empty header unit at end of current FITS file
	 */
	status = 0;
	ffcrhd(unit, &status);
	FITSERR(FinishBintableHeader, ffcrhd);
	/*
	 * write out the Bintable header
	 */
	WriteHeader(unit, header);

	/*
	 * define structure of Bintable array
	 */
	varidat = 0;
	status = 0;
	dummynrows = 1;	/* dummy value, will be modified later on */
	/*  Cffempty(unit, table->nColumns, table->tforms, varidat, dummynrows, */
/*  		   &status); */
/*  	FITSERR(FinishBintableHeader, Cffempty); */
	/*
	tscal = 1.;
	tzero = 0.;
	for (cols=0; cols<table->nColumns; ++cols) {
		fftscl(unit, cols+1, tscal, tzero, &status);
		FITSERR(FinishBintableHeader, fftscl);
	}
	*/
} /* FinishBintableHeader */

static void CompleteBintable(FitsFileUnit_t unit, Bintable_t *table)
{
	Status_t	status;

	/*
	 *	we have just written the last block of the current Bintable
	 *	i.e., the total number of rows is known now
	 *	now, re-define the size of the current FITS data unit
	 *	and modify NAXIS2 keyword value according to the actual
	 *	number of columns in this Bintable
	 */
	status = 0;
	/*  Cffempty(unit, table->ByteWidth * table->nRows, &status); */
/*  	FITSERR(CompleteBintable, Cffempty); */
	ffmkyj(unit, "NAXIS2", table->nRows, "&", &status);
	FITSERR(CompleteBintable, ffmkyj);
	table->MaxRows = table->nRows;

} /* CompleteBintable */


/*-----------------------------------------------------------------------------
 *	SYNOPSIS:
 *		int WriteBintable(FitsFileUnit_t unit, Bintable_t *table,
 *						  unsigned nRows, BOOL last)
 *
 *	LOGIC:
 *		- foreach <column> in Bintable
 *			- write column <column> with FITSIO routine FCPCLx
 *		- if this is the last row block
 *			- tell FITSIO size of Bintable
 *			- update value of NAXIS1 (= total number of rows)
 *
 *	DEPENDENCIES:
 *		function			source				package
 *		-----------------------------------------------
 *		FCPCL{S,L,I,J,E,D}						FITSIO
 *		CompleteBintable	see above			
 *		Error				termio.c			  "
 *		PrgExit					"				  "
 *---------------------------------------------------------------------------*/
int WriteBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows,
				  BOOL last)
{
	Status_t	status;
	unsigned	n;
	int			frow, felem, nelem;

	frow = table->nRows+1;
	felem = 1;
	nelem = nRows;
	for (n=status=0; n<table->nColumns && nelem; ++n)

		#define WRCOL(a)	a(unit, n+1, frow, felem, nelem, table->cols[n],\
							&status);										\
							FITSERR(WriteBintable, a)

		switch(table->TagVec[n]) {

			case S: /* string */
					ffpcls(unit, n+1, frow, felem, nelem,
						   (char **)table->cols[n], &status);
					break;
			case L: /* logical */
					WRCOL(ffpcll);
					break;
			case I: /* short integer */
					WRCOL(ffpcli);
					break;
			case J: /* integer */
					WRCOL(ffpclj);
					break;
			case E:	/* float */
					WRCOL(ffpcle);
					break;
			case D: /* double - floating point format */
					WRCOL(ffpcld);
					break;
			default:
					Error("WriteBintable", "Unkown tag field detected");
					PrgExit(MISC_ERR);
		}

		#undef WRCOL

	table->nRows += nRows;
	if (last && table->nRows) {
		/*
		 *	last row block has been written
		 */
		CompleteBintable(unit, table);

		return table->nRows;
	} else
		return nRows;
} /* WriteBintable */



/*-----------------------------------------------------------------------------
 *	SYNOPSIS:
 *		int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table,
 *							  unsigned nRows, BOOL last)
 *
 *	LOGIC:
 *		This function does the same as `WriteBintable' above but significantly
 *		faster. This is mainly a consequence of bypassing the FITSIO
 *		FCPCLx-functions to write Bintable columns. Using these functions
 *	    repeatedly to write a complete Bintable to disk generates tremendous
 *		I/O activities since FITS Bintable data are physically stored in a
 *		_row-wise_ fashion on disk. This problem can be circumvented
 *		by first creating an exact copy of the Bintable data section that
 *		will be created on disk in memory, and then, dumping this memory
 *		area to disk with the low-level function FCPTBS. In more detail
 *		the logic is:
 *
 *		- allocate memory for Bintable data that are to be written in this call
 *		- foreach <column>
 *			- store data for <column> in memory
 *		- write memory to disk using FCPTBS
 *		- release memory
 *		- if this is the last row block
 *			- tell FITSIO size of Bintable
 *			- update value of NAXIS1 (= total number of rows)
 *
 *	IMPORTANT NOTE:
 *		Building the Bintable data section in memory requires in-situ
 *		knowledge of how FITSIO organizes binary data in Bintables.
 *		This information is not documented anywhere and thus,
 *		`WriteFastBintable' would probably not get the blessing of the
 *		HEASARC FITSIO gurus. The things to remember are:
 *
 *			1. A Bintable row does _not_ contain any "alignment" space.
 *			2. Logicals are coded as the ASCII characters 'T' and 'F'
 *			   for TRUE and FALSE respectively.
 *			3. Data items are stored in big-endian format!
 *
 *		  The last point necessitates, that on little-endian machines
 *		  like Dec/Alphas, Intel based boxes, etc., a certain amount
 *		  of byte swapping is required prior to actually pass the data
 *		  to FITSIO, i.e. FCPTBS. Here, this is taken care of through
 *		  calling the lowlevel routines:
 *				- PutHalfWord
 *				- PutWord
 *				- PutDoubleWord
 *
 *		By using `WriteFastBintable' instead of `WriteBintable' usually
 *		a performace gain of at least a factor two is attainable!
 *		This fact by far outweighs the "negative" aspects expressed above.
 *		Credit for the original idea of `WriteBintable' goes to AM.
 *
 *	DEPENDENCIES:
 *		function				source				package
 *		---------------------------------------------------
 *		memcpy										libc.a
 *		malloc/free									   "
 *		FCPTBS										FITSIO
 *		Put{Half,Double}Word	lowlevel.c			
 *		CompleteBintable	
 *		Error					termio.c			  "
 *		PrgExit						"				  "
 *---------------------------------------------------------------------------*/
int WriteFastBintable(FitsFileUnit_t unit, Bintable_t *table, unsigned nRows,
					  BOOL last)
{
	int			frow, DataBlockSize;
	unsigned	i, j, n, AccuWidth, multi, FullColSize;
	BYTE		*DataBlockPtr = 0, *DataPtr;
	Status_t	status;
	unsigned        TableByteWidth;
	DTYPE           tzero, tscal;
	char                    tmpbuff;
	unsigned        index;

	TableByteWidth = table->ByteWidth;
	/*
	 *	get memory to store row block
	 */
	DataBlockSize = nRows * TableByteWidth;
	if (nRows)
	  GETMEM(DataBlockPtr, BYTE, DataBlockSize, "WriteFastBintable", -1);
	/*
	 *	loop over columns
	 */
	frow		= table->nRows+1;
	AccuWidth	= 0;
	for (n=status=0; n<table->nColumns && nRows; ++n) {
		DataPtr		= DataBlockPtr + AccuWidth;
		tzero           = table->Tzero[n];
		tscal           = table->Tscal[n];
		multi		= table->Multiplicity[n];
		FullColSize	= multi * table->DataColSize[n];

		if ( tscal == 0 ) tscal = 1.;

		#define COPYDATA(Routine, Type)						      \
			for (i=0; i<nRows; ++i) {					      \
				for (j=0; j<table->Multiplicity[n]; ++j)		      \
					Routine((((Type *)table->cols[n])[i*multi+j] - tzero)/tscal, \
							DataPtr + j * table->DataColSize[n]); \
				DataPtr += TableByteWidth;				      \
			}

		

		switch(table->TagVec[n]) {


		case S: /* string */
		  for (i=0; i<nRows; ++i) {
		    memcpy((char *)DataPtr, ((char **)table->cols[n])[i],
			   FullColSize);
		    DataPtr += TableByteWidth;
		  }
		  break;


		case L: /* logical: L-type */
		  for (i=0; i<nRows; ++i) {
		    for (j=0; j<multi; ++j)
		      ((char *)DataPtr)[j] =
			((LTYPE *)table->cols[n])[i*multi+j] ?
			'T' : 'F';
		    DataPtr += TableByteWidth;
		  }
		  break;

		case B: /* Unsigned 8-bit integer */
		  for (i=0; i<nRows; ++i) {
		    for (j=0; j<multi; ++j) {
		      ((char *)DataPtr)[j]=((BTYPE *)table->cols[n])[i*multi+j];
		    }
		    DataPtr += TableByteWidth;
		  }	
		  break;

		case X: /* Unsigned 8-bit integer */

#if BYTESWAPPED
		  for ( i=0; i<nRows; ++i )
		    SWAPBYTES(table,n)
#endif
		  for (i=0; i<nRows; ++i) {
		    for (j=0; j<multi; ++j) {
		      ((char *)DataPtr)[j]=((BTYPE *)table->cols[n])[i*multi+j];
		    }
		    DataPtr += TableByteWidth;
		  }	
		  break;
		  
		case I: /* short integer: I-type */
		  if ( table->Tscal[n] == 1 && table->Tzero[n] == 32768 )
		    { COPYDATA(PutHalfWord, UTYPE); }
		  else 
		    { COPYDATA(PutHalfWord, ITYPE); }
		  break;
		case J: /* long integer: J-type */
		  if ( table->Tscal[n] == 1 &&
               table->Tzero[n] == (double) 2147483648UL )
		    { COPYDATA(PutWord, VTYPE);}
		  else {COPYDATA(PutWord, JTYPE);}
		  break;

		case E: /* single precision float point: E-type */
		  COPYDATA(PutWord, int);
		  break;
		  
		case D: /* double - floating point format */
		  COPYDATA(PutDoubleWord, DTYPE);
		  break;
		  
		default:
		  Error("WriteFastBintable", "Unkown tag field detected");
		  PrgExit(MISC_ERR);
		}

		AccuWidth += FullColSize;
	} /* for loop over columns */

	/*
	 *	now write data block to disk using FCPTBS, check status and
	 *	release memory
	 */
	ffptbb(unit, frow, 1, DataBlockSize, DataBlockPtr, &status);
	FITSERR(WriteFastBintable, ffptbb);

	if (nRows)
		free(DataBlockPtr);

	/*
	 *	update internal row counter
	 */
	table->nRows += nRows;

	/*  FT 15/10/2002 - was:
	 * if (last && table->nRows) { 
	 */
	if (last) {
		/*
		 *	last row block has been written
		 */
		CompleteBintable(unit, table);

		return table->nRows;
	} else
		return nRows;
} /* WriteFastBintable */



int ReadBintable(const FitsFileUnit_t unit, Bintable_t *table,
				 const unsigned nColumns, const unsigned ActCols[],
				 const unsigned FromRow, unsigned *nRows)
{
	char			Comment[FLEN_COMMENT];
	char                    tmpbuff;
	Status_t		status;
	int			anyf;
	unsigned		n, ColNr, EndCol;
	unsigned                index,i,j;



	if (!table->MaxRows) {
		status = 0;
		ffgkyj(unit, "NAXIS2", (long *)&table->MaxRows, Comment, &status);
		FITSERR(ReadBintable, "ffgkyj");
	}
	if (FromRow > (unsigned)table->MaxRows) {
		*nRows = 0;
		return -1;
	}

	EndCol = nColumns ? nColumns : table->nColumns;
	status = 0;
	if (FromRow + *nRows - 1 > (unsigned)table->MaxRows)
		*nRows = table->MaxRows - FromRow + 1;
	for (n=0; n<EndCol; ++n) {

	  ColNr = ActCols ? ActCols[n] : n;

          #define RDCOL(a)  a(unit, ColNr+1, FromRow, 1,                                   \
						  *nRows * table->Multiplicity[ColNr], 0,  \
						  table->cols[ColNr],			   \
						  &anyf, &status);			   \
						  FITSERR(ReadBintable, a)


	  if (table->cols[ColNr])
	    switch(table->TagVec[ColNr]) {

	    case S: /* string */
	      ffgcvs(unit, ColNr+1, FromRow, 1, *nRows, " ",
		     table->cols[ColNr], &anyf, &status);
	      break;
	    case L: /* logical */
	      ffgcl(unit, ColNr+1, FromRow, 1, *nRows,
		    table->cols[ColNr], &status);
	      break;
	    case I:	/* short integer */
	      if ( table->Tscal[ColNr] == 1 && table->Tzero[ColNr] == 32768 ) 
		{ RDCOL(ffgcvui); }
	      else 
		{ RDCOL(ffgcvi); }
	      break;
	    case X:     /* array of bits */
	      RDCOL(ffgcvb);
	      /* swap bytes */
#if BYTESWAPPED
	      for ( i=0; i<*nRows; ++i )
		SWAPBYTES(table,ColNr);
#endif
	      break;
	    case B:	/* unsigned 8-bit integer */
	      RDCOL(ffgcvb);
	      break;
	    case J:	/* integer */
	      if ( table->Tscal[ColNr] == 1 &&
               table->Tzero[ColNr] == (double) 2147483648UL ) 
		{ RDCOL(ffgcvuj); } 
	      else
		{ RDCOL(ffgcvj); }
	      break;
	    case E: /* float */
	      RDCOL(ffgcve);
	      break;
	    case D: /* double - floating point format */
	      RDCOL(ffgcvd);
	      break;
	    default:
	      Error("ReadBintable", "Unknown tag field detected");
	      PrgExit(MISC_ERR);
	    }
	}

	return 0;
} /* ReadBintable */



int ReadBintableWithNULL(const FitsFileUnit_t unit, Bintable_t *table,
				 const unsigned nColumns, const unsigned ActCols[],
				 const unsigned FromRow, unsigned *nRows)
{
	char			Comment[FLEN_COMMENT];
	char                    tmpbuff;
	Status_t		status;
	int			anyf;
	unsigned		n, ColNr, EndCol;
	unsigned                index,i,j;



	if (!table->MaxRows) {
		status = 0;
		ffgkyj(unit, "NAXIS2", (long *)&table->MaxRows, Comment, &status);
		FITSERR(ReadBintable, "ffgkyj");
	}
	if (FromRow > (unsigned)table->MaxRows) {
		*nRows = 0;
		return -1;
	}

	EndCol = nColumns ? nColumns : table->nColumns;
	status = 0;
	if (FromRow + *nRows - 1 > (unsigned)table->MaxRows)
		*nRows = table->MaxRows - FromRow + 1;
	for (n=0; n<EndCol; ++n) {

	  ColNr = ActCols ? ActCols[n] : n;

          #define RDCOLNULL(a)  a(unit, ColNr+1, FromRow, 1,                                   \
						  *nRows * table->Multiplicity[ColNr], -1,  \
						  table->cols[ColNr],			   \
						  &anyf, &status);			   \
						  FITSERR(ReadBintable, a)


	  if (table->cols[ColNr])
	    switch(table->TagVec[ColNr]) {

	    case S: /* string */
	      ffgcvs(unit, ColNr+1, FromRow, 1, *nRows, " ",
		     table->cols[ColNr], &anyf, &status);
	      break;
	    case L: /* logical */
	      ffgcl(unit, ColNr+1, FromRow, 1, *nRows,
		    table->cols[ColNr], &status);
	      break;
	    case I:	/* short integer */
	      if ( table->Tscal[ColNr] == 1 && table->Tzero[ColNr] == 32768 ) 
		{ RDCOLNULL(ffgcvui); }
	      else 
		{ RDCOLNULL(ffgcvi); }
	      break;
	    case X:     /* array of bits */
	      RDCOLNULL(ffgcvb);
	      /* swap bytes */
#if BYTESWAPPED
	      for ( i=0; i<*nRows; ++i )
		SWAPBYTES(table,ColNr);
#endif
	      break;
	    case B:	/* unsigned 8-bit integer */
	      RDCOLNULL(ffgcvb);
	      break;
	    case J:	/* integer */
	      if ( table->Tscal[ColNr] == 1 &&
               table->Tzero[ColNr] == (double) 2147483648UL ) 
		{ RDCOLNULL(ffgcvuj); } 
	      else
		{ RDCOLNULL(ffgcvj); }
	      break;
	    case E: /* float */
	      RDCOLNULL(ffgcve);
	      break;
	    case D: /* double - floating point format */
	      RDCOLNULL(ffgcvd);
	      break;
	    default:
	      Error("ReadBintable", "Unknown tag field detected");
	      PrgExit(MISC_ERR);
	    }
	}

	return 0;
} /* ReadBintableWithNULL */



void WriteWholeBintable(FitsFileUnit_t unit, Bintable_t *table)
{
	table->nRows = 0;
	WriteFastBintable(unit, table, table->MaxRows, TRUE);
} /* WriteWholeBintable */



BOOL KeyWordMatch(FitsFileUnit_t unit, const char *KeyWord,
				  ValueTag_t vtag, const void *NominalValue,
				  void *ActValue)
{
	Status_t	status;
	char		Comment[FLEN_COMMENT], CpKeyWord[FLEN_KEYWORD];

	strcpy(CpKeyWord, KeyWord);
	status = 0;
	switch (vtag) {

		#define CHK(a, b) 												\
					a	dum;											\
																		\
					b(unit, CpKeyWord, &dum, Comment, &status);			\
					FITSERR(KeyWordMatch, b);							\
					if (ActValue)										\
						*(a *)ActValue = dum;							\
					return dum == *(a *)NominalValue

		case E: { CHK(ETYPE, ffgkye); }
		case D: { CHK(DTYPE, ffgkyd); }
		case J: { CHK(long, ffgkyj); }
		case L: { CHK(int, ffgkyl); }
		case S: {
					char dum[FLEN_VALUE];

					ffgkys(unit, CpKeyWord, dum, Comment, &status);
					FITSERR(KeyWordMatch, ffgkys);
					if (ActValue)
						strcpy((char *)ActValue, dum);
					return STREQ((char *)NominalValue, dum);
				}
		default:
				Error("KeyWordMatch", "Illegal value tag field");
				return FALSE;
		#undef CHK
	}
} /* KeyWordMatch */



int ColNameMatch(const char *ColName, const Bintable_t *table)
{
	unsigned	n;

	for (n=0; n<table->nColumns; ++n)
		if (STREQ(ColName, table->ColNames[n]))
			return n;

	return -1;
} /* ColNameMatch */




/* OBSOLETE --- use CopyRows */
void RowCp(Bintable_t *table, const unsigned ToRow, const unsigned FromRow)
{
	unsigned	n;		/* running index		*/

	if (ToRow != FromRow)
		for (n=0; n<table->nColumns; ++n)
			switch (table->TagVec[n]) {
				case E:
				case F: EVEC(*table, ToRow, n) =  EVEC(*table, FromRow, n);
						break;
				case D: DVEC(*table, ToRow, n) =  DVEC(*table, FromRow, n);
						break;
				case I: IVEC(*table, ToRow, n) =  IVEC(*table, FromRow, n);
						break;
			        case B: BVEC(*table, ToRow, n) =  BVEC(*table, FromRow, n);
		                                break;
				case J: JVEC(*table, ToRow, n) =  JVEC(*table, FromRow, n);
						break;
			        case L: LVEC(*table, ToRow, n) =  LVEC(*table, FromRow, n);
				 		break;
			        case X: XVEC(*table, ToRow, n) = XVEC(*table, FromRow, n);
			                        break;
				case S: SVASSGN(*table, ToRow, n, SVEC(*table, FromRow, n));
						break;
				default:
						Error("RowCp", "Type of column #%u unknown", n);
			}
} /* RowCp */


void RowIllegal(Bintable_t *table, const unsigned RowNr)
{
	unsigned	n;	/* running index	*/

	for (n=0; n<table->nColumns; ++n)
		switch (table->TagVec[n]) {
			case E:
			case F: SET_E_NULL(EVEC(*table, RowNr, n));
					break;
			case D: SET_D_NULL(DVEC(*table, RowNr, n));
					break;
			case I: SET_IJ_NULL(IVEC(*table, RowNr, n));
					break;
			case J: SET_IJ_NULL(JVEC(*table, RowNr, n));
					break;
			case L: SET_L_NULL(LVEC(*table, RowNr, n));
					break;
			default:
					Error("RowIllegal", "Type of column #%u unknown", n);
		}
} /* RowIllegal */



void ElimIllegalFRows(FTYPE Vec[], unsigned nRows, unsigned *NewRows)
{
	unsigned	LegalPtr = 0, n;

	for (n=0; n<nRows; ++n)
		if (!IS_E_NULL(Vec[n]) && LegalPtr++ != n)
			Vec[LegalPtr-1] = Vec[n];

	*NewRows = LegalPtr;
} /* ElimIllegalFRows */



FitsHeader_t RetrieveFitsHeader(FitsFileUnit_t unit)
{
	int				HDU_Type, nhdu, dummy;
	int				keyno;
	long			naxes[10], ldummy;
	char			card[FLEN_CARD], keyword[FLEN_KEYWORD],
					value[FLEN_VALUE], comment[FLEN_COMMENT],
					dtype[2];
	Status_t		status;
	FitsHeader_t	*header = EmptyHeader();

	/*
	 * initialize entries in header structure
	 */
	header->nAxes = 0;
	header->AxesSizes = NULLPTR(JTYPE);
	header->nCards = header->nKeyWords = 0;
	header->first = header->actual = (FitsCard_t *)NULL;

	ffghdn(unit, &nhdu);
	/*
	 *	first, get type of FITS header
	 */
	status = 0;
	ffmrhd(unit, 0, &HDU_Type, &status);
	FITSERR(RetrieveFitsHeader,ffmrhd );
	header->type = HDU_Type;
	
	if (header->type == PRIMARY) {
		ffghpr(unit, NUM_OF(naxes), &dummy, &dummy, &header->nAxes, naxes,
			   &ldummy, &ldummy, &dummy, &status);
		FITSERR(RetrieveFitsHeader,ffghpr );
		ffmahd(unit, nhdu, &dummy, &status);
		FITSERR(RetrieveFitsHeader,ffghpr );
	}
	/*
	 *	now, read all card images in current FITS header
	 */
	keyno = 1;
	ffgrec(unit, keyno++, card, &status);
	while(!STREQ(card, "END") && status == 0) {
		unsigned n = 0;

		while(!isspace(card[n]) && (n<sizeof(keyword)-1) && card[n]!='=') {
			keyword[n] = card[n];
			++n;
		}
		keyword[n] = '\0';
		ffpsvc(card, value, comment, &status);
		FITSERR(RetrieveFitsHeader, ffghpr);
		if (value[0] == '\0' || isspace(value[0])) {
			/*
			 * keyword has no value, its either "COMMENT" or "HISTORY"
			 */
			char	*CommentPtr = comment;

			if (strlen(comment) >= 2 && comment[0] == ' ' && comment[1] == ' ')
				/*
				 *	since ffphis/ffpcom will always add to blanks, we remove
				 *	two from the beginning
				 */
				CommentPtr += 2;

			if (toupper(keyword[0]) == 'C')	/* COMMENT */
				AddComment(header, CommentPtr);
			else
				AddHistory(header, CommentPtr);
		} else {
			dtype[0] = ' ';
			dtype[1] = '\0';
			ffdtyp(value, dtype, &status);
			switch (dtype[0]) {
				case 'C' : {
							char	*tmp = strrchr(value, '\'');

							while(isspace(*--tmp));
							*++tmp = '\0';
						   	AddCard(header, keyword, S, value+1, comment);
							break;
						   }
				case 'L' : {
							LTYPE lval = toupper(value[0]) == 'T';

							AddCard(header, keyword, L, &lval, comment);
							break;
				}
			        case 'I' : { 
				    if( !strncmp(keyword,"TZERO",5) ||
					!strncmp(keyword,"TSCAL",5) ) {
				      double dval = atof(value);
				      AddCard(header, keyword, G, &dval, comment);
				    } else {
				      JTYPE jval = atoi(value);
				      AddCard(header, keyword, J, &jval, comment);
				    }
				    break;
				}
		                case 'F' : {
							DTYPE dval = atof(value);

							AddCard(header, keyword, D, &dval, comment);
							break;
						   }
			}
		}
		ffgrec(unit, keyno++, card, &status);
	}

	return *header;
} /* RetrieveFitsHeader */



int	GetColNameIndx(const Bintable_t *table, const char *ColName)
{
	unsigned	i, j;

	for (i=0; i<table->nColumns; ++i) {
		for (j=0; ColName[j] && table->ColNames[i][j]; ++j)
			if (toupper(ColName[j]) != toupper(table->ColNames[i][j]))
				break;
			if (ColName[j] == '\0' && table->ColNames[i][j] == '\0')
				return i;
	}

	return -1;
} /* GetColNameIndx */



int ColIndices(const Bintable_t *table, unsigned n, ...)
{
	char		*Name;
	int			*Ind;
	unsigned	i;
	va_list		ap;

	va_start(ap, n);
	for (i=0; i<n; ++i) {
		Name = va_arg(ap, char *);
		Ind = va_arg(ap, int *);
		if ((*Ind = GetColNameIndx(table, Name)) < 0) {
			Error("ColIndices", "Could not find \"%s\" column - this is fatal",
				  Name);
			return -1;
		}
	}

	va_end(ap);
	return 0;
} /* ColIndices */



static unsigned TypeSize(const ValueTag_t type)
{
	switch (type) {
		case E:
		case F: return sizeof(ETYPE);
		case D: return sizeof(DTYPE);
		case I: return sizeof(ITYPE);
		case J: return sizeof(JTYPE);
		case X: return sizeof(XTYPE);
	}
	return 0;
} /* TypeSize */



void **ReadImage(FitsFileUnit_t unit, ValueTag_t type,
				 int NullValue,
				 unsigned xmin, unsigned xmax,
				 unsigned ymin, unsigned ymax)
{
	void		**p;			/* pointer to vector of row pointers	*/
	int			nElements;		/* # of elements in a row				*/
	unsigned	n,				/* running index						*/
				ElementSize,	/* size of an image pixel in bytes		*/
				RowSize;		/* size of bytes of a complete row		*/
	int			anyf;			/* true if any pixel undefined			*/
	Status_t	status;			/* return value from cfitsio			*/

	/*
	 *	allocate space for pointer vector
	 */
	if (!(ElementSize = TypeSize(type))) {
		Error("ReadImage", "Type of image is unkown - cannot proceed");
		return (void **)NULL;
	}

	GETMEM(p, void *, ymax-ymin+1, "ReadImage", (void **)NULL);
	p -= ymin;
	nElements = xmax - xmin + 1;
	RowSize = nElements * ElementSize;
	status = 0;
	for (n=ymin; n<=ymax; ++n) {
		if (!(p[n] = malloc(RowSize))) {
			Error("ReadImage", "Cannot allocate space for row #%u", n);
			return (void **)NULL;
		}

		/*
		 *	read in a row
		 */
		#define RDROW(a)	a(unit, 0, nElements*(n-ymin)+1, nElements,		\
							  NullValue, p[n], &anyf, &status);				\
							if (status)										\
								Error("ReadImage", "FITSIO routine " CSTR(a)\
									  " returned error #%d - aborting",		\
									  status)

		switch(type) {
			case E:
			case F:	RDROW(ffgpve);
					break;
			case D: RDROW(ffgpvd);
					break;
			case I: RDROW(ffgpvi);
					break;
			case J: RDROW(ffgpvj);
					break;
		}

		((char **)p)[n] -= ElementSize * xmin;
	}
	return p;
} /* ReadImage */


unsigned ReleaseBintable(FitsHeader_t *head, Bintable_t	*table)
{
	unsigned	n;
	FitsCard_t	*ptr1, *ptr2;

	/*
	 *	release memory of column vectors
	 */
	if (table && table->nBlockRows) {
		for (n=0; n<table->nColumns; ++n)
			free(table->cols[n]);
		memset(table, '\0', sizeof(Bintable_t));
	}

	/*
	 *	free all cards in header
	 */
	if (head) {
		ptr1 = head->first;
		while ((ptr2 = ptr1->next)) {
			free(ptr1);
			ptr1 = ptr2;
		}
		free(ptr1);
		memset(head, '\0', sizeof(FitsHeader_t));
	}

	return 0;
} /* ReleaseBintable */



unsigned ReleaseImage(void **Image, ValueTag_t type,
					  unsigned xmin, unsigned xmax,
					  unsigned ymin, unsigned ymax)
{
	unsigned	ElementSize;	/* size of an image pixel in bytes	*/

	if (!Image)
		return 0;

	if ((ElementSize = TypeSize(type))) {
		unsigned	n;

		for (n=ymin; n<=ymax; ++n)
			free(((char **)Image)[n] + ElementSize * xmin);
		free(Image+ymin);

		return 0;
	} else {
		Error("ReleaseImage", "Type of image is unkown - cannot release it");
		return 1;
	}
} /* ReleaseImage */


int ChecksumCalc(FitsFileUnit_t out)
{
   
  int i, status=0, numhdu;
  char taskname[MAXFNAME_LEN];

  get_toolnamev(taskname);

  /* Get how many hdu are present in file */

  if ( fits_get_num_hdus(out, &numhdu, &status))
    {
      headas_chat(MUTE, "%s: Error: ChecksumCalc: Unable to get the total number of HDUs\n",taskname);
      goto update_end;
    }
  
  for (i=1; i<=numhdu; i++) {

  if ( fits_movabs_hdu(out, i, NULL, &status))
    {
      headas_chat(MUTE,"%s: Error: ChecksumCalc: Unable to move in hdu %d in file\n",taskname, i);
      goto update_end;
    }
  
  if(fits_write_chksum(out, &status))
    {
      goto update_end;
    }
  }
  
  return 0;
  
  
 update_end:

  return 1;
}/* ChecksumCalc */ 

int CopyRows(Bintable_t *table, const unsigned ToRow, const unsigned FromRow)
{

  unsigned	n, jj;		/* running index		*/
  char taskname[MAXFNAME_LEN];
 
  get_toolnamev(taskname);
  
  if (ToRow != FromRow)
    for (n=0; n<table->nColumns; ++n)
      switch (table->TagVec[n]) {
      case E:
      case F:
	if(table->Multiplicity[n] > 1)
	  for (jj=0; jj< table->Multiplicity[n]; jj++)
	    EVECVEC(*table,ToRow,n,jj)=EVECVEC(*table,FromRow,n,jj);
	else
	  EVEC(*table, ToRow, n) =  EVEC(*table, FromRow, n);
	break;
      case D: 
	if(table->Multiplicity[n] > 1)
	  for (jj=0; jj< table->Multiplicity[n]; jj++)
	    DVECVEC(*table,ToRow,n,jj)=DVECVEC(*table,FromRow,n,jj);
	else
	  DVEC(*table, ToRow, n) =  DVEC(*table, FromRow, n);
	break;
      case I:
	if(!table->Tzero[n])
	  {
	    if(table->Multiplicity[n] > 1)
	      for (jj=0; jj< table->Multiplicity[n]; jj++)
		IVECVEC(*table,ToRow,n,jj)=IVECVEC(*table,FromRow,n,jj);
	    else
	      IVEC(*table, ToRow, n) =  IVEC(*table, FromRow, n);
	  }
	else
	  {
	    if(table->Multiplicity[n] > 1)
	      for (jj=0; jj< table->Multiplicity[n]; jj++)
		UVECVEC(*table,ToRow,n,jj)=UVECVEC(*table,FromRow,n,jj);
	    else
	      UVEC(*table, ToRow, n) =  UVEC(*table, FromRow, n);
	  }
	
	break;
      case B: 
	if(table->Multiplicity[n] > 1)
	  for (jj=0; jj< table->Multiplicity[n]; jj++)
	    BVECVEC(*table,ToRow,n,jj)=BVECVEC(*table,FromRow,n,jj);
	else
	  BVEC(*table, ToRow, n) =  BVEC(*table, FromRow, n);
	break;
     case J:
       if(!table->Tzero[n])
	 {
	   if(table->Multiplicity[n] > 1)
	     for (jj=0; jj< table->Multiplicity[n]; jj++)
	       JVECVEC(*table,ToRow,n,jj)=JVECVEC(*table,FromRow,n,jj);
	   else
	     JVEC(*table, ToRow, n) =  JVEC(*table, FromRow, n);
	 }
       else
	 {
	   if(table->Multiplicity[n] > 1)
	     for (jj=0; jj< table->Multiplicity[n]; jj++)
	       VVECVEC(*table,ToRow,n,jj)=VVECVEC(*table,FromRow,n,jj);
	   else
	     VVEC(*table, ToRow, n) =  VVEC(*table, FromRow, n);
	 }
       break;
       
     case L:
       if(table->Multiplicity[n] > 1)
	 for (jj=0; jj< table->Multiplicity[n]; jj++)
	   LVECVEC(*table,ToRow,n,jj)=LVECVEC(*table,FromRow,n,jj);
       else
	 LVEC(*table, ToRow, n) =  LVEC(*table, FromRow, n);
       break;
     case X: 
       
       if(table->Multiplicity[n] == 1)
	 XVEC1BYTE(*table, ToRow, n) = XVEC1BYTE(*table, FromRow, n);

       else if(table->Multiplicity[n] == 2)
	 XVEC2BYTE(*table, ToRow, n) = XVEC2BYTE(*table, FromRow, n);

       else if(table->Multiplicity[n] == 4)
	 XVEC4BYTE(*table, ToRow, n) = XVEC4BYTE(*table, FromRow, n);
       
       else
	 {
	   headas_chat(NORMAL, "%s: Error: CopyRows: Multeplicity of column #%u not handled.\n", taskname,n);
	   return NOT_OK;
	 }
       break;
     case S: SVASSGN(*table, ToRow, n, SVEC(*table, FromRow, n));
       break;
     default:
       headas_chat(NORMAL, "%s: Error: CopyRows: Datatype of column #%u unknown.\n",taskname,n);
       return NOT_OK;
      
     }

 return OK;
} /* CopyRows */

void ManageDSKeywords(FitsHeader_t *header, const char *Typ, const char *Form,const char *value)
{
  
  int        n;
  char       KeyWord[FLEN_KEYWORD];
  BOOL       found=0;
  FitsCard_t *card;


  for (n=1; !found ; n++)
    {
      sprintf(KeyWord, "%s%d",KWNM_DSTYP,n);
      if(ExistsKeyWord(header, KeyWord,&card))
	{
	  if(!strcmp(card->u.SVal, Typ))
	    {
	      AddCard(header, KeyWord, S, Typ, CARD_COMM_DSTYP);
	      sprintf(KeyWord, "%s%d" , KWNM_DSFORM , n);
	      AddCard(header, KeyWord, S, Form, CARD_COMM_DSFORM);
	      sprintf(KeyWord, "%s%d",KWNM_DSVAL,n);
	      AddCard(header, KeyWord, S, value, CARD_COMM_DSVAL);
	      found=1;
	    }
	}
      else
	{

	  AddCard(header, KeyWord, S, Typ, CARD_COMM_DSTYP);
	  sprintf(KeyWord, "%s%d",KWNM_DSFORM,n);

	  AddCard(header, KeyWord, S, Form, CARD_COMM_DSFORM);
	  sprintf(KeyWord, "%s%d",KWNM_DSVAL,n);

	  AddCard(header, KeyWord, S, value, CARD_COMM_DSVAL);
	  found=1;
	}    
    }

}/* ManageDSKeywords */
/*
 *	CopyHdu:
 *
 *
 *	DESCRIPTION:
 *        Routine to copy hdu between two files
 *     
 *
 *	DOCUMENTATION:
 *        
 *
 *      FUNCTION CALL:
 *             get_toolnamev(char *);   
 *             headas_chat(int ,char *, ...);
 *
 *          
 *             int fits_movabs_hdu(fitsfile *fptr, int hdunum, int *hdutype, int *status);
 *             int fits_copy_hdu(fitsfile *fptr, fitsfile *fptr, int morekeys, int *status);
 *
 *	CHANGE HISTORY:
 *        0.1.0: - BS 01/03/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */
int CopyHdu(FitsFileUnit_t evunit, FitsFileUnit_t outunit, int hducount)
{
  int status=OK;
  char taskname[MAXFNAME_LEN];
 
  get_toolnamev(taskname);  
   
  if(fits_movabs_hdu(evunit, hducount, NULL, &status))
    {
      headas_chat(CHATTY, "%s: Error: CopyHdu: Unable to move in  hdu num: %d.\n", taskname,hducount);
      goto hdu_end;
    }
  
  if(fits_copy_hdu(evunit, outunit, 0, &status))
    {
      headas_chat(CHATTY, "%s: Error: CopyHdu: Unable to copy %d HDU.\n",taskname, hducount);
      goto hdu_end;
    }
  
  return OK;

 hdu_end:
  return NOT_OK;
  
}/* CopyHdu */


void ManageDMKeywords(FitsHeader_t *header, const char *Typ, const char *Form,const char *comment)
{
  
  int        n;
  char       KeyWord[FLEN_KEYWORD];
  BOOL       found=0;
  FitsCard_t *card;

  for (n=1; !found ; n++)
    {
      sprintf(KeyWord, "%s%d",KWNM_MTYPE,n);
      if(!ExistsKeyWord(header, KeyWord,&card))
	{
	  AddCard(header, KeyWord, S, Typ, CARD_COMM_MTYPE);
	  
	  sprintf(KeyWord, "%s%d",KWNM_MFORM,n);
	  AddCard(header, KeyWord, S, Form, comment);
	  found=1;
	}    
    }

}/* ManageDMKeywords */


int GetAttitudeATTFLAG(char* attfile, char* attflag){
  
  char           taskname[MAXFNAME_LEN];
  FitsFileUnit_t inunit=NULL;
  FitsHeader_t   head;
  FitsCard_t     *card;
  int            status=OK;
  
  get_toolnamev(taskname);


  /* Open readonly input attitude file */
  if ((inunit=OpenReadFitsFile(attfile)) <= (FitsFileUnit_t )0)
    {
      headas_chat(NORMAL,"%s: Error: Unable to open\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n", taskname, attfile);
      goto GetAttitudeATTFLAG_end;
    }

  /* Move in ATTITUDE extension */
  if (fits_movnam_hdu(inunit, ANY_HDU,KWVL_EXTNAME_ATT, 0, &status))
    { 
      headas_chat(NORMAL,"%s: Error: Unable to find '%s' extension in\n", taskname,KWVL_EXTNAME_ATT);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n",taskname, attfile);
      goto GetAttitudeATTFLAG_end;
    }

  /* Retrieve ATTFLAG from input attitude file */    

  head=RetrieveFitsHeader(inunit);
  
  if((ExistsKeyWord(&head, KWNM_ATTFLAG, &card)))
    {
      switch(card->ValueTag){
      case J:	/* signed integer */
	sprintf(attflag, "%li", card->u.JVal);
	break;
      case I:	/* unsigned integer */
	sprintf(attflag, "%d", card->u.IVal);
	break;
      default: 	/* string */
	strcpy(attflag, card->u.SVal);
	break;
      }
    }
  else
    {
      strcpy(attflag, DEFAULT_ATTFLAG);
      headas_chat(NORMAL,"%s: Warning: GetAttitudeATTFLAG: 'ATTFLAG' keyword not found in %s\n", taskname, attfile);
      headas_chat(NORMAL,"%s: Warning: GetAttitudeATTFLAG:  using default value %s\n", taskname, DEFAULT_ATTFLAG);
    }
  
  if( CloseFitsFile(inunit))
    {
      headas_chat(NORMAL,"%s: Error: Unable to close\n", taskname);
      headas_chat(NORMAL,"%s: Error: '%s' file.\n ", taskname, attfile);
      goto GetAttitudeATTFLAG_end;
    }

  return OK;

 GetAttitudeATTFLAG_end:
  
  CloseFitsFile(inunit);
  return NOT_OK;
  
} /* GetAttitudeATTFLAG */

