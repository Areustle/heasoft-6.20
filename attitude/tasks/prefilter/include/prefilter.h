#ifndef PREFILTER_H
#define PREFILTER_H

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/prefilter.h,v $
 * $Revision: 1.8 $
 * $Date: 2004/12/30 22:56:26 $
 *
 *
 * $Log: prefilter.h,v $
 * Revision 1.8  2004/12/30 22:56:26  rwiegand
 * Implemented writing NULLs for parameters which can not be calculated
 * at particular timestamps because of missing dependencies (e.g., pointing
 * vector cannot be found without attitude).  Require an attitude record
 * within attextrap of output record time or set attitude (and attitude
 * dependent parameters) to NULL.
 *
 * Revision 1.7  2003/11/26 21:06:37  rwiegand
 * Updated parameter stamping call.
 *
 * Revision 1.6  2003/06/23 22:57:38  rwiegand
 * Added iteration code that indicates that current iteration should be skipped.
 *
 * Revision 1.5  2003/01/22 17:32:43  rwiegand
 * Added parameter for writing HISTORY keywords to output.  Fixed conversion
 * of string to AtTime.
 *
 * Revision 1.4  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 * Revision 1.3  2002/03/21 19:41:23  rwiegand
 * Added support for FITS header keywords
 *
 * Revision 1.2  2002/03/15 18:28:18  rwiegand
 * Added clobber parameter
 *
 * Revision 1.3  2002/01/31 14:00:15  rwiegand
 * Renamed tool prefilter
 *
 * Revision 1.2  2002/01/28 19:19:43  rwiegand
 * Provide short/long names for prefilter definitions
 *
 * Revision 1.1  2002/01/16 20:18:16  rwiegand
 * Initial revision
 *
 */


#include <stddef.h>

#include "config.h"    /* offsetof */


#ifdef PREFILTER_NAMESPACE

#define Filter         Prefilter
#define Details        PrefilterDetails
#define Parameter      PrefilterParameter
#define Derivation     PrefilterDerivation
#define Context        PrefilterContext
#define Arguments      PrefilterArguments
#define Output         PrefilterOutput
#define Keyword        PrefilterKeyword
#define KeywordBuffer  PrefilterKeywordBuffer

#define add_dependency           prefilter_add_dependency
#define add_keyword              prefilter_add_keyword
#define create_parameter         prefilter_create_parameter
#define define_parameter         prefilter_define_parameter
#define put_keywords             prefilter_put_keywords

#endif

/*
 * prefilter return codes
 */

typedef enum
{
	PREFILTER_OK,
	PREFILTER_ERROR,
	PREFILTER_DONE,
	PREFILTER_SKIP,
	PREFILTER_SETUP_ERROR,
	PREFILTER_DERIVE_ERROR,
	PREFILTER_FITS_ERROR,
	PREFILTER_OUTPUT_ERROR,
	PREFILTER_CODE_DUMMY
} PrefilterCode;


/*
 * prefilter parameter modes
 */

typedef enum
{
	PREFILTER_IGNORE,           /* parameter not derived */
	PREFILTER_OUTPUT,           /* parameter output */
	PREFILTER_TRANSIENT,        /* parameter derived, but not written */
	PREFILTER_MODE_DUMMY
} PrefilterMode;


/*
 * Type constants.  These will match the (primitive) types used in
 * Derived.  Necessary until C adopts a typeof operator.
 * Used for conversion from derived type to FITS type.
 */

typedef enum
{
	PREFILTER_VOID,
	PREFILTER_CHAR,
	PREFILTER_SHORT,
	PREFILTER_INT,
	PREFILTER_LONG,
	PREFILTER_FLOAT,
	PREFILTER_DOUBLE,
	PREFILTER_TYPE_DUMMY
} PrefilterType;


struct Filter;
typedef struct Filter Filter;

struct Details;
typedef struct Details Details;

struct Parameter;
typedef struct Parameter Parameter;

/*
 * Arguments, Context, and Derived are defined by
 * the application
 */
struct Arguments;
typedef struct Arguments Arguments;

struct Context;
typedef struct Context Context;

struct Derived;
typedef struct Derived Derived;

typedef int Iterator (const Arguments * args, Context * context);

typedef int Derivation (const Context *, Derived *);



typedef struct
{
	char name[80];
	char comment[80];
	int type;
	void * pvoid;
	union
	{
		long lvalue;
		int ivalue;
		short svalue;
		unsigned char bvalue;
		float fvalue;
		double dvalue;
		char string[80];
	} value;
} Keyword;



/*
 * The user sets the name, derivation, dependencies, derived{Offset,Type},
 * fits{Type,Units,Comment} fields and registers parameters with
 *	define_parameter (Filter *, Parameter *);
 */
struct Parameter
{
	int mode;                  /* see enum PrefilterMode */
	int defined;               /* definition order */

	const char * name;
	Derivation * derivation;
	Iterator * initialization;
	Parameter ** dependencies; /* (NULL terminated) list of dependencies
	                              for this parameter */

	size_t derivedOffset;      /* see macro PREFILTER_OFFSET */
	int derivedType;           /* see enum PrefilterType */
	                           /* Must match Derived member */

	/* FITS column data */
	const char * fitsName;     /* name of FITS column */
	int fitsType;              /* see FITS data types in fitsio.h */
	int fitsArray;             /* array length */
	const char * fitsUnits;
	const char * fitsComment;
	Keyword * fitsNull;
	int fitsColumn;            /* column index (determined at creation) */
};


/*
 * Macro for use when initializing the Parameter.derivedOffset field.
 * Determines the offset of the given member (in bytes) from the start
 * of the Derived structure.
 */

#define PREFILTER_OFFSET(member) \
		offsetof(Derived, member)


struct Output;
typedef struct Output Output;

struct Output
{
	const char * outname;        /* output file name */
	const char * extension;      /* name of table extension
	                                 (default PREFILTER) */
	int clobber;
	int history;

	const char ** parameters;    /* parameters to output */
};


typedef struct
{
	int hdu;               /* which FITS HDU to update */

	int timestamp;         /* flag for fits_xxx DATE keyword */
	int checksums;         /* flag for fits_add_chksm */

	int allocated;
	int used;
	int heap;
	Keyword * keywords;

} KeywordBuffer;



struct Filter
{
	Details * details;   /* the caller should not touch this */

	Arguments * arguments;
	Context * context;
	Derived * derived;
	Output * output;

	int (* initialization)(Arguments * args, Filter * filter);
	int (* finalization)(Arguments * args, Filter * filter);

	int (* argument_code)(const Arguments * args);
	int (* context_code)(const Context * context);
	int (* derived_code)(const Derived * derived);

	int (* iterate)(const Arguments * args, Context * context);
};


/*
 * These functions are provided by the prefilter system
 */

int add_keyword (KeywordBuffer * buffer, int type, const char * name,
		const void * value, const char * comment);
Keyword * create_keyword (int type, const char * name,
		const void * value, const char * comment);
int initialize_keyword (Keyword * pkeyword, int type, const char * name,
		const void * value, const char * comment);
int put_keywords (Filter * filter, KeywordBuffer * buffer);
void release_keyword_buffer (KeywordBuffer * buffer);

Parameter * create_parameter (const char * name);
Parameter ** create_dependency_list (int length);

/* register a parameter with the setup object (transfers ownership) */
int define_parameter (Filter * filter, Parameter * parameter);


int prefilter_execute (Filter * filter);

int filter_code (const Filter * filter);
int argument_code (const Filter * filter);
int context_code (const Filter * filter);
int derived_code (const Filter * filter);

void clear_context_code (Context * context);


#endif
