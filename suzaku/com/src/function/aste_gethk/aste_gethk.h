/* aste_gethk.h */

#ifndef _ASTE_GETHK_
#define _ASTE_GETHK_

#define	ASTE_GETHK_MAX_HKFILE_NUM	256
#define ASTE_GETHK_GTI_ERROR		-2

/* list of HK files */
typedef struct {
	char *filenames[ASTE_GETHK_MAX_HKFILE_NUM];	/* HK file names */
	int num_file;					/* number of files */
} ASTE_HK;

/************************************************************************
	aste_gethk_init()	set HK file list

	RETURN VALUE
		NULL:	Error
		others:	pointer to ASTE_HK structure
*************************************************************************/
ASTE_HK *aste_gethk_init(
	char *filelist	/* input: an HK file to read or
			          a list of HK files when start with "@" */
	);

/************************************************************************
	aste_gethk_id()		get id for specified HK name

	RETURN VALUE
		0 or positive values:		successs
		negative values (-1):		memory allocation error
*************************************************************************/
int aste_gethk_id(
	ASTE_HK *aste_hk,	/* input: HK struct */
	char *hk_name		/* input: HK name */
	);

/************************************************************************
	aste_gethk()	return HK value

	RETURN VALUE
		ANL_OK (0):	success
		ANL_NG (-1):	not found / illegal HK id
		others:		CFITSIO error
*************************************************************************/
int aste_gethk(
	int id,			/* input: ID number of HK item	*/
	double aste_time,	/* input: aste_time */
	int typecode,		/* input: TINT, TDOUBLE, etc */
	int nelem,		/* input: number of elements to read */
	void *value,		/* output: HK value */
	double *stime		/* output: time iroiro
   					stime[0]: time when value was set,
   					stime[1]: previous time when HK is put,
   					stime[2]: next time when HK is put
		   		*/
	);

/************************************************************************
	aste_gethk_register()	register HK item to read (obsolete)

	RETURN VALUE
		ANL_TRUE (1):	success
		ANL_FALSE (0):	memory allocation error
*************************************************************************/
int aste_gethk_register(
	ASTE_HK *aste_hk,	/* input: HK struct */
	char *data_name,	/* input: HK name */
	int *id			/* output: ID of HK item */
	);

/************************************************************************
	aste_gethk_int()	return HK value with integer (obsolete)

	RETURN VALUE
		ANL_TRUE (1):	success
		ANL_FALSE (0):	not found / illegal HK id
*************************************************************************/
int aste_gethk_int(
	ASTE_HK *aste_hk,	/* input: HK struct (not used) */
	char *hk_name,		/* input: HK name (not used) */
	int *id_ptr,		/* input: ID number of HK item	*/
	double aste_time,	/* input: aste_time */
	int *value,		/* output: HK value */
	double *stime		/* output: time iroiro */
	);

/************************************************************************
	aste_gethk_double()	return HK value with double (obsolete)

	RETURN VALUE
		ANL_TRUE (1):	success
		ANL_FALSE (0):	not found / illegal HK id
*************************************************************************/
int aste_gethk_double(
	ASTE_HK *aste_hk,	/* input: HK struct (not used) */
	char *hk_name,		/* input: HK name (not used) */
	int *id_ptr,		/* input: ID number of HK item	*/
	double aste_time,	/* input: aste_time */
	double *value,		/* output: HK value */
	double *stime		/* output: time iroiro */
	);

#endif
