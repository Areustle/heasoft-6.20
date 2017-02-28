/* finterp constants */
#define FNTRP_CHAR_BUF   1025
#define FNTRP_BIG_BUF    2560L
#define FNTRP_SET_NULL   0
#define FNTRP_REPEAT     1
#define FNTRP_IGNORE     2

/* Error messages */
#define FNTRP_ERROR      1000 /* start of error numbers */
#define FNTRP_BAD_PAR    1001 /* bad parameter in .par file */
#define FNTRP_BAD_UNIT   1002 /* units of two columns are different */
#define FNTRP_SORT_NULL  1003 /* undefined element in sort column */
#define FNTRP_BAD_COL    1004 /* column types do not match, or bad column no.*/
#define FNTRP_BAD_ORDER  1005 /* 'order' must be 0 or 1 */
#define FNTRP_BAD_EXTRAP 1006 /* 'extrap' must be REPEAT, IGNORE, or NULL */
#define FNTRP_BAD_PTR    1007 /* function passed bad value or null pointer */
#define FNTRP_BAD_ARG    1008 /* function was passed argument with bad value */
#define FNTRP_OUT_OF_MEM 1009 /* dynamic allocation failed */

typedef struct{
	double	*d;
	char	*l;
	char	**s;
	char	*nul;
	long	numrows,
		width,
		repeat;
	int	type;
}finterp_array;

void Finterp();
int Interpolate(fitsfile *, fitsfile *, int, int, int, int, int, int, int,
	double, char *, int, int, int *);
int InitArray(finterp_array *, int, long, long, long, long *, int *);
void FreeArray(finterp_array *);
int ReadArray(fitsfile *, int, long, long, long, finterp_array *,
    int *, int *);
int CopyArray(finterp_array *, long, finterp_array *, long, long, int *);
int InterpArray(double *isort, finterp_array *in, long ini, double *osort,
    finterp_array *out, long outi, int *status);
int FillArray(finterp_array *, long, finterp_array *, long, long, int *);
int SetArrayNull(finterp_array *, long, long, int, int *);
int WriteArray(fitsfile *, int, long, long, long, finterp_array *, int *);
int CheckTime(fitsfile *infp, fitsfile *outfp, char *intime, char
    	*outtime, double *toffset, int *status);
int AppendColFormat(fitsfile *infp, fitsfile *outfp, int, int,
	int *outcol, int *status);
int CopyNumberKey(char *keyword, int typecode, fitsfile *infp,
    fitsfile *outfp, int incol, int outcol, int *status);
int ginterp(char *, char *, char *, char *, char *, char *, char *,
	int *, int *, int *, int *, int *);
int CopyThruExt(fitsfile *infp, fitsfile *outfp, int, int, int *status);
int WriteError(char *, int );
int NumListItems(char *);
int CopyListItem(char *, int, char *);
char *strip(char *, char *);
