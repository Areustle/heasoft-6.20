#define MAXEXCLUDE 100          /* Maximum number of keywords which
                                   could be excluded from comparison.*/
#define FDFCOM  1024		/* Max length of ftdiff file title banners, comments,
				   and excluded keyword list */
#define DIFFERR    1            /* fdiff error exit */
#define DIFFWRN    2            /* fdiff warning exit */
#define DIFFMAX    3            /* # of difference exceeds the maximum */
typedef struct {
    char kname[FLEN_KEYWORD];   /* fits keyword name */
    char * kvalue ;    /* fits keyword name */
/*    char kvalue[FLEN_VALUE]; */   /* fits keyword name */
    int kindex;                 /* position at the header */
}FitsKey;

typedef struct {
    char col_name[FLEN_VALUE];   /* Column  name    */
    int  col_num;                /* Column position */
    int  col_type;               /* Column type     */
    long col_repeat;             /* column repeat   */
    long col_width;              /* column width    */
}FitsCol;

static char comm[FDFCOM];
static char title[FDFCOM];
static char task[8];
static char version[8];
static int chatter;
static int cmpnum = 1;          /* compare the numerical keyword value? */
static int cmpdata = 1;         /* compare the table columns */
static int caldsum = 1;         /* calculate the data sum */
static int hdumaxdiff = 999;      /* maximum number of the difference
                                     allowed in a HDU. */
static int hdundiff = 0;

static double tolerance = 0;    /* allowed difference in numerical elements */
static double reltol = 0;       /* allowed relative difference */

