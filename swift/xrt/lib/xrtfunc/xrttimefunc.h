#include  <xrtdefs.h>
#include  <math.h>
#include <pil.h>
#include <xrt_termio.h>
#include <headas_stdio.h>
#include <highfits.h>
#include <headas_utils.h>
#include <misc.h>


/*******************************
*      defines / typedefs      *
********************************/
#define PI        3.14159265358979323846
#define PIS       3.141592653
#define EPSR4     1E-7
#define MAXR4     1E38
#define MAX_INTEG 2147483647

#define TIME_PIX  0.00002175

#define OFFSETS_602 379862
#define OFFSETS_301 189931
#define OFFSETS_200 126200
#define OFFSETS_100 63100


typedef struct
{
  int diff;
  int offset;
}Offset_t;
typedef struct {
  double detx;
  double dety;
  double x;
  double y;
  double v;
  double vhat[3];
} Coord_t;

typedef struct {
  double start;
  double stop;
} GtiCol_t;

typedef struct {
  unsigned START;
  unsigned STOP;
} GtiExt_t;




int TimePix(double fst, double fet, int pixs, double *time_pix);
void TimeRow(double time1, double time2, double *time_row);
void WTTimeTag(double ro_time, int rawy,int src_dety,
	       double time_row,double *time_tag);
void WTTimeTagFirstFrame(double ro_time, double time_row,double *time_tag);
void PDTimeTag(int amp,double ro_time, int offset,int src_detx,int src_dety, double timepix,
	       double *time_tag);
int RADec2XY(double ra,double dec,int xsize,int ysize, double ranom,double decnom,double roll, double pixelsize, double *x, double *y);
int WriteGTIKeywords(FitsFileUnit_t ounit);
int UpdateGTIExt(FitsFileUnit_t outunit, int nrows, GtiCol_t *gtivalues);
int CopyGTIKeywords(FitsFileUnit_t inunit,FitsFileUnit_t outunit);
int WriteGTIExt(FitsFileUnit_t inunit, FitsFileUnit_t outunit, int nrows, GtiCol_t *gtivalues);
int UpdateGTIKeywords(FitsFileUnit_t outunit, int nrows, GtiCol_t *gti);
int SetDoubleColumnToNull(FitsFileUnit_t unit, unsigned nrows, unsigned colnum);
int CalculateNPixels(double start, double stop, int *offsets);
