/* 32 bits Pixel Quality Code

*** LSB is defined as B0 and MSB is B31!
*** Be careful that "fdump" displays the
*** MSB in the leftmost. The MSB appears
*** in the 1st column of a "fv" Vector Table.
*** Example:
*** ---------------------------------
***  MSB                          LSB
*** ---------------------------------
***   3         2         1
***  10987654321098765432109876543210  B0 - B31 definition
*** ---------------------------------
***           1         2         3
***  12345678901234567890123456789012 "fv" convention
*** ---------------------------------
*** b00000000000100000000000000000000: BAD CTI column
*** b00000000010000000000000000000000: flickering pixels
*** b00000000010100000000000000000000: both of above
 --------------------------------------

B0  reserved
B1  edge of area-dsicri area
B2  pixels two pixels apart from the segment boundary
B3  pixels two pixels apart from the preceding pixels in a bad column

B4  pixels two pixels apart from bad CTE columns
B5  pixels two pixels apart from hot pixels
B6  pixels two pixels apart from flickering pixels
B7  pixels two pixels apart from the window/frame boundary

B8  pixels one pixel apart from the segment boundary
B9  pixels at the 3rd trailing row of the SCI rows
B10 pixels one pixel apart from the preceding pixels in a bad column
B11 preceding pixels in a bad column

B12 pixels one pixel apart from bad CTE columns
B13 pixels one pixel apart from hot pixels
B14 pixels one pixel apart from flickering pixels
B15 pixels at the 2nd preceding row of the SCI rows

B16 inside the calibration mask area
B17 segment boundary
B18 pixels at the 2nd trailing row of the SCI rows
B19 pixels one pixel apart from the frame/window boundary

B20 bad CTE columns
B21 hot pixels
B22 flickering pixels
B23 frame/window boundary

B24 outside of aread-discri area
B25 other kinds of bad pixels
B26 reserved
B27 reserved

B28 pixels at the preceding row of the SCI rows
B29 pixels at the trailing row of the SCI rows
B30 pixels at the SCI rows and read with AP4 or AP256
B31 pixels at the SCI rows

*/

#ifndef _xisPixelQuality_h_
#define _xisPixelQuality_h_

#define NBITS_BCCODE			32

#define BIT_RESERVED_0			0
#define BIT_AREADISCRI_EDGE		1
#define BIT_2PIX_FROM_SEGBOUNDARY	2
#define BIT_2PIX_FROM_PRECEDING		3

#define BIT_2PIX_FROM_BADCTE		4
#define BIT_2PIX_FROM_HOT		5
#define BIT_2PIX_FROM_FLICKERING	6
#define BIT_2PIX_FROM_WINBOUNDARY	7

#define BIT_1PIX_FROM_SEGBOUNDARY	8
#define BIT_SCI_3rd_TRAILING_ROW	9
#define BIT_1PIX_FROM_PRECEDING		10
#define BIT_PRECEDING			11

#define BIT_1PIX_FROM_BADCTE		12
#define BIT_1PIX_FROM_HOT		13
#define BIT_1PIX_FROM_FLICKERING	14
#define BIT_SCI_2nd_PRECEDING_ROW	15

#define BIT_CALMASK			16
#define BIT_SEGBOUNDARY			17
#define BIT_SCI_2nd_TRAILING_ROW	18
#define BIT_1PIX_FROM_WINBOUNDARY	19

#define BIT_BADCTE        		20
#define BIT_HOT				21
#define BIT_FLICKERING			22
#define BIT_WINBOUNDARY			23

#define BIT_OUTSIDE_AREADISCRI		24
#define BIT_OTHER_BAD			25
#define BIT_RESERVED_26			26
#define BIT_RESERVED_27			27

#define BIT_SCI_PRECEDING_ROW		28
#define BIT_SCI_TRAILING_ROW		29
#define BIT_SCI_AP_ROW			30
#define BIT_SCI_ROW			31

#define BITS_RESERVED	(BIT_RESERVED_0|BIT_RESERVED_26|BIT_RESERVED_27)

typedef struct {
	double start;
	double stop;
	short actx;
	short acty1;
	short acty2;
	unsigned int bccode;	/* badcolumn code */
} BADCOL_INFO;

typedef struct {
	unsigned char image[XISactiveFrameVsize][XISactiveFrameHsize];
} CALMASK_INFO;

typedef struct {
	char *filename;
	long num_hotpix;	/* number of hotpixels in file */
	long num_added;		/* number of hotpixels added */
	long num_dupli;		/* number of hotpixels duplicated */
} HOTPIXFILE_INFO;

typedef struct {
	char instrume[FLEN_VALUE];
	char *badcol_filename;
	char *calmask_filename;
	char *hotpix_filenames;
	SCI_PARAM sci;		/* SCI_PARAM is defined in xisSciUtil.h */

	long num_badcol;
	long num_hotpixfile;
	BADCOL_INFO *badcol_list;
	HOTPIXFILE_INFO *hotpixfile_list;
	unsigned int *badcol_cache;

	CALMASK_INFO *calmask;

	double bscale;
	unsigned char *actexpo;
} PIXQ_INFO;

typedef struct {
	int flag_sel;
	unsigned int pixq_min;
	unsigned int pixq_max;
	unsigned int pixq_and;
	unsigned int pixq_eql;

	unsigned long clean_zero;
	unsigned long safe;
	unsigned long select;
	unsigned long bits[NBITS_BCCODE];
	unsigned long total;
	int safe_bit_level;		/* BIT_SEGBOUNDARY */
	unsigned bits_reserved;		/* BITS_RESERVED */
	char *cm[NBITS_BCCODE];
} PIXQ_STAT;

int xisPixelQualityInit(char *instrume, PIXQ_INFO *p);
unsigned xisPixelQuality(PIXQ_INFO *p, double t, int actx, int acty, int rawy);
int xisPixqReadBadcolFile(PIXQ_INFO *p, char *badcol_filename);
int xisPixqReadCalmaskFile(PIXQ_INFO *p, char *calmask_filename);
int xisPixqReadHotpixFiles(PIXQ_INFO *p, char *hotpix_filenames);
int xisPixqAddBadcolCache(PIXQ_INFO *p);

int xisPixqStatInit(PIXQ_STAT *s);
int xisPixqStatAdd(PIXQ_STAT *s, unsigned pixel_quality);
int xisPixqStatWrite(PIXQ_STAT *s, FILE *fp);
int xisPixqStatWriteFits(PIXQ_STAT *s, fitsfile *fp);

int xisPixqExpMapGenACT(PIXQ_INFO *p, PIXQ_STAT *s, double obstime);

#endif /* _xisPixelQuality_h_ */
