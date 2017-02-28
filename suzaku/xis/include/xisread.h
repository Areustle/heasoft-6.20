/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 21:33:25 1999 by E. Miyata*/
/*******************************************************************
 *          Includes for xisread
 *	version 4.5  2006.08.24		Y.ISHISAKI
 *              COLUMN_INF move to xisEditEventFitsUtil.h
 ******************************************************************/
#ifndef _XIS_READ_
#define _XIS_READ_

typedef struct {
  int read_out_time_h;
  int read_out_time_l;
  unsigned int read_out_time;
  int exptime;
  int aedate;
} TIMEMATCH;

#endif
