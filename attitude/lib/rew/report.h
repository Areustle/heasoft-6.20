/*
 * $Source: /headas/headas/attitude/lib/rew/report.h,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/29 11:46:01 $
 *
 *
 * $Log: report.h,v $
 * Revision 1.4  2005/08/29 11:46:01  rwiegand
 * Added debug message control.
 *
 * Revision 1.3  2005/02/09 18:54:07  rwiegand
 * Cleaned up #includes.
 *
 * Revision 1.2  2003/07/25 20:02:15  rwiegand
 * Updates for WCS compliance.
 *
 * Revision 1.1  2003/05/14 13:41:39  rwiegand
 * Support for logging and reading/writing FITS images and keywords.
 *
 * Revision 1.2  2002/03/21 19:41:59  rwiegand
 * Allow formatted reporting without a report_t object
 *
 * Revision 1.1  2002/02/20 14:39:17  miket
 * Merging Bob's original RCS files with HEAdas revisions
 *
 * Revision 1.2  2001/11/02 20:01:48  rwiegand
 * Ensure __func__ is available
 *
 * Revision 1.1  2001/11/01 14:44:21  rwiegand
 * Initial revision
 *
 */

#ifndef REPORT_H
#define REPORT_H 1


/* ensure __func__ is ok */
#ifdef __STDC__
#if __STDC_VERSION > 199900L
#elif defined(HAVE___func__)
#elif defined(HAVE___FUNCTION__)
#define __func__ __FUNCTION__
#else
#define __func__ 0
#endif
#else
#define __func__ 0
#endif

#include <stdarg.h>


enum
{
  REPORT_NULL,       /* do not distribute */
  REPORT_NONE,       /* no code */
  REPORT_DEFAULT,    /* leave code unchanged */
  REPORT_FLUSH,      /* flush buffers */
  REPORT_STATUS,
  REPORT_ERROR,
  REPORT_EXCEPTION,
  REPORT_WARNING,
  REPORT_VERBOSE,
  REPORT_DEBUG,
  REPORT_SILENT,
  REPORT_MONITOR,
  REPORT_CODE_DUMMY
};


enum
{
  REPORT_BAD_FUNCTION = 1,
  REPORT_ALLOCATE_FAILED,
  REPORT_ERROR_DUMMY
};


typedef struct
{
  const char * module;
  const char * file;
  int code;
  void * user;
  const char * string;
} report_t;


int report_info (report_t * info);

int add_report_function (int (*f)(report_t *));
int remove_report_function (int (*f)(report_t *));

int report_stdout (report_t * info);

int report_code (int code, const char * format, ...);
int report_detail (report_t * info, int code, const char * format, ...);

int report_status (const char * format, ...);
int report_monitor (const char * format, ...);
int report_error (const char * format, ...);
int report_warning (const char * format, ...);
int report_verbose (const char * format, ...);
int report_debug (const char * format, ...);

const char * report_code_string (int code, char * space);

int report_headas (report_t * info);
int stderr_chatter (int level);
int report_fits_errors ();


void debug_set (int flags);
int debug_test (int flags);
void debug_toggle (int flags);


#endif
