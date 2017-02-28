/* $Id: anl_msg.c,v 1.5 2007/11/01 17:21:25 ishisaki Exp $
  anl_msg.c
     Includes following routines:
        anl_msg_chatter, anl_msg_put_chatter, anl_msg
		anl_msg, anl_msg_always, anl_msg_error, anl_msg_warning, anl_msg_info,
		anl_msg_debug, anl_msg_debug1, anl_msg_debug2, anl_msg_debug3

	2005/11/28 Y.ISHISAKI	version 1.70
		created new
*/

#include <stdio.h>
#include <stdarg.h>

#define _ANL_MSG_C_		/* disable gcc macros */
#include "anl_msg.h"

static int chatter = ANL_MSG_INFO;

void
anl_put_msg_chatter(int level)
{
	chatter = (level < 0) ? 0 : level;	/* ignore negative values */
}

int
anl_msg_chatter(void)
{
	return chatter;
}

#define ANL_MSG(level, fmt) \
	if ( level <= chatter ) {\
		va_list ap;\
		va_start(ap, fmt);\
		anl_flush();\
		vfprintf((unsigned)level < ANL_MSG_INFO ? stderr : stdout, fmt, ap);\
		anl_flush();\
		va_end(ap);\
	}

void
anl_msg(int level, char *fmt, ...)
{
	ANL_MSG(level, fmt)
}

void
anl_msg_always(char *fmt, ...)
{
	ANL_MSG(ANL_MSG_ALWAYS, fmt)
}

void
anl_msg_error(char *fmt, ...)
{
	ANL_MSG(ANL_MSG_ERROR, fmt)
}

void
anl_msg_warning(char *fmt, ...)
{
	ANL_MSG(ANL_MSG_WARNING, fmt)
}

void
anl_msg_info(char *fmt, ...)
{
	ANL_MSG(ANL_MSG_INFO, fmt)
}

void
anl_msg_debug(char *fmt, ...)
{
	ANL_MSG(ANL_MSG_DEBUG, fmt)
}

void
anl_msg_debug1(char *fmt, ...)
{
	ANL_MSG(ANL_MSG_DEBUG1, fmt)
}

void
anl_msg_debug2(char *fmt, ...)
{
	ANL_MSG(ANL_MSG_DEBUG2, fmt)
}

void
anl_msg_debug3(char *fmt, ...)
{
	ANL_MSG(ANL_MSG_DEBUG3, fmt)
}

void
anl_msg_(int *level, char *message, int message_len)
{
	FILE *fp = (unsigned)(*level) < ANL_MSG_INFO ? stderr : stdout;
	anl_flush();
	fwrite(message, message_len, 1, fp);
	anl_flush();
}
