/* $Id: anl.h,v 1.9 2007/04/30 21:24:40 ishisaki Exp $ */

#ifndef _ANL_H_
#define _ANL_H_

#define ANL_OK		0
#define ANL_NG		(-1)

#define ANL_YES		1
#define ANL_NO		0

#define ANL_TRUE	1
#define ANL_FALSE	0

#define ANL_ENA		1
#define ANL_DIS		0

#define ANL_ON		1
#define ANL_OFF		0

/* flow control (obsolete) */
#define ASCA_ANL_OK     (0)
#define ASCA_ANL_QUIT   (-1)
#define ASCA_ANL_SKIP   (-3)
#define ASCA_ANL_LOOP   (-4)
#define ANL_QUIT   (-1)
#define ANL_ERROR  (-2)
#define ANL_SKIP   (-3)
#define ANL_LOOP   (-4)
/* flow control (new) */
#define ANL_ENDLOOP	1
#define ANL_DISCARD	2
#define ANL_NOCOUNT	4
#define ANL_NEWROOT	8

/* event */
#define EVENTID_OBS     (0)
#define EVENTID_PSEUDO	(-1)

/* sensor ID */
#define SENSOR_PSEUDO	(-1)

#ifdef __cplusplus
extern "C"
{
#endif

/* anl_flush.c */
void anl_flush(void);

/* anl_task.c */
char *anl_task_name(void);
char *anl_task_version(void);
char *anl_task_credits(void);
void anl_put_exit_status(int exit_status);
int anl_exit_status(int *num_put, int *exit_status);	/* returns num_put */

#ifdef __cplusplus
}
#endif

#include "anl_msg.h"

#endif	/* _ANL_H_ */
