/* $Id: anl_task.c,v 1.8 2007/11/01 17:21:25 ishisaki Exp $
c anl_task.c
c     Includes following routines:
c        anl_put_task_name, anl_task_name,
c        anl_put_task_version, anl_task_version,
c        anl_put_task_credits, anl_task_credits,
c        anl_put_module_num, anl_module_num,
c        anl_put_hbook_info, anl_hbook_info
c        anl_put_bnk_info, anl_bnk_info
c        anl_put_evs_info, anl_evs_info
c        anl_put_exit_status, anl_exit_status

	2004/02/08 Y.ISHISAKI	version 1.40
		created to put/get task_info

	2005/02/18 Y.ISHISAKI	version 1.60
		anl_put_module_num(), anl_module_num() move to "anl_tool.c"

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "anl_misc.h"

static struct {
	char *task_name, *task_version, *task_credits;
	int hbook_buf_size, hbook_shared_flag;
	char *hbook_shared_file;
	int bnk_buf_size, bnk_shared_flag;
	char *bnk_shared_file;
	int evs_shared_flag;
	char *evs_shared_file;
	int num_put, exit_status;
} task_info = {
	NULL, NULL, NULL,		/* task_name, task_version, task_credits */
	0, 0,					/* hbook_buf_size, hbook_shared_flag */
	NULL,					/* hbook_shared_file */
	0, 0,					/* bnk_buf_size, bnk_shared_flag */
	NULL,					/* bnk_shared_file */
	0,						/* evs_shared_flag */
	NULL,					/* evs_shared_file */
	0, 0					/* num_put, exit_status */
};

void
anl_put_task_name(char *task_name)
{
	task_info.task_name = task_name;
}

char *
anl_task_name(void)
{
	return task_info.task_name;
}

void
anl_put_task_version(char *task_version)
{
	task_info.task_version = task_version;
}

char *
anl_task_version(void)
{
	return task_info.task_version;
}

void
anl_put_task_credits(char *task_credits)
{
	task_info.task_credits = task_credits;
}

char *
anl_task_credits(void)
{
	return task_info.task_credits;
}

void
anl_put_hbook_info(int buf_size, int shared_flag, char *shared_file)
{
	task_info.hbook_buf_size = buf_size;
	task_info.hbook_shared_flag = shared_flag;
	task_info.hbook_shared_file = shared_file;
}

char *
anl_hbook_info(int *buf_size, int *shared_flag)
{
	*buf_size = task_info.hbook_buf_size;
	*shared_flag = task_info.hbook_shared_flag;
	return task_info.hbook_shared_file;
}

void
anl_put_bnk_info(int buf_size, int shared_flag, char *shared_file)
{
	task_info.bnk_buf_size = buf_size;
	task_info.bnk_shared_flag = shared_flag;
	task_info.bnk_shared_file = shared_file;
}

char *
anl_bnk_info(int *buf_size, int *shared_flag)
{
	*buf_size = task_info.bnk_buf_size;
	*shared_flag = task_info.bnk_shared_flag;
	return task_info.bnk_shared_file;
}

void
anl_put_evs_info(int shared_flag, char *shared_file)
{
	task_info.evs_shared_flag = shared_flag;
	task_info.evs_shared_file = shared_file;
}

char *
anl_evs_info(int *shared_flag)
{
	*shared_flag = task_info.evs_shared_flag;
	return task_info.evs_shared_file;
}

void
anl_put_exit_status(int exit_status)
{
	task_info.num_put++;
	task_info.exit_status = exit_status;
}

int
anl_exit_status(int *num_put, int *exit_status)
{
	if ( NULL != num_put ) {
		*num_put = task_info.num_put;
	}
	if ( 0 < task_info.num_put ) {
		if ( NULL != exit_status ) {
			*exit_status = task_info.exit_status;
		}
	}
	return task_info.num_put;
}
