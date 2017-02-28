/* $Id: anl_misc.h,v 1.8 2005/02/23 06:59:08 ishisaki Exp $ */

#ifndef _ANL_MISC_H_
#define _ANL_MISC_H_

/*------------------------------------------------------------------
fortran filename   : anl_lun.f
------------------------------------------------------------------*/

/********begin line 1**(anl_lun.f)********
====> SUBROUTINE ANL_GETLUN( LUN )
0005> INTEGER  LUN
0008> INTEGER INT_LUN / 99 /
0011> LUN = INT_LUN
--- subroutine ANL_GETLUN * 1 args *

(01) LUN        |        PINT | dim:00 | out:1
----------end line 14---------*/

#ifdef CCALLSFSUB1
#define ANL_GETLUN(A1)  CCALLSFSUB1(ANL_GETLUN,anl_getlun,PINT,A1)
#endif

/********begin line 17**(anl_lun.f)********
====> SUBROUTINE ANL_FREELUN( LUN )
0021> INTEGER LUN
--- subroutine ANL_FREELUN * 1 args *

(01) LUN        |         INT | dim:00 | out:0
----------end line 24---------*/

#ifdef CCALLSFSUB1
#define ANL_FREELUN(A1)  CCALLSFSUB1(ANL_FREELUN,anl_freelun,INT,A1)
#endif

/*------------------------------------------------------------------
fortran filename   : anl_version.f
------------------------------------------------------------------*/

/********begin line 3**(anl_version.f)********
====> SUBROUTINE ANL_PUT_VERSION(SUBPROG, VERSION_NUMBER)
0008> CHARACTER*(*) SUBPROG, VERSION_NUMBER
0010> INTEGER I
0011> LOGICAL FOUND
--- subroutine ANL_PUT_VERSION * 2 args *

(01) SUBPROG    |      STRING | dim:00 | out:0
(02) VERSION_NUMBER |      STRING | dim:00 | out:0
--- arg. string
(01) SUBPROG
	len    : 255
	dim    : 0
(02) VERSION_NUMBER
	len    : 255
	dim    : 0
----------end line 25---------*/
/*
#define anl_put_version_ELEMS_1          ZTRINGV_NUM(1)
#define anl_put_version_ELEMLEN_1        ZTRINGV_NUM(255)
#define anl_put_version_ELEMS_2          ZTRINGV_NUM(1)
#define anl_put_version_ELEMLEN_2        ZTRINGV_NUM(255)
*/

#ifdef CCALLSFSUB2
#define ANL_PUT_VERSION(A1,A2)  CCALLSFSUB2(ANL_PUT_VERSION,anl_put_version,STRING,STRING,A1,A2)
#endif

/* anl_tool.c */

#define ANL_VERBOSE_FULL	-1
#define ANL_VERBOSE_NONE	0
#define ANL_VERBOSE_EVENT	1
#define	ANL_VERBOSE_CHAIN	2
#define	ANL_VERBOSE_EVS		4
#define	ANL_VERBOSE_BNK		8

/* anl_body.c */

extern char *anl_version;

#ifdef __cplusplus
extern "C"
{
#endif

/* anl_flush.c */
void anl_flush(void);

/* anl_tool.c */
void anl_put_module_num(int module_num);
int anl_module_num(void);
int anl_current_module(void);
void anl_put_verbose_level(int verbose);
int anl_verbose_level(void);
void anl_put_module_list(void *p);
char *anl_routine_name(int imodule);
char *anl_routine_version(int imodule);
int anl_routine_flag(int imodule);
void anl_routine_startup(int imodule, int *status);
void anl_routine_init(int imodule, int *status);
void anl_routine_com(int imodule, int *status);
void anl_routine_his(int imodule, int *status);
void anl_routine_bgnrun(int imodule, int *status);
void anl_routine_ana(int imodule, int *nevent, int *eventid, int *status);
void anl_routine_endrun(int imodule, int *status);
void anl_routine_exit(int imodule, int *status);
void anl_put_version(char *name, char *version);
void anl_getlun(int *lun);
void anl_freelun(int lun);
void anl_put_profile_flag(int flag);
int anl_profile_flag(void);
void anl_profile_reset(void);
void anl_profile_update(int imodule);
void anl_profile_module(int imodule, double *utime, double *stime);

/* anl_task.c */
void anl_put_task_name(char *task_name);
char *anl_task_name(void);
void anl_put_task_version(char *task_version);
char *anl_task_version(void);
void anl_put_task_credits(char *task_credit);
char *anl_task_credits(void);
void anl_put_hbook_info(int buf_size, int shared_flag, char *shared_file);
char *anl_hbook_info(int *buf_size, int *shared_flag);
void anl_put_bnk_info(int buf_size, int shared_flag, char *shared_file);
char *anl_bnk_info(int *buf_size, int *shared_flag);
void anl_put_evs_info(int shared_flag, char *shared_file);
char *anl_evs_info(int *shared_flag);
void anl_put_exit_status(int exit_status);
int anl_exit_status(int *num_put, int *exit_status);	/* returns num_put */

/* anl_main.c */
int anl_main(int argc, char **argv);

/* anl_body.c */
int anl_body(void);

#ifdef __cplusplus
}
#endif

#endif	/* _ANL_MISC_H_ */
