/* $Id */

#ifndef _ANL_MSG_H_
#define _ANL_MSG_H_

#define ANL_MSG_ALWAYS		-1
#define ANL_MSG_ERROR		0
#define ANL_MSG_WARNING		1
#define ANL_MSG_INFO		2
#define ANL_MSG_DEBUG1		3
#define ANL_MSG_DEBUG2		4
#define ANL_MSG_DEBUG3		5
#define ANL_MSG_DEBUG		ANL_MSG_DEBUG1

#ifdef __cplusplus
extern "C"
{
#endif

#if defined(__GNUC__) && !defined(_ANL_MSG_C_)

#define anl_msg(ANL_MSG_LEVEL,ANL_MSG_FMT,ANL_MSG_ARGS...) \
do {\
  if ( (ANL_MSG_LEVEL) <= anl_msg_chatter() ) {\
    anl_flush();\
    fprintf( ((unsigned)(ANL_MSG_LEVEL) < ANL_MSG_INFO) ? stderr : stdout,\
        ANL_MSG_FMT, ## ANL_MSG_ARGS);\
    anl_flush();\
  }\
} while (0)

#define anl_msg_always(ANL_MSG_FMT,ANL_MSG_ARGS...) \
	anl_msg(ANL_MSG_ALWAYS, ANL_MSG_FMT, ## ANL_MSG_ARGS)

#define anl_msg_error(ANL_MSG_FMT,ANL_MSG_ARGS...) \
	anl_msg(ANL_MSG_ERROR, ANL_MSG_FMT, ## ANL_MSG_ARGS)

#define anl_msg_warning(ANL_MSG_FMT,ANL_MSG_ARGS...) \
	anl_msg(ANL_MSG_WARNING, ANL_MSG_FMT, ## ANL_MSG_ARGS)

#define anl_msg_info(ANL_MSG_FMT,ANL_MSG_ARGS...) \
	anl_msg(ANL_MSG_INFO, ANL_MSG_FMT, ## ANL_MSG_ARGS)

#define anl_msg_debug(ANL_MSG_FMT,ANL_MSG_ARGS...) \
	anl_msg(ANL_MSG_DEBUG, ANL_MSG_FMT, ## ANL_MSG_ARGS)

#define anl_msg_debug1(ANL_MSG_FMT,ANL_MSG_ARGS...) \
	anl_msg(ANL_MSG_DEBUG1, ANL_MSG_FMT, ## ANL_MSG_ARGS)

#define anl_msg_debug2(ANL_MSG_FMT,ANL_MSG_ARGS...) \
	anl_msg(ANL_MSG_DEBUG2, ANL_MSG_FMT, ## ANL_MSG_ARGS)

#define anl_msg_debug3(ANL_MSG_FMT,ANL_MSG_ARGS...) \
	anl_msg(ANL_MSG_DEBUG3, ANL_MSG_FMT, ## ANL_MSG_ARGS)
#else

/* anl_msg.c */
void anl_msg(int level, char *fmt, ...);
void anl_msg_always(char *fmt, ...);
void anl_msg_error(char *fmt, ...);
void anl_msg_warning(char *fmt, ...);
void anl_msg_info(char *fmt, ...);
void anl_msg_debug(char *fmt, ...);
void anl_msg_debug1(char *fmt, ...);
void anl_msg_debug2(char *fmt, ...);
void anl_msg_debug3(char *fmt, ...);

#endif

/* anl_msg.c */
void anl_put_msg_chatter(int chatter);
int anl_msg_chatter(void);

/* anl_flush.c */
void anl_flush(void);

#ifdef __cplusplus
}
#endif

#endif	/* _ANL_MSG_H_ */
