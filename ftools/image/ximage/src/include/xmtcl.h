#ifndef XMTCL_H
#define XMTCL_H

#include <tcl.h>

#ifdef _TCL
#undef VOID
#endif

/*
 *  XIMAGE interpretter is global variable, so may be accessed anywhere
 */
extern Tcl_Interp *xm_interp;

/*
 * TCL dynamic string for reading lines prompted for
 * from the standard input.
 */
extern Tcl_DString *xm_tcl_line;

/*
 * Tcl and linked C variable as to whether to echo commands from a
 * script.
 */
extern int xm_echo_script;

/*
 * TCL Channels used for implementing log and script files
 */
extern Tcl_Channel XM_Stdout_Chan;
extern Tcl_Channel XM_Stderr_Chan;
extern Tcl_Channel XM_Logfile_Chan;
extern Tcl_Channel XM_Script_Chan;

/*
 * Names for the TCL logging an scripting channels.
 */
extern char* xm_log_out;
extern char* xm_log_err;
extern char* xm_script_file;
extern char* xm_read_line;

/* tcl procedures for handling log channel */

extern int xm_input_log(ClientData, char*, int, int*);
extern int xm_close_log(ClientData, Tcl_Interp*);
extern int xm_output_log(ClientData, char*,int,int*);
extern int xm_watch_log(ClientData, int);
extern int xm_handle_log(ClientData,int,ClientData*);

/* tcl command function definitions */

Tcl_ObjCmdProc xm_log;

/* tcl i/o */

extern int xm_tcl_write(char *, int);
extern int xm_tcl_read(char *, char *, int *);


#endif

/*
 *  Tcl variable and command results management prototypes
 *    Called same as FORTRAN counterparts for consistent behavior
 */

void tclunset(const char *name, int isglobal, int *status);
void tclvari(const char *name, int value, int isreadonly, int isglobal, 
             int *status);
void tclvarl(const char *name, int value, int isreadonly, int isglobal, 
             int *status);
void tclvars(const char *name, const char *value, int isreadonly, int isglobal, 
             int *status);
void tclvarr(const char *name, float value, int isreadonly, int isglobal, 
             int *status);
void tclvard(const char *name, double value, int isreadonly, int isglobal, 
             int *status);
void tclavari(const char *name, int value, int isreadonly, int isglobal, 
              int *status);
void tclavarl(const char *name, int value, int isreadonly, int isglobal, 
              int *status);
void tclavars(const char *name, const char *value, int isreadonly, 
              int isglobal, int *status);
void tclavarr(const char *name, float value, int isreadonly, int isglobal, 
              int *status);
void tclavard(const char *name, double value, int isreadonly, int isglobal, 
              int *status);
void tclvarlr(const char *name, float *valary, int valnum, int isreadonly, 
              int isglobal, int *status);
void tclresi(const char *cmd, int *value, int *status);
void tclresl(const char *cmd, int *value, int *status);
void tclress(const char *cmd, char *value, int maxlen, int *status);
void tclresr(const char *cmd, float *value, int *status);
void tclresd(const char *cmd, double *value, int *status);
void tclreslr(const char *cmd, float *valary, int *valnum, int maxlen, 
              int *status);
void tclresld(const char *cmd, double *valary, int *valnum, int maxlen, 
              int *status);
void tclresobj(const char *cmd, Tcl_Obj **valObjPtr, int *status);
