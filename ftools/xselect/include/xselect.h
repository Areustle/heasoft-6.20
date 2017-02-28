/* cfortran stuff */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "cfortran.h"

#define MAXNCMDS 100
#define MAXCMD 17
#define MAXPVALUE 201

#define xsel_init_ELEMS_1 ZTRINGV_NUM(1)
#define xsel_init_ELEMLEN_1 ZTRINGV_NUM(255)
PROTOCCALLSFSUB2(XSEL_INIT, xsel_init,PZTRINGV,PINT)
#define XselInit(prefix_str,lenprefix) \
CCALLSFSUB2(XSEL_INIT,xsel_init,PZTRINGV, PINT, \
prefix_str,lenprefix)

#define xsel_cmd_ELEMS_1 ZTRINGV_NUM(1)
#define xsel_cmd_ELEMLEN_1 ZTRINGV_NUM(16)
PROTOCCALLSFSUB2(XSEL_CMD, xsel_cmd,ZTRINGV,INT)
#define XselCmd(cmd,noption) \
CCALLSFSUB2(XSEL_CMD,xsel_cmd,ZTRINGV, INT,\
cmd,noption)

#define ldpospar_ELEMS_1 ZTRINGV_NUM(1)
#define ldpospar_ELEMLEN_1 ZTRINGV_NUM(16)
PROTOCCALLSFSUB1(LDPOSPAR, ldpospar,ZTRINGV)
#define LdPosPar(cmd) \
CCALLSFSUB1(LDPOSPAR, ldpospar,ZTRINGV, \
cmd)

#define ldcmdpar_ELEMS_2 ZTRINGV_NUM(1)
#define ldcmdpar_ELEMLEN_2 ZTRINGV_NUM(200)
PROTOCCALLSFSUB2(LDCMDPAR, ldcmdpar,LONG,ZTRINGV)
#define LdCmdPar(ipos,cmd) \
CCALLSFSUB2(LDCMDPAR, ldcmdpar,LONG,ZTRINGV, \
ipos,cmd)

#define prparm_ELEMS_1 ZTRINGV_NUM(1)
#define prparm_ELEMLEN_1 ZTRINGV_NUM(16)
PROTOCCALLSFSUB1(PRPARM, prparm,ZTRINGV)
#define XselLparm(cmd) \
CCALLSFSUB1(PRPARM, prparm,ZTRINGV, \
cmd)

PROTOCCALLSFSUB3(XSET_FPLOTWIN, xset_fplotwin,STRING,STRING,INT)
#define XsetFlotWin(fwin,fbkg,ffv) \
CCALLSFSUB3(XSET_FPLOTWIN, xset_fplotwin,STRING,STRING,INT,\
fwin,fbkg,ffv)

/* xpi prototypes */
int xsel_read_cmd(char** xselcmds, int* ncmd, int *status);
int cget_val(char* desc, char *type,char* parval);
int xselopenlog(char *filename);
void xselcloselog();
void writelog(char *str, int chatty); 
void copylog(char *infile, int toterminal); 
void setterm(int termset);
int toterm();
void handler(int sig);
int  qinter();
void cset_xsel_ctrl(char* varname, int val);


/* tcl/xpi interface */
extern int  logchat ;
extern int termchat ;
extern char xsl_pfix[256];
extern int ftout_buffer;

int  expand_pval(char *pval, int *status); 
void prompt_cmd(char *desc,char *def);
int receive_ans(char *type, char *ans);
void log_terminal(char *mess);
void log_file(char *mess);
void Xsl_tcl_end();
void set_xsel_tarray(char *array_name, char *index, char *val);
