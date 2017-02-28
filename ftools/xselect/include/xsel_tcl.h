int Tcl_AppInit(Tcl_Interp *interp);
int XSLCmd(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[]);
int XSLXlog(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[]);
int XSLLparm(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[]);
int XSLLcmd(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[]);
int XSLScript(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[]);
int XSLTerm(ClientData clientdata, Tcl_Interp *interp, int argc,
                 char *argv[]);

extern int nothercmd;
extern char* othercmd[];

extern Tcl_Interp *xsl_interp;  /* global xselect tcl pointer */

extern Tcl_Channel Xsl_Prompt_Chan; /* xselect parameter prompt 
                                       output channel */
extern Tcl_Channel Xsl_Cmd_Chan;    /* xselect command input 
				       channel */
extern Tcl_Channel Xsl_Log_Chan;    /* xselect message out channel */
extern Tcl_Channel Xsl_Std_Chan;    /* xselect message stdout/stderr 
                                            channel */
