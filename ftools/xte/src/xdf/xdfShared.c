/*

$Id: xdfShared.c,v 1.3 1997/04/02 19:29:02 elwin Exp $

$Log: xdfShared.c,v $
Revision 1.3  1997/04/02 19:29:02  elwin
Fixing that dang #define ITCL_NAMESPACES problem

Revision 1.2  1997/02/26 20:58:03  oneel
Numerious xdf updates.  FTP should work to get both index as well as
data files.

 * Revision 1.1  1997/02/12  20:04:40  oneel
 * let's do xdf shared
 *

*/

/* something's gone a bit off in itcl2.2p2+ so we need the following */
#define ITCL_NAMESPACES 1


#include <tcl.h>
#include <itcl.h>
#include "xdf.h"


/*
 * tcl_bin_copy
 */
int
xdf_tcl_bin_copy( cdata, interp, argc, argv )
  ClientData *cdata;
Tcl_Interp *interp;
int argc;
char **argv;
{
  char *buf;
  int bsize, mode, read;
  Tcl_Channel inchan, ouchan;
  
  if( argc != 4 ) return TCL_ERROR;
  
  if( sscanf(argv[3],"%d",&bsize) != 1 ) return TCL_ERROR;
  buf = (char *)malloc(bsize*sizeof(char));
  
  inchan = Tcl_GetChannel(interp, argv[1], &mode);
  if( ! ( mode & TCL_READABLE ) ) return TCL_ERROR;
  
  ouchan = Tcl_GetChannel(interp, argv[2], &mode);
  if( ! ( mode & TCL_WRITABLE ) ) return TCL_ERROR;
  
  while( ! Tcl_Eof(inchan) ){
    if( (read = Tcl_Read(inchan, buf, bsize)) == -1 ) return TCL_ERROR;
    if( Tcl_Write(ouchan, buf, read) != read ) return TCL_ERROR;
  }
  
  free(buf);
  
  return TCL_OK;
}

int Xdf_Init (interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
  if (Tcl_PkgRequire(interp, "Iwidgets", (char*)NULL, 0) == NULL) {
    return TCL_ERROR;
  }
  
  Tcl_CreateCommand (interp, "SelectObs", SelectObs,
		     (ClientData)Tk_MainWindow(interp), 
		     (Tcl_CmdDeleteProc *)NULL);
  
  Tcl_CreateCommand (interp, "SelectAppIds", SelectAppIds,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);
  
  Tcl_CreateCommand (interp, "SelectFiles", SelectFiles,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);
  
  /*
   * Special command for copying binary files.
   */
  Tcl_CreateCommand(interp, "xdfbcopy", xdf_tcl_bin_copy,
		    (ClientData *)NULL, (Tcl_CmdDeleteProc *)NULL);
  
  Tcl_SetVar(interp, "tcl_rcFileName", "~/.itkwishrc", TCL_GLOBAL_ONLY);

  
  return TCL_OK;
}




