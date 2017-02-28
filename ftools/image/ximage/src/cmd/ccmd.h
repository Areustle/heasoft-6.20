
Tcl_ObjCmdProc chheader, drawcmd, gridcmd, curvecmd, selectcmd, wcscmd, remapcmd;

#define NCCMD 6

static char* ccmd[NCCMD]= {
   "chheader", 
   "draw",
   "grid",
   "curve",
   "select",
   "wcs"
};
static Tcl_ObjCmdProc* ccmdfnc[NCCMD]= {
   chheader, 
   drawcmd,
   gridcmd,
   curvecmd,
   selectcmd,
   wcscmd
};
