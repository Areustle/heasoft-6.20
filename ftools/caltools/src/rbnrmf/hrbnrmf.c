/*
filename : hrbnrmf.c
purpose  : c wrapper for host rbnrmf task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define RBNRMF rbnrmf_
#endif
#ifdef vms
#define RBNRMF rbnrmf
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "rbnrmf.par"
 
int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv,DEFAULT_PFILE);
RBNRMF();
CloseDefaultPF();
return(RETURN);
}
