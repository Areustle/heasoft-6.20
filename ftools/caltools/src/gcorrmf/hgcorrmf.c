/*
filename : hgcorrmf.c
purpose  : c wrapper for host gcorrmf task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define GCORRMF gcorrf_
#endif
#ifdef vms
#define GCORRMF gcorrf
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "gcorrmf.par"
 
int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv,DEFAULT_PFILE);
GCORRMF();
CloseDefaultPF();
return(RETURN);
}
