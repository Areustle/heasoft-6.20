/*
filename : hgcorpha.c
purpose  : c wrapper for host gcorpha task
author   : Banashree Seifert
*/
#include <stdio.h>
#ifdef unix
#define GCORPHA gcorpa_
#endif
#ifdef vms
#define GCORPHA gcorpa
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "gcorpha.par"
 
int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv,DEFAULT_PFILE);
GCORPHA();
CloseDefaultPF();
return(RETURN);
}
