/*
filename : hcmprmf.c
purpose  : c wrapper for host cmprmf task
author   : kaa
*/
#include <stdio.h>
#ifdef unix
#define CMPRMF cmprmf_
#endif
#ifdef vms
#define CMPRMF cmprmf
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "cmprmf.par"
 
int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv,DEFAULT_PFILE);
CMPRMF();
CloseDefaultPF();
return(RETURN);
}
