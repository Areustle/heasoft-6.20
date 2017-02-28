/*
filename : hdmprmf.c
purpose  : c wrapper for host dmprmf task
author   : Rehana Yusaf
*/
#include <stdio.h>
#ifdef unix
#define DMPRMF dmprmf_
#endif
#ifdef vms
#define DMPRMF dmprmf
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif
 
int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv);
DMPRMF();
CloseDefaultPF();
return(RETURN);
}
