/*
filename : detect.f
purpose  : c wrapper for host detect task
author   : Banashree Mitra Seifert
*/
#include <stdio.h>
#ifdef unix
#define DETECT detect_
#endif
#ifdef vms
#define DETECT detect
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_;

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv);
DETECT();
CloseDefaultPF();
return(RETURN);
}

