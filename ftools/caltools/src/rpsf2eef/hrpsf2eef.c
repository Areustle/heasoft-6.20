/*
filename  : hrpsf2eef.c
purpose   : c wrapper for host rpsf2eef task
author    : Banashree Seifert
*/
#include <stdio.h>
#ifdef unix
#define RPSF2EEF rpsf2f_
#endif
#ifdef vms 
#define RPSF2EEF rpsf2f
#endif
#ifdef unix 
#define RETURN 0 
#endif
#ifdef vms
#define RETURN 1
#endif

int MAIN_;

main(argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv);
RPSF2EEF();
CloseDefaultPF();
return(RETURN);
}

