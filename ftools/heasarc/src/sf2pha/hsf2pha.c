/*
filename : hsf2pha.f
purpose  : c wrapper for host sf2pha task
author   : Ian M. George 
*/
#include <stdio.h>
#ifdef unix
#define SF2PHA sf2pha_ 
#endif
#ifdef vms
#define SF2PHA sf2pha 
#endif
#ifdef unix
#define RETURN 0
#endif
#ifdef vms
#define RETURN 1
#endif

#define DEFAULT_PFILE "sf2pha.par"

int MAIN_;

main (argc,argv)
int argc;
char **argv;
{
OpenDefaultPF(argc,argv,DEFAULT_PFILE);
SF2PHA();
CloseDefaultPF();
return(RETURN);
}

