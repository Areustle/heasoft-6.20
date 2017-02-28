/*
 filename: hperdgrm.c
 purpose:  c - wrapper for host PERDGRM task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define PERDGRM perdgm_ 
#endif
#ifdef vms
#define PERDGRM perdgm 
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
 OpenDefaultPF(argc, argv);
 PERDGRM();
 CloseDefaultPF();
 return(RETURN);
}

