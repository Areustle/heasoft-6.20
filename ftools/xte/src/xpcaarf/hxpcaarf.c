/*
 filename: hxpcaarf.c
 purpose:  c - wrapper for host XPCAARF task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define XPCAARF xpcaaf_
#endif
#ifdef vms
#define XPCAARF xpcaaf 
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
 XPCAARF();
 CloseDefaultPF();
 return(RETURN);
}
