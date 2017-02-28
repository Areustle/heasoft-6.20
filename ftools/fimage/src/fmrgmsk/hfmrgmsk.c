/*
 filename: hfmrgmsk.c
 purpose:  c - wrapper for host FMRGMSK task
 author:   Janice Tarrant
*/

#include <stdio.h>

#ifdef unix
#define FMRGMSK fmrgmk_
#endif
#ifdef vms
#define FMRGMSK fmrgmk
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
 FMRGMSK();
 CloseDefaultPF();
 return(RETURN);
}
