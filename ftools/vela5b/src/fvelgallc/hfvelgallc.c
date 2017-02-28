/*
 filename: hfvelgallc.c
 purpose:  c - wrapper for host FVELGALLC task
 author:   Kent Blackburn (Modified for FVELGALLC by Jesse Allen)
*/

#include <stdio.h>

#ifdef unix
#define FVELGALLC fvelgc_
#endif
#ifdef vms
#define FVELGALLC fvelgc
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
 FVELGALLC();
 CloseDefaultPF();
 return(RETURN);
}
