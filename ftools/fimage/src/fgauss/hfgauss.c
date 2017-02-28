/*
 filename: hfgauss.c
 purpose:  c - wrapper for host FGAUSS task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FGAUSS fgauss_
#endif
#ifdef vms
#define FGAUSS fgauss 
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
 FGAUSS();
 CloseDefaultPF();
 return(RETURN);
}
