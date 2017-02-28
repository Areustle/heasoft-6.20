/*
 filename: hfhelp.c
 purpose:  c - wrapper for host FLCOL task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FHELP fhelp_
#endif
#ifdef vms
#define FHELP fhelp 
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
 FHELP();
 CloseDefaultPF();
 return(RETURN);
}
