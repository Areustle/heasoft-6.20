/*
 filename: hfparkey.c
 purpose:  c - wrapper for host FPARKEY task
 author:   James Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FPARKEY fparky_
#endif
#ifdef vms
#define FPARKEY fparky 
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
 FPARKEY();
 CloseDefaultPF();
 return(RETURN);
}
