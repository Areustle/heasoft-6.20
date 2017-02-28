/*
 filename: hfadapt.c
 purpose:  c - wrapper for host FADAPT task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FADAPT fadapt_
#endif
#ifdef vms
#define FADAPT fadapt 
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
 FADAPT();
 CloseDefaultPF();
 return(RETURN);
}
