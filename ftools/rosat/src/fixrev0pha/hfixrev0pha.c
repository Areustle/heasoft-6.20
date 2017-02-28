/*
 filename: hpcarf.c
 purpose:  c - wrapper for host FARITH task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FIXREV0PHA fixrea_
#endif
#ifdef vms
#define FIXREV0PHA fixrea 
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
 FIXREV0PHA();
 CloseDefaultPF();
 return(RETURN);
}
