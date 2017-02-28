/*
 filename: hfimconcat.c
 purpose:  c - wrapper for host FARITH task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FIMCONCAT fimcot_
#endif
#ifdef vms
#define FIMCONCAT fimcot 
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
 FIMCONCAT();
 CloseDefaultPF();
 return(RETURN);
}
