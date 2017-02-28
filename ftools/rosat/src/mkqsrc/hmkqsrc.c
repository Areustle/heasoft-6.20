/*
 filename: hmkqsrc.c
 purpose:  c - wrapper for host FARITH task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define MKQSRC mkqsrc_
#endif
#ifdef vms
#define MKQSRC mkqsrc 
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
 MKQSRC();
 CloseDefaultPF();
 return(RETURN);
}
