/*
 filename: hfarith.c
 purpose:  c - wrapper for host FARITH task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FARITH farith_
#endif
#ifdef vms
#define FARITH farith 
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
 FARITH();
 CloseDefaultPF();
 return(RETURN);
}
