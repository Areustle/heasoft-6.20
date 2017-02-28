/*
 filename: hascaray.c
 purpose:  c - wrapper for host ASCARAY task
 author/date:   Richard Fink 631 March 10 1997
		James Peachey August 1996
*/

#include <stdio.h>

#ifdef unix
#define ASCARAY ascary_
#endif
#ifdef vms
#define ASCARAY ascary 
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
 ASCARAY();
 CloseDefaultPF();
 return(RETURN);
}
