/*
 filename: hseextrct.c
 purpose:  c - wrapper for host SEEXTRCT task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define SEEXTRCT seextt_
#endif
#ifdef vms
#define SEEXTRCT seextt
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
 SEEXTRCT();
 CloseDefaultPF();
 return(RETURN);
}
