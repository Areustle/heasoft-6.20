/*
 filename: heconvpha.c
 purpose: c - wrapper for host ECONVPHA task
 author/date: Peter J.T. Leonard, January, 1997
*/

#include <stdio.h>

#ifdef unix
#define ECONVPHA econva_
#endif
#ifdef vms
#define ECONVPHA econva
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
 ECONVPHA();
 CloseDefaultPF();
 return(RETURN);
}
