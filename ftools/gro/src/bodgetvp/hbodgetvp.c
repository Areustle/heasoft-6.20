/*
 filename: hbodgetvp.c
 purpose: c - wrapper for host BODGETVP task
 author/date: Peter J.T. Leonard, August, 1997
*/

#include <stdio.h>

#ifdef unix
#define BODGETVP bodgep_
#endif
#ifdef vms
#define BODGETVP bodgep
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
 BODGETVP();
 CloseDefaultPF();
 return(RETURN);
}
