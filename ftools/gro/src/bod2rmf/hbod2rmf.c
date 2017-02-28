/*
 filename: hbod2rmf.c
 purpose: c - wrapper for host BOD2RMF task
 author/date: Peter J.T. Leonard, RITSS, September, 1998
*/

#include <stdio.h>

#ifdef unix
#define BOD2RMF bodrmf_
#endif
#ifdef vms
#define BOD2RMF bodrmf
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
 BOD2RMF();
 CloseDefaultPF();
 return(RETURN);
}
