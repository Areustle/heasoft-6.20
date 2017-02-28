/*
 filename: heconvrmf.c
 purpose: c - wrapper for host ECONVRMF task
 author/date: Peter J.T. Leonard and Jeffrey M.S. Silvis, March, 1997
*/

#include <stdio.h>

#ifdef unix
#define ECONVRMF econvf_
#endif
#ifdef vms
#define ECONVRMF econvf
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
 ECONVRMF();
 CloseDefaultPF();
 return(RETURN);
}
