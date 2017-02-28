/*
 filename: hbct.c
 purpose:  c - wrapper for host BCT task
 author:   Dr. Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define BCT bct_
#endif
#ifdef vms
#define BCT bct 
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
 BCT();
 CloseDefaultPF();
 return(RETURN);
}
