/*
 filename: ascatape.c
 purpose:  c - wrapper for host ascatape task
 author:   Don Jennings, ADF, Code 631, NASA/GSFC/HSTX
*/

#include <stdio.h>

#ifdef unix
#define ASCAtape ascate_
#endif
#ifdef vms
#define ASCAtape ascate
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
 ASCAtape();
 CloseDefaultPF();
 return(RETURN);
}
