/*
 filename: hftabcopy.c
 purpose:  c - wrapper for host FTABCOPY task
 author:   Emily A. Greene
*/

#include <stdio.h>

#ifdef unix
#define FTABCOPY ftabcy_
#endif
#ifdef vms
#define FTABCOPY ftabcy 
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
 FTABCOPY();
 CloseDefaultPF();
 return(RETURN);
}
