/*
 filename: hftabpar.c
 purpose:  c - wrapper for host FTABPAR task
 author:   James Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FTABPAR ftabpr_
#endif
#ifdef vms
#define FTABPAR ftabpr 
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
 FTABPAR();
 CloseDefaultPF();
 return(RETURN);
}
