/*
 filename: hfplot.c
 purpose:  c - wrapper for host FPLOT task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FPLOT fplot_
#endif
#ifdef vms
#define FPLOT fplot 
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
 FPLOT();
 CloseDefaultPF();
 return(RETURN);
}
