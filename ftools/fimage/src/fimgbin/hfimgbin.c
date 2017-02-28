/*
 filename: hfimgbin.c
 purpose:  c - wrapper for host FIMGBIN task
 author:   Dr. James Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FIMGBIN fimgbn_
#endif
#ifdef vms
#define FIMGBIN fimgbn 
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
 FIMGBIN();
 CloseDefaultPF();
 return(RETURN);
}
