/*
 filename: hfpartab.c
 purpose:  c - wrapper for host FPARTAB task
 author:   James Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FPARTAB fpartb_
#endif
#ifdef vms
#define FPARTAB fpartb 
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
 FPARTAB();
 CloseDefaultPF();
 return(RETURN);
}
