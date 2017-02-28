/*
 filename: hsrc2pha.c
 purpose:  c - wrapper for host FARITH task
 author:   Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define SRC2PHA src2pa_
#endif
#ifdef vms
#define SRC2PHA src2pa 
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
 SRC2PHA();
 CloseDefaultPF();
 return(RETURN);
}
