/*
 filename: hfmedian.c
 purpose:  c - wrapper for host FMEDIAN task
 author:   Banashree M Seifert
*/

#include <stdio.h>

#ifdef unix
#define FMEDIAN fmedin_
#endif
#ifdef vms
#define FMEDIAN fmedin 
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
 FMEDIAN();
 CloseDefaultPF();
 return(RETURN);
}
