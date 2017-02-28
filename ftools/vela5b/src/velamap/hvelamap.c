/*
 filename: hvelamap.c
 purpose:  c - wrapper for host VELAMAP task
 author:   Kent Blackburn (Modified for VELAMAP by Jesse Allen)
*/

#include <stdio.h>

#ifdef unix
#define VELAMAP velamp_
#endif
#ifdef vms
#define VELAMAP velamp
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
 VELAMAP();
 CloseDefaultPF();
 return(RETURN);
}
