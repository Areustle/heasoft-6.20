/*
 filename: hvelabox.c
 purpose:  c - wrapper for host VELABOX task
 author:   Kent Blackburn (Modified for VELABOX by Jesse Allen)
*/

#include <stdio.h>

#ifdef unix
#define VELABOX velabx_
#endif
#ifdef vms
#define VELABOX velabx
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
 VELABOX();
 CloseDefaultPF();
 return(RETURN);
}
