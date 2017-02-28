/*
 filename: hfkeytab.c
 purpose:  c - wrapper for host FKEYTAB task
 author:   James Kent Blackburn
*/

#include <stdio.h>

#ifdef unix
#define FKEYTAB fkeytb_
#endif
#ifdef vms
#define FKEYTAB fkeytb
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
 FKEYTAB();
 CloseDefaultPF();
 return(RETURN);
}
