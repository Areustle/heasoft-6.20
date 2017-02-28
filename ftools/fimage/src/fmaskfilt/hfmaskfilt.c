/*
 filename: hfmaskfilt.c
 purpose:  c - wrapper for host FMASKFILT task
 author:   Emily A. Greene   4/8/93
*/

#include <stdio.h>

#ifdef unix
#define FMASKFILT fmaskt_
#endif
#ifdef vms
#define FMASKFILT fmaskt
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
 FMASKFILT(); 
 CloseDefaultPF();
 return(RETURN);
}
