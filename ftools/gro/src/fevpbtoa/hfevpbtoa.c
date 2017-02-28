/*
 filename: hfevpbtoa.c
 purpose:  c - wrapper for host FEVPBTOA task
 author:   Mark Cresitello-Dittmar
*/

#include <stdio.h>

#ifdef unix
#define FEVPBTOA fevpba_
#endif
#ifdef vms
#define FEVPBTOA fevpba 
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
 FEVPBTOA();
 CloseDefaultPF();
 return(RETURN);
}
