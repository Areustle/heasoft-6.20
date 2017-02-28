/*
 filename: haddsine.c
 purpose:  c - wrapper for host ADDSINE task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define ADDSINE addsie_
#endif
#ifdef vms
#define ADDSINE addsie 
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
 ADDSINE();
 CloseDefaultPF();
 return(RETURN);
}
