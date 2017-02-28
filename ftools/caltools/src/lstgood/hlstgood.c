/*
 filename: hlstgood.c
 purpose:  c - wrapper for host LSTGOOD task
 author:   M. Tripicco
*/

#include <stdio.h>

#ifdef unix
#define LSTGOOD lstgod_
#endif
#ifdef vms
#define LSTGOOD lstgod
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
 LSTGOOD();
 CloseDefaultPF();
 return(RETURN);
}
