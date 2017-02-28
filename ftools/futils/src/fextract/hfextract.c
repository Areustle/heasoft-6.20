/*
 filename: hfextract.c
 purpose:  c - wrapper for host FEXTRACT task
 author:   Janice Tarrant
*/

#include <stdio.h>

#ifdef unix
#define FEXTRACT fextrt_
#endif
#ifdef vms
#define FEXTRACT fextrt 
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
 FEXTRACT();
 CloseDefaultPF();
 return(RETURN);
}
