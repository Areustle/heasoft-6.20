/*
 filename: hbemerge.c
 purpose:  c - wrapper for host BEMERGE task
 author:   Brian K. Elza
*/

#include <stdio.h>

#ifdef unix
#define BEMERGE bemere_
#endif
#ifdef vms
#define BEMERGE bemere
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
 BEMERGE();
 CloseDefaultPF();
 return(RETURN);
}
