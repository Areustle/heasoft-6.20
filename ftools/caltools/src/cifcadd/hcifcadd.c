/*
 filename: hcifcadd.c
 purpose:  c - wrapper for host CIFCADD task
 author:   L. Breedon
*/

#include <stdio.h>

#ifdef unix
#define CIFCADD cifcad_
#endif
#ifdef vms
#define CIFCADD cifcad
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
 CIFCADD();
 CloseDefaultPF();
 return(RETURN);
}
