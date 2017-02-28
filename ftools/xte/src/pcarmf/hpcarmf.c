/*
 filename: hpcarmf.c
*/

#include <stdio.h>

#ifdef unix
#define PCARMF pcarmf_
#endif
#ifdef vms
#define PCARMF pcarmf
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
 PCARMF();
 CloseDefaultPF();
 return(RETURN);
}


