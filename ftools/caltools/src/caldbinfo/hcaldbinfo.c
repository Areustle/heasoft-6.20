/*
 filename: hcaldbinfo.c
 purpose:  c - wrapper for host CALDBINFO task
 author:   Ian M George
*/

#include <stdio.h>

#ifdef unix
#define CALDBINFO caldbo_
#endif
#ifdef vms
#define CALDBINFO caldbo
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
 CALDBINFO();
 CloseDefaultPF();
 return(RETURN);
}
