/*
 filename: hleuv.c
 purpose:  c - wrapper for host exoleuv task
*/

#include <stdio.h>

#ifdef unix
#define EXOLEUV exoleuv_
#endif
#ifdef vms
#define EXOLEUV exoleuv
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
 EXOLEUV();
 CloseDefaultPF();
 return(RETURN);
}
