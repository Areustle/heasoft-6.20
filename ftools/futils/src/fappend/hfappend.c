/*
 filename: hfappend.c
 purpose:  c - wrapper for host FAPPND task
 author:   Janice Tarrant
*/

#include <stdio.h>

#ifdef unix
#define FAPPEND fapped_
#endif
#ifdef vms
#define FAPPEND fapped 
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
 FAPPEND();
 CloseDefaultPF();
 return(RETURN);
}
