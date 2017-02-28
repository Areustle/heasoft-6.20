/*
 filename: headdmap.c
 purpose:  c - wrapper for host EADDMAP task
 author/date:   James Peachey August 1996
*/

#include <stdio.h>

#ifdef unix
#define EADDMAP eaddmap_
#endif
#ifdef vms
#define EADDMAP eaddmap
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
 EADDMAP();
 CloseDefaultPF();
 return(RETURN);
}
