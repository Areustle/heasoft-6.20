/*
 filename : hcispi.c
 purpose  : c - wrapper for host SISPI task
 author   : Koji Mukai, modifying Kent Blackburn's hgisrti.c
 data     : June 30, 1994
*/

#include <stdio.h>

#ifdef unix
#define SISPI sispi_
#endif
#ifdef vms
#define SISPI sispi
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
 SISPI();
 CloseDefaultPF();
 return(RETURN);
}
