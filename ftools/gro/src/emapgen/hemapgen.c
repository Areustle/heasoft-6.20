/*
 filename: hemapgen.c
 purpose:  c - wrapper for host EMAPGEN task
 author/date:   James Peachey August 1996
*/

#include <stdio.h>

#ifdef unix
#define EMAPGEN emapgn_
#endif
#ifdef vms
#define EMAPGEN emapgn
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
 EMAPGEN();
 CloseDefaultPF();
 return(RETURN);
}
