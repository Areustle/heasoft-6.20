/*
 filename: hmkcaldb.c
 purpose:  c - wrapper for host mkcaldb task
 author:   Ron Zellar 
*/

#include <stdio.h>

#ifdef unix
#define MKCALB mkcalb_
#endif
#ifdef vms
#define MKCALB mkcalb
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
 MKCALB();
 CloseDefaultPF();
 return(RETURN);
}
