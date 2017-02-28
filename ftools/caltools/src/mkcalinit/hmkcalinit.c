/*
 filename: hmkcalinit.c
 purpose:  c - wrapper for host mkcalinit task
 author:   Ron Zellar 
*/

#include <stdio.h>

#ifdef unix
#define MKCALT mkcalt_
#endif
#ifdef vms
#define MKCALT mkcalt
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
 MKCALT();
 CloseDefaultPF();
 return(RETURN);
}
