/*
 filename: hofverify.c
 purpose:  c - wrapper for host FVERIFY task
 author:   William Pence
*/

#include <stdio.h>

#ifdef unix
#define OFVERIFY ofvery_
#endif
#ifdef vms
#define OFVERIFY ofvery 
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
 OFVERIFY();
 CloseDefaultPF();
 return(RETURN);
}
