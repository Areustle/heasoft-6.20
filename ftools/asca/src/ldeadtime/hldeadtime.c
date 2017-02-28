/*
 filename: hldeadtime.c
 purpose:  c - wrapper for host LDEADTIME task
*/

#include <stdio.h>

#ifdef unix
#define LDEADTIME ldeade_
#endif
#ifdef vms
#define LDEADTIME ldeade
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
 LDEADTIME();
 CloseDefaultPF();
 return(RETURN);
}
