/*
 filename: ha2source.c
 purpose:  c - wrapper for host A2SOURCE task
 author:   Jesse Allen
*/

#include <stdio.h>

#ifdef unix
#define A2SOURCE a2soue_
#endif
#ifdef vms
#define A2SOURCE a2soue
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
 A2SOURCE();
 CloseDefaultPF();
 return(RETURN);
}
