/*
 filename: haddshots.c
 purpose:  c - wrapper for host ADDSHOTS task
 author:   Jim Lochner
*/

#include <stdio.h>

#ifdef unix
#define ADDSHOTS addshs_ 
#endif
#ifdef vms
#define ADDSHOTS addshs  
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
 ADDSHOTS();
 CloseDefaultPF();
 return(RETURN);
}

