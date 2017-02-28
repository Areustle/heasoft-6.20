/*
 filename: hlike.c
 purpose:  c - wrapper for host LIKE task
 author/date:   Sandhia Bansal - 01/07/02
*/

#include <stdio.h>

#ifdef unix
#define LIKE like_
#endif
#ifdef vms
#define LIKE like 
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
 LIKE();
 CloseDefaultPF();
 return(RETURN);
}
