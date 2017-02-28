/*
 filename: hfbbft2pha.c
 purpose:  c - wrapper for host FBBFT2PHA task
 author/date:   James Peachey September 1998
*/

#include <stdio.h>

#define FBBFT2PHA fbbfta_
#define RETURN 0

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 FBBFT2PHA();
 CloseDefaultPF();
 return(RETURN);
}
