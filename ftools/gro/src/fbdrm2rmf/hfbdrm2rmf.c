/*
 filename: hfbdrm2rmf.c
 purpose:  c - wrapper for host FBDRM2RMF task
 author/date:   James Peachey September 1998
*/

#include <stdio.h>

#define FBDRM2RMF fbdrmf_
#define RETURN 0

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 FBDRM2RMF();
 CloseDefaultPF();
 return(RETURN);
}
