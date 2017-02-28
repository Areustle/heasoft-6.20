/*
 filename: hxstar2table.c
 purpose:  c - wrapper for host XSTAR2TABLE task
 author/date:   James Peachey August 1996
*/

#include <stdio.h>

#define XSTAR2TABLE xstar2table
#define RETURN 0

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
 OpenDefaultPF(argc, argv);
 XSTAR2TABLE();
 CloseDefaultPF();
 return(RETURN);
}
