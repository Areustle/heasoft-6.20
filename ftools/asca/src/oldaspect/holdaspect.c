/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/oldaspect/holdaspect.c,v 1.2 1999/06/18 15:40:53 peachey Exp $   */
/*                   */
/*
 filename: holdaspect.c
 purpose:  c - wrapper for host OLDASPECT task
*/

#include <stdio.h>

#define ASPECT oldAspect
#define RETURN 0

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;

{
  void ASPECT();

  OpenDefaultPF(argc, argv); 
  ASPECT();
  CloseDefaultPF();
  return(RETURN);
}
