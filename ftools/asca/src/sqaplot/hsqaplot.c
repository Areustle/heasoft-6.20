/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/hsqaplot.c,v 3.6 1996/04/16 23:42:30 dunfee Exp $   */
/*                   */
/*
 filename:	hsqaplot.c
 purpose:	c - wrapper for host SQAPLT task
 author:	Geoffrey B. Crew
 date:		May 1993
*/

#ifdef unix
# ifndef SISDEBUG
#  define SQAPLT sqaplt_
# else
   extern void parse_extras();
# endif SISDEBUG
#endif

#ifdef vms
# define SQAPLT sqaplt 
#endif

int MAIN_;			/* work around SunOS 4.1.3 bug */

main (argc, argv)
int argc;
char **argv;
{
 int	status;

#ifdef SISDEBUG
 if (argc > 1 && argv[1][0] == '-') parse_extras(&argc, &argv);
#endif SISDEBUG

 OpenDefaultPF(argc, argv);
 status = SQAPLT();
 CloseDefaultPF();
 return(status);
}
