/*
 filename: hofaintdfe.c
 purpose:  c - wrapper for host OFAINTDFE task
*/

#define RETURN 0

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
int argc;
char **argv;
{
  void ofaintdfe();

  OpenDefaultPF(argc, argv);
  ofaintdfe();
  CloseDefaultPF();
  return(RETURN);
}
