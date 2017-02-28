#include <stdio.h>

#ifndef convex
#include <string.h>
#endif

/*
 * Allow PWDRIV to be calleable by FORTRAN using the two commonest
 * calling conventions. Both conventions append length arguments for
 * each FORTRAN string at the end of the argument list, and convert the
 * name to lower-case, but one post-pends an underscore to the function
 * name (PG_PPU) while the other doesn't. Note the VMS is handled
 * separately below. For other calling conventions you must write a
 * C wrapper routine to call pwdriv() or pwdriv_().
 */
#ifdef PG_PPU
#define PWDRIV pwdriv_
#else
#define PWDRIV pwdriv
#endif

/*.......................................................................
 * This is a stub version of the Tk PGPLOT widget device driver to
 * be included in the main PGPLOT library. The real driver resides in a
 * dedicated library, which when cited before libpgplot on the link line,
 * overrides this stub. The rational behind this is that if the real
 * driver were included in the PGPLOT library all applications that are
 * currently linked with PGPLOT would have to be changed to link with the
 * Tcl/Tk libraries.
 */
void PWDRIV(ifunc, rbuf, nbuf, chr, lchr, len)
 int   *ifunc, *nbuf, *lchr;
 int   len;
 float rbuf[];
 char  *chr;
{
  int i;
/*
 * Branch on the specified PGPLOT opcode.
 */
  switch(*ifunc) {

/*--- IFUNC=1, Return device name ---------------------------------------*/

  case 1:
     for(i=0; i < len; i++)
        chr[i] = ' ';
     *lchr = 0;
     break;

  default:
    fprintf(stderr, "/POW: Unexpected opcode=%d in stub driver.\n", *ifunc);
    *nbuf = -1;
    break;
  };
  return;
}
