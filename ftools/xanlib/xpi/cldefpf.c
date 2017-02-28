/* $Id: cldefpf.c,v 3.9 2011/07/13 20:44:54 irby Exp $

   $Log: cldefpf.c,v $
   Revision 3.9  2011/07/13 20:44:54  irby
   Replace call to XPISAVEPAR with one to ape_trad_close for wrapping up
   the par file output.  Also, CloseDefaultPF should return a status (for
   the benefit of our new unit test if nothing else).

   Revision 3.8  2006/05/19 14:05:50  irby
   Fix call to g95_runtime_stop (needs no arguments).

   Revision 3.7  2006/05/16 14:39:15  irby
   When using g95 on Darwin, call g95_runtime_start (instead of f_setarg)
   and g95_runtime_stop.

   Revision 3.6  2000/04/25 22:29:00  ngan
   Defined the global status variable.

   Revision 3.5.1.1  1996/04/16 01:38:54  dunfee
   Start of pristine ftools CVS...

 * Revision 1.2  1995/06/02  19:57:12  oneel
 * Corrected comments
 *

   */

#include "cfortran.h"
#include "hea_status.h"     
#include "ape/ape_trad.h"

#define XPISAVEPAR(stat1) \
  CCALLSFSUB1(XPISAVEPAR,xpisavepar,INT,stat1)

int CloseDefaultPF ()
{
  int stat1 = 0;

  ape_trad_close(1);

/* Darwin g95 */
#if defined(__APPLE__) && defined(g95Fortran)
  g95_runtime_stop();
#endif

  /* Terminate the Tool */
  SetHEAStatus(stat1);
  HEATermination();

  return stat1;

}
