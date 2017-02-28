/*
 *  xrinit.c
 */
#include "pil.h"
#include "xronos.h"

/*
 *  Local helper routine prototype (implemented at end)
 */
int xrverboselogger(char *msg);

int xrinit(XRTask *task, int *status) {
/*
 *  Standard Xronos initialization
 *
 *  I task   - Contains properties of task
 *  O status - Error flag (0=OK)
 */

/*
 * Register name and version
 */
   set_toolname(task->name);
   set_toolversion(task->version);
/*
 *  Set high chat for PIL messages
 */
   PILSetLoggerFunction(xrverboselogger);

   return(*status);
}

/*
 *  Local helper routine
 */
int xrverboselogger(char *msg) {
/*
 *  Logger function given to PIL to set high chat for PIL messages
 *
 *  I msg  - Message to log
 */
   return(headas_chat(VERBOSE, "%s\n", msg));
}

