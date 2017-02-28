/* Let's check to see if "ftoolstruct.h" has already been included, if it
   hasn't been then let's define all of the C MACRO calls and define XPI 
   so that if this routine is called a second time it will not be processed. 
*/

#ifndef FTOOLSTRUCT_LOADED
#define FTOOLSTRUCT_LOADED
   
/******* COMMON block definition for Fcerr and Fcerrm ********

   Set up definitions for use of COMMON block /task/ which is used
   in the subroutine FCERR and FCERRM.
 
   In order for the FCERR subroutine to function correctly, you need to
   write the name of your task to the task common block.  To do this
   include the following statement in your C code:
 
   C2FCBSTR("string_for_taskname",TASK.taskname,0);
 
   where "string_for_taskname" is the name of your task.
   ***********************************************/
 
typedef struct { char taskname[41]; } TASK_DEF;  /* define struct/CB */
#define TASK COMMON_BLOCK(TASK,task)             /* cfortran stuff */
extern TASK_DEF TASK;

/******************* Pgfout common block ******************/
/* This structure is required to use Pgfout to function properly 
   you can assign this integer via:
 
   PGFCOUNT.count = integer_value;
   */

typedef struct {int count; } PGFCOUNT_DEF;
#define PGFCOUNT COMMON_BLOCK(PGFCOUNT,pgfcount)
extern PGFCOUNT_DEF PGFCOUNT;

#endif

