#include "cfortran.h"
#include <unistd.h>

int getc_(char *chr_ptr)
 
/* Reads a single byte from the current terminal.  The read waits
 * until the user types something.
 *
 * chr_ptr   In/Ret  The character read
 */
{
   return (read(0,chr_ptr,1));
}
 
void putc_(char *chr_ptr)
 
/* Write a single character to the terminal.
 *
 * chr_ptr   Input   The character to be written
 */
{
   write(0,&chr_ptr[0],1);
   return;
}

void rename_(char *oldname, char *newname)

/* MJT 09July 1996 */
/* mv file1 --> file2 */
{
int stat;
   stat=rename(oldname, newname);
   return;
}

int getpid_()
/* MJT 17July96 */
{
   return getpid();
}

void getdir(dir)
char *dir;
/* MJT 23 July96 */
/* PDW 02 Sept99 */
{
 getwd(dir);
 return;
}
FCALLSCSUB1(getdir,GETDIR,getdir,PSTRING)

/* !!! MJT 05Aug96 !!! */
FCALLSCSUB1(unlink,UNLINK,unlink,STRING)


#include <time.h>

/* Returns the current time in HR - hour (0-23), 
   MIN - minutes (0 - 59), SEC - seconds (0 - 61) (for leap secs) */

void gtstime(int *hr, int *min, int *sec )
{
   struct tm* tm;    /* date/time structure */
   time_t clock;     /* current time in seconds  */
 
   clock = time(0);
   tm = localtime(&clock);
  *hr = (int) tm->tm_hour;
  *min = (int) tm->tm_min;
  *sec = (int) tm->tm_sec;
}
FCALLSCSUB3(gtstime,GTSTIME,gtstime,PINT,PINT,PINT)


/* Returns the current date in YR - year (last two digits), 
   MON - month (1 - 12), DAY - day (1 - 31) */

void gtsdate(int *yr, int *mon, int *day )
{
   struct tm* tm;    /* date/time structure */
   time_t clock;     /* current time in seconds  */
 
   clock = time(0);
   tm = localtime(&clock);
  *yr = (int) ((tm->tm_year < 100) ? tm->tm_year : tm->tm_year-100);
  *mon = (int) tm->tm_mon+1;
  *day = (int) tm->tm_mday;
}
FCALLSCSUB3(gtsdate,GTSDATE,gtsdate,PINT,PINT,PINT)

/* Return number of seconds since 1/1/1970 */

int xantime()
{
  time_t clock;
  int itime;

  clock = time(0);
  itime = clock;

  return itime;
}
FCALLSCFUN0(INT,xantime,TIME,time)

#include <stdlib.h>

float getran(int seed) {
/*
 *  Micah Johnson - Apr. 3, 1998
 *  Returns random float in interval [0.0, 1.0)
 *
 *  The standard FORTRAN implementation (the one used for sun,osf,etc.)
 *  was not compilable under g77.  Also, I abandoned a Numerical Recipes
 *  implementation for fear of copyright issues.
 *
 *  seed is ignored after it is first set.  This is 
 *  different from the standard implementation of GETRAN,
 *  but it's the only way, because after the initial seed value
 *  is set, rand() is a black box.  There is no way to know the
 *  interim values. The impact of this difference is projected to 
 *  be minimal, unless a set of random numbers must be reproducable
 *  within same program run.
 *
 */
   static int start_seed = 1;

   if (start_seed) {
      srand(seed);
      start_seed = 0 ;
   }
   return (float) (rand()/(RAND_MAX + 1.0));
}
FCALLSCFUN1(FLOAT,getran,GETRAN,getran,INT)
