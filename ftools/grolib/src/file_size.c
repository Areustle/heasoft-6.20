#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

/*      $Id: file_size.c,v 3.2 2006/04/17 20:54:27 irby Exp $
 *      $Log: file_size.c,v $
 *      Revision 3.2  2006/04/17 20:54:27  irby
 *      Updates for compliance with g95 and gfortran:
 *
 *      - Replace call to lnblnk with equivalent call to len_trim.
 *        lnblnk (a routine which used to live in libg2c) is not currently
 *        available with g95 or gfortran.
 *
 *      - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
 *
 *      - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
 *
 *      - Fix non-integer loop variables.
 *
 *      Revision 3.1  2002/04/16 20:32:08  irby
 *      Additions to libgro - previously these codes existed in the following
 *      libraries:
 *
 *        libsenstv
 *        libsysutil
 *        libutil
 *        libftio
 *
 * Revision 1.1  1996/02/20  20:51:01  programs
 * Initial revision
 *
 */

/*
  FORTRAN Calling sequence:

  integer function file_size ( path )
  integer size
  character*(*) path

  size = file_size ( path )
*/

int file_size_ ( path, len )
char *path;
int len;
{
  int errno, new_len, i;
  struct stat buf;
  char string[256];

/*  Clip off any trailing blank space  */

  new_len = 0;

  for ( i=0;i<len;i++ ) {
    if ( path[i] != ' ' ) {
      new_len++;
    }
    else {
      break;
    }
  }

  bcopy(path,string,new_len);
  string[new_len] = '\0';

  errno = stat ( string, &buf );

  if ( errno == -1 ) {
    c_fcerr ("Error getting file size: ");
    return -1;
  }

  return buf.st_size;
}

