*- delfil.sun - delete file on SUN/UNIX
      subroutine xdelfil(file,status)
*  deletes the file specified
*  AMTP - 20 September 1992
*  JP - 12 October 1999: length of 'rm -f ' is 6, not 3
* Import :
      character*(*) file
      integer*4 status
* External reference :
      integer*4 lenact
      character(500) tmpfil
*-
      tmpfil = 'rm -f '//file
      call spawn(tmpfil,lenact(file)+6,status)

      return

      end
