      subroutine xrenam(old,new)
* Author :
*  Andy Pollock (HEAGIP::POLLOCK)
* History :
*  8 April 1993 : original
* Import :
      character*(*) old
      character*(*) new
* Local variables :
      character(256) command
      integer*4 lc
      integer*4 status
* External reference :
      integer*4 lenact
*-
      command='mv '//old
      lc=lenact(command)
      command(lc+2:)=new
      lc=lenact(command)
      call spawn(command,lc,status)
      call xwait(1.)

      return

      end
