      subroutine xnxarg(prompt,string,iparse,ibeg,iend,qskip,
     &                  iflag,idelim)
c
c parse a string, or prompt for it if it is blank
c origin unknown
c 7 Jan 199: added check for eof on return from xcread - Nick
c
      character*(*) prompt,string
      integer*4 iparse,ibeg,iend,iflag,idelim
      logical*4 qskip
      include 'xparinc.inc'

C needed to ensure XPRSBD common block is initialized under gfortran

      CALL XPARSE(' ')

      call xgtarg(string,iparse,ibeg,iend,qskip,iflag,idelim)
      if(iflag.eq.1)then
         call xcread(prompt,string,iflag)
         if(iflag.lt.0)return
         iparse=0
         call xgtarg(string,iparse,ibeg,iend,qskip,iflag,idelim)
      endif

      end
