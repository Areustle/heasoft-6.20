      SUBROUTINE XCHOSE(STRING, CHOICE, NCH, CHOSE, ICH, QEOF, *)
      CHARACTER*(*) STRING, CHOICE(*), CHOSE
      INTEGER*4 NCH, ICH
      LOGICAL*4 QEOF
C---
C XPARSE subroutine to prompt for a response that requires one option
C to be returned
C---
C   string   c*   i: prompt string
C   choice   c*(nch)   i: possible choices
C   nch   i4   i: no. of possible choices
C   chose   c*   w/r: on return the actual chosen string
C   ich   i4   i/r: on input the default choice (when an empty
C         string is returned), on returned the index of the
C         actual string returned in choice.
C   qeof   L4   r: the eof condition raised if true.
C     Alternate returns:
C      1 - when qeof is true
C---
C 1985-Oct-11 - rashafer 
C---
      include 'xparinc.inc'
      character type*7
      integer*4 lenact
      integer*4 ist,ichp,iparse,jch,iflag,ibeg,iend,ierr,idelim
      logical*4 qskip
      data type/'options'/

      ist=lenact(string)
      ichp=max(ich,0)
      if(ichp.gt.0) ichp=lenact(choice(ich))
      if((ist.gt.0).and.(ichp.gt.0)) then
C** make the compound prompt string
          xprmsw=string(:lenact(string))//' ('//choice(ich)(:lenact(
     &        choice(ich)))//')'
          call xcread(xprmsw(:lenact(xprmsw)),chose,ierr)
      else
          call xcread(string,chose,ierr)
      end if
      if(ierr.lt.0) goto 900
C** come from for repeat process when the match failed
  100 continue
      iparse=0
      call xgtarg(chose,iparse,ibeg,iend,qskip,iflag,idelim)
      if(qskip.or.(iflag.ne.0)) then
         if(ich.gt.nch) then
            chose=choice(ich)
            call xmatch(chose(:lenact(chose)),choice,nch,jch)
         else
            jch=ich
         end if
      else
         iend = MIN(iend, ibeg+len(choice(1))-1)
         call xmatch(chose(ibeg:iend),choice,nch,jch)
      end if
      if(jch.le.0) then
         call xunids(chose,choice,nch,jch,type)
         call xcread('Please try again:',chose,ierr)
         if(ierr.lt.0) goto 900
         goto 100
      end if
      ich=jch
      qeof=.false.
      return
C---
C** eof handler
  900 continue
      qeof = .true.
      return 1
      end
