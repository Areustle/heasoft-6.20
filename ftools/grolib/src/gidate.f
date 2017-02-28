      subroutine gidate (idat)
      implicit none
      integer idat(3)
c
c  O  idat  (i) Integer array of current date
c
c gidate uses the XANLIB routine GETDAT to return the
c current date in an integer array
c
      integer di, i, ierr
      real*8 dumval
      character(3) mons(12)
      character(11) cdate
      data mons/'jan','feb','mar','apr','may','jun',
     &          'jul','aug','sep','oct','nov','dec'/

c
c Initialize
c
      idat(1) = 0
      idat(2) = 0
      idat(3) = 0

      call GETDAT(cdate)
c
c Set day
c Use index to account for one- or two-digit day from GETDAT
c
      di = index(cdate,'-')
      call STRNUM(cdate(1:di-1), -4, dumval, ierr)
      if (ierr .eq. 0) idat(1) = int(dumval)
c
c Set month
c
      call LOCASE(cdate(di+1:di+3))
      i = 1
      do while (cdate(di+1:di+3).ne.mons(i) .and. i.le.12) 
         i = i + 1
      enddo
      if (i .le. 12) idat(2) = i
c
c Set year
c
      call STRNUM(cdate(di+5:di+8), -4, dumval, ierr)
      if (ierr .eq. 0) idat(3) = int(dumval)

      return
      end
