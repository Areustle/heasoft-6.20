      subroutine xdstr(value, sigdig, str, slen)
      implicit none
c
c  Return format based on input value
c
c  I  value  (d)  Double value
c  I  sigdig (i)  Significant digits to print
c  O  str    (c)  String
c  O  slen   (i)  Length of string
c
      real*8 value
      integer sigdig, slen
      character*(*) str
c
c  Local variables
c
      real*8 dd
      real*4 dr
      integer locsig, di, LENACT
      integer width, dec
      character(30) fmt

      locsig = sigdig
      if ( locsig.le.0 ) locsig = 8

      dd = ABS(value)
      if ( dd.eq.0.0 ) then
         dec = MAX(1,locsig - 1)
         width = dec + 2
         write (fmt,'(a,i2,a,i2,a)') '(f', width, '.', dec, ')'
         call RMVBLK(fmt)
         write (str, fmt) value
      elseif ( dd.gt.0.001 .and. dd.lt.100000 ) then
         dr = MAX(float(nint(dd)),1.0)
         di = int(LOG10(dr) + 1.0)
         dec = MAX(locsig - di,1)
         width = di + 1 + dec
         if ( value.lt.0 ) width = width + 1
         write (fmt,'(a,i2,a,i2,a)') '(f', width, '.', dec, ')'
         call RMVBLK(fmt)
         write (str, fmt) value
         call RMVBLK(str)
      else
         dec = MAX(locsig - 1,1)
         width = dec + 6
         if ( value.lt.0 ) width = width + 1
         write (fmt,'(a,i2,a,i2,a)') '(1pe', width, '.', dec, ')'
         call RMVBLK(fmt)
         write (str, fmt) value
      endif
      slen = LENACT(str)

      return
      end
