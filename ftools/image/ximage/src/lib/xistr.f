      subroutine xistr(value, str, slen)
      implicit none
c
c  Return string representing integer
c
c  I  value  (i)  Integer value
c  O  str    (c)  String
c  O  slen   (i)  Length of string
c
      integer value, slen
      character*(*) str
c
c  Local variables
c
      integer width, LENACT
      character(30) fmt

      if ( value.eq.0 ) then
         width = 1
      else
         width = INT(LOG10(ABS(REAL(value)))) + 1
         if ( value.lt.0 ) width = width + 1
      endif
      if ( width.gt.len(str) ) width = len(str)
      write (fmt,'(a,i2,a)') '(i', width, ')'
      call RMVBLK(fmt)
      write(str,fmt) value
      slen = LENACT(str)

      return
      end
