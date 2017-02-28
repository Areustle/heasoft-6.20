      subroutine txwrkey(filename, keyroot, icol, rvalue, comment, 
     &                   status)
      implicit none
c
c  Appends real keyword entry in command/comment buffer
c
c  I  filename (s)  Location of text file
c  I  keyroot  (s)  Keyword root
c  I  icol     (i)  Column number  (If 0, keyroot is keyword)
c  I  rvalue   (r)  Real value 
c  I  comment  (s)  Comment string
c  O  status   (r)  Error flag (0 = OK)
c
      character*(*) filename, keyroot, comment
      integer icol, status
      real*4 rvalue

      include '../include/io.inc'
      include 'txtio.inc'
c
c  Local variables
c
      character(20) keyword, str
      character(80) card
      character(100) line
      integer slen, hdtype, LENACT
      real*8 dd

      if ( status.ne.0 ) return

      if ( icol.eq.0 ) then
         keyword = keyroot
      else
         call xistr(icol, str, slen)
         write (keyword,'(2a)') keyroot(1:LENACT(keyroot)), str(1:slen)
      endif
      call UPC(keyword)

      dd = rvalue
      call xdstr(dd, -1, str, slen)
      write (ZWRite, *) keyword(1:LENACT(keyword)), ' = ', str(:slen),
     &                   ' / ', comment(1:LENACT(comment))
      call ftgthd(ZWRite, card, hdtype, status)

      line = '$echo '//card(1:LENACT(card))

      call txwrcom(filename, line, status)

      return
      end