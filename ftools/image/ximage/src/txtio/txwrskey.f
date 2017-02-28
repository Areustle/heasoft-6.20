      subroutine txwrskey(filename, keyroot, icol, svalue, comment, 
     &                    status)
      implicit none
c
c  Appends string keyword entry in command/comment buffer
c
c  I  filename (s)  Location of text file
c  I  keyroot  (s)  Keyword root
c  I  icol     (i)  Column number  (If 0, keyroot is keyword)
c  I  svalue   (s)  String value 
c  I  comment  (s)  Comment string
c  O  status   (r)  Error flag (0 = OK)
c
      character*(*) filename, keyroot, comment, svalue
      integer icol, status

      include '../include/io.inc'
      include 'txtio.inc'
c
c  Local variables
c
      character(20) keyword, str
      character(80) card
      character(100) line
      integer hdtype, slen, LENACT

      if ( status.ne.0 ) return

      if ( icol.eq.0 ) then
         keyword = keyroot
      else
         call xistr(icol, str, slen)
         write (keyword,'(2a)') keyroot(1:LENACT(keyroot)), str(1:slen)
      endif
      call UPC(keyword)

      write (ZWRite, *) keyword(1:LENACT(keyword)), ' = ',
     &                   svalue(:LENACT(svalue)), ' / ', 
     &                   comment(1:LENACT(comment))
      call ftgthd(ZWRite, card, hdtype, status)

      line = '$echo '//card(1:LENACT(card))

      call txwrcom(filename, line, status)

      return
      end