      subroutine strjust(instr, just, outlen, outstr)
      implicit none
c
c  Justify string
c
c  I  instr   (s)  String to justify
c  I  just    (s)  Justification (center, left, right)
c  I  outlen  (i)  Length of output string
c  O  outstr  (s)  Justified string
c
      character*(*) instr, just, outstr
      integer*4 outlen
c
c  Local variables
c
      integer*4 inlen, LENACT
      character(1) jch

      jch = just(1:1)
      call upc(jch)

      if ( jch.eq.'L' ) then
         outstr = instr
         call RMVLBK(outstr)
      elseif ( jch.eq.'R' ) then
         inlen = LENACT(instr)
         outstr = ' '
         outstr(outlen-inlen+1:outlen) = instr(:inlen)
      elseif ( jch.eq.'C' ) then
         inlen = LENACT(instr)
         outstr = ' '
         outstr(int(outlen/2)+1-int(inlen/2):) = instr(:inlen)
      endif

      return
      end
