      subroutine findchr (str, ibeg, iend, chrlist, iloc, ch, status)
      implicit none
c
c  Find first occurence of character in chrlist beginning at
c  ibeg in str.
c
c  I  str     (s)  Input string
c  I  ibeg    (i)  Start index
c  I  iend    (i)  Stop index
c  I  chrlist (s)  String where each character is to be searched for
c  O  iloc    (i)  Location of found character
c  O  ch      (c)  Character found
c  O  status  (i)  Error flag (0=OK)
c
      character*(*) str, chrlist
      integer ibeg, iend, iloc, status
      character(1) ch
c
c  Local variables
c
      integer i, j, listlen, LENACT

      status = -1
      
      listlen = LENACT(chrlist)
      i = ibeg
      do while ( i.le.iend .and. status.ne.0 ) 
         do j = 1, listlen
            if ( str(i:i).eq.chrlist(j:j) ) then
               iloc = i
               ch = chrlist(j:j)
               status = 0
            endif
         enddo
         i = i + 1
      enddo
               
      return
      end
