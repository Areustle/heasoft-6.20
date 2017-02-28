      subroutine qustrip(String)
      implicit none
c
c  Strip start and end double or single quote from string
c
c   I/O  string   (s)  String
c
      character*(*) String
c
c  Local variables
c
      integer i, ilen, LENACT

      ilen = LENACT(string)

      if ( ilen.le.2 ) return

      if ( (string(1:1).eq.'"' .and. string(ilen:ilen).eq.'"') .or.
     &     (string(1:1).eq.'''' .and. string(ilen:ilen).eq.'''') ) then
         do i = 2, ilen-1
            String(i-1:i-1) = String(i:i)
         enddo
         String(ilen-1:) = ' '
      endif

      return
      end
