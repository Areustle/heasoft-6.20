      subroutine parseilst(strlist, delim, maxint, intary, numint, 
     &                      status)
      implicit none
c
c  Parse string containing list of integers and put into
c  array of integers
c
c  I  strlist   (s)  List of integers
c  I  delim     (c)  Character delimiting the integers
c  I  maxint    (i)  Maximum number of integers
c  O  intary    (i)  Array of integers
c  O  numint    (i)  Number of integers
c  O  status    (i)  Error flag (0=OK)
c
      character*(*) strlist
      character(1) delim
      integer maxint, numint, status
      integer intary(maxint)
c
c  Local variables
c
      integer ibeg, listlen, iloc, LENACT
      character(1) dc
      real*8 dd

      status = 0
      numint = 0
      ibeg = 1
      listlen = LENACT(strlist)

      do while ( ibeg.le.listlen ) 
         call findchr(strlist, ibeg, listlen, delim, iloc, dc, status)
         if ( status.ne.0 ) iloc = listlen
         status = 0
         call strnum(strlist(ibeg:iloc), -4, dd, status)
         if ( status.ne.0 ) then
            call XWRITE(' parseilist: String conversion failed', 10)
            return
         endif
         if ( numint+1.gt.maxint ) then
            call XWRITE(' parseilist: Integer array overflow', 10)
            status = -1
            return
         endif
         numint = numint + 1
         intary(numint) = int(dd)
         ibeg = iloc + 1
      enddo

      status = 0

      return
      end
