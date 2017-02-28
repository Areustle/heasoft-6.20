      subroutine matchkey (Value, Keys, Nkeys, Ipos, Status)

      implicit none
c
c  Matches value in set of keys
c
c  I  value  (c)  Entered value
c  I  keys   (c)  Array of possible values
c  I  nkeys  (i)  Number of possible values
c  O  ipos   (i)  Position of matching option
c I/O status (i)  Status flag ( 0 = OK )

      character*(*) Value
      integer Nkeys, Ipos, Status
      character*(*) Keys(Nkeys)
c
c  Local variables
c
      integer i, lenval, LENACT
      logical found
      character(255) dumval

      Status = 0
      Ipos = 0

      dumval = Value
      call UPC(dumval)
      lenval = LENACT(dumval)
      found = .FALSE.
      i = 1
      do while ( i.le.Nkeys .and. .not.found )
         if ( dumval(1:lenval) .eq. Keys(i)(1:lenval) ) then
            found = .TRUE.
            Ipos = i
         endif
         i = i + 1
      enddo
      if ( .not.found ) Status = -1

      return
      end
