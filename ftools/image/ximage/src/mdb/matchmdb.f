      subroutine matchmdb (Value, Keys, Nkeys, Ipos, Status)

      implicit none
c
c  Matches value in list of mdb keys
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
      integer*4 LENACT
      character(255) msg

      call matchkey (Value, Keys, Nkeys, Ipos, Status)
      if ( status.ne.0 ) then
         write (msg, 991) Value(1:LENACT(Value))
         call XWRITE (msg, 10)
      endif

      return
 991  format (' Invalid MDB key (',a,')')
      end
