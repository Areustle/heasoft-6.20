      subroutine matchopts (Value, Opts, Nopts, Ipos, Status)
      implicit none
c
c  Matches value in set of options
c
c  I  value  (c)  Entered value
c  I  opts   (c)  Array of possible values
c  I  nopts  (i)  Number of possible values
c  O  ipos   (i)  Position of matching option
c I/O status (i)  Status flag ( 0 = OK )

      character*(*) Value
      integer Nopts, Ipos, Status
      character*(*) Opts(Nopts)
c
c  Local variables
c
      integer i, LENACT
      character(255) msg, validopts

      call matchkey (Value, Opts, Nopts, Ipos, Status)
      if ( status.ne.0 ) then
         write (msg, 991) Value(1:LENACT(Value))
         call XWRITE (msg, 10)
         validopts = ' '
         do i = 1, Nopts
            write (validopts,'(a,1x,a)') validopts(:LENACT(validopts)), 
     &                                   Opts(i)(:LENACT(Opts(i)))
         enddo
         call XWRITE (validopts, 10) 
         Status = -1
      endif

      return
 991  format (' Invalid option (',a,') - Use one of the following:')
      end
