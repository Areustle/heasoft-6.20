      subroutine gheadtype(keyname, type, status)
      implicit none
c
c  Get type for keyname
c
c  I  keyname   (s)  Keyname to search for
c  O  type      (s)  Key type (i, s, or d)
c  O  status    (i)  Error flag (0=OK)
c
      character*(*) keyname
      character(1) type
      integer*4 status

      include '../include/maxvals.inc'
      include 'header.inc'
c
c  Local variables
c
      character(10) upbuf
      integer*4 i
      logical found

      upbuf = keyname(1:10)
      call upc(upbuf)
      found = .FALSE.
      status = 0
      type = ' '

      i = 1
      do while ( .not.found .and. i.le.int_num ) 
         if ( upbuf.eq.int_keys(i) ) then
            found = .TRUE.
            type = 'i'
         endif
         i = i + 1
      enddo

      i = 1
      do while ( .not.found .and. i.le.chr_num ) 
         if ( upbuf.eq.chr_keys(i) ) then
            found = .TRUE.
            type = 's'
         endif
         i = i + 1
      enddo

      i = 1
      do while ( .not.found .and. i.le.dbl_num ) 
         if ( upbuf.eq.dbl_keys(i) ) then
            found = .TRUE.
            type = 'd'
         endif
         i = i + 1
      enddo

      if ( .not.found ) status = -1

      return
      end
