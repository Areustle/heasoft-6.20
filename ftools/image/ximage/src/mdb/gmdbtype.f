      subroutine gmdbtype(keyname, type, status)
      implicit none
c
c  Get type for mdb keyname
c
c  I  keyname   (s)  Keyname to search for
c  O  type      (s)  Key type (i, s, or d)
c  O  status    (i)  Error flag (0=OK)
c
      character*(*) keyname
      character(1) type
      integer*4 status

      include 'mdb.inc'
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
      do while ( .not.found .and. i.le.mdbi_num ) 
         if ( upbuf.eq.mdbi_keys(i) ) then
            found = .TRUE.
            type = 'i'
         endif
         i = i + 1
      enddo

      i = 1
      do while ( .not.found .and. i.le.mdbs_num ) 
         if ( upbuf.eq.mdbs_keys(i) ) then
            found = .TRUE.
            type = 's'
         endif
         i = i + 1
      enddo

      i = 1
      do while ( .not.found .and. i.le.mdbd_num ) 
         if ( upbuf.eq.mdbd_keys(i) ) then
            found = .TRUE.
            type = 'd'
         endif
         i = i + 1
      enddo

      if ( .not.found ) status = -1

      return
      end
