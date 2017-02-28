      subroutine gheads(mapid, key, val, mode, status)
      implicit none
c
c  Return/set string header in header.inc
c
c  I  mapid  (s)  Map for header to be manipulated
c  I  key    (s)  Parameter in header
c I/O val    (s)  Value to be set or retrieved
c  I  mode   (i)  0=retrieve 1=set
c  I  status (i)  Error flag (0 = OK)
c
      character*(*) mapid, key, val
      integer*4 mode, status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include 'header.inc'
c
c  Local variables
c
      integer*4 LENACT
      character(80) ds
      character(10) action
      integer*4 imap, ikey
      integer*4 tchat, lchat
      logical verbose

      status = 0
      action = ' '

      verbose = .FALSE.
      call xgtcht(tchat, lchat)
      if ( tchat.ge.ghchat .or. lchat.ge.ghchat ) verbose = .TRUE.

      if ( verbose ) then
         if ( mode.eq.0 ) then
            action = ' Look up'
         elseif (mode.eq.1) then
            action = ' Assign'
         endif

         write(ZWRite,'(a,1x,4a)') action(:LENACT(action)), 
     &                     key(:LENACT(key)), ' for ', 
     &                     mapid(:LENACT(mapid)), ' header'
         call xwrite(ZWRite, ghchat)
      endif
c
c        Quoted key behavior 
c         If begins with '\' convert key into type and return
c
      if ( mode.eq.0 .and. key(1:1).eq.CHAR(92) ) then
         val = key(2:)
         call XWRITE(' Return quoted value', ghchat)
         return
      endif

      call mapidx(mapid, imap, status)
      if ( status.ne.0 ) return

      call matchhdr (key, chr_keys, chr_num, ikey, status)
      if ( status.ne.0 ) return

      if ( mode.eq.0 ) then
         
         ds = chr_hdr(ikey, imap)
         val = ds(1:MIN(LEN(val),LENACT(ds)))
         if ( verbose ) then
            write(ZWRite,'(1x,3a)') key(:LENACT(key)), ' = ',
     &                           val(:LENACT(val))
            call XWRITE(ZWRite, ghchat)
         endif
      else if ( mode.eq.1 ) then
         if ( verbose ) then
            write(ZWRite,'(1x,3a)') key(:LENACT(key)), ' = ', 
     &                           val(:LENACT(val))
            call XWRITE(ZWRite, ghchat)
         endif
         chr_hdr(ikey, imap) = 
     &              val(1:MIN(LENACT(val),LEN(chr_hdr(ikey,imap))))
      else
         write(ZWRite,'(a,i5)') ' gheads: Invalid mode - ', mode
         call XWRITE(ZWRite,10)
      endif
    
      return
      end
