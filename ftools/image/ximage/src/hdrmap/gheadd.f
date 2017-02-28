      subroutine gheadd(mapid, key, val, mode, status)
      implicit none
c
c  Return/set double header in header.inc
c
c  I  mapid  (s)  Map for header to be manipulated
c  I  key    (s)  Parameter in header
c I/O val    (d)  Value to be set or retrieved
c  I  mode   (i)  0=retrieve 1=set
c  O  status (i)  Error flag (0 = OK)
c
      character*(*) mapid, key
      real*8 val
      integer*4 mode, status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include 'header.inc'
c
c  Local variables
c
      integer*4 LENACT
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
     &                             key(:LENACT(key)), ' for ', 
     &                             mapid(:LENACT(mapid)), ' header'
         call xwrite(ZWRite, ghchat)
      endif
c
c        Quoted key behavior 
c         If begins with '\' convert key into type and return
c
      if ( mode.eq.0 .and. key(1:1).eq.CHAR(92) ) then
         call strnum(key(2:),8,val,status)
         call XWRITE(' Return quoted value', ghchat)
         return
      endif

      call mapidx(mapid, imap, status)
      if ( status.ne.0 ) return

      call matchhdr (key, dbl_keys, dbl_num, ikey, status)
      if ( status.ne.0 ) return

      if ( mode.eq.0 ) then
         val = dbl_hdr(ikey, imap)
         if ( verbose ) then
            write(ZWRite,*) ' ',key(:LENACT(key)), ' = ', val
            call RMVXBK(ZWRite(2:))
            call XWRITE(ZWRite, ghchat)
         endif
      else if ( mode.eq.1 ) then
         if ( verbose ) then
            write(ZWRite,*) ' ',key(:LENACT(key)), ' = ', val
            call RMVXBK(ZWRite(2:))
            call XWRITE(ZWRite, ghchat)
         endif
         dbl_hdr(ikey, imap) = val
      else
         write(ZWRite,'(a,i5)') ' gheadd: Invalid mode - ', mode
         call XWRITE(ZWRite,10)
      endif
    
      return
      end
