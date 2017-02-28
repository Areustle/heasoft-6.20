      subroutine gmdbd(itel, key, val, mode, status)
      implicit none
c
c  Return/set double value in mdb.inc
c
c  I  itel   (i)  Telescope index
c  I  key    (s)  Keyname in mdb
c I/O val    (d)  Value to be set or retrieved
c  I  mode   (i)  0=retrieve 1=set
c  O  status (i)  Error flag (0 = OK)
c
      character*(*) key
      real*8 val
      integer*4 itel, mode, status

      include '../include/io.inc'
      include 'mdb.inc'
c
c  Local variables
c
      integer*4 LENACT
      character(10) action
      integer*4 ikey
      integer*4 tchat, lchat
      logical verbose

      status = 0
      action = ' '

      verbose = .FALSE.
      call xgtcht(tchat, lchat)
      if ( tchat.ge.gmchat .or. lchat.ge.gmchat ) verbose = .TRUE.

      if ( verbose ) then
         if ( mode.eq.0 ) then
            action = ' Look up'
         elseif (mode.eq.1) then
            action = ' Assign'
         endif
      endif

      if ( mode.eq.1 .and. itel.le.0 ) then
        call XWRITE(' Cannot assign mdb for unknown mission', 10)
        status = -1
        return
      endif

      call matchmdb (key, mdbd_keys, mdbd_num, ikey, status)
      if ( status.ne.0 ) return

      if ( itel.le.0 ) then
         val = mdbd_defs(ikey)
         if ( verbose ) then
            write(ZWRite,*) ' Unknown mission: default ',
     &                      key(:LENACT(key)), ' is ',
     &                      mdbd_defs(ikey)
            call XWRITE(ZWRite, gmchat)
         endif
         return
      elseif ( verbose ) then
         write(ZWRite,'(a,1x,7a)') action(:LENACT(action)), 
     &      key(:LENACT(key)), ' for ', 
     &      ZTElescop(itel)(:LENACT(ZTElescop(itel))),' : ',
     &      ZINstrume(itel)(:LENACT(ZINstrume(itel))),' : ',
     &      ZDEtnam(itel)(:LENACT(ZDEtnam(itel)))
         call xwrite(ZWRite, gmchat)
      endif

      if ( mode.eq.0 ) then
         val = dbl_mdb(ikey, itel)
         if ( verbose ) then
            write(ZWRite,*) key(:LENACT(key)), ' = ', val
            call RMVXBK(ZWRite(2:))
            call XWRITE(ZWRite, gmchat)
         endif
      else if ( mode.eq.1 ) then
         if ( verbose ) then
            write(ZWRite,*) key(:LENACT(key)), ' = ', val
            call RMVXBK(ZWRite(2:))
            call XWRITE(ZWRite, gmchat)
         endif
         dbl_mdb(ikey, itel) = val
      else
         write(ZWRite,'(a,i5)') ' gmdbd: Invalid mode - ', mode
         call XWRITE(ZWRite,10)
         status = -1
         return
      endif

      return
      end
