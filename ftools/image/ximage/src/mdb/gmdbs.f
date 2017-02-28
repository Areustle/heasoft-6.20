      subroutine gmdbs(itel, key, val, mode, status)
      implicit none
c
c  Return/set string value in mdb.inc
c
c  I  itel   (i)  Telescope index
c  I  key    (s)  Keyname in mdb
c I/O val    (s)  Value to be set or retrieved
c  I  mode   (i)  0=retrieve 1=set
c  O  status (i)  Error flag (0 = OK)
c
      character*(*) key, val
      integer*4 itel, mode, status

      include 'mdb.inc'
      include '../include/io.inc'
c
c  Local variables
c
      integer*4 LENACT
      character(80) ds
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

      call matchmdb (key, mdbs_keys, mdbs_num, ikey, status)
      if ( status.ne.0 ) return

      if ( itel.le.0 ) then
         if ( verbose ) then
            write(ZWRite,*) ' Unknown mission: default ',
     &                      key(:LENACT(key)), ' is ''', 
     &                      mdbs_defs(ikey)(:LENACT(mdbs_defs(ikey))),
     &                      ''''
            call XWRITE(ZWRite, gmchat)
         endif
         val = mdbs_defs(ikey)
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
         ds = str_mdb(ikey, itel)
         val = ds(1:MIN(LEN(val),LENACT(ds)))
         if ( verbose ) then
            write(ZWRite,*) key(:LENACT(key)), ' = ', val(:LENACT(val))
            call XWRITE(ZWRite, gmchat)
         endif
      else if ( mode.eq.1 ) then
         str_mdb(ikey, itel) = 
     &              val(1:MIN(LENACT(val),LEN(str_mdb(ikey,itel))))
         if ( verbose ) then
            ds = str_mdb(ikey, itel)
            write(ZWRite,*) key(:LENACT(key)), ' = ', ds(:LENACT(ds))
            call XWRITE(ZWRite, gmchat)
         endif
      else
         write(ZWRite,'(a,i5)') ' gmdbs: Invalid mode - ', mode
         call XWRITE(ZWRite,10)
         status = -1
         return
      endif

      return
      end
