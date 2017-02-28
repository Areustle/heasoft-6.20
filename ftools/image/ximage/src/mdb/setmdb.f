      subroutine setmdb(itel, key, val, status)
      implicit none
c
c  Set keyword value in mission database
c
c  I  itel   (i)  Telescope index
c  I  key    (s)  MDB Keyword
c  I  val    (s)  Keyword value
c  O  status (i)  Error flag (0=OK)
c
      integer itel, status
      character*(*) key, val

      include 'mdb.inc'
c
c  Local variables
c
      character(1) type
      character(4) nulbuf
      real*4 dr
      real*8 dd, DNULL
      integer di, LENACT

      call XWRITE(' setmdb: ', 30)

      status = 0

      if ( itel.gt.ZEXpmax ) then
         call XWRITE(' Exceeded maximum missions', 10)
         status = -1
         return
      endif

      call gmdbtype(key, type, status)
      if ( status.ne.0 ) then
         call XWRITE(' Keyword undefined: ', 10)
         call XWRITE(key, 10)
         return
      endif

      call UPC(type)
      if ( type.eq.'I' ) then
         call strnum(val, -4, dd, status)
         if ( status.ne.0 ) then
            call XWRITE(' Could not parse integer', 10) 
            return
         endif
         di = dd
         call gmdbi(itel, key, di, 1, status)
      elseif ( type.eq.'S' ) then
         call rmvchr(val,'"')
         call gmdbs(itel, key, val, 1, status)
      elseif ( type.eq.'D' ) then
         nulbuf = val(1:4)
         call upc(nulbuf)
         if ( LENACT(val).eq.4 .and. nulbuf.eq.'NULL' ) then
            dd = DNULL()
         else 
            call strnum(val, 8, dd, status)
         endif
         if ( status.ne.0 ) then
            call XWRITE(' Could not parse real', 10) 
            return
         endif
         call gmdbd(itel, key, dd, 1, status)
      endif
         
      if ( status.ne.0 ) then
         call XWRITE(' Could not set keyword in mdb', 10)
      endif

      return
      end
