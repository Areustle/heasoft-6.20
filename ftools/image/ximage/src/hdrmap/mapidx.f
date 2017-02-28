      SUBROUTINE mapidx(mapid, ptridx, status)
      implicit none
c
c  Translates map id string into pointer index
c
c  I  mapid   (s)  Map id string
c  O  ptridx  (i)  Pointer index
c  O  status  (i)  Error flag (0=OK)
c
      character*(*) mapid
      integer ptridx, status

      include '../include/io.inc'
      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      logical expmap
      integer mlen, mapnum, LENACT
      character*(MAX_IDSTR) tmpstr
      real*8 dd

      logical ISINT

      status = 0
      mlen = LENACT(mapid)

      if ( mlen.eq.0 ) then
         call xwrite(' Invalid map id string: Blank', 5)
         status = -1
         return
      endif

      if ( mlen.le.MAX_IDSTR ) then
        tmpstr = mapid
      else
        tmpstr = mapid(1:MAX_IDSTR)
      endif

      call upc(tmpstr)
c
c  Remove 'EX' 
c
      expmap = .FALSE.
      if ( tmpstr(1:2).eq.'EX' ) then
         expmap = .TRUE.
         tmpstr = mapid(3:mlen) 
         call upc(tmpstr)
         mlen = mlen - 2
      endif
c
c  CUR=>"Current Map", EXCUR=>"Current Exposure Map"
c  DIS=>"Display Map", EXDIS=>"Display Exposure Map"
c  SAV=>"Saved Map", EXSAV=>"Saved Exposure Map"
c  MAP=>"Current Map", EXMAP=>"Current Exposure Map" [Deprecated]
c  MAP1-9, EXMAP1-9 (aliases 1-9, EX1-9)
c  BGMAP=>"Background map"
c  TMPMAP1-5 (used as reserved buffers)
c
c  ptridx -> map name
c  ------------------
c  1 -> MAP1
c  2 -> EXMAP1
c  ...
c  MAX_SLOTS*2-1 -> MAP[MAX_SLOTS]
c  MAX_SLOTS*2   -> EXMAP[MAX_SLOTS]
c  MAX_SLOTS*2+1 -> BGMAP
c  MAX_SLOTS*2+2 -> TMPMAP1
c  ...
c  MAX_SLOTS*2+1+MAX_TMPMAPS -> TMPMAP[MAX_TMPMAPS]
c
      if ( tmpstr(1:3).eq.'CUR' ) then

         ptridx = ICUrmap
         if ( expmap .and. MOD(ptridx,2).eq.1 ) then
            ptridx = ICUrmap + 1
         endif

      elseif ( tmpstr(1:3).eq.'DIS' ) then

         if ( IDIsmap.gt.0 ) then
            ptridx = IDIsmap
         else
            ptridx = ICUrmap
         endif
         if ( expmap .and. MOD(ptridx,2).eq.1 ) then
            ptridx = ptridx + 1
         endif

      elseif ( tmpstr(1:3).eq.'SAV' ) then

         ptridx = MAX_SLOTS*2-1
         if ( expmap ) then
            ptridx = ptridx + 1
         endif

      elseif ( tmpstr.eq.'BGMAP' ) then

         ptridx = MAX_SLOTS*2+1

      elseif ( tmpstr(1:3).eq.'MAP' .or. ISINT(tmpstr) ) then

         if ( tmpstr.eq.'MAP' ) then
            call xwarn(' Map id = MAP, assuming current', 10)
            ptridx = ICUrmap
         else
            if ( tmpstr(1:3).eq.'MAP' ) tmpstr = tmpstr(4:mlen)
            if ( .not.ISINT(tmpstr) ) then
               call xwrite(
     &          ' Invalid map id string: Failed to convert integer', 5)
               status = -1
               return
            endif

            call strnum(tmpstr, -4, dd, status)
            mapnum = INT(dd)
            if ( mapnum.lt.1 .or. mapnum.gt.MAX_SLOTS ) then
               call xwrite(' Map number out of range', 5)
               status = -1
               return
            endif
c
c  Indices are ordered such that:
c  MAP1 = 1, EXMAP1 = 2, MAP2 = 3, etc.
c
            if ( expmap ) then
               ptridx = mapnum*2
            else
               ptridx = mapnum*2 - 1
            endif
         endif

      elseif ( tmpstr(1:6).eq.'TMPMAP' ) then

         tmpstr = tmpstr(7:mlen)
         if ( .not.ISINT(tmpstr) ) then
            call xwrite(
     &       ' Invalid tmpmap id string: Failed to convert integer', 5)
            status = -1
            return
         endif

         call strnum(tmpstr, -4, dd, status)
         mapnum = INT(dd)
         if ( mapnum.lt.1 .or. mapnum.gt.MAX_TMPMAPS ) then
            call xwrite(' Tmpmap number out of range', 5)
            status = -1
            return
         endif

         ptridx = MAX_SLOTS*2 + 1 + mapnum

      else

         write(ZWRite,'(2a)') ' Invalid map id string: ', tmpstr
         call xwrite(ZWRite, 5)
         status = -1

      endif

      if ( ptridx.gt.MAX_MAPS .or. ptridx.lt.1 ) then
         call xwrite(' Map pointer index out of range', 5)
         status = -1
      endif
      
      return
      end
