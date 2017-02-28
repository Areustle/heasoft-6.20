      subroutine map(Cmdid,Status)

      implicit none

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/mapdef.inc'
c
c  Manipulate image maps
c  
c  I  cmdid        (i) Command id
c  O  status       (i) Error flag (0=OK)
c
      integer Cmdid, Status
c
c  Local variables
      integer i, argc, cnt, numlen, slen1, slen2, idx, maxidx, LENACT
      logical set, copy, move, free, isequal, gettmp, show, found
      logical iseqmapid, isloaded
      character*(MAX_IDSTR) mapid, fromid, toid
      character(4) arrow, exarrow
      character(15) file, exfile
      character(9) szstr
      character(2) zmstr, numstr
      character(13) censtr
      character(5) codestr

      character(100) ds1, ds2
      integer di1, di2
      real*8 dd1, dd2

      set = .FALSE.
      copy = .FALSE.
      move = .FALSE.
      free = .FALSE.
      isequal = .FALSE.
      gettmp = .FALSE.
      show = .FALSE.

      Status = 0
      CALL GPARL(Cmdid,'SET',set,Status)
      CALL GPARL(Cmdid,'COPY',copy,Status)
      CALL GPARL(Cmdid,'MOVE',move,Status)
      CALL GPARL(Cmdid,'FREE',free,Status)
      CALL GPARL(Cmdid,'ISEQUAL',isequal,Status)
      CALL GPARL(Cmdid,'GETTMP',gettmp,Status)
      if ( Status.ne.0 ) return
c
c  If no options set, show state of maps
c
      cnt = 0
      if ( set ) cnt = cnt + 1
      if ( copy ) cnt = cnt + 1
      if ( move ) cnt = cnt + 1
      if ( free ) cnt = cnt + 1
      if ( isequal ) cnt = cnt + 1
      if ( gettmp ) cnt = cnt + 1
      if ( cnt.eq.0 ) show = .TRUE.
      if ( cnt.gt.1 ) then
         call xwrite(" Use only one option at a time", 10)
         status = -1
         return
      endif
c
c Check number of arguments
c
      call numcarg(cmdid,argc,status)
      if ( set .and. argc.ne.1 ) then
         call xwrite(" Usage: map set <mapid>", 10)
         status = -1
      endif
      if ( copy .and. argc.ne.2 ) then
         call xwrite(" Usage: map copy <from_mapid> <to_mapid>", 10)
         status = -1
      endif
      if ( move .and. argc.ne.2 ) then
         call xwrite(" Usage: map move <from_mapid> <to_mapid>", 10)
         status = -1
      endif
      if ( free .and. argc.ne.1 ) then
         call xwrite(" Usage: map free <mapid>", 10)
         status = -1
      endif
      if ( isequal .and. argc.ne.2 ) then
         call xwrite(" Usage: map isequal <mapid> <mapid>", 10)
         status = -1
      endif
      if ( gettmp .and. argc.ne.0 ) then
         call xwrite(" Usage: set m [map gettmp]", 10)
         status = -1
      endif
      if ( status.ne.0 ) return
      if ( argc.eq.1 .and. .not.free ) then
c        For usage "map <mapid>", assume "map set <mapid>"
         set = .true.
      endif
c
c  Retrieve arguments
c
      if ( set .or. free ) then
         call nextcarg(cmdid,mapid, MAX_IDSTR, Status)
      elseif ( copy .or. move .or. isequal ) then
         call nextcarg(cmdid,fromid, MAX_IDSTR, Status)
         call nextcarg(cmdid,toid, MAX_IDSTR, Status)
      endif 
      if ( status.ne.0 ) return

      if ( set ) then

         
         call setcurmap(mapid, status)
         if ( status.eq.0 ) then
            write(ZWRite,'(2a)') ' Current map set to ', 
     &                           mapid(:LENACT(mapid))
            call xwrite(ZWRite, 10)
         endif

      elseif ( copy ) then

         call mapcopy(fromid, toid, status)
         if ( status.eq.0 ) then
            write(ZWRite,'(4a)') ' Copied ', fromid(:LENACT(fromid)),
     &                           ' to ', toid(:LENACT(toid))
            call xwrite(ZWRite, 10)
         endif

      elseif ( move ) then

         call mapcopy(fromid, toid, status)
         if ( status.ne.0 ) return

         call mapfree(fromid, status)
         if ( status.eq.0 ) then
            write(ZWRite,'(4a)') ' Moved ', fromid(:LENACT(fromid)),
     &                           ' to ', toid(:LENACT(toid))
            call xwrite(ZWRite, 10)
         endif

      elseif ( free ) then

         if ( isloaded(mapid) ) then
            call mapfree(mapid, status)
            if ( status.eq.0 ) then
               write(ZWRite,'(2a)') ' Freed ', mapid(:LENACT(mapid))
               call xwrite(ZWRite, 10)
            endif
         else
            call freetmpmap(mapid, status)
         endif

      elseif ( isequal ) then

         call tclretl(iseqmapid(fromid, toid), status)

      elseif ( gettmp ) then

         call gettmpmap(toid, status)
         if ( status.ne.0 ) return
         call tclrets(toid, status)

      elseif ( show ) then
c
c   Show state of maps
c
         call xwrite(' ', 5)
         arrow = ' '
         exarrow = ' '
         szstr = 'Size'
         zmstr = 'Rb'
         censtr = 'Center'
         codestr = 'Codes'
         file = 'MAP'
         exfile = 'EXMAP'
         write(ZWRite,'(1x,a,2x,6(1x,a),3x,a)') arrow,file,szstr,
     &         zmstr,censtr,codestr,exarrow,exfile
         call xwrite(ZWRite,5)
         do i = 1, LEN(szstr)
            szstr(i:i) = '-'
         enddo
         do i = 1, LEN(zmstr)
            zmstr(i:i) = '-'
         enddo
         do i = 1, LEN(censtr)
            censtr(i:i) = '-'
         enddo
         do i = 1, LEN(codestr)
            codestr(i:i) = '-'
         enddo
         do i = 1, LEN(file)
            file(i:i) = '-'
         enddo
         do i = 1, LEN(exfile)
            exfile(i:i) = '-'
         enddo
         write(ZWRite,'(1x,a,2x,6(1x,a),3x,a)') arrow,file,szstr,
     &         zmstr,censtr,codestr,exarrow,exfile
         call xwrite(ZWRite,5)
         do i = 1, MAX_SLOTS
            call xistr(i,numstr,numlen)
            write(mapid,'(2a)') 'MAP', numstr(1:numlen)
            call gheads(mapid, 'FILE', file, 0, status)
c
c  Size string (WidxHgt)
c
            if ( isloaded(mapid) ) then
               call gheadi(mapid, 'SZX', di1, 0, status)
               call xistr(di1,ds1,slen1)
               if ( slen1.gt.LEN(szstr) ) then
                  szstr = 'BIG'
               else
                  call gheadi(mapid, 'SZY', di2, 0, status)
                  call xistr(di2,ds2,slen2)
                  if ( slen1 + slen2 + 1.gt.LEN(szstr) ) then
                     szstr = ds1(:slen1)
                  else
                     write(szstr,'(3a)') ds1(:slen1), 'x', ds2(:slen2)
                  endif
               endif
c
c  Rebin string
c
               call gheadd(mapid, 'ZMX', dd1, 0, status)
               di1 = INT(dd1)
               if ( di1 .ge. 1 ) then
                  call xistr(di1,ds1,slen1)
               else
                  ds1 = '<1'
                  slen1 = 2
               endif
               if ( slen1.gt.LEN(ds1) ) then
                  zmstr = '-'
               else
                  zmstr = ds1(:slen1)
               endif
c
c  Center string
c
               call gheadd(mapid, 'DRPIX1', dd1, 0, status)
               call gheadd(mapid, 'DRPIX2', dd2, 0, status)
               if ( dd1.lt.10000. .and. dd2.lt.10000 ) then
                  write(censtr,'(f6.1,a,f6.1)') dd1, ',', dd2
               else
                  dd1 = dd1/1000.
                  dd2 = dd2/1000.
                  write(censtr,'(f5.1,a,f5.1,a)') dd1,'k,',dd2,'k'
               endif
               call rmvblk(censtr)
c
c  Map codes
c
               call gheads(mapid, 'MAPCODES', ds1, 0, status)
               slen1 = LENACT(ds1)
               if ( slen1.gt.LEN(codestr) ) then
                  codestr = '<'//ds1(slen1-LEN(codestr)+2:slen1)
               else
                  codestr = ds1(:slen1)
               endif
            else 
               szstr = ' '
               zmstr = ' '
               censtr = ' '
               codestr = ' '
            endif
            arrow = '     '
            if ( mapids(IDISmap).eq.mapid ) arrow(1:1) = 'D'
            if ( mapids(ICURmap).eq.mapid ) arrow(2:2) = 'C'
            if ( arrow.ne.' ' ) arrow(3:4) = '->'
            write(mapid,'(2a)') 'EXMAP', numstr(1:numlen)
            call gheads(mapid, 'FILE', exfile, 0, status)
            exarrow = '     '
            if ( mapids(IDISmap).eq.mapid ) exarrow(1:1) = 'D'
            if ( mapids(ICURmap).eq.mapid ) exarrow(2:2) = 'C'
            if ( exarrow.ne.' ' ) exarrow(3:4) = '->'
c           write(ZWRite,'(1x,a,i2,1x,a,1x,a,i2,1x,a)') 
c    &           arrow,i,file,exarrow,i,exfile
            write(ZWRite,'(1x,a,i2,6(1x,a),i2,1x,a)') arrow,i,file,
     &            szstr,zmstr,censtr,codestr,exarrow,i,exfile
            call xwrite(ZWRite,5)
         enddo
         call xwrite(' ',5)
         call xwrite('  C-> Current Map  D-> Displayed Map ',5)
         call xwrite('  Codes: S=smooth F=flip Rb=rebin Rz=resize '//
     &               'Ro=rotate Rm=remove Rs=rescale',5)
         call xwrite('         I=int() Ft=float() '//
     &               '+,-,*,/=marith ops',5)
         call tclrun(' if { $savmode } '//
     &               '{ txwrite "  Saved Map: $savmap" 5 }', 
     &               status)
c
c  List used temporary maps
c
         ZWRite = '  Tmp Maps :'
         call mapidx('TMPMAP1', idx, status)
         maxidx = idx + MAX_TMPMAPS - 1
         found = .FALSE.
         do i = idx, maxidx
            if ( MAPlock(i) ) then
               write(ZWRite,'(a,1x,a)') ZWRite(1:LENACT(ZWRite)),
     &                                  MAPids(i)
               found = .TRUE.
            endif
         enddo
         if ( found ) call xwrite(ZWRite, 10)
         call xwrite(' ',5)
         status = 0
      endif

      return
      end
