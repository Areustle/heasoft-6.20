      subroutine rdmdb(filename, status)
      implicit none
c
c  Read mission database from file
c
c  Format:
c
c  TELESCOP : INSTRUME : DETNAM {
c     KEY1 = VAL1  KEY2 = VAL2
c  }
c
c  Spaces are only required to separate keyword/value pairs
c  (telescop:instrume:detnam{key1=val1 key2=val2} will also work)  
c  Other spaces are there only for readability
c
c  I  filename  (s)  Text file location
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer status

      include '../include/io.inc'
      include 'mdb.inc'
c
c  Local variables
c
      integer lun, ibeg, iend, len, LENACT
      integer state, iloc, itel, expnm0, repcnt, edetidx
      character(1) ch, quote
      parameter ( quote = '"' )
      character(4) spclch
      character(100) line, tel, inst, det, key, val

      integer TELREAD, INSREAD, DETREAD, KEYREAD, STARTKEY, VALREAD
      parameter (TELREAD=1, INSREAD=2, DETREAD=3, STARTKEY=4)
      parameter (KEYREAD=5, VALREAD=6)
      parameter (spclch=':{}=')

      status = 0

      call getlun(lun)
      call openwr(lun,filename,'old',' ',' ',0,1,status)
      if ( status.ne.0 ) then
         call xwrite('MDB file not found: ', 10)
         call xwrite(filename, 10)
         call frelun(lun)
         return
      endif

      expnm0 = ZEXpnum
      repcnt = 0
      state = TELREAD
      line = ' '
      ch = ' ' 
      ibeg = 0
      iend = 0
      len = 0
      do while (.TRUE.)
         status = 0
         if ( ibeg.gt.len ) ibeg = 0
c
c  Read new line
c
         if ( ibeg.eq.0 ) then
            read(lun,'(a)',END=50,ERR=40) line
            if ( line(1:1).eq.'!' .or. line(1:1).eq.'#' ) goto 100
            len = LENACT(line)
            ibeg = 1
         endif
c
c  Bypass leading spaces
c
         do while ( line(ibeg:ibeg).eq.' ' .and. ibeg.le.len ) 
            ibeg = ibeg + 1
         enddo
         if ( ibeg.gt.len ) goto 100
c
c  Check state and parse accordingly
c
         if ( state.eq.TELREAD ) then
            call xwrite(' MDB state: TELREAD', 40)
            call findchr (line, ibeg, len, spclch, iloc, ch, status)
            if ( ch.ne.':' .or. status.ne.0 ) goto 40
            iend = iloc - 1
            tel = line(ibeg:iend)
            call rmvblk(tel)
            call upc(tel)
            call xwrite(tel, 40)
            ibeg = iend + 2
            state = INSREAD
         elseif ( state.eq.INSREAD ) then
            call xwrite(' MDB state: INSREAD', 40)
            call findchr (line, ibeg, len, spclch, iloc, ch, status)
            if ( status.eq.0 ) then
               iend = iloc - 1
               if ( ch.eq.'{' ) then
                  state = KEYREAD
               elseif ( ch.eq.':' ) then
                  state = DETREAD
               else
                  goto 40
               endif
            else
               state = STARTKEY
               iend = len
            endif
            inst = line(ibeg:iend)
            call rmvblk(inst)
            call upc(inst)
            call xwrite(inst, 40)
            if ( state.ne.DETREAD ) then
               det = ' '
               itel = edetidx(tel, inst, det)
               if ( itel.le.0 ) then
                  call xwrite(' ADDMDB: No DETNAM', 40)
                  call addmdb(tel, inst, det, itel, status)
                  if ( status.ne.0 ) goto 40
               else
                  repcnt = repcnt + 1
               endif
            endif
            ibeg = iend + 2
         elseif ( state.eq.DETREAD ) then
            call xwrite(' MDB state: DETREAD', 40)
            call findchr (line, ibeg, len, spclch, iloc, ch, status)
            if ( status.eq.0 ) then
               if ( ch.eq.'{' ) then
                  state = KEYREAD
                  iend = iloc - 1
               else
                  goto 40
               endif
            else
               state = STARTKEY
               iend = len
            endif
            det = line(ibeg:iend)
            call rmvblk(det)
            call upc(det)
            call xwrite(det, 40)
            itel = edetidx(tel, inst, det)
            if ( itel.le.0 ) then
               call xwrite(' ADDMDB: Has DETNAM', 40)
               call addmdb(tel, inst, det, itel, status)
               if ( status.ne.0 ) goto 40
            else
               repcnt = repcnt + 1
            endif
            ibeg = iend + 2
         elseif ( state.eq.STARTKEY ) then
            call xwrite(' MDB state: STARTKEY', 40)
            call findchr (line, ibeg, len, spclch, iloc, ch, status)
            if ( status.eq.0 ) then
               if ( ch.eq.'{' ) then
                  ibeg = iloc + 1
                  state = KEYREAD
               else
                  goto 40
               endif
            else
               ibeg = 0
            endif
         elseif ( state.eq.KEYREAD ) then
            call xwrite(' MDB state: KEYREAD', 40)
            call findchr (line, ibeg, len, spclch, iloc, ch, status)
            if ( status.eq.0 ) then
               if ( ch.eq.'}' ) then
                  state = TELREAD
                  ibeg = iloc + 1
               elseif ( ch.eq.'=' ) then
                  iend = iloc - 1
                  key = line(ibeg:iend)
                  call rmvblk(key)
                  call xwrite(key, 40)
                  state = VALREAD
                  ibeg = iloc + 1
               else
                  goto 40
               endif
            else
               ibeg = 0
            endif
         elseif ( state.eq.VALREAD ) then
            call xwrite(' MDB state: VALREAD', 40)
            if ( line(ibeg:ibeg).eq.quote ) then
               ibeg = ibeg + 1
               call findchr (line, ibeg, len, quote, iloc, ch, 
     &                       status)
               if ( status.eq.0 ) then
                  iend = iloc - 1
                  val = line(ibeg:iend)
                  call xwrite(val, 40)
                  ibeg = iloc + 1
                  state = KEYREAD
               else
c
c  This part will need some work if we allow quoting across lines
c
                  call XWRITE(' Missing closing quote', 10)
                  goto 40
               endif
            else
               call findchr(line, ibeg, len, ' }', iloc, ch,
     &                      status)
               if ( status.ne.0 ) then
                  iend = len
                  val = line(ibeg:len)
                  call xwrite(val, 40)
                  ibeg = 0
                  state = KEYREAD
               else
                  iend = iloc - 1
                  val = line(ibeg:iend)
                  call xwrite(val, 40)
                  ibeg = iloc + 1
                  if ( ch.eq.'}' ) then
                     state = TELREAD
                  elseif ( ch.eq.' ' ) then
                     state = KEYREAD
                  else
                     goto 40
                  endif
               endif
            endif

            call setmdb(itel, key, val, status)
            if ( status.ne.0 ) goto 40
         else
            goto 40
         endif
 100     continue
      enddo

  40  continue
      call XWRITE (' Error reading text file', 5)
      if ( state.eq.TELREAD ) then
         call XWRITE(' Failed to read telescope in line:', 10)
      elseif ( state.eq.INSREAD ) then
         call XWRITE(' Failed to read instrument in line:', 10)
      elseif ( state.eq.KEYREAD ) then
         call XWRITE(' Failed to read key in line:', 10)
      elseif ( state.eq.STARTKEY ) then
         call XWRITE(' Failed to find first { :', 10)
      elseif ( state.eq.VALREAD ) then
         call XWRITE(' Failed to read key value in line:', 10)
      else
         call XWRITE(' Bad parser state ', 10)
      endif
      call XWRITE(line, 10)
      status = -1

  50  continue

      close(lun)
      call frelun(lun)

      if ( status.ne.0 ) then
         call XWRITE(' Mission file is improperly formatted', 10)
      endif
      if ( ZEXpnum.eq.expnm0 .and. repcnt.eq.0 ) then
         CALL XWRITE(' Warning: no missions read from file',10)
      else
         WRITE (ZWRite,99004) ZEXpnum - expnm0 + repcnt
         CALL XWRITE(ZWRite,10)
         WRITE (ZWRite,99005) ZEXpnum
         CALL XWRITE(ZWRite,15)
         WRITE (ZWRite,99006) expnm0, ZEXpnum - expnm0, repcnt
         CALL XWRITE(ZWRite,15)
      endif

      return
99004 FORMAT (' No of detectors read in:',i5)
99005 FORMAT (' Total no of detectors:',i5)
99006 FORMAT (' Existing: ',i3,' New: ', i3,' Replaced: ', i3)
      end
