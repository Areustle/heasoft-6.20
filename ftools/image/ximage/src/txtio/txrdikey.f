      subroutine txrdikey(filename, keyroot, icol, ivalue, status)
      implicit none
c
c  Parses integer keyword entry in command/comment section of qdp file
c
c  I  filename (s)  Location of text file
c  I  keyroot  (s)  Keyword root
c  I  icol     (i)  Column number  (If 0, keyroot is keyword)
c  O  ivalue   (i)  Integer value 
c  O  status   (r)  Error flag (0 = OK)
c
      character*(*) filename, keyroot
      integer icol, ivalue, status

      include '../include/io.inc'
      include 'txtio.inc'
c
c  Local variables
c
      character(20) keyword, ibuf, linekey
      character(80) card, value, comment
      integer i, ilen, eqidx, hdtype, LENACT
      logical found
      real*8 dd

      if ( status.ne.0 ) return

      call txrdfile(filename, status)
      if ( status.ne.0 ) return

      if ( icol.eq.0 ) then
         keyword = keyroot
      else
         call xistr(icol, ibuf, ilen)
         write (keyword,'(2a)') keyroot(1:LENACT(keyroot)), ibuf(1:ilen)
      endif
      call UPC(keyword)

      found = .FALSE.
      i = 1
      do while ( i.le.NQCom .and. .not.found )
         if ( QCOm(i)(1:5).ne.'$echo' ) goto 100
         eqidx = INDEX(QCOm(i),'=')
         if ( eqidx.eq.0 ) goto 100
         call ftgthd(QCOm(i)(6:), card, hdtype, status)
         if ( status.ne.0 ) goto 100
         if ( hdtype.ne.0 ) goto 100
         call ftpsvc(card, value, comment, status)
         if ( status.ne.0 ) goto 100
         linekey = card(1:8)
         call UPC(linekey)
         if ( keyword.eq.linekey ) then
            call strnum(value, -4, dd, status)
            ivalue = NINT(dd)
            found = .TRUE.
         endif
 100     continue
         i = i + 1
         status = 0
      enddo

      if ( found ) then
         write(ZWRite,*) ' Found txrdikey: ', keyword
         call xwrite(ZWRite, 50)
      else
         write(ZWRite,*) ' Failed to find txrdikey: ', keyword
         call xwrite(ZWRite, 50)
         status = -1
      endif

      return
      end
