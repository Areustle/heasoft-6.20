      subroutine wrmdb(filename, itel, status)
      implicit none
c
c  Write contents of mission db to file
c
c  I  filename   (s)  Output file ( if blank, send to STDOUT )
c  I  itel       (i)  Telescope index ( <=0 means all )
c  O  status     (i)  Error flag (0=OK)
c
      character*(*) filename
      integer*4 itel, status

      include 'mdb.inc'
c
c  Local variables
c
      integer*4 lun, istart, istop, imdb
      character(30) valstr
      character(100) dpair, ds
      integer*4 wcnt, vallen, prlen
      integer*4 i, maxwidth, sigdig, LENACT
      parameter(maxwidth=72, sigdig=10)
      character(1) quote
      parameter ( quote = '"' )
      logical ISDNULL

      status = 0

      if ( filename.ne.' ' ) then
         call XWRITE(' Writing mdb file... ', 10)
         call XWRITE(filename, 15)
         CALL GETLUN(lun)
         CALL OPENWR(lun, filename, 'UNKNOWN', ' ', 'LIST', 0, 0, 
     &               status)
         IF ( status.NE.0 ) THEN
            call XWRITE (' Error opening new mdb file', 10)
            return
         ENDIF
      else
         call XWRITE(' ', 10)
         lun = 6
      endif
c
c  Write comments (to file only)
c
      if ( filename.ne.' ' ) then
         write(lun,'(a)') '!'
         write(lun,'(a)') '!  XIMAGE mission database file'
         write(lun,'(a)') '!  Output from CHMDB command'
         write(lun,'(a)') '!'
      endif

      if ( itel.le.0 ) then
         istart = 1
         istop = ZEXpnum
      else
         istart = itel
         istop = itel
      endif
     
      ds = ' '
      do imdb = istart, istop
         if ( ZDEtnam(imdb).eq.' ' ) then
            write(ds,'(4a)') ZTElescop(imdb), ' : ',
     &                       ZINstrume(imdb), ' {'
         else
            write(ds,'(6a)') ZTElescop(imdb), ' : ',
     &         ZINstrume(imdb), ' : ', ZDEtnam(imdb), ' {'
         endif
         call RMVXBK(ds)
         write(lun,'(a)') ds(:LENACT(ds))
         ds = ' '
         wcnt = 0
         do i = 1, mdbi_num
            call xistr(int_mdb(i,imdb), valstr, vallen)
            write (dpair,'(3a)') mdbi_keys(i), ' = ',
     &                           valstr(:vallen)
            call RMVXBK(dpair)
            prlen = LENACT(dpair)
            if ( wcnt+2+prlen.gt.maxwidth ) then
               write(lun,'(a)') ds(:LENACT(ds))
               ds = ' '
               wcnt = 0
            endif
            if ( wcnt.eq.0 ) then
               write(ds,'(4x,a)') dpair(:prlen)
               wcnt = 4 + prlen
            else
               write(ds(wcnt+1:),'(2x,a)') dpair(:prlen)
               wcnt = wcnt + 2 + prlen
            endif
         enddo
         do i = 1, mdbd_num
            if ( ISDNULL(dbl_mdb(i,imdb)) ) then
               valstr = 'NULL'
               vallen = 4
            else
               call xdstr(dbl_mdb(i,imdb), sigdig, valstr, vallen)
            endif
            write (dpair,'(3a)') mdbd_keys(i), ' = ',
     &                           valstr(:vallen)
            call RMVXBK(dpair)
            prlen = LENACT(dpair)
            if ( wcnt+2+prlen.gt.maxwidth ) then
               write(lun,'(a)') ds(:LENACT(ds))
               ds = ' '
               wcnt = 0
            endif
            if ( wcnt.eq.0 ) then
               write(ds,'(4x,a)') dpair(:prlen)
               wcnt = 4 + prlen
            else
               write(ds(wcnt+1:),'(2x,a)') dpair(:prlen)
               wcnt = wcnt + 2 + prlen
            endif
         enddo
         do i = 1, mdbs_num
            write (dpair,'(5a)') mdbs_keys(i)(:LENACT(mdbs_keys(i))),
     &                            ' = ', quote, str_mdb(i,imdb)(:LENACT(
     &                            str_mdb(i,imdb))), quote
            prlen = LENACT(dpair)
            if ( wcnt+2+prlen.gt.maxwidth ) then
               write(lun,'(a)') ds(:LENACT(ds))
               ds = ' '
               wcnt = 0
            endif
            if ( wcnt.eq.0 ) then
               write(ds,'(4x,a)') dpair(:prlen)
               wcnt = 4 + prlen
            else
               write(ds(wcnt+1:),'(2x,a)') dpair(:prlen)
               wcnt = wcnt + 2 + prlen
            endif
         enddo
         if ( ds.ne.' ' ) write(lun,'(a)') ds(:LENACT(ds))
         write(lun,'(a)') '}'
         write(lun,'(a)') ' '
      enddo

      if ( filename.ne.' ' ) then
         close (lun)
         call frelun(lun)
      endif

      if ( status.ne.0 ) then
         call XWRITE(' Failed to write mdb file', 5)
      endif
            
      RETURN
      END
