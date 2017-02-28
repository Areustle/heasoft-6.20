      subroutine prhead(mapid, template, status)
      implicit none
c
c print header based on template file
c
c  I  mapid    (s) Map indentifier
c  I  template (s) Location of template file
c  O  status   (i) Error flag (0=OK)
c
      character*(*) mapid, template
      integer status

      include '../include/io.inc'
      include '../include/sitedef.inc'
      include '../include/maxvals.inc'
c
c  Local variables
c
      INTEGER*4 LENACT

      integer lun
      character(80) line, comment
      character(8) keyword
      character(1) type

      character(300) ds
      integer*4 di
      real*8 dd
      logical isdnull

      status = 0

      call lkupfile(template, 'hdr', 'header template', status)
      if ( status.ne.0 ) return

      call getlun(lun)
      call openwr(lun,template,'old',' ',' ',0,1,status)
      if ( status.ne.0 ) then
         call XWRITE(' Could not open template file', 10)
         call frelun(lun)
         return
      endif

      call XWRITE(' Using template file:', 20)
      call XWRITE(template, 20)

      do while (.TRUE.)
         read(lun,'(a)',END=500,ERR=400) line
         call evaltmpl (line, mapid, keyword, di, dd, ds, type, 
     &                  comment, status)
         if ( status.ne.0 ) goto 500

         ZWRite = ' '

         if ( type.eq.'I' ) then
            write(ZWRite,'(2a,i10,2a)') keyword, ' = ', di,
     &                            ' / ', comment(:LENACT(comment))
         elseif ( type.eq.'D' ) then
            if ( isdnull(dd) ) then
               write(ZWRite,'(3a)') keyword, ' = NULL / ', 
     &                      comment(:LENACT(comment))
            else
               write(ZWRite,'(2a,1pe15.7,2a)') keyword, ' = ', dd,
     &                            ' / ', comment(:LENACT(comment))
            endif
         elseif ( type.eq.'S' ) then
            write(ZWRite,'(5a)') keyword, ' = ''', 
     &                      ds(:LENACT(ds)), ''' / ', 
     &                      comment(:LENACT(comment))
         endif

         if ( ZWRite.ne.' ' ) then
            call ftgthd(ZWRite, line, di, status)
            call XWRITE (line, 10)
         endif

      enddo

 400  call XWRITE (' Error reading template file', 10)
 500  continue

      close(lun)
      call frelun(lun)

      return
      end
