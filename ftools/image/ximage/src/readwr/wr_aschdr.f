      subroutine wr_aschdr (lun, mapid, template)

      implicit none
c
c  Writes contents of header to ASCII file based on template
c
c  I  lun       (i)  Logical unit of open ascii file
c  I  mapid     (c)  Which header to print
c  I  template  (c)  Template to base output on
c
      integer lun
      character*(*) mapid, template
c
c  Local variables
c
      integer tlun, status, LENACT
      character(80) msg, line, comment
      character(8) keyword
      character(1) type

      character(100) ds
      integer*4 di
      real*8 dd
      logical isdnull

      status = 0

      call getlun(tlun)
      call openwr(tlun,template,'old',' ',' ',0,1,status)
      if ( status.ne.0 ) then
         call XWRITE(' Could not open template file', 10)
         call frelun(tlun)
         return
      endif

      call XWRITE(' Using template file:', 20)
      call XWRITE(template, 20)

      do while (.TRUE.)
         status = 0
         read(tlun,'(a)',END=50,ERR=40) line
         call evaltmpl (line, mapid, keyword, di, dd, ds, type, 
     &                  comment, status)

         msg = ' '

         if ( type.eq.'I' ) then
            write(msg,'(2a,i10,2a)') keyword, ' = ', di,
     &                               ' / ', comment(:LENACT(comment))
         elseif ( type.eq.'D' ) then
            if ( .not.isdnull(dd) ) then
               write(msg,'(2a,1pe15.7,2a)') keyword, ' = ', dd,
     &                               ' / ', comment(:LENACT(comment))
            endif
         elseif ( type.eq.'S' ) then
            if ( ds.ne.' ' ) then
               write(msg,'(5a)') keyword, ' = ''',
     &                            ds(:LENACT(ds)), ''' / ',
     &                            comment(:LENACT(comment))
            endif
         endif

         if ( msg.ne.' ' ) then
            call ftgthd(msg, line, di, status)
            write(lun, '(2a)') '!', line(:LENACT(line))
         endif

      enddo

  40  call XWRITE (' Error reading template', 10)
  50  continue

      close(tlun)
      call frelun(tlun)

      return
      end
