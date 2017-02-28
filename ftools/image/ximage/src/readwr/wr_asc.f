      subroutine wr_asc( filename, template, map, szx, szy, mapid,
     &                   maptype, sigdig, nonull, status)
      implicit none
c
c  Write integer map to ascii file
c
c  I  filename   (c) Filename for ascii file
c  I  template   (c) Header template file
c  I  mode       (i) 1=map 2=exmap 3=rmap
c  I  map        (r) Image map
c  I  szx,szy    (i) Size of map
c  I  mapid      (c) Header id
c  I  exmap      (r) Exposure map
c  I  rmap       (r) Display map
c  I  sigdig     (i) Number of significant digits
c  I  nonull     (l) If true, exclude nulls (replace w/ 0)
c  O  status     (i) Error flag
c
      CHARACTER*(*) filename, template, mapid, maptype
      INTEGER*4 szx, szy, sigdig, status
      REAL*4 map(szx,szy)
      LOGICAL nonull

      INTEGER*4 LENACT
      logical isrnull
c
c Local variables
c
      integer*4 i, j, wcnt, len1, len2
      integer*4 imin, imax, nullen
      real*8 dd
      character(300) ds
      character(30) str1, str2
      character(10) nulstr
      integer*4 lun

c
c  NULL treatment
c
      if ( nonull ) then
         if ( maptype.eq.'I' ) then
            nulstr = '0'
            nullen = 1
         else
            dd = 0.d0
            call xdstr(dd, sigdig, str1, len1)
            nulstr = str1
            nullen = len1
         endif
      else
         nulstr = 'NO'
         nullen = 2
      endif

      ds = ' Writing ASCII file: '//filename(:LENACT(filename))
      CALL XWRITE(ds,5)
      CALL GETLUN(lun)
      CALL OPENWR(lun, filename, 'UNKNOWN', ' ', 'LIST', 0, 0, status)
      IF ( status.NE.0 ) THEN
         call XWRITE (' error opening new ascii file', 10)
         return
      ENDIF
c
c  Write QDP commands
c
      call xistr(szx, str1, len1)
      call xistr(szy, str2, len2)
      write(lun,'(a,1x,i1,1x,a,1x,i1,1x,a)') 'R', 1, str1(:len1), 
     &                                            1, str2(:len2)
      call gheadd(mapid, 'DATAMIN', dd, 0, status)
      imin = dd
      call gheadd(mapid, 'DATAMAX', dd, 0, status)
      imax = dd
      call xistr(imin, str1, len1)
      call xistr(imax, str2, len2)
      write(lun,'(4a)') 'IMA MIN ', str1(:len1), ' MAX ', str2(:len2)
c
c  Write header comments
c
      call wr_aschdr (lun, mapid, template)
c
c  Write image data
c
      ds = ' '
      wcnt = 0
      do i = 1, szx
         do j = 1, szy
            if ( isrnull(map(i,j)) ) then
               str1 = nulstr
               len1 = nullen
            elseif ( maptype.eq.'I' ) then
               call xistr(NINT(map(i,j)), str1, len1)
            else
               dd = map(i,j)
               call xdstr(dd, sigdig, str1, len1)
            endif
            if ( wcnt + len1 + 1 .gt. 78 ) then
               write(lun,'(2a)') ds(:LENACT(ds)), '-'
               ds = ' '
               wcnt = 0
            endif
            write(ds,'(a,1x,a)') ds(:LENACT(ds)), str1(:len1)
            wcnt = wcnt + len1 + 1
         enddo
         if ( wcnt.gt.0 ) then
            write(lun,'(a)') ds(:LENACT(ds))
            ds = ' '
            wcnt = 0
         endif
      enddo

      if ( ds.ne.' ' ) write(lun,'(a)') ds(:LENACT(ds))

      close (lun)
      call frelun(lun)

      if ( status.eq.0 ) then
         write (ds,'(1x,i15,2a)') Szx*Szy, ' records written',
     &                                     ' to file'
         call RMVXBK(ds(2:))
         call XWRITE(ds,5)
      else
         call XWRITE(' Failed to write ASCII image', 5)
      endif
            
      RETURN
      END
