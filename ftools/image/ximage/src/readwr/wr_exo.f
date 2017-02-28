      subroutine wr_exo( filename, map, szx, szy, mapid, 
     &                   maptype, i2buf, status)
      implicit none
c
c  Writes header and image to file in exosat format
c
c  I  filename   (c)  Filename for ascii file
c  I  mode       (i)  1=map 2=exmap 3=rmap
c  I  map        (r)  Image map
c  I  szx,szy    (i)  Size of map
c  I  mapid      (c)  Header id
c  I  maptype    (c)  Map data type (I=integer R=real)
c  I  i2buf     (i*2) Integer*2 buffer
c  O  status     (i)  Error flag
c
      CHARACTER*(*) filename, mapid, maptype
      INTEGER*4 szx, szy, status
      REAL*4 map(szx,szy)
      INTEGER*2 i2buf(szx)

      INTEGER*4 LENACT
      logical isrnull
c
c Local variables
c
      integer*2 header(64)
      integer*4 i, j, lun
      integer*4 irec , k , jnrt , lim , ii
      integer*4 ifr , inx , ift , ilr , iny , inyv
      integer*4 ire , ios
      real*4 rd, mapval

      include '../include/io.inc'

      ZWRite = ' Writing EXOSAT file: '//filename(:LENACT(filename))
      CALL XWRITE(ZWRite,5)
c
c  Get header
c
      call getexohdr (mapid, header, status)

c     irec or header(2) or header(13) or header(14) (13 and 14 are also the
c     number of pixels in the image) the record lenght is given in
c     is given in short words (I*2)
c
      irec = Header(13)
      k = 0
      rd = irec/64 - 1
      IF ( rd.LT.0 ) THEN
         jnrt = 64/irec
         lim = irec
      ELSE
         jnrt = 1
         lim = 64
      ENDIF
c
c     write the record lenght irec (from I*2 to byte needed for openwr)
c
      ire = irec*2
      Header(3) = jnrt + 1
c
c  Open file
c
      CALL GETLUN(lun)
      CALL OPENWR(lun, filename, 'UNKNOWN', 'D', ' ', ire, 0, status)
      IF ( status.NE.0 ) THEN
         call XWRITE (' error opening new exosat file', 10)
         return
      ENDIF
c
c  Write header to file, pad header to record size
c
      DO 100 i = 1 , jnrt
         ios = 0
         WRITE (UNIT=lun,REC=i,IOSTAT=ios,ERR=400)
     &          (Header(j),j=k+1,k+lim)
         IF ( ios.NE.0 ) GOTO 400
         k = k + irec
 100  CONTINUE
c
c first record of data
      ifr = Header(3)
c number of x and y pixels
      inx = Header(13)
      iny = Header(14)
c type of image header 1 without header 2 exosat 3 compress
      ift = Header(61)
c number of record in bytes
      irec = Header(2)*2
c last record of data
      ilr = iny + ifr - 1
c column ogf the mapm
      inyv = iny
c
c    last column of image is first data record in output file
c
c  Round reals, set negatives and nulls to zero
c   and truncate at short integer
c
      if ( maptype.ne.'I' ) then
         call xwarn(' Real data rounded to integer values', 10)
      endif
      DO 200 i = ifr , ilr
         DO 150 ii = 1 , inx
            mapval = map(ii,inyv)
            if ( isrnull(mapval) ) then
               i2buf(ii) = 0
            else
               i2buf(ii) = MAX(MIN(nint(mapval),32768),0)
            endif
 150     CONTINUE
         inyv = inyv - 1
         WRITE (lun,REC=i,IOSTAT=ios,ERR=500) (i2buf(j),j=1,inx)
         IF ( ios.NE.0 ) GOTO 500
 200  CONTINUE
      CLOSE (lun)
      CALL FRELUN(lun)

      write (ZWRite,'(1x,i15,2a)') Szx*Szy, ' records written',
     &                                  ' to file'
      call RMVXBK(ZWRite(2:))
      call XWRITE(ZWRite,5)
c
      RETURN

 400  CALL XWRITE(' Failed to write header to exosat file',10)
      Status = 1
      RETURN
c
 500  CALL XWRITE(' Failed to write image to exosat file',10)
      Status = 1
      RETURN
      END
