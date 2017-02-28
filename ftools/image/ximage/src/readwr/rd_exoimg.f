      subroutine rd_exoimg(Lun, Rec1, Map, I2map, Szx, Szy, Status)
      implicit none
c
c  Reads an image from exosat file into integer map
c  (formerly RDIMA)
c
c  I  Lun      (i)  Logical unit of open exosat file
c  I  Rec1     (i)  First record
c  O  Map      (r)  Image map
c  O  I2map   (i*2) Temprary buffer
c  I  Szx      (i)  Size of maps in x
c  I  Szy      (i)  Size of maps in y
c  O  Status   (i)  Error flag (0 = OK)
c
      integer*4 Lun, Szx, Szy, Rec1, Status
      real*4 Map(Szx,Szy)
      integer*2 I2map(Szx,Szy)
c
c  Local variables
c
      integer*4 firstrec , lastrec
      integer*4 ii, i, column
      character(100) ds

      firstrec = Rec1
      lastrec = Szy + firstrec - 1
      column = Szy
c
c read the data image(*,*): n.b. last column of image is first
c data record of image file
c
      DO 100 i = firstrec , lastrec
         READ (Lun,REC=i,IOSTAT=Status) (I2map(ii,column),ii=1,Szx)
         do ii = 1, Szx
            Map(ii,column) = I2map(ii,column)
         enddo
         column = column - 1
         IF ( Status.NE.0 ) THEN
            WRITE (ds,99001) Status
            CALL XWRITE(ds,10)
            RETURN
         ENDIF
 100  CONTINUE
      IF ( column.NE.0 ) THEN
         WRITE (ds,99002) column
         CALL XWRITE(ds,10)
         RETURN
      ENDIF

99001 FORMAT (' Error ',i3,' in reading image from exosat file')
99002 FORMAT (
     &' The number of columns still to be read in the               imag
     &e should be 0, instead it is ',i3)
      END
