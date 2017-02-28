      subroutine rd_exosz(Filename, Szx, Szy, Rec1, Nbyte, Status)
      implicit none
c
c  Open an image file of type exosat with direct access
c  without knowing the record length in advance
c  Then, reads header to determine size of image
c  (From RDHEA routine)
c
c  I  Filename    (c) Name of exosat file
c  O  Szx         (i) Size of image in x
c  O  Szy         (i) Size of image in y
c  O  Rec1        (i) First record
c  O  Nbyte       (i) Number of bytes
c  O  Status      (i) Error flag  ( 0 = OK )
c
      character*(*) Filename
      integer*4 Szx, Szy, Rec1, Nbyte, Status
c
c  Local variables
c
      integer i, lun
      integer*2 header(64)

      Status = 0

      call getlun(lun)
      CALL OPENWR(lun,Filename,'OLD','D',' ',32,1,Status)
      IF ( Status.NE.0 ) THEN
         CALL XWRITE(' Error opening exosat file',5)
         CALL XWRITE(Filename,5)
         goto 100
      ENDIF
      READ (lun,REC=1,IOSTAT=Status) (header(i),i=1,16)
      IF ( Status.NE.0 ) THEN
         CALL XWRITE (' Error finding record length', 5)
         CALL XWRITE(Filename,5)
         goto 100
      ENDIF

      Rec1 = header(3)
      Nbyte = header(13)*2
      Szx = header(13)
      Szy = header(14)
 
  100 continue
      CLOSE (lun)
      call frelun(lun)

      return

      END
