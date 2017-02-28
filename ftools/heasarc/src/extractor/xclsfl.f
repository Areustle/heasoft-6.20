c *****************************************************************

      SUBROUTINE XCLSFL(Lun, Status)

      INTEGER Lun, Status

c Set the checksums and close the file

      INTEGER i, chdu, hdutype

      CHARACTER(72) contxt


      Status = 0

      CALL FTGHDN(Lun,chdu)
      DO i = 1, chdu
         CALL FTMAHD(Lun, i, hdutype, Status)
         CALL FTPCKS(Lun, Status)
         WRITE(contxt,'(a,i4)') 
     &           'Failed to set the checksum in extension ', i
         IF ( Status .NE. 0 ) GOTO 999
      ENDDO
 
      CALL FTCLOS(Lun, Status)
      CALL FRELUN(Lun)

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF
 
      RETURN
      END

