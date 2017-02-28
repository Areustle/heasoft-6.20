**==readday.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
C
C Author: Jesse Allen
C History:
C  Version 0.0  03/29/1998  First draft
C  Version 1.0  10/30/1998  Lorraine Breedon 
C                           Parse Database filename rather than fov, fitsunit\
C                           Access DETNAM keyword to determine fov size.\

C This is a subroutine to read HEAO-1 A-2 DSDISK raw data header-type
C information.
C
      SUBROUTINE READDAY(rawunit,rawfile,I3fov,Igptot,Irntot,Days,
     &                       Status)

c rawunit O  i       logical unit no for raw datafile
c rawfile I  ch*100  raw data filename
c I3fov   O  i       Integer flag for instrument fov
c                    (0 for 1.5x3.0 deg fov ; 1 for 3.0x3.0 deg fov)
c Igptot  O  i       Total no of rows in raw data file
c Irntot  O  i       Total no of data records in raw data file
C Days    O  r*4     Array containing the days of mission (of 1977)

 
 
      IMPLICIT NONE
 
      LOGICAL anyf
 
      INTEGER rawunit , Irntot , Igptot , blocksize , hdutype,
     &        Status, I3fov,strlen,lenact,i
 
      REAL Days(1200)
 
      character(80) errm , st1,comment
      character(15) fov_string
      CHARACTER*(*) rawfile 
 
 
      Status = 0

c get free logical unit number
      CALL XGTLUN(rawunit,Status)
      IF (Status .NE.0 ) THEN
         errm = ' Problem obtaining free unit number'
         CALL XAERROR(errm,1)
         return
      ENDIF

  
      CALL FTOPEN(rawunit,rawfile,0,blocksize,Status)
      IF ( Status.NE.0 ) THEN
         WRITE (errm,
     &'('' Unable to open raw fits data base : '',a40)')
     & rawfile
         CALL XAERROR(errm,1)
         return
      ENDIF
 
      CALL FTMRHD(rawunit,1,hdutype,Status)
      CALL FTGKYS(rawunit,'DETNAM',fov_string,comment,Status)
      strlen=lenact(fov_string)

C determine wether large or small fov
      IF ((fov_string(:strlen) .NE. 'MEDs+HED_3s') .AND.
     & (fov_string(:strlen) .NE. 'MEDl+HED_3l')) THEN
         WRITE (errm,
     &'('' ERROR : DETNAM keyword '',a15,'' is incorrect !'')')
     & fov_string
         CALL XAERROR(errm,1)
         WRITE (errm,
     &'('' The current data base is : '',a80)')
     & rawfile
         CALL XAERROR(errm,1)
         WRITE (errm,
     &'('' DETNAM value should be MEDs+HED_3s or MEDl+HED_3l'')')
          CALL XAERROR(errm,1)
         WRITE (errm,
     &'('' ...hence the current data base is INVALID ! '')')
         CALL XAERROR(errm,1)
         Status = 1
         return
      ELSEIF (fov_string(:strlen) .EQ. ('MEDs+HED_3s')) THEN
          I3fov = 0
      ELSEIF (fov_string(:strlen) .EQ. ('MEDl+HED_3l')) THEN
          I3fov = 1
      ENDIF

      CALL FTGKYJ(rawunit,'NAXIS2',Igptot,comment,Status)
      DO i = 1 , Igptot
         CALL FTGCVE(rawunit,1,i,1,1,0.0,Days(i),anyf,Status)
      ENDDO
      Irntot = 12*Igptot + 1
 
      IF ( Igptot.LT.2 .OR. Igptot.GT.1200 ) THEN
 
         WRITE (st1,*) 'Illegal IGPTOT value, IGPTOT = ' , Igptot
         CALL XWRITE(st1,20)
 
         STOP
      ENDIF
 
      RETURN
      END
