**==dscprn.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
c dscprn.for
c
C copied from DU0:[300,102]DSCPRN.FTN
C
C  THIS ROUTINE READS DIRECT ACCESS DISK
C  AND FINDS TIMES FOR RECORDS
C
C MOD 21-AUG-86 TO RETURN RN OF 1ST RECORD WITH DAYS>SDAY
C MOD 03-AUG-87 TO MAKE INITIAL ESTIMATE OF DESIRED RECORD NUMBER
c MOD 04-FEB-88 TO NOT PRINT TIMES FOR EACH RECORD


c computes the record number for the desired day

 
      SUBROUTINE DSCPRN(Sday,Irn,IGPtot,DAYs)

c Igptot  I  i       Total no of rows in raw data file
C Days    I  r*4     Array containing the days of mission (of 1977)
C Sday    I  r*4     First day when source is in FOV
C Irn     O  i       Data record no in raw data file for Sday


      IMPLICIT NONE
C*** Start of declarations inserted by SPAG

      REAL DAYs(1200) , Sday
      INTEGER i , IGPtot , Irn , ist
C*** End of declarations inserted by SPAG
C
C
C DO CRUDE RUN THROUGH OF DAYS
      ist = 1
      DO i = 1 , IGPtot , 30
         IF ( DAYs(i).LE.Sday ) THEN
            ist = i
            GOTO 100
         ENDIF
      ENDDO
 100  Irn = 3
      DO i = ist , IGPtot
         Irn = 3 + 12*(i-1)
         IF ( DAYs(i).GE.Sday ) GOTO 200
      ENDDO
 200  RETURN

      END
