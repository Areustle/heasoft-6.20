**==tbldal.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* Loads the aliass table
      SUBROUTINE TBLDAL(File_alias,Ierr)
 
*
      IMPLICIT NONE
 
      CHARACTER*(*) File_alias
      INTEGER*4 Ierr
      LOGICAL*4 first_time
      SAVE first_time
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 lun
      INTEGER*4 i
      INTEGER*4 j
      INTEGER*4 LENACT
      character(100) zwrite
      character(1000) inline
      DATA first_time/.TRUE./
 
      CALL GETLUN(lun)
      IF ( first_time ) THEN
         first_time = .FALSE.
         TBLacnt = 0
      ENDIF
 
C      OPEN (lun,FILE=File_alias,IOSTAT=Ierr,STATUS='old')
      CALL OPENWR(lun,File_alias,'old',' ',' ',0,1,Ierr)
 
      IF ( Ierr.NE.0 ) THEN
         WRITE (zwrite,99001)
     &       File_alias(:MIN(len(zwrite)-42,LENACT(File_alias))) , Ierr
         CALL XWRITE(zwrite,5)
         RETURN
      ELSE
 
         READ (lun,'(a)',IOSTAT=Ierr) inline
 
         DO WHILE ( Ierr.EQ.0 )
            TBLacnt = TBLacnt + 1
            IF ( TBLacnt.GT.TBLAMAX ) THEN
               CALL XWRITE(' alias table too small',5)
               TBLacnt = TBLAMAX
               GOTO 100
            ENDIF
 
            i = 1
            j = 1
            TBLaname(TBLacnt) = ' '
            TBLacomm(TBLacnt) = ' '
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLaname(TBLacnt))) )
               TBLaname(TBLacnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(TBLacomm(TBLacnt))) )
               TBLacomm(TBLacnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
 
            CALL YSTCLS1(TBLaname(TBLacnt))
            CALL YSTCLS1(TBLacomm(TBLacnt))
            CALL YSTCLQ1(TBLaname(TBLacnt))
            CALL YSTCLQ1(TBLacomm(TBLacnt))
            READ (lun,'(a)',IOSTAT=Ierr) inline
 
         ENDDO
         Ierr = 0
      ENDIF
 
 
 100  CLOSE (lun)
      CALL FRELUN(lun)
 
 
      RETURN
99001 FORMAT (' Error opening load_commands file ',a,' no ',i4)
      END
