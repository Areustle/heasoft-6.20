**==TBLDKW.spg  processed by SPAG 3.09I  at 11:00 on 20 Aug 1992
* Loads the keywords table
      SUBROUTINE TBLDKW(File_keyword,Ierr)
 
*
      IMPLICIT NONE
 
      CHARACTER*(*) File_keyword
      INTEGER*4 Ierr
 
      INCLUDE 'estec_tbl.inc'
 
      INTEGER*4 lun
      INTEGER*4 i
      INTEGER*4 j
      INTEGER*4 LENACT
      character(200) inline , zwrite
 
      CALL GETLUN(lun)
 
      Tblwcnt = 0
      CALL OPENWR(lun,File_keyword,'old',' ','L',0,1,Ierr)
 
 
      IF ( Ierr.NE.0 ) THEN
         WRITE (zwrite,99001) File_keyword(:LENACT(File_keyword)) , Ierr
C         CALL XWRITE(zwrite,5)
         RETURN
      ELSE
 
 
         READ (lun,'(a)',IOSTAT=Ierr) inline
 
         DO WHILE ( Ierr.EQ.0 )
            Tblwcnt = Tblwcnt + 1
            IF ( Tblwcnt.GT.TBLWMAX ) THEN
               CALL XWRITE(' keyword table too small',10)
               Tblwcnt = TBLWMAX
               GOTO 100
            ENDIF
 
            i = 1
            j = 1
            Tblwname(Tblwcnt) = ' '
            Tblwcomm(Tblwcnt) = ' '
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(Tblwname(Tblwcnt))) )
               Tblwname(Tblwcnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(Tblwcomm(Tblwcnt))) )
               Tblwcomm(Tblwcnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
 
            CALL YSTCLS(Tblwname(Tblwcnt))
            CALL YSTCLS(Tblwcomm(Tblwcnt))
            CALL YSTCLQ(Tblwname(Tblwcnt))
            CALL YSTCLQ(Tblwcomm(Tblwcnt))
            READ (lun,'(a)',IOSTAT=Ierr) inline
 
         ENDDO
         Ierr = 0
      ENDIF
 
 
 100  CLOSE (lun)
      CALL FRELUN(lun)
 
 
      RETURN
99001 FORMAT (' Error opening load_commands file ',a,' no ',i4)
      END
