**==tbldkw.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* Loads the keywords table
      SUBROUTINE TBLDKW1(File_keyword,Ierr)
 
*
      IMPLICIT NONE
 
      CHARACTER*(*) File_keyword
      INTEGER*4 Ierr
 
      INCLUDE 'tbl.inc'
 
      INTEGER*4 lun
      INTEGER*4 i
      INTEGER*4 j
      INTEGER*4 LENACT
      character(1000) inline , zwrite
 
      CALL GETLUN(lun)
 
      TBLwcnt = 0
      CALL OPENWR(lun,File_keyword,'old',' ','L',0,1,Ierr)
 
 
      IF ( Ierr.NE.0 ) THEN
         WRITE (zwrite,99001)
     &      File_keyword(:MIN(len(zwrite)-42,LENACT(File_keyword))),Ierr
         CALL XWRITE(zwrite,5)
         RETURN
      ELSE
 
 
         READ (lun,'(a)',IOSTAT=Ierr) inline
 
         DO WHILE ( Ierr.EQ.0 )
            IF ( inline.NE.' ' ) THEN
               TBLwcnt = TBLwcnt + 1
               IF ( TBLwcnt.GT.TBLWMAX ) THEN
                  CALL XWRITE(' keyword table too small',5)
                  TBLwcnt = TBLWMAX
                  GOTO 100
               ENDIF
 
               i = 1
               j = 1
               TBLwname(TBLwcnt) = ' '
               TBLwcomm(TBLwcnt) = ' '
               DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                    .AND. (j.LE.LEN(TBLwname(TBLwcnt))) )
                  TBLwname(TBLwcnt)(j:j) = inline(i:i)
                  i = i + 1
                  j = j + 1
               ENDDO
               j = 1
               i = i + 1
               DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                    .AND. (j.LE.LEN(TBLwcomm(TBLwcnt))) )
                  TBLwcomm(TBLwcnt)(j:j) = inline(i:i)
                  i = i + 1
                  j = j + 1
               ENDDO
 
               CALL YSTCLS1(TBLwname(TBLwcnt))
               CALL YSTCLS1(TBLwcomm(TBLwcnt))
               CALL YSTCLQ1(TBLwname(TBLwcnt))
               CALL YSTCLQ1(TBLwcomm(TBLwcnt))
            ENDIF
            READ (lun,'(a)',IOSTAT=Ierr) inline
 
         ENDDO
         Ierr = 0
      ENDIF
 
 
 100  CLOSE (lun)
      CALL FRELUN(lun)
 
 
      RETURN
99001 FORMAT (' Error opening load_commands file ',a,' no ',i4)
      END
