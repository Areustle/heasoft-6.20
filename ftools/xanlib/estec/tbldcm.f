**==TBLDCM.spg  processed by SPAG 3.09I  at 10:53 on  6 Aug 1992
* Loads the commands
      SUBROUTINE TBLDCM(File_command,Ierr)
 
*
      IMPLICIT NONE
 
      CHARACTER*(*) File_command
      INTEGER*4 Ierr
 
      INCLUDE 'estec_tbl.inc'
 
      INTEGER*4 lun
      INTEGER*4 i
      INTEGER*4 j
      INTEGER*4 LENACT
      character(200) inline , zwrite
 
      CALL GETLUN(lun)
 
      Tblccnt = 0
      CALL OPENWR(lun,File_command,'old',' ','L',0,1,Ierr)

 
 
      IF ( Ierr.NE.0 ) THEN
         WRITE (zwrite,99001)
     &      File_command(:MIN(len(zwrite)-42,LENACT(File_command))),Ierr
C         CALL XWRITE(zwrite,5)
         RETURN
      ELSE
 
 
         READ (lun,'(a)',IOSTAT=Ierr) inline
 
         DO WHILE ( Ierr.EQ.0 )
            Tblccnt = Tblccnt + 1
            IF ( Tblccnt.GT.TBLCMAX ) THEN
               CALL XWRITE(' command table too small',10)
               Tblccnt = TBLCMAX
               GOTO 100
            ENDIF
 
            i = 1
            j = 1
            Tblcname(Tblccnt) = ' '
            Tblcdesc(Tblccnt) = ' '
            Tblcacce(Tblccnt) = ' '
            Tblcwtyp(Tblccnt) = ' '
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(Tblcname(Tblccnt))) )
               Tblcname(Tblccnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(Tblcdesc(Tblccnt))) )
               Tblcdesc(Tblccnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(Tblcacce(Tblccnt))) )
               Tblcacce(Tblccnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
            j = 1
            i = i + 1
            DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                 .AND. (j.LE.LEN(Tblcwtyp(Tblccnt))) )
               Tblcwtyp(Tblccnt)(j:j) = inline(i:i)
               i = i + 1
               j = j + 1
            ENDDO
 
            CALL YSTCLS(Tblcname(Tblccnt))
            CALL YSTCLS(Tblcdesc(Tblccnt))
            CALL YSTCLS(Tblcacce(Tblccnt))
            CALL YSTCLS(Tblcwtyp(Tblccnt))
            CALL YSTCLQ(Tblcname(Tblccnt))
            CALL YSTCLQ(Tblcdesc(Tblccnt))
            CALL YSTCLQ(Tblcacce(Tblccnt))
            CALL YSTCLQ(Tblcwtyp(Tblccnt))
            READ (lun,'(a)',IOSTAT=Ierr) inline
 
         ENDDO
         Ierr = 0
      ENDIF
 
 
 100  CLOSE (lun)
      CALL FRELUN(lun)
 
 
      RETURN
99001 FORMAT (' Error opening load_commands file ',a,' no ',i4)
      END
