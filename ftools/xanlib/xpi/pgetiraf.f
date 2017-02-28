**==pgetiraf.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
* This gets the default value of a parameter out of a parameter file
* from an iraf task.  It depends on PFILES pointing to the system parameter
* files and UPARM pointing to your copy.
 
      SUBROUTINE PGETIRAF(Parfile,Parameter,Value,Status)
 
      CHARACTER*(*) Parfile , Parameter , Value
      INTEGER*4 Status
 
*
* parfile is the parameter file you want the info from
*   eg.  fsort.par
*
* parameter is the parameter name
*
* value is the string value of that parameter
*
* status is 0 if ok
*	1 parameter file not found
*	2 parameter file not copied
*	3 parameter not found
*
*
 
      character(1000) pfname
* parameter table
 
* max number of parameters
      INTEGER*4 TBLPMAX
      PARAMETER (TBLPMAX=300)
* count of parameters
      INTEGER*4 tblpcnt
* parameter name
      character(20) tblpname(TBLPMAX)
* parameter description
      character(80) tblpdesc(TBLPMAX)
* parameter type
      character(5) tblptype(TBLPMAX)
* parameter minimum
      character(10) tblpminp(TBLPMAX)
* parameter maximum
      character(10) tblpmaxp(TBLPMAX)
* parameter default
      character(50) tblpdefl(TBLPMAX)
* parameter update
      character(10) tblpupd(TBLPMAX)
* parameter actual update
      character(10) tblpupda(TBLPMAX)
 
* Table holding status of paramter
* .true. = it's still good, .false. = it's been read
 
* parameter filename
      character(100) tblpfname
 
 
      INTEGER*4 lun
      INTEGER*4 i
      INTEGER*4 j , tblfpr , ierr
      INTEGER*4 LENACT
      character(1000) inline , zwrite
      character(1) tchar
 
 
      character(80) p1
      character(80) p2
 
      Status = 0
 
      CALL XCHATY(10,10)
 
      CALL TBFDPRIRAF(Parfile,pfname,Status)
      IF ( Status.NE.0 ) RETURN
 
 
 
 
      CALL GETLUN(lun)
 
      tblpfname = pfname
 
      tblpcnt = 0
      OPEN (lun,FILE=pfname,IOSTAT=ierr,STATUS='old')
 
 
      IF ( ierr.NE.0 ) THEN
         WRITE(zwrite,99001)pfname(:MIN(len(zwrite)-42,LENACT(pfname))),
     &                      ierr
         CALL XWRITE(zwrite,5)
         Status = 1
         RETURN
      ELSE
 
 
         READ (lun,'(a)',IOSTAT=ierr) inline
 
         DO WHILE ( ierr.EQ.0 )
            IF ( inline(1:1).NE.'#' .AND. inline.NE.' ' ) THEN
               tblpcnt = tblpcnt + 1
               IF ( tblpcnt.GT.TBLPMAX ) THEN
                  CALL XWRITE(' parameter table too small',5)
                  tblpcnt = TBLPMAX
                  GOTO 100
               ENDIF
 
               i = 1
               j = 1
               tblpname(tblpcnt) = ' '
               tblpdesc(tblpcnt) = ' '
               tblptype(tblpcnt) = ' '
               tblpminp(tblpcnt) = ' '
               tblpmaxp(tblpcnt) = ' '
               tblpdefl(tblpcnt) = ' '
               tblpupd(tblpcnt) = ' '
               tblpupda(tblpcnt) = ' '
               DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                    .AND. (j.LE.LEN(tblpname(tblpcnt))) )
                  tblpname(tblpcnt)(j:j) = inline(i:i)
                  i = i + 1
                  j = j + 1
               ENDDO
               j = 1
               i = i + 1
               DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                    .AND. (j.LE.LEN(tblptype(tblpcnt))) )
                  tblptype(tblpcnt)(j:j) = inline(i:i)
                  i = i + 1
                  j = j + 1
               ENDDO
               j = 1
               i = i + 1
               DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                    .AND. (j.LE.LEN(tblpupda(tblpcnt))) )
                  tblpupda(tblpcnt)(j:j) = inline(i:i)
                  i = i + 1
                  j = j + 1
               ENDDO
               j = 1
               i = i + 1
               tchar = ','
               IF ( inline(i:i).EQ.'"' ) THEN
                  tchar = '"'
                  i = i + 1
               ENDIF
               DO WHILE ( (inline(i:i).NE.tchar) .AND. 
     &                    (i.LE.LEN(inline)) .AND. 
     &                    (j.LE.LEN(tblpdefl(tblpcnt))) )
                  tblpdefl(tblpcnt)(j:j) = inline(i:i)
                  i = i + 1
                  j = j + 1
               ENDDO
               IF ( tchar.EQ.'"' ) i = i + 1
               j = 1
               i = i + 1
               DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                    .AND. (j.LE.LEN(tblpminp(tblpcnt))) )
                  tblpminp(tblpcnt)(j:j) = inline(i:i)
                  i = i + 1
                  j = j + 1
               ENDDO
 
               j = 1
               i = i + 1
               DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
     &                    .AND. (j.LE.LEN(tblpmaxp(tblpcnt))) )
                  tblpmaxp(tblpcnt)(j:j) = inline(i:i)
                  i = i + 1
                  j = j + 1
               ENDDO
 
 
               j = 1
               i = i + 1
C               DO WHILE ( (inline(i:i).NE.',') .AND. (i.LE.LEN(inline))
C     &                    .AND. (j.LE.LEN(Tblpdesc(Tblpcnt))) )
C                  Tblpdesc(Tblpcnt)(j:j) = inline(i:i)
C                  i = i + 1
C                  j = j + 1
C               ENDDO
               tblpdesc(tblpcnt) = inline(i:LEN(inline))
               CALL YSTCLS1(tblpname(tblpcnt))
               CALL YSTCLS1(tblpdesc(tblpcnt))
               CALL YSTCLS1(tblptype(tblpcnt))
               CALL YSTCLS1(tblpminp(tblpcnt))
               CALL YSTCLS1(tblpmaxp(tblpcnt))
               CALL YSTCLS1(tblpdefl(tblpcnt))
               CALL YSTCLS1(tblpupda(tblpcnt))
               CALL YSTCLQ1(tblpname(tblpcnt))
               CALL YSTCLQ1(tblpdesc(tblpcnt))
               CALL YSTCLQ1(tblptype(tblpcnt))
               CALL YSTCLQ1(tblpminp(tblpcnt))
               CALL YSTCLQ1(tblpmaxp(tblpcnt))
               CALL YSTCLQ1(tblpdefl(tblpcnt))
               CALL YSTCLQ1(tblpupda(tblpcnt))
            ENDIF
            READ (lun,'(a)',IOSTAT=ierr) inline
 
         ENDDO
         ierr = 0
      ENDIF
 
 
 100  CLOSE (lun)
      CALL FRELUN(lun)
 
 
      p1 = Parameter
      CALL UPC(p1)
      tblfpr = 0
 
      DO 200 i = 1 , tblpcnt
         p2 = tblpname(i)
         CALL UPC(p2)
         IF ( p1.EQ.p2 ) THEN
            tblfpr = i
            RETURN
         ENDIF
 200  CONTINUE
 
      tblfpr = 0
 
      IF ( tblfpr.EQ.0 ) THEN
         Status = 3
         RETURN
      ENDIF
 
 
      Value = tblpdefl(tblfpr)
      Status = 0
 
 
      RETURN
99001 FORMAT (' Error opening load_commands file ',a,' no ',i4)
      END
