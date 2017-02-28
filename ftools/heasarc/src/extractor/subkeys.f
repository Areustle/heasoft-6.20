c This is a subroutine to read in subspace keywords from 
c XMM-Newton and Chandra data 

      SUBROUTINE GETSUBKEYS(Unit,Status)

      IMPLICIT NONE

      INTEGER Unit,Status

      INCLUDE 'expar.inc'
      INCLUDE 'keys.inc'
      INCLUDE 'subkeys.inc'

      INTEGER i,j,n
      character(30) keynm
      character(80) tmpstr
      character(100) errormsg

      INTEGER lenact
      EXTERNAL lenact

      IF(Status.NE.0) RETURN

c Read the DSTYP# keywords.

      keynm='DSTYP'
      CALL FTGKNS(unit, keynm(1:lenact(keynm)), 1, MAXCOLS, dstyp, 
     &            ndstyp, Status) 
      errormsg = 'Failed to read DSTYP keywords'
      IF ( Status .NE. 0 ) GOTO 999

      IF( ndstyp .EQ. 0 ) RETURN

c Set idstime to point to the DSTYP# entry for TIME which has a DSVAL# equal
c to TABLE.

      idstime = 0
      DO i = 1, ndstyp
         CALL upc(dstyp(i))
         IF ( dstyp(i) .EQ. 'TIME' ) THEN
            keynm='DSVAL'
            dsval(1,i) = ' '
            CALL DSSCAT(keynm, 1, i)
            CALL FTGKYS(unit, keynm(1:LENACT(keynm)), dsval(1,i),
     &                  errormsg, Status)
            IF ( Status .EQ. 0 .AND. dsval(1,i)(1:5) .EQ. 'TABLE' ) THEN
               idstime = i
            ENDIF
            Status = 0
         ENDIF
      ENDDO

c Read the DSUNI# keywords

      DO i = 1, ndstyp
         dsuni(i) = ' '
      ENDDO
      keynm='DSUNI'
      CALL FTGKNS(unit, keynm(1:lenact(keynm)), 1, ndstyp, dsuni, 
     &            n, Status) 
      errormsg = 'Failed to read DSUNI keywords'
      IF ( Status .NE. 0 ) GOTO 999

c Read the DSFORM# keywords

      DO i = 1, ndstyp
         dsform(i) = ' '
      ENDDO
      keynm='DSFORM'
      CALL FTGKNS(unit, keynm(1:lenact(keynm)), 1, ndstyp, dsform, 
     &            n, Status) 
      errormsg = 'Failed to read DSFORM keywords'
      IF ( Status .NE. 0 ) GOTO 999

c Read the #DSVAL# values

      DO i = 1, ndstyp
         j = 1
         Status = 0
         DO WHILE ( Status .EQ. 0 )
            keynm='DSVAL'
            dsval(j,i) = ' '
            CALL DSSCAT(keynm, j, i)
            CALL FTGKYS(unit,keynm(1:LENACT(keynm)),dsval(j,i),
     &                  errormsg,Status)
            IF(Status.NE.0) THEN
               dsval(j,i) = ' '
            ENDIF
            j = j + 1
         ENDDO
         idsval(i)=j-2
      ENDDO

      Status = 0

c Read the #DSREF# values.

      DO i = 1, ndstyp
         j = 1
         Status = 0
         DO WHILE ( Status .EQ. 0 )
            keynm = 'DSREF'
            dsref(j,i) = ' '
            CALL DSSCAT(keynm, j, i)
            CALL FTGKYS(unit,keynm(1:LENACT(keynm)),tmpstr,
     &                  errormsg,Status)
            IF ( Status .EQ. 0 ) THEN
               dsref(j,i) = tmpstr
            ELSE
               dsref(j,i) = ' '
            ENDIF
            j = j + 1
         ENDDO
         idsref(i)=j-2
      ENDDO

      Status = 0

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(errormsg)
         CALL fcerrm(Status)
      ENDIF

      RETURN
      END

c ********************************************************************

      SUBROUTINE MODSUBKEY(str,nrange,a,b,status)

      IMPLICIT NONE

      character(80) str
      INTEGER nrange
      DOUBLE PRECISION a(nrange),b(nrange)
      INTEGER status

c This is a subroutine to modify the subspace keyword
c Arguments :
c    str       c       i: keyword
c    nrange    i       i: number of ranges
c    a         d       i: low value of new selected ranges
c    b         d       i: high value of new selected ranges 
c    status    i       r: 0 = OK, !0 = failure in splitsubkey


      DOUBLE PRECISION cin(2,80), cout(2,80)
      INTEGER nin, nout, i, j
      character(80) tmpstr

      INTEGER lenact
      EXTERNAL lenact

      DO i = 1, 2
         DO j = 1, 80
            cin(i,j) = 0.0d0
            cout(i,j) = 0.0d0
         ENDDO
      ENDDO

c Get the ranges from the current keyword string

      CALL SPLITSUBKEY(str, cin, nin, status)
      IF ( Status .NE. 0 ) THEN
         CALL fcerr('Failure in SPLITSUBKEYS')
         WRITE(tmpstr, '(a,i4)') 'Error Status Returned : ', Status
         CALL fcerr(tmpstr)
         RETURN
      ENDIF

c Loop round handling all the possible cases of overlapping ranges between
c current and those entered as subroutine arguments

      i = 1
      j = 1
      nout = 0
      DO WHILE(j .LE. nrange .AND. i .LE. nin)

c  cin(1,i) cin(2,i) a(j) b(j)

         IF ( cin(2,i) .LT. a(j) ) THEN
            i = i + 1

c  a(j) b(j) cin(1,i) cin(2,i)  

         ELSEIF ( cin(1,i) .GT. b(j) ) THEN
            j = j + 1

c  cin(1,i) a(j) cin(2,i) b(j)

         ELSEIF ( cin(1,i) .LT. a(j) .AND. cin(2,i) .LT. b(j) ) THEN
            nout = nout + 1
            cout(1,nout) = a(j)
            cout(2,nout) = cin(2,i)
            i = i + 1

c  cin(1,i) a(j) b(j) cin(2,i)

         ELSEIF ( cin(1,i) .LT. a(j) .AND. cin(2,i) .GE. b(j) ) THEN
            nout = nout + 1
            cout(1,nout) = a(j)
            cout(2,nout) = b(j)
            j = j + 1

c  a(j) cin(1,i) cin(2,i) b(j) 

         ELSEIF ( cin(1,i) .GE. a(j) .AND. cin(2,i) .LT. b(j) ) THEN
            nout = nout + 1
            cout(1,nout) = cin(1,i)
            cout(2,nout) = cin(2,i)
            i = i + 1

c  a(j) cin(1,i) b(j) cin(2,i)  

         ELSEIF ( cin(1,i) .GE. a(j) .AND. cin(2,i) .GE. b(j) ) THEN
            nout = nout + 1
            cout(1,nout) = cin(1,i)
            cout(2,nout) = b(j)
            j = j + 1

         ENDIF

      ENDDO

      str=' '
      IF ( nout .EQ. 0 ) RETURN

      DO  i = 1, nout
         tmpstr = ' '
         CALL DSSCAT(tmpstr, 1, int(cout(1,i)))
         tmpstr = tmpstr(1:LENACT(tmpstr))//':'
         CALL DSSCAT(tmpstr, 1, int(cout(2,i)))

         IF ( i .EQ. 1 ) THEN
            str = tmpstr(:LENACT(tmpstr))
         ELSE
            str = str(:LENACT(str))//','//tmpstr(:LENACT(tmpstr))
         ENDIF
      ENDDO

      RETURN
      END

c ************************************************************************

      SUBROUTINE SPLITSUBKEY(str,val,n,status)

      IMPLICIT NONE

      INTEGER n,status
      DOUBLE PRECISION val(2,80)
      CHARACTER*(*) str

c Splits up the value of a subspace keyword. Assumes that the keyword is form
c consists of ranges of numbers given as #:# with ranges separated by commas.
c
c  str       c        i: keyword
c  val       d        r: ranges from keyword
c  n         i        r: number of ranges
c  status    i        r: 0 = OK
c                        1 = failed to read single value
c                        2 = failed to read first value in range
c                        3 = failed to read second value in range

      INTEGER ipos, colpos, compos, i, endpos
      CHARACTER(255) contxt
      logical qdone

      Status = 0
      n = 0

c If the value is TABLE or blank we can't do anything with it 

      if ( str .EQ. 'TABLE' .OR. str .EQ. ' ' ) RETURN

      qdone =.false.
      i = 0
      ipos = 1
      CALL UPC(str)

      DO WHILE ( .NOT.qdone )

         compos = index(str(ipos:),',') + ipos - 1
         IF ( compos .LE. (ipos-1) ) THEN
            qdone = .true.
            endpos = len(str)
         ELSE
            endpos = compos - 1
         ENDIF
         i = i + 1
         colpos = index(str(ipos:endpos),':') + ipos - 1

         IF ( colpos .EQ. 0 ) THEN

            READ(str(ipos:endpos),*,iostat=Status) val(1,i)
            IF ( Status .NE. 0 ) THEN
               Status = 1
               contxt = 'Failed to read value from '//str(ipos:endpos)
               CALL fcecho(contxt)
               contxt = 'str = '//str
               CALL fcecho(contxt)
               WRITE(contxt,'(a,i5,1x,i5)') 'ipos, endpos = ', 
     &                                      ipos, endpos
               CALL fcecho(contxt)
               RETURN
            ENDIF
            val(2,i) = val(1,i)

         ELSE

            IF ( colpos .GT. 1 ) THEN
               READ(str(ipos:colpos-1),*,iostat=Status) val(1,i)
               IF ( Status .NE. 0 ) THEN
                  Status = 2
                  contxt = 'Failed to read value from '//
     &                     str(ipos:colpos-1)
                  CALL fcecho(contxt)
                  contxt = 'str = '//str
                  CALL fcecho(contxt)
                  WRITE(contxt,'(a,i5,1x,i5)') 'ipos, colpos-1 = ', 
     &                                      ipos, colpos-1
                  CALL fcecho(contxt)
                  RETURN
               ENDIF
            ENDIF
            READ(str(colpos+1:endpos),*,iostat=Status) val(2,i)
            IF ( Status .NE. 0 ) THEN
               Status = 3
               contxt = 'Failed to read value from '//
     &                  str(colpos+1:endpos)
               CALL fcecho(contxt)
               contxt = 'str = '//str
               CALL fcecho(contxt)
               WRITE(contxt,'(a,i5,1x,i5)') 'colpos+1, endpos = ', 
     &                                      colpos+1, endpos
               CALL fcecho(contxt)
               RETURN
            ENDIF

         ENDIF

         ipos = compos + 1

      ENDDO

      n = i

      RETURN
      END

c ****************************************************************************

      SUBROUTINE FINSUBKEYS(Status)

      IMPLICIT NONE

      INTEGER Status

c Routine to check whether any of the keys set correspond to subspace keywords and
c if so modify them appropriately.

      INCLUDE 'expar.inc'
      INCLUDE 'keys.inc'
      INCLUDE 'subkeys.inc'

      INTEGER MAXRANGE
      PARAMETER (MAXRANGE=80)
      INTEGER i,j,k,ilen,nrange
      DOUBLE PRECISION a(MAXRANGE),b(MAXRANGE)
      CHARACTER(255) wrtstr

      INTEGER lenact
      EXTERNAL lenact

      IF ( Status .NE. 0 ) RETURN

      DO i = 1, ndstyp

         CALL UPC(dstyp(i))
         nrange = 0

         DO j = 1, nkeys

            CALL UPC(key(j))
            IF ( dstyp(i) .eq. key(j) ) THEN

               nrange = nrange + 1
               a(nrange) = MIN(keyval(1,j), keyval(2,j))
               b(nrange) = MAX(keyval(1,j), keyval(2,j))

            ENDIF

         ENDDO

         IF ( nrange .GT. 0 ) THEN

            DO k = 1, idsval(i)
               ilen = LENACT(dsval(k,i))
               IF ( dsval(k,i)(1:ilen) .NE. 'TABLE') THEN
                  CALL MODSUBKEY ( dsval(k,i), nrange, a, b, Status)
                  IF ( Status .NE. 0 ) THEN
                     wrtstr = 
     &  ' Warning: failed to get values from DSS keyword string '//
     &  dsval(k,i)
                     CALL fcecho(wrtstr)
                     Status = 0
                  ENDIF
               ENDIF
            ENDDO

         ENDIF

      ENDDO

      RETURN
      END

c ***********************************************************************

      SUBROUTINE corsubkeys(Qinreg, Status)

      INCLUDE 'expar.inc'

      INTEGER Status
      LOGICAL Qinreg(0:MAXCCD-1)

c Subroutine to remove DS keywords referring to CCDs which have Qinreg=FALSE

      INCLUDE 'extractor.inc'
      INCLUDE 'subkeys.inc'

      DOUBLE PRECISION val(2,80)

      INTEGER iccol, j, k, nval, i

      CHARACTER(255) wrtstr

      LOGICAL qfound

      Status = 0

c Find the DSTYP corresponding to ccol

      iccol = -1
      DO j = 1, ndstyp
         IF ( dstyp(j)(:lenact(ccol)) .EQ. ccol(:lenact(ccol)) ) THEN
            iccol = j
         ENDIF
      ENDDO

c If there is no DSTYP keyword then give up now.

      IF ( iccol .EQ. -1 ) RETURN
      
c Loop round the DSVAL for the chip ID and find whether there are any
c entries for which qinreg=false


      j = 1
      DO WHILE ( j .LE. idsval(iccol) )

c Split up DSVAL

         CALL splitsubkey(dsval(j,iccol), val, nval, Status)
         IF ( Status .NE. 0 ) THEN
            CALL fcerr('Failure in SPLITSUBKEYS')
            WRITE(wrtstr, '(a,i4)') 'Error Status Returned : ', Status
            CALL fcerr(wrtstr)
            RETURN
         ENDIF

c Loop round the chips looking for cases where qinreg=TRUE and the chip
c number is included in the DSVAL keyword.

         qfound = .FALSE.
         DO k = 0, MAXCCD-1

            IF ( qinreg(k) ) THEN
               DO i = 1, nval
                  IF ( k .GE. val(1,i) .AND. 
     &                 k .LE. val(2,i) ) THEN
                     qfound = .TRUE.
                  ENDIF
               ENDDO
            ENDIF
         ENDDO

c If no match was found then remove the corresponding DSVAL and DSREF
c for all DSTYPs which have multiple DSVAL and DSREF

         IF ( qfound ) THEN

            j = j + 1

         ELSE
  
            DO k = 1, ndstyp
               IF ( idsval(k) .GT. 1 ) THEN
                  DO i = j, idsval(k)-1
                     dsval(i,k) = dsval(i+1,k)
                  ENDDO
                  idsval(k) = idsval(k) - 1
               ELSEIF ( k .EQ. iccol ) THEN
                  j = j + 1
               ENDIF
               IF ( idsref(k) .GT. 1 ) THEN
                  DO i = j, idsref(k)-1
                     dsref(i,k) = dsref(i+1,k)
                  ENDDO
                  idsref(k) = idsref(k) - 1
               ENDIF
            ENDDO

         ENDIF

      ENDDO

cd
cd      DO k = 1, ndstyp
cd         WRITE(*,*) 'DSTYP ',k, ' ', dstyp(k)(:20)
cd         DO j = 1, idsval(k)
cd            WRITE(*,*) 'DSVAL ',j, ' ', dsval(j,k)(:20)
cd         ENDDO
cd         DO j = 1, idsref(k)
cd            WRITE(*,*) 'DSREF ',j, ' ', dsref(j,k)(:20)
cd         ENDDO
cd      ENDDO
cd

      RETURN
      END

c ***********************************************************************

      SUBROUTINE DSSCAT(str,i,j)

      IMPLICIT NONE

      CHARACTER*(*) str
      INTEGER i, j

c Adds numbers to the data subspace keywords. Returns i//str//j unless
c i==1 in which case returns str//j.

      character(10) tmpsti, tmpstj

      INTEGER lenact
      EXTERNAL lenact

      IF ( i .LT. 0 .OR. j .LT. 0 ) RETURN

      IF(i.LT.10) THEN
         WRITE(tmpsti,'(I1)') i
      ELSE IF (i.lt.100) THEN
         WRITE(tmpsti,'(I2)') i
      ELSE IF (i.lt.1000) THEN
         WRITE(tmpsti,'(I3)') i
      ELSE IF (i.lt.10000) THEN
         WRITE(tmpsti,'(I4)') i
      ELSE IF (i.lt.100000) THEN
         WRITE(tmpsti,'(I5)') i
      ELSE IF (i.lt.1000000) THEN
         WRITE(tmpsti,'(I6)') i
      ENDIF

      IF(j.LT.10) THEN
         WRITE(tmpstj,'(I1)') j
      ELSE IF (j.lt.100) THEN
         WRITE(tmpstj,'(I2)') j
      ELSE IF (j.lt.1000) THEN
         WRITE(tmpstj,'(I3)') j
      ELSE IF (j.lt.10000) THEN
         WRITE(tmpstj,'(I4)') j
      ELSE IF (j.lt.100000) THEN
         WRITE(tmpstj,'(I5)') j
      ELSE IF (j.lt.1000000) THEN
         WRITE(tmpstj,'(I6)') j
      ENDIF

      IF ( i .GT. 1 ) THEN
         str = tmpsti(1:LENACT(tmpsti))//str(1:LENACT(str))
     &         //tmpstj(1:LENACT(tmpstj))
      ELSE
         str = str(1:LENACT(str))//tmpstj(1:LENACT(tmpstj))
      ENDIF

      RETURN

      END

c *******************************************************************

      SUBROUTINE g1tblrf(Ccdno, Colnam, Tblref, Status)

      IMPLICIT NONE

      INTEGER Ccdno, Status
      CHARACTER Colnam*(*), Tblref*(*)

c Routine to return the table referenced for DSTYP=Colnam and CCD Ccdno.
c    Ccdno    i      i: The CCD number
c    Colnam   c      i: The DSTYP
c    Tblref   c      r: The table reference
c    Status   i      r: 0==OK
c                       1==no DSTYP for Colnam
c                       2==no DSTYP for CCD number
c                       3==The input CCD number is not covered by the DSVAL

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
      INCLUDE 'subkeys.inc'

      DOUBLE PRECISION val(2,80)

      INTEGER icol, iccdcol, i, j, ilen, ipoint, nval

      CHARACTER(255) wrtstr

      INTEGER lenact
      EXTERNAL lenact

      Status = 0

c Find the entry for DSTYP=Colnam which has DSVAL=TABLE

      icol = -1
      DO i = 1, ndstyp
         ilen = lenact(colnam)
         IF ( dstyp(i)(:ilen) .EQ. Colnam(:ilen) ) THEN
            IF ( dsval(1,i)(1:5) .EQ. 'TABLE' ) icol = i
         ENDIF
      ENDDO

      IF ( icol .EQ. -1 ) THEN
         Status = 1
         RETURN
      ENDIF

c Get the DSTYP number for the CCD number

      iccdcol = -1
      DO i = 1, ndstyp
         ilen = lenact(ccol)
         IF ( dstyp(i)(:ilen) .EQ. ccol(:ilen) ) iccdcol = i
      ENDDO

      IF ( iccdcol .EQ. -1 ) THEN
         Status = 2
         RETURN
      ENDIF

c Now find the DSVAL that covers the input CCD number

      ipoint = -1
      DO i = 1, idsval(iccdcol)

         CALL splitsubkey(dsval(i,iccdcol), val, nval, Status)
         IF ( Status .NE. 0 ) THEN
            CALL fcerr('Failure in SPLITSUBKEYS')
            WRITE(wrtstr, '(a,i4)') 'Error Status Returned : ', Status
            CALL fcerr(wrtstr)
            RETURN
         ENDIF
         DO j = 1, nval
            IF ( Ccdno .GE. val(1,j) .AND. Ccdno .LE. val(2,j) ) THEN
               ipoint = i
            ENDIF
         ENDDO

      ENDDO

      IF ( ipoint .EQ. -1 ) THEN
         Status = 3
         RETURN
      ENDIF

c and return the DSREF leaving out any preceding colon.

      Tblref = dsref(ipoint, icol)(index(dsref(ipoint,icol),':')+1:)

      RETURN
      END

c *******************************************************************

      SUBROUTINE gtblrfs(Colnam, Tblref, Ccdnum, Ntblref, Status)

      IMPLICIT NONE

      INCLUDE 'expar.inc'

      INTEGER Ntblref, Status
      INTEGER Ccdnum(MAXCCD)
      CHARACTER Colnam*(*), Tblref(MAXCCD)*(*)

c Routine to return all tables referenced for DSTYP=Colnam
c    Colnam   c      i: The DSTYP
c    Tblref   c      r: The table references
c    Ccdnum   i      r: The CCD number corresponding to each table reference
c    Ntblref  i      r: The number of table references returned
c    Status   i      r: 0==OK
c                       1==no DSTYP for Colnam
c                       2==no DSTYP for CCD number


      INCLUDE 'subkeys.inc'
      INCLUDE 'extractor.inc'

      INTEGER icol, i, ilen, iccdcol

      INTEGER lenact
      EXTERNAL lenact

      Status = 0
      Ntblref = 0

c Find the entry for DSTYP=Colnam

      icol = -1
      DO i = 1, ndstyp
         ilen = lenact(colnam)
         IF ( dstyp(i)(:ilen) .EQ. Colnam(:ilen) ) THEN
            IF ( dsval(1,i)(1:5) .EQ. 'TABLE' ) icol = i
         ENDIF
      ENDDO

      IF ( icol .EQ. -1 ) THEN
         Status = 1
         RETURN
      ENDIF

c Find the entry for the CCD DSTYP

      iccdcol = -1
      DO i = 1, ndstyp
         ilen = lenact(ccol)
         IF ( dstyp(i)(:ilen) .EQ. ccol(:ilen) ) iccdcol = i
      ENDDO

      IF ( iccdcol .EQ. -1 ) THEN
         Status = 2
         RETURN
      ENDIF

c Load the output arrays. Reading the CCD value assumes it is in the format
c either a single integer or #:# where # are identical integers.

      Ntblref = 0
      DO i = 1, idsref(icol)
         IF ( dsref(i,icol) .NE. ' ' ) THEN
            Ntblref = Ntblref + 1
            Tblref(Ntblref) = dsref(i,icol)(index(dsref(i,icol),':')+1:)
            ilen = index(dsval(i,iccdcol),':')
            READ(dsval(i,iccdcol)(ilen+1:),*) Ccdnum(Ntblref)
         ENDIF
      ENDDO

      RETURN
      END

