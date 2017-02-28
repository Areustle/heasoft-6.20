C=======================================================================
      SUBROUTINE CTALOG(DATA_DIR,IPSF,PARAM1,M,FILEID,IRET)
C=======================================================================
C* This subroutine looks whether a calibration file for a certain event
c* selection exists and returns the filename FILEID.
C*
C* Logical units:
C*    LU=2    calibration file catalog
C=======================================================================
C+ ISSUE: 2   STARTED: 29 SEP 1988    PROGRAMMER: C. VON MONTIGNY
C+            UPDATED: 24 NOV 1988    BY CVM
C+            UPDATED: 05 MAY 1991    BY AE
C+ $Id: ctalog.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C+ 2.0	E.S.Panduranga	06/21/91	Moved source from IBM to SUN.
C+					Stripped off trailing blanks.
C+					Declare undeclared variables.
C+					Form catalog file name, open it.
C+ $Log: ctalog.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:07  irby
C+ Additions to libgro - previously these codes existed in the following
C+ libraries:
C+
C+   libsenstv
C+   libsysutil
C+   libutil
C+   libftio
C+
c Revision 1.1  1996/08/15  17:27:23  programs
c Initial revision
c
c Revision 2.1  1991/09/09  17:42:41  nancy
c First controlled version on the Sun.
c
c 31 Jul 2001, DLB.  Linux version
C=======================================================================


Cesp  ! declaring variables undeclared on IBM !
      integer	ipsf, m, iret, ifle, iflg
      character	catalog*80

      CHARACTER*(*)  DATA_DIR
      character(2) FILEID
      INTEGER*2 FILENR,FILEMX
      INTEGER*2 PARAM1(M),PARAM2(29),PARDUM
      LOGICAL SELCT

      integer	i

      character(80)	id

      save

      common	/id/	id

      id = '$Id: ctalog.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

C-AE  OPEN (2, FILE='CATALOG',STATUS='UNKNOWN')
Cesp  ! Using unit 9 for Catalog now.	!
Cesp  ! First form the name of the file, and then open it !

      i = index(data_dir, ' ') - 1
      catalog = data_dir(1:i) // 'catlog'

      open(unit=9, file=catalog, status='unknown', iostat=ios)

      SELCT=.FALSE.
      IFLE=0
      IRET=99
      FILEMX=0
C-AE 1  READ(2,'(2I3,29(I3))',END=8000) IFLG,FILENR,PARAM2
    1	read(9, '(2I3,29(I3))', end=8000, iostat=ios) iflg, filenr, param2
c	write(*,*),'param1: ', param1
c	write(*,*),'param2: ', param2
C        write(*,*),'filenr: ', filenr, ' filemx: ', filemx
        IF(FILENR.GT.FILEMX) FILEMX=FILENR
C        write(*,*),'filenr: ', filenr
        PARDUM=PARAM2(7)
        PARAM2(7)=PARAM1(7)
C-AE     CALL CMPARE(PARAM1,M,PARAM2,M,SELCT)
         CALL CMPARE(PARAM1,M,PARAM2,29,SELCT)

         IF(SELCT) THEN
             WRITE(FILEID,'(I2)') FILENR
C             write(*,*),'2 filenr: ', filenr
             IF(FILENR.LT.10) WRITE(FILEID(1:1),'(''0'')')
             IFLE=1
             IF(IPSF.EQ.IFLG) THEN
               IRET=1
             IF(PARDUM.EQ.PARAM1(7)) THEN
               IRET=0
               GOTO 9999
             END IF
             END IF
         END IF
       GOTO 1

 8000  IF(IFLE.EQ.0)  IRET=2
 9999 CONTINUE

C-AE  CLOSE(2)
Cesp  ! On the sun we just need to close the file !
Cesp  rewind(9)
      close(9)

      RETURN
      END
