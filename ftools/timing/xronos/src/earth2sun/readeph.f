      SUBROUTINE readeph(lue,Jdutc,Frc,Rce,Rcs,Etut,Vce,ierr)

c Reads and interpolates ephemeris file (JPL data)

c This routine is called by subroutine barycen to bake barycenter corrections.
c barycen ignors etut and vce.  Rcs is used only in the general relativity
c part of subroutine barycen.

c File lue should have been opened by the calling routine.
 
C   call_var.          type I/O description
c   lue                 (i) I   lu of JPL2000 ephemeris file
CP  JDUTC               I4  I   julian day number of ephemeris lookup
CP  FRC                 R8  I   day fraction, between -0.5 and 0.5
CP  RCE(3)              R8    O vector from SSBC to Earth (light-s)
CP  RCS(3)              R8    O vector from SSBC to Sun   (light-s)
CP  ETUT                R8    O Time difference TDB-UTC (seconds)
CP  VCE(3)              R8    O time derivative of RCE
c   ierr                (i)   O error status
C
C************************ FFORM VERSION 3.0 ********** 31-JAN-90 09:20
C
CA  author : Lloyd Rawley-STX date: 14-JUL-1989
CA  update : GDWirth          date: 02-AUG-1989
CA  update : TMB              date: 27-SEP-1990 16:10 ULTRIX version
CU  update : TMB              date: 01-OCT-1990 15:47
CU  update : TMB              date: 30-OCT-1990 11:28 no header
c   update : eal HSTX         date     FEB-1994 To read from FITS ephemeris file
C
CT  status: not tested
C
C   general description:
CG  Reads and interpolates ephemeris file (JPL data), returning
CG   positions of the Earth and Sun with respect to the solar system
CG   barycenter (SSBC) and the difference between Ephemeris Time (TDB)
CG   and Universal Time (UTC) at the same epoch.
C
C   routines_called    type     description
CR  A1UTCF              R8      function returning atomic time - UTC
C
C***********************************************************************
C
C   variables   meaning
C
C     NDAYCH        I*4       (parameter) number of days of ephemeris
C                              to keep in memory
C     LUE           I*4       (parameter) logical unit for reading
C     DAYSEC        R*8       (parameter) ratio of one second to one day
C     ETATC         R*8       (parameter) time difference TDT-A1
C     RCE(3)        R*8        vector from SSBC to earth (light-s)
C     RCS(3)        R*8        vector from SSBC to sun   (light-s)
C     VCE(3)        R*8        time derivative of RCE
C     ETUT          R*8        time difference TDB-UTC (seconds)
C     EARTH(3,4,NDAYCH)
C                   R*8        earth position and its first three
C                               derivatives for each day
C     SUN(3,NDAYCH) R*8        sun positions for each day in memory
C     TDBTDT(NDAYCH)
C                   R*8        relativistic time correction (TDB-TDT)
C     TDBDOT(NDAYCH)
C                   R*8        rate of change of TDBTDT
C     TDTUT(NDAYCH) R*8        time difference TDT-UTC
C     FRC           R*8        day fraction (between -0.5 and 0.5)
C     DT            R*8        temporary variable for Taylor series
C     DT2           R*8        temporary variable for Taylor series
C     DT3           R*8        temporary variable for Taylor series
C     NSET          I*4        loop index
C     JDUTC         I*4        Julian day number of ephemeris lookup
C     JD0           I*4        beginning JD of ephemeris file
C     JD1           I*4        ending    JD of ephemeris file
C     JDCH0         I*4        first JD of ephemeris in memory
C     JDCH1         I*4        last  JD of ephemeris in memory
C     NREC          I*4        pointer to current record in ephemeris file
C     I             I*4        loop index
C     HEADER        C*120      header of ephemeris file
C
C
      IMPLICIT NONE
      include '../../include/io.inc'
      INTEGER*4 NDAYCH
      PARAMETER (NDAYCH=16)
      REAL*8 DAYSEC
      PARAMETER (DAYSEC=1.D0/86400.D0)
      REAL*8 ETATC
      PARAMETER (ETATC=32.184D0-0.0343817D0)
C
      REAL*8 Rce(3)
      REAL*8 Rcs(3)
      REAL*8 Vce(3)
      REAL*8 earth(3,4,NDAYCH)
      REAL*8 sun(3,NDAYCH)
      REAL*8 tdbtdt(NDAYCH)
      REAL*8 tdbdot(NDAYCH)
      REAL*8 tdtut(NDAYCH)
      REAL*8 Etut
      REAL*8 A1UTCF
      REAL*8 Frc
      REAL*8 dt
      REAL*8 dt2
      REAL*8 dt3
      INTEGER*4 lue
      INTEGER*4 nset
      INTEGER*4 Jdutc
      INTEGER*4 jd0
      INTEGER*4 jd1
      INTEGER*4 jdch0
      INTEGER*4 jdch1
      INTEGER*4 nrec
      INTEGER*4 i
      INTEGER*4 ierr
      character(80) comm
      double precision nulvd, dbuf
      data nulvd /-1.2d34/
      logical anynul
      data anynul /.false./
      DATA jdch0/0/
      SAVE 
      parameter (subname = 'readeph:')

C-----------------------------------------------------------------------
C     Choose set of coefficents to use; if out of range, read new chunk.
C-----------------------------------------------------------------------
      ierr = 0
      nset = 1 + (Jdutc-jdch0)
      IF ( nset.GT.NDAYCH .OR. nset.LT.1 ) THEN
C---------------------------------------------------------
C        If this is the first call, read the header record
C---------------------------------------------------------
         IF ( jdch0.EQ.0 ) THEN
            CALL ftgkyd(lue,'TSTART',dbuf,comm,ierr)
            jd0 = int(dbuf)
            CALL ftgkyd(lue,'TSTOP' ,dbuf,comm,ierr)
            jd1 = int(dbuf)
            IF ( jd1-jd0.LT.(NDAYCH-1) ) THEN
               ierr = 2
               errm = 'File is too small for interpolation'
               GOTO 999
            ENDIF
         ENDIF
C-------------------------------------------------
C           Check if date is in range of ephemeris
C-------------------------------------------------
         IF ( (Jdutc.LT.jd0) .OR. (Jdutc.GT.jd1) ) THEN
            ierr = 2
            errm = 'Out of range of ephemeris file'
            GOTO 999
         ELSE
C-----------------------------------------------------------------------
C           Read earth and sun positions and TDB-TDT from the ephemeris.
C           Get AT-UT from the leap second table in A1UTCF.
C-----------------------------------------------------------------------
            jdch1 = MIN(Jdutc+(NDAYCH-1),jd1)
            jdch0 = jdch1 - (NDAYCH-1)
            DO nset = 1 , NDAYCH
               nrec = nset + (jdch0-jd0)
               CALL ftgcvd(lue,1,nrec,1,12,nulvd,earth(1,1,nset)
     &                    ,anynul,ierr)
               CALL ftgcvd(lue,2,nrec,1, 3,nulvd,sun(1,nset)
     &                    ,anynul,ierr)
               CALL ftgcvd(lue,3,nrec,1, 1,nulvd,tdbtdt(nset)
     &                    ,anynul,ierr)
               IF(ierr.ne.0) THEN
                  errm = 'Reading ephemeris file'
                  GOTO 999
               ENDIF
               tdtut(nset) = ETATC + A1UTCF(jdch0+nset-1)
            ENDDO
C-------------------------------------------------------------------
C              Make rough computation of time derivative of TDB-TDT.
C-------------------------------------------------------------------
            DO nset = 1 , NDAYCH - 1
               tdbdot(nset) = tdbtdt(nset+1) - tdbtdt(nset)
            ENDDO
            tdbdot(NDAYCH) = tdbdot(NDAYCH-1)
            nset = 1 + (Jdutc-jdch0)
         ENDIF
      ENDIF
C----------------------------------------------------------------------
C     Now use Taylor series to get the coordinates at the desired time.
C----------------------------------------------------------------------
      dt = Frc + tdtut(nset)*DAYSEC
      dt2 = dt*dt
      dt3 = dt2*dt
      Etut = tdbtdt(nset) + dt*tdbdot(nset) + tdtut(nset)
      DO i = 1 , 3
         Rcs(i) = sun(i,nset)
         Rce(i) = earth(i,1,nset) + dt*earth(i,2,nset)
     &            + 0.5D0*dt2*earth(i,3,nset) + (1.D0/6.D0)
     &            *dt3*earth(i,4,nset)
         Vce(i) = (earth(i,2,nset)+dt*earth(i,3,nset))*DAYSEC
      ENDDO
 
      RETURN
999   continue
      errm = subname//' '//errm
      CALL xaerror(errm,1)
      return
      END
