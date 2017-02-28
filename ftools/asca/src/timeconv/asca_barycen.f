C  FTOOLs info $Header: /headas/headas/ftools/asca/src/timeconv/asca_barycen.f,v 3.11 2002/12/26 16:42:26 irby Exp $
C
      Subroutine asca_barycen(geotime,barytime)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : barycentric correction by JPL ephemeris correction
C-              using "JPLEPH" by Dr. John Deeter
C-
C-   Modified 26-JUL-1993   Y.Sekimoto
C-   Modified 20-JUN-1993   M.Hirayama
C-    Name of this routine changed (barycen -> asca_barycen)
C-    Name of this routine changed (barycen_init -> asca_barycen_init)
C-    Argument mjdref removed (-> constant in the routine)
C-   Modified 4-Aug-1997  Jeff Guerber, Hughes STX Corp, NASA-Goddard
C-    Increased GEOFIL, in common/jplsub/, from char*50 to *160.
C-   Modified 1998-11-09   Jeff Guerber
C-    In BCJPL(), removed READONLY from the OPEN statement since some versions
C-    of g77 object. Also removed unused variable LEN to prevent warning.
C-
C
C     NOTE: At first glance, it appears that the function ETMIUT, which
C     is called by JPLEPH, often returns the wrong result because its
C     internal leap-seconds table only goes up to June 1992.  This would
C     normally be true, except that here the input is not UTC but Ascatime,
C     which is by definition TAI-27.0 sec. (ie, UTC as of 1992 July 1).
C     Since ASCA was launched in 1993, ETMIUT will always return 59.184 sec,
C     which is just the difference between Ascatime and TDT (= TAI+32.184),
C     and that's what we want.  (JRG, RSTX/GSFC, Mar. 1998)
C
C----------------------------------------------------------------------
      Implicit None
C----------------------------------------------------------------------
c input :
      Real*8 geotime                    ! ascatime at Geocenter
c      Real*8 mjdref                     ! mjd reference time for asca
c output
      Real*8 barytime                   ! ascatime at Barycenter
c      Real*8 mjdcor                     ! ET by MJD at Solar Barycentror
c const :
      Real*8 DAY2SEC
      Parameter (DAY2SEC = 86400.d0)
      Real*8 JD2MJD
      Parameter (JD2MJD=2400000.5d0)
      Real*8 MJDREF
      Parameter (MJDREF=48988.d0)
c local
      Real*8 utd,utf                    ! UTC integer & fraction
      Real*8 etd,etf                    ! ET  integer & fraction

c      utd = int(mjd+JD2MJD)
c      utf = mjd+JD2MJD-utd

      utd = int(JD2MJD + MJDREF + geotime/DAY2SEC)
      utf = ((JD2MJD - utd) + MJDREF) + geotime/DAY2SEC

      Call JPLEPH(utd,utf,etd,etf)

c      mjdcor = etd-JD2MJD+etf

      barytime = (( (etd - JD2MJD) - MJDREF ) + etf )*DAY2SEC

  999 Return
      End
c--------------
c asca_barycent_init
c---------------
      Subroutine asca_barycen_init(alptag,deltag,geofile)
      Implicit None
c input :
      Real*8 alptag,deltag              ! alpha & delta of target by
      Character*(*) geofile
      double precision deg2rad
      ! J2000.0 in degree
c Common
      Common  /JPLSUB/ pos,GEOFIL
      Real*8 pos(3)
      Character GEOFIL*160
c deg2rad added sincs cosd,sind not portable
      deg2rad = 1.745329251994330d-02

c GEOFIL by John Deeter
c      GEOFIL = '/utk/home/sekimoto/barycen/deeter/earth.dat'
      GEOFIL = geofile
c      write(*,*) 'GEOFIL in init = ', GEOFIL

C BEO
      pos(1) = cos(deg2rad*alptag)*cos(deg2rad*deltag)
      pos(2) = sin(deg2rad*alptag)*cos(deg2rad*deltag)
      pos(3) = sin(deg2rad*deltag)
      Return
      End
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c********************************************************
c
c Program to make JPL barycentric correction
c
c Seven files:
c
c 1.  explain.txt  (this file).
c
c 2.  jpltest.for  (sample program for using routines).
c                  PROGRAM TEST
c
c 3.  j2000.for    (routine to precess from 1950 to 2000)
c                  SUBROUTINE J2000(ALFI,DELI,UALFI,UDELI,EFLAG,
c                 &                 ALFO,DELO,UALFO,UDELO)
c
c 4.  jpleph.for   (routines to interpolate on table of
c                 earth-SSBC vectors and compute time correction)
c                 SUBROUTINE JPLEPH(UTD,UTF,ETD,ETF)
c                 FUNCTION   ETMIUT(UTC)
c                 SUBROUTINE DMS(ANG,IANG,DEG,XMIN,SEC)
c                 FUNCTION   BCJPL(ETD,ETF)
c                 FUNCTION   BESSEL(TAB,JO,P)
c
c 5.  earth.dat    (table of earth-SSBC vectors, identical to
c                 table in Astronomical Almanac)
c
c 6.  jpltest.jcl  (sample control file for Facom)
c
c 7.  jpltest.com  (sample control file for Vax)
c
c
c Notes:
c
c (a) subroutine J2000 precesses year 1950 coordinates to year
c     2000, including corrections for proper motion if this is
c    supplied.  'Old' coordinates by convention contained a
c     small offset called the 'e-term of aberration, 'new'
c    (J2000) coordinates do not.  This offset is removed if
c     eflag=.true., and is assumed to be already removed if
c     eflag=.false.  This routine was copied from a recent issue
c     of the (American) Astronomical Almanac.
c
c (b) subroutine JPLEPH is the routine to convert supplied UTC
c     at the geocenter to ET at the solar system barycenter.
c     To maintain microsecond precision in the computations,
c     the input and output times are divided into integer
c     and fractional parts (UTD and UTF for input;
c     ETD and ETF for output).  Note that UTD is assumed to be
c     exactly an integer (even though it is defined to be a real
c     number) -- the result may not be correct if UTD has a
c     fractional part.  All times in these programs are julian
c     days:  JD for input and JED for output.
c
c (c) function ETMIUT supplies the correction from JD to JED.
c
c (d) subroutine DMS converts an angle (degrees) to sign, degrees,
c     minutes, seconds.
c
c (e) function BCJPL handles the interpolation on the table of
c     earth-barycenter vectors, and returns the time correction
c     in seconds.  Source coordinates are supplied through the
c     common block JPLSUB in the array POS(3), which are the
c     x-y-z coordinates on the unit sphere.
c
c (f) function BESSEL does bessel interpolation (copied from the
c     Astronomical Almanac).
c
c
c All computations are done in double precision (REAL*8).
c
c
c J. Deeter
c
c Uchu-ken, 92.3.7
c Modified by Y.Sekimoto 93.7.26 for miranda
c

      Subroutine JPLEPH(UTD,UTF,ETD,ETF)
C TEST J2000 SUBROUTINE
      Implicit Real*8 (A-H, O-Z)
      Implicit Integer*4 (I-N)
C
      Data DAYSEC/86400.0D0/
      Data PI/3.1415926535D0/
C
      TWOPI=2.0D0*PI
      RAD=180.0D0/PI
C
      UTC=UTD + UTF
      DET=ETMIUT(UTC)
      ETF=UTF + DET/DAYSEC
      TDT=UTC + DET/DAYSEC
C
C.. TDB-TDT DUE TO VARIATIONS IN GRAV. POTENTIAL AROUND EARTH ORBIT
C..  SEE ASTR. ALMANAC 1992, P. B5.
      GEE=MOD(357.53D0 + 0.98560028D0*(TDT-2451545.0D0), 360.0D0)/RAD
      DGRAV=0.001658D0*SIN(GEE) + 0.000014D0*SIN(2.0*GEE)
      ETF=ETF + DGRAV/DAYSEC
C
      ETD=UTD-0.5D0
      ETF=ETF+0.5D0
      ZF=INT(ETF)
      ETD=ETD+ZF
      ETF=ETF-ZF
C
      DSUN=BCJPL(ETD,ETF)
      ETF=ETF + DSUN/DAYSEC
      ETD=ETD-0.5D0
      ETF=ETF+0.5D0
      ZF=INT(ETF)
      ETD=ETD+ZF
      ETF=ETF-ZF
C
C TEST OUTPUT
C     WRITE(6,35) UTD,UTF, DET
C     WRITE(6,36)          DGRAV
C     WRITE(6,37) ETD,ETF, DSUN
C35   FORMAT(1H0,F9.0,F14.11,F12.6)
C36   FORMAT(1X, 23X,        F12.6)
C37   FORMAT(1X, F9.0,F14.11,F12.6)
C
      Return
      End
C
      Function ETMIUT(UTC)
C.. Return ET-UT (ET=TDT), INPUT=UTC
C
      Implicit Real*8 (A-H, O-Z)
      Implicit Integer*4 (I-N)
C
      Dimension ZJD(20),DET(20)
C
      Data NDET/9/
      Data ZJD( 1),DET( 1) /2444239.5D0, 19.0D0/
C                                               1980 JAN 1
      Data ZJD( 2),DET( 2) /2444786.5D0, 20.0D0/
C                                               1981 JUL 1
      Data ZJD( 3),DET( 3) /2445151.5D0, 21.0D0/
C                                               1982 JUL 1
      Data ZJD( 4),DET( 4) /2445516.5D0, 22.0D0/
C                                               1983 JUL 1
      Data ZJD( 5),DET( 5) /2446247.5D0, 23.0D0/
C                                               1985 JUL 1
      Data ZJD( 6),DET( 6) /2447161.5D0, 24.0D0/
C                                               1988 JAN 1
      Data ZJD( 7),DET( 7) /2447892.5D0, 25.0D0/
C                                               1990 JAN 1
      Data ZJD( 8),DET( 8) /2448257.5D0, 26.0D0/
C                                               1991 JAN 1
      Data ZJD( 9),DET( 9) /2448804.5D0, 27.0D0/
C                                               1992 JUL 1
C
      Do 20 I=2,NDET
        If(UTC.lt.ZJD(I)) Goto 25
   20 Continue
      I=NDET+1
   25 Continue
      ETMIUT=DET(I-1) + 32.184D0
C
      Return
      End
C
      Subroutine DMS(ANG,IANG,DEG,XMIN,SEC)
      Implicit Real*8 (A-H, O-Z)
      Implicit Integer*4 (I-N)
C
      Character IANG*1
C
      IANG='+'
      If(ANG.lt.0.0) IANG='-'
C
      DEG=INT(ABS(ANG))
      XMIN=INT(60.0D0*(ABS(ANG)-DEG))
      SEC=3600.0D0*(ABS(ANG) - DEG - XMIN/60.0D0)
C
      Return
      End
C
      Function BCJPL(ETD,ETF)
C
      Implicit Real*8 (A-H, O-Z)
      Implicit Integer*4 (I-N)
C
      Common /JPLSUB/ POS(3),GEOFIL
      Common /EARTH/ DT,T(100),X(100),Y(100),Z(100)
C
      Character GEOFIL*160
      Dimension TAB(10)
      Integer LUN
      Parameter (LUN = 12)
      Logical first
      data first /.true./
C
      Data T(1)/0.0D0/
      Data DT/1.0D0/
      Data N/100/
C
      Data AU/499.004782D0/
C
C     Added BCJPL=0.0 otherwise undefined value might be returned on error.
C                         KM, 2001 December
      BCJPL=0.0D0
C
      TO=T(1)
      TF=T(1) + REAL(N-1)*DT
C
c      Write (6,12) TO,TF,ETD,ETF
   12 Format(1H0,2F12.1,F12.1,F12.9)
C
      If((ETD-4.0 .GT. TO) .AND. (ETD+4.0 .LT. TF)) GOTO 20
C
C.. READ TABLE
C
      Open (UNIT=LUN, FILE=GEOFIL, STATUS='OLD')
      write(*,*) 'success opening file: ', geofil
C
      TS=ETD-11.0
   15 Continue
      Read (LUN,*,end=100) TI,XI,YI,ZI
      If (TI.lt.TS) Then
        Goto 15
      Else
        Goto 16
      Endif
  100 Write(*,*) GEOFIL ,'is end:the file does not have the data'
      Close(LUN)
      Goto 999
C
   16 Do 18 I=1,N
        Read (LUN,*) T(I),X(I),Y(I),Z(I)
C      WRITE (6,17) T(I),X(I),Y(I),Z(I)
C 17   FORMAT(1X, F10.1, 3F14.10)
   18 Continue
C
      Close(LUN)
C
   20 Continue
C
      TO=T(1)
      DT=T(2)-T(1)
C
C..  BESSEL INTERPOLATION
      IO=NINT(ETD-TO) + 1
C
      Do 30 II=1,6
        I=IO - 3 + II
        TAB(II)=AU*(POS(1)*X(I) + POS(2)*Y(I) + POS(3)*Z(I))
   30 Continue
C
      JO=3
      BCJPL=BESSEL(TAB,JO,ETF)
C
  999 Return
      End
C
      Function BESSEL(TAB,JO,P)
C.. FOURTH ORDER BESSEL INTERPOLATION
C..  JO=CENTRAL VALUE, P=FRACTION (0.0 TO 1.0)
C..  USES SIX ENTRIES FROM TABLE (JO-2 TO JO+3)
C.    SEE ASTR. ALMANAC 1992, P. K14
C
      Implicit Real*8 (A-H, O-Z)
      Implicit Integer*4 (I-N)
C
      Dimension TAB(*)
C
      D1=TAB(JO+1) - TAB(JO)
      D2=TAB(JO+2) - TAB(JO+1) - TAB(JO) + TAB(JO-1)
      D3=TAB(JO+2) - 3.0D0*TAB(JO+1) + 3.0D0*TAB(JO) - TAB(JO-1)
      D4=TAB(JO+3) - 3.0D0*TAB(JO+2) + 2.0D0*TAB(JO+1) + 2.0D0*TAB(JO)
     &             - 3.0D0*TAB(JO-1) + TAB(JO-2)
C
      B2=P*(P-1.0D0)/4.0D0
      B3=P*(P-1.0D0)*(P-0.5D0)/6.0D0
      B4=(P+1.0D0)*P*(P-1.0D0)*(P-2.0D0)/48.0D0
C
C      S1=TAB(JO) + P*D1
C      S2=TAB(JO) + P*D1 + B2*D2
C      S3=TAB(JO) + P*D1 + B2*D2 + B3*D3
C      S4=TAB(JO) + P*D1 + B2*D2 + B3*D3 + B4*D4
C
C      WRITE (6,*) S1,S2,S3,S4
C
      BESSEL=TAB(JO) + P*D1 + B2*D2 + B3*D3 + B4*D4
C
  999 Return
      End
