c  %W% %G%
c  [geomag.f

      subroutine geomag(JD, x_pos, y_pos, z_pos, geo_mag_rig, status)

c
c
c  Description:
c
c   This is a short interface between the calling code (auxqtys,f) and 
c   E. Daly's (ESTEC/WMA) XYZMAG code for calculating the geomagnetic 
c   rigidity at a given time and position on an orbit.
c   The geomagnetic rigidity is the minimum momentum required by a photon to 
c   penetrate the Earth's magnetic field to the height given. Units are Gev/c.
c
c
c  Arguments:
c
c   JD             i : Julian day (J2000)
c   x_pos          i : X-coordinate of satellite position
c   y_pos          i : Y-coordonate of satellite position
c   z_pos          i : Z-coordonate of satellite position
c   geo_mag_rig    o : Calculated geomagnetic rigidity
c   status         o : Exit status of routine
c
c
c  Exit Status (status)
c   0 = Normal processing took place
c   1 = Error in FORTRAN processing
c
c
c  Called Routines:
c
c   XYZMAG           : FORTRAN routine calculating geomagnetic rigidity.
c                      ESTEC/WMA code - E. Daly.
c
c
c  Pseudo_Code:
c   subroutine geomag
c   initialise variables
c   T = JD - 2440000.5         - Calc. Truncated Julian Day from JD
c   assign xyz(3) = sat_pos    - Set up array required by XYZMAG
c   call XYZMAG
c   return
c
c
c  History:
c
c   Adam Hazell     1/10/95      V1.0  -  Original.
c
c  geomag.f]


c
c  Initialise subroutine parameter variable types
c
      implicit none

      DOUBLE PRECISION JD, x_pos, y_pos, z_pos
      DOUBLE PRECISION geo_mag_rig
      integer          status

c
c  Initialise variable types for XYZMAG
c
      
      REAL T, B0, FL, RC, GLAT
      REAL WORLD(4), XYZ(3)


      status = 0


c
c  Calculate truncated julian day from julian day
c

      T = real(JD - 2440000.5D0)



c
c Put position vector of satellite into XYZ array
c

      XYZ(1) = real(x_pos)
      XYZ(2) = real(y_pos)
      XYZ(3) = real(z_pos)

c
c  Call to E. J. Daly's code ("XYZMAG")
c

      call XYZMAG(T, XYZ, B0, FL, RC, GLAT, WORLD)

      geo_mag_rig = RC

      write(*,1000) XYZ(1), XYZ(2), XYZ(3), RC, T
 1000 format(/'Geo magnetic rigidity at (',
     1         f6.1,', ',f6.1,', ',f6.1,') = ',f15.10,'GeV/c',/,
     1         'on TJD: ',F9.2)

      return
      end       

C***********************************************************************
C*  Institute:     MPE               *                *                *
C*                                   *                *   SUBROUTINE   *
C*  DDDDDD         AA       LLL      *       GRO      *     P4FRIG     *
C*  DDDDDDD      AA  AA     LLL      *     COMPTEL    *                *
C*  DD    DD    AA    AA    LLL      *                * First Version  *
C*  DD    DD    AA    AA    LLL      ******************                **
C*  DD    DD    AAAAAAAA    LLL      *                * Author:         *
C*  DD    DD    AAAAAAAA    LLL      *                * Georg           *
C*  DD    DD    AA    AA    LLL      *    COMPASS     * Weidenspointner *
C*  DD    DD    AA    AA    LLL      *                *                **
C*  DDDDDDD     AA    AA    LLLLLLLL *                * Date :         *
C*  DDDDDD      AA    AA    LLLLLLLL *                *  30-11-95      *
C*                                   *                *                *
C*  Data        Access      Layer    *                *                *
C***********************************************************************
C*  Function: CALCULATES VERTICAL CUTOFF RIGIDITY                      *
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
C                                                                      *
C Inputs:                                                              *
C =======                                                              *
C                                                                      *
C    Name   Type        Description                                    *
C    ------ ----------- ---------------------------------------------- *
C    T      REAL        time in decimal TJD                            *
C    XYZ(1) REAL        x-location in inertial coordinates             *
C    XYZ(2) REAL        y-location in inertial coordinates             *
C    XYZ(3) REAL        z-location in inertial coordinates             *
C                                                                      *
C----------------------------------------------------------------------*
C                                                                      *
C Outputs:                                                             *
C ========                                                             *
C                                                                      *
C    Name   Type        Description                                    *
C    ------ ----------- ---------------------------------------------- *
C    
C    
C    
C                                                                      *
C----------------------------------------------------------------------*
C                                                                      *
C Calls:                                                               *
C ======                                                               *
C                                                                      *
C    Not all routines in this subroutine and the subroutines called    *
C    from here are actually used.                                      *
C    As can be seen from the comments below, the "core code" was       *
C    developed by E.J.  Daly ESTEC and some collegues, which was       *
C    provided to me by Kevin Bennett, ESTEC. I changed as little as    *
C    possible to achieve the desired goal (see Description).           *
C    In the following only the most important routines are listed.     *
C                                                                      *
C    Name       Function                                               *
C    ------     ------------------------------------------------------ *
C    XYZMAG     "Dummy" that calls all important routines              *
C    CALEND     see there and XYZMAG                                   *
C    GIN_CAR    coordinate transformation (see XYZMAG)                 *
C    CAR_WM                   -"-                                      *
C    GEO_BL     calculate field shell L-value and                      *
C               magnetic field strength                                *
C    READC      initialize field coefficients                          *
C                                                                      *
C----------------------------------------------------------------------*
C                                                                      *
C Description:                                                         *
C ============                                                         *
C                                                                      *
C    The vertical cutoff rigidity is calculated for the given time     *
C    (decimal TJD) and the given location (inertial coordinates).      *
C                                                                      *
C    The provided position is converted to geographical longitude and  *
C    latitude, and the altitude is determined.                         *
C                                                                      *
C    The appropriate IGRF (International Geophysical Reference Field)  *
C    is initialized. IGRF data is available every 5 years. The latest  *
C    model before the provided point in time is extrapolated.          *
C                                                                      *
C    The shell-field L-value and the magnetic field strength are       *
C    calculated for the given location in the time-dependent field     *
C    model.                                                            *
C                                                                      *
C    The shell-field L-value is converted to vertical cutoff rigidity, *
C    assuming a perfect dipole field.                                  *
C                                                                      *
C    For more information: see COM-RP-MPE-DRG-???
C
C***********************************************************************



C COMPTEL95 PROGRAM/ROUTINES

C note: BLTIME in common..../bmodel/ modold, tmold, bltime, mmoold, ipass  

c Installation points:
c The NSSDC standard geomagnetic model
C data files DGRF...IGRF are converted into Fortran block data
c by the utility DGRFTOBD.for, to produce a file DGRFBD.FOR to be compiled
c and linked with this program.
c Link COMPTEL95 (this) with BINT, SHELL, TRANSFOS and DGRFBD
c One undefined reference will appear: BEXT, but this is of no
c consequence and refers to the perturbation model for the outer
c magnetosaphere (not relevant for low alt rigidity work)

C E.J. Daly 11/95

C              For CompTel: take geocentric coordinates
C              and produce geomagnetic field parameters
C ____________________________________E.J. Daly ESTEC/WMA 3/91 

C inputs:
C              T        = Truncated Julian day
C              xyz(3)   = inertial coordinates 

C outputs:
C              B0       = field strength (gauss)
C              FL       = field-shell L-value  (Earth radii)
C              RC       = cut-off rigidity (in GV/c)
C              GLAT     = invariant geomagnetic latitude (degrees)
C              WORLD(4) = longitude, latitude, altitude, local time

C Compiled from various routines by E.J. Daly ESTEC/WMA 3/91 
C                                at short notice!!! no guarantees given...!
C geomagnetic field routines come from the SHELLG code of G.Kluge,
C some also from the SAPRE orbit generator of S. Pallaschke,
C thanks to C. Tranquille for a lot of others...


C uses geomagnetic field model data file (unit 11, SHELLG00.DAT)
C which contains a number of models. We use the IGRF 1980 120-term
C model (no. 11 in file) extrapolated to 1990.

C standard geomagnetic coordinates B and L are computed
C and then invariant geomagnetic latitude (Arccos(1/sqrt(L))
C and vertical rigidity cutoff            (1/L**2)
C are computed. Personally, I think inv. lat. is better
C since it's easier to grasp (especially for Irishmen).
C The only advantage of Cutoff is that it needs less computing.
C Off, course, you could always just use L.


C this is the main subroutine that calls everything:
C   (it also needs to interface to/from all those DOUBLE PRECISION variables)

        SUBROUTINE XYZMAG(T,XYZ, B0,FL,RC,GLAT,WORLD)

        implicit none

C       INPUT
        REAL T, XYZ(3)

C       OUTPUT
        REAL B0, FL, RC, GLAT, WORLD(4)

        INTEGER   IYEAR, IMONTH, IDAY, IHOUR, IMIN, ISOD
        INTEGER   y,doy
        INTEGER   jd2ymd, tjd
        DOUBLE PRECISION    dyear
        DOUBLE PRECISION    dt,DSEC,dx,dy,dz,db,dl,dlong,dlat,dalt,dphi
        DOUBLE PRECISION    xc,yc,zc
        REAL    VERCUT
        REAL    DEGREE
        DATA DEGREE/57.2958/

        jd2ymd = +1

C convert truncated Julian day to
C   year (e.g. 90 for year 1990), month (January is 1),
C   day of month, hour of day, minute of hour and second of minute 
C first convert TJD to (double precision) Mean Jul. day
        DT = T + 6718.
c        write(*,*) 'tjd -> mean julian day: ', t, ' ', dt
C then do the rest
        CALL CALEND(DT, jd2ymd,IYEAR,IMONTH,IDAY,IHOUR,IMIN,DSEC)
c        write(*,*) 'year   ', iyear
c        write(*,*) 'month  ', imonth
c        write(*,*) 'day    ', iday
c        write(*,*) 'hour   ', ihour
c        write(*,*) 'minute ', imin
c        write(*,*) 'sec    ', dsec
        ISOD = int(DSEC + IMIN*60.0 + IHOUR*3600.0)
c        write(*,*) 'time of day in sec ', isod

        DX=XYZ(1)
        DY=XYZ(2)
        DZ=XYZ(3)

C Convert (input) inertial coordinates, using the time (input in TJD),
C to geographic longitude and latitude, altitude and local time (in hours)
      CALL GIN_CAR(DT,DX,DY,DZ,  XC,YC,ZC)
      CALL CAR_WM(ISOD,XC,YC,ZC,  DLONG,DLAT,DALT,DPHI)
c      write(*,*) 'long, lat, alt :', dlong, '  ', dlat, '  ', dalt
c      write(*,*) 'local time :', dphi

C ejd 9511
c      CALL CAR_BL(XC,YC,ZC,  DL,DB)
C instead pass geo, as required by new model code:

C                                                    C
C Calculate shell-field L-value and magnetic field B C
C for that location and time                         C
C                                                    C

C Convert (input) time to decimal years, rounded to the first 
C decimal digit assuming 365 days per year to initialize 
C the time-dependent magnetic field model to that time
      tjd = int(t)
c      write(*,*) 'TJD : ', tjd
      call p4tjdy(tjd,y,doy)
c      write(*,*) 'year, doy :', y, '  ', doy
      dyear = int((y + doy/365.)*10. + 0.5)/10.0d0
c      write(*,*) 'rounded year :', dyear
      
C Calculate shell field L-value and magnetic field strength
      call geo_bl(dyear,dlong,dlat,dalt,dl,db)
      FL=real(DL)
      B0=real(DB)

C Calculate invariant geomagnetic latitude
      IF (FL .GT. 1)THEN
        GLAT=degree*ACOS(1/SQRT(FL))
      ELSE
        GLAT=-999
      ENDIF

C Calculate vertical cutoff Rigidity
      RC = VERCUT( FL)

      WORLD(1) = real(DLONG)
      WORLD(2) = real(DLAT)
      WORLD(3) = real(DALT)
      WORLD(4) = real(DPHI)
      
      RETURN
      END      

C.........................................
        REAL FUNCTION vercut(L)
C compute vertical cut-off rigidity: simple function of L
C in a perfect diploe
C                    E.J. Daly ESA/ESTEC/WMA  9/87

        implicit none
        REAL L

C  here, units of M are (GeV/c).Re**2
* ideal:REAL M/59.6/, MO4/14.9/
*  Smart & Shea, ICRC 1985 SH 6.1-12, p332:
        REAL MO4
        DATA MO4/16.2/


        vercut=MO4/L**2

        RETURN
        END

      SUBROUTINE CALEND(DJ,KK,IY,J,K,IH,IMI,DS)
C  NEW VERSION JUNE 1972 FROM ARTICLE BY FLIEGEL AND VAN FLANDERN  ACM 1
C  KK=+1 CALCULATE DATE AND HOUR FROM MODIFIED JULIAN DAY
C  KK=-1 CALCULATE MODIFIED JULIAN DAY FROM DATE AND HOUR
C  DJ =MODIFIED JULIAN DAY   (JULIAN DAY - 2433282.5)  TO MATCH IBM.SOLU
C  I=YEAR,J=MONTH,K=DAY
C  IY=MODIFIED YEAR   (YEAR-1900)
C  IH=HOUR,IMI=MINUTES,DS=SECONDS
C
      IMPLICIT NONE

C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     INPUT/OUTPUT
      DOUBLE PRECISION DJ
      INTEGER KK
      INTEGER IY, J, K, IH, IMI
      DOUBLE PRECISION DS

C     LOCAL
      DOUBLE PRECISION JD, DJJ, TIMEH, TIMEM
      INTEGER I, L, N, ID

      IF(KK.EQ.-1) GOTO 10
      JD=DJ+2433283.D0
C     JD=DJ+2433282.5D0+.5
C                       .5 BECAUSE JULIAN DAYS START AT NOON
      L=int(JD+68569)
      N=4*L/146097
      L=L-(146097*N+3)/4
      I=4000*(L+1)/1461001
      L=L-1461*I/4+31
      J=80*L/2447
      K=L-2447*J/80
      L=J/11
      J=J+2-12*L
      I=100*(N-49)+I+L
      IY=I-1900
C-----THE FOLLOWING LINE WAS INCLUDED BY LHJ/JCD ON 81-08-21
C-----DUE TO ROUNDING-OFF ERRORS.
      DJJ=DJ+5.79D-6
      ID=int(DJJ)
      TIMEH=(DJJ-ID)*24.0
      IH=int(TIMEH)
      TIMEM=(TIMEH-IH)*60.0
      IMI=int(TIMEM)
      DS=(TIMEM-IMI)*60.0
C  NOW CHECK FOR ROUNDING ERRORS
      IF (DS.LT.59.9999D00) GOTO 1
      IMI=IMI+1
      DS=0.0
    1 IF(IMI.LT.60) GOTO 2
      IH=IH+1
      IMI=IMI-60
    2 IF(IH.LT.24) GOTO 3
      K=K+1
      IH=IH-24
C  THE PROBABILITY OF THE DAY BEING BEYOND TH END OF THE MONTH CAN BE
C  IGNORED(1/730 + 1/43800 + 1/2628000)  COMPARED WITH EXTRA MACHINE TIM
    3 RETURN
C
   10 I=IY+1900
      JD=K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12)/12
     *   -3*((I+4900+(J-14)/12)/100)/4
      DJ=JD-2433283.D0+((DS/60.0+IMI)/60.0+IH)/24.0
C     DJ=JD-2433282.5D0-.5+((DS/60.0+IMI)/60.0+IH)/24.0
C                       .5 BECAUSE JULIAN DAYS START AT NOON
      RETURN
      END

      SUBROUTINE GIN_CAR(TMJD,XIN,YIN,ZIN,XC,YC,ZC)

      IMPLICIT NONE

C     INPUT
      DOUBLE PRECISION TMJD, XIN, YIN, ZIN

C     OUTPUT
      DOUBLE PRECISION XC, YC, ZC

      DOUBLE PRECISION RAD, TSID
      DOUBLE PRECISION ST, CT

      DATA RAD/0.0174532925199433D0/
      TSID=RAD*DMOD(100.075542D0+360.985647335D0*TMJD,360.0D0)
      ST=DSIN(TSID)
      CT=DCOS(TSID)
      XC= XIN*CT+YIN*ST
      YC=-XIN*ST+YIN*CT
      ZC=ZIN
      RETURN
      END



      SUBROUTINE CAR_WM(ISOD,XC,YC,ZC,DLONG,DLAT,DHAUT,PHI)

      IMPLICIT NONE
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)

C     INPUT
      INTEGER ISOD
      DOUBLE PRECISION XC, YC, ZC

C     OUTPUT
      DOUBLE PRECISION DLONG, DLAT, DHAUT, PHI

C     LOCAL
      DOUBLE PRECISION RAD, FLAT, REQ
      DOUBLE PRECISION E2, E4, E6
      DOUBLE PRECISION R1, R, SEL, CEL, DEL, S2EL, C2EL, S4EL
      DOUBLE PRECISION D, SLAT, D2

      DATA RAD,FLAT,REQ
     .     /0.0174532925199433D0,0.3352329D-2,6378.165D0/
      E2=FLAT*(2.0-FLAT)
      E4=E2*E2
      E6=E4*E2
      DLONG=ATAN2(YC,XC)/RAD
      DLONG=DMOD( (DLONG+360.0D0) ,360.0D0 )
      R1=XC*XC +YC*YC
      R=SQRT(R1 +ZC*ZC)
      R1=SQRT(R1)
      SEL=ZC/R
      CEL=R1/R
C---- DEL=DARSIN(SEL)
      DEL=ASIN (SEL)
      S2EL=2.0*SEL*CEL
      C2EL=1.0-2.0*SEL*SEL
      S4EL=2.0*S2EL*C2EL
      D=R/REQ
      DLAT=DEL + (E2/2.0+E4/8.0+E6/16.0)/D*S2EL + (-E4/16.0-E6/21.0
     1         +(E4/4.0+E6/8.0)/D)/D*S4EL
      SLAT = SIN(DLAT)
      D2 = SQRT(1.0-E2*SLAT*SLAT)
      DHAUT = R1/COS(DLAT) - REQ/D2
      DLAT = DLAT/RAD
      PHI = real(ISOD)/3600.0D0+DLONG/15.0D0
      PHI = DMOD(PHI,24.0D0)
      RETURN
      END

c      SUBROUTINE CAR_BL(XC,YC,ZC,FL,B0)
c      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c      DIMENSION V(3)
c      REAL BLTIME/1990.0/
c      INTEGER MODEL/11/,DEBUG/0/,IPASS/0/
c      COMMON /ipass/ IPASS
c      IF (IPASS.EQ.0) THEN
c         WRITE(6,*)' Opening geomag.model file and initialising model'
c         OPEN(11,FILE='SHELLG00.DAT',STATUS='OLD')
c         CALL INITC(MODEL,BLTIME,DEBUG)
c         IPASS=1
c      ENDIF
c      RE=6371.2
c      V(1)=XC/RE
c      V(2)=YC/RE
c      V(3)=ZC/RE
c      CALL SHELLC(V,FL,B0)
c      RETURN
c      END

C new one to replace car_bl:
      SUBROUTINE geo_bl(time,dlong,dlat,dalt,fl,b0)

C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      IMPLICIT NONE
C     INPUT
      DOUBLE PRECISION time, dlong, dlat, dalt
C     OUTPUT
      DOUBLE PRECISION fl, b0

C     LOCAL
      INTEGER kp, outer
      character(32) MODLBL
      INTEGER MMOFLG
      DOUBLE PRECISION modold, mmoold, tmold, bltime
      common /bmodel/ gmagmo, colat, elong, modold, mmoold, tmold,
     &                bltime, mmoflg, ipass
      INTEGER MODEL,IPASS, NMAX
      DOUBLE PRECISION fkp, den, vel, dst, ut, amjd, alpha
      DOUBLE PRECISION tzero, gmagmo, colat, elong
      INTEGER geodetic2geocentric
      DOUBLE PRECISION GCLAT, Radius
      DATA MODEL/0/

C MODEL = 0 --> DGRF/IGRF model is initialized in subroutine readc

C important: makes field model time-dependent
      BLTIME=time
       
      if ((ipass .eq. 0) .or.
     &    (model .ne. modold) .or. 
     &    (bltime .ne. tmold) .or.
     &    (mmoflg .ne. mmoold)) then

C  standard value .311653 selected for gmagmo by setting MMOFLG to zero.
C  (MMOFLG is flag for magnetic moment of Earth)

        MMOFLG=0
        modold = model
        tmold = bltime
        mmoold = mmoflg
c        WRITE(6,*)' Initialising geomagnetic model'
        ipass=1
c                                  
        call readc(model, bltime, nmax, tzero, modlbl, gmagmo, mmoflg,
     &             colat, elong)
c        WRITE(6,*)' Initialised model:',modlbl
c        WRITE(6,*) 'T0:',tzero,', mag.moment:',gmagmo,', Nmax:',nmax

      end if

C geodetic to geocentric:
        geodetic2geocentric = 1
        Call CONVRT(geodetic2geocentric, DLAT,DALT, GCLAT,Radius)

C SHELL can use an external field model but that makes no sense in 
c this application, so all the driving parameters are zero
c It can also compute a pitch-angle(alpha)-dependent L; 
c 90 is the old standard way:

        outer=0
        kp=0
        fkp=0
        den=0
        vel=0
        dst=0
        ut=0
        amjd=0
        alpha=90

        CALL SHELL(gclat, dlong, radius, outer, kp, fkp,
     &                 den, vel, dst, gmagmo, colat, elong, ut, amjd,
     &                 alpha, b0, fl)

      RETURN
      END

C**************************************************************************

      SUBROUTINE P4TJDY(TJD,YEAR,DOY)
C***********************************************************************
C*  Institute:     MPE               *                * Subroutine:    *
C*                                   *                *                *
C*  DDDDDD         AA       LLL      *       GRO      *   P4TJDY       *
C*  DDDDDDD      AA  AA     LLL      *     COMPTEL    *                *
C*  DD    DD    AA    AA    LLL      *                * Revision: 3    *
C*  DD    DD    AA    AA    LLL      ******************                *
C*  DD    DD    AAAAAAAA    LLL      *                * Author:        *
C*  DD    DD    AAAAAAAA    LLL      *                *   Roland       *
C*  DD    DD    AA    AA    LLL      *    COMPASS     *     Diehl      *
C*  DD    DD    AA    AA    LLL      *                *                *
C*  DDDDDDD     AA    AA    LLLLLLLL *                * Date :         *
C*  DDDDDD      AA    AA    LLLLLLLL *                *  1991-06-10    *
C*                                   *                *                *
C*  Data        Access      Layer    *                *                *
C***********************************************************************
C*  Function: CONVERT DATE IN TJD FORMAT TO DATE IN CHRISTIAN YEAR
C             AND DAY
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
C                                                                      *
C Inputs:                                                              *
C =======                                                              *
C    Name   Type        Description                           Location *
C    ------ ----------- ------------------------------------- -------- *
C    ...... char*...    .............                         ........ *
C    ...... int (...)   ..............                        ........ *
C    ...... REAL (...)  ..............                        ........ *
C++   INPUT: TJD    DAY NUMBER IN TJD FORMAT
C                                                                      *
C Outputs:                                                             *
C ========                                                             *
C    Name   Type        Description                           Location *
C    ------ ----------- ------------------------------------- -------- *
C    ...... integer     ...........                           P....I   *
C    ...... REAL        ...........                           P....R   *
C    ...... char*1      ...........                           P....C   *
C++   OUTPUT: YEAR  YEAR NUMBER
C++           DOY   DAY OF YEAR NUMBER
C                                                                      *
C Modified COMMON Variables:   none                                    *
C ==========================                                           *
C                                                                      *
C Calls:                                                               *
C ======                                                               *
C             Name       Function                                      *
C             ------     --------------------------------------------- *
C REW         P4OOR      error message                                 *
C                                                                      *
C Needs:   nothing                                                     *
C ======                                                               *
C                                                                      *
C Signals:   nothing                                                   *
C ========                                                             *
C                                                                      *
C Description:                                                         *
C ============                                                         *
C++   SOFTWARE FROM BENNETT / IGSE / ITOG
C    .........................                                         *
C                                                                      *
C Documentation:                                                       *
C ==============                                                       *
C    ...............                                                   *
C                                                                      *
C Subroutine History:                                                  *
C ===================                                                  *
C    Date       Id   Rev.  Changes                                     *
C    --------  ----  ----  ------------------------------------------- *
C++   HISTORY:  6 MAY 1991 RD FROM KBENNETT
C++            11 MAY 1991 RD->V01.0 FUNCTIONING
C++            23 MAY 1991 RD->V02.0 CONVERT TO ANSII STANDARDS
C    91-06-10   hs     3   extracted from TJDDOY;                      *
C                          renamed (old name TJDDOY);                  *
C                                                                      *
C***********************************************************************
      IMPLICIT NONE

C     INPUT
      INTEGER      TJD

C     OUTPUT
      INTEGER      YEAR
      INTEGER      DOY

C     LOCAL
      INTEGER      LOCTJD
      INTEGER      TJDBAS
      INTEGER      P4TJD

C     IF (TJD.LT.0 .OR. TJD.GT.99999) THEN
      IF (TJD.LT.0 .OR. TJD.GT.88000) THEN
C REW: actually P4TJD(YEAR) only supports years up to 2100 (TJD ~88000)
         YEAR = 0
         DOY = 0
         write(*,*) 'TJD out of range', TJD
C REW    CALL P4OOR('TJD',TJD)
         RETURN
      END IF

      LOCTJD = TJD
C                     ! START SOMEWHERE
C     YEAR = 1980
      YEAR = 1969
C REW: increase range of function (at some runtime cost)
C                     ! WILL FAIL IN 200
      TJDBAS  = P4TJD (YEAR)

C REW: jump to within one year to avoid many iterations
      IF (LOCTJD .GT. TJDBAS + 1000) THEN
          YEAR = YEAR + (LOCTJD - TJDBAS) / 366
C REW: avoid overshooting by assuming every year is a leap year
          TJDBAS = P4TJD (YEAR)
C     write(*,*) 'JUMP:YEAR',YEAR,' LOCTJD',LOCTJD,' > TJDBAS',TJDBAS
      END IF

C     IF (LOCTJD.LE.TJDBAS ) LOCTJD = TJD + 10000
C        ! PASSED TJD 0
C REW: what?! can't just change input

C     DO WHILE (LOCTJD.GT.TJDBAS )
 99   CONTINUE
         IF (LOCTJD.LE.TJDBAS ) GOTO 101
         YEAR = YEAR + 1
         TJDBAS  = P4TJD (YEAR)
         GO TO 99
101   CONTINUE

      YEAR = YEAR - 1
      DOY = TJD - P4TJD (YEAR)

C     write(*,*) 'P4TJDY(',TJD,') => year', YEAR, ' doy =>', DOY

      RETURN
      END


      INTEGER FUNCTION P4TJD (IYEAR)
C***********************************************************************
C*  Institute:     MPE               *                * Integer        *
C*                                   *                *   Function :   *
C*  DDDDDD         AA       LLL      *       GRO      *     P4TJD      *
C*  DDDDDDD      AA  AA     LLL      *     COMPTEL    *                *
C*  DD    DD    AA    AA    LLL      *                * Revision: 3    *
C*  DD    DD    AA    AA    LLL      ******************                *
C*  DD    DD    AAAAAAAA    LLL      *                * Author:        *
C*  DD    DD    AAAAAAAA    LLL      *                *   Roland       *
C*  DD    DD    AA    AA    LLL      *    COMPASS     *     Diehl      *
C*  DD    DD    AA    AA    LLL      *                *                *
C*  DDDDDDD     AA    AA    LLLLLLLL *                * Date :         *
C*  DDDDDD      AA    AA    LLLLLLLL *                *  1991-06-10    *
C*                                   *                *                *
C*  Data        Access      Layer    *                *                *
C***********************************************************************
C*  Function: RETURNS TJD OF JAN 0
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
C                                                                      *
C Inputs:                                                              *
C =======                                                              *
C    Name   Type        Description                           Location *
C    ------ ----------- ------------------------------------- -------- *
C    ...... char*...    .............                         ........ *
C    ...... int (...)   ..............                        ........ *
C    ...... REAL (...)  ..............                        ........ *
C                                                                      *
C Outputs:                                                             *
C ========                                                             *
C    Name   Type        Description                           Location *
C    ------ ----------- ------------------------------------- -------- *
C    ...... integer     ...........                           P....I   *
C    ...... REAL        ...........                           P....R   *
C    ...... char*1      ...........                           P....C   *
C                                                                      *
C Modified COMMON Variables:   none                                    *
C ==========================                                           *
C                                                                      *
C Calls:                                                               *
C ======                                                               *
C             Name       Function                                      *
C             ------     --------------------------------------------- *
C REW         P4OOR      error message                                 *
C                                                                      *
C Needs:   nothing                                                     *
C ======                                                               *
C                                                                      *
C Signals:   nothing                                                   *
C ========                                                             *
C                                                                      *
C Description:                                                         *
C ============                                                         *
C
C  CALCULATE THE TRUNCATED JULIAN DAY OF DAY 0 OF THE GIVEN YEAR.
C
C  THE FIRST TJD = 0 IS MIDNIGHT OF MAY 24 1968
C  FOR ERRONEOUS YEARS THE VALUE RETURNED IS -1
C
C                                                                      *
C Documentation:                                                       *
C ==============                                                       *
C    ...............                                                   *
C                                                                      *
C Subroutine History:                                                  *
C ===================                                                  *
C    Date       Id   Rev.  Changes                                     *
C    --------  ----  ----  ------------------------------------------- *
C    V1.0 JB  <900905.1452>
C  VERSION  2: R. DIEHL, MPE : ADAPT FROM HP TO IBM FOR COMPASS        *
C    91-06-10   hs     3   extracted from TJDDOY;                      *
C                          renamed (old name TRUNJD);                  *
C                                                                      *
C***********************************************************************

      IMPLICIT NONE

      INTEGER      IYEAR
      INTEGER      DELYR
C
      IF (IYEAR.LE.1968) THEN
C REW:   CALL P4OOR('YEAR',IYEAR)
         P4TJD  = -1
C REW: added this block to catch upper limit of function's validity
      ELSE IF (IYEAR.GE.2100) THEN
         P4TJD  = -1
      ELSE
         P4TJD  = 221
C         ! OFFSET FOR REST OF 19
C REW: apparently meant 'REST OF 1968' since
C (TJD(1969 Jan 1) - TJD 0 = JD 2440000 = 1968 May 23) = 221

         DELYR = IYEAR - 1969
C         ! YEARS SINCE DAY 0 OF
         P4TJD  = P4TJD  + 365 * DELYR
C         ! AMOUNT OF DAYS

C        P4TJD  = MOD(P4TJD +(DELYR/4),10000)
C REWapplying the MOD 10000 would limit the valid range to 10000 days

         P4TJD = P4TJD + (DELYR/4)
C REW: valid through 2100 (which is a leap year)

C         ! ACCOUNT FOR LEAP YEAR
      END IF
C
      RETURN
      END

      BLOCK DATA BMODEL_BLOCK
      COMMON /bmodel/ gmagmo, colat, elong, modold, mmoold, tmold,
     &                bltime, mmoflg, ipass
      DOUBLE PRECISION gmagmo, colat, elong
      DOUBLE PRECISION modold, mmoold, tmold, bltime
      INTEGER mmoflg, IPASS
      DATA IPASS/0/
      END
