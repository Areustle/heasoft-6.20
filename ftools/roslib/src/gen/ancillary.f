C
C                             ancillary.f
C
C This file contains the following files used by AO and TILT packages. 
C Note that some file have been modified for the FTOOLS conversion.
C
C getray_n.f livtim.f ssx.f ban.f datejd.f caljd1.f cprec1.f angsep.f
C errstc.f ymdhms.f getgrd.f scater.f geosol.f atmabs.f densdr.f init.f wrerr.f
C gauleg.f fluor.f aden.f dradrs.f polre1.f repol1.f qgaus.f abs_x.f cnvfrc.f 
C jdcal1.f equecl.f cvequi.f streu.f oxy.f argon.f resval.f helium.f euler1.f 
C jdeps1.f cnvdms.f funct.f tlocal.f hydro.f ambar.f nitro.f nbin.f grav.f
C jdnut1.f 
C		(Srilal Jul '96)
C--------------------------------------------------------------------------

        SUBROUTINE GETRAY_N(INANC, TLOG,SPEC, dirstr)
C
CC  This routine returns Raymond and Smith spectra
C
C************************ FFORM VERSION 1.2 ************ 25-NOV-92 12:43
C
CA  author : JHS        date: 18-APR-1988 15:52
CU  update : SXRB       date: 25-NOV-1992 12:22
C
CT  status: not tested
C
C   general description 
CG  This routine returns Raymond and Smith spectra in the standard 729
CG  energy vector.  The routine will interpolate between calculated
CG  spectra on a delta log_10 T grid of 0.05.
C
C   call_var.          type I/O description 
CP  SPEC                R4    O array with photons per bin
CP  TLOG                R4      log_10 of desired RS spectrum
CP                              5.0 < log_10 T < 8.5
C
C   include_block name          description 
CI                              
C
C   routines_called    type     description 
CR                              
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER*4 BITPIX, BLOCK, GCOUNT, I, ICALL, INDHIGH, INDLOW, J, 
     +      LUN2, NAXES(10), NAXIS, PCOUNT, STATUS
        REAL*4    DELE(729), EFAR(2,729), SPEC(729), THIGH, TLOW,
     +      TLOG, TLOGX, W1, W2, XSPEC1(730), XSPEC2(730), ARRAY(81,729)
        character(80) INANC, DIRSTR
        character(80) CALFILE
        character(80) context
        LOGICAL ANYF, BADSTA, EXTEND, SIMPLE
C
        COMMON /EFFAREA/ DELE, EFAR
C
        SAVE  ICALL
        DATA ICALL /0/

C    LENACT is used to correctly concatenate a filename to 'dirstr'
      integer lenact
      external lenact
C     DIRSTR = ' '
      status = 0
      BADSTA = .false.

C   Access CALDB to get the name of the calfile for 'RAYMOND_SPECTRA'
C   Raymond and Smith thermal spectra (1991 vintage)
C
c     calFILE=DIRSTR(1:lenact(dirstr))//'RSPECTRA.FITS'
c     calFILE='/FTP/caldb/data/rosat/pspc/bcf/raymond_spectra_v1.fits
       call access_caldb(INANC, 'RAYMOND_SPECTRA', calfile, status)
       if (status .ne. 0) then
       context = 'GETRAY: Error in getting CALDB file for RAYMOND_SPEC.'
       call fcerr(context)
       goto 999
       endif
C

C
C  Open and read in the R&S spectral grid
C
        IF (ICALL .EQ. 0) THEN
c           LUN2 = 85
	call cgetlun(LUN2)
            CALL FTOPEN(LUN2,CALFILE,0,BLOCK,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
 		context = 'Unable to open calfile :'//CALFILE
        	call fcerr(context)
	        go to 999
            ENDIF
C
C  Read the FITS file header information
C
            CALL FTGHPR (LUN2,10,SIMPLE,BITPIX,NAXIS,NAXES,
     +              PCOUNT,GCOUNT,EXTEND,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information:'
     &      //CALFILE
		call fcerr(context)
		go to 999
            ENDIF
C
C  Read the data file
C
            CALL FTG2DE (LUN2,0,0,NAXES(1),NAXES(1),NAXES(2),ARRAY,
     +              ANYF,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context = 'Unable to read the Data file :'
     &      //CALFILE
		call fcerr(context)
		go to 999
            ENDIF
C
C  Close the FITS file
C
            CALL FTCLOS(LUN2,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
                context='Unable to close  FITS file: '//CALFILE
		call fcerr(context)
		go to 999
            ENDIF
            ICALL = 1
        ENDIF
C
C  Get indices
C
        TLOGX = TLOG
        IF (TLOG .LT. 5.) TLOGX = 5.
        IF (TLOG .GT. 8.5) TLOGX = 8.45
        INDLOW  = (TLOGX-5.)/0.05 + 1
C
        INDLOW  = INDLOW + 1
C
        INDHIGH = INDLOW + 1
        TLOW    = FLOAT(INDLOW-2)*0.05 + 5.
        THIGH   = TLOW + 0.05
        W1      = (THIGH-TLOGX)/0.05
        W2      = (TLOGX-TLOW) /0.05
C
        write(context,
c    &        '(''INDLOW='',i5,''INDHIGH='',i5,''TLOW'',f9.5,''THIGH'',f9.5)')
     &	'(2i5,2f9.5)')
     &     INDLOW,INDHIGH,TLOW,THIGH
	call fcecho(context)
C
C  Get the spectra to interpolate between
C
        DO I=1,729
            XSPEC1(I) = ARRAY(INDLOW,I)
            XSPEC2(I) = ARRAY(INDHIGH,I)
        ENDDO
C
        DO J=1,729
            SPEC(J) = (W1*XSPEC1(J)+W2*XSPEC2(J))
        ENDDO
C
999     continue
	if (status .ne. 0) then
	call fcerrm(status)
	stop
	endif

        RETURN
        END

        SUBROUTINE LIVTIM(A1LL,DEADTP,AXE,AEXE,
     +      FLIVE1,FLIVE2,FLIVE,IERR)
C
CC  Calculates PSPC livetime factor from Count Rates and Deadtime Param.
C
C************************ FFORM VERSION 1.2 ************ DD-MMM-YY HH:MM
C
CA  author : GRH        date: 13-MAR-1990 09:02 
CU  update : GRH        date: 13-MAR-1990 10:02 
C
CT  status: not tested
C
C   general description:
CG  The PSPC livetime factor, a value between 0 and 1) which has to be
CG  multiplied to the exposure time to obtain the effective live 
CG  exposure time, is calculated from a product of two values:
CG
CG  FLIVE1 using the input A1-lower-level-discriminator count rate 
CG  (A1LL) [cts/s] and the deadtime factor (DEADTP) [musec] according 
CG  to the recipe in the TN-ROS-ME-ZA00-025. The deadtime parameter
CG  DEADTP, which is actually a function of mean energy and PSPC, 
CG  should be specified from outside as a parameter.
CG
CG  FLIVE2 from the ratio between the accepted and evaluated X-ray 
CG  event rate (AEXE) and the accepted X-ray event rate (AXE). A 
CG  difference between those two indicates loss of events in the
CG  telemetry stream because of a busy readout.
C
C   call_var.          type I/O description
CP  A1LL                R4  I   EE-A1LL count rate from HK-data [cts/s]
CP  AXE                 R4  I   EE-AXE  count rate from HK-data [cts/s]
CP  AEXE                R4  I   EE-AEXE  count rate from HK-data [cts/s]
CP  DEADTP              R4  I   Deadtime Parameter (ca. 190-260 [musec])
CP  FLIVE1              R4    O PSPC Livetime Factor (between 0 and 1)
CP  FLIVE2              R4    O ER Livetime Factor (between 0 and 1)
CP  FLIVE               R4    O Ttotal Livetime Factor (between 0 and 1)
CP  IERR                I     O = 0 no error 
CP                              = 1 negative square root ARG (FLIVE1=1)
CP                              = 1 denominator = 0 (FLIVE2=1)
CP                              = 3 
C
C   include_block_name          description
CI  R$COMMON:CGENL.CMN          general parameter common block
C
C   routines_called    type     description
CR  HFLAG               R       output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description
CX  
C
c Authors/modifications
c Banashree M Seifert (Nov6, 1996)
c
c introduced subinfo to inform that count rate=0 when axe=0 
c instead of erroring out
C***********************************************************************
C
        IMPLICIT NONE
        REAL*4        A1LL,AXE,AEXE,ARG,DEADTP,FLIVE1,FLIVE2,FLIVE
ccc        INTEGER*4     IERR ,IOS
        integer     IERR 

        character(80) subinfo
        
        IERR = 0

C     first: calculate PSPC livetime FLIVE1

        ARG = 2.0E-6 * A1LL * DEADTP            

C     check for error condition

        IF((ARG .LT. 0.0) .OR. (ARG .GT. 1.0)) THEN
ccc            IERR = 1
            FLIVE1 = 1.
        ELSE 
            FLIVE1=SQRT(1.0-ARG)
        ENDIF

C     second: calculate ER livetime FLIVE2
C     check for error condition

        IF(AXE.EQ.0.0)THEN
           subinfo='one of accepted X-ray event rate in HK data is zero'
           call wtinfo(0,0,1,subinfo)
ccc            IERR = 1
            FLIVE2 = 1.
        ELSE 
            FLIVE2=AEXE/AXE
        ENDIF

C     third: multiply the two values
 
       FLIVE = FLIVE1 * FLIVE2

        RETURN
        END     

        SUBROUTINE SSX (infile, DJUL,RALOI,DECLOI,RASAI,DECSAI,ALTI,
     +      RASUI,DECSUI,SPECIN,SPCOUT,DENSIT,IFLAG,IERR, dirstr)
C
CC  Driving routine for the calculation of scattered X-ray spectra
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 16:29
C
CA  author : SLS        date: 18-DEC-1987 09:06
CU  update : SLS        date: 25-FEB-1988 13:19
CU  update : SXRB       date:  7-APR-1992 16:14
CU  update : SXRB       date: 15-OCT-1992 18:54 precession RA, DEC
CU  update : SXRB       date: 25-NOV-1992 12:47 
C
CT  status : not tested
C
C   general description 
CG  This program organizes the calculation of atmospheric column
CG  densities along the line of sight of the observation and the
CG  spectra of scattered solar X-rays from those column densities.
CG  With IFLAG = 2, an almost complete radiative transfer treatment
CG  is given.  With IFLAG = 1, the scattered spectrum is calculated
CG  assuming that the atmosphere is transparant.  This seems to work
CG  fine for zenith angles less than 97 degrees.  The column
CG  densities calculated for O2, N2, O, and A are good to four
CG  significant digits while He is better than 90% of the actual
CG  value and H is somewhere greater than 50%.  This is due to
CG  limiting the CG  integral to 2000 km.  The column
CG  densities of A and H are small enough to be ignored for the
CG  calculation of scattered solar X-ray intensity.  At the altitudes
CG  which ROSAT will fly oxygen dominates all of the scattering.
CG  In the input parameter list RALOI and DECLOI are for epoch J2000;
CG  the others (RASAI, DECSAI, RASUI, DECSUI) are for the epoch of
CG  observation, they are precessed to J2000 in this routine.
C
C   call_var.          type I/O description 
CP                              
CP  ALTI                R4  I   altitude of satellite in (km)
CP  DECLOI              R8  I   look declination (radian) (J2000)
CP  DECSAI              R8  I   satellite declination (radian) 
CP  DECSUI              R8  I   sun declination (radian)
CP  DJUL                R8  I   Julian date
CP  IERR                I4    O = 0 no error
CP                              = 1 vanishing column density
CP                              = 2 zenith angle > 100 degrees
CP  IFLAG               I4  I   = 1 simple model
CP                              = 2 full model (x70 longer)
CP  RALOI               R8  I   look right ascension (radian) (J2000)
CP  RASAI               R8  I   satellite right ascension (radian)
CP  RASUI               R8  I   sun right ascension (radian)
CP  DENSIT              R4    O line of sight densities of N2, O2,
CP                              O, A, He, H
CP  SPCOUT              R4    O output spectrum
CP  SPECIN              R4  I   input spectrum
C
C   include_block name          description 
CI  CCONST.CMN                   contains useful constants
CI  CPOS.CMN                    contains position information
CI  CDENS.CMN                   contains atmospheric density data
C
C   routines_called    type     description 
CR  DENSDR              SR      controls the density integrations
CR  INIT                SR      calculates the Cartesian coords of the
CR                              satellite, Sun, and look direction
CR  ANGSEP              SR      calculates angle between two directions
CR  SCATER              SR      scatters the incident spectrum
CR  GETGRD              SR      returns the PSPC energy grid
CR  GEOSOL              SR      returns solar/geomagnetic data
CR  ATMABS              SR      calculates atmospheric absorption
CR  CPREC1              SR      precession from one epoch to another
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C   A           starting point along look direction for integration
C   B           end point along look direction for integration
C   DATREF      Julian date of the start of the orbit
C   DK          declination of the satellite
C   DKL         declination of the look direction
C   DT          time interval of one step in Julian days
C   NPATH       number of calculation steps along the look direction
C   NUM         number of observations in a block
C   RA          right ascension of the satellite
C   RAL         right ascension of the look direction
C   SONNE(1)    right ascension of the Sun
C   SONNE(2)    declination of the Sun
C   SPCOUT      scattered solar spectrum
C   SPECIN      solar spectrum incident on the atmosphere
C   SLONG       ecliptic longitude of the Sun
C   ZENANG      zenith angle of the observation
C
        IMPLICIT NONE
C
C       PARAMETER NUM=729
C
C  Include common blocks
C
        INCLUDE '../../include/CDENS.CMN'
        INCLUDE '../../include/ROSCONST.INC'
        INCLUDE '../../include/CPOS.CMN'
C
        INTEGER*4 I, ID, IERR, IFLAG, II, IIFLAG, IM, ISTAT,
     +      M, NPATH
C
        REAL*4 A, ALTI, B, CANG1, CANG2, CSCATW, DENSIT(6), SPC1(729), 
     +      SPC2(729), SPECIN(729), SPCOUT(729), SSIGN, TEMP
C
        REAL*8 DECLOI, DECSAI, DECSUI, DELANG, DJUL, DTEMP, RALOI, 
     +      RASAI, RASUI, TEMPA, TEMPB, TEMPC, JD2000
C
        DATA JD2000 /2451544.5D0/

	character(80)  dirstr
	character*(*) infile 

C    LENACT is used to correctly concatenate a filename to 'dirstr'
      integer lenact
      external lenact
C
C  Get the ROSAT energy grid
C

C  ECEN and DELE are read in main and passed to SSX
C  No nead to call GETGRD routine

C       IF (ECEN(1) .EQ. 0) THEN
c           CALL GETGRD(DELE,ECEN, dirstr)
c       ENDIF


C
C  Set minimum and maximum height
C
        IF(IFLAG .EQ. 1) THEN
            HMIN = 220.
        ELSE
            HMIN = 110.
        ENDIF
        HMAX = 2000.
C
C  Shift the valiables so the originals are not corrupted
C
        ALT = ALTI
        DECLO = DECLOI
        RALO = RALOI
        DECSA = DECSAI
        RASA = RASAI
        DECSU = DECSUI
        RASU = RASUI
C
C  Next two CALLs included @15-OCT-1992:
C  Precess equatorial coordinates from DJUL to J2000
C
        CALL CPREC1(DJUL,JD2000,RASA,DECSA,RASA,DECSA,IERR)
        CALL CPREC1(DJUL,JD2000,RASU,DECSU,RASU,DECSU,IERR)
C
C  Now all equatorial coordinates refer to the same epoch JD2000
C
        SONNE(1) = RASU
        SONNE(2) = DECSU
C
C  Calculate the zenith angle
C
        CALL ANGSEP(RASA,DECSA,RALO,DECLO,DELANG,ISTAT)
        ZENANG = DELANG/RAD
C
C  Calculate the solar zenith angle
C
        CALL ANGSEP(RASA,DECSA,RASU,DECSU,DELANG,ISTAT)
        SOLZEN = DELANG/RAD
C
        IF((ISTAT .EQ. 0) .AND. (ZENANG .LT. 100.)) THEN
C
C  Get the solar and geomagnetic parameters
C
            CALL GEOSOL (infile, DJUL,GEO,dirstr)
C
C  Set the time
C
            AMJD = SNGL(DJUL - 2400000.5D0)
C
C  Find the cosine of the scattering angle
C
            CALL ANGSEP(RASU,DECSU,RALO,DECLO,DELANG,ISTAT)
            CSCATW = DCOS(DELANG)
C
C  Initialize directions
C
            CALL INIT
            CANG1 = ACOS((REARTH + HMIN)/(REARTH + ALT))
            CANG2 = (PIH + CANG1)/RAD
C
C  Check for a shortened path length due to the satellite being
C  behind the Earth
C
            A = 0.
            IF(SOLZEN .GT. 90.) THEN
                TEMPC = DXS(2)**2 + DXS(3)**2
                TEMPB = DBLE((REARTH + HMIN)**2)
                IF(TEMPB .GT. TEMPC) THEN
C
C  Path starts in the dark
C
                    TEMPC = TEMPC - TEMPB
                    TEMPB = 2.D0*(DXS(2)*DXL(2) + DXS(3)*DXL(3))
                    TEMPA = DXL(2)**2 + DXL(3)**2
                    A = SNGL((-TEMPB + DSQRT(TEMPB*TEMPB
     +                              - 4.D0*TEMPA*TEMPC))/(2.D0*TEMPA))
                ENDIF
            ENDIF
C
C  Set the path length for the integration
C
            IF((IFLAG .EQ. 2) .AND. (ZENANG .GT. CANG2)) THEN
C
C  Full treatment and looking down
C
                TEMP = HMIN
                SSIGN = -1.
            ELSE
C
C  Both methods and looking up
C
                TEMP = HMAX
                SSIGN = 1.
            ENDIF
C
            TEMPC = DXS(1)**2 + DXS(2)**2 + DXS(3)**2
            TEMPB = DBLE((REARTH + TEMP)**2)
            TEMPC = TEMPC - TEMPB
            TEMPB = 2.D0*(DXS(1)*DXL(1) + DXS(2)*DXL(2) +
     +                      DXS(3)*DXL(3))
            TEMPA = DXL(1)**2 + DXL(2)**2 + DXL(3)**2
            DTEMP = (-TEMPB + SSIGN*DSQRT(TEMPB*TEMPB
     +                       - 4.D0*TEMPA*TEMPC))/(2.D0*TEMPA)
C
C  For full treatment, check to see if the path ends in the dark
C
            IF((IFLAG .EQ. 2) .AND.
     +                      ((DXS(1) + DTEMP*DXL(1)) .GT. 0.D0)) THEN
C
C  There is possible shadowing of the line-of-sight
C
                TEMPA = (DXS(2) + DTEMP*DXL(2))**2 +
     +                          (DXS(3) + DTEMP*DXL(3))**2
                TEMPB = DBLE((REARTH + HMIN)**2)
                IF(TEMPB .GT. TEMPA) THEN
C
C  The path ends in shadow
C
                    IF(A .GT. 1.E-10) THEN
C
C  The whole path is in shadow
C
                        DTEMP = 0.D0
                    ELSE
C
C  The path starts in the sun and ends in shadow
C
                        TEMPC = DXS(2)**2 + DXS(3)**2 - TEMPB
                        TEMPB = 2.D0*(DXS(2)*DXL(2) +
     +                                  DXS(3)*DXL(3))
                        TEMPA = DXL(2)**2 + DXL(3)**2
                        DTEMP = (-TEMPB - DSQRT(TEMPB*TEMPB
     +                              - 4.D0*TEMPA*TEMPC))/(2.D0*TEMPA)
                    ENDIF
                ENDIF
            ENDIF
            B = SNGL(DTEMP)
C
C  Set the number of steps for the integration
C
            IF(B .GT. A) THEN
C
C  Part of the path is in the sun
C
                NPATH = INT((B - A)/100. + 0.5)
                IF(NPATH .LT. 20) NPATH = 20
                IF(NPATH .GT. 40) NPATH = 40
C
C  Get the densities
C
                CALL DENSDR (A,B,IFLAG,NPATH)
C
C  Copy line of sight column densities into return variable
C
                DO ID=1,6
                    DENSIT(ID) = DENS(1,ID)
                ENDDO
C
C  Process by the two methods
C
                IF(IFLAG .EQ. 1) THEN
C
C  Simple model
C  See if there is anything to scatter off of
C
                    IF(DENS(1,3) .GT. 1.E10) THEN
C
C  Do the scattering
C
                        CALL SCATER(CSCATW,IFLAG,729,SPECIN,SPCOUT)
                    ELSE
C
C  Column density is too low to worry about
C
                        IERR = 1
                        DO I=1,729
                            SPCOUT(I) = 0.
                        ENDDO
                        DO I=1,6
                            DENSIT(I) = 0.
                        ENDDO
                    ENDIF
                ELSE
C
C  Full model
C  First, reset the output spectrum to 0.
C
                    DO IM=1,729
                        SPCOUT(IM) = 0.
                    ENDDO
C
C  Start the loop over the scatter points to calculate the band rates
C  at the satellite
C
                    DO IM=1,NPATH
                        M = IM
C
C  Absorb the spectrum by the atmosphere between the Sun and the
C  scatter point
C
                        IIFLAG = 1
                        CALL ATMABS(IIFLAG,M,729,SPECIN,SPC2)
C
C  Scatter the resultant spectrum
C
                        CALL SCATER(CSCATW,M,729,SPC2,SPC1)
C
C  Absorb the scattered spectrum by the atmosphere between the scatter
C  point and the satellite
C
                        IIFLAG = 2
                        CALL ATMABS(IIFLAG,M,729,SPC1,SPC2)
C
C  Add to the resultant spectrum
C
                        DO II=1,729
                            SPCOUT(II) = SPCOUT(II) +
     +                                      DENS(M,20)*SPC2(II)
                        ENDDO
C
C  End the loop over the line-of-sight positions
C
                    ENDDO
                ENDIF
C
C  Path is entirely in shadow
C
            ELSE
                IERR = 1
                DO I=1,729
                    SPCOUT(I) = 0.
                ENDDO
                DO I=1,6
                    DENSIT(I) = 0.
                ENDDO
            ENDIF
C
C  ZENANG > 100 degrees so zero everything
C
        ELSE
            IERR = 2
            DO I=1,729
                SPCOUT(I) = 0.
            ENDDO
            DO I=1,6
                DENSIT(I) = 0.
            ENDDO
        ENDIF
C
        RETURN
        END
        SUBROUTINE BAN(N,SPECIN,BAND,DRM)
C
CC  This routine convolves an input spectrum with the counter response
C
C************************ FFORM VERSION 1.2 ************ 25-NOV-92 12:24
C
CA  author : SLS        date:  5-JAN-1988 12:33
CU  update : SLS        date: 25-FEB-1988 13:06
CU  update : SXRB       date: 25-NOV-1992 12:22
C
CT  status: not tested
C
C   general description 
CG  This routine convolves an input spectrum with the counter response.
CG  It also bins the result in coarse bands
C
C   call_var.          type I/O description 
CP  BAND                R4    O band rates
CP  N                   I4      array size
CP  SPECIN              R4  I   incident spectrum
CP  DRM                 R4  I   detector response matrix
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER*4 I, II, N
C
        REAL*4 BAND(8), DRM(256,729), SPECIN(N), TEMP
C
C  Zero the band array
C
        DO 20 I=1,8
            BAND(I) = 0.
   20   ENDDO
C
C  Loop over the channels of interest
C
        DO I=8,201
C
C  Convolve the spectrum with the counter response
C
            TEMP = 0.
            DO II=1,700
                TEMP = TEMP + DRM(I,II)*SPECIN(II)
            ENDDO
C
C  Sort the model counts into the proper bins
C
            IF(I .LE. 19) THEN
                BAND(1) = BAND(1) + TEMP
            ELSEIF(I .LE. 41) THEN
                BAND(2) = BAND(2) + TEMP
            ELSEIF(I .LE. 51) THEN
                BAND(3) = BAND(3) + TEMP
            ELSEIF(I .LE. 69) THEN
                BAND(4) = BAND(4) + TEMP
            ELSEIF(I .LE. 90) THEN
                BAND(5) = BAND(5) + TEMP
            ELSEIF(I .LE. 131) THEN
                BAND(6) = BAND(6) + TEMP
            ELSE
                BAND(7) = BAND(7) + TEMP
            ENDIF
            BAND(8) = BAND(8) + TEMP
   50   ENDDO
C
        RETURN
        END
      subroutine DATEJD(DT,LCHECK,DJD,IERR)
C
CC    Calculates the Julian Date out of the Calendar Date
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 16:56
C
CA  author : KOD        date: 22-AUG-1989 18:31
CU  update : KOD        date: 28-MAR-1990 20:11
C
CT  status: tested
C
C   general description 
CG  Subroutine to calculate the Julian Date out of the calendar date.
CG  The calendar date is assumed to be given in the Gregorian system
CG  for dates starting with October 15, 1582, and in the Julian
CG  system until October 4, 1582 (in between it is undefined).
CG
CG  This subroutine is valid for the years  -4712  to  +3500 ,
CG  and has been written according to
CG     van Flandern and Pulkkinen:
CG     'Low-Precision Formulae for Planetary Positions',
CG     Astrophysical Journal Supplement Series 41, 1979, p.392.
CG  and
CG     Jean Meeus, 'Astronomical Formulae for Calculators',
CG                 third edition, 1985, p.23-25 .
CG
CG  An extended input check is possible for valid dates.
C
C   call_var.          type I/O description 
CP  DT                  R8  I   Calendar Date  (YYYYMMDD.HHMMSSS)
CP  LCHECK              L4  I   extended input check if LCHECK=.TRUE.
CP
CP  DJD                 R8    O Julian Date
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning (unusual input values)
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  YMDHMS              SR      splits the variable DT into its parts
CR  CALJD1              SR      converts the Calendar Date to J.D.
CR  ERRSTC              SR      continue error stack for one level
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DT, DH, DJD
C     REAL*4        HFLAG
      REAL*4         SEC
C     INTEGER*4     IOS
      INTEGER*4     IERR, IY, IM, ID, IHR, IMT, ISTAT
      LOGICAL*4     LCHECK, LCHK
      character(8)   RTNAME
C
      DATA          RTNAME /'DATEJD'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR=0
C
      DH = DT
      CALL YMDHMS(DH,IY,IM,ID,IHR,IMT,SEC,ISTAT)
CN!   ERROR handling
        if  (ISTAT.gt.1) then
          IERR = max(IERR,ISTAT)
          call ERRSTC(RTNAME,10)
        endif
C
      LCHK=LCHECK
      CALL CALJD1(IY,IM,ID,IHR,IMT,SEC,LCHK,DH,IERR)
CN!       ERROR handling
          if  (ISTAT.gt.1) then
            IERR = max(IERR,ISTAT)
            call ERRSTC(RTNAME,20)
          endif
C
      DJD=DH
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine CALJD1(IY,IM,ID,IHR,IMT,SEC,LCHECK,DJD,IERR)
C
CC    Converts the calendar date to the Julian Date
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 16:43
C
CA  author : KOD        date: 18-JUL-1989 15:03
CU  update : KOD        date: 28-MAR-1990 15:54
CU  update : KOD        date: 28-MAR-1990 19:23
C
CT  status: tested (08-AUG-1989 17:45 / KOD)
C
C   general description 
CG  Subroutine to calculate the Julian Date out of the calendar date.
CG  The calendar date is assumed to be given in the Gregorian system
CG  for dates starting with October 15, 1582, and in the Julian
CG  system until October 4, 1582 (in between it is undefined).
CG
CG  This subroutine is valid for the years  -4712  to  +3500 ,
CG  and has been written according to
CG     van Flandern and Pulkkinen:
CG     'Low-Precision Formulae for Planetary Positions',
CG     Astrophysical Journal Supplement Series 41, 1979, p.392.
CG  and
CG     Jean Meeus, 'Astronomical Formulae for Calculators',
CG                 third edition, 1985, p.23-25 .
CG
CG  For negative years all the other parameters must be positive.
CG  An extended input check is possible for valid dates.
C
C   call_var.          type I/O description 
CP  IY                  I4  I   year         (-4712 .. +3500)
CP  IM                  I4  I   month        (1 .. 12)
CP  ID                  I4  I   day of month (1 .. 28,29,30,31)
CP  IHR                 I4  I   hours        (0 .. 23)
CP  IMT                 I4  I   minutes      (0 .. 59)
CP  SEC                 R4  I   seconds      (0.0 .. 59.999...)
CP
CP  LCHECK              L4  I   extended input check if LCHECK=.TRUE.
CP
CP  DJD                 R8    O corresponding Julian Date
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  CNVFRC              SR      conversion to fractional number
CR  JDCAL1              SR      converts Jul. Date into Calendar Date
CR  WRERR               SR      formats error code in standard form
CR  ERRSTC              SR      continue error stack for one level
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
C     INTEGER*4     IOS
      INTEGER*4     IY, IM, ID, IERR, IYR, IMTH, IDAY, IHR, IMT, ISTAT,
     &              JD, IYY, IMM, IHR1, IMT1, IDAT
C     REAL*4        HFLAG
      REAL*4        SEC, SEC1
      REAL*8        DJD, DFRC, DH
      LOGICAL*4     LCHECK, LPLUS
      character(8)   RTNAME
C
      DATA          RTNAME /'CALJD1'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      JD = 0
      IERR = 0
C
CR!   if wrong input
C
      if ( IY .lt.-4712 .or. IY .gt.3500 .or.
     &     IM .lt.1     .or. IM .gt.12   .or.
     &     ID .lt.1     .or. ID .gt.31   .or.
     &     IHR.gt.23    .or. IMT.gt.59   .or.
     &     SEC.gt.60.0 )  then
C
        IERR = 2
        call WRERR(RTNAME,IERR,'Error in input parameters IY,IM,ID')
C
      endif
C
C
      if  (IERR.lt.2)  then
C
C       calculate the Julian Day for 0:00 UT
C
        IDAT = isign((iabs(IY)*100+IM)*100+ID,IY)
C
        if (IDAT.ge.15821015) then
C
C         assume Gregorian calendar date as input:
C
          JD = 367*IY - 7*(IY+(IM+9)/12)/4 - 3*((IY+(IM-9)/7)/100+1)/4
     &       + 275*IM/9 + ID + 1721029
C
        else if (IDAT.le.15821004) then
C
C         assume Julian calendar date as input:
C
          if (IM.gt.2) then
            IYY = IY
            IMM = IM
          else
            IYY = IY - 1
            IMM = IM + 12
          endif
C
          if (IYY.gt.0) then
            DH=0.D0
          else
            DH=0.75D0
          endif
C
          JD = idint(365.25d0*dble(IYY)-DH) +
     &         idint(30.6001d0*dble(IMM+1)) +
     &         ID + 1720995
C
          else
C
            IERR = max(IERR,2)
            call WRERR(RTNAME,IERR,'Error in date: calendar reform')
C
        endif
C
C       calculate the fractional part:
C
        LPLUS = .true.
        IHR1 = IHR
        IMT1 = IMT
        SEC1 = SEC
        call CNVFRC(LPLUS,IHR1,IMT1,SEC1,DFRC,ISTAT)
C
CN!     ERROR handling
C
        if (ISTAT.gt.1) then
          IERR = max(IERR,ISTAT)
          call ERRSTC(RTNAME,10)
        endif
C
C       add both parts
C
        DJD = dble(JD) + DFRC/24.D0 - 0.5D0
C
        IF (LCHECK) THEN
C
CJ!       extended input check
C
          DH = dble(JD) - 0.5D0*0.0
          call JDCAL1(DH,IYR,IMTH,IDAY,IHR1,IMT1,SEC1,ISTAT)
C
CN!       ERROR handling
C
          if  (ISTAT.gt.1) then
            IERR = max(IERR,ISTAT)
            call ERRSTC(RTNAME,20)
          endif
C
CR!       if inconsistent results
C
          if (IYR .ne.IY  .or. IMTH.ne.IM .or. IDAY.ne.ID) then
            IERR = max(IERR,2)
            call WRERR(RTNAME,IERR,'Error: Date does not exist')
          endif
        endif
      endif
C
CH!   CJD
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine CPREC1(DJD1,DJD2,DLOLD,DBOLD,DLNEW,DBNEW,IERR)
C
CC    Converts equatorial coordinates from one equinox to another
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 16:55
C
CA  author : KOD        date: 26-SEP-1989 15:55
CU  update : KOD        date: 28-MAR-1990 20:03
C
CT  status: tested
C
C   general description 
CG  Subroutine to convert the equatorial coordinates from one
CG  equinox to another, according to
CG     Jean Meeus, 'Astronomical Formulae for Calculators',
CG                 third edition, 1985, p.65,66.
CG
CGT Notes: the effect of the proper motion must be calculated BEFORE
CGT        converting the coordinates to another epoch.
CGT
CGT        The equinox of 1900.0 corresponds to JD 2415020.313,
CGT        the equinox of 1950.0 corresponds to JD 2433282.423,
CGT        according to J.Meeus, p.65 .
CGT        The equinox of 2000.0 corresponds to JD 2451545.000.
C
C   call_var.          type I/O description 
CP  DJD1                R8  I   Julian Date for the initial epoch
CP  DJD2                R8  I   Julian Date for the  final  epoch
CP
CP  DLOLD               R8  I   old longitude  (radians)
CP  DBOLD               R8  I   old latitude   (radians)
CP
CP  DLNEW               R8    O new longitude  (radians)
CP  DBNEW               R8    O new latitude   (radians)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
CI  IGENL.INC         parameter block with trig. constants
C
C   routines_called    type     description 
CR  CVEQUI              SR      calculates quantities for epoch conv.
CR  REPOL1              SR      rectangular to polar coord. conversion
CR  DRADRS              R8      'resets' radians
CR  WRERR               SR      formats error code in standard form
CR  ERRSTC              SR      continue error stack for one level
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
      INCLUDE '../../include/IGENL.INC'
C
      REAL*8        DJD1, DJD2, DLOLD, DBOLD, DLNEW, DBNEW, DZETA, DZ,
     &              DTHETA, DL, DCL, DSL, DCB, DSB, DCT, DST, DA, DB,
     &              DC, DH1, DR, DPHI, DH2, DRADRS, DJ1, DJ2
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR , ISTAT
      character(8)   RTNAME
C
      DATA          RTNAME /'CPREC1'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      if (DLOLD.lt.0.D0 .or. DLOLD.gt.D2PI) then
        IERR = max(IERR,1)
        call WRERR(RTNAME,IERR,'Warning: unusual input value for DLOLD')
      endif
C
      if (dabs(DBOLD).gt.DPID2) then
        IERR = max(IERR,2)
        call WRERR(RTNAME,IERR,'Error in input parameter DBOLD')
      endif
C
      if (IERR.lt.2) then
        DJ1 = DJD1
        DJ2 = DJD2
        call CVEQUI(DJ1,DJ2,DZETA,DZ,DTHETA,ISTAT)
C
        if (ISTAT.gt.1) then
          IERR = max(IERR,ISTAT)
          call ERRSTC(RTNAME,10)
        endif
C
      endif
C
      if (IERR.lt.2) then
C
        DL = DLOLD + DZETA
        DCL = cos(DL)
        DSL = sin(DL)
C
        DCB = cos(DBOLD)
        DSB = sin(DBOLD)
C
        DCT = cos(DTHETA)
        DST = sin(DTHETA)
C
        DA = DCB * DSL
        DB = DCT * DCB * DCL - DST * DSB
        DC = DST * DCB * DCL + DCT * DSB
C
        DH1 = 0.D0
        call REPOL1(DB,DA,DH1,DR,DPHI,DH2)
        DLNEW = DRADRS(DPHI + DZ)
C
        DBNEW = asin(DC)
        if (abs(DBNEW).gt.0.5D0*DPID2) DBNEW = sign(acos(DR),DBNEW)
C
      endif
C
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine ANGSEP(DLONG1,DLAT1,DLONG2,DLAT2,DANG,IERR)
C
CC    Calculates the angular separation between two celestial bodies
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 16:48
C
CA  author : KOD        date: 06-SEP-1989 16:40
CU  update : KOD        date: 20-MAR-1990 15:45
CU  update : KOD        date: 24-OCT-1991 13:57  (sin --> dsin, ...)
C
CT  status: tested
C
C   general description 
CG
CG  Subroutine to calculate the angular separation between two
CG  celestial bodies. The 'latitude' is defined as to have
CG  0 degrees at the equator and +90 degrees at the north pole.
CG  Special care is taken for small angles, according to
CG    R.W.Sinnott, 'Sky and Telescope', August 1984, p.159.
C
C   call_var.          type I/O description 
CP
CP  DLONG1              R8  I   'longitude 1' (e.g. R.A.; radians)
CP  DLAT1               R8  I   'latitude  1' (e.g. Dec.; radians)
CP
CP  DLONG2              R8  I   'longitude 2' (e.g. R.A.; radians)
CP  DLAT2               R8  I   'latitude  2' (e.g. Dec.; radians)
CP
CP  DANG                R8    O angular separation       (radians)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  R$COMMON:CGENL.CMN          general parameter common block
CI  R$INCLUDE:IGENL.INC         parameter block with trig. constants
C
C   routines_called    type     description 
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
      INCLUDE '../../include/IGENL.INC'
C
      REAL*8        DLONG1, DLAT1, DLONG2, DLAT2, DANG, D1, D2, DR
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR 
      character(8)   RTNAME
C
      DATA          RTNAME /'ANGSEP'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      if (max(abs(DLONG1),abs(DLONG2)).gt.D2PI) then
        IERR = max(IERR,1)
        call WRERR(RTNAME,IERR,'Unusual value for the longitude')
      endif
 
      if (max(abs(DLAT1), abs(DLAT2)).gt.DPID2) then
        IERR = max(IERR,2)
        call WRERR(RTNAME,IERR,'Error in input parameters')
      endif
C
      if (IERR.lt.2) then
C
        D1 = dsin ( 0.5D0 * ( DLAT1 -  DLAT2 ) )
        D2 = dsin ( 0.5D0 * (DLONG1 - DLONG2 ) )
        DR = D1 * D1 + D2 * D2 * dcos(DLAT1) * dcos(DLAT2)
        DANG = 2.D0 * datan2 ( dsqrt(DR), dsqrt(1.D0-DR) )
C
      endif
C
C     IF (HFLAG(RTNAME,'out: !<output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
        SUBROUTINE ERRSTC(RTNAME,NUM)
C
        INTEGER*4 NUM
        character(8) RTNAME
C
        RETURN
        END
      subroutine YMDHMS(DT,IY,IM,ID,IHR,IMT,SEC,IERR)
C
CC    Splits the calendar date into its components
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:11
C
CA  author : KOD        date: 19-JUL-1989 18:31
CU  update : KOD        date: 28-MAR-1990 15:52
C
CT  status: tested (02.08.1989 / K.Dennerl)
C
C   general description 
CG  Subroutine to split the calendar date, supplied as a single
CG  number in the form YYYYMMDD.HHMMSSS, into its components.
CG  The years may cover the range -9999 .. +9999.
CG  In case of negative years only the year IY will be negative.
CG  Resolution (given by REAL*8): 10~msec.
C
C   call_var.          type I/O description 
CP  DT                  R8  I   calendar date (YYYYMMDD.HHMMSSS)
CP
CP  IY                  I4    O year
CP  IM                  I4    O month
CP  ID                  I4    O day
CP  IHR                 I4    O hour
CP  IMT                 I4    O minute
CP  SEC                 R4    O second
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DT, DH
C     REAL*4        HFLAG
      REAL*4        SEC
C     INTEGER*4     IOS
      INTEGER*4     IERR, IY, IM, ID, IHR, IMT, I1, I2, ISEC1,
     &              ISEC2
      LOGICAL*4     LPLUS
      character(8)   RTNAME, CHDAT1, CHDAT2
C
      DATA          RTNAME /'YMDHMS'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
C     extract sign:
C
      LPLUS = (DT.ge.0.D0)
      DH = dabs(DT)
C
C     check length of calendar date:
C
      if (DH.gt.99991231.2359599D0) then
C
        IERR = max(IERR,2)
        call WRERR(RTNAME,IERR,'Error in IY')
C
      else
C
C       split according to decimal point:
C
        I1 = int(DH)
        I2 = nint((DH-int(DH))*1.D8)
        write(CHDAT1,'(I8.8)') I1
        write(CHDAT2,'(I8.8)') I2
C
C       read individual parts:
C
        read(CHDAT1,'(I4,I2,I2)') IY, IM, ID
        read(CHDAT2,'(I2,I2,I2,I2)') IHR, IMT, ISEC1, ISEC2
C
C       reconstruct seconds:  (rounded to 10 msec)
C
        SEC = real(ISEC1) + real(ISEC2)*1.E-2
C
C       (rough) check for valid calendar date:
C
        if (IM.lt.1 .or. IM.gt.12 .or. ID.lt.1 .or. ID.gt.31 .or.
     &      IHR.gt.23 .or. IMT.gt.59 .or. SEC.gt.60.0) then
          IERR = max(IERR,2)
          call WRERR(RTNAME,IERR,'Error in IM, ID, IHR, IMT, SEC')
        endif
C
C       add sign:
C
        if (.not. LPLUS)  IY = -IY
C
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END


C*******      Not used anymore *********
c       SUBROUTINE GETGRD(DELE,ECEN, dirstr)
C
CC  Gets the ROSAT energy grid
C
C************************ FFORM VERSION 1.0 ************ 25-FEB-88 08:57
C
CA  update : JHS               date: 9-FEB-1988 15:44
CU  update : SLS               date: 23-FEB-1988 10:23
C
CT  status: not tested
C
C   general description 
CG  Gets the ROSAT energy grid
C
C   call_var.          type I/O description 
CP  DELE                R4    O contains the bin widths in eV
CP  ECEN                R4    O contains the bin centers in eV
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
C       PARAMETER (IPAR=729)
C
c       INTEGER*4 IOS
c       REAL*4 DELE(729), ECEN(729)
c       character(80) DIRSTR
c       character(80) INFILE
C
C       DATA DIRSTR /' '/
C    LENACT is used to correctly concatenate a filename to 'dirstr'
c     integer lenact
c     external lenact
C
C  Open file
C
c       INFILE = DIRSTR(1:lenact(dirstr))//'ENERGY.GRD'
c       OPEN(UNIT=20,FILE=INFILE,
c    +          FORM='FORMATTED',TYPE='OLD',IOSTAT=IOS)
C
C  Read the ROSAT bin center energies (eV)
c
c       READ(20,*) ECEN
C
C  Read the ROSAT bin widths (eV)
C
c       READ(20,*) DELE
C
C  Close file
C
c       CLOSE(UNIT=20)
C
c       RETURN
c       END


      SUBROUTINE SCATER(CSCATW,M,N,SPC1,SPC2)
C
CC  Scatters the solar X-rays
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 16:51
C
CA  author : SLS        date: 28-DEC-1987 14:31
CU  update : SLS        date: 23-FEB-1988 10:01
CU  update : SXRB       date:  7-APR-1992 16:51
C
CT  status: not tested
C
C   general description 
CG  Scatters the solar X-rays.  Only Thomson scattering and fluorescent
CG  scattering from nitrogen and oxygen is considered.
C
C   call_var.          type I/O description 
CP  CSCATW              R4  ?/? cosine of the scattering angle
CP  M                   I4  I   density bin index
CP  N                   I4      number of fine bins
CP  SPC1                R4  I   input spectrum
CP  SPC2                R4    O output spectrum
C
C   include_block name          description 
CI  CDENS.CMN                   contains column and space density info
C
C   routines_called    type     description 
CR  FLUOR               SR      calculates fluorescent cross sections
CR  STREU               SR      calculates elastic cross sections
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INCLUDE '../../include/CDENS.CMN'
C
        INTEGER*4 I, IERR1, IERR2, KONT, M, N
C
        REAL*4 CSCATW, E, SIGMA, SPC1(N), SPC2(N), TAU
C
C  Get the number of scattered photons
C
        DO 100 I=1,N
C
C  Zero the output bin
C
            SPC2(I) = 0.
C
            IF(SPC1(I) .GT. 0.) THEN
C
C  Get the optical depth for elastic scattering
C  E is the bin center energy
C
                E = ECEN(I)
                TAU = 0.
                DO 40 KONT=1,6
C
C  Get the scattering cross section
C
                    CALL STREU(E,KONT,CSCATW,SIGMA,IERR1)
C
C  Add to the optical depth
C
                    TAU = TAU + SIGMA*DENS(M,KONT)
   40           ENDDO
C
C  Now do the scattering, optically thin case [1 - exp(-TAU) = TAU]
C
                SPC2(I) = SPC1(I)*TAU
C
C  Do the fluorescent scattering for N2, O2, and O
C
                DO 50 KONT=1,3
                    IF(E .GT. EDGE(KONT)) THEN
C
C  Get the cross section for fluorescent scattering
C
                        CALL FLUOR(E,KONT,SIGMA,IERR2)
C
C  Set the optical depth
C
                        TAU = SIGMA*DENS(M,KONT)
C
C  Do the scattering, again, optically thin case.  NLINE is the bin
C  index of the line
C
                        SPC2(NLINE(KONT)) = SPC2(NLINE(KONT)) +
     +                      TAU*SPC1(I)
                    ENDIF
   50           ENDDO
            ENDIF
  100   ENDDO
C
        RETURN
        END

C-----------------------------------------------------------------------
C
C       SUBROUTINE GEOSOL (INANC, DJUL,GEO,dirstr)
C
CC  Returns solar 10.7 cm flux and geomagnetic Kp index
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 16:46
C
CA  author : SLS        date: 20-FEB-1991 08:38
CU  update : SXRB       date:  7-APR-1992 16:46  
CU  update : SXRB       date: 27-OCT-1992 08:56 open with READONLY, SAVE
C   Modified  : Srilal Weera  Oct 96
C Note:This routine is now modified. Data are read in main and passed to GEOSOL.
C New arrays are defined to facilitate reading FITS files.
C JULFTMP() is made REAL*4 since the DATE_JULIAN column in the data file is
C single precision. (otherwise gives a warning mesg. on ALPHAS with -g option)
C
C
CT  status: not tested
C
C   general description 
CG  Returns solar 10.7 cm flux and geomagnetic Kp index.  Requires
CG  data file SG.DAT
C
C   call_var.          type I/O description 
CP  DJUL                R8  I   Julian date
CP  GEO                 R4  I   10.7 cm fluxes and geomagnetic index
CP                              GEO(1) = 10.7 cm solar flux 1.71 days
CP                                      before the date
CP                                      (1.E-22 Watts/m**2/Hz)
CP                              GEO(2) = 10.7 cm solar flux average over
CP                                      4 solar rotations centered on
CP                                      the date in question
CP                                      (1.E-22 Watts/m**2/Hz)
CP                              GEO(3) = geomagnetic planetary index, Kp
CP                                      0.279 days before the date
CP                                      (3- = 2.667, 3o = 3.000,
CP                                      3+ = 3.333, etc.)
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
        SUBROUTINE GEOSOL (infile, DJUL,GEO,dirstr)
C
C   variables   meaning
C
        IMPLICIT NONE
C
C
        INTEGER*4 I, I1, I2, IC, IFA, IFL, II, IKP, IOS,  
     +          NF, NK, NTOT
        INTEGER*4 IDATE(3000), j

C IDATE reads the first column of fits file (not used in the calculations)
C

        REAL*4 A(8), F, GEO(3), RKP(24000), F107(3000), F107AV(3000)
        REAL*8 DJUL, JULKP(24000), TEMPJD, JULF(3000)
        REAL*4  JULFTMP(3000)
        REAL*4 KP1(3000), KP2(3000), KP3(3000), KP4(3000)
 	REAL*4 KP5(3000), KP6(3000), KP7(3000), KP8(3000)
C   KP_(3000) reads Geo/Mag columns from the fits file. Note, this is
C   diferent than the old way of reading sequentially from the SG.DAT file

C 
        character(80)  dirstr
        CHARACTER*(*) INFILE
	INTEGER STATUS
C
C       DATA DIRSTR /' '/
        DATA IC, IFA, IFL, IKP /0, 1, 1, 1/

       COMMON /GEOSL1/ IDATE, JULFTMP, F107, F107AV,
     &        KP1, KP2, KP3, KP4, KP5, KP6, KP7, KP8

C
C  save all variables for future calls
C
        SAVE

C    LENACT is used to correctly concatenate a filename to 'dirstr'
        integer lenact
        external lenact

        STATUS = 0

C The following fix is done since data are read into REAL*4 array
        DO 125 i=1,3000
	JULF(i) = JULFTMP(i)
125         continue

        IF(IC .EQ. 0) THEN
            IC = 1
cc          INFILE = DIRSTR(1:lenact(dirstr))//'SG.DAT'
cc          OPEN(UNIT=72,STATUS='OLD',FILE=INFILE,READONLY)
cc          READ(72,1000) TEMP
c1000       FORMAT(A80)
cc         READ(72,1000) TEMP
C
            IOS = 0
            NF = 1
            NK = 0
cc          DO WHILE (IOS .EQ. 0)
c               READ(72,*,IOSTAT=IOS) ITEMP, JULF(NF), F107(NF),
c    +              F107AV(NF), (A(I),I=1,8)
cc              IF(IOS .EQ. 0) THEN
        do 165 j=1,2099
		 A(1) = KP1(j)
		 A(2) = KP2(j)
		 A(3) = KP3(j)
		 A(4) = KP4(j)
		 A(5) = KP5(j)
		 A(6) = KP6(j)
		 A(7) = KP7(j)
		 A(8) = KP8(j)
                    DO I=1,8
                       NK = NK + 1
                       JULKP(NK) = JULF(NF) + I*0.125D0 - 0.0625D0
                       RKP(NK) = A(I)
                    ENDDO
                    JULF(NF) = JULF(NF) + 0.5D0
                    NF = NF + 1
cc              ENDIF
cc          ENDDO
  165          continue
            NF = NF - 1
cc          CLOSE(UNIT=72)



C
C  Determine the average 10.7 cm flux over 4 solar rotations
C
            DO I=1,NF
                IF(I .LE. 28) THEN
                    I1 = 1
                    I2 = 56
                    NTOT = 56
                ELSEIF(I .LE. 35) THEN
                    I1 = I - 27
                    I2 = I + 28
                    NTOT = 56
                ELSEIF(I .LE. 42) THEN
                    I1 = 1
                    I2 = 84
                    NTOT = 84
                ELSEIF(I .LE. 49) THEN
                    I1 = I - 41
                    I2 = I + 42
                    NTOT = 84
                ELSEIF(I .LE. 56) THEN
                    I1 = 1
                    I2 = 112
                    NTOT = 112
                ELSEIF(I .LT. NF-56) THEN
                    I1 = I - 55
                    I2 = I + 56
                    NTOT = 112
                ELSEIF(I .LE. NF-49) THEN
                    I1 = NF - 111
                    I2 = NF
                    NTOT = 112
                ELSEIF(I .LT. NF-42) THEN
                    I1 = I - 41
                    I2 = I + 42
                    NTOT = 84
                ELSEIF(I .LE. NF-35) THEN
                    I1 = NF - 83
                    I2 = NF
                    NTOT = 84
                ELSEIF(I .LT. NF-28) THEN
                    I1 = I - 27
                    I2 = I + 28
                    NTOT = 56
                ELSE
                    I1 = NF - 55
                    I2 = NF
                    NTOT = 56
                ENDIF
C
                F = 0.
                DO II=I1,I2
                    F = F + F107(II)
                ENDDO
                F = F/NTOT
C
C                PRINT *, JULF(I), F107AV(I), F, F-F107AV(I)
C
                F107AV(I) = F
            ENDDO
        ENDIF
C
C  Get the 10.7 cm fluxes
C
        TEMPJD = DJUL - 1.71D0
        DO WHILE ((TEMPJD .GT. JULF(IFL)) .AND. (IFL .LE. NF))
            IFL = IFL + 1
        ENDDO
C
        IF(IFL .GT. NF) THEN
         call fcecho('Observation date beyond time interval covered by')
         call fcecho('data file containing solar and geophysical data.')
            GEO(1) = 0.
            GEO(2) = 0.
            GEO(3) = 0.
        ELSE
            GEO(1) = (TEMPJD - JULF(IFL-1))*(F107(IFL) - 
     +              F107(IFL-1)) + F107(IFL-1)
C
            DO WHILE (DJUL .GT. JULF(IFA))
                IFA = IFA + 1
            ENDDO
            GEO(2) = (DJUL - JULF(IFA-1))*(F107AV(IFA) - 
     +              F107AV(IFA-1)) + F107AV(IFA-1)
C
C  Get the Kp index
C
            TEMPJD = DJUL - 0.279D0
            DO WHILE (TEMPJD .GT. JULKP(IKP))
                IKP = IKP + 1
            ENDDO
            GEO(3) = (TEMPJD - JULKP(IKP-1))*(RKP(IKP) - 
     +              RKP(IKP-1))/0.125D0 + RKP(IKP-1)
            GEO(3) = GEO(3)/10.
        ENDIF
C
 999    continue
       if ( status .ne. 0 ) then
		call fcerrm(status)
        status = 0
	endif

        RETURN
        END
C-----------------------------------------------------------------------
C
             SUBROUTINE  ATMABS (IFLAG,M,N,SPC1,SPC2)
C
CC  This routine calculates the atmospheric absorption.
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 16:39
C
CA  author : SLS        date:  5-JAN-1988 14:15
CU  update : SLS        date: 23-FEB-1988 10:25
CU  update : SXRB       date:  7-APR-1992 16:38
C
CT  status: not tested
C
C   general description 
CG  This routine calculates the atmospheric absorption and operates on
CG  spectrum SPC1 to yield SPC2.
C
C   call_var.          type I/O description 
CP  IFLAG               I4  I   = 1 Sun to point
CP                              = 2 point to satellite
CP  M                   I4  I   step number along the line of sight
CP  N                   I4      number of bins in spectrum
CP  SPC1                R4  I   input spectrum
CP  SPC2                R4    O output spectrum
C
C   include_block name          description 
CI  CDENS.CMN                   contains atmospheric density data
C
C   routines_called    type     description 
CR  ABS_X               SR      returns the absorption cross sections
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER*4 I, IC, IFLAG, III, M, N
C
        REAL*4 E, SPC1(N), SPC2(N), TAU, XSECT(6)
C
        INCLUDE '../../include/CDENS.CMN'
C
        DO 100 I=1,N
            IF(SPC1(I) .LE. 0.) THEN
C
C  The input spectrum is 0.
C
                SPC2(I) = 0.
            ELSE
C
C  Set the energies, in EV
C
                E = ECEN(I)
C
C  Get the cross sections
C
                CALL ABS_X(E,XSECT)
C
C  Calculate the optical depth
C
                TAU = 0.
                DO 60 IC = 1,6
                    III = IC + IFLAG*6
                    TAU = TAU + XSECT(IC)*DENS(M,III)
   60           ENDDO
C
C  Absorb the spectrum
C
                SPC2(I) = SPC1(I)*EXP(-TAU)
            ENDIF
C
C  End the loop over the spectrum
C
  100   ENDDO
C
        RETURN
        END
      SUBROUTINE DENSDR (A,B,IFLAG,NPATH)
C
CC  This routine drives the collection of space and column densities
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 16:38
C
CA  author : SLS        date: 21-DEC-1987 12:47
CU  update : SLS        date: 21-MAR-1988 10:53
CU  update : SXRB       date:  7-APR-1992 16:34
C
CT  status: not tested
C
C   general description 
CG  This routine calls ADEN and associated routines to evaluate the
CG  space and column densities of the constituents of the atmosphere.
C
C   call_var.          type I/O description 
CP  A                   R4  I/? (IFLAG = 2) not used
CP                              (IFLAG = 1) starting point for the
CP                              integral along the line-of-sight
CP  B                   R4  I/? (IFLAG = 2) ending point for the
CP                              integral along the line-of-sight
CP                              (IFLAG = 1) ending point for the
CP                              integral along the line-of-sight
CP                              if (B-A < 0) do not do the integration
CP  IFLAG               I4  ?/? = 2 integrate both from the Sun to the
CP                                 scatter point and along the line of
CP                                 sight
CP                              = 1 integrate only along the line-of
CP                                 sight
CP  NPATH               I4  ?/? number of calculation steps along the
CP                              line-of-sight
C
C   include_block name          description 
CI  CDENS.CMN                   contains the calculated space and
CI  CPOS.CMN                    contains the position information
CI  CCONST.CMN                   contains useful constants
C
C   routines_called    type     description 
CR  ADEN                SR      supplies atmospheric elemental
CR                              densities
CR  REPOL1              SR      converts from spherical to Cartesian
CR                              coords
CR  GAULEG              SR      supplies Gauss-Legendre parameters
CR  EQUECL              SR      transforms between spherical coord
CR                              systems
CR  QGAUS               SR      evaluates the Gaussian-Legendre integral
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C   ALION       from ADEN, contains elemental densities
C   KONT        for QGAUS, specifies which integral to do
C   SAT         for ADEN, position coordinates of the scatter point
C   SS          from QGAUS, value of the integral
C   W           from GAULEG, weights for the integration
C   X           from GAULEG, distances for the integration
C
        IMPLICIT NONE
C
C  Include blocks
C
        INCLUDE '../../include/CDENS.CMN'
        INCLUDE '../../include/ROSCONST.INC'
        INCLUDE '../../include/CPOS.CMN'
C
C  Declare variables
C
        INTEGER I, IC, IERR
        INTEGER IFLAG, II, KONT, N, NPATH
C
        REAL*4 A, AA, B, BB, AMMW, ALION(6)
        REAL*4 RHO, SS, SSAT(3), TEMP(2), TEMPER, W(50), X(50)
        REAL*8 JD2000, DR, DLG, DLT, R, DSSAT(3)
C
        DATA JD2000 /2451544.5D0/
C
C  Start the calculations
C
        IF(IFLAG .EQ. 1) THEN
C
C  Integrate only along the line-of-sight.  Use A and B for the
C  starting and ending distances from the satellite along the
C  line-of-sight.
C
            IF((B - A) .GT. 0.) THEN
                DO 3 IC=1,6
                    CALL QGAUS(A,B,IC,IFLAG,NPATH,SS)
                    ALION(IC) = SS
                    DENS(1,IC) = SS
    3           ENDDO
            ELSE
                DO 5 IC=1,6
                    ALION(IC) = 0.
                    DENS(1,IC) = 0.
    5           ENDDO
            ENDIF
        ELSE
C
C  Call GAULEG to get the positions and weights for evaluation along
C  the look direction.
C
            IF(B .GT. 0.) THEN
                CALL GAULEG(A,B,NPATH,W,X)
C
C  Loop through all of the points along the look direction
C
                DO 100 I=1,NPATH
                    DENS(I,19) = X(I)
                    DENS(I,20) = W(I)
C
C  Calculate the position of the point
C
                    R = DBLE(X(I))
                    DO 10 II=1,3
                        DXP(II) = DXS(II) + R*DXL(II)
   10               ENDDO
C
C  Convert to spherical (ecliptic) coords
C
                    CALL REPOL1(DXP(1),DXP(2),DXP(3),DR,DLG,DLT)
                    DLT = PIH - DLT
C
C  Transform to celestial coords
C
                    DLG = DLG + SLONG - PI
                    IF(DLG .LT. 0.) DLG = DLG + PI2
                    IF(DLG .GT. PI2) DLG = DLG - PI2
                    CALL EQUECL (JD2000,.TRUE.,.FALSE.,DLG,DLT,
     +                      DSSAT(1),DSSAT(2),IERR)
                    SSAT(1) = DSSAT(1)
                    SSAT(2) = DSSAT(2)
                    TEMPER = 6378.17*SQRT(1. - 6.7E-3*SIN(SSAT(2))**2)
                    SSAT(3) = SNGL(DR) - TEMPER
C
C  Get the space densities at the point
C
                    CALL ADEN(AMJD,SONNE,SSAT,GEO,TEMP,ALION,AMMW,RHO)
C
C  Stuff the densities into the array DENS, convert from # m**-3 to
C  # cm**-2 km**-1
C
                    DO 20 II=1,6
                        DENS(I,II) = 10.**(ALION(II) - 1.0)
   20               ENDDO
C
C  Get the column densities from the Sun to the point and from the
C  point to the satellite
C
                    DO 50 KONT=0,1
                        IF(KONT .EQ. 0) THEN
C
C  If (KONT = 0) set up for the integral from the Sun to the point
C
                            AA = -SQRT((REARTH + HMAX + 10.)**2. -
     +                          SNGL(DXP(2)*DXP(2) + DXP(3)*DXP(3)))
                            BB = SNGL(DXP(1))
                        ELSE
C
C  If (KONT = 1) set up for the integral from the point to the satellite
C
                            AA = 0.
                            BB = X(I)
                        ENDIF
C
C  Set the number of steps
C
                        N = INT((BB - AA)/100. + 0.5)
                        IF(N .LT. 15) N = 15
                        IF(N .GT. 40) N = 40
C
                        DO 40 IC=1,6
C
C  Get the column densities
C
                            CALL QGAUS(AA,BB,IC,KONT,N,SS)
C
C  Stuff the density into the array
C
                            II = 6  + (KONT*6) + IC
                            DENS(I,II) = SS
   40                   ENDDO
C
   50               ENDDO
C
C  Write out the density array
C
C     INTEGER*4 IOS_D
C                   WRITE(25,2000) (DENS(I,II),II=1,20)
C2000               FORMAT(1H0,6(1PE12.4)/1H ,6(1PE12.4)/
C    +                  1H ,6(1PE12.4)/1H ,2(0PF15.5))
C
  100           ENDDO
C
            ENDIF
C
C  Some more diagnostic output
C
            DO 150 IC=1,6
                ALION(IC) = 0.
  150       ENDDO
C
            DO 200 I=1,NPATH
                DO 190 IC=1,6
                    ALION(IC) = ALION(IC) + W(I)*DENS(I,IC)
  190           ENDDO
  200       ENDDO
C
        ENDIF
C
        RETURN
        END
        SUBROUTINE INIT
C
CC  This routine sets up the position and look information for DENS_DR
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 16:50
C
CA  author : SLS        date: 21-DEC-1987 12:47
CU  update : SXRB       date: 7-APR-1992 16:49
C
CT  status: not tested
C
C   general description 
CG  This routine converts the satellite position and look direction
CG  into Cartesian coords for the integrations
C
C   call_var.          type I/O description 
CP                              
C
C   include_block name          description 
CI  CCONST.CMN                   contains useful constants
CI  CPOS.CMN                    contains the position information
C
C   routines_called    type     description 
CR  POLRE1              SR      converts from spherical to Cartesian
CR                              coords
CR  EQUECL                      transforms between ecliptic and
CR                              equatorial coord systems
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C   ALT         altitude of the satellite
C   DXL         Cartesian coords of the unit vector in the look directio
C   DXS         Cartesian coords of the satellite position
C               (there is a rotation so that the Sun is at X = -big)
C   REARTH      radius of the Earth
C   DR          radius vector |  double precision
C
        IMPLICIT NONE
C
C  Include blocks
C
        INCLUDE '../../include/ROSCONST.INC'
        INCLUDE '../../include/CPOS.CMN'
C
C  Declare variables
C
        INTEGER I, IERR
C
        REAL*8 DELALO, DELASA, DELOLO, DELOSA, DR, DUM, 
     +      JD2000, TEMP
C
        DATA JD2000 /2451544.5D0/
C
C  Start the calculations
C
C  First, turn the look direction, satellite position, and sun
C  position into ecliptic coords
C
        CALL EQUECL (JD2000,.TRUE.,.TRUE.,RALO,DECLO,
     +          DELOLO,DELALO,IERR)
        CALL EQUECL (JD2000,.TRUE.,.TRUE.,RASA,DECSA,
     +          DELOSA,DELASA,IERR)
        CALL EQUECL (JD2000,.TRUE.,.TRUE.,RASU,DECSU,
     +          SLO,DUM,IERR)
        SLONG = SLO
C
C  Second, rotate so that the Sun is at -PI (negative infinity
C  on the X-axis).
C
        TEMP = SLO - PI
        DELASA = PIH - DELASA
        DELOSA = DELOSA - TEMP
        IF(DELOSA .LT. 0.) DELOSA = DELOSA + PI2
        IF(DELOSA .GE. PI2) DELOSA = DELOSA - PI2
        DELALO = PIH - DELALO
        DELOLO = DELOLO - TEMP
        IF(DELOLO .LT. 0.) DELOLO = DELOLO + PI2
        IF(DELOLO .GE. PI2) DELOLO = DELOLO - PI2
C
C  Third, convert to Cartesian coords, CARPOL requires REAL*8 input
C
        DR = DBLE(REARTH + ALT)
        CALL POLRE1(DR,DELOSA,DELASA,DXS(1),DXS(2),DXS(3),IERR)
        DR = 1.D0
        CALL POLRE1(DR,DELOLO,DELALO,DXL(1),DXL(2),DXL(3),IERR)
C
C  Protect against divide by 0
C
        DO 10 I=1,3
           IF(DABS(DXS(I)) .LT. 1.D-10) DXS(I) = 1.D-10
           IF(DABS(DXL(I)) .LT. 1.D-10) DXL(I) = 1.D-10
   10   ENDDO
C
        RETURN
        END
       SUBROUTINE WRERR (RTNAME,IERR,TEXT)
C
       INTEGER*4 IERR
       character(8) RTNAME
       character(80) TEXT
C
       RETURN
       END
        SUBROUTINE GAULEG(A,B,N,W,X)
C
CC  Numerical Recipes routine for the calculation of Gauss-Legendre pars
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 17:13
C
CA  author : SLS        date: 21-DEC-1987 12:47
C
CT  status: not tested
C
C   general description 
CG  Given the upper and lower bounds of an integration (A,B) and the
CG  number of integration steps (N), this routine returns the arrays X
CG  and W of length N containing the abscissas and weights of the
CG  Gauss-Legendre N point quadrature formula.  (Numerical Recipes p.
CG  125.)
C
C   call_var.          type I/O description 
CP  A                   R4  I   lower integration limit
CP  B                   R4  I   upper integration limit
CP  N                   I4      number of points
CP  W                   R4    O weight array
CP  X                   R4    O array containing the X positions
C
C   include_block name          description 
CI                              
C
C   routines_called    type     description 
CR                              
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C   EPS         tolerance for the calculation of the parameters
C
        IMPLICIT NONE
C
C  Declare variables
C
        INTEGER I, J, M, N
C
        REAL*4 A, B, W(N), X(N)
        REAL*8 DI, DJ, DN, EPS, P1, P2, P3, PP, XL, XM, Z, Z1
C
        DATA EPS /3.D-14/

        PP = 0.0
        Z1 = 0.0
C
C  Start the calculations
C
C  The roots are symmetric in the interval so only one half need to be
C  found
C
        M = (N + 1)/2
        DN = DBLE(FLOAT(N))
        XM = 0.5D0*DBLE(B + A)
        XL = 0.5D0*DBLE(B - A)
C
C  Loop over the desired roots
C
        DO 20 I=1,M
            DI = DBLE(FLOAT(I))
            Z = DCOS(3.141592654D0*(DI - 0.25D0)/(DN + 0.5D0))
C
C  Starting the the above approximation to the Ith root, enter the
C  main loop of refinement by Newton's method.
C
            DO WHILE (DABS(Z - Z1) .GT. EPS)
                P1 = 1.D0
                P2 = 0.D0
 
C
C  Loop up the recurrance relation to get the Lengdre polynomial
C  evaluated at Z
C
                DO 10 J=1,N
                    DJ = DBLE(FLOAT(J))
                    P3 = P2
                    P2 = P1
                    P1 = ((2.D0*DJ - 1.D0)*Z*P2 -
     +                  (DJ - 1.D0)*P3)/DJ
   10           ENDDO
C
C  P1 is now the desired Legendre polynomial.  Next compute PP,
C  its derivative, by a standard relation involving also P2, the
C  polynomial of one lower order.
C
                PP = DN*(Z*P1 - P2)/(Z*Z - 1.D0)
                Z1 = Z
C
C    Newton's method
C
                Z = Z1 - P1/PP
            ENDDO
C
C  Scale the root to the desired interval
C
            X(I) = XM - XL*Z
C
C  Put in its symmetric counterpart
C
            X(N+1-I) = XM + XL*Z
C
C  Compute the weight
C
            W(I) = 2.D0*XL/((1.D0 - Z*Z)*PP*PP)
C
C  Put in its symmetric counterpart
C
            W(N+1-I) = W(I)
   20   ENDDO
C
        RETURN
        END
        SUBROUTINE FLUOR(E,KONT,SIGMA,IERR)
C
CC  This routine returns the fluorescent scattering cross sections
C
C************************ FFORM VERSION 1.2 ************ 14-APR-92 10:12
C
CA  author : JHS        date:
CU  update : SXRB       date:  7-APR-1992 16:57
C
CT  status: not tested
C
C   general description 
CG  This subroutine determines the cross section for fluorescent
CG  radiation from the photoabsorption cross section.  The cross
CG  section, SIGMA, is in cm**2/sr.  The absorption cross sections
CG  are from Henke.
C
C   call_var.          type I/O description 
CP  E                   R4  I/? input energy in eV
CP  IERR                I4    O = 0 no error
CP                              = 3 illegal value for KONT
CP  KONT                I4  I   1-N2, 2-O2, 3-O, 4-A, 5-He, 6-H
CP  SIGMA               R4    O cross section in cm**2
C
C   include_block name          description 
CI                              
C
C   routines_called    type     description 
CR  NITRO               R4      returns absorption cross section for N
CR  OXY                 R4      returns absorption cross section for O
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER*4 IERR, IERR1, IERR2, II, KONT
C
        REAL*4 E, NITRO, OXY, SCALE, SIGMA
C
        SCALE = 0.

        IERR = 0
        IF((KONT .GT. 6) .OR. (KONT .LT. 1)) THEN
            IERR = 3
        ELSE
C
C  There is a valid value for KONT
C
            SIGMA = 0.
            II = 2
C
C  KONT = 1 : get nitrogen cross section
C
            IF(KONT .EQ. 1) THEN
                SCALE = 0.0047
                IF(E .LT. 401.) THEN
                    SIGMA = 0.
                ELSE
                    SIGMA = 2.*NITRO(E,II,IERR1)
                ENDIF
C
C  KONT = 2 : molecular oxygen
C
            ELSEIF(KONT .EQ. 2) THEN
                SCALE = 0.0077
                IF(E .LE. 532.) THEN
                    SIGMA = 0.
                ELSE
                    SIGMA = 2.*OXY(E,II,IERR2)
                ENDIF
C
C  KONT = 3 : atomic oxygen
C
            ELSEIF (KONT .EQ. 3) THEN
                SCALE = 0.0077
                IF(E .LE. 546.) THEN
                    SIGMA = 0.
                ELSE
                    SIGMA = OXY(E,II,IERR2)
                ENDIF
C
C  KONT = 4 : argon
C
C            ELSEIF(KONT .EQ. 4) THEN
C                SCALE = 0.
C                SIGMA = 0.
C
C  KONT = 5 : helium
C
C            ELSEIF(KONT .EQ. 5) THEN
C                SCALE = 0.
C                SIGMA = 0.
C
C  KONT = 6 : hydrogen
C
C            ELSEIF(KONT .EQ. 6) THEN
C                SCALE = 0.
C                SIGMA = 0.
C
            ENDIF
C
C  Now put it all together and isotropize by 4 PI
C
            SIGMA = SCALE*SIGMA/12.566637
        ENDIF
C
        RETURN
        END
        SUBROUTINE ADEN(AMJD,SONNE,SAT,GEO,TEMP,ALION,AMMW,RHO)
C
CC  This routine does the model atmosphere calculation (CIRA 72)
C
C************************ FFORM VERSION 1.2 ************  8-APR-92 06:59
C
                                               
CU  update : H. H. FINK date:    MAY-1984
CU  update : SLS        date: 15-DEC-1987 13:22
CU  update : SXRB       date:  7-APR-1992 17:14
C
CT  status: not tested
C
C   general description 
CG  This routine calculates the model atmospheric densities of the
CG  reference atmosphere published in CIRA 1972, p. 332.  The
CG  calculation is good for altitudes above 110 km.
C
C   call_var.          type I/O description 
CP  ALION               R4    O log10 of the densities of the elements
CP                              ALION(1) = N2, molecular nitrogen (m**-3
CP                              ALION(2) = O2, molecular oxygen (m**-3)
CP                              ALION(3) = O, atomic oxygen (m**-3)
CP                              ALION(4) = A, argon (m**-3)
CP                              ALION(5) = He, helium (m**-3)
CP                              ALION(6) = H, hydrogen (m**-3)
CP  AMJD                R4  I   date and time in modified Julian days
CP                              and fraction thereof
CP                              (Julian date - 2400000.5)
CP  AMMW                R4    O average atomic weight
CP  GEO                 R4  I   10.7 cm fluxes and geomagnetic index
CP                              GEO(1) = 10.7 cm solar flux 1.71 days
CP                                      before the date
CP                                      (1.E-22 Watts/m**2/Hz)
CP                              GEO(2) = 10.7 cm solar flux averaged
CP                                      over 4 solar rotations centered
CP                                      on the date in question
CP                                      (1.E-22 Watts/m**2/Hz)
CP                              GEO(3) = geomagnetic planetary index,
CP                                      Kp,0.279 days before the date
CP                                      (3- = 2.667, 3o = 3.000,
CP                                      3+ = 3.333, etc.)
CP  RHO                 R4      total mass density (kg m**-3)
CP  SAT                 R4  I   coords of the point to be calculated
CP                              SAT(1) = right ascension  (radians)
CP                              SAT(2) = declination (radians)
CP                              SAT(3) = altitude  (km)
CP  SONNE               R4  I   celesial coordinates of the Sun
CP                              SONNE(1) = right ascension (radians)
CP                              SONNE(2) = declination (radians)
CP  TEMP                R4    O atmospheric temperatures
CP                              TEMP(1) = temp above the point (K)
CP                              TEMP(2) = temperature at the point (K)
C
C   include_block name          description 
CI  CCONST.CMN                   contains useful constants
C
C   routines_called    type     description 
CR  AMBAR               R4      evaluates equation (1) for the model
CR                              atmosphere
CR  GRAV                R4      Evaluates equation (8) in the model
CR                              atmosphere routine
CR  TLOCAL              R4      Evaluates equation (10) or (13) of the
CR                              model atmosphere routine depending on Z
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER I, J, N
C
        REAL*4 AIN, AL, AL10, AL10T5, ALNH5, ALTR, AMBAR1, AMBAR2, AMJD,
     +    AMMW, AN, ANM, ARG, ARG0, AUX, AVOGAD, C, CAPPHI, CONS25, DF,
     +    DLNHE, DLR, DLR20, DLRGM, DLRSA, DLRSL, DTG, DTG18, DTG20,
     +    DZ, ETA, EXPKP, F, FACT1, FACT2, FOFZ, GOFT, GRAVL, GSUBX, H,
     +    HSIGN, R, R1, R2, R3, RHO, RSTAR, S, SSIGN, SUM1, SUM2, SUM3,
     +    SUMN, SUMNM, T500, TAU, THETA, TINF, TLOC1, TLOC2, TLOC3,
     +    TLOC4, TSUBC, TSUBL, TSUBX, Z, Z1, Z2, Z3, Z4, ZEND, ZR
        REAL*4 AMBAR, GRAV, TLOCAL
        REAL*4 ALION(6), ALN(6), ALPHA(5), AMW(6), FRAC(4)
        REAL*4 GEO(3), SAT(3), SONNE(2), TC(4), TEMP(2), WT(5)
C
        INCLUDE '../../include/ROSCONST.INC'
C
C  ALPHA contains the thermal diffusion coefficients of equation (6)
C
        DATA ALPHA /0.0, 0.0, 0.0, 0.0, -0.38/
C
C  AL10 is ALOG(10.)
C
        DATA AL10     /2.3025851/
C
C  AMW contains the molecular weights in the order N2, O2, O, A, HE, H
C
        DATA AMW /28.0134, 31.9988, 15.9994, 39.948, 4.0026, 1.00797/
C
C  AVOGAD is Avogadro's number in MKS units
C
        DATA AVOGAD /6.02257E+26/
        DATA CONS25 /0.35355339/
C
C  FRAC contains the assumed sea-level volume-fractions in the order
C  N2, O2, A, He
C
        DATA FRAC /0.78110, 0.20955, 9.3432E-3, 6.1471E-6/
C
C  RSTAR is the universal gas constant in MKS units
C
        DATA RSTAR /8314.32/
C
C  R1, R2, R3 are step sizes for 90<H<100, 100<H<500, and 500<H
C
        DATA R1, R2, R3 /0.010, 0.025, 0.075/
C
C  WT contains the weights for the Newton-Cotes five-point
C  quadrature formula
C
        DATA WT /0.31111111, 1.4222222, 0.53333333, 1.4222222,
     +      0.31111111/
C
C  INITIALIZE
C
        AMBAR2 = 0.0
        GRAVL  = 0.0
        TLOC1  = 0.0
        TLOC2  = 0.0
        TLOC3  = 0.0
        TLOC4  = 0.0
C
C  Equation (14)
C
        TSUBC = 379.0 + 3.24*GEO(2) + 1.3*(GEO(1) - GEO(2))
C
C  Equation (15)
C
        ETA = 0.5*ABS(SAT(2) - SONNE(2))
        THETA = 0.5*ABS(SAT(2) + SONNE(2))
C
C  Equation (16)
C
        H = SAT(1) - SONNE(1)
        TAU = H - 0.64577182 + 0.10471976*SIN(H + 0.75049158)
C
C  Equation (17)
C
        C = COS(ETA)**2.2
        S = SIN(THETA)**2.2
        DF = S + (C - S)*ABS(COS(0.5*TAU))**3
        TSUBL = TSUBC*(1.0 + 0.3*DF)
C
C  Equation (18)
C
        EXPKP = EXP(GEO(3))
        DTG18 = 28.0*GEO(3) + 0.03*EXPKP
C
C  Equation (20)
C
        DTG20 = 14.0*GEO(3) + 0.02*EXPKP
        DLR20 = 0.012*GEO(3) + 1.2E - 5*EXPKP
C
C  The following statements effect a continuous transition from
C  equation (20) at heights well below 350 km to equation (18)
C  well above 350 km.
C
        F = 0.5*(TANH(0.04*(SAT(3) - 350.0)) + 1.0)
        DLRGM = DLR20*(1.0 - F)
        DTG = DTG20*(1.0 - F) + DTG18*F
        TINF = TSUBL + DTG
        TEMP(1) = TINF
C
C  Equation (9)
C
        TSUBX = 371.6678 + 0.0518806*TINF -
     +      294.3503*EXP(-0.00216222*TINF)
C
C  Equation (11)
C
        GSUBX = 0.05428571*(TSUBX - 183.0)
C
C  The TC array will be an argument in the call to TLOCAL, which
C  evaluates equation (10) or equation (13)
C
        TC(1) = TSUBX
        TC(2) = GSUBX
C
C  A and GSUBX/A of equation (13)
C
        TC(3) = (TINF - TSUBX)/PIH
        TC(4) = GSUBX/TC(3)
C
C  Equation (5)
C
        Z1 = 90.0
        Z2 = AMIN1(SAT(3),100.0)
        AL = ALOG(Z2/Z1)
        N = IFIX(AL/R1) + 1
        ZR = EXP(AL/FLOAT(N))
        AMBAR1 = AMBAR(Z1)
        TLOC1 = TLOCAL(Z1,TC)
        ZEND = Z1
        SUM2 = 0.0
        AIN = AMBAR1*GRAV(Z1)/TLOC1
C
        DO 20 I=1,N
            Z = ZEND
            ZEND = ZR*Z
            DZ = 0.25*(ZEND - Z)
            SUM1 = WT(1)*AIN
            DO 10 J=2,5
                Z = Z + DZ
                AMBAR2 = AMBAR(Z)
                TLOC2 = TLOCAL(Z,TC)
                GRAVL = GRAV(Z)
                AIN = AMBAR2*GRAVL/TLOC2
                SUM1 = SUM1 + WT(J)*AIN
   10       ENDDO
            SUM2=SUM2+DZ*SUM1
   20   ENDDO
C
        FACT1 = 1000.0/RSTAR
        RHO = 3.46E-6*AMBAR2*TLOC1*EXP(-FACT1*SUM2)/AMBAR1/TLOC2
C
C  Equation (2)
C
        ANM = AVOGAD*RHO
        AN = ANM/AMBAR2
C
C  Equation (3)
C
        FACT2 = ANM/28.960
        ALN(1) = ALOG(FRAC(1)*FACT2)
        ALN(4) = ALOG(FRAC(3)*FACT2)
        ALN(5) = ALOG(FRAC(4)*FACT2)
C
C  Equation (4)
C
        ALN(2)=ALOG(FACT2*(1.0+FRAC(2))-AN)
        ALN(3)=ALOG(2.*(AN-FACT2))
        IF(SAT(3) .LE. 100.0) THEN
            TEMP(2) = TLOC2
C
C  Put in negligable hydrogen for use in do-loop 90
C
            ALN(6) = ALN(5) - 25.0
        ELSE
C
C  Equation (6)
C
            Z3 = AMIN1(SAT(3),500.0)
            AL = ALOG(Z3/Z)
            N = IFIX(AL/R2) + 1
            ZR = EXP(AL/FLOAT(N))
            SUM2 = 0.0
            AIN = GRAVL/TLOC2
C
            DO 40 I=1,N
                Z = ZEND
                ZEND = ZR*Z
                DZ = 0.25*(ZEND - Z)
                SUM1 = WT(1)*AIN
                DO 30 J=2,5
                    Z = Z + DZ
                    TLOC3 = TLOCAL(Z,TC)
                    GRAVL = GRAV(Z)
                    AIN = GRAVL/TLOC3
                    SUM1 = SUM1 + WT(J)*AIN
   30           ENDDO
                SUM2 = SUM2 + DZ*SUM1
   40       ENDDO
C
            Z4 = AMAX1(SAT(3),500.0)
            AL = ALOG(Z4/Z)
            R = R2
            IF(SAT(3).GT.500.0) R = R3
            N = IFIX(AL/R) + 1
            ZR = EXP(AL/FLOAT(N))
            SUM3 = 0.0
C
            DO 60 I=1,N
                Z = ZEND
                ZEND = ZR*Z
                DZ = 0.25*(ZEND - Z)
                SUM1 = WT(1)*AIN
                DO 50 J=2,5
                    Z = Z + DZ
                    TLOC4 = TLOCAL(Z,TC)
                    GRAVL = GRAV(Z)
                    AIN = GRAVL/TLOC4
                    SUM1 = SUM1 + WT(J)*AIN
   50           ENDDO
                SUM3 = SUM3 + DZ*SUM1
   60       ENDDO
C
            IF(SAT(3) .LE. 500.0) THEN
                T500 = TLOC4
                TEMP(2) = TLOC3
                ALTR = ALOG(TLOC3/TLOC2)
                FACT2 = FACT1*SUM2
                HSIGN = 1.0
            ELSE
                T500 = TLOC3
                TEMP(2) = TLOC4
                ALTR = ALOG(TLOC4/TLOC2)
                FACT2 = FACT1*(SUM2+SUM3)
                HSIGN = -1.0
            ENDIF
C
            DO 70 I=1,5
                ALN(I) = ALN(I) - (1.0 + ALPHA(1))*ALTR - FACT2*AMW(I)
   70       ENDDO
C
C  Equation (7)
C
            AL10T5 = ALOG10(T500)
            ALNH5 = (5.5*AL10T5 - 39.40)*AL10T5 + 73.13
            ALN(6) = AL10*(ALNH5 + 6.0) + HSIGN*(ALOG(TLOC4/TLOC3) +
     +          FACT1*SUM3*AMW(6))
        ENDIF
C
C  Equation (23)
C
        CAPPHI=AMOD((AMJD-36204.0)/365.2422,1.0)
C
C  Change to avoid illegal exponentiation
C
        AUX = SIN(PI2*CAPPHI + 6.035)
        IF(AUX .LE. 0.) THEN
            AUX = -AUX
            SSIGN = -1.
        ELSE
            SSIGN = 1.
        ENDIF
C
C  Equation (22)
C
        TAU = CAPPHI + 0.09544*(0.5 + SSIGN*0.5*AUX**1.650-0.5)
C
        GOFT = 0.02835 + 0.3817*(1.0 + 0.4671*SIN(PI2*TAU + 4.137))
     +      *SIN(PI4*TAU+4.259)
        FOFZ = (5.876E-7*SAT(3)**2.331 + 0.06328)*EXP(-2.868E-3*SAT(3))
C
C  Equation (21)
C
        DLRSA = FOFZ*GOFT
C
        ARG = 0.0013*(SAT(3) - 90.0)**2
        IF(ARG .LT. 180.) THEN
            ARG0 = EXP(-ARG)
        ELSE
            ARG0 = 0.
        ENDIF
C
C       DLRSL = 0.014*(SAT(3) - 90.0)*EXP(-0.0013*(SAT(3) - 90.0)**2)*
C    +      SIGN(1.0,SAT(2))*SIN(PI2*CAPPHI + 1.72)*SIN(SAT(2))**2
C
C  Equation (24)
C
        DLRSL = 0.014*(SAT(3) - 90.0)*ARG0*SIGN(1.0,SAT(2))*
     +      SIN(PI2*CAPPHI + 1.72)*SIN(SAT(2))**2
C
C  Sum the delta-log-rhos and apply to the number-densities
C
        DLR = AL10*(DLRGM + DLRSA + DLRSL)
C
        DO 80 I=1,6
            ALN(I) = ALN(I) + DLR
   80   ENDDO
C
C  Equation (25)
C
        DLNHE = 0.65*ABS(SONNE(2)/0.4091609)*(SIN(PIF-0.5*SAT(2)*
     +      SIGN(1.0,SONNE(2)))**3-CONS25)
        ALN(5) = ALN(5) + AL10*DLNHE
C
C  Compute the mass-density and mean-molecular-weight and convert
C  number-density logarithms from natural to common
C
        SUMN = 0.0
        SUMNM = 0.0
C
        DO 90 I=1,6
            AN = EXP(ALN(I))
            SUMN = SUMN + AN
            SUMNM = SUMNM + AN*AMW(I)
            ALION(I) = ALN(I)/AL10
   90   ENDDO
C
        AMMW = SUMNM/SUMN
        RHO = SUMNM/AVOGAD
C
        RETURN
        END
      real*8 function DRADRS(DRAD)
C
CC    'Resets' radians to the interval  0 .. 2 * pi
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 16:58
C
CA  author : KOD        date: 19-JUL-1989 18:31
CU  update : KOD        date: 29-MAR-1990 15:08
C
CT  status: tested
C
C   general description 
CG  'Resets' radians to the interval  0  .. 2 * pi.
C
C   call_var.          type I/O description 
CP  DRAD                R8  I   radians (may be negative)
CP  DRADRS              R8      radians inside the interval  0 .. 2 * pi
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
CI  IGENL.INC         parameter block with trig. constants
C
C   routines_called    type     description 
CR  RESVAL              SR      'resets' periodic variables
CR  ERRSTC              SR      continue error stack for one level
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
      INCLUDE '../../include/IGENL.INC'
C
      REAL*8        DRAD, DXMIN, DXMAX, DXOLD, DXNEW
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR , ISTAT
      character(8)   RTNAME
C
      DATA          RTNAME /'DRADRS'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      DXMIN = 0.D0
      DXMAX = D2PI
      DXOLD = DRAD
C
      CALL RESVAL(DXMIN,DXMAX,DXOLD,DXNEW,ISTAT)
C
      if (ISTAT.gt.1) then
        IERR = max(IERR,ISTAT)
        call ERRSTC(RTNAME,10)
      endif
C
      if (IERR.lt.2) DRADRS = DXNEW
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine POLRE1(DR,DPHI,DTHETA,DX,DY,DZ,IERR)
C
CC    Converts polar coordinates into rectangular coordinates
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:09
C
CA  author : KOD        date: 19-JUN-1989 18:31
CU  update : KOD        date: 20-MAR-1990 15:08
C
CT  status: tested
C
C   general description 
CG  Subroutine to convert the polar coordinates (DR,DPHI,DTHETA)
CG  (mathematical notation) into the rectangular coordinates
CG  (DX,DY,DZ).
C
C   call_var.          type I/O description 
CP  DR                  R8  I   radius  (same unit as DX,DY,DZ)
CP  DPHI                R8  I   azimuthal angle  (radians)
CP  DTHETA              R8  I   polar     angle  (radians)
CP
CP  DX                  R8    O x-value (same unit as DX,DY,DZ)
CP  DY                  R8    O y-value (same unit as DX,DY,DZ)
CP  DZ                  R8    O z-value (same unit as DX,DY,DZ)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
CI  IGENL.INC         parameter block with trig. constants
C
C   routines_called    type     description 
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
      INCLUDE '../../include/IGENL.INC'
C
      REAL*8        DR, DPHI, DTHETA, DX, DY, DZ, DST
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR 
      character(8)   RTNAME
C
      DATA          RTNAME /'POLRE1'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR=0
C
      if (DR.lt.0.D0 .or. DPHI.lt.0.D0 .or. DPHI.gt.D2PI .or.
     $    DTHETA.lt.0.D0 .or. DTHETA.gt.DPI) then
        IERR = 1
        call WRERR(RTNAME,IERR,'Error in input parameters')
      endif
C
      DST = dsin(DTHETA)
C
      DX = DR * DST * dcos(DPHI)
      DY = DR * DST * dsin(DPHI)
      DZ = DR * dcos(DTHETA)
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine REPOL1(DX,DY,DZ,DR,DPHI,DTHETA)
C
CC    Converts rectangular coordinates into polar coordinates
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:10
C
CA  author : KOD        date: 15-JUN-1989 18:31
CU  update : KOD        date: 20-MAR-1990 15:09
C
CT  status: tested
C
C   general description 
CG  Subroutine to convert the rectangular coordinates (DX,DY,DZ)
CG  into the polar coordinates (DR,DPHI,DTHETA) - mathematical
CG  notation.
C
C   call_var.          type I/O description 
CP  DX                  R8  I   x-value
CP  DY                  R8  I   y-value
CP  DZ                  R8  I   z-value
CP
CP  DR                  R8    O radius   (same unit as DX,DY,DZ)
CP  DPHI                R8    O azimuthal angle  (radians)
CP  DTHETA              R8    O  polar    angle  (radians)
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
CI  IGENL.INC         parameter block with trig. constants
C
C   routines_called    type     description 
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
      INCLUDE '../../include/IGENL.INC'
C
      REAL*8        DX, DY, DZ, DR, DPHI, DTHETA, DH
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      character(8)   RTNAME
C
      DATA          RTNAME /'REPOL1'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      DR = dsqrt(DX*DX+DY*DY+DZ*DZ)
C
      if (DR.eq.0.0D0) then
        DPHI   = 0.0D0
        DTHETA = 0.0D0
      else
        DTHETA = dacos(DZ/DR)
        DH = dsqrt(DX*DX+DY*DY)
        if (DH.eq.0.0D0) then
          DPHI = 0.0D0
        else
          DPHI = dasin(DY/DH)
          if (DX  .lt. 0.0D0) DPHI = DPI  - DPHI
          if (DPHI.lt. 0.0D0) DPHI = DPHI + D2PI
        endif
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
        SUBROUTINE QGAUS(A,B,IC,KONT,N,SS)
C
CC  Modified Numerical Recipes routine for Gauss-Legendre integration
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 17:12
C
CA  author : SLS        date: 21-DEC-1987 12:47
CU  update : SXRB       date: 7-APR-1992 17:12
C
CT  status: not tested
C
C   general description 
CG  This routine returns as SS the integral of the function FUNC
CG  between the limits A and B, by N-point Gaussian-Legendre
CG  integration.  The function is evaluated exactly N times at
CG  interior points in the range of integration.  This is a modified
CG  routine out of Numerical Recipes
C
C   call_var.          type I/O description 
CP  A                   R4  ?/? lower integration limit
CP  B                   R4  ?/? upper integration limit
CP  IC                  I4  ?/? call number to the routine
CP  KONT                I4  ?/? = 0 integrate from Sun to scatter point
CP                               1 integrate from scatter point to ROSAT
CP  N                   I4  ?/? number of points
CP  SS                  R4    O integral value
C
C   include_block name          description 
CI                              
C
C   routines_called    type     description 
CR  FUNCT               SR      drives ADEN to derive the atmospheric
CR                              densities
CR  GAULEG              SR      supplies Gauss-Legendre parameters
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C   FUNC        from FUNCT, array containing values for the integration
C   W           from GAULEG, weights for the integration
C   X           from GAULEG, positions for the integration
C
        IMPLICIT NONE
C
C  Declare variables
C
        INTEGER I, IC, KONT, N
C
        REAL*4 A, B, FUNC(50), SS, W(50), X(50)
C
C  Start the calculations
C
        IF(IC .EQ. 1) THEN
C
C  This is the first time through so get the Gauss-Legendre parameters
C
            CALL GAULEG(A,B,N,W,X)
        ENDIF
C
C  Now get the function values
C
        CALL FUNCT(IC,KONT,N,X,FUNC)
C
C  Set up the summation
C
        SS = 0.
C
C  Do the summation
C
        DO 10 I=1,N
            SS = SS + W(I)*FUNC(I)
   10   ENDDO
C
        RETURN
        END
        SUBROUTINE ABS_X(E,XSECT)
C
CC  This routine returns the cross sections for atmospheric absorption.
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 17:14
C
CA  author : SLS        date:  5-JAN-1988 14:15
CU  update : SLS        date: 18-APR-1988 10:02
CU  update : SXRB       date: 7-APR-1992 17:13
C
CT  status: not tested
C
C   general description 
CG  This routine returns the cross sections for atmospheric absorption
CG  and is used with ADEN.  It calls functions to get the cross sections
CG  for argon, hydrogen, helium, nitrogen, and oxygen.  The cross
CG  section for oxygen is doubled for O2.
C
C   call_var.          type I/O description 
CP  E                   R4      energy in eV
CP  XSECT               R4    O cross section in cm**-2
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR  ARGON               R4      returns argon cross section
CR  HELIUM              R4      returns helium cross section
CR  HYDRO               R4      returns hydrogen cross section
CR  NITRO               R4      returns nitrogen cross section
CR  OXY                 R4      returns oxygen cross section
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER*4 IERR1, IERR2, IERR3, IERR4, IERR5, IFLAG
C
        REAL*4 ARGON, E, HELIUM, HYDRO, NITRO, OXY, XSECT(6)
C
        IFLAG = 2
C
C  Cross section for N2
C
        XSECT(1) = 2.*NITRO(E,IFLAG,IERR1)
C
C  Get the cross section for oxygen
C
        XSECT(3) = OXY(E,IFLAG,IERR2)
C
C  Double the cross section for O2
C
        XSECT(2) = 2.*XSECT(3)
C
C  Get the cross section for argon
C
        XSECT(4) = ARGON(E,IFLAG,IERR3)
C
C  Get the cross section for helium
C
        XSECT(5) = HELIUM(E,IFLAG,IERR4)
C
C  Get the cross section for hydrogen
C
        XSECT(6) = HYDRO(E,IFLAG,IERR5)
C
        RETURN
        END
      subroutine CNVFRC(LPLUS,IINT,IM,SEC,DDMS,IERR)
C
CC    Converts minutes and seconds into a decimal fraction
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 16:54
C
CA  author : KOD        date: 19-JUL-1989 18:31
CU  update : KOD        date: 20-MAR-1990 14:41
C
CT  status: tested
C
C   general description 
CG  Subroutine to convert minutes and seconds into a decimal fraction
CG  of degrees (or hours) and to append it to the degree (or hour)
CG  value.
CG  The sign has to be given explicitely; the values for IINT,IM,SEC
CG  should be positive, possible signs there will be ignored.
C
C   call_var.          type I/O description 
CP  LPLUS               L4  I   sign (LPLUS=.true. for positive values)
CP  IINT                I4  I   integer part (degrees or hours)
CP  IM                  I4  I   minutes
CP  SEC                 R4  I   seconds
CP
CP  DDMS                R8    O decimal value (degrees or hours)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DDMS, DM, DS, DH
C     REAL*4        HFLAG
      REAL*4        SEC
C     INTEGER*4     IOS
      INTEGER*4     IERR , IINT, IM
      LOGICAL*4     LPLUS
      character(8)   RTNAME
C
      DATA          RTNAME /'CNVFRC'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      DM = dabs(dble(IM))
      DS = dabs(dble(SEC))
C
      if (dmax1(DM,DS).ge.60.D0) then
C
        IERR = 2
        call WRERR(RTNAME,IERR,'Error in input parameters DM,DS')
C
      else
C
         DH = dabs(dble(IINT)) + ( DM + DS/60.D0 ) / 60.D0
C
         if (LPLUS) then
            DDMS =  DH
         else
            DDMS = -DH
         endif
C
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine JDCAL1(DJD,IY,IM,ID,IHR,IMT,SEC,IERR)
C
CC    Calculates the Calendar Date out of the Julian Date
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:04
C
CA  author : KOD        date: 19-JUL-1989 18:31
CU  update : KOD        date: 29-MAR-1990 15:36
C
CT  status: tested (08-AUG-1989 17:45 / KOD)
C
C   general description 
CG  Subroutine to calculate the Calendar Date out of the Julian Date.
CG
CG  The calendar date will be given in the Gregorian system for
CG  dates starting with October 15, 1582, and in the Julian system
CG  until October 4, 1582  (which precedes the Oct.15 immediately).
CG
CG  This subroutine is valid for the years  -4712  to  +3500 ,
CG  (cf. 'Sky and Telescope, May 1984, p.455) and has been written
CG  according to
CG     Jean Meeus, 'Astronomical Formulae for Calculators',
CG                 third edition, 1985, p.26 .
CG  For negative years only the variable IY will be negative.
C
C   call_var.          type I/O description 
CP  DJD                 R8  I   Julian Date
CP
CP  IY                  I4    O year         (-4712 .. +3500)
CP  IM                  I4    O month        (1 .. 12)
CP  ID                  I4    O day of month (1 .. 28,29,30,31)
CP  IHR                 I4    O hours        (0 .. 23)
CP  IMT                 I4    O minutes      (0 .. 59)
CP  SEC                 R4    O seconds      (0.0 .. 59.999...)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  CNVDMS              SR      splits fractional part
CR  WRERR               SR      formats error code in standard form
CR  ERRSTC              SR      continue error stack for one level
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DJD, DH, DF
C     REAL*4        HFLAG
      REAL*4        SEC, SEC1
C     INTEGER*4     IOS
      INTEGER*4     IERR , IY, IM, ID, IHR, IMT, JD, IA, IB, IC,
     &              IG, IE, IHR1, IMT1, ISTAT
      LOGICAL       LPLUS
      character(8)   RTNAME
C
      DATA          RTNAME /'JDCAL1 '/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      if (DJD.lt.0.D0 .or. DJD.gt.2999773.5D0) then
        IERR = 2
        call WRERR(RTNAME,IERR,'Error in the input parameter DJD')
      endif
C
      if (IERR.lt.2) then
        DH = DJD + 0.5D0
        JD = idint(DH)
        DF = DH - dble(JD)
C
        if (JD.lt.2299161) then
          IA = JD
        else
          IA = idint( (dble(JD)-1867216.25D0) / 36524.25D0 )
          IA = JD + 1 + IA - IA/4
        endif
C
        IB = IA + 1524
        IC = idint( (dble(IB)-122.1D0) / 365.25D0 )
        IG = idint( 365.25D0 * dble(IC) )
        IE = idint( dble(IB-IG) / 30.6001D0 )
C
        ID = IB - IG - int( 30.6004 * real(IE) )
C
        if (IE.le.13) then
          IM = IE - 1
        else
          IM = IE - 13
        endif
C
        if (IM.ge.3) then
          IY = IC - 4716
        else
          IY = IC - 4715
        endif
C
        DF = DF * 24.D0
        LPLUS = .TRUE.
        call CNVDMS(DF,LPLUS,IHR1,IMT1,SEC1,ISTAT)
C
        if (ISTAT.gt.1) then
          IERR = max(IERR,ISTAT)
          call ERRSTC(RTNAME,10)
        else
          IHR = IHR1
          IMT = IMT1
          SEC = SEC1
        endif
C
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine EQUECL(DJD,LMEAN,LDIR,DLOLD,DBOLD,DLNEW,DBNEW,IERR)
C
CC    Converts coordinates between equatorial and ecliptical system
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:02
C
CA  author : KOD        date: 19-JUL-1989 18:31
CU  update : KOD        date: 29-MAR-1990 15:13
C
CT  status: tested
C
C   general description 
CG  Subroutine to convert coordinates between the equatorial and the
CG  ecliptical system (LII,BII).
CG
CG  To convert the TRUE positions, use the MEAN obliquity of the
CG  ecliptic (normal case);
CG  for the APPARENT positions (affected by aberration and nutation)
CG  use the TRUE obliquity.
CG  If the equatorial coordinates refer to a standard equinox
CG  (1950, 2000, ...), the obliquity of the ecliptic for this
CG  epoch should be used.
CG
CG  (Jean Meeus, 'Astronomical Formulae for Calculators',
CG                third edition, 1985, p.43-44)
C
C   call_var.          type I/O description 
CP  DJD                 R8  I   Julian Day for the conversion
CP
CP  LMEAN               L4  I   mean or true obliquity of the ecliptic:
CP                                LMEAN = .true.  : mean obliquity
CP                                LMEAN = .false. : true obliquity
CP
CP  LDIR                L4  I   transformation direction:
CP                                LDIR = .true.  : equ --> ecl
CP                                LDIR = .false. : equ <-- ecl
CP
CP  DLOLD               R8  I   old longitude  (radians)
CP  DBOLD               R8  I   old latitude   (radians)
CP
CP  DLNEW               R8    O new longitude  (radians)
CP  DBNEW               R8    O new latitude   (radians)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
CI  IGENL.INC         parameter block with trig. constants
C
C   routines_called    type     description 
CR  EULER1              SR      general coordinate transformation
CR  JDEPS1              SR      calculates mean/true obl. of the ecl.
CR  WRERR               SR      formats error code in standard form
CR  ERRSTC              SR      continue error stack for one level
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
      INCLUDE '../../include/IGENL.INC'
C
      REAL*8        DJD, DLOLD, DBOLD, DLNEW, DBNEW, D1, D2, D3,
     &              DEPS, DPHI1, DTHETA1, DPHI2, DTHETA2, DJ
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR , ISTAT
      LOGICAL*4     LMEAN, LDIR, LMN
      character(8)   RTNAME
C
      DATA          RTNAME /'EQUECL'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
C
      if (DLOLD.lt.0.D0 .or. DLOLD.GT.D2PI) then
        IERR = 1
        call WRERR(RTNAME,IERR,'Warning: unusual value for DLOLD')
      endif
C
      if (dabs(DBOLD).gt.DPID2) then
        IERR = max(IERR,2)
        call WRERR(RTNAME,IERR,'Error in input parameter DBOLD')
      endif
C
      if (DJD.lt.0.D0 .or. DJD.gt.3.D6) then
        IERR = max(IERR,2)
        call WRERR(RTNAME,IERR,'Error in the input parameter DJD')
      endif
C
C
      if  (IERR.lt.2)  then
C
        DJ = DJD
        LMN = LMEAN
        call JDEPS1(DJ,LMN,DEPS,ISTAT)
C
        if (ISTAT.gt.1) then
          IERR = max(IERR,ISTAT)
          call ERRSTC(RTNAME,10)
        endif
C
      endif
C
C
      if  (IERR.lt.2)  then
C
        D1 = 0.D0
        D2 = DEPS
        D3 = 0.D0
C
        DPHI1   = DLOLD
        DTHETA1 = DPID2 - DBOLD
C
        if (LDIR) then
          call EULER1(D1,D2,D3,DPHI1,DTHETA1,DPHI2,DTHETA2,ISTAT)
        else
          call EULER1(-D3,-D2,-D1,DPHI1,DTHETA1,DPHI2,DTHETA2,ISTAT)
        endif
C
        if (ISTAT.gt.1) then
          IERR = max(IERR,ISTAT)
          call ERRSTC(RTNAME,20)
        else
          DLNEW = DPHI2
          DBNEW = DPID2 - DTHETA2
        endif
C
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine CVEQUI(DJD1,DJD2,DZETA,DZ,DTHETA,IERR)
C
CC    Calculates quantities needed for equinox conversions
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 16:56
C
CA  author : KOD        date: 26-SEP-1989 15:09
CU  update : KOD        date: 20-MAR-1990 14:44
C
CT  status: tested
C
C   general description 
CG  Subroutine to calculate the quantities needed for the accurate
CG  reduction of positions from one equinox to another,
CG  according to
CG     Jean Meeus, 'Astronomical Formulae for Calculators',
CG                 third edition, 1985, p.65 .
C
C   call_var.          type I/O description 
CP  DJD1                R8  I   Julian Date for the initial epoch
CP  DJD2                R8  I   Julian Date for the  final  epoch
CP
CP  DZETA               R8    O 'zeta'   (radians)
CP  DZ                  R8    O 'z'      (radians)
CP  DTHETA              R8    O 'theta'  (radians)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DJD1, DJD2, DZETA, DZ, DTHETA, DJD0, DYEAR,
     &              DT0, DT, DH, DF
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR 
      character(8)   RTNAME
C
      PARAMETER ( DJD0  = 2415020.313D0  )
      PARAMETER ( DYEAR =  36524.2199D0  )
      PARAMETER ( DF    = 4.848136811D-6 )
C
      DATA          RTNAME /'CVEQUI'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      if (min(DJD1,DJD2).lt.0.D0 .or. max(DJD1,DJD2).gt.3.D6) then
        IERR = max(IERR,2)
        call WRERR(RTNAME,IERR,'Error in the input parameter DJD')
      endif
C
      if (IERR.lt.2) then
C
        DT0 = ( DJD1 - DJD0 ) / DYEAR
        DT  = ( DJD2 - DJD1 ) / DYEAR
C
        DH = 2304.250D0 + 1.396D0 * DT0
        DZETA = ( DH + ( 0.302D0 + 0.018D0 * DT ) * DT ) * DT
C
        DZ = DZETA + ( 0.791D0 + 0.001D0 * DT ) * DT * DT
C
        DH = 2004.682D0 - 0.853D0 * DT0
        DTHETA = ( DH - ( 0.426D0 + 0.042D0 * DT ) * DT ) * DT
C
        DZETA  = DZETA  * DF
        DZ     = DZ     * DF
        DTHETA = DTHETA * DF
C
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
        SUBROUTINE STREU(E,KONT,CSCATW,SGSCAT,IERR)
C
CC  This routine returns the elastic scattering cross section
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 16:56
C
CA  author : H. H. Fink date: 24-MAY-1984
CU  update : JHS        date: 19-NOV-1985 15:52
CU  update : SLS        date: 21-MAR-1988 11:10
CU  update : SXRB       date: 7-APR-1992 16:53
C
CT  status: not tested
C
C   general description 
CG  This routine calculates the elastic scattering cross section for a
CG  given energy, E, (E such that the wavelength is > 10 A) and a given
CG  scattering angle cosine, CSCATW, for the constituents of the upper
CG  atmosphere.  The cross section is in cm**2/sr.
C
C   call_var.          type I/O description 
CP  CSCATW              R4  I   cosine of the scattering angle
CP  E                   R4  I   energy of the scattered X-ray
CP  KONT                I4  I   1-N2, 2-O2, 3-O, 4-A, 5-He, 6-H
CP  SGSCAT              R4    O scattering cross section
CP  IERR                I4    O 0 = no error
CP                              1 = wavelength less than 10 A
CP                              3 = illegal value for KONT
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR  NBIN                I4      
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C   ALAM        wavelength of the X-ray
C   ARG         scattering angle correction factor
C
        IMPLICIT NONE
C
        INTEGER*4 IERR, J, KONT, N, NBIN
C
        REAL*4 ALAM, AO, AP, ARG, AU, CSCATW, E, F, FA(6,11), FAK
        REAL*4 FE(6,11), F0(6), F11(6), SCT
        REAL*4 SF, SF0(6), SF11(6), SGSCAT, VOR, W(11), WM, WO, WU
C
        DATA (FA(1,J),J=1,11)
     +      /7.0, 5.8, 4.2, 3.0, 2.3, 1.9, 1.65, 1.55, 1.5, 1.4, 1.3/
        DATA (FA(2,J),J=1,11)
     +      /8.0, 7.1, 5.3, 3.9, 2.9, 2.2, 1.8, 1.6, 1.5, 1.4, 1.35/
        DATA (FA(3,J),J=1,11)
     +      /8.0, 7.1, 5.3, 3.9, 2.9, 2.2, 1.8, 1.6, 1.5, 1.4, 1.35/
        DATA (FA(4,J),J=1,11)
     +      /18.0, 15.9, 12.6, 10.4, 8.7, 7.8, 7.0, 6.3, 5.4, 4.7, 4.1/
        DATA (FA(5,J),J=1,11)
     +      /2.0, 1.88, 1.46, 1.05, 0.75, 0.52, 0.35, 0.24, 0.18,
     +      0.14, 0.11/
        DATA (FA(6,J),J=1,11)
     +      /1.0, 0.81, 0.48, 0.25, 0.13, 0.07, 0.04, 0.03, 0.02,
     +      0.01, 0.0/
C
        DATA (FE(1,J),J=1,11)
     +      / 7.0,  5.2,  3.3, 1.9, 1.7, 1.4, 1.2, 1.0, 0.9, 0.9, 0.8/
        DATA (FE(2,J),J=1,11)
     +      / 8.0,  6.4,  3.9, 2.4, 2.0, 1.6, 1.5, 1.3, 1.2, 1.1, 0.9/
        DATA (FE(3,J),J=1,11)
     +      / 8.0,  6.4,  3.9, 2.4, 2.0, 1.6, 1.5, 1.3, 1.2, 1.1, 0.9/
        DATA (FE(4,J),J=1,11)
     +      /18.0, 14.2, 10.3, 8.2, 6.9, 5.8, 4.8, 4.0, 3.4, 2.8, 2.4/
        DATA (FE(5,J),J=1,11)
     +      / 2.0, 1.76, 1.06,0.55,0.28,0.14,0.06,0.03,0.02,0.01,0.01/
        DATA (FE(6,J),J=1,11)
     +      / 1.0, 0.66, 0.23,0.06,0.02, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
        DATA F0  /7., 8., 8., 18., 2., 1./
        DATA SF0 /7., 8., 8., 18., 2., 1./
        DATA F11 /1.15, 1.25, 1.25, 3.6, 0.09, 0.0/
        DATA SF11 /0.6, 0.8, 0.8, 2.2, 0.0, 0.0/
        DATA W /0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0/
C
C  Define the function AP
C
        AP(WO,WU,WM,AO,AU) = AO + (WO - WM)*(AU - AO)/(WO - WU)
C
        IERR = 0
C
        ALAM = 12396./E
C
        IF((KONT .LT. 1) .OR. (KONT .GT. 6)) THEN
C
C  Illegal value for KONT
C
            IERR = 3
        ELSE
C
            VOR = 3.29E-26
            SCT = 1. + CSCATW*CSCATW
C
            IF(CSCATW .EQ. 1.) THEN
                F = F0(KONT)
                SF = SF0(KONT)
            ELSE
                ARG = SQRT((1. - CSCATW)*0.5)/ALAM
                IF(ARG .GT. 1.) THEN
                    F = F11(KONT)
                    SF = SF11(KONT)
                ELSE
C
                    N = NBIN(ARG,W,11)
                    F = AP(W(N),W(N-1),ARG,FA(KONT,N),
     +                  FA(KONT,N-1))
                    SF = AP(W(N),W(N-1),ARG,FE(KONT,N),
     +                  FE(KONT,N-1))
                ENDIF
            ENDIF
C
            FAK = F*F + F0(KONT) - SF
            IF(KONT .LT. 3) FAK = FAK*2.
            SGSCAT = VOR*FAK*SCT
C
        ENDIF
C
        RETURN
        END
        REAL FUNCTION OXY(E,IFLAG,IERR)
C
CC  This routine returns the absorption cross section for oxygen
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 17:06
C
CU  author : SLS        date: 11-JAN-1988 08:31
CU  update : SXRB       date: 7-APR-1992 17:05
C
CT  status: not tested
C
C   general description 
CG  This routine returns the absorption cross section for oxygen for
CG  the given energy E.  The source for the data is Atomic and Nuclear
CG  Data Tables, January 1982.  The energy is given in eV and the cross
CG  section is returned in cm**2.
C
C   call_var.          type I/O description 
CP  E                   R4  I   energy in eV
CP  IERR                I4    O = 0 no error
CP                              = 1 energy out of range, returns OXY=0.
CP  IFLAG               I4  I   = 1 cross section in cm**2/g
CP                              = 2 cross section in cm**2/atom
CP  OXY                 R4      absorption cross section in cm**2
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
C
        IMPLICIT NONE
C
        INTEGER*4 IERR, IFLAG
        REAL*4 E
C
        IERR = 0
        IF((E .LT. 10.) .OR. (E .GT. 10000.)) THEN
            IERR = 1
            OXY = 0.
        ELSE
C
C  Shorten the search, is the energy above or below the edge
C
            IF(E .LE. 531.7) THEN
C
C  The energy is below the edge
C
                IF(E .LE. 72.4) THEN
                    OXY = EXP(5.109191 + (5.015504 -
     +                  0.809852*ALOG(E))*ALOG(E))
                ELSE
C
                    IF (E .LE. 108.5) THEN
                        OXY = EXP(7.088210 + (3.973261 -
     +                      0.6744102*ALOG(E))*ALOG(E))
                    ELSE
C
                        IF(E .LE. 183.3) THEN
                            OXY = EXP(15.98324 + (-0.0275407 -
     +                          0.225682*ALOG(E))*ALOG(E))
                        ELSE
C
                            OXY = EXP(20.31263 + (-1.655993 -
     +                          0.0726184*ALOG(E))*ALOG(E))
                        ENDIF
                    ENDIF
                ENDIF
            ELSE
C
C  The energy is above the edge
C
                IF(E .LE. 1041.) THEN
                    OXY = EXP(17.02914 + (0.1351656 -
     +                  0.1996027*ALOG(E))*ALOG(E))
                ELSE
C
                    IF(E .LE. 2984.3) THEN
                        OXY = EXP(21.70445 + (-1.187325 -
     +                      0.1061099*ALOG(E))*ALOG(E))
                    ELSE
C
                        OXY = EXP(22.42236 + (-1.315738 -
     +                      0.1012737*ALOG(E))*ALOG(E))
                    ENDIF
                ENDIF
            ENDIF
C
            IF(IFLAG .EQ. 2) THEN
C
C  Convert to cm**-2/atom
C
                OXY = OXY*2.65685E-23
            ENDIF
C
        ENDIF
C
        RETURN
        END
        REAL FUNCTION ARGON(E,IFLAG,IERR)
C
CC  This routine returns the absorption cross section for argon
C
C************************ FFORM VERSION 1.0 ************ 14-JAN-88 11:26
C
CA  author : SLS               date: 12-JAN-1988 15:38
CU  update : SXRB       date: 7-APR-1992 17:10  
C
CT  status: not tested
C
C   general description 
CG  This routine returns the absorption cross section for argon for
CG  the given energy E.  The source for the data is Atomic and Nuclear
CG  Data Tables, January 1982.  The energy is given in eV and the cross
CG  section is returned in cm**2.
C
C   call_var.          type I/O description 
CP  E                   R4  I   energy in eV
CP  IERR                I4  I/O = 0 no error
CP                              = 1 energy out of range
CP  IFLAG               I4  I   = 1 cross section in cm**2/g
CP                              = 2 cross section in cm**2/atom
CP  ARGON               R4    O absorption cross section in cm**2
C
C   include_block name          description 
CI  R$COMMON:CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
CR  WRERR               SR      writes to ERROR output stream
C
C   extensions/system calls     description 
CX  MTH$ALOG                    
CX  MTH$EXP                     
C
C***********************************************************************
C
C   variables   meaning
C
C
        IMPLICIT NONE
C
        INTEGER*4 IERR, IFLAG
C
        REAL*4 E
C
        IERR = 0
        IF((E .LT. 10.) .OR. (E .GT. 10000.)) THEN
            IERR = 1
            ARGON = 0.
        ELSE
C
C  Shorten the search, check if the energy is below the L edge
C
            IF(E .LE. 245.) THEN
C
C  The energy is below the L edge
C  Energy is less than 49.3 eV
C
                IF(E .LE. 49.3) THEN
                    ARGON = 6.447E16 * (E ** (-1.*7.5377))
C
C  Energy is less than 95.3 eV
C
                ELSEIF (E .LE. 95.3) THEN
                    ARGON = EXP(-44.25655 + (24.79339 -
     +                  2.834183*ALOG(E))*ALOG(E))
C
C  Energy is less than 151.7 eV
C
                ELSEIF(E .LE. 151.) THEN
                    ARGON = EXP(-10.95178 + (9.565466 -
     +                  1.096330*ALOG(E))*ALOG(E))
C
C  Energy is less than 245.0 eV
C
                ELSE
                    ARGON = EXP(10.34494 + (1.125267 -
     +                  0.2601366*ALOG(E))*ALOG(E))
                ENDIF
            ELSEIF (E .LE. 1740.) THEN
C
C  The energy is above the L edge
C  Energy is less than 524.9 eV
C
                IF(E .LE. 524.9) THEN
                    ARGON = EXP(-24.78518 + (13.43732 -
     +                  1.266509*ALOG(E))*ALOG(E))
C
C  Energy is less than 705.0 eV
C
                ELSEIF(E .LE. 705.0) THEN
                    ARGON = EXP(10.32483 + (2.033633 -
     +                  0.3408063*ALOG(E))*ALOG(E))
C
C  Energy is less than 1011.7 eV
C
                ELSEIF(E .LE. 1011.7) THEN
                    ARGON = EXP(15.67738 + (0.4402372 -
     +                  0.2221846*ALOG(E))*ALOG(E))
C
C  Energy is less than 1740.0 eV
C
                ELSE
                    ARGON = EXP(28.17977 + (-3.123039 +
     +                  0.031713378*ALOG(E))*ALOG(E))
                ENDIF
            ELSE
C
C  Energy is less than 3202.9 eV
C
                IF(E .LT. 3202.9) THEN
                    ARGON = EXP(14.56680 + (0.4723388 -
     +                  0.2056220*ALOG(E))*ALOG(E))
C
C  Energy is less than 4466.3 eV
C
                ELSEIF(E .LT. 4466.3) THEN
                    ARGON = EXP(28.31554 - 2.609014*ALOG(E))
C
C  Energy is less than 5898.8 eV
C
                ELSEIF(E .LT. 5898.8) THEN
                    ARGON = EXP(13.76291 + (0.8990408 -
     +                  0.2113705*ALOG(E))*ALOG(E))
C
C  Energy is less than 10000 eV
C
                ELSE
                    ARGON = EXP(21.53926 + (-0.9151472 -
     +                  0.1055789*ALOG(E))*ALOG(E))
                ENDIF
            ENDIF
C
            IF(IFLAG .EQ. 2) THEN
C
C  Convert to cm**-2/atom
C
                ARGON = ARGON*6.6338E-23
            ENDIF
C
        ENDIF
C
        RETURN
        END
      subroutine RESVAL(DXMIN,DXMAX,DXOLD,DXNEW,IERR)
C
CC    'Resets' periodic variables to their basic interval
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:10
C
CA  author : KOD        date: 19-JUL-1989 18:31
CU  update : KOD        date: 20-MAR-1990 15:10
C
CT  status: tested
C
C   general description 
CG  Subroutine to 'reset' the values of periodically defined
CG  variables to their basic interval.
C
C   call_var.          type I/O description 
CP  DXMIN               R8  I   left  boundary of basic interval
CP  DXMAX               R8  I   right boundary of basic interval
CP
CP  DXOLD               R8  I   value before resetting
CP  DXNEW               R8    O value after  resetting
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DXMIN, DXMAX, DXOLD, DXNEW, DDIFF, DH
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR 
      character(8)   RTNAME
C
      DATA          RTNAME /'RESVAL'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      if (DXMIN.ge.DXMAX) then
        IERR = 2
        call WRERR(RTNAME,IERR,'Error in input parameters DXMIN,DXMAX')
      endif
C
      if (IERR.lt.2) then
        DDIFF = DXMAX - DXMIN
        DH = (DXOLD-DXMIN) / DDIFF
        DH = DH - DINT(DH)
        if (DH.lt.0.D0) DH = DH + 1.D0
        DXNEW = DXMIN + DH * DDIFF
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
        REAL FUNCTION HELIUM(E,IFLAG,IERR)
C
CC  This routine returns the absorption cross section for helium
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 17:09
C
CA  author : SLS        date: 12-JAN-1988 15:45
CU  update : SXRB       date: 7-APR-1992 17:09
C
CT  status: not tested
C
C   general description 
CG  This routine returns the absorption cross section for helium for
CG  the given energy E.  The source for the data is Atomic and Nuclear
CG  Data Tables, January 1982.  The energy is given in eV and the cross
CG  section is returned in cm**2.
C
C   call_var.          type I/O description 
CP  E                   R4  I   energy in eV
CP  IERR                I4    O = 0 no error
CP                              = 1 energy out of range
CP  IFLAG               I4  I   = 1 cross section in cm**2/g
CP                              = 2 cross section in cm**2/atom
CP  HELIUM              R4      absorption cross section in cm**2
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
C
        IMPLICIT NONE
C
        INTEGER*4 IERR, IFLAG
C
        REAL*4 E
C
        IERR = 0
        IF((E .LT. 10.) .OR. (E .GT. 10000.)) THEN
            IERR = 1
            HELIUM = 0.
        ELSE
C
            IF(E .LE. 108.5) THEN
                HELIUM = EXP(18.04381 + (-0.6154932 -
     +              0.2052815*ALOG(E))*ALOG(E))
            ELSE
C
                IF(E .LE. 524.9) THEN
                    HELIUM = EXP(16.29060 + (0.046130 -
     +                  0.2666275*ALOG(E))*ALOG(E))
                ELSE
C
                    IF(E .LE. 1740.0) THEN
                        HELIUM = EXP(25.61902 + (-3.052529 -
     +                      0.0096876*ALOG(E))*ALOG(E))
                    ELSE
C
                        IF(E .LE. 4466.3) THEN
                            HELIUM = EXP(22.08981 + (-1.943097 -
     +                          0.09498362*ALOG(E))*ALOG(E))
                        ELSE
C
                            HELIUM = EXP(30.46602 + (-4.063684 +
     +                          0.038733*ALOG(E))*ALOG(E))
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
C
            IF(IFLAG .EQ. 2) THEN
C
C  Convert to cm**2
C
                HELIUM = HELIUM*6.64711E-24
            ENDIF
C
        ENDIF
C
        END
      subroutine EULER1(D1,D2,D3,DPH1,DTH1,DPH2,DTH2,IERR)
C
CC    General transformation of spherical coordinates
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:03
C
CA  author : KOD        date: 15-JUL-1989 18:31
CU  update : KOD        date: 20-MAR-1990 14:57
C
CT  status: tested
C
C   general description 
CG
CG  Subroutine to convert spherical coordinates between coordinate
CG  systems which are rotated with respect to each other.
CG
CG  This subroutine converts the azimuthal and polar angles
CG  (DPH1,DTH1), given in mathematical notation and in radians,
CG  into the corresponding angles with respect to a coordinate
CG  system which orientation is given by the EULER angles (D1,D2,D3)
CG  in the following way:
CG
CG  The original cartesian coordinate system (X0,Y0,Z0) is rotated
CG  in three steps:
CG
CGT   1) rotation around the Z0-axis by the angle D1  --->  (X1,Y1,Z1)
CGT   2) rotation around the X1-axis by the angle D2  --->  (X2,Y2,Z2)
CGT   3) rotation around the Z2-axis by the angle D3  --->  (X3,Y3,Z3)
CG
CG  The calculation is accelerated by using differences of angles.
C
C   call_var.          type I/O description 
CP  D1                  R8  I   EULER angle 1   (radians, -2 pi .. 2 pi)
CP  D2                  R8  I   EULER angle 2   (radians, -2 pi .. 2 pi)
CP  D3                  R8  I   EULER angle 3   (radians, -2 pi .. 2 pi)
CP
CP  DPH1                R8  I   original  azimuthal angle (rad, 0..2 pi)
CP  DTH1                R8  I   original    polar   angle (rad, 0..pi)
CP
CP  DPH2                R8    O resulting azimuthal angle (rad, 0..2 pi)
CP  DTH2                R8    O resulting   polar   angle (rad, 0..pi)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
CI  IGENL.INC         parameter block with trig. constants
C
C   routines_called    type     description 
CR  REPOL1              SR      cartesian to polar coordinate conversion
CR  DRADRS              R8      'resets' radians
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
      INCLUDE '../../include/IGENL.INC'
C
      REAL*8        D1, D2, D3, DPH1, DPH2, DTH1, DTH2,
     &              DST, DCT, DH, DS1, DC1, DS2, DC2, DX, DY, DZ,
     &              DR, DPHI, DTHETA, DRADRS
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR 
      character(8)   RTNAME
C
      DATA          RTNAME /'EULER1'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      if (max(dabs(D1),dabs(D2),dabs(D3)).gt.D2PI .or.
     &    DPH1.lt.0.D0) then
        IERR = max(IERR,1)
        call WRERR(RTNAME,IERR,'Warning: Unusual input parameters')
      endif
C
      if (DTH1.lt.0.D0 .or. DTH1.gt.DPI) then
        IERR = max(IERR,2)
        call WRERR(RTNAME,IERR,'Error in input parameters')
      else
C
        DST = sin(DTH1)
        DCT = cos(DTH1)
C
        DH  = DPH1 - D1
        DS1 = sin(DH)
        DC1 = cos(DH)
C
        DS2 = sin(D2)
        DC2 = cos(D2)
C
        DX = DST*DC1
        DY = DST*DS1*DC2 + DCT*DS2
        DZ =-DST*DS1*DS2 + DCT*DC2
C
        call REPOL1(DX,DY,DZ,DR,DPHI,DTHETA)
C
        if (DABS(DR-1.D0).gt.1.E-6) then
          IERR = max(IERR,2)
        else
          DPH2 = DRADRS(DPHI - D3)
          DTH2 = DTHETA
        endif
C
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine JDEPS1(DJD,LMEAN,DEPS,IERR)
C
CC    Calculates the MEAN/TRUE obliquity of the ecl. for the Jul. Date
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:05
C
CA  author : KOD        date: 29-AUG-1989 17:22
CU  update : KOD        date: 29-MAR-1990 15:53
C
CT  status: tested
C
C   general description 
CG  Subroutine to calculate the MEAN/TRUE obliquity of the ecliptic
CG  for a given Julian Date, according to
CG     Jean Meeus, 'Astronomical Formulae for Calculators',
CG                 third edition, 1985, p.44,69,70,81.
CG
CG  The parameters for calculating the mean obliquity have been
CG  taken from 'THE ASTRONOMICAL ALMANACH', 1984, S21.
CG
CG  For APPARENT positions (affected by aberration and nutation)
CG  use the TRUE obliquity, otherwise the MEAN obliquity.
C
C   call_var.          type I/O description 
CP  DJD                 R8  I   Julian Date
CP
CP  LMEAN               L4  I   mean or true obliquity of the ecliptic:
CP                                LMEAN = .true.  : mean obliquity
CP                                LMEAN = .false. : true obliquity
CP
CP  DEPS                R8    O obliquity of the ecliptic  (radians)
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  JDNUT1              SR      calculates the nutation parameters
CR  WRERR               SR      formats error code in standard form
CR  ERRSTC              SR      continue error stack for one level
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DJD, DEPS, DJD0, DYEAR, D1, D2, D3, D4, DT, DP,
     &              DE, DJ
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR , ISTAT
      LOGICAL*4     LMEAN
      character(8)   RTNAME
C
      PARAMETER ( DJD0 = 2451545.D0 )
      PARAMETER ( DYEAR =  36525.D0 )
C
      PARAMETER ( D1 =  0.40909280422D0 )
      PARAMETER ( D2 = -2.26965525D-4 )
      PARAMETER ( D3 = -2.86040D-9 )
      PARAMETER ( D4 =  8.78967D-9 )
C
      DATA          RTNAME /'JDEPS1'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      if (DJD.lt.0.D0 .or. DJD.gt.3.D6) then
        IERR = 2
        call WRERR(RTNAME,IERR,'Error in the input parameter DJD')
      endif
C
      if (IERR.lt.2) then
C
        DT = (DJD - DJD0) / DYEAR
C
        DEPS = D1 + ( D2 + ( D3 + D4 * DT ) * DT ) * DT
C
        if (.not.LMEAN) then
C
          DJ = DJD
          call JDNUT1(DJ,DP,DE,ISTAT)
C
          if  (ISTAT.gt.1) then
            IERR = max(IERR,ISTAT)
            call ERRSTC(RTNAME,10)
          else
            DEPS = DEPS + DE
          endif
C
        endif
C
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
      subroutine CNVDMS(DDEG,LPLUS,IDEG,IMIN,RSEC,IERR)
C
CC    Calculates minutes and seconds out of a decimal number
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 16:54
C
CA  author : KOD        date: 19-JUL-1989 18:31
CU  update : KOD        date: 28-MAY-1991 10:46  ( IMIN,RSEC < 60 )
C
CT  status: tested
C
C   general description 
CG  Subroutine to calculate the minutes and seconds out of a decimal
CG  number (degrees or hours). Negative values are allowed.
C
C   call_var.          type I/O description 
CP  DDEG                R8  I   number to be converted (degr. or hours)
CP
CP  LPLUS               L4    O sign of DDEG ( > 0  if  LPLUS=.true.)
CP  IDEG                I4    O integer part (positive, degr. or hours)
CP  IMIN                I4    O minutes (positive, < 60 )
CP  RSEC                R4    O seconds (positive, < 60 )
CP
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DDEG, DH
      REAL*4        RSEC
      INTEGER*4     IERR, IDEG, IMIN
      LOGICAL*4     LPLUS
      character(8)   RTNAME
C
      DATA          RTNAME /'CNVDMS'/
C
      IERR = 0
C
      if (abs(DDEG).gt.1.D3) then
        IERR = max(IERR,1)
        call WRERR(RTNAME,IERR,'Possible error in input parameters')
      endif
C
      LPLUS = (DDEG.ge.0.D0)
C
      DH   = dabs(DDEG)
      IDEG = idint(DH)
      IMIN = idint((DH-dble(IDEG))*60.D0)
      RSEC = sngl(((DH-dble(IDEG))*60.D0-dble(IMIN))*60.D0)
C
      if (RSEC.ge.60.0) then
        RSEC = 0.0
        IMIN = IMIN + 1
      endif
C
      if (IMIN.ge.60) then
        IMIN = 0
        IDEG = IDEG + 1
      endif
C
      RETURN
      END
        SUBROUTINE FUNCT (IC,KONT,N,X,FUNC)
C
CC  This routine drives ADEN to get the densities for the integration
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 16:43
C
CA  author : SLS        date: 21-DEC-1987 12:47
CU  update : SXRB       date:  7-APR-1992 16:40
C
CT  status: not tested
C
C   general description 
CG  This routine calls ADEN to evaluate the atmospheric densities for
CG  use in the numerical integration
C
C   call_var.          type I/O description 
CP  FUNC                R4    O array containing values for integration
CP  IC                  I4  I   call number to the routine
CP  KONT                I4  I   = 0 integrate from Sun to scatter point
CP                              = 1 integ from scatter point to ROSAT
CP  N                   I4      number of points
CP  X                   R4  I   array containing the steps in X
C
C   include_block name          description 
CI  CCONST.CMN                   contains useful constants
CI  CPOS.CMN                    contains the position information
C
C   routines_called    type     description 
CR  ADEN                SR      supplies atmospheric elemental
CR                              densities
CR  REPOL1              SR      converts from spherical to Cartesian
CR                              coords
CR  EQUECL              SR      transforms between ecliptic and
CR                              equatorial coord systems
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C   ALION       from ADEN, contains elemental densities
C   DENSIT      temporary array to store densities
C   DTEMP       double precision variable for the position
C
        IMPLICIT NONE
C
C  Include blocks
C
        INCLUDE '../../include/ROSCONST.INC'
        INCLUDE '../../include/CPOS.CMN'
C
C  Declare variables
C
        INTEGER I, IC, IERR, II, KONT, N
C
        REAL*4 ALION(6), AMMW, DENSIT(50,5), FUNC(N)
        REAL*4 RHO, SSAT(3), TEMP(2), TEMPER, X(N)
C
        REAL*8 DLG, DLT, DR, DSSAT(3), DTEMP(3), JD2000
C
        DATA JD2000 /2451544.5D0/
C
C  Start the calculations
C
        IF(IC .EQ. 1) THEN
C
C  This is the first time through so get all the densities
C
C
C  For the Sun to scattering point set the values for Y and Z
C
            DTEMP(2) = DXP(2)
            DTEMP(3) = DXP(3)
C
            DO 100 I=1,N
C
C  Find the Cartesian coord of the point
C
                IF(KONT .EQ. 0) THEN
C
C  Set X for the integration from the Sun to the point
C
                    DTEMP(1) = DBLE(X(I))
                ELSE
C
C  Set X, Y, and Z for the integral from the point to the satellite
C
                    DO 50 II=1,3
                        DTEMP(II) = DXS(II) + DBLE(X(I))*DXL(II)
   50               ENDDO
                ENDIF
C
C  Get the ecliptic coords of the point
C
                CALL REPOL1(DTEMP(1),DTEMP(2),DTEMP(3),DR,DLG,DLT)
                DLT = PIH - DLT
                DLG = DLG + SLONG - PI
                IF(DLG .GE. PI2) DLG = DLG - PI2
                IF(DLG .LT. 0.) DLG = DLG + PI2
C
C  Get the celestial coords
C
                CALL EQUECL (JD2000,.TRUE.,.FALSE.,DLG,DLT,
     +                      DSSAT(1),DSSAT(2),IERR)
                SSAT(1) = DSSAT(1)
                SSAT(2) = DSSAT(2)
                TEMPER = 6378.517*SQRT(1. - 6.7E-3*SIN(SSAT(2))**2)
                SSAT(3) = SNGL(DR) - TEMPER
C
C  Get the atmospherical elemental densities
C
                CALL ADEN(AMJD,SONNE,SSAT,GEO,TEMP,ALION,AMMW,RHO)
C
C  Stuff the array FUNC with the value to be used
C
                FUNC(I) = 10.**(ALION(1) - 1.)
C
C  Stuff the array DENSIT with densities 2 => 6
C  Convert from log10 of the density per
C  cubic meter to number density per cubic cm.  Also scale by 10**5
C  to compensate for the integral being over km.
C  Density = 10.**(ALION - 6 + 5).  The result is in # cm**-2 km**-1.
C
                DO 75 II=2,6
                    DENSIT(I,II-1) = 10.**(ALION(II) - 1.)
   75           ENDDO
C
  100       ENDDO
C
        ELSE
C
C  This is not the first time through so just stuff the array FUNC
C  with the appropriate values from DENSIT
C
            II = IC - 1
            DO 200 I=1,N
                FUNC(I) = DENSIT(I,II)
  200       ENDDO
C
        ENDIF
C
        RETURN
        END
        REAL FUNCTION TLOCAL(Z,TC)
C
CC  Evaluates equation (10) or (13) of CIRA 72
C
C************************ FFORM VERSION 1.2 ************  8-APR-92 07:06
C
                                               
CU  update : H. H. FINK date:    MAY-1984
CU  update : SLS        date: 15-DEC-1987 13:22
CU  update : SXRB       date:  7-APR-1992 17:14
C
CT  status: not tested
C
C   general description 
CG  Evaluates equation (10) or (13) of the model atmosphere routine
CG  depending on Z
C
C   call_var.          type I/O description 
CP  TLOCAL              R4      
CP  Z                   R4      height
CP  TC                  R4  I   
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        REAL*4 DZ, TC(4), Z
C
        DZ = Z - 125.0
        IF(DZ .LE. 0.0) THEN
            TLOCAL = ((-9.8204695E-6*DZ - 7.3039742E-4)*DZ**2 +
     +          1.0)*DZ*TC(2) + TC(1)
        ELSE
            TLOCAL = TC(1) + TC(3)*ATAN(TC(4)*DZ*
     +          (1.0 + 4.5E-6*DZ**2.5))
        ENDIF
C
        RETURN
        END
        REAL FUNCTION HYDRO(E,IFLAG,IERR)
C
CC  This routine returns the absorption cross section for hydrogen
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 17:09
C
CA  author : SLS        date: 12-JAN-1988 15:47
CU  update : SXRB       date: 7-APR-1992 17:08
C
CT  status: not tested
C
C   general description 
CG  This routine returns the absorption cross section for hydrogen for
CG  the given energy E.  The source for the data is Atomic and Nuclear
CG  Data Tables, January 1982.  The energy is given in eV and the cross
CG  section is returned in cm**2.
C
C   call_var.          type I/O description 
CP  E                   R4  I   energy in eV
CP  IERR                I4    O = 0 no error
CP                              = 1 energy out of range
CP  IFLAG               I4  I   = 1 cross section in cm**2/g
CP                              = 2 cross section in cm**2/atom
CP  HYDRO               R4      absorption cross section in cm**2
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
C
        IMPLICIT NONE
C
        INTEGER*4 IERR, IFLAG
C
        REAL*4 E
C
        IERR = 0
        IF((E .LT. 10.) .OR. (E .GT. 10000.)) THEN
            IERR = 1
            HYDRO = 0.
        ELSE
C
            IF(E .LE. 108.5) THEN
                HYDRO = EXP(21.89447 + (-2.356918 -
     +              0.079616*ALOG(E))*ALOG(E))
            ELSE
C
                IF(E .LE. 524.9) THEN
                    HYDRO = EXP(22.19324 + (-2.486793 -
     +                  0.0655064*ALOG(E))*ALOG(E))
                ELSE
C
                    IF(E .LE. 2622.0) THEN
                        HYDRO = EXP(23.41958 + (-2.894383 -
     +                      0.031693*ALOG(E))*ALOG(E))
                    ELSE
C
                        HYDRO = EXP(27.0070 + (-3.759371 +
     +                          0.0203055*ALOG(E))*ALOG(E))
                    ENDIF
                ENDIF
            ENDIF
            IF(IFLAG .EQ. 2) THEN
C
C  Convert to cm**2/atom
C
                HYDRO = HYDRO*1.67382E-24
            ENDIF
C
        ENDIF
C
        RETURN
        END
        REAL FUNCTION AMBAR(Z)
C
CC  Evaluates equation (1) for the model atmosphere
C
C************************ FFORM VERSION 1.2 ************  8-APR-92 07:03
C
                                               
CU  update : H. H. FINK date:    MAY-1984
CU  update : SLS        date: 15-DEC-1987 13:22
CU  update : SXRB       date:  7-APR-1992 17:14
C
CT  status: not tested
C
C   general description 
CC  Evaluates equation (1) for the CIRA 72 model atmosphere
C
C   call_var.          type I/O description 
CP  Z                   R4      height
CP  AMBAR               R4      
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER*4 I, J
        REAL*4 AMB, C(7), DZ, Z
C
        DATA C /28.82678, -7.40066E-2, -1.19407E-2, 4.51103E-4,
     +      -8.21895E-6, 1.07561E-5, -6.97444E-7/
C
        DZ = Z - 90.0
        AMB = C(7)
        DO 10 I=1,6
            J = 7 - I
            AMB = DZ*AMB + C(J)
   10   ENDDO
C
        AMBAR = AMB
C
        RETURN
        END
        REAL FUNCTION NITRO(E,IFLAG,IERR)
C
CC  This routine returns the absorption cross section for nitrogen
C
C************************ FFORM VERSION 1.2 ************  7-APR-92 17:07
C
CA  author : SLS        date: 12-JAN-1988 15:42
CU  update : SXRB       date: 7-APR-1992 17:06
C
CT  status: not tested
C
C   general description 
CG  This routine returns the absorption cross section for nitrogen for
CG  the given energy E.  The source for the data is Atomic and Nuclear
CG  Data Tables, January 1982.  The energy is given in eV and the cross
CG  section is returned in cm**2.
C
C   call_var.          type I/O description 
CP  E                   R4  I   energy in eV
CP  IERR                I4    O = 0 no error
CP                              = 1 energy out of range
CP  IFLAG               I4  I   = 1 cross section in cm**2/g
CP                              = 2 cross section in cm**2/atom
CP  NITRO               R4      absorption cross section in cm**2
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
C
        IMPLICIT NONE
C
        INTEGER*4 IERR, IFLAG
C
        REAL*4 E
C
        IERR = 0
        IF((E .LT. 10.) .OR. (E .GT. 10000.)) THEN
            IERR = 1
            NITRO = 0.
        ELSE
C
            IF(E .LE. 108.5) THEN
                NITRO = EXP(11.93593 + (1.660150 -
     +              0.4194906*ALOG(E))*ALOG(E))
            ELSE
C
                IF(E .LE. 400.) THEN
                    NITRO = EXP(18.17406 + (-1.087547 -
     +                  0.117212*ALOG(E))*ALOG(E))
                ELSE
C
                    IF(E .LE. 2165.) THEN
                        NITRO = EXP(18.24654 + (-0.2593173 -
     +                      0.1750234*ALOG(E))*ALOG(E))
                    ELSE
C
                        NITRO = EXP(22.88454 + (-1.486585 -
     +                      0.093863*ALOG(E))*ALOG(E))
                    ENDIF
                ENDIF
            ENDIF
            IF(IFLAG .EQ. 2) THEN
C
C  Convert to cm**2/atom
C
                NITRO = NITRO*2.32640E-23
            ENDIF
C
        ENDIF
C
        RETURN
        END
        FUNCTION NBIN(E,EN,N)
C
CC  Used in STREU, the Thomson cross section routine
C
C************************ FFORM VERSION 1.2 ************  8-APR-92 07:12
C
CA  author : SXRB       date:  8-APR-1992 07:11
C
CT  status: not tested
C
C   general description 
CG  Used in STREU, the Thomson cross section routine
C
C   call_var.          type I/O description 
CP  NBIN                I4      
CP  E                   R4  I   
CP  N                   I4      
CP  EN                  R4  I   
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        INTEGER I, N, NA, NBIN
C
        REAL*4 E, EN(N)
C
        NA = 0
        DO 10 I=1,N
            IF((NA .LE. 0) .AND. (E .LE. EN(I))) NA = I
   10   CONTINUE
        NBIN = NA
C
        RETURN
        END
        REAL FUNCTION GRAV(Z)
C
CC  Evaluates equation (8) in the model atmosphere routine
C
C************************ FFORM VERSION 1.2 ************  8-APR-92 07:08
C
                                               
CU  update : H. H. FINK date:    MAY-1984
CU  update : SLS        date: 15-DEC-1987 13:22
CU  update : SXRB       date:  7-APR-1992 17:14
C
CT  status: not tested
C
C   general description 
CG  Evaluates equation (8) in the CIRA 72 model atmosphere routine
C
C   call_var.          type I/O description 
CP  GRAV                R4      
CP  Z                   R4      height
C
C   include_block name          description 
CI 
C
C   routines_called    type     description 
CR 
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
        REAL*4 Z
C
        GRAV = 9.80665/(1.0 + Z/6356.766)**2
C
        RETURN
        END
      subroutine JDNUT1(DJD,DP,DE,IERR)
C
CC    Calculates the nutation in longitude and in obliquity from the JD
C
C************************ FFORM VERSION 1.2 ************ 30-MAR-90 17:06
C
CA  author : KOD        date: 29.08L-1989 14:45
CU  update : KOD        date: 20-MAR-1990 15:04
C
CT  status: tested
C
C   general description 
CG  Subroutine to calculate the nutation in longitude and the
CG  nutation in obliquity which are needed for the calculation
CG  of the APPARENT place of a star and for that of the APPARENT
CG  sidereal time, according to
CG     Jean Meeus, 'Astronomical Formulae for Calculators',
CG                 third edition, 1985, p.69-70 .
C
C   call_var.          type I/O description 
CP  DJD                 R8  I   Julian Date
CP  DP                  R8    O nutation in longitude (radians)
CP  DE                  R8    O nutation in obliquity (radians)
CP  IERR                I4    O = 0 no error
CP                              = 1 warning
CP                              = 2 error
CP                              = 3 severe error
C
C   include_block name          description 
CI  CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  WRERR               SR      formats error code in standard form
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX                              
C
C***********************************************************************
C
C   variables   meaning
C
C
      IMPLICIT NONE
      INCLUDE '../../include/ROSCONST.INC'
C
      REAL*8        DJD, DP, DE, DJD0, DYEAR, DT, DLS, DLM,
     &              DMS, DMM, DOM, DH, DLS2, DLM2, DOM2, DF
C     REAL*4        HFLAG
C     INTEGER*4     IOS
      INTEGER*4     IERR 
      character(8)   RTNAME
C
      PARAMETER ( DJD0 = 2415020.D0 )
      PARAMETER ( DYEAR =  36525.D0 )
C
      PARAMETER ( DF = 4.848136811D-6 )
C
      DATA          RTNAME /'JDNUT1'/
C
C     IF (HFLAG(RTNAME,'in: <input_parameters> ').NE.0) THEN
C       WRITE (OUTPT,101,IOSTAT=IOS) !<input_parameters>
C 101   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      IERR = 0
C
      if (DJD.lt.0.D0 .or. DJD.gt.3.D6) then
        IERR = 2
        call WRERR(RTNAME,IERR,'Error in the input parameter DJD')
      endif
C
      if (IERR.lt.2) then
C
        DT = (DJD - DJD0) / DYEAR
C
        DLS = 4.8816283D0 + (628.3319506D0 + 5.2883D-6 * DT) * DT
        DLM = 4.7199672D0 + (8399.709144D0 - 1.9775D-5 * DT) * DT
        DMS = 6.2565830D0 + (628.3019466D0 - 2.6180D-6 * DT) * DT
        DMM = 5.1680002D0 + (8328.691104D0 + 1.6043D-4 * DT) * DT
        DOM = 4.5236020D0 - (33.75714610D0 + 3.6268D-5 * DT) * DT
C
        DLS2 = 2.D0 * DLS
        DLM2 = 2.D0 * DLM
        DOM2 = 2.D0 * DOM
C
        DH = - (17.2327D0 + 0.01737D0 * DT ) * sin(DOM)
     &       - ( 1.2729D0 + 0.00013D0 * DT ) * sin(DLS2)
     &       + 0.2088D0 * sin(DOM2) - 0.2037D0 * sin(DLM2)
     &       + ( 0.1261D0 - 0.00031D0 * DT) * sin(DMS)
     &       + 0.0675 * sin(DMM)
     &       - (0.0497D0 - 0.00012D0 * DT) * sin(DLS2+DMS)
     &       - 0.0342D0 * sin(DLM2-DOM)
     &       - 0.0261D0 * sin(DLM2+DMM)
     &       + 0.0214D0 * sin(DLS2-DMS)
     &       - 0.0149D0 * sin(DLS2-DLM2+DMM)
     &       + 0.0124D0 * sin(DLS2-DOM)
     &       + 0.0114D0 * sin(DLM2-DMM)
C
        DP = DH * DF
C
        DH =   ( 9.2100D0 + 0.00091D0 * DT ) * cos(DOM)
     &       + ( 0.5522D0 - 0.00029D0 * DT ) * cos(DLS2)
     &       - 0.0904D0 * cos(DOM2)
     &       + 0.0884D0 * cos(DLM2)
     &       + 0.0216D0 * cos(DLS2+DMS)
     &       + 0.0183D0 * cos(DLM2-DOM)
     &       + 0.0113D0 * cos(DLM2+DMM)
     &       - 0.0093D0 * cos(DLS2-DMS)
     &       - 0.0066D0 * cos(DLS2-DOM)
C
        DE = DH * DF
C
      endif
C
C     IF (HFLAG(RTNAME,'out: <output_parameters> ').NE.0) THEN
C       WRITE (OUTPT,9901,IOSTAT=IOS) !<output_parameters>
C9901   FORMAT (  )
C       CALL WRFLAG(OUTPT)
C     ENDIF
C
      RETURN
      END
