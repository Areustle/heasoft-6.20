      subroutine scc2utc(luc,ichat,SCC,UTCI,UTCR,IERR)

C @(#)sccut2.for	1.1 (MPE-Garching) 3/16/94 16:46:26
c Originally:
c#        SUBROUTINE SCCUT2(SCC,UTCI,UTCR,SCCTAB,IERR)
C
CC  ROSAT Conversion from Spacecraft Clock to UTC
C
c  Adapted to XANADU timing analysis by eal  NASA/Goddard, HSTX  April, 1994
c
c# Replaced or deleted parts of the original program are denoted by c#.
c
c# 
c  New parts added for XANADU and FITS are set between blank c# lines.
c# 
C************************ FFORM VERSION 3.0 ********** 31-JAN-90 09:20
C
CA  author : TMB              date: 20-FEB-1991 11:05 from SCCUTC
CA  update : TMB/FDM          date: 15-MAY-1991 11:40 bug corrected 
CA  update : TMB              date: 04-JUL-1991 17:12 TEST ONLY
CA  update : TMB              date: 16-JUL-1991 17:16 new calib. (HBO)
CA  update : TMB              date: 28-JAN-1992 10:09 table input (CfA)
CA  update : TMB              date: 17-MAR-1992 10:10 new reference times
CA  update : JPF              date: 28-JUL-1992 12:10 table from input
CA  update : TMB              date: 20-OCT-1992 12:10 new table (HBO)
C
CT  status: not tested
C
C   general description:
CG  This module, given in input a value of the spacecraft clock, computes
CG   the corresponding UTC, by using the interpolation made by HBO.
CG   The output is divided into integer part and fractional part
CG   to avoid loss of precision.
CG   The interpolation parameters are read in from a calibration table
C
C   call_var.          type I/O description
CP  SCC                 R8  I   input spacecraft clock
CP  UTCI                I4    O output UTC (integer part)
CP  UTCR                R8    O output UTC (fractional part)
CP  SCCTAB              C   I   name of SCC2JD parameter table
CP  IERR                I     O = 0 no error 
C
C   include_block_name          description
CI  RCOMMON:CGENL.CMN           general parameter common block
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
C     NCOMP         I*4      (parameter) maximum number of polynomials
C     PCOEFF        I*4      (parameter) maximum order of polynomial
C     UTCI          R*8       output UTC (integer part)
C     JD90          R*8      (parameter) Julian date of midnight
C                                        between Dec 31,89 - Jan 1,90
C     DAYSEC        R*8      (parameter) one day in second units
C     A(NCOMP,NCOEFF) 
C                   R*8       array with polynomial coefficients
C     UTCR          R*8       output UTC (fractional part)
C     SCC           R*8       input spacecraft clock
C     DREF          R*8       days since reference date
C     UTH           R*8       seconds since midnight Dec 31,89 -> Jan 1,90
C     SCCTAB        C*(*)     name of SCC2JD parameter table
C     FIRST         L*4       flag for first call
C
      IMPLICIT NONE
C
c#
      integer luc,pcount,cmax,hdutype,nulvj,ichat
      parameter (cmax = 100)
      logical anynul
      integer*4 tbcol(cmax), size
      character(16) ttype(cmax),tform(cmax),tunit(cmax),extname,routine
      character(80) errm
      double precision nulvd
      data routine /'scc2utc'/
      data nulvj,nulvd /0,0.d0/
c#
      INTEGER     NCOMP 
      PARAMETER    (NCOMP     =      20       )
      INTEGER     PCOEFF
      PARAMETER    (PCOEFF    =      10       )
      DOUBLE PRECISION
     &        JD90
      PARAMETER    (JD90      = 2447892.5D+00 )
      DOUBLE PRECISION
     &        DAYSEC
      PARAMETER    (DAYSEC    =   86400.0D+00 )
C
      DOUBLE PRECISION
     &        A(NCOMP,PCOEFF)
      DOUBLE PRECISION
     &        STARTT(NCOMP)
      DOUBLE PRECISION
     &        ENDT(NCOMP)
      DOUBLE PRECISION
     &        REFTIM(NCOMP)
C----------------------------------------------------------------
C I AM ADDING AN ARRAY FOR THE TIME ADDED TO SCC WHICH WE WILL
C SUBTRACT OFF THE TIME TO PERFORM THE INTERPOLATION 7/28/92 JPF
C----------------------------------------------------------------
      DOUBLE PRECISION
     &        SCCADD(NCOMP)
C----------------------------------------------------------------
C VARIABLE TO HOLD THE TIME ADDED TO SCC 7/28/92 JPF
C----------------------------------------------------------------
      DOUBLE PRECISION
     &        TIMADD
      DOUBLE PRECISION
     &        UTCR
      DOUBLE PRECISION
     &        SCC
      DOUBLE PRECISION
     &        DREF
      DOUBLE PRECISION
     &        REF
      DOUBLE PRECISION
     &        UTH
      INTEGER     PARTYP(NCOMP)
      INTEGER     NCOEFF(NCOMP)
      INTEGER     UTCI
Cold  INTEGER*4     ACTS
c#       INTEGER     KUN
Cold  INTEGER*4     KNUL
c#       INTEGER     DUN
c#       INTEGER     DNUL
c#       INTEGER     LL1
c#       INTEGER     LL2
c#       INTEGER     SYST
c#       INTEGER     C_UNIT
      INTEGER     NCOL
      INTEGER     N
      INTEGER     I
      INTEGER     J
c#       INTEGER     NSC
c#       INTEGER     ACOL
c#       INTEGER     AROW
c#       INTEGER     LENCH
      INTEGER     IERR 
      LOGICAL     FIRST
c#       LOGICAL     NULL
      LOGICAL     DONE
Cold  character(80)  SCCPAT
c#       character(72)  SCCTAB
Cold  character(72)  SCCOPE
c#       character(4)   FTYPE
      character(8)   RTNAME
      data RTNAME /'scc2utc'/
C
c#       INCLUDE 'RCOMMON:CGENL.CMN'
      SAVE
C
c#       INCLUDE 'MID_INCLUDE:ST_DEF.INC'
c#       INCLUDE 'MID_INCLUDE:ST_DAT.INC'
C
Cold      DATA          SCCTAB /'scc_to_utc.tbl'/
          DATA          FIRST  /.TRUE./
C
      IERR=0
C-----------------------------------------------------------------
C     On first call, open calibration table and read in parameters
C-----------------------------------------------------------------
          IF(FIRST) THEN
             FIRST=.FALSE.
C------------------------------------------------------
C        Get keyword with local definition of EXSAS_CAL
C------------------------------------------------------
Cold         CALL STKRDC('SCCDIR',80,1,80,ACTS,SCCPAT,KUN,KNUL,IERR)
C----------------------------------
C        Assemble pathname-filename
C----------------------------------
Cold         LL1=LENCH(SCCPAT)
Cold         LL2=LENCH(SCCTAB)
Cold         IF((LL1+LL2).GT.72) THEN
Cold            IERR = 1
Cold          ELSE
C--------------------------------------------------
C           Check whether the system is VMS or UNIX
C--------------------------------------------------
Cold            CALL STKRDI('AUX_MODE',1,1,ACTS,SYST,KUN,KNUL,IERR)
Cold            IF(SYST.GT.1) THEN
C---------------------------------
C              Finally assemble it
C---------------------------------
Cold               SCCOPE=SCCPAT(1:LL1)//'/'//SCCTAB(1:LL2)
Cold             ELSE
Cold               SCCOPE=SCCPAT(1:LL1)//SCCTAB(1:LL2)
Cold            ENDIF
Cold         ENDIF
C-------------------------------------------
C        Open the SCC--UTC calibration table
C-------------------------------------------
Cold         CALL TBTOPN(SCCOPE,F_I_MODE,C_UNIT,IERR)
c#          CALL TBTOPN(SCCTAB,F_I_MODE,C_UNIT,IERR)
c# 
         CALL ftmahd(luc,2,hdutype,IERR)
c# 
C----------------------------------------------------
C        Get general infos about the correction table
C----------------------------------------------------
c#          CALL TBIGET(C_UNIT,NCOL,N,NSC,ACOL,AROW,IERR)
c# 
         IF(hdutype.EQ.2)THEN 
            CALL ftghbn(luc,cmax,N,NCOL,ttype,tform,tunit
     &              ,extname,pcount,IERR)
         ENDIF
         IF(hdutype.EQ.1) THEN 
            CALL ftghtb(luc,cmax,size,N,NCOL,ttype,tbcol,tform
     &                   ,tunit,extname,IERR)
         ENDIF
c# 
C-------------------------------------------------------
C        Check if it's a valid scc--utc correction table
C-------------------------------------------------------
c#          CALL STDRDC(C_UNIT,'FILE_TYPE',3,1,1,KUN,FTYPE,
c#      &               DUN,DNUL,IERR)
c#          IF(FTYPE(1:3).NE.'UTC') THEN
c# C---------------------------------------
c# C           Not a valid correction table
c# C---------------------------------------
c#             IERR=2
c#            ELSE
C-------------------------------
C           Read in coefficients
C-------------------------------
            DO I=1,N
C--->          Start time of the Ith interval
c#                CALL TBERDD(C_UNIT,I,1,STARTT(I),NULL,IERR)
c# 
               CALL ftgcvd(luc,1,I,1,1,nulvd,STARTT(I),anynul,IERR)

c# 
C--->          End  time of the Ith interval
c#                CALL TBERDD(C_UNIT,I,2,ENDT(I)  ,NULL,IERR)
c# 
               CALL ftgcvd(luc,2,I,1,1,nulvd,ENDT(I)  ,anynul,IERR)
c# 
C--->          Type of function of interpolation:
C--->                   (1) polynomial (only one in this version)
c#                CALL TBERDI(C_UNIT,I,3,PARTYP(I),NULL,IERR)
c# 
               CALL ftgcvj(luc,3,I,1,1,nulvj,PARTYP(I),anynul,IERR)
c# 
C--->          Number of coefficients for this interval
c#                CALL TBERDI(C_UNIT,I,4,NCOEFF(I),NULL,IERR)
c# 
               CALL ftgcvj(luc,4,I,1,1,nulvj,NCOEFF(I),anynul,IERR)
c# 
C--->          Actual coefficients
               DO J=1,NCOEFF(I)
c#                   CALL TBERDD(C_UNIT,I,4+J,A(I,J),NULL,IERR)
c# 
                  CALL ftgcvd(luc,4+J,I,1,1,nulvd,A(I,J),anynul,IERR)
c# 
               ENDDO
C--->          Reference time
c#                CALL TBERDD(C_UNIT,I,NCOL-1,REFTIM(I),NULL,IERR)
c# 
               CALL ftgcvd(luc,NCOL-1,I,1,1,nulvd,REFTIM(I),anynul,IERR)
c# 
C THIS IS WHERE I'LL READ IN THE TIME ADDED TO SCC JPF
c#                CALL TBERDD(C_UNIT,I,NCOL,SCCADD(I),NULL,IERR)
c# 
               CALL ftgcvd(luc,NCOL  ,I,1,1,nulvd,SCCADD(I),anynul,IERR)
c# 
            ENDDO
c#          ENDIF
C----------------------------------
C        Close the correction table
C----------------------------------
c#          CALL TBTCLO(C_UNIT,IERR)
c# 
         IF(IERR.NE.0) THEN
            errm = 'scc2utc: reading from correction file'
            CALL xerror(errm,1)
            RETURN
         ENDIF
c# 
C-----------------------------------------------------------
C        Terminal output for range of correction application
C-----------------------------------------------------------
c#          WRITE(6,*)
c#          WRITE(6,*)' WARNING! The SCC--> UTC conversion is'
c#          WRITE(6,*)' calibrated only up to:'
c#          WRITE(6,1000)ENDT(N)
c# 1000     FORMAT(1X,'        SCC = ',F12.1)
c#          WRITE(6,*)
c#          WRITE(6,*)' Beyond that value.... HIC SUNT LEONES!'
c#          WRITE(6,*)
c# 
      ENDIF
c#          CALL xwrite(' ',ichat)
c#          CALL xwrite(' WARNING! The SCC--> UTC conversion is',ichat)
c#          CALL xwrite(' calibrated only up to:',ichat)
c#          WRITE(comm,1000)ENDT(N)
c# 1000     FORMAT(1X,'        SCC = ',F12.1)
c#          CALL xwrite(comm,ichat)
c#          CALL xwrite(' ',ichat)
c#          CALL xwrite(' Beyond that value.... HIC SUNT LEONES!',ichat)
c#          CALL xwrite(' ',ichat)

c Fatal error if SCC value is not covered in table.

      IF(scc.gt.endt(n)) THEN
         errm = 'SCC value too large for SCC-UTC correction table.'
         CALL xerror(errm,1)
         RETURN
      ENDIF
C-----------------------------------------------------------------
C     Compute time in seconds after midnight dec 31,89 -> jan 1,90
C-----------------------------------------------------------------
C-------------------------------------------------
C     Loop over the components to find current one
C-------------------------------------------------
      UTH = 0.0D0
      DONE = .FALSE.
      DO I=1,N
         IF((SCC.GE.STARTT(I)) .AND.
     &      (SCC.LE.ENDT(I)))  THEN
            DONE = .TRUE.
C-----------------------------
C           Set reference time
C-----------------------------
            REF = REFTIM(I)
C-------------------
C ADDITIVE TIME HERE
C-------------------
            TIMADD = SCCADD(I)
            DO J=1,NCOEFF(I)
C-----------------
C SUBTRACT IT HERE
C-----------------
               UTH = UTH + A(I,J)*((SCC-TIMADD)**(J-1))
Cold           UTH = UTH + A(I,J)*SCC**(J-1)
            ENDDO
         ENDIF
      ENDDO
C-----------------------------------------------
C     If time is after last calibration point...
C       you're extrapolating...
C-----------------------------------------------
      IF(.NOT.DONE) THEN
C--------------------------
C        Set reference time
C--------------------------
         REF = REFTIM(N)
C-------------------
C ADDITIVE TIME HERE
C-------------------
         TIMADD = SCCADD(N)
         DO J=1,NCOEFF(N)
C-----------------
C SUBTRACT IT HERE
C-----------------
            UTH = UTH + A(N,J)*((SCC-TIMADD)**(J-1))
Cold        UTH = UTH + A(N,J)*(SCC**(J-1))
         ENDDO
      ENDIF
C-------------------------------------
C     Get days since the reference day
C-------------------------------------
      DREF = UTH / DAYSEC
C---------------------
C     Get integer part
C---------------------
      UTCI =INT(DREF)
C------------------------
C     Get fractional part
C------------------------
      UTCR = DREF - UTCI
C----------------------------------------
C     Check if it's going to the next day
C----------------------------------------
      UTCR = UTCR + 0.5
      IF(UTCR.GE.1.0) THEN
         UTCR = UTCR - 1.0
         UTCI = UTCI + 1
      ENDIF
C-----------------------------------------------------------
C     Add reference date  (JD)
C      WARNING! The 0.5 is lost while converting to integer,
C      but this is taken into account above (15-MAY-1991)
C-----------------------------------------------------------
      UTCI = REF + UTCI
C
      RETURN
      END     
