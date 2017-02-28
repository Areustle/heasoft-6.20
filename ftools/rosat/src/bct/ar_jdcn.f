      SUBROUTINE AR_JDCN(Nd,Nm,Ny,Jd,Ierr)
C
CC  Converty (DD,MM,YY) to Julian day

c This routine came from MIDAS.  It converts input integers for day
c month and year into a double precision Julian Day.
C
**==AR_JDC.spg  processed by SPAG 3.09I  at 11:08 on 12 Jan 1994
C
C************************ FFORM VERSION 1.0 ************  3-AGU-89
C
CA  author : UMKRG            date: 03-AUG-1989
CU  update : TMB              date: 15-OCT-1990 08:38 ULTRIX version
C
CT  status: not tested
C
C   general description
CG  converts day,month,year (ND,NM,NY:last two digits) to julian day
CG   WARNING: Only valid between 1980 and 1999
CG   From:         H. OGELMAN         VERSION 16/05/88
C   Ning Gan: 
C      June, 1998 
C   Adopt the new algorithm from AC p.B2
C   Now the input Ny will be the for digit year. The restriction
C   between 1980--1999 is removed. Replaced the old ar_jdc.
C   The ar_jdcn means ar_jdc_new.
C
C   call_var.          type I/O description
CP  ND                 I*4  I   day of date
C   NM                 I*4  I   month of date
C   NY                 I*4  I   year of date
C   JD                 I*8    O julian day
C   IERR               I*4    O error def.
C
C
C***********************************************************************
C
C   variables   meaning
C
C     JD         R*8            julian day
C     ND         I*4            day of date
C     NM         I*4            month of date
C     NY         I*4            year of date
C     IY         I*4            years since 1980
 
      IMPLICIT NONE
      INTEGER*4 FOUR
      PARAMETER (FOUR=4)
      REAL*8 JD0
      PARAMETER (JD0=1721013.5D0)
      REAL*8 Jd
      INTEGER*4 Nd
      INTEGER*4 Nm
      INTEGER*4 Ny
      double precision k1,k2,k3
C
      INTEGER*4 Ierr
C      
      if(nd.ne.0) then 
          k1 = 367.0 * ny
          k2 = int(7 * (ny + int((nm +9)/12.0))/4.0)
          k3 = int((275*nm)/9.0)
          jd = k1 - k2 + k3 + nd + JD0 
      else 
	  ierr = 1
      endif
      RETURN
      END
