
       SUBROUTINE JETX_VIGN(Off_axis,Energy,Vcor,Ierr)
c
c  I  off-axis in arcmin
c  I  energy in keV (constant value)
c      ONLY USED IN OLD VERSION (ignored in this version) 
c  O  vcor vignetting correction value
c  O  ierr error
 
      INTEGER*4 Ierr
      REAL*4 Vcor,Energy,Off_axis,v1,v2,v3,v4,v5,v0
      INCLUDE '../include/io.inc'

c  Initialize to avoid warning
      v0 = 0.
      v1 = 0.
      v2 = 0.
      v3 = 0.
      v4 = 0.
      v5 = 0.
c  --
c
c original version P. Giommi & F. Fiore 10 April 1997
c
c new version P. Giommi & F. Fiore  10 June 1997
c Modified by S. Campana for JEt-X vignetting function 4/2/98
c 
c
c mm_pix = size of raw pixel (1.59 arcsec) in millimeters
c
c      mm_pix=0.027 
c      r=off_axis*60./1.59 * mm_pix 
c

c      if      (energy .LT. 0.88 .AND. energy .ge. 0.3) then
      if      (energy .LT. 0.88 ) then
        v0=1.000000
        v1=5.9080184E-03
        v2=-7.9552345E-03
        v3=9.2729751E-04
        v4=-4.9238541E-05
        v5=9.5969995E-07
      else if (energy .LT. 4.77 .AND. energy .ge. 0.88) then
        v0=1.000000
        v1=5.4275501E-03
        v2=-8.3732773E-03
        v3=1.0193160E-03
        v4=-5.5404915E-05
        v5=1.0925288E-06
c      else if (energy .LT. 10.0 .AND. energy .ge. 4.77) then
      else if (energy .ge. 4.77) then
        v0=1.000000
        v1=8.5760531E-04
        v2=-1.6809050E-02
        v3=1.7872758E-03
        v4=-7.6970835E-05
        v5=1.2280474E-06
c      else
c        zwrite = ' energy out of range in jetx_vign '
c        call xwrite(zwrite,10)
c        vcor=0.
c        ierr=1
c        return
      endif
c
      Vcor = v0+v1*off_axis+v2*off_axis**2.+v3*off_axis**3.
     &        +v4*off_axis**4.+v5*off_axis**5.

c      write(*,*) 'jetx_vign: En = ', Energy, ' Vcor = ', Vcor  

      RETURN
      END


