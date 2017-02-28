 
 
 
      SUBROUTINE SAX_VIGN(Off_axis,Energy,Vcor,Ierr)
c
c  I  off-axis in mm
c  I  energy in keV (constant value)
c      ONLY USED IN OLD VERSION (ignored in this version) 
c  O  vcor vignetting correction value
c  O  ierr error
 
      INTEGER*4 Ierr
      REAL*4 Vcor , Energy , Off_axis
c 1/7/98 new vars
      INTEGER*4 meloop, i
      REAL*8 VcorMe(3), v1me(3),v2me(3)
      INCLUDE '../include/io.inc'
c
c original version P. Giommi & F. Fiore 10 April 1997
c
c new version P. Giommi & F. Fiore  10 June 1997
c new version uses in-flight vignetting from CRAB data
c new version FT  1 jul 1998 vignetting energy dependent
c
c mm_pix = size of raw pixel (18.4 arcsec) in millimeters
c
c      mm_pix=0.17
c      r=off_axis*60./18.4 * mm_pix
c
c 15/09/1999 FT - Energy Bounds changed    
c
      meloop = 3
c      if      (energy .LE. 1.25) then
      if      (energy .LE. (1.25+1.49)/2. ) then
        meloop = 2
        v1me(1)=0.0
        v2me(1)=0.0
        v1me(2)=0.025791
        v2me(2)=1.749571
        v1me(3)=0.032542
        v2me(3)=1.628563
c        write(*,*) 'Banda 1 < ',(1.25+1.49)/2.
c      else if (energy .LE. 1.49) then
      else if (energy .LE. (1.49+1.74)/2. ) then
        v1me(1)=1.727309267927315E-02
        v2me(1)=1.98208631362280
        v1me(2)=2.227229082339847E-02
        v2me(2)=1.89971183751855
        v1me(3)=2.841363419881844E-02
        v2me(3)=1.78462052186727
c        write(*,*) 'Banda 2 < ',(1.49+1.74)/2.
c      else if (energy .LE. 1.74) then
      else if (energy .LE. (1.74+2.11)/2. ) then
        v1me(1)=1.746315904762756E-02
        v2me(1)=1.98520183562456
        v1me(2)=2.270231061200483E-02
        v2me(2)=1.88302038427282
        v1me(3)=2.806746565520834E-02
        v2me(3)=1.78447559351713
c        write(*,*) 'Banda 3 < ',(1.74+2.11)/2.
c      else if (energy .LE. 2.11) then
      else if (energy .LE. (2.11+3.11)/2. ) then
        v1me(1)=2.030498690781272E-02
        v2me(1)=1.91855735688935
        v1me(2)=2.354522884676458E-02
        v2me(2)=1.86208185747440
        v1me(3)=2.764646611301573E-02
        v2me(3)=1.80343353770296
c        write(*,*) 'Banda 4 < ',(2.11+3.11)/2.
c      else if (energy .LE. 3.11) then
      else if (energy .LE. (3.11+4.5)/2. ) then
        v1me(1)=1.983722552430309E-02
        v2me(1)=1.91711318836520
        v1me(2)=2.422441837740836E-02
        v2me(2)=1.84442076983885
        v1me(3)=2.743244657008172E-02
        v2me(3)=1.79239991975789
c        write(*,*) 'Banda 5 < ',(3.11+4.5)/2.
c      else if (energy .LE. 4.5) then
      else if (energy .LE. (4.5+5.4)/2. ) then
        v1me(1)=2.093360330240181E-02
        v2me(1)=1.90773842059734
        v1me(2)=2.292646354947332E-02
        v2me(2)=1.88057211702968
        v1me(3)=2.671029353300194E-02
        v2me(3)=1.81442405347719
c        write(*,*) 'Banda 6 < ',(4.5+5.4)/2.
c      else if (energy .LE. 5.4) then
      else if (energy .LE. (5.4+6.4)/2. ) then
        v1me(1)=2.434586124351594E-02
        v2me(1)=1.87277770719182
        v1me(2)=2.485557921418442E-02
        v2me(2)=1.86751809772383
        v1me(3)=2.524555593448276E-02
        v2me(3)=1.87371657440308
c        write(*,*) 'Banda 7 < ',(5.4+6.4)/2.
c      else if (energy .LE. 6.4) then
      else if (energy .LE. (6.4+8.04)/2. ) then
        v1me(1)=2.696942216714384E-02
        v2me(1)=1.88425896761067
        v1me(2)=2.975055020393113E-02
        v2me(2)=1.86516022715976
        v1me(3)=3.246351331868780E-02
        v2me(3)=1.83117332851822
c        write(*,*) 'Banda 8 < ',(6.4+8.04)/2.
      else
        v1me(1)=3.827204462848392E-02
        v2me(1)=1.86167534625685
        v1me(2)=4.326816067708913E-02
        v2me(2)=1.83881625828630
        v1me(3)=4.593963660711761E-02
        v2me(3)=1.82136002413847
c        write(*,*) 'Banda ULTIMA'
      endif     
c      else
c        zwrite = ' energy out of range in sax_vign '
c        call xwrite(zwrite,10)
c        write(*,*) 'energy = ', energy
c        vcor=0.
c        return
c      endif
c      Vcor = 1./(1.+v1*r**v2)
c      v1=0.013867
c      v2=1.662397
c
c     Compute vcor
c
      Vcor = 0
      DO i = 0, meloop - 1
         VcorMe(3-i) = 1./(1.+v1me(3-i)*off_axis**v2me(3-i))
         Vcor = Vcor + VcorMe(3-i) 
      ENDDO

      Vcor = Vcor/meloop

      RETURN
      END
