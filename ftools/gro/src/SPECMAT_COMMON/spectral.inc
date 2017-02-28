*This is an include file for SPECTL and MATRIX.  

      integer first,last,spacing,jmax,nornmax,nintmax,modemax
      parameter (first=-9)          ! first bin in incident spectrum
      parameter (last=260)          ! last bin in incident spectrum
      parameter (spacing=200)       ! energy bins from 20 MeV - 20 GeV
      parameter (jmax=25)           ! maximum # of observed bins
      parameter (nornmax=600)       ! max # of orientation intervals
      parameter (nintmax=60)        ! max # of specified time intervals
      parameter (modemax=87)        ! max # of exposure modes
