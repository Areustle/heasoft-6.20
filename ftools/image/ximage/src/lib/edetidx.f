      function edetidx (Telescop, Instrume, Detnam)
c
c Function to EXACTLY match input telescop, instrume with the
C common zsatellites and zdetectors arrays from info files
c The index of the match is returned.  
C -2 if unmatched.
c
c  Used for adding new missions on the fly.  Don't want specific
c  TELESCOP/INSTRUME/DETNAM to match generic TELESCOP/INSTRUME
c  and prevent adding specific detector.
c
c  I  telescop  c  Telescope  
c  I  instrume  c  Instrument
c  I  detnam    c  Detector name
c      
      integer edetidx
      character*(*) Telescop, Instrume, Detnam
c      
c Local variables 
      integer i
      character(80) teltmp1, instmp1, dettmp1
      character(80) teltmp2, instmp2, dettmp2
      logical found 
      include '../include/startup.inc'
c
      edetidx = -2
      found = .FALSE.

      teltmp1 = Telescop
      instmp1 = Instrume
      dettmp1 = Detnam
      call UPC(teltmp1)
      call UPC(instmp1)
      call UPC(dettmp1)

      i = 1
      do while (i.le.ZEXpnum .and. .not.found)
         teltmp2 = ZTElescop(i)
         instmp2 = ZINstrume(i)
         dettmp2 = ZDEtnam(i)
c
         if (teltmp1 .eq. teltmp2 .and.
     &       instmp1 .eq. instmp2 .and.
     &       dettmp1 .eq. dettmp2 ) then
            found = .TRUE.
            edetidx = i
         endif
         i = i + 1
      enddo
      
      return
      end
