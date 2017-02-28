      function detidx (Telescop, Instrume, Detnam)
c
c Function to match input telescop, instrume with the
C common zsatellites and zdetectors arrays from info files
c The index of the match is returned.  
C -2 if unmatched.
c
c  I  telescop  c  Telescope  
c  I  instrume  c  Instrument
c  I  detnam    c  Detector name
c      
      integer detidx
      character*(*) Telescop, Instrume, Detnam
c      
c Local variables 
      integer i
      character(80) teltmp1, instmp1, dettmp1
      character(80) teltmp2, instmp2, dettmp2
      logical found
      include '../include/startup.inc'
c
      detidx = -2
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
c  Using the first 10 letters satellite and 
c  the first 4 letters for the detector
c
         if (teltmp1(1:10) .eq. teltmp2(1:10) .and.
     &       instmp1(1:4) .eq. instmp2(1:4) .and.
     &       dettmp1 .eq. dettmp2 ) then
            found = .TRUE.
            detidx = i
         endif
         i = i + 1
      enddo
c
c  If not found, and detnam is nonblank, look through again
c  to see if just telescop and instrume match.
c
      if ( .not.found .and. detnam.ne.' ' ) then
         i = 1
         do while (i.le.ZEXpnum .and. .not.found)
            teltmp2 = ZTElescop(i)
            instmp2 = ZINstrume(i)
            dettmp2 = ZDEtnam(i)
c
c  Using the first 10 letters satellite and 
c  the first 4 letters for the detector
c
            if (teltmp1(1:10) .eq. teltmp2(1:10) .and.
     &          instmp1(1:4) .eq. instmp2(1:4) .and.
     &          dettmp2.eq.' ' ) then
               found = .TRUE.
               detidx = i
            endif
            i = i + 1
         enddo
      endif
      
      return
      end
