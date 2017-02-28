C -----------------------------------------------------------------------

      subroutine gt_wmap(chatter,iunit,nx,ny,wmap,opticx,opticy,
     $     deltx,delty,status)
      implicit none
      integer iunit,status,nx,ny,chatter
      double precision wmap(nx,ny),opticx,opticy,deltx,delty
*+
C     This routine gets the WMAP and the things you need to use it:
C     the optical axis of the instrument and the size of the "pixels"
C     (deltx, delty)  It assumes that the CDELT keywords are in degrees
C     (there is no keyword to tell us for sure) and returns them as 
C     arcmin.  The formula for finding the x distance from the optical 
C     axis to any given WMAP "pixel" is:
C     dx=(ix-opticx)*deltx
C     where dx is the (angular) distance in arcmin, ix is the x index 
C     number of the pixel.  Similarily for y.
C     It assumes that file has been scrolled to the apropriate extension 
C     and the file is closed upon completion.
C
C Passed Parameters
C  CHATTER      i    : How much info to report
C  IUNIT        i    : Where's the file?
C  NX,NY        i    : Dimensions of the WMAP
C  WMAP            o : The array for the WMAP
C  OPTIC_          o : Optical Axis coordinates
C  DELT_           o : Pixel sizes in arcmin
C  IERR            o : Error Flag (zero if all OK)
C
C Author/Modification History
C  Lawrence E Brown (1.0.0:94 August), Original
c
c Banashree M Seifert (Oct9, 1996) 1.1.0:
c        . initialisation of nullval (Linux problem)
c        . file left open when it exits the program
c          (this is done for convenience of PCPAPPHA)
C Ning Gan (MARCH 31, 1998) 1.2.0:
C Add checking for the CDELTx keyword. If the keyword is not 
C presented, the deltx will set to the default value. 
C kaa  1/11/02 1.3.0: Added HDUVERS=2 WMAP support.
c -------------------------------------------------------------------
*-
C     LOCAL variables
      integer group,wmbin,ihdvrs
      double precision nullval,valx,valy
      logical anyfl
      character(256) comment,message
      character(30) errstr, wrnstr
      character(7) version
      character(10) hduvers
      parameter (version='1.3.0')

      errstr = '** GT_WMAP '//version//' ERROR:'
      wrnstr = '** GT_WMAP '//version//' WARNING:'
      
      group=0
      deltx=0
      delty=0
      nullval=0.d0

      call ftg2dd(iunit,group,nullval,nx,nx,ny,wmap,anyfl,status)
      if (status.ne.0) goto 999

c Get the HDUVERS.

      CALL ftgkys(iunit,'HDUVERS',hduvers,comment,status)
      IF ( status .NE. 0 ) THEN
         status = 0
         CALL ftgkys(iunit,'HDUVERS1',hduvers,comment,status)
      ENDIF
      IF ( status .NE. 0 ) THEN
         ihdvrs = 0
         status = 0
      ELSE
         READ(hduvers(1:1),'(i1)') ihdvrs
      ENDIF

c Get the rebin factor in the WMAP. pre HDUVERS=2 this is read from the
c WMREBIN keyword. With HDUVERS=2 it can be read from CDELT1P

      IF ( ihdvrs .LT. 2 ) THEN
         call ftgkyj(iunit,'WMREBIN',wmbin,comment,status)
      ELSE
         call ftgkyj(iunit,'CDELT1P',wmbin,comment,status)
      ENDIF
      if (status.ne.0) then
         message=wrnstr//
     $        'Can''t find rebinning parameter for WMAP, assuming 15.'
         call fcerr(message)
         status=0
         call ftcmsg()
         wmbin=15
      endif
C     Override DELTX values if specified in the par file
      call uclgsd('deltx',deltx, status)
      if(status.eq.0.and.deltx.ne.0) then
         deltx=deltx*wmbin
      else
         status=0
         call ftcmsg()
         call ftgkyd(iunit,'CDELT1',deltx,comment,status)
	 if(status.eq.0) then 
C     we will  assume that the CDELTS should be positive
C     for our purposes and so take absolute values
             deltx=abs(deltx)
             if(deltx/wmbin.lt.2.4e-4) then
                message='US Rev0 data may have wrong value in PHA file.'
     $           //' Standard value is DELTX=2.595021E-4.'
                call fcecho(message)
                write(message,*) wrnstr,'X pixel size of wmap is',
     $           deltx/wmbin,' degrees.  Are you sure about this?'
                call fcecho(message)
                write(message,*) wrnstr,'If this is incorrect, ',
     $           ' Check CDELT1 keyword in pha file or change',
     $           ' DELTX parameter and run tool again'
                call fcecho(message)
             endif
C     Can not find the CDELT1 keyword 
	 else
	     deltx = 2.595021E-4*wmbin 
             message='DELTX is set to the standard value '
     $             //'of 2.595021E-4.'
             call fcecho(message)
         endif
      endif
C     Override DELTY values if specified in the par file
      call uclgsd('delty',delty, status)
      if(status.eq.0.and.delty.ne.0) then
         delty=delty*wmbin
      else
         status=0
         call ftcmsg()
         call ftgkyd(iunit,'CDELT2',delty,comment,status)
	 if(status.eq.0) then 
             delty=abs(delty)
             if(delty/wmbin.lt.2.4e-4) then
                message='US Rev0 data may have wrong value in PHA file.'
     $           //' Standard value is DELTY=2.595021E-4.'
                call fcecho(message)
                write(message,*) wrnstr,'Y pixel size of wmap is',
     $           delty/wmbin,' degrees.  Are you sure about this?'
                call fcecho(message)
                write(message,*) wrnstr,'If this is incorrect, ',
     $           ' Check CDELT2 keyword in pha file or change',
     $           ' DELTY parameter and run tool again'
                call fcecho(message)
             endif
         else
	     delty=2.595021E-4*wmbin
             message='DELTY is set to the standard value '
     $            // 'of 2.595021E-4.'
             call fcecho(message)
         endif
      endif
      call ftgkyd(iunit,'OPTAXISX',opticx,comment,status)
      call ftgkyd(iunit,'OPTAXISY',opticy,comment,status)
      if(status.ne.0.or.opticx.lt.0..or.opticy.lt.0) then
C     get the (base raw detector coordinate) optical axis from the par
C     file
         if(chatter.gt.10) then
            message=wrnstr//
     $           'Optical axis keywords are absent or wrong'
            call fcecho(message)
            message=wrnstr//
     $           'Using values from par file.'
            call fcecho(message)
         endif
         status=0
         call ftcmsg()

c Get the CRVAL. Later code wants this in binned coordinates. For HDUVERS=2
c a later CRVAL#P is in unbinned coordinates so divide by the binning factor.

         IF ( ihdvrs .LT. 2 ) THEN
            call ftgkyd(iunit,'CRVAL1',valx,comment,status)
            call ftgkyd(iunit,'CRVAL2',valy,comment,status)
         ELSE
            call ftgkyd(iunit,'CRVAL1P',valx,comment,status)
            call ftgkyd(iunit,'CRVAL2P',valy,comment,status)
            valx = (valx + (wmbin-1)/2.)/ wmbin
            valy = (valy + (wmbin-1)/2.)/ wmbin
         ENDIF
         if(status.ne.0) goto 999

C     Get the default base optical x axis
         call uclgsd('optaxisx',opticx, status)
         if(status.NE.0) then
            message=wrnstr//
     $           'Couldn''t get OPTAXISX parameter. Using 4119.0'
            call fcecho(message)
            status = 0
            call ftcmsg()
            opticx=4119.0
         endif
C     Get the default base optical y axis
         call uclgsd('optaxisy',opticy, status)
         if(status.NE.0) then
            message=wrnstr//
     $           'Couldn''t get OPTAXISY parameter. Using 3929.0'
            call fcecho(message)
            status = 0
            call ftcmsg()
            opticy=3929.0
         endif
         opticx=(opticx-1.0)*2.595021E-4/deltx+2.0-valx
         opticy=(opticy-1.0)*2.595021E-4/delty+2.0-valy
      endif
C     we will assume (DANGER) that the CDELTS are degrees and convert
C     to arcmin
      deltx=deltx*60.0
      delty=delty*60.0
 999  if(status.ne.0) then
         message=wrnstr//
     $        'Unable to get WMAP array'
         call fcerr(message)
         call fcerrm(status)
      endif
ccc      call ftclos(iunit,status)
      if(status.ne.0) call fcerrm(status)
      return
      end

