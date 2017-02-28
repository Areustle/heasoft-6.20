*+WMAP2OAH

      subroutine wmap2oah(iunit,chatter,wmap,nx,ny,opticx,opticy,deltx,
     $     delty,ntheta,minthet,maxthet,oahist,area,ierr)
      implicit none
      integer chatter,nx,ny,ierr,iunit
      double precision wmap(nx,ny)
      double precision opticx,opticy,deltx,delty
      integer ntheta,mtheta
      parameter(mtheta=20)
      real minthet(mtheta),maxthet(mtheta),oahist(mtheta)
      real area
C  Description
C     This takes a WMAP (and associated parameters) and  generates 
C     an Off Axis Histogram (and associated parameters).
C  Parameters
c -------------- authors/modifications ----------------------------
c Original author is unknown
c Banashree M Seifert (Oct 9, 1996)
c         . initialisations are done (Linux behave scrazy otherwise)
c ----------------------------------------------------------------------
C     
C     Internal Variables
      double precision step,dx2,dy2,dx,dy,thetabar,totcnts
      integer istep,ix,iy,ithet,totpix,npixsou
      real pixsize
      character(160) message
      character(80) comm
      real pixsou


      totcnts=0.0
      totpix=0
      ntheta=mtheta
      do ix=1,mtheta
         oahist(ix)=0.
      enddo

C     generate thet_min and thet_max
      step=60.0/real(ntheta)
      minthet(1)=0
      maxthet(1)=step

      do 5 istep=2,ntheta
         minthet(istep)=maxthet(istep-1)
         maxthet(istep)=real(istep)*step
 5    continue
      do 10 ix=1,nx
         dx=(real(ix)-opticx)*deltx
         dx2=dx*dx
         do 20 iy=1,ny
            if(wmap(ix,iy).le.0) goto 20
            dy=(real(iy)-opticy)*delty
            dy2=dy*dy
            thetabar=sqrt(dx2+dy2)
            ithet=thetabar/step+1
            oahist(ithet)=oahist(ithet)+wmap(ix,iy)
            totcnts=totcnts+nint(wmap(ix,iy))
            totpix=totpix+1
 20      continue
 10   continue
      do 30 ithet=1,ntheta
         oahist(ithet)=oahist(ithet)/totcnts
 30   continue
c Read in the other keywords necessary
      ierr = 0
      npixsou=0
      pixsou=0.
      comm='             '
      pixsize=0.0
      call FTGKYE(iunit,'NPIXSOU ',pixsou,comm,ierr)
      npixsou=int(pixsou)
      if(ierr.NE.0) then
         npixsou = 0
         if(chatter.ge.10) then
            message='Could not find NPIXSOU keyword in PHA file'
            call fcecho(message)
         endif
         call ftcmsg()
         ierr = 0
      endif
      call FTGKYE(iunit,'PIXSIZE ',pixsize,comm,ierr)
      if(ierr.NE.0) then
         pixsize = 0
         if(chatter.ge.10) then
            message='Could not find PIXSIZE keyword in PHA file'
            call fcecho(message)
         endif
         call ftcmsg()
         ierr = 0
      endif
      area = real(npixsou) * (pixsize*pixsize)
      if(area.eq.0) then
         if(chatter.ge.5) then
         message='Warning: Couldn''t calculate accurate sky area due to'
     $        //' missing keywords.'
         call fcecho(message)
         message='Using detector area instead. '
         call fcecho(message)
         endif
C     Set AREA = the size of the non blank pixels in the WMAP in deg^2
         area=real(totpix)*deltx*delty/3600.0
      endif
      return
      end
