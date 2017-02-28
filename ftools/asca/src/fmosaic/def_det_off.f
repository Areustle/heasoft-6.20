c=========================================================================
      subroutine def_det_off(nev)
c-------------------------------------------------------------------------
c     calculate the offset of the detector center with respect to
c     image center
c
c     calculations are performed using all events
c 
c     They are in detector pixels and should be added to x0,y0 (see wcsparam.f)
c
c     MODIFICATIONS:
c     1. final rms is calculated in detector pixels
c        Mon Apr 17 20:05:48 EDT 1995 
c     2. Adapted to fmosaic (ftools) Thu Sep 23 10:17:00 EDT 1999 
c                                                  (Ilana Harrus)
c     Modified: Tue Mar  7 10:22:53 EST 2000 (Ilana Harrus); 
c                Bypass the wcs libraries. Call to FITSIO routines.
c
c     Modified: Friday Jun 29 07:24:05 EDT 2001 (Ilana Harrus);
c               Erase the computation of the array -- 
c               Change the comparison from det to sky- 
c               This makes the computation of detoff_x,detoff_y inacurate 
c               to within a couple of pixels-- 
c
c-------------------------------------------------------------------------
      implicit none
      include 'common_evts.inc'
      include 'common_img.inc'
      include 'pi.inc'
     

c---- local variables
      real*8 dx,dy,x,y,ra,dec,xd,yd
     + ,detx0,dety0
      integer i,nev,status

      real*8 xrot

      real dr,dd,avr,avd,rmsr,rmsd,maxr,maxd

c
c==== calculate transformation
c      print*,' '
c      print*,' Fit DET OFFSETS using all available events'

C------ General initialisation
c------
         status=0
         i=1
c------
         dx=0.d0
         dy=0.d0
         x=0.d0
         y=0.d0
         ra=0.d0
         dec=0.d0 
         xd=0.d0
         yd=0.d0
         xrot=0.d0
         detx0=0.d0
         dety0=0.d0

c------- Set offsets to zero
         detoff_x=0.d0
         detoff_y=0.d0
c------
         dr=0.
         dd=0.
         avr=0.
         avd=0.
         rmsr=0.
         rmsd=0.
         maxr=0.
         maxd=0.
c
c------- Loop over all events
         do i=1,nev
c.......... point in the sky (from event list)
            x=dble(skyx(i))
            y=dble(skyy(i))
            ra=0.d0
            dec=0.d0
c       if(i.lt.10) print *,i,x,y,skyx(i),skyy(i),detx(i),dety(i)
c.......... RA, DEC of the point (x,y)
        call ftwldp(x,y,sky_ra0,sky_dec0,sky_x0,sky_y0,
     &   sky_dx,sky_dy,0.,'-TAN',ra,dec,status)
c         if(i.lt.10) print*,'sky',i,x,y,ra,dec
c.......... Now to the detectors plane
           xrot=pa_pnt


          detx0=det_x0+detoff_x
          dety0=det_y0+detoff_y
      
         call ftxypx(ra,dec,sky_ra0,sky_dec0,detx0,dety0,
     &   sky_dx,sky_dy,xrot,'-TAN',xd,yd,status)
c         if(i.lt.10) print*,'det',i,xd,yd,ra,dec

          dx=dx+xd-dble(detx(i))
          dy=dy+yd-dble(dety(i))

c---- RMS (in RA, DEC) after transformation
c         print*,' Deviation of (SKY->RA,DEC->DET)-(DET) [pixels] '
c                      

          dr=(detx(i)-sngl(xd))
          dd=(dety(i)-sngl(yd))
c-------
          avr=avr+dr
          avd=avd+dd
          rmsr=rmsr+dr**2
          rmsd=rmsd+dd**2
          maxr=amax1(maxr,abs(dr))
          maxd=amax1(maxd,abs(dd))

         end do 

c-- final computation and printout.
         dx=dx/dble(nev)
         dy=dy/dble(nev)
         detoff_x=-dx
         detoff_y=-dy
         rmsr=sqrt(rmsr/nev)
         rmsd=sqrt(rmsd/nev)
         avr=avr/nev
         avd=avd/nev
 
c         print 20,sngl(detoff_x),sngl(detoff_y),rmsr,rmsd
c 20      format('OFFSETS x,y [pix]: ',f7.2,1x,f7.2,1x,
c     >       '  rms[pix]:',f7.2,1x,f7.2)

      return
      end








