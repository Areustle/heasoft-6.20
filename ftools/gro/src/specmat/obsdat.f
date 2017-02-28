* OBSDAT
*
* Read data from input file to determine basic information about
* the observation.
* Figures out which of the three different input file formats it
* is and calls the appropriate subroutine to do the real work.

* The three file formats are:
*  Old pulsar:  SMDB format with an ASCII header.  Two lines of
*     information per header line.
*  New pulsar:  Defined by Joe Fierro.  FITS compatible.
*     SMDB format with FITS header.
*  DC:  ASCII data.  Header with information about the observation,
*     then a table of fluxes in individual energy bands.  There are
*     two minor variants handled by LIKEFILE.

*
* Written for Spectral version 2.7,  December 1994
* PLN, Stanford
*
* Note:  All of this code used to be in the main routine "matrix.f".
*
*   @(#) obsdat.f 1.2@(#)

      subroutine obsdat(nregns,pulsar,pstyle,emin,emax,nints,inttime,
     >     celeq,srclon,srclat,thetamax,cutoff,
     >     cutangle,eregn,conetheta,data_dir,misc_dir,selfil,conefile)

      implicit none

      include '../SPECMAT_COMMON/spectral.inc'   ! for nintmax

      character*(*)  data_dir,misc_dir,selfil,conefile,pulsar
      character(100) pstyle
      real emin,emax,srclon,srclat,thetamax,cutoff
      real cutangle(0:jmax),eregn(0:jmax)
      real conetheta(0:jmax)
c      real conetheta(0:jmax),zenithcut
      real*8 inttime(2,nintmax)
      integer nregns,nints
      logical celeq

c      real zenoff,zenmax,angacc

      save

csb-02/07      call getenv('PULSAR',pulsar)

c      if (pulsar.eq.'PULSAR') then   
c	 call getenv('PSTYLE',pstyle)
c	 if (pstyle.eq.'OLD') then  ! old style pulsar file
c            call selections(emin,emax,inttime,nints,celeq,
c     >        srclon,srclat,thetamax,cutoff,data_dir)
c	    zenoff = 4.   ! 4 sigma zenith cutoff 
c	    zenmax = 105.
c            angacc = 1.
c	 else if (pstyle.eq.'NEW') then  ! new style pulsar file
c	    call psrfits(emin,emax,nints,inttime,celeq,
c     >        srclon,srclat,thetamax,cutoff,zenmax,zenoff,angacc,
c     >	      data_dir,misc_dir)
c	 end if
*        Get obs. energy bands.
c         call cones(conefile,jmax,nregns,eregn,conetheta,thetamax,angacc)
c	 if (cutoff.gt.0.) then   ! fixed zenith cutoff
c            do i = 0,nregns
c               cutangle(i) = cutoff
c            end do
c	 else     ! energy-dependent zenith cutoff
c            do i = 1,nregns
c               cutangle(i) = zenithcut(eregn(i-1),zenoff,zenmax)
c            end do
c            cutangle(0) = zenithcut(30.,zenoff,zenmax)
c	 end if
c      else  ! DC data file
         call likefile(emin,emax,inttime,nints,celeq,
     >        srclon,srclat,nregns,eregn,conetheta,
     >        cutangle,data_dir,selfil)
c      end if

      return
      end



