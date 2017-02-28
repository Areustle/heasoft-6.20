ch1  shadow
c         *************************************************************
c         shadow is a preliminary version of a routine that will be
c         used to calculate arrays of livetime taking into account
c         earth shadowing.
c
c         shadow will be called for each entry of the mode-livetime
c         file.  on the first call of a given mode configuration, the
c         output livetime map will be initialized.  subsequent calls
c         for the same mode configuration will accumulate into the
c         livetime array.  consequently, it is most efficient to
c         treat all like modes within the time range of interest by
c         sequential calls to shadow and then generate the exposure
c         before considering the next mode.
c
c
c         calling sequence:
c         -----------------
c
c             call shadow(livtim,eltim,epos,tstep,ltmap )
c
c             variable    type  i/o     description
c             --------    ----  ----    ---------------------------
c               livtim    r*4    i      livetime of the mode-livetime
c                                       entry
c               eltim     r*4    i      elapsed time of the mode-
c                                       livetime entry
c               epos(4)   r*4    i      earth right ascension and dec-
c                                       lination at the start and at the
c                                       end of the interval of the mode-
c                                       livetime entry
c               tstep     r*4    i      time step size for the shadow
c                                       analysis in seconds.  typically,
c                                       10 for 0.5 degree bins.
c               ltmap     r*4    o      accumulated livetime
c
c         method
c         ------
c
c         for each call, the earth postion is given at the start and end
c         of an interval of time over which the live time has been
c         accumulated.  it is assummed that the earth moves uniformly
c         along a great circle defined by these two points on a sphere.
c         the routine earthp is called to give the position at inter-
c         mediate locations.  at steps of size tstep in time, the earth
c         location is evaluated, and the cosine of the angle between the
c         earth and each skybin is calculated.  this number is compared
c         with the cosine of cutoff angle for each energy interval to
c         decide if the bin is shadowed of clear.
c
c         after the last call for a given mode, the ltmap array is used
c         to accumulate exposure.
c
c         called by:  nxtmod
c         calls:      earthp   to find the earth location
c
c     this test version written by d. l. bertsch.  5/31/91.
c     modified for use in SPECTRAL by p. l. nolan  5/11/92.
c
c    @(#) shadow.f 1.2@(#)
c
c     *****************************************************************

      subroutine shadow(livtim,eltim,epos,tstep,econe,ltmap,nregns,
     >     sra,sdec)
      
      implicit none
      include '../SPECMAT_COMMON/spectral.inc'
      include '../SPECMAT_COMMON/param.cmn'
      
      real*4     ltmap(0:jmax), livtim, ltinc
      real elim(0:jmax),econe(0:jmax)
      real*4     sbdec, cbdec, epos(4), tstep
      real	 eltim, sra, sdec
      real	 slat, clat, rlon, cdelra, ctheta
      real       sind, cosd
      integer	 ntotl, nstep, k, nregns
      
      logical qinit, qnew

      save

      external   sind, cosd
      
      data qinit/.true./


c--->       initialization on the first call only.
      if( qinit) then
         qinit = .false.

C--->       Determine earth limit cosine values.
         do k=0,nregns
            elim(k) = cosd(180. - econe(k) )
         end do

c--->       calculate angles and trignometric functions for each bin in
c           the skymap.
         sbdec = sind(sdec)
         cbdec = cosd(sdec)
      end if
cesp  ! end of subroutine initialization


c--->    clear out the livetime array
	 do k = 0,jmax
	 	ltmap(k) = 0.0
	 enddo
cesp  ! end of initialization for mode change.

c---> determine the number of steps for the shadow analysis.
      ntotl = 1 + eltim/tstep
      ltinc = livtim/ntotl
      qnew = .true.

c--->increment the livetime in the bins that are not shadowed.

c---> loop over the elapsed time interval
      do nstep = 1, ntotl
         call earthp(nstep, ntotl, epos, qnew, slat, clat, rlon)
         cdelra = cosd(rlon*rd2dg-sra)
         ctheta = slat*sbdec + clat*cbdec*cdelra
         do k = 0,nregns
            if (ctheta.lt.elim(k)) ltmap(k)=ltmap(k)+ltinc
         enddo
      enddo
      
      return
      end
