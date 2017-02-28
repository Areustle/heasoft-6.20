CH1  SHADOW
CH1  $Id: shadow.f,v 1.4 2013/05/21 19:08:25 irby Exp $
C         *************************************************************
C         SHADOW is a subroutine that will be used to calculate arrays 
C	  of livetimes taking into account earth shadowing.
C
C         SHADOW will be called for each entry of the exposure history
C         file.  On the first call of a given mode configuration, the
C         output livetime map will be initialized.  Subsequent calls
C         for the same mode configuration will accumulate into the
C         livetime array.  Consequently, it is most efficient to
C         treat all like modes within the time range of interest by
C         sequential calls to SHADOW and then generate the exposure
C         before considering the next mode.
C
C
C         Calling Sequence:
C         -----------------
C
C             call shadow(tnd,livtim,eltim,epos,tstep,econe,ltmap )
C
C             Variable    Type  I/O     Description
C             --------    ----  ----    ---------------------------
C               tnd       I*4    I      Direction and Type mode bit
C                                       mask.
C               livtim    R*4    I      Livetime of the Mode-Livetime
C                                       entry
C               eltim     R*4    I      Elapsed time of the Mode-
C                                       Livetime entry
C               epos(4)   R*4    I      Earth Right Ascension and Dec-
C                                       lination at the start and at the
C                                       end of the interval of the Mode-
C                                       Livetime entry
C               tstep     R*4    I      Time step size for the shadow
C                                       analysis in seconds.  Typically,
C                                       10 for 0.5 degree bins.
C               econe(10) R*4    I      Earth limb cutoff angles in
C                                       degrees for each energy interval
C               ltmap( )  R*4    O      Accumulated livetime in each map
C                                       bin for the current mode.  This
C                                       is a three dimensional array;
C                                       two sky angles and energy.  Its
C                                       size is the same as the exposure
C                                       map.
C
C         Method
C         ------
C
C         For each call, the earth postion is given at the start and end
C         of an interval of time over which the live time has been
C         accumulated.  It is assummed that the earth moves uniformly
C         along a great circle defined by these two points on a sphere.
C         The routine EARTHP is called to give the position at inter-
C         mediate locations.  At steps of size TSTEP in time, the earth
C         location is evaluated, and the cosine of the angle between the
C         earth and each skybin is calculated.  This number is compared
C         with the cosine of cutoff angle for each energy interval to
C         decide if the bin is shadowed of clear.
C
C         After the last call for a given mode, the LTMAP array is used
C         to accumulate exposure.
C
C         Called by:  NXTMOD
C         Calls:      EARTHP   to find the Earth location
C
C     Written by D. L. Bertsch.  5/31/91.
C
C     *****************************************************************
C	Modification History:
CH5     2.00    E.S.Panduranga 08/29/91
CH5                     Moved code from IBM to SUN.
CH5                     Stripped off line numbers and trailing blanks.
CH5			Changed INCLUDE(FILE) to INCLUDE 'file.cmn'
CH5 $Log: shadow.f,v $
CH5 Revision 1.4  2013/05/21 19:08:25  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.3  2002/12/26 17:05:30  irby
CH5 Split data statements out of declarations for f90 compatibility.
CH5
CH5 Revision 1.2  2002/04/18 19:37:56  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:04  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.5  1993/11/15  20:03:38  albert
c Removed the initialization of the livetime array from the shadow routine
c because this is better done in the nxtmod routine which knows when there
c is an instrument mode which requires initialization. This corrects a program
c bug.
c
c Revision 2.4  1993/05/06  13:09:38  albert
c Re-arranged code to add livetime array more efficiently
c
c Revision 2.3  1992/05/14  15:16:57  albert
c Removed comment about shadow being a test version.
c
c Revision 2.2  1992/04/01  22:00:40  albert
c Used variable dimension arrays passed as subroutine parameters.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
C     *****************************************************************

      subroutine shadow(tnd,livtim,eltim,epos,tstep,econe,ltmap )

Cae   real*4     ltmap(200,200,10), livtim, ltinc, econe(10), elim(10)
Cae   real*4     sbdec(200), cbdec(200), epos(4), tstep
      real*4     ltmap(naxis1,naxis2,naxis3), livtim, ltinc, econe(10)
      real*4     elim(10), epos(4), tstep
      integer*4  tnd
      real	 eltim, pi, torad, as, crvl1, cdel1, ds, crvl2, cdel2
      real	 slat, clat, rlon, bra, cdelra, ctheta
      integer	 i, j, k, ntotl, nstep

      logical qinit, qnew

      data qinit/.true./

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'
      real*4     sbdec(axl), cbdec(axl), ctmax, mnelim
      data       mnelim/1./
      integer    kcount, lcount
      data       kcount/0/, lcount/0/
      real       cbdmn, cbdmx
      data       cbdmn/1.0/, cbdmx/-1.0/
      real       sbdmn, sbdmx
      data       sbdmn/1.0/, sbdmx/-1.0/

      character(80)	id

      save

      common	/id/id
      id = '$Id: shadow.f,v 1.4 2013/05/21 19:08:25 irby Exp $'

C--->       Initialization on the first call only.


      if (qinit) then
         qinit = .false.

         pi = 4.0*atan(1.0)
         torad = pi/180.0

C--->       Determine earth limit cosine values.

         do i=1,naxis3
            elim(i) = cos( pi - torad*econe(i) )
            if (elim(i) .lt. mnelim) mnelim = elim(i)
         enddo

C--->       Calculate angles and trignometric functions for each bin in
C     the skymap.

         crvl1 = crval1 * torad
         cdel1 = cdelt1 * torad
         crvl2 = crval2 * torad
         cdel2 = cdelt2 * torad

         as = crvl1 + (crpix1-0.5) * cdel1
         ds = crvl2 + (crpix2-0.5) * cdel2

         do i = 1, naxis2
            sbdec(i) = sin(ds)
            cbdec(i) = cos(ds)
            if (cbdec(i) .gt.  cbdmx) cbdmx = cbdec(i)
            if (cbdec(i) .lt.  cbdmn) cbdmn = cbdec(i)
            if (sbdec(i) .gt.  sbdmx) sbdmx = sbdec(i)
            if (sbdec(i) .lt.  sbdmn) sbdmn = sbdec(i)
            ds = ds + cdel2
         enddo
      endif

Cesp  ! End of Subroutine Initialization



C---> Determine the number of steps for the shadow analysis.

      ntotl = 1 + eltim/tstep
      ltinc = livtim/ntotl
      qnew = .true.

C--->Increment the livetime in the bins that are not shadowed.

C---> Loop over the elapsed time interval
      do nstep = 1, ntotl
         call earthp(nstep, ntotl, coords, epos, qnew, slat, clat, rlon)
C------> Loop over the X axis
         bra = as
         do i = 1, naxis1
            cdelra = cos(rlon-bra)
            bra = bra + cdel1
            ctmax = 0
c
c---------->Check if any pixel on this line can be shadowed.
c
            if (clat*cdelra .lt. 0) then
               ctmax = cbdmn*clat*cdelra
            else
               ctmax = cbdmx*clat*cdelra
            endif

            if (slat .lt. 0) then
               ctmax = ctmax + slat*sbdmn
            else
               ctmax = ctmax + slat*sbdmx
            endif

            if (ctmax .gt. mnelim) then

C------------->If a pixel might be shadowed loop over the Y axis
               do j = 1, naxis2

                  ctheta = slat*sbdec(j) + clat*cbdec(j)*cdelra

                  if (ctheta .gt. mnelim) then
                     

C------------------->If any energy is shadowed loop over the Energies

                     do k = 1, naxis3
                        if( ctheta.gt.elim(k) ) then
                           ltmap(i,j,k) = ltmap(i,j,k) - ltinc
                        endif
                     enddo
                  endif
               enddo
            endif
         enddo
      enddo

      return
      end
