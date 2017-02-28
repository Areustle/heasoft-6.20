C*****************************************************************
C SUBROUTINE:
C     onequat
C
C DESCRIPTION:      
C     computes the collimator correction for a light curve based
C      on a single, average quaternion value
C      
C AUTHOR:
C      James Lochner  9/95
C      
C MODIFICATION HISTORY:
C     Feb 20, 1996 - change to dividing by the coll. correction (since it's
C                     < 1.0)
C     Feb 29, 1996 - add tstart, tstop to correctly handle subsets of data
C     Aug 25, 1999 - bug fix - when computing collimator corrected rates,
C                     trap for null values in input rate values (looking
C                     for rate(i) = -100.0, using nlval assigned in readlc2)
C      
C NOTES:
C
C USEAGE:      
C      CALL onequat(iunit2, coor, quats, x_lo, x_hi, x_units,
C     $     y_lo, y_hi, y_units, npts, tstart, tstop, time, rate,
C     $     collrate, err, collerr, collresp, chat, ierr)
C      
C ARGUMENTS:
C     iunit2    - unit number for collimator cube file
C     coor      - unit vector of source position
C     quats     - 2-d array of quaternion values
C     x_lo,x_hi - limits on the spatial grid in coll. cube
C     y_lo,y_hi -      "                  "
C     xunits    - units of spatial grid x axis
C     yunits    - units of spatial grid y axis
C     npts      - number of points in light curve
C     tstart    - desired start time in light curve
C     tstop     - desired stop time in light curve
C     time      - array of time values in light curve
C     rate      - array of light curve intensities
C     err       - array of light curve intensity uncertainties
C     collrate  - array of corrected intensities
C     collerr   - array of corrected uncertainties
C     collresp  - value of the collimator correction
C     chat      - amount of chatter
C     ierr      - error code for subroutine
C
C PRIMARY LOCAL VARIABLES (selected):
C     N.B. e = energy, c1 = coord. 1 (i.e. x), c2 = coord. 2 (i.e. y)
C     c1lo, c1hi, c2lo, c2hi
C               - subcube to extract from the coll. cube
C     egrd, c1grd, c2grd
C               - energy & position grid points for subcube
C     cresp3d   - coll. cube in energy, x, and y position
C     tmpimg    - 2 x 2 subset of cresp3d
C     cresp1d   - 1-d coll. resp. on coll cube's energy grid
C     context   - error message
C     status    - error number
C
C CALLED ROUTINES:
C     xtcol1 - extract subcube from input cube of energy & position (CALLIB)
C     subroutine fcerr  - echo message to terminal
C     subroutine fcerrm - echo fitsio error message to terminal 
C      
C *******************************************************************************

      SUBROUTINE onequat(iunit2, coor, quats, x_lo, x_hi, x_units,
     $     y_lo, y_hi, y_units, npts, tstart, tstop, time, rate,
     $     collrate, err, collerr, collresp, chat, ierr)


C start with the declarations
      integer iunit2, chat, ierr, npts
      real quats(250000,4), rate(npts), collrate(npts)
      real err(npts), collerr(npts), collresp(1)
      real x_lo, x_hi, y_lo, y_hi
      double precision tstart, tstop, time(npts)
      character(20) x_units, y_units

      character(80) message
      character(8) csys
      character(40) taskname
      COMMON /task/ taskname

      real quat(4), att(3,3), coor(3), sccoor(3)
      integer ien, icx, icy
      integer j, k, n, ftstatus
      
      integer maxe, maxc1, maxc2
      parameter (maxe=10, maxc1=30, maxc2=30)
      character(30) wrnstr, errstr2
      logical qok
      real e_lo, e_hi
      real c1lo, c1hi, c2lo, c2hi
      character(20) c1unt, c2unt, enunt
      real egrd(2,maxe), c1grd(2,maxc1), c2grd(2,maxc2)
      real cresp3d(maxe,maxc1,maxc2)
      real cresp1d(maxe)
      real work1d(maxe*maxc1*maxc2)
      real tmpimg(2,2)
      real x(2), y(2)
      real temp

c Initalization
	wrnstr = '** ONEQUAT WARNING:'
	errstr2 = '** ONEQUAT ERROR:'

      ierr = 0
      ftstatus = 0

C     Convert from the 2-d quats to the 1-d quat
      do j = 1,4
         quat(j) = quats(1,j)
      end do
      
C Compute the spacecraft attitude from the quaternions
      call xteamat(quat,att)
      
C Compute the source position in the spacecraft coord.
      call xtetoirf(att,coor,sccoor,1)
      
C     check that the source position is within the coll. cube
      qok = .true.
      if((sccoor(2).LT.x_lo).OR.(sccoor(2).GT.x_hi)) then
         qok = .false.
         message = errstr2//' Coord-1 out-of-range'
      elseif((sccoor(3).LT.y_lo).OR.(sccoor(3).GT.y_hi)) then
         qok = .false.
         message = errstr2//' Coord-2 out-of-range'
      endif	   
      if(.NOT.qok) then
         ierr = 1
         goto 999
      endif


c -------------------------------------------------------------------
c ------------- Extract nearest Spatial elements from Cube ----------
c Extract appropriate elements from the collimator cube
c ... to do this, we set up the limits for the 3-dimensional sub-cube we 
c     want extracted. In our case we want a sub-cube which represents the
c     four nearest points in the spatial dimensions, and all the points in
c     the energy dimension. 
c ... This is achieved by setting 
c     c1lo=c1hi=sccoor(2), 
c     c2lo=c2hi=sccoor(3)
c     e_lo>e_hi (actual values dont matter)

c ... Set up "dummy" Energy i/ps (to return ALL values in energy dimension)
      e_lo = 1
      e_hi = 0
      enunt = 'keV' 
c ... Set up coordsystem
      csys = 'XMA_CART'
c ... Set up Coord-1 (ALPHA) values
      c1lo = sccoor(2)
      c1hi = sccoor(2)
      c1unt = x_units
c ... Set up Coord-2 (BETA) values
      c2lo = sccoor(3)
      c2hi = sccoor(3)
      c2unt = y_units
c ... Just do it
      call xtcol1(iunit2, chat, maxe, maxc1, maxc2, 
     &     e_lo, e_hi, enunt,
     &     csys, c1lo, c1hi, c1unt, c2lo, c2hi, c2unt,
     &     ien, egrd, icx, c1grd, icy, c2grd, 
     &     cresp3d, work1d, ierr)
      IF(ierr.NE.0) then
         goto 999
      endif
      if (chat .gt. 15) then
         message = 'xtcol1x complete'
         call fcecho(message)
      endif
      
c OK, so now it is believed that we have the sub-cube in the cresp3d
c array, along with the relevant grid-point arrays in engrd, cxgrd, cygrd
      if(icx.NE.2) then
         write(message,'(a,a,i12)') errstr2,' icx =', icx
         call fcecho(message)
         ierr = 1
         goto 998
      elseif(icy.NE.2) then
         write(message,'(a,a,i12)') errstr2,' icy =', icy
         call fcecho(message)
         ierr = 1
         goto 998
      endif
           
c -------------------------------------------------------------------
c ------------- Perform Bi-linear interpolation ---------------------
C We do this for each energy-slice of the sub-cube
c ... fill in the grid points
      x(1) = c1grd(1,1)
      x(2) = c1grd(1,2)
      y(1) = c2grd(1,1)
      y(2) = c2grd(1,2)

c ... Loop over the energies, putting the result in tmpimg
      do n = 1,ien
         do k = 1, icy
            do j = 1, icx
               tmpimg(j,k) = cresp3d(n,j,k)
            end do
         end do
c ... interpolate to the source position (not allowing extrapolation)
         call bilint(chat, sccoor(2), sccoor(3), 2, 2, 
     &        2, 2, x, y, tmpimg, .false., temp, ierr) 
         cresp1d(n) = temp 
         IF(ierr.NE.0) then
            goto 999
         endif
      enddo
 
c So now we have a 1-dimension array cresp1d, with ien elements, and 
c energies specified by the 2-dimensional egrd array.

c Temp dump ...
      if (chat .gt. 15) then
         message = ' ... after bilint '
         call fcecho(message)
      endif
 
C Obtain single value of the collimator correction over the energy range
c     Perform simple average
      collresp(1) = 0.
      do k = 1, ien
         collresp(1) = collresp(1) + cresp1d(k)
      enddo
      collresp(1) = collresp(1) / ien

      IF (chat .GE. 10) THEN
        WRITE(message, '(A, F8.6)') 'Collimator response is ',
     &    collresp(1)
        CALL fcecho(message)
      ENDIF

C Finally, go through the light curve, Dividing the input intensity by the
C     coll. resp. (since collresp < 1.).  Times outside of tstart and tstop
C     get INDEF values.
      k = 1
      do while (time(k) .lt. tstart)
         collrate(k) = -99.0
         collerr(k) = -99.0
         k = k + 1
      end do

      do while (time(k) .ge. tstart .and. time(k) .le. tstop
     $     .and. k .le. npts)
         
         if (rate(k) .ne. -100.0) then
            collrate(k) = rate(k) / collresp(1)
         else 
            collrate(k) = -99.0
         end if
         if (err(k) .ne. -100.0) then
            collerr(k) = err(k) / collresp(1)
         else
            collerr(k) = -99.0
         end if
         k = k + 1
      end do

      do while (k .le. npts)
         collrate(k) = -99.0
         collerr(k) = -99.0
         k = k + 1
      end do

      
C Exit subroutine
998     continue
999     continue 

        if (ierr .ne. 0) call fcecho(message)
        if (ftstatus .ne. 0) call fcerrm(ftstatus) 
	return
	end
