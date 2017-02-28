**************************************************************************** 
C FTOOLS FIMAGE TASK:
C      fadapt 
C
C FILE:
C      fadapt.f 
C
C DESCRIPTION: 
C      Performs elliptical adaptive smoothing of a FITS image.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / May '94
C
C MODIFICATION HISTORY:
C     10/17/97 PDW 3.0a - Replace old get header routines
C     12/03/97 PDW 3.0b - Introduce clobber parameter... sort of...
C                         call ffinit--an ftool utility--instead of ftinit
C     12/29/99 NG  3.0c - Updates for compressed images.
C     05/24/00 WDP 3.0d - Major changes: improved the way it converges to
C                         the optimum sized ellipse; Pixels with values above
C                         the counts threshold will now not get smoothed; 
C     05/26/00 JP  3.0e - Added % complete counter.
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    counts        Minimal number of counts within ellipse
C    smajor        Maximum semimajor axis of ellipse in pixels
C    ratio         Ratio of sigma in y to x
C    theta         Position angle of ellipse
C    boundary      Boundary (constant,nearest,reflect,wrap)
C    constant      Constant for boundary extension
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C
C CALLED ROUTINES:
C      subroutine gadapt - gets parameters from environment
C      subroutine smthadapt - smooth/write output image.
C
C****************************************************************************** 
      subroutine fadapt

      character(160)  infile,outfile 
      character(20)  datatype,boundary

      integer counts
      double precision  smajor,ratio,theta,constant,nullval

      logical  copyprime,copyall

      integer  status

      character(40) taskname
      common /task/ taskname

      taskname = 'fadapt3.0e'
      status = 0

C     * Get the parameters from the par file *
      call gadapt(infile,outfile,counts,smajor,ratio,theta,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

C     * Pass input parameters to core of smoothing software *
      call smthadapt(infile,outfile,counts,smajor,ratio,theta,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

      return
      end

C****************************************************************************** 
C SUBROUTINE:
C     gadapt 
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gadapt uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gadapt(infile,outfile,counts,smajor,ratio,theta,
C     &            boundary,constant,datatype,nullval,copyprime,
C     &            copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    counts        Minimal number of counts within ellipse
C    smajor        Maximum semimajor axis of ellipse in pixels
C    ratio         Ratio of sigma in y to x
C    theta         Position angle of ellipse
C    boundary      Boundary (constant,nearest,reflect,wrap)
C    constant      Constant for boundary extension
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C    status        Status flag
C
C PRIMARY LOCAL VARIABLES:
C      contxt - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo message to std error
C      subroutine uclgs_ - get parameter from par file
C
C****************************************************************************** 

      subroutine gadapt(infile,outfile,counts,smajor,ratio,theta,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

      character*(*)  infile,outfile 
      character*(*)  datatype,boundary
      integer  counts
      double precision  smajor,ratio,theta,constant,nullval
      logical  copyprime,copyall
      integer status
      character(80) contxt

C  get the name of the input FITS file
      call uclgst('infile',infile,status)
      if (status .ne. 0) then
        contxt = 'could not get infile parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfile',outfile,status)
      if (status .ne. 0) then
        contxt = 'could not get outfile parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of counts for adaptive 
      call uclgsi('counts',counts,status)
      if (status .ne. 0) then
        contxt = 'could not get counts parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of smajor for adaptive 
      call uclgsd('smajor',smajor,status)
      if (status .ne. 0) then
        contxt = 'could not get smajor parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of ratio for adaptive 
      call uclgsd('ratio',ratio,status)
      if (status .ne. 0) then
        contxt = 'could not get ratio parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of theta for adaptive 
      call uclgsd('theta',theta,status)
      if (status .ne. 0) then
        contxt = 'could not get theta parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the boundary variable type
	call uclgst('boundary', boundary, status)
	if (status .ne. 0) then
	    contxt = 'could not get boundary parameter'
	    call fcerr(contxt)
	    goto 999
        else
            call ftupch(boundary)
	endif

C  get the value of constant for boundary 
      call uclgsd('constant',constant,status)
      if (status .ne. 0) then
        contxt = 'could not get constant parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the datatype variable name
	call uclgst('datatype', datatype, status)
	if (status .ne. 0) then
	  contxt = 'could not get datatype parameter'
	  call fcerr(contxt)
	  goto 999
        else
          if (datatype .eq. ' ') datatype = '-'
          call ftupch(datatype)
	endif

C  get the value of null pixels  
      call uclgsd('nullval',nullval,status)
      if (status .ne. 0) then
        contxt = 'could not get nullval parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get whether to copy other keywords from primary array of infile
      call uclgsb('copyprime', copyprime, status)
      if (status .ne. 0) then
        contxt = 'could not get copyprime parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get whether to copy other extensions from the input file
      call uclgsb('copyall', copyall, status)
      if (status .ne. 0) then
        contxt = 'could not get copyall parameter'
        call fcerr(contxt)
        goto 999
      endif

 999  continue
      return
      end

C****************************************************************************** 
C SUBROUTINE:
C      smthadapt
C
C DESCRIPTION: 
C      Reads n-d images from input files and smooths image using
C      elliptical adaptive filter and then writes out smoothed image.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / May '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      smthadapt uses FITSIO calls to read image file
C
C USAGE:
C	call smthadapt(infile,outfile,counts,smajor,ratio,theta,
C     &               boundary,constant,datatype,nullval,copyprime,
C     &               copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    counts        Minimal number of counts within ellipse
C    smajor        Maximum semimajor axis of ellipse in pixels
C    ratio         Ratio of sigma in y to x
C    theta         Position angle of ellipse
C    boundary      Boundary (constant,nearest,reflect,wrap)
C    constant      Constant for boundary extension
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C    status        Status flag
C
C PRIMARY LOCAL VARIABLES:
C      message - error message 
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ft____ - FITSIO routines
C
C****************************************************************************** 
      subroutine smthadapt(infile,outfile,counts,smajor,ratio,theta,
     &                     boundary,constant,datatype,nullval,
     &                     copyprime,copyall,status)

      character*(*)  infile,outfile 
      character*(*)  datatype,boundary
      integer  counts, bestcnts
      double precision  smajor,ratio,theta,constant,nullval
      logical  copyprime,copyall
      integer status,ipixel,opixel
      character(160) contxt,filnam,history
      character(80)  comment
      double precision  a,b,c,f,area,oldarea,darea,bestarea
      integer  naxis1,naxis2,lrgnx,lrgny,nxk,nyk,extcnt
      integer  iunit,ounit,block,extnum,hdtype,dtype
      integer  naxis,naxes(10),bitpix,pcount,gcount,group
      integer  iarray,kernel,patch,oarray,ipxl,jpxl,cnts
      logical  simple,extend,anyf
      double precision  bscale,bzero,cursmjr,minsmjr2,maxsmjr2

C  the following MEM common block definition is in the system iraf77.inc file
C
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C note:
C       datatype        value
C       logical         1
C       integer*2       3
C       Integer         4
C       Long Integer    5
C       Real            6
C       Double          7
C       Complex         8

C initialize variables
      iunit = 15
      ounit = 16
      block = 2880
      group = 1
      comment = ' '

C  get the filename and extension
      call fcpars(infile,filnam,extnum,status)
      if (extnum .eq. -99) extnum = 0

C  open the input FITS file
      call ftopen(iunit,filnam,0,block,status)
      call ftmahd(iunit,extnum+1,hdtype,status)
      if (status .ne. 0) then
         contxt = 'unable to open infile'
         call fcerr(contxt)
         call fcerrm (status)
         return
      endif

C check that requested extension is an image
      call ftghdt(iunit, hdtype, status)
      if (hdtype .ne. 0) then
        contxt = ' requested extension is not an image'
        call fcerr(contxt)
        goto 999
      endif

C get the image keywords
      call ftghpr(iunit,10,simple,bitpix,naxis,naxes,pcount,gcount,
     &            extend,status)

C determine the data type for the output image
      if (datatype .eq. '-') then
        dtype = bitpix
      else if (datatype .eq. 'B') then
        dtype = 8
      else if (datatype .eq. 'I') then
        dtype = 16
      else if (datatype .eq. 'J') then
        dtype = 32
      else if (datatype .eq. 'E') then
        dtype = -32
      else if (datatype .eq. 'D') then
        dtype = -64
      else
        contxt = ' Unsupported datatype requested for smoothed image'
        call fcerr(contxt)
        goto 999
      endif

C verify that this is a 2D image
      if (naxis .ne. 2) then
        contxt = ' Only 2D images are supported'
        call fcerr(contxt)
        goto 999
      endif

C set the x and y dimension of the image
      naxis1 = naxes(1)
      naxis2 = naxes(2)

C calculate the maximum elliptical adaptive parameters from user inputs
      call eladpar(smajor,ratio,theta,a,b,c,f,lrgnx,lrgny,status)

C verify that the kernel is less than twice the image size
      if ( (lrgnx/2) .gt. naxis1) then
         contxt = 'max x dimen of kernel more than twice that of image'
         call fcerr(contxt)
         contxt = 'try making max kernel smaller using smaller smajor'
         call fcerr(contxt)
         goto 999
      endif
      if ( (lrgny/2) .gt. naxis2) then
         contxt = 'max y dimen of kernel more than twice that of image'
         call fcerr(contxt)
         contxt = 'try making max kernel smaller using smaller smajor'
         call fcerr(contxt)
         goto 999
      endif

C allocate the dynamic memory for input image
      iarray = 0
      call udmget (naxis1*naxis2, 7, iarray, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for input'
         call fcerr (contxt)
         goto 998
      endif

C allocate the dynamic memory for kernel
      kernel = 0
      call udmget (lrgnx*lrgny, 4, kernel, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for kernel'
         call fcerr (contxt)
         goto 998
      endif

C allocate the dynamic memory for patch
      patch = 0
      call udmget (lrgnx*lrgny, 7, patch, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for patch'
         call fcerr (contxt)
         goto 998
      endif

C allocate the dynamic memory for output image
      oarray = 0
      call udmget (naxis1*naxis2, 7, oarray, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for output'
         call fcerr (contxt)
         goto 998
      endif

C read in the bscale keyword value
      call ftgkyd(iunit,'BSCALE',bscale,comment,status)
      if (status .eq. 202) then
        bscale = 1.0d0
        status = 0
      endif

C read in the bzero keyword value
      call ftgkyd(iunit,'BZERO',bzero,comment,status)
      if (status .eq. 202) then
        bzero = 0.0d0
        status = 0
      endif

C get the image from input FITS file as doubles
      call ftg2dd(iunit,group,nullval,naxis1,naxis1,naxis2,
     &            MEMD(iarray),anyf,status)

C move back to beginning of input FITS file
      call ftmahd(iunit,1,hdtype,status)

C initialize the output FITS file      
      call ffinit(ounit,outfile,status)
      if (status .ne. 0) then
         contxt = ' could not open outfile may already exist?'
         call fcerr(contxt)
         goto 999
      endif

C pointer to location of the first pixel in dynamic memory array
      ipixel = iarray - 1
      opixel = oarray - 1

C     initial guess for the semi major axis
      cursmjr = smajor / 1.414213563d0

C loop through all pixels in the input image
      do 60 jpxl = 1, naxis2
      do 50 ipxl = 1, naxis1

C get the pixel value and set initial area to 1 pixel
      ipixel = ipixel + 1
      opixel = opixel + 1
      bestcnts = MEMD(ipixel)
      bestarea = 1.0d0

C simply copy the pixel value to the output image if above the threshold level
      if (bestcnts .ge. counts) go to 30

C pixel is below the threshold; initialize the best counts to a magic value
      bestcnts = 100000001

C work within the area parameter space since counts are more linear there
      minsmjr2 = 0.0d0
      maxsmjr2 = smajor * smajor
      area = 0.0d0

C do a binary search through the semi-major axis space

C  Note: the first time through this loop for each pixel, the previous
C  value of cursmjr is used.
   10  oldarea = area

C calculate the current elliptical adaptive parameters from user inputs
      call eladpar(cursmjr,ratio,theta,a,b,c,f,nxk,nyk,status)

C calculate the elliptical adaptive kernel
      call eladker(MEMI(kernel),nxk,nyk,a,b,c,f,area)

C calculate the patch of image to be folded with kernal
      call bldpatch(ipxl,jpxl,MEMD(iarray),naxis1,naxis2,
     &              MEMD(patch),nxk,nyk,boundary,constant)

C calculate the integrand
      call kintgrd(MEMI(kernel),MEMD(patch),nxk,nyk,cnts)

C save the values that are closest to (and greater than) the desired counts
      if (cnts .ge. counts .and. cnts .le. bestcnts) then
         bestcnts = cnts
         bestarea = area
      end if

C decide how to proceed:
      darea = abs( oldarea - area )

C terminate the search if there is no change in area ( < 1 pixel)
      if ( darea .lt. 0.5d0) then

C The search of semi-major axis parameter space has converged.
C If the minimum number of counts was not found within even
C the largest ellipse, then take the last counts and area value
C as the best estimate.

        if (bestcnts .eq. 100000001)then
           bestcnts = cnts
           bestarea = area
        end if

        goto 30
      else
        if ( cnts .lt. counts) then
          minsmjr2 = cursmjr * cursmjr
          cursmjr = sqrt( (maxsmjr2 + minsmjr2) / 2.0d0 )
          goto 10
        else if ( cnts .gt. counts) then
          maxsmjr2 = cursmjr * cursmjr
          cursmjr = sqrt( (maxsmjr2 + minsmjr2) / 2.0d0 )
          goto 10
        else if ( cnts .eq. counts) then
C         got exactly the right number of counts, so exit
          goto 30
        endif
      endif

C write the output pixel value
  30   MEMD(opixel) = bestcnts / bestarea

C start over with next pixel in the input image
  50  continue

C % complete counter
      call xclock(jpxl, naxis2, 1)

  60  continue

C put image into correct HDU based on parameters
      if ((extnum .eq. 0) .and. (.not. copyprime) .and.
     &    (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
        history = 'TASK: FADAPT on FILENAME: ' // filnam
        call ftphis(ounit,history,status)

      else if ((extnum .eq. 0) .and. (copyprime) .and. 
     &         (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        history = 'TASK: FADAPT on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)

      else if ((extnum .eq. 0) .and. (.not. copyprime) .and. 
     &         (copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        history = 'TASK: FADAPT on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
 100    call ftmrhd(iunit,1,hdtype,status)
        if (status .ne. 0) then 
          status = 0
          goto 999
        endif
        call ftcrhd(ounit,status)
        call ftcopy(iunit,ounit,0,status)
        goto 100

      else if ((extnum .eq. 0) .and. (copyprime) .and. 
     &         (copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        history = 'TASK: FADAPT on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
 200    call ftmrhd(iunit,1,hdtype,status)
        if (status .ne. 0) then 
          status = 0
          goto 999
        endif
        call ftcrhd(ounit,status)
        call ftcopy(iunit,ounit,0,status)
        goto 200

      else if ((extnum .ne. 0) .and. (.not. copyprime) .and.
     &         (.not. copyall)) then

        call ftmahd(iunit,extnum+1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
        history = 'TASK: FADAPT on FILENAME: ' // filnam
        call ftphis(ounit,history,status)

      else if ((extnum .ne. 0) .and. (copyprime) .and. 
     &         (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftcopy(iunit,ounit,0,status)
        call ftmrhd(iunit,extnum,hdtype,status)
        call ftcrhd(ounit,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        history = 'TASK: FADAPT on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)

      else if ((extnum .ne. 0) .and. (.not. copyprime) .and. 
     &         (copyall)) then

        call ftmahd(iunit,extnum+1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
        history = 'TASK: FADAPT on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        extcnt = 0
        call ftmahd(iunit,1,hdtype,status)
 300    call ftmrhd(iunit,1,hdtype,status)
        if (status .ne. 0) then 
          status = 0
          goto 999
        end if
        call ftcrhd(ounit,status)
        extcnt = extcnt + 1
        if (extcnt .ne. extnum) then
          call ftcopy(iunit,ounit,0,status)
        else
          call ftmahd(iunit,1,hdtype,status)
          call ftcopy(iunit,ounit,0,status)
          call ftmahd(iunit,extcnt+1,hdtype,status)
        endif
        goto 300

      else if ((extnum .ne. 0) .and. (copyprime) .and. 
     &         (copyall)) then

        extcnt = 0
        call ftmahd(iunit,1,hdtype,status)
        call ftcopy(iunit,ounit,0,status)
 400    call ftmrhd(iunit,1,hdtype,status)
        if (status .ne. 0) then 
          status = 0
          goto 999
        end if
        call ftcrhd(ounit,status)
        extcnt = extcnt + 1
        if (extcnt .ne. extnum) then
          call ftcopy(iunit,ounit,0,status)
        else
          call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &                extend,status)
          call xcopyscale (iunit, ounit, status)
          history = 'TASK: FADAPT on FILENAME: ' // filnam
          call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
        endif
        goto 400
      end if

C report possible alternatives when memory allocation fails
      goto 999
 998  contxt = '  When memory allocation falls you can:'
      call fcecho(contxt)
      contxt = '  1) try again when system load is lighter'
      call fcecho(contxt)
      contxt = '  2) try again with smaller sigma and/or nsigma'
      call fcecho(contxt)
      contxt = '  3) try again with smaller image'
      call fcecho(contxt)

 999  if (status .ne. 0) then
         call fcerrm(status)
         status = 0
      endif
      call ftclos(iunit,status)
      status = 0
      call ftclos(ounit,status)

C free the dynamic memory
      call udmfre (iarray, 7, status)
      call udmfre (kernel, 4, status)
      call udmfre (patch, 7, status)
      call udmfre (oarray, 7, status)
      return
      end

C****************************************************************************** 
C SUBROUTINE:
C      eladpar
C
C DESCRIPTION: 
C      Compute the elliptical adaptive parameters from user inputs
C      for both 1D and 2D adaptives.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / May '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C       The ratio must be 0.0 for a 1D adaptive
C       nx and ny will be forced to be odd
C
C USAGE:
C	call eladpar(smajor,ratio,theta,a,b,c,f,nx,ny,stat)
C
C ARGUMENTS:
C    smajor        Semi-major axis of ellipse
C    ratio         Ratio of semi-major to semi-minor
C    theta         Position angle of ellipse
C    a,b,c,f       Ellipse parameters
C    nx,ny         kernel dimensions
C    stat          Status flag
C
C PRIMARY LOCAL VARIABLES:
C    message       Message string
C
C CALLED ROUTINES:
C    fcerr         Report error messages
C
C****************************************************************************** 
      subroutine eladpar(smajor,ratio,theta,a,b,c,f,nx,ny,stat)
      double precision  smajor,ratio,theta,a,b,c,f
      integer  nx,ny,stat
      double precision  sx2,sy2,cost,sint,discrim,deg2rad,eps
      parameter (deg2rad = 0.0174532925)
      parameter (eps = 1.0d-07)
      logical  double_eq
      character(80)  message

C     treat the most common circular case first, for efficiency
      if (ratio .eq. 1.0)then
          a = 1.0d0 / (smajor * smajor)
          b = 0.0d0
          c = a
          f = 0.5
          discrim =  - 4.0d0 * a * c
          nx = 2.0d0 * smajor + 1.0d0
          if ( mod(nx,2) .eq. 0 ) nx = nx + 1
          ny = nx
          return
      end if
         
      sx2 = smajor ** 2
      sy2 = (ratio * smajor) ** 2
      cost = cos(deg2rad * theta)
      sint = sin(deg2rad * theta)
C     * 1D adaptive has ratio of 0.0 *
      if ( double_eq(ratio,0.0d0,eps) ) then
        f = 0.5d0
        nx = 2.0d0 * smajor * abs(cost) + 1.0d0
        ny = 2.0d0 * smajor * abs(sint) + 1.0d0
        if ( double_eq(theta,0.0d0,eps) .or. 
     &       double_eq(theta,180.0d0,eps) ) then
          a = 1.0d0 / sx2
          b = 0.0d0
          c = 0.0d0
        else if ( double_eq(theta,90.0d0,eps) ) then
          a = 0.0d0
          b = 0.0d0
          c = 1.0d0 / sx2
        else
          stat = 1001
          message = 'Cannot make 1D adaptive with this theta'
          call fcerr(message)
        endif
C     * All other ratios are 2D adaptives *
      else
        a = (cost ** 2) / sx2 + (sint ** 2) / sy2
        b = 2.0d0 * ( 1.0d0 / sx2 - 1.0d0 / sy2 ) * cost * sint
        c = (sint ** 2) / sx2  + (cost ** 2) / sy2
        discrim = b ** 2 - 4.0d0 * a * c
        f = 0.5d0
        nx = 2.0d0 * sqrt( -8.0d0 * c * f / discrim ) + 1.0d0
        ny = 2.0d0 * sqrt( -8.0d0 * a * f / discrim ) + 1.0d0
      end if
C     * force kernel size to next nearest odd integer *
      if ( mod(nx,2) .eq. 0 ) nx = nx + 1
      if ( mod(ny,2) .eq. 0 ) ny = ny + 1

      return
      end

C****************************************************************************** 
C FUNCTION:
C      double_eq
C
C DESCRIPTION: 
C      Compares 2 floating point numbers for "near" equality
C
C AUTHOR:  
C      Dr. James Kent Blackburn / May '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C       returns a boolean
C
C USAGE:
C	double_eq(float1,float2,epsilon)
C
C ARGUMENTS:
C    float1        First floating point number
C    float2        Second floating point number
C    epsilon       nearness of two floats for a true result
C
C PRIMARY LOCAL VARIABLES:
C    result        boolean result of comparison
C
C CALLED ROUTINES:
C
C****************************************************************************** 

      logical function double_eq(float1,float2,epsilon)
      double precision float1,float2,epsilon
      logical result

      result = .false.
      if ( abs(float1 - float2) .lt. epsilon ) result = .true.
      double_eq = result
      return
      end

C****************************************************************************** 
C SUBROUTINE:
C      eladker
C
C DESCRIPTION: 
C      Compute the elliptical adaptive parameters from user inputs
C      for both 1D and 2D adaptives.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / May '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C       The ratio must be 0.0 for a 1D adaptive
C       nx and ny will be forced to be odd
C
C USAGE:
C	call eladker(kernel,nx,ny,a,b,c,f,norm)
C
C ARGUMENTS:
C    kernel        Elliptical adaptive kernel array
C    nx,ny         kernel dimensions
C    a,b,c,f       Ellipse parameters
C    norm          Area in kernal, used in normalization
C
C PRIMARY LOCAL VARIABLES:
C    i,j,x,y       Integer counters
C    x0,y0         Position of center pixel
C    norm          Normalization for kernel
C    temp          Temporary storage variable
C
C CALLED ROUTINES:
C
C****************************************************************************** 
      subroutine eladker(kernel,nx,ny,a,b,c,f,norm)
      integer  nx,ny
      integer  kernel(nx,ny)
      double precision  a,b,c,f
      double precision  norm,temp,temp1
      integer  i,j,x0,y0,x,y,i2,j2

      x0 = nx / 2 + 1
      y0 = ny / 2 + 1
      norm = 0.0d0

C  do the more common circular case separately, for efficiency
      if (nx .eq. ny .and. a .eq. c)then
        temp1 = 0.5 * a

C  do one quadrant of the circle and fold to the other 3 quandrants
        do 120 j = 1, y0 - 1
          y = j - y0
          j2 = ny - j + 1

          do 110 i = 1, x0 - 1
            x = i - x0
            i2 = nx - i + 1

            temp = temp1 * (x**2 + y**2)
            if ( temp .le. f ) then
              norm = norm + 4.0d0
              kernel(i,j) = 1
              kernel(i,j2) = 1
              kernel(i2,j) = 1
              kernel(i2,j2) = 1
            else
              kernel(i,j) = 0
              kernel(i,j2) = 0
              kernel(i2,j) = 0
              kernel(i2,j2) = 0
            end if
 110      continue
 120    continue

C do the central horizontal and vertical line of pixels thru the circle
        do 130 j = 1, ny
           y = j - y0
           temp = temp1 * y**2
           if (temp .le. f)then
             kernel(x0,j) = 1
             kernel(j,y0) = 1
             norm = norm + 2.0d0
           else
             kernel(x0,j) = 0
             kernel(j,y0) = 0
           end if
 130    continue

C the central pixel was counted twice
        norm = norm - 1.0d0
        return
      end if

C  this section is for the non-circular case
      do 20 j = 1, ny
        y = j - y0
        do 10 i = 1, nx
          x = i - x0
          temp = 0.5d0 * (a*x**2 + c*y**2 + b*x*y)
          if ( temp .le. f ) then
            kernel(i,j) = 1
            norm = norm + 1
          else
            kernel(i,j) = 0
          end if
  10    continue
  20  continue
      
      return
      end


C****************************************************************************** 
C SUBROUTINE:
C      kintgrd
C
C DESCRIPTION: 
C      sum up the counts in the elliptical area
C
C AUTHOR:  
C      Dr. James Kent Blackburn / May '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call kintgrd(kernal,patch,nxk,nyk,cnts)
C
C ARGUMENTS:
C    kernel        Elliptical adaptive kernel array
C    patch         subset of image
C    nxk,nyk       dimensions of kernal and path array
C    cnts          counts in the elliptical area
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C****************************************************************************** 
      subroutine kintgrd(kernal,patch,nxk,nyk,cnts)
      integer  nxk,nyk,cnts
      integer  kernal(nxk,nyk)
      double precision  patch(nxk,nyk)
      integer i,j

      cnts = 0
      do 20 j=1,nyk
      do 10 i=1,nxk
        if (kernal(i,j) .gt. 0) then
          cnts = cnts + patch(i,j)
        endif
  10  continue
  20  continue
      return
      end


C****************************************************************************** 
C SUBROUTINE:
C      bldpatch
C
C DESCRIPTION: 
C      make the image patch that overlaps with the kernal using the
C      appropriate boundary condition
C
C AUTHOR:  
C      Dr. James Kent Blackburn / May '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call bldpatch(i,j,inimag,nxi,nyi,
C     &              patch,nxk,nyk,boundary,constant)
C
C ARGUMENTS:
C    i,j           central pixel position in image
C    inimag        input image to take patch from
C    nxi,nyi       dimension of input image
C    patch         subset of image
C    nxk,nyk       dimensions of patch array
C    boundary      type of boundary condition
C    constant      value for constant boundary condition
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C****************************************************************************** 
      subroutine bldpatch(i,j,inimag,nxi,nyi,
     &              patch,nxk,nyk,boundary,constant)
      integer  i,j,nxi,nyi,nxk,nyk
      double precision  inimag(nxi,nyi)
      double precision  patch(nxk,nyk),constant
      character*(*)  boundary
      integer npx,npy,nxpat,nypat,imgx,imgy
      logical constflg

      imgx = 0
      imgy = 0
      constflg = .false.
      nypat = 1
      do 25 npy = j - nyk/2, j + nyk/2
        if ((npy .ge. 1) .and. (npy .le. nyi)) then
          imgy = npy
        else if (npy .lt. 1) then
          if ( boundary .eq. 'CONSTANT' ) then
            constflg = .true.
          else if ( boundary .eq. 'NEAREST' ) then
            imgy = 1
          else if ( boundary .eq. 'REFLECT' ) then
            imgy = 1 - npy
          else if ( boundary .eq. 'WRAP' ) then
            imgy = nyi + npy
          end if
        else if (npy .gt. nyi) then
          if ( boundary .eq. 'CONSTANT' ) then
            constflg = .true.
          else if ( boundary .eq. 'NEAREST' ) then
            imgy = nyi
          else if ( boundary .eq. 'REFLECT' ) then
            imgy = nyi - (npy - nyi)
          else if ( boundary .eq. 'WRAP' ) then
            imgy = 1 + (npy - nyi)
          end if
        endif
      nxpat = 1
      do 15 npx = i - nxk/2, i + nxk/2
        if ((npx .ge. 1) .and. (npx .le. nxi)) then
          imgx = npx
        else if (npx .lt. 1) then
          if ( boundary .eq. 'CONSTANT' ) then
            constflg = .true.
          else if ( boundary .eq. 'NEAREST' ) then
            imgx = 1
          else if ( boundary .eq. 'REFLECT' ) then
            imgx = 1 - npx
          else if ( boundary .eq. 'WRAP' ) then
            imgx = nxi + npx
          end if
        else if (npx .gt. nxi) then
          if ( boundary .eq. 'CONSTANT' ) then
            constflg = .true.
          else if ( boundary .eq. 'NEAREST' ) then
            imgx = nxi
          else if ( boundary .eq. 'REFLECT' ) then
            imgx = nxi - (npx - nxi)
          else if ( boundary .eq. 'WRAP' ) then
            imgx = 1 + (npx - nxi)
          end if
        endif
        if ( constflg ) then
          patch(nxpat,nypat) = constant
          constflg = .false.
        else
          patch(nxpat,nypat) = inimag(imgx,imgy)
        endif
        nxpat = nxpat + 1
  15  continue
      nypat = nypat + 1
  25  continue

      return
      end
