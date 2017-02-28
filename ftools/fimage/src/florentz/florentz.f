**************************************************************************** 
C FTOOLS FIMAGE TASK:
C      florentz 
C
C FILE:
C      florentz.f 
C
C DESCRIPTION: 
C      Performs elliptical lorentzian smoothing of a FITS image.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C     10/17/97 PDW 2.9b - Replace old get header routines
C     12/03/97 PDW 2.9c - Introduce clobber parameter... sort of...
C                         call ffinit--an ftool utility--instead of ftinit
C     12/29/99 NG  2.9d - Updates for compressed images.
C
C NOTES:
C      florentz is supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    sigma         Sigma of lorentzian along major axis of ellipse
C    ratio         Ratio of sigma in y to x
C    theta         Position angle of ellipse
C    nsigma        Extent of lorentzian kernel in sigma
C    boundary      Boundary (constant,nearest,reflect,wrap)
C    constant      Constant for boundary extension
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C
C CALLED ROUTINES:
C      subroutine glorentz - gets parameters from environment
C      subroutine smthlorentz - smooth/write output image.
C
C****************************************************************************** 
      subroutine florez

      character(160)  infile,outfile 
      character(20)  datatype,boundary

      double precision  sigma,ratio,theta,nsigma,constant,nullval

      logical  copyprime,copyall

      integer  status

      character(40) taskname
      common /task/ taskname

      taskname = 'florentz2.9d'
      status = 0

C     * Get the parameters from the par file *
      call glorentz(infile,outfile,sigma,ratio,theta,nsigma,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

C     * Pass input parameters to core of smoothing software *
      call smthlorentz(infile,outfile,sigma,ratio,theta,nsigma,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

      return
      end

C****************************************************************************** 
C SUBROUTINE:
C     glorentz 
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
C      glorentz uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call glorentz(infile,outfile,sigma,ratio,theta,nsigma,
C     &            boundary,constant,datatype,nullval,copyprime,
C     &            copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    sigma         Sigma of lorentzian along major axis of ellipse
C    ratio         Ratio of sigma in y to x
C    theta         Position angle of ellipse
C    nsigma        Extent of lorentzian kernel in sigma
C    boundary      Boundary (constant,nearest,reflect,wrap)
C    constant      Constant for boundary extension
C    datatype      Data type of output image"
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

      subroutine glorentz(infile,outfile,sigma,ratio,theta,nsigma,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

      character*(*)  infile,outfile 
      character*(*)  datatype,boundary
      double precision  sigma,ratio,theta,nsigma,constant,nullval
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

C  get the value of sigma for lorentzian 
      call uclgsd('sigma',sigma,status)
      if (status .ne. 0) then
        contxt = 'could not get sigma parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of ratio for lorentzian 
      call uclgsd('ratio',ratio,status)
      if (status .ne. 0) then
        contxt = 'could not get ratio parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of theta for lorentzian 
      call uclgsd('theta',theta,status)
      if (status .ne. 0) then
        contxt = 'could not get theta parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of nsigma for lorentzian 
      call uclgsd('nsigma',nsigma,status)
      if (status .ne. 0) then
        contxt = 'could not get nsigma parameter'
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
C      smthlorentz
C
C DESCRIPTION: 
C      Reads n-d images from input files and smooths image using
C      elliptical lorentzian filter and then writes out smoothed image.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      smthlorentz uses FITSIO calls to read image file
C
C USAGE:
C	call smthlorentz(infile,outfile,sigma,ratio,theta,nsigma,
C     &               boundary,constant,datatype,nullval,copyprime,
C     &               copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    sigma         Sigma of lorentzian along major axis of ellipse
C    ratio         Ratio of sigma in y to x
C    theta         Position angle of ellipse
C    nsigma        Extent of lorentzian kernel in sigma
C    boundary      Boundary (constant,nearest,reflect,wrap)
C    constant      Constant for boundary extension
C    datatype      Data type of output image"
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
      subroutine smthlorentz(infile,outfile,sigma,ratio,theta,nsigma,
     &                     boundary,constant,datatype,nullval,
     &                     copyprime,copyall,status)

      character*(*)  infile,outfile 
      character*(*)  datatype,boundary
      double precision  sigma,ratio,theta,nsigma,constant,nullval
      logical  copyprime,copyall
      integer status
      character(160) contxt,filnam,history
      character(80)  comment
      double precision  a,b,c,f
      integer  naxis1,naxis2,nxk,nyk,extcnt
      integer  iunit,ounit,block,extnum,hdtype,dtype
      integer  naxis,naxes(10),bitpix,pcount,gcount,group
      integer  iarray,kernel,patch,oarray
      logical  simple,extend,anyf
      double precision  bscale,bzero

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
      end if

C verify that this is a 2D image
      if (naxis .ne. 2) then
        contxt = ' Only 2D images are supported'
        call fcerr(contxt)
        goto 999
      endif

C set the x and y dimension of the image
      naxis1 = naxes(1)
      naxis2 = naxes(2)

C calculate the elliptical lorentzian parameters from user inputs
      call ellzpar(sigma,ratio,theta,nsigma,a,b,c,f,nxk,nyk,status)

C verify that the kernel is less than twice the image size
      if ( (nxk/2) .gt. naxis1) then
         contxt = ' x dimension of kernel more than twice that of image'
         call fcerr(contxt)
         contxt = ' try making kernel smaller using smaller nsigma'
         call fcerr(contxt)
         goto 999
      endif
      if ( (nyk/2) .gt. naxis2) then
         contxt = ' y dimension of kernel more than twice that of image'
         call fcerr(contxt)
         contxt = ' try making kernel smaller using smaller nsigma'
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
      call udmget (nxk*nyk, 7, kernel, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for kernel'
         call fcerr (contxt)
         goto 998
      endif

C allocate the dynamic memory for patch
      patch = 0
      call udmget (nxk*nyk, 7, patch, status)
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

C calculate the elliptical lorentzian kernel
      call ellzker(MEMD(kernel),nxk,nyk,a,b,c,f)

C convolve the elliptical lorentzian kernel with the image
      call convolve(MEMD(iarray),naxis1,naxis2,MEMD(kernel),
     &              MEMD(patch),nxk,nyk,boundary,
     &              constant,MEMD(oarray))

C put image into correct HDU based on parameters
      if ((extnum .eq. 0) .and. (.not. copyprime) .and.
     &    (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
        history = 'TASK: FLORENTZ on FILENAME: ' // filnam
        call ftphis(ounit,history,status)

      else if ((extnum .eq. 0) .and. (copyprime) .and. 
     &         (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        history = 'TASK: FLORENTZ on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)

      else if ((extnum .eq. 0) .and. (.not. copyprime) .and. 
     &         (copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        history = 'TASK: FLORENTZ on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
 100    call ftmrhd(iunit,1,hdtype,status)
        if (status .ne. 0) then 
          status = 0
          goto 999
        end if
        call ftcrhd(ounit,status)
        call ftcopy(iunit,ounit,0,status)
        goto 100

      else if ((extnum .eq. 0) .and. (copyprime) .and. 
     &         (copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        history = 'TASK: FLORENTZ on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
 200    call ftmrhd(iunit,1,hdtype,status)
        if (status .ne. 0) then 
          status = 0
          goto 999
        end if
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
        history = 'TASK: FLORENTZ on FILENAME: ' // filnam
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
        history = 'TASK: FLORENTZ on FILENAME: ' // filnam
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
        history = 'TASK: FLORENTZ on FILENAME: ' // filnam
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
          history = 'TASK: FLORENTZ on FILENAME: ' // filnam
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
      call udmfre (kernel, 7, status)
      call udmfre (patch, 7, status)
      call udmfre (oarray, 7, status)
      return
      end

C****************************************************************************** 
C SUBROUTINE:
C      ellzpar
C
C DESCRIPTION: 
C      Compute the elliptical lorentzian parameters from user inputs
C      for both 1D and 2D lorentzians.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C       The ratio must be 0.0 for a 1D lorentzian
C       nx and ny will be forced to be odd
C
C USAGE:
C	call ellzpar(sigma,ratio,theta,nsigma,a,b,c,f,nx,ny,stat)
C
C ARGUMENTS:
C    sigma         Sigma of lorentzian along major axis of ellipse
C    ratio         Ratio of sigma in y to x
C    theta         Position angle of ellipse
C    nsigma        Extent of lorentzian kernel in sigma
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
      subroutine ellzpar(sigma,ratio,theta,nsigma,a,b,c,f,nx,ny,stat)
      double precision  sigma,ratio,theta,nsigma,a,b,c,f
      integer  nx,ny,stat
      double precision  sx2,sy2,cost,sint,discrim,deg2rad,eps
      parameter (deg2rad = 0.0174532925)
      parameter (eps = 1.0d-07)
      logical  dbl_pt_eq
      character(80)  message

      sx2 = sigma ** 2
      sy2 = (ratio * sigma) ** 2
      cost = cos(deg2rad * theta)
      sint = sin(deg2rad * theta)
C     * 1D lorentzian has ratio of 0.0 *
      if ( dbl_pt_eq(ratio,0.0d0,eps) ) then
        f = (nsigma ** 2) / 2.0d0
        nx = 2.0d0 * sigma * nsigma * abs(cost) + 1.0d0
        ny = 2.0d0 * sigma * nsigma * abs(sint) + 1.0d0
        if ( dbl_pt_eq(theta,0.0d0,eps) .or. 
     &       dbl_pt_eq(theta,180.0d0,eps) ) then
          a = 1.0d0 / sx2
          b = 0.0d0
          c = 0.0d0
        else if ( dbl_pt_eq(theta,90.0d0,eps) ) then
          a = 0.0d0
          b = 0.0d0
          c = 1.0d0 / sx2
        else
          stat = 1001
          message = 'Cannot make 1D Lorentzian with this theta'
          call fcerr(message)
        endif
C     * All other ratios are 2D lorentzians *
      else
        a = (cost ** 2) / sx2 + (sint ** 2) / sy2
        b = 2.0d0 * ( 1.0d0 / sx2 - 1.0d0 / sy2 ) * cost * sint
        c = (sint ** 2) / sx2  + (cost ** 2) / sy2
        discrim = b ** 2 - 4.0d0 * a * c
        f = (nsigma ** 2) / 2.0d0
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
C      dbl_pt_eq
C
C DESCRIPTION: 
C      Compares 2 floating point numbers for "near" equality
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C       returns a boolean
C
C USAGE:
C	dbl_pt_eq(float1,float2,epsilon)
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

      logical function dbl_pt_eq(float1,float2,epsilon)
      double precision float1,float2,epsilon
      logical result

      result = .false.
      if ( abs(float1 - float2) .lt. epsilon ) result = .true.
      dbl_pt_eq = result
      return
      end

C****************************************************************************** 
C SUBROUTINE:
C      ellzker
C
C DESCRIPTION: 
C      Compute the elliptical lorentzian parameters from user inputs
C      for both 1D and 2D lorentzians.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C       The ratio must be 0.0 for a 1D lorentzian
C       nx and ny will be forced to be odd
C
C USAGE:
C	call ellzker(kernel,nx,ny,a,b,c,f)
C
C ARGUMENTS:
C    kernel        Elliptical lorentzian kernel array
C    nx,ny         kernel dimensions
C    a,b,c,f       Ellipse parameters
C
C PRIMARY LOCAL VARIABLES:
C    i,j,x,y       Integer counters
C    x0,y0         Position of center pixel
C    norm          Normalization for kernel
C
C CALLED ROUTINES:
C
C****************************************************************************** 
      subroutine ellzker(kernel,nx,ny,a,b,c,f)
      integer  nx,ny
      double precision  kernel(nx,ny),a,b,c,f
      double precision norm,z
      integer i,j,x0,y0,x,y

      x0 = nx / 2 + 1
      y0 = ny / 2 + 1
      norm = 0.0d0

      do 20 j = 1, ny
        y = j - y0
        do 10 i = 1, nx
          x = i - x0
          z = (a*x**2 + c*y**2 + b*x*y)
          if ( (0.5d0 * z) .le. f ) then
            kernel(i,j) = 1.0d0 / (1.0d0 + z)
            norm = norm + kernel(i,j)
          else
            kernel(i,j) = 0.0d0
          end if
  10    continue
  20  continue
      if ( norm .gt. 0.0d0 ) then
        do 40 j = 1, ny
          do 30 i = 1, nx
            kernel(i,j) = kernel(i,j) / norm
  30      continue
  40    continue
      endif

      return
      end
