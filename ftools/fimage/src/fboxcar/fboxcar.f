**************************************************************************** 
C FTOOLS FIMAGE TASK:
C      fboxcar 
C
C FILE:
C      fboxcar.f 
C
C DESCRIPTION: 
C      Performs rectangular boxcar smoothing of a FITS image.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C     10/17/97 PDW 2.9a - Replace old get header routines
C     12/03/97 PDW 2.9b - Introduce clobber parameter... sort of...
C                         call ffinit--an ftool utility--instead of ftinit
C     12/29/99 NG  2.9c - Updates for the compressed images.
C
C NOTES:
C      fboxcar is supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xwindow       Boxcar size in x direction in pixels
C    ywindow       Boxcar size in x direction in pixels
C    boundary      Boundary (constant,nearest,reflect,wrap)
C    constant      Constant for boundary extension
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C
C CALLED ROUTINES:
C      subroutine gboxcar - gets parameters from environment
C      subroutine smthbxcr - smooth/write output image.
C
C****************************************************************************** 
      subroutine fboxcr

      character(160)  infile,outfile 
      character(20)  datatype,boundary

      double precision  constant,nullval

      logical  copyprime,copyall

      integer  xwindow,ywindow,status

      character(40) taskname
      common /task/ taskname

      taskname = 'fboxcar2.9c'
      status = 0

C     * Get the parameters from the par file *
      call gboxcar(infile,outfile,xwindow,ywindow,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

C     * Pass input parameters to core of smoothing software *
      call smthbxcr(infile,outfile,xwindow,ywindow,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

      return
      end

C****************************************************************************** 
C SUBROUTINE:
C     gboxcar 
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
C      gboxcar uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gboxcar(infile,outfile,xwindow,ywindow,
C     &             boundary,constant,datatype,nullval,copyprime,
C     &             copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xwindow       Boxcar size in x direction in pixels
C    ywindow       Boxcar size in x direction in pixels
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

      subroutine gboxcar(infile,outfile,xwindow,ywindow,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

      character*(*)  infile,outfile 
      character*(*)  datatype,boundary
      double precision  constant,nullval
      logical  copyprime,copyall
      integer  xwindow,ywindow,status
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

C  get the value of xwindow for boxcar 
      call uclgsi('xwindow',xwindow,status)
      if (status .ne. 0) then
        contxt = 'could not get xwindow parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of ywindow for boxcar 
      call uclgsi('ywindow',ywindow,status)
      if (status .ne. 0) then
        contxt = 'could not get ywindow parameter'
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
C      smthbxcr
C
C DESCRIPTION: 
C      Reads n-d images from input files and smooths image using
C      rectangular boxcar filter and then writes out smoothed image.
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      smthbxcr uses FITSIO calls to read image file
C
C USAGE:
C	call smthbxcr(infile,outfile,xwindow,ywindow,
C     &               boundary,constant,datatype,nullval,copyprime,
C     &               copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xwindow       Boxcar size in x direction in pixels
C    ywindow       Boxcar size in x direction in pixels
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
      subroutine smthbxcr(infile,outfile,xwindow,ywindow,
     &                    boundary,constant,datatype,nullval,
     &                    copyprime,copyall,status)

      character*(*)  infile,outfile 
      character*(*)  datatype,boundary
      double precision  constant,nullval
      logical  copyprime,copyall
      integer xwindow,ywindow,status
      character(160) contxt,filnam,history
      character(80)  comment
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
C       data_type       value
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

C calculate the rectangular boxcar parameters from user inputs
      call bxcrpar(xwindow,ywindow,nxk,nyk)

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

C calculate the rectangular boxcar kernel
      call bxcrker(MEMD(kernel),nxk,nyk)

C convolve the rectangular boxcar kernel with the image
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
        history = 'TASK: FBOXCAR on FILENAME: ' // filnam
        call ftphis(ounit,history,status)

      else if ((extnum .eq. 0) .and. (copyprime) .and. 
     &         (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        history = 'TASK: FBOXCAR on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)

      else if ((extnum .eq. 0) .and. (.not. copyprime) .and. 
     &         (copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        history = 'TASK: FBOXCAR on FILENAME: ' // filnam
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
        history = 'TASK: FBOXCAR on FILENAME: ' // filnam
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
        history = 'TASK: FBOXCAR on FILENAME: ' // filnam
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
        history = 'TASK: FBOXCAR on FILENAME: ' // filnam
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
        history = 'TASK: FBOXCAR on FILENAME: ' // filnam
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
          history = 'TASK: FBOXCAR on FILENAME: ' // filnam
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
C      bxcrpar
C
C DESCRIPTION: 
C      Compute the rectangular boxcar parameters from user inputs
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call bxcrpar(xwindow,ywindow,nx,ny)
C
C ARGUMENTS:
C    xwindow       Boxcar size in x direction in pixels
C    ywindow       Boxcar size in x direction in pixels
C    nx,ny         kernel dimensions
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C****************************************************************************** 
      subroutine bxcrpar(xwindow,ywindow,nx,ny)
      integer  xwindow,ywindow,nx,ny

      nx = xwindow
      ny = ywindow

C     * force kernel size to next nearest odd integer *
      if ( mod(nx,2) .eq. 0 ) nx = nx + 1
      if ( mod(ny,2) .eq. 0 ) ny = ny + 1

      return
      end


C****************************************************************************** 
C SUBROUTINE:
C      bxcrker
C
C DESCRIPTION: 
C      Compute the rectangular boxcar kernel 
C
C AUTHOR:  
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call bxcrker(kernel,nx,ny)
C
C ARGUMENTS:
C    kernel        Elliptical gaussian kernel array
C    nx,ny         kernel dimensions
C
C PRIMARY LOCAL VARIABLES:
C    i,j           Integer counters
C    norm          Normalization for kernel
C
C CALLED ROUTINES:
C
C****************************************************************************** 
      subroutine bxcrker(kernel,nx,ny)
      integer  nx,ny
      double precision kernel(nx,ny),norm
      integer i,j

      norm = 0.0d0

      do 20 j = 1, ny
        do 10 i = 1, nx
          kernel(i,j) = 1.0d0
          norm = norm + kernel(i,j)
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

