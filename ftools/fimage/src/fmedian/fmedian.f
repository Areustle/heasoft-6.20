**************************************************************************** 
C FTOOLS FIMAGE TASK:
C      fmedian 
C
C FILE:
C      fmedian.f 
C
C DESCRIPTION: 
C      Performs rectangular median smoothing of a FITS image.
C
C The whole code is copied from fboxcar as it is. Only replacement
C needed in the routine "convolve" which is a part of MISC.FOR.
C So, the subroutine convolve is named as median_convolve. In this
C routine instead of multiplying by kernel (in the old routine), it
C calls a C routine GETMEDIAN where the median value gets
C calculated. Before calling this routine "getmedian", the 2-D image
C needed to be arranged as 1-D array. So a new variable 
C  double precision newarray(*), median
C was declared
C
C AUTHOR:  
C     Banashree M Seifert  August, 1997
C
C MODIFICATION HISTORY:
C     10/17/97 PDW 2.9a - Replace old get header routines
C     12/03/97 PDW 2.9b - Introduce clobber parameter... sort of...
C                         call ffinit--an ftool utility--instead of ftinit
C     12/29/99 NG  2.9c - Updates for compressed images.
C
C NOTES:
C      fmedian is supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xwindow       Box size in x direction in pixels
C    ywindow       Box size in x direction in pixels
C    boundary      Boundary (constant,nearest,reflect,wrap)
C    constant      Constant for boundary extension
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C
C CALLED ROUTINES:
C      subroutine gpmedian - gets parameters from environment
C      subroutine domedian - smooth/write output image.
C
C***************************************************************************** 
      subroutine fmedin

      character(160)  infile,outfile 
      character(20)  datatype,boundary

      double precision  constant,nullval

      logical  copyprime,copyall

      integer  xwindow,ywindow,status

      character(40) taskname
      common /task/ taskname


      taskname = 'fmedian2.9c'
      status = 0


C     * Get the parameters from the par file *
      call gpmedian(infile,outfile,xwindow,ywindow,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

C     * Pass input parameters to core of smoothing software *
      call domedian(infile,outfile,xwindow,ywindow,
     &            boundary,constant,datatype,nullval,copyprime,
     &            copyall,status)

      return
      end

C***************************************************************************** 
C SUBROUTINE:
C     gpmedian
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:
C      Banashree M Seifert, August, 1997
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gpmedian uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gpmedian(infile,outfile,xwindow,ywindow,
C     &             boundary,constant,datatype,nullval,copyprime,
C     &             copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xwindow       Box size in x direction in pixels
C    ywindow       Box size in x direction in pixels
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
C***************************************************************************** 
      subroutine gpmedian(infile,outfile,xwindow,ywindow,
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

C  get the value of xwindow for box 
      call uclgsi('xwindow',xwindow,status)
      if (status .ne. 0) then
        contxt = 'could not get xwindow parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of ywindow for box 
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
C      domedian
C
C DESCRIPTION: 
C      Reads n-d images from input files and smooths image using
C      rectangular median filter and then writes out smoothed image.
C
C AUTHOR:  
C      Banashree M Seifert,  August, 1997
C
C MODIFICATION HISTORY:
C
C NOTES:
C      domedian uses FITSIO calls to read image file
C
C USAGE:
C	call domedian(infile,outfile,xwindow,ywindow,
C     &               boundary,constant,datatype,nullval,copyprime,
C     &               copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xwindow       Box size in x direction in pixels
C    ywindow       Box size in x direction in pixels
C    newarray      the patch matrix is made 1-d array
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
C***************************************************************************** 
      subroutine domedian(infile,outfile,xwindow,ywindow,
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
      integer  iarray,patch,oarray,newarray
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

C calculate the rectangular box parameters from user inputs
      call boxpar(xwindow,ywindow,nxk,nyk)

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

C allocate the dynamic memory for patch
      patch = 0
      call udmget (nxk*nyk, 7, patch, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for patch'
         call fcerr (contxt)
         goto 998
      endif

C allocate the dynamic memory for newarray
      newarray = 0
      call udmget (nxk*nyk, 7, newarray, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for newarray'
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

C convolve the rectangular box kernel with the image
      call median_convolve(MEMD(iarray),naxis1,naxis2,
     &                     MEMD(patch),nxk,nyk,MEMD(newarray),
     >                     boundary, constant,MEMD(oarray))

C put image into correct HDU based on parameters
      if ((extnum .eq. 0) .and. (.not. copyprime) .and.
     &    (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)
        history = 'TASK: FMEDIAN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)

      else if ((extnum .eq. 0) .and. (copyprime) .and. 
     &         (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        history = 'TASK: FMEDIAN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call putimage(ounit,dtype,pcount,gcount,bscale,bzero,
     &                naxis1,naxis2,MEMD(oarray),status)

      else if ((extnum .eq. 0) .and. (.not. copyprime) .and. 
     &         (copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        history = 'TASK: FMEDIAN on FILENAME: ' // filnam
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
        history = 'TASK: FMEDIAN on FILENAME: ' // filnam
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
        history = 'TASK: FMEDIAN on FILENAME: ' // filnam
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
        history = 'TASK: FMEDIAN on FILENAME: ' // filnam
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
        history = 'TASK: FMEDIAN on FILENAME: ' // filnam
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
          history = 'TASK: FMEDIAN on FILENAME: ' // filnam
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
      call udmfre (patch, 7, status)
      call udmfre (oarray, 7, status)
      return
      end

C***************************************************************************** 
C SUBROUTINE:
C      boxpar
C
C DESCRIPTION: 
C      Compute the rectangular box parameters from user inputs
C
C AUTHOR:  
C      Banashree M Seifert,  August, 1997
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C	call boxpar(xwindow,ywindow,nx,ny)
C
C ARGUMENTS:
C    xwindow       window size in x direction in pixels
C    ywindow       window size in y direction in pixels
C    nx,ny         kernel dimensions
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C***************************************************************************** 
      subroutine boxpar(xwindow,ywindow,nx,ny)
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
C      convolve
C
C DESCRIPTION:
C      Convolve an arbitrary kernel with an image(array).
C
C AUTHOR:
C      Banashree M Seifert,  August, 1997
C      (same as convolve subroutine in MISC.FOR by James Kent Blackburn
C       last part where calculation for box kernel is done, it is replaced
C       by call to C function getmedian.c)
C      (Treatment of upper boundary handling in 'wrap' and 'reflect' modes
C      corrected by Mohan Rajagopal, Stanford, May 16, 1997. in FBOXCAR)
C MODIFICATION HISTORY:
C NOTES:
C
C USAGE:
C       call median_convolve(inimag,nxi,nyi,patch,nxk,nyk,
C      &              boundary,constant,outimag)
C
C ARGUMENTS:
C    inimag        Input Image
C    nxi,nyi       Image dimensions
C    patch         Patch of image that is size of kernel array
C    nx,ny         kernel dimensions
C    boundary      type of boundary condition
C    constant      value of constant used with CONSTANT B.C.
C    outimag       Output Image
C
C PRIMARY LOCAL VARIABLES:
C    i,j,ii,jj     Dummy loop counters
C    npx,npy       Location in image space + extended boundary
C    imgx,imgy     Location in image space
C    nxpat,nypat   Location in the patch
C    constflg      Flag set to true when the pixel value = constant
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine median_convolve(inimag,nxi,nyi,patch,nxk,nyk,
     >                           newarray,boundary,constant,outimag)
      integer  nxi,nyi,nxk,nyk
      double precision  inimag(nxi,nyi),outimag(nxi,nyi)
      double precision  patch(nxk,nyk),constant
      character*(*)  boundary
      integer i,j,ii,jj,npx,npy,nxpat,nypat,imgx,imgy
      logical constflg

      integer k, totalbox, total, iii
      double precision newarray(*), median

      iii=0
      imgx=0
      imgy=0
      total=nxi*nyi
      constflg = .false.
      do 40 j = 1, nyi
      do 30 i = 1, nxi
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
              imgy = nyi - (npy - nyi - 1)
            else if ( boundary .eq. 'WRAP' ) then
              imgy = (npy - nyi)
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
              imgx = nxi - (npx - nxi -1)
            else if ( boundary .eq. 'WRAP' ) then
              imgx = (npx - nxi)
            end if
          endif
          if ( constflg ) then
            patch(nxpat,nypat) = constant
            constflg = .false.
          else
            patch(nxpat,nypat) = inimag(imgx,imgy)
          endif
          nxpat = nxpat + 1
  15    continue
        nypat = nypat + 1
  25    continue
 
c this index variable iii is used for counter in xclock only
c newarray is same as patch matrix expressed in 1-D
c totalbox is the total no. of elements in the box (nxk * nyk)
        iii = iii+1
        call xclock(iii,total,10)
        totalbox = nxk * nyk
        k = 0
        outimag(i,j) = 0.0d0
        median=0.0d0
        do ii = 1,nxk
           do jj = 1,nyk
              k = k+1
              newarray(k) = patch(ii,jj)
           enddo
        enddo

        call getmedian(newarray, totalbox, median)
        outimag(i,j) = median

  30  continue
  40  continue
 
      return
      end

C******************************************************************************


