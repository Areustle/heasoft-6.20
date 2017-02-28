****************************************************************************
C FTOOLS FIMAGE TASK:
C      fimgbin
C
C FILE:
C      fimgbin.f
C
C DESCRIPTION:
C      Performs rebinning of a FITS image.
C
C AUTHOR:
C      Dr. James Kent Blackburn / April '94
C
C MODIFICATION HISTORY:
C      August 94: Made to work on one row at a time
C     10/17/97 PDW 3.1a - Replace old get header routines
C     12/03/97 PDW 3.1b - Introduce clobber parameter... sort of...
C                         call ffinit--an ftool utility--instead of ftinit
C     1999-05-10 Jeff Guerber 3.1c - In dorebin, should multiply by norm
C                         instead of dividing.
C     2000-01-11- Ning Gan     3.1d - Updates for reading compressed images. 
C     2000-03-30- Ning Gan     3.1e - Expanded the datatype specification. 
C                                     Initialized the pointer oarray and
C                                     iarray to 0.
C                                   - Added overflow parameter.
C
C NOTES:
C      fimgbin is supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xbinsize      X number of pixels to rebin as one
C    ybinsize      Y number of pixels to rebin as one
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    average       Average the counts in new bin or just sum?
C    copyprime     Copy keywords from primary array of infile?
C    copyall       Copy all other extensions in infile?
C
C CALLED ROUTINES:
C      subroutine gimgbin - gets parameters from environment
C      subroutine rbimage - rebin image.
C
C******************************************************************************
      subroutine fimgbn

      implicit none
      character(160)  infile,outfile
      character(20)  datatype

      double precision  nullval

      logical  average,copyprime,copyall
      logical  overflow

      integer  xbinsize,ybinsize,status

      character(40) taskname
      common /task/ taskname

      taskname = 'fimgbin3.1e'
      status = 0

C     * Get the parameters from the par file *
      call gimgbin(infile,outfile,xbinsize,ybinsize,datatype,overflow,
     &             nullval,average,copyprime,copyall,status)

C     * Pass input parameters to core of rebinning software *
      call rbimage(infile,outfile,xbinsize,ybinsize,datatype,
     &             overflow, nullval,average,copyprime,copyall,status)

      return
      end

C******************************************************************************
C SUBROUTINE:
C     gimgbin
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
C      gimgbin uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gimgbin(infile,outfile,xbinsize,ybinsize,datatype,
C    &              nullval,average,copyprime,copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xbinsize      X number of pixels to rebin as one
C    ybinsize      Y number of pixels to rebin as one
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    average       Average the counts in new bin or just sum?
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

      subroutine gimgbin(infile,outfile,xbinsize,ybinsize,datatype,
     &          overflow,nullval,average,copyprime,copyall,status)

      implicit none
      character*(*)  infile,outfile
      character*(*)  datatype
      double precision  nullval
      logical overflow
      logical  average,copyprime,copyall
      integer  xbinsize,ybinsize,status
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

C  get the value of xbinsize for rebin
      call uclgsi('xbinsize',xbinsize,status)
      if (status .ne. 0) then
        contxt = 'could not get xbinsize parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get the value of ybinsize for rebin
      call uclgsi('ybinsize',ybinsize,status)
      if (status .ne. 0) then
        contxt = 'could not get ybinsize parameter'
        call fcerr(contxt)
        goto 999
      endif
      if (ybinsize .lt. 1 ) ybinsize = xbinsize

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

C  Ignore the overflow/underflow pixel? 
      call uclgsb('overflow', overflow, status)
      if (status .ne. 0) then
        contxt = 'could not get overflow parameter'
        call fcerr(contxt)
        goto 999
      endif 

C  get the value of null pixels
      call uclgsd('nullval',nullval,status)
      if (status .ne. 0) then
        contxt = 'could not get nullval parameter'
        call fcerr(contxt)
        goto 999
      endif

C  get whether to average pixels from primary array of infile
      call uclgsb('average', average, status)
      if (status .ne. 0) then
        contxt = 'could not get average parameter'
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
C      rbimage
C
C DESCRIPTION:
C      Reads 2-d images from input files and rebins image
C
C AUTHOR:
C      Dr. James Kent Blackburn / April '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      smthbxcr uses FITSIO calls to read image file
C
C USAGE:
C	call rbimage(infile,outfile,xbinsize,ybinsize,datatype,
C    &               nullval,average,copyprime,copyall,status)
C
C ARGUMENTS:
C    infile        Input FITS image to be fit
C    outfile       Output FITS image
C    xbinsize      X number of pixels to rebin as one
C    ybinsize      Y number of pixels to rebin as one
C    datatype      Data type of output image
C    nullval       Value to substitute for Null Pixels
C    average       Average the counts in new bin or just sum?
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
      subroutine rbimage(infile,outfile,xbinsize,ybinsize,datatype,
     &               overflow, nullval,average,copyprime,copyall,status)

      implicit none
      character*(*)  infile,outfile
      character*(*)  datatype
      double precision  nullval
      logical  average,copyprime,copyall
      logical overflow
      integer xbinsize,ybinsize,status
      character(160) contxt,filnam,history
      character(80)  comment
      integer  naxis1,naxis2,nximg,nyimg,extcnt
      integer  iunit,ounit,block,extnum,hdtype,dtype
      integer  naxis,naxes(10),bitpix,pcount,gcount,group
      integer  iarray,oarray,larray
      logical  simple,extend
      double precision  norm,bscale,bzero

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
      iarray = 0
      oarray = 0
      larray = 0

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
C      if (datatype .eq. '-') then
C        dtype = bitpix
C      else if (datatype .eq. 'B') then
C        dtype = 8
C      else if (datatype .eq. 'I') then
C        dtype = 16
C      else if (datatype .eq. 'J') then
C        dtype = 32
C      else if (datatype .eq. 'E') then
C        dtype = -32
C      else if (datatype .eq. 'D') then
C        dtype = -64
C      else
C        contxt = ' Unsupported datatype requested for smoothed image'
C        call fcerr(contxt)
C        goto 999
C      end if

      status = 0
      dtype = bitpix
      call dataty (datatype, dtype, status)
      if(status.ne.0) then 
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

C calculate the rebinned image dimensions from user inputs
      nximg = ( naxis1 / xbinsize )
      nyimg = ( naxis2 / ybinsize )

C calculate normalization factor
      if (average) then
        norm = 1.0d0 / ( xbinsize * ybinsize )
      else
        norm = 1.0d0
      endif

C allocate the dynamic memory for one input image row
      call udmget (naxis1*ybinsize, 7, iarray, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for input'
         call fcerr (contxt)
         goto 998
      endif

C allocate the dynamic memory for one output image row
      call udmget (nximg, 7, oarray, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for output'
         call fcerr (contxt)
         goto 998
      endif

      call udmget (nximg, 1, larray, status)
      if (status .ne. 0) then
         contxt = ' Error allocating dynamic memory for output'
         call fcerr (contxt)
         goto 998
      endif
      call setnullflag(MEMB(larray),nximg,.FALSE.)

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

C move back to beginning of input FITS file
      call ftmahd(iunit,1,hdtype,status)

C initialize the output FITS file
      call ffinit(ounit,outfile,status)
      if (status .ne. 0) then
         contxt = ' could not open outfile may already exist?'
         call fcerr(contxt)
         goto 999
      endif

C swap out the naxes for the output array
      naxes(1) = nximg
      naxes(2) = nyimg

C put image into correct HDU based on parameters
      if ((extnum .eq. 0) .and. (.not. copyprime) .and.
     &    (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        call rswcskw(ounit,xbinsize,ybinsize,status)
        history = 'TASK: FIMGBIN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
     &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
     &               norm,MEMD(iarray),MEMD(oarray),MEMB(larray),
     &               overflow, status)

      else if ((extnum .eq. 0) .and. (copyprime) .and.
     &         (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        call rswcskw(ounit,xbinsize,ybinsize,status)
        history = 'TASK: FIMGBIN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
     &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
     &               norm,MEMD(iarray),MEMD(oarray),MEMB(larray),
     &               overflow, status)

      else if ((extnum .eq. 0) .and. (.not. copyprime) .and.
     &         (copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftphpr(ounit,.true.,8,0,0,0,1,.true.,status)
        call rswcskw(ounit,xbinsize,ybinsize,status)
        history = 'TASK: FIMGBIN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
     &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
     &               norm,MEMD(iarray),MEMD(oarray),MEMB(larray),
     &               overflow, status)
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
        call rswcskw(ounit,xbinsize,ybinsize,status)
        history = 'TASK: FIMGBIN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
     &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
     &               norm,MEMD(iarray),MEMD(oarray),MEMB(larray),
     &               overflow, status)
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
        call rswcskw(ounit,xbinsize,ybinsize,status)
        history = 'TASK: FIMGBIN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
     &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
     &               norm,MEMD(iarray),MEMD(oarray),MEMB(larray),
     &               overflow, status)

      else if ((extnum .ne. 0) .and. (copyprime) .and.
     &         (.not. copyall)) then

        call ftmahd(iunit,1,hdtype,status)
        call ftcopy(iunit,ounit,0,status)
        call ftmrhd(iunit,extnum,hdtype,status)
        call ftcrhd(ounit,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        call rswcskw(ounit,xbinsize,ybinsize,status)
        history = 'TASK: FIMGBIN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
     &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
     &               norm,MEMD(iarray),MEMD(oarray),MEMB(larray),
     &               overflow, status)

      else if ((extnum .ne. 0) .and. (.not. copyprime) .and.
     &         (copyall)) then

        call ftmahd(iunit,extnum+1,hdtype,status)
        call ftphpr(ounit,simple,dtype,naxis,naxes,pcount,gcount,
     &              extend,status)
        call xcopyscale (iunit, ounit, status)
        call rswcskw(ounit,xbinsize,ybinsize,status)
        history = 'TASK: FIMGBIN on FILENAME: ' // filnam
        call ftphis(ounit,history,status)
        call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
     &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
     &               norm,MEMD(iarray),MEMD(oarray),MEMB(larray),
     &               overflow, status)
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
          call rswcskw(ounit,xbinsize,ybinsize,status)
          history = 'TASK: FIMGBIN on FILENAME: ' // filnam
          call ftphis(ounit,history,status)
        call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
     &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
     &               norm,MEMD(iarray),MEMD(oarray),MEMB(larray),
     &               overflow,status)
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
      if(iarray.ne.0) call udmfre (iarray, 7, status)
      if(oarray.ne.0) call udmfre (oarray, 7, status)
      return
      end


C******************************************************************************
C SUBROUTINE:
C      dorebin
C
C DESCRIPTION:
C      rebin the image and write it out
C
C AUTHOR:
C      Dr. James Kent Blackburn / April '94
C
C MODIFICATION HISTORY:
C      August 94: made this routine responsible for IO as well
C      1999-05-10 Jeff Guerber - Should multiply by norm instead of dividing.
C
C NOTES:
C
C USAGE:
C       call dorebin(iunit,ounit,dtype,pcount,gcount,bscale,bzero,
C    &               naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg,
C    &               norm,inarray,outarray,overflow, status)
C
C ARGUMENTS:
C    iunit,ounit    input and output FITS file unit numbers
C    dtype          datatype of output image
C    pcount         Value of PCOUNT keyword
C    gcount         Value of GCOUNT keyword
C    bscale         Image scale factor
C    bzero          Image offset factor
C    naxis1         Number of pixels along X-axis of input image
C    naxis2         Number of pixels along Y-axis of input image
C    xbinsize       x bin dimension
C    ybinsize       y bin dimension
C    nximg          Number of pixels along X-axis of output image
C    nyimg          Number of pixels along Y-axis of output image
C    norm           normalization factor
C    inarray        input 2-D image array of doubles
C    outarray       output 2-D image array of doubles
C    larray         logical array (is null?)
C    overflow       ignore the overflow pixel?
C    status         integer status flag
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine dorebin(iunit,ounit,dtype,pcount,gcount,
     &      bscale,bzero,naxis1,naxis2,xbinsize,ybinsize,
     &      nximg,nyimg,norm,inarray,outarray,larray,overflow, status)
      implicit none
      integer  iunit,ounit,dtype,pcount,gcount,status
      integer  naxis1,naxis2,xbinsize,ybinsize,nximg,nyimg
      double precision bscale,bzero
      double precision inarray(naxis1,ybinsize),outarray(nximg)
      logical larray(nximg)
      integer i,j,ii,jj,group,naxis,outaxes(2)
      integer ifpixel,ofpixel,inelem,onelem
      double precision norm,pixel,nullval
      logical anyf
      logical overflow

      nullval = 0
      naxis = 2
      outaxes(1) = nximg
      outaxes(2) = nyimg
      ifpixel = 1
      inelem = naxis1 * ybinsize
      ofpixel = 1
      onelem = nximg
      group = 1

      call ftpdef(ounit,dtype,naxis,outaxes,pcount,gcount,status)
      call ftpscl(ounit,bscale,bzero,status)

      do 40 j = 1, nyimg
          call ftgpvd(iunit,group,ifpixel,inelem,
     &        nullval,inarray,anyf,status)
          do 30 i = 1, nximg
              pixel = 0.0d0
              do 20 jj = 1, ybinsize
                  do 10 ii = 1, xbinsize
                      pixel = pixel + inarray(xbinsize*(i-1)+ii,jj)
   10             continue
   20         continue
              pixel = pixel * norm
              outarray(i) = pixel
   30     continue

           
          if(overflow) call ouflow(outarray,larray, onelem, dtype)
          call ftpprd(ounit,group,ofpixel,onelem,
     &        outarray,status)
          if(status.ne.0) then
              call fcerr('Error writing data to file')
              return
          endif
          ifpixel = ifpixel + inelem
          ofpixel = ofpixel + onelem
   40 continue

      return
      end


C******************************************************************************
C SUBROUTINE:
C      rswcskw
C
C DESCRIPTION:
C      rescale World Coordinate System Keywords
C
C AUTHOR:
C      Dr. James Kent Blackburn / April '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C     call rswcskw(ounit,xbin,ybin,status)
C
C ARGUMENTS:
C    ounit          output unit number
C    xbin           x bin factor
C    ybin           y bin factor
C    status         status flag
C
C PRIMARY LOCAL VARIABLES:
C    keyword        w.c.s. keyword name
C    keyval         w.c.s. keyword value
C    comment        w.c.s. keyword comment field
C
C CALLED ROUTINES:
C    ft____        FITSIO subroutines
C
C******************************************************************************
      subroutine rswcskw(ounit,xbin,ybin,status)
      implicit none
      integer ounit,xbin,ybin,status,decimals
      character(8) keyword
      character(80) comment
      double precision keyval

      comment = ' '
      decimals = 20

C     Correct the CRPIX1 and CRPIX2 keywords
      keyword = 'CRPIX1'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = ((keyval - 0.5d0) / xbin) + 0.5d0
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif
      keyword = 'CRPIX2'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = ((keyval - 0.5d0) / ybin) + 0.5d0
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif

C     Correct the CDELT1 and CDELT2 keywords
      keyword = 'CDELT1'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * xbin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif
      keyword = 'CDELT2'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * ybin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif

C     Correct the CD001001, CD001002, CD002001 and CD002002 keywords
      keyword = 'CD001001'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * xbin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif
      keyword = 'CD001002'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * xbin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif
      keyword = 'CD002001'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * ybin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif
      keyword = 'CD002002'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * ybin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif

C     Correct the CD1_1, CD1_2, CD2_1 and CD2_2 keywords
      keyword = 'CD1_1'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * xbin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif
      keyword = 'CD1_2'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * xbin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif
      keyword = 'CD2_1'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * ybin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif
      keyword = 'CD2_2'
      call ftgkyd(ounit,keyword,keyval,comment,status)
      if (status .eq. 0) then
        keyval = keyval * ybin
        call ftmkyd(ounit,keyword,keyval,decimals,comment,status)
      else
        status = 0
      endif

      return
      end


      subroutine setnullflag(nullflag, nelem, flagval)
      integer nelem
      logical nullflag(nelem)
      logical flagval
      integer i
      
      do i = 1,  nelem
         nullflag(i) = flagval
      enddo
      return 
      end 
      
      
