C**************************************************************************** 
C SELECTOR TASK:
C      fimgdmp
C
C FILE:
C      fimgdmp.f
C
C DESCRIPTION: 
C      Reads a 2-d image and displays on the screen.
C       ( extended to 1-d and 3-d images.  (Srilal))
C AUTHOR:  
C      Vidya Sagar. Jun '92
C
C MODIFICATION HISTORY:
C       10/2/92 - fixed X, Y transponsition problem, removed filename
C                 printing in the middle of real output, added updating
C                 of parameter file if xlow, etc are changed by program.
C                 changed real output to G format for easier reading (EAG)
C       10/22/92 - fixed problem with trying to read in array larger than
C                   dimensioned (EAG)
C                - made default extension the primary array
C       11/9/92  - fixed unable to open infile core dump (EAG)
C       6/4/93   - only read in appropriate part of array (EAG)
C       1/18/95 EAG 3.3a - clean up, add showscale parameter
C       6/21/95  - added dynamic memory for values array (kaa)
C       9/11/95  - reads 1, 2 and 3-d images and displays to the screen (Srilal)
C    05/11/1998 toliver 3.6 - hacked subroutine firiwp to expand output
C                             format to accomodate maximum integer value
C                             (thread bugs/fimgdmp-digits-980203)
C    11/29/1999 ngan 3.7 -   Supported the compressed image.
C
C NOTES:
C      fimgdmp supported in IRAF and HOST environments
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      infile    - input FITS file and extension number
C      naxes(1)  - number of x bins in histogram
C      naxes(2)  - number of y bins in histogram
C      naxes(3)  - number of z bins in histogram
C
C CALLED ROUTINES:
C      subroutine gimgdp - gets parameters from environment
C      subroutine firiwp - read input image file and write pixel values.
C
C*****************************************************************************
      subroutine fimgdp

C
      character * 160 infile 
      character * 160 outfil
      character * 80  context

      integer         naxes(3)  
      integer         bitpix
      integer         xlow,xhigh
      integer         ylow,yhigh
      integer         zlow,zhigh
      integer         xnbins, ynbins,znbins
      integer         status

      logical         prflag, showscale, bscaleflag


C pointer for array of pixel values

      integer values

C dynamic memory allocation stuff

      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C     datatype gives a symbolic code for the data type, e.g.,  4 = Integer*4
C     1 is boolean
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 complex



      character(40) taskname
      common /task/ taskname

      taskname = 'fimgdmp3.7'
      infile = ' '
      outfil = ' '
      naxes(1) = 0 
      naxes(2) = 0 
      naxes(3) = 0 
      bitpix = 0

      prflag = .false.
      showscale = .true.

      call ftcmsg

C  get the parameters from the par file
      call gimgdp(infile,outfil,prflag,xlow,xhigh,
     &     ylow,yhigh,zlow,zhigh,showscale, status)

      if (status .ne. 0) return

      call gbnwrd(infile,outfil,prflag,showscale,bitpix,naxes,
     +     xlow,xhigh,ylow,yhigh,zlow,zhigh,status)

         

      if (status .ne. 0) return

      xnbins = xhigh - xlow + 1
      ynbins = yhigh - ylow + 1
      znbins = zhigh - zlow + 1

C get the memory for the values array

      values = 0
      call udmget(xnbins*ynbins*znbins, 6, values, status)
      if ( status .ne. 0 ) then
         context = ' Insufficient memory available for array'
         call fcerr (context)
         return
      endif

      call firiwp(infile,outfil,MEMR(values),xnbins,ynbins,znbins,xlow,
     &xhigh,ylow,yhigh,zlow,zhigh,bitpix,naxes,
     &showscale,status)
      return
      end

C****************************************************************************
C SUBROUTINE:
C      gimgdp
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gimgdp uses F77/VOS like calls to read parameter from .par file
C
C USAGE:

C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      naxes(1)   - # of x-bins
C      naxes(2)   - # of y-bins
C      naxes(3)   - # of z-bins
C
C PRIMARY LOCAL VARIABLES:
C      ctxt - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsr - get real parameter
C      subroutine uclgst - get string parameter
C
C****************************************************************************
      subroutine gimgdp(infile,outfil,prflag,xlow,xhigh,
     +     ylow,yhigh,zlow,zhigh, showscale, status)
C
      character*(*) outfil 
      character*(*) infile
      character(80) ctxt
      character(160) filnam
C

      integer   xlow,xhigh,ylow,yhigh,zlow,zhigh 
      integer status,block
      integer ftstatus,hdtype,extnum,iunit

      integer naxis
      
      logical   prflag, showscale, inopen
C
C
C  initialize variables
      status = 0
      inopen = .false.
       xlow = 1
       ylow = 1
       zlow = 1
       xhigh = 1
       yhigh = 1
       zhigh = 1

C  get the name of the input FITS file

      call uclgst('infile',infile,status)
      if (status .ne. 0) then
         ctxt = 'could not get INFILE parameter'
         call fcerr(ctxt)
         goto 999
      endif


C  get the name of the output FITS file

      call uclgst('outfil',outfil,status)
      if (status .ne. 0) then
         ctxt = 'could not get OUTFIL parameter'
         call fcerr(ctxt)
         goto 999
      endif



C  get the input FITS filename and extension number
      iunit = 15
      status = 0

      call fcpars(infile,filnam,extnum,status)
      if (status .ne. 0) goto 999 

C EAG 8/25/93 default extension is primary array
      if (extnum .eq. -99) extnum = 0

C  open the input FITS file
      call ftopen(iunit,filnam,0,block,status)
      if (status .ne. 0) then
         ctxt = 'unable to open infile'
         call fcerr(ctxt)
         goto 999
      endif
      inopen = .true.

C  Move to the required extension
      call ftmahd(iunit,extnum+1,hdtype,status)
      if (status .ne. 0) then
         write(ctxt,34) 'error moving to extension number ',extnum
         call fcerr(ctxt)
         goto 999
      endif

      ftstatus = 0
C --- Get the number of  dimensions  of the array.
      call ftgidm(iunit,naxis,status)

       if (naxis .gt. 0) then
C  get xlow
      call uclgsi('xlow',xlow,status)
      if (status .ne. 0) then
         ctxt = 'could not get XLOW parameter'
         call fcerr(ctxt)
         goto 999
      endif


C  get xhigh
      call uclgsi('xhigh',xhigh,status)
      if (status .ne. 0) then
         ctxt = 'could not get XHIGH parameter'
         call fcerr(ctxt)
         goto 999
      endif
      endif

       if (naxis .gt. 1) then
C  get ylow
      call uclgsi('ylow',ylow,status)
      if (status .ne. 0) then
         ctxt = 'could not get YLOW parameter'
         call fcerr(ctxt)
         goto 999
      endif


C  get yhigh 
      call uclgsi('yhigh',yhigh,status)
      if (status .ne. 0) then
         ctxt = 'could not get YHIGH parameter'
         call fcerr(ctxt)
         goto 999
      endif
      endif


       if (naxis .gt. 2) then
C  get zlow
      call uclgsi('zlow',zlow,status)
      if (status .ne. 0) then
         ctxt = 'could not get ZLOW parameter'
         call fcerr(ctxt)
         goto 999
      endif


C  get zhigh 
      call uclgsi('zhigh',zhigh,status)
      if (status .ne. 0) then
         ctxt = 'could not get ZHIGH parameter'
         call fcerr(ctxt)
         goto 999
      endif
      endif

C  get the name of the print file

      call uclgsb('prflag',prflag,status)
      if (status .ne. 0) then
         ctxt = 'could not get PRINT flag parameter'
         call fcerr(ctxt)
         goto 999
      endif

C get whether to show the data scaled or not
      call uclgsb ('showscale', showscale, status)
      if (status .ne. 0) then
         ctxt = ' could not get SHOWSCALE parameter'
         call fcerr (ctxt)
         goto 999
      endif

 999  continue
      if (inopen) call ftclos(iunit,ftstatus)

 34   format(a50,i6)
      return
      end

C****************************************************************************
C SUBROUTINE:
C      firiwp
C
C DESCRIPTION: 
C      Reads 2-d image from input file and writes pixel values to output.
C
C AUTHOR:  
C      Vidya Sagar Jul '92 
C
C MODIFICATION HISTORY:
C       10/22/92 - made the primary array the default extension (EAG)
C     05/11/1998 - expanded output format to accomodate maximum
C                  integer value (toliver) (didn't know they were still writin'
C                  code this fugly back in '92)
C
C NOTES:
C      firiwp uses FITSIO calls to read image file
C
C USAGE:
C       subroutine firiwp(infile,outfil,values,xnbins,ynbins,znbins,xlow,
C    &xhigh,ylow,yhigh,zlow,zhigh,bitpix,naxes,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C       outfil  - name of output file
C       values  - array of values to be printed
C       xnbins  - number of values to print in the X direction
C       ynbins  - number of values to print in the Y direction
C       znbins  - number of values to print in the Z direction
C       xlow, xhigh - low and high values in X
C       ylow, yhigh - low and high values in Y
C       zlow, zhigh - low and high values in Z
C       bitpix  - number of bits/pixel
C      naxes(1)   - x dimension
C      naxes(2)   - y dimension
C      naxes(3)   - z dimension
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - report an error number to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftgbnh - get BINARY table header
C      subroutine ftgtbh - get ASCII table header
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftopen - open a FITS file
C      function fcstln   - index of last non-blank character
C
C****************************************************************************** 
      subroutine firiwp(infile,outfil,values,xnbins,ynbins,znbins,xlow,
     &xhigh,ylow,yhigh,zlow,zhigh,bitpix,naxes,
     &showscale,status)
      character*(*) infile, outfil
      character(10)  ctxt1
      character(14) ctxt14
      character(80) ctxt2
      character(10)  ctxt3
      character(80) ctxt4, ctxt
      character(10) ctxtz
      character(160) filnam

      integer i
      integer j
      integer k
      integer n1
      integer n2
      integer n
      integer inc
      integer ninc
      integer block
      integer bitpix
      integer xlow,xhigh
      integer ylow,yhigh
      integer zlow,zhigh
      integer iy
      integer kz
      integer ivalue
      integer hdtype
      integer naxes(3), naxis
      integer xnbins,ynbins,znbins
      integer status, iunit,extnum
      integer fcstln
      integer fpixels(3), lpixels(3), incs(3)
 
      logical anyf, showscale
      logical inopen

      real nullv
      real values(xnbins,ynbins,znbins)

C  initialize variables
      inopen = .false.
      nullv = 0
      incs(1) = 1
      incs(2) = 1
      incs(3) = 1

C  get the input FITS filename and extension number
      iunit = 15
      status = 0
      call fcpars(infile,filnam,extnum,status)
      if (status .ne. 0) goto 999 

C EAG 8/25/93 default to primary array
      if (extnum .eq. -99) extnum = 0
      
C  open the input FITS file
      call ftopen(iunit,filnam,0,block,status)
      if (status .ne. 0) then
         ctxt = 'unable to open infile'
         call fcerr(ctxt)
         goto 999
      endif
      inopen = .true.

C  Move to the required extension
      call ftmahd(iunit,extnum+1,hdtype,status)
      if (status .ne. 0) then
         call fcerrm (status)
         write(ctxt,34) 'error moving to extension number ',extnum
         call fcerr(ctxt)
         goto 999
      endif

C --- Get the pixel values contained in the naxes(1) x naxes(2) x naxes(3) array.
      fpixels(1) = xlow
      fpixels(2) = ylow
      fpixels(3) = zlow
      lpixels(1) = xhigh
      lpixels(2) = yhigh
      lpixels(3) = zhigh

c turn off scaling, if so requested
      if (.not. showscale) call ftpscl (iunit, 1.0D0, 0.0D0, status)
      call ftgsve (iunit, 0, 3, naxes, fpixels, lpixels, incs, nullv,
     &     values, anyf, status)

      if (status .ne. 0)  goto 999

      call ftgidm(iunit,naxis,status)
C loop over Z (naxis3) 
      do 305 k = 1, znbins
         kz = zlow + k - 1
         write(7,110) ' '
         write(ctxtz,'(a,i3)')'z=',kz
      if (naxis .eq. 3) then
         write(7,110) ctxtz
         write(7,110) ' '
      endif

      n = 1
      n1 = 1
      n2 = 0
      if (bitpix .eq. 16 .or. bitpix .eq. 32 
     &     .or. bitpix .eq. 8) then
         inc =  7
         ninc = 9
      else if (bitpix.eq.-32 .or. bitpix.eq.-64)then
         inc  = 5
         ninc = 13
      else
         ctxt = ' unknown value of BITPIX'
         call fcerr (ctxt)
         goto 999
      endif

 99   continue
      if (outfil .eq. 'STDOUT') then
         call fcecho(' ')
      else
         write(7,110) ' '
      endif
      ctxt1 = ' '
      ctxt2 = ' '
      ctxt3 = ' '
      n2 = n2 + inc
      if ((xnbins .le. inc) .or. ((n2-xnbins) .gt. 0)) n2 = xnbins 

      do 308 i = n1, n2
         write(ctxt1,120) i + xlow - 1
         ctxt2(n:n+ninc) = ctxt1
         n = n + ninc + 1
 308  continue

      ctxt4 = ' '
      ctxt4(11:80) = ctxt2(1:n+ninc+1)
      if (outfil .eq. 'STDOUT') then
         call fcecho(ctxt4)
         call fcecho(' ')
      else
         write(7,110) ctxt4 
         write(7,110) ' '
      endif
      ctxt4 = ' '
      ctxt1 = ' '
      ctxt2 = ' '
      n = 1
C loop over Y (naxis2)
      do 306 i = 1, ynbins
         iy = ylow + i - 1
C loop over X (naxis1)
         do 307 j = n1, n2

            if (bitpix .eq. 8 .or. bitpix .eq. 16 
     &           .or. bitpix .eq. 32) then
               ivalue = values(j,i,k)
               write(ctxt1,120)ivalue 
               ctxt2(n:n+ninc) = ctxt1
               ctxt1 = ' '
               n = n + ninc + 1

            else if (bitpix .eq. -32 .or. bitpix .eq. -64) then
               ctxt14 = ' '
               write(ctxt14,16)values(j,i,k) 
               ctxt2(n:n+ninc) = ctxt14
               ctxt14 = ' '
               n = n + ninc + 1
            endif

 307     continue
         write(ctxt3,120)iy
         ctxt4(1:ninc+1) = ctxt3
         ctxt4(11:80) = ctxt2(1:n)
         ctxt3 = ' '
         if (outfil .eq. 'STDOUT') then
            call fcecho(ctxt4(1:fcstln(ctxt4)))
         else
            write(7,110) ctxt4(1:fcstln(ctxt4))
         endif
         n = 1
 306  continue
      if (n2 .lt. xnbins) then
         n = 1
         n1 = n2 + 1
         goto 99 
      endif
 305  continue
 999  continue
      status = 0
      if (inopen) call ftclos(iunit,status)
      close(7)
 16   format(1pg14.6)
 34   format(a50,i6)
 110  format(a)
120   FORMAT (I10)
      return
      end

C****************************************************************************
C SUBROUTINE:
C      gbnwrd
C
C DESCRIPTION: 
C      Gets the necessary keywords from the input FITS file.
C
C AUTHOR:  
C      Vidya Sagar Jul '92 
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gbnwrd FITSIO calls to read image file
C
C USAGE:
C        subroutine gbnwrd(infile,outfil,prflag,bitpix,naxes,
C    +   xlow,xhigh,ylow,yhigh,zlow,zhigh,status)
C
C ARGUMENTS:
C      infile   - input FITS file and extension number
C      outfile  - output file.
C      prflag   - flag to indicate if header is to be printed.
C      naxes(1) - x dimension
C      naxes(2) - y dimension
C      naxes(3) - z dimension
C      xlow     - Min value of x
C      xhigh    - Max value of x
C      ylow     - Min value of y
C      yhigh    - Max value of y
C      zlow     - Min value of z
C      zhigh    - Max value of z
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - report an error number to terminal
C      subroutine fcpars - parse off filename and extension number
C      subroutine ftclos - close a FITS file
C      subroutine ftmahd - absolute move to FITS header
C      subroutine ftopen - open a FITS file
C
C****************************************************************************** 

      subroutine gbnwrd(infile,outfil,prflag,showscale,bitpix,naxes,
     &xlow,xhigh,ylow,yhigh,zlow,zhigh,status)

      character*(*) infile
      character*(*) outfil 
      character(160) filnam
      character(80) ctxt 
      character(12) ctxt5

      integer bitpix
      integer naxes(3)
      integer naxis
      integer extnum
      integer kk
      integer iunit
      integer block
      integer status
      
      integer xlow
      integer xhigh
      integer ylow
      integer yhigh
      integer zlow
      integer zhigh
      integer hdtype
      integer ftstatus

      real*8 bscale, bzero

      logical prflag, showscale, inopen, bscaleflag, bzeroflag

C  get the input FITS filename and extension number
      iunit = 15
      status = 0
      inopen = .false.

      call fcpars(infile,filnam,extnum,status)
      if (status .ne. 0) goto 999 

C EAG 8/25/93 default extension is primary array
      if (extnum .eq. -99) extnum = 0

C  open the input FITS file
      call ftopen(iunit,filnam,0,block,status)
      if (status .ne. 0) then
         ctxt = 'unable to open infile'
         call fcerr(ctxt)
         goto 999
      endif
      inopen = .true.

C  Move to the required extension
      call ftmahd(iunit,extnum+1,hdtype,status)
      if (status .ne. 0) then
         write(ctxt,34) 'error moving to extension number ',extnum
         call fcerr(ctxt)
         goto 999
      endif


       naxes(1) = 1
       naxes(2) = 1
       naxes(3) = 1
C --- Get the dimensions and  size of the array.
       call ftgidt(iunit,bitpix,status)
       call ftgidm(iunit,naxis,status)
       call ftgisz(iunit,naxis,naxes, status)

C If the values are scaled, make sure they print as reals
      call ftgkyd(iunit,'BSCALE',bscale,ctxt,status)
      if (status .eq. 202) then 
         status = 0
         bscaleflag = .false.
      else if  (bscale .eq. 1.0) then
         bscaleflag = .false.
      else
         bscaleflag = .true.
      endif
         
      call ftgkyd(iunit,'BZERO',bzero,ctxt,status)
      
      if (status .eq. 202) then
         status = 0
         bzeroflag = .false.
      else if ( mod(bzero,1.0d0) .eq. 0.0) then         
         bzeroflag = .false.
      else
         bzeroflag = .true.
      endif

      if ((bscaleflag.or.bzeroflag) .and. showscale) bitpix = -64

C --- Check to see if the X,Y,Z, limits input by the user are
C --- valid. If not make the limits equal to the FITS 
C --- file limits.
C       if updated, change value in parameter file, too (EAG 10/2/92)
      if (xlow .le. 0 .or. xlow .gt. naxes(1)) then
         xlow = 1
         call uclpsi ('xlow', xlow, status)
      endif
      if (ylow .le. 0 .or. ylow .gt. naxes(2)) then
         ylow = 1
         call uclpsi ('ylow', ylow, status)
      endif
      if (zlow .le. 0 .or. zlow .gt. naxes(3)) then
         zlow = 1
         call uclpsi ('zlow', zlow, status)
      endif
      if (xhigh .gt. naxes(1)) then
         xhigh = naxes(1)
         call uclpsi ('xhigh', xhigh, status)
      endif
      if (yhigh .gt. naxes(2)) then
         yhigh = naxes(2)
         call uclpsi ('yhigh', yhigh, status)
      endif
      if (zhigh .gt. naxes(3)) then
         zhigh = naxes(3)
         call uclpsi ('zhigh', zhigh, status)
      endif
      if (status .ne. 0) then
         ctxt = ' unable to update parameter file'
         call fcerr (ctxt)
         goto 999
      endif


      if (outfil .ne. 'STDOUT') then
         open(7,file=outfil,access='sequential',form='formatted',
     +        status='new',err=32)
      endif
      if (status .eq. 0) goto 33
C
 32   continue
      call fcerr('Unable to open output file')
      status = 1
      goto 999

 33   continue

      if (prflag) then

         if (outfil .eq. 'STDOUT') then

            do 10 kk = 1,99999 
               call ftgrec(iunit,kk,ctxt,status)
               if (status .ne. 0 .or. ctxt(1:3) .eq. 'END') goto 31
               call fcecho(ctxt)
               ctxt = ' '
 10         continue

         else if (outfil .ne. 'STDOUT') then

            do 20 kk = 1, 99999
               call ftgrec(iunit,kk,ctxt,status)
               if (status .ne. 0 .or. ctxt(1:3) .eq. 'END') goto 31
               write(7,110) ctxt
               ctxt = ' '
 20         continue

         else
         endif

         ctxt = ' '
 31      continue
         ctxt = ' '
         if (outfil .eq. 'STDOUT') call fcecho(ctxt)
         if (outfil .ne. 'STDOUT') then
            write(7,110) ctxt
         endif

         ctxt(1:22) = 'XLOW (current) =   '
         if (bitpix .eq. 8 .or. bitpix .eq. 16
     &        .or. bitpix .eq. 32) then
            write(ctxt5,9) xlow
         else
            write(ctxt5,7) xlow
         endif
         ctxt(23:35) = ctxt5
         if (outfil .eq. 'STDOUT') call fcecho(ctxt)
         if (outfil .ne. 'STDOUT') then
            write(7,110) ctxt
         endif
         ctxt = ' '
C
         ctxt(1:22) = 'XHIGH (current)=   '
         if (bitpix .eq. 8 .or. bitpix .eq. 16
     &        .or. bitpix .eq. 32) then
            write(ctxt5,9) xhigh
         else
            write(ctxt5,7) xhigh
         endif
         ctxt(23:35) = ctxt5
         if (outfil .eq. 'STDOUT') call fcecho(ctxt)
         if (outfil .ne. 'STDOUT') then
            write(7,110) ctxt
         endif
C
         ctxt = ' '
         ctxt(1:22) = 'YLOW (current) =   '
         if (bitpix .eq. 8 .or. bitpix .eq. 16
     &        .or. bitpix .eq. 32) then
            write(ctxt5,9) ylow
         else
            write(ctxt5,7) ylow
         endif
         ctxt(23:35) = ctxt5
         if (outfil .eq. 'STDOUT') call fcecho(ctxt)
         if (outfil .ne. 'STDOUT') then
            write(7,110) ctxt
         endif
C
         ctxt = ' '
         ctxt(1:22) = 'YHIGH (current)=   '
         if (bitpix .eq. 8 .or. bitpix .eq. 16
     &        .or. bitpix .eq. 32) then
            write(ctxt5,9) yhigh
         else
            write(ctxt5,7) yhigh
         endif
         ctxt(23:35) = ctxt5
         if (outfil .eq. 'STDOUT') call fcecho(ctxt)
         if (outfil .ne. 'STDOUT') then
            write(7,110) ctxt
         endif

         ctxt = ' '
         ctxt(1:22) = 'ZLOW (current) =   '
         if (bitpix .eq. 8 .or. bitpix .eq. 16
     &        .or. bitpix .eq. 32) then
            write(ctxt5,9) zlow
         else
            write(ctxt5,7) zlow
         endif
         ctxt(23:35) = ctxt5
         if (outfil .eq. 'STDOUT') call fcecho(ctxt)
         if (outfil .ne. 'STDOUT') then
            write(7,110) ctxt
         endif
C
         ctxt = ' '
         ctxt(1:22) = 'ZHIGH (current)=   '
         if (bitpix .eq. 8 .or. bitpix .eq. 16
     &        .or. bitpix .eq. 32) then
            write(ctxt5,9) zhigh
         else
            write(ctxt5,7) zhigh
         endif
         ctxt(23:35) = ctxt5
         if (outfil .eq. 'STDOUT') call fcecho(ctxt)
         if (outfil .ne. 'STDOUT') then
            write(7,110) ctxt
         endif
       endif

 7    format(e12.3)


 999  continue
      ftstatus = 0
      if (inopen) call ftclos(iunit,ftstatus)
 3    format(i4)
 9    format(i6)
 34   format(a50,i6)
 110  format(a80)

      return
      end
