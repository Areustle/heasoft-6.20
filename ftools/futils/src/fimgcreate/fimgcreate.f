C******************************************************************************
C FTOOLS TASK:
C      fimgcreate
C
C FILE:
C      fimgcreate.f 
C
C DESCRIPTION: 
C      Create a FITS primary array image from ASCII template file.    
C
C AUTHOR:  
C      William Pence  9/28/95
C
C MODIFICATION HISTORY:
C
c  Banashree M Seifert (March18, 1997)
c      . added option that user can input datafile=none and in that
c        case, the output imagefile will bw with pixels=0.0
c        For this, one new subroutine CRNULLIMG added.
C      01/12/98 PDW 1.0a - Altered crimg to allow input data file to be
C                          stdin, thereby supporting piped data as well as
C                          hand entered.
C      01/21/00 NG  1.0b - Increase the limit to 30000 characters and 
C                          3000 numbers  per line 
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine gimgcr - gets parameters from environment
C      subroutine imgcr -  create the FITS image file
C
C******************************************************************************
      subroutine fimgce

      integer bitpix,nskip,status
      character(160) datfil,outfil,hdfil
      character(80) caxes
      double precision nulval
      logical histry,nulflg

      character(40) taskname
      common /task/ taskname

      taskname = 'fimgcreate 1.0b'
      status=0

C  get the input parameters
      call gimgcr(bitpix,caxes,datfil,outfil,hdfil,nskip,
     &     histry,nulflg,nulval,status)

C  create the FITS file
      if (status .le. 0)then
            call imgcr(bitpix,caxes,datfil,outfil,hdfil,nskip,
     &      histry,nulflg,nulval,status)
      end if

      if (status .gt. 0)call fcerrm(status)
      end
C******************************************************************************
C SUBROUTINE:
C      gimgcr
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C AUTHOR:  
C      William Pence   9/28/95
C
C MODIFICATION HISTORY:
C
C NOTES:
C      gimgcr uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call gimgcr(bitpix,caxes,datfil,outfil,hdfil,nskip,
C            histry,nulflg,nulval,status)
C
C ARGUMENTS:
C    Output:
C      bitpix   - FITS datatype of image
C      caxes    - string giving list of dimensions for image
C      datfil   - name of ASCII data template file
C      outfil   - name of output FITS file
C      hdfil    - name of ASCII header template file
C      nskip    - number of lines in data templete file to skip over
C      histry   - if true, append a history record to the FITS file
C      nulval   - double value to use for undefined values if bitpix = -32, -64
C      status   - returned error status (0 = OK)
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C
C CALLED ROUTINES:
C      subroutine fcerr - echo error message to terminal
C      subroutine uclgst - get string parameter
C      subroutine uclgsi - get integer parameter
C      subroutine uclgsb - get boolian parameter
C
C****************************************************************************** 
      subroutine gimgcr(bitpix,caxes,datfil,outfil,hdfil,nskip,
     &       histry,nulflg,nulval,status)

      character*(*) caxes,outfil,datfil,hdfil
      integer bitpix,nskip,status
      logical histry,nulflg
      double precision nulval
      character(80) context

C  get datatype for output FITS file
      call uclgsi('bitpix',bitpix,status)
      if (status .ne. 0) then
         context = 'could not get bitpix parameter'
         call fcerr(context)
         goto 999
      endif

C  get the string listing size of each data axis
      call uclgst('naxes',caxes,status)
      if (status .ne. 0) then
         context = 'could not get naxes parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the input data template file
      call uclgst('datafile',datfil,status)
      if (status .ne. 0) then
         context = 'could not get datafile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the output FITS file
      call uclgst('outfile',outfil,status)
      if (status .ne. 0) then
         context = 'could not get outfile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the name of the optional input header template file
      call uclgst('headfile',hdfil,status)
      if (status .ne. 0) then
         context = 'could not get headfile parameter'
         call fcerr(context)
         goto 999
      endif

C  get the number of lines to skip in the data template file
      call uclgsi('nskip',nskip,status)
      if (status .ne. 0) then
         context = 'could not get nskip parameter'
         call fcerr(context)
         goto 999
      endif

C  get the history record flag
      call uclgsb('history',histry,status)
      if (status .ne. 0) then
         context = 'could not get history flag'
         call fcerr(context)
         goto 999
      endif

C  test for null values, if bitpix = -32 or -64?
      call uclgsb('nulltest',nulflg,status)
      if (status .ne. 0) then
         context = 'could not get nulltest parameter'
         call fcerr(context)
         goto 999
      endif

C  get double undefined value
      call uclgsd('nullval',nulval,status)
      if (status .ne. 0) then
         context = 'could not get nullval parameter'
         call fcerr(context)
         goto 999
      endif

 999  continue

      return
      end

C******************************************************************************
      subroutine imgcr(bitpix,caxes,datfil,outfil,hdfil,nskip,
     &            histry,nulflg,nulval,status)

      integer bitpix,nskip,status,npixel,intnul
      character*(*) datfil,outfil,hdfil,caxes
      double precision nulval
      logical histry,nulflg
      integer i,naxis,naxes(10)
      character(80) context

      integer ounit

C  parse the caxes string to get number and size of axes
      call gtaxes(caxes,naxis,naxes,status)
      if (status .gt. 0)then
         context='unable to parse the axes sizes: '//caxes
         call fcerr(context)
         go to 900
      end if

C  create the new FITS file
      call ftgiou(ounit,status)
      call ffinit(ounit,outfil,status)
      if (status .gt. 0)then
         context='unable to create the FITS file; may exist? '//outfil
         call fcerr(context)
         go to 999
      end if

C  write the required primary array keywords
      call ftphpr(ounit,.true.,bitpix,naxis,naxes,0,0,.true.,status)
      if (status .gt. 0)then
         context='unable to write primary header keywords'
         call fcerr(context)
         go to 999
      end if

C  If this is an integer image with null values, write the BLANK keyword
      if (nulflg .and. bitpix .gt. 0)then
         intnul = nulval
         call ftpkyj(ounit,'BLANK',intnul,'Null pixel value',status)
      end if

C  force FITSIO to read the header now.  This must be done now, so that
C  it will not scale the data by any BSCAL or BZERO keyword which
C  may be added next.
      call ftrdef(ounit,status)

C  process the header template file if specified by the user and add keywords
      call addhd(ounit,hdfil,status)
      if (status .gt. 0)go to 999

C  write history record, if required
      if (histry)then
         call timestamp(ounit)
         call ftpdat(ounit,status)
      end if

      npixel=1
      do 10 i=1,naxis
         npixel=npixel*naxes(i)
10    continue

      if ((datfil(1:1) .eq. 'n' .and. datfil(2:2).eq.' ') 
     + .or. (datfil(1:1) .eq. 'N' .and. datfil(2:2).eq.' ')
     + .or. (datfil(1:4) .eq. 'none' .and. datfil(5:5).eq.' ') 
     + .or. (datfil(1:4) .eq. 'NONE' .and. datfil(5:5).eq.' ')
     + ) then
           call crnullimg(ounit,nulval,npixel,status)
      else
C          process the data template file and write the image data
           call crimg(ounit,datfil,nulflg,nulval,nskip,npixel,status)
      endif

999   continue
C  close  or delete the file
      if (status .le. 0) then
          call ftclos(ounit,status)
      else
C         delete output file since there was an error
          call ftdelt(ounit, status)
      endif

      call ftfiou(ounit,status)
900   continue
      end

C******************************************************************************
C SUBROUTINE:
C      addhd
C
C DESCRIPTION: 
C	process the ASCII header template file
C
C AUTHOR:  
C      William Pence   9/28/95
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call addhd(ounit,hdfil,status)
C
C ARGUMENTS:
C	ounit  - fortran unit number to use to write to FITS file
C	hdfil - name of the header template file
C	status - returned error status
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine addhd(ounit,hdfil,status)

      integer aunit,ounit,status,hdtype
      character*(*) hdfil
      character tmplte*100,recstr*80,context*80

      if (hdfil .ne. ' ')then
         call ftgiou(aunit,status)
         open(unit=aunit,file=hdfil,status='OLD',err=997)
         
C   read next line from template file and write it to FITS file
 10      read(aunit,1000,end=999,err=998)tmplte
 1000    format(a)

C   ignore blank or comment lines in the template file
         if ((tmplte .eq. ' ') .or. (tmplte(1:1) .eq. '#')) goto 10

C   parse the template and return 80 character keyword record
         call ftgthd(tmplte,recstr,hdtype,status)
         if (status .lt. 0)go to 999

         if (hdtype .eq. 0 .or. hdtype .eq. 1)then
C   append the record to the FITS file
            call ftprec(ounit,recstr,status)
            go to 10
         else if (hdtype .eq. -1)then
C   this was a keyword to be deleted; ignore it
            go to 10
         else
C   this was the END record, so quit
            go to 999
         end if

 997     continue
C   error opening template file
         context='error opening header template file'
         call fcerr(context)
         status=104
         call ftfiou(aunit,status)
         return

 998     continue
C    error reading template file
         context='error reading header template file'
         call fcerr(context)
         status=108

 999     continue
         close(unit=aunit)
         call ftfiou(aunit,status)
      end if
      end

C******************************************************************************
C SUBROUTINE:
C      crimg
C
C DESCRIPTION: 
C      process the ASCII data template file and write the data to the FITS file
C
C AUTHOR:  
C      William Pence   9/28/95
C
C MODIFICATION HISTORY:
C      01/12/98 - PDW: Allowed input data file to come from stdin, thereby
C                      supporting piped data as well as hand entered.
C
C NOTES:
C
C USAGE:
C       call crimg(ounit,datfil,nulflg,nulval,nskip,npixel,status)
C ARGUMENTS:
C	ounit  - fortran unit number to use to write to FITS file
C	datfil - name of the template file
C       nulflg - interpret values = nulval as undefined pixels?
C       nulval - pixels equal to this value are set undefined, if NULFLG = T
C	nskip  - number of rows to skip at the beginning of the template file
C	npixel - number of pixels to write
C	status - returned error status (0=OK)
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine gtoken - find starting position and width of each token 
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C****************************************************************************** 
      subroutine crimg(ounit,datfil,nulflg,nulval,nskip,npixel,status)

      integer aunit,ounit,nskip,npixel,status
      logical nulflg
      double precision nulval
      character*(*) datfil
      integer i,j,s1,s2,fields,begcol(3000),twidth(3000),ntodo,fpixel
      character dtline*30000,svalue*30000,context*80
      double precision dvalue(3000)

C  open the data template file
      if (datfil(1:1).eq.'-') then
         aunit = 5
      else
         call ftgiou(aunit,status)
         open(unit=aunit,file=datfil,status='old',err=997)
      endif

C  skip lines at beginning of data template file
      do 10 i = 1,nskip
         read(aunit,1000,end=998,err=998)dtline
 1000    format(a)
 10   continue	

      ntodo=npixel
      fpixel=1

      do 20 j=1,100000000

C  read next line
         read(aunit,1000,end=999,err=998)dtline
         if (dtline(1:1) .eq. '#' .or. dtline .eq. ' ')go to 20

C  find start and width of each token on the line
          call gtoken(3000,dtline,fields,begcol,twidth)
          fields=min(fields,ntodo)
C  convert each token to a double precision value
          do 30 i=1,fields
            s1=begcol(i)
            s2=begcol(i)+twidth(i)-1
            call ftc2dd(dtline(s1:s2),dvalue(i),status)
            if (status .ne. 0)then
               context='error reading values in template file'  
               call fcerr(context)
               go to 999
            end if
 30       continue

C  write the pixels to the FITS file
         if (nulflg)then
            call ftppnd(ounit,1,fpixel,fields,dvalue,nulval,status)      
         else
            call ftpprd(ounit,1,fpixel,fields,dvalue,status)      
         end if

         if (status .ne. 0)then
            context='error writing pixel values to FITS file'
            call fcerr(context)
             go to 999
         end if

C  continue loop if there are still more pixels to do
         ntodo=ntodo-fields
         fpixel=fpixel+fields
         if (ntodo .le. 0)go to 999
20    continue
     
 997  continue
C  error opening template file
      context='error opening data template file'
      call fcerr(context)
      call ftfiou(aunit,status)
      status=104
      return

 998  continue
C  error reading template file
      context='error reading data template file'
      call fcerr(context)
      status=108

 999  continue
      if (aunit.ne.5) then
         close(aunit)
         call ftfiou(aunit,status)
      endif
      end

C**********************************************************
      subroutine gtaxes(string,nvals,values,status)

C     parse the input caxes string of integers, and return
C     an array of upto 10 integers

C      string - input string representing a list of integers separated
C              by commas
C      nvals - output number of integers in the list
C      values - output array of integers from the list

C     the values in string must be separated by a commas without white space
C     (e.g., "300,200,500")

      character*(*) string
      integer nvals, values(*),status
      character(30) list(30)
      character(80) context
      integer i, j, fcstln, stlen, list_index

C  get the number and list of integers
      stlen = fcstln(string)
      list_index = 1
      i = 1
      j = 1
 10   if (i .eq. stlen) goto 12
      if (string(i:i) .eq. ',') goto 11
      i = i + 1
      goto 10
 11   list(j) = string(list_index:i-1)
      i = i + 1
      list_index = i
      j = j + 1
      goto 10
 12   list(j) = string(list_index:i)
      nvals = j

C  convert the strings to integers
      do 20 i = 1, nvals
            read(list(i),1000,err=999) values(i)
 20   continue
 1000 format(BN,I9)

      return

C error in parsing row range list
 999  context = ' Error in parsing dimension: ' // list(i)
      call fcerr (context)
      status=900
      end

c************************************************************
C SUBROUTINE:
C      crnullimg
C
C DESCRIPTION:
C      Creates a empty image with pixels = 0.0
C
C AUTHOR:
C      Banashree M Seifert(March 19, 1997) v1.0 
C
C MODIFICATION HISTORY:
C
C USAGE:
C      call crnullimg(ounit,nulval,npixel,status)
C
C ARGUMENTS:
C       ounit  - fortran unit number to use to write to FITS file
c       nulval - double value to use for pixels if bitpix = -32, -64
c       npixel - number of pixels to write
C       status - returned error status
C
C PRIMARY LOCAL VARIABLES:
c       context -- str -- writing out the info
c       atatime -- int -- to write 3000 values at a time
c       fpixel  -- int -- first pixel no. to write
c       todo    -- int -- no. of pixel left to do 
c       dvalue  -- dbl -- values to put in
c       again   -- log -- if to go for the loop 
c
C******************************************************************************
      subroutine crnullimg(ounit,nulval,npixel,status)

      implicit none
      integer ounit,npixel,status
      double precision nulval

c----------- internals --------------------------
      character(80) context
      integer atatime, fpixel,todo,i
      parameter (atatime=3000)
      double precision dvalue(atatime)
      logical again

c----------------------------------------------------------------
c fills in the empty imagefile with dvalue 
c It does 3000 dvalues at a time to avoid allocating huge space 
c fpixel is the first no. of the pixel
c again -- true, then do the loop
c          false, get out of the loop
c --------------------------------------------------------------
      fpixel = 1
      again = .true.
      do while(again)

         todo=min(atatime,npixel)
         do i=1,todo
            dvalue(i)=nulval
         enddo

         status=0
         call ftpprd(ounit,1,fpixel,todo,dvalue,status)
         again = .false. 
         if(status .ne. 0) then
            context='Error writing pixel values to FITS file'
            call fcerr(context)
            return
         endif

         fpixel = fpixel + todo
         npixel = npixel - todo
         if(npixel .gt. 0) again = .true.

      enddo

      context='Written the output image .........'
      call fcecho(context)
 
      return
      end

c -------------------------------------------------------------------------
c           end of crnullimg
c -------------------------------------------------------------------------

