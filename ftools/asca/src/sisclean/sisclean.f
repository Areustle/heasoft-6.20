C*************************************************************************
C SUBROUTINE:
C	SISCLEAN
C
C FILE: 
C	sisclean.f
C
C DESCRIPTION:
C	This is the original siscleaning routine.  It takes an input
C	image file, and plots the integral of the number of pixels with
C       n counts against n.  Then the user inputs a cutoff ('heat factor')
C       and a region file is written eliminating all pixels with more
C       counts than the cutoff value.  Also a cleaned image file 
C       is written.  
C
C AUTHOR/DATE:
C	Jim Ingham 8/2/93
C
C MODIFICATION HISTORY:
C       3/9/94 EAG 2.8a add dynamic memory, rationalize string lengths
C       3/6/98 PDW 2.8b Fix log(0) problem in Poisson calculation in
C                       sisclean.
C       
C
C NOTES:
C	
C
C
C USAGE:
C	call siscln 
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:	
C     sisreg -   The output cleaning region file
C     imfil  -   The input image file
C     clnim  -   The output cleaned image
C     PLOT   -   Draw the plot.
C     plotdv -   The plotting device
C     pixcut -   The heat factor
C     fstat  -   The status flag
C	
C CALLED ROUTINES:
C     sclgpar  - gets all the parmeters but the heat factor	
C     sisclean - the main algorithm
C     fcerr    - sends error message to stderr
C
C******************************************************************************
      subroutine siscln      

      character(160) sisreg,imfil,clnim
      character(80)  plotdv
      integer fstat, pointer, idim, jdim
      logical plot

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
C	datatype	value
C	logical		1
C	integer*2	3
C	Integer		4
C	Long Integer	5
C	Real		6
C	Double		7
C	Complex		8

      character(40) taskname
      common /task/ taskname

      taskname = 'sisclean2.8b'

c Start with a zero status value:        
      fstat = 0

C First get all the parameters but the heat factor:

      call sclgpar(sisreg,imfil,clnim,plotdv,plot,fstat)
      IF(fstat.ne.0) then
         call fcerr('Error in sclgpar')
         goto 999
      ENDIF

c     read in FITS image input file:
      
      call clnrdi (imfil, idim, jdim, pointer, fstat)
      if (fstat .ne. 0) goto 999

C Now call the main algorithm

      call sisclean (sisreg, imfil, memi(pointer), idim, jdim, clnim,
     &     plotdv, plot, fstat)
      IF(fstat.ne.0) call fcerr('Error in sisclean')

      fstat = 0
      call udmfre (pointer, 4, fstat)

 999  return
      end


C*************************************************************************
C SUBROUTINE:
C	SCLGPAR
C
C FILE: 
C	sisclean.f
C
C DESCRIPTION:
C	This is the original siscleaning routine.  It takes an input
C	image file, and plots the integral of the number of pixels with
C   n counts against n.  Then the user inputs a cutoff ('heat factor')
C   and a region file is written eliminating all pixels with more
C   counts than the cutoff value.  Also a cleaned image file 
C   is written.  
C
C AUTHOR/DATE:
C	Jim Ingham 8/2/93
C
C MODIFICATION HISTORY:
C       
C
C NOTES:
C	
C     Eric Gotthelf, GSFC/NASA ASCA Guest Observer Facility.
C     April 26 1993. 
C
C     Keith Arnaud. Modified to use Keith Gendreau's method
C                   of plotting up the histogram then choosing
C                   a heat factor.
C     April 28 1993.
C
C     A thought platform. Not for release.
C
C Nonetheless, put into XSELECT by popular demand,
C     Jim Ingham  5/93
C     
C Put into Ftool form on 8/93
C
C     There is no right way to do this without comprimising the 
C     statistical quality of the original data. 
C     
C     RESULTS DEPEND ON CHARACTERISTICS OF INPUT FIELD. 
C
C     ABSOLUTLY NO GUARANTIES, WHETHER EXPRESSED OR IMPLIED COMES 
C     WITH THIS PROGRAM! DON'T ASSUME ANYTHING! 
C     NOTHING IS CLAIMED TO BE CORRECT.
C     
C	
C
C USAGE:
C	call sclgpar(sisreg,imfil,clnim,plotdv,plot,fstat)       
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:	
C     sisreg -   The output cleaning region file
C     imfil  -   The input image file
C     clnim  -   The output cleaned image
C     PLOT   -   Draw the plot.
C     plotdv -   The plotting device
C     pixcut -   The heat factor
C     fstat  -   The status flag
C	
C CALLED ROUTINES:
C     
C     
C     fcerr    - sends error message to stderr
C
C******************************************************************************
      subroutine sclgpar(sisreg,imfil,clnim,plotdv,plot,fstat)      

      character*(*) sisreg,imfil,clnim,plotdv
      character(80) context
      integer fstat
      logical plot
      
C   First read in the parameters

      call uclgst('imfil',imfil,fstat)
      if(fstat.ne.0) then
         context = 'could not get imfil parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgst('clnim',clnim,fstat)
      if(fstat.ne.0) then
         context = 'could not get clnim parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgst('sisreg',sisreg,fstat)
      if(fstat.ne.0) then
         context = 'could not get sisreg parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgst('plotdv',plotdv,fstat)
      if(fstat.ne.0) then
         context = 'could not get plotdv parameter'
         call fcerr(context)
         goto 999
      endif

      call uclgsb('plot',plot,fstat)
      if(fstat.ne.0) then
         context = 'could not get plot parameter'
         call fcerr(context)
         goto 999
      endif

 999  return
      end	
      

C*************************************************************************
C SUBROUTINE:
C	SISCLEAN
C
C FILE: 
C	sisclean.f
C
C DESCRIPTION:
C	This is the original siscleaning routine.  It takes an input
C	image file, and plots the integral of the number of pixels with
C       n counts against n.  Then the user inputs a cutoff ('heat factor')
C       and a region file is written eliminating all pixels with more
C       counts than the cutoff value.  Also a cleaned image file 
C       is written.  
C
C AUTHOR/DATE:
C	Jim Ingham 8/2/93
C
C MODIFICATION HISTORY:
C       PDW 3/6/98: Keep track of minimum pixel count number, in case
C                   there are no pixels having 0 counts, don't want
C                   to do log(0) in Poisson calculation
C       
C
C NOTES:
C	
C     Eric Gotthelf, GSFC/NASA ASCA Guest Observer Facility.
C     April 26 1993. 
C
C     Keith Arnaud. Modified to use Keith Gendreau's method
C                   of plotting up the histogram then choosing
C                   a heat factor.
C     April 28 1993.
C
C     A thought platform. Not for release.
C
C Nonetheless, put into XSELECT by popular demand,
C     Jim Ingham  5/93
C     
C Put into Ftool form on 8/93
C
C     There is no right way to do this without comprimising the 
C     statistical quality of the original data. 
C     
C     RESULTS DEPEND ON CHARACTERISTICS OF INPUT FIELD. 
C
C     ABSOLUTLY NO GUARANTIES, WHETHER EXPRESSED OR IMPLIED COMES 
C     WITH THIS PROGRAM! DON'T ASSUME ANYTHING! 
C     NOTHING IS CLAIMED TO BE CORRECT.
C     
C	
C
C USAGE:
C	call sisclean(sisreg,imfil,clnim,plotdv,plot,fstat) 
C	
C ARGUMENTS:
C	
C
C PRIMARY LOCAL VARIABLES:	
C     sisreg -   The output cleaning region file
C     imfil  -   The input image file
C     clnim  -   The output cleaned image
C     PLOT   -   Draw the plot.
C     plotdv -   The plotting device
C     pixcut -   The heat factor
C     fstat  -   The status flag
C	
C CALLED ROUTINES:
C     clnrdi   - reads the image into a pgplot format array	
C     clnwti   - writes the cleaned image
C     plt      - plots the Gendreaux plot
C     fcerr    - sends error message to stderr
C
C******************************************************************************
      subroutine sisclean(sisreg, imfil, imag, idim, jdim, clnim,
     &     plotdv, plot, fstat)     

      character*(*) sisreg,clnim,plotdv, imfil
      integer fstat,pixcut
      logical plot
      
      integer idim, jdim, i, j, ncmd, index, fcstln
      INTEGER nremve, len1, ierr
      integer imag(idim, jdim)

      real histo(100,3)
      REAL mean
      integer iery(3), ilun, minidx
      
      character(80) str1,context
      character(20) string
      character(60) cmd(10)
      
c     start:
      
c  create histogram of pixel values

      do 50 i = 1, 100
         histo(i,1) = i-1
         histo(i,2) = 0
         histo(i,3) = 0
 50   continue
      minidx = 100
      do 55 j = 1, jdim
         do 57 i = 1, idim
            index = imag(i, j)+1
            if (index .gt. 0 .and. index .le. 100) then
               histo(index,2) = histo(index,2) + 1
               minidx = min(minidx,index)
            endif
 57      continue
 55   continue

c  Also calculate a Poisson distribution whose peak matches the
c  histo(1) data.

      histo(minidx,3) = histo(minidx,2)
      mean = -log(histo(minidx,3)/idim/jdim)
      do 60 i = minidx+1, 100
         histo(i,3) = histo(i-1,3) * mean / float(i-minidx)
 60   continue

c  Convert the observed and Poisson distributions from differential
c  to integral.

      do 65 i = 99, 1, -1
         histo(i,2) = histo(i,2) + histo(i+1,2)
         histo(i,3) = histo(i,3) + histo(i+1,3)
 65   continue

      if(plot) then
c  plot the image histogram

         iery(1) = 0
         iery(2) = 0
         iery(3) = 0
         cmd(1) = 'LA X Counts/pixel'
         cmd(2) = 'LA Y Number of pixels'
         cmd(3) = 'LSTYLE 2 ON 3'
         cmd(4) = 'log y'
         write(string,*) histo(1,3)*1.1
         cmd(5) = 'r 0 10 1 '//string
         cmd(6) = 'LA T '//imfil
         if(plotdv.ne.'NONE') then
            len1 = fcstln(plotdv)
            cmd(7) = 'DEVICE '//plotdv(1:len1)
            ncmd = 7
         else
            ncmd = 6
         endif
         call plt(histo, iery, 100, 100, 3, cmd, ncmd, ierr)
         
      ENDIF
C Now get the heat factor:      
      call uclgsi('pixcut',pixcut,fstat)
      if(fstat.ne.0) then
         context = 'could not get pixcut parameter'
         call fcerr(context)
         goto 999
      endif
      
      
c Now go back round the image and write out positions with number
c of counts exceeding the heat factor. Set the image value to zero
c for these pixels

      ilun = 15
      open(unit=ilun, file=sisreg, status='NEW')

      write(ilun,'(a)') 
     &     '# Hot pixel exclusion list generated by SISCLEAN'

      nremve = 0
      do 70 j = 1, jdim
         do 75 i = 1, idim
            if(imag(i,j) .ge. pixcut) THEN
               write(ilun,'('' -POINT('',i4,'','',i4,'')'')') i, j
               imag(i,j) = 0
               nremve = nremve + 1
            endif
 75      continue
 70   continue

      close(ilun)

      write(str1,'(i5, '' pixels removed from image '')') nremve
      call fcecho(str1(:fcstln(str1)))
      call clnwti(clnim, idim, jdim, imag, fstat)
      if(fstat.ne.0) then
         call fcerr('Error in clnwti')
         goto 999
      endif
      
 999  return
      end     

C*************************************************************************
C SUBROUTINE:
C	CLNRDI
C
C FILE: 
C	sisclean.f
C
C DESCRIPTION:
C   This reads the fits image into a PGPLOT array.	
C	
C
C AUTHOR/DATE:
C	K. Arnaud 4/93
C   Made into Ftool:
C   J. Ingham 8/3/93
C
C MODIFICATION HISTORY:
C       PDW 03/06/98: Initialize null value before reading image
C       
C
C NOTES:
C	
C	
C
C USAGE:
C	call clnrdi (imfil, idim, jdim, imag,fstat)
C	
C ARGUMENTS:
C     filename  -   The input image file
C     idim      -   X dimension of input image
C     jdim      -   Y dimension of input image
C     matrix    -   Stores the input image
C     fstat     -   Error flag
C
C PRIMARY LOCAL VARIABLES:	
C	
C CALLED ROUTINES:
C     ft*      - FITSIO routines	
C     fcerrm   - translates FITSIO error messages
C     fcerr    - sends error message to stderr
C
C******************************************************************************
      subroutine clnrdi(filename,idim,jdim,pointer,fstat)


      character*(*)filename
      integer idim, jdim
      integer pointer, fstat

c	FITS declarations
      integer nullval
      integer iunit,bitpix,naxis,pcount,gcount
      integer group, rwmode, blocksize, ival, status
      logical extend,anyf
      character(80)  comment

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
C	datatype	value
C	logical		1
C	integer*2	3
C	Integer		4
C	Long Integer	5
C	Real		6
C	Double		7
C	Complex		8
      
c	start:

c	intialize variables:

      iunit=15
      rwmode = 0
      bitpix=32
      pcount=0
      gcount=1
      extend=.false.

      group  = 0

c	start:

c	write out FITS image of correction matrix:
C       open the new FITS file:

      call ftopen(iunit,filename,rwmode,blocksize,fstat)
      if(fstat.ne.0) then
         comment = 'Error opening image file '//filename
         call fcerr(comment)
         call fcerrm(fstat)
         goto 999
      endif

      call ftgkyj(iunit, 'NAXIS', ival, comment, fstat)
      naxis = ival

      if(fstat.ne.0) then
         call fcerr('Error getting NAXIS keyword')
         call fcerrm(fstat)
         goto 999
      else  if (naxis .ne. 2) then
         call fcerr('Image is not a 2-dim array')
         fstat = -10
         goto 999
      end if 

      call ftgkyj(iunit, 'NAXIS1', ival, comment, status)
      idim = ival
      call ftgkyj(iunit, 'NAXIS2', ival, comment, status)
      jdim = ival
      if(fstat.ne.0) then
         call fcerr('Error getting NAXIS keywords from image file.')
         call fcerrm(fstat)
         goto 999
      endif

C allocat the matrix dynamically
      pointer = 0
      call udmget (idim*jdim, 4, pointer, fstat)
      if (fstat .ne. 0) then
         Call fcerr (' Error allocating dynamic memory')
         goto 999
      endif


C	read the primary array of data:

      nullval = -1
      call ftg2dj(iunit,group,nullval,idim,idim,jdim,
     &     memi(pointer),anyf,fstat)
      
      if (anyf) then
         call fcecho('WARNING: undefined element encountered.')
      endif
      if(fstat.ne.0) then
         call fcerr('Error getting data from image file.')
         call fcerrm(fstat)
         status = 0
         call udmfre (pointer, 4, status)
      endif
      
      status = 0
      call ftclos(iunit, status)

 999  return
      end
      
C*************************************************************************
C SUBROUTINE:
C	clnwti
C
C FILE: 
C	sisclean.f
C
C DESCRIPTION:
C   This writes the cleaned image.	
C	
C
C AUTHOR/DATE:
C	K. Arnaud 4/93
C   Made into Ftool:
C   J. Ingham 8/3/93
C
C MODIFICATION HISTORY:
C       
C
C NOTES:
C	
C USAGE:
C	call clnwti(clnim, idim, jdim, imag, fstat)
C	
C ARGUMENTS:
C     clnim     -   The cleaned image file
C     idim      -   X dimension of input image
C     jdim      -   Y dimension of input image
C     imag      -   Stores the input image
C     fstat     -   Error flag
C
C PRIMARY LOCAL VARIABLES:	
C	
C CALLED ROUTINES:
C     ft*      - FITSIO routines	
C     fcerrm   - translates FITSIO error messages
C     fcerr    - sends error message to stderr
C
C******************************************************************************
      subroutine clnwti(filename,idim,jdim,matrix,fstat)

c   subroutine arguments:

      implicit none

      character*(*) filename
      integer idim, jdim
      integer matrix(idim, jdim),fstat

c	FITS declarations

      integer iunit,bitpix,naxis,naxes(2),pcount,gcount
      integer group
      logical simple,extend

c	start:

c	intialize variables:

      iunit=15
      simple=.true.
      bitpix=32
      naxis=2
      naxes(1)=idim
      naxes(2)=jdim
      pcount=0
      gcount=1
      extend=.false.

      group  = 1

c	start:

c	write out FITS image of correction matrix:
C       open the new FITS file:

      call ftinit(iunit,filename,2880,fstat)
      if(fstat.ne.0) then
         call fcerr('Error opening output cleaned image.')
         call fcerrm(fstat)
         goto 999
      endif

C       write the required primary array keywords:

      call ftphpr(iunit,simple,bitpix,naxis,naxes,pcount,gcount,
     &     extend,fstat)
      if(fstat.ne.0) then
         call fcerr('Error writing output cleaned image keywords.')
         call fcerrm(fstat)
         goto 999
      endif
      

C	define primary array structure:

      call ftpdef(iunit,bitpix,naxis,naxes,pcount,gcount,fstat)
      if(fstat.ne.0) then
         call fcerr('Error defining output cleaned image structure.')
         call fcerrm(fstat)
         goto 999
      endif

C	write the primary array of data:

      call ftp2dj(iunit,group,idim,idim,jdim,matrix,fstat)
      if(fstat.ne.0) then
         call fcerr('Error writing output cleaned image.')
         call fcerrm(fstat)
         goto 999
      endif

      call ftclos(iunit,fstat)

 999  return
      end

