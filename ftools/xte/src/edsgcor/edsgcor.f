C ***************************************************************************
C SELECTOR TASK
C      edsgcor
C
C FILE:
C      edsgcor.f
C
C DESCRIPTION:      
C     Converts an EDS Gain/Offset file from ASCII to FITS
C      
C AUTHOR:
C      James Lochner  8/95
C      
C MODIFICATION HISTORY:
C      
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      corfil	 - name of input ascii file with EDS Gain/Offset values
C      date	 - date on which values take effect
C      time      - time at which values take effect
C      chatter   - how much to tell user
C
C CALLED ROUTINES:
C     subroutine gcorpar  - gets parameters from environment
c     subroutine cor2fits - converts the ascii file to fits
C ************************************************************************** 

      Subroutine EDSGCR

c start with initial declarations
      character(160) corfil
C      character(8) date, time
	character(68) date, time
      
      logical abort
        
      character(40) taskname
      common /task/ taskname

      taskname = 'edsgcor0.0'
      abort = .false.
        
c get the parameters from the par file
      call gcorpar(corfil, date, time)

c Convert the ascii file to a fits file   
      call cor2fits(corfil, date, time)

c  Exit subroutine

	return
	end
C*****************************************************************
C SUBROUTINE:
C      gcorpar
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  5/95
C
C MODIFICATION HISTORY:
C      
C NOTES:
C      gcorpar uses F77/VOS like calls to read parameters from .par file
C
C USEAGE:      
C      call gcorpar(corfil, date)
C      
C ARGUMENTS:
C     corfil	- name of ascii file containing EDS gain/offset values
C     date      - date on which values take effect
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      subroutine uclgst - get string parameter
C      
C *******************************************************************************

      SUBROUTINE gcorpar(corfil,date,time)


c start with the declarations
      character*(*) corfil, date, time
      
      character(80) context
      integer  status
      
      status = 0

      
c get the name of the ascii file containing the gain/offset values
	call uclgst('corfil',corfil,status)
	if (status .ne. 0) then
	    context = 'could not get CORFIL parameter'
	    call fcerr(context)
	    go to 999
	endif

C get date (yymmdd format) on which values take effect
	call uclgst('date',date,status)
	if (status .ne. 0) then
	    context = 'could not get DATE parameter'
	    call fcerr(context)
	    go to 999
	endif

C get time (hhmmss format) on which values take effect
	call uclgst('time',time,status)
	if (status .ne. 0) then
	    context = 'could not get TIME parameter'
	    call fcerr(context)
	    go to 999
	endif


c Exit subroutine
999	continue 
	if (status .ne. 0) call fcerrm(status)

	return
	end
C*****************************************************************
C SUBROUTINE:
C      cor2fits
C
C DESCRIPTION:      
C     Converts an ASCII EDS Gain/Offset file to FITS
C      
C AUTHOR:
C      James Lochner  8/95
C
C MODIFICATION HISTORY:
C	7/6/98 - by ZG, change the string length of outdate and filedate in
C		compliance with new-format date string.
C      
C NOTES:
C
C USEAGE:      
C      call cor2fits(corfil, date)
C      
C ARGUMENTS:
C     corfil	- name of ascii file containing EDS gain/offset values
C     date      - date on which values take effect
C     time      - time at which values take effect
C     
C     PRIMARY LOCAL VARIABLES:
C     filedate - date in 6 character yymmdd for name of output file
C     outdate  - date in dd/mm/yy for output file fits header
C     outtime  - time in hh:mm:ss for output file fits header
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C     subroutine parsdate - parses the input date appropriately
C     subroutine parstime - parses the input time to hh:mm:ss
C *******************************************************************************

      SUBROUTINE cor2fits(corfil,date,time)


c start with the declarations
      character*(*) corfil, date, time

      integer iunit,ounit
      character(70) comment, outfile
      character(80) context
      character(15) ttype(11), extname
C      character(8) outtime, outdate
	character(68) outtime,outdate
C      character(6) filedate      
	character(68) filedate
      character(1) tform(11), tunit(11)
      integer  ftstatus, bitpix, naxis, naxes, pc, gc
      integer tfields, rowlen, frow, felem, blocksize
      
      integer typer(11)
      integer slot0g(11),slot1g(11),slot2g(11),slot3g(11),slot4g(11)
      integer slot0f(11),slot1f(11),slot2f(11),slot3f(11),slot4f(11)
      real*4 aconst, bconst
      
      integer i
      logical ex, simple, extend
      ftstatus = 0

C     Parse the input date and time
      call parsdate(date, filedate, outdate)
      call parstime(time, outtime)
      
C open the ascii file
      call ftgiou(iunit,ftstatus)
      call faopen(iunit,corfil,1,133,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not open ascii gain & offset ile'
         call fcerr(context)
         go to 999
      endif

C open the fits file
      call ftgiou(ounit,ftstatus)
      outfile = 'edsgcor_'//filedate//'.fits'
      inquire(FILE=outfile,EXIST=ex)
      if (ex) then
         open(UNIT=ounit,FILE=outfile,STATUS='unknown')
         close(UNIT=ounit,STATUS='delete')
      endif
      blocksize = 1
      call ftinit(ounit,outfile,blocksize,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not initialize new FITS file'
         call fcerr(context)
         go to 999
      endif
      
C write the primary header 
      simple = .true.
      extend = .true.
      bitpix = 8
      naxis = 0
      naxes = 0 
      pc = 0 
      gc = 0
      call ftphpr(ounit,simple,bitpix,naxis,naxes,pc,gc,extend,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not write required primary keywords'
         call fcerr(context)
         go to 999
      endif

C      Define structure of primary array 
      call ftpdef(ounit,bitpix,naxis,naxes,pc,gc,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not define structure of primary array'
         call fcerr(context)
         go to 999
      endif

C      Create Extension 
      call ftcrhd(ounit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not create new extension'
         call fcerr(context)
         go to 999
      endif

C write the column name info      
      tfields = 11
      rowlen = 44
      ttype(1) = 'TYPER'
      ttype(2) = 'SLOT_0_GAIN'
      ttype(3) = 'SLOT_1_GAIN'
      ttype(4) = 'SLOT_2_GAIN'
      ttype(5) = 'SLOT_3_GAIN'
      ttype(6) = 'SLOT_4_GAIN'
      ttype(7) = 'SLOT_0_OFFSET'
      ttype(8) = 'SLOT_1_OFFSET'
      ttype(9) = 'SLOT_2_OFFSET'
      ttype(10) = 'SLOT_3_OFFSET'
      ttype(11) = 'SLOT_4_OFFSET'
      do i = 1,11
         tform(i) = 'J'
      end do
      
C Write Required Header Keywords 
      extname = 'BIN'
      call ftphbn(ounit,8,tfields,ttype,tform,tunit,extname,
     &     pc,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not write required extension keywords'
         call fcerr(context)
         go to 999
      endif

C Add OGIP and CALDB header Keywords
      call ftpkys(ounit,'TELESCOP','XTE',
     $     'Name of mission/satellite',ftstatus)
      call ftpkys(ounit,'INSTRUME','PCA',
     $     'Name of instrument/detector',ftstatus)
      call ftpkys(ounit,'DETNAM','ALL',
     $     'ALL PCUs are included in dataset',ftstatus)
      call ftpkys(ounit,'FILTER','NONE',
     $     'No filter in use/available',ftstatus)
      call ftpkys(ounit,'CCNM0001','EDS_GCOR',
     $     'CALDB code: EDS gain corrections',ftstatus)
      call ftpkys(ounit,'CCLS0001','BCF',
     $     'CALDB Class: Basic Calibration File',ftstatus)
      call ftpkys(ounit,'CVTP0001','DATA',
     $     'CALDB Type: File contains DATA',ftstatus)
      
C Add the effective date and time
      call ftpkys(ounit,'CVSD0001',outdate,
     $     'CALDB Validity Start date (dd/mm/yy)',ftstatus)
      call ftpkys(ounit,'CVST0001',outtime,
     $     'CALDB Validity Start Time (hh:mm:ss)',ftstatus)

C Add the OGIP Calibration Description keyword
      call ftpkys(ounit,'CDES0001','PCA EDS Gain & Offsets',
     $     ' ',ftstatus)

C Add the author and creation date values
      call ftpkys(ounit,'CREATOR','EDSGCOR/XTE/GOF',
     $     'FTOOL which produced this file',ftstatus)
      call ftpdat(ounit,ftstatus)
      
C Add comment fields to column names
      comment = 'Typer Table Address'
      call ftmcom(ounit,'TTYPE1',comment,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not modify column name comment'
         call fcerr(context)
         go to 999
      endif      
      
C read the initial 49 lines from the ascii file and write them
c     as COMMENT keywords
      call ftpcom(ounit,' ----  Start of Original Ascii Header  ----',
     $     ftstatus)
      do i = 1, 44
         read(iunit,1000) comment
1000     format(' ',A70)
         call ftpcom(ounit,comment,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'could not write the comment keywords'
            call fcerr(context)
            go to 999
         endif
      end do
      call ftpcom(ounit,' ----  End of Original Ascii Header  ----',
     $     ftstatus)

C do nothing with the ascii file's column headers      
      do i = 45,49
         read (iunit,1000) comment
      end do
      
C Read the ascii table, acquiring the columns
      do i = 1,8
         read(iunit,*) typer(i),slot0g(i),slot1g(i),slot2g(i),slot3g(i),
     $        slot4g(i),slot0f(i),slot1f(i),slot2f(i),slot3f(i),
     $        slot4f(i)
      end do

C Define Extension data structure
      call ftbdef(ounit,tfields,tform,pc,8,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not define extension data structure'
         call fcerr(context)
         go to 999
      endif
      
C Write the columns to the FITS file.
      frow = 1
      felem = 1
      call ftpclj(ounit,1,frow,felem,8,typer,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not write typer column'
         call fcerr(context)
         go to 999
      endif
      call ftpclj(ounit,2,frow,felem,8,slot0g,ftstatus)
      call ftpclj(ounit,3,frow,felem,8,slot1g,ftstatus)
      call ftpclj(ounit,4,frow,felem,8,slot2g,ftstatus)
      call ftpclj(ounit,5,frow,felem,8,slot3g,ftstatus)
      call ftpclj(ounit,6,frow,felem,8,slot4g,ftstatus)
      call ftpclj(ounit,7,frow,felem,8,slot0f,ftstatus)
      call ftpclj(ounit,8,frow,felem,8,slot1f,ftstatus)
      call ftpclj(ounit,9,frow,felem,8,slot2f,ftstatus)
      call ftpclj(ounit,10,frow,felem,8,slot3f,ftstatus)
      call ftpclj(ounit,11,frow,felem,8,slot4f,ftstatus)
      
C Write the final two values in ascii file into the FITS header      
      read(iunit,*) aconst,bconst

      call ftpkyf(ounit,'A_CNST',aconst,4,
     $     'channel to energy factor (A)',ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not define extension data structure'
         call fcerr(context)
         go to 999
      endif

      call ftpkyf(ounit,'B_CNST',bconst,1,
     $     'channel offset (B)',ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not define extension data structure'
         call fcerr(context)
         go to 999
      endif

C Compute and Write the DATASUM and CHECKSUM keywords
      call ftpcks(ounit,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'could not write datasum & checksum values'
         call fcerr(context)
         go to 999
      endif
      
C Close the input and output files
      call ftclos(ounit,ftstatus)
      call ftclos(iunit,ftstatus)
      call ftfiou(ounit,ftstatus)
      call ftfiou(iunit,ftstatus)
      
c Exit subroutine
999	continue 
	if (ftstatus .ne. 0) call fcerrm(ftstatus)

	return
	end




C*****************************************************************
C SUBROUTINE:
C      parsdate
C
C DESCRIPTION:      
C      Parses an input date string of the form yymmdd to a 6
C       character string yymmdd and an 8 character string dd/mm/yy.
C      
C AUTHOR:
C      James Lochner 11/95
C
C MODIFICATION HISTORY:
C	7/6/98 - by ZG, change the string length of outdate and slashdate,
C		add an if block to check for new-format date string.
C      
C NOTES:
C
C USEAGE:      
C      call parsdate(indate, outdate, slashdate)
C      
C ARGUMENTS:
C     indate    - input string with date in form yymmdd
C     outdate   - output 6 character string in form yymmdd
c     slashdate - output string with date in from dd/mm/yy
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C     subroutine fcstln - returns length of a character string (integer)
C     subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      
C *******************************************************************************

      SUBROUTINE parsdate(indate, outdate, slashdate)


c start with the declarations
      character*(*) indate
C      character(6) outdate
C      character(8) slashdate
	character(68) outdate, slashdate
      
      integer i, inlength
      integer fcstln, ftstatus
      logical slash
C	by ZG, add logical variable dash to check new-format date string
	logical dash

      slash = .false.
      ftstatus = 0

C	by ZG, add the following statement
	dash = .false.

      inlength = fcstln(indate)
      do i = 1,inlength
         if (indate(i:i) .eq. '/') slash = .true.
C	by ZG, add the following statement to check new-format string
	 if (indate(i:i) .eq. '-') dash = .true.
      end do

          if (slash) then
            outdate = indate(1:2)//indate(4:5)//indate(7:8)
            slashdate = indate(7:8)// '/' //indate(4:5)// '/' //
     $        indate(1:2)
          else
            outdate = indate(1:6)
            slashdate = indate(5:6)// '/' //indate(3:4)// '/' //
     $        indate(1:2)
          endif

C	by ZG, add the following if block to check for new-format date string

	  if (dash) then
	    outdate = indate(1:10)
	    slashdate = indate(9:10)// '/' //indate(6:7)// '/' //
     $        indate(1:4)
	  endif
      
c Exit subroutine
999	continue 
	if (ftstatus .ne. 0) call fcerrm(ftstatus)

	return
	end


C*****************************************************************
C SUBROUTINE:
C      parstime
C
C DESCRIPTION:      
C      Parses an input time string of the form hhmmss to a 
C       character string hh:mm:ss.
C      
C AUTHOR:
C      James Lochner 11/95
C
C MODIFICATION HISTORY:
C      
C NOTES:
C
C USEAGE:      
C      call parstime(intime, outtime)
C      
C ARGUMENTS:
C     intime    - input string with time in form hhmmss
C     outtime   - output 6 character string in form hh:mm:ss
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcstln - returns length of a character string (integer)
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      
C *******************************************************************************

      SUBROUTINE parstime(intime, outtime)


c start with the declarations
      character*(*) intime
      character(8) outtime
      
      integer i, inlength
      integer ftstatus, fcstln
      logical colon

      colon = .false.
      ftstatus = 0
      
      inlength = fcstln(intime)
      do i = 1,inlength
         if (intime(i:i) .eq. ':') colon = .true.
      end do

      if (colon) then
         outtime = intime(1:8)
      else
         outtime = intime(1:2)// ':' // intime(3:4) // ':' //
     $        intime(5:6)
      endif
      
c Exit subroutine
999	continue 
	if (ftstatus .ne. 0) call fcerrm(ftstatus)

	return
	end
