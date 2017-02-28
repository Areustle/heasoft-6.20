CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC eaddmap.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  eaddmap
C
C  This is a rewrite of the old egret routine addmap to convert it to an Ftool
C  Major changes have been made to the following routines:
C
C  eaddmap itself
C      1. Output ascii record now called be a logical number created by fitsio
C         rather than the fixed number 3, which conflicts with fitsio.
C      2. Fitsio errors printed to ascii file with new routine, prterr_file.f
C
C  readin: input no longer comes from an ascii file but rather from the
C          Xanadu Parameter Interface (XPI).
C  nreadft: (formerly readft) all i/o converted to standard fitsio
C  nwrtfts: (formerly nwrtfts) all i/o converted to standard fitsio
C
C   Writing nreadft and nwrtfts, allows us to delete a SUN specific set 
C   of routines.
C
C   Jeff Silvis HSTX     7 Aug 1997
CH1
CH1  $Id: eaddmap.f,v 1.4 1997/11/03 22:55:50 silvis Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - Hughes-STX - 01/29/92
CH1
CH1  Function: Main program to add 2 sky maps in FITS format.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  Sun 4/280 - Fortran 2.1
CH1
CH2  Calling Sequence:  N.A.
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2
CH2  Called by:  None
CH2
CH2  Calls:
CH2   addiff: Add 2 maps that have a different coordinate system
CH2   addsam: Add 2 maps that have the same parameters
CH2   addsys: Add 2 maps that have the same coordinate system
CH2   intens: Generate the intensity map from the counts and exposure maps
CH2   nreadft: Read the input FITS files
CH2   readin: Read the user input and do some initializations
CH2   tstmap: Test the input map read for compatibility with the output map
CH2   nwrtfts: Write the output FITS files
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  sign       I*4        -      Sign in the 2 maps operation (+ or -)
CH3  sgn        I*4        1      Temporary value of sign (first time: +)
CH3  intflg     L*4        -      Determines if intensity map is to be created
CH3  today      Ch*24      -      Current date
CH3  case       Ch*4       -      Case for adding 2 maps
CH3  filnam(3)  Ch*80      -      Names of the maps (1st input, 2nd input, out)
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       3              Program output listing
CH4       6              Terminal output
CH4
CH4  Method:
CH4    Write the program title, version number and date
CH4    Call readin to read the user input and initialize the program
CH4    For (i = 1 to 2) do
CH4       Call nreadft to read the input file i
CH4       Call tstmap to see what case input map i falls into
CH4       if (map i has the same parameters as the output map) then
CH4          Call addsam to add map i to the output map
CH4       if (map i has the same coordinate system as the output map) then
CH4          Call addsys to add map i to the output map
CH4       if (map i has a different coordinate system as the output map) then
CH4          Call addiff to add map i to the output map
CH4       else
CH4          Terminate the program in error
CH4       end if
CH4    Call intens to generate the intensity map if requested
CH4    Call nwrtfts to write the FITS files generated
CH4  End eaddmap
CH4
CH5 $Log: eaddmap.f,v $
CH5 Revision 1.4  1997/11/03 22:55:50  silvis
CH5 The files used to define the global variables were called: oufits.cmn.f
CH5 and infits.cmn.f and the make file was trying to compile them.  Their
CH5 names were changed to oufits.cmn.inc and infits.cmn.inc and
CH5 were changed in the programs that used these files.
CH5
CH5 Revision 1.3  1997/09/18 19:38:13  silvis
CH5 A large number of changes were made to the code to make it compatible
CH5 with g77.  Most of these changes involved shorting certain lines.
CH5
CH5 Jeff Silvis
CH5 HSTX
CH5 18 Sept. 1997
CH5
CH5 Revision 1.2  1997/09/05 19:59:00  silvis
CH5 Several changes were made to the above code to make it run on Linux.  It
CH5 still does not run on Linux but I wanted to archive the code and test it on
CH5 solaris and sun to confirm that it still runs there.
CH5
CH5
CH5 Jeff Silvis
CH5
CH5 Revision 1.1  1997/09/03 20:15:55  silvis
CH5 This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
CH5 two EGRET maps.
CH5
CH5 Jeff Silvis
CH5 3 Sept 1997
CH5 Hughes STX
CH5
C
C   Conversion to Ftool complete 8 Aug 1997
C
C    Jeff Silvis HSTX
C
C Revision 1.3  1993/05/24  13:50:51  albert
C Changed input counts map array from integer to real to preserve precision
C in low counts maps.
C
C Revision 1.2  1992/04/16  16:25:29  albert
C Made the bin data arrays local variables with a maximum size of 648000 and
C passed them to the different routines so that they can be dimensioned
C variably.
C
C Revision 1.1  1992/01/29  19:54:11  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC eaddmap.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine eaddmap()
      implicit none
      integer lunout
      integer   i,sign,sgn,maxsiz
      parameter (maxsiz=2592000)
      real      icntbn(maxsiz)
      real      ocntbn(maxsiz),iexpbn(maxsiz),oexpbn(maxsiz),
     &          ointbn(maxsiz)
      logical   intflg
C*************************************************************************
C
C       Ftool change
C
C    Fitsio is now used to find the date.  So the character today
C    has been replaced with integer day,month,year
C
C      character today*24,case*4,filnam(3)*256,tstmap*4
C
C  JS
C*************************************************************************
      character case*4,filnam(3)*256,tstmap*4
      integer day,month,year
       character        id*80
      integer   status

      common /id/id
      data	sgn/1/
      include 'infits.cmn.inc'
C---> Initialize variables
      id= '$Id: eaddmap.f,v 1.4 1997/11/03 22:55:50 silvis Exp $'
C***************************************************************************
C
C            Ftool Change
C
C      The variable lunout provides a logicial number that is
C      is used for output to an ascii file.  In the original code this
C      was hardwired to be 3.  However the logical number 3 is used
C      by XPI to link with the parameter file, so the hardwiring is
C      fatal!  A value for lunout was assigned by the fitsio
C      routine, ftgiou, in the main routine, eaddamp.  Then lunout
C      is an arguement in any routine that has output to the ascii
C      file.
C
C
C      Jeff Silvis  28 April 1997
C
C***************************************************************************
      call ftgiou(lunout, status)
      open(lunout, file='eaddmap.list')
C***************************************************************************
C    Ftools change
C    fdate is sun dependent and must be replaced with fitsio.
C
C    Write the program title
C      call fdate(today)
C      write(lunout,'(26x,'' A D D M A P   P R O G R A M'',//,28x,a)')
C     &   today
C**************************************************************************
       call ftgsdt(day,month,year,status)
       write(lunout,*) ' A D D M A P   P R O G R A M'
       write(lunout,10) day,month,year
   10  format (i2,'/',i2,'/',i4)
C***************************************************************************
C
C            Ftool Change
C
C  The variable id is no longer used.  There are problems with Linux.
C
C
C
C      write(lunout,'(//,14x,a)') id
C
C
C***************************************************************************
C
C
C---> Read and write the user input
      call readin(lunout,filnam,sign,intflg,maxsiz,ocntbn,oexpbn,ointbn,
     & status,*9000)
C---> Loop for the 2 input maps
      do i=1,2
C------> Read the current input FITS file(s)
         call nreadft(lunout,i,filnam(i),intflg,maxsiz,icntbn,iexpbn,
     &     status,*9000)
C
C***************************************************************************
C
C            Ftool Change
C
C  If the status is greater than 0 then a fitsio error has occurred.  
C  This means that we must not continue the process and print an 
C  error message to the output file.
C 
       if (status .gt. 0 )  then
           goto 9000
       endif
C
C    J. Silvis HSTX
C     6 Aug 1997
C
C***************************************************************************
C------> Compare input map i with the output map
	 case = tstmap(lunout,i,maxsiz)


C------> Add (or subtract) the maps if they are the same
         if (case .eq. 'same') then

	    call addsam(lunout,i,sgn,intflg,icntbn,iexpbn,ocntbn,oexpbn)


C------> Add (or subtract) the maps if they have the same coordinate system
         else if (case .eq. 'csys') then
C
	    call addsys(lunout,i,sgn,intflg,icntbn,iexpbn,ocntbn,oexpbn)


C------> Add (or subtract) the maps if they are different 
	 else if (case .eq. 'diff') then
C
	    call addiff(lunout,i,sgn,intflg,icntbn,iexpbn,ocntbn,oexpbn)

	 else
	    goto 9000
	 end if

         sgn = sign
      end do
C---> Compute the intensity map bins and the output map parameters
      call intens(lunout,intflg,ocntbn,oexpbn,ointbn,*9000)
      call nwrtfts(filnam(3),sign,intflg,ocntbn,oexpbn,ointbn,
     &       status,*9000)
C***************************************************************************
C
C            Ftool Change
C
C  If the status is greater than 0 then a fitsio error has occurred.  
C  This means that we must not continue the process and print an 
C  error message to the output file.
C 
       if (status .gt. 0 )  then
           goto 9000
       endif
C
C    J. Silvis HSTX
C     6 Aug 1997
C
C***************************************************************************      
      write(lunout,'(//,'' eaddmap program terminated'')')
      write(6,*) 'The eaddmap program listing is in file eaddmap.list'
      close(lunout)
      call ftfiou(lunout,status)
      return

9000  write(lunout,'(//,'' eaddmap program terminated due to errors'')')
      write(6,*) 'The eaddmap program listing is in file eaddmap.list'
C***************************************************************************
C
C            Ftool Change
C   The routine below will print any fitsio errors to the file eaddmap.list
C
      call prterr_file(lunout,status)
C***************************************************************************
      close(lunout) 
      call ftfiou(lunout,status)
      return
CCCCCCCCCCCCCCCCCCCCCCC End eaddmap.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end









