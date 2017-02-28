CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(IMREADAT) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  IMREADAT
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: readat.f,v 1.5 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Reads the bin data from the FITS file and stores it in
CH1            an array. The data is read differently depending on
CH1            whether it is rectangular, polar or Aitoff.
c              In the Linux version, bytes are reversed to be 
c              compatible with Unix.  DLB.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call IMREADAT
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2
CH2  Called by:  READFT
CH2
CH2  Calls:
CH2   FREAD : System routine to read a record
CH2   KMVC  : System routine to move bytes
CH2
CH3  COMMON Use:
CH3  COMMON Block Name: FITSDT (Holds the FITS file variables)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 bitpix     Integer   Number of bits per pixels
CH3 naxis      Integer   Number of axis in the map
CH3 naxis1     Integer   Number of bins on the 1st axis
CH3 naxis2     Integer   Number of bins on the 2nd axis
CH3 naxis3     Integer   Number of bins on the 3rd axis
CH3 bscale(3)  Real      Bin scaling factor (counts, exposure, intensty)
CH3 bzero(3)   Real      Bin offset value (counts, exposure, intensty)
CH3 ftparm     Real*4    5 parameters (200 groups). index 1 to 5 are:
CH3   (5,200)            1:number bins in group,   2:position on axis1,
CH3                      3:position on axis2,      4:increment on axis1,
CH3                      5:increment on axis 2
CH3 gridtp     Ch*4      Grid type ('RECT', 'POLA' or 'AITF')
CH3 headpf(2)  Real      Two pointers for header buffer
CH3 evclas     Integer   Event class
CH3 energy     Real      Energy level ranges
CH3  (2,10)
CH3 pcount     Integer   Number of parameters in FITS file
CH3 gcount     Integer   Number of groups in FITS data
CH3 naxs12(200)Integer   Number of bins on axis with variable # of bins
CH3 crval1     Real      Coordinate of reference point on axis 1
CH3 crpix1     Real      Array index of reference point on axis 1
CH3 cdelt1     Real      Increment of coordinate along axis 1
CH3 crval2     Real      Coordinate of reference point on axis 2
CH3 crpix2     Real      Array index of reference point on axis 2
CH3 cdelt2     Real      Increment of coordinate along axis 2
CH3 coords     Ch*4      Coordinate system used ('GALA' or 'CELE')
CH3 buffer(3)  Ch*2880   FITS record buffer (may hold up to 3 header rc)
CH3 cntbin     Integer   Counts map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3 expbin     Real      Exposure map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3 intbin     Real      Intensity map bins (up to 10 energy levels)
CH3 (200,200,10)
CH3
CH3  COMMON Block Name: GLOBAL (Holds the main variables)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 strtim     Real*8    FITS data start time (TJD & MSD combined)
CH3 endtim     Real*8    FITS data end time (TJD & MSD combined)
CH3 retcod     Integer   Program return code (0 = normal)
CH3 specin(10) Real      Spectral indexes
CH3 walldi     Integer   Wall distance (from SAGE Namelist)
CH3 maxlev     Integer   Maximum number of energy levels (10)
CH3 tascco     Integer   TASC in coincidence flag
CH3 acs        Integer   ACS value
CH3 tunit      Integer   Unit number for timeline file
CH3 eunit      Integer   Unit number for exposure history file
CH3 calfil(2)  Ch*8      Names of the calibration files
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  exgbin     R*4        -      Exposure bins for galactic disk maps
CH3  cngbin     I*4        -      Counts bins for galactic disk maps
CH3  length     I*4        -      Length of record read
CH3  wrdlen     I*4        -      Number of bytes per word
CH3  index      I*4        -      Index into the input buffer
CH3  idat       I*4        -      Raw data read in I*4 format
CH3  idat2      I*4        -      Raw data read in I*2 format
CH3  g          I*4        -      FITS data group index
CH3  p          I*4        -      FITS data parameter in group index
CH3  buff(2880) Ch*1       -      Input buffer for FITS record
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       1              Input FITS file
CH4       6              Printer report
CH4
CH4  Method:
CH4     Initialize variables
CH4     Read the first FITS data record
CH4     If (the map is rectangular) then
CH4        For (k=1 to number of energy levels) do
CH4           For (j=1 to number of bins on axis 2) do
CH4              For (i=1 to the number of bins on axis 1) do
CH4                 Get the bin data at the current buffer index
CH4                 Scale and store the bin data
CH4                 Initialize the exposure bin to 0
CH4                 Increment the input buffer index
CH4                 If (the index is > 2880) then
CH4                    Read the next input record from the FITS file
CH4                    Set the record index to 1
CH4                 End if
CH4              End for
CH4           End for
CH4        End for
CH4     Else if (the map is polar) then
CH4        For (k=1 to number of energy levels) do
CH4           For (g=1 to number of groups) do
CH4              For (p=1 to the number of group parameters) do
CH4                 Get the next group parameter
CH4                 Read a new input record if needed
CH4              End for
CH4              For (n=1 to the number of bins in the group)
CH4                 Compute the i and j array indexes
CH4                 Get the next bin value and scale it
CH4                 Initialize the exposure bin to 0
CH4                 increment the input buffer index
CH4                 if (the index is > 2880) then
CH4                    read the next input record from the FITS file
CH4                    set the record index to 1
CH4                 end if
CH4              end for
CH4           end for
CH4        end for
CH4     Else if (the map is Aitoff) then
CH4        For (k=1 to number of energy levels) do
CH4           For (j=1 to number of groups) do
CH4              For (p=1 to the number of group parameters) do
CH4                 Get the next group parameter
CH4                 Read a new input record if needed
CH4              End for
CH4              For (i=1 to the number of bins in the group)
CH4                 Get the next bin value and scale it
CH4                 Initialize the exposure bin to 0
CH4                 increment the input buffer index
CH4                 if (the index is > 2880) then
CH4                    read the next input record from the FITS file
CH4                    set the record index to 1
CH4                 end if
CH4              end for
CH4           end for
CH4        end for
CH4     end if
CH4  End IMREADAT
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Changed include(file) to include 'file.cmn'.
CH5				Changed ! comments to C comments.
CH5				Merged changes from version 1.03 on IBM.
CH5				Corrected problem when the next
CH5                             FITS buffer to read is at i=naxis1
CH5                             and j=naxis2.
CH5
c
c      Linux version.  2001/08/08  D.L.Bertsch.
c      The main differences are in the calls to ftio (fread, etc.,) 
c      and in reversing the bytes for compatibility with Unix.
c
CH5 $Log: readat.f,v $
CH5 Revision 1.5  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.4  2006/04/17 20:54:27  irby
CH5 Updates for compliance with g95 and gfortran:
CH5
CH5 - Replace call to lnblnk with equivalent call to len_trim.
CH5   lnblnk (a routine which used to live in libg2c) is not currently
CH5   available with g95 or gfortran.
CH5
CH5 - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
CH5
CH5 - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
CH5
CH5 - Fix non-integer loop variables.
CH5
CH5 Revision 1.3  2002/12/26 17:16:32  irby
CH5 Fix variable declarations for f90 compatibility, e.g.:
CH5       INTEGER         EVTTJD*2
CH5 is properly declared as:
CH5       INTEGER*2       EVTTJD
CH5
CH5 Revision 1.2  2002/04/18 19:37:56  irby
CH5 Changed ../INTMAP_COMMON include filename suffixes to just .cmn instead
CH5 of .cmn.f (the makefile generator [mistakenly] creates a makefile for them
CH5 if they're .f).  Makefile generated using mkmk version 1.81.
CH5
CH5 Revision 1.1  2002/04/16 20:24:03  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.2  1992/04/01  21:54:48  albert
c Used variable dimension arrays for the bin data obtained as subroutine
c parameters. Removed the special processing for galactic disk map since
c not needed with variable arrays. Read the old exposure map file if requested.
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine IMREADAT(convert,cntbin,expbin)

Cae   real      exgbin(594,66,10)
Cae   integer   cngbin(594,66,10)
      real      expbin(naxis1,naxis2,naxis3)
      integer   cntbin(naxis1,naxis2,naxis3),pscale,i,j,k,n,g,p
      integer   length,wrdlen,index,idat,edat,convert
      integer*2 idat2,edat2
      character buff(2880),ebuf(2880)

      integer   status, fread

      include '../INTMAP_COMMON/global.cmn'
      include '../INTMAP_COMMON/fitsdt.cmn'

      save

      equivalence (buffer(1),buff(1)),(buffer(2),ebuf(1))
Cae   equivalence (cntbin(1,1,1),cngbin(1,1,1))
Cae   equivalence (expbin(1,1,1),exgbin(1,1,1))

      character(80)	id
      common	/id/id

      id = '$Id: readat.f,v 1.5 2013/05/21 19:08:24 irby Exp $'


C---> Initialize variables
      index = 1
      wrdlen = bitpix / 8

C---> Read the first FITS data record
c     call FREAD(buff,1,length,*2000,*3000)

c        Changes for Linux
      status = fread(buff,1,length)
      if (status.eq.1 ) go to 2000
      if (status.eq.2 ) go to 3000
      if (convert .eq. 1) 
     *   call reflect(buff,wrdlen,length)

c     if (usexpf) call FREAD(ebuf,3,length,*2000,*3000)
      if (usexpf) then
         status = fread(ebuf,3,length)
         if( status.eq.1 ) go to 2000
         if( status.eq.2 ) go to 3000
         if (convert .eq. 1) 
     *      call reflect(ebuf,wrdlen,length)
      endif

C---> Read and store the rectangular maps FITS files
      if (gridtp.eq.'RECT') then

C------> Get the bin data for all energies, all y bins, all x bins
Cesp	 ! F77 1.2 does not support ! comments !
Cesp     do k=1,naxis3                      ! Energy levels
Cesp        do j=1,naxis2                   ! Y axis
Cesp           do i=1,naxis1                ! X axis
Cesp	 ! Energy levels
         do k=1,naxis3
Cesp	    ! Y axis
            do j=1,naxis2
Cesp	       ! X axis
               do i=1,naxis1

C---------------> Get the next data item from the buffer into idat
                  if (wrdlen.eq.4) then
                     call KMVC(idat,1,buff(index),1,wrdlen)
                     if (usexpf) call KMVC(edat,1,ebuf(index),1,wrdlen)
                  else if (wrdlen.eq.2) then
                     call KMVC(idat2,1,buff(index),1,wrdlen)
                     idat = idat2
		     if (usexpf) then
                        call KMVC(edat2,1,ebuf(index),1,wrdlen)
                        edat = edat2
		     endif
                  endif

C---------------> Store the bin data (and initialize exposure bins)
Cesp	 	  ! F77 1.2 does not support ! comments !
Cesp              if (naxis1.le.200) then                      !rectangu
Cesp                 cntbin(i,j,k) = idat*bscale(1) + bzero(1) !  maps
Cesp                 expbin(i,j,k) = 0.0
Cesp              else                                         !galactic
Cesp                 cngbin(i,j,k) = idat*bscale(1) + bzero(1) !disk map
Cesp                 exgbin(i,j,k) = 0.0
Cesp              endif
Cae
Cesp 		  rectangular maps !
Cae               if (naxis1.le.200) then
Cae                   cntbin(i,j,k) = idat*bscale(1) + bzero(1)
Cae                   expbin(i,j,k) = 0.0
Cesp 		  galactic disk map !
Cae               else
Cae                   cngbin(i,j,k) = idat*bscale(1) + bzero(1)
Cae                   exgbin(i,j,k) = 0.0
Cae                endif

                  cntbin(i,j,k) = idat*bscale(1) + bzero(1)
                  if (usexpf) then
                     expbin(i,j,k) = edat*bscale(2) + bzero(2)
                  else
                     expbin(i,j,k) = 0.0
                  endif

C---------------> Increment the buffer index & get next buffer if needed
                  index = index + wrdlen
                  if (index.gt.2880) then
c     call FREAD(buff,1,length,*100,*3000)

c     Changes for Linux
                     status = fread(buff,1,length)
                     if( status.eq.1 ) go to 100
                     if( status.eq.2 ) go to 3000
                     if (convert .eq. 1) 
     *                  call reflect(buff,wrdlen,length)

c     if (usexpf) call FREAD(ebuf,3,length,*100,*3000)
                     if (usexpf) then
                        status = fread(ebuf,3,length)
                        if( status.eq.1 ) go to 100
                        if( status.eq.2 ) go to 3000
                        if (convert .eq. 1) 
     *                     call reflect(ebuf,wrdlen,length)
                     endif

                     index = 1
                  endif

               end do
            end do
         end do

C---> Read and store the polar FITS file
      else if (gridtp.eq.'POLA') then

C------> Loop over all the energy levels
         do k=1,naxis3
            i = 0
            j = 0

C---------> Read all the groups
            do g=1,gcount

C------------> Read the parameters for group g
               do p=1,pcount
                  call KMVC(idat2,1,buff(index),1,wrdlen)
                  ftparm(p,g) = idat2
                  if (p.gt.1) ftparm(p,g) = ftparm(p,g) / 10.0
                  index = index + wrdlen
                  if (index.gt.2880) then
c     call FREAD(buff,1,length,*2000,*3000)

c     Changes for Linux
                     status = fread(buff,1,length)
                     if( status.eq.1 ) go to 2000
                     if( status.eq.2 ) go to 3000
                     if (convert .eq. 1) 
     *                  call reflect(buff,wrdlen,length)

c     if (usexpf) call FREAD(ebuf,3,length,*2000,*3000)
                     if (usexpf) then
                        status = fread(ebuf,3,length)
                        if( status.eq.1 ) go to 2000
                        if( status.eq.2 ) go to 3000
                        if (convert .eq. 1) 
     *                     call reflect(ebuf,wrdlen,length)
                     endif

                     index = 1
                  endif
               end do
               ftparm(4,g) = cdelt1
               if (g.lt.3) ftparm(4,g) = cdelt1/2.0

C------------> Read the n (n=1st parameter) bin data values of the group
Cesp	       ! F77 1.2 does not support ! comments !
Cesp           do n=1,ftparm(1,g)           ! number of bins in group
Cesp	       ! number of bins in group
               do n=1,int(ftparm(1,g))
                  call KMVC(idat2,1,buff(index),1,wrdlen)
		  if (usexpf) call KMVC(edat2,1,ebuf(index),1,wrdlen)
                  j = j + 1

C---------------> Start a new polar bin when the circle is complete
                  if (i.eq.0 .or. j.gt.naxs12(i)) then
                     j = 1
                     i = i + 1
                     naxs12(i) = 360.0 / ftparm(5,g)
                  endif

C---------------> Store the bin data (and initialize the exposure bins)
                  cntbin(i,j,k) = idat2*bscale(1) + bzero(1)
		  if (usexpf) then
	             expbin(i,j,k) = edat2*bscale(2) + bzero(2)
		  else
	             expbin(i,j,k) = 0.0
		  endif

C---------------> Increment the buffer index & get next buffer if needed
                  index = index + wrdlen
                  if (index.gt.2880) then
c     call FREAD(buff,1,length,*200,*3000)

c     Changes for Linux
                     status = fread(buff,1,length)
                     if( status.eq.1 ) go to 200
                     if( status.eq.2 ) go to 3000
                     if (convert .eq. 1) 
     *                  call reflect(buff,wrdlen,length)

c     if (usexpf) call FREAD(ebuf,3,length,*200,*3000)
                     if (usexpf) then
                        status = fread(ebuf,3,length)
                        if( status.eq.1 ) go to 200
                        if( status.eq.2 ) go to 3000
                        if (convert .eq. 1) 
     *                     call reflect(ebuf,wrdlen,length)
                     endif

                     index = 1
                  endif
               end do
            end do
         end do

C---> Read and store the Aitoff FITS file
      else if (gridtp.eq.'AITF') then
	 pscale = 100.0

C------> Loop over all the energy levels and over all the groups
Cesp	 ! F77 1.2 does not support ! comments !
Cesp     do k=1,naxis3                      ! energy levels
Cesp        do j=1,gcount                   ! number of groups (latitude)
Cesp	 ! energy levels
         do k=1,naxis3
Cesp	    ! number of groups (latitude)
            do j=1,gcount

C------------> Get all the parameters in the group
Cesp	       ! F77 1.2 does not support ! comments !
Cesp           do p=1,pcount                ! number of parameters
Cesp	       ! number of parameters
               do p=1,pcount
                  call KMVC(idat2,1,buff(index),1,wrdlen)
                  ftparm(p,j) = idat2 / 100.0
		  if (p .eq. 1) then
		     if (j.eq.1.and.ftparm(p,j).lt.1) pscale = 1.0
                     ftparm(p,j) = idat2 / pscale
		  end if
                  index = index + wrdlen
                  if (index.gt.2880) then
c     call FREAD(buff,1,length,*2000,*3000)

c     Changes for Linux
                     status = fread(buff,1,length)
                     if( status.eq.1 ) go to 2000
                     if( status.eq.2 ) go to 3000
                     if (convert .eq. 1) 
     *                  call reflect(buff,wrdlen,length)

c     if (usexpf) call FREAD(ebuf,3,length,*2000,*3000)
                     if (usexpf) then
                        status = fread(ebuf,3,length)
                        if( status.eq.1 ) go to 2000
                        if( status.eq.2 ) go to 3000
                        if (convert .eq. 1) 
     *                     call reflect(ebuf,wrdlen,length)
                     endif

                     index = 1
                  endif
               end do
               naxs12(j) = ftparm(1,j)

C------------> Loop over all the longitude bins for the current j
Cesp	       ! F77 1.2 does not support ! comments !
Cesp           do i=1,naxs12(j)             ! # of bins in group (longitude)
Cesp	       ! # of bins in group (longitude)
               do i=1,naxs12(j)
                  
C---------------> Store the bin data (and initialize the exposure bins)
                  call KMVC(idat2,1,buff(index),1,wrdlen)
		  if (usexpf) call KMVC(edat2,1,ebuf(index),1,wrdlen)
                  cntbin(i,j,k) = idat2*bscale(1) + bzero(1)
		  if (usexpf) then
	             expbin(i,j,k) = edat2*bscale(2) + bzero(2)
		  else
	             expbin(i,j,k) = 0.0
		  endif

C---------------> Increment the buffer index & get next buffer if needed
                  index = index + wrdlen
                  if (index.gt.2880) then
c     call FREAD(buff,1,length,*300,*3000)

c     Changes for Linux
                     status = fread(buff,1,length)
                     if( status.eq.1 ) go to 300
                     if( status.eq.2 ) go to 3000
                     if (convert .eq. 1) 
     *                  call reflect(buff,wrdlen,length)

c     if (usexpf) call FREAD(ebuf,3,length,*300,*3000)
                     if (usexpf) then
                        status = fread(ebuf,3,length)
                        if( status.eq.1 ) go to 300
                        if( status.eq.2 ) go to 3000
                        if (convert .eq. 1) 
     *                     call reflect(ebuf,wrdlen,length)
                     endif

                     index = 1
                  endif

               end do
            end do
         end do
      endif
      
      return

C---> End of file reached
 100  if (i.lt.naxis1.or.j.lt.naxis2) goto 2000
      return
 200  if (n.lt.ftparm(1,g) .or. g.lt.gcount) goto 2000
      return
 300  if (i.lt.naxs12(j) .or. j.lt.gcount) goto 2000
      return
 2000 write(6,*) 'IMREADAT: Premature end of the FITS file'
      retcod = 8
      return

C---> Read I/O error
 3000 write(6,*) 'IMREADAT: I/O error in reading the FITS file'
      retcod = 12
      return

CCCCCCCCCCCCCCCCCCCC END SKYMAP.SOURCE(IMREADAT) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
