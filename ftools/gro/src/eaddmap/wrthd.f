CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC wrthd.f CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  wrthd
CH1
CH1
CH1  Programmer(s) and Completion Date:
CH1     Jeff Silvis - Hughes-STX - 11 Aug 1997
CH1  Modified:
CH1     Ning Gan    - RSTX  1 July 1998
CH1        - Used the ftdt2s to construct the date string. 
CH1        - Updated for the new format of DATE keywords.
CH1
CH1   Function: Writes the header for an EGRET FITS FILE. The old egret routine
CH1   wrtfts.f has been split into two routines.  The main one is 
CH1   nwrtfts.f which writes the data and calls this routine.  The values
CH1   for most of the header keywords are taken from global variables.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH2  Calling Sequence: 
CH2          subroutine wrthd(lun_fts,sign,fil_type,fits_outnm,status)
CH2
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     lun_fits    I*4         The logical number that the fits header 
CH2                              will be written to.  This was opened in 
CH2                              nwrtfts.f. 
CH2	sign	    I*4	    I   Sign for the map operation (+1 or -1)
CH2     fil_type    I*4         Specifies which fits files is being stored:
CH2                             i.e. 1=counts, 2=exposure, and 3=intensity.
CH2     fits_outnm  CH*80       Name of the output fits file.
CH2     status      I*4         gives the error status of the fitsio functions
CH2     
CH2
CH2  Called by:  nwrtfts
CH2
CH2  Calls:
CH2
CH2 ftgsdt: Gets the date 
CH2 ftphis: Adds a history keyword to the header
CH2 ftpkye: Puts a real exponential floating point(R*4) keyword into the header
CH2 ftpkyf: Puts real fixed-format floating point (R*4) keyword into the header
CH2 ftpkyg: Puts double precision fixed-format(R*8) keyword into the header. 
CH2 ftpkyj: Puts an integer (I*4) keyword into the header.
CH2 ftpkyl: Puts logical (L*4) keyword into the header. 
CH2 ftpkys: Puts character string keyword into the header. 
CH2 ftprec: Puts an 80 character record into the header
CH2
CH2
CH3  COMMON Use:
CH3  COMMON Block Name: oufits (Information about the output FITS file)
CH3  Variable   Type                    Definition
CH3    Name
CH3 ---------  -------   -----------------------------------------------
CH3 ocrvl1     Real*8    Position of the reference point on the first axis
CH3 ocdel1     Real*8    Increment on the first axis
CH3 ocrvl2     Real*8    Position of the reference point on the second axis
CH3 ocdel2     Real*8    Increment on the second axis
CH3 onaxs1     Integer   Number of bins on the first axis
CH3 onaxs2     Integer   Number of bins on the second axis
CH3 onaxs3     Integer   Number of bins on the third axis (energy levels)
CH3 obscal(3)  Real*4    Scale to use to convert the FITS values (1 index for
CH3			 each of the 3 output maps)
CH3 obzero(3)  Integer   Offset to use to convert the FITS values (1 index for
CH3			 each of the 3 output maps)
CH3 oindex(2)  Integer   Index for the oheader buffer (2 input files)
CH3 ogridt     Ch*4      Grid type ('RECT', or 'AITF')
CH3 oenrgy     Real      Energy level ranges
CH3  (2,10)
CH3 ocrpx1     Real      Array index of reference point on axis 1
CH3 ocrpx2     Real      Array index of reference point on axis 2
CH3 oznmax(10) Real      Maximum zenith angle for each energy level
CH3 omapsz(3)  Integer   Map size (number of integers in each of the 3 maps)
CH3 omaxvl(3)  Real      Maximum counts bin value for the 3 output maps
CH3 ocoord     Ch*4      Coordinate system used ('GALA' or 'CELE')
CH3 oheadr     Ch*80     Buffer to save the header information of the 2 input
CH3  (2,300)		 maps
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3
CH3  coment     Ch*47      -      Comment for FITS header line
CH3
CH4
CH4  Method
CH4 
CH4  The type of file (counts, exposure or intensity) is passed
CH4  to the routine from nwrtfts.  This routine then writes the various 
CH4  global variables into the header using the fitsio routines.
CH4
CCCCCCCCCCCCCCCCCCCCCCCCC WRTHD CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
      subroutine wrthd(lun_fts,sign,fil_type,fits_outnm,status)
      implicit none
      integer     	sign,i,j,lun_fts,num,cdate(3),fil_type
      character   	fits_outnm*80,coment*47, card*77
      character   	ctype1*8,ctype2*8,string*24,strng2*8,frmt*4
      character         id*80
      character         date_string*68
      double precision  zenm
      include 'infits.cmn.inc'
      include 'oufits.cmn.inc'

      common /id/id
      data num/1/
C
C      New variable added for fitsio
C

      integer status,naxes(3),naxis,bitpix
      logical simple
C
C
C  All fitsio routines use "inherited status" convention.  This means that 
C  if an error has already occurred (i.e. status has a value other than 0)
C  then a subroutine will exit immediately without changing the value of
C  status.   This convention will be maintained for the EGRET I/O.  (See 
C  section 4.6 of the FITSIO User's Guide Version 5.0 by William Pence)
C
       if (status .gt. 0 )  then 
           return
       endif
C
C    J. Silvis HSTX 
C     6 Aug 1997  
C
C      The values below will be sent to the header.
C
C      Note about bitpix.  This determines the data type that
C      will stored inside the fits file.  A value of 16 for bitpix
C      means that integers will be stored in the FITS file.  This is not
C      necessarily the same data type input.  The input data are real and
C      this is reflected by the version of the data input function.
C      In this case ftp3de, the "e" menas that real data is being input
C      to the FITS file.  The real data are scaled to integers using the
C      keywords BSCALE and BZERO.
C
C
       simple=.true.
       bitpix=16
       naxis=3
       naxes(1)=onaxs1
       naxes(2)=onaxs2
       naxes(3)=onaxs3

C   Initialize variables
      id= '$id$'

C---> Set the FITS keywords for the coordinate system
      if (ocoord .eq. 'GALA') then
	 ctype1 = 'GLON    '
	 ctype2 = 'GLAT    '
      else if (ocoord .eq. 'CELE') then
	 ctype1 = 'RA      '
	 ctype2 = 'DEC     '
      else if (ocoord .eq. 'ERTH') then
	 ctype1 = 'LATD    '
	 ctype2 = 'LONG    '
      else if (ocoord .eq. 'INST') then
	 ctype1 = 'TETX    '
	 ctype2 = 'TETY    '
      else if (ocoord .eq. 'TGAL') then
	 ctype1 = 'GTLON   '
	 ctype2 = 'GTLAT   '
      else if (ocoord .eq. 'TCEL') then
	 ctype1 = 'CTRA    '
	 ctype2 = 'CTDEC   '
      else if (ocoord .eq. 'SUN') then
	 ctype1 = 'SLON    '
	 ctype2 = 'SLAT    '
      else if (ocoord .eq. 'MOON') then
	 ctype1 = 'MLON    '
	 ctype2 = 'MLAT    '
      endif
C
C     Mandatory keywords
C
      coment = 'Standard FITS format'
      call ftpkyl(lun_fts,'SIMPLE',simple,coment,status)
      coment = 'Number of bits per pixel (2-byte integers)'
      call ftpkyj(lun_fts,'BITPIX',bitpix,coment,status)
      coment = 'Number of axis in primary array'
      call ftpkyj(lun_fts,'NAXIS',naxis,coment,status)
      coment = 'Number of bins per row'
      call ftpkyj(lun_fts,'NAXIS1',naxes(1),coment,status)
      coment = 'Number of rows'
      call ftpkyj(lun_fts,'NAXIS2',naxes(2),coment,status)
      coment = 'Number of energy levels'
      call ftpkyj(lun_fts,'NAXIS3',naxes(3),coment,status)

C
C     If the variable to enter is double precesion we use 
C     ftpkyg.  If it is real we use ftpkyf and ftpkys for
C     strings.
C
      coment = 'Min coordinate of reference bin (RA, GLON)'
      call ftpkyg(lun_fts,'CRVAL1',ocrvl1,2,coment,status)
C
      coment = 'Location of reference point in array'
      call ftpkyf(lun_fts,'CRPIX1',ocrpx1,2,coment,status)
      coment = 'Interval in degrees'
      call ftpkyg(lun_fts,'CDELT1',ocdel1,2,coment,status)
      coment = 'Coordinate type'
      call ftpkys(lun_fts,'CTYPE1',ctype1,coment,status)
C
      coment = 'Min coordinate of reference bin (RA, GLON)'
      call ftpkyg(lun_fts,'CRVAL2',ocrvl2,2,coment,status)
      coment = 'Location of reference point in array'
      call ftpkyf(lun_fts,'CRPIX2',ocrpx2,2,coment,status)
      coment = 'Interval in degrees'
      call ftpkyg(lun_fts,'CDELT2',ocdel2,2,coment,status)
      coment = 'Coordinate type'
      call ftpkys(lun_fts,'CTYPE2',ctype2,coment,status)

C
C    The keywords BSCALE and BZERO are used to scale the real
C    or double precision data to integers.  There are three 
C    possible fits files to be made, a counts file, a exposure
C    file and an intensity file.  The values for bzero and 
C    bscale are kept in three element vectors that are declared
C    as common variables.  The vector obzero stores the BZERO
C    values and obscal stores the BSCALE values.  The integer
C    input fil_type specifies which fits files is being stored,
C    i.e. 1=counts, 2=exposure, and 3=intensity.
C
C    J. Silvis 18 July 1997
C
C
C    **********************************************************
C
C    Fitsio will sometimes print the keyword O_BSCALE e.g.
C
C   O_BSCALE=          0.000152593 / Original BSCALE Value
C
C   BSCALE itself is created in the routine intens.f.  The relevant lines
C   from intens.f are:
C
C   200:      data   type /'COUNTS', 'EXPOSURE','INTENSITY'/, maxi2/32767/
C   219:                if (ocntbn(i,j,k).gt.omaxvl(1)) then
C   220:                   omaxvl(1) = ocntbn(i,j,k)
C   272:      maxint = maxi2
C   277:          obscal(i) = omaxvl(i) / maxint
C
C   If omaxvl is small ( e.g. 5) fitsio and the original EGRET fits
C   writer decide to set the scaling to 1.  If fitsio changes BSCALE to
C   1 the keyword O_BSCALE is added to the header by FTCLOS.F.  I have decided
C   to leave this keyword in the header even though it is not in the original
C   headers generated by the original EGRET fits writer.  It supplies 
C   information that is correct and may be useful at some point.
C
C   J. Silvis      28 July 1997
C
      coment = 'Units of primary array'
      if (fil_type .eq. 1)  then
            call ftpkys(lun_fts,'BUNIT','COUNTS',coment,status)
      else if (fil_type .eq. 2) then
            call ftpkys(lun_fts,'BUNIT','EXPOSURE',coment,status)
      else if (fil_type .eq. 3) then
            call ftpkys(lun_fts,'BUNIT','INTENSITY',coment,status)       
      endif
C

      coment = 'Data scaling '
      if (fil_type .eq. 1)  then
           coment = 'Data scaling (none)'
      endif
      call ftpkye(lun_fts,'BSCALE',obscal(fil_type),8,coment,status)
      coment = 'Data offset '
      if (fil_type .eq. 1)  then
           coment = 'Data offset (none)'
      endif
       call ftpkye(lun_fts,'BZERO',obzero(fil_type),7,coment,status)

      coment = 'The reference point is at the pixel center'
      call ftpkyl(lun_fts,'PIXCENT',.true.,coment,status)
C     
C     Now write the keywords that will be taken directly from
C     the input fits file.
C
      card='** Following information saved from the 1st input file **'
      call ftphis(lun_fts,card,status)
      do j=1,oindex(1)
  	 if ((oheadr(1,j)(1:5).ne.'FILEA')
     &        .and.(oheadr(1,j)(1:5).ne.'FILEB')) then
          call ftprec(lun_fts,oheadr(1,j),status)
  	 end if
      end do

      card='*** End of information saved from the 1st input file ***'
      call ftphis(lun_fts,card,status)
C---> Write the energy levels
      j = 0
      coment = 'Minimum energy of map in level xx (Mev)'
      do i=1,onaxs3
         j = j + 1
         write(strng2,'(a6,i1,a1)') 'MINENG',j,' '
         coment(2:3) = 'in'
         if (i.eq.10) write(strng2,'(a6,i2)') 'MINENG',j
         write(coment(32:33),'(i2)') j
         call ftpkyj(lun_fts,strng2,int(oenrgy(1,i)),coment,status)
         strng2(2:3) = 'AX'
         coment(2:3) = 'ax'
         call ftpkyj(lun_fts,strng2,int(oenrgy(2,i)),coment,status)
      end do
C
      coment = 'Maximum angle from zenith in level xx'
      do i=1,onaxs3
	 zenm = oznmax(i)
         write(strng2,'(a6,i1,a1)') 'ZENMAX',i,' '
         if (i.eq.10) write(strng2,'(a6,i2)') 'ZENMAX',i
         write(coment(36:37),'(i2)') i
         call ftpkyg(lun_fts,strng2,zenm,3,coment,status)
      end do

C
c---> write additional header information
C      call idate(cdate)
C     
C      Use fitsio to find the date.
C
       call ftgsdt(cdate(1),cdate(2),cdate(3),status)
C       write(string,'(i2.2,a1,i2.2,a1,i2.2)') cdate(1),'/',cdate(2),
C     &   '/',mod(cdate(3),100)
       call ftdt2s(cdate(3), cdate(2), cdate(1), date_string, status)
       coment = 'Current date in yyyy-mm-dd'
       call ftpkys(lun_fts,'DATE',date_string,coment,status) 
C
C
      coment = 'Map type of the primary array data'
      if (ogridt.eq.'RECT') then
         call ftpkys(lun_fts,'PRIMTYPE','RECT_MAP',coment,status)
      else if (ogridt.eq.'AITF') then
         call ftpkys(lun_fts,'PRIMTYPE','AITOFF_MAP',coment,status)
      end if
C
       coment = 'Data type in the file'
       
       if (fil_type .eq. 1) then 
         call ftpkys(lun_fts,'FILETYPE','EGRET_COUNTS_MAP',
     &       coment,status)
       endif 
       if (fil_type .eq. 2) then 
         call ftpkys(lun_fts,'FILETYPE','EGRET_EXPOSR_MAP',
     &        coment,status)
       endif 
       if (fil_type .eq. 3) then 
         call ftpkys(lun_fts,'FILETYPE','EGRET_INTENS_MAP',
     &        coment,status)
       endif 
C
C     Record whether addition or subtraction was used on the maps
C
      string = 'SUM_OF_MAPS'
      coment = 'Addition of 2 maps'
      if (sign .eq. -1) then
         coment = 'Subtraction of 2 maps'
	  string(3:3) = 'B'
      end if
      call ftpkys(lun_fts,'MAPTYPE',string,coment,status)
C

      coment = 'Number of elements in the map'
      call ftpkyj(lun_fts,'MAPSIZE',omapsz,coment,status)
      coment = 'File produced at Goddard Space Flight Center'
      call ftpkys(lun_fts,'ORIGIN','GSFC',coment,status)
      coment = 'Compton Observatory (Gamma Ray Observatory)'
      call ftpkys(lun_fts,'TELESCOP','GRO',coment,status)
      coment = 'The GRO instrument is EGRET'
      call ftpkys(lun_fts,'INSTRUME','EGRET',coment,status)
      coment = 'Original EGRET FITS file name'
      call ftpkys(lun_fts,'FILENAME',fits_outnm,coment,status)
      coment = 'PI is Carl Fichtel'
      call ftpkys(lun_fts,'OBSERVER','FICHTEL',coment,status)
      coment = 'Name of object or nominal pointing id'
      call ftpkys(lun_fts,'OBJECT1','        ',coment,status)
      coment = 'Name used for file cataloging & archiveing'
      call ftpkys(lun_fts,'FILE-ID','        ',coment,status)
      coment = 'Version of file used for cataloging'
      call ftpkys(lun_fts,'FILE-VER','        ',coment,status)
C
C---> Write the names of the files added to this map
      runnum = runnum + 1
      if (runnum .lt. 10) frmt = '(i1)'
      if (runnum .ge. 10) frmt = '(i2)'
      if (runnum .ge. 100) frmt = '(i3)'
      do i=1,2
         do j=1,oindex(i)
	    if ((oheadr(i,j)(1:5).eq.'FILEA').or.
     &                 (oheadr(i,j)(1:5).eq.'FILEB')) then
              if (oheadr(i,j)(6:8) .eq. '000') then
	          write(oheadr(i,j)(6:8), frmt) runnum
	          write(oheadr(i,j)(75:77), frmt) runnum
	       end if
               strng2 = oheadr(i,j)(1:8)
               string = oheadr(i,j)(9:32)
               coment = oheadr(i,j)(33:79)
               card = strng2 // string // coment
               call ftprec(lun_fts,card,status)
	    end if
         end do
      end do
      return

      end





