C*==fregcn.spg  processed by SPAG 4.50J  at 16:02 on  2 May 1997
C******************************************************************************
C SELECTOR TASK:
C      fregcon
C
C FILE:
C      fregcon.f
C
C DESCRIPTION:
C      SAOimage region file converter from one instrument/detector to another.
C
C AUTHOR/DATE:
C      Srilal Weera  and Banshree Mitra 2/2/96
C
C MODIFICATION HISTORY:
C
C kaa Sep 1999  v1.1  Added support for SAOtng regions with positions in RA
C                     and Dec (either HMS or decimal degrees).
C                     Added a new parameter "factor" which if !=1 means that
C                     the regions are scaled by this parameter. If factor!=1
C                     then infil1 and infil2 are not prompted for.
C
C
C NOTES:
C
C
C USAGE:
C      HOST: call fregcn
C      IRAF: task fregcn
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      factor  - rebinning factor for region file. If this is not 1 then
C                require infil1 and infil2
C      infil1  - input FITS file and extension number for the
C		 first observation
C      infil2  - input FITS file and extension number for the
C		 second observation
C      regfile  - Region file
C      outfile  - Output file
C
C
C CALLED ROUTINES:
C      subroutine gregcon - gets parameters from parameter file
C      subroutine ffregcon- Convert region from one observation to the other
C
C******************************************************************************
 
      SUBROUTINE FREGCN
      IMPLICIT NONE
      REAL factor
      character(160) infil1 , infil2 , regfile , outfile
      character(40) TASkname
      COMMON /TASK  / TASkname
 
      TASkname = 'fregcon v1.1'
      CALL FTCMSG
 
C  get parameters from parameter file
      CALL GREGCON(factor,infil1,infil2,regfile,outfile)
 
C  Convert region from observation 1 to observation 2.
      CALL FFREGCON(factor,infil1,infil2,regfile,outfile)
 
      RETURN
      END
**==gregcon.spg  processed by SPAG 4.50J  at 16:02 on  2 May 1997
 
 
C*****************************************************************************
C SUBROUTINE:
C      gregcon
C
C DESCRIPTION:
C      Get parameters from parameter file
C
C
C NOTES:
C       gregcon uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C     call gregcon(factor,infil1,infil2,regfile,outfile)
C
C ARGUMENTS:
C      factor  - rebinning factor for region file. If this is not 1 then
C                require infil1 and infil2
C      infil1  - input FITS file and extension number for the
C		 first observation
C      infil2  - input FITS file and extension number for the
C		 second observation
C      regfile  - Region file
C      outfile  - Output file
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*****************************************************************************
 
      SUBROUTINE GREGCON(Factor,Infil1,Infil2,Regfile,Outfile)
      IMPLICIT NONE
      CHARACTER*(*) Infil1 , Infil2 , Regfile , Outfile
      REAL Factor
      character(80) context
      INTEGER status
 
C  initialize variables
      status = 0

C  get any rebinning factor. If this is not 1 then we just adjust the
C  region file for rebinning otherwise we prompt for image files to
C  specify the transformation
      CALL UCLGSR('factor',Factor,status)
      context = 'could not get FACTOR parameter'
      IF ( status.NE.0 ) GOTO 100

      IF ( ABS(Factor-1.) .LE. 1.e-5 ) THEN 

C  get the name of the first FITS file
         CALL UCLGST('infil1',Infil1,status)
         context = 'could not get INFIL1 parameter'
         IF ( status.NE.0 ) GOTO 100
 
C  get the name of the second FITS file
         CALL UCLGST('infil2',Infil2,status)
         context = 'could not get INFIL2 parameter'
         IF ( status.NE.0 ) GOTO 100

      ENDIF
 
C  get the name of the region file
      CALL UCLGST('regfile',Regfile,status)
      context = 'could not get REGFILE parameter'
      IF ( status.NE.0 ) GOTO 100
 
C  get the name of the output file
      CALL UCLGST('outfile',Outfile,status)
      context = 'could not get OUTFILE parameter'
      IF ( status.NE.0 ) GOTO 100

 100  CONTINUE
      IF ( status.NE.0 ) THEN
         CALL FCERR(context)
         CALL FCERRM(status)
         STOP
      ENDIF
 
      RETURN
      END
**==ffregcon.spg  processed by SPAG 4.50J  at 16:02 on  2 May 1997
 
 
C******************************************************************************
C SUBROUTINE:
C      ffregcon
C
C DESCRIPTION:
C      subroutine ffregcon- Region converter from one observation to the other
C
C AUTHOR/DATE:
C      Srilal Weera  and Banashree Seifert  2/2/96
C
C MODIFICATION HISTORY:
C      Banashree M Seifert (April, 1997)
C          . Changes are made accordinf as suggested by KAA for  OSF and Linux
C----------------------------------------------------------------------------
Cfregcon fails under both OSF and Linux(at least).The fixes to fregcon.f are :
C546c546
C<           fmtstr='(a1,'//cm1//',a1,f8.2,'//cm2(2:)//'(a1,f8.2),a1)'
C---
C>           fmtstr='(a1,a'//cm1//',a1,f8.2,'//cm2(2:)//'(a1,f8.2),a1)'
C608c608
C<           fmtstr='(a1,'//cm1//',a1,f8.2,a,f8.2,'
C---
C>           fmtstr='(a1,a'//cm1//',a1,f8.2,a,f8.2,'
C621,622c621,623
C<       close(iunitr)
C<       close(ounit)
C---
C>       call ftclos (iunitr, status)
C>       call ftclos (ounit , status)
C>         if (status .ne. 0) call fcerrm(status)
C----------------------------------------------------------------------------
C
C
C NOTES:
C
C
C USAGE:
C      call ffregcon(factor,infil1,infil2,regfile,outfile)
C
C ARGUMENTS:
C      factor  - rebinning factor for region. If this is not 1 then
C                use infil1 and infil2 to remap regions
C      infil1  - input FITS file and extension number for the
C                first observation
C      infil2  - input FITS file and extension number for the
C                second observation
C      regfile  - Region file
C      outfile  - Output file
C
C PRIMARY LOCAL VARIABLES:
C      context - error message
C      status  - error number
C      fname   - input fits file name
C      errstr  - concatenated error message
C      comment - comment string found in FITS file
C      history - history string
C      maxcl   - maximum number of columns supported by software
C      simple  - FITS primary header flag
C      extend  - FITS extension header flag
C      exact   - FITS keyword match flag
C      inopen  - input file open flag
C      bool    - expression evaluates to boolean flag
C      extnum  - extension number in input FITS file
C      status  - fitsio library call return status
C      iunit   - input file unit number
C      record  - string containing contents of one line from eunit file
C      block   - fits file block size
C      htype   - fits file header type
C      bitpix  - number of bits per pixel in primary array
C      naxis   - number of dimensions of in array
C      naxes   - number of points along each dimension
C      pcount  - value of pcount keyword
C      gcount  - value of gcount keyword
C      rowlen  - length of FITS table in bytes
C      nrows   - number of records in FITS table
C      tfields - total number of columns in FITS table
C      varidat - size in bytes of variable data area
C      clnum   - column number
C      ttype   - array of column names
C      tform   - array of column formats
C      tunit   - array of column units
C      tbcol   - column number of first char in each field
C      extname - extension name
C
C CALLED ROUTINES:
C      subroutine fcpars - parse filename and extension from infile
C      subroutine ft____ - FITSIO routines
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C******************************************************************************
 
      SUBROUTINE FFREGCON(Factor,Infil1,Infil2,Regfile,Outfile)
      IMPLICIT NONE
      REAL Factor
      CHARACTER*(*) Infil1 , Infil2 , Regfile , Outfile
      character(80) context , errstr
      character(160) fname
      character(8) cm1 , cm2
      character(80) fmtstr
      INTEGER extnum , status , iunit , iunitr , ounit , block , htype
      DOUBLE PRECISION cdlt11 , cdlt21 , crpx11 , crpx21 , crvl11 , 
     &                 crvl21 , crot21
      DOUBLE PRECISION cdlt12 , cdlt22 , crpx12 , crpx22 , crvl12 , 
     &                 crvl22 , crot22
      DOUBLE PRECISION xsky , ysky , xpix , ypix
      character(16) ctyp11 , ctyp12
      LOGICAL inopen , exact
c ------------------ variables for calling read_region -----------
      INTEGER MAXPOINTS
      PARAMETER (MAXPOINTS=40)
      character(70) comm_line(10)
      character(10) shape(MAXPOINTS)
      character(1) sign(MAXPOINTS)
      DOUBLE PRECISION points(MAXPOINTS,MAXPOINTS)
      DOUBLE PRECISION newpoints(MAXPOINTS,MAXPOINTS)
      DOUBLE PRECISION scl_factor
      INTEGER line_no , shape_no , npoints(MAXPOINTS) , 
     &        char_no(MAXPOINTS), type
      INTEGER errflg , m1 , m2 , i , j , jj
      LOGICAL qfiles
 
c ----------------------------------------------------------------
C   initialize variables
      status = 0
      iunit = 15
      iunitr = 16
      ounit = 17
      block = 0
      exact = .TRUE.
      inopen = .FALSE.
      context = ' '
      errstr = ' '

C   Set the logical which determines whether we are doing a simple
C   rebinning of the region file or remapping based on image files.

      IF ( ABS(Factor-1.) .LE. 1.e-5 ) THEN
         qfiles = .TRUE.
      ELSE
         qfiles = .FALSE.
      ENDIF

C   If the files are required then read in the information

      IF ( qfiles ) THEN
 
C   Obtain keywords from the first FITS file
 
C   get the filename and extension
         CALL FCPARS(Infil1,fname,extnum,status)
 
         IF ( extnum.EQ.-99 ) extnum = 1
 
C   if the extension is less than 1 then give an error and exit
         context = 'Primary extension not supported'
         IF ( extnum.LT.1 ) THEN
            Status = 1
            GOTO 500
         ENDIF

C   open the input FITS file
         CALL FTOPEN(iunit,fname,0,block,status)
         context = 'Unable to open infile'
         IF ( status.NE.0 ) GOTO 500
         inopen = .TRUE.
 
C   move to the header in the input file
         CALL FTMAHD(iunit,extnum,htype,status)
         context = 'Error moving to extension number '
         IF ( status.NE.0 ) GOTO 500
 
 
C      Get keywords from the first FITS file
C      NOTE: the second subscript refers to the input filename
 
         CALL FTGKYD(iunit,'CROTA2',crot21,context,status)
         context = 'unable to get CROTA2 value from the first FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CRPIX1',crpx11,context,status)
         context = 'unable to get CRPIX1 value from the first FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CRPIX2',crpx21,context,status)
         context = 'unable to get CRPIX2 value from the first FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CDELT1',cdlt11,context,status)
         context = 'unable to get CDELT1 value from the first FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CDELT2',cdlt21,context,status)
         context = 'unable to get CDELT2 value from the first FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CRVAL1',crvl11,context,status)
         context = 'unable to get CRVAL1 value from the first FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CRVAL2',crvl21,context,status)
         context = 'unable to get CRVAL2 value from the first FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYS(iunit,'CTYPE1',ctyp11,context,status)
         context = 'unable to get CTYPE1 value from the first FITS file'
         IF ( status.NE.0 ) GOTO 500
 
C	(assume CTYPE1 and CTYPE2 use same type of projection)
         ctyp11 = ctyp11(5:9)
 
 
         IF ( inopen ) CALL FTCLOS(iunit,status)
         inopen = .FALSE.
 
C   Obtain keywords from the second FITS file
 
C   get the filename and extension
         CALL FCPARS(Infil2,fname,extnum,status)
  
         IF ( extnum.EQ.-99 ) extnum = 1
 
C   if the extension is less than 1 then give an error and exit
         context = 'Primary extension not supported'
         IF ( extnum.LT.1 ) THEN
            Status = 2
            GOTO 500
         ENDIF
 
C   open the input FITS file
         CALL FTOPEN(iunit,fname,0,block,status)
         context = 'Unable to open infile'
         IF ( status.NE.0 ) GOTO 500
         inopen = .TRUE.
 
C   move to the header in the input file
         CALL FTMAHD(iunit,extnum,htype,status)
         context = 'Error moving to extension number '
         IF ( status.NE.0 ) GOTO 500
 
 
C      Get keywords from the second FITS file
C      NOTE: the second subscript refers to the input filename
 
         CALL FTGKYD(iunit,'CROTA2',crot22,context,status)
         context = 
     &      'unable to get CROTA2 value from the second FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CRPIX1',crpx12,context,status)
         context = 
     &      'unable to get CRPIX1 value from the second FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CRPIX2',crpx22,context,status)
         IF ( status.NE.0 ) THEN
            context = 
     &            'unable to get CRPIX2 value from the second FITS file'
            CALL FCERR(context)
            GOTO 500
         ENDIF
 
         CALL FTGKYD(iunit,'CDELT1',cdlt12,context,status)
         context = 
     &      'unable to get CDELT1 value from the second FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CDELT2',cdlt22,context,status)
         context = 
     &      'unable to get CDELT2 value from the second FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CRVAL1',crvl12,context,status)
         context = 
     &      'unable to get CRVAL1 value from the second FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYD(iunit,'CRVAL2',crvl22,context,status)
         context = 
     &      'unable to get CRVAL2 value from the second FITS file'
         IF ( status.NE.0 ) GOTO 500
 
         CALL FTGKYS(iunit,'CTYPE1',ctyp12,context,status)
         context = 
     &      'unable to get CTYPE1 value from the second FITS file'
         IF ( status.NE.0 ) GOTO 500
 
C	(assume CTYPE1 and CTYPE2 use same type of projection)
         ctyp12 = ctyp12(5:9)
 
         IF ( inopen ) CALL FTCLOS(iunit,status)
         inopen = .FALSE.
 
C	All the required keywords are now read from input FITS files.

      ENDIF

C	Next, obtain the shape of the region, its size and center
C	coordinates from the Region file.
C
C     First, open the region file and output file for read and write
 
      OPEN (UNIT=iunitr,FILE=Regfile,STATUS='old',IOSTAT=Status)
      context = '  Error opening  input regionfile '
     &          //Regfile(:MIN(len(context)-34,len(Regfile)))
      IF ( Status .NE. 0 ) GOTO 500

      OPEN (UNIT=ounit,FILE=Outfile,STATUS='unknown',IOSTAT=status)
      context = '  Error opening  output file '
     &          //Outfile(:MIN(len(context)-34,len(Outfile)))
      IF ( Status .NE. 0 ) GOTO 500
 
C   	Read the shape(s) and dimensions of the selected region(s)
C	from the region_file.
 
      CALL READ_REGION(iunitr,char_no,MAXPOINTS,line_no,comm_line,
     &                 shape_no,shape,npoints,points,sign,type,errflg)
      IF ( errflg .NE. 0 ) THEN
         CALL fcecho('Failure in READ_REGION')
         WRITE(context,'(a,i5)') 'Error flag = ', errflg
         CALL fcecho(context)
      ENDIF
 
C	Copy  comment lines from the region_file to the output file
 
      DO 100 i = 1 , line_no
         WRITE (ounit,'(a)') comm_line(i)
 100  CONTINUE
 
C	Write the keyword values as COMMENTS to the output file
 
      IF ( qfiles ) THEN

         WRITE (ounit,99002) '## 1st Region: '
         WRITE (ounit,99003) '## CRPIX1 = ' , crpx11
         WRITE (ounit,99003) '## CRPIX2 = ' , crpx21
         WRITE (ounit,99003) '## CDELT1 = ' , cdlt11
         WRITE (ounit,99003) '## CDELT2 = ' , cdlt21
         WRITE (ounit,99003) '## CRVAL1 = ' , crvl11
         WRITE (ounit,99003) '## CRVAL2 = ' , crvl21
         WRITE (ounit,99003) '## CROAT2 = ' , crot21
         WRITE (ounit,99001) '## CTYPE1 = ' , ctyp11
         WRITE (ounit,99002) '##             '
 
         WRITE (ounit,99002) '## 2nd Region: '
         WRITE (ounit,99003) '## CRPIX1 = ' , crpx12
         WRITE (ounit,99003) '## CRPIX2 = ' , crpx22
         WRITE (ounit,99003) '## CDELT1 = ' , cdlt12
         WRITE (ounit,99003) '## CDELT2 = ' , cdlt22
         WRITE (ounit,99003) '## CRVAL1 = ' , crvl12
         WRITE (ounit,99003) '## CRVAL2 = ' , crvl22
         WRITE (ounit,99003) '## CROAT2 = ' , crot22
         WRITE (ounit,99001) '## CTYPE1 = ' , ctyp12
         WRITE (ounit,99002) '##             '

      ENDIF
 
C	Next, data will be written to the output file.
 
      DO 200 i = 1 , shape_no
 
C	Check if the shape is a polygon.
C	Polygon is a special case since xpix, ypix needs to be evaluated for
C	EACH corner of the polygon.
C 	(all other shapes have a SINGLE pair of xpix, ypix values at the
C	 center of the region)
 
 
         IF ( shape(i).EQ.'POLYGON' ) THEN
 
            DO 120 j = 1 , npoints(i) , 2
 
               xpix = points(i,j)
               ypix = points(i,j+1)

               IF ( type .EQ. 3 ) THEN

 
C	if type=3 then coordinates are in pixels so need to calculate 
c       the celestial coordinates corresponding to pixel values in the 
c       region file for the first region
 
                  IF ( qfiles ) THEN

                     CALL FTWLDP(xpix,ypix,crvl11,crvl21,crpx11,crpx21,
     &                           cdlt11,cdlt21,crot21,ctyp11,xsky,ysky,
     &                           status)
                     context = 'Failure in FTWLDP call'
                     IF ( status .NE. 0 ) GOTO 500
 
C 	calculate the corresponding new pixel values in the second region
C	for the same  celestial coordinates
 
                     CALL FTXYPX(xsky,ysky,crvl12,crvl22,crpx12,crpx22,
     &                           cdlt12,cdlt22,crot22,ctyp12,xpix,ypix,
     &                           status)
                     context = 'Failure in FTXYPX call'
                     IF ( status .NE. 0 ) GOTO 500

C       if type=3 and we are binning then just multiply the positions by
C       the binning factor

                  ELSE

                     xpix = xpix * Factor
                     ypix = ypix * Factor

                  ENDIF

               ENDIF
 	
               newpoints(i,j) = xpix
               newpoints(i,j+1) = ypix
 
 
 120        CONTINUE
 
 
            m1 = char_no(i)
            m2 = npoints(i)
c MJT 05July 1996 making linux happy...
            CALL FTKEYN('a',m1,cm1,status)
            CALL FTKEYN('a',m2,cm2,status)
            context = 'Failure in FTKEYN call'
            IF ( status .NE. 0 ) GOTO 500

            IF ( type .EQ. 3 ) THEN
               fmtstr = '(a1,'//cm1//',a1,f8.2,'//cm2(2:)//
     &                  '(a1,f8.2),a1)'
            ELSE
               fmtstr = '(a1,'//cm1//',a1,f11.6,'//cm2(2:)//
     &                  '(a1,f11.6),a1)'
            ENDIF
            WRITE (ounit,fmtstr) sign(i) , shape(i) , '(' , 
     &                           newpoints(i,1) , 
     &                           (',',newpoints(i,jj),jj=2,npoints(i)) , 
     &                           ')'
 
 
         ELSE
 
C If the shape is not a polygon then calculate a single pair of
C	xpix, ypix values for the center of the region.
 
            xpix = points(i,1)
            ypix = points(i,2)

            IF ( type .EQ. 3 ) THEN
 
C	if type=3 then coordinates are in pixels so need to calculate 
c       the celestial coordinates corresponding to pixel values in the 
c       region file for the first region

               IF ( qfiles ) THEN
 
                  CALL FTWLDP(xpix,ypix,crvl11,crvl21,crpx11,crpx21,
     &                        cdlt11,cdlt21,crot21,ctyp11,xsky,ysky,
     &                        status)
                  context = 'Failure in FTWLDP call'
                  IF ( status .NE. 0 ) GOTO 500
 
C 	calculate the corresponding new pixel values in the second region
C	for the same  celestial coordinates
 
                  CALL FTXYPX(xsky,ysky,crvl12,crvl22,crpx12,crpx22,
     &                        cdlt12,cdlt22,crot22,ctyp12,xpix,ypix,
     &                        status)
                  context = 'Failure in FTXYPX call'
                  IF ( status .NE. 0 ) GOTO 500

C       if type=3 and we are binning then just multiply the positions by
C       the binning factor

               ELSE

                  xpix = xpix * Factor
                  ypix = ypix * Factor

               ENDIF

            ENDIF
 
C 	For CIRCLE,BOX, ELLIPSE and ANNULUS, we have to scale the dimensions
C       For BOX and ELLIPSE we also need to reset the rotation angle

            IF ( qfiles ) THEN
 
               scl_factor = (DABS(cdlt11)+DABS(cdlt21))
     &                      /(DABS(cdlt12)+DABS(cdlt22))

            ELSE

               scl_factor = Factor

            ENDIF
 

            points(i,3) = points(i,3)*scl_factor

            IF ( shape(i) .NE. 'CIRCLE' ) THEN
               points(i,4) = points(i,4)*scl_factor
            ENDIF

            IF ( qfiles .AND. 
     &           (shape(i) .EQ. 'BOX' .OR. shape(i) .EQ. 'ELLIPSE') 
     &                        ) THEN
               IF ( npoints(i) .EQ. 5 ) THEN
                  points(i,5) = points(i,5) - (crot21-crot22)
               ELSE
                   npoints(i) = 5
                   points(i,5) =  - (crot21-crot22)
               ENDIF
            ENDIF

 
            m1 = char_no(i)
            m2 = npoints(i) - 2
c MJT 05July 1996 making linux happy...
            CALL FTKEYN('a',m1,cm1,status)
            CALL FTKEYN('a',m2,cm2,status)
            context = 'Failure in FTKEYN call'
            IF ( status .NE. 0 ) GOTO 500

            IF ( type .EQ. 3 ) THEN
               fmtstr = '(a1,'//cm1//',a1,f8.2,a,f8.2,'//cm2(2:)
     &               //'(a,f8.2),a)'
            ELSE
               fmtstr = '(a1,'//cm1//',a1,f11.6,a,f11.6,'//cm2(2:)
     &               //'(a,f8.2),a)'
            ENDIF
 
            WRITE (ounit,fmtstr) sign(i) , shape(i) , '(' , xpix , ',' , 
     &                           ypix , (',',points(i,j),j=3,npoints(i))
     &                           , ')'

 
         ENDIF
 200  CONTINUE
 
C ---- close region file and output file

      CLOSE (iunitr)
      CLOSE (ounit)
 

 500  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL FCERR(context)
         CALL FCERRM(Status)
      ENDIF

      RETURN

99001 FORMAT (a12,a16)
99002 FORMAT (a15)
99003 FORMAT (a12,e16.10)
      END
**==read_region.spg  processed by SPAG 4.50J  at 16:02 on  2 May 1997
 
c -------------------------------------------------------------------
c                start of reading region file
c -------------------------------------------------------------------
*+READ_REGION
 
      SUBROUTINE READ_REGION(Iunit,Char_no,Maxpoints,Line_no,Comm_line,
     &                       Shape_no,Shape,Npoints,Points,Sign,Type,
     &                       Errflg)
 
c ------------------------- description ------------------------------
c READ_REGION reads the input region file, extracts the information
c and passed to the called routine.
c
c     ( follows the format of read_region routine in EXTRACTOR )
c
c ------------- variable definitions ---------------------------
      IMPLICIT NONE
      CHARACTER*(*) Comm_line(*)
      CHARACTER*(*) Shape(*)
      CHARACTER*(*) Sign(*)
      INTEGER Maxpoints , MMAXPOINTS
      PARAMETER (MMAXPOINTS=40)
      DOUBLE PRECISION Points(MMAXPOINTS,MMAXPOINTS)
      INTEGER Iunit , Line_no , Shape_no, Type
      INTEGER Npoints(*) , Char_no(*)
      INTEGER Errflg
 
c ----------------------- internal variables ---------------------
      character(200) line , temp
      character(100) subinfo
      character(20) char1 , char_points(MMAXPOINTS,MMAXPOINTS)
      INTEGER i , j , count , end , start, ipt
 
c ----------------------- variable directory ---------------------
c iunit     int  i/p  unit no. of input file
c char_no   int  o/p  no. of letters in the particular shape
c maxpoints int  o/p  maximum number of shapes and maximum number of
c                     points for a particular shape
c line_no   int  o/p  no. of comment lines
c comm_line char o/p  content of comment lines
c shape_no  int  o/p  no. of shapes in the input region file
c shape     char o/p  the shape of a particular region
c                     six shapes are supported at present:
c                     1. CIRCLE
c                     2. BOX
c                     3. POLYGON
c                     4. POINT
c                     5. ELLIPSE
c                     6. ANNULUS
c npoints   int  o/p  no. of parameters to define the shape
c points    real o/p  parameters for defining shape
c sign      char o/p  sign preceeding the shape -> " " , "!", or " "
c type      int  o/p  type of region file
c                     1. position in HMS
c                     2. position in Degrees
c                     3. position in Pixels
c errflg    int  o/p  error flag defining error status
c
c ----------------- internal variable directory --------------------
c
c line        char  content of the line read
c temp        char  assign read character to temp
c char1       char  temporary character save
c char_points char  the parameter values read in character
c i           int   count for do loop
c count       int   count of the lines read
c end         int   end point of a character in the line read
c start       int start point of a character in the line read
c
c ----------------------- called routines -----------------------
c    none
c ---------------------------------------------------------------
c
c Banashree Mitra Seifert (Feb 1996) 1:0:0
c kaa  Sep 24 1999 1.1.0     Handles output from saotng with positions
c                            in RA and Dec either as HMS or degrees.
c                            In this case positions are returned in
c                            degrees.
c
c ---------------------------------------------------------------
      character(5) VERSION
      PARAMETER (VERSION='1.1.0')
      character(12) subname
 
      subname = 'read_region'
      subinfo = 'using '//subname//VERSION
cc      CALL WTINFO(1,1,1,subinfo)
 
c ------------------------- initialisation -----------------------------
      Line_no = 0
      count = 0
      Shape_no = 0
      Type = 3
 100  count = count + 1
      READ (Iunit,'(a)',END=800) line
      CALL FTUPCH(line)

c Check what type of region file this in. Note that we will end up
c converting type 1s to 2s so change the stored string appropriately

      IF ( line(1:1).EQ.'#' ) THEN
         Line_no = Line_no + 1
         Comm_line(Line_no) = line
         IF ( index(line,'FORMAT') .NE. 0 ) THEN
            IF ( index(line,'HMS') .NE. 0 ) THEN
               Type = 1
               ipt = index(line,'HMS')
               Comm_line(Line_no) = line(1:ipt-1)
               Comm_line(Line_no)(ipt:) = 'DEGREES'
               Comm_line(Line_no)(ipt+7:) = line(ipt+3:)
            ELSEIF ( index(line,'DEGREES') .NE. 0 ) THEN
               Type = 2
            ELSEIF ( index(line,'PIXELS') .NE. 0 ) THEN
               Type = 3
            ENDIF
         ENDIF
         GOTO 100
      ELSEIF ( (line(1:1).EQ.'-') .OR. (line(1:1).EQ.'!') ) THEN
         Shape_no = Shape_no + 1
         Sign(Shape_no) = line(1:1)
      ELSEIF ( (line(1:1).EQ.' ') .OR. (line(1:1).EQ.'+') ) THEN
         Shape_no = Shape_no + 1
         Sign(Shape_no) = line(1:1)
      ELSE
         subinfo = 'none of these "-","!", or " " is'//' encountered'
         CALL WTERRM(subname,VERSION,subinfo)
         Errflg = 1
         RETURN
      ENDIF
 
c -----------------------------------------------------------------
c excluded or included region is found by line(1:1)
c Now to find out the first starting point after '('
c -----------------------------------------------------------------
 
      DO 200 i = 1 , 20
         char1 = line(i:i)
         IF ( char1.EQ.'(' ) GOTO 300
 200  CONTINUE
 
c now we have region,ie, BOX,CIRCLE,POLYGON,POINTS,ELLIPSE, or ANNULUS
 
 300  end = i - 1
      Char_no(Shape_no) = end - 1
      IF ( line(2:end).EQ.'CIRCLE' ) THEN
         Shape(Shape_no) = line(2:end)
 
      ELSEIF ( line(2:end).EQ.'BOX' ) THEN
         Shape(Shape_no) = line(2:end)
 
      ELSEIF ( line(2:end).EQ.'ELLIPSE' ) THEN
         Shape(Shape_no) = line(2:end)
 
      ELSEIF ( line(2:end).EQ.'POINT' ) THEN
         Shape(Shape_no) = line(2:end)
 
      ELSEIF ( line(2:end).EQ.'POLYGON' ) THEN
         Shape(Shape_no) = line(2:end)
 
      ELSEIF ( line(2:end).EQ.'ANNULUS' ) THEN
         Shape(Shape_no) = line(2:end)
 
      ELSE
         subinfo = 'Unknown region: '//line(1:end)
         CALL WTERRM(subname,VERSION,subinfo)
         Errflg = 1
         RETURN
      ENDIF
 
 
c Now get all the points
 
c move past '('
 
      start = index(line,'(') + 1
      Npoints(Shape_no) = 0
      DO WHILE ( index(line(start:),',') .NE. 0 )
         end = index(line(start:),',') - 1
         Npoints(Shape_no) = Npoints(Shape_no) + 1
         char_points(Shape_no,Npoints(Shape_no))
     &                         = line(start:start+end-1)
         start = start + end + 1
      ENDDO
      end = index(line(start:),')') - 1
      Npoints(Shape_no) = Npoints(Shape_no) + 1
      char_points(Shape_no,Npoints(Shape_no)) = line(start:start+end-1)

c If type is pixels or degrees we can just read the points. If HMS then
c we read the coordinate pairs and convert to degrees.

      IF ( Type . GE. 2 ) THEN 
         DO i = 1 , Npoints(Shape_no)
            READ (char_points(Shape_no,i),*,IOSTAT=Errflg)
     &            Points(Shape_no,i)
         ENDDO
      ELSEIF ( Type .EQ. 1 .AND. (Shape(Shape_no).EQ.'POINT' .OR.
     &         Shape(Shape_no).EQ.'POLYGON') ) THEN
         DO i = 1 , Npoints(Shape_no)/2
            j = 2*i - 1
            CALL cnvhms(char_points(Shape_no,j), 
     &                  char_points(Shape_no,j+1), Points(Shape_no,j), 
     &                  Points(Shape_no,j+1), Errflg)
         ENDDO

      ELSE
         CALL cnvhms(char_points(Shape_no,1), char_points(Shape_no,2),
     &               Points(Shape_no,1), Points(Shape_no,2), Errflg)
         DO i = 3 , Npoints(Shape_no)
            READ (char_points(Shape_no,i),*,IOSTAT=Errflg)
     &            Points(Shape_no,i)
         ENDDO
      ENDIF
 
      GOTO 100

c Jump point for end of processing file
 
 800  CONTINUE

c If type was HMS we have changed it to DEGREES

      IF ( Type .EQ. 1 ) Type = 2

      RETURN
      END
c --------------------------------------------------------------
c             end of read_region subroutine
c --------------------------------------------------------------
 

c -------------------------------------------------------------------
c                start of hms -> deg position conversion routine
c -------------------------------------------------------------------
*+CNVHMS
 
      SUBROUTINE CNVHMS(Cra, Cdec, Radeg, Decdeg, Errflg)

c ------------------------- description ------------------------------
c CNVHMS reads the RA and DEC strings passed and converts them from
c HMS format to decimal degrees.
c
c ------------- variable definitions ---------------------------
      IMPLICIT NONE
      CHARACTER*(*) Cra, Cdec
      DOUBLE PRECISION Radeg, Decdeg
      INTEGER Errflg
 
c ----------------------- internal variables ---------------------
      DOUBLE PRECISION dval
      INTEGER isign, ipt, ilen
      INTEGER lenact
      EXTERNAL lenact
 
c ----------------------- variable directory ---------------------
c Cra       char i/p   RA
c Cdec      char i/p   Dec
c Radeg     double o/p RA in decimal degrees
c Decdeg    double o/p Dec in decimal degrees
c errflg    int  o/p  error flag defining error status
c
c ----------------------- called routines -----------------------
c    none
c ---------------------------------------------------------------
c
c kaa Sep 24, 1999
c
c ---------------------------------------------------------------

c Convert the RA string to decimal degrees

      ilen = lenact(Cra)
      ipt = ilen
      DO WHILE ( Cra(ipt:ipt) .NE. ':' .AND. ipt .GT. 1 )
         ipt = ipt - 1
      ENDDO
      READ(Cra(ipt+1:ilen),*,iostat=Errflg) dval
      IF ( Errflg .NE. 0 ) RETURN
      radeg = dval/3600.d0

      ipt = ipt - 1
      DO WHILE ( Cra(ipt:ipt) .NE. ':' .AND. ipt .GT. 1 )
         ipt = ipt - 1
      ENDDO
      READ(Cra(ipt+1:ipt+2),*,iostat=Errflg) dval
      IF ( Errflg .NE. 0 ) RETURN
      Radeg = Radeg + dval/60.d0

      READ(Cra(1:ipt-1),*,iostat=Errflg) dval
      IF ( Errflg .NE. 0 ) RETURN
      Radeg = (Radeg + dval) * 15.d0

c Repeat for Dec - first check whether Dec is negative

      IF ( index(Cdec,'-') .EQ. 0 ) THEN
         isign = 1
      ELSE
         isign = -1
      ENDIF

      ilen = lenact(Cdec)
      ipt = ilen
      DO WHILE ( Cdec(ipt:ipt) .NE. ':' .AND. ipt .GT. 1 )
         ipt = ipt - 1
      ENDDO
      READ(Cdec(ipt+1:ilen),*,iostat=Errflg) dval
      IF ( Errflg .NE. 0 ) RETURN
      decdeg = dval/3600.d0

      ipt = ipt - 1
      DO WHILE ( Cdec(ipt:ipt) .NE. ':' .AND. ipt .GT. 1 )
         ipt = ipt - 1
      ENDDO
      READ(Cdec(ipt+1:ipt+2),*,iostat=Errflg) dval
      IF ( Errflg .NE. 0 ) RETURN
      Decdeg = Decdeg + dval/60.d0

      READ(Cdec(1:ipt-1),*,iostat=Errflg) dval
      IF ( Errflg .NE. 0 ) RETURN
      Decdeg = Decdeg + ABS(dval)
      Decdeg = Decdeg * isign

      RETURN
      END



