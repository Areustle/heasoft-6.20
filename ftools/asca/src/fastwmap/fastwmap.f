C  FTOOLs info $Header: /headas/headas/ftools/asca/src/fastwmap/fastwmap.f,v 3.14 2013/05/21 19:08:06 irby Exp $
C      
C*****************************************************************************
C TASK:
C      fastwmap
C
C FILE:
C      fastwmap.f
C
C DESCRIPTION:
C	Program to add a WMAP to FAST mode spectral files
C	
C
C AUTHOR/DATE:
C       Jim Ingham, 6/27/94
C	Hughes STX
C 
C MODIFICATION HISTORY:
C       kaa  11/27/95     Bug fixes : OPTIC keywords were in WMAP bins and
C                                     not detector coordinates.
C                                     The WMAP boundaries were not being
C                                     specified correctly. Added a new routine
C                                     called raw2det to do this.
C                         Horizontal start and end addresses for the area
C                         discriminator, the instrument, the chip number and
C                         the discriminator selection are all read from the 
C                         input file
C       kaa  9/3/96       v3.2 : modifies BACKSCAL keyword in SPECTRUM 
C                                extension so XSPEC background subtraction 
C                                will work.
C       kaa  1/11/02      v3.3 : added support for HDUVERS=2 WMAPs.
C NOTES:
C	
C
C USAGE:
C      HOST: fastwmap
C      IRAF: fastwmap
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C
C	filenm    - input FITS file and extension number
C       outfile   - output file name
C       binsiz    - binning for the WMAP
C       calfil    - filename for calibration file
C	status    - status of operation
C
C CALLED ROUTINES:
C      subroutine gfastwmap - gets parameters from environment
C      subroutine getcal - gets the information from the cal file 
C      subroutine dowmap - makes the WMAP
C      subroutine raw2det - does SIS raw to detector coordinate transform
C
C******************************************************************************

      subroutine fastwp()

      character(160) filenm,outfile,calfil
      integer binsiz, status
      logical clobber

      character(40) taskname
      common /task/ taskname

      taskname = 'fastwmap3.3'

      status = 0

C Get the parameters from the par file
      call gfastwmap(filenm,outfile,binsiz,calfil,clobber,status)

      if( status.ne.0) then
         call fcerr('Error getting parameters')
         return
      endif

C Now write the WMAP:
        
      call dowmap(filenm,outfile,binsiz,calfil,clobber,status)

      return
      end
C*****************************************************************************
C SUBROUTINE
C      gfastwmap
C
C FILE:
C      fastwmap.f
C
C DESCRIPTION:
C	Get the parameters from the par file
C	
C
C AUTHOR/DATE:
C       Jim Ingham, 6/27/94
C	Hughes STX
C 
C MODIFICATION HISTORY:
C       kaa  11/27/95       Horizontal start and end addresses for the
C                           area discriminator are read from the input
C                           file rather than the parameter file. Ditto
C                           for the instrument and chipno and discriminator
C                           selection. Added CALDB support.
C
C USAGE:
C        call gfastwmap(filenm,outfile,binsiz,calfil,clobber,status)
C   
C NOTES:
C	
C
C ARGUMENTS:
C
C	filenm    - input FITS file and extension number
C       outfile   - output file name
C       binsiz    - binning for the WMAP
C       calfil    - filename for calibration file
c       clobber   - overwrite output file if it exists
C	status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C       ilun      - logical unit number
C
C CALLED ROUTINES:
C
C
C************************************************************
      subroutine gfastwmap(filenm,outfile,binsiz,calfil,clobber,status)

      character*(*) filenm,outfile,calfil
      integer binsiz, status
      logical clobber

      integer extno, nret, nfound, chatter
      integer rwmode, block, tmpstat, ilun
      character online*80, instru*4, messag*160, target*160
      character comment*80
      logical qauto


c Get the input filename

      call UCLGST('infile',filenm,status)
      if(status.ne.0) then
         call fcerr('Error getting infile parameter')
         return
      endif

c Test that the input file can be opened and get the instrument

      rwmode = 0
      block = 1
      ilun = 15

      call FTOPEN(ilun,filenm,rwmode,block,status)
      IF(status.eq.103) then
         messag = 'Unable to find FITS file: '//filenm
         call fcerr(messag)
         return
      ELSE IF(status.ne.0) then
         messag = 'Unable to open FITS file: '//filenm
         call fcerr(messag)
         call fcerrm(status)
         tmpstat = 0
         call FTCLOS(ilun,tmpstat)
         return
      ENDIF

C Read the instrument

      call FTGKYS(ilun, 'INSTRUME', instru, comment, status)
      if(status.ne.0) then
         call fcerr('Unable to find INSTRUME keyword in input file')
         return
      endif

      CALL FTCLOS(ilun, status)
      if(status.ne.0) then
         call fcerr('Unable to close input file')
         return
      endif

c Get the output filename

      call UCLGST('outfile',outfile,status)
      if(status.ne.0) then
         call fcerr('Error getting outfile parameter')
         return
      endif

c Get the WMAP binsize

      call UCLGSI('binsize',binsiz,status)
      if(status.ne.0) then
         call fcerr('Error getting binsiz parameter')
         return
      endif

c Get the calibration filename

      call UCLGST('calfile',calfil,status)
      if(status.ne.0) then
         call fcerr('Error getting calfile parameter')
         return
      endif
      qauto = .false.
      IF ( calfil .EQ. 'AUTO' ) qauto = .true.

c If the the calibration filename is set to CALDB or AUTO then interpret

      if ( calfil .EQ. 'CALDB' .OR. qauto ) then

         chatter = 0

c First try the CALDB - if AUTO is set then suppress all i/o

         if ( qauto ) chatter = -1

         call gtcalf(chatter,'ASCA',instru,'-','-', 'ASCALIN', 
     &               'now', 'now', 'now', 'now', '-',1,calfil, 
     &               extno, online, nret, nfound, status)

         chatter = 0

c If successful then optionally write out diagnostics

         if(status.eq.0) then
            if(nfound.EQ.0) then
               call wterrm(taskname,'versn?', 'No files found in CALDB')
               status = 1
               return
            elseif(nfound.GT.1) then	
               call wtwarm(taskname,'versn?',chatter,1,
     &	                   'More than one file found in CALDB')
               call wtinfo(chatter,20,2,'Using 1st one found')
            elseif(nfound.EQ.1) then	
               call wtinfo(chatter,20,2,'GTCALF worked as expected')
               messag = 'found file: '//calfil
               call wtinfo(chatter,20,3,messag)
            endif 
         else

c If the CALDB search was not successful and we are not on AUTO then
c return an error message

            if (.not.qauto) then
               call fcerr('Error accessing the CALDB for calfile')
               return

c If we are on AUTO then check out the refdata area

            else
               status = 0
               target = 's'//instru(4:4)//'_teldef_ascalin.fits'
               call fgfcal( calfil, target, status )
               if (status.ne.0) then
                  messag = 'Failed to find '//target(1:22)
     &                     //' in refdata area' 
                  call fcerr(messag)
                  return
               endif
            endif
         endif
      endif

c Get the clobber flag

      call UCLGSB('clobber', clobber, status)
      if ( status .ne. 0 ) then
         call fcerr('Error getting clobber parameter')
         return
      endif

      return
      end

C*****************************************************************************
C SUBROUTINE
C      dowmap
C
C FILE:
C      fastwmap.f
C
C DESCRIPTION:
C	Add the weighted map
C	
C
C AUTHOR/DATE:
C       Jim Ingham, 6/27/94
C	Hughes STX
C 
C MODIFICATION HISTORY:
C       kaa  11/29/95      Now reads instrument, chip number, stah and endh,
C                          and discriminator selection from file. Fixed buggy 
C                          use of four_chip_image and used a new routine 
C                          called raw2det to do raw->det coord transformation
C
C NOTES:
C	
C
C ARGUMENTS:
C
C	filenm    - input FITS file and extension number
C       outfile   - output file name
C       binsiz    - binning for the WMAP
C       calfil    - name of the cal file
C       clobber   - overwrite output file if it exists
C	status    - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C       ilun      - logical unit number
C
C CALLED ROUTINES:
C
C
C************************************************************
      subroutine dowmap(filenm,outfile,binsiz,calfil,clobber,status)

      character*(*) filenm,outfile,calfil
      integer status, binsiz
      logical clobber

      real detx,dety,rbinsiz,rlowx,rlowy,rhighx,rhighy,backscal
      integer stah, endh, chipno
      integer olun,ilun,rwmode,block,i,j,clun,ivalue
      integer tmpstat,naxes(2),hdutyp,image(426,426),ix,iy
      integer lowx,lowy,highx,highy
      character caltime*80, messag*160, hstrng*8, comment*80, instru*4
      character ccdpow*8, inorout*4, hduclas1*20
      logical found

      rwmode = 0
      block = 1
      olun = 16
      ilun = 15
      clun = 17
      rbinsiz = float(binsiz)

      call FTOPEN(ilun,filenm,rwmode,block,status)
      IF(status.eq.103) then
         messag = 'Unable to find FITS file: '//filenm
         call fcerr(messag)
         return
      ELSE IF(status.ne.0) then
         messag = 'Unable to open FITS file: '//filenm
         call fcerr(messag)
         call fcerrm(status)
         tmpstat = 0
         call FTCLOS(ilun,tmpstat)
         return
      ENDIF

C Read the instrument

      call FTGKYS(ilun, 'INSTRUME', instru, comment, status)
      messag = 'Unable to find INSTRUME keyword'
      if (status.ne.0) goto 999

C Read the chip number from the CCDPOW string

      write(hstrng,'(a1,a1,a6)') 'S', instru(4:4), 'CCDPOW'
      CALL FTGKYS(ilun, hstrng, ccdpow, comment, status)
      messag = 'Unable to find '//hstrng//' keyword'
      IF (status.ne.0) goto 999

      found = .false.
      DO i = 1, 4
         IF ( ccdpow(i:i) .EQ. '1' ) THEN
            IF ( found) THEN
               messag = 'More than 1 CCD in use - cannot be fastmode !'
               CALL fcerr(messag)
               return
            ELSE
               found = .true.
               chipno = i - 1
            ENDIF
         ENDIF
      ENDDO

C Read the area discriminator horizontal start and end addresses

      write(hstrng,'(a1,a1,a5,i1)') 'S', instru(4:4), '_STAH', chipno
      call FTGKYJ(ilun, hstrng, stah, comment, status)
      messag = 'Unable to find '//hstrng//' keyword'
      IF (status.ne.0) goto 999

      write(hstrng,'(a1,a1,a5,i1)') 'S', instru(4:4), '_ENDH', chipno
      call FTGKYJ(ilun, hstrng, endh, comment, status)
      messag = 'Unable to find '//hstrng//' keyword'
      IF (status.ne.0) goto 999

C Read the discriminator selection that has been used to generate this
C file. First checks the ARENA keyword to see whether area discrimination
C was enabled. If it was then check the ARIO keyword to see whether inside
C or outside was selected.

      write(hstrng,'(a1,a1,a6)') 'S', instru(4:4), '_ARENA'
      call FTGKYJ(ilun, hstrng, ivalue, comment, status)
      messag = 'Unable to find '//hstrng//' keyword'
      IF (status.ne.0) goto 999

      IF ( ivalue .EQ. 0 ) THEN
         inorout = 'BOTH'
      ELSE
         write(hstrng,'(a1,a1,a5,i1)') 'S', instru(4:4), '_ARIO', chipno
         call FTGKYJ(ilun, hstrng, ivalue, comment, status)
         messag = 'Unable to find '//hstrng//' keyword'
         IF (status.ne.0) goto 999
         IF ( ivalue .EQ. 0 ) THEN
            inorout = 'OUT'
         ELSE
            inorout = 'IN'
         ENDIF
      ENDIF

C Set the detector parameter, then read the calibration file and
C set the variables in the common blocks

      call fwmpdet(instru)
      call read_sis_cal(calfil,clun,caltime,status)
      messag = 'Failed to get SIS calibration data'
      if(status.ne.0) goto 999

C Initialize output FITS file

      if ( clobber ) call delfil(outfile)
      call FTINIT(olun,outfile,block,status)
      messag = 'Unable to create FITS file: '//outfile
      if(status.ne.0) goto 999

C Now write the weighted map:
C First translate the CHIP coordinate values to DETX and DETY:
C Get the bounds in DETX DETY:

      IF ( inorout .EQ. 'IN' ) THEN

         call raw2det(stah,1,chipno,rlowx,rlowy)
         call raw2det(endh,422,chipno,rhighx,rhighy)

      ELSEIF ( inorout .EQ. 'OUT' .OR. inorout .EQ. 'BOTH' ) THEN

         call raw2det(6,1,chipno,rlowx,rlowy)
         call raw2det(426,422,chipno,rhighx,rhighy)

      ENDIF

C Convert the boundaries from detector coordinates to WMAP bins

      highx = int(MAX(rhighx,rlowx)/rbinsiz)
      highy = int(MAX(rhighy,rlowy)/rbinsiz)
      lowx = int(MIN(rlowx,rhighx)/rbinsiz)
      lowy = int(MIN(rlowy,rhighy)/rbinsiz)

      naxes(1) = (highx - lowx) + 1
      naxes(2) = (highy - lowy) + 1

      call FTPHPR(olun,.TRUE.,32,2,naxes,0,1,.TRUE.,status)
      messag = 'Failed to write mandatory header information'
      if(status.ne.0) goto 999

      call xcopynoscale(ilun,olun,status)      

C Now add the required header keywords:

      call primary_sis_axis_key(olun,binsiz,lowx,lowy,naxes,
     &     instru,calfil,caltime,status)

      call FTPDEF(olun,32,2,naxes,0,1,status)

C For the IN or the BOTH options the entire WMAP should be selected.
C For the OUT option those bins corresponding to RAWX values between
C stah and endh should be excluded.

      do j = 1, naxes(2)
         do i = 1, naxes(1)
            image(i,j) = 1
         enddo
      enddo

      if (inorout.eq.'OUT') then

         do i = stah, endh
            do j = 1, 422
               call raw2det(i,j,chipno,detx,dety)
               ix = int((detx - rlowx)/rbinsiz)+1
               iy = int((dety - rlowy)/rbinsiz)+1
               image(ix,iy) = -1
            enddo
         enddo

      endif

C Backscal is set to the fraction of the detector image that is selected.

      backscal = 0.
      do j = 1, naxes(2)
         do i = 1, naxes(1)
            IF ( image(i,j) .GT. 0. ) backscal = backscal + 1.
         enddo
      enddo
      backscal = backscal / (1280/binsiz)**2

C Now write the image matrix to the outfile:

      call FTP2DJ(olun,0,426,naxes(1),naxes(2),image,status)
      messag = 'Error writing FITS image.'
      if(status.ne.0) goto 999

C Okay, now copy what remains into the out file...

      do i = 1, 99

C Move to the next extension in the input file

         call FTMRHD(ilun,1,hdutyp,status)
         write(messag,'(a26,I2,a10,a)') 'Error moving to extension ',i,
     &        ' in file: ',filenm(:min(122,len(filenm)))

C An error status of 107 means that we have reached the end of the input
C file

         if(status.eq.107) then
            status = 0
            goto 999
         else if(status.ne.0) then
            goto 999
         endif

C Start the next extension in the output file

         call FTCRHD(olun,status)

C Copy the entire extension from input to output file

         call FTCOPY(ilun,olun,0,status)
         write(messag,'(a24,I2,a12,a)') 'Error copying extension ',i,
     &        ' from file: ',filenm(:min(122,len(filenm)))
         if(status.ne.0) goto 999

C If this extension is the SPECTRUM then modify the BACKSCAL keyword

         CALL FTGKYS(ilun, 'HDUCLAS1', hduclas1, comment, status)
         messag = 'Unable to find HDUCLAS1 keyword'
         IF (status.ne.0) goto 999

         IF ( hduclas1(1:8) .EQ. 'SPECTRUM' ) THEN
            CALL FTMKYE(olun, 'BACKSCAL', backscal, 13, '&', status)
            messag = 'Unable to modify BACKSCAL keyword'
            IF (status.ne.0) goto 999
         ENDIF

      enddo

 999  continue
      if (status.ne.0) then
         call fcerr(messag)
         call fcerrm(status)
      endif

      status = 0
      call FTCLOS(olun,status)
      if(status.ne.0) then
         call fcerr('Error closing outfile')
         call fcerrm(status)
         status = 0
      endif

      call FTCLOS(ilun,status)
      if(status.ne.0) then
         call fcerr('Error closing infile')
         call fcerrm(status)
         status = 0
      endif      
      
      return
      end
      

C**********************************************************************
C SUBROUTINE primary_sis_axis_key
C
C FILE:
C      fastwmap.f
C
C DESCRIPTION:
C	Add the weighted map
C	
C USAGE
C    call primary_sis_axis_key(ilun,binsiz,lowx,lowy,naxes,instru,calname,
c                                               caltime,status)
C 
C ARGUMENTS
C ilun    -  logical unit number
C binsiz  -  the binsize for the WMAP
C lowx    -  the DETX coordinate of the lower L.H. corner of the chip
C lowy    -  the DETY coordinate of the lower L.H. corner of the chip
C naxes   -  the x and y sizes of the image
C instru  -  the instrument name
C calname -  calibration file name
C caltime -  time fro calname
C status  -  status variable
C
C PRIMARY LOCAL VARIABLE
C
C AUTHOR/DATE:
C       Jim Ingham, 6/27/94
C	Hughes STX
C 
C MODIFICATION HISTORY:
C       kaa  11/27/95    Changed OPTIC keywords so that they are written
C                        in detector coords rather than WMAP bins.
C
C 
C **********************************************************************

      subroutine primary_sis_axis_key(ilun,binsiz,flowx,flowy,naxes,
     &     instru,calname,caltime,status)

      character*(*) calname,caltime,instru
      integer ilun,status,binsiz,flowx,flowy,naxes(2)
      real rbinsiz

      include 'asca_defs.inc'
      include 'asca_common.inc'

      character(80) comment
      character(8) keyname
      integer decimals

      character(40) taskname
      common /task/ taskname
      
      decimals = 8

      x_det_col = 1
      y_det_col = 2
      rbinsiz = float(binsiz)

      call ftmkys (ilun, 'TELESCOP', 'ASCA', ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'TELESCOP', 'ASCA', ' ', status)
      end if

      call ftmkys (ilun, 'INSTRUME', instru, ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'INSTRUME', instru, ' ', status)
      end if

      call ftmkys (ilun, 'DATAMODE', 'FAST', ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'DATAMODE', 'FAST', ' ', status)
      end if

      call ftmkys (ilun, 'HDUCLASS', 'ogip', ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'HDUCLASS', 'ogip', ' ', status)
      end if

      call ftmkys (ilun, 'HDUCLAS1', 'IMAGE', ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'HDUCLAS1', 'IMAGE', ' ', status)
      end if

      call ftmkys (ilun, 'HDUCLAS2', 'WMAP', ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'HDUCLAS2', 'WMAP', ' ', status)
      end if
      call ftmkys (ilun, 'HDUVERS', '2.0.0', ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'HDUVERS', '2.0.0', ' ', status)
      end if

      call ftmkyj (ilun, 'AXLEN1', naxes(1), ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkyj (ilun, 'AXLEN1', naxes(1), ' ', status)
      end if

      call ftmkyj (ilun, 'AXLEN2', naxes(2), ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkyj (ilun, 'AXLEN2', naxes(2), ' ', status)
      end if

      call ftmkys (ilun, 'CONTENT', 'SPECTRUM', ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'CONTENT', 'SPECTRUM', ' ', status)
      end if
C
C     call ftmkys (ilun, 'EXTNAME', 'WMAP', ' ', status)
C     if (status .eq. 202) then
C        status = 0
C        call ftpkys (ilun, 'EXTNAME', 'WMAP', ' ', status)
C     end if
C
      write(comment,'(a14, a12, a54)') taskname,
     &     ': CAL FILE: ', calname(1:54)
      call ftmkys (ilun, 'SISLIN1', comment, ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'SISLIN1', comment, ' ', status)
      end if
      
      write(comment,'(a14, a23, a43)') taskname, 
     &     ': CAL FILE DATE STAMP: ', caltime(1:43)
      call ftmkys (ilun, 'SISLIN2', comment, ' ', status)
      if (status .eq. 202) then
         status = 0
         call ftpkys (ilun, 'SISLIN2', comment, ' ', status)
      end if
      
      if (status .ne. 0) then 
         comment = 'Error writing history cards'
         call fcerrm(status)
         status = 0
      end if
      call modify_f_kw(ilun, 'FOV_X_MM', fov_x_size, decimals,
     &     'Detector X field of view (mm)', status)
      
      call modify_f_kw(ilun, 'FOV_Y_MM', fov_y_size, decimals,
     &     'Detector Y field of view (mm)', status)
      
      optical_x_axis = optical_x_axis / det_x_scale +
     &       det_x_center
      optical_y_axis = - optical_y_axis / det_y_scale +
     &       det_y_center

      call modifyn_f_kw(ilun, 'OPTIC', x_det_col, 
     &     optical_x_axis, decimals, 
     &     'Optical axis X in detector coords (pixels)', status)
           
      call modifyn_f_kw(ilun, 'OPTIC', y_det_col, 
     &     optical_y_axis, decimals, 
     &     'Optical axis Y in detector coords (pixels)', status)

c Write the keywords relating WMAP pixels to detector image pixels.

      keyname = 'WCSTYP'
      WRITE(keyname(7:7),'(i1)') x_det_col
      CALL ftukys(ilun, keyname(1:7), 'PHYSICAL',
     &            ' ', status)
      WRITE(keyname(7:7),'(i1)') y_det_col
      CALL ftukys(ilun, keyname(1:7), 'PHYSICAL',
     &            ' ', status)
           
      keyname = 'CTYPE#P'
      WRITE(keyname(6:6),'(i1)') x_det_col
      CALL ftukys(ilun, keyname(1:7), 'DETX',
     &            'Source of X axis', status)
      WRITE(keyname(6:6),'(i1)') y_det_col
      CALL ftukys(ilun, keyname(1:7), 'DETY',
     &            'Source of Y axis', status)

c The reference pixel is the origin of the WMAP 

      keyname = 'CRPIX#P'
      WRITE(keyname(6:6),'(i1)') x_det_col
      CALL ftukye(ilun, keyname(1:7), 1.0, decimals,
     &            'Detector X ref pixel', status)
      WRITE(keyname(6:6),'(i1)') y_det_col
      CALL ftukye(ilun, keyname(1:7), 1.0, decimals,
     &            'Detector Y ref pixel', status)

c This locates the Lower Left hand corner of the chip
c in the four chip space:
      
      keyname = 'CRVAL#P'
      WRITE(keyname(6:6),'(i1)') x_det_col
      CALL ftukye(ilun, keyname(1:7),
     &            (flowx*binsiz-(binsiz-1)/2.), decimals,
     &            'Detector X ref pixel value', status)
      WRITE(keyname(6:6),'(i1)') y_det_col
      CALL ftukye(ilun, keyname(1:7),
     &            (flowy*binsiz-(binsiz-1)/2.), decimals,
     &            'Detector Y ref pixel value', status)

c The rebinning factor

      keyname = 'CDELT#P'
      WRITE(keyname(6:6),'(i1)') x_det_col
      CALL ftukye(ilun, keyname(1:7), rbinsiz, decimals,
     &       'X axis increment in units of original pixels', status)
      WRITE(keyname(6:6),'(i1)') y_det_col
      CALL ftukye(ilun, keyname(1:7), rbinsiz, decimals,
     &       'Y axis increment in units of original pixels', status)


c Then the keywords relating WMAP pixels to the detector physical plane.

      call modifyn_f_kw(ilun, 'CRPIX', x_det_col, 
     &     640.5-(flowx-1)*rbinsiz, decimals, 
     &     'Detector X ref pixel (center of address space)', 
     &     status)
      
      call modifyn_f_kw(ilun, 'CRPIX', y_det_col,
     &     640.5-(flowy-1)*rbinsiz, decimals, 
     &     'Detector Y ref pixel (center of address space)', 
     &     status)

      call modifyn_f_kw(ilun, 'CRVAL', x_det_col, 0.0, 
     &     decimals, 'Detector X ref pixel value (pixels)', status)
      
      call modifyn_f_kw(ilun, 'CRVAL', y_det_col, 0.0, 
     &     decimals, 'Detector Y ref pixel value (pixels)', status)


C Scale the delta keyword:

      det_x_scale = det_x_scale*rbinsiz
      call modifyn_f_kw(ilun, 'CDELT', x_det_col, det_x_scale, 
     &     decimals, 'Detector X pixel scale (mm/pixel)', status)
      
      det_y_scale = det_y_scale*rbinsiz
      call modifyn_f_kw(ilun, 'CDELT', y_det_col, det_y_scale,
     &     decimals, 'Detector Y pixel scale (mm/pixel)', status)
      
      if (status .ne. 0)
     &     call fcerr('Error updating detector keywords')


      return
      end
   
C**********************************************************************
C SUBROUTINE fwmpdet
C
C DESCRIPTION:
C    This just sets the detector in Eric's ASCA_COMMON so the 
C    read_sis_cal doesn't choke.
C
C**********************************************************************
      subroutine fwmpdet(instru)

      character*(*) instru

      include 'asca_defs.inc'
      include 'asca_common.inc'

      if(instru .eq. 'SIS0' ) THEN
         detector = SIS0
      else if(instru.eq.'SIS1') then
         detector = SIS1
      endif

      return
      end

C**********************************************************************
C SUBROUTINE raw2det
C
C DESCRIPTION:
C    Converts an SIS raw coordinate into detector coordinate
C
C**********************************************************************

      subroutine raw2det(rawx, rawy, chipno, detx, dety)

      real detx, dety
      integer rawx, rawy, chipno

      include 'asca_defs.inc'
      include 'asca_common.inc'

c  Arguments :
c     rawx     i        i: Raw X coordinate
c     rawy     i        i: Raw Y coordinate
c     chipno   i        i: Chip number (0-3)
c     detx     r        r: Detector X coordinate
c     dety     r        r: Detector Y coordinate

c This routine requires the appropriate camera teldef file
c to have been read and the common blocks initialized using
c read_sis_cal.

      real rx, ry, x, y
      real cos_ang, sin_ang

      call four_chip_image(rawx, rawy, chipno, x, y)

      x = x * xyscale - det_x_center
      y = y * xyscale - det_y_center 

      cos_ang = cos(det_rotation * deg_to_rad) 
      sin_ang = sin(det_rotation * deg_to_rad) 
              
      rx = x * cos_ang - y * sin_ang
      ry = x * sin_ang + y * cos_ang
              
      rx =  rx + det_x_center
      ry = -ry + det_y_center

      detx = max(min(rx+det_x_pix1, float(sis_size)),det_x_pix1)
      dety = max(min(ry+det_y_pix1, float(sis_size)),det_y_pix1)

      return
      end
