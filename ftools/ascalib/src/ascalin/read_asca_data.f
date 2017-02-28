C******************************************************************************
C SUBROUTINE:
C
C      read_asca_data
C
C DESCRIPTION:
C
C      opens and gets information from an asca event file
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,    July 1993
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C
C      Modification of the open_gis_data and open_sis_data subroutines.
C
C      Jeff Guerber, RITSS/GSFC, 1999-03-02:  axlen1/2 (asca_defs.inc) are
C          integers so read TLMAXn with ftgkyj not ftgkye.  Fixed comments
C          to describe this routine instead of open_asca_data.
C
C NOTES:
C
C USAGE:
C
C      subroutine read_asca_data (data_name, frf_name, creator, object,
C     &     sequence, iunit,
C     &     exten, detect, pos_det, timecol, rawxcol, rawycol, chipcol,
C     &     detxcol, detycol, skyxcol, skyycol, phacol, picol, histry,
C     &     status)
C
C ARGUMENTS:
C
C     data_name - input FITS file and extension number
C     frf_name  - output telemetry file name
C     creator   - output creator/author name
C     object    - output object name
C     sequence  - output sequence ID
C     iunit     - FORTRAN I/O unit for opened data file
C     exten     - extension number
C     detect    - detector name
C     pos_det   - position algorithm used
C     timecol   - input time colum name
C     rawxcol   - raw electronic coordinate input X column name
C     rawycol   - raw electronic coordinate input Y column name
C     chipcol   - input CCD ID
C     detxcol   - physical coordinate output X column name
C     detycol   - physical coordinate output Y column name
C     skyxcol   - sky X column name, assumed 'X'
C     skyycol   - sky Y column name, assumed 'Y'
C     phacol    - raw electronic input PHA column name
C     picol     - physical coordinate output PI column name
C     histry    - whether to add history record to header
C     status    - FITSIO status
C
C PRIMARY LOCAL VARIABLES:
C
C     dmode  - datamode string
C     detect - detector string
C     maxcl  - maximum number of FITS table columns
C     contxt - error discription string and temp string
C     mode   - common block containing various det mode and data file info
C
C     fitsio variables - hdtype, rows, filenm, xtensn, rwmode, block, rowlen
C                        vardat, fcstln, crpix1, crpix2, tfield, exact
C                        tbcol, ttype, tform, tunit, extnam, comment,  keywd
C
C CALLED ROUTINES:
C
C      subroutine fcecho - echo message to terminal
C      function fcstln   - returns index of last non-blank character
C      subroutine ftxxxx - FITSIO calls
C
C******************************************************************************

      subroutine read_asca_data (data_name, frf_name, creator, object,
     &     sequence, iunit,
     &     exten, detect, pos_det, timecol, rawxcol, rawycol, chipcol,
     &     detxcol, detycol, skyxcol, skyycol, phacol, picol, histry,
     &     status)

        implicit none

        character*(*) data_name, frf_name, creator, object
        character*(*) sequence, detect, pos_det
        character*(*) rawxcol, rawycol, chipcol, detxcol, detycol
        character*(*) skyxcol, skyycol, phacol, picol, timecol
        integer iunit, exten, status
        logical histry

        include 'asca_defs.inc'

        integer xsize, ival, i, stat

        integer maxcl
        parameter (maxcl = 512)

        real fval
        double precision tscale, tzero
        integer hdtype, nrecords, xtensn, rwmode, block, rowlen
        integer vardat, fcstln, tfield, tbcol(maxcl)
        character(80) filenm, ttype(maxcl), tform(maxcl), tunit(maxcl)
        character(80) extnam, contxt, comment
        character(40) taskname
        character(15) keywd, dmode
        character(15) radecproj, bitrate, telescope, sval, radecsys
        logical exact

        include 'asca_common.inc'

        common /task/ taskname

        tzero  = 0.0D0
        tscale = 1.0D0

        exact = .false.
        rwmode = 1

C     get the input file name and extension

        call fcpars (data_name, filenm, xtensn, status)
        contxt = 'Unable to parse GIS data file name and extension '
        if (status .ne. 0) goto 999

C     open data file

        call ftopen (iunit, filenm, rwmode, block, status)
        contxt = 'Unable to open GIS data file: '//filenm
        if (status .ne. 0) goto 999

C     check for reasonable extension number

        if (xtensn .eq. -99) xtensn = 1
        exten = xtensn
        contxt = 'Primary array not supported'
        if (xtensn .eq. 0) then
           status = 1
           goto 998
        end if

C     get the mission name from the primary header

        call ftgkys (iunit, 'TELESCOP', telescope, contxt, status)
        contxt = 'Cannot determine telescope'
        if (status .ne. 0) goto 998

        if (index(telescope, 'ASCA') .eq. 0) then
           contxt = 'Is this ASCA data? TELESCOP= ' // telescope
           status = 1
           goto 998
        end if

C     get the detector name from the primary header

        call ftgkys (iunit, 'INSTRUME', detect, contxt, status)
        contxt = 'Cannot determine detector'
        if (status .ne. 0) goto 998

C     check for known detector

        detector = -1

        if (index(detect,'SIS0') .ne. 0) then
           dettype = SIS
           detector = SIS0
        else if (index(detect,'SIS1') .ne. 0) then
           dettype = SIS
           detector = SIS1
        else if (index(detect,'SIS2') .ne. 0) then
           dettype = SIS
           detector = SIS2
        else if (index(detect,'GIS2') .ne. 0) then
           dettype = GIS
           detector = GIS2
        else if (index(detect,'GIS3') .ne. 0) then
           dettype = GIS
           detector = GIS3
        else
           contxt = ' Unknown instrument: ' // detect
           status = 1
           goto 998
        endif

C     look for the FRF filename:

        call ftgkys (iunit, 'TLM_FILE', frf_name, contxt, status)
        contxt = 'Cannot determine TLM_FILE'
        if (status .ne. 0) goto 998

C     look for the object name:

        call ftgkys (iunit, 'OBJECT', object, contxt, status)
        contxt = 'Cannot determine OBJECT'
        if (status .eq. 202) then
           object = 'N/A'
           status = 0
        end if
        if (status .ne. 0) goto 998

C     look for the sequence number:

        call ftgkys (iunit, 'SEQNUM', sequence, contxt, status)
        contxt = 'Cannot determine SEQNUM'
        if (status .eq. 202) then
           sequence = 'N/A'
           status = 0
        end if
        if (status .ne. 0) goto 998

C     look for the RA_NOM keyword:

        call ftgkye (iunit, 'RA_NOM', ra_nom, contxt, status)
        if (status .eq. 202) then
           contxt = 'WARNING: Cannot find RA_NOM  in primary hdr, set to
     & zero'
           call fcecho(contxt)
           ra_nom = 0.0
           status = 0
        else if (status .ne. 0) then
           contxt = 'Cannot determine RA_NOM in primary header'
           goto 998
        end if

C     look for the DEC_NOM keyword:

        call ftgkye (iunit, 'DEC_NOM', dec_nom, contxt, status)
        if (status .eq. 202) then
           contxt = 'WARNING: Cannot find DEC_NOM in primary hdr, set to
     & zero'
           call fcecho(contxt)
           dec_nom = 0.0
           status = 0
        else if (status .ne. 0) then
           contxt = 'Cannot determine DEC_NOM in primary header'
           goto 998
        end if

C     look for the creator/author:

        call ftgkys (iunit, 'CREATOR', creator, contxt, status)
        if (status .eq. 202) then
           status = 0
           call ftgkys (iunit, 'AUTHOR', creator, contxt, status)
        end if
        contxt = 'Cannot determine CREATOR/AUTHOR'
        if (status .ne. 0) goto 998

        if (dettype .eq. GIS) then
           rt_off = -1.0
           call ftgkyj(iunit, 'RT_LD', ival, comment, status)
           if (status .eq. 0) then
              rt_off = ival
           else if (status .eq. 202) then
              status = 0
           else
              contxt = 'Cannot get RT_LD'
              if (status .ne. 0) goto 998
           end if

           rt_scale = -1.0
           call ftgkyj(iunit, 'RT_B_CD', ival, comment, status)
           if (status .eq. 0) then
              rt_scale = 1.0
              if (ival .gt. 0) rt_scale = float(2**ival)
           else if (status .eq. 202) then
              status = 0
           else
              contxt = 'Cannot get RT_B_CD'
              if (status .ne. 0) goto 998
           end if
        else
           do i=1, nsisob
              write(keywd,'(a1,i1.1,a6)') 'S', detector-1, sisob(i)
              call ftgkyj(iunit, keywd, sis_ob(i), comment, status)
c              write(*,*) sisob(i), keywd, sis_ob(i)
           end do
           contxt = 'Cannot get SIS AE/DE/LV/AD keywords'
           if (status .ne. 0) goto 998
           write(keywd,'(a1,i1.1,a6)') 'S', detector-1, 'CCDPOW'
           call ftgkys(iunit, keywd, sval, comment, status)
           contxt = 'Cannot get SIS CCDPOW keyword'
           if (status .ne. 0) goto 998
           ccd_mode = 0
           do i=1,nccd
              ccds(i) = 0
              if (sval(i:i) .eq. '1') ccds(i) = 1
              ccd_mode = ccd_mode + ccds(i)
           end do
           write(keywd,'(a1,i1.1,a6)') 'S', detector-1, '_GRADE'
           call ftgkys(iunit, keywd, sval, comment, status)
           contxt = 'Cannot get SIS GRADE keyword'
           if (status .ne. 0) goto 998
           do i=1,ngrade
              grade(i) = 0
              if (sval(i:i) .eq. '1') grade(i) = 1
           end do
        end if

C     move to the correct extension

        call ftmrhd (iunit, xtensn, hdtype, status)
        contxt = 'Error moving to requested extension (1)'
        if (status .ne. 0) goto 998

C     get header depending on extension type

        if (hdtype .eq. 1) then
           call ftghtb (iunit, maxcl, rowlen, nrecords, tfield,
     &          ttype, tbcol, tform, tunit, extnam, status)
        else if (hdtype .eq. 2) then
           call ftghbn (iunit, maxcl, nrecords, tfield, ttype,
     &          tform, tunit, extnam, vardat, status)
        else
           contxt = 'File extension type not supported'
           goto 998
        endif

        contxt = 'Error getting extension header info'
        if (status .ne. 0) goto 998

C     check for DATAMODE

        call ftgkys (iunit, 'DATAMODE', dmode, contxt, status)
        contxt = 'Cannot determine DATAMODE'
        if (status .ne. 0) goto 998

C     get the data mode

        datamode = -1

        if (index(dmode,'NA') .ne. 0) datamode = NA_mode

        if (index(dmode,'PH') .ne. 0) datamode = PH_mode
        if (index(dmode,'PH2') .ne. 0) datamode = PH2_mode
        if (index(dmode,'MPC') .ne. 0) datamode = MPC_mode
        if (index(dmode,'MEMORY_CHECK') .ne. 0) datamode = MEMORY_mode
        if (index(dmode,'PCAL') .ne. 0) datamode = PCAL_mode
        if (index(dmode,'PCAL') .ne. 0) datamode = PCAL_mode

        if (index(dmode,'FAINT') .ne. 0)  datamode = FAINT_mode
        if (index(dmode,'BRIGHT') .ne. 0)  datamode = BRIGHT_mode
        if (index(dmode,'BRIGHT2') .ne. 0) datamode = BRIGHT2_mode
        if (index(dmode,'FAST') .ne. 0) datamode = FAST_mode
        if (index(dmode,'FAST2') .ne. 0) datamode = FAST2_mode
        if (index(dmode,'FRAME') .ne. 0) datamode = FRAME_mode
        if (index(dmode,'HISTOGRAM') .ne. 0) datamode = HISTOGRAM_mode
        if (index(dmode,'DARK_IMAGE') .ne. 0) datamode = DARK_IMAGE_mode
        if (index(dmode,'INTEGRATION') .ne.0) datamode=INTEGRATION_mode
        if (index(dmode,'NONOBSERVATION') .ne.0) datamode=NONOBS_mode

        if (datamode .eq. -1) then
           contxt = 'Unknown DATAMODE: ' // dmode
           status = 1
           goto 998
        endif

C     check if valid ASCALIN file:

        if ( ((dettype .eq. GIS .and. (datamode.eq.PH_mode .or.
     &       datamode.eq.PH2_mode))) .or. (dettype .eq. SIS .and.
     &       (datamode.eq.FAINT_mode .or. datamode .eq.
     &       BRIGHT_mode .or. datamode.eq.BRIGHT2_mode)) ) then
        else
           contxt = 'Non-imaging data modes not supported: ' // dmode
           status = 1
           goto 998
        end if

C     check for BIT_RATE

        call ftgkys (iunit, 'BIT_RATE', bitrate, contxt, status)
        if (status .ne. 0) then
                contxt = 'Cannot determine BIT_RATE: '
                call fcecho (contxt)
                status = 1
                goto 998
        endif

C     get the bit rate

        bit_rate = -1

        if (index(bitrate,'HIGH') .ne. 0)   bit_rate = HIGH_bit_rate
        if (index(bitrate,'MEDIUM') .ne. 0) bit_rate = MEDIUM_bit_rate
        if (index(bitrate,'LOW') .ne. 0)    bit_rate = LOW_bit_rate
        if (index(bitrate,'NA') .ne. 0)     bit_rate = NA_bit_rate

        if (bit_rate .eq. -1) then
           continue
           contxt = 'Unknown BIT_RATE: ' // bitrate
           call fcecho (contxt)
           status = 1
           goto 998
        endif

        tzero  = 0.0D0
        tscale = 1.0D0

C     reset scaling for table columns (ignore scaling parameters):

        do i=1, tfield
           call fttscl(iunit, i, tscale, tzero, status)
        end do

        contxt = 'Error reseting TSCALE/TZERO values'
        if (status .ne. 0) goto 999

        n_events = nrecords

C     Get extension observation parameters

C     check that the TIME columns exist

        call ftgcno (iunit, exact, timecol, time_col, status)
        contxt = 'TIME column does not exist'
        if (status .ne. 0) goto 998

C     check that the SKY X, Y columns exist

        call ftgcno (iunit, exact, 'X', x_sky_col, status)
        call ftgcno (iunit, exact, 'Y', y_sky_col, status)
        contxt = 'SKY X or Y column do not exist'
        if (status .ne. 0) goto 998

        if (dettype .eq. GIS) then

C     check that the PHA and PI columns exist

           call ftgcno (iunit, exact, phacol, pha_col, status)
           call ftgcno (iunit, exact, picol, pi_col, status)
           contxt = 'PHA or PI column do not exist'
           if (status .ne. 0) goto 998

C     check that the SPREAD, RT, and RTI columns exist

           call ftgcno (iunit, exact, 'SPREAD', spread_col, status)
           call ftgcno (iunit, exact, 'RISE_TIME', rt_col, status)
           contxt = 'SP, or RT column do not exist'
           if (status .ne. 0) goto 998

C     check that the RTI column exist

           call ftgcno (iunit, exact, 'RTI', rti_col, status)
           contxt = 'RTI column do not exist'
           if (status .ne. 0) goto 998

        else

C     check that the PHA columns exist

           call ftgcno (iunit, exact, phacol, pha_col, status)
           contxt = 'PHA column do not exist'
           if (status .eq. 219) then
              status = 0
              call ftgcno (iunit, exact, 'PHAS', pha_col, status)
           end if
           if (status .ne. 0) goto 998

C     check that the CCD ID columns exist

           call ftgcno (iunit, exact, chipcol, c_raw_col, status)
           contxt = 'CCD ID  column do not exist'
           if (status .ne. 0) goto 998

        end if

C     check that the RAW X, Y columns exist

        call ftgcno (iunit, exact, rawxcol, x_raw_col, status)
        call ftgcno (iunit, exact, rawycol, y_raw_col, status)
        contxt = 'ELECTRONIC X or Y column do not exist'
        if (status .ne. 0) goto 998

C     check that the DET X, Y columns exist

        call ftgcno (iunit, exact, detxcol, x_det_col, status)
        call ftgcno (iunit, exact, detycol, y_det_col, status)
        contxt = 'DETECTOR X or Y column do not exist'
        if (status .ne. 0) goto 998

C     Get start and stop time of data

        call ftgkyd(iunit, 'TSTART', tstart, comment, status)
        contxt = 'Cannot get TSTART'
        if (status .ne. 0) goto 998

        call ftgkyd(iunit, 'TSTOP', tstop, comment, status)
        contxt = 'Cannot get TSTOP'
        if (status .ne. 0) goto 998

        if (dettype .eq. GIS) then

           call ftgkyj(iunit, 'PHA_BINS', ival, comment, status)
           pha_size = ival
           det_pi_size = pha_size
           contxt = 'Cannot get PHA_BINS'
           if (status .ne. 0) goto 998

           call ftgkyj(iunit, 'RISEBINS', ival, comment, status)
           contxt = 'Cannot get RISE_BIN or RISEBINS'
           if (status .eq. 202) then
              status = 0
              call ftgkyj(iunit, 'RISE_BIN', ival, comment, status)
           end if
           rise_size = ival
           if (status .ne. 0) goto 998

           call ftgkyj(iunit, 'SP_BINS', ival, comment, status)
           spread_size = ival
           contxt = 'Cannot get SP_BINS'
           if (status .ne. 0) goto 998

           call ftgkyj(iunit, 'RAWXBINS', ival, comment, status)
           xsize = ival
           contxt = 'Cannot get RAWXBINS'
           if (status .ne. 0) goto 998

           call ftgkyj(iunit, 'RAWYBINS', ival, comment, status)
           pos_size = ival
           contxt = 'Cannot get RAWYBINS'
           if (status .ne. 0) goto 998

           if (xsize .ne. pos_size) then
              contxt = 'WARNING: X and Y scales differ !!!'
              call fcecho (contxt)
           end if

           pos_size = min(pos_size, xsize)

C     determine position algorithm used:

           contxt = 'Cannot determine POS_DET'
           call ftgkys (iunit, 'POS_DET', pos_det, contxt, status)
           if (status .ne. 0) goto 998

           pos_meth = -1
           if (index(pos_det,'FLF') .ne. 0)  pos_meth = FLF
           if (index(pos_det,'POW2') .ne. 0) pos_meth = POW2

           contxt = 'Unknown POS_DET keyword walue'
           if (pos_meth .eq. -1) then
              status = 1
              goto 998
           end if

           if (rt_scale .eq. -1) then

              call ftgkyj(iunit, 'RT_LD', ival, comment, status)
              rt_off = ival
              contxt = 'Cannot get RT_LD'
              if (status .ne. 0) goto 998

              call ftgkyj(iunit, 'RT_B_CD', ival, comment, status)
              contxt = 'Cannot get RT_B_CD'
              if (status .ne. 0) goto 998
              rt_scale = 1.0
              if (ival .gt. 0) rt_scale = real(2**ival)

           end if

        end if

        call ftgkys (iunit, 'RADECSYS', radecsys, comment, status)
        contxt = ' cannot get RADECSYS'
        if (status .ne. 0) goto 998

        call ftgkye (iunit, 'EQUINOX', equinox, comment, status)
        contxt = ' cannot get EQUINOX'
        if (status .ne. 0) goto 998

        call catnum(keywd, 'TCTYP', x_sky_col)
        call ftgkys (iunit, keywd, radecproj, comment, status)
        call catnum(keywd, 'TCTYP', y_sky_col)
        call ftgkys (iunit, keywd, radecproj, comment, status)
        contxt = ' cannot get RA---TAN/DEC--TAN'
        if (status .ne. 0) goto 998

        call catnum(keywd, 'TCRVL' , x_sky_col)
        call ftgkye(iunit, keywd, crval1, comment, status)
        call catnum(keywd, 'TCRVL' , y_sky_col)
        call ftgkye(iunit, keywd, crval2, comment, status)
        contxt = ' cannot get TCRVLn'
        if (status .ne. 0) goto 998

        call catnum(keywd, 'TCRPX' , x_sky_col)
        call ftgkye(iunit, keywd, crpix1, comment, status)
        call catnum(keywd, 'TCRPX' , y_sky_col)
        call ftgkye(iunit, keywd, crpix2, comment, status)
        contxt = ' cannot get TCRPCn'
        if (status .ne. 0) goto 998

        call catnum(keywd, 'TCDLT' , x_sky_col)
        call ftgkye(iunit, keywd, cdelt1, comment, status)
        call catnum(keywd, 'TCDLT' , y_sky_col)
        call ftgkye(iunit, keywd, cdelt2, comment, status)
        contxt = ' cannot get TCDLTn'
        if (status .ne. 0) goto 998

        call catnum(keywd, 'TLMAX' , x_sky_col)
        call ftgkyj(iunit, keywd, axlen1, comment, status)
        call catnum(keywd, 'TLMAX' , y_sky_col)
        call ftgkyj(iunit, keywd, axlen2, comment, status)
        contxt = ' cannot get TLMAXn'
        if (status .ne. 0) goto 998

        call ftgkye(iunit, 'RA__MEAN', ramean, comment, status)
        call ftgkye(iunit, 'DEC_MEAN', decmean, comment, status)
        contxt = ' cannot get RA__MEAN/DEC_MEAN'
        if (status .eq. 202) then
           status = 0
           ramean = -999.0
           decmean = -999.0
        end if
        if (status .ne. 0) goto 998

        call ftgkye(iunit, 'RA_PNT', raavg, comment, status)
        call ftgkye(iunit, 'DEC_PNT', decavg, comment, status)
        contxt = ' cannot get RA_PNT/DEC_PNT'
        if (status .ne. 0) goto 998

        call ftgkye(iunit, 'PA_PNT', rollavg, comment, status)
        contxt = ' cannot get PA_PNT'
        if (status .ne. 0) goto 998

        call catnum(keywd, 'OPTIC' , x_det_col)
        call ftgkye(iunit, keywd, optical_x_axis, comment, status)
        call catnum(keywd, 'OPTIC' , y_det_col)
        call ftgkye(iunit, keywd, optical_y_axis, comment, status)
        contxt = ' cannot get DET OPTICn'
        if (status .ne. 0) goto 998

        call catnum(keywd, 'OPTIC' , x_sky_col)
        call ftgkye(iunit, keywd, optical_x_sky, comment, status)
        call catnum(keywd, 'OPTIC' , y_sky_col)
        call ftgkye(iunit, keywd, optical_y_sky, comment, status)
        contxt = ' cannot get SKY OPTICn'
        if (status .eq. 202) then
           status = 0
           optical_x_sky = -1.0
           optical_y_sky = -1.0
        end if
        if (status .ne. 0) goto 998

 999    continue

        return

 998    call fcerr(contxt)
        call ftclos (iunit, stat)

        end
