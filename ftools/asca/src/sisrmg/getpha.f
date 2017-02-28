*+GETPHA
	subroutine GETPHA( file, ints, dbls, mesg, status )

	character*( * )		file,mesg
	integer			ints(22)
	double precision	dbls(2)
        integer status
*
*	Interface to GET_PHA since cfortran.h is limited to 20 args.
*
*	Arguments:	see GET_PHA below.
*       Dependencies:	see GET_PHA below.
*       Author:		G. B. Crew, MIT, 1996 Feb
*-GETPHA

*
*	Parameters definitions have corresponding C #defines
*	generated automatically, so please preserve the style.
*
	integer	fi_lun, fi_inst, fi_chip, fi_rawx, fi_rawy
	integer fi_dmode, fi_cmode, fi_event_th, fi_split
	integer fi_gmask, fi_tr_grade, fi_zerodef, fi_pi
	integer fi_chan1, fi_n_chan, fi_bright, fi_chip_in
	integer fi_ascatime, fi_rddcv
* ints:
	parameter( fi_lun = 1 )
	parameter( fi_inst = 2 )
	parameter( fi_chip = 3 )
	parameter( fi_rawx = 4 )
	parameter( fi_rawy = 5 )
	parameter( fi_dmode = 6 )
	parameter( fi_cmode = 7 )
	parameter( fi_event_th = 8 )
	parameter( fi_split = 9 )
	parameter( fi_gmask = 10 )
	parameter( fi_tr_grade = 11 )
	parameter( fi_zerodef = 12 )
	parameter( fi_pi = 13 )
	parameter( fi_chan1 = 14 )
	parameter( fi_n_chan = 15 )
	parameter( fi_bright = 16 )
	parameter( fi_chip_in = 17 )
	parameter( fi_rddcv = 18 )
* dbls:
	parameter( fi_ascatime = 1 )

        call GET_PHA ( file, ints(fi_lun), ints(fi_chip_in),
     &                 ints(fi_inst), ints(fi_chip),
     &                 ints(fi_rawx), ints(fi_rawy),
     &                 ints(fi_dmode), ints(fi_cmode),
     &                 ints(fi_event_th), ints(fi_split),
     &                 ints(fi_gmask), ints(fi_tr_grade),
     &                 ints(fi_zerodef), ints(fi_pi),
     &                 ints(fi_chan1), ints(fi_n_chan),
     &                 ints(fi_bright), ints(fi_rddcv),
     &                 dbls(fi_ascatime),
     &                 mesg,
     &                 status )
	end


*+GET_PHA
        subroutine GET_PHA( file, lun, chip_in,
     &                      inst, chip,
     &                      rawx, rawy,
     &                      dmode, cmode,
     &                      event_th, split,
     &                      gmask, tr_grade,
     &                      zerodef, pi,
     &                      chan1, n_chan,
     &                      bright, rddcv,
     &                      ascatime,
     &                      context, status )

        implicit none

        integer m_col
        parameter( m_col = 64 )

        character*( * ) file, context
        integer lun, chip_in
        integer inst, chip, rawx, rawy
        integer dmode, cmode, event_th, split
        double precision ascatime
        integer gmask, tr_grade, zerodef
        integer pi, chan1, n_chan, bright, rddcv
        integer status
        integer xstatus

*       Reads PHA file headers and determines appropriate mode
*       parameters, to be used in SIS rmf generation
*
*       Arguments:
*         lun           (i) : Fortran logical unit number for FITSIO
*         file          (i) : PHA file name
*         chip_in       (i) : The chip number to assume, if known (0-3);
*                             -1 if unknown, to be determined from PHA file
*         inst          (r) : Instrument number, 0 or 1 for SIS-0 or SIS-1
*         chip          (r) : Chip number, 0-3
*         rawx, rawy    (r) : Raw coordinates of the center of WMAP
*         dmode         (r) : Datamode, a la 2 digit code in the file name
*                             2 for bright, 3 for fast or 12 for bright2
*                             (12 means echo-corrected)
*         cmode         (r) : Clocking mode, 1, 2 or 4
*         event_th      (r) : Event threshold or lower level discrimination
*                             setting for the chip, whichever is higher, in PHA
*         split         (r) : Split threshold
*         ascatime      (r) : Time of observation
*         gmask         (r) : Grade mask, 1 for g0, 5 for g02, 29 for g0234,
*                             93 for g02346 etc., however, see below
*         tr_grade      (r) : 1 if the above is to be trusted, 0 if not,
*                             in this humble routine's opinion
*         zerodef       (r) : 0 for the old definition of zero level,
*                             i.e., old faintdfe; 1 for the new definition;
*                             2 for one that mimicks on-board Bright processing
*         pi            (r) : 1 if the pha file contains PI spectrum, 0 if PHA
*         chan1, n_chan (r) : First channel (usually 1) and number of channels
*         bright        (r) : 1 if bright-mode binning, 0 if linear
*         rddcv         (r) : RDD correction version number, 0 otherwise
*         context       (r) : message if there is a problem
*         status        (r) : 0 if successfull
*
*       Dependencies:
*              FITSIO (Fortran) routines; FCERR, which is in ftools library
*              but requires xanlib then fitsio underneath
*
*       Author:
*              Koji Mukai, GSFC, 1996 Feb
*              Corrected 'un-rebinned Bright mode bug', KM, 2001 March
*-GET_PHA

        integer block, hd_type, ext_num, row_len, n_row
        integer maxdim, bitpix, naxis, naxes( 2 ), pcount, gcount
        integer wbinfac, ldiscri, chan_n, maxchan
        integer t_bcol( m_col ), t_field, k, ihdvrs
        logical simple, extend, prime_copy
        real xoff, yoff, detx, dety, timedel
        double precision tstart, tstop
        character(4) instr
        character(8) keynam, mode_s, chantype, grades, dfgrades, hduvers
        character(14) cmpmode
*       character(72) context
        character(80) comment
        character(80) t_type( m_col ), t_form( m_col ), t_unit( m_col )
        character(160) ext_nam

*       Open the file
        call FTOPEN( lun, file, 0, block, status )
        context = 'Error opening spectrum file'
        if( status .ne. 0 ) goto 900

*       Get required primary header keywords
        maxdim = 2
        call FTGHPR( lun, maxdim, simple, bitpix, naxis, naxes,
     &               pcount, gcount, extend, status )
        context = 'Error reading primary header'
        if( status .ne. 0 ) goto 900

*       Obtain other information from primary header keywords
        call FTGKYS( lun, 'TELESCOP', instr, comment, status )
        context = 'Cannot find TELESCOP keyword'
        if( status .ne. 0 ) goto 900
        if( instr .ne. 'ASCA' ) then
          context = 'File does not contain ASCA data'
          goto 900
        end if

        call FTGKYS( lun, 'INSTRUME', instr, comment, status )
        context = 'Cannot find INSTRUME keyword'
        if( status .ne. 0 ) goto 900
        if( instr .eq. 'SIS0' ) then
          inst = 0
        else if( instr .eq. 'SIS1' ) then
          inst = 1
        else
          context = 'File does not contain SIS data'
          goto 900
        end if

*
*  The following section computes the rawx,rawy center of the WMAP
*  It should really try to figure out where most of the photons are.
*
        CALL ftgkys(lun,'HDUVERS',hduvers,comment,status)
        IF ( status .NE. 0 ) THEN
           status = 0
           CALL ftgkys(lun,'HDUVERS1',hduvers,comment,status)
        ENDIF
        IF ( status .NE. 0 ) THEN
           ihdvrs = 0
           status = 0
        ELSE
           READ(hduvers(1:1),'(i1)') ihdvrs
        ENDIF
        IF ( ihdvrs .LT. 2 ) THEN
           call FTGKYJ( lun, 'WMREBIN', wbinfac, comment, status )
           context = 'Cannot find WMREBIN keyword'
        ELSE
           call FTGKYJ( lun, 'CDELT1P', wbinfac, comment, status )
           context = 'Cannot find CDELT1P keyword'
        ENDIF
        if( status .ne. 0 ) goto 900
        IF ( ihdvrs .LT. 2 ) THEN
           call FTGKYE( lun, 'CRVAL1', xoff, comment, status )
           context = 'Cannot find CRVAL1 keyword'
        ELSE
           call FTGKYE( lun, 'CRVAL1P', xoff, comment, status )
           context = 'Cannot find CRVAL1P keyword'
        ENDIF
        if( status .ne. 0 ) goto 900
        IF ( ihdvrs .LT. 2 ) THEN
           call FTGKYE( lun, 'CRVAL2', yoff, comment, status )
           context = 'Cannot find CRVAL2 keyword'
        ELSE
           call FTGKYE( lun, 'CRVAL2P', yoff, comment, status )
           context = 'Cannot find CRVAL2P keyword'
        ENDIF
        if( status .ne. 0 ) goto 900
        IF ( ihdvrs .LT. 2 ) THEN
          detx = ( xoff + naxes( 1 ) * 0.5 ) * wbinfac - 0.5
          dety = ( yoff + naxes( 2 ) * 0.5 ) * wbinfac - 0.5
	ELSE
          detx = xoff + ( ( naxes( 1 ) - 1 ) * 0.5 ) * wbinfac + 0.5
          dety = yoff + ( ( naxes( 2 ) - 1 ) * 0.5 ) * wbinfac + 0.5
	ENDIF

*       Approximate conversion back from detx, dety to chip, rawx, rawy
        if( inst .eq. 0 ) then
          if( detx .lt. 640.5 .and. dety .gt. 640.5 ) then
            chip = 0
            rawx = int( 637.2 - ( 1281.0 - dety ) )
            rawy = int( 1067.0 - ( 1281.0 - detx ) )
          else if( detx .lt. 640.5 .and. dety .lt. 640.5 ) then
            chip = 1
            rawx = int( 1081.0 - ( 1281.0 - dety ) )
            rawy = int( 1068.6 - ( 1281.0 - detx ) )
          else if( detx .gt. 640.5 .and. dety .lt. 640.5 ) then
            chip = 2
            rawx = int( -644.3 + ( 1281.0 - dety ) )
            rawy = int( -212.5 + ( 1281.0 - detx ) )
          else if( detx .gt. 640.5 .and. dety .gt. 640.5 ) then
            chip = 3
            rawx = int( -200.3 + ( 1281.0 - dety ) )
            rawy = int( -214.4 + ( 1281.0 - detx ) )
          end if
        else
          if( detx .gt. 640.5 .and. dety .lt. 640.5 ) then
            chip = 0
            rawx = int(  637.2 - dety )
            rawy = int( 1067.9 - detx )
          else if( detx .gt. 640.5 .and. dety .gt. 640.5 ) then
            chip = 1
            rawx = int( 1077.6 - dety )
            rawy = int( 1066.4 - detx )
          else if( detx .lt. 640.5 .and. dety .gt. 640.5 ) then
            chip = 2
            rawx = int( -647.6 + dety )
            rawy = int( -214.7 + detx )
          else if( detx .lt. 640.5 .and. dety .lt. 640.5 ) then
            chip = 3
            rawx = int( -203.2 + dety )
            rawy = int( -213.2 + detx )
          end if
        end if
	if( chip_in .ge. 0 ) chip = chip_in

        call FTGKYS( lun, 'DATAMODE', mode_s, comment, status )
        context = 'Cannot get DATAMODE keyword'
        if( status .ne. 0 ) goto 900
        if( mode_s .eq. 'BRIGHT2' ) then
          dmode = 12
*	  default grades 0234
          dfgrades = '00011101'
        else if( mode_s .eq. 'BRIGHT' ) then
          dmode = 02
*	  default grades 0234
          dfgrades = '00011101'
        else if( mode_s .eq. 'FAST' ) then
          dmode = 03
*	  default grades 0
          dfgrades = '00000001'
        else
          context = 'Unknown SIS data mode'
          goto 900
        end if

        write( keynam, '(''S'',i1,''_DESTA'')' ) inst
        call FTGKYJ( lun, keynam, cmode, comment, status )
        if( status .eq. 0 ) then
          prime_copy = .true.
        else if( status .eq. 202 ) then
          prime_copy = .false.
          status = 0
        else
          context = 'Failed to read Sn_DESTA keyword'
          goto 900
        end if

        if( dmode .eq. 3 ) then
*         Fast mode, use cmode of 1 even though FAST mode is different
          cmode = 1
        else
          if( prime_copy ) then
            write( keynam, '(''S'',i1,''CCDMOD'')' ) inst
            call FTGKYJ( lun, keynam, cmode, comment, status )
            context = 'Error reading SnCCDMOD keyword'
            if( status .ne. 0 ) goto 900
          else
            call FTGKYE( lun, 'TIMEDEL', timedel, comment, status )
            context = 'Error reading TIMEDEL keyword'
            if( status .ne. 0 ) goto 900
            if( timedel .gt. 3.99 .and. timedel .lt. 4.01 ) then
              cmode = 1
            else if( timedel .gt. 7.99 .and. timedel .lt. 8.01 ) then
              cmode = 2
            else if( timedel .gt. 15.99 .and. timedel .lt. 16.01 ) then
              cmode = 4
            end if
          end if
        end if

        call FTGKYD( lun, 'TSTART', tstart, comment, status )
        call FTGKYD( lun, 'TSTOP',  tstop,  comment, status )
        context = 'Cannot get TSTART or TSTOP'
        if( status .ne. 0 ) goto 900
        ascatime = ( tstart + tstop ) * 5.0D-01

        if( prime_copy ) then
          write( keynam, '(''S'',i1,''_EVTR'',i1)' ) inst, chip
          call FTGKYJ( lun, keynam, event_th, comment, status )
          context = 'Cannot get Sn_EVTRm keyword'
          if( status .ne. 0 ) goto 900
          write( keynam, '(''S'',i1,''_LVENA'')' ) inst
          call FTGKYJ( lun, keynam, ldiscri, comment, status )
          context = 'Cannot get Sn_LVENA keyword'
          if( status .ne. 0 ) goto 900
          if( ldiscri .eq. 1 ) then
*           Level discri on
            write( keynam, '(''S'',i1,''_LVDL'',i1)' ) inst, chip
            call FTGKYJ( lun, keynam, ldiscri, comment, status )
            context = 'Cannot get Sn_LVDLm keyword'
            if( status .ne. 0 ) goto 900
            event_th = max( event_th, ldiscri )
          end if
        else
*         Make a guess
          event_th = 100
        end if

        call FTGKYJ( lun, 'SPLIT_TH', split, comment, status )
        context = 'Failed to read SPLIT_TH value'
        if( status .eq. 202 ) then
*         couldn't find SPLIT_TH, try Sn_SPTRm keywords
          write( keynam, '(''S'',i1,''_SPTR'',i1)' ) inst, chip
          status = 0
          call FTGKYJ( lun, keynam, split, comment, status )
          context = 'Cannot get Sn_SPTRm keyword'
          if( status .ne. 0 ) goto 900
        else if( status .ne. 0 ) then
          goto 900
        end if

        if( prime_copy ) then
          write( keynam, '(''S'',i1,''_GRADE'')' ) inst
          call FTGKYS( lun, keynam, grades, comment, status )
          context = 'Failed to read Sn_GRADE keyword'
          if( status .ne. 0 ) goto 900
          if( index( comment, 'XSELECT' ) .ne. 0 ) then
            tr_grade = 1
          else
            tr_grade = 0
            grades = dfgrades
          end if
        else
          tr_grade = 0
          grades = dfgrades
        end if
        gmask = 0
        do k = 1, 8
          gmask = gmask * 2
          if( grades( k: k ) .eq. '1' ) then
            gmask = gmask + 1
          end if
        end do

        call FTGKYJ( lun, 'ZERODEF', zerodef, comment, status )
        context = 'Failed to read ZERODEF keyword'
        if( status .eq. 202 ) then
*         ZERODEF keyword not found, which means old faintdfe.
          if ( dmode .eq. 2 ) then
*           this is a BRIGHT mode file...
	    zerodef = 2
	  else if ( dmode .eq. 3 ) then
*           this is a FAST mode file...
	    zerodef = 3
	  else
            zerodef = 0
	  end if
          status = 0
        else if( status .ne. 0 ) then
          goto 900
        end if

*       Get rdd correction code version number, 0 == uncorrected
        call FTGKYJ( lun, 'RDDCOR_V', rddcv, comment, status )
        context = 'Failed to read RDDCOR_V keyword'
        if( status .eq. 202 ) then
*         RDDCOR_V keyword not found, which means not RDD corrected
          rddcv = 0
          status = 0
        else if( status .ne. 0 ) then
          goto 900
        end if


*       Now let's try going to the extension header
*       THIS ASSUMES SPECTRUM IN EXTENSION 1
        ext_num = 1
        call FTMRHD( lun, ext_num, hd_type, status )
        context = 'Error moving to requested extension'
        if( status .ne. 0 ) goto 900
        if( hd_type .eq. 1 ) then
          call FTGHTB( lun, m_col, row_len, n_row, t_field,
     &                 t_type, t_bcol, t_form, t_unit, ext_nam, status )
        else if( hd_type .eq. 2 ) then
          call FTGHBN( lun, m_col, n_row, t_field, t_type,
     &                         t_form, t_unit, ext_nam, pcount, status )
        else
          context = 'File extension type not supported'
          goto 900
        end if

        call FTGKYS( lun, 'CHANTYPE', chantype, comment, status )
        context = 'Failed to get CHANTYPE keyword'
        if( status .ne. 0 ) goto 900
        if( chantype .eq. 'PI      ' ) then
          pi = 1
        else if( chantype .eq. 'PHA     ' ) then
          pi = 0
        else
          context = 'Channel type unknown'
          goto 900
        end if

        call FTGCNO( lun, .false., 'CHANNEL', chan_n, status )
        context = 'Channel colum not found'
        if( status .ne. 0 ) goto 900
        write( keynam, '(''TLMIN'',i1)' ) chan_n
        call FTGKYJ( lun, keynam, chan1, comment, status )
        context = 'Failed to get TLMIN keyword'
        if( status .ne. 0 ) goto 900
        write( keynam, '(''TLMAX'',i1)' ) chan_n
        call FTGKYJ( lun, keynam, maxchan, comment, status )
        context = 'Failed to get TLMAX keyword'
        if( status .ne. 0 ) goto 900
        n_chan = maxchan - chan1 + 1

        call FTGKYS( lun, 'CMPMODE', cmpmode, comment, status )
        context = 'Failed to get CMPMODE keyword'
        if( status .eq. 202 ) then
*         CMPMODE keyword not found.
*         Previous version assumed bright=0, leading to 'unrebinned
*         Bright mode' bug --- corrected by KM, 2001 March
	  if( dmode .eq. 12 ) then
*           Bright2 mode data, saved without rebinning
            bright = 0
	  else
*           Bright or Fast mode data, saved without rebinning
	    bright = 1
	  end if
          status = 0
        else if( status .ne. 0 ) then
          goto 900
        else
          if( dmode .eq. 12 ) then
            if( cmpmode .eq. 'BRIGHT2LINEAR' ) then
              context = 'CMPMODE eq BRIGHT2LINEAR'
              goto 900
            else if( cmpmode .eq. 'LINEAR' ) then
              bright = 0
            else
              context = 'CMPMODE ne LINEAR'
              goto 900
            end if
          else
            if( cmpmode .eq. 'BRIGHT2LINEAR' ) then
              bright = 0
            else if( cmpmode .eq. 'LINEAR' ) then
              bright = 1
            else
              context = 'CMPMODE value unknown'
              goto 900
            end if
          end if
        end if

	call FTCLOS(lun, status)
	context = 'Error closing file'
	if( status .eq. 0) then
	  context = ' '
	end if
        return

*       return with error message

 900    continue
*       call FCERR( context )

*	we don't care about any error from this:
	call FTCLOS(lun, xstatus)

        end
