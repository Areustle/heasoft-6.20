c fitsread.f
c         T.Takahashi 1993 8 16
c         read science fits file for ASCA_ANL
c
c         8/21 SIS, GIS(X,Y,RT)             H.Kubo
c         8/23 GIS MPC mode                 Y. Sekimoto
c         8/29 SIS Fast mode                Y. Sekimoto
c        11/9  ftmrhd xtensn = 1            H.Kubo   (fcpars changed ?)
c        11/10 FITSREAD:ENTRY FITSREAD:ACCEPT FITSREAD:OK added by H.Kubo
c        11/18 EVS(FITSREAD:BEGIN,ENTRY,OK) H.Kubo
c        12/2  change system flag           H.Kubo
c        12/5  GTI                          H.Kubo
c        12/6  Pseudo Event Generator       H.Kubo
c        12/10 include asca_anl.inc         H.Kubo
c        12/13 remove ENDRUN flag           H.Kubo
c         2/9  BRIGHT2 mode                 H.Kubo
c  1994   3/29 RT, RTI Real->Integer        H.Kubo
c              BNK('ANL:SENSOR')            H.Kubo
c         3/30 BNK('ANL:RAWX') etc          H.Kubo
c         5/25 Keyword Check changed        H.Kubo
c         6/28 ANL_initroot,
c                  ANL:FILENAME introduced  H.Kubo
c         6/28 FITSREAD:FILENAME 132->256   H.Kubo
c         7/15 FITSREAD_read_data           H.Kubo
c              fitsread_local.inc           H.Kubo
c         7/18 ANL_getlun called only once  H.Kubo
c         9/13 ANL:SKYREF                   H.Kubo
c         9/19 ANL:EULER                    H.Kubo
c         9/22 ANL:EULER flag               H.Kubo      version 0.71
c        10/11 SIS:SIS0 Pseudo, SIS:SIS1 Pseudo,
c              GIS:GIS2 Pseudo, GIS:GIS3 Pseudo
c                              introduced   H.Kubo      version 0.72
c        11/4  write_GTI format changed     H.Kubo      version 0.73
c 1995   04/20 set length of ANL:FILENAME   Y.Ishisaki  version 0.74
c 2006   06/08 small modifications for g95  Y.Ishisaki  version 0.74.1
c
c -------------
c init routine
c -------------
      Subroutine FITSREAD_init (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
      Include 'fitsread_local.inc'
c local
      Integer i, lunin
      Integer nwords
      Character*10 words(2)
c vesion
      Call ANL_put_version('FITSREAD','version 0.74')
c Common BNK/EVS
      Call ANL_initroot(i)
c EVS Definition
      Call EVSdef('FITSREAD:BEGIN')
      Call EVSdef('FITSREAD:ENTRY')
      Call EVSdef('FITSREAD:OK')
c (Event Generator)
      Call EVSdef('FITSREAD:PSEUDO')
      Call EVSdef('FITSREAD:OBS')
c BNK Definition
c (filename)
      Call BNKdef('FITSREAD:FILENAME',256)
c (GTI)
      Call BNKdef('FITSREAD:STDGTI_NUM',4)
      Call BNKdef('FITSREAD:ALLGTI_NUM',4)
      Call BNKdef('FITSREAD:STDGTI_START',8*MAXROW)
      Call BNKdef('FITSREAD:STDGTI_STOP',8*MAXROW)
      Call BNKdef('FITSREAD:ALLGTI_START',8*MAXROW)
      Call BNKdef('FITSREAD:ALLGTI_STOP',8*MAXROW)
c
c tell logical unit number to other routines
      Call ANL_getlun(lunin)
      Call FITSREAD_ana_lun (lunin)
      Call FITSREAD_bgnrun_ini (lunin)
      Call FITSREAD_endrun_ini (lunin)
      Call FITSREAD_exit_ini (lunin)
c
c TTYPE & Instrument & TFORM
c keyword(1,*)=TTYPE, keyword(2,*)=Instrument, keyword(3,*)=BNK size
      keyword(1,1) = 'RAWX'
      keyword(2,1) = 'ANL'
      keyword(3,1) = '4'
      keyword(1,2) = 'RAWY'
      keyword(2,2) = 'ANL'
      keyword(3,2) = '4'
      keyword(1,3) = 'DETX'
      keyword(2,3) = 'ANL'
      keyword(3,3) = '4'
      keyword(1,4) = 'DETY'
      keyword(2,4) = 'ANL'
      keyword(3,4) = '4'
      keyword(1,5) = 'X'
      keyword(2,5) = 'ANL'
      keyword(3,5) = '4'
      keyword(1,6) = 'Y'
      keyword(2,6) = 'ANL'
      keyword(3,6) = '4'
      keyword(1,7) = 'TIME'
      keyword(2,7) = 'ANL'
      keyword(3,7) = '8'
      keyword(1,8) = 'PHA'
      keyword(2,8) = 'ANL'
      keyword(3,8) = '4'
      keyword(1,9) = 'PHAS'
      keyword(2,9) = 'SIS/GIS'
      keyword(3,9) = '36/1024'
      keyword(1,10) = 'PI'
      keyword(2,10) = 'ANL'
      keyword(3,10) = '4'
      keyword(1,11) = 'RISE_TIME'
      keyword(2,11) = 'GIS'
      keyword(3,11) = '4'
      keyword(1,12) = 'RTI'
      keyword(2,12) = 'GIS'
      keyword(3,12) = '4'
      keyword(1,13) = 'SPREAD'
      keyword(2,13) = 'GIS'
      keyword(3,13) = '4'
      keyword(1,14) = 'CCDID'
      keyword(2,14) = 'SIS'
      keyword(3,14) = '4'
      keyword(1,15) = 'GRADE'
      keyword(2,15) = 'SIS'
      keyword(3,15) = '4'
      keyword(1,16) = 'END'
c
      Do i=1,MAXWORD
        If (keyword(1,i).eq.'END') Goto 10
        If (keyword(2,i).eq.'SIS/GIS') then
          Call CLword(keyword(3,i),'/',nwords,words)
c          Call FITSREAD_BNKdef(keyword(1,i),'SIS',words(1))
c          Call FITSREAD_BNKdef(keyword(1,i),'GIS',words(2))
        else
c          Call FITSREAD_BNKdef(keyword(1,i),keyword(2,i),keyword(3,i))
        Endif
      Enddo
 10   Continue
      keyword_num = i-1
c      write(*,*) ' keyword_num', keyword_num
c      Call BNKlst
c
      Call FITSREAD_show_parameter
c
      status = ASCA_ANL_OK
c
      Return
      End
c
c ------------------------
c  FITSREAD_show_parameter
c ------------------------
      Subroutine FITSREAD_show_parameter
      Implicit NONE
c include
      Include 'Includes.inc'
      Include 'fitsread_local.inc'
c
      Write(*,*)
      Write(*,*) 'ANL:  *** FITSREAD show parameter ***'
      Write(*,*)
 10   Format(4X, A16, F10.4, A6)
      Write(*,10) 'PSEUDO_INTERVAL', evgen_dt, ' (sec)'
 20   Format(4X, A16, 4X, A)
      If ( euler_flag ) then
         Write(*,20) 'EULER', 'ENA'
      Else
         Write(*,20) 'EULER', 'DIS'
      End if
c
      Return
      End
c
c ----------
c  FITSREAD_change_parameter
c ----------
      Subroutine FITSREAD_change_parameter
      Implicit NONE
c include
      Include 'Includes.inc'
      Include 'fitsread_local.inc'
c const:
      Integer NTABLE
      Parameter (NTABLE=4)
c local
      Character*16 table(NTABLE)
      Data table /
     &     'SHOW', 'PSEUDO_INTERVAL', 'EULER', 'EXIT'
     & /
      Integer ans(0:NTABLE)
c
 10   Continue
      Call INQUIRE('*** FITSREAD option ***'
     &     , NTABLE, table, table, 1, ans)
      If ( 'SHOW' .eq. table(ans(1)) ) then
         Call FITSREAD_show_parameter
      Else if ( 'PSEUDO_INTERVAL' .eq. table(ans(1)) ) then
         Call FDPRD('Interval of pseudo event in sec (0=none)',
     &        evgen_dt)
      Else if ( 'EULER' .eq. table(ans(1)) ) then
         Call LOGRD('Use Euler angle (ANL:EULER)', euler_flag)
      Else if ( 'EXIT' .eq. table(ans(1)) ) then
         Return
      End if
      Goto 10
c
      End
c
c -----------
c com dialog
c -----------
      Subroutine FITSREAD_com (status)
      Implicit NONE
c output:
      Integer status
c include
      Include 'Includes.inc'
c
      Call FITSREAD_change_parameter
      status = ASCA_ANL_OK
      Return
      End
c
c ---------------------
c histogram definition
c ---------------------
      Subroutine FITSREAD_his (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c
      status = ASCA_ANL_OK
      Return
      End
c
c ----------
c begin run
c ----------
      Subroutine FITSREAD_bgnrun(status)
      Implicit NONE
c output:
      Integer status
c include
      Include 'Includes.inc'
c local common
      Include 'fitsread_local.inc'
c const:
      Integer RWMODE
      Parameter (RWMODE=0)
c entry
      Integer lun
c constant
      Integer maxcl
      Parameter(maxcl=512)
c local:
      Integer lunin
      Save lunin
      Integer block
      Character*132 data_file
      Save data_file
      Data data_file /'frf.fits'/
      Integer xtensn
      Integer hdutype
      Integer nrecords,tfield,vardat
      Character*80 ttype(maxcl), tform(maxcl), tunit(maxcl)
      Character*80 extnam
      Character*15 instrument, datamode
      Character*80 contxt
      Integer pha_bins
      Real*8  skyref(3)
      Character*6 tctyp_keyword(3)
      Data        tctyp_keyword /'TCTYP2','TCTYP3','TCTYP4'/
      Character*6 tcrvl_keyword(3)
      Data        tcrvl_keyword /'TCRVL2','TCRVL3','TCRVL4'/
      Character*8 tctyp
      Logical skyx_ref, skyy_ref
      Integer tc_status
      Integer i, len
c
c GTI
      Integer stdgti_num, allgti_num
c function
      Integer Lenrd
c (1) EVS set
      Call EVSset('FITSREAD:BEGIN')
c (2) open fits science file
      Call TXTrd('Input fits filename', data_file)
      len = Lenrd(data_file)
      Call ftopen(lunin, data_file(:len), RWMODE, block, status)
      If (status.eq.0) then
         Write (*,*) ' successfully opened'
         Call BNKput('FITSREAD:FILENAME', len, data_file(:len))
         Call BNKput('ANL:FILENAME', len, data_file(:len))
      else
         Write(*,*) ' FITSREAD:Open error'
         status = ASCA_ANL_QUIT
         Return
      Endif
c
c (3) access header info
      xtensn= 1
      Call ftmrhd(lunin, xtensn, hdutype, status)
      If (status.ne.0) then
         Write(*,*) ' FITSREAD:ftmrhd error', status
         status = ASCA_ANL_QUIT
         Return
      Endif

      If (hdutype.eq.2) then
         Call ftghbn(lunin, maxcl, nrecords, tfield, ttype,
     &        tform, tunit, extnam, vardat, status)
         fits_nrecords = nrecords ! common var.
         Write(*,*) 'number of rows ',nrecords
         Call ftgkys(lunin,'INSTRUME',instrument,contxt,status)
         Write(*,*) 'Instrument : ',instrument
         Call ftgkys(lunin,'DATAMODE',datamode,contxt,status)
         Write(*,*) 'Data Mode : ',datamode
         Call ftgkys(lunin,'BIT_RATE',bit_rate,contxt,status)
         Write(*,*) 'Bit Rate : ', bit_rate
         Call ftgkyd(lunin,'TSTART', tstart, contxt, status)
         Call ftgkyd(lunin,'TSTOP', tstop, contxt, status)
      else
         Write(*,*) ' FITSREAD:hdutype error'
         status = ASCA_ANL_QUIT
         Return
      Endif
c
c (4) get sky coordinate at reference pixel
      skyx_ref = .FALSE.
      skyy_ref = .FALSE.
      Do i=1,3
        skyref(i) = 0.0d0
      Enddo
      Do i=1,3
        tc_status = 0
        Call ftgkys(lunin, tctyp_keyword(i), tctyp, contxt, tc_status)
        If (tc_status.eq.0) then
          If (tctyp.eq.'RA---TAN') then
            Call ftgkyd(lunin,tcrvl_keyword(i),skyref(1),contxt,status)
            If (status.eq.0) skyx_ref = .TRUE.
          Else if (tctyp.eq.'DEC--TAN') then
            Call ftgkyd(lunin,tcrvl_keyword(i),skyref(2),contxt,status)
            If (status.eq.0) skyy_ref = .TRUE.
          Endif
        Endif
      Enddo
      If ((skyx_ref).and.(skyy_ref)) then
        skyref(3) = 0.0d0
        Call BNKput('ANL:SKYREF', 8*3, skyref)
      else
        Write(*,*) 'FITSREAD: Can not get TCRVL',
     &              '(sky coordinate at reference pixel)'
      Endif
c
c (5) number of bins for PHA of GIS
      If ((instrument.eq.'GIS2').or.(instrument.eq.'GIS3')) then
         Call ftgkyj(lunin,'PHA_BINS',pha_bins,contxt,status)
         If ( status .ne. 0 ) then
            Write(*,*) 'FITSREAD: Can not get PHA_BINS (status=',
     &           status, ')'
            status = 0
         Else
            Write(*,*) 'PHA bins : ',pha_bins
         End if
      Endif
c
c (5) obtain column number for each keyword
        If ((instrument.eq.'SIS0').or.(instrument.eq.'SIS1').or.
     &      (instrument.eq.'GIS2').or.(instrument.eq.'GIS3')) then
          Call FITSREAD_posget(lunin,tform,status)
        else
          status = ASCA_ANL_QUIT
          Write(*,*) 'FITSREAD: Can not get SIS/GIS event data'
          Return
        Endif
c
c (6) read GTI
        Call FITSREAD_read_GTI(lunin,stdgti_num,allgti_num,status)
        status = 0
c (7) event generator
      If (evgen_dt.gt.0) then
        If (stdgti_num.eq.0) then
          Write(*,*)
     &     'There is not STDGTI, so pseudo event cat not be generated'
        else
          Call FITSREAD_write_GTI
          Call BNKput('ANL:PSEUDO_INTERVAL', 8, evgen_dt)
        Endif
      Endif
c
c (8) pass instrument and datamode to "_ana" entry
        Call FITSREAD_ana_ini (instrument, datamode)
c
      status = ASCA_ANL_OK
      Return
c
      Entry FITSREAD_bgnrun_ini(lun)
      lunin = lun
      Return
      End
c
c ---------------
c FITSREAD_posget
c ---------------
      Subroutine FITSREAD_posget(lunin, tform, status)
      Implicit NONE
c input
      Integer lunin
      Character*(*) tform(*)
c output
      Integer status
c local common
      Include 'fitsread_local.inc'
c const
      Logical EXACT
      Parameter (EXACT=.true.)
c local
      Integer i, pos
c main
      entry_keyword = 0
      Do i=1, keyword_num
        status = 0
        Call ftgcno(lunin, EXACT, keyword(1,i), pos, status)
        If (status.eq.0) then
          entry_keyword = entry_keyword + 1
          keyword_pos(1,entry_keyword) = i
          keyword_pos(2,entry_keyword) = pos
          keyword(4,i) = tform(pos)
          If (keyword(1,i).eq.'TIME') pos_time = pos
        Endif
      Enddo
      status = 0
      Return
      End
c
c -----------
c display GTI
c -----------
      Subroutine FITSREAD_write_GTI
      Implicit None
c include
      Include 'fitsread_local.inc'
c local
      Integer size
      Integer stdgti_num
      Real*8 stdgti_start(MAXROW), stdgti_stop(MAXROW)
      Integer i
c
      Call BNKget('FITSREAD:STDGTI_NUM', 4, size, stdgti_num)
      Call BNKget('FITSREAD:STDGTI_START',
     &             8*stdgti_num, size, stdgti_start)
      Call BNKget('FITSREAD:STDGTI_STOP',
     &             8*stdgti_num, size, stdgti_stop)
      Write(*,'(5X,A6,8X,A5,17X,A4)') 'STDGTI','START','STOP'
      Do i=1, stdgti_num
        Write(*,'(5X,I3,3X,E20.12,X,E20.12)') i,
     &        stdgti_start(i),stdgti_stop(i)
      Enddo
      Return
      End
c
c ----------
c  Read GTI
c ----------
      Subroutine FITSREAD_read_GTI(lunin,stdgti_num,allgti_num,status)
      Implicit None
c input
      Integer lunin
c output
      Integer status
      Integer stdgti_num, allgti_num
c include
      Include 'fitsread_local.inc'
c const
      Logical EXACT
      Parameter (EXACT = .true.)
c local
      Integer i
      Integer xtensn
      Integer hdutype
      Integer nrows
      Integer tfields
      Character*80 ttype(MAXWORD), tform(MAXWORD), tunit(MAXWORD)
      Character*80 extname
      Integer      varidat
      Integer pos_start, pos_stop
      Real*8 nullval
      Real*8 dval
      Logical anyf
      Real*8  stdgti_start(MAXROW), stdgti_stop(MAXROW)
      Real*8  allgti_start(MAXROW), allgti_stop(MAXROW)
c STDGTI
c move to next HDU
      xtensn= 1
      Call ftmrhd(lunin, xtensn, hdutype, status)
      Call ftghbn(lunin, MAXWORD, nrows, tfields, ttype,
     &            tform, tunit, extname, varidat, status)
c if next HDU does not exist
      If (status.ne.0) then
        stdgti_num = 0
        allgti_num = 0
        Call BNKput('FITSREAD:STDGTI_NUM', 4, stdgti_num)
        Call BNKput('FITSREAD:ALLGTI_NUM', 4, allgti_num)
        Write(*,*) ' FTISREAD: Can not get STDGTI (status=', status, ')'
        Write(*,*) ' FTISREAD: use TSTART and TSTOP keywords'
        stdgti_num = 1
        Call BNKput('FITSREAD:STDGTI_NUM', 4, stdgti_num)
        stdgti_start(1) = tstart
        stdgti_stop(1) = tstop
        Call BNKput('FITSREAD:STDGTI_START',8*stdgti_num, stdgti_start)
        Call BNKput('FITSREAD:STDGTI_STOP',8*stdgti_num, stdgti_stop)
        allgti_num = 0
        Call BNKput('FITSREAD:ALLGTI_NUM', 4, allgti_num)
        Return
      Endif
c read header information
      If (extname.eq.'STDGTI') then
        stdgti_num = nrows
c        Write(*,*) ' STDGTI number',stdgti_num
        Call BNKput('FITSREAD:STDGTI_NUM', 4, stdgti_num)
        Call ftgcno(lunin, EXACT, 'START', pos_start, status)
        Call ftgcno(lunin, EXACT, 'STOP', pos_stop, status)
c read STDGTI
        Do i=1, nrows
          Call ftgcvd(lunin, pos_start, i, 1, 1, nullval,
     &                dval, anyf, status)
          stdgti_start(i) = dval
c          Write(*,*) ' STDGTI start ',i,' ',dval
          Call ftgcvd(lunin, pos_stop, i, 1, 1, nullval,
     &                dval, anyf, status)
          stdgti_stop(i) = dval
c          Write(*,*) ' STDGTI stop  ',i,' ',dval
        Enddo
        Call BNKput('FITSREAD:STDGTI_START',8*stdgti_num, stdgti_start)
        Call BNKput('FITSREAD:STDGTI_STOP',8*stdgti_num, stdgti_stop)
      else
        stdgti_num = 0
        Call BNKput('FITSREAD:STDGTI_NUM', 4, stdgti_num)
        Write(*,*) 'FITSREAD: Can not get STDGTI'
      Endif
c ALLGTI
c move to next HDU
      xtensn= 1
      Call ftmrhd(lunin, xtensn, hdutype, status)
      Call ftghbn(lunin, MAXWORD, nrows, tfields, ttype,
     &            tform, tunit, extname, varidat, status)
c if next HDU does not exist
      If (status.ne.0) then
        Write(*,*) 'FITSREAD: Can not get ALLGTI'
        xtensn = -1
        status = 0
        Call ftmrhd(lunin, xtensn, hdutype, status)
        allgti_num = 0
        Call BNKput('FITSREAD:ALLGTI_NUM', 4, allgti_num)
        Return
      Endif
c read header information
      If (extname.eq.'ALLGTI') then
        allgti_num = nrows
c        Write(*,*) ' ALLGTI number ',allgti_num
        Call BNKput('FITSREAD:ALLGTI_NUM', 4, allgti_num)
        Call ftgcno(lunin, EXACT, 'START', pos_start, status)
        Call ftgcno(lunin, EXACT, 'STOP', pos_stop, status)
c read ALLGTI
        Do i=1, nrows
          Call ftgcvd(lunin, pos_start, i, 1, 1, nullval,
     &                dval, anyf, status)
          allgti_start(i) = dval
c          Write(*,*) ' ALLGTI start ',i,' ',dval
          Call ftgcvd(lunin, pos_stop, i, 1, 1, nullval,
     &                dval, anyf, status)
          allgti_stop(i) = dval
c          Write(*,*) ' ALLGTI stop ',i,' ',dval
        Enddo
        Call BNKput('FITSREAD:ALLGTI_START',8*allgti_num, allgti_start)
        Call BNKput('FITSREAD:ALLGTI_STOP',8*allgti_num, allgti_stop)
      else
        Write(*,*) 'FITSREAD: Can not get ALLGTI'
        allgti_num = 0
        Call BNKput('FITSREAD:ALLGTI_NUM', 4, allgti_num)
      Endif
c move to event's HDU
      xtensn =-2
      Call ftmrhd(lunin, xtensn, hdutype, status)
      Return
      End
c
c ------------
c  ana routine
c ------------
      Subroutine FITSREAD_ana(nevent, eventid, status)
      Implicit NONE
c input:
      Integer nevent
c output:
      Integer eventid
      Integer status
c include
      Include 'Includes.inc'
c local common
      Include 'fitsread_local.inc'
c input for entry
      Integer lun
c local:
      Integer lunin
      Save lunin
      Character*15 instrument, instr
      Character*15 datamode, d_mode
      Save instrument, datamode
      Logical first
      Save    first
      Integer obs_num
      Save    obs_num
      Data    obs_num/0/
      Logical data_end
      Save    data_end
      Data    data_end/.false./
      Integer size
      Real*8  ascatime
      Real*8  euler(3)
c function:
ccc delete ascatool dependence 1998/04/13      Integer ascaEulerAng
c
      Call EVSset('FITSREAD:ENTRY')
c
      If (instrument.eq.'SIS0') then
        Call BNKput('ANL:SENSOR',4,SENSOR_S0)
      else if (instrument.eq.'SIS1') then
        Call BNKput('ANL:SENSOR',4,SENSOR_S1)
      else if (instrument.eq.'GIS2') then
        Call BNKput('ANL:SENSOR',4,SENSOR_S2)
      else if (instrument.eq.'GIS3') then
        Call BNKput('ANL:SENSOR',4,SENSOR_S3)
      Endif
c
      If (datamode.eq.'PH') then
         Call EVSset('GIS:PH mode')
      Else if (datamode.eq.'MPC') then
         Call EVSset('GIS:MPC mode')
      Else if (datamode.eq.'BRIGHT') then
         Call EVSset('SIS:BRIGHT mode')
      Else if (datamode.eq.'BRIGHT2') then
         Call EVSset('SIS:BRIGHT2 mode')
      Else if (datamode.eq.'FAINT') then
         Call EVSset('SIS:FAINT mode')
      Else if (datamode.eq.'FAST') then
         Call EVSset('SIS:FAST mode')
      Endif
c
      If (evgen_dt.gt.0) then
        If (obs_num.gt.fits_nrecords) then
          status = ASCA_ANL_QUIT
          Return
        Endif
        Call FITSREAD_event_gen(first, eventid, lunin, instrument,
     &                          datamode, obs_num, data_end, status)
        If (data_end) then
           status = ASCA_ANL_QUIT
           data_end = .false.
           return
        Endif
        If (status.eq.0) then
          If (eventid.eq.EVENTID_OBS) then
            Call EVSset('FITSREAD:OBS')
            If (instrument.eq.'SIS0') then
              Call EVSset('SIS:SIS0 Event')
            else if (instrument.eq.'SIS1') then
              Call EVSset('SIS:SIS1 Event')
            else if (instrument.eq.'GIS2') then
              Call EVSset('GIS:GIS2 Event')
            else if (instrument.eq.'GIS3') then
              Call EVSset('GIS:GIS3 Event')
            Endif
          else if (eventid.eq.EVENTID_PSEUDO) then
            Call EVSset('FITSREAD:PSEUDO')
            Call BNKput('ANL:SENSOR',4,SENSOR_PSEUDO)
            If (instrument.eq.'SIS0') then
              Call EVSset('SIS:PSEUDO Event')
              Call EVSset('SIS:SIS0 Pseudo')
            else if (instrument.eq.'SIS1') then
              Call EVSset('SIS:PSEUDO Event')
              Call EVSset('SIS:SIS1 Pseudo')
            else if (instrument.eq.'GIS2') then
              Call EVSset('GIS:PSEUDO Event')
              Call EVSset('GIS:GIS2 Pseudo')
            else if (instrument.eq.'GIS3') then
              Call EVSset('GIS:PSEUDO Event')
              Call EVSset('GIS:GIS3 Pseudo')
            Endif
          Endif
          If ( 'HIGH' .eq. bit_rate ) then
             Call EVSset('ANL:BIT HIGH')
          Else if ( 'MEDIUM' .eq. bit_rate ) then
             Call EVSset('ANL:BIT MEDIUM')
          Else if ( 'LOW' .eq. bit_rate ) then
             Call EVSset('ANL:BIT LOW')
          End if
        Endif
      else
        If (nevent.gt.fits_nrecords) then
          status = ASCA_ANL_QUIT
          Return
        Endif
        If ((instrument.eq.'SIS0').or.(instrument.eq.'SIS1').or.
     &      (instrument.eq.'GIS2').or.(instrument.eq.'GIS3')) then
          Call FITSREAD_read_data(lunin,nevent,instrument,status)
        Endif
        eventid = EVENTID_OBS
        If (instrument.eq.'SIS0') then
          Call EVSset('SIS:SIS0 Event')
        else if (instrument.eq.'SIS1') then
          Call EVSset('SIS:SIS1 Event')
        else if (instrument.eq.'GIS2') then
          Call EVSset('GIS:GIS2 Event')
        else if (instrument.eq.'GIS3') then
          Call EVSset('GIS:GIS3 Event')
        Endif
        If (status.eq.0) then
           Call  EVSset('FITSREAD:OBS')
        End if
        If ( 'HIGH' .eq. bit_rate ) then
           Call EVSset('ANL:BIT HIGH')
        Else if ( 'MEDIUM' .eq. bit_rate ) then
           Call EVSset('ANL:BIT MEDIUM')
        Else if ( 'LOW' .eq. bit_rate ) then
           Call EVSset('ANL:BIT LOW')
        End if
      Endif
c set status, EVS
      If (status.eq.0) then
        If ( euler_flag ) then
          Call BNKget('ANL:TIME', 8, size, ascatime)
ccc delete ascatool dependence 1998/04/14
ccc          If ( 1 .lt. ascaEulerAng(ascatime, euler) ) then
ccc            Call BNKput('ANL:EULER', 0, euler)
ccc          Else
ccc             Call BNKput('ANL:EULER', 8*3, euler)
ccc          End if
        End if
        status = ASCA_ANL_OK
        Call EVSset('FITSREAD:OK')
      else
        Write(*,*) ' FITSREAD:read error'
        status = ASCA_ANL_QUIT
        Return
      Endif
      Return
c
      Entry FITSREAD_ana_lun(lun)
        lunin = lun
      Return
c
      Entry FITSREAD_ana_ini(instr, d_mode)
        instrument = instr
        datamode = d_mode
        first = .true.
        obs_num = 0
      Return
      End
c
c ----------------
c Event Generator
c ----------------
      Subroutine FITSREAD_event_gen(first, eventid, lunin, instrument,
     &                              datamode, obs_num, data_end, status)
      Implicit None
c input
      Logical first
      Integer eventid
      Integer lunin
      Character*(*) instrument
      Character*(*) datamode
c output
      Integer obs_num
      Logical data_end
      Integer status
c include
      Include 'Includes.inc'
      Include 'fitsread_local.inc'
c local
      Logical hold
      Save    hold
      Logical equal
      Save    equal
      Real*8  evtime
      Save    evtime
      Real*8  evgen_time
      Save    evgen_time
      Integer std_pos
      Save    std_pos
      Integer size
      Integer stdgti_num
      Real*8 stdgti_start(MAXROW)
      Real*8 stdgti_stop(MAXROW)
c
      If (first) then
c get STDGTI
        Call BNKget('FITSREAD:STDGTI_NUM', 4, size, stdgti_num)
        Call BNKget('FITSREAD:STDGTI_START',
     &                 8*stdgti_num, size, stdgti_start)
        Call BNKget('FITSREAD:STDGTI_STOP',
     &                 8*stdgti_num, size, stdgti_stop)
        std_pos = 1
        evgen_time = stdgti_start(std_pos)
        eventid = EVENTID_PSEUDO
        Call FITSREAD_bnkput(lunin, eventid, obs_num, instrument,
     &                       datamode, evgen_time, status)
        evgen_time = evgen_time + evgen_dt
        first = .false.
        hold  = .false.
        equal = .false.
        Return
      Endif
c read event time from file
      If (hold.eqv..false.) then
        obs_num = obs_num + 1
c        Write(*,*) ' obs_num', obs_num
        If (obs_num.gt.fits_nrecords) then
          data_end = .true.
          Return
        else
          Call FITSREAD_read_time(lunin, instrument, obs_num,
     &                                 evtime, status)
        Endif
      Endif
c compare between event time and pseudo event time
      If (equal.eqv..true.) then
        equal = .false.
        eventid = EVENTID_OBS
        hold  = .false.
        Call FITSREAD_bnkput(lunin, eventid, obs_num, instrument,
     &                       datamode, evtime, status)
      else
        If (evtime.lt.evgen_time) then
          eventid = EVENTID_OBS
          hold  = .false.
          Call FITSREAD_bnkput(lunin, eventid, obs_num, instrument,
     &                       datamode, evtime, status)
        else
          eventid = EVENTID_PSEUDO
          hold = .true.
          equal = (evtime.eq.evgen_time)
          Call FITSREAD_bnkput(lunin, eventid, obs_num, instrument,
     &                         datamode, evgen_time, status)
          evgen_time = evgen_time + evgen_dt
          If (evgen_time.gt.stdgti_stop(std_pos)) then
            std_pos = std_pos + 1
            If (std_pos.gt.stdgti_num) then
              evgen_time = stdgti_stop(std_pos-1)
            else
              evgen_time = stdgti_start(std_pos)
            Endif
          Endif
        Endif
      Endif
      Return
      End
c
c ----------
c  read time
c ----------
      Subroutine FITSREAD_read_time(lunin, instrument, obs_num,
     &                              evtime, status)
      Implicit None
c input
      Integer lunin
      Character*(*) instrument
      Integer obs_num
c output
      Real*8  evtime
      Integer status
c local common
      Include 'fitsread_local.inc'
c local
      Real*8 nullval
      Real*8  dval
      Logical anyf
c
      Call ftgcvd(lunin, pos_time, obs_num, 1,1, nullval, dval,
     &              anyf, status)
      evtime = dval
      Return
      End
c
c -----------------------------
c  bunkput for Event Generator
c -----------------------------
      Subroutine FITSREAD_bnkput(lunin, eventid, obs_num, instrument,
     &                                  datamode, time, status)
      Implicit None
c input
      Integer lunin
      Integer eventid
      Integer obs_num
      Character*(*) instrument
      Character*(*) datamode
      Real*8  time
c output
      Integer status
c include
      Include 'Includes.inc'
c
      If (eventid.eq.EVENTID_PSEUDO) then
        If ((instrument.eq.'SIS0').or.(instrument.eq.'SIS1')) then
          Call BNKput('SIS:TIME', 8, time)
        else if ((instrument.eq.'GIS2').or.(instrument.eq.'GIS3')) then
          Call BNKput('GIS:TIME', 8, time)
        Endif
        status = 0
      else if (eventid.eq.EVENTID_OBS) then
        If ((instrument.eq.'SIS0').or.(instrument.eq.'SIS1').or.
     &      (instrument.eq.'GIS2').or.(instrument.eq.'GIS3')) then
          Call FITSREAD_read_data(lunin,obs_num,instrument,status)
        Endif
      Endif
      Return
      End
c
c ------------------
c FITSREAD_read_data
c ------------------
      Subroutine FITSREAD_read_data(lunin,nevent,instr,fitstatus)
      Implicit None
c input:
      Integer lunin, nevent
      Character*(*) instr
c output:
      Integer fitstatus
c local
      Integer inull
      Logical anyf
      Integer ival(256)
      Real*4  eval, enull
      Real*8  dval, dnull
      Character*30 ttype,tform,bnkname
      Integer pos, size
      Integer i
c local common
      Include 'fitsread_local.inc'
c function
      Integer Lenrd
c
      Do i=1,entry_keyword
        ttype = keyword(1,keyword_pos(1,i))
        tform = keyword(4,keyword_pos(1,i))
        Call CLatoi(tform,size)
c        Write(*,*) ' size',size
        If ((Index(tform,'I')+Index(tform,'J')
     &                       +Index(tform,'B')).ne.0) then
          Call ftgcvj(lunin, keyword_pos(2,i), nevent, 1, size,
     &                inull, ival, anyf,fitstatus)
          bnkname = instr(1:Lenrd(instr)-1)//':'//ttype(1:Lenrd(ttype))
          Call BNKput(bnkname,4*size, ival)
        else if (Index(tform,'E').ne.0) then
          Call ftgcve(lunin, keyword_pos(2,i), nevent, 1, size,
     &                enull, eval,anyf,fitstatus)
          bnkname = instr(1:Lenrd(instr)-1)//':'//ttype(1:Lenrd(ttype))
          Call BNKput(bnkname,4*size, eval)
        else if (Index(tform,'D').ne.0) then
          Call ftgcvd(lunin, keyword_pos(2,i), nevent, 1, size,
     &                dnull, dval,anyf,fitstatus)
          bnkname = instr(1:Lenrd(instr)-1)//':'//ttype(1:Lenrd(ttype))
          Call BNKput(bnkname,8*size, dval)
        else
          Write(*,*) ' FITSREAD:TFORM ',tform(1:Lenrd(tform)),
     &               ' not supported'
        Endif
      Enddo
      Return
      End
c
c ------
c endrun
c ------
      Subroutine FITSREAD_endrun (status)
      Implicit NONE
c output:
      Integer status
c include
      Include 'Includes.inc'
c input for entry
      Integer lun
c local common
      Include 'fitsread_local.inc'
c local:
      Integer lunin
      Save lunin
c
      Call ftclos(lunin, status)
      If (status.ne.0) then
        status = ASCA_ANL_QUIT
        Write(*,*) ' fitsread:ftclos error ',status
      else
        status = ASCA_ANL_OK
      Endif
      Return
c
      Entry FITSREAD_endrun_ini(lun)
      lunin = lun
      Return
      End
c -----
c exit routine
c -----
      Subroutine FITSREAD_exit (status)
      Implicit  NONE
c output:
      Integer status
c include
      Include 'Includes.inc'
c entry:
      Integer lun
c local:
      Integer lunin
      Save lunin
c
      Call ANL_freelun(lunin)
      status = ASCA_ANL_OK
      Return
c
      Entry FITSREAD_exit_ini(lun)
      lunin = lun
      Return
      End

c block data subprogram
c initialize COMMON area
      Block Data fitsread_local_initial_value
      Implicit NONE
c include
      Include 'fitsread_local.inc'
c initial value
      Data evgen_dt /0/
      Data euler_flag /.false./
      End
