c Routines that do selection.
c          XSL_SELECT()          Main selection routine
c          XSL_CHIP_SEL()        Select chip (filters events and fixes GTI)
c          XSL_EVENT_SEL()       Select on events
c          XSL_FILTER_SEL()      Make GTI filter from MKF selection
c          XSL_HK_SEL()          Make GTI filter from HK selection


c ----------------------------------------------
      subroutine XSL_SELECT()
c ----------------------------------------------
c  This routine controls the various selection possibilities,
c  i.e. those that use FSELECT or maketime.  These are:
c  Obscat selection using FSELECT
c  Event data selection using FSELECT
c  Event data selection for given chip(s) using FSELECT and GTI calculation
c  HK selection using maketime
c  Filter File selection using maketime
c  Jim Ingham 6/93
c
c  Added the bit that modified primary header keywords after 'select fast'
c
c  Modified the behavior of 'select fast' using new information from ISAS
c     Koji Mukai 9/96
c  Added select chip option
c     kaa  9/98

      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1, str2, newnam
c File I/O unit numbers
      
c General reusable integers for lengths of strings 
      integer len1, len2
c ---------------------------------------------      
      integer MXCMDS
      parameter (MXCMDS = 20 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      character(40) cmnarena, cmnario
      character(8) keyarena, keyario, valarena, valario
      character(256) pkfilnam, chipsel
      integer ilun
      integer nwhats,myindex
      integer LENACT,i
      logical ANSW
      character(255) contxt
      
      data commad /
     &     'CHIP',
     &     'EVENTS',
     &     'EXPREF',
     &     'FAST',
     &     'HK',
     &     'MKF',
     &     'OBSCAT',
     &     13*'NA'/
      data comdes /
     &     'select events from specified chip(s)',
     &     'select events using a Boolean expression',
     &     'select events based on EXPREF value',
     &     'select events based on FAST mode area discrimination',
     &     'creates GTI file from HK files',
     &     'creates GTI file from the Filter file',
     &     'selects a subset of the loaded catalogue',
     &     13*'NA'/
      
      data nwhats / 7 /

      CALL xsl_match_cmd('select_what', commad, comdes, nwhats, 
     &                   MXCMDS, comno, str1, myindex, status)
      IF ( status .NE. 0 ) THEN
         status = 0
         return
      ENDIF

c ------------------------------------------------
c SELECT FAST
      IF(str1.eq.'FAST') THEN
         if (datamode.ne.'FAST') then
            call XWRITE('Only use FILTER FAST for FAST mode data',
     &           5)
            return
         endif
c Now get the keywords we need from the obscat:
c     First the chip number, and the ARENA:
         if( nfiles.eq.0 ) then
            call XWRITE('You must have CHOSEn or READ some data'//
     &           ' to use SELECT FAST.',5)
            return
         endif

         if(arena.eq. 1) then
            if(ario.eq.0 ) then
               call XWRITE('Only the photons outside the area '//
     &              'discrimination region are in the telemetry.',5)
               call XWRITE('SELECT FAST is unnecessary.',5)
            else
               call XWRITE('Only the photons inside the area '//
     &              'discrimination region are in the telemetry.',5)
               call XWRITE('SELECT FAST is unnecessary.',5)
            endif
            return
         endif
         status = 0
         IF(nstrsel.eq.MXHIST) THEN
            do i =1, MXHIST - 1
               strsel(i) = strsel(i+1)
            enddo
         else
            nstrsel = nstrsel + 1
         ENDIF
         call XSL_GETDUMMYPARST('in_or_out','select_str',str1,
     &        status)
         contxt = 'Failed to get in_or_out'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(str1)
         if ( index(str1,'IN').ne.0) then
c            New info from ISAS (1996 Sep) --- RAWX=0 is _always_ inside
c            if(ario.eq.0) then
c               strsel(nstrsel) = 'RAWX==0'
c            else if ( ario.eq.1) then
c               strsel(nstrsel) = 'RAWX==1'
c            endif
            strsel(nstrsel) = 'RAWX==0'
            in_or_out = 'IN'
         else if(index(str1,'OUT').ne.0) then
c            if(ario.eq.0) then
c               strsel(nstrsel) = 'RAWX==1'
c            else if (ario.eq.1) then
c               strsel(nstrsel) = 'RAWX==0'
c            endif   
            strsel(nstrsel) = 'RAWX==1'
            in_or_out = 'OUT'
         else
            call XWRITE('Please enter ''inside'' or ''outside''',5)
            return
         endif
         str1 = 'Running FSELECT with expression '
     &     //strsel(nstrsel)(:min(lenact(strsel(nstrsel)),len(str1)-32))
         call XWRITE(str1,5)
         call XSL_EVENT_SEL()

         if( SELCT ) then
*           New section by KM, to modify keywords
*           (Delete previous command file and work2 files)
            valarena = '#1'
            if( instru .eq. 'SIS0' ) then
               keyarena = 'S0_ARENA'
               cmnarena = 'S0 Area discrimination enable/disabel'
               write( keyario, '(a,i1)' ) 'S0_ARIO', ccdno
               write( cmnario, '(a,i1)' )
     &                   'S0 area discrimination IN/OUT for ccd ', ccdno
            else if( instru .eq. 'SIS1' ) then
               keyarena = 'S1_ARENA'
               cmnarena = 'S1 Area discrimination enable/disabel'
               write( keyario, '(a,i1)' ) 'S1_ARIO', ccdno
               write( cmnario, '(a,i1)' )
     &                   'S1 area discrimination IN/OUT for ccd ', ccdno
            end if
            if( in_or_out .eq. 'IN' ) then
               valario = '#1'
            else if( in_or_out .eq. 'OUT' ) then
               valario = '#0'
            end if
*           (Open new command file)
            call XSL_OPCF(cmdfil,ilun)
            if( BININ ) then
               pkfilnam = evnin( :LENACT( evnin ) ) // '+0'
               call XSL_FPARKEY
     &                  ( ilun, keyarena, valarena, cmnarena, pkfilnam )
               call XSL_FPARKEY
     &                     ( ilun, keyario, valario, cmnario, pkfilnam )
            else if( BINOUT ) then
               pkfilnam = evnout( :LENACT( evnout ) ) // '+0'
               call XSL_FPARKEY
     &                  ( ilun, keyarena, valarena, cmnarena, pkfilnam )
               call XSL_FPARKEY
     &                     ( ilun, keyario, valario, cmnario, pkfilnam )
            else
               do i = 1, nfiles
                  pkfilnam = work1( i )( :LENACT( work1( i ) ) ) // '+0'
                  call XSL_FPARKEY
     &                  ( ilun, keyarena, valarena, cmnarena, pkfilnam )
                  call XSL_FPARKEY
     &                     ( ilun, keyario, valario, cmnario, pkfilnam )
               enddo
            end if
*           (Close command file)
            call XSL_CLCF( ilun )
*           (Run command file)
            call XSL_RUNCF( cmdfil, ECHO, status )
         end if

c ---------------------------------------------------
c SELECT OBSCAT
      ELSE IF(str1.eq.'OBSCAT') THEN
         call XSL_GETDUMMYPARST('cat_sel','select_str',catsel,status)
         IF(status.ne.0) then
            call XWRITE('Error getting selection expression',5)
            return
         ENDIF

c Now invalidate any previous reads:
         IF(READ) THEN
            call XSL_CLEAR(2)
         ENDIF
         
         IF(LOADED) then
c If the catalogue is loaded, then don't overwrite it!  Rather, 
c a new catalogue is made with the correct default name, and so
c we are no longer left with a 'loaded catalogue'

            IF(instru .eq.'NONE') THEN
               call XSL_GETDUMMYPARST('instrument','select_str2',instru,
     &                              status)
               contxt = 'Failed to get instrument'
               IF ( status .NE. 0 ) GOTO 99999
            ENDIF
            CALL XSL_MDB_INST(keymis, submis, instru, obsno, status)

c Deal with the case where the loaded catalogue has the default name:
c by moving the loaded catalogue to a new name
            IF(index(obscat(obsno),catnam(:LENACT(catnam)))
     &           .ne.0) THEN
               call XSL_MOVE_AWAY(catnam,newnam)
               call XSL_CAT_FILT(newnam,obscat(obsno),catsel,
     &              ECHO,status)
c However, if we failed to make a new catalogue, then we must reset the 
c catnam to the name we moved the loaded one to... (Whew!!!)
               if(status.ne.0) then
                  catnam = newnam
               endif
            ELSE
               call XSL_CAT_FILT(catnam,obscat(obsno),catsel,
     &              ECHO,status)
            ENDIF
            IF(status.eq.0) THEN
               LOADED = .FALSE.
               MADE = .TRUE.
               catnam = obscat(obsno)
            ELSE
               catsel = 'NONE'
               call XSL_RMFILE(obscat(obsno))
            ENDIF 
         ELSE IF (MADE) THEN
            call XSL_MOVE_AWAY(catnam,newnam)
            call XSL_CAT_FILT(newnam,catnam,catsel,ECHO,status)
            IF(status.ne.0) THEN
               catsel = 'NONE'
               len2 = LENACT(catnam)
               call XSL_RENAME(newnam,catnam,1,str2,len2,ierr)
               return
            ENDIF
         ELSE
            IF(instru .eq.'NONE') THEN
               call XSL_GETDUMMYPARST('instrument','select_str2',instru,
     &                              status)
               contxt = 'Failed to get instrument'
               IF ( status .NE. 0 ) GOTO 99999
            ENDIF
            CALL XSL_MDB_INST(keymis,submis,instru,obsno,status)
            catnam = obscat(obsno)
            call XSL_EXIST(catnam,status)
            IF(status.ne.0) THEN
               call XWRITE('No catalogue found',5)
               return
            ENDIF
            call XSL_CAT_FILT(catnam,tempfl,catsel,ECHO,status)
            IF(status.ne.0) THEN
               call XSL_RMFILE(tempfl)
               catsel = 'NONE'
               return
            ELSE
               len2 = LENACT(catnam)
               call XSL_RENAME(tempfl,catnam,1,str2,len2,ierr)
               MADE = .TRUE.
            ENDIF
         ENDIF
c --------------------------------------------------------
c SELECT CHIP or EXPREF
      ELSE IF(str1.eq.'CHIP' .OR. str1.eq.'EXPREF') THEN
         IF(READ) THEN
            status = 0
            call XSL_GETDUMMYPARST('chip_sel','select_str',chipsel,
     &           status)
            contxt = 'Failed to get chip_sel'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_CHIP_SEL(chipsel)
         ELSE
            call XWRITE('No data has been read in.',5)
            status = -10
         ENDIF
c --------------------------------------------------------
c SELECT EVENTS
      ELSE IF(str1.eq.'EVENTS') THEN
         IF(READ) THEN
            status = 0
            IF(nstrsel.eq.MXHIST) THEN
               do i =1, MXHIST - 1
                  strsel(i) = strsel(i+1)
               enddo
            else
               nstrsel = nstrsel + 1
            ENDIF
            call XSL_GETDUMMYPARST('event_sel','select_str',
     &                           strsel(nstrsel), status)
            contxt = 'Failed to get event_sel'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_EVENT_SEL()
         ELSE
            call XWRITE('No data has been read in.',5)
            status = -10
         ENDIF
c ------------------------------------------------------------
c SELECT HK
      ELSE IF (str1.eq.'HK') THEN
         IF(.NOT.HKREAD) THEN
            call XWRITE('No HK files read in',5)
            return
         ENDIF
c First input the selection criterion, and then do the selection
         call XSL_GETDUMMYPARST('hk_sel','select_str',hkstr,status)
         IF(status.ne.0) then
            call XWRITE('Error getting selection expression',5)
            return
         ENDIF
         len1 = LENACT(hkstr)
         hkstr = '"'//hkstr(1:len1)//'"'
         len1 = LENACT(hkstr)
c
         IF(BINOUT) THEN
            IF(CLEAN) THEN
               call XWRITE('The HK select output will invalidate '
     &              ,5 ) 
               call XWRITE('the accumulated event list '//
     &              'for SISCLEAN',5)
               call xsl_uclgsb('save_file', ANSW, status)
               IF(status.ne.0) THEN
                  call XWRITE('Reply not understood, '//
     &                 'continuing',5)
                  ANSW = .FALSE.
                  status = 0
               ENDIF
               IF(ANSW) THEN
                  call XSL_SAVE(2)
               ENDIF
               BINOUT = .FALSE.
               call XSL_RMFILE(evnout)
            ENDIF
         ENDIF
         call XSL_HK_SEL()
c ----------------------------------------------------------
c SELECT MKF
      ELSE IF (str1.eq.'MKF') THEN
c We undo the previous filtering:
         FILTER = .FALSE.

         call xsl_uclgst('mkf_name',ffilin,status)
         contxt = 'Failed to get mkf_name'
         IF ( status .NE. 0 ) GOTO 99999


         IF(nffstr.eq.MXHIST) THEN
            do i =1, MXHIST - 1
               ffstr(i) = ffstr(i+1)
            enddo
         else
            nffstr = nffstr + 1
         ENDIF
         CALL XSL_GETDUMMYPARST('mkf_exp','select_str',ffstr(nffstr),
     &                        status)
         contxt = 'Failed to get mkf_exp'
         IF ( status .NE. 0 ) GOTO 99999
         IF(ffstr(nffstr).eq.' ') THEN
            call XWRITE('Empty selection expression',5)
            status = -10
            return
         ENDIF
         call XSL_FINDMKF(.FALSE.)
         if(status.ne.0) then
            call XWRITE('Error in SELECT MKF from FIND MKF',5)
            return
         endif
c$$$         IF(BINOUT) THEN
c$$$            IF(CLEAN) THEN
c$$$               call XWRITE('The MKF select output will '//
c$$$     &              'invalidate ',5 ) 
c$$$               call XWRITE('the accumulated event list for '//
c$$$     &              'SISCLEAN',5)
c$$$               call xsl_uclgsb('save_file', ANSW, status)
c$$$               IF(status.ne.0) THEN
c$$$                  call XWRITE('Reply not understood, '//
c$$$     &                 'continuing',5)
c$$$                  ANSW = .FALSE.
c$$$                  status = 0
c$$$               ENDIF
c$$$               IF(ANSW) THEN
c$$$                  call XSL_SAVE(2)
c$$$               ENDIF
c$$$               BINOUT = .FALSE.
c$$$               call XSL_RMFILE(evnout)
c$$$            ENDIF
c$$$         ENDIF
         call XSL_FILTER_SEL()
         if(status.ne.0) then
            nffstr = nffstr - 1
         endif
      ELSE
         call XWRITE('Unknown option in select',5)
      ENDIF

99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_SELECT: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end       
c     
c     
c
c ---------------------------------------------
      subroutine XSL_EVENT_SEL()
c ---------------------------------------------
c
c Called by XSELECT main
c
c Alan Smale 1992 Nov

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c     Scratch variables
c The command line:
      character(4096) comlin
c Strings used, reused, reused again as temporary space
      character(512) str1
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings
      integer len1, len2, len3
c ---------------------------------------------
      integer LENACT
      integer i,maxlen
      status = 0

c Give the user the chance to save the workspace or evnin files
      call XSL_SAVE_WS_OR_EVNIN()
c elete previous command file and work2 files

      call XSL_RMWORK(work2,nfiles)

c Open new command file

      call XSL_OPCF(cmdfil,ilun)

c If we've already performed a data-changing operation like a
c SELECT or a faint-to-bright conversion we'll be using
c the WORK1 filenames as input. Otherwise it'll be the original
c filenames. Either way, the WORK2 files are the output files, which
c are then copied (back) to the WORK1 files and the .WORK. logical
c reset for subsequent observations.


c     Delete leading, trailing, and blanks, and null characters.
c     (note: RMVNUL and RMVXBK don't work on the Sun)
c     Add " at beginning and end,  unless the
c     expression is a filename (and thus begins with an '@').

      call RMVLBK(strsel(nstrsel))

      len3 = LENACT( strsel(nstrsel) )

      IF(strsel(nstrsel)(1:1).ne.'@')then

         strsel(nstrsel)(len3+1:len3+1)='"'
         maxlen = len(strsel(nstrsel))
         do i= maxlen, 2, -1
            strsel(nstrsel)(i:i) = strsel(nstrsel)(i-1:i-1)
         end do

         strsel(nstrsel)(1:1)='"'

      ENDIF

c     Construct instruction strings controlling NFILES applications
c     of FSELECT.  Remember to point at the event extension (evtnum)

      len3 = LENACT( strsel(nstrsel) )
      IF(BININ) THEN
         comlin='fselect '//
     +        'infile='//evnin(:LENACT(evnin))//' '//
     +        'outfile='//work2(1)(:LENACT(work2(1)))//' '//
     +        'expr='//strsel(nstrsel)(1:len3)//' '//
c     +                 'index = " " '//
     +        'histkw=yes '
C         len1 = LENACT( comlin )
         call XSL_WRTCF(ilun,comlin,1)

      ELSE IF(BINOUT) THEN
         comlin='fselect '//
     +        'infile='//evnout(:LENACT(evnout))//' '//
     +        'outfile='//work2(1)(:LENACT(work2(1)))//' '//
     +        'expr='//strsel(nstrsel)(1:len3)//' '//
c     +                 'index = " " '//
     +        'histkw=yes '
C         len1 = LENACT( comlin )
         call XSL_WRTCF(ilun,comlin,1)

      ELSE
         DO i = 1, nfiles
            IF( WORK )then
               len1 = LENACT( work1(i) )
               len2 = LENACT( work2(i) )

               comlin='fselect '//
     +              'infile='//work1(i)(1:len1)//' '//
     +              'outfile='//work2(i)(1:len2)//' '//
     +              'expr='//strsel(nstrsel)(1:len3)//' '//
c     +                 'index = " " '//
     +              'histkw=yes '

               str1 = 'Processing file: '//work1(i)
               call XSL_MESSAGE(ilun,str1)
            ELSE
               len1 = LENACT( filenm(i) )
               len2 = LENACT( work2(i) )

               comlin='fselect '//
     +              'infile='//filenm(i)(1:len1)//'+'//evtnum//' '//
     +              'outfile='//work2(i)(1:len2)//' '//
     +              'expr='//strsel(nstrsel)(1:len3)//' '//
c     +                 'index = " " '//
     +              'histkw=yes '
               str1 = 'Processing file: '//filenm(i)
               call XSL_MESSAGE(ilun,str1)

            ENDIF

C            len1 = LENACT( comlin )
            call XSL_WRTCF(ilun,comlin,1)

c     We've now sent the command to do the selection.
         END DO
      ENDIF

      call XSL_CLCF(ilun)

c     Run the command file
      call XSL_RUNCF(cmdfil,ECHO,status)

c     Set the SELCT logical
      if(status.eq.0) THEN
         SELCT = .TRUE.

c Finally copy the files over
         IF(BININ) THEN
c If BININ, then only the evnin file has been altered.
            len1 = LENACT(evnin)
            call XSL_RENAME(work2(1),evnin,1,str1,len1,ierr)
         ELSE IF(BINOUT) THEN
c If BININ, then only the evnin file has been altered.
            len1 = LENACT(evnout)
            call XSL_RENAME(work2(1),evnout,1,str1,len1,ierr)
         ELSE
c Otherwise, all the selected files will be in WORK2, which should
c become the CURRENT files. Thus, copy the WORK2 files to WORK1.

c Open new command file

            call XSL_OPCF(cmdfil,ilun)
            do i=1,nfiles
               len1 = LENACT(work1(i))
               call XSL_RENAME(work2(i),work1(i),2,str1,len1,ierr)
               write(ilun,53) str1(1:len1)
            enddo
            call XSL_CLCF(ilun)

c Run the command file
            call XSL_RUNCF(cmdfil,ECHO,ierr)
c Set the WORK logical, if it isn't set already.
            WORK = .TRUE.
c Reset the MERGED logical -- individual files have changed.
            MERGED = .FALSE.
         ENDIF
      ELSE
c Erase the unsuccessful selection from the list:
         strsel(nstrsel) = 'NONE'
         nstrsel = nstrsel - 1
         call XWRITE('Error in Fselect, No selection made',5)
      ENDIF

 53   format(a)
      return
      end

c
c
c --------------------------------------------
      subroutine XSL_FILTER_SEL()
c --------------------------------------------
c This produces a fits gti file from the filter file output by MKFILTER
c This is ASCA specific at present
c J. Ingham

      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line:
      character(4096) comlin
c File I/O unit numbers
      integer ilun, ilun2
c General reusable integers for lengths of strings
      integer  len2, len3, len4, len5
c ---------------------------------------------
      integer LENACT,i

C check that the filter file(s) exists.
      do i=1,nmkf
         len2 = LENACT(mkfnam(i))
         call XSL_EXIST(mkfnam(i),status)
         IF(status.ne.0) then
            call XWRITE('Could not find MKF file '
     &                           //mkfnam(i)(:len2),5)
            return
         ENDIF
      enddo
c Now open the command file, and clear the workspaces:

      do i=1,nmkf
         call XSL_RMFILE(work2(i))
      enddo
c Open the command file:
      call XSL_OPCF(cmdfil,ilun)

c Also open the listfile:
      call GETLUN(ilun2)
      call XSL_RMFILE(lstfil)
      call XSL_OPEN(ilun2,lstfil,'NEW',' ',' ',0,0,ierr)

      len2 = LENACT(ffstr(nffstr))
      len3 = LENACT(ffflt)
      len4 = LENACT(keytim)
C Now build up the command file:
      do i=1,nmkf
         call XSL_RMFILE(work2(i))
         len5 = LENACT(work2(i))
C   The makefile files are like expanded HK files:
         comlin = 'maketime infile='//mkfnam(i)(:LENACT(mkfnam(i)))//
     +      ' '//'outfile='//work2(i)(:len5)//' '//
     +      'expr="'//ffstr(nffstr)(1:len2)//'" '//
     +      'name=NAME value=VALUE '//
     +      'time='//keytim(1:len4)//' '//
     +      'start=START stop=STOP '//' '//
c     +      'hkdate = DATE hktime = TIME'//' '//
     +      'compact=no'//' '//
     +      'histkw=yes'

C         len1 = LENACT( comlin )
         call XSL_WRTCF(ilun,comlin,1)
c This is the list of work files for mgtime:
         write(ilun2,'(A)') work2(i)(:len5)
      enddo
      close(ilun2)
      call FRELUN(ilun2)

c Now write out the string for merging the maketime results:
      IF(nmkf.gt.1) THEN
         call XSL_RMFILE(work2(nmkf+1))
         comlin = 'mgtime ingtis=@'//
     &      lstfil(:LENACT(lstfil))//' '//
     &      'outgti='//work2(nmkf+1)(:LENACT(work2(nmkf+1)))//' '//
     &         'merge=OR instarts=START instops=STOP '//
     &         'indates=MJDREF intimes=" " '//
     &         'outstart=START outstop=STOP'
C         len1 = LENACT(comlin)
         call XSL_WRTCF(ilun,comlin,1)
      ENDIF
c If there was an old GTI file, then merge it with the newly made one:
      IF(FFTFL) THEN
         call XSL_EXIST(ffflt,status)
         IF(status.ne.0) THEN
            call XWRITE('Error finding old filter file',5)
            return
         ENDIF
         IF(nmkf.eq.1) THEN
            call XSL_RMFILE(work2(nmkf+1))
            comlin = 'mgtime ingtis="'//
     &           work2(nmkf)(:LENACT(work2(nmkf)))//' '//
     &           ffflt(:len3)//'" '//
     &           'outgti='//
     &           work2(nmkf+1)(:LENACT(work2(nmkf+1)))//' '//
     &           'merge=AND instarts=START instops=STOP '//
     &           'indates=MJDREF intimes=" " '//
     &           'outstart=START outstop=STOP'
C            len1 = LENACT(comlin)
            call XSL_WRTCF(ilun,comlin,1)
c This copies the output back to ffflt
            call XSL_RENAME(work2(nmkf+1),ffflt,0,comlin,
     &                         LENACT(ffflt),status)
            write(ilun,'(A)') comlin(:LENACT(comlin))
         ELSE
            call XSL_RMFILE(work2(nmkf+2))
            comlin = 'mgtime ingtis="'//
     &           work2(nmkf+1)(:LENACT(work2(nmkf+1)))//' '//
     &           ffflt(:len3)//'" '//
     &           'outgti='//
     &           work2(nmkf+2)(:LENACT(work2(nmkf+2)))//' '//
     &           'merge=AND instarts=START instops=STOP '//
     &           'indates=MJDREF intimes=" " '//
     &           'outstart=START outstop=STOP'
C            len1 = LENACT(comlin)
            call XSL_WRTCF(ilun,comlin,1)
c This copies the output back to ffflt
            call XSL_RENAME(work2(nmkf+2),ffflt,0,comlin,
     &                         LENACT(ffflt),status)
            write(ilun,'(A)') comlin(:LENACT(comlin))
         ENDIF

c If there was no old ffflt, just move the product to the correct name:
      ELSE
         IF(nmkf.eq.1) THEN
            call XSL_RENAME(work2(nmkf),ffflt,0,comlin,
     &                         LENACT(ffflt),status)
         ELSE
            call XSL_RENAME(work2(nmkf+1),ffflt,0,comlin,
     &                         LENACT(ffflt),status)
         ENDIF
         write(ilun,'(A)') comlin(:lenact(comlin))
      ENDIF

      call XSL_CLCF(ilun)

      call XSL_RUNCF(cmdfil,ECHO,status)

      IF(STATUS.ne.0) THEN
         FFTFL = .FALSE.
      ELSE
c Check to make sure the file is not empty:
c The GTI's are in the FIRST extension in the maketime output file:
         call XSL_GETKWI(ffflt,'1',wrkdir,'NAXIS2',len2,len3,1,1,
     &        .FALSE.,status)
         IF(status.ne.0.or.len3.ne.0) THEN
            call XWRITE('Error in output GTI file',5)
            FFTFL = .FALSE.
         ELSE IF(len2.eq.0) THEN
            call XWRITE(
     & 'The selection criteria produced no GTI''s',5)
            status = -10
            FFTFL = .FALSE.
         ELSE

c Finally, remake the GTI list file:
            FFTFL = .TRUE.
            call XSL_GTI_FLT(comlin,1)
         ENDIF
      ENDIF
      return
      end
c
c
c
c ----------------------------------------------
      subroutine XSL_HK_SEL()
c ----------------------------------------------
c
c Called by XSELECT main
c
c Jim Ingham 1993 March

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line
      character(4096) comlin
c Strings used, reused, reused again as temporary space
      character(255) str1
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings
      integer len1, len2, len3
c ---------------------------------------------
      integer LENACT,temp

      status = 0

      IF(HKREAD) THEN

c  Merge the HK files if necessary
         IF(MANYHK.AND. .NOT. MERGHK) THEN
            IF( WORKHK )then
               call XSL_MERGE(hkwrk1,nhkfil,lstfil,mrghkf,'HK',cmdfil,
     +                 str1,ECHO,ierr)
            ELSE
               call XSL_MERGE(hkflnm,nhkfil,lstfil,mrghkf,'HK',cmdfil,
     +                 str1,ECHO,ierr)
            ENDIF
            if(ierr.eq.0) then
               MERGHK=.TRUE.
            else
               call XWRITE('Error in HKSEL from fmerge',5)
               return
            endif
         ENDIF

         IF(MERGHK) THEN
            len2 = LENACT(mrghkf)
            comlin = 'maketime infile='//mrghkf(1:len2)
         ELSE
            len2 = LENACT(hkflnm(1))
            comlin = 'maketime infile='//hkflnm(1)(1:len2)
         ENDIF
         len1 = LENACT(hkstr)
         len2 = LENACT(comlin)
         len3 = LENACT(hktfl)
         temp = LENACT(keytim)

C   We only accept expanded files if they are input that way:
         IF(EXPAND.AND. .NOT. WORKHK) then
            comlin = comlin(1:len2)//' '//
     &           'outfile='//hktfl(1:len3)//
     +          ' '//'expr='//hkstr(1:len1)//' '//
     +          'name=NAME value=VALUE '//
     +          'time='//keytim(1:temp)//' '//
     +          'start=START stop=STOP '//' '//
c     +          'hkdate = DATE hktime = TIME'//' '//
     +          'compact=no'//' '//
     +          'histkw=yes'
         ELSE
            comlin = comlin(1:len2)//' '//
     &           'outfile='//hktfl(1:len3)//
     +          ' '//'expr='//hkstr(1:len1)//' '//
     +          'name=NAME value=VALUE '//
     +          'time='//keytim(1:temp)//' '//
     +          'start=START stop=STOP '//' '//
     +          'hkdate=DATE hktime=TIME'//' '//
     +          'compact=yes'//' '//
     +          'histkw=yes'
         ENDIF
c Now set up the command file
C         len1 = LENACT( comlin )

         call XSL_RMFILE(hktfl)

         call XSL_OPCF(cmdfil,ilun)

         call XSL_WRTCF(ilun,comlin,1)
         call XSL_CLCF(ilun)

         call XSL_RUNCF(cmdfil,ECHO,ierr)
         IF(STATUS.ne.0.or.ierr.ne.0) THEN
            return
         ELSE
            HKSEL = .TRUE.
c Now remake the list file:
            call XSL_GTI_FLT(str1,1)
         ENDIF

      ELSE
         call XWRITE('No HK files read in yet',5)
      ENDIF

      return
      end

