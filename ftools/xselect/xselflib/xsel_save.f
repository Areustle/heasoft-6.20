c Routines to save various things in xselect.
c      XSL_SAVE(mode)                      Main save routine
c      XSL_BINOUT_SAV(messg)               Actually a wrap up for XSL_SAVE(2)
c      XSL_SAVE_CLEAN(outfile,mode)        Saves the output of clean 
c      XSL_SAVE_CURVE(outfile,mode)        Saves the lightcurve
c      XSL_SAVE_EVENTS(outfile,mode)       Saves the extracted event file
c      XSL_SAVE_IMAGE(outfile,mode)        Saves the extracted image
c      XSL_SAVE_TIME(outfile,...)          Saves the time filter files
c      XSL_SAVE_WS_OR_EVNIN()              Saves the workspace or event in files


c     ---------------------------------------------
      subroutine XSL_SAVE(mode)
c     ---------------------------------------------
c     
c     Called by XSELECT main
c     
c     Alan Smale 1992 Nov

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c     ---------------------------------------------
c     Scratch variables
c     Strings used, reused, reused again as temporary space
      character(255) str1, str2, str3
c     File I/O unit numbers
      integer ilun
c     General reusable integers for lengths of strings 
      integer len1
c     ---------------------------------------------
      integer LENACT
      integer i,mode
      logical GROUP,CLOBBR

      integer MXCMDS
      parameter (MXCMDS = 23 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      character(255) tmpvec(MXNSEL)
      integer nwhats,myindex
      character(8) time_mod(4)
      character(64) time_des(4)
      character(255) contxt

      INTEGER APE_TRAD_GET_MODE
      INTEGER APE_TRAD_SET_MODE
      
      data commad /'ALL','CLEAN','CURVE','DFE','EVENTS',
     &     'EXPANDED','FAINT','FAST','GOODTIME','HKBIN','HKSEL',
     &     'IMAGE','INTENSITY','MKFBIN','MKFSEL','OBSCAT','PHASE',
     &     'REGION','SESSION','SELECTION','SPECTRUM',
     &     'TIME','WORKSPACE'/
      data comdes /'save CLEAN, EVENTS, IMAGE, CURVE, SPEC or subset',
     &     'save sisclean region',
     &     'save the accumulated light curve',
     &     'save the DFE file made by FAINT',
     &     'save the event list output from bin',
     &     'save expanded HK files',
     &     'save the results of the faint command',
     &     'save the results of the fast command',
     &     'save accumulated GTI''s output from extract',
     &     'save the results of the hkbin command',
     &     'save the GTI file from the HK selection',
     &     'save the accumulated image',
     &     'save the intensity GTI files','save the current obscat',
     &     'save the results of the mkfbin command',
     &     'save the GTI file from the filter file selection',
     &     'save the phase selections',
     &     'save accumulated region filter',
     &     'save session and exit',
     &     'save the results of the select events command',
     &     'save accumulated spectrum',
     &     'save the cursor or by hand timing files',
     &     'save the workspace files'/
      
      data nwhats / 23 /
      data time_mod /'ALL','CURSOR','KEYBOARD','QUIT'/
      data time_des /'Save ALL timing filters',
     &               'Save filters entered with FILTER TIME CURSOR',
     &       'Save filters entered with FILTER TIME UT, MJD, or SSC',
     &        'Quit save time'/


      IF(mode.eq.1) THEN
         str1 = 'SESSION'
      ELSE IF(mode.eq.2) THEN
         str1 = 'EVENTS'
         status = 0
      ELSE IF(mode.eq.3) THEN
         str1 = 'CLEAN'
         status = 0
      ELSE IF(mode.eq.4) THEN
         str1 = 'IN_EVENTS'
         status = 0
      ELSE IF(mode.eq.5) THEN
         str1 = 'FAINT'
         status = 0
      ELSEIF(mode.eq.6) THEN
         str1 = 'PHASE'
         status = 0
      ELSE
         CALL xsl_match_cmd('save_what', commad, comdes, nwhats, 
     &                      MXCMDS, comno, str1, myindex, status)
         IF ( status .NE. 0 ) THEN
            status = 0
            return
         ENDIF
      ENDIF

c ---------------------------------------------
c Save ALL
      IF(str1.eq.'ALL') THEN
c Each of these routines could ask for the clobber, so force the issue here
c Retrieve and save old mode in str1; set new mode to 'hl' temporarily:
         status = APE_TRAD_GET_MODE('clobberit',str1)
         status = APE_TRAD_SET_MODE('clobberit','hl')
         call xsl_uclgsb('clobberit',CLOBBR,status)
         contxt = 'Failed to get clobberit'
         IF ( status .NE. 0 ) GOTO 99999
c Get and save new mode in str2; set new mode to old mode saved above:
         status = APE_TRAD_GET_MODE('clobberit',str2)
         status = APE_TRAD_SET_MODE('clobberit',str1(:LENACT(str1)))
c Read value of dummy parameter (save_str) from par file;
c Put value (if any) in 'outfile' and pass back here in str2:
         call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
         contxt = 'Failed to get outfile'
         IF ( status .NE. 0 ) GOTO 99999

         if (SPEC) then
            GROUP = .FALSE.
            call XWRITE(' ',5)
            call XWRITE('Saving the Spectrum:',5)
            if ( CLOBBR ) then
               str2 = '!'//str2(:len(str2)-1)
            endif
            call XSL_GRPSAV(str2,1)
         endif
         if (IMAGE) then
            call XWRITE(' ',5)
            call XWRITE('Saving the Image:',5)
            if ( CLOBBR ) then
               str2 = '!'//str2(:len(str2)-1)
            endif
            call XSL_SAVE_IMAGE(str2,1)
         endif
         if (CURV) then
            call XWRITE(' ',5)
            call XWRITE('Saving the Light Curve:',5)
            if ( CLOBBR ) then
               str2 = '!'//str2(:len(str2)-1)
            endif
            call XSL_SAVE_CURVE(str2,1)
         endif
c Do SAVE CLEAN for SIS, and for GIS if there is no binout file. 
         if(CLEAN.and.(instru(1:3).eq.'SIS'.or.
     &        (instru(1:3).eq.'GIS'.and. .not. BINOUT))) then
            call XWRITE(' ',5)
            call XWRITE('Saving the Cleaned events list(s):',5)
            if ( CLOBBR ) then
               str2 = '!'//str2(:len(str2)-1)
            endif
            call XSL_SAVE_CLEAN(str2,1)
         else if (BINOUT) then
            call XWRITE(' ',5)
            call XWRITE('Saving the Filtered Events list:',5)
            if ( CLOBBR ) then
               str2 = '!'//str2(:len(str2)-1)
            endif
            call XSL_SAVE_EVENTS(str2,1)
         endif
c This little two-step is so the default will come up next time
c without an extension (here XSL_XTEND removes the extension of
c str2 when passed ' ').
         call XSL_XTEND(str2,' ',status)
         call XSL_UCLPST('outfile',str2,status)
c ---------------------------------------------
c SAVE INTENSITY
      ELSE IF(str1.eq.'INTENSITY') THEN
         IF(numint.eq.0) THEN
            call XWRITE('No intensity filters entered yet.',5)
            return
         ELSE IF(numint.eq.1) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str2,'-int',status)
            status = 0
            call XSL_COPY(intvec(1),str2,status)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)         
         ELSE
            write(str2,577) numint
 577        format('There are ',i3,' files.')
            call XWRITE(str2,5)
            call XWRITE('They will be named sequentially from '//
     &           'the given base name.',5)
            call XWRITE('A default extension of .int will be '//
     &           'added if none is given.',5)
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_GETEXT(str2,str3)
            IF(str3.eq.' ') THEN
               str3 = 'gti'
            ENDIF
            call XSL_FAFNA(str2,str3,MXNSEL,tmpvec)
            do i=1,numint
               status = 0
               len1 = LENACT(intvec(i))
               call xsl_filename(intvec(i),str2,len1)
               call XSL_COPY(str2,tmpvec(i),status)
            enddo
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)         
         ENDIF
c ---------------------------------------------
c SAVE HKBIN
      ELSE IF(str1.eq.'HKBIN')then
         IF(HKCURV) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str2,'-gti',status)
            call XSL_COPY(hkcurf,str2,status)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE('No HK binning available.',5)
         ENDIF

c ---------------------------------------------
c SAVE HKSEL
      ELSE IF(str1.eq.'HKSEL')then
         IF(HKSEL) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str2,'-gti',status)
            call XSL_COPY(hktfl,str2,status)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE('No HK selection made.',5)
         ENDIF

c ---------------------------------------------
c SAVE MKFBIN
      ELSE IF(str1.eq.'MKFBIN')then
         IF(FFCURV) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str2,'-gti',status)
            call XSL_COPY(ffcurf,str2,status)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE('No MKF binning available.',5)
         ENDIF

c ---------------------------------------------
c SAVE MKFSEL
      ELSE IF(str1.eq.'MKFSEL')then
         IF(FFTFL) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str2,'-gti',status)
            call XSL_COPY(ffflt,str2,status)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE('No filter file selection made.',5)
         ENDIF
c ---------------------------------------------
c SAVE PHASE
      ELSE IF(str1.eq.'PHASE')then
         IF(XPHTFL) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str2,'-wi',status)
            call XSL_COPY(xphflt,str2,status)
            XPHSAV = .TRUE.
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE('No PHASE selection made.',5)
         ENDIF
c ---------------------------------------------
c SAVE DFE
      ELSE IF(str1.eq.'DFE')then
         call XSL_EXIST(dfefil,status)
         if(status.eq.0) then
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str2,'dfe',status)
            call XSL_COPY(dfefil,str2,status)
            if(status.eq.0)then
               usrdfe = dfefil
            endif
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         else
            call XWRITE('No DFE file made',5)
         endif
c ---------------------------------------------
c SAVE SPECTRUM
      ELSE IF(str1.eq.'SPECTRUM')then
         IF( SPEC )then
            status = 0
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
c Bin the spectrum first, Note, GROUP = .FALSE. means the user will
c get prompted whether to group or not:
            GROUP = .FALSE.
            call XSL_GRPSAV(str2,0)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE(' No spectrum has been accumulated yet.',5)
         ENDIF
c ----------------------------------------------
c SAVE EVENTS
      ELSE IF(str1.eq.'EVENTS') THEN
         IF(BINOUT)THEN
            call  XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_SAVE_EVENTS(str2,mode)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE('Output events file from extract '//
     &           'not found.',5)
         ENDIF
c ----------------------------------------
c SAVE IN_EVENTS

      ELSE IF(str1.eq.'IN_EVENTS') THEN
         IF(BININ) THEN
            call  XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str2,'-evt',status)
            call XSL_COPY(evnin,str2,status)
            IF(status.eq. -10) THEN
               call XWRITE('No files written',5)
               status = 0
            ELSE IF(status.ne.0) THEN
               call XWRITE('INPUT event list not found.',5)
               status = 0
               return
            ELSE
               call XWRITE('No product event list for '//
     &              'extract found.',5)
            ENDIF
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ENDIF
c ---------------------------------------------
c SAVE SESSION
      ELSE IF(str1.eq.'SESSION') THEN
         call XSL_RMFILE(savfil)
         call GETLUN(ilun)
         call XSL_OPEN(ilun,savfil,'NEW',' ',' ',0,0,ierr)     
c     First write the logicals:
         str2 = 
     &        '  READ      SPEC      CURV      '//
     &        'IMAGE     SELCT    MERGED '
         write(ilun,15) str2(1:58)
         write(ilun,26) READ,SPEC,CURV,IMAGE,SELCT,MERGED
         str2 = 
     &        ' USEGTI    MERGTI     USEHK    HKREAD    '//
     &        'MANYHK    MERGHK '
         write(ilun,15) str2(1:58)
         write(ilun, 26) USEGTI,MERGTI,USEHK,HKREAD,MANYHK,MERGHK 
         str2 =
     &        ' EXPAND     MANY      MADE      WORK     '//
     &        'WRKGTI    WORKHK '
         write(ilun,15) str2(1:58)
         write(ilun,26) EXPAND,MANY,MADE,WORK,WRKGTI,WORKHK         
         str2 = ' LOADED   FILTER     HKSEL     CLEAN    CLEANI '
         write(ilun,15) str2(1:47)
         write(ilun,26) LOADED,FILTER,HKSEL,CLEAN,CLEANI
         str2 = ' FITTFL     ASCTFL     XWNTFL     FFTFL  REGION'
         write(ilun,15) str2(1:58)
         write(ilun,26) FITTFL,ASCTFL,XWNTFL,FFTFL,REGION
c     These are the HKBIN logicals:
         write(ilun,16)
         write(ilun,15) 'npar'
         write(ilun,31) npar
         write(ilun,16)
         write(ilun,15) 'HKCURV  parlis'
         do i=1,npar
            len1 = LENACT(parlis(i))
            write(ilun,27) HKCURV,parlis(i)(1:len1)
         enddo
c     These are the MKFBIN logicals:
         write(ilun,16)
         write(ilun,15) 'nffpar'
         write(ilun,31) nffpar
         write(ilun,16)
         write(ilun,15) 'FFCURV  ffplis'
         do i=1,nffpar
            len1 = LENACT(ffplis(i))
            write(ilun,27) FFCURV,ffplis(i)(1:len1)
         enddo
         write(ilun,16)
         
         str2 = 'datdir'
         write(ilun,15) str2(1:6)
         len1 = LENACT(datdir)
         write(ilun,15) datdir(1:len1)
         write(ilun,16)
         
         str2 = 'hkdir'
         write(ilun,15) str2(1:6)
         len1 = LENACT(hkdir)
         write(ilun,15) hkdir(1:len1)
         write(ilun,16)
         
         str2 = 'plotdv'
         write(ilun,15) str2(1:6)
         len1 = LENACT(plotdv)
         write(ilun,15) plotdv(1:len1)
         
         str2 = 'instru'
         write(ilun,15) str2(1:6)
         len1 = LENACT(instru)
         write(ilun,15) instru(1:len1)
         
         str2 = 'prefix'
         write(ilun,15) str2(1:6)
         len1 = LENACT(prefix)
         write(ilun,15) prefix(1:len1)

         str2 = 'catnam'
         write(ilun,15) str2(1:6)
         len1 = LENACT(catnam)
         write(ilun,15) catnam(1:len1)

         str2 = 'catflt'
         write(ilun,15) str2(1:6)
         len1 = LENACT(catflt)
         write(ilun,15) catflt(1:len1)

         str2 = 'choflt'
         write(ilun,15) str2(1:6)
         len1 = LENACT(choflt)
         write(ilun,15) choflt(1:len1)

         str2 = 'catsel'
         write(ilun,15) str2(1:6)
         len1 = LENACT(catsel)
         write(ilun,15) catsel(1:len1)

         str2 = 'ffilin'
         write(ilun,15) str2(1:6)
         len1 = LENACT(ffilin)
         write(ilun,15) ffilin(1:len1)

         str2 = 'ffstr'
         write(ilun,15) str2(1:6)
         len1 = LENACT(ffstr)
         write(ilun,15) ffstr(nffstr)(1:len1)

         str2 = 'hkstr'
         write(ilun,15) str2(1:6)
         len1 = LENACT(hkstr)
         write(ilun,15) hkstr(1:len1)

         str2 = ' phalcut phahcut'
         write(ilun,15) str2(:16)
         write(ilun,32) phalcut,phahcut

         str2 = ' phabin'
         write(ilun,15) str2(:7)
         write(ilun,32) phabin

         str2 = ' xbinf'
         write(ilun,15) str2(:7)
         write(ilun,32) xbinf

         str2 = ' binsiz'
         write(ilun,15) str2(:8)
         write(ilun,41) binsiz

         str2 = ' numreg  numasc  numgti'
         write(ilun,15) str2(1:22)
         write(ilun,32) numreg,numasc,numgti
         
         IF(numreg.gt.0) THEN
            str2 = 'region files'
            write(ilun,15) str2(1:12)
            do i=1,numreg
               len1 = LENACT(regvec(i))
               write(ilun,15) regvec(i)(1:len1)
            enddo
            write(ilun,16)
         ENDIF
         
         IF(numasc.gt.0) THEN
            str2 = 'ascii timing files'
            write(ilun,15) str2(1:18)
            do i=1,numasc
               len1 = LENACT(ascvec(i))
               write(ilun,15) ascvec(i)(1:len1)
            enddo
            write(ilun,16)
         ENDIF
         
         IF(numgti.gt.0) THEN
            str2 = 'gti timing files'
            write(ilun,15) str2(1:16)
            do i=1,numgti
               len1 = LENACT(gtivec(i))
               write(ilun,15) gtivec(i)(1:len1)
            enddo
            write(ilun,16)
         ENDIF
         
         
         str2 = ' obsno  nfiles  nhkfil'
         write(ilun,15) str2(1:22)
         write(ilun,32) obsno,nfiles,nhkfil
         
         str2 = 'catidx'
         write(ilun,15) str2(1:6)
         do i=1,nfiles
            write(ilun,31) catidx(i)
         enddo
         write(ilun,16)

         str2 = 'filenm'
         write(ilun,15) str2(1:6)
         do i=1,nfiles
            len1 = LENACT(filenm(i))
            write(ilun,15) filenm(i)(1:len1)
         enddo
         write(ilun,16)
         
         str2 = 'gtifnm'
         write(ilun,15) str2(1:6)
         do i=1,nfiles
            len1 = LENACT(gtifnm(i))
            write(ilun,15) gtifnm(i)(1:len1)
         enddo
         write(ilun,16)
         
         str2 = 'hkflnm'
         write(ilun,15) str2(1:6)
         do i=1,nhkfil
            len1 = LENACT(hkflnm(i))
            write(ilun,15) hkflnm(i)(1:len1)
         enddo
         write(ilun,16)

C     I am adding stuff at the end, this is for version .94.  I add it 
C     here because I don't want to have to write a converter.
C     When I get the time I will move it to a more logical place.

         str2 = ' xbinh'
         write(ilun,15) str2(:7)
         write(ilun,32) extrbinh
         write(ilun,16)
C     More stuff:
         str2 = '   xcolf       ycolf       xfkey      yfkey'
         write(ilun,15) str2(:LENACT(str2))
         write(ilun,33) xcolf(:10),ycolf(:10),xfkey(:10),yfkey(:10)
         write(ilun,16)        
         
         str2 = '   xcolh       ycolh       xhkey      yhkey'
         write(ilun,15) str2(:LENACT(str2))
         write(ilun,33) xcolh(:10),ycolh(:10),xhkey(:10),yhkey(:10)
         write(ilun,16)        
         
         str2 = ' keymis'
         write(ilun,15) str2(:7)
         write(ilun,15) keymis(:LENACT(keymis))
         write(ilun,16)

         str2 = ' keypha'
         write(ilun,15) str2(:7)
         write(ilun,15) keypha(:LENACT(keypha))
         write(ilun,16)

         str2 = ' UNBIN    BINOUT     BININ'
         write(ilun,15) str2(:6)
         write(ilun,26) UNBIN,BINOUT,BININ
         write(ilun,16)

         write(ilun,15) 'numcti'
         write(ilun,31)  numcti
         write(ilun,16)

         write(ilun,15) 'XPHTFL  nphase          epoch'//
     &        '               period'   
         write(ilun,34) XPHTFL,nphase,epoch,period
         write(ilun,15) 'start   stop'
         do i=1,nphase
            write(ilun,35) phase(1,i),phase(2,i)
         enddo

         write(ilun,16)
         write(ilun,15) 'WTMAPB    FAST   WTMAPFIX   SWMAPX   SWMAPY'  
         write(ilun,'(5(L1,4x))') WTMAPB,FAST,WTMAPFIX,SWMAPX,SWMAPY

         write(ilun,16)
         write(ilun,15) 'SMOOTH SAVSMOOTH'  
         write(ilun,'(L1,4x,L1)') SMOOTH, SAVSMOOTH
         write(ilun,15) 'SMETHOD     BOUND   CONST'
         write(ilun,'(a10,2x,a10,2x,i4)') smethod,bound,const
         write(ilun,15) 'SIGMA  NSIGMA' 
         write(ilun,'(f7.2,2x,i4)') sigma,nsigma
         write(ilun,15) 'XWINDOW  YWINDOW'
         write(ilun,'(f7.2,2x,f7.2)') xwindow,ywindow

c this is the ffstr information again, since we didn't get it all before.
c The reason? Cause it would break pre .099 save sessions

         write(ilun,16)

         write(ilun,15) 'nstrsel nffstr '
         write(ilun,32) nstrsel,nffstr

         str2 = 'strsel'
         write(ilun,15) str2(1:6)
         do i=1,nstrsel
            len1 = LENACT(strsel(i))
            write(ilun,15) strsel(i)(1:len1)
         enddo

         str2 = 'ffstr'
         write(ilun,15) str2(1:6)
         do i=1,nffstr
            len1 = LENACT(ffstr(i))
            write(ilun,15) ffstr(i)(1:len1)
         enddo

         str2 = 'datamode'
         write(ilun,15) str2(1:8)
         write(ilun,15) datamode

         str2 = 'MKFDIR'
         write(ilun,15) str2(:6)
         write(ilun,15) mkfdir(:LENACT(mkfdir))

         str2 = 'TIMEDEL      PHAMIN   PHAMAX'
         write(ilun,15) str2(1:8)
         write(ilun,'(e17.10,4x,i7,4x,i7)') timedel,phamin,phamax

         str2 = 'usrdfe                               FAINT'
         write(ilun,15) str2(1:8)
         write(ilun,'(a)') usrdfe(:LENACT(usrdfe))
         write(ilun,'(L1)') FAINT

         str2 = 'numint   INTENS'
         write(ilun,15) str2(:20)
         write(ilun,'(I6,2x,L1)') numint,INTENS

         str2 = 'intvec, intexp'
         write(ilun,15) str2(:20)
         do i=1,numint
            write(ilun,'(a)') intvec(i)(:LENACT(intvec(i)))
            write(ilun,'(a)') intexp(i)(:LENACT(intexp(i)))
         enddo

         write(ilun,15) 'submis'
         write(ilun,15) submis(1:LENACT(submis))

         write(ilun,15) 'clnmet  cphal  cphah'
         write(ilun,32) clnmet,  cphal,  cphah
         write(ilun,15) 'cellsz   logprb   bkgthr'
         write(ilun,'(I4,2x,f10.5,2x,i4)') cellsz,logprb,bkgthr

         write(ilun,15) 'ccdno   stah  endh  arena  ario  in_or_out'
         write(ilun,'(5(i4,2x),a4)') ccdno,   stah,  endh,  arena,
     &        ario,  in_or_out
         write(ilun,15) 'DIRTY'
         write(ilun,'(L1)') DIRTY

         write(ilun,15) 'numhnd'
         write(ilun,31) numhnd
         write(ilun,15) 'CTINDX   HNDNDX  INTNDX'
         do i=1,MXNSEL
            write(ilun,32) ctindx(i),hndndx(i),intndx(i)
         enddo
         write(ilun,15) 'mkfdnm'
         write(ilun,15) mkfdnm
         write(ilun,15) 'xcf  ycf  sizef  xch   ych   sizeh'
         write(ilun,'(6(I6,2x))') xcf,ycf,sizef,xch,ych,sizeh
         write(ilun,15) ' rxbval  ribval  phbval'
         write(ilun,32) rxbval,ribval, phbval
         write(ilun,15) phamxkwd(:lenact(phamxkwd))
         write(ilun,15) gtinam(:lenact(gtinam))

         str2 = ' keyccd'
         write(ilun,15) str2(:7)
         write(ilun,15) keyccd(:LENACT(keyccd))

         str2 = ' keygrd'
         write(ilun,15) str2(:7)
         write(ilun,15) keygrd(:LENACT(keygrd))
         str2 = ' gfilter'
         write(ilun,15) str2(:8)
         write(ilun,15) gfilter(:LENACT(gfilter))

         str2 = ' timesys'
         write(ilun,15) str2(:8)
         write(ilun,15) timesys(:LENACT(timesys))
         str2 = ' mjdrefi mjdreff'
         write(ilun,15) str2(:16)
         write(ilun,42) mjdrefi, mjdreff

         str2 = ' colfilter'
         write(ilun,15) str2(:10)
         write(ilun,15) colfilter(:LENACT(colfilter))


 15      format(a)
 16      format(' ')
 26      format(3x,L1,4x,5(2x,3x,L1,4x),/)
 27      format(3x,L1,4x,2x,a)
 31      format(I4)
 32      format(3(i6,2x),/)
 33      format(4(a10,2x),/)
 34      format(1x,L1,1x,I3,7x,f23.15,3x,f23.15)
 35      format(f8.5,2x,f8.5)
 41      format(e12.5)
 42      format(1x,i6,1x,f23.15)
         
         close(ilun)
         call FRELUN(ilun)
         call XWRITE('Session saved, goodbye',5)
c     First save the Xselect par file:
         ierr = 0
         call xpisavepar(ierr)
c     Then clean up the fdump par file:
         CALL XSL_CLEANPAR()
c     Remove the last run file
         call XSL_RMFILE(cmdfil)
         call XSL_EXIT(status)
         
c ---------------------------------------------
c SAVE CURVE
      ELSEIF(str1.eq.'CURVE')then
         IF ( CURV )then
            status = 0
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_SAVE_CURVE(str2,0)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE(' No curve has been accumulated yet.',5)
         ENDIF
c     ---------------------------------------------
c     SAVE EXPANDED HK FILE
c     

      ELSEIF(str1.eq.'EXPANDED')then
         IF(EXPAND.AND.WORKHK) then
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_COPY(hkwrk1(1),str2,status)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE('HK files are not expanded',5)
         ENDIF
c ---------------------------------------------
c SAVE IMAGE
      ELSEIF(str1.eq.'IMAGE')then
         IF ( IMAGE )then
            status = 0
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_SAVE_IMAGE(str2,0)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE(' No image has been accumulated yet.',5)
         ENDIF

c     
c ---------------------------------------------
c SAVE ACCUMULATED REGION FILE
      ELSEIF(str1.eq.'REGION') THEN
         call XSL_EXIST(regfil,ierr)
         if(ierr.ne.0) then
            call XWRITE('No accumulated region file',5)
            return
         endif
         call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
         contxt = 'Failed to get outfile'
         IF ( status .NE. 0 ) GOTO 99999
         call XSL_XTEND(str2,'-reg',status)
         call XSL_COPY(regfil,str2,status)
         call XSL_XTEND(str2,' ',status)
         call XSL_UCLPST('outfile',str2,status)
c ---------------------------------------------
c SAVE WORKSPACE FILES
c     
      ELSEIF(str1.eq.'SELECTION'.or.str1.eq.'FAINT'.or.str1.eq.'FAST'
     &        .or.str1.eq.'WORKSPACE')then
         IF ( WORK )then
            IF( MANY ) then
               status = 0
               call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
               contxt = 'Failed to get outfile'
               IF ( status .NE. 0 ) GOTO 99999
               call XSL_FAFNA(str2,'.fits',MAXFIL,filout)
               
               do i=1, nfiles
                  call XSL_COPY( work1(i), filout(i) ,status)
               end do
               
            ELSE
               status = 0
               call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
               contxt = 'Failed to get outfile'
               IF ( status .NE. 0 ) GOTO 99999
               call XSL_COPY( work1(1), str2 ,status)
            ENDIF
         ELSEIF(BININ.and.FAST) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str3,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_COPY(evnin,str3,status)
         ELSE
            call XWRITE(' No files are in the workspace.',5)
         ENDIF
c -------------------------------------------------------
c SAVE CLEANED EVENTS FILES
      ELSEIF(str1.eq.'CLEAN') then
         IF (CLEAN) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_SAVE_CLEAN(str2,0)
            call XSL_XTEND(str2,' ',status)
            call XSL_UCLPST('outfile',str2,status)
         ELSE
            call XWRITE
     &           ('No cleaned events files made yet.',5)
            return
         ENDIF         

c -------------------------------------------------------
c SAVE OBSCAT
      ELSE IF(str1.eq.'OBSCAT') THEN
         IF(MADE) THEN
            call XSL_GETDUMMYPARST('outfile','save_str',str3,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str3,'cat',status)
            call XSL_COPY(catnam,str3,status)
            call XSL_XTEND(str3,' ',status)
            call XSL_UCLPST('outfile',str3,status)
         ELSE
            call XWRITE('No catalogue made.',5)
         ENDIF
c -------------------------------------------------------
c SAVE TIME
      ELSE IF(str1.eq.'TIME') THEN

         call XSL_UCLGOT('save_which',status)
         len1 = 0
         call XSL_UCLGOT('save_str',len1)
 913     if(status.ne.0.and.len1.ne.0) then
            call XSL_CMDLIST(time_mod,time_des,4,4)
            len1 = 1
            status = 0
            call xsl_uclgst('save_which',str2,status)
            contxt = 'Failed to get save_which'
            IF ( status .NE. 0 ) GOTO 99999
         ELSE
            len1 = 1
            status = 0
            call XSL_GETDUMMYPARST('save_which','save_str',str2,status)
            contxt = 'Failed to get save_which'
            IF ( status .NE. 0 ) GOTO 99999
         ENDIF
         call UPC(str2)
         call gmatch(time_mod,4,str2,myindex,status)
         IF(status.eq.1) THEN
            call XWRITE('save_which argument not found.',5)
            go to 913
         ELSE IF (status.eq.2) THEN
            call XWRITE('save_which argument not unique',5)
            go to 913
         ENDIF

         IF(str2 .eq. 'QUIT' ) then
            return
         ELSE IF ( str2.eq.'ALL' ) THEN
            call xsl_uclgst('outfile',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            if(numcti.gt.0) then
               call XSL_SAVE_TIME(str2,'cursor','curs_gti',ctivec,
     &              ctindx,numcti,MXNSEL)
            endif
            if (numhnd.gt.0) then
               call XSL_SAVE_TIME(str2,'keyboard','hnd_gti',hndvec,
     &              hndndx,numhnd,MXNSEL)
            endif
         ELSE IF ( str2.eq. 'CURSOR' ) THEN
            call xsl_uclgst('outfile',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            if(numcti.gt.0) then
               call XSL_SAVE_TIME(str2,'cursor','curs_gti',ctivec,
     &              ctindx,numcti,MXNSEL)
            endif
         ELSE IF ( str2.eq. 'KEYBOARD' ) THEN
            call xsl_uclgst('outfile',str2,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            if (numhnd.gt.0) then
               call XSL_SAVE_TIME(str2,'keyboard','hnd_gti',hndvec,
     &              hndndx,numhnd,MXNSEL)
            endif
         ENDIF
         call XSL_XTEND(str2,' ',status)
         call XSL_UCLPST('outfile',str2,status)         

c -------------------------------------------------------
c SAVE GOODTIME
      ELSEIF(str1.eq.'GOODTIME') THEN
         call XSL_EXIST(ascout,status)
         IF(status.ne.0) then
            call XWRITE('No output timing filter found.',5)
            return
         ELSE         
            call XSL_GETDUMMYPARST('outfile','save_str',str3,status)
            contxt = 'Failed to get outfile'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_XTEND(str3,'-gti',status)
            call XSL_COPY(ascout,str3,status)
            call XSL_XTEND(str3,' ',status)
            call XSL_UCLPST('outfile',str3,status)
         ENDIF
      ELSE
         call XWRITE(' SAVE option not found ',5)
      ENDIF

99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_SAVE: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end

c ----------------------------------------
      subroutine XSL_BINOUT_SAV(messg)
c ----------------------------------------

      include 'xsel.inc'
      include 'xselvar.inc'

      character*(*) messg
      character(160) mystr
      integer LENACT
      logical ANSW

      status = 0

      IF (CLEAN) THEN
         mystr = messg(:LENACT(messg))//
     &        ' invalidates the input events list for SISCLEAN.'
      ELSE
         mystr = messg(:LENACT(messg))//
     &        ' invalidates the old output extractor events list.'
      ENDIF
      call XWRITE(mystr(1:80),5)
      IF ( LENACT(mystr) .gt. 80) THEN
         call XWRITE(mystr(81:),5)
      ENDIF

      IF (SELCT) THEN
         call XWRITE('N.B. If you did a select events AFTER '//
     &        'doing extract events',5)
         call XWRITE('you will lose the effects of '//
     &        'this selection in this session.',5)
      ENDIF
      call xsl_uclgsb('save_file',ANSW,status)
      IF(ANSW) THEN
         call XSL_SAVE(2)
      ENDIF
      call XSL_RMFILE(evnout)
      BINOUT = .FALSE.

      return
      end

c ----------------------------------------
      subroutine XSL_SAVE_CLEAN(outfile,mode)
c ----------------------------------------
c This saves the output cleaned events file(s)
c J. Ingham June 94

      include 'xsel.inc'
      include 'xselvar.inc'

      integer mode, LENACT,i
      character(255) str3
      character*(*) outfile
      LOGICAL USEEVT
      logical TVREGION,TVSPEC,TVIMAGE,TVPHACUT,TSMOOTH,TSAVSMOOTH
      logical TVGISCLEAN,TSPEC,TCURV,TIMAGE,TWMAPB,TWMAPFIX
      logical TSWMAPX,TSWMAPY

      IF(index(instru,'SIS').ne.0) THEN
         if(mode.eq.0) then
            call XSL_XTEND(outfile,'-evt',i)
         else
            call XSL_XTEND(outfile,'evt',i)
         endif
         call XSL_COPY(evnin,outfile,status)
         str3 = 'Wrote cleaned events file to '//
     &        outfile(:LENACT(outfile))
C         print*,str3
         call XWRITE(str3,5)
         call XWRITE(' ',5)
      ELSE IF(index(instru,'GIS').ne.0)THEN
         if(WORK) THEN
            if(MANY) THEN
               status = 0
               call XSL_FAFNA(outfile,'.fits',MAXFIL,filout)

               do i=1, nfiles
                  call XSL_COPY( work1(i), filout(i) ,status)
               end do

            ELSE
               status = 0
               call XSL_COPY( work1(1), outfile ,status)
            ENDIF
         ELSE IF(BININ) THEN
            status = 0
            call XSL_COPY( evnin, outfile ,status)
         ENDIF
      ENDIF

c For mode = 2 we DO NOT want to reuse the output events file
      IF(mode.ne.2) THEN
         call xsl_uclgsb('use_events',USEEVT,status)
         IF ( status .NE. 0 ) RETURN
         IF(USEEVT) THEN
c If you want to use the event data, then all the selections,
c and all the data should be cleared.
c First warn the user:
            str3 = 'Changing Data directory from:'
            call XWRITE(str3,5)
            str3 = '    '//datdir(:min(lenact(datdir),len(str3)-4))
            call XWRITE(str3,5)
            str3 = 'to the current working directory.'
            call XWRITE(str3,5)
c Now reset, but save some flags:
            TVREGION    = VREGION
            TVSPEC      = VSPEC
            TVIMAGE     = VIMAGE
            TVPHACUT    = VPHACUT
            TVGISCLEAN  = VGISCLEAN
            TSPEC       = SPEC
            TCURV       = CURV
            TIMAGE      = IMAGE
            TWMAPB      = WTMAPB
            TWMAPFIX    = WTMAPFIX
            TSWMAPX     = SWMAPX
            TSWMAPY     = SWMAPY
            TSMOOTH     = SMOOTH
            TSAVSMOOTH  = SAVSMOOTH


            call XSL_CLEAR(2)

            VREGION   = TVREGION
            VSPEC     = TVSPEC
            VIMAGE    = TVIMAGE
            VPHACUT   = TVPHACUT
            VGISCLEAN = TVGISCLEAN
            SPEC      = TSPEC
            CURV      = TCURV
            IMAGE     = TIMAGE
            WTMAPB    = TWMAPB
            WTMAPFIX  = TWMAPFIX
            SWMAPX    = TSWMAPX
            SWMAPY    = TSWMAPY
            SMOOTH    = TSMOOTH
            SAVSMOOTH = TSAVSMOOTH

            MADE = .FALSE.
c     Reset the pha cuts to the flag value
            phalcut = -20
            phahcut = -20
            LOADED = .FALSE.
            datdir = wrkdir
            filenm(1) = outfile
            call XSL_DATDIR(filenm(1),datdir,0)
c     The extractor puts the GTI extension in the second extension
c     of the output events file:
            gtifnm(1) = filenm(1)(:LENACT(filenm(1)))//'[2]'
            USEGTI = .TRUE.
            nfiles = 1
            READ  = .TRUE.
C Also, the extractor puts the events extension into the first extension:
            evtnum = '1'
            gtinum = '2'
            catnum = '1'
         ELSE
            EVTSAV = .TRUE.
         ENDIF
      ENDIF
      CLNSAV = .TRUE.

      return
      end

c
c
c ----------------------------------------
      subroutine XSL_SAVE_CURVE(outfile,mode)
c ----------------------------------------
c This saves the output lightcurve file
c J. Ingham June 94

      include 'xsel.inc'
      include 'xselvar.inc'

      integer mode
      character(255) str3
      character*(*) outfile
      LOGICAL USEEVT,SAVQDP,CLOBBR


C Preserve the clobber information:
      if ( outfile(1:1) .eq. '!' ) THEN
         CLOBBR = .TRUE.
      else
         CLOBBR = .FALSE.
      endif

      if(USEQDP) THEN
         call xsl_uclgsb('save_qdp',SAVQDP,status)
         IF ( status .NE. 0 ) RETURN
      ELSE
         SAVQDP = .FALSE.
      ENDIF
      IF(SAVQDP)THEN
         call XSL_XTEND(outfile,'lc_qdp',status)
         call XSL_COPY(curfil,outfile,status)
         if(status.eq.0) then
            str3 = 'Wrote QDP light curve to file '//outfile
            call XWRITE(str3,5)
         else
            str3 = 'Failed to write QDP light curve to file '//outfile
            call XWRITE(str3,5)
         endif

         call XSL_XTEND(outfile,'lc',status)
         call XSL_COPY(curfits,outfile,status)
         if(status.eq.0) then
            str3 = 'Wrote FITS light curve to file '//outfile
            call XWRITE(str3,5)
         else
            str3 = 'Failed to write FITS light curve to file '//outfile
            call XWRITE(str3,5)
         endif

      ELSE
         if(mode.eq.0) then
            call XSL_XTEND(outfile,'-lc',status)
         else
            call XSL_XTEND(outfile,'lc',status)
         endif
         if(CLOBBR.and.outfile(1:1) .ne. '!' ) outfile = '!'//outfile
         call XSL_COPY(curfits,outfile,status)
         if(status.eq.0) then
            str3 = 'Wrote FITS light curve to file '//outfile
            call XWRITE(str3,5)
         else
            str3 = 'Failed to write FITS light curve to file '//outfile
            call XWRITE(str3,5)
         endif

      ENDIF
c     Now save the xronow window file
      status = 0
      call xsl_uclgsb('save_xrwin',USEEVT,status)
      IF ( status .NE. 0 ) RETURN
      IF(USEEVT) THEN
         str3 = outfile
         call XSL_XTEND(str3,'wi',status)
         IF(status.ne.0) then
            call XWRITE('Error making window file name,'// 
     &           'file not saved',5)
         ELSE
            if(CLOBBR.and.outfile(1:1) .ne. '!' ) outfile = '!'//outfile
            call XSL_COPY(xronwn,str3,status)
            if(status.eq.0) then
               str3 = 'Wrote Xronos window file to '//outfile
               call XWRITE(str3,5)
            else
               str3 = 'Failed to write Xronos window file to '
     &              //outfile
               call XWRITE(str3,5)
            endif

         ENDIF
      ENDIF


      return
      end
c
c
c ----------------------------------------
      subroutine XSL_SAVE_EVENTS(outfile,mode)
c ----------------------------------------
c This saves the output events file
c J. Ingham June 94

      include 'xsel.inc'
      include 'xselvar.inc'

      integer mode, LENACT
      character*(*) outfile
      character(255) str3
      LOGICAL USEEVT
      logical TVREGION,TVSPEC,TVIMAGE,TVPHACUT,TSMOOTH,TSAVSMOOTH
      logical TVGISCLEAN,TSPEC,TCURV,TIMAGE,TWMAPB,TWMAPFIX
      logical TSWMAPX,TSWMAPY

      IF(CLEAN.and.instru(1:3).eq.'SIS') THEN
         if(mode.eq.1) then
            call XSL_XTEND(outfile,'xsl_ucl',status)
         else
            call XSL_XTEND(outfile,'-xsl_ucl',status)
         endif
      ELSE
         if(mode.eq.1) then
            call XSL_XTEND(outfile,'evt',status)
         else
            call XSL_XTEND(outfile,'-evt',status)
         endif
      ENDIF
      call XSL_COPY(evnout,outfile,status)
      IF( status.eq.0) then
         str3 = 'Wrote events list to file '//outfile
         call XWRITE (str3,5)
      ELSE IF(status.eq. -10) THEN
         call XWRITE('No files written',5)
         return
      ELSE IF(status.ne.0) THEN
         call XWRITE('Output event list not found.',5)
         return
      ENDIF

c For mode = 2 we DO NOT want to reuse the output events file
      IF(mode.ne.2) THEN
         call xsl_uclgsb('use_events',USEEVT,status)
         IF ( status .NE. 0 ) RETURN
         IF(USEEVT) THEN
c If you want to use the event data, then all the selections,
c and all the data should be cleared.
c First warn the user:
            str3 = 'Changing Data directory from:'
            call XWRITE(str3,5)
            str3 = '    '//datdir(:min(lenact(datdir),len(str3)-4))
            call XWRITE(str3,5)
            str3 = 'to the current working directory.'
            call XWRITE(str3,5)
c Now reset, but save some flags:
            TVREGION    = VREGION
            TVSPEC      = VSPEC
            TVIMAGE     = VIMAGE
            TVPHACUT    = VPHACUT
            TVGISCLEAN  = VGISCLEAN
            TSPEC       = SPEC
            TCURV       = CURV
            TIMAGE      = IMAGE
            TWMAPB      = WTMAPB
            TWMAPFIX    = WTMAPFIX
            TSWMAPX     = SWMAPX
            TSWMAPY     = SWMAPY
            TSMOOTH     = SMOOTH
            TSAVSMOOTH  = SAVSMOOTH

            call XSL_CLEAR(2)

            VREGION   = TVREGION
            VSPEC     = TVSPEC
            VIMAGE    = TVIMAGE
            VPHACUT   = TVPHACUT
            VGISCLEAN = TVGISCLEAN
            SPEC      = TSPEC
            CURV      = TCURV
            IMAGE     = TIMAGE
            WTMAPB    = TWMAPB
            WTMAPFIX  = TWMAPFIX
            SWMAPX    = TSWMAPX
            SWMAPY    = TSWMAPY
            SMOOTH    = TSMOOTH
            SAVSMOOTH = TSAVSMOOTH

            MADE = .FALSE.
c     Reset the pha cuts to the flag value
            phalcut = -20
            phahcut = -20
            LOADED = .FALSE.
            datdir = wrkdir
            filenm(1) = outfile
            call XSL_DATDIR(filenm(1),datdir,0)
c     The extractor puts the GTI extension in the second extension
c     of the output events file:
            gtifnm(1) = filenm(1)(:LENACT(filenm(1)))//'[2]'
            USEGTI = .TRUE.
            nfiles = 1
            READ  = .TRUE.
C Also, the extractor puts the events extension into the first extension:
            evtnum = '1'
            gtinum = '2'
            catnum = '1'
         ELSE
            EVTSAV = .TRUE.
         ENDIF
      ENDIF

      return
      end


c
c
c ----------------------------------------
      subroutine XSL_SAVE_IMAGE(outfile,mode)
c ----------------------------------------
c This saves the output events file
c J. Ingham June 94

      include 'xsel.inc'
      include 'xselvar.inc'

      integer mode
      character*(*) outfile
      character(255) str1
      logical CLOBBR

      if(outfile(1:1).eq. '!') then
         CLOBBR = .TRUE.
      else
         CLOBBR = .FALSE.
      endif

      if(mode.eq.0) then
         call XSL_XTEND(outfile,'-img',status)
      else
         call XSL_XTEND(outfile,'img',status)
      endif
      call XSL_COPY(imfil,outfile,status)
      if(status.eq.0) then
         str1 = 'Wrote image to file '//outfile
         call XWRITE (str1,5)
      else
         str1 = 'Failed writing image to file '//outfile
         call XWRITE (str1,5)
      endif

      IF(SMOOTH) THEN
         call xsl_uclgsb('save_smooth',SAVSMOOTH,status)
         IF ( status .NE. 0 ) RETURN
         IF(SAVSMOOTH) THEN
            CALL XSL_XTEND(outfile,'smooth',status)
C This is required since the previous XSL_COPY strips the leading !
            IF ( CLOBBR ) THEN
               outfile = '!'//outfile
            ENDIF
            call XSL_COPY(simfil,outfile,status)
            if(status.eq.0) then
               str1 = 'Wrote smoothed image to file '//outfile
               call XWRITE (str1,5)
            else
               str1 = 'Failed writing smoothed image to file '//outfile
               call XWRITE (str1,5)
            endif

         ENDIF
      ENDIF

      return
      end

c
c
c --------------------------------------------------
      subroutine XSL_SAVE_TIME(outfile,kind,exten,ctivec,
     &     ctindx,numcti,MXNSEL)
c --------------------------------------------------
c This saves the CTI or HND files.  I use the names from the CTI,...
c If CTINDX>0, this is a real file to be saved...
c Outfile is the base name
c J. Ingham 9/13/94

      integer MXNSEL
      character*(*) outfile,ctivec(MXNSEL),kind,exten
      integer ctindx(MXNSEL),numcti,LENACT,status,len1

      character(256) str1
      character(36) str2
      character(256) tmpvec(32)
      integer i

      if(MXNSEL.gt.32) THEN
         call XWRITE('tmpvec too small in XSL_SAVE_TIME',5)
         return
      endif

      IF(numcti.eq.0) THEN
         str1 = 'No '//kind(:LENACT(kind))//
     &        ' selection files entered yet.'
         call XWRITE(str1,5)
         return
      ELSE IF(numcti.eq.1) THEN
         call XSL_XTEND(outfile,exten,status)
         status = 0
         do i=1,MXNSEL
            if(ctindx(i).gt.0) then
               call XSL_COPY(ctivec(i),outfile,status)
               goto 455
            endif
         enddo
 455     continue
      ELSE
         write(str1,77) numcti
 77      format('There are ',i3,' files.')
         call XWRITE(str1,5)
         call XWRITE('They will be named sequentially from '//
     &        'the given base name.',5)
         str1 = 'A default extension of .'//exten(:LENACT(exten))//
     &        'will be added.'
         call XWRITE(str1 ,5)
         call XSL_GETEXT(outfile,str2)
         call XSL_FAFNA(outfile,exten,MXNSEL,tmpvec)
         do i=1,numcti
            if(ctindx(i).gt.0) then
               status = 0
               len1 = LENACT(ctivec(i))
               call xsl_filename(ctivec(i),str1,len1)
               call XSL_COPY(str1,tmpvec(i),status)
            endif
         enddo
      ENDIF

      return
      end

c
c
c ----------------------------------------
      subroutine XSL_SAVE_WS_OR_EVNIN()
c ----------------------------------------
c This allows the user to save workspace or evnin files if they
c might be overwritten

      include 'xsel.inc'
      include 'xselvar.inc'

      logical ANSW
c The exception is if BININ is true, in which case we assume the
c selection acts on that, and overwrite it, so...
c First give the user a chance to save them:
      status = 0
      IF(BININ) THEN
         IF(CLEAN) THEN
            call XWRITE('The select output will '//
     &           'overwrite the cleaned event list',5) 
            call XWRITE('     if you have not saved it already.',5)
         ELSE
            call XWRITE('The select output will '//
     &           'overwrite input event list for extract ',5)
            call XWRITE('   if you have not saved it already.',5)
         ENDIF
         status = 0
         call xsl_uclgsb('save_file', ANSW, status)
         IF(status.ne.0) THEN
            call XWRITE('Reply not understood, continuing',5)
            ANSW = .FALSE.
            status = 0
         ENDIF
         IF(ANSW) THEN
            IF(CLEAN) THEN
               call XSL_SAVE(3)
            ELSE
               call XSL_SAVE(4)
            ENDIF
         ENDIF
      ELSE IF(WORK) THEN
         call XWRITE('The file(s) already in the workspace'//
     &        ' will be overwritten.',5)
         call xsl_uclgsb('save_file',ANSW,status)
         IF(status.ne.0) THEN
            call XWRITE('Reply not understood, continuing',5)
            ANSW = .FALSE.
            status = 0
         ENDIF
         IF(ANSW) THEN
            call XSL_SAVE(5)
         ENDIF
      ENDIF


      return
      end

