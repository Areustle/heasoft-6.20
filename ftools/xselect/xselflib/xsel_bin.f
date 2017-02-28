c
c Split out from xsel_extract.f to try to make it easier to track
c
c ---------------------------------------------
      subroutine XSL_BIN(mode)
c ---------------------------------------------
c Routine to bin spectrum, light curve, image.
c Called by XSELECT main
c
c This is the new version of the binning subroutine,
c which uses the EXTRACTOR to do the binning.
c
c Currently implemented:
c
c     EXTRACT SPECTRUM
c     EXTRACT CURVE
c     EXTRACT IMAGE
c     EXTRACT EVENTS
c     EXTRACT ALL    (Bins CURVE, SPECTRUM and IMAGE simultaneously)
c
c  BIN may be used as a synonym for EXTRACT
c
c     Alan Smale  1992 Nov, 1993 March
c     Jim Ingham  1993 June ->
c     Mod. by Jeff Guerber, HSTX, Aug 1997. Replaced xsl_extstrip with fcpars.

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1, str2, str3, str4, str5
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings
      integer len1, len2, len4, len5, len6
c ---------------------------------------------
      integer MAXITM,mode
      parameter (MAXITM = 5)
      character(255) tmpvec(MAXITM),tmpnam,dumnam,gtilst
      character(255) tspecname, tevtname, extftool
      character(255) contxt, wrtstr
      character(2048) str1lon
      real tbinsz
      integer LENACT
      integer i,ninvec,len7, loside, hiside
      integer tphabn,txbinf,tphalc,tphahc,txcf,tycf,tsizef
      integer iwmapver
      double precision lcthr

c Local logicals to control creation of products

      logical DOEVNT, DOCURV, DOSPEC, DOIMAG, DOLCEV, CONTINUE
      logical FULLIMAGE, COPYALL, LCTZERO

c This is the command list stuff
      integer MXCMDS
      parameter (MXCMDS = 20 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      integer nwhats,myindex
      data commad /'ALL',
     &     'CURVE',
     &     'EVENTS',
     &     'IMAGE',
C     &     'LCEVENTS',
     &     'SPECTRUM',
     &     15*'NA'/
      data comdes /'extracta spectrum, image and light curve',
     &     'extract a light curve',
     &     'extract a filtered event list',
     &     'extract an image',
C     &     'extract the lightcurve for Xronos input',
     &     'extract a spectrum',
     &     15*'NA'/
      data nwhats / 5 /

c First set all the logicals to false
      DOEVNT = .FALSE.
      DOCURV = .FALSE.
      DOIMAG = .FALSE.
      DOSPEC = .FALSE.
      DOLCEV = .FALSE.

c Initialize temporary variables

      tbinsz = 0.0
      tphabn = 0
      tphahc = 0
      tphalc = 0
      tsizef = 0
      txbinf = 0
      txcf = 0
      tycf = 0

c Initialize status

      status=0

c The modes are for:
c 0 -- This is the normal operation when called from the command line
c 1 -- This bins up an image in the key{x,y}(2) coordinates
c      This is used for the SISCLEAN method 1 command
c 2 -- This outputs a filtered event list, using the key{x,y}(2) coord.
c      This is used in the SISCLEAN method 1 command
c 3 -- This outputs the filtered event list.
c 4 -- This outputs the filtered event list + an image in key{x,y} (2).

      IF(mode.eq.0)THEN
c THis gives the help string:
         status = 1
         IF(comno.eq.0) THEN
            call XSL_CMDLIST(commad,comdes,nwhats,MXCMDS)
         ENDIF
         IF (status .NE. 0 ) THEN
            status=0
            call xsl_uclgst('bin_what',str1,status)
            contxt = 'Failed to get bin_what'
            IF ( status .NE. 0 ) GOTO 99999
            call UPC(str1)
            CALL XSL_PARSE(str1,tmpvec,ninvec,MAXITM,status)
            IF(status.ne.0) THEN
               call XWRITE('Too many responses for extract',5)
               status = 0
               goto 999
            ENDIF
            do i=1,ninvec
               call gmatch(commad,nwhats,tmpvec(i),myindex,status)
               IF(status.eq.1) THEN
                  call XWRITE('bin_what argument '//
     &                 tmpvec(i)(:LENACT(tmpvec(i)))//
     &                 ' not found. Options are :',5)
               ELSE IF (status.eq.2) THEN
                   call XWRITE('bin_what argument '//
     &                 tmpvec(i)(:LENACT(tmpvec(i)))//
     &                 ' not unique. Options are :',5)
               ENDIF
            enddo
            IF ( status .NE. 0 ) THEN
                call XSL_CMDLIST(commad,comdes,nwhats,MXCMDS)
            ENDIF
         ENDIF
         IF ( status .NE. 0 ) THEN
            status = 0
            return
         ENDIF
      ELSE IF(mode.eq.1) THEN
         str1 = 'IMAGE'
         tmpvec(1) = str1
         ninvec = 1
      ELSE IF (mode.eq.2.or.mode.eq.4) THEN
         str1 = 'EVENTS IMAGE'
         ninvec = 2
         tmpvec(1) = 'EVENTS'
         tmpvec(2) = 'IMAGE'
      ELSE IF (mode.eq.3) THEN
         str1 = 'EVENTS'
         tmpvec(1) = str1
         ninvec = 1
      ENDIF

c This quits out of bin:
      IF(str1(1:1).eq.'Q') THEN
         return
      ENDIF

c If there is a filtered event list from SISCLEAN, use it.

      IF (BININ) THEN
         call XSL_EXIST(evnin,status)

         IF(status.ne.0) THEN
            call XWRITE('Input file for extract not found',5)
            BININ = .FALSE.
            return
         ENDIF
      ENDIF


c Remove the output timing selection so extractor won't prompt you
      call XSL_RMFILE(ascout)
c Now get the temporary parameters if they are entered:
c (-1 is the flag they have not been)
c The old values will be reset at the end
      tbinsz = binsiz
      call xsl_uclgsr('binsize_t',binsiz,status)
      contxt = 'Failed to get binsize_t'
      IF ( status .NE. 0 ) GOTO 99999
      IF(binsiz.eq.-1.0) THEN
         binsiz = tbinsz
      ENDIF
      tphabn = phabin
      call xsl_uclgsi('pharebin_t',phabin,status)
      contxt = 'Failed to get pharebin_t'
      IF ( status .NE. 0 ) GOTO 99999
      IF(phabin.eq.-1)THEN
         phabin = tphabn
      ENDIF
      txbinf = xbinf
      call xsl_uclgsi('xybinsize_t',xbinf,status)
      contxt = 'Failed to get xybinsize_t'
      IF ( status .NE. 0 ) GOTO 99999
      IF(xbinf.eq.-1) THEN
         xbinf = txbinf
      ENDIF
      tphalc = phalcut
      call xsl_uclgsi('phalcut_t',phalcut,status)
      contxt = 'Failed to get phalcut_t'
      IF ( status .NE. 0 ) GOTO 99999
      IF(phalcut.eq.-1) THEN
         phalcut = tphalc
      ENDIF
      tphahc = phahcut
      call xsl_uclgsi('phahcut_t',phahcut,status)
      contxt = 'Failed to get phahcut_t'
      IF ( status .NE. 0 ) GOTO 99999
      IF(phahcut.eq.-1) THEN
         phahcut = tphahc
      ENDIF

      txcf = xcf
      call xsl_uclgsi('xcenter_t',xcf,status)
      contxt = 'Failed to get xcenter_t'
      IF ( status .NE. 0 ) GOTO 99999
      IF(xcf.eq.-1) THEN
         xcf = txcf
      ENDIF

      tycf = ycf
      call xsl_uclgsi('ycenter_t',ycf,status)
      contxt = 'Failed to get ycenter_t'
      IF ( status .NE. 0 ) GOTO 99999
      IF(ycf.eq.-1) THEN
         ycf = tycf
      ENDIF

      tsizef = sizef
      call xsl_uclgsi('xysize_t',sizef,status)
      contxt = 'Failed to get xysize_t'
      IF ( status .NE. 0 ) GOTO 99999
c This one has to be -2 since -1 is a legit value...
      IF(sizef.eq.-2) THEN
         sizef = tsizef
      ENDIF

c Check the binsize to see if it is legit.
      call XSL_CHKBINSIZE()
      if(status.eq.-5) then
         status = 0
         call xsl_uclgsb('continue',CONTINUE,status)
         contxt = 'Failed to get continue'
         IF ( status .NE. 0 ) GOTO 99999
         IF(.NOT.CONTINUE) THEN
            status = -5
            return
         ENDIF
      else if( status .ne. 0 ) then
         status = -10
         return
      endif


c ---------------------------------------------
c NEWBIN SPECTRUM / CURVE / IMAGE / ALL
c

      IF ( .NOT.READ ) then
         contxt = 'No data has been read in yet'
         status = -5
         GOTO 999
      ENDIF

c Get the type of extractor to be used

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'extract', 
     &              extftool, status)
      contxt = 'Failed to get extract entry from MDB'
      IF ( status .NE. 0 ) GOTO 99999

c Set up the logicals. 

      do i=1,ninvec
         if(tmpvec(i)(1:1).eq.'C')then
            DOCURV = .TRUE.
         else if(tmpvec(i)(1:1).eq.'I')then
            if ( VIMAGE .and. extftool .EQ. 'extractor' ) THEN
               DOIMAG = .TRUE.
            else
               if ( extftool .NE. 'extractor' ) then
                  wrtstr = 'Cannot make images from '//
     &                     instru(:lenact(instru))//' in '//
     &                     datamode(:lenact(datamode))//' mode'
                  call XWRITE(wrtstr, 5)
               else if (.NOT.VIMAGE) then
                  call XWRITE('Images from this data '//
     &                        'would not be valid.',5)
                  call XWRITE('No image made',5)
               endif
            endif
         else if(tmpvec(i)(1:1).eq.'S')then
            if(VSPEC) then
               DOSPEC = .TRUE.
            else
               call XWRITE('Spectra from this data '//
     &                     'would not be valid.',5)
               call XWRITE('No spectrum made',5)
            endif
         else if(tmpvec(i)(1:1).eq.'A')then
            DOCURV = .TRUE.
            if(VIMAGE.and.extftool .EQ. 'extractor' ) DOIMAG = .TRUE.
            if(VSPEC) DOSPEC = .TRUE.
         else if(tmpvec(i)(1:1).eq.'E')then 
            if( extftool .EQ. 'extractor' ) then
               DOEVNT = .TRUE.
            else
               wrtstr = 'Cannot extract events from '//
     &                  instru(:lenact(instru))//' in '//
     &                  datamode(:lenact(datamode))//' mode'
               call XWRITE(wrtstr, 5)
               status = -10
               return
            endif
         else if(tmpvec(i)(1:1).eq.'L')then 
            DOLCEV  = .TRUE.
            DOCURV = .TRUE.
         else
            call XWRITE('Unrecognized option in bin',5)
         endif
      enddo
      IF(.NOT.DOEVNT.and. .NOT.DOIMAG .and. .NOT. DOSPEC .and.
     &   .NOT. DOCURV ) then
         call XWRITE('There is nothing for me to do.',5)
         return
      endif
c Delete previous files and Reset the logicals

      if( DOCURV.or.DOLCEV )THEN
         call XSL_RMFILE( curfil )
         call XSL_RMFILE(curfits)
         CURV = .FALSE.
         call XSL_RMFILE( unbfil )
         UNBIN = .FALSE.
      endif
      if( DOIMAG ) THEN
         call XSL_RMFILE(  imfil )
         IMAGE = .FALSE.
c Also it invalidates the old smoothed image:
         SMOOTH = .FALSE.
         SAVSMOOTH = .FALSE.
         call XSL_RMFILE(simfil)
      endif
      if( DOSPEC ) THEN
         call XSL_RMFILE( spcfil )
         SPEC = .FALSE.
      endif

c If there is an output file from a previous run of extract use it
c by copying it over to the work2 space...
c If there is a BININ file (from SISCLEAN...) then use it instead.
      IF(DOEVNT) THEN
         EVTSAV = .FALSE.
         IF ( BININ ) THEN
            BINOUT = .FALSE.
            call XSL_RMFILE(evnout)
         ELSE IF ( BINOUT ) THEN
            len2 = LENACT(work2(1))
            call XSL_RENAME(evnout,work2(1),1,str4,len2,status)
         ELSE
C This is just in case an old half-written one if hanging around
C from a failed extract run.
            call XSL_RMFILE(evnout)
         ENDIF
      ENDIF

c First do any mission-specific stuff that is required prior to the 
c extract

      CALL XSL_PRE_EXTR()
c set up and run EXTRACTOR command. No merge is now necessary with this
c task. Allow for cases where data is merged, or selection has been
c done. The BININ has priority. First do the SAEXTRCT or SEEXTRCT cases.

      IF ( extftool .EQ. 'saextrct' .OR. 
     &     extftool .EQ. 'seextrct' ) THEN

c Now add on the timing filters:
c First the gti files
         IF(FITTFL.or.HKSEL.or.FFTFL) then
            len1 = LENACT(str1lon)
            len2 = LENACT(gtiflt)
            call XSL_FILENAME(gtiflt,dumnam,len2)
            str3 = 'gtiandfile = "@'
     &           //dumnam(:len2)//'"' 
         ELSE
            str3 = ' gtiandfile = "-"'
         ENDIF
                        
c Now we pass the results to the saextrct task:
c     First remove the old files if present:
         len1 = LENACT(prefix)
         tmpnam = prefix(:len1)//'_tmpfillc.fits'
         call XSL_RMFILE(tmpnam)
         tmpnam = prefix(:len1)//'_tmpfilsp.fits'
         call XSL_RMFILE(tmpnam)
         tmpnam = prefix(:len1)//'_tmpfil'
         gtilst = prefix(:len1)//'_gtilst.tmp'
         call XSL_RMFILE(gtilst)

c     Then build the list of files that will be passed:
         call XSL_RMFILE(lstfil)
         call GETLUN(ilun)
         call XSL_OPEN(ilun,lstfil,'NEW',' ',' ',0,0,ierr)
         IF(WORK) THEN
            do i=1,nfiles
               len1=LENACT( work1(i) )
               str1 = work1(i)(:len1 )//'+'//evtnum
               write(ilun,53) str1(1:len1+4 )
            end do
         ELSE
            do i=1,nfiles
               len1=LENACT( filenm(i) )
               str1 = filenm(i)(:len1 )//'+'//evtnum
               write(ilun,53) str1(1:len1+4 )
            end do
         ENDIF
         close(ilun)
         call FRELUN(ilun)
            
         len1 = LENACT(lstfil)
         call XSL_FILENAME(lstfil,dumnam,len1)

c Now we need to build up a list of the GTI extensions:

         IF ( USEGTI ) THEN         
            call GETLUN(ilun)
            call XSL_OPEN(ilun,gtilst,'NEW',' ',' ',0,0,ierr)
            IF(WRKGTI) THEN
               do i=1,nfiles
                  write(ilun,53) gtiwk1(i)(:LENACT( gtiwk1(i) ))
               end do
            ELSE
               do i=1,nfiles
                  len1=LENACT( gtifnm(i) )
                  write(ilun,53) gtifnm(i)(:len1 ) 
               end do
            ENDIF
            close(ilun)
            call FRELUN(ilun)
            str5 = 'gtiorfile = "@'//gtilst(:LENACT(gtilst))//'" '
         ELSE         
            str5 = 'gtiorfile = "-" '
         ENDIF        

         IF(phalcut.eq.-20.or.phahcut.eq.-20) then
            write(str2,498) binsiz
         ELSE
            write(str2,499) binsiz,phalcut,phahcut
         ENDIF
 498     format(' binsz = ',1pe14.7,' chmin = INDEF chmax = INDEF ',
     &          ' chint = INDEF chbin = INDEF')
 499     format(' binsz = ',1pe14.7,' chmin = ',I8,
     &          ' chmax = ',I8,' chint = INDEF chbin = INDEF')
         IF(DOCURV.and.DOSPEC) THEN
            str2 = str2(:LENACT(str2))//
     &           ' printmode=BOTH lcmode=RATE spmode=SUM'
         ELSE IF(DOCURV) THEN
            str2 = str2(:LENACT(str2))//
     &           ' printmode=LIGHTCURVE lcmode=RATE spmode=SUM'
         ELSE IF(DOSPEC) THEN
            str2 = str2(:LENACT(str2))//
     &           ' printmode=SPECTRUM lcmode=RATE spmode=SUM'
         ENDIF

C Get the thresholding parameter, write it and timemin & timemax:
         call xsl_uclgsd('exposure',lcthr,status)
         contxt = 'Failed to get exposure'
         IF ( status .NE. 0 ) GOTO 99999
         write(str4,511) lcthr
 511     format('timemin = INDEF timemax = INDEF mfracexp = ',
     &          1pe14.7)
         str2 = str2(:LENACT(str2))//' '//str4

         if(XPHTFL) then
            str4 = 'phasefile = '//xphflt(:LENACT(xphflt))
         else
            str4 = 'phasefile = "-"'
         endif

C set any detector filter selection

         IF ( DETFL ) THEN
            str1 = '@'//detfil(:LENACT(detfil))
         ELSE
            str1 = keypha(:LENACT(keypha))
         ENDIF

         IF ( extftool .EQ. 'saextrct' ) THEN

            str1lon= 'saextrct infile = @'//dumnam(:LENACT(dumnam))//
     &               ' '//str5(:LENACT(str5))//' '//
     &               str3(:LENACT(str3))//' gticols = "START STOP" '//
     &               'outroot = '//tmpnam(:LENACT(tmpnam))//
     &               ' extenpha = "sp.fits" extenlc = "lc.fits" '//
     &               str4(:LENACT(str4))//' timecol = '//
     &               keytim(:LENACT(keytim))//' accumulate = ONE '//
     &               'columns = "'//str1(:LENACT(str1))//'" '//
     &               'mlcinten = INDEF mspinten = INDEF '//
     &               'writesum = - writemean = - '//
     &               str2(:LENACT(str2))//' timeint = INDEF '//
     &               ' ephem = INDEF period = INDEF phaseint = INDEF'//
     &               ' obsdate = MJDREF obstime = "TSTART TSTOP" '//
     &               'sensecase = no timezero = INDEF '//
     &               'clobber = yes negative = IGNORE dryrun = no '//
     &               'chkit = no '

         ELSE

            str1lon= 'seextrct infile = @'//dumnam(:LENACT(dumnam))//
     &               ' '//str5(:LENACT(str5))//
     &               str3(:LENACT(str3))//' gticols = "START STOP" '//
     &               'outroot = '//tmpnam(:LENACT(tmpnam))//
     &               ' extenpha = "sp.fits" extenlc = "lc.fits" '//
     &               str4(:LENACT(str4))//' timecol = '//
     &               keytim(:LENACT(keytim))//
     &               ' columns = "'//str1(:LENACT(str1))//'" '//
     &               'mlcinten = INDEF mspinten = INDEF '//
     &               str2(:LENACT(str2))//' timeint = INDEF '//
     &               ' ephem = INDEF period = INDEF phaseint = INDEF'//
     &               ' obsdate = MJDREF obstime = "TSTART TSTOP" '//
     &               'sensecase = no timezero = INDEF '//
     &               'clobber = yes negative = IGNORE chkit = no '

         ENDIF

c Now do the case of standard extractor for event files.

      ELSEIF ( extftool .EQ. 'extractor' ) THEN

c Set up the input filename

         call GETLUN(ilun)

c If BININ then we are using an event list output from the extractor

         IF(BININ) THEN
            len1 = LENACT(evnin)
            call XSL_FILENAME(evnin,dumnam,len1)
            str1lon = 'extractor filename = "'//dumnam(:len1)//'"'

c If BINOUT then the extractor will be putting out a filtered event list

         ELSE IF ( BINOUT ) THEN
            IF ( DOEVNT ) THEN
               len1 = LENACT(work2(1))
               call XSL_FILENAME(work2(1),dumnam,len1)
               str1lon = 'extractor filename = "'//dumnam(:len1)//'"'
            ELSE
               len1 = LENACT(evnout)
               call XSL_FILENAME(evnout,dumnam,len1)
               str1lon = 'extractor filename = "'//dumnam(:len1)//'"'
            ENDIF

c If MERGED then we are reading a merged event file

         ELSE IF ( MERGED ) THEN
            len1 = LENACT(merfil)
            call XSL_FILENAME(merfil,dumnam,len1)
            str1lon='extractor filename = "'//dumnam(1:len1)//'"'

c If WORK we are reading workspace files

         ELSE IF( WORK )then
            call XSL_RMFILE(lstfil)
            call XSL_OPEN(ilun,lstfil,'NEW',' ',' ',0,0,ierr)
            IF( MANY )then
               do i=1,nfiles
                  len1=LENACT( work1(i) )
                  write(ilun,53) work1(i)(1:len1 )
               end do
            ELSE
               len1=LENACT( work1(1) )
               write(ilun,53) work1(1)(1:len1 )
            ENDIF
            close(ilun)

            len1 = LENACT(lstfil)
            call XSL_FILENAME(lstfil,dumnam,len1)
            str1lon='extractor filename = "@'//dumnam(1:len1)//'"'

c Otherwise the input is one or more standard event files

         ELSE
            call XSL_RMFILE(lstfil)
            call XSL_OPEN(ilun,lstfil,'NEW',' ',' ',0,0,ierr)
            IF( MANY )then
               do i=1,nfiles
c Strip off extensions, if there are any
                  call FTRTNM(filenm(i),dumnam,status)
                  len1=LENACT( dumnam )
                  write(ilun,53) dumnam(1:len1)
               end do
            ELSE
               call FTRTNM(filenm(1),dumnam,status)
               len1=LENACT( dumnam )
               write(ilun,53) dumnam(1:len1)
            ENDIF
            close(ilun)
               
            len1 = LENACT(lstfil)
            call XSL_FILENAME(lstfil,dumnam,len1)
            str1lon='extractor filename = "@'//dumnam(:len1)//'"'
         ENDIF
         call FRELUN(ilun)            

c Add any arguments required to the filename. Currently works for PHA/PI
c and any centering/resizing

         str2 = '['
         len2 = 1

c don't put the automatic cut in if the user has set a filter through the
c filter column command

         IF ( INDEX(colfilter, keypha(:LENACT(keypha))) .EQ. 0 ) THEN

            str2 = str2(:len2)//keypha(1:LENACT(keypha))//'='
            len2 = LENACT(str2)

            IF ( phalcut .GE. 0 .AND. phahcut .GE. 0 ) THEN
               WRITE(str2(len2+1:),'(i6,a,i6,a)') phalcut, ':', 
     &                                            phahcut, ','
            ELSEIF ( phamax .GE. phamin ) THEN
               WRITE(str2(len2+1:),'(i6,a,i6,a)') phamin, ':', 
     &                                            phamax, ','
            ELSE
               str2 = '['
            ENDIF

         ENDIF

c now do the centering/resizing

         len2 = LENACT(str2)
         IF ( sizef .GT. 0 ) THEN
            str2 = str2(:len2)//xcolf(:LENACT(xcolf))//'='
            len2 = LENACT(str2)
            IF ( mod(sizef,2) .EQ. 0 ) THEN
               loside = sizef/2
               hiside = loside - 1
            ELSE
               loside = (sizef-1)/2
               hiside = loside
            ENDIF
            WRITE(str2(len2+1:),'(i6,a,i6,a)') 
     &        xcf-loside, ':', xcf+hiside, ','
            len2 = LENACT(str2)
            str2 = str2(:len2)//ycolf(:LENACT(ycolf))//'='
            len2 = LENACT(str2)
            WRITE(str2(len2+1:),'(i6,a,i6,a)') 
     &        ycf-loside, ':', ycf+hiside, ','
         ENDIF

         len2 = LENACT(str2)
         IF ( sizeh .GT. 0 ) THEN
            str2 = str2(:len2)//xcolh(:LENACT(xcolh))//'='
            len2 = LENACT(str2)
            IF ( mod(sizeh,2) .EQ. 0 ) THEN
               loside = sizeh/2
               hiside = loside - 1
            ELSE
               loside = (sizeh-1)/2
               hiside = loside
            ENDIF
            WRITE(str2(len2+1:),'(i6,a,i6,a)') 
     &        xch-loside, ':',xch+hiside, ','
            len2 = LENACT(str2)
            str2 = str2(:len2)//ycolh(:LENACT(ycolh))//'='
            len2 = LENACT(str2)
            WRITE(str2(len2+1:),'(i6,a,i6,a)') 
     &        ych-loside, ':',ych+hiside, ','
         ENDIF

c Add on any column filtering string specified

         IF ( LENACT(colfilter) .GT. 0 .AND. 
     &        colfilter .NE. 'NONE' ) THEN
            str2(len2+1:) = ' '//colfilter(:LENACT(colfilter))//','
         ENDIF

c Close out the filtering and add to the command string

         len2 = LENACT(str2)
         str2(len2:len2) = ']'
         IF ( len2 .GT. 2 ) THEN
            len1 = LENACT(str1lon)
            IF ( str1lon(len1:len1) .EQ. '"' ) THEN
               str1lon = str1lon(1:len1-1)//str2(:len2)//'" '
            ELSE
               str1lon = str1lon(1:len1)//str2(:len2)//' '
            ENDIF
         ENDIF

         len1 = LENACT( str1lon )

c Now write the output event filename and get the copyall parameter if 
c required:

         IF ( DOEVNT ) THEN
            len1 = LENACT(evnout)
            call XSL_FILENAME(evnout,dumnam,len1)
            tevtname = dumnam
            str1lon = str1lon(:LENACT(str1lon))//
     &              ' eventsout = '//dumnam(:len1)
            call xsl_uclgsb('copyall',COPYALL,status)
            contxt = 'Failed to get copyall'
            IF ( status .NE. 0 ) GOTO 99999
            len1 = LENACT(str1lon)
            IF ( COPYALL ) THEN
               str1lon = str1lon(:len1)//' copyall=yes '
            ELSE
               str1lon = str1lon(:len1)//' copyall=no '
            ENDIF
         ELSE
            str1lon = str1lon(:LENACT(str1lon))//
     &              ' eventsout = NONE'
         ENDIF
         len1 = LENACT( str1lon )

c Now set the region
         IF ( REGION ) then
c Do this so that any edits to the region files will be picked up:
cc this code should not be necessary and now conflicts with xsl_reg_rebin
cc            IF(mode.eq.2) THEN
cc               call XSL_CAT(regvec,numreg,regfil,MAXFIL)
cc            ELSE
cc               call XSL_CAT(regvec,numreg,regfil,MAXFIL)
cc            ENDIF
            len2 = LENACT(regfil)
            call XSL_FILENAME(regfil,dumnam,len2)
         ELSE
            dumnam = 'NONE'
            len2 = 4
         ENDIF            
         str1lon = str1lon(1:len1)//' '//
     +             'regionfile = "'//dumnam(:len2)//'" '

c Now set the light curve stuff            
            
         IF( DOCURV )then
            call xsl_uclgsd('exposure',lcthr,status)
            contxt = 'Failed to get exposure'
            IF ( status .NE. 0 ) GOTO 99999
            write(str3,'(1pe14.7)') lcthr
            call xsl_uclgsb('offset',LCTZERO,status)
            contxt = 'Failed to get offset'
            IF ( status .NE. 0 ) GOTO 99999
            IF ( LCTZERO ) THEN
               str5 = 'lctzero = yes'
            ELSE
               str5 = 'lctzero = no'
            ENDIF
            call xsl_uclgsb('use_qdp',USEQDP,status)
            contxt = 'Failed to get use_qdp'
            IF ( status .NE. 0 ) GOTO 99999
c The FITS binned light curve will be the new standard, so we 
c will always write it, and only write the QDP if asked.
            IF(USEQDP) THEN                  
               len2 = LENACT( curfil )
               call XSL_FILENAME(curfil,dumnam,len2)
               str4 = 'qdpfile = '//dumnam(1:len2)
            ELSE
               str4 = 'qdpfile = NONE '
            ENDIF
            len2 = LENACT(curfits)
            call XSL_FILENAME(curfits,dumnam,len2)
            IF(binsiz.eq.0.0) THEN
c Binsize = 0 used to be for the light curve to feed to XRONOS, 
c Now bin LCEVENTS does this.
               call XWRITE('To output unbinned light '//
     &                     'curve for XRONOS, use BIN LCEVENTS',5)
               goto 999
            ENDIF
            IF(DOLCEV) THEN
               call XSL_RMFILE(xronwn)
               str1lon = str1lon(:LENACT(str1lon))//' '//
     +                   str4(:LENACT(str4))//' '//
     +                   'fitsbinlc = "'//dumnam(1:len2)//'"  '//
     +                   'lcthresh = '//str3(:LENACT(str3))//' '//
     +                   str5(:LENACT(str5))//' '//
     +                   'xronwn = '//xronwn(:LENACT(xronwn))//' '//
     +                   'unbinlc = '//unbfil(:LENACT(unbfil))
            ELSE
               str1lon = str1lon(:LENACT(str1lon))//' '//
     +                   str4(:LENACT(str4))//' '//
     +                   'fitsbinlc = "'//dumnam(1:len2)//'"  '//
     +                   'lcthresh = '//str3(:LENACT(str3))//' '//
     +                   str5(:LENACT(str5))//' '//
     +                   'xronwn = '//xronwn(:LENACT(xronwn))//' '//
     +                    'unbinlc = NONE'

            ENDIF
         ELSE
            str1lon = str1lon(:LENACT(str1lon))//' '//
     +                'qdpfile = NONE '//
     +                'fitsbinlc = "NONE"  '//
     +                'unbinlc = NONE'
         ENDIF
         len1 = LENACT( str1lon )

c This sets the spectrum output

         IF( DOSPEC )then
            len2 = LENACT( spcfil )
            call XSL_FILENAME(spcfil,dumnam,len2)
            tspecname = dumnam
            str1lon = str1lon(1:len1)//' '//
     +                'phafile = "'//dumnam(1:len2)//'" '
         ELSE
            str1lon = str1lon(1:len1)//' '//
     +                'phafile = NONE'
         ENDIF
         len1 = LENACT( str1lon )

c This sets the image output - if the user has set XYSIZE then make
c sure that we use fullimage=no otherwise go by what the fullimage
c logical has been set to.

         IF( DOIMAG )then
            call xsl_uclgsb('fullimage',FULLIMAGE,status)
            contxt = 'Failed to get fullimage'
            IF ( status .NE. 0 ) GOTO 99999
            len2 = LENACT( imfil )
            call XSL_FILENAME(imfil,dumnam,len2)
            str1lon = str1lon(1:len1)//' '//
     +                'imgfile = "'//dumnam(1:len2)//'" '
            len1 = LENACT(str1lon)
            IF ( sizef .GT. 0 .OR. .NOT.FULLIMAGE ) THEN
               str1lon = str1lon(1:len1)//' '//'fullimage = no '
            ELSE
               str1lon = str1lon(1:len1)//' '//'fullimage = yes '
            ENDIF
         ELSE
            str1lon = str1lon(1:len1)//' '//'imgfile = NONE '
         ENDIF
         len1 = LENACT(str1lon)

c Now add on the timing filters:

         CALL XSL_TIMEFILE(dumnam, len2)
         IF ( len2 .GT. 0 ) THEN
            str1lon = str1lon(1:len1)//' timefile = "@'
     &                //dumnam(:len2)//'"'
         ELSE
            str1lon = str1lon(1:len1)//' timefile = NONE'
         ENDIF
         len1 = LENACT( str1lon )

c set the adjustgti parameter

         IF ( ADJUSTGTI ) THEN
            str1lon = str1lon(1:len1)//' adjustgti=yes'
         ELSE
            str1lon = str1lon(1:len1)//' adjustgti=no'
         ENDIF
         len1 = LENACT( str1lon )

c and any grade filters:

         str1lon = str1lon(1:len1)//' gstring = "'
     &                //gfilter(:lenact(gfilter))//'"'
         len1 = LENACT( str1lon )

c add the timepixr parameter if required

         IF ( timepixr .GE. 0.0 ) THEN
            WRITE(str2,'(f5.3)') timepixr
            str1lon = str1lon(1:len1)//' tpixrpar = '//str2(1:5)
            len1 = LENACT( str1lon )
         ENDIF

c For some odd reason this timeorder option messes up light curves.
c Take it out till it's fixed.
c         IF(TORDER) THEN
c            str1lon = str1lon(:len1)//' timeorder = yes'
c            len1 = len1 + 16
c         ELSE
             str1lon = str1lon(:len1)//' timeorder = no'
             len1 = len1 + 15
c         ENDIF

c Set up the rest of the names we will need:

         len5 = LENACT(keytim)
         len6 = LENACT(keypha)
         len7 = LENACT(ascout)
         call XSL_FILENAME(ascout,dumnam,len7)
         write(str2,440) phabin,extrbinh,xbinf,binsiz
 440     format('specbin = ',I4,' binh = ',I4,' binf = ',I4,
     &          ' binlc = ',1pe14.7)

         write(str4,'(a,f10.2)') 'timeref = ',timref

         IF(WTMAPB) THEN
            str4 = str4(:LENACT(str4))//' wtmapb = yes'
            IF (WTMAPFIX) THEN
               str4 = str4(:LENACT(str4))//' wtmapfix = yes'
               IF (SWMAPX) THEN
                  str4 = str4(:LENACT(str4))//' swmapx = yes'
               ELSE
                  str4 = str4(:LENACT(str4))//' swmapx = no'
               ENDIF
               IF (SWMAPY) THEN
                  str4 = str4(:LENACT(str4))//' swmapy = yes'
               ELSE
                  str4 = str4(:LENACT(str4))//' swmapy = no'
               ENDIF
            ELSE
               str4 = str4(:LENACT(str4))//' wtmapfix = no'
            ENDIF
            call xsl_uclgsi('wmapver',iwmapver,status)
            contxt = 'Failed to get wmapver'
            IF ( status .NE. 0 ) GOTO 99999
            len4 = LENACT( str4 )
            WRITE(str4(len4+1:),'(a,i1,1x)') ' wmapver = ', iwmapver
         ELSE
            str4 = str4(:LENACT(str4))//' wtmapb = no'
         ENDIF

         str1lon = str1lon(1:len1)//' '//
     +             'xcolf = '//xcolf(:LENACT(xcolf))//' '//
     +             'ycolf = '//ycolf(:LENACT(ycolf))//' '//
     +             'xcolh = '//xcolh(:LENACT(xcolh))//' '//
     +             'ycolh = '//ycolh(:LENACT(ycolh))//' '//
     +             'xfkey = '//xfkey(:LENACT(xfkey))//' '//
     +             'yfkey = '//yfkey(:LENACT(yfkey))//' '//
     +             'xhkey = '//xhkey(:LENACT(xhkey))//' '//
     +             'yhkey = '//yhkey(:LENACT(yhkey))//' '//
     +             'phamax = '//phamxkwd(:LENACT(phamxkwd))//' '//
     +             str2(:LENACT(str2))//' '//
     +             'tcol = '//keytim(1:len5)//' '//
     +             'ecol = '//keypha(1:len6)//' '//
     +             'ccol = '//keyccd(1:lenact(keyccd))//' '//
     +             'gcol = '//keygrd(1:lenact(keygrd))//' '//
     +             'gti = "'//gtinam(:LENACT(gtinam))//'" '//
     +             'events = "'//evtnam(:LENACT(evtnam))//'" '//
     +             'gtitxt = '//dumnam(1:len7)//' '//
     +             str4(:LENACT(str4))//' '//
     +             'gtinam = "GTI" exitnow = no '

c This is the end of the set-up for input event file(s)
            
      ENDIF

c Now set up the output command file and write the string with the
c extractor command

      call XSL_RMFILE(cmdfil)
      call XSL_OPCF(cmdfil,ilun)
      call XSL_WRTCF(ilun,str1lon,1)

c Close the command file and run it

      call XSL_CLCF(ilun)
      call XSL_RUNCF(cmdfil,ECHO,status)

c If the extractor failed then make sure not to lose an previous
c extract ouput file (it was copied to work2(1) to be input to
c the extractor.

      IF ( status .NE. 0 ) THEN
         IF ( BINOUT .and. DOEVNT ) THEN
            len2 = LENACT(evnout)
            call XSL_RENAME(work2(1),evnout,1,str2,len2,status)
         ENDIF
         contxt = 'Error in '//extftool(:lenact(extftool))
         GOTO 999
      ENDIF

c On success: set all the appropriate flags.

      if ( DOSPEC ) SPEC   = .TRUE.
      if ( DOCURV ) CURV   = .TRUE.
      if ( DOIMAG ) IMAGE  = .TRUE.
      if ( DOLCEV ) UNBIN  = .TRUE.
      IF ( DOEVNT ) BINOUT = .TRUE.

c If the extract was done by saextrct or seextrct then need to rename 
c the output files.

      IF ( extftool .EQ. 'saextrct' .OR. 
     &     extftool .EQ. 'seextrct' ) THEN

         len1 = LENACT(prefix)
         IF(DOCURV) THEN
            tmpnam = prefix(:len1)//'_tmpfillc.fits'
            len2 = LENACT(curfits)
            call XSL_RENAME(tmpnam,curfits,1,str1,len2,status)
         ENDIF
         IF(DOSPEC) THEN
            tmpnam = prefix(:len1)//'_tmpfilsp.fits'
            len2 = LENACT(spcfil)
            call XSL_RENAME(tmpnam,spcfil,1,str1,len2,status)
         ENDIF

      ENDIF

c If an event file is written out and multiple files were read in
c then need to run a sort since the events may now be out of time
c order

      IF ( DOEVNT .AND. MANY ) THEN
         call GETLUN(ilun)
         call XSL_RMFILE(cmdfil)
         call XSL_RMFILE(tempfl)
         call XSL_OPCF(cmdfil,ilun)
         str1lon = 'fmemsort '//
     &             ' infile='//tevtname(:LENACT(tevtname))//
     &             char(92)//char(91)//evtnam(:LENACT(evtnam))//
     &             char(92)//char(93)//' '//
     &             ' outfile='//tempfl(:LENACT(tempfl))//' '//
     &             ' column='//keytim(:LENACT(keytim))//' '//
     &             ' method=heap ascend=yes copyall=yes history=yes'
         call XSL_WRTCF(ilun,str1lon,1)
         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)
         IF ( status .EQ. 0 ) THEN
            len2 = LENACT(tevtname)
            call XSL_RENAME(tempfl,tevtname,1,str1,len2,status)
         ELSE
            call XWRITE(
     & 'fmemsort on event file failed - possibly due to lack of memory',
     &                         5)
            call XWRITE(
     & 'you may want to try using fsort on the output event file',
     &                         5)
            call XWRITE(
     & 'or not if you do not require the events to be time-ordered',5)
               status = 0
         ENDIF
      ENDIF

c Run any mission-dependent post-processing required

      IF ( DOSPEC .OR. DOEVNT ) CALL XSL_POST_EXTR(DOSPEC, DOEVNT)

c Now reset the temporary variables before exiting

 999  binsiz = tbinsz
      phabin = tphabn
      xbinf = txbinf
      phalcut = tphalc
      phahcut = tphahc
      xcf = txcf
      ycf = tycf
      sizef = tsizef

 53   format(a)

99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_BIN: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end

