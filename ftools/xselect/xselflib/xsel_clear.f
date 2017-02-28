c ---------------------------------------------
      subroutine XSL_CLEAR(mode)
c ---------------------------------------------

      IMPLICIT NONE

c Called by XSELECT main
c

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(512) str1, str2, str4
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2, len3
c ---------------------------------------------
      integer MXCMDS
      parameter (MXCMDS = 24 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      character(255) tmpvec(MXNSEL)
      integer MXCHOS,nchose
      parameter( MXCHOS = 30)
      character(80) chosen(MXCHOS,4)
      character(255) rmvvec(MXCHOS), contxt
      integer nwhats,myindex
      integer LENACT
      real rbuf(2,MXNSEL),rmin,rmax
      integer i,j,k,mode,nctiold,indx
      logical ANSW,BRIEF,ALL,ONLYDD
      character(8) time_mod(5)
      character(64) time_des(5)
      data time_mod /'ALL','CURSOR','FILE','KEYBOARD','QUIT'/
      data time_des /'Clear ALL timing filters',
     &     'Clear filters entered with FILTER TIME CURSOR',
     &     'Clear filters entered with FILTER TIME FILE',
     &     'Clear filters entered with FILTER TIME UT, MJD, or SSC',
     &     'Quit clear time'/

      data commad /'ALL','CLEAN',
     &     'COLUMN',
     &     'DAT*A',
     &     'DATAMODE','DETECTOR',
     &     'EVENTS',
     &     'IN_EVENTS','INTENSITY',
     &     'FAINT',
     &     'FAST',
     &     'GRADE',
     &     'HKSEL',
     &     'MKFSEL',
     &     'OBSCATS',
     &     'PHA_CUTOFF','PHASE','REGION',
     &     'SELECTION','SMOOTH','TIME',
     &     'XYCENTER','XYSIZE','WORKSPACE'/

      data comdes /'clears all files, and logicals',
     &     'deletes the cleaned event list',
     &     'clears any filters on columns in the event file',
     &  'clears the filename list, and filters','clears the datamode',
     &     'clears the detector filters',
     &     'deletes the output event list from extract',
     &     'clears the input event list to extract',
     & 'clears the intensity filters','clears faint logical and files',
     &     'clears fast logical and files',
     &     'clears grade filtering',
     &     'clears HK selection gti',
     &     'clears filter file selection output',
     &     'deletes selected obscats',
     &     'clears PHA upper and lower bounds',
     &     'clears the phase selection',
     &     'clears region filter',
     &     'clears selection and product files',
     &     'clears the smoothed image',
     &     'clears the various timing filters',
     &     'resets XYCENTER to the center of the instrument',
     &     'resets the XSIZING to the default behaviour', 
     &     'clears the workspace files'/
      data nwhats / 24 /

      status = 0

c If mode = 1, then clear all
c Mode = 3 means clear all, but retain datdir and filenm ... list 
      IF(mode.eq.1.or.mode.eq.3) THEN
         str4 = 'ALL'
c Mode = 2 means clear data,
      ELSE IF (mode.eq.2) THEN
         str4 = 'DATA'
      ELSE
         CALL xsl_match_cmd('clear_what', commad, comdes, nwhats, 
     &                      MXCMDS, comno, str4, myindex, status)
      ENDIF

      IF ( status .NE. 0 ) THEN
         status = 0
         return
c --------------------------------------------------
c CLEAR DATA or ALL
      ELSE IF(str4.eq.'ALL'.or.str4.eq.'DATA') THEN
c Give a warning for clear all unless I am doing the clear:
         IF(str4.eq.'ALL'.and.mode.ne.1) THEN
            call XWRITE('WARNING: CLEAR ALL will remove all '//
     &           'temporary files',5)
            call xsl_uclgsb('proceed',ANSW,status)
            contxt = 'Failed to get proceed'
            IF ( status .NE. 0 ) GOTO 99999
            IF(.not. ANSW) THEN
               return
            ENDIF
         ENDIF
         IF(mode.ne.3) THEN
c Erase the filenames:
            do i=1,MAXFIL
               filenm(i)=' '
               hkflnm(i) = ' '
               gtifnm(i) = ' '
            end do
         ENDIF

c Create a command file containing all the REMOVE commands      
c necessary to delete temporary files.

c Open new command file

         call XSL_OPCF(cmdfil,ilun)
c Remove all the work files         
         len2 = LENACT(prefix)
         str1 = prefix(:len2)//'_*work*.xsl'
         call XSL_GETRM(str2,str1)
         len1 = LENACT(str2)
         write(ilun,53) str2(1:len1)
c Remove all the merged files
         str1 = prefix(:len2)//'_*mer*.xsl'
         call XSL_GETRM(str2,str1)
         len1 = LENACT(str2)
         write(ilun,53) str2(1:len1)
         
         IF(str4 .eq.'ALL')THEN
            str1 = prefix(:len2)//'_*.xsl'
            call XSL_GETRM(str2,str1)
            len1 = LENACT(str2)
            write(ilun,53) str2(1:len1)
            
            str1 = prefix(:len2)//'_*.def'
            call XSL_GETRM(str2,str1)
            len1 = LENACT(str2)
            write(ilun,53) str2(1:len1)
            
            str1 = prefix(:len2)//'_*.tmp'
            call XSL_GETRM(str2,str1)
            len1 = LENACT(str2)
            write(ilun,53) str2(1:len1)
         ENDIF
         call XSL_CLCF(ilun)

         call XSL_RUNCF(cmdfil,ECHO,ierr)
c Reset the number of files
         IF(mode.ne.3) THEN
            nfiles=0
            nhkfil = 0
            npar = 0
c Reset the data directory
            IF(str4.ne.'DATA') then
               datdir = 'NONE'
               mkfdir = 'NONE'
            ENDIF
         ENDIF
c reset the timedel keyword:
         timedel = -1.0d0

c Clear the filter arrays                 
         IF(str4.eq.'ALL') THEN
c Reset the datamode
            datamode = 'NONE'
            IF(ASCTFL) then
               ASCTFL = .FALSE.
               do i=1,numasc
                  ascvec(i) = ' '
               enddo
               numasc = 0
            ENDIF
            IF(FITTFL) then
               FITTFL = .FALSE.
               do i=1,numgti
                  gtivec(i) = ' '
               enddo
               numgti = 0
            ENDIF
            XWNTFL = .FALSE.
            XPHTFL = .FALSE.
            XPHSAV = .FALSE.
            IF(REGION) then
               REGION = .FALSE.
               do i=1,numreg
                  regvec(i) = ' '
               enddo 
               numreg = 0
            ENDIF
            IF(DETFL) then
               DETFL = .FALSE.
               do i = 1, numdet
                  detvec(i) = ' '
               enddo
               numdet = 0
            ENDIF
            do i=1,nmkf
               mkfnam(i) = ' '
            enddo
            nmkf = 0
            ffilin = 'def'
         ENDIF
c Reset the logicals            
         IF(mode.ne.3) THEN
            READ   = .FALSE.
            MANY   = .FALSE.
            USEGTI = .FALSE.
            USEHK  = .FALSE.
            HKREAD = .FALSE.
            MANYHK = .FALSE.
         ENDIF
         VREGION = .TRUE.
         VSPEC = .TRUE.
         VIMAGE = .TRUE.
         VPHACUT = .TRUE.
         VGISCLEAN = .TRUE.
         SPEC   = .FALSE.
         CURV   = .FALSE.
         IMAGE  = .FALSE.
         WRKGTI = .FALSE.
         FILTER = .FALSE.
         MERGHK = .FALSE.
         WORKHK = .FALSE.
         EXPAND = .FALSE.
         MERGED = .FALSE.
         WORK   = .FALSE.
         CLEAN  = .FALSE.
         CLEANI = .FALSE.
         DIRTY  = .FALSE.
         UNBIN  = .FALSE.
         BININ  = .FALSE.
         BINOUT = .FALSE.
         EVTSAV = .FALSE.
         choflt = 'NONE'
         WTMAPB = .TRUE.
         WTMAPFIX = .FALSE.
         SWMAPX = .FALSE.
         SWMAPY = .FALSE.
         USEQDP = .FALSE.
         SMOOTH = .FALSE.
         SAVSMOOTH = .FALSE.
         FAINT = .FALSE.
         FAST = .FALSE.
         SELCT  = .FALSE.
         INTENS = .FALSE.
         usrdfe = '-'
c Unset the image offset and size
         xch = 0
         ych = 0
         xcf = 0
         ycf = 0
         sizeh = -1
         sizef = -1

         do i=1,nstrsel
            strsel(i) = 'NONE'
         enddo
         nstrsel = 0

         if(str4.eq.'ALL') THEN
c Reset the prompt
            prompt = prefix(:LENACT(prefix))//' >'
            HKSEL  = .FALSE.
            FFTFL = .FALSE.
c Reset the instrument type to NONE
            instru = 'NONE'
            MADE   = .FALSE.
c Reset the SubMission to NONE
            submis = 'NONE'

c Reset the catalogue, hk and event selection strings

            do i=1,nstrsel
               strsel(i) = 'NONE'
            ENDDO
            DO i=1, nffstr
               ffstr(i) = 'NONE'
            ENDDO
            nstrsel = 0
            nffstr = 0
            hkstr =  'NONE'

            catflt = 'NONE'
            catsel = 'NONE'
c Reset the number of phases and intensity selections to 0:
            numint = 0
            nphase = 0
c Reset the grade filter string
            gfilter = 'NONE'
c Reset the column filter string
            colfilter = 'NONE'
c Unset the ctindx hndndx,intndx flag vectors
            do i=1,MXNSEL
               CTINDX(i) = 0
               HNDNDX(i) = 0
               INTNDX(i) = 0
            enddo
            numcti = 0
            numhnd = 0

c Reset the pha cuts to the flag value
            phalcut = -20
            phahcut = -20
            phamin = 100000
            phamax = 0
            LOADED = .FALSE.
c Reset the filter file junk:
            nffpar = 0
         ENDIF
c -----------------------------------------------
c CLEAR WORKSPACE
      ELSE IF(STR4.EQ.'WORKSPACE') THEN
         IF(WORK) THEN
c Open new command file

            call XSL_OPCF(cmdfil,ilun)
c Remove all the work files         
            len2 = LENACT(prefix)
            str1 = prefix(:len2)//'_*work*.xsl'
            call XSL_GETRM(str2,str1)
            len1 = LENACT(str2)
            write(ilun,53) str2(1:len1)
            call XSL_CLCF(ilun)
            
            call XSL_RUNCF(cmdfil,ECHO,ierr)
            WORK = .FALSE.
         ENDIF
c -----------------------------------------------
c CLEAR CLEAN
      ELSE IF(str4.eq.'CLEAN') THEN
         IF(CLEAN) THEN
            IF(index(instru,'SIS').ne.0) THEN
               call XSL_RMFILE(evnin)
               call XSL_RMFILE(hotpxl)
               BININ = .FALSE.
               CLEAN = .FALSE.
            ELSEIF(index(instru,'GIS').ne.0)THEN
               IF(WORK) THEN
                  call XSL_RMWORK(work1,nfiles)
                  WORK = .FALSE.
               ELSE IF(BININ) THEN
                  call XSL_RMFILE(evnin)
                  BININ = .FALSE.
               endif
               CLEAN = .FALSE.
            ENDIF
         ENDIF
c -----------------------------------------------
c CLEAR COLUMN
      ELSEIF (str4.eq.'COLUMN') THEN
         colfilter = 'NONE'
c -----------------------------------------------
c CLEAR DATAMODE
      ELSEIF (str4.eq.'DATAMODE') THEN
         datamode = 'NONE'
         call XSL_SET(6)
c --------------------------------------------------
c CLEAR DETECTOR
      ELSE IF(str4.eq.'DETECTOR') then
         if(numreg.eq.1) then
            str2 = 'all'
         else
            call xsl_uclgst('clear_which',str2,status)
            contxt = 'Failed to get clear_which'
            IF ( status .NE. 0 ) GOTO 99999
         endif
         IF(str2.eq.'all') then
c Set numdet to zero, then the next if statement will do the clear
            numdet = 0
         ELSE IF(str2.eq.'last') THEN
c CLEAR the last detector on the detector list
            str2 = '-'//detvec(numdet)
            call XSL_DETECTOR_FLT(str2)
         ELSE
c CLEAR the detectors listed in the clear_which string
            
c First put a '-' before each filename
            call XSL_PUTCHAR(str2,'-',status)
            IF(status.ne.0) THEN
               call XWRITE('Error in CLEAR DETECTOR '//
     &              'from PUTCHAR',5)
               status = 0
               return
            ENDIF
            call XSL_DETECTOR_FLT(str2)
         ENDIF
c Now clear everything, if there are no detectors left
         IF(numdet.eq.0)THEN
            call XSL_RMFILE( detfil )
            DETFL = .FALSE.
            do i=1,numdet
               detvec(i) = ' '
            enddo 
         ENDIF
         IF ( BINOUT ) THEN
            call XWRITE('**********************************'//
     &           '***********************************',5)
            call XWRITE('N.B. If you did the FILTER DETECTOR '//
     &           'before doing EXTRACT EVENTS',5)
            call XWRITE('you will also need to do CLEAR EVENTS '//
     &           'now to remove its effects',5)
            call XWRITE('**********************************'//
     &           '***********************************',5)
         ENDIF

c -----------------------------------------------
c CLEAR EVENTS
      ELSEIF(STR4.EQ.'EVENTS') THEN
         CALL XSL_RMFILE( evnout )
         BINOUT = .FALSE.
         EVTSAV = .FALSE.
      ELSE IF(str4.eq.'IN_EVENTS') THEN
         call XSL_RMFILE(evnin)   
         BININ = .FALSE.
c -----------------------------------------------
c CLEAR GRADE
      ELSEIF (str4.eq.'GRADE') THEN
         gfilter = 'NONE'
c -------------------------------------------------
c CLEAR INTENSITY
      ELSE IF(str4.eq.'INTENSITY' ) THEN
         call xsl_uclgst('clear_which',str2,status)
         contxt = 'Failed to get clear_which'
         IF ( status .NE. 0 ) GOTO 99999
         IF(numint .eq. 0 ) THEN
            call XWRITE('No intensity filters entered',5)
            return
         ENDIF
         IF(str2.eq.'all'.or.str2.eq.'ALL') then
            str2 = ' '
            len2 = 1
            do i=1,numint
               call XSL_RMFILE(intvec(i))
               len1 = LENACT(intvec(i))
               str2 = str2(:len2)//' -'//intvec(i)(:len1)
               len2 = len2 + len1 + 2
            enddo

c Remove the leading blank:
            str2 = str2(3:)
            numint = 0
         ELSE IF(str2.eq.'last'.or.str2.eq.'LAST') THEN
c CLEAR the last filter on the  list
            str2 = '-'//intvec(numint)
            call XSL_RMFILE(intvec(numint))
            numint = numint-1
         ELSE
            call XSL_PARSE(str2,tmpvec,len1,MXNSEL,status)
c This is really dopey, I think that I am parsing this 3 times now.
            nctiold = numint
c Remove the unwanted files:
            do i=1,len1 
               do j = 1, numint
                  if ( index(intvec(j),
     &                 tmpvec(i)(:LENACT(tmpvec(i)))) .ne.0 ) THEN
                     do k=j,numint - 1
                        len3 = LENACT(intvec(k))
                        call XSL_RENAME(intvec(k+1),intvec(k),1,
     &                       str4,len3,status)
c                          str4 = 'Moving '//
c     &                         intvec(k+1)(:LENACT(intvec(k+1)))//
c     &                         ' to '//
c     &                         intvec(k)(:LENACT(intvec(k)))
c                          call XWRITE(str4,5)
                     enddo
                     numint = numint - 1
                     GOTO 329
                  endif
               enddo
c Get here for file not found
               str4 = 'Error removing file '//
     &              tmpvec(i)(:LENACT(tmpvec(i)))//' , not in list'
               call XWRITE( str4 ,5 )
c jump to here if file found               
 329           continue
            enddo
            IF(numint .eq. nctiold) THEN
               call XWRITE('No files removed',5)
            ENDIF
C now build up the removal string 
c We just need to remove all those with i > numint
            str2 = ' '
            len2 = 1
            do i=numint + 1, nctiold
               len3 = LENACT(intvec(i))
               str2 = str2(:len2)//' -'//intvec(i)(:len3)
               len2 = len2 + len3 + 2
            enddo
            str2 = str2(3:)
         ENDIF 
c Then this removes the files from the FITS GTI list:
         if(LENACT(str2).eq.0 ) then
            Call XWRITE('No files removed',5)
            return
         endif
         call XSL_GTI_FLT(str2,0)

c Now remove the light curve, since otherwise this will affect further 
c intensity selections...
         IF ( CURV ) THEN
            call XWRITE(' ',5)
            call XWRITE('Xselect uses the light curve to make up',5)
            call XWRITE('the GTI''s which implement the '//
     &           'intensity filter.',5)
            call XWRITE(' ',5)
            call XWRITE('So if you made a light curve with these '//
     &           'filters,',5)
            call XWRITE('and plan to do further intensity '//
     &           'filtering, ',5)
            call XWRITE('you must remove the light curve ',5)
            call XWRITE('or the previous filters will'//
     &           ' affect your results.',5)
            call XWRITE(' ',5)
            call XWRITE('Shall I remove the light curve for you '//
     &           '(you will get a chance to save it)?',5)
            call xsl_uclgsb('remove',ANSW,status)
            contxt = 'Failed to get remove'
            IF ( status .NE. 0 ) GOTO 99999
            if ( ANSW ) THEN
               call xsl_uclgsb('save_file',ANSW,status)
               contxt = 'Failed to get save_file'
               IF ( status .NE. 0 ) RETURN
               if ( ANSW ) then
                  call xsl_uclgst('outfile',str2,status)
                  contxt = 'Failed to get outfile'
                  IF ( status .NE. 0 ) GOTO 99999
                  call XSL_SAVE_CURVE(str2,1)
                  call XSL_XTEND(str2,' ',status)
                  call XSL_UCLPST('outfile',str2,status)
               ENDIF
               CURV = .FALSE.
               call XSL_RMFILE(curfits)
            ENDIF
         ENDIF
C Also, if there was a output events list, we need to warn the user that
C they might need to clear it...

         IF ( BINOUT ) THEN
            call XWRITE('**********************************'//
     &           '***********************************',5)
            call XWRITE('N.B. If you did the FILTER INTENSITY '//
     &           'before doing EXTRACT EVENTS',5)
            call XWRITE('you will also need to do CLEAR EVENTS '//
     &           'to remove its effects',5)
            call XWRITE('**********************************'//
     &           '***********************************',5)
         ENDIF

c Finally set the INTENS flag
         IF(numint.eq.0) then
            INTENS = .FALSE.
         ENDIF

c -------------------------------------------------
c CLEAR MKFSEL
      ELSE IF(str4.eq.'MKFSEL') THEN
         call XSL_RMFILE( ffflt )
         do i=1,nffstr
            ffstr(i) = 'NONE'
         enddo
         nffstr = 0
         FFTFL = .FALSE.
c We must also remake the list file:
         call XSL_GTI_FLT(ffstr(1),1)
c Also clear the binned curves:
         nffpar = 0        
         IF ( BINOUT ) THEN
            call XWRITE('**********************************'//
     &           '***********************************',5)
            call XWRITE('N.B. If you did the SELECT MKF '//
     &           'before doing EXTRACT EVENTS',5)
            call XWRITE('you will also need to do CLEAR EVENTS '//
     &           'now to remove its effects',5)
            call XWRITE('**********************************'//
     &           '***********************************',5)
         ENDIF

c -------------------------------------------------
c CLEAR HKSEL
      ELSE IF(str4.eq.'HKSEL') THEN
         call XSL_RMFILE( hktfl )
         hkstr = ' '
         HKSEL = .FALSE.
c We must also remake the list file:
         call XSL_GTI_FLT(hkstr,1)
c Also clear the binned curves:
         npar = 0        

c -------------------------------------------------
c CLEAR OBSCATS
      ELSE IF(str4.eq.'OBSCATS') THEN
         call xsl_uclgst('clear_which',str2,status)
         contxt = 'Failed to get clear_which'
         IF ( status .NE. 0 ) GOTO 99999
         IF(str2.eq.'NOT_ENTERED') THEN
            call xsl_uclgsb('brief',BRIEF,status)
            contxt = 'Failed to get brief'
            IF ( status .NE. 0 ) GOTO 99999
            call xsl_uclgsb('all',ALL,status)
            contxt = 'Failed to get all'
            IF ( status .NE. 0 ) GOTO 99999
            call xsl_uclgsb('only_datadir',ONLYDD,status)
            contxt = 'Failed to get only_datadir'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_SHOWCATS(1,rmvvec,chosen,nchose,MXCHOS,
     &           ONLYDD,BRIEF,ALL)
            IF(status .eq. -30) THEN
               return
            ELSE IF(status.ne.0) THEN
               call XWRITE('Error in XSL_SHOWCATS',5)
               return
            ENDIF
         ELSE
            call XSL_PARSE(str2,rmvvec,nchose,MXCHOS,status)
         ENDIF

         call XSL_OPCF(cmdfil,ilun)
         do i=1, nchose
            call XSL_GETRM(str2,rmvvec(i))
            write(ilun,'(a)') str2(:LENACT(str2))
            IF(rmvvec(i).eq.catnam) THEN
               MADE = .FALSE.
               LOADED = .FALSE.
            ENDIF
         enddo
         call XSL_CLCF(ilun)
         call FRELUN(ilun)
         call XSL_RUNCF(cmdfil,.FALSE.,status)

c -------------------------------------------------
c CLEAR PHA
      ELSE IF(str4.eq.'PHA_CUTOFF') then
         phahcut = -20
         phalcut = -20
         IF ( BINOUT ) THEN
            call XWRITE('**********************************'//
     &           '***********************************',5)
            call XWRITE('N.B. If you did the FILTER PHA_CUTOFF '//
     &           'before doing EXTRACT EVENTS',5)
            call XWRITE('you will also need to do CLEAR EVENTS '//
     &           'now to remove its effects',5)
            call XWRITE('**********************************'//
     &           '***********************************',5)
         ENDIF
c -------------------------------------------------
c CLEAR PHASE
      ELSE IF(str4.eq.'PHASE') then
         call XSL_RMFILE(xphflt)
         XPHTFL = .FALSE.
         XPHSAV = .FALSE.
         IF ( BINOUT ) THEN
            call XWRITE('**********************************'//
     &           '***********************************',5)
            call XWRITE('N.B. If you did the FILTER PHASE '//
     &           'before doing EXTRACT EVENTS',5)
            call XWRITE('you will also need to do CLEAR EVENTS '//
     &           'now to remove its effects',5)
            call XWRITE('**********************************'//
     &           '***********************************',5)
         ENDIF
c --------------------------------------------------
c CLEAR REGION
      ELSE IF(str4.eq.'REGION') then
         if(numreg.eq.1) then
            str2 = 'all'
         else
            call xsl_uclgst('clear_which',str2,status)
            contxt = 'Failed to get clear_which'
            IF ( status .NE. 0 ) GOTO 99999
         endif
         IF(str2.eq.'all') then
c Set numreg to zero, then the next if statement will do the clear
            numreg = 0
         ELSE IF(str2.eq.'last') THEN
c CLEAR the last region on the region list
            str2 = '-'//regvec(numreg)
            call XSL_REGION_FLT(str2)
         ELSE
c CLEAR the regions listed in the clear_which string
            
c First put a '-' before each filename
            call XSL_PUTCHAR(str2,'-',status)
            IF(status.ne.0) THEN
               call XWRITE('Error in CLEAR REGION '//
     &              'from PUTCHAR',5)
               status = 0
               return
            ENDIF
            call XSL_REGION_FLT(str2)
         ENDIF
c Now clear everything, if there are no regions left
         IF(numreg.eq.0)THEN
            call XSL_RMFILE( regfil )
            REGION = .FALSE.
            do i=1,numreg
               regvec(i) = ' '
            enddo 
         ENDIF
         IF ( BINOUT ) THEN
            call XWRITE('**********************************'//
     &           '***********************************',5)
            call XWRITE('N.B. If you did the FILTER REGION '//
     &           'before doing EXTRACT EVENTS',5)
            call XWRITE('you will also need to do CLEAR EVENTS '//
     &           'now to remove its effects',5)
            call XWRITE('**********************************'//
     &           '***********************************',5)
         ENDIF

c --------------------------------------------------
c CLEAR SELECTION, FAINT, or FAST
      ELSE IF(str4.eq.'SELECTION'.or.str4.eq.'FAINT'
     &        .or.str4.eq.'FAST') THEN
         if( BININ.and.FAST.and.str4.eq.'FAST' ) THEN
            call XSL_RMFILE(evnin)
         else if( WORK )then
            call XSL_OPCF(cmdfil,ilun)
            do i=1,nfiles
               call XSL_GETRM(str1,work1(i))
               len1 = LENACT(str1)
               write(ilun,53) str1(1:len1)
               
               call XSL_GETRM(str1,work2(i))
               len1 = LENACT(str1)
               write(ilun,53) str1(1:len1)
            end do
            call XSL_CLCF(ilun)

            call XSL_RUNCF(cmdfil,ECHO,ierr)
         endif
         MERGED = .FALSE.
         WORK   = .FALSE.
         IF(str4.eq.'SELECTION') THEN
            SELCT  = .FALSE.
            do i=1,nstrsel
               strsel(i) = 'NONE'
            enddo
            nstrsel = 0
            
         ELSE IF(str4.eq.'FAST') THEN
            FAST = .FALSE.
         ELSE IF (str4.eq.'FAINT') THEN
            IF ( CLEAN ) THEN
               call XSL_RMFILE(evnin)
               CLEAN = .FALSE.
            ENDIF
         ENDIF
C
C In all cases, the output events file will bear the effects of these
C operations, so it must be removed.
C
         IF ( BINOUT ) THEN
            call XSL_RMFILE('evnout')
            BINOUT = .FALSE.
         ENDIF
c---------------------------------------------------
c CLEAR SMOOTH
      ELSE IF(str4.eq.'SMOOTH') THEN
         if(SMOOTH) then
            call XSL_RMFILE(simfil)
            SMOOTH = .FALSE.
            SAVSMOOTH = .FALSE.
         endif
c---------------------------------------------------
c CLEAR TIME
      ELSE IF (str4 .eq. 'TIME' ) THEN
         call XSL_UCLGOT('clear_which',status)
 913     if(status.ne.0.) then
            status = 0
           call XSL_CMDLIST(time_mod,time_des,5,5)
         ENDIF
         call xsl_uclgst('clear_which',str2,status)
         contxt = 'Failed to get clear_which'
         IF ( status .NE. 0 ) GOTO 99999
         call UPC(str2)
         call gmatch(time_mod,5,str2,myindex,status)
         IF(status.eq.1) THEN
            call XWRITE('Clear_which argument not found.',5)
            go to 913
         ELSE IF (status.eq.2) THEN
            call XWRITE('Clear_which argument not unique',5)
            go to 913
         ENDIF

         IF(str2 .eq. 'QUIT' ) then
            return
         ELSE IF ( str2.eq.'ALL' ) THEN
            call XWRITE('This will remove ALL timing filters',5)
            call xsl_uclgsb('proceed',ANSW,status)
            contxt = 'Failed to get proceed'
            IF ( status .NE. 0 ) GOTO 99999
            if (ANSW) then
               numgti = 0
               call XSL_RMFILE( gtiflt )
               do i=1,numgti
                  gtivec(i) = ' '
               enddo
               do i=1,numasc
                  ascvec(i) = ' '
               enddo
               numasc = 0
               do i=1,MXNSEL
                  ctindx(i) = 0
                  hndndx(i) = 0
               enddo
               numhnd = 0
               numcti = 0
c We must also remake the list file:
               call XSL_GTI_FLT(str1,1)
            ELSE
               return
            ENDIF
            
         ELSE IF ( str2.eq. 'FILE' ) THEN

c If there is only one, don't bother to prompt for it's name...
            IF (numgti.eq.1.and.numasc.eq.0) THEN
               str2 = 'Clearing timing filter: '//gtivec(1)
               call XWRITE(str2,5)
               str2 = 'ALL'
            ELSE IF (numgti.eq.0.and.numasc.eq.1) THEN
               str2 = 'Clearing timing filter: '//ascvec(1)
               call XWRITE(str2,5)
               str2 = 'ALL'
            ELSE
               call xsl_uclgst('clear_list',str2,status)
               contxt = 'Failed to get clear_list'
               IF ( status .NE. 0 ) GOTO 99999
            ENDIF

            IF(str2.eq.'all'.or.str2.eq.'ALL') then
c Clear all
               if ( numgti.gt.0) then
                  numgti = 0
                  call XSL_RMFILE( gtiflt )
                  do i=1,numgti
                     gtivec(i) = ' '
                  enddo
c We must also remake the list file:
                  call XSL_GTI_FLT(str1,1)
               endif

               if ( numasc .gt. 0 ) then
                  numasc = 0
                  do i=1,numasc
                     ascvec(i) = ' '
                  enddo
                  call XSL_ASCII_FLT('NONE',1)
               endif
            ELSE IF(str2.eq.'last'.or.str2.eq.'LAST') THEN
c CLEAR the last filter on the list
               str2 = '-'//gtivec(numgti)
               call XSL_GTI_FLT(str2,0)
            ELSE
c CLEAR the gti's listed in the clear_which string
               call XSL_PARSE(str2,rmvvec,len1,MXCHOS,status)
c         write(*,*) 'number of files = ',len1
               IF(status.ne.0) THEN
                  call XWRITE('Error parsing the input list', 5)
                  status = 0
                  return
               ENDIF
c We undo the filtering, so:
               FILTER = .FALSE.

               str2 = ' '
               str4 = ' '
               do i=1,len1
                  call XSL_CHKTYPE(rmvvec(i),len2,status)
                  IF(status.eq.-10) THEN
                     call XWRITE('Error determining '//
     &                    'file type for '//
     &                    rmvvec(i)(:LENACT(rmvvec(i))),5)
                     call XWRITE('The file is empty',5)
                     status = 0
                     return
                  ELSE IF(status.eq.-20) THEN
                     call XWRITE('Error determining '//
     &                    'file type for '//
     &                    rmvvec(i)(:LENACT(rmvvec(i))),5)
                     call XWRITE('The file was not found',5)
                     status = 0
                     return
                  ENDIF
c Now add the input files to the correct strings, to be passed
c to the _FLT routines
c            write(*,*)'Type of the ',i,'th file is ',len2
                  IF(len2.eq.1) THEN
                     str4 = str4(:LENACT(str4))//' -'//
     &                    rmvvec(i)(:LENACT(rmvvec(i)))
                  ELSE IF(len2.eq.2)THEN
                     str2 = str2(:LENACT(str2))//' -'//
     &                    rmvvec(i)(:LENACT(rmvvec(i)))
                  ELSE IF(len2.eq.3) THEN
                     XPHTFL = .FALSE.
                     xwnflt = 'NONE'                  
                  ENDIF
               enddo
c This way of appending the list adds a blank to the beginning of the
c line: remove it too:
               IF(str4.ne.' ') THEN
                  IF(str4(1:1).eq.' ') THEN
                     str4 = str4(2:)
                  ENDIF
                  call XSL_GTI_FLT(str4,0)
               ENDIF
               IF(str2.ne.' ') THEN
                  IF(str2(1:1).eq.' ') THEN
                     str2 = str2(2:)
                  ENDIF
                  call XSL_ASCII_FLT(str2,0)
               ENDIF
            ENDIF

         ELSE IF(str2.eq.'CURSOR') THEN
            if ( numcti .eq. 1 ) then
               str2 = 'ALL'
            else
               call XSL_UCLGOT('clear_list',status)
               if(status.ne.0) then
                  call XWRITE('Enter cursor timing files to '//
     &                 'clear by index.',5)
                  len1 = LENACT(ctivec(2))
                  call XSL_FILENAME(ctivec(2),str2,len1)
                  str2 = 'i.e. '//str2(:len1)//' is 2'
                  call XWRITE(str2,5)
                  call XWRITE('Enter as a range (i.e. 1,2,4-7),'//
     &                 'Or ''all'' to clear all,',5)
                  call XWRITE(' or ''last'' to clear the last '//
     &                 'entered filter.',5)
                  status = 0
               endif
               call xsl_uclgst('clear_list',str2,status)
               contxt = 'Failed to get clear_lists'
               IF ( status .NE. 0 ) GOTO 99999
            endif

c            print*,'Before:'
c            write(*,'(20(I2,1x))') (ctindx(i),i=1,20)

            IF(str2.eq.'ALL'.or.str2.eq.'all') THEN
               do i=1,MXNSEL
                  if(ctindx(i).ne.0) then
                     len1 = LENACT(ctivec(i))
                     call XSL_FILENAME(ctivec(i),str1,len1)
                     str1 = 'Removing filter file '//str1(:len1)
                     call XWRITE(str1,5)
                  endif
                  ctindx(i) = 0
               enddo
               numcti = 0
            ELSE IF(str2.eq.'last'.or.str2.eq.'LAST') THEN
               do i = 1, MXNSEL
                  if(ctindx(i).eq.numcti) then
                     ctindx(i) = 0
                     numcti = numcti - 1
                     len1 = LENACT(ctivec(i))
                     call XSL_FILENAME(ctivec(i),str1,len1)
                     str1 = 'Removing filter file '//str1(:len1)
                     call XWRITE(str1,5)
                     goto 577
                  endif
               enddo
 577           continue
            ELSE
               rmin = 1
               rmax = MXNSEL
               call XSL_UCLGSGPARSE(str2,rbuf,MXNSEL,rmin,rmax,j,status)
               if(status.ne.0) then
                  call XWRITE('Error parsing range',5)
                  call XWRITE('No files cleared',5)
                  return
               else
                  do i=1,j
                     do indx=int(rbuf(1,i)),int(rbuf(2,i))
                        len1 = LENACT(ctivec(indx))
                        call XSL_FILENAME(ctivec(indx),str1,len1)
                        if ( ctindx(indx).eq.0) then
                           str1= 'Cursor file: '//str1(:len1)//
     &                          'was not used.'
                           call XWRITE(str1,5)
                        else
                           str1 = 'Removing cursor file '//
     &                          str1(:len1)
                           call XWRITE(str1,5)
C This is to keep the last working somewhat...
                           do k=1,MXNSEL
                              if(ctindx(k).gt.ctindx(indx)) then
                                 ctindx(k) = ctindx(k) - 1
                              endif
                           enddo
                           ctindx(indx)  = 0
                           numcti = numcti - 1
                        endif
                     enddo
                  enddo
               endif
            ENDIF
            call XSL_GTI_FLT(str2,1)

c            print*,'After:'
c           write(*,'(20(I2,1x))') (ctindx(i),i=1,20)
            
         ELSE IF (str2.eq.'KEYBOARD') then
            if ( numhnd .eq. 1 ) then
               str2 = 'ALL'
            else
               call XSL_UCLGOT('clear_list',status)
               if(status.ne.0) then
                  call XWRITE('Enter by-hand timing files to '//
     &                 'clear by index.',5)
                  len1 = lenact(hndvec(2))
                  call XSL_FILENAME(hndvec(2),str2,len1)
                  str2 = 'i.e. '//str2(:len1)
     &                 //' is 2'
                  call XWRITE(str2,5)
                  call XWRITE('Enter as a range (i.e. 1,2,3-5), '//
     &                 'Or ''all'' to clear all,' ,5)
                  call XWRITE(' or ''last'' to clear the last '//
     &                 'entered filter.',5)
                  status = 0
               endif
               call xsl_uclgst('clear_list',str2,status)
               contxt = 'Failed to get clear_list'
               IF ( status .NE. 0 ) GOTO 99999
            endif

            IF(str2.eq.'ALL'.or.str2.eq.'all') THEN
               do i=1,MXNSEL
                  if(hndndx(i).ne.0) then
                     len1 = lenact(hndvec(2))
                     call XSL_FILENAME(hndvec(2),str1,len1)
                     str1 = 'Removing filter file '//str1(:len1)
                     call XWRITE(str1,5)
                  endif
                  hndndx(i) = 0
               enddo
               numhnd = 0
            ELSE IF(str2.eq.'last'.or.str2.eq.'LAST') THEN
               do i = 1, MXNSEL
                  if(hndndx(i).eq.numhnd) then
                     hndndx(i) = 0
                     numhnd = numhnd - 1
                     len1 = LENACT(hndvec(i))
                     call XSL_FILENAME(hndvec(i),str1,len1)
                     str1 = 'Removing filter file '//str1(:len1)
                     call XWRITE(str1,5)
                     goto 579
                  endif
               enddo
 579           continue
            ELSE
               rmin = 1
               rmax = MXNSEL
               call XSL_UCLGSGPARSE(str2,rbuf,MXNSEL,rmin,rmax,j,status)
               if(status.ne.0) then
                  call XWRITE('Error parsing range',5)
                  call XWRITE('No files cleared',5)
                  return
               else
                  do i=1,j
                     do indx=int(rbuf(1,i)),int(rbuf(2,i))
                        len1 = LENACT(hndvec(indx))
                        call XSL_FILENAME(hndvec(indx),str1,len1)
                        if ( hndndx(indx).eq.0) then
                           str1= 'Timing file: '//
     &                          str1(:len1)//
     &                          'was not used.'
                           call XWRITE(str1,5)
                        else
                           str1 = 'Removing timing file '//
     &                          str1(:len1)
                           call XWRITE(str1,5)
C This is to keep the last working somewhat...
                           do k=1,MXNSEL
                              if(hndndx(k).gt.hndndx(indx)) then
                                 hndndx(k) = hndndx(k) - 1
                              endif
                           enddo
                           hndndx(indx)  = 0
                           numhnd = numhnd - 1
                        endif
                     enddo
                  enddo
               endif
            ENDIF
            call XSL_GTI_FLT(str2,1)
            
         ENDIF
         IF ( BINOUT ) THEN
            call XWRITE('**********************************'//
     &           '***********************************',5)
            call XWRITE('N.B. If you entered these timing '//
     &           'filters before doing EXTRACT EVENTS',5)
            call XWRITE('you will also need to do CLEAR EVENTS '//
     &           'now to remove their effects',5)
            call XWRITE('**********************************'//
     &           '***********************************',5)
         ENDIF
      ELSE IF (str4.eq.'XYCENTER') THEN
         call XSL_SET_XYCENTER()
      ELSE IF (str4.eq.'XYSIZE') THEN
         sizef = -1
         call XWRITE('Default sizing restored',5)
      ELSE
         call XWRITE('Unknown option for clear',5)
      ENDIF
      
 53   format(a)

99999 IF ( status .NE. 0 .AND. status .NE. 200 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_CLEAR: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end
