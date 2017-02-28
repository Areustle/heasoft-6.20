c Routines to set the xselect filters
c      XSL_ENTER_FILTER()                 top-level routine to set filters     
c      XSL_ASCII_FLT(ascstr,mode)         sets ASCII time filter
c      XSL_DETECTOR_FLT(str1)             sets detector filter
c      XSL_GTI_FLT(gtistr, mode)          sets GTI time filter
c      XSL_REGION_FLT(str1)               sets region filter
c      XSL_XWIN_FLT(xwinstr)              sets XRONOS window filter

c
c ---------------------------------------------
      subroutine XSL_ENTER_FILTER()
c ---------------------------------------------
c  THis controls the filter entry routines
c  J.Ingham 6/93

      IMPLICIT NONE

      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1, str2, str3, str4
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings 
      integer len1, len2, len3
c ---------------------------------------------      
      integer MAXTIM, MAXCOL
      parameter(MAXTIM = 40, MAXCOL = 4)
      integer MXCMDS,MAXINT,MXHAND
      parameter (MXCMDS = 20, MAXINT = 20, MXHAND = 20 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      character(8) filter_mode(6)
      character(128) timvec(MAXTIM)
      character(512) comlin
      integer nwhats,myindex, i,LENACT, ninten, nfmodes,newcti,newhnd
      logical ANSW,NEWSEL,PLTQDP,UT,MJD,SCC,PLOT,REMOVE
c For filter INTENSITY
      real intensity(2,MAXINT)
      real rmin, rmax
c For FILTER TIME
      double precision tstart(MXHAND),tstop(MXHAND)
      DOUBLE PRECISION tlims(2), dtimes(MXHAND*2)
      integer ngtis,headlen,ntimes
      character(32) headline
      character(64) filter_des(6)
      character(255) contxt

      data commad /
     &     'COLUMN',
     &     'DETECTOR',
     &     'GRADE',
     &     'INTENSITY',
     &     'PHA_CUTOFF',
     &     'PHASE',
     &     'REGION',
     &     'TIME',
     &     'MKF',
     &     'HK',
     &     10*'NA'/
      
      data comdes /
     &     'enter column filtering',
     &     'enter detectors to use',
     &     'enter grade filtering',
     &     'enter intensity selection ranges',
     &     'enter low and high pha cutoffs',
     &     'enter phase filtering data',
     &     'enter SAOIMAGE format region files',
     &     'enter ascii, Xronos window or fits GTI files',
     &     'enter MKF filter',
     &     'enter HK filter',
     &     10*'NA'/
      data nwhats / 10 /

c These are the options for FILTER TIME      
      data filter_mode /'CURSOR','FILE','MJD','QUIT','SCC','UT'/
      data filter_des /'Enter timing filter with the mouse',
     &     'Enter file containing timing filters',
     &     'Enter timing filter in MJD',
     &     'Quit filter time',
     &     'Enter timing filter in SpaceCraft time',
     &     'Enter timing filter in UT'/

      data nfmodes / 6 /

      CALL xsl_match_cmd('filter_what', commad, comdes, nwhats, 
     &                   MXCMDS, comno, str1, myindex, status)

      IF ( status .NE. 0 ) THEN
         status = 0
         return
      ENDIF

c ------------------------------------------------
c FILTER COLUMN
      IF(str1.eq.'COLUMN') THEN

         call XSL_GETDUMMYPARST('colfilter','filter_str',colfilter,
     &                          status)
         contxt = 'Failed to get column filter string'
         IF ( status .NE. 0 ) GOTO 99999

c ------------------------------------------------
c FILTER DETECTOR
      ELSEIF(str1.eq.'DETECTOR') THEN

         call XSL_GETDUMMYPARST('detector','filter_str',str1,status)
         contxt = 'Failed to get detector'
         IF ( status .NE. 0 ) GOTO 99999
         status=0
         CALL XSL_DETECTOR_FLT(str1)

c ------------------------------------------------
c FILTER GRADE
      ELSEIF(str1.eq.'GRADE') THEN

         call XSL_GETDUMMYPARST('grades','filter_str',gfilter,status)
         contxt = 'Failed to get grade filter string'
         IF ( status .NE. 0 ) GOTO 99999

c ------------------------------------------------
c FILTER HK
      ELSEIF(str1.eq.'HK') THEN

         IF(.NOT.HKREAD) THEN
            call XWRITE('No HK files read in',5)
            return
         ENDIF

c First input the selection criterion

         call XSL_GETDUMMYPARST('hk_sel','select_str',hkstr,status)
         IF(status.ne.0) then
            call XWRITE('Error getting selection expression',5)
            return
         ENDIF
         len1 = LENACT(hkstr)
         hkstr = '"'//hkstr(1:len1)//'"'

c Run maketime on the HK file to set up the GTI filter.

         call XSL_HK_SEL()

c ------------------------------------------------
c FILTER INTENSITY
      ELSEIF(str1.eq.'INTENSITY') THEN
         IF( .NOT. CURV ) THEN
            call XWRITE('You must have made a light curve to '//
     &           'enter intensity filters.',5)
            status = -10
            return
         ENDIF
         if(numint.eq.MXNSEL) then
            call XWRITE('Too many intensity selections.',5)
            return
         endif
         call XSL_GETDUMMYPARST('intensity','filter_str',str1,status)
         contxt = 'Failed to get intensity'
         IF ( status .NE. 0 ) GOTO 99999
C Convert the range to a range expression:
         len1 = LENACT(str1)
         i = 0
         call XSL_ISRANGE(str1,ANSW)
         IF(.NOT. ANSW) THEN
            do while(i.le.len1)
               i = i + 1
               if(str1(i:i).eq.' ')then
                  continue
               else if (str1(i:i).eq.'@') then
                  if(i.eq.len(str1)) then
                     call XWRITE('Your file has only an @ '//
     &                    'at the end of the string!',5)
                     status = -25
                     return
                  endif
                  str1 = str1(i+1:)
                  i = len1 + 1
               else
                  str1 = str1(i:)
                  i = len1 + 1
               endif
            enddo
c Now read in the file:
            
         ENDIF
C Now make the range into a MAKETIME expression:
         rmin = 0.0
         rmax = 100000
         call XSL_UCLGSGPARSE(str1,intensity,MAXINT,rmin,rmax,
     &        ninten,status)
         str2 = ' '
         len3 = LENACT(keyrat)
         do i = 1, ninten
            len1 = LENACT(str2)
            if(intensity(1,i).eq.intensity(2,i) ) then
               write(str3,57) keyrat(:len3),intensity(2,i)
 57            format(a,' .EQ. ',1pg12.4,' .OR. ')
               str2 = str2(:len1)//str3
            else if(intensity(2,i).eq.rmax) then
               write(str3,59) keyrat(:len3),intensity(1,i)
 59            format(a,' .GE. ',1pg12.4,' .OR. ')
               str2 = str2(:len1)//str3
            else 
               write(str3,61) keyrat(:len3),intensity(1,i),
     &              keyrat(:len3),intensity(2,i)
 61            format(a,' .GE. ',1pg12.4,' .AND. ',a,' .LE. ' ,
     &                1pg12.4,' .OR. ')
               str2 = str2(:len1)//str3
            endif
c            print*,'Partial string:',str2(:LENACT(str2))
         enddo
c Get rid of the trailing '.OR. '
         str2 = str2(:LENACT(str2)-5)
         call XWRITE('Making GTI to implement intensity selection'
     &        //' with Boolean expression:',5)
         str3 = ' * '//str2(:min(lenact(str2),len(str3)))
         call XWRITE(str3,5)
         intexp(numint + 1) = str2

c Now set up to run the perl script which runs maketime
         
         call GETLUN(ilun)
         call XSL_RMFILE(cmdfil)
         call XSL_RMFILE(intvec(numint+1))
         call XSL_OPCF(cmdfil,ilun)
         
         
         comlin = 'xsl_filter_intensity '//
     &        curfits(:LENACT(curfits))//' '//
     &        intvec(numint+1)(:LENACT(intvec(numint+1)))//
     &        ' "'//str2(:LENACT(str2))//'" '//
     +        keytim(1:LENACT(keytim))
         len1 = lenact(comlin)

         IF ( ECHO ) THEN
            comlin(len1+1:) = ' yes'
         ELSE
            comlin(len1+1:) = ' no'
         ENDIF

         call XSL_WRTCF(ilun,comlin,1)
         call XSL_CLCF(ilun)
         call XSL_RUNCF(cmdfil,ECHO,status)

         IF(status.ne.0) THEN
            call XWRITE('Error making GTI file for Intensity '//
     &           'selection',5)
            return
         ELSE
            INTENS = .TRUE.
            numint = numint + 1
C Now add it to the list of GTI filters:
            call XSL_GTI_FLT(intvec(numint),0)
         ENDIF

c ------------------------------------------------
c FILTER MKF
      ELSE IF(str1 .eq. 'MKF') THEN

c Code copied from xsel_select.f

c We undo the previous filtering:
         FILTER = .FALSE.

         call xsl_uclgst('mkf_name',ffilin,status)
         contxt = 'Failed to get mkf_name'
         if ( status .NE. 0 ) GOTO 99999

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
            call XWRITE('Error in FILTER MKF from FIND MKF',5)
            return
         endif

         call XSL_FILTER_SEL()
         if(status.ne.0) then
            nffstr = nffstr - 1
         endif
         
c ------------------------------------------------
c FILTER PHASE
      ELSE IF(str1 .eq. 'PHASE') THEN
c First check for a Xronos window file entered through filter time
c For now,we just clear it, if found.
         IF(XWNTFL) THEN
            str1 = 'Only one Xronos window file can be used at'//
     &           'a time.'
            call XWRITE(str1,5)
            str1 = 'Clearing the file entered through filter time'
            call XWRITE(str1,5)
            XWNTFL = .FALSE.
            IF ( BINOUT .and. .NOT. EVTSAV) THEN
               call XSL_BINOUT_SAV('Clearing this filter')
            ENDIF
         ENDIF
c Next check for a previously made phase file, and give the user a chance 
c to save it.
         IF(XPHTFL.AND. .NOT. XPHSAV) THEN
            str1 = 'A Xronos Window file with phase selections '//
     &           'already exists, and will be overwritten.'
            call XWRITE(str1,5)
            call xsl_uclgsb('save_file',ANSW,status)
            contxt = 'Failed to get save_file'
            IF ( status .NE. 0 ) GOTO 99999
            IF(ANSW) THEN
               call XSL_SAVE(6)
            ENDIF
            call XSL_RMFILE(xphflt)
            XPHTFL = .FALSE.
            IF ( BINOUT .and. .NOT. EVTSAV) THEN
               call XSL_BINOUT_SAV('Clearing this filter')
            ENDIF
         ENDIF
c Now get the epoch, period, and phase intervals:
         call XSL_GETDUMMYPARDT('epoch','filter_str',epoch,status)
         contxt = 'Failed to get epoch'
         IF ( status .NE. 0 ) GOTO 99999
c Convert to truncated Julian days:
         epoch = epoch - timref
         call XSL_GETDUMMYPARDT('period','filter_str2',period,status)
         contxt = 'Failed to get period'
         IF ( status .NE. 0 ) GOTO 99999
         rmin = 0.0
         rmax = 1.0
         call xsl_uclgsg('phases',phase,MXPHAS,rmin,rmax,
     &        nphase,status)
         contxt = 'Failed to get phases'
         IF ( status .NE. 0 ) GOTO 99999

c Now check the phases:
         call XSL_CHKPHASE()
         if(status.ne.0) then
            return
         endif

c Now write out the phases
         call XSL_PHASEOUT(xphflt,epoch,period,phase,
     &        nphase,MXPHAS,status)
         IF(status.eq.0) THEN
            XPHTFL = .TRUE.
         ELSE
            call XWRITE('Error writing phase filter',5)
         ENDIF
c ------------------------------------------------
c FILTER PHA
      ELSE IF(str1 .eq. 'PHA_CUTOFF') THEN
         if(VPHACUT) THEN
            call XSL_GETDUMMYPARIT('phalcut','filter_str',phalcut,
     &                             status)
            contxt = 'Failed to get phalcut'
            IF ( status .NE. 0 ) GOTO 99999
            call XSL_GETDUMMYPARIT('phahcut','filter_str2',phahcut,
     &                             status)
            contxt = 'Failed to get phahcut'
            IF ( status .NE. 0 ) GOTO 99999
            if (phalcut.lt.phamin .AND. phamin.LE.phamax) then
               write(str1,87) keypha(:LENACT(keypha)),phalcut
 87            format('Warning: your ',a,' lower bound: ',i5,
     &              ' is below the minimum value.')
               call XWRITE(str1,5)
            endif
            if (phahcut.gt.phamax .AND. phamin.LE.phamax) then
               write(str1,89) keypha(:LENACT(keypha)),phahcut
 89            format('Warning: your ',a,' upper bound: ',i5,
     &              ' is above the maximum value.')
               call XWRITE(str1,5)
            endif
         else
            call XWRITE('PHA_CUTOFF would not be valid for '//
     &           'this data.',5)
         endif
c ------------------------------------------------
c FILTER REGION
      ELSE IF(str1 .eq. 'REGION') THEN
         if(VREGION) THEN
            call XSL_GETDUMMYPARST('region','filter_str',str1,status)
            contxt = 'Failed to get region'
            IF ( status .NE. 0 ) GOTO 99999
            status=0
            call XSL_REGION_FLT(str1)
         else
            call XWRITE('Region filters would not be valid for '//
     &           'this data.',5)
         endif            
c ------------------------------------------------
c FILTER TIME
      ELSE IF(str1 .eq. 'TIME') THEN
c First, what kind of time filtering will we do?:
         status = 0
         i = 0
         REMOVE = .FALSE.
         call XSL_UCLGOT('filter_str',status)
         call XSL_UCLGOT('entry_mode',i)
 915     if(status.ne.0.) then
            status = 0
            call XSL_CMDLIST(filter_mode,filter_des,6,nfmodes)
            call xsl_uclgst('entry_mode',str1,status)
            contxt = 'Failed to get entry_mode'
            IF ( status .NE. 0 ) GOTO 99999
         ELSE
            call XSL_GETDUMMYPARST('entry_mode','filter_str',str1,
     &                             status)
            contxt = 'Failed to get entry_mode'
            IF ( status .NE. 0 ) GOTO 99999
         ENDIF
         call UPC(str1)
         call gmatch(filter_mode,6,str1,myindex,status)
         IF(status.eq.1) THEN
            call XWRITE('entry_mode argument not found.',5)
            go to 915
         ELSE IF (status.eq.2) THEN
            call XWRITE('entry_mode argument not unique',5)
            go to 915
         ENDIF

         IF(str1 .eq. 'QUIT' ) then
            return
         ELSE IF (str1.eq.'FILE') then
            call XSL_GETDUMMYPARST('filter_files','filter_str2',str1,
     &                           status)
            contxt = 'Failed to get filter_files'
            IF ( status .NE. 0 ) GOTO 99999
c This is the TIMING FILES section, where the user enters their own files:
c We will determine the type internally:
c         write(*,*) str1,status
            call XSL_PARSE(str1,timvec,len1,MAXTIM,status)
c         write(*,*) 'number of files = ',len1
            IF(status.ne.0) THEN
               call XWRITE('Error parsing the input list', 5)
               status = 0
               return
            ENDIF
c We undo the filtering, so:
            FILTER = .FALSE.
c Now determine each type, and pass it to the appropriate routine.
c Yes, it's dopey to parse them, put them into a string, and then
c parse them again.  When I have time I'll fix it.
            str2 = ' '
            str3 = ' '
            str4 = ' '
            do i=1,len1
c Remember to strip off the - if there is one.
               IF(timvec(i)(1:1).ne.'-') THEN
                  call XSL_CHKTYPE(timvec(i),len2,status)
               ELSE
                  call XSL_CHKTYPE(timvec(i)(2:),len2,status)
                  REMOVE = .TRUE.
               ENDIF
               IF(status.eq.-10) THEN
                  call XWRITE('Error determining file type for '//
     &                 timvec(i)(:LENACT(timvec(i))),5)
                  call XWRITE('The file is empty',5)
                  status = 0
                  return
               ELSE IF(status.eq.-20) THEN
                  call XWRITE('Error determining file type for '//
     &                 timvec(i)(:LENACT(timvec(i))),5)
                  call XWRITE('The file was not found',5)
                  status = 0
                  return
               ENDIF
c Now add the input files to the correct strings, to be passed
c to the _FLT routines
c            write(*,*)'Type of the ',i,'th file is ',len2
               IF(len2.eq.1) THEN
                  str4 = str4(:LENACT(str4))//' '//
     &                 timvec(i)(:LENACT(timvec(i)))
               ELSE IF(len2.eq.2)THEN
                  str2 = str2(:LENACT(str2))//' '//
     &                 timvec(i)(:LENACT(timvec(i)))
               ELSE IF(len2.eq.3) THEN
                  str3 = str3(:LENACT(str3))//' '//
     &                 timvec(i)(:LENACT(timvec(i)))
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
            IF(str3.ne.' ') THEN
               IF(str3(1:1).eq.' ') THEN
                  str3 = str3(2:)
               ENDIF
c Next check for a previously made phase file, and give the user a chance 
c to save it.
               IF(XPHTFL.AND. .NOT. XPHSAV) THEN
                  REMOVE = .TRUE.
                  str1 = 'A Xronos Window file with phase '//
     &                 'selections already exists, '//
     &                 ' and will be superceded.'
                  call xsl_uclgsb('save_file',ANSW,status)
                  contxt = 'Failed to get save_file'
                  IF ( status .NE. 0 ) GOTO 99999
                  IF(ANSW) THEN
                     call XSL_SAVE(6)
                  ENDIF
                  call XSL_RMFILE(xphflt)
                  XPHTFL = .FALSE.
               ENDIF
c Now enter the file?
               call XSL_XWIN_FLT(str3)
            ENDIF
            IF ( REMOVE .and. BINOUT .and. .NOT. EVTSAV ) THEN
               call XSL_BINOUT_SAV(
     &              'You are removing some filters, this will')
            ENDIF

         ELSE IF(str1.eq.'CURSOR') THEN

C This is the CURSOR ENTRY section
C The way it works is as follows:  The index ctindx stores the numcti at
C the time when the file was written.  So a non-zero value of ctindx(i)
C indicates that ctivec(i) is in use.
C Also the last one is given by ctindx(i) = numcti.  So clear last is
C still easy to do.

            IF ( CURV ) THEN
c If plotdv has not been set, set it:          
               IF(plotdv.eq. ' '.or. plotdv .eq.'NONE'
     &              .or.plotdv.eq.'/NONE') THEN
                  call XSL_GETDEV()
                  IF(status.ne.0) THEN
                     status = 0
                     return
                  ENDIF
               ENDIF
c Check if there is an open slot in the CTIVEC list
               do newcti = 1,MXNSEL
                  if ( CTINDX(newcti).eq.0 ) then
                     goto 877
                  endif
               enddo
C This is the failure branch               
               call XWRITE('No more selections allowed',5) 
               return
C Otherwise the new element is newcti.
 877           NEWSEL = .FALSE.
               IF(USEQDP) THEN
                  call xsl_uclgsb('plot_qdp',PLTQDP,status)
                  contxt = 'Failed to get plot_qdp'
                  IF ( status .NE. 0 ) GOTO 99999
               ELSE
                  PLTQDP = .FALSE.
               ENDIF
               call XSL_TIME_CURSOR(PLTQDP,NEWSEL,ctivec(newcti),
     &                              .FALSE.,.FALSE.,.TRUE.,.FALSE.)
               IF(NEWSEL) THEN
c This line remakes the GTI filter list, and sets the GTITFL variable.
                  numcti = numcti + 1
                  ctindx(newcti) = numcti
                  call XSL_GTI_FLT(ctivec(newcti),1)
               ENDIF
            ELSE
               call XWRITE(' No light curve has been '//
     &              'accumulated yet.',5)
            ENDIF
C This is UT, MJD and SCC, i.e. the HAND set:
         ELSE
C Find the output file name if there is one available:
            do newhnd = 1,MXNSEL
               if(HNDNDX(newhnd).eq.0) then
                  goto 879
               endif
            enddo
c This is the failure branch
            write(str1,'(a,a,I4)')  'Too many GTI selections already',
     &           ', the maximum is: ',MXNSEL
            call XWRITE(str1,5)
            return

 879        continue


C This is the 'By Hand' entry section
C      First get the timing system:
            UT = str1.eq.'UT'
c For UT we will use secfmjdref to convert the time to seconds from mjdref
            MJD = str1.eq.'MJD'
c For MJD, we will subtract mjdref and multiply by day2sec
            SCC = str1.eq.'SCC'
c For SCC, we will just transcribe the time

c Put up the plot as well?

            call xsl_uclgsb('use_plot',PLOT,status)
            contxt = 'Failed to get use_plot'
            IF ( status .NE. 0 ) GOTO 99999
            if (PLOT) then
               IF ( CURV ) THEN
c If plotdv has not been set, set it:          
                  IF(plotdv.eq. ' '.or. plotdv .eq.'NONE'
     &                 .or.plotdv.eq.'/NONE') THEN
                     call XSL_GETDEV()
                     IF(status.ne.0) THEN
                        status = 0
                        return
                     ENDIF
                  ENDIF
                  NEWSEL = .FALSE.
                  PLTQDP = .FALSE.
                  call XSL_TIME_CURSOR(PLTQDP,NEWSEL,hndvec(newhnd),UT,
     &                                 MJD,.TRUE.,.TRUE.)  
                  IF(NEWSEL) THEN
c This line remakes the GTI filter list, and sets the GTITFL variable.
                     numhnd = numhnd + 1
                     hndndx(newhnd) = numhnd
                     call XSL_GTI_FLT(hndvec(newhnd),1)
                  ENDIF               
                  return
               ELSE
                  call XWRITE('No light curve has been made yet,',
     &                 5)
                  call XWRITE('So I can not put up the plot.',5)
                  PLOT = .FALSE.
               ENDIF
            ENDIF

            status = 0
            i = 0
c See whether any list was entered on the command line:
            call XSL_UCLGOT('filter_str2',status)
            call XSL_UCLGOT('filter_list',i)

            if(status.eq.0.or.i.eq.0) then

C The list was entered on the command line.
C Parse it up, the entries must be complete.

               ngtis = 0
               call XSL_GETDUMMYPARST('filter_list','filter_str2',str1,
     &                              status)
               contxt = 'Failed to get filter_list'
               IF ( status .NE. 0 ) GOTO 99999

               CALL str2sec(str1, UT, MJD, ' ', tlims, dtimes, MXHAND,
     &                      ntimes)
               contxt = 'Failed to interpret input time strings'
               IF ( status .NE. 0 ) GOTO 99999

               contxt = 'Require both start and stop times'
               IF ( MOD(ntimes,2) .NE. 0 ) GOTO 99999

               DO i = 1, ntimes/2
                  IF ( dtimes(2*i-1) .gt. dtimes(2*i) ) then 
                     call XWRITE('Error START time after STOP', 5)
                     write(str1,'(1x,e17.5,1x,e17.5)')
     &                       dtimes(2*i-1), dtimes(2*i)
                     call XWRITE(str1,5)
                  ELSE
                     ngtis = ngtis + 1
                     call XSL_DINSERT(ngtis,tstart,tstop,MXHAND,
     &                                dtimes(i*2-1),dtimes(i*2),'IN')
                  ENDIF
               ENDDO

c Else prompt the user for the times

            else

               call XWRITE(' ',5)
               if ( UT ) then
                  str1 = ' * Enter the start and stop times in UT' 
               else if ( MJD ) then
                  str1 = ' * Enter the start and stop times in MJD' 
               else if ( SCC ) then
                  str1 = ' * Enter the start and stop times in SCC' 
               ENDIF
               call XWRITE(str1,5)
               call XWRITE(' ',5)
               call XWRITE('Use a comma to separate the times',5)
               call XWRITE('Type ''x'' to exit time-entry',5)
               call XWRITE('Type ''c'' to cancel time-entry',5)
               
               if (UT ) THEN
                  call XWRITE(' ',5)
                  call XWRITE('The format for times is '//
     &                 'yyyy-mm-ddThh:mm:ss.sss',5) 
                  call XWRITE('or dd/mm/yy hh:mm:ss.ss',5)
                  call XWRITE('If you enter:',5)
                  call XWRITE('   R ''any partial time string''',5)
                  call XWRITE('all the times times you enter'//
     &                 ' subsequently ',5)
                  call XWRITE(
     &                 'will be appended to this partial time string.',
     &                 5)
                  call XWRITE(' ',5)
                  call XWRITE('The only restriction is that the '//
     &                 'partial string ',5)
                  call XWRITE('must contain only '//
     &                 'complete fields.',5)
                  call XWRITE(' ',5)
               endif
               headline = ' '
               headlen = 1
               ngtis = 0
c Now read in the list of times...
               status = 0
               call xsl_uclgst('filter_list',str1,status)
               contxt = 'Failed to get filter_list'
               IF ( status .NE. 0 ) GOTO 99999

               do while (index(str1,'x').eq.0 .and.
     &                   index(str1,'c').eq.0)

                  len1 = index(str1,'R')
                  if ( len1.ne.0 ) then

c Add a space to seperate the reference time from the next field:
                     headline = str1(len1 + 1:LENACT(str1))//' '
                     headlen = LENACT(headline) + 1
                     str1 = 'Got Reference time: '//headline(:headlen)
                     call XWRITE(' ',5)
                     call XWRITE(str1,5)
                     call XWRITE(' ',5)

                  else

                     CALL str2sec(str1, UT, MJD, headline, tlims, 
     &                            dtimes, MXHAND, ntimes)
                     contxt = 'Failed to interpret input time strings'
                     IF ( status .NE. 0 ) GOTO 99999

                     IF ( MOD(ntimes,2) .NE. 0 ) THEN
                        call XWRITE(
     &                   'Please enter both start and stop times', 5)
                        GOTO 237
                     ENDIF

                     DO i = 1, ntimes/2
                        IF ( dtimes(2*i-1) .gt. dtimes(2*i) ) then 
                           call XWRITE(
     &                       'Error START time after STOP', 5)
                           write(str1,'(1x,e17.5,1x,e17.5)')
     &                             dtimes(2*i-1), dtimes(2*i)
                           call XWRITE(str1,5)
                        ELSE
                           call XSL_DINSERT(ngtis,tstart,tstop,MXHAND,
     &                                dtimes(i*2-1),dtimes(i*2),'IN')
                        ENDIF
                     ENDDO

                  endif                  
 237              status = 0
                  call xsl_uclgst('filter_list',str1,status)
                  contxt = 'Failed to get filter_list'
                  IF ( status .NE. 0 ) GOTO 99999
c                  print*,'Times were ',dstart,dstop
               enddo

c Here is where we exit if the user entered a 'c'

               if ( index(str1,'c').ne.0) then
                  call XWRITE('Entry cancelled',5)
                  return
               endif

            endif          

            if(ngtis.gt.0) THEN
C Now write the GTI file:
               call XSL_WRITE_GTI(hndvec(newhnd),tstart,tstop,ngtis,
     &              MXHAND,mjdrefi,mjdreff,keyuni,status)
               if ( status.eq.0) then
                  len1 = lenact(hndvec(newhnd))
                  call XSL_FILENAME(hndvec(newhnd),str1,len1)
                  str1 = 'Wrote GTI''s to file '//str1(:len1)
                  call XWRITE(str1,5)
                  numhnd = numhnd + 1
                  hndndx(newhnd) = numhnd
                  call XSL_GTI_FLT(hndvec(newhnd),1)
               else
                  call XWRITE('Error writing GTI file',5)
               endif
            else
               call XWRITE('No GTI''s entered.',5)
            endif
         endif
      ELSE
         call XWRITE('Unknown option in Filter.',5)
      ENDIF
      
99999 IF ( status .NE. 0 ) THEN
         CALL XWRITE(contxt, 5)
         WRITE(contxt, '(a,i5)') ' XSL_ENTER_FILTER: Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      return
      end
      

c
c ------------------------------------------------
      subroutine XSL_ASCII_FLT(ascstr,mode)
c ------------------------------------------------
c
c  Sets the ascii timing files
c  J. Ingham  5/93

      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings
      integer len1
c ---------------------------------------------
      integer LENACT
      integer i,mode
      character*(*) ascstr
      character(255) tmpvec(MXNSEL),str1
      integer ninvec

      ninvec = 0

c Now the ASCII file:
       IF (ascstr.eq.'reuse') THEN
          continue
       ELSE IF(ascstr.eq.'NONE') then
         numasc = 0
         ASCTFL = .FALSE.
       ELSE
          IF(mode.eq.0) THEN
             call XSL_PARSE(ascstr,tmpvec,ninvec,MXNSEL,status)
             IF(ninvec + numasc .gt. MXNSEL.or.status.ne.0) THEN
                call XWRITE('Too many ascii files',5)
                status = 0
                return
             ENDIF
c Check that the new files actually exist
c and remove the - files.
            i = 1
 340        IF(tmpvec(i)(1:1).eq.'-') THEN
                call XSL_LSTDEL(ninvec,numasc,MXNSEL,
     &                           i,tmpvec,ascvec,status)
                i = i - 1
                IF(status.ne.0) THEN
                   str1 = 'Error removing file '//
     &                  tmpvec(i)(:LENACT(tmpvec(i)))//
     &                  ' ,not in list' 
                   call XWRITE(str1,5)
                   return
               ENDIF
            ELSE
               call XSL_EXIST(tmpvec(i),ierr)
               if(ierr.ne.0) then
                  len1 = LENACT(tmpvec(i))
                  call XWRITE('ASCII filter file '
     &                   //tmpvec(i)(:len1)//' not found',5)
                  return
               endif
            ENDIF
            i = i + 1
            IF(i.le.ninvec) THEN
                 goto 340
            ENDIF
         ENDIF
c This part rebuilds the old list:
c Check that the old files still exist:
         IF(numasc.gt.0) THEN
         do i=1, numasc
            call XSL_EXIST(ascvec(i),ierr)
            if(ierr.ne.0) then
               len1 = LENACT(ascvec(i))
               call XWRITE('ASCII filter file '
     &                   //ascvec(i)(:len1)//'not found',5)
               return
            endif
         enddo
         ENDIF
c Now add them to the list:
         IF(ninvec.gt.0) THEN
            do i=1,ninvec
               ascvec(i+numasc) = tmpvec(i)
            enddo
         ENDIF
         numasc = numasc + ninvec
         call XSL_RMFILE(ascflt)
         call GETLUN(ilun)
         call XSL_OPEN(ilun,ascflt,'NEW',' ',' ',0,0,status)
         IF(numasc.eq.0) THEN
            ASCTFL = .FALSE.
         ELSE
c THis line would or the files:
c                  call XSL_CAT(ascvec,numasc,ascflt,MXNSEL)
c This is the way to and the files:
           do i=1,numasc
             write(ilun,55) ascvec(i)(:LENACT(ascvec(i)))
 55          format(a)
           enddo
           ASCTFL = .TRUE.
        ENDIF
        close(ilun)
        CALL FRELUN(ilun)
      ENDIF

      return
      end

c ---------------------------------------------
      subroutine XSL_DETECTOR_FLT(str1)
c ---------------------------------------------

      CHARACTER str1*(*)

c This sets up the detector filter file. When detector filter is set up 
c the logical DETFL stored in xselvar.inc is set to true.
c The filter information is stored in the file *_detector.xsl (detfil)
c and in the array detvec. This routine also handles detector filters
c being removed through the clear command.

      include 'xsel.inc'
      include 'xselvar.inc'

      INTEGER MAXARG
      PARAMETER (MAXARG=40)

      INTEGER nargs, i, j, k, olun

      character(20) arglst(MAXARG)
      CHARACTER(255) contxt

      logical remove, dupl

      INTEGER lenact
      EXTERNAL lenact

      status = 0
      remove = .false.

c Parse the input string into space separated elements and place in the
c array ARGLST.

      CALL XSL_PARSE(str1, arglst, nargs, MAXARG, status)
      IF ( status .NE. 0 ) THEN
         CALL xwrite('Failed to parse input string', 5)
         RETURN
      ENDIF

c Expand the list, translating any wild cards and any mission-specificity.

      CALL XSL_DET_LIST(arglst, nargs, MAXARG)
      IF ( status .NE. 0 .OR. nargs+numdet .GT. MAXARG ) THEN
         CALL xwrite('Too many detectors specified', 5)
         RETURN
      ENDIF

c If we are removing detectors then do so

      i = 1
      DO WHILE ( i .LE. nargs )
         IF ( arglst(i)(1:1) .EQ. '-' ) THEN
            call XSL_LSTDEL(nargs,numdet,MXNSEL,i,arglst,detvec,status)
            i = i - 1
            remove = .TRUE.
            IF(status.ne.0) THEN
               call XWRITE(
     &          'Error removing detector filter, not in list',5)
               return
            ENDIF
         ENDIF
         i = i + 1
      ENDDO

c Add the new detectors to the list

      IF ( nargs .GT. 0 ) THEN
         DO i = 1, nargs
            detvec(i+numdet) = arglst(i)
         ENDDO
      ENDIF
      numdet = numdet + nargs

c Eliminate any duplicates using a fairly mindless procedure.

      i = 2
      DO WHILE ( i .LE. numdet )
         dupl = .false.
         j = 0
         DO WHILE ( .NOT.dupl .AND. j .LT. (i-1) )
            j = j + 1
            IF ( detvec(i) .EQ. detvec(j) ) dupl = .true.
         ENDDO
         IF ( dupl ) THEN
            DO k = j, numdet-1
               detvec(k) = detvec(k+1)
            ENDDO
            numdet = numdet - 1
         ENDIF
         i = i + 1
      ENDDO

c Write the list of detectors to the detector filter file

      IF ( numdet .GT. 0 ) THEN
         DETFL = .TRUE.
         CALL XSL_RMFILE(detfil)
         CALL GETLUN(olun)
         CALL XSL_OPEN(olun, detfil, 'NEW', ' ', ' ', 0, 0, status)
         IF ( status .NE. 0 ) THEN
            contxt = 'Error opening '//detfil(:lenact(detfil))
            CALL xwrite(contxt, 5)
            RETURN
         ENDIF         
         DO i = 1, numdet
            WRITE(olun,'(a)') detvec(i)(:lenact(detvec(i)))
         ENDDO
         CLOSE(olun)
         CALL FRELUN(olun)
      ELSE
         DETFL = .FALSE.
      ENDIF

C If we removed some detector filters, we do not want to use the BINOUT file
C But give the user a chance to save it...

      IF(REMOVE.and.BINOUT.and. .NOT. EVTSAV ) THEN
         call XSL_BINOUT_SAV('You have removed some filters, this')
      ENDIF

      RETURN
      END

c
c ------------------------------------------------
      subroutine XSL_GTI_FLT(gtistr,mode)
c ------------------------------------------------
c  Sets the GTI timing files
c  mode = 0 means parse the string, and add.
c  mode = 1 means remake (because a new timing file has been added,
c           or some have been removed)
c  J. Ingham  5/93

      include 'xsel.inc'
      include 'xselvar.inc'

      integer LENACT,len4,ilun
      integer i,j,mode
      character*(*) gtistr
      character(255) tmpvec(MXNSEL),context
      integer ninvec

c Set FITTFL to false, and it will get reset if anything remains.
      FITTFL = .FALSE.
      ninvec = 0

c Add on the gti files:

      IF (gtistr.eq.'reuse'.and.mode.eq.0) THEN
          continue
      ELSE IF(gtistr.eq.'NONE'.or.gtistr.eq.'none'
     &                                .and.mode.eq.0) then
          FITTFL = .FALSE.
      ELSE
c This is the part that is adding a new file to the list.
         IF(mode.eq.0) THEN
            call XSL_PARSE(gtistr,tmpvec,ninvec,MXNSEL,status)
            if(status.ne.0) THEN
               call XWRITE('Too many gti files entered',5)
               return
            ENDIF
C We need to see if the added files will overflow the GTI list:
C But don't count the removed ones...
            j = 0
            do i=1,ninvec
               if(tmpvec(i)(1:1).eq.'-') then
                  j = j + 1
               endif
            enddo

            IF(ninvec + numgti - j .gt. MXNSEL) THEN
               call XWRITE('Too many gti files',5)
               return
            ENDIF
c First remove the ones that are to be removed:

            i = 1
 332        IF(tmpvec(i)(1:1).eq.'-') THEN
                call XSL_LSTDEL(ninvec,numgti,MXNSEL,
     &                                 i,tmpvec,gtivec,status)
                i = i - 1
                IF(status.ne.0) THEN
                   call XWRITE('Error removing file, not in list',
     &                  5)
                   return
                ENDIF
             ENDIF
            i = i + 1
             IF(i.le.ninvec) THEN
                goto 332
             ENDIF

c Check that the new files actually exist
             if ( ninvec .gt. 0 ) then
                i = 1
 330            call XSL_EXIST(tmpvec(i),ierr)
                if(ierr.ne.0) then
                   len4 = LENACT(tmpvec(i))
C                   print*,'Got to B with i = ',i,' and numgti ',numgti
                 write(context,331)i,numgti
331          format('Got to B with i = ',i6,' and numgti ',i6)
                 call XWRITE(context,5)
                   context = 'GTI filter file '//tmpvec(i)(:len4)//
     &                  'not found'
                   call XWRITE(context,5)
C     If the file does not exist, shift down the list...
                   do j = ninvec,i+1,-1
                      tmpvec(j-1) = tmpvec(j)
                   enddo
                   ninvec = ninvec - 1
                   i = i - 1
                endif
                i = i + 1
                IF(i.le.ninvec) THEN
                   goto 330
                ENDIF
             ENDIF
         ENDIF

c This part remakes the old list:

c Then check that the old files still exist:
         IF(numgti.gt.0) then
            i = 1
 334        call XSL_EXIST(gtivec(i),ierr)
            if(ierr.ne.0) then
               len4 = LENACT(gtivec(i))
               context = 'GTI filter file '//gtivec(i)(:len4)//
     &                                    'not found'
               call XWRITE(context,5)
c If not found, shift down the list:
               do j = numgti,i+1,-1
                  gtivec(j-1) = gtivec(j)
               enddo
               numgti = numgti - 1
               i = i - 1
            endif
            i = i + 1
            IF(i.le.numgti) THEN
               goto 334
            ENDIF
         ENDIF
c Now add them to the list:
         IF(ninvec.gt.0) THEN
            do i=1,ninvec
               gtivec(i+numgti) = tmpvec(i)
            enddo
         ENDIF
         numgti = numgti + ninvec
c Finally merge them all, remember you may have erased the only file.
         call XSL_RMFILE(gtiflt)
         call GETLUN(ilun)
         call XSL_OPEN(ilun,gtiflt,'NEW',' ',' ',0,0,status)
         IF(numgti.gt.0) THEN
            do i=1,numgti
               write(ilun,59) gtivec(i)(:LENACT(gtivec(i)))
            enddo
            FITTFL = .TRUE.
         ENDIF
c Now add the HKSEL file, if it exists.
         IF(HKSEL) THEN
            write(ilun,59) hktfl(:LENACT(hktfl))
            FITTFL = .TRUE.
         ENDIF
         IF(FFTFL) THEN
            write(ilun,59) ffflt(:LENACT(ffflt))
            FITTFL = .TRUE.
         ENDIF
c Now add on the CURSOR and HAND-entered GTI files:
         do i=1,MXNSEL
            IF(CTINDX(i) .ne. 0) then
               write(ilun,59) ctivec(i)(:LENACT(ctivec(i)))
               FITTFL = .TRUE.
            ENDIF
            IF(HNDNDX(i) .ne. 0 ) THEN
               write(ilun,59) hndvec(i)(:LENACT(hndvec(i)))
               FITTFL = .TRUE.
            ENDIF
         enddo

         close(ilun)
         call FRELUN(ilun)
      ENDIF
c The new file invalidates the old merged GTI file
      call XSL_RMFILE(mrgtif)
      MERGTI = .FALSE.

 59   format(a)
      return
      end

c
c
c --------------------------------------------------
      subroutine XSL_REGION_FLT(str1)
c --------------------------------------------------
c   This sets the region filter
c   J. Ingham 5/93

      include 'xsel.inc'
      include 'xselvar.inc'

      character(255) tmpvec(MAXFIL)
      character(255) str1
      integer LENACT,len4
      integer i,ninvec
      logical REMOVE

      REMOVE = .FALSE.


c Cat the files into regfil, since EXTRACTOR can only take one
c region file.

      call XSL_PARSE(str1,tmpvec,ninvec,MXNSEL,status)
      IF(ninvec + numreg .gt. MXNSEL.or.status.ne.0) THEN
         call XWRITE('Too many region files',5)
         status = 0
         return
      ENDIF
c Check that the new files actually exist
      i = 1
 330  IF(tmpvec(i)(1:1).eq.'-') THEN
         call XSL_LSTDEL(ninvec,numreg,MXNSEL,
     &        i,tmpvec,regvec,status)
         i = i - 1
         REMOVE = .TRUE.
         IF(status.ne.0) THEN
            call XWRITE('Error removing file, not in list',5)
            return
         ENDIF
      ELSE
         call XSL_EXIST(tmpvec(i),ierr)
         if(ierr.ne.0) then
            len4 = LENACT(tmpvec(i))
            call XWRITE('REGION selection file '
     &           //tmpvec(i)(:len4)//
     &           ' not found',5)
            return
         endif
      ENDIF
      i = i + 1
      IF(i.le.ninvec) THEN
         goto 330
      ENDIF
c Then check that the old files still exist:
      IF(numreg.gt.0) THEN
         do i=1, numreg
            call XSL_EXIST(regvec(i),ierr)
            if(ierr.ne.0) then
               len4 = LENACT(regvec(i))
               call XWRITE('REGION file '
     &              //regvec(i)(:len4)//
     &              'not found',5)
               return
            endif
         enddo
      ENDIF
c Now add them to the list:
      IF(ninvec.gt.0) THEN
         do i=1,ninvec
            regvec(i+numreg) = tmpvec(i)
         enddo
      ENDIF
      numreg = numreg + ninvec
      call XSL_CAT(regvec,numreg,regfil,MAXFIL)
      IF(numreg.eq.0) THEN
         REGION = .FALSE.
      ELSE
         REGION = .TRUE.
      ENDIF

C If we removed some region files, we do not want to use the BINOUT file
C But give the user a chance to save it...

      IF(REMOVE.and.BINOUT.and. .NOT. EVTSAV ) THEN
         call XSL_BINOUT_SAV('You have removed some filters, this')
      ENDIF

      return
      end
c
c
c
c
c ------------------------------------------------
      subroutine XSL_XWIN_FLT(xwinstr)
c ------------------------------------------------
c  Sets the Xronos window timing files
c  J. Ingham  5/93

      include 'xsel.inc'
      include 'xselvar.inc'

      character*(*) xwinstr

c Finally the Xronos Window file:

      IF(xwinstr.eq.'reuse') THEN
         continue
      ELSE IF(xwinstr.eq.'NONE') THEN
        XWNTFL = .FALSE.
      ELSE IF(xwinstr(1:1).eq.'-') THEN
        xwnflt = 'NONE'
      ELSE
        call XSL_EXIST(xwinstr,status)
        IF(status.ne.0) THEN
           call XWRITE('Xronos window file not found',5)
           return
        ENDIF
        xwnflt = xwinstr
        XWNTFL = .TRUE.
      ENDIF

      return
      end
c

