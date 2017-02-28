c ---------------------------------------------
      subroutine XSL_CAT_SETUP(cteles,cinstr,cdatd,chkdir,keymis,
     &                         instru,datdir,mkfdir,hkdir,status)
c ---------------------------------------------
c This takes the values for telescope, instru, data directory and
c hkdir from the observation catalogue, and uses them to setup
c Xselect...  Mostly this just does a little checking.
c J. Ingham 1/5/94
      character*(*) cteles,cinstr,cdatd,chkdir,keymis,instru
      character*(*) datdir,hkdir,mkfdir
      integer status,LENACT

      IF(cteles.eq.'NOT_FOUND') THEN
         call XWRITE('Can''t get mission from obscat, '//
     &        'set it with SET MISSION',5)
      ELSE IF (index(cteles,'::').ne.0) THEN
         call XWRITE('This catalogue contains more than '//
     &        'one mission',5)
      ELSE
         keymis = cteles
         call XSL_SET(4)
      ENDIF
      IF(cinstr.eq.'ALL'.or.cinstr.eq.'USER'
     &     .or.cinstr.eq.'NOT_FOUND') THEN
         call XWRITE('Can''t get instrument from obscat, '//
     &        'set it using SET INSTRUMENT',5)
      ELSE IF(index(cinstr,'::').ne.0) THEN
         call XWRITE('This catalogue contains more '//
     &        'than one instrument.',5)
      ELSE
         instru = cinstr
         call XSL_SET(5)
      ENDIF
      IF(cdatd.eq.'NONE'.or.cdatd.eq.'NOT_FOUND') THEN
         call XWRITE('Data dir. is not in this obscat '//
     &        'set it using SET DATADIR',5)
      ELSE
         call XSL_CHKDIR(cdatd,status)
         IF(status.eq.0) THEN
            call XSL_EXPAND_DIR(cdatd,status)
            IF(status.ne.0) then
               datdir = 'NONE'
            ENDIF
         ELSE
            call XWRITE('The datadir in this catalogue:',5)
            call XWRITE(cdatd(:LENACT(cdatd)),5)
            call XWRITE('was not found',5)
            status = -20
            cdatd = 'DIR_NOT_FOUND'
            datdir = 'NONE'
            return
         ENDIF
         datdir = cdatd
         mkfdir = datdir
      ENDIF
      IF(chkdir.eq.'NONE'.or.chkdir.eq.'NOT_FOUND') THEN
         call XWRITE('HK dir. is not in this obscat '//
     &        'set it using SET DATADIR',5)
      ELSE
         call XSL_CHKDIR(chkdir,status)
         IF(status.eq.0) THEN
            call XSL_EXPAND_DIR(chkdir,status)
            if(status.ne.0) then
               call XWRITE('Error expanding HK directory',5)
            endif
         ELSE
            call XWRITE('The HK dir in this catalogue:',5)
            call XWRITE(chkdir(:LENACT(chkdir)),5)
            call XWRITE('was not found',5)
            chkdir = 'DIR_NOT_FOUND'
         ENDIF
         hkdir = chkdir
      ENDIF
c status = +20 indicates that a catalogue was chosen:
      status = 20

      return
      end
c
c --------------------------------------------------------
      subroutine XSL_CC2I(str1,parnum,nplot,status)
c --------------------------------------------------------
c   Converts a string of space or comma delimited integers into an array
c   of integers.
c   Jim Ingham 2/19/93
c
      integer status,nchar,factor,i,j,coeff,len1
      integer nplot,parnum(*),lenact
      character*(*) str1
      character(255) string
      logical first

      first = .false.
      status = 0
      len1 = LENACT(str1)
      str1 = str1(1:len1)//' '
      len1 = len1 + 1
      nchar = 0
      nplot = 0

      do 100 i = 1,len1
         if(str1(i:i).eq.' '.or.str1(i:i).eq.',') then
            if (.not.first)  then
               goto 100
            else if (str1(i-1:i-1).eq.' '.or.str1(i-1:i-1).eq.',') then
               goto 100
            else
              nplot = nplot + 1
              factor = 1
              parnum(nplot) = 0
              do 150 j = nchar,1,-1
                 if (string(j:j).eq.'0') then
                    coeff = 0
                 else if (string(j:j) .eq.'1') then
                    coeff = 1
                else if (string(j:j) .eq.'2') then
                    coeff = 2
                 else if (string(j:j) .eq.'3') then
                    coeff = 3
                 else if (string(j:j) .eq.'4') then
                    coeff = 4
                 else if (string(j:j) .eq.'5') then
                    coeff = 5
                 else if (string(j:j) .eq.'6') then
                    coeff = 6
                 else if (string(j:j) .eq.'7') then
                    coeff = 7
                 else if (string(j:j) .eq.'8') then
                    coeff = 8
                 else if (string(j:j) .eq.'9') then
                    coeff = 9
                 else
                   status = 1
                    return
                 endif
                 parnum(nplot) = parnum(nplot) + coeff*factor
                 factor = factor*10
 150           continue
               nchar = 0
            endif
         else
            first = .true.
            nchar = nchar + 1
            string(nchar:nchar) = str1(i:i)
         endif
 100  continue

      return
      end
c -------------------------------------------------------
      subroutine XSL_CHKBINSIZE()
c -------------------------------------------------------
c This gets the TIMEDEL keywords out of the events files, sets timedel
c to the MAX of them, and checks that the binsize is not greater than this.
c Jim Ingham 3/28/94

      include 'xsel.inc'
      include 'xselvar.inc'

      double precision dtemp
      integer i,statd
      character(512) str1

      status = 0
      IF(READ) THEN
c Only look for the timedel if it has not yet been found:
         IF(timedel.lt.0.0d0) THEN
            DO i = 1, nfiles
               call XSL_GETKWD(filenm(i),evtnum,datdir,'TIMEDEL',
     &              dtemp,statd,1,1,.FALSE.,status)
               if(status.ne.0) then
                  call XWRITE('Error opening event file: ',5)
                  str1 = '    '//filenm(i)
                  call XWRITE(str1,5)
                  write(str1,'(a,i3)') 'Fitsio error no.: ',status
                  call XWRITE(str1,5)
                  return
               endif
c Include the TIMEDEL from this file if it was found:
               IF(statd.eq.0) then
                  timedel = max(dtemp,timedel)
               ENDIF
            ENDDO
         ENDIF
         IF(timedel.gt.binsiz)THEN
            call XWRITE('WARNING: You have set the binsize '//
     &           ' smaller than the resolution of the data.',5)
            write(str1,557) timedel
 557        format('   MAX(TIMEDEL) = ',e10.2)
            call XWRITE(str1,5)
            call XWRITE(' ',5)
            status = -5
         ENDIF
      ENDIF
      return
      end
c
c
c --------------------------------------------------
      subroutine XSL_CHKDEV(plotdv,devlst,MAXDEV,ndev,LIST,status)
c --------------------------------------------------
c  This subroutine checks plotdv to make sure it is a legimate pgplot
c  device driver.
c  Jim Ingham 8/93

      integer status,ndev,i,len1,niter,point,LENACT,remain,MAXDEV
      character(80) stemp
      character*(*) devlst(MAXDEV)
      character*(*) plotdv
      character(4) device
      logical LIST


c in case this is a filename/device split out the device section

      len1 = index(plotdv, '/')
      device = plotdv(len1:)
      call UPC(device)

      status = 0
      len1 = LENACT(device)
      IF(devlst(1).eq.'NOT_LISTED') THEN
         call XSL_LSTDEV(devlst,MAXDEV,ndev,status)
c This is the flag for lstdev too small
         IF(status.eq.-10)THEN
            status = 0
            return
         ENDIF
      ENDIF
      do i=1,ndev
         IF(index(devlst(i),device(:len1)).ne.0) THEN
            goto 100
         ENDIF
      enddo
      status = -20
      IF(LIST) THEN
         call XWRITE(' ',5)
         IF(index(plotdv,'?').eq.0) THEN
            stemp = 'The device type entered - '//
     &              plotdv(:LENACT(plotdv))//
     &              ' - is not available.'
            call XWRITE(stemp,5)
         ENDIF
         call XWRITE('The available device types are:',5)
         call XWRITE(' ',5)
         niter = ndev/4
         remain = ndev - niter*4
         do i=0,niter-1
            point = i*4+1
            call XWRITE(devlst(point)(:10)//'  '//
     &                  devlst(point+1)(:10)//'  '//
     &                  devlst(point+2)(:10)//'  '//
     &                  devlst(point+3)(:10)
     &                                               ,5)
         enddo
         IF(remain.eq.0) THEN
            continue
         ELSE IF(remain.eq.1) THEN
              call XWRITE(devlst(ndev),5)
         ELSE
            point = niter*4
            stemp = devlst(point+1)(1:10)//'  '
            do i=2,remain
               stemp = stemp(:(i-1)*12)//devlst(point+i)(:10)//'  '
            enddo
            call XWRITE(stemp,5)
         ENDIF
         call XWRITE(' ',5)
      ENDIF

 100  return
      end

c
c
c ---------------------------------------------
      subroutine XSL_CHKPHASE()
c ---------------------------------------------
c This checks the periods and phases in the phase file info in memory

      include 'xsel.inc'
      include 'xselvar.inc'

      integer i
      character(128) str1
      double precision convp,temp


c First convert the period, it is in days:
      IF(keyuni(1:1).eq.'s'.or.keyuni(1:1).eq.'S') THEN
         convp = period*(60**2)*24
      ELSE IF(keyuni(1:1).eq.'m'.or.keyuni(1:1).eq.'M') THEN
         convp = period*60*24
      ELSE IF(keyuni(1:1).eq.'h'.or.keyuni(1:1).eq.'H') THEN
         convp = period*24
      ELSE IF(keyuni(1:1).eq.'d'.or.keyuni(1:1).eq.'D') THEN
         convp = period
      ELSE
c If I don't understand the time units, abort the check.
         return
      ENDIF

c Now check the period:
      if(convp.lt.timedel) then
         call XWRITE('ERROR: The period you have entered'//
     &        ' is less than the resolution of the data',5)
         write(str1,495) convp,timedel
 495     format('Your period: ',e15.10,', TIMEDEL: ',e15.10)
         call XWRITE(str1,5)
         status = -10
         return
      endif

c Then the phases
      do i=1,nphase
         if((phase(2,i)-phase(1,i))*convp.lt.timedel) then
            temp = (phase(2,i)-phase(1,i))*convp
            write(str1,497,err=999) phase(1,i),phase(2,i),temp,timedel
 497        format('ERROR The phase interval: ',g7.4,'-',g7.4,
     &           ' = ',e15.10,/,
     &           ' produces an interval < TIMEDEL',/,
     &           'TIMEDEL =  ',e15.10)
            call XWRITE(str1,5)
 999        status = -10
            return
         endif
      enddo


      return
      end


c
c ----------------------------------------------
      subroutine XSL_CMDLIST(commad,comdes,nwhats,MXCMDS)
c ----------------------------------------------
c This displays the list commad, the elements 'NA' are not displayed
c   J. Ingham 6/93

      integer MXCMDS
      character*(*) commad(MXCMDS),comdes(MXCMDS)
      character(20) blank
      character(80) string
      integer i,LENACT,nwhats,length

      length = len(commad(1))
      if (length .ge. 20 ) then
         do i=1,nwhats
            string = commad(i)(:20)//'-->  '
     &           //comdes(i)(:LENACT(comdes(i)))
            call XWRITE(string(:LENACT(string)),5)
         enddo
      else
         length = 20-length
         blank = ' '
         do i=1,nwhats
            string = commad(i)//blank(1:length)//'-->  '
     &           //comdes(i)(:LENACT(comdes(i)))
            call XWRITE(string(:LENACT(string)),5)
         enddo
      endif
      return
      end
c
c -----------------------------------------------------------------
      subroutine XSL_DINSERT(nint,start,stop,MXGTI,x1,x2,andor)
c -----------------------------------------------------------------
c  This inserts the interval x1 to x2 into the intervals
c  start <-> stop, assumed time ordered and non-overlapping, inc'ing
c  or excluding as dictated by the variable andor.
c   Jim Ingham 6/93

      integer MXNINT,nint,i,nnint,diff,lindex,rindex,MXGTI
      parameter (MXNINT = 50)
      double precision start(MXGTI),stop(MXGTI),x1,x2,t1,t2
      double precision tmp_start(MXNINT),tmp_stop(MXNINT)
      character(2) andor

      if ( MXGTI.gt.MXNINT) then
         call XWRITE('Error in XSL_DINSERT, MXGTI too large',5)
         return
      endif

      IF(x1.gt.x2) then
         t1 = x2
         t2 = x1
      ELSE
         t1 = x1
         t2 = x2
      ENDIF
      IF(nint.eq.0) then
        start(1) = t1
        stop(1) = t2
        nint = 1
        return
      ENDIF
      lindex = 0
      rindex = 0
C     write(*,*) 'x1 = ',x1,' x2 = ',x2
      do i=nint,1,-1
          IF(t1.gt.start(i)) THEN
             lindex = i
             goto 200
          ENDIF
      enddo
 200  IF(andor(1:2).eq.'IN') THEN
C        write(*,*) 'lindex = ',lindex
         IF(lindex.eq.nint) THEN
            IF(t1.le.stop(nint)) THEN
               IF(t2.gt.stop(nint)) THEN
                  stop(nint) = t2
               ENDIF
            ELSE
               IF(nint.eq.MXNINT) THEN
                  call XWRITE('Too many intervals',5)
                  return
               ENDIF
               nint = nint+1
               start(nint) = t1
               stop(nint) = t2
            ENDIF
         ELSE
            do i=nint,lindex+1,-1
               start(i+1) = start(i)
               stop(i+1) = stop(i)
            enddo
            start(lindex+1) = t1
            stop(lindex+1) = t2
            nint = nint + 1
            nnint = 1
            tmp_start(1) = start(1)
            tmp_stop(1) = stop(1)
            do 40 i = 2, nint
               if(start(i) .le. stop(i-1)) then
                  tmp_stop(nnint) = max(tmp_stop(nnint),stop(i))
               else
                  nnint = nnint + 1
                  tmp_start(nnint) = start(i)
                  tmp_stop(nnint) = stop(i)
               endif
 40         continue
            do 50 i=1,nnint
               start(i) = tmp_start(i)
               stop(i) = tmp_stop(i)
 50         continue
            nint = nnint
         ENDIF
c This is the exclusion case:
      ELSE
         do i=nint,1,-1
          IF(t2.gt.start(i)) THEN
             rindex = i
             goto 210
          ENDIF
         enddo
 210     IF(lindex.eq.0) THEN
            IF(rindex.eq.0) return
         ENDIF
         IF(lindex.eq.rindex) then
            IF(t2.lt.stop(rindex)) THEN
              do i=nint,rindex+1,-1
                 start(i+1) = start(i)
                 stop(i+1) = stop(i)
              enddo
              start(rindex+1) = t2
              stop(rindex+1) = stop(rindex)
              stop(rindex) = t1
              nint = nint + 1
           ELSE
              stop(rindex) = t1
           ENDIF
         ELSE
           IF(t2.ge.stop(rindex)) THEN
              if(stop(lindex).gt.t1) then
                 stop(lindex) = t1
              endif
              diff = rindex-lindex
              IF(rindex.eq.nint) THEN
                 nint = lindex
              ELSE
                do i=lindex + 1, nint-diff
                    start(i) = start(i+diff)
                    stop(i) = stop(i+diff)
                enddo
                nint = nint - diff
             ENDIF
          ELSE IF(t2.lt.stop(rindex)) THEN
              start(rindex) = t2
              if(stop(lindex).gt.t1) then
                 stop(lindex) = t1
              endif
              diff = rindex - lindex - 1
              IF(diff.eq.0) return
              do i=lindex + 1, nint-diff
                 start(i) = start(i+diff)
                 stop(i) = stop(i+diff)
             enddo
             nint = nint - diff
         ENDIF
        ENDIF
      ENDIF

      return
      end

c
c
c
c
c ---------------------------------------------
      subroutine XSL_FAFNA(root,exten,M,array)
c ---------------------------------------------
c Called by XSELECT main
c
c (Fill A File Name Array)
c
c Fills a character array with (e.g.) filenames of the
c general construction rootNN.exten, where
c     'root' is the ROOT of the filename (input)
c     'exten' is the EXTENSION (input)
c     NN is a number going from 0 to up to M
c         where M < 1000
c     'array' is the output array, which will contain
c     e.g. 'workspace256.fits' for root='workspace',
c     exten='fits', M=300
c
c Needs to be compiled with XANLIB (for LENACT)
c
c     Alan Smale 1992 Nov 23
c
      implicit none

      integer M
      character*(*) array(M)
      character*(*) root, exten
      character(3) NN
      integer i, ie, nfiles
      integer len1, len2
      integer LENACT

      nfiles = M
      if( nfiles .GT. 999 )then
         nfiles = 999
         call XWRITE(' XSELECT: Too many files for FAFNA ',5)
      endif

      if ( M .LT. 1 ) nfiles = 1

      len1 = LENACT( root )
      len2 = LENACT( exten )

      if(exten(1:1) .eq. '.')then
         ie=2
      else
         ie=1
      endif

      do i = 1, nfiles

         write(NN,1)i
 1       format(i3.3)

         array(i) = root(1:len1)//NN//'.'//exten(ie:len2)

c         len3 = LENACT( array(i) )
c         write(*,'(a)')array(i)(1:len3)

      end do

      return
      end
c
c
c ------------------------------------------------------------
      subroutine XSL_FAST_SETUP(catnam,instru,in_or_out,ccdno,arena,
     &     ario,stah,endh,catidx,nrows,MAXROWS,status)
c ------------------------------------------------------------
c This queries the obscat for the FAST mode area discrimination parameters
c J. Ingham June 26, 1994
      character*(*) catnam, instru,in_or_out
      integer len1,MAXCOL,MAXROWS,MAXVAR,i,j
      integer ccdno,arena,ario,stah,endh,status,catidx(MAXROWS),nrows

      parameter(MAXCOL = 4,MAXVAR = 24)
      integer intvals(MAXCOL,MAXVAR),nvals(MAXCOL)
      character(8) incol(MAXCOL)
      character(80) outcom
      character(255) str1

c Now get the CCDPOW keyword, check for more than one value:
      if (instru .eq. 'SIS0' ) then
         call XSL_COL2KEY('S0CCDPOW','NONE',outcom,1,1,
     &        catnam,catidx,nrows,MAXROWS,status)
      else if (instru .eq. 'SIS1' ) then
         call XSL_COL2KEY('S1CCDPOW','NONE',outcom,1,1,
     &        catnam,catidx,nrows,MAXROWS,status)
      endif
      if( status.ne.0 ) then
         str1 = 'Error getting CCDPOW keyword from catalogue '
     &        //catnam
         call XWRITE(str1,5)
         return
      endif
C Trap errors in the CCDMODE parameter here:
      len1 = index(outcom,'::')
      if(len1.gt.1) then
         call XWRITE('Your FAST mode data is in more than'//
     &        ' one CCD mode.  I got:',5)
         do while (len1.ne.0)
            str1 = ' * '//outcom(:len1-1)
            call XWRITE(str1,5)
            outcom = outcom(len1+2:)
            len1 = index(outcom,'::')
         enddo
         status = -10
         return
      else if (len1.eq.1) then
         call XWRITE('Ill formed result of XSL_COL2KEY.  Got:',5)
         str1 = ' * '//outcom
         call XWRITE(str1,5)
         status = -10
         return
      endif

C Now translate the CCDMODE:
      ccdno = index(outcom,'1') - 1
      if(index(outcom(ccdno+2:),'1').ne.0) then
         IF (instru.eq.'SIS0') THEN
            str1 = 'The value of SOCCDPOW'
         ELSE
            str1 = 'The value of S1CCDPOW'
         ENDIF
         str1 = str1(:21)//' indicates that your data is not in 1'//
     &        ' CCD mode.'
         call XWRITE(str1,5)
         call XWRITE('I can not translate the region '//
     &        'discriminator information.',5)
         status = -12
         return
      endif

C Now get the S{0,1}_{STAH,ENDH,ARIO}{CCDNO} keywords:
      if(instru .eq. 'SIS0') then
         incol(1) = 'S0_ARENA'
         write(incol(2),'(a,I1)') 'S0_ARIO',ccdno
         write(incol(3),'(a,I1)') 'S0_STAH',ccdno
         write(incol(4),'(a,I1)') 'S0_ENDH',ccdno
      else
         incol(1) = 'S1_ARENA'
         write(incol(2),'(a,I1)') 'S1_ARIO',ccdno
         write(incol(3),'(a,I1)') 'S1_STAH',ccdno
         write(incol(4),'(a,I1)') 'S1_ENDH',ccdno
      endif
      call XSL_COL2INT(incol,intvals,nvals,4,MAXCOL,catnam,
     &     catidx,nrows,MAXROWS,status)
      if( status.ne.0 ) then
         str1 = 'Error getting keywords from catalogue '//catnam
         call XWRITE(str1,5)
         return
      endif

C Trap errors in the parameters: error getting values:
      do i=1,4
         if ( nvals(i).le.0) then
            str1 = 'Error getting value for '//incol(i)
            call XWRITE(str1,5)
            status = -11
            return
         endif
      enddo
      if(status.ne.0) then
         return
      endif

c Take the value from the last occurance, since the first
c usually corresponds to before the satellite pointing is fixed:
      arena = intvals(1,nvals(1))
      ario = intvals(2,nvals(2))
      stah = intvals(3,nvals(3))
      endh = intvals(4,nvals(4))

C Trap errors in the parameters: multiple values:
      do i=1,4
         if(nvals(i).gt.1) then
            str1 = 'Your FAST mode data has more than '//
     &           'one value for '//
     &           incol(i)
            call XWRITE(str1,5)
            call XWRITE('   I got:',5)
            do j = 1,nvals(i)
               write(str1,'(a,I4)') ' * ',intvals(i,j)
               call XWRITE(str1,5)
            enddo
            str1 = 'Proceeding using the last value '//
     &           'but this may lead to errors'
            call XWRITE(str1,5)
            status = -10
         endif
      enddo

C Now write out the result
      if(arena .eq. 0) then
         call XWRITE('Area discrimination is disabled.',5)
         write(str1,'(a,i4,a,i4)') 'The mask is set from ',stah,
     &        ' to ',endh
         call XWRITE(str1,5)
         call XWRITE('Both the area inside and outside the mask '//
     &        'in the telemetry.',5)
         call XWRITE('Use SELECT FAST to select the region '//
     &        ' inside or outside the mask.',5)
         in_or_out = 'BOTH'
      else
         call XWRITE('Area discrimination is enabled.',5)
         write(str1,'(a,i4,a,i4)') 'The mask is set from ',stah,
     &        ' to ',endh
         call XWRITE(str1,5)
         if(ario.eq.0) then
            call XWRITE('Only photons outside the mask are '//
     &           'in the telemetry.',5)
            in_or_out = 'OUT'
         else
            call XWRITE('Only photons inside the mask are '//
     &           'in the telemetry.',5)
            in_or_out = 'IN'
         endif
         CALL XWRITE('SELECT FAST is unnecessary in this '//
     &        'case.',5)
      endif

      return
      end

c
c ---------------------------------------------
      subroutine XSL_FINDMKF(MERG)
c ---------------------------------------------
c This gets the name of mkf files.  ffilin is the input.
c If ffilin='def', then the mkf files are assumed of the name given
c in the parameter mkf_def_expr, in the datdir, they are found, and
c read in.  Otherwise, ffilin is assumed to be a list of filenames,
c with the complete path (or relative path from the work directory.
c Jim Ingham June 93
c
      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(255) str1, str2
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings
      integer len1, len2, len3
c ---------------------------------------------
      integer i,LENACT
      character(255) context
      logical MERG

      status = 0
      IF(ffilin.ne.'def') THEN

C Only get the MKF directory if it is not already set:
         IF(mkfdir.eq.'NONE') THEN
            call XSL_GET_DIR('mkf_dir','NONE',mkfdir,status)
            IF(status.ne.0) THEN
               return
            ENDIF
         ENDIF

         call XSL_PARSE(ffilin,mkfnam,nmkf,MXNMKF,status)
         IF(status.ne.0) THEN
            call XWRITE('Too many mkf files',5)
            return
         ENDIF
         do i=1, nmkf
            call XSL_DATDIR(mkfnam(i),mkfdir,0)
            call XSL_EXIST(mkfnam(i),status)
            if(status.ne.0) then
               context = 'MKF file '
     &              //mkfnam(i)(:LENACT(mkfnam(i)))//
     &            'not found.'
               call XWRITE(context,5)
               return
            endif
         enddo
      ELSE
C Only prompt for the MKFDIR if it has not already already been set.
         IF(mkfdir.eq.'NONE') THEN
            call XSL_GET_DIR('mkf_dir','NONE',mkfdir,status)
            IF(status.ne.0) THEN
               return
            ENDIF
         ENDIF
c This is a simple way to do this, namely it is assumed that the MKF
c files can be uniquely determined by some universal regular
c expression, stored in the par file.
         call XSL_LIST(lstfil,mkfdnm,mkfdir,wrkdir,status)

c Now read in the list file:
         call GETLUN(ilun)
         call XSL_OPEN(ilun,lstfil,'OLD',' ',' ',0,0,status)
         do i=1,MXNMKF
            read(ilun,'(a)',END=99) mkfnam(i)
         enddo
c Make sure that this is the last line!
         read(ilun,'(a)',END=99) str1
c If you haven't found an end of file by now there must
c be too many files:
         call XWRITE('Error: Too many MKF files',5)
         close(ilun)
         call FRELUN(ilun)
         status = -10
         return

 99      nmkf = i-1
         IF(nmkf.eq.0) then
           status = -10
           call XWRITE('No MKF files found with default name.',5)
           return
         ENDIF
c Now put on the mkf dir:
         do i=1,nmkf
            call XSL_DATDIR(mkfnam(i),mkfdir,0)
         enddo
         close(ilun)
         call FRELUN(ilun)
       ENDIF

       IF(MERG.and.nmkf.gt.1) THEN
c Set up and run the FMERGE for the columns set in MKFBIN

         call XSL_DATDIR(mermkf,wrkdir,0)
c Write the columns parameter
         str2 = 'columns= @'//tempfl(:LENACT(tempfl))
         call XSL_RMFILE(tempfl)
         call GETLUN(ilun)
         call XSL_OPEN(ilun,tempfl,'NEW',' ',' ',0,0,status)
         write(ilun,'(a)') keytim(:LENACT(keytim))
         do i=1,nffpar
            write(ilun,'(a)') ffplis(i)(:LENACT(ffplis(i)))
         enddo
         close(ilun)
         call FRELUN(ilun)
         len3 = LENACT(str2)
         call XSL_RMFILE(mermkf)
         call XSL_OPCF(cmdfil,ilun)

         len2 = LENACT( mermkf )

         IF(ffilin.eq.'def') THEN
c Remove the previous merge file:
            call XSL_RMFILE(mermkf)
c Move to the data directory:
            call XSL_CD(ilun,mkfdir)
            len1 = LENACT( lstfil )
            comlin = 'fmerge '//
     +     'infiles=@'//lstfil(1:len1)//' '//
     +     'outfile='//mermkf(1:len2)//' '//
     +     str2(:len3)//' '//
     +     'mextname="MERMKF" '//
     +     'copyprime="yes" '//
     +     'lastkey=" " '//
     +     'history="yes" '
            call XSL_WRTCF(ilun,comlin,1)
            call XSL_CD(ilun,wrkdir)
         ELSE
c Remove the previous merge file:
            call XSL_RMFILE(mermkf)
            comlin = 'fmerge '//
     +     'infiles='//ffilin(:LENACT(ffilin))//' '//
     +     'outfile='//mermkf(1:len2)//' '//
     +     str2(:len3)//' '//
     +     'mextname="MERMKF" '//
     +     'copyprime="yes" '//
     +     'lastkey=" " '//
     +     'history="yes" '
            call XSL_WRTCF(ilun,comlin,1)
         ENDIF
C         len1 = LENACT( comlin )
         call XSL_CLCF(ilun)

         call XSL_RUNCF(cmdfil,ECHO,status)
         IF(status.ne.0) THEN
           call XWRITE('Error merging MKF files',5)
         ENDIF
       ENDIF


       return
       end
c
c
c --------------------------------------------------
      subroutine XSL_GETDEV()
c --------------------------------------------------
c  This routine gets the plot device
c  Jim Ingham
c
      include 'xsel.inc'
      include 'xselvar.inc'
      integer i,LENACT,len1,len2
      character(80) str1

c len2 will count the nuber of bad answers, 3 are allowed.
      len2 = 0

      call XSL_GETDUMMYPARST('plotdev','set_str',str1,status)
      IF ( status .NE. 0 ) RETURN

      len1 = LENACT( str1 )
c A / is not allowed on the command line and will cause an
c error and bring on the prompt. It is acceptable in answer
c to the prompt, though. Add the / if necessary.

      if(index(str1,'/') .NE. 0)then
         plotdv = str1
      else
         plotdv = '/'//str1(:max(len1,len(plotdv)-1))
      endif

c Now, we're OK unless the user has answered the prompt and used
c quotes. Change the quotes into spaces and delete the spaces.

      len1 = LENACT( plotdv )
      do i = 1, len1
         if(plotdv(i:i).eq.'"')plotdv(i:i)=' '
      end do

c if RMVBLK worked properly I wouldn't have to do the following:

      if(plotdv(2:2).eq.' ')then
         do i = 2,len1+1
            plotdv(i:i) = plotdv(i+1:i+1)
         end do
      endif

c finally there's the chance we have two // on the plotting device
c now, if the user used both / and "" when answering the prompt!

      len1 = LENACT( plotdv )
      if (plotdv(1:2) .eq. '//')then
         do i = 1,len1
            plotdv(i:i) = plotdv(i+1:i+1)
         end do
      end if

c Now check for a valid device type:
      call XSL_CHKDEV(plotdv,devlst,MAXDEV,ndev,.TRUE.,status)
      IF(status.ne.0) THEN
         status = 0
         call XWRITE('No device chosen',5)
         plotdv = 'NONE'
         return
      ENDIF

c tell the user what plotting device he's ended up with.

      len1 = LENACT( plotdv )
      str1 = ' Plotting device chosen: '//plotdv(1:len1)
      CALL XWRITE(str1,5)

c Also store the value:

      call XSL_UCLPST('stored_devicetype',plotdv,status)

      return
      end
c
c
c ------------------------------------------------------------
      subroutine XSL_GET_DEFOBSEL(instru,datamode,submis,keymis,defsel)
c ------------------------------------------------------------

      character*(*) instru,datamode,submis,keymis,defsel

c This routine constructs the default selection expression when searching
c for data files.
c J. Ingham 4/94

      integer LENACT,idef,status
      CHARACTER(8) instkey, dmodekey

c Get the default expression from the mission database

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'ofilt', defsel,
     &              status)
      IF ( status .NE. 0 ) RETURN

c Now we want to and this with any instrument and datamode set. We have
c to get from the mission database what the instrument and datamade keywords
c are called.

      IF ( defsel .EQ. 'NONE' ) THEN
         idef = 0
      ELSE
         idef = LENACT(defsel)
      ENDIF

      IF ( instru .ne. 'NONE' ) THEN

         CALL XSL_MDBS(keymis, submis, ' ', ' ', 'instkey', 
     &                 instkey, status)
         IF ( status .NE. 0 ) RETURN

         IF ( idef .GT. 0 ) THEN
            defsel(idef+1:) = '&&'
            idef = idef + 2
         ENDIF
         defsel(idef+1:) = instkey//'=='''//
     &                     instru(:lenact(instru))//''''
         idef = idef + 12 + lenact(instru)

      ENDIF

      IF ( datamode .ne. 'NONE' ) THEN

         CALL XSL_MDBS(keymis, submis, instru, ' ', 'dmodekey', 
     &                 dmodekey, status)
         IF ( status .NE. 0 ) RETURN

         IF ( idef .GT. 0 ) THEN
            defsel(idef+1:) = '&&'
            idef = idef + 2
         ENDIF
         defsel(idef+1:) = dmodekey//'=='''//
     &                     datamode(:lenact(datamode))//''''
         idef = idef + 12 + lenact(datamode)

      ENDIF

c Make sure that we return a NONE string if nothing has been set

      IF ( idef .EQ. 0 ) defsel = 'NONE'

      END

c
c
c ------------------------------------------------------------
      subroutine XSL_GET_DIR(parname,dumpar,dirname,status)
c ------------------------------------------------------------
c This gets the dirname from the user, using the parameter parname.
c It does an ls and queries again if dirname ends in 'ls'
c Dumpar is in case you need to go through a dummy parameter the
c first time
c J. Ingham 6/94
      character*(*) parname,dumpar,dirname
      character(255) str1
      integer status, LENACT, len1

      IF(dumpar.eq.'NONE') then
         call xsl_uclgst(parname,dirname,status)
         IF ( status .NE. 0 ) RETURN
      ELSE
         call XSL_GETDUMMYPARST(parname,dumpar,dirname,status)
         IF ( status .NE. 0 ) RETURN
      ENDIF
      IF(dirname.ne.'NONE') THEN
c First make sure the directory exists...
         call XSL_CHKDIR(dirname,status)
c If the directory exists, then expand relative paths...
         IF(status.eq.0) THEN
            call XSL_EXPAND_DIR(dirname,status)
            if(status.ne.0) then
               call XWRITE('Error expanding data directory',5)
               return
            endif
c Status = -10 means that the user wants an ls done on the path entered
c Then cycle until the path is entered
         else if( status.eq.-10) then
            status = 0
 25         call xsl_uclgst(parname,dirname,status)
            str1 = "dir = "//dirname(1:lenact(dirname))//'!'
            call xwrite(str1,7)
            IF ( status .NE. 0 ) RETURN
            call XSL_CHKDIR(dirname,status)
            if( status.eq.-10) then
               status = 0
               goto 25
            else if ( status .ne. 0) then
               len1 = LENACT(dirname)
               str1 = 'The directory '//dirname(1:len1)//
     &              ' doesn''t exist'
               call XWRITE(str1,5)
               dirname = 'NONE'
               return
            else
               call XSL_EXPAND_DIR(dirname,status)
               if(status.ne.0) then
                  call XWRITE('Error expanding directory',5)
                  return
               endif
            endif
         else if ( status .ne. 0) then
            len1 = LENACT(dirname)
            str1 = 'The directory '//dirname(1:len1)//
     &           ' doesn''t exist'
            call XWRITE(str1,5)
            dirname = 'NONE'
            return
         endif
      ENDIF

      return
      end


c
c
c ---------------------------------------------
         subroutine XSL_GET_LIST(lstvec,ninvec,MAXVEC,lstfil,status)
c ---------------------------------------------
c This reads a file into a vector of strings, chucking out blanks,
c and returns
c   status =   0   Successful return
c   status = -10   List file not found
c   status = -20   Too many entries in list file
c   Jim Ingham 11/93

         integer ninvec, MAXVEC, status
         character*(*) lstvec(MAXVEC),lstfil
         character(256) string
         integer i,ilun

         status = 0
         call GETLUN(ilun)
         call XSL_OPEN(ilun,lstfil,'OLD',' ',' ',0,0,status)
         if(status.ne.0) then
            status = -10
            goto 998
         endif
         do i=1,MAXVEC-ninvec
            ninvec = ninvec + 1
            read(ilun,'(a)',END=99) lstvec(ninvec)
c Chuck out blank strings
            if(lstvec(ninvec).eq. ' ') then
               ninvec = ninvec - 1
            endif
         enddo
c Make sure that this is the last line!
         read(ilun,'(a)',END=99) string
c If you haven't found an end of file by now there must
c be too many files:
         status = -20
         ninvec = MAXVEC
         go to 999
c Check that the last one is not a blank
 99      ninvec = ninvec - 1
 999     close(ilun)
 998     call FRELUN(ilun)

         return
         end
c
c ---------------------------------------------
      subroutine XSL_I2LJSTR(number,string)
c ---------------------------------------------
c This turns a number into a left justified string.
c The number is assumed less than 10000.
      character(1) one
      character(2) two
      character(3) three
      character(4) four
      character*(*) string
      integer number

      if(number.lt.10) then
         write(one,'(i1)') number
         string = one
      elseif(number.lt.100) then
         write(two,'(i2)') number
         string = two
      elseif(number.lt.1000) then
         write(three,'(i3)') number
         string = three
      elseif(number.lt.10000) then
         write(four,'(i4)') number
         string = four
      else
         string = '*****'
      endif


      return
      end
c
c -----------------------------------------------------------------
      subroutine XSL_INSERT(nint,start,stop,MXGTI,x1,x2,andor)
c -----------------------------------------------------------------
c  This inserts the interval x1 to x2 into the intervals
c  start <-> stop, assumed time ordered and non-overlapping, inc'ing
c  or excluding as dictated by the variable andor.
c   Jim Ingham 6/93

      integer MXNINT,nint,i,nnint,diff,lindex,rindex,MXGTI
      parameter (MXNINT = 50)
      real start(MXGTI),stop(MXGTI),x1,x2,t1,t2
      real tmp_start(MXNINT),tmp_stop(MXNINT)
      character(2) andor

      if ( MXGTI.gt.MXNINT) then
         call XWRITE('Error in XSL_INSERT, MXGTI too large',5)
         return
      endif

      IF(x1.gt.x2) then
         t1 = x2
         t2 = x1
      ELSE
         t1 = x1
         t2 = x2
      ENDIF
      IF(nint.eq.0) then
        start(1) = t1
        stop(1) = t2
        nint = 1
        return
      ENDIF
      lindex = 0
      rindex = 0
C     write(*,*) 'x1 = ',x1,' x2 = ',x2
      do i=nint,1,-1
          IF(t1.gt.start(i)) THEN
             lindex = i
             goto 200
          ENDIF
      enddo
 200  IF(andor(1:2).eq.'IN') THEN
C        write(*,*) 'lindex = ',lindex
         IF(lindex.eq.nint) THEN
            IF(t1.le.stop(nint)) THEN
               IF(t2.gt.stop(nint)) THEN
                  stop(nint) = t2
               ENDIF
            ELSE
               IF(nint.eq.MXNINT) THEN
                  call XWRITE('Too many intervals',5)
                  return
               ENDIF
               nint = nint+1
               start(nint) = t1
               stop(nint) = t2
            ENDIF
         ELSE
            do i=nint,lindex+1,-1
               start(i+1) = start(i)
               stop(i+1) = stop(i)
            enddo
            start(lindex+1) = t1
            stop(lindex+1) = t2
            nint = nint + 1
            nnint = 1
            tmp_start(1) = start(1)
            tmp_stop(1) = stop(1)
            do 40 i = 2, nint
               if(start(i) .le. stop(i-1)) then
                  tmp_stop(nnint) = max(tmp_stop(nnint),stop(i))
               else
                  nnint = nnint + 1
                  tmp_start(nnint) = start(i)
                  tmp_stop(nnint) = stop(i)
               endif
 40         continue
            do 50 i=1,nnint
               start(i) = tmp_start(i)
               stop(i) = tmp_stop(i)
 50         continue
            nint = nnint
         ENDIF
c This is the exclusion case:
      ELSE
         do i=nint,1,-1
          IF(t2.gt.start(i)) THEN
             rindex = i
             goto 210
          ENDIF
         enddo
 210     IF(lindex.eq.0) THEN
            IF(rindex.eq.0) return
         ENDIF
         IF(lindex.eq.rindex) then
            IF(t2.lt.stop(rindex)) THEN
              do i=nint,rindex+1,-1
                 start(i+1) = start(i)
                 stop(i+1) = stop(i)
              enddo
              start(rindex+1) = t2
              stop(rindex+1) = stop(rindex)
              stop(rindex) = t1
              nint = nint + 1
           ELSE
              stop(rindex) = t1
           ENDIF
         ELSE
           IF(t2.ge.stop(rindex)) THEN
              if(stop(lindex).gt.t1) then
                 stop(lindex) = t1
              endif
              diff = rindex-lindex
              IF(rindex.eq.nint) THEN
                 nint = lindex
              ELSE
                do i=lindex + 1, nint-diff
                    start(i) = start(i+diff)
                    stop(i) = stop(i+diff)
                enddo
                nint = nint - diff
             ENDIF
          ELSE IF(t2.lt.stop(rindex)) THEN
              start(rindex) = t2
              if(stop(lindex).gt.t1) then
                 stop(lindex) = t1
              endif
              diff = rindex - lindex - 1
              IF(diff.eq.0) return
              do i=lindex + 1, nint-diff
                 start(i) = start(i+diff)
                 stop(i) = stop(i+diff)
             enddo
             nint = nint - diff
         ENDIF
        ENDIF
      ENDIF

      return
      end

c
c
c ---------------------------------------------
      subroutine XSL_ISRANGE(buffer,isrange)
c ---------------------------------------------
c This tells whether buffer is a range expression, or a BOOLEAN expression.
c 0 for range, 1 otherwise.
c J. Ingham 4/94

      character*(*) buffer
      logical isrange
      character(1) chara
      integer LENACT,i,length

      isrange = .TRUE.
      i = 1

      length = LENACT(buffer)
      do i=1,length
         chara = buffer(i:i)
         if(chara.ne.'-'.and.chara.ne.' '.and.chara.ne.','
     &        .and.chara.ne.'*'.and.chara.ne.'0'
     &        .and.chara.ne.'1'.and.chara.ne.'2'.and.chara.ne.'3'
     &        .and.chara.ne.'4'.and.chara.ne.'5'.and.chara.ne.'6'
     &        .and.chara.ne.'7'.and.chara.ne.'8'.and.chara.ne.'9') then
            isrange = .FALSE.
            go to 999
         endif
      enddo

 999  return
      end
c
c -----------------------------------------------------------
      SUBROUTINE XSL_LCPLOT()

c Plots the lightcurve using FPLOT. The only slightly complicated
c issue is that it reads the rate file to get the start time and
c writes this to the top of the plot
c  kaa  10/1/99

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
      INCLUDE 'xselplt.inc'

      INTEGER MXCMDS
      PARAMETER (MXCMDS=30)

      DOUBLE PRECISION toffset, tfirst, dmjdtime, timdel

      INTEGER itimes(4)
      INTEGER len1, ilun, block, tcnum, mjd, i, totbin, nusrcm

      character(80) qdpbuf(MXCMDS)
      CHARACTER(1024) comlin
      CHARACTER(255) cstr, pcofil, qdpstr
      CHARACTER sign
      CHARACTER(72) contxt, comment

      LOGICAL qanyf

      INTEGER lenact
      EXTERNAL lenact

      DATA pcofil /'xsl_pco.tmp'/

c First we need to read the rate file to get the time of the first event
c since we are going to want to write that into the header of the plot.

      CALL getlun(ilun)
      CALL ftopen(ilun, curfits, 0, block, status)
      contxt = 'Failed to open '//curfits(:lenact(curfits))
      IF ( status .NE. 0 ) GOTO 999

c Find the rate extension

      CALL ftmnhd(ilun, 2, 'RATE', 0, status)
      contxt = 'Failed to find RATE extension'
      IF ( status .NE. 0 ) GOTO 998

c Check that there are actually some bins to plot

      CALL FTGKYJ(ilun, 'NAXIS2', totbin, comment, status)
      contxt = 'Failed to read NAXIS2 keyword'
      IF ( status .NE. 0 ) GOTO 998
      IF ( totbin .EQ. 0 ) THEN
         CALL xwrite('Lightcurve has no bins', 5)
         CALL ftclos(ilun, status)
         CALL frelun(ilun)
         RETURN
      ENDIF

c Get the binsize

      call FTGKYD(ilun, 'TIMEDEL', timdel, comment, status)
      contxt = 'Failed to find TIMEDEL keyword'
      IF ( status .NE. 0 ) GOTO 998

c Get the TIMEZERO time offset

      CALL xsl_getkif(ilun, 'TIMEZERO', 'TIMEZERI', 'TIMEZERF',
     &                toffset, status)
      contxt = 'Failed to find TIMEZERO keyword(s)'
      IF ( status .NE. 0 ) GOTO 998

c Find the time of the first element in the table and add to toffset

      CALL ftgcno(ilun, .TRUE., 'TIME', tcnum, status)
      contxt = 'Failed to find TIME column'
      IF ( status .NE. 0 ) GOTO 998
      CALL ftgcvd(ilun, tcnum, 1, 1, 1, 0.0d0, tfirst, qanyf, status)
      contxt = 'Failed to read first element of TIME column'
      IF ( status .NE. 0 ) GOTO 998
      toffset = toffset + tfirst

c We are done with the FITS file

      CALL ftclos(ilun, status)
      contxt = 'Failed to close file'
      IF ( status .NE. 0 ) GOTO 999
      CALL frelun(ilun)

c Open the file containing a list of PLT commands to run after first
c deleting any file of the same name left around

      CALL XSL_RMFILE(pcofil)

      CALL getlun(ilun)
      CALL XSL_OPEN(ilun, pcofil, 'NEW', ' ', ' ', 0, 0, status)
      contxt = 'Failed to open '//pcofil(:lenact(pcofil))
      IF ( status .NE. 0 ) GOTO 999

c Construct the label for the top of the plot

      dmjdtime = mjdrefi + mjdreff + toffset/86400
      mjd = INT(dmjdtime)
      CALL sla_dd2tf(4, (dmjdtime-DBLE(mjd)), sign, itimes)
      cstr = ' '
      WRITE(cstr,'(a,i6,a)') 'LA OT Offset = ', mjd, ' '
      len1 = 22
      DO i = 1, 3
         WRITE(cstr(len1+1:len1+3),'(i2.2,'':'')') itimes(i)
         len1 = len1 + 3
      ENDDO
      cstr(len1:len1) = '.'
      WRITE(cstr(len1+1:len1+4),'(i4.4)') itimes(4)
      len1 = len1 + 4
      WRITE(cstr(len1+1:),'(a,f20.6,a)') ' (SC time: ', toffset, ' )'
      len1 = lenact(cstr)

      WRITE(ilun,'(a)') cstr(:len1)

c Construct the label giving the binsize

      WRITE(cstr,'(a,1pg13.6,a)') 'LA T Binsize = ', timdel, keyuni
      WRITE(ilun,'(a)') cstr(:len1)

c and the filename

      cstr = 'LA F '//curfits(:lenact(curfits))
      WRITE(ilun,'(a)') cstr(:len1)

c force errors to be plotted and no line

      WRITE(ilun,'(a)') 'ERROR ON 2'
      WRITE(ilun,'(a)') 'LINE OFF 2'

c check whether the user has specified any QDP commands and add them in

      CALL xsl_uclgst('qdp_commands', qdpstr, status)
      IF ( status .NE. 0 ) RETURN
      IF (qdpstr.ne.'NONE') THEN
         CALL xsl_parse(qdpstr, qdpbuf, nusrcm, MXCMDS, status)
         DO i = 1, nusrcm
            WRITE(ilun,'(a)') qdpbuf(i)
         ENDDO
      ENDIF

      CLOSE(ilun)
      CALL frelun(ilun)

c Open the command file

      CALL xsl_opcf(cmdfil, ilun)

c Set up the command line to run fplot

      comlin = fpltwin(:lenact(fpltwin))//' fplot '//
     &         'infile='//curfits(:lenact(curfits))//' '//
     &         'xparm=TIME yparm=RATE[ERROR] rows="-" '//
     &         'device="'//plotdv(:lenact(plotdv))//'" '//
     &         'pltcmd="@'//pcofil(:lenact(pcofil))//'" '//
     &         'offset=yes maxpts=100000 '//
     &         'binmode=DEFAULT sensecase=no'

      CALL xsl_wrtcf(ilun, comlin, 1)

      comlin = '/bin/rm -f '//pcofil(:lenact(pcofil))
      CALL xsl_wrtcf(ilun, comlin, 1)

      CALL xsl_clcf(ilun)
      CALL xsl_runcf(cmdfil, ECHO, status)

      IF ( status .NE. 0 ) THEN
         CALL xwrite('Failed to run fplot on lightcurve file', 5)
         RETURN
      ENDIF

 998  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL xwrite(contxt, 5)
         WRITE(contxt, '(a,i4)') 'Status = ', status
         CALL xwrite(contxt, 5)
         status = 0
         CALL ftclos(ilun, status)
         CALL frelun(ilun)
         RETURN
      ENDIF

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL xwrite(contxt, 5)
         WRITE(contxt, '(a,i4)') 'Status = ', status
         CALL xwrite(contxt, 5)
      ENDIF

      RETURN
      END

c
c -----------------------------------------------------------
      SUBROUTINE XSL_SPPLOT()

c Plots the spectrum using FPLOT.
c  kaa  1/24/01

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
      INCLUDE 'xselplt.inc'

      INTEGER MXCMDS
      PARAMETER (MXCMDS=30)

      INTEGER len1, ilun, i, nusrcm

      character(80) qdpbuf(MXCMDS)
      CHARACTER(1024) comlin 
      CHARACTER(255) cstr, pcofil, qdpstr
      CHARACTER(72) contxt

      INTEGER lenact
      EXTERNAL lenact

      DATA pcofil /'xsl_pco.tmp'/

c Open the file containing a list of PLT commands to run after first
c deleting any file of the same name left around

      CALL XSL_RMFILE(pcofil)

      CALL getlun(ilun)
      CALL XSL_OPEN(ilun, pcofil, 'NEW', ' ', ' ', 0, 0, status)
      contxt = 'Failed to open '//pcofil(:lenact(pcofil))
      IF ( status .NE. 0 ) GOTO 999

c Construct the label for the top of the plot

      cstr = 'LA T Plot of Spectrum for '
     +       //instru(:LENACT(instru))//' '//
     +       fpltbkg(:lenact(fpltbkg))

      len1 = lenact(cstr)

      WRITE(ilun,'(a)') cstr(:len1)

c force errors to be plotted and no line

      WRITE(ilun,'(a)') 'ERROR ON 2'
      WRITE(ilun,'(a)') 'LINE OFF 2'

c check whether the user has specified any QDP commands and add them in

      CALL xsl_uclgst('qdp_commands', qdpstr, status)
      IF ( status .NE. 0 ) RETURN
      IF ( qdpstr .NE. 'NONE' ) THEN
         CALL xsl_parse(qdpstr, qdpbuf, nusrcm, MXCMDS, status)
         DO i = 1, nusrcm
            WRITE(ilun,'(a)') qdpbuf(i)
         ENDDO
      ENDIF

      CLOSE(ilun)
      CALL frelun(ilun)

c Open the command file

      CALL xsl_opcf(cmdfil, ilun)

c Set up the command line to run fplot

      comlin = fpltwin(:lenact(fpltwin))//' fplot '//
     &         'infile='//spcfil(:lenact(spcfil))//' '//
     &         'xparm=CHANNEL yparm=COUNTS rows="-" '//
     &         'device="'//plotdv(:lenact(plotdv))//'" '//
     &         'pltcmd="@'//pcofil(:lenact(pcofil))//'" '//
     &         'offset=no maxpts=250000'

      CALL xsl_wrtcf(ilun, comlin, 1)

      comlin = '/bin/rm -f '//pcofil(:lenact(pcofil))
      CALL xsl_wrtcf(ilun, comlin, 1)

      CALL xsl_clcf(ilun)
      CALL xsl_runcf(cmdfil, ECHO, status)

      IF ( status .NE. 0 ) THEN
         CALL xwrite('Failed to run fplot on spectrum file', 5)
         RETURN
      ENDIF

      IF ( status .NE. 0 ) THEN
         CALL xwrite(contxt, 5)
         WRITE(contxt, '(a,i4)') 'Status = ', status
         CALL xwrite(contxt, 5)
         status = 0
         CALL ftclos(ilun, status)
         CALL frelun(ilun)
         RETURN
      ENDIF

 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL xwrite(contxt, 5)
         WRITE(contxt, '(a,i4)') 'Status = ', status
         CALL xwrite(contxt, 5)
      ENDIF

      RETURN
      END

c
c -----------------------------------------------------------
      subroutine XSL_TIME_CURSOR(PLTQDP,NEWSEL,ctifile,UT,MJD,ANSW,
     &                           KEYBD)
c -----------------------------------------------------------
c This puts up the lightcurve, and allows the user to enter an ascii
c timing selection region.  The file is written to filenm.
c  J. Ingham 6/3/93

      include 'xsel.inc'
      include 'xselvar.inc'

      character*(*) ctifile
      double precision toffset,timdel,dstart,dstop
      integer MXCLBN,MXCMD,MXVEC,MXNINT,MXLCLN
      parameter (MXCLBN = 131072, MXCMD = 100, MXVEC = 20, MXNINT = 50)
      real    xmin,xmax,tstart(MXNINT),tstop(MXNINT),x1,x2,lx(2)
      real    ymin,ymax,ly(2),y1,y2,lcdata(MXCLBN)
      double precision tlims(2), dtimes(2)
      integer iery(MXVEC),itimes(4),ntimes
      integer ichat,nrow,npts,nvec,ncmd,i,j,ncmdt,nptst
      integer len1,LENACT,nint,PGBEGIN,headlen
      character chr1,chr2,sign
      character(2) twochr
      character(4) answer,fourchr
      character(85) cmd(MXCMD)
      character(255) str1,str2
      logical FIRST,NEWSEL,ANSW,PLTQDP,UT,MJD,KEYBD
      double precision dblstart(MXNINT),dblstop(MXNINT)
      character(32) headline

      status = 0
      headlen = 1
      headline=' '
      
c There are 5 columns in the lcdata array:
c { TIME, TIME_ERROR, RATE, RATE_ERROR, DEADTIME },
c so MXLCLN is the maximum number of points in the light curve:
      MXLCLN = MXCLBN/5

      nint = 0

      ichat = 0

C To make a selection, or no:
      if( .NOT. ANSW ) then
         call XWRITE(' ',5)
         call XWRITE('Note: cursor timing selection is now done'//
     &        ' with the command',5)
         call XWRITE('   FILTER TIME CURSOR',5)
         call XWRITE(' ',5)
         goto 210
      endif

c For the MOUSE

      IF(.NOT. KEYBD) THEN
         CALL xwrite(' ', 5)
         CALL xwrite(
     &      'To start selection enter quit at the PLT prompt then', 5)
         CALL xwrite(
     & ' - to select a time range click the left mouse button on ', 5)
         CALL xwrite(
     & '   the start time then again on the stop time. To cancel ', 5)
         CALL xwrite(
     & '   the selection click the second time outside the plot.', 5)
         CALL xwrite(
     & ' - to exclude a time range press e on the keyboard when the', 5)
         CALL xwrite(
     & '   cursor is at the start time then again at the stop time', 5)
         CALL xwrite(
     & ' - press x on the keyboard to finish cursor selection and', 5)
         CALL xwrite(
     & '   return to the xselect prompt', 5)
         CALL xwrite(
     & ' - press c on the keyboard to cancel cursor selection and', 5)
         CALL xwrite(
     & '   return to the xselect prompt', 5)
         CALL xwrite(
     & ' - press p on the keyboard to return to the PLT prompt', 5)
c For the KEYBOARD
      ELSE
         call XWRITE('Enter as: ''mode start, stop''',5)
         call XWRITE('mode = c means cancel selection ',5)
         call XWRITE('mode = e means exclude the interval',5)
         call XWRITE('mode = i means include the interval',5)
         call XWRITE('mode = m means switch to mouse mode',5)
         call XWRITE('mode = p means return to PLT prompt',5)
         call XWRITE('mode = x means exit, writing selection '
     &        ,5)
         call XWRITE(' ',5)
         call XWRITE('start = l starts the interval at the left'//
     &        ' edge of the plot.',5)
         call XWRITE('stop  = r stops the interval at the right'//
     &        ' edge of the plot.',5)
         IF ( .NOT. UT .AND. .NOT. MJD ) THEN
            call XWRITE(' ',5)
            call XWRITE('N.B. Enter times as shown on the plot''s'//
     &           ' X-axis, I will apply the offset for you.',5)
         ENDIF
      ENDIF
      call XWRITE(' ',5)
      call XWRITE('Enter QUIT at PLT prompt to continue',5)
c Read in the light curve data

 210  IF(PLTQDP) THEN
         call RDQDP(ichat,0,curfil,lcdata,MXCLBN,iery,MXVEC,
     &        nrow,npts,nvec,cmd,MXCMD,ncmd,status)

         if(status.ne.0) then
            call XWRITE('Error reading QDP curve file',5)
            go to 999
         endif
      ELSE
         call XSL_FITS_2_QDP(curfits,keytim,keyrat,keyrte,ratext,
     &        lcdata,MXCLBN,MXLCLN, iery,MXVEC,
     &        nrow,npts,nvec,cmd,MXCMD,ncmd,status)


         IF(status.ne.0) then
            call XWRITE('Error reading FITS curve file',5)
            write(str1,'(a,i3)') 'FITSIO Error no: ',status
            call XWRITE(str1,5)
            return
         ENDIF
      ENDIF

      len1 = LENACT(plotdv)
      IF(PGBEGIN(0,plotdv(1:len1),1,1).ne.1) THEN
         call XWRITE('Error in device type',5)
         goto 999
      ENDIF
      IF(ANSW.AND..NOT.KEYBD) THEN
         call PGQINF('CURSOR',answer,len1)
         IF(answer.eq.'no'.or.answer.eq.'NO') THEN
            call XWRITE('Your device has no cursor,',5)
            ANSW = .FALSE.
         ENDIF
      ENDIF
c Tack on skip line so that we can plot the selected ranges
c Note the resetting of colors because PLT numbers vector
c differently when skip is set.

      DO i = 1, 5
         lcdata((i-1)*nrow+npts+1) = -1.2e-34
      ENDDO

      npts = npts + 1
      cmd(ncmd+1) = 'SKIP SINGLE'
      cmd(ncmd+2) = 'COL 2 ON 1'
      cmd(ncmd+3) = 'COL 0 ON 2'
      cmd(ncmd+4) = 'COL 1 ON 3'
      cmd(ncmd+5) = 'COL 0 ON 4'
      ncmd = ncmd + 5

c
c  Now the toffset:
c  And get the xmin and xmax for the unscaled plot:
c  use xmin and xmax as temp flags.
      xmin = -1
      xmax = -1
      do i=1,ncmd
c         print*,cmd(i)(:LENACT(cmd(i)))
         if(index(cmd(i),'Label Top').ne.0) then
            read(cmd(i),50,END=55,ERR=55) toffset,timdel
 50         format(33x,G23.16,12x,g13.6)
c This is really disgusting, I write it one way in XSL_FITS_2_QDP,
c then take it apart and rewrite it here.
c This is to support the QDP format which no one uses any more...
c Convert to MJD day plus hour, min, sec.
            dstart = mjdrefi + mjdreff + toffset/86400D0
            len1 = int(dstart)
            dstop = dstart - dble(len1)
            call sla_dd2tf(4,dstop,sign,itimes)
            write(str1,'(''MJD '',I6)') len1
            len1 = 12
            do j=1,3
               if(itimes(j).lt.10) then
                  write(twochr,'(''0'',I1)') itimes(j)
               else
                  write(twochr,'(I2)') itimes(j)
               endif
               str1 = str1(:len1)//twochr//':'
               len1 = len1 + 3
            enddo
            if(itimes(4).lt.10) then
               write(fourchr,'(''000'',I1)') itimes(4)
            else if(itimes(4).lt.100) then
               write(fourchr,'(''00'',I2)') itimes(4)
            else if(itimes(4).lt.1000) then
               write(fourchr,'(''0'',I3)') itimes(4)
            else if(itimes(4).lt.10000) then
               write(fourchr,'(I4)') itimes(4)
            endif
            str1 = str1(:len1-1)//'.'//fourchr
            write(cmd(i),'(a,a,a,F20.6,a)') 'Label OTop Offset = ',
     &            str1(:len1+4),' (SC time: ',toffset,' )'
            write(cmd(ncmd+1),'( ''Label Top Binsize = '',f10.4,a)')
     &           timdel,keyuni(:lenact(str1))
            xmin = 0
         else if(index(cmd(i),'rescale x').ne.0) then
            read(cmd(i)(index(cmd(i),'0')+1:),*,
     &           END=55,ERR=55,iostat=ierr) tlims(2)
            xmax = 0
         endif
 55      continue
      enddo

      tlims(1) = 0
      IF(xmin.eq.-1) THEN
         call XWRITE('Could not find Toffset',5)
      ELSE
C I added one command, so...
         ncmd = ncmd + 1
      ENDIF
      IF(xmax.eq.-1) THEN
         call XWRITE('Could not get min and max times for the plot'
     &        ,5)
      ENDIF

      call PLT(lcdata,iery,nrow,npts,nvec,cmd,ncmd,ierr)
      if(.NOT.ANSW) THEN
         goto 998
      endif
c Now set the position for the interval line

      call PGQWIN(xmin,xmax,ymin,ymax)
c      tlims(2) = xmax
c      tlims(1) = xmin

      ly(1) = ymin + 0.95*(ymax-ymin)
      ly(2) = ly(1)
c Now get the mouse clicks:
      chr2 = 'i'
      first = .TRUE.
      DO WHILE (chr2.ne.'x' .and. chr2.ne.'c')
         IF(.not. KEYBD) THEN
            call PGCURSE(x1,y1,chr1)
c Process the first mouse click:
c Exit
            IF(chr1.eq.'x') THEN
               chr2 = 'x'
               goto 990
            ELSEIF(chr1.eq.'c') THEN
               chr2 = 'c'
               goto 990
c Switch to Keyboard mode
            ELSE IF(chr1.eq.'k') then
               KEYBD = .TRUE.
               goto 300
c Go back to PLT prompt
            ELSE IF(chr1.eq.'p') THEN
               ncmdt = ncmd
c              ncmdt = ncmdt + 1
c              WRITE(cmd(ncmdt),*) 'r ', xmin, xmax, ymin, ymax
               DO i = 1, nint
                  lcdata(npts+i) = (tstart(i)+tstop(i))/2.
                  lcdata(npts+nrow+i) = (tstop(i)-tstart(i))/2.
                  lcdata(npts+2*nrow+i) = (ly(2)+ly(1))/2.
                  lcdata(npts+3*nrow+i) = (ly(2)-ly(1))/2.
                  lcdata(npts+4*nrow+i) = 1.
               ENDDO
               nptst = npts + nint
               call PLT(lcdata,iery,nrow,nptst,nvec,cmd,ncmdt,ierr)
               call PGQWIN(xmin,xmax,ymin,ymax)
               ly(1) = ymin + 0.95*(ymax-ymin)
               ly(2) = ly(1)
               goto 320
            ENDIF
c If the first click was outside the plot, start again
            IF(x1.gt.xmax.or.x1.lt.xmin.or.y1.gt.ymax.or.
     &           y1.lt.ymin) THEN
               goto 300
            ENDIF
c Mark the first point:
            call PGSCI(3)
            IF(chr1.eq.'e') THEN
               call PGPT(1,x1,ly(1),5)
            ELSE
               call PGPT(1,x1,ly(1),2)
            ENDIF
            call PGSCI(1)
c Now get the second mouse click:
            call PGCURSE(x2,y2,chr2)
c Put cursor on the left
            IF(chr2.eq.'l') THEN
               x2 = SNGL(tlims(1))
c Put the cursor on the right
            ELSE IF(chr2.eq.'r') THEN
               x2 = SNGL(tlims(2))
c If the second click is outside the plot, start again:
            ELSE IF(x2.gt.xmax.or.x2.lt.xmin.or.y2.gt.ymax.or.
     &              y2.lt.ymin) THEN
               call PGSCI(0)
               IF(chr1.eq.'e') THEN
                  call PGPT(1,x1,ly(1),5)
               ELSE
                  call PGPT(1,x1,ly(1),2)
               ENDIF
               call PGSCI(1)
               goto 300
            ELSE
C Synch the second point to the time grid:
C            do i=1,npts
C               if(x2.gt.lcdata(i)) then
C                  x2 = lcdata(i) + timdel
C                  goto 501
C               endif
C            enddo
C 501        continue

            ENDIF
C DO KEYBOARD input
         ELSE
            call XCREAD('Enter start and stop times > ',str1,ierr)
c
c This IF block grabs the character, and if the indication is that
c there will be no start and stops this time, jump over the plotting part
c
            IF(index(str1,'x').ne.0) THEN
C Exit the read loop
               chr2 = 'x'
               goto 990
            ELSE IF (index(str1,'c').ne.0) then
               chr2 = 'c'
               goto 990
            ELSE IF(index(str1,'m').ne.0) THEN
C Switch to the mouse driven mode
               KEYBD = .FALSE.
               goto 300
            ELSE IF(index(str1,'p').ne.0) THEN
C Go back to PLT
               ncmdt = ncmd
c              ncmdt = ncmdt + 1
c              WRITE(cmd(ncmdt),*) 'r ', xmin, xmax, ymin, ymax
               DO i = 1, nint
                  lcdata(npts+i) = (tstart(i)+tstop(i))/2.
                  lcdata(npts+nrow+i) = (tstop(i)-tstart(i))/2.
                  lcdata(npts+2*nrow+i) = (ly(2)+ly(1))/2.
                  lcdata(npts+3*nrow+i) = (ly(2)-ly(1))/2.
                  lcdata(npts+4*nrow+i) = 1.
               ENDDO
               nptst = npts + nint
               call PLT(lcdata,iery,nrow,nptst,nvec,cmd,ncmdt,ierr)
               call PGQWIN(xmin,xmax,ymin,ymax)
               ly(1) = ymin + 0.95*(ymax-ymin)
               ly(2) = ly(1)
               goto 320
            ELSE IF( index(str1,'i').ne.0) THEN
               chr1 = 'i'
               str1 = str1(index(str1,'i')+1:)
            ELSE IF (index(str1,'e').ne.0 ) THEN
               chr1 = 'e'
               str1 = str1(index(str1,'e')+1:)
            ELSEIF(index(str1,'R').ne.0) THEN
                  len1 = index(str1,'R')
c Add a space to seperate the reference time from the next field:
                  headline = str1(len1 + 1:LENACT(str1))//' '
                  headlen = LENACT(headline) + 1
                  str1 = 'Got Reference time: '//headline(:headlen)
                  call XWRITE(' ',5)
                  call XWRITE(str1,5)
                  call XWRITE(' ',5)
                  goto 300
            ENDIF
c
c Now parse the start and stop times, and then proceed to plot them...
c

            if( index(str1,',') .eq. 0 ) then
               call XWRITE
     &              ('Please reenter the data comma delimited.',5)
               goto 300
            endif

            call str2sec(str1, UT, MJD, headline, tlims, dtimes,
     &                   2, ntimes)
            IF ( status .NE. 0 .OR. ntimes .NE. 2 ) THEN
               call XWRITE('Error translating '//
     &           'start and/or stop time, please re-enter times', 5)
               WRITE(str1,'(a,i6,a,i2)') 'status = ', status, 
     &                                   ' ntimes = ', ntimes
               call XWRITE(str1, 5)
               goto 300
            ENDIF

c if either UT or MJD then subtract the SCC offset. If SCC then the time
c will have been entered as relative to the offset. If the times were set
c to the left or right edge of the plot then they are relative to the offset
c even if UT or MJD is in use.

            x1 = SNGL(dtimes(1))
            x2 = SNGL(dtimes(2))
            IF ( UT .OR. MJD ) THEN
               IF (dtimes(1) .NE. tlims(1)) x1 = x1 - SNGL(toffset)
               IF (dtimes(2) .NE. tlims(2)) x2 = x2 - SNGL(toffset)
            ENDIF

         ENDIF

c ERASE THE LINES, AND THE END POINT
         call PGSCI(0)
         do i=1,nint
            lx(1) = tstart(i)
            lx(2) = tstop(i)
            call PGLINE(2,lx,ly)
         enddo
         IF(chr1.eq.'e') THEN
            call PGPT(1,x1,ly(1),5)
         ELSE
            call PGPT(1,x1,ly(1),2)
         ENDIF
         call PGSCI(1)
c If this is the first entry, and excluded region is chosen,
c Include the rest of the region.
         IF(FIRST) THEN
            FIRST = .FALSE.
            if(chr1.eq.'e') then
               nint = 1
               tstart(1) = SNGL(tlims(1))
               tstop(1) = SNGL(tlims(2))
            endif
         ENDIF
         IF(chr1.ne.'e') THEN
c MERGE THE LINES
            call XSL_INSERT(nint,tstart,tstop,MXNINT,x1,x2,'IN')
         ELSE
            call XSL_INSERT(nint,tstart,tstop,MXNINT,x1,x2,'EX')
         ENDIF
c NOW DRAW THE NEW LINE
 320     do i=1,nint
            lx(1) = tstart(i)
            lx(2) = tstop(i)
            call PGLINE(2,lx,ly)
         enddo
c
c Make sure you didn't overflow the tstart and tstop arrays...
c
         IF ( nint.eq.MXNINT ) THEN
            call XWRITE('You have added the maximum number of'//
     &           'intervals, I will stop now!',5)
            go to 990
         ENDIF
 300     CONTINUE
      ENDDO

 990  call PGEND

c Now WRITE the cursor timing file:
c If no lines were entered, then don't bother to open the file.
      if(nint.eq.0.or.chr2.eq.'c') goto 999
      len1 = lenact(ctifile)
      call XSL_FILENAME(ctifile,str2,len1)
      str2 = 'Writing timing selections to file '//str2(:len1)
      call XWRITE(str2,5)
c Open a new FITS file for the timing files:
      call XSL_RMFILE(ctifile)
      do i=1,nint
         dblstart(i) = dble(tstart(i)) + toffset
         dblstop(i) = dble(tstop(i)) + toffset
      enddo

      call XSL_WRITE_GTI(ctifile,dblstart,dblstop,nint,MXNINT,
     &     mjdrefi,mjdreff,keyuni,status)

      if (status.ne.0) then
         call XWRITE('Error writing data to FITS '//
     &        'Cursor timing file',5)
         write(str1,'(''FITSIO error number:'',I3)') status
         call XWRITE(str1,5)
      else
c Set the timing selection logical
         NEWSEL = .TRUE.
      endif

      return

 998  call PGEND
 999  return

      end
c
c ---------------------------------------------
      subroutine XSL_LSTDEV(devlst,MAXDEV,ndev,status)
c ---------------------------------------------
c This lists the available device drivers:
c This is just hacking of GRDLEV, in PGPLOT
c Jim Ingham 8/93
      integer MAXDEV,status
      integer i,j,ndev,nbuf,lchr
      real rbuf(6)
      character(72) chr
      character*(*) devlst(MAXDEV)

c This gets the number of drivers currently loaded
      call grexec(0,0,rbuf,nbuf,chr,lchr)
      ndev = nint(rbuf(1))
      IF(ndev.gt.MAXDEV) then
         call XWRITE('You need a bigger buffer for device types',5)
         write(chr,10) ndev
 10      format('number of devices = ',i3)
         call XWRITE(chr,5)
         status = -10
         return
      ENDIF

c Now list the drivers:

      do i=1,ndev
         call grexec(i,1,rbuf,nbuf,chr,lchr)
c There is a (... comment at the end of the device type,
c which I don't want.

         do j=1,lchr
            if(chr(j:j).eq.'(') then
               lchr = j - 1
               goto 40
            endif
         enddo
 40      devlst(i) = '/'//chr(:lchr)
      enddo

      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_LOADCAT()
c ---------------------------------------------
c  This routine loads the catalogue from datdir
c  You can specify any catalogue name, or if you specify 'def', you
c will be prompted for a data dir, and the program will look for a
c catalogue with the default obscat name that would have
c been constructed if the session name had been the last member
c of the path to the datdir.
c   J. Ingham 6/93
      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1
c File I/O unit numbers

c General reusable integers for lengths of strings
      integer len1, len2, len3
c ---------------------------------------------
      integer MXNKWD
      parameter (MXNKWD = 5)
      character(64) kwdval(MXNKWD),chosen(1,4)
      character(16) kwdnam(MXNKWD)
      character(255) chonam
      character(255) dumnam
      integer LENACT,kwdst(MXNKWD),nchose,i
      logical BRIEF,ALL,ONLYDD

      data (kwdnam(i),i=1,MXNKWD)  /'TELESCOP', 'INSTRUME',
     &                               'DATADIR','HKDIR','DATAMODE'/
      status = 0

c ---------------------------------------------------
c LOAD THE OBSCAT
c Set MAKE to FALSE, and reset the catalogue selections:
      MADE = .FALSE.
      LOADED = .FALSE.
      catflt = 'NONE'
      choflt = 'NONE'
      catsel = 'NONE'
c Do a clear data to start, but keep the data directory:
      dumnam = datdir
      call XSL_CLEAR(2)
      datdir = dumnam

      call xsl_uclgst('load_str',str1,status)
      IF(status.ne.0) THEN
         call XWRITE('Error getting dummy parameter',5)
         return
      ENDIF

c If nothing was entered on the command line for the obscat name,
c then list the obscats.  If 'def' was given, find the default named
c catalogue in the data directory, otherwise use what was given.

      IF(str1.eq.'NOT_ENTERED') then
         call xsl_uclgsb('brief',BRIEF,status)
         IF ( status .NE. 0 ) RETURN
         call xsl_uclgsb('all',ALL,status)
         IF ( status .NE. 0 ) RETURN
         call xsl_uclgsb('only_datadir',ONLYDD,status)
         IF ( status .NE. 0 ) RETURN
         call XSL_SHOWCATS(1,chonam,chosen,nchose,1,ONLYDD,BRIEF,ALL)
         IF(status .eq. -30) THEN
            status = 0
            return
         ELSE IF(status.ne.0) THEN
            call XWRITE('Error in XSL_SHOWCATS',5)
            return
         ELSE
c set up the new catalogue:
            catnam = chonam
            call XSL_CAT_SETUP(chosen(1,1),chosen(1,2),chosen(1,3),
     &           chosen(1,4), keymis,instru,
     &           datdir,mkfdir,hkdir,status)
            if(status.eq.-20) then
               call XWRITE('No catalogue loaded',5)
               return
            endif
C This is a Hack to get the datamode, should really go in SETUP and SHOW
            call XSL_GETKWST(chonam,'1','NONE','DATAMODE',datamode,
     &           kwdst(1),1,1,.FALSE.,status)
            if(kwdst(1).eq.0.and.datamode.ne.'NONE') then
               call XSL_SET(6)
            endif
c A new catalogue has been chosen, make sure obsno is set:
            call XSL_MDB_INST(keymis,submis,instru,obsno,status)
            status = 0
            go to 777
         ENDIF
      ELSE IF(str1.eq.'def'.or.str1.eq.'DEF') THEN
c get the instrument, if none is set
         IF(instru.eq.'NONE') then
            call XSL_SET(1)
         ENDIF
c get the data directory if none is set
         IF(datdir.eq.'NONE') THEN
            call XSL_GET_DIR('data_dir','NONE',datdir,status)
            if ( status .ne. 0) then
               return
            endif
         ENDIF

c Now get the last member of the path containing the catalogue
         call XSL_PATHEND(datdir,str1,len3)
c Deal with errors
         if(len3.eq.0) THEN
            call XWRITE('Cannot get default name from '//
     &                                    'directory path:',5)
            call XWRITE('     '//datdir(:LENACT(datdir)),5)
            return
         endif
c Now get the obsno
         call XSL_MDB_INST(keymis,submis,instru,obsno,status)
c Now we must strip off the session prefix, and add the lastmember
c and the directory.
         len1 = LENACT(prefix)
         len2 = LENACT(obscat(obsno))
         call XSL_FILENAME(obscat(obsno),dumnam,len2)
         catnam = str1(:len3)//dumnam(len1+1:len2)
         call XSL_DATDIR(catnam,datdir,0)
c Now make sure the catalogue exists:
         call XSL_EXIST(catnam,status)
         IF(status.ne.0) then
            len1 = LENACT(catnam)
            call XWRITE('Catalogue '//catnam(:len1)//'not found.',
     &           5)
            return
         ENDIF
      ELSE
c copy over the name
         catnam = str1
c This bit checks to see if catnam has a directory path
c or just a filename
         len2 = LENACT(catnam)
         len1 = len2
         call XSL_FILENAME(catnam,str1,len2)

c If it is just a filename, prepend catdir...
         IF(len1.eq.len2) THEN
            call XSL_DATDIR(catnam,catdir,0)
         ENDIF
c Now make sure the catalogue exists:
         call XSL_EXIST(catnam,status)
         IF(status.ne.0) then
            len1 = LENACT(catnam)
            call XWRITE('Catalogue '//catnam(:len1)//'not found.',
     &           5)
            return
         ENDIF
         call XSL_GETKWST(catnam,'1',wrkdir,kwdnam,kwdval,kwdst,
     &                        MXNKWD,MXNKWD,.FALSE.,status)
         call XSL_CAT_SETUP(kwdval(1),kwdval(2),kwdval(3),kwdval(4),
     &        keymis,instru,datdir,mkfdir,hkdir,status)
         if(status.eq.-20) then
            call XWRITE('No catalogue loaded',5)
            return
         endif
         if(kwdst(5).eq.0) then
            datamode = kwdval(5)(:min(lenact(kwdval(5)),len(datamode)))
            call XSL_SET(6)
         endif
      ENDIF
c Now display the catalogue

 777  LOADED = .TRUE.
      status = 0

      IF(SHOWOC) THEN
         call XSL_DUMPCAT(.FALSE.,'-','no')
      ENDIF



      return
      end
c
c
c ----------------------------------------------
      subroutine XSL_LOAD_NAMES(mode)
c ----------------------------------------------
c This gets the session name, and loads all the file names
c   J. Ingham

      include 'xsel.inc'
      include 'xselvar.inc'

      integer mode,LENACT,len1,len2
      character(512) str1

c XSLTYP is the filetype of the temporary workspace files
      xsltyp = '.xsl'
c FTYPE is the filetype of the input data files
      ftype = '.fits'

      If(mode.eq.0) then
         call xsl_uclgst('prefix',prefix,status)
         IF ( status .NE. 0 ) RETURN
      endif
      len1 = LENACT(prefix)

c Get the work directory

      wrkdir = "./"
      call XSL_EXPAND_DIR(wrkdir,status)
      len2 = lenact(wrkdir)
      

c SPCFIL is the temporary file that spectral data will be put in.
      spcfil=wrkdir(:len2)//prefix(:len1)//'_hist.xsl'
c CURFIL is the temporary file that binned QDP light curve data
c will be put in.
      curfil=wrkdir(:len2)//prefix(:len1)//'_qdp_curve.xsl'
c CURFITS is the temporary file that binned QDP light curve data
c will be put in.
      curfits=wrkdir(:len2)//prefix(:len1)//'_fits_curve.xsl'
c IMFIL is the temporary file that an image will be put in.
      imfil=wrkdir(:len2)//prefix(:len1)//'_image.xsl'
c SIMFIL is the temporary file for smoothed images
      simfil=wrkdir(:len2)//prefix(:len1)//'_image_smoothed.xsl'
c UNBFIL is the file for the unbinned light curve for Xronos
      unbfil=wrkdir(:len2)//prefix(:len1)//'_unbinned.xsl'
c CLNIM is the temporary file storing the cleaned SIS image
      clnim=wrkdir(:len2)//prefix(:len1)//'_clean_image.xsl'
c REGFIL is the region file for the extractor
      regfil=wrkdir(:len2)//prefix(:len1)//'_region.xsl'
c DETFIL is the detector filter file for extractions
      detfil=wrkdir(:len2)//prefix(:len1)//'_detector.xsl'
c SISREG is the excluded region from sisclean
      sisreg=wrkdir(:len2)//prefix(:len1)//'_siscleanreg.xsl'
c GTITFL is the fits timing file from HKSEL
      gtiflt=wrkdir(:len2)//prefix(:len1)//'_fits_in.xsl'
c FFFLT is the fits timing file from the filter file
      ffflt = wrkdir(:len2)//prefix(:len1)//'_mkf_out.xsl'
c ALLGTI is the merged GTI file for ALL FITS GTI's
      allgti = wrkdir(:len2)//prefix(:len1)//'_all_gti.xsl'
c ASCTFL is the ascii timing file for the extractor
      ascflt=wrkdir(:len2)//prefix(:len1)//'_ascii_in.xsl'
c ASCOUT is the cumulative timing file, computed by extractor
      ascout=wrkdir(:len2)//prefix(:len1)//'_ascii_out.xsl'
c XWNFLT is the input Xronos window file for BIN
      xwnflt=wrkdir(:len2)//prefix(:len1)//'_xronos_in.xsl'
c XPHFLT is the xronos window file containing phase selections
      xphflt = wrkdir(:len2)//prefix(:len1)//'_xronos_phase.xsl'
c XRONWN is the output window file from bin
      xronwn=wrkdir(:len2)//prefix(:len1)//'_xronos_out.xsl'
c GTITFL is the fits timing file from HKSEL
      hktfl=wrkdir(:len2)//prefix(:len1)//'_hksel_out.xsl'
c HOTPXL is the hot pixel list from sisclean
      hotpxl=wrkdir(:len2)//prefix(:len1)//'_hotpixel.xsl'
c EVNIN is the output events file to be fed back to the extractor
      evnin=wrkdir(:len2)//prefix(:len1)//'_in_event.xsl'
c EVNOUT is the output events file from the extractor
      evnout=wrkdir(:len2)//prefix(:len1)//'_out_event.xsl'
c MERFIL is the temporary file for a merged event dataset.
      merfil=wrkdir(:len2)//prefix(:len1)//'_merged.xsl'
c MRGTIF is the temporary file for the GTI file associated with MERFILE
      mrgtif=wrkdir(:len2)//prefix(:len1)//'_mergti.xsl'
c MRGHKF is the temporary file for the merged HK file
      mrghkf=wrkdir(:len2)//prefix(:len1)//'_merghk.xsl'
c WORK1F, WORK2F are the root filenames for the temporary workspace files
      work1f=wrkdir(:len2)//prefix(:len1)//'_work1'
      work2f=wrkdir(:len2)//prefix(:len1)//'_work2'
c WGTI1F, WGTI2F are ditto for the GTI temporary workspace files
      wgti1f=wrkdir(:len2)//prefix(:len1)//'_workgti1'
      wgti2f=wrkdir(:len2)//prefix(:len1)//'_workgti2'
c WKHK1F,WKHK2F for the HK temporary workspace files
      wkhk1f=wrkdir(:len2)//prefix(:len1)//'_workhk1'
      wkhk2f=wrkdir(:len2)//prefix(:len1)//'_workhk2'
c FFCURF is the root name for the FF curve file workspace
      ffcurf = wrkdir(:len2)//prefix(:len1)//'_ffcurve.fits'
c MERMF is the filename for the merged MKF files:
      mermkf = wrkdir(:len2)//prefix(:len1)//'_mermkf.xsl'
c HKCURF is the root name for the HK curve file workspace
      hkcurf = wrkdir(:len2)//prefix(:len1)//'_hkcurve.fits'
c CTIF is the root name for the cursor timing files:
      ctif = wrkdir(:len2)//prefix(:len1)//'_cursor_gti'
c HNDF is the root name for the Hand entered GTI timing files
      hndf = wrkdir(:len2)//prefix(:len1)//'_keybd_gti'
c INTF is the root name for the intensity GTI files:
      intf = wrkdir(:len2)//prefix(:len1)//'_intensity_gti'
c TEMPFL for storing intermediate catalogues
      tempfl=wrkdir(:len2)//prefix(:len1)//'_temp.xsl'
c SAVFIL stores the saved session data
      savfil=wrkdir(:len2)//prefix(:len1)//'_session.xsl'
c DFEFIL stores the ASCA SIS Dark Frame Error files
      dfefil=wrkdir(:len2)//prefix(:len1)//'_dfe.xsl'
c This is the temporary catalogue made for choose with sel.expr.:
      chocat = wrkdir(:len2)//prefix(:len1)//'_choose_cat.xsl'
c
c                ASCII files -- types .RUN or .TMP
c
c CMDFIL is the list of SELECTOR commands to be executed
      cmdfil=wrkdir(:len2)//prefix(:len1)//'_xsel.run'
c LSTFIL is the list of filenames of input FITS files
      lstfil=wrkdir(:len2)//prefix(:len1)//'_files.tmp'
c OBSFIL is the list of all filenames in the current directory
      obsfil=wrkdir(:len2)//prefix(:len1)//'_obscat.tmp'

c ERRFIL is the dump of stderr for ftools
      errfil=prefix(:len1)//'_error.tmp'

c
c Now set up the workspace filenames:
C     First for the data files

      call XSL_FAFNA(work1f,xsltyp,MAXFIL,work1)
      call XSL_FAFNA(work2f,xsltyp,MAXFIL,work2)

C Then for the GTI files

      call XSL_FAFNA(wgti1f,xsltyp,MAXFIL,gtiwk1)
      call XSL_FAFNA(wgti2f,xsltyp,MAXFIL,gtiwk2)

C Then for the HK files

      call XSL_FAFNA(wkhk1f,xsltyp,MAXFIL, hkwrk1)
      call XSL_FAFNA(wkhk2f,xsltyp,MAXFIL, hkwrk2)

c For the cursor time files
      call XSL_FAFNA(ctif,xsltyp,MXNSEL,ctivec)

c For the hand entered time files
      call XSL_FAFNA(hndf,xsltyp,MXNSEL,hndvec)

c For the intensity GTI's:
      call XSL_FAFNA(intf,xsltyp,MXNSEL,intvec)


c
c                FITS Catalogue Names
c
c Get the catalogue directory

      call xsl_uclgst('def_obscat_dir',catdir,status)
      IF ( status .NE. 0 ) RETURN
      IF(catdir.eq.'WORK'.or.catdir.eq.'work') THEN
         catdir = wrkdir
      ELSE
         call XSL_CHKDIR(catdir,status)
         IF(status.ne.0) THEN
            call XWRITE('The observation catalogue directory:',5)
            str1 = '    '//catdir(:LENACT(catdir))
            call XWRITE(str1,5)
            call XWRITE('does not exist',5)
            call XWRITE('Use set obsdir to set it.',5)
         ENDIF
      ENDIF

      call XSL_SET_OBSCATNAME()

      return
      end
c
c
c -------------------------------------------------
      subroutine XSL_LSTDEL(nin1,nin2,MXNSEL,index,vec1,vec2,status)
c -------------------------------------------------
c This removes the index'th element from vec1, and finds the element
c that matches it from vec2, and removes that as well.  vec1(index) starts
c with a '-' which is stripped before the comparison.  If vec1(index) is
c not found in vec2, the status flag is set to non-zero.
c   J. Ingham  6/93

      integer nin1,nin2,MXNSEL,index,i,j,status
      character*(*) vec1(MXNSEL),vec2(MXNSEL)
      character(255) temp

      temp = vec1(index)(2:)
      status = 0

      do i=1,nin2
         IF(temp.eq.vec2(i)) THEN
            do j=i,nin2-1
               vec2(j) = vec2(j+1)
            enddo
            vec2(nin2) = ' '
            nin2 = nin2 - 1
            goto 100
         ENDIF
      enddo
      status = 10
 100  IF(nin1.eq.1) THEN
         vec1(nin1) = ' '
         nin1 = 0
      ELSE
         do i = index,nin1-1
              vec1(i) = vec1(i+1)
         enddo
         vec1(nin1) = ' '
         nin1 = nin1 - 1
      ENDIF

      return
      end

c ---------------------------------------------------------
      subroutine XSL_MATCH_CMD(parnam, commad, comdes, nwhats,
     &                         MXCMDS, comno, parval, indx, status)
c ---------------------------------------------------------

      INTEGER MXCMDS, nwhats, indx, comno, status
      CHARACTER*(*) parnam, parval
      CHARACTER*(*) commad(MXCMDS), comdes(MXCMDS)

c Get the parameter and match it to one of the allowed commands.
c Arguments :
c   parnam      c        i: The parameter name for the command
c   commad      c        i: A list of valid commands
c   comdes      c        i: The descriptions of the valid commands
c   nwhats      i        i: The number of valid commands
c   MXCMDS      i        i: The size of the commad, comdes arrays
c   comno       i      i/r: Number of arguments on the command line
c   parval      c        r: The command returned
c   indx        i        r: The number of the command in the array  
c   status      i        r: 0 == OK

      CHARACTER(72) tmpstr

      INTEGER lenact
      EXTERNAL lenact

      status = 1

      IF ( comno .EQ. 0 ) THEN
         call XSL_CMDLIST(commad,comdes,nwhats,MXCMDS)
      ENDIF

      IF ( status .NE. 0 ) THEN

         status = 0
         call xsl_uclgst(parnam, parval, status)
         IF ( status .NE. 0 ) RETURN
         call UPC(parval)
         call gmatch(commad, nwhats, parval, indx, status)
         IF ( status .EQ. 1 ) THEN
            tmpstr = parnam(:lenact(parnam))//' argument not found.'//
     &               ' Options are :'
            call XWRITE(tmpstr, 5)
         ELSEIF ( status .EQ. 2 ) THEN
            tmpstr = parnam(:lenact(parnam))//' argument not unique.'//
     &               ' Options are :'
            call XWRITE(tmpstr, 5)
         ENDIF

         IF ( status .NE. 0 ) THEN
            call XSL_CMDLIST(commad,comdes,nwhats,MXCMDS)
         ENDIF

      ENDIF

      RETURN
      END

c ---------------------------------------------------------
      subroutine XSL_MORE(filenm)
c ---------------------------------------------------------
c Use XWRITE to write the file filenm to the screen:
c  J. Ingham 5/93
      character*(*) filenm
      character(4096) str1
      integer ilun, ierr
      logical LOOP

      LOOP = .TRUE.

      call GETLUN(ilun)
      call XSL_OPEN(ilun,filenm,'OLD',' ',' ',0,0,ierr)
      do while (LOOP)
         read(ilun,'(a)',end=99,err = 99) str1
         call XWRITE(str1, 5)
      enddo
 99   close(ilun)
      call FRELUN(ilun)

      return
      end
c
c
c ---------------------------------------------
      subroutine XSL_MOVE_AWAY(filename,newname)
c ---------------------------------------------
c This moves the file filename to a distinct file (newname)made by
c adding _vn before the suffix.  If _vn is found on the
c end, then _v(n+1) is added instead

      character*(*) newname,filename
      character(1) charac
      character(255) tempname
      integer len1,len2,len3,pathlen,oldidx,LENACT,i,status

c If the file doesn't exist, then exit
      call XSL_EXIST(filename,status)
      IF(status.ne.0) THEN
         newname = filename
         return
      ENDIF
c strip off the filename, and work on it
      pathlen = LENACT(filename)
      len1 = pathlen
      call XSL_FILENAME(filename,tempname,len1)
      pathlen = pathlen - len1
c      tempname = filename
      oldidx = 0
 67   len1 = LENACT(tempname)
      len2 = 0
      len3 = 0
c set len2 = position of the last character befor the '.' in filename
      do i=len1,1,-1
         if(tempname(i:i).eq.'.') THEN
            len2 = i - 1
            goto 78
         endif
      enddo
c set len3 = position of the character before
c the '_' in the last occurance of '_v'
 78   do i=len1,1,-1
         if(i.eq.1) THEN
            continue
         else if(tempname(i-1:i).eq.'_v') then
            len3 = i-2
            goto 77
         endif
      enddo

 77   if(len3.eq.0) then
         IF(len2.gt.0) THEN
            newname = tempname(:len2)//'_v1'//
     &           tempname(len2+1:len1)
            oldidx = 1
         ELSE
            newname = tempname(:len1)//'_v1'
            oldidx = 1
         ENDIF
      else
         charac = tempname(len3+3 :len3+3 )
         read(charac,87,err=97) oldidx
 87      format(I1)
         if(oldidx.lt.9) then
            write(newname,88,err = 97) tempname(1:len3+2),oldidx+1,
     &           tempname(len3+4:len1)
 88         format(a,I1,a)
         else
            newname = tempname(1:len3+2)//
     &           '1'//tempname(len3+4:len1)
         endif
      endif
c Now add the path back onto newnwame:
      IF(pathlen.gt.0) then
         newname = filename(:pathlen)//newname
      ENDIF
      call XSL_EXIST(newname,status)
      IF(status.eq.0.and.oldidx.lt.9) THEN
c         if(oldidx.lt.9) THEN
            tempname = newname(pathlen+1:)
            goto 67
c         else
c If we've gotten to 9, erase the first one:
c            newname = filename(:pathlen)//tempname(1:len3+2)//
c     &           '1'//tempname(len3+4:len1)
c         endif
      ELSE
         status = 0
      ENDIF
      len3 = LENACT(newname)
      call XSL_RENAME(filename,newname,1,tempname,len3,status)
c      context = ' * Moving '//filename(:LENACT(filename))//' to '//
c     &     newname
c      call XWRITE(' ',5)
c      call XWRITE(context,5)
c      call XWRITE(' ',5)
      return

 97   call XWRITE('Error writing new file name',5)
      return
      end
c
c
c -----------------------------------------------------
      subroutine XSL_PUTCHAR(string,char,status)
c -----------------------------------------------------
c   This subroutine puts dashes in front of each space delimited
c   element of string
c  J. Ingham 7/93

      character*(*) string,char
      character(255) dummy
      integer len1,len2,start,status,i,LENACT,point

      status = 0
      len1 = LENACT(string)
      dummy = ' '

      IF(len1.gt.255) THEN
         call XWRITE('String length too long in XSL_PUTCHAR',5)
         status = -10
         return
      ENDIF
c First remove leading blanks
      start = 1
      do i=1,len1
         IF(string(i:i).eq.' ') THEN
            start = start+1
         ELSE
            goto 120
         ENDIF
      enddo
 120  len2 = LENACT(char)
      dummy(:len2) = char(:len2)
      point = len2+1
      do i=start,len1
         IF(string(i:i).eq.' ')THEN
            IF(string(i+1:i+1).ne.' ') THEN
               dummy(point:point+len2) = ' '//char(:len2)
               point = point + len2 + 1
            ENDIF
         ELSE
            dummy(point:point) = string(i:i)
            point = point + 1
         ENDIF
      enddo
      string = dummy

      return
      end
c
c
c ------------------------------------------------------
      SUBROUTINE XSL_RDSTRING(namlis,nfiles,filenm,
     &      nadded,datdir,remove,QUIET,STRIP)
c ------------------------------------------------------
c  This routine reads takes a list of files as a single string, and
c  parses it into a list.  It also checks for the existence of the
c  files, and eliminates them from the list if they do not exist.
c  Jim Ingham Feb/93
c  Mod: Jeff Guerber, HSTX, Aug. 1997. Replaced XSL_EXTSTRIP with FCPARS.
c
      include 'xsel.inc'

      integer MAXADD
      parameter(MAXADD = 200)
      character(255) filenm(MAXFIL)
      character*(*) datdir
      character(255) namlis,temp,tempv(MAXADD)
      character(255) messag
      integer nfiles,len1,len2,len3,LENACT,status,ierr,i
      integer remove,nadded
      logical QUIET, STRIP

      nadded = 0

      call XSL_FILES_PARSE(namlis,datdir,tempv,len1,MAXFIL,status)
      if(status.ne.0) then
         status = 0
         return
      endif
      if((nfiles + len1).gt.MAXFIL) then
         call XWRITE('Too many input files.',5)
         status = 0
         return
      endif
      nadded = 0
      do i=1,len1
c Now prepend the directory string
         if(datdir.ne.'NONE') then
            call XSL_DATDIR(tempv(i),datdir,remove)
         endif
c Now check to see that the file exists:
         call XSL_FITS_EXIST(tempv(i),ierr)
         if(ierr.eq.0) then
c Remove any extensions or filters if the STRIP flag is set
            if (STRIP) then
               call FTRTNM(tempv(i),temp,status)
            else
               temp = tempv(i)
            endif
            nadded = nadded + 1
            filenm(nadded + nfiles) = temp
         else if(.NOT. QUIET) THEN
            len2 = LENACT(tempv(i))

            messag =
     &           'The file '//tempv(i)(1:len2)//' was not added:'
            len3 = LENACT( messag )
            call XWRITE(messag(1:len3),5)

            messag ='    -- it does not exist.'
            len3 = LENACT( messag )
            call XWRITE(messag(1:len3),5)

         endif
      enddo
      nfiles = nfiles + nadded

      return
      end
c
c
c ------------------------------------------------------
      SUBROUTINE XSL_RDVECT(namvec,ninvec,nfiles,filenm,index,
     &                            nadded,datdir,remove,QUIET)
c ------------------------------------------------------
c  This routine reads takes a list of files as a vector, and
c  adds it to the filenm list.  It also checks for the existence of the
c  files, and eliminates them from the list if they do not exist.
c  Jim Ingham Feb/93
c  Mod: Jeff Guerber HSTX Aug. 1997. Replaced XSL_EXTSTRIP with FCPARS.
c  Mod: Jeff Guerber RSTX June 1998. Add status=0 before call fcpars
c
      INCLUDE 'xsel.inc'

      integer ninvec,nadded,point,index(MAXFIL)
      character*(*) filenm(MAXFIL),namvec(MAXFIL)
      character*(*) datdir
      character(255) temp,messag
      integer nfiles,len2,len3,LENACT,status,ierr,i,remove
      logical QUIET

      nadded = 0
      if(nfiles.eq.0) then
         point = 0
         do i=1,ninvec
C  Now prepend the directory string
            if(datdir.ne.'NONE') then
               call XSL_DATDIR(namvec(i),datdir,remove)
            endif
            status = 0
            call XSL_FITS_EXIST(namvec(i),ierr)
            if(ierr.eq.0) then
               call FTRTNM(namvec(i),temp,status)
               point = point + 1
               filenm(point) = temp
               index(point) = i
            else if(.NOT. QUIET) then
               len2 = LENACT(namvec(i))

               messag =
     &          'The file '//namvec(i)(1:len2)//' was not added:'
               len3 = LENACT( messag )
               call XWRITE(messag(1:len3),5)

               messag ='    -- it does not exist.'
               len3 = LENACT( messag )
               call XWRITE(messag(1:len3),5)
            endif
         enddo
         nadded = point
         nfiles = nadded
      else
          if((nfiles + ninvec).gt.MAXFIL) then
            call XWRITE('Too many input files.',5)
            return
         endif
         nadded = 0
         do i=1,ninvec
C  Now prepend the directory string
            if(datdir.ne.'NONE') then
               call XSL_DATDIR(namvec(i),datdir,remove)
            endif
            status = 0
            call XSL_FITS_EXIST(namvec(i),ierr)
            if(ierr.eq.0) then
               call FTRTNM(namvec(i),temp,status)
               nadded = nadded + 1
               filenm(nadded + nfiles) = temp
               index(nadded + nfiles) = i
             else  if(.NOT.QUIET) THEN
               len2 = LENACT(namvec(i))

               messag =
     &            'The file '//namvec(i)(1:len2)//' was not added:'
               len3 = LENACT( messag )
               call XWRITE(messag(1:len3),5)

               messag ='    -- it does not exist.'
               len3 = LENACT( messag )
               call XWRITE(messag(1:len3),5)
            endif
         enddo
         nfiles = nfiles + nadded
      endif

      return
      end
c ----------------------------------------------------------
      subroutine XSL_REG_REBIN(xbinfold)
c ----------------------------------------------------------

      INTEGER xbinfold

c  Modifies the region filters for a change in the xybinsize.
c  xbinfold is the old value of the xybinsize - the current one is
c  xbinf, which is stored in xselvar.inc.
c  kaa  9/27/99

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

      REAL scalef
      INTEGER ilun, lent
      CHARACTER(255) tmpfil, comlin, contxt

      INTEGER lenact
      EXTERNAL lenact

      DATA tmpfil /'xsel_regtemp.xsl'/

      lent = LENACT(tmpfil)

c Calculate the scale factor to be passed to fregcon. Note that because
c of the way this factor is defined in fregcon doubling xybinsize means
c halving the scale factor.

      scalef = FLOAT(xbinfold) / FLOAT(xbinf)

      CALL XWRITE(
     &   'Modifying the region filters for the new xybinsize...',5)

c Set up the command file

      CALL XSL_OPCF(cmdfil, ilun)

      comlin = '/bin/rm -f '//tmpfil(:lent)
      CALL XSL_WRTCF(ilun, comlin, 1)

      WRITE(comlin,'(a,f11.5,a,a,a,a)') 'fregcon factor=', scalef,
     & ' regfile=', regfil(:LENACT(regfil)), ' outfile=',
     &   tmpfil(:lent)
      CALL XSL_WRTCF(ilun, comlin, 1)

      comlin = '/bin/mv '//tmpfil(:lent)//' '//regfil(:LENACT(regfil))
      CALL XSL_WRTCF(ilun, comlin, 1)

      call XSL_CLCF(ilun)
      call XSL_RUNCF(cmdfil,ECHO,status)

      IF ( status .NE. 0 ) THEN
         CALL XWRITE('Failure while running fregcon', 5)
         WRITE(contxt, '(a,i5)') 'Status = ', status
         CALL XWRITE(contxt, 5)
      ENDIF

      RETURN
      END
c
c ----------------------------------------------------------
      subroutine XSL_RESTORE(SAVED,old)
c ----------------------------------------------------------
c  Reads in the values from the xsel_session.sav file, and puts them
c  in the correct places
c  Jim Ingham   4/7/93
      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

      integer i,old,ilun
      character(255) str2
      logical SAVED,USE

      old = 0

      call GETLUN(ilun)
      call XSL_OPEN(ilun,savfil,'OLD',' ',' ',0,0,ierr)
      IF(ierr.ne.0) THEN
         goto 999
      ENDIF

      call xsl_uclgsb('use_old',USE,status)
      IF ( status .NE. 0 ) RETURN
      IF(.not. USE) then
         SAVED = .FALSE.
         close(ilun)
         call FRELUN(ilun)
         call XSL_CLEAR(1)
         call XSL_RMFILE(savfil)
         return
      ENDIF

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,26,END=97,ERR = 98 ,IOSTAT = ierr)
     &                     READ,SPEC,CURV,IMAGE,SELCT,MERGED
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun, 26,END=97,ERR = 98 ,IOSTAT = ierr)
     &            USEGTI,MERGTI,USEHK,HKREAD,MANYHK,MERGHK
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,26,END=97,ERR = 98 ,IOSTAT = ierr)
     &                EXPAND,MANY,MADE,WORK,WRKGTI,WORKHK
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,26,END=97,ERR = 98 ,IOSTAT = ierr)
     &                    LOADED,FILTER,HKSEL,CLEAN,CLEANI
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,26,END=97,ERR = 98 ,IOSTAT = ierr)
     &                   FITTFL,ASCTFL,XWNTFL,FFTFL,REGION
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,31,END=97,ERR = 98 ,IOSTAT = ierr) npar
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      do i=1,npar
         read(ilun,27,END=97,ERR = 98 ,IOSTAT = ierr) HKCURV,parlis(i)
      enddo

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,31,END=97,ERR = 98 ,IOSTAT = ierr) nffpar
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      do i=1,nffpar
         read(ilun,27,END=97,ERR = 98 ,IOSTAT = ierr) FFCURV,ffplis(i)
      enddo
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) datdir
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) hkdir
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) plotdv

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) instru

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) prefix

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) catnam

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) catflt

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) choflt

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) catsel

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) ffilin

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) ffstr(1)

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) hkstr

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=97,ERR = 98 ,IOSTAT = ierr) phalcut,phahcut

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=97,ERR = 98 ,IOSTAT = ierr) phabin

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=97,ERR = 98 ,IOSTAT = ierr) xbinf

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,41,END=97,ERR = 98 ,IOSTAT = ierr) binsiz

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=97,ERR = 98 ,IOSTAT = ierr)
     &                              numreg,numasc,numgti

      IF(numreg.gt.0) THEN
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
         do i=1,numreg
            read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) regvec(i)
         enddo
         CALL XSL_CAT(regvec,numreg,regfil,MAXFIL)
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      ENDIF

      IF(numasc.gt.0) THEN
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
         do i=1,numasc
            read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) ascvec(i)
         enddo
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      ENDIF

      IF(numgti.gt.0) THEN
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
         do i=1,numgti
            read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) gtivec(i)
         enddo
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      ENDIF


      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=97,ERR = 98 ,IOSTAT = ierr) obsno,nfiles,nhkfil

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      do i=1,nfiles
         read(ilun,31,END=97,ERR = 98 ,IOSTAT = ierr) catidx(i)
      enddo
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      do i=1,nfiles
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) filenm(i)
      enddo
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      do i=1,nfiles
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) gtifnm(i)
      enddo
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      do i=1,nhkfil
         read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) hkflnm(i)
      enddo
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr)


C From version .94 on I have started to add stuff at the end.  I will
C write a converter at some point, at which time I will put this stuff
C in a more logical place.

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=99,ERR = 98 ,IOSTAT = ierr) extrbinh
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,33,END=99,ERR = 98 ,IOSTAT = ierr) xcolf,ycolf,
     &                                                 xfkey,yfkey
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,33,END=99,ERR = 98 ,IOSTAT = ierr) xcolh,ycolh,
     &                                                 xhkey,yhkey
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) keymis
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) keypha
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,26,END=99,ERR = 98 ,IOSTAT = ierr) UNBIN,BINOUT,BININ

      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=97,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,31,END=97,ERR = 98 ,IOSTAT = ierr) numcti

c This is an addition to the save file for Xselect 098e, it is
c to save phase information that  would not have been entered in
c earlier versions, so I won't announce its absence.

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,34,END=101,ERR = 98 ,IOSTAT = ierr)
     &     XPHTFL,nphase,epoch,period
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      do i=1,nphase
         read(ilun,35,END=197,ERR = 98 ,IOSTAT = ierr)
     &        phase(1,i),phase(2,i)
      enddo
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(5(L1,4x))',END=101,ERR = 98 ,IOSTAT = ierr)
     &     WTMAPB,FAST,WTMAPFIX,SWMAPX,SWMAPY
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(L1,4x,L1)',END=101,ERR = 98 ,IOSTAT = ierr)
     &     SMOOTH,SAVSMOOTH

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(a10,2x,a10,2x,i4)',END=101,ERR = 98 ,
     &        IOSTAT = ierr) smethod,bound,const
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(i4,2x,i4)',END=101,ERR = 98 ,
     &        IOSTAT = ierr) sigma,nsigma
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(f7.2,2x,f7.2)',END=101,ERR = 98 ,
     &        IOSTAT = ierr) xwindow,ywindow

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=101,ERR = 98 ,IOSTAT = ierr) nstrsel,nffstr

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2(1:6)
      do i=1,nstrsel
          read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr)
     &        strsel(i)
      enddo

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2(1:6)
      do i=1,nffstr
          read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr)
     &        ffstr(i)
      enddo
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2(1:8)
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) datamode

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2(1:6)
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) mkfdir

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2(1:8)
      read(ilun,'(e17.10,4x,i7,4x,i7)',END=101,ERR = 98 ,
     &        IOSTAT = ierr) timedel,phamin,phamax

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2(1:8)
      read(ilun,'(a)',END=101,ERR = 98 ,IOSTAT = ierr) usrdfe
      read(ilun,'(l1)',END=101,ERR = 98 ,IOSTAT = ierr) FAINT

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2(1:8)
      read(ilun,'(i6,2x,l1)',END=101,ERR = 98 ,IOSTAT = ierr)
     &        numint,INTENS

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2(1:8)
      do i=1,numint
         read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) intvec(i)
         read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) intexp(i)
      enddo

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) submis

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=101,ERR = 98 ,IOSTAT = ierr)
     &        clnmet,  cphal,  cphah
      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(I4,2x,f10.5,2x,i4)',END=101,ERR = 98 ,
     &        IOSTAT = ierr) cellsz,logprb,bkgthr

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(5(i4,2x),a4)',END=101,ERR = 98 ,IOSTAT = ierr)
     &        ccdno,   stah,  endh,  arena,  ario,  in_or_out

      read(ilun,15,END=101,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(L1)',END=101,ERR = 98 ,IOSTAT = ierr) DIRTY

      read(ilun,15,END=102,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,31,END=102,ERR = 98 ,IOSTAT = ierr) numhnd

      read(ilun,15,END=102,ERR = 98 ,IOSTAT = ierr) str2
      do i = 1, MXNSEL
         read(ilun,32,END=102,ERR = 98 ,IOSTAT = ierr) ctindx(i),
     &           hndndx(i),intndx(i)
      enddo
      read(ilun,15,END=102,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=102,ERR = 98 ,IOSTAT = ierr) mkfdnm

      read(ilun,15,END=102,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,'(6(I6,2x))',END=102,ERR = 98 ,IOSTAT = ierr)
     &        xcf,ycf,sizef,xch,ych,sizeh

      read(ilun,15,END=102,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,32,END=102,ERR = 98 ,IOSTAT = ierr)
     &        rxbval,ribval, phbval

c read the keyword for the size of the PHA array - this was added later so
c have to allow for old saved sessions not having this last line.

      read(ilun,15,iostat=ierr) phamxkwd
      IF ( ierr .NE. 0 ) THEN
         phamxkwd = 'TLMAX'
         ierr = 0
      ENDIF

c read the keyword for the GTI name - this was added later so
c have to allow for old saved sessions not having this last line.

      read(ilun,15,iostat=ierr) gtinam
      IF ( ierr .NE. 0 ) THEN
         gtinam = 'GTI'
         ierr = 0
      ENDIF

c read the keyword for the CCD ID keyword name - this was added later so
c have to allow for old saved sessions not having these last lines.

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) keyccd
      IF ( ierr .NE. 0 ) THEN
         keyccd = 'NONE'
         ierr = 0
      ENDIF

c read the keyword for the GRADE/PATTERN keyword name - this was added later so
c have to allow for old saved sessions not having these last lines.

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) keygrd
      IF ( ierr .NE. 0 ) THEN
         keygrd = 'NONE'
         ierr = 0
      ENDIF

c read the grade filter string - this was added later so
c have to allow for old saved sessions not having these last lines.

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) gfilter
      IF ( ierr .NE. 0 ) THEN
         gfilter = 'NONE'
         ierr = 0
      ENDIF

c read the TIMESYS and MJDREFI/F information

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) timesys
      IF ( ierr .NE. 0 ) THEN
         timesys = 'TT'
         ierr = 0
      ENDIF

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      IF ( ierr .EQ. 0 ) THEN
         READ(str2, *) mjdrefi, mjdreff
      ELSE
         mjdrefi = 0
         mjdreff = 0.0d0
         ierr = 0
      ENDIF

c read the column filter string

      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) str2
      read(ilun,15,END=99,ERR = 98 ,IOSTAT = ierr) colfilter
      IF ( ierr .NE. 0 ) THEN
         colfilter = ' '
         ierr = 0
      ENDIF

      goto 101

C Come here if we error out of getting CTINDX, happens between 1.0 and 1.1
C We fix this by just looking for the files...
C Otherwise jump to 101

 102  if ( numcti.gt.0) then
         numhnd = 0
         numcti = 1
         do i=1,MXNSEL
            call XSL_EXIST(ctivec(i),status)
            if(status.eq.0) then
               catidx(i) = numcti
               numcti = numcti + 1
            endif
         enddo
      endif

 101  SAVED = .TRUE.
c We go to 100 to close everything out

      goto 100

c We go to 197 if the phase information was corrupted

 197  call XWRITE('Too few phases in Phase information',5)
      call XWRITE('Redo FILTER PHASE to continue',5)
      SAVED = .TRUE.
      goto 100

c We go to 97 if the saved file truncated

 97   call XWRITE('Save file was truncated, could not restore.',5)
      SAVED = .FALSE.
      goto 100

c We go to 99 if the save file is v.95 or before.

 99   old = 1
      SAVED = .TRUE.
      goto 100

C Come here on error reading the new stuff.  Have to think of what
C I want to do with these errors.

 98   call XWRITE
     &     ('Error reading in save file, could not restore.',5)
      call FRELUN(ilun)
      SAVED = .FALSE.
c Finally this closes everything:
 100  close(ilun)
      call FRELUN(ilun)
      call XSL_RMFILE(savfil)

      return
c Come here for no saved file:
 999  call FRELUN(ilun)
      SAVED = .FALSE.
      return



 15   format(a)
 26   format(3x,L1,4x,5(2x,3x,L1,4x),/)
 27   format(3x,L1,4x,2x,a)
 31   format(I4)
 32   format(3(i6,2x),/)
 33   format(4(a10,2x),/)
 34   format(1x,L1,1x,I3,7x,f23.15,3x,f23.15)
 35   format(f8.5,2x,f8.5)
 41   format(e12.5)

      end

c ----------------------------------------
      subroutine XSL_SET_DIR(parnam,datdir,status)
c ----------------------------------------
c This checks if the datdir is set on the command line with parameter
c named parnam, and if so checks that the dir exists...  If not, and datdir
c is NONE, then it queries for it.

c J Ingham 4/94

      character*(*) parnam,datdir
      integer status

      status = 0
      call XSL_UCLGOT(parnam,status)

      if(datdir.eq.'NONE'.or.status.eq.0) then
         status = 0
         call XSL_GET_DIR(parnam,'NONE',datdir,status)
      else
         status = 0
         return
      endif

      return
      end

c
c ---------------------------------------------
      subroutine XSL_SHOWCATS(mode,chonam,chosen,nchose,MXCHOS,
     &     ONLYDD,BRIEF,ALL)
c ---------------------------------------------
c This routine makes a listing of the catalogues in the datadir,
c if that has been entered, and in the working directory.  Then the
c user can pick from the listing.  The choice(s) are put in chosen:
c
c mode = 0 just lists the catalogue
c mode = 1 prompts for choices.
c
c ==============================
c chonam(i) = catalog name
c chosen(i,1) = catalog telescop
c chosen(i,2) = catalog instrument
c chosen(i,3) = catalog data directory
c chosen(i,4) = catalog hk directory
c ==============================
c J. Ingham 11/93

      include 'xsel.inc'
      include 'xselvar.inc'

      integer MXNCAT, MXNKWD, MAXREC
      parameter (MXNCAT = 100, MXNKWD = 5, MAXREC = 8)
      integer MXCHOS,nchose
      character*(*) chonam(MXCHOS),chosen(MXCHOS,4)
      character(32) cobjec(MXNCAT)
      character(255) catvec(MXNCAT)
      character(80) kwdnam(MXNKWD),kwdval(MXNKWD)
      character(80) spaces,dashes
      character(255) selexp(MXNCAT,MAXREC),seltmp(MAXREC)
      integer nexpre(MXNCAT),indx(MXNCAT),nindx,niter,rem,k
      character(512) str1
      character(255) cdatd(MXNCAT),chkdir(MXNCAT)
      character(16) cinstr(MXNCAT),cteles(MXNCAT)
      integer nindat,ncats,i,j,l,len1,point,LENACT
      integer kwdst(MXNKWD),maxnam,maxobj,maxtel,maxins,mode
      logical BRIEF,ALL,ONLYDD
      integer NCHOO,nprdat,nprwrk
      parameter(NCHOO = 100)
      real buffer1(2,NCHOO)
      real rmin, rmax
      integer ibuf(2,NCHOO),nr,start
      logical FIXDIR

      data (kwdnam(i),i=1,MXNKWD)  /'DATADIR','HKDIR','TELESCOP',
     &    'INSTRUME', 'OBJECT' /

c THis sets up the range parameter stuff
      nr=20
      rmin = -100.
      rmax = 100000.

      FIXDIR = .FALSE.

      nindx = 0
      nprdat = 0
      nprwrk = 0
      do i=1,80
         spaces(i:i) = ' '
         dashes(i:i) ='-'
      enddo
      nindat = 0
      ncats = 0
      status = 0

      call XWRITE(' ',5)
      call XWRITE('Finding catalogue files:',5)
      call XWRITE(' ',5)
      IF(datdir.ne.'NONE') THEN
         call XWRITE('Looking in the data directory:',5)
         str1 = '    '//datdir(:LENACT(datdir))
         call XWRITE(str1,5)

c First get a listing of all the files of the type .cat:
         call XSL_LIST(lstfil,'*.cat',datdir,wrkdir,status)
c Now read in the list file:
         call XSL_GET_LIST(catvec,nindat,MXNCAT,lstfil,status)
         IF(status.eq.-10) THEN
            call XWRITE('List file not found???',5)
            return
         ELSE IF(status.eq.-20) THEN
            call XWRITE('Too many catalogues in data directory',5)
            write(str1,85) MXNCAT
 85         format('Stopping at ',i3)
            call XWRITE(str1,5)
            status = 0
         ENDIF
         IF(nindat.ne.0) then
c Now get the instrument, telescope, and object out of the files.
            do i=1,nindat
               call XSL_GETKWST(catvec(i),'1',datdir,kwdnam,kwdval,
     &             kwdst, MXNKWD,MXNKWD,.FALSE.,status)
               IF(status.ne.0) THEN
                  str1 = 'Error getting keywords from '//catvec(i)
                  call XWRITE(str1,5)
               ENDIF
               cteles(i) = kwdval(3)(:min(lenact(kwdval(3)),
     &                                    len(cteles(i))))
               cinstr(i) = kwdval(4)(:min(lenact(kwdval(4)),
     &                                    len(cinstr(i))))
               cobjec(i) = kwdval(5)(:min(lenact(kwdval(5)),
     &                                    len(cobjec(i))))
               cdatd(i) = kwdval(1)(:min(lenact(kwdval(1)),
     &                                    len(cdatd(i))))
               IF(cdatd(i).ne.'NOT_FOUND') THEN
                  call XSL_CHKDIR(cdatd(i),status)
                  IF(status.eq.0) THEN
                     call XSL_EXPAND_DIR(cdatd(i),status)
                     IF(status.ne.0) THEN
                        status = 0
                     ENDIF
                     if(cdatd(i) .ne. datdir) THEN
                        FIXDIR = .TRUE.
                     endif
                  ELSE
                     FIXDIR = .TRUE.
                     status = 0
                  ENDIF
               ELSE
                  FIXDIR = .TRUE.
               ENDIF
               chkdir(i) = kwdval(2)
c Now see if this is a file we will print:
c N.B. we do not filter on datadir for the ones in the data directory,
c since they quite possibly don't have that keyword in them.
               IF(ALL.or.((keymis.eq.'NONE'.or.keymis.eq.cteles(i)).and.
     &              (instru.eq.'NONE'.or.instru.eq.cinstr(i))))THEN
c This is the number of obscats we will print in the datdir
                  nprdat = nprdat + 1
                  nindx = nindx + 1
                  indx(nindx) = i
c Now get the selection expressions:
                  call XSL_GET_SEL(catvec(i),datdir,seltmp,nexpre(i),
     &                 MAXREC,status)
                  IF(status.ne.0) THEN
                     call XWRITE
     &                    ('Error getting selection expressions',5)
                     nexpre(i) = 0
                     status = 0
                  ELSE
                     do j=1,nexpre(i)
                        selexp(i,j) = seltmp(j)
                     enddo
                  ENDIF
               ENDIF
            enddo
            if (FIXDIR.and.datdir.ne.'NONE'.and.datdir.ne.wrkdir) then
               status = 0
               IF (STATUS.eq.0) THEN
                  call XWRITE('The catalogues in the data '//
     &                 'directory do not have',5)
                  call XWRITE
     &                 ('   the correct directory path in them.',5)
                  call XWRITE('Shall I fix this for you?',5)
                  call XWRITE('N.B. You must have write access '//
     &                 'to the datadir to do this.',5)
                  call xsl_uclgsb('fix_datadir',FIXDIR,status)
                  IF(status.ne.0) THEN
                     call XWRITE
     &                    ('Error getting fix_datadir parameter.',5)
                  ELSE IF(FIXDIR) THEN
                     kwdval(1) = datdir(:min(lenact(datdir),
     &                                       len(kwdval(1))))
                     kwdval(2) = hkdir(:min(lenact(hkdir),
     &                                       len(kwdval(2))))
                     do i=1,nindat
                        call XSL_PUTKWST(catvec(i),'1',datdir,kwdnam,
     &                       kwdval,2,MXNKWD,status)
                        if ( status.ne.0) then
                           str1 = 'Error modifying catalogue '
     &                          //catvec(i)
                           call XWRITE(str1,5)
                           goto 789
                        endif
                        cdatd(i) = datdir
                        chkdir(i) = hkdir
                     enddo
 789                 continue
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
c Now look in the work directory, unless ONLYDD is set:
      IF(ONLYDD) THEN
         ncats = nindat
      ELSE
         if(wrkdir.eq.catdir) THEN
            call XWRITE('Looking in the working directory: ',5)
         else
            call XWRITE('Looking in the obscat directory: ',5)
         endif
         str1 = '    '//catdir(:LENACT(catdir))
         call XWRITE(str1,5)
         call XWRITE(' ',5)
c First get a listing of all the files of the type .cat:
         call XSL_LIST(lstfil,'*.cat',catdir,wrkdir,status)
c Now read in the list file:
         ncats = nindat
         call XSL_GET_LIST(catvec,ncats,MXNCAT,lstfil,status)
         IF(status.eq.-10) THEN
            call XWRITE('List file not found???',5)
            return
         ELSE IF(status.eq.-20) THEN
            if(wrkdir.eq.catdir) THEN
               call XWRITE
     &              ('Too many catalogues in work directory',5)
            else
               call XWRITE
     &              ('Too many catalogues in obscat directory',5)
            endif
            write(str1,85) MXNCAT - nindat
            call XWRITE(str1,5)
            status = 0
         ENDIF
         IF(ncats.eq.nindat) then
            if(wrkdir.eq.catdir) THEN
               call XWRITE
     &              ('No *.cat files found in work directory ',5)
            else
               call XWRITE
     &              ('No *.cat files found in obscat directory ',5)
            endif
         ELSE
c Now get the instrument, telescope, and object out of the files.
            do i=nindat + 1,ncats
               call XSL_GETKWST(catvec(i),' ',catdir,kwdnam,kwdval,
     &             kwdst, MXNKWD,MXNKWD,.FALSE.,status)
               IF(status.ne.0) THEN
                  str1 = 'Error getting keywords from '//catvec(i)
                  call XWRITE(str1,5)
                  status = 0
               ENDIF
               cteles(i) = kwdval(3)(:min(lenact(kwdval(3)),
     &                                    len(cteles(i))))
               cinstr(i) = kwdval(4)(:min(lenact(kwdval(4)),
     &                                    len(cinstr(i))))
               cobjec(i) = kwdval(5)(:min(lenact(kwdval(5)),
     &                                    len(cobjec(i))))
               cdatd(i) = kwdval(1)(:min(lenact(kwdval(1)),
     &                                    len(cdatd(i))))
c Expand the datdir filename:
               IF(cdatd(i).ne.'NOT_FOUND') THEN
                  call XSL_CHKDIR(cdatd(i),status)
                  IF(status.eq.0) THEN
                     call XSL_EXPAND_DIR(cdatd(i),status)
                     if(status.ne.0) then
                        status = 0
                     endif
                  ELSE
                     status = 0
                  ENDIF
               ENDIF
               chkdir(i) = kwdval(2)
c Now see if this is a file we will print:
               IF(ALL.or.(keymis.eq.'NONE'.or.keymis.eq.cteles(i)).and.
     &              (instru.eq.'NONE'.or.instru.eq.cinstr(i)).and.
     &              (datdir.eq.'NONE'.or.datdir.eq.cdatd(i)))THEN
                  nprwrk = nprwrk + 1
                  nindx = nindx + 1
                  indx(nindx) = i
                  call XSL_GET_SEL(catvec(i),catdir,seltmp,nexpre(i),
     &                 MAXREC,status)
                  IF(status.ne.0) THEN
                     call XWRITE
     &                    ('Error getting selection expressions',5)
                     nexpre(i) = 0
                     status = 0
                  ELSE
                     do j=1,nexpre(i)
                        selexp(i,j) = seltmp(j)
                     enddo
                  ENDIF
               ENDIF
            enddo
         ENDIF
      ENDIF
c Now print out the list of catalogues:
c First in the data directory
      IF(nprdat.gt.0) THEN
         maxnam = 8
         maxobj = 6
         maxtel = 9
         maxins = 10
         j = 1
         i = indx(j)
         do while(i.le.nindat.and.j.le.nindx)
            maxnam = max(maxnam,LENACT(catvec(i)))
            maxobj = max(maxobj,LENACT(cobjec(i)))
            maxtel = max(maxtel,LENACT(cteles(i)))
            maxins = max(maxins,LENACT(cinstr(i)))
            j = j+1
            i = indx(j)
         enddo
         call XWRITE('In the CURRENT DATA DIRECTORY:',5)
         call XWRITE('------------------------------',5)
         call XWRITE(' ',5)
         str1 = 'INDEX'//'   '//
     &        'FILENAME'//spaces(1:3+maxnam-8)//
     &        'OBJECT'//spaces(1:3+maxobj-6)//
     &        'TELESCOPE'//spaces(1:3+maxtel-9)//
     &        'INSTRUMENT'
         call XWRITE(str1,5)
         str1 = dashes(1:5)//'   '//
     &        dashes(:maxnam)//'   '//dashes(:maxobj)//'   '//
     &        dashes(:maxtel)//'   '//dashes(:maxins)
         call XWRITE(str1,5)
         l = 1
         i = indx(l)
         do while(i.le.nindat.and.l.le.nindx)
            write(str1,95) l,catvec(i)(:maxnam),cobjec(i)(:maxobj),
     &           cteles(i)(:maxtel),cinstr(i)(:maxins)
            call XWRITE(str1,5)
            IF(.NOT. BRIEF) THEN
               str1 = '        DATA DIRECTORY: '//cdatd(i)
               call XWRITE(str1,5)
               IF(nexpre(i).gt.0) THEN
                  str1 = '        LIST STRING: '//selexp(i,1)
                  call XWRITE(str1,5)
               ENDIF
            ENDIF
            IF(nexpre(i).gt.1) THEN
c If brief, only write out the last selection:
               IF(BRIEF) THEN
                  call XWRITE('        LAST SELECTION:',5)
                  start = nexpre(i)
               ELSE
                  call XWRITE('        SELECTIONS:',5)
                  start = 2
               ENDIF
               do j=start,nexpre(i)
                  len1 = LENACT(selexp(i,j))
                  niter = len1/68
                  rem = len1 - niter*68
                  point = 1
                  do k = 1,niter
                     IF(k.eq.1) THEN
                        str1 = '        * '
     &                       //selexp(i,j)(point:point+66)
                        call XWRITE(str1,5)
                     ELSE
                        str1 = '          '
     &                       //selexp(i,j)(point:point+66)
                        call XWRITE(str1,5)
                     ENDIF
                     point = point+67
                  enddo
                  if(rem.gt.0) then
                     IF(niter.eq.0) THEN
                        str1 = '        * '
     &                       //selexp(i,j)(point:point+rem)
                        call XWRITE(str1,5)
                     ELSE
                        str1 = '          '
     &                       //selexp(i,j)(point:point+rem)
                        call XWRITE(str1,5)
                     ENDIF
                  endif
               enddo
            ENDIF
            call XWRITE(' ',5)
            l = l+1
            i = indx(l)
         enddo
      ENDIF
c Next in the current working directory:
      IF(nprwrk.gt.0) THEN
         maxnam = 8
         maxobj = 6
         maxtel = 9
         maxins = 10
         j = nindat+1
         i = indx(j)
         do while(j.le.nindx)
            maxnam = max(maxnam,LENACT(catvec(i)))
            maxobj = max(maxobj,LENACT(cobjec(i)))
            maxtel = max(maxtel,LENACT(cteles(i)))
            maxins = max(maxins,LENACT(cinstr(i)))
            j = j+1
            i = indx(j)
         enddo
         if(wrkdir.eq.catdir) THEN
            call XWRITE('In WORK DIRECTORY:',5)
            call XWRITE('------------------',5)
         ELSE
            call XWRITE('In OBSCAT DIRECTORY:',5)
            call XWRITE('--------------------',5)
         ENDIF
         call XWRITE(' ',5)
         str1 = 'INDEX'//'   '//
     &        'FILENAME'//spaces(1:3+maxnam-8)//
     &        'OBJECT'//spaces(1:3+maxobj-6)//
     &        'TELESCOPE'//spaces(1:3+maxtel-9)//
     &        'INSTRUMENT'
         call XWRITE(str1,5)
         str1 = dashes(1:5)//'   '//
     &        dashes(:maxnam)//'   '//dashes(:maxobj)//'   '//
     &        dashes(:maxtel)//'   '//dashes(:maxins)
         call XWRITE(str1,5)
         l = nprdat + 1
         i = indx(l)
         do while(l.le.nindx)
            write(str1,95) l,catvec(i)(:maxnam),cobjec(i)(:maxobj),
     &           cteles(i)(:maxtel),cinstr(i)(:maxins)
 95         format(1x,i3,4x,a,3x,a,3x,a,3x,a)
            call XWRITE(str1,5)
            IF(.NOT.BRIEF) THEN
               str1 = '        DATA DIRECTORY: '
     &              //cdatd(i)
               call XWRITE(str1,5)
               IF(nexpre(i).gt.0) THEN
                  str1 ='        LIST STRING: '
     &                 //selexp(i,1)
                  call XWRITE(str1,5)
               ENDIF
            ENDIF
            IF(nexpre(i).gt.1) THEN
c If brief, only write out the last selection:
               IF(BRIEF) THEN
                  call XWRITE('        LAST SELECTION:',5)
                  start = nexpre(i)
               ELSE
                  call XWRITE('        SELECTIONS:',5)
                  start = 2
               ENDIF
               do j=start,nexpre(i)
                  len1 = LENACT(selexp(i,j))
                  niter = len1/68
                  rem = len1 - niter*68
                  point = 1
                  do k = 1,niter
                     IF(k.eq.1) THEN
                        str1 = '        * '
     &                       //selexp(i,j)(point:point+66)
                        call XWRITE(str1,5)
                     ELSE
                        str1 = '          '
     &                       //selexp(i,j)(point:point+66)
                        call XWRITE(str1,5)
                     ENDIF
                     point = point+67
                  enddo
                  if(rem.gt.0) then
                     IF(niter.eq.0) THEN
                        str1 = '        * '
     &                       //selexp(i,j)(point:point+rem)
                        call XWRITE(str1,5)
                     ELSE
                        str1 = '          '
     &                       //selexp(i,j)(point:point+rem)
                        call XWRITE(str1,5)
                     ENDIF
                  endif
               enddo
            ENDIF
            call XWRITE(' ',5)
            l = l+1
            i = indx(l)
         enddo
      ENDIF
c Finally prompt for the catalogue:
      IF(ncats.gt.0.and.mode.eq.1) THEN
         call XWRITE(' ',5)
c CHOOSE RANGES FROM THE INDEX **
         call xsl_uclgsg('obscat_choices',buffer1,
     &      Nchoo,rmin,rmax,nr,status)
         IF(status.ne.0) THEN
            call XWRITE
     &           ('Error getting the obscat_choices parameter',5)
            status = -30
            return
         ENDIF
c Now, get on with processing ranges
c Put contents of BUFFER1 into IBUF

         do i=1,nr
            if( buffer1(2,i).eq.rmax )then
               buffer1(2,i) = nincat
            endif
            if( buffer1(1,i).eq.rmin)then
               buffer1(1,i) = 1
            endif

            ibuf(1,i) = INT( buffer1(1,i) )
            ibuf(2,i) = INT( buffer1(2,i) )
         end do

c Check that the entries are legitimate:
         do i=1,nr
            IF(ibuf(2,i).gt.nindx) THEN
               write(str1,240) nindx
 240           format('Selection out of bounds, only ',I3,
     &              'entries in the catalogue')
               call XWRITE(str1,5)
               status = -30
               return
            ELSEIF(ibuf(2,i).lt.0.or.ibuf(1,i).lt.0) THEN
               call XWRITE('Negative indices are not allowed',5)
               status = -30
               return
            ENDIF
         enddo

c 0 is the flag to cancel the choice.
         IF(ibuf(1,1).eq.0) THEN
            status = -30
            return
         ENDIF
c Now fill the chosen vector:
         nchose = 0

         do i=1,nr
            do j=ibuf(1,i),ibuf(2,i)
               nchose = nchose+1
               IF(nchose.gt.MXCHOS) THEN
                  call XWRITE
     &                 ('Too many files chosen in XSL_SHOWCATS',5)
                  status = -30
                  return
               ENDIF
               k = indx(j)
c Prepend the directory name:
               IF(k.le.nindat) then
                  call XSL_DATDIR(catvec(k),datdir,0)
               ELSE
                  call XSL_DATDIR(catvec(k),catdir,0)
               ENDIF
               chonam(nchose) = catvec(k)
               chosen(nchose,1) = cteles(k)
               chosen(nchose,2) = cinstr(k)
C
C At this point, reset the datadir, and hkdir to datadir, or cwd if they
C are not in the chosen obscat.
C
               IF (cdatd(k).eq.'NOT_FOUND') THEN
                  IF( datdir.ne.'NONE' ) then
                     chosen(nchose,3) = datdir
                  ELSE
                     chosen(nchose,3) = wrkdir
                  ENDIF
               ELSE
                  chosen(nchose,3) = cdatd(k)
               ENDIF
               IF (chkdir(k).eq.'NOT_FOUND') THEN
                  IF( hkdir.ne.'NONE' ) then
                     chosen(nchose,4) = hkdir
                  ELSE
                     chosen(nchose,4) = wrkdir
                  ENDIF
               ELSE
                  chosen(nchose,4) = chkdir(k)
               ENDIF
            enddo
         enddo
      ENDIF

      return
      end


c
c
c -----------------------------------------------------------
      subroutine XSL_GETDUMMYPARBT(parname, dummy, lbuff, status)
c -----------------------------------------------------------
c  This subroutine reads in the dummy parameter from the par file,
c  If it has been given a value this is put into parname, and passed
c  back to Xselect in ibuff. If not, XSL_UCLGSB of parname is called,
c  and passed in buffer
c  J. Ingham 6/7/93
c
      integer status,LENACT
      character*(*) parname, dummy
      character(255) string
      logical lbuff

      call xsl_uclgst(dummy,string,status)
      IF(status.ne.0) THEN
         call XWRITE('Error getting dummy parameter',5)
         return
      ENDIF

      IF(string.eq.'NOT_ENTERED') then
         call xsl_uclgsb(parname,lbuff,status)
      ELSE
         IF(index(string,'n').ne.0.or.index(string,'f').ne.0) THEN
            lbuff = .FALSE.
         ELSEIF(index(string,'y').ne.0.or.index(string,'t').ne.0) THEN
            lbuff = .TRUE.
         ELSE
            string = 'Could not interpret logical value for parameter'
     &           //parname(:LENACT(parname))
            call XWRITE(string,5)
            status = -10
            lbuff = .TRUE.
         ENDIF
         call XSL_UCLPSB(parname,lbuff,status)
      ENDIF

      return
      end
c
c
c -----------------------------------------------------------
      subroutine XSL_GETDUMMYPARIT(parname, dummy, ibuff, status)
c -----------------------------------------------------------
c  This subroutine reads in the dummy parameter from the par file,
c  If it has been given a value this is put into parname, and passed
c  back to Xselect in ibuff. If not, XSL_UCLGST of parname is called,
c  and passed in buffer
c  J. Ingham 6/7/93
c
      integer status
      character*(*) parname, dummy
      character(255) string
      integer ibuff,LENACT

      call xsl_uclgst(dummy,string,status)
      IF(status.ne.0) THEN
         call XWRITE('Error getting dummy parameter',5)
         return
      ENDIF

      IF(string.eq.'NOT_ENTERED') then
         call xsl_uclgsi(parname,ibuff,status)
      ELSE
         read(string,*,err=999,iostat = status) ibuff
 999     if(status.ne.0) then
            string ='Error in parameter: '//parname(:LENACT(parname))//
     &           ' please enter an integer.'
            call XWRITE(string,5)
            call xsl_uclgsi(parname,ibuff,status)
         endif
         call XSL_UCLPSI(parname,ibuff,status)
      ENDIF

      return


      end
c
c
c -----------------------------------------------------------
      subroutine XSL_GETDUMMYPARRT(parname, dummy, ebuff, status)
c -----------------------------------------------------------
c  This subroutine reads in the dummy parameter from the par file,
c  If it has been given a value this is put into parname, and passed
c  back to Xselect in ibuff. If not, XSL_UCLGST of parname is called,
c  and passed in buffer
c  J. Ingham 6/7/93
c
      integer status,LENACT
      character*(*) parname, dummy
      character(255) string
      real ebuff

      call xsl_uclgst(dummy,string,status)
      IF(status.ne.0) THEN
        call XWRITE('Error getting dummy parameter',5)
        return
      ENDIF

      IF(string.eq.'NOT_ENTERED') then
        call xsl_uclgsr(parname,ebuff,status)
      ELSE
        read(string,*,err = 999,iostat=status ) ebuff
 999     if(status.ne.0) then
            string ='Error in parameter: '//parname(:LENACT(parname))//
     &           ' please enter an real number.'
            call XWRITE(string,5)
            call xsl_uclgsr(parname,ebuff,status)
         endif
        call XSL_UCLPSR(parname,ebuff,status)
      ENDIF

      return
      end
c
c
c -----------------------------------------------------------
      subroutine XSL_GETDUMMYPARDT(parname, dummy, dbuff, status)
c -----------------------------------------------------------
c  This subroutine reads in the dummy parameter from the par file,
c  If it has been given a value this is put into parname, and passed
c  back to Xselect in ibuff. If not, XSL_UCLGST of parname is called,
c  and passed in buffer
c  J. Ingham 6/7/93
c
      integer status,LENACT
      character*(*) parname, dummy
      character(255) string
      double precision dbuff

      call xsl_uclgst(dummy,string,status)
      IF(status.ne.0) THEN
        call XWRITE('Error getting dummy parameter',5)
        return
      ENDIF

      IF(string.eq.'NOT_ENTERED') then
        call xsl_uclgsd(parname,dbuff,status)
      ELSE
        read(string,*,err = 999,iostat = status) dbuff
 999     if(status.ne.0) then
            string ='Error in parameter: '//parname(:LENACT(parname))//
     &           ' please enter an double precision number.'
            call XWRITE(string,5)
            call xsl_uclgsd(parname,dbuff,status)
         endif
        call XSL_UCLPSD(parname,dbuff,status)
      ENDIF

      return
      end
c
c
c -----------------------------------------------------------
      subroutine XSL_GETDUMMYPARST(parname, dummy, buffer, status)
c -----------------------------------------------------------
c  This subroutine reads in the dummy parameter from the par file,
c  If it has been given a value this is put into parname, and passed
c  back to Xselect in buffer. If not, XSL_UCLGST of parname is called,
c  and passed in buffer
c  J. Ingham 6/7/93
c
      integer status
      character*(*) parname, dummy, buffer

      call xsl_uclgst(dummy,buffer,status)
      IF(status.ne.0) THEN
        call XWRITE('Error getting dummy parameter',5)
        return
      ENDIF

      IF(buffer.eq.'NOT_ENTERED') then
        call xsl_uclgst(parname,buffer,status)
      ELSE
        call XSL_UCLPST(parname,buffer,status)
      ENDIF

      return
      end



c
c ---------------------------------------------
      SUBROUTINE XSL_PHASEOUT(Xronoutfile,epoch,period,phase,
     &     nphase,MAXPHASE,ierr)
c ---------------------------------------------
c This writes out a XRONOS window file containing phase information
c Taken from extractor 1.0l, and modified to write PHASE not GTI.
c J. Ingham 1/94

      CHARACTER*(*) Xronoutfile
      INTEGER nphase,MAXPHASE
      REAL phase(2,MAXPHASE)
      DOUBLE PRECISION epoch,period

      INTEGER ilun , ierr , i , ise , j , iv , LENACT,itemp
      character(255) errormsg


      CALL GETLUN(ilun)


      ierr = 0
c open window file
      CALL XSL_OPEN(ilun,Xronoutfile,'UNKNOWN',' ',' ',0,0,ierr)
      IF ( ierr.NE.0 ) THEN
         errormsg = 'Failed to open file '//
     &        Xronoutfile(:LENACT(Xronoutfile))
         CALL XWRITE(errormsg,5)
         GOTO 400
      ENDIF
c header line, there are only phase windows in the file:
      itemp = nphase
      WRITE (ilun,99001,IOSTAT=ierr,ERR=400) itemp
c write time windows
      WRITE (ilun,99002,IOSTAT=ierr,ERR=400) 0, 1000
c      IF ( Imaxgti.GT.0 ) THEN
c         DO 50 i = 1 , Imaxgti
c            WRITE (ilun,*,IOSTAT=ierr,ERR=400)
c     &                       '                    ' ,
c     &             Gti(1,i)/86400.D0 , Gti(2,i)/86400.D0 , i
c 50      CONTINUE
c      ENDIF
c write phase windows
      WRITE (ilun,99003,IOSTAT=ierr,ERR=400) nphase , 10
      IF (nphase.GT.0) THEN
         WRITE (ilun, *, IOSTAT=ierr, ERR=400)
     &        '                    ',
     &                                       epoch, period
         DO i = 1, nphase
            WRITE (ilun, *, IOSTAT=ierr, ERR=400)
     &           '                    ',
     &             phase(1,i), phase(2,i), i
         ENDDO
      ENDIF
c write intensity windows
c loop for series
      DO 200 ise = 1 , 4
c loop for original bin, new bin, interval
         DO 100 j = 1 , 3
            iv = 2 + 3*(ise-1) + j
C            WRITE (ilun, 3000, IOSTAT=ierr, ERR=99) nwi(iv), cint(j),
C     &             ise, nmax(iv)
            WRITE (ilun,99004,IOSTAT=ierr,ERR=400) 0 , 0 , 0 , 0
C            IF (nwi(iv).GT.0) THEN
C               DO i = 1, nwi(iv)
C                  WRITE (ilun, *, IOSTAT=ierr, ERR=99)
C     &                    '                    ', fwia(iv-2, i),
C     &                   fwio(iv-2, i), i
C               ENDDO
C            ENDIF
 100     CONTINUE
 200  CONTINUE
c  write exposure windows
c loop for series
      DO 300 ise = 1 , 4
c loop for original bin, new bin, interval
         DO 250 j = 1 , 3
            iv = 11 + 3*(ise-1) + j
C            WRITE (ilun, 4000, IOSTAT=ierr, ERR=99) nwi(iv), cint(j),
C     &             ise, nmax(iv)
            WRITE (ilun,99005,IOSTAT=ierr,ERR=400) 0 , 0 , 0 , 0
C            IF (nwi(iv).GT.0) THEN
C               DO i = 1, nwi(iv)
C                  WRITE (ilun, *, IOSTAT=ierr, ERR=99)
C     &                    '                    ', ewia(iv-11),
C     &                   ewio(iv-11), i
C               ENDDO
C            ENDIF
 250     CONTINUE
 300  CONTINUE
c close window file
      CLOSE (ilun)
C      IF (lcd.GE.2) WRITE (*, 5000) cfilwi
C      IF (lul.NE.0) WRITE (lul, 5000)
C 5000 FORMAT (/, ' ', 'Written window file  ', $)
C      CALL xrwrplust(lut,cfilwi)
      RETURN

 400  errormsg = 'Unable to write output XRONOS window file: ' //
     &           Xronoutfile
      CALL XWRITE(errormsg,5)
      STOP
99001 FORMAT (I4,' Windows in this < Xronos Window File > ')
99002 FORMAT (I4,' Time Wind.: start       stop  (days)',T70,'max ',I5)
99003 FORMAT (I4,' Phase Wind.: epoch  period  (days)',
     &        '/ start stop (0->1) phases',T70,'max ',I4)
99004 FORMAT (I4,' Ints. Wind. for ',A,' in Series',I2,
     &        ' : min  max (c/s)',T70,'max ',I4)
99005 FORMAT (I4,' Exps. Wind. for ',A,' in Series',I2,
     &        ' : min  max (0->50)',T70,'max ',I4)
      END

C ------------------------------------------------------------
C These are some timing routines using SLALIB:

c --------------------------------------------------
      double precision function utoffset(day, fracday)

      integer day
      double precision fracday

      include 'xsel.inc'
      include 'xselvar.inc'

      double precision utc

      double precision sla_dtt
      external sla_dtt

c Check the TIMESYS in use. If isn't TT then assume it is UTC
c so no offset required

      utoffset = 0.0d0

      IF ( timesys(1:2) .NE. 'TT' ) RETURN

      utc = day + fracday
      utoffset = sla_dtt(utc)

      RETURN
      END

c --------------------------------------------------
      subroutine str2sec(instr, UT, MJD, reftim, tlims, dtimes, 
     &                   mxtimes, ntimes)
c --------------------------------------------------

      IMPLICIT NONE

      CHARACTER instr*(*), reftim*(*)
      INTEGER mxtimes, ntimes
      DOUBLE PRECISION dtimes(mxtimes), tlims(2)
      LOGICAL UT, MJD

c converts a time string in either UT, MJD, or SCC into double
c precision SCC values. Times in the string are assumed to be ","
c delimited. A "l" or an "r" as the time string returns the value
c of tlims(1) or tlims(2) appropriately. If reftim is set then it
c is prepended to all the time strings.

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

      DOUBLE PRECISION tread
      INTEGER ipt, iend, reflen
      LOGICAL qdone

      DOUBLE PRECISION secfmjdref
      INTEGER lenact, lchop
      EXTERNAL secfmjdref, lenact, lchop

      status = 0
      reflen = LENACT(reftim)
      ipt = lchop(instr)
      ntimes = 0
      qdone = .FALSE.

      DO WHILE ( .NOT.qdone )

         ntimes = ntimes + 1
         IF ( ntimes .GT. mxtimes ) THEN
            CALL xwrite('STR2SEC: too many times in string', 5)
            RETURN
         ENDIF

c find the next comma delimiter and hence the end of the time string

         iend = index(instr(ipt:), ',')
         IF ( iend .EQ. 0 ) THEN
            iend = LENACT(instr)
            qdone = .TRUE.
         ELSE
            iend = iend + ipt - 2
         ENDIF

c handle the special case of 'l' or 'r' or nothing specified ie ',x' or 'x,'.
c in this latter case assume 'l' or 'r' respectively for the missing number.

         IF ( ipt .GT. iend ) THEN
            IF ( ntimes .EQ. 1 ) tread = tlims(1)
            IF ( ntimes .EQ. 2 ) tread = tlims(2)
         ELSEIF ( index(instr(ipt:iend),'l') .NE. 0 ) THEN
            tread = tlims(1)
         ELSEIF ( index(instr(ipt:iend),'r') .NE. 0 ) THEN
            tread = tlims(2)

c otherwise it is a time specification

         ELSE

c insert the reference time string if requested

            IF ( reflen .GT. 0 ) THEN
               instr = instr(:ipt-1)//reftim(:reflen)//instr(iend+1:)
               iend = iend + reflen
            ENDIF

c translate the current string

            IF ( UT ) THEN
               tread = sec2uni*
     &            secfmjdref(instr(ipt:iend), mjdrefi,mjdreff, status) 
            ELSEIF ( MJD ) then
               call SLA_DFLTIN(instr(:iend), ipt, tread, status) 
               tread =  (tread - mjdrefi - mjdreff) * day2uni
            ELSE
               call SLA_DFLTIN(instr(:iend), ipt, tread, status)
            ENDIF

         ENDIF

         dtimes(ntimes) = tread

         ipt = iend + 2
         IF ( ipt .LT. lenact(instr) ) THEN
            ipt = lchop(instr(ipt:)) + ipt - 1
         ENDIF

      ENDDO

      RETURN
      END

c --------------------------------------------------
      double precision function secfmjdref(instr,refmjdi,
     &                                     refmjdf,status)
c --------------------------------------------------
c
      character*(*) instr
      integer status,refmjdi,day
      double precision refmjdf,factor,fracday

      double precision utoffset
      external utoffset

      data factor / 86400d0 /

      status = 0

c Convert the UTC string into day and fraction of a day

      CALL xsl_dt2mjd(instr,day,fracday,status)

c Calculate the number of seconds between day and fraction of a
c day and the MJDREF in use for the spacecraft

      secfmjdref = ((day-refmjdi) + (fracday-refmjdf))*factor

c Apply any necessary correction from UTC to spacecraft TIMESYS.

      secfmjdref = secfmjdref + utoffset(day, fracday)

      return
      end

c ------------------------------------------------------------
      subroutine xsl_dt2mjd(instr,intday,fracday,status)
c ------------------------------------------------------------
c
C     The acceptable date/time string is yyyy-mm-dd(Thh:mm:ss.ddd) or
C     dd/mm/yy( hh:mm:ss.ddd). The time string is optional.

      character*(*) instr
      integer status, intday
      double precision dday, fracday
      integer year, month, day, minute, hour        
      double precision second
      integer i
      character(8) olddate
      character(32) oldtime
      character(68) datetime  
      integer iy,im,id
      character(80) errmes, contxt
      integer decimal

      INTEGER lenact
      EXTERNAL lenact

      status = 0

C     Convert the old style dd/mm/yy  hh:mm:ss to yyyy-mm-ddThh:mm:ss

      decimal = 1
      i = index(instr,'/')
      if(i.ne.0) then
        i = i -2 
        olddate = instr(i:i+7)
        call fts2dt(olddate,year,month,day,status)
        if(status.ne.0) goto 143
        minute = 0
        hour = 0
        second = 0.0
        i = index(instr,':')
        if(i.ne.0) then
           i = i -2
           oldtime = instr(i:)
           call fts2tm(oldtime,iy,im,id,hour,minute,second,status)
           contxt = 'Error returned from fts2tm'
           if(status.ne.0) goto 143 
        endif
        call fttm2s(year,month,day,hour,minute,second,decimal,datetime,
     &      status)
        contxt = 'Error returned from fttm2s'
        if(status.ne.0) goto 143 

      else 

c  new style yyyy-mm-dd

        datetime = instr

      endif 

C  split up the date string

      call fts2tm(datetime,year,month,day,hour,minute,second,status)
      contxt = 'Error returned from fts2tm'
      if(status.ne.0) goto 143 

C  convert the year, month and day to MJD

      call sla_cldj(year, month, day, dday, status)
      contxt = 'Error returned from sla_cldj'
      if(status.ne.0) goto 143
      intday = INT(dday)

C  Calculate the fractional date (time of day)

      fracday = hour/24.0 + minute/1440.0 + second/86400.0

      return

143   CONTINUE
      WRITE(errmes, '(a,a,i6)') contxt(1:lenact(contxt)), 
     &                          ', status = ', status
      CALL XWRITE(errmes,5)
      errmes = 'xsl_dt2mjd: date/time = "'//instr//'"'
      CALL XWRITE(errmes,5)
      return   
      end

c --------------------------------------------------
      subroutine skip_delim(string,pos,delims)
c --------------------------------------------------
c
      character*(*) string,delims
      integer pos,lstring

      lstring = len(string)
      do while (index(delims,string(pos:pos)).ne.0)
         pos = pos + 1
         if (pos.gt.lstring) then
            pos = lstring
            goto 999
         endif
      enddo

 999  return
      end


c ----------------------------------------
      subroutine XSL_TIMEFILE(timfil, lenstr)
c ----------------------------------------

      include 'xsel.inc'
      include 'xselvar.inc'

      CHARACTER timfil*(*)
      INTEGER lenstr

c This subroutine makes a temporary file containing the names of all
c the timing filter files in use.

      INTEGER ilun, olun, ier

      CHARACTER(255) stread
      CHARACTER(72) contxt

      INTEGER lenact
      EXTERNAL lenact

c If there are no timing filters in use then return a zero-length filename

      timfil = ' '
      lenstr = 0
      IF ( .NOT.FITTFL .AND. .NOT.HKSEL .AND. .NOT.FFTFL .AND.
     &     .NOT.ASCTFL .AND. .NOT.XPHTFL .AND. .NOT.XWNTFL ) RETURN

c Open the temporary file

      timfil = 'xsel_timefile.asc'
      CALL XSL_RMFILE(timfil)
      CALL getlun(olun)
      CALL XSL_OPEN(olun, timfil, 'new', ' ', ' ', 0, 0, ier)
      IF ( ier .NE. 0 ) THEN
         CALL frelun(olun)
         CALL xwrite('Failed to open xsel_timefile.asc', 5)
         timfil = ' '
         RETURN
      ENDIF
      lenstr = LENACT(timfil)

c If GTI files are are in use then read the file listing GTI files and
c transfer the contents to the output file

      IF( FITTFL .or. HKSEL .or. FFTFL ) then

         CALL getlun(ilun)
         CALL XSL_OPEN(ilun, gtiflt, 'old', ' ', ' ', 0, 1, ier)
         IF ( ier .NE. 0 ) THEN
            CALL frelun(ilun)
            contxt = 'Failed to open '//
     &                 gtiflt(:MIN(LENACT(gtiflt),len(contxt)-15))
            CALL xwrite(contxt, 5)
            GOTO 100
         ENDIF

         READ(ilun,'(a)',iostat=ier) stread
         DO WHILE ( ier .EQ. 0 )
            WRITE(olun,'(a)') stread
            READ(ilun,'(a)',iostat=ier) stread
         ENDDO

         CLOSE(ilun)
         CALL frelun(ilun)

      ENDIF

 100  CONTINUE

c IF required repeat the trick with the ASCII files

      IF( ASCTFL ) then
         CALL getlun(ilun)
         CALL XSL_OPEN(ilun, ascflt, 'old', ' ', ' ', 0, 1, ier)
         IF ( ier .NE. 0 ) THEN
            CALL frelun(ilun)
            contxt = 'Failed to open '//
     &                 ascflt(:MIN(LENACT(ascflt),len(contxt)-15))
            CALL xwrite(contxt, 5)
            GOTO 200
         ENDIF

         READ(ilun,'(a)',iostat=ier) stread
         DO WHILE ( ier .EQ. 0 )
            WRITE(olun,'(a)') stread
            READ(ilun,'(a)',iostat=ier) stread
         ENDDO

         CLOSE(ilun)
         CALL frelun(ilun)

      ENDIF

 200  CONTINUE

c Then either of the two possible Xronos window files

      IF( XPHTFL ) then
         WRITE(olun,'(a)') xphflt
      ENDIF

      IF ( XWNTFL ) THEN
         WRITE(olun,'(a)') xwnflt
      ENDIF

c Close out the file of timing filters

      CLOSE(olun)
      CALL frelun(olun)

      RETURN
      END 

c ---------------------------------------------
      subroutine XSL_RSPSAV(specfile)
c ---------------------------------------------
c Runs rmf and arf generation if required.
c
      IMPLICIT NONE

      include 'xsel.inc'
      include 'xselvar.inc'

      character*(*) specfile
      character(512) scrnam
      CHARACTER(1024) comlin
      CHARACTER(10) rsptyp
      integer ilun, lenact
      logical extend, needreg

c Check whether response parameter is set to true.

      status = 0
      call xsl_uclgst('response', rsptyp, status)
      call upc(rsptyp)
      IF ( status .NE. 0 .OR. rsptyp(1:1) .EQ. 'N' ) RETURN

c Get name from the MDB for the perl script to run

      CALL XSL_MDBS(keymis, submis, instru, datamode, 'respscript', 
     &              scrnam, status)
      IF ( status .NE. 0 .OR. scrnam .EQ. 'NONE' ) RETURN

c Get from the MDB whether this instrument needs the region file informtion

      CALL XSL_MDBB(keymis, submis, instru, datamode, 'respneedreg', 
     &              needreg, status)
      IF ( status .NE. 0 ) needreg = .FALSE.

c Get from the MDB whether this instrument supports an extended response
c option.

      CALL XSL_MDBB(keymis, submis, instru, datamode, 'extendresp', 
     &              extend, status)
      IF ( status .NE. 0 ) extend = .FALSE.

      IF ( rsptyp(1:1) .EQ. 'E' .AND. .NOT.extend ) THEN
         comlin = 'Extended and point source responses are identical'//
     &            ' for this instrument'
         CALL xwrite(comlin, 5)
      ENDIF

c Run perl script

      call XSL_RMFILE(cmdfil)
      call XSL_OPCF(cmdfil, ilun)
      comlin = scrnam(:lenact(scrnam))//' '//specfile(:lenact(specfile))
      IF ( needreg ) THEN
         comlin = comlin(:lenact(comlin))//' '//regfil(:lenact(regfil))
      ENDIF
      IF ( rsptyp(1:1) .EQ. 'E' .AND. extend ) THEN
         comlin = comlin(:lenact(comlin))//' yes'
      ENDIF
      call XSL_WRTCF(ilun, comlin, 0)
      call XSL_CLCF(ilun)
      call XSL_RUNCF(cmdfil, ECHO, status)

      status = 0

      RETURN
      END
