c
c
c --------------------------------------------------
      subroutine XSL_APPEND(fromfil,tofil,ECHO,status)
c --------------------------------------------------
c This appends the extension given by fromfil to the end of tofil.
c J. Ingham 4/94
c Mod. by Jeff Guerber, HSTX, Aug. 1997. Replaced XSL_EXTSTRIP with FCPARS.

      character*(*) fromfil,tofil
      integer status,LENACT,itemp,ilun
      character(1024) comlin
      character(255) cmdfil, lstfil,errfil,wrkdir
      character(255) tmpnam
      logical ECHO

      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

c Check that the source and target files exist:

      call FCPARS(fromfil,tmpnam,itemp,status)
      call XSL_FITS_EXIST(tmpnam,status)
      IF(status.ne.0) THEN
          comlin = 'Error appending '//fromfil(:LENACT(fromfil))//
     &        ' to '//tofil(:LENACT(tofil))
         call XWRITE(comlin,5)
         call XWRITE('The source file  was not found.',5)
         goto 999
      ENDIF

      call FCPARS(tofil,tmpnam,itemp,status)
      call XSL_FITS_EXIST(tmpnam,status)
      IF(status.ne.0) THEN
         comlin = 'Error appending '//fromfil(:LENACT(fromfil))//
     &        ' to '//tofil(:LENACT(tofil))
         call XWRITE(comlin,5)
         call XWRITE('The target file was not found.',5)
         goto 999
      ENDIF
c Make up the command line, and run it:

      comlin = 'fappend infile='//fromfil(:LENACT(fromfil))//
     &          ' outfile='//tofil(:LENACT(tofil))//
     &          ' pkeywds=yes history=yes '

      call XSL_OPCF(cmdfil,ilun)
      call XSL_WRTCF(ilun,comlin,1)
      call XSL_CLCF(ilun)
      call XSL_RUNCF(cmdfil,ECHO,status)

      IF(status.ne.0) THEN
         comlin = 'Error appending '//fromfil(:LENACT(fromfil))//
     &        ' to '//tofil(:LENACT(tofil))
         call XWRITE(comlin,5)
      ENDIF

 999  return
      end
c --------------------------------------------------------
      subroutine XSL_CAT_FILT(incat, outcat,expr,ECHO,status)
c --------------------------------------------------------
c  This filters incat, with expr, and puts the result in outcat,
c  J. Ingham
      character(1024) comlin
      character(255) cmdfil, lstfil,errfil,wrkdir
      character*(*) incat, outcat,expr
      character(255) newnam
      integer ilun,status,LENACT,len2,len3,len4
      logical ECHO
      integer NKEYS,i
      parameter(NKEYS = 4)
      character(16) outkey(NKEYS),incol(NKEYS)
      character(40) outcom(NKEYS)
      character(255) errmsg
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir
      data (outkey(i),i=1,NKEYS) /'INSTRUME','TELESCOP',
     &     'OBJECT','DATAMODE'/
      data (incol(i),i=1,NKEYS)  /'INSTRUME','TELESCOP',
     &     'OBJECT','DATAMODE'/
      data (outcom(i),i=1,NKEYS) /'The instrument for the catalogue',
     &                           'The telescope for the catalogue',
     &                           'The object for the catalogue',
     &                           'The datamode for the catalogue'/

      status = 0

c Move away outcat, if it exists already
      call XSL_MOVE_AWAY(outcat,newnam)
      len2 = LENACT(outcat)
      len3 = LENACT(incat)
      len4 = LENACT(expr)
c Make sure there isn't an old outcat

c      call XSL_RMFILE(outcat)

      comlin = 'fselect '//incat(1:len3)//' '//
     &                 outcat(1:len2)//' '//
     &                 'expr="'//expr(1:len4)//'" '//
     &                 'histkw=yes copyall=yes'
C      len1 = LENACT(comlin)
      call XSL_OPCF(cmdfil,ilun)
      call XSL_WRTCF(ilun,comlin,1)
      call XSL_CLCF(ilun)
      call XSL_RUNCF(cmdfil,ECHO,status)
      if(status.ne.0) then
         len2 = len(expr)
         errmsg = 'Error filtering catalogue with: '
         len3 = LENACT(errmsg)+1
         IF(len4.lt.len2 - len3) THEN
            errmsg = errmsg(:len3)//expr(1:len4)
         ELSE
            errmsg = errmsg(:len3)//expr(1:len4-3)//'...'
         ENDIF
         call xwrite (errmsg,5)
      ELSE
c Now update the keywords:
         call XSL_COL2KEY(incol,outkey,outcom,NKEYS,NKEYS,
     &        outcat,-1,1,1,status)
      ENDIF
      return
      end
c
c
c ---------------------------------------------------------------
      subroutine XSL_FFORHK(npar,HKCURV,parlis,hkcurf,plotdv,
     &     keytim,tempfl,ECHO,MAXFIL)
c ---------------------------------------------------------------
c  This plots either the HK or the FF output.
c  J. Ingham  6/93

      include 'xselplt.inc'
      integer npar,len1,status,MAXFIL
      character*(*) parlis(MAXFIL),hkcurf,plotdv,keytim
      character*(*) tempfl
      integer MXCMDS,MAXBUF,nusrcm
      parameter ( MXCMDS = 30, MAXBUF=100 )
      character(80) qdpbuf(MXCMDS)
      character(255) str1,pltcmd,qdpstr
      character(255) str2,cmdfil, lstfil,errfil,wrkdir,xparm
      real rmin,rmax,buf1(2,MAXBUF)
      integer  LENACT,i,j,nr,parnum(MAXBUF),nplot,ierr,len2,ilun,ilun2
      integer nplts,nperpl,nthisp,rem,xindex
      logical HKCURV ,ECHO, VERT
      common /xselcmd/ cmdfil, lstfil,errfil,wrkdir

c Check that MAXBUF > MAXFIL

      IF ( MAXBUF .LT. MAXFIL )
     &  CALL xwrite('Warning in XSL_FFORHK: MAXBUF < MAXFIL',5)

c Get the number of curves per plot

      status = 0
      call xsl_uclgsi('curves_per_plot',nperpl,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgsb('plot_vertical',VERT,status)
      IF ( status .NE. 0 ) RETURN
      call xsl_uclgst('qdp_commands',qdpstr,status)
      IF ( status .NE. 0 ) RETURN
c If the QDP string is a file, then read in that file now:
      IF (qdpstr.ne.'NONE') THEN
         call XSL_PARSE(qdpstr,qdpbuf,nusrcm,MXCMDS,status)
      ELSE
         nusrcm = 0
      ENDIF

      rmin = 1
      rmax = npar
      nplot = 0

      IF( .NOT.HKCURV )then
         call XWRITE(' No curve has been accumulated yet.',5)
         return
      ENDIF

      if( npar.gt.1) then
c Type out prompt, and parameter list

         str1='The available parameters are:'
         CALL XWRITE(str1(:LENACT(str1)),5)
         call XWRITE('----------------------------',5)
         str1 = ' 0 - '//keytim(:LENACT(keytim))
         call XWRITE(str1,5)
         do i=1,npar
c Include only those parameters that were fcurved
            write(str1,50) i,parlis(i)(:LENACT(parlis(i)))
 50         format(i2,' - ',a)
            CALL XWRITE(str1,5)
         enddo
c First get the X-axis parameter:
         call xsl_uclgsi('plot_xparm',xindex,status)
         IF ( status .NE. 0 ) RETURN
c 0 is the flag for TIME
         IF(xindex.eq.0) THEN
            xparm = keytim
         ELSE
            xparm = parlis(xindex)
         ENDIF

c Then get the list of Y-axis parameters:

         call xsl_uclgsg('plot_which',buf1,
     &        MAXFIL,rmin,rmax,nr,status)
         if( status.ne.0) then
            call XWRITE('Error getting plot_which parameter',5)
            return
         endif
c Process the ranges:
         do i=1,nr
c                if(buf1(2,i).eq.rmax) then
c                   buf1(2,i) = buf1(1,i)
c                endif
            do j=int(buf1(1,i)),int(buf1(2,i))
               nplot = nplot+1
               parnum(nplot) = j
            enddo
         enddo
c Check that the ranges are valid
         do i=1,nplot
            if(parnum(i).gt.npar ) then
               write(str1,70) parnum(i)
 70            format(i2,' is not a valid selection')
               len1 = LENACT(str1)
               call XWRITE(str1(1:len1),5)
               return
            endif
         enddo
      else if (npar.eq.1) then
         nplot = 1
         parnum(1) = 1
         xparm = keytim
      endif
c Now plot the parameters in groups of nperpl:

      len1 = LENACT(hkcurf )
      len2 = LENACT( plotdv )
C get a LUN for the PLT command file:
      call GETLUN(ilun2)
C calculate the number of plots and remainder
      nplts = nplot/nperpl
      rem = nplot - nplts*nperpl
c i=0 to nplts-1 are the full plots, i=nplts is the remainder plot.
      do i=0,nplts
c Write out the command file:
c If there is no remainder, then exit
c nthisp is the number of variables in the current plot:
         IF(i.lt.nplts) THEN
c This is a full plot
            nthisp = nperpl
         ELSE IF (rem.eq.0) THEN
            call FRELUN(ilun)
            call FRELUN(ilun2)
            return
         ELSE
c This does the remainder
            nthisp = rem
         ENDIF
c Open the fiel for QDP commands we pass to Fplot
         call XSL_RMFILE(tempfl)
         call XSL_OPEN(ilun2,tempfl,'NEW',' ',' ',0,0,ierr)
         IF(xindex.eq.0) THEN
c If you are plotting against time, and the plots are vertical
c then the command file is 1 command,
c so we can send this from FPLOT, otherwise build colors,...
            IF(nplot.gt.1.and. .NOT. VERT) THEN
               write(ilun2,52) plotdv(:LENACT(plotdv)),
     &              xparm(:LENACT(xparm)),hkcurf(:LENACT(hkcurf))
 52            format('Device ',a,/,'LW 1',/,'LA X ',a,/
     &              ,'LA T Plot of file ',a)
               write(ilun2,51) (j,j+1,j=1,nthisp)
 51            format('COLOR ',i2,' ON ',i2)
               write(ilun2,57) (j+nperpl,j,.85-j*.025,
     &              parlis(parnum(i*nperpl + j))
     &              (:LENACT(parlis(parnum(i*nperpl + j))))
     &              ,j=1,nthisp)
 57            format('LABEL ',i2,' COLOR ',i2,
     &              ' VPOS .15 ',f7.3,
     &              ' CSIZE .8  "',a,'"')
               pltcmd = '@'//tempfl
            ELSE
               write(ilun2,52) plotdv(:LENACT(plotdv)),
     &              xparm(:LENACT(xparm)),hkcurf(:LENACT(hkcurf))
               write(ilun2,53) (j+1,j=1,nthisp)
               write(ilun2,56) (j+1,
     &              parlis(parnum(i*nperpl + j))(:40),j=1,nthisp)
               write(ilun2,'(a)') 'PLOT VERT'
               IF(nthisp.eq.4) THEN
                  write(ilun2,'(a)') 'CSIZE .8'
               ELSE IF(nthisp.ge.5) then
                  write(ilun2,'(a)') 'CSIZE .6'
               ENDIF
               pltcmd = '@'//tempfl
            ENDIF
         ELSE
c If you are not plotting against time, then don't connect the points:
c This is only for the full plots:
c Now write the commands
            write(ilun2,52) plotdv(:LENACT(plotdv)),
     &           xparm(:LENACT(xparm)),hkcurf(:LENACT(hkcurf))
            IF(nthisp.gt.1) THEN
               IF(VERT) THEN
                  write(ilun2,53) (j+1,j=1,nthisp)
 53               format('MARKER 1 ON ',i2)
                  write(ilun2,56) (j+1,
     &                 parlis(parnum(i*nperpl + j))(:40),j=1,nthisp)
 56               format('LABEL G',i1,' ',a40)
                  write(ilun2,'(a)') 'PLOT VERT'
                  IF(nthisp.eq.4) THEN
                     write(ilun2,'(a)') 'CSIZE .8'
                  ELSE IF(nthisp.ge.5) then
                     write(ilun2,'(a)') 'CSIZE .6'
                  ENDIF
               ELSE
                  write(ilun2,54) (j,j+1,j,j+1,j=1,nthisp)
 54               format('COLOR ',i2,' ON ',i2,/,
     &                 'MARKER ',i2,' ON ',i2)
                  write(ilun2,55) (j+nperpl,j,j,.85-j*.025,
     &                 parlis(parnum(i*nperpl + j))
     &                 (:LENACT(parlis(parnum(i*nperpl + j))))
     &                 ,j=1,nthisp)
 55               format('LABEL ',i2,' MARKER ',i2,' COLOR ',i2,
     &                 ' VPOS .15 ',f7.3,
     &                 ' CSIZE .8  "',a,'"')

               ENDIF
            ELSE
               write(ilun2,'(a)') 'MARKER 1 on 2'
               write(ilun2,56) 2,
     &              parlis(parnum(i*nperpl + 1))(:40)
            ENDIF

            pltcmd = '@'//tempfl
         ENDIF
c Now add the optional QDP commands.
         IF(nusrcm.gt.0) THEN
            do j=1,nusrcm
               write(ilun2,'(a)') qdpbuf(j)
            enddo
         ENDIF

         close(ilun2)
         call FRELUN(ilun2)

         call XSL_OPCF(cmdfil,ilun)
         str2 = parlis(parnum(i*nperpl + 1))
         do j=2,nthisp
            str2 = str2(:LENACT(str2))//' '//
     &           parlis(parnum(i*nperpl+j))
         enddo

c Use the offsetting if the X-axis is time, otherwise don't.
         IF(xindex.eq.0)THEN
            str1=fpltwin(:lenact(fpltwin))//' fplot '//
     +           'infile = '//hkcurf(1:LENACT(hkcurf))//' '//
     +           'xparm = '//xparm(:LENACT(xparm))//' '//
     +           'yparm = "'//str2(:LENACT(str2))//'"'//
     +           ' '//'rows = "-" '//
     +           'device = "'//plotdv(1:len2)//'" '//
     +           'offset = yes '//
     +           'pltcmd = '//pltcmd(:LENACT(pltcmd))//
     +           fpltbkg(:lenact(fpltbkg))
         ELSE
            str1=fpltwin(:lenact(fpltwin))//' fplot '//
     +           'infile = '//hkcurf(1:LENACT(hkcurf))//' '//
     +           'xparm = '//xparm(:LENACT(xparm))//' '//
     +           'yparm = "'//str2(:LENACT(str2))//'"'//
     +           ' '//'rows = "-" '//
     +           'device = "'//plotdv(1:len2)//'" '//
     +           'offset = no '//
     +           'pltcmd = '//pltcmd(:LENACT(pltcmd))//
     +           fpltbkg(:lenact(fpltbkg))
         ENDIF
         call XSL_WRTCF(ilun,str1,0)
         call XSL_CLCF(ilun)

         call XSL_RUNCF(cmdfil,ECHO,ierr)
      enddo

      return
      end
c
c
c ----------------------------------------------
      subroutine XSL_GTIFILTER(tstart,tstop)
c ----------------------------------------------
c This routine uses the GTI files associated with the data, plus all of
c the FITS  GTI's, to filter the data.  It puts the result in work1
c Jim Ingham Feb/93
c Modified April 94 to include all FITS GTI's, and not merge the output.
      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(255) str1,tmplis(MXNSEL+1)
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings
      integer len1, len2, len3, totgti, i
c ---------------------------------------------
      integer LENACT
      double precision tstart, tstop

      status=0

      IF( .NOT.READ ) then
         call XWRITE(' No data has been read in yet.',5)
         return
      ENDIF

c This is no longer an error, since there may be other GTI's
c$$$          IF( .NOT.USEGTI ) THEN
c$$$               call XWRITE('USEGTI is false, no GTI',5)
c$$$               call XWRITE('Error in XSL_GTIFILTER',5)
c$$$               status = -10
c$$$               return
c$$$          ENDIF

c  Now merge the gtifile part, if necessary

      IF( USEGTI .and. MANY .AND. .NOT. MERGTI) then
         IF( WRKGTI )then
            call XSL_MERGEGTI(gtiwk1,nfiles,'OR',lstfil,mrgtif,
     +           cmdfil,ECHO,status)
         ELSE
            call XSL_MERGEGTI(gtifnm,nfiles,'OR',lstfil,mrgtif,
     +           cmdfil,ECHO,status)
         ENDIF
         if(status.eq.0) then
            MERGTI = .TRUE.
         else
            call XWRITE('Error in merging GTI files',5)
            return
         endif
      ENDIF

c Now merge the file GTI's with the other FITS GTI's, REMEMBER, they will be
c listed in the file gtiflt, so just:
      call XSL_EXIST(gtiflt,status)
      IF (status.eq.0 ) THEN
         call GETLUN(ilun)
         call XSL_OPEN(ilun,gtiflt,'OLD',' ',' ',0,0,status)
         do i=1,MXNSEL
            read(ilun,'(a)',end = 99,err = 999) tmplis(i)
         enddo
         i = i+1
c Fill the tmplis vector with files to pass to XSL_MERGEGTI
 99      close(ilun)
         call FRELUN(ilun)
         if(USEGTI) then
            IF(MANY ) THEN
               tmplis(i) = mrgtif
            ELSE IF(WRKGTI) THEN
               tmplis(i) = gtiwk1(1)
            ELSE
               tmplis(i) = gtifnm(1)
            ENDIF
            totgti = i
         ELSE
            totgti = i-1
         ENDIF
         call XSL_MERGEGTI(tmplis,totgti,'AND',lstfil,allgti,
     +        cmdfil,ECHO,status)
         if(ierr.ne.0) then
            call XWRITE('Error merging GTI files',5)
            return
         endif
      ELSE
         status = 0
         IF(USEGTI) THEN
            IF(MANY ) THEN
               call XSL_RMFILE(allgti)
               call XSL_COPY(mrgtif,allgti,status)
            ELSE IF(WRKGTI) THEN
               tmplis(1) = gtiwk1(1)
               call XSL_RMFILE(allgti)
               call XSL_COPY(mrgtif,allgti,status)
            ELSE
               tmplis(1) = gtifnm(1)
               call XSL_RMFILE(allgti)
               call XSL_COPY(mrgtif,allgti,status)
            ENDIF
         ELSE
c There is no filtering to do..., Flag this by setting FILTER to false.
            FILTER = .FALSE.
            return
         ENDIF
      ENDIF

c  Now use fltime to filter the data
      call XSL_OPCF(cmdfil,ilun)
      len3 = LENACT(keytim)

      do i=1,nfiles
         IF(WORK) then
            len1 = LENACT(work1(i))
            comlin = 'fltime '//work1(i)(1:len1)//' '
            len1 = LENACT(comlin)+1
            str1 = 'Now filtering '//work1(i)(1:len1)
            call XSL_MESSAGE(ilun,str1)
         ELSE
            len1 = LENACT(filenm(i))
            comlin = 'fltime '//filenm(i)(1:len1)//'+'//evtnum//' '
            len1 = LENACT(comlin)+1
            str1 = 'Now filtering '//filenm(i)(1:len1)
            call XSL_MESSAGE(ilun,str1)
         ENDIF
         len2 = LENACT(allgti)
         comlin = comlin(1:len1)//'gtifile='
     &        //allgti(1:len2)//' '
         len1 = LENACT(comlin)+1

c  Now add the other arguments

         len2 = LENACT(work2(i))

         comlin = comlin(1:len1)//'outfile='//work2(i)(1:len2)//
     +        ' column='//keytim(1:len3)//' '//
     +        'obsdate=MJDREF'//' '//
     +        'obstime=" "'//' '//
     +        'gticols=START,STOP'//' '//
     +        'gtidate=MJDREF'//' '//
     +        'gtitime=" "'//' '//
     +        'exposure=EXPOSURE'//' '//
     +        'copyall=no'



         call XSL_WRTCF(ilun,comlin,1)

      enddo
      call XSL_CLCF(ilun)
      
      call XSL_RUNCF(cmdfil,ECHO,status)

      IF(status.ne.0) THEN
         FILTER = .FALSE.
         call XWRITE('Error filtering files',5)
         return
      ENDIF
c  Now copy work2 back to work1

      call XSL_OPCF(cmdfil,ilun)
      do i=1,nfiles
         len1 = LENACT(work1(i))
         call XSL_RENAME(work2(i),work1(i),2,str1,len1,ierr)
         write(ilun,'(a)') str1(:len1)
      enddo
      call XSL_CLCF(ilun)

      call XSL_RUNCF(cmdfil,ECHO,status)
      if(status.eq.0) then
         WORK = .TRUE.
         FILTER = .TRUE.
c Finally we have  to get the start time of the first GTI and the stop time
c of the last GTI out of the file:

         call XSL_TLIMITS(allgti,wrkdir,'START','STOP',
     &        tstart,tstop,status)
         if(status.ne.0) THEN
            call XWRITE
     &           ('Error getting first and last GTI times',5)
         endif
      ELSE
         call XWRITE
     &        ('Error copying filtered files to workspace',5)
         FILTER = .FALSE.
         WORK = .FALSE.
      ENDIF

      return

 999  status = -10
      call XWRITE
     &     ('Error reading in the accumulated list of GTI''s',5)
      return
      end

c
c
c
c ----------------------------------------------
      subroutine XSL_HKEXPAND(param,dtime,ierrno)
c ----------------------------------------------
c
c     Jim Ingham 2/17/93

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c The command line
      character(1024) comlin
c Strings used, reused, reused again as temporary space
      character(255) str2, str3
c File I/O unit numbers
      integer ilun
c General reusable integers for lengths of strings
      integer len1, len2, len3
c ---------------------------------------------

      integer LENACT,i,ierrno
      double precision dtime
      character*(*) param

      IF( MERGHK ) THEN
       len1 = LENACT(mrghkf)
       comlin = 'hkexpand '//mrghkf(1:len1)//' '//hkwrk2(1)//' '
      ELSE IF( WORKHK ) THEN
       len1 = LENACT(hkwrk1(1))
       comlin = 'hkexpand '//hkwrk1(1)(1:len1)//' '//hkwrk2(1)//' '
      ELSE
       len1 = LENACT(hkflnm(1))
       comlin = 'hkexpand '//hkflnm(1)(1:len1)//' '//hkwrk2(1)//' '
      ENDIF
      len1 = LENACT(comlin) + 1

c    Now set up the rest of the command string for hkexpand
c     First the param part

      if(param(1:1).ne.'-') then
         str2 = 'param= "'//parlis(1)//' '
         len2 = LENACT(str2)+1
         do i=2,npar
            len3 = LENACT(parlis(i))
            str2 = str2(1:len2)//parlis(i)(1:len3)//' '
            len2 = LENACT(str2) + 1
         enddo
         comlin = comlin(1:len1)//str2(1:len2)//'" '
         len1 = LENACT(comlin) + 1
      else
         CALL XWRITE('- not yet supported',5)
         return
      endif

c     Now the rest of it
      write(str3,50) dtime
 50   format('dtime = ',e15.7)
      len3 = LENACT(str3)
      comlin = comlin(1:len1)//str3(1:len3)//' '//'name="NAME" '//
     &          'value="VALUE" time="TIME" tnull=" " '//
     &          'constdiv="yes"  copyall="no"'
C      len1 = LENACT(comlin)


c Delete the hkwrk2 file:

      call XSL_RMFILE(hkwrk2(1))

c Now set up the command file
c Open new command file

      call XSL_OPCF(cmdfil,ilun)

      call XSL_WRTCF(ilun,comlin,1)

      call XSL_CLCF(ilun)

c Run the command file
      call XSL_RUNCF(cmdfil,ECHO,ierrno)
      IF(ierrno.ne.0) THEN
         return
      ENDIF

c Now move the output to hkwrk1:
      len2 = LENACT(hkwrk1(1))
      call XSL_RENAME(hkwrk2(1),hkwrk1(1),1,str2,len2,status)

c Finally set the expand variable
      EXPAND = .TRUE.
      WORKHK = .TRUE.
      return
      end
c
c
c
c ---------------------------------------------
      subroutine XSL_MERGE(files,nfiles,lstfil,merfil,outext,
     +     cmdfil,str1,ECHO,ierr)
c ---------------------------------------------
c Called by XSEL, this routine merges together NFILES files
c with names FILES(NFILES) into MERFIL, using LSTFIL
c as a temporary staging area for the filenames, and returns
c ierr=0 for a successful operation.
c
c     Alan Smale 1992 Oct

      implicit none

      integer nfiles
      character*(*) files(nfiles),lstfil,outext
      character*(*) merfil,cmdfil,str1
      integer ierr, ilun, i
      integer len1, len2, LENACT
      logical ECHO


      call GETLUN(ilun)
      call XSL_RMFILE(lstfil)
      call XSL_OPEN(ilun,lstfil,'NEW',' ',' ',0,0,ierr)

      do i=1,nfiles
         len1=LENACT( files(i) )
         write(ilun,53) files(i)(1:len1 )
      end do

      close(ilun)
      call FRELUN(ilun)

c Set up and run the FMERGE

      call XSL_RMFILE(merfil)

      call XSL_OPCF(cmdfil,ilun)

      len1 = LENACT( lstfil )
      len2 = LENACT( merfil )

      str1 = 'fmerge '//
     +     'infiles = @'//lstfil(1:len1)//' '//
     +     'outfile = '//merfil(1:len2)//' '//
     +     'columns = "-" '//
     +     'mextname = '//outext(:LENACT(outext))//' '//
     +     'copyprime = "yes" '//
     +     'lastkey = "-" '//
     +     'history = "yes" '

C      len1 = LENACT( str1 )
      call XSL_WRTCF(ilun,str1,1)
      call XSL_CLCF(ilun)

      call XSL_RUNCF(cmdfil,ECHO,ierr)
      IF(ierr.ne.0) THEN
         ierr = len1
      ENDIF

 53   format(a)
      return
      end
c
c
c
c ---------------------------------------------
      subroutine XSL_MERGEGTI(files,nfiles,mode,lstfil,
     +    merfil,cmdfil,ECHO,ierr)
c ---------------------------------------------
c Called by XSEL, this routine merges together NFILES GTI files
c with names FILES(NFILES) into MERFIL, using LSTFIL
c as a temporary staging area for the filenames, and returns
c ierr=0 for a successful operation.
c
c     Jim Ingham 2/9/93

      implicit none

      integer nfiles
      character*(*) files(nfiles),lstfil,merfil,cmdfil
      character*(*) mode
      character(1024) comlin
      integer ierr, ilun, i
      integer len1, len2, LENACT
      logical ECHO

c Set up the LSTFIL, using the XSL_OPEN - XSL_WRTCF - XSL_CLCF sequence
c used when setting up command files.

      call GETLUN(ilun)
      call XSL_RMFILE(lstfil)
      call XSL_OPEN(ilun,lstfil,'NEW',' ',' ',0,0,ierr)

      do i=1,nfiles
         len1=LENACT( files(i) )
         write(ilun,53) files(i)(1:len1 )
      end do

      close(ilun)
      call FRELUN(ilun)

c Set up and run the mgtime

      call XSL_RMFILE(merfil)

      call XSL_OPCF(cmdfil,ilun)

      len1 = LENACT( lstfil )
      len2 = LENACT( merfil )

      comlin = 'mgtime '//
     +     'ingtis=@'//lstfil(1:len1)//' '//
     +     'outgti='//merfil(1:len2)//' '//
     +     'merge="'//mode(1:3)//'"'//' '//
     +     'instarts="START"'//' '//
     +     'instops="STOP"'//' '//
     +     'indates="MJDREF"'//' '//
     +     'intimes=" "'//' '//
     +     'outstart="START"'//' '//
     +     'outstop="STOP"'

C      len1 = LENACT( comlin )
      call XSL_WRTCF(ilun,comlin,1)
      call XSL_CLCF(ilun)

      call XSL_RUNCF(cmdfil,ECHO,ierr)

 53   format(a)
      return
      end
