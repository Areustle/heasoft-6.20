      SUBROUTINE EXTRACT(Cmdid,Filename,Status)
      IMPLICIT NONE
c
c  Extract a spectrum or light curve from FITS file
c
c  I  cmdid      (i)  Command id
c  I  filename   (s)  Filename of currently loaded FITS file
c  O  status     (i)  Error flag (0=OK)
c
      INTEGER*4 Cmdid, Status
      CHARACTER*(*) Filename

      INCLUDE 'expar.inc'
      INCLUDE '../stat/detect.inc'

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      include '../include/dynmem.inc'
c
c  Local variables
c
      character(10) uptmp
      integer equinox, itel, clerr, clrmode
      CHARACTER*(MAX_FILELEN) device, qdpfile, engfile, bkgfile, winfile
      CHARACTER*(MAX_FILELEN) fileimage, regionfile , filecolor, rspfile
      CHARACTER*(MAX_FILELEN) filecl3
      CHARACTER*(MAX_IDSTR) wcsid
      integer blocksize, numchan
      integer numsrc, numreg
      character(80) telescop , instrume, detnam, filter, telstr
      character(10) xcol, ycol, tcol, ecol
      integer extnum, nrows, xi, yi, ti, ei
      character(80) field, chanstr
      character(100) ds
      integer slen

      integer dumsz(2), equimg
      real*8 crval(2), crpix(2), cdelt(2), dumcen(2), crota2, dumzm(2)
      character(80) ctype(2), cunit(2)

      INTEGER argc, ieof , iex , in1 , in2 , LENACT
      INTEGER*4 chan(4)
      INTEGER*4  i, j, p_npha
      character(300) cmd
 
      INTEGER ratio(MAXDET,6)
      REAL*4 chi(MAXDET)
      REAL*8 gti(2,MAXGTI) , timewin(2,MAXGTI)
      INTEGER*4 timelun
      INTEGER*4 lun, hdrlun
      INTEGER*4 maxpha, imaxgti , imaxpha , imaxtime , imaxtw
      INTEGER*4 emin, emax
 
      REAL*8 mjd
 
      LOGICAL timevar, psf_pha, there
 
      character*(MAX_FILELEN) filename1, psfhdr

      LOGICAL coldet, colreg, plot
      integer colmode

      filecolor = ' '
      filecl3 = ' '
      filename1 = ' '
      regionfile = ' '
      qdpfile = 'none'
      engfile = ' '
      winfile = ' '
      emin = 0
      emax = 0
      ecol = 'PI'
      coldet = .FALSE.

      do i = 1, 4
         chan(i) = -1
      enddo


      plot = .FALSE.
      plot = .FALSE.
      timevar= .FALSE.
      psf_pha=.FALSE.
c
c timelun need to be initialize because,if qdpfile='none' timelun=-1,but
c if twice the extractor is run within the same ximage and previously
c was timelun =-1 than no qdp file will be created even if qdpfile is 
c different from none  
      timelun=50
c
c  Retrieve argument as filename
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.eq.1 ) then
         call nextcarg(cmdid,filename1, MAX_FILELEN, status)
         argc = argc - 1
      else
         filename1 = Filename
      endif
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'REGIONFILE',regionfile,status)
      CALL GPARS(Cmdid,'COLORFILE',filecolor,status)
      CALL GPARS(Cmdid,'QDPFILE',qdpfile,status)
      CALL GPARS(Cmdid,'ENGFILE',engfile,status)
      CALL GPARS(Cmdid,'WINDOWFILE',winfile,status)
      CALL GPARI(Cmdid,'EMINQDP',emin,status)
      CALL GPARI(Cmdid,'EMAXQDP',emax,status)
      CALL GPARI(Cmdid,'C1',chan(1),status)
      CALL GPARI(Cmdid,'C2',chan(2),status)
      CALL GPARI(Cmdid,'C3',chan(3),status)
      CALL GPARI(Cmdid,'C4',chan(4),status)
      CALL GPARS(Cmdid,'ECOL',ecol,status)
      CALL GPARL(Cmdid,'DETECT_SOURCES',coldet,status)
      CALL GPARL(Cmdid,'PLOT_COLOR',plot,status)
      CALL GPARS(Cmdid,'CL3FILE',filecl3,status)
      CALL GPARL(Cmdid,'VARIABILITY',timevar,status)
      CALL GPARL(Cmdid,'PSF_PHA',psf_pha,status)
      if ( status.ne.0 ) return

      call qustrip(filecolor)
      call qustrip(filecl3)
      call qustrip(regionfile)
      call qustrip(engfile)
      call qustrip(qdpfile)
      call qustrip(winfile)

      if ( coldet .and. NUMdet.le.0 ) then
         call xwrite(' No detected sources available', 5)
         return
      endif
c
      IF ( filename1.EQ.' ' ) THEN
         DO WHILE ( .TRUE. )
            CALL XCREAD(' File name  : ',filename1,ieof)
            IF ( filename1.EQ.' ' ) THEN
               CALL XWARN(' Please type a filename or EXIT',10)
               GOTO 250
            ENDIF
            uptmp = filename1(1:10)
            CALL UPC(uptmp)
            IF ( ieof.NE.0 .OR. uptmp.EQ.'EXIT' .OR. uptmp.EQ.'QUIT'
     &           .OR. uptmp.EQ.'HELP' ) THEN
               status = 1
               RETURN
            ENDIF
            GOTO 300
 250     ENDDO
      ENDIF
c
 300  CONTINUE
      CALL DIRPOS(filename1,in1,in2)
      fileimage = filename1(in2+1:LENACT(filename1))
      iex = INDEX(fileimage,'.')
      IF ( iex.NE.0 ) fileimage = fileimage(1:iex-1)
c
c use the default regionfile name
c
      if( regionfile.eq.' '.and. .not.coldet ) then
         regionfile = fileimage(1:LENACT(fileimage)) // '.reg'
         inquire (file=regionfile,exist=there)
         if ( .not.there ) then
            call xwrite(
     &       ' ERROR: Regionfile required unless DETECT_SOURCES', 10)
            status = -1
            return
         endif
      endif
c
c  if regionfile begins with @, read as region list
c
      colreg = .FALSE.
      if ( regionfile.ne.' ' ) then
         if ( regionfile(1:1).eq.'@' ) colreg = .TRUE.
         if ( colreg ) then
            ZWRite = ' Region list: '//Regionfile(:LENACT(Regionfile))
            CALL XWRITE(ZWRite,10)
         else
            ZWRite = ' Regionfile: '//Regionfile(:LENACT(Regionfile))
            CALL XWRITE(ZWRite,10)
         endif
      endif
            
      if ( coldet .and. colreg ) then
         call xwrite(' DETECT_SOURCES and region list incompatible', 5)
         return
      endif

      
      if (coldet.or.colreg) then
         if (psf_pha) then
            call xwrite(
     &      ' Color-color plot and PSF_PHA must be done separately', 10)
            status = -1
            return
         endif
      else
         psf_pha = .TRUE.
         if ( timevar ) then
            call xwrite(
     &         ' VARIABILITY available only for color-color plot', 10)
            timevar = .FALSE.
         endif
      endif
c
      IF ( qdpfile.EQ.' ' ) THEN
         qdpfile = fileimage(1:LENACT(fileimage))//'.qdp'
      ELSEIF ( qdpfile.NE.'none' ) THEN
         iex = INDEX(qdpfile,'.')
         IF ( iex.LE.0 )qdpfile = qdpfile(:LENACT(qdpfile))//'.qdp'
      ENDIF

      IF ( winfile.NE.' ' ) THEN
         CALL DIRPOS(winfile,in1,in2)
         fileimage = winfile(in2+1:LENACT(winfile))
         IF ( INDEX(winfile,'.').EQ.0 )
     &        winfile = winfile(1:LENACT(winfile))//'.wi'
      ENDIF
      inquire (file=winfile,exist=there)
      if ( .not.there ) winfile = ' '
      IF ( qdpfile.EQ.'none' ) timelun = -1
c
c  Do extract...
c
      CALL GETLUN(lun)
      CALL FTOPEN(lun,filename1,0,blocksize,Status)
      IF ( Status.NE.0 ) THEN
         CALL XAERROR(' File not found in extract',5)
         CALL XWRITE( filename1, 10)
         CALL FTCLOS(lun, Status)
         CALL FRELUN(lun)
         RETURN
      ENDIF
      call rd_detkeys(lun, .FALSE., telescop, instrume, detnam,
     &                filter, itel, status)
      telstr = telescop(:LENACT(telescop))//' '//
     &         instrume(:LENACT(instrume))//' '//
     &         detnam(:LENACT(detnam))
      call rd_objkey(lun, field, status)
c
c  Determine channels for color-color output
c
      if ( coldet.or.colreg ) then
         if ( chan(1).lt.0 .and. itel.gt.0 ) then
            chanstr = ' '
            call gmdbs(itel, 'EXCHAN', chanstr, 0, status)
            call parseilst(chanstr, ',', 4, chan, numchan, status)
         endif
         if ( chan(1).lt.0 ) then
            call XWARN(' Channel values unavailable for mission', 10)
            chan(1) = 0
            chan(2) = 40
            chan(3) = 86
            chan(4) = 200
            write (ZWRite,*) ' Using ', chan
            call RMVXBK(ZWRite(2:))
            call XWRITE(ZWRite, 10)
            call XWRITE(' Results are suspect', 10)
         endif
         if ( .not.(chan(1).gt.0 .and. chan(1).lt.chan(2) .and. 
     &              chan(2).lt.chan(3) .and. chan(3).lt.chan(4)) ) then
            call xwrite(' Channels must satisfy: 0<C1<C2<C3<C4', 10)
            status = -1
            goto 500
         endif
      endif

      status = 0
      CALL GETGTI(lun,gti,MAXGTI,imaxgti,Status)
      IF ( Status.NE.0 ) then
         call XWRITE(' Failed to read GTI table', 10)
         goto 500 
      ENDIF

      extnum = -1
      call go_evttbl(Lun, extnum, nrows, status)
      IF ( Status.ne.0 ) THEN
         CALL XERROR(' EVENTS table not found',5)
         goto 500
      ELSE
        CALL XWRITE(' Found EVENTS table',10)
      ENDIF    
c
c  Get columns
c
      xcol = 'X'
      ycol = 'Y'
      tcol = 'TIME'
      write(ZWRite,'(3a)') ' Using ', ecol(:LENACT(ecol)), 
     &                                ' energy column'
      call xwrite(ZWRite, 10)

      call getcol(lun, xcol, xi, status)
      call getcol(lun, ycol, yi, status)
      call getcol(lun, tcol, ti, status)
      call getcol(lun, ecol, ei, status)
      call rd_elimits(lun, ei, emin, emax, status)

      if ( Regionfile.ne.' ' ) then
         dumcen(1) = 0.d0
         dumcen(2) = 0.d0

c        ** Support region files of degree and hms format

         call tclresi("set default(equinox)", equinox, status)
         call rd_eqxkey (lun, equinox, equimg, Status)
         call rd_ckeys(Lun, xi, yi, dumcen, ctype, cunit, crval, crpix,
     &                 cdelt, crota2)
         dumsz(1) = 100
         dumsz(2) = 100
         dumzm(1) = 1.d0
         dumzm(2) = 1.d0
         call genckeywcs(dumsz, ctype(1), ctype(2), cunit(1), cunit(2),
     &                   crval, crpix, cdelt, crota2, equimg, MAX_IDSTR,
     &                   wcsid, Status)
         call wcssetdet(wcsid, 1, crpix, crpix, dumzm, status)
         call setregwcs(wcsid, status)
         call xwrite(' Initializing internal region structures', 10)
         call xinitreg(Regionfile, numreg, status)
         
         if ( status.ne.0 ) then
            call xwrite(' Failed to read regionfile', 10)
            return
         endif
         if ( numreg.le.0 ) then
            call xwrite(' No regions in regionfile', 10)
            return
         endif
      endif
c
c  Allocate temporary space for npha
c
      colmode = 0
      if ( coldet ) then
         colmode = 1
         numsrc = NUMdet
      elseif ( colreg ) then
         colmode = 2
         numsrc = numreg
      else
c        Dummy value for non-color modes
         numsrc = 1
      endif
      if ( numsrc.gt.MAXDET ) then
         call xwrite(' Too many sources, truncating', 10)
         numsrc = MAXDET
      endif

      if ( colmode.gt.0 ) then
         maxpha = chan(4)
      else
         if ( emax.gt.0 ) then
            maxpha = emax
         else
            call xwrite(' Maximum energy channel not found', 10)
            call xistr(DEFPHA, ds, slen)
            write(ZWRite,'(3a)') ' Defaulting to ', ds(1:slen), ' ...'
            call xwrite(ZWRite, 10)
            maxpha = DEFPHA
         endif
      endif

      call workalloc(1, Numsrc, maxpha, p_npha, status)
      do i = 1, numsrc
         do j = 1, 6
            ratio(i,j) = 0
         enddo
      enddo

      if ( status.ne.0 ) goto 500
      call xwrite(' Reading events...', 10)
      CALL GETEVENTS(lun,memi(p_npha),numsrc,maxpha,imaxpha,timelun,
     &               imaxtime,mjd,winfile,timewin,MAXGTI,
     &               imaxtw,nrows,xi,yi,ti,ei,imaxgti,gti,
     &               colmode,chan,ratio,timevar,chi,Status)
      IF ( Status.NE.0 ) then
         call XWRITE(' Failed to read events table', 10)
         goto 500 
      ENDIF
500   CONTINUE
      clerr = 0
      CALL FTCLOS(lun,clerr)
      CALL FRELUN(lun)
      if ( status.ne.0 ) return 

c      IF ( imaxtime.LE.0 ) THEN
c         CALL XWARN('No light curve data found',5)
c      ENDIF
      IF ( imaxpha.LE.0 ) CALL XWARN('No spectral data found',5)
c
c color-color diagram
c
      call tclress('set default(device)', device, MAX_FILELEN, status)
      IF (filecolor.EQ.' ' .and. filecl3.eq.' ') filecolor = fileimage
      IF ( coldet.or.colreg ) THEN
         if ( filecolor.ne.' ' ) then
            clrmode = 0
            CALL XTEND(filecolor,'col')
            call wrcolor(filecolor, device, telstr,
     &                   field, chan, ratio, chi, clrmode, 
     &                   coldet, numsrc, status)
c
            IF ( plot ) THEN
               cmd = 'qdp '//filecolor(:LENACT(filecolor))
               CALL SPAWN(cmd,LENACT(cmd),status)
            ENDIF
         endif
         if ( filecl3.ne.' ' ) then
            clrmode = 1
            CALL XTEND(filecl3,'cl3')
            call wrcolor(filecl3, device, telstr,
     &                   field, chan, ratio, chi, clrmode, 
     &                   coldet, numsrc, status)
c
            IF ( plot ) THEN
               cmd = 'qdp '//filecl3(:LENACT(filecl3))
               CALL SPAWN(cmd,LENACT(cmd),status)
            ENDIF
         endif
      ENDIF
c
c
c
      IF ( imaxgti.LE.0 ) THEN
         imaxgti = 1
         gti(1,1) = 0
         gti(2,1) = 0
      ENDIF
 
      IF ( timelun.NE.-1 .AND. qdpfile.NE.'none' ) THEN
         IF ( imaxtime.GT.0 ) CALL WRITE_QDP(qdpfile,timelun,imaxtime,
     &        gti,imaxgti,mjd,timewin,MAXGTI,imaxtw,emin,emax,
     &        regionfile,status)
      ENDIF
 
      bkgfile = ' '
      rspfile = ' '
 
      IF ( status.NE.0 ) THEN
         WRITE (filename1,99001) status
 
         CALL XWRITE(filename1,5)
      ENDIF
 
c  pha file for psf
 
      IF (psf_pha) THEN
       IF ( engfile.EQ.' ' ) THEN
           engfile = telescop(:LENACT(telescop))
     &              //'_'//instrume(:LENACT(instrume))//'.psf'
       ENDIF
       CALL XWRITE(' Create ASCII energy file for psf calculation',
     &                  10)
       IF ( regionfile.ne.' ' ) THEN
          CALL GETLUN(lun)
          CALL OPENWR(lun,engfile,'NEW',' ','L',0,0,status)
          IF ( status.NE.0 ) THEN
             CALL XAERROR(' opening pha file',10)
            RETURN
          ENDIF
          WRITE (lun,'(a,1x,a)') '!', telstr(:LENACT(telstr))
          WRITE (lun,'(a,1x,a)') '!',filename1(:LENACT(filename1))
          call lkupcal(itel, 'eng_*.hdr', psfhdr, status)
          if ( status.eq.0 ) then
             call getlun(hdrlun)
             call openwr(hdrlun,psfhdr,'OLD',' ','L',0,1,status)
             do while ( status.eq.0 )
                read (hdrlun,FMT='(a)',IOSTAT=status,ERR=325) ds
                write(lun, '(a)') ds(:LENACT(ds))
             enddo
 325         continue
             close(hdrlun)
             call frelun(hdrlun)
          endif
          status = 0
          WRITE (lun,'(a)') '!'
          WRITE (lun,'(a,i6)') '! Total channels: ', imaxpha
          WRITE (lun,'(a)') '!'
          DO 350 i = 1 , imaxpha
             WRITE (lun,*) memi(p_npha+i-1)
 350      CONTINUE
          CLOSE (lun)
          CALL FRELUN(lun)
       ELSE
          call XWRITE(' No region file, ASCII pha not written', 10)
       ENDIF
      ENDIF
      CALL EXDELTMP
      if ( regionfile.ne.' ' ) then
         call xfreereg(status)
         call wcsdecref(wcsid)
      endif
      call workalloc(0, Numsrc, MAXPHA, p_npha, clerr)
 
      RETURN
99001 FORMAT (' Status was not 0, it was ',i4)
      END
