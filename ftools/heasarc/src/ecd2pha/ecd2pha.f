
*+ECD2PHA
c      -----------------
       subroutine ecd2pa
c      -----------------
c --- DESCRIPTION -----------------------------------------------------
c ECD2PHA converts Einstein SSS spectra to OGIP standard so that the
c spectra can be used with FTOOLS such as MATHPHA and GRPPHA 
c
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character(80) infile, outfile
      integer chatter
      integer nchan,dtype,errflg,detchans,fchan,extnum
      character(20) instrum, tlscop,context,chantype
      character(20) filter,detnam
      character(30) object
      character(70) desc,termdesc,hduclas2
      character(8) task
      integer phsize
      parameter (phsize = 5012)
      integer channel(phsize), counts(phsize), grping(phsize)
      integer qualty(phsize),numint
      integer*2 conv(phsize)
      real rcts(phsize), serr(phsize), syserr(phsize)
      real ascale,bscale,cscale,texpos,expos(20),mepoc(20)
      real sttime,stptime,jcgparm,mean_ice
      real ra,dec,equinox,meanepoc
      character(68) date_obs,time_obs,date_end,time_end
      integer majorfrm,minorfrm
      character(60) backfil,corfil,rmffil,arffil
      logical qerror, qsys, qqual, killit,copyall,copyprime
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c infile     char   : Input filename
c outfile    char   : Output filename
c chatter    int    : Chattiness flag (>20 verbose)
c nchan      int    : Number of channels in input file
c dtype      int    : datatype, 1 is counts, and 2 is rate
c instrum    int    : Instrument used
c task       char   : Task name : ECD2PHA
c phsize     int    : Array dimension
c channel    int    : Array containing Channel numbers
c counts     int    : Array for counts
c rate       real   : Array containing count rate
c grping     int    : Grouping Array
c qualty     int    : Array containg quality flags
c conv       int*2  : conversion array
c serr       real   : statistcal errors
c syserr     real   : fractional systematic errors
c qerror     logical: true if statistical errors 
c qsys       logical: true if systematic errors
c qqual      logical: true if quality flags set
c --- CALLED ROUTINES -------------------------------------------------
c
c ECD_GP     : Gets parameters
c ECD_RDAT   : Reads Einstein SSS spectra
c ECD_WT     : Write output file
c 
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c                                   
c Rehana Yusaf (1995 Jan 18) 1.0.0; Original
C Peter D Wilson (1998 Jun 22) 1.0.1; Mods to make compatible with new fcpars
c Ning Gan (1998 Jun 29) 1.0.2; use fts2dt to extract the date.
c kaa (2012 Dec 13) 1.0.3; fixed bug in ecd_wt which caused bus error on some files
      character(5) version
      parameter (version = '1.0.3')
      character(40) taskname
      COMMON/task/taskname
      taskname='ECD2PHA'//version
*-
c ---------------------------------------------------------------------
c
c --- GET PARAMETERS ---
c
      context = 'fatal error'
      termdesc =' ECD2PHA Ver '//version//' terminated !'
      errflg = 0
      call ecd_gp(infile,outfile,killit,copyall,
     &            copyprime,errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF
c
c --- USER INFO ---
c
      IF (chatter.GE.1) THEN
        desc = ' Main ECD2PHA Ver '//version
        call fcecho(desc)
      ENDIF
      task = 'ECD2PHA'
c
c --- READ PHA FILE ---
c
      call ecd_rdat(infile,hduclas2,object,fchan,extnum,
     &             channel,counts,rcts,dtype,qerror,serr,
     &             qsys,syserr,qqual,qualty,nchan,detchans,conv,
     &             phsize,instrum,tlscop,detnam,filter,
     &             chantype,texpos,ascale,bscale,cscale,
     &             backfil,corfil,rmffil,arffil,sttime,stptime,
     &             majorfrm,minorfrm,jcgparm,mean_ice,ra,dec,equinox,
     &             date_obs,time_obs,date_end,time_end,meanepoc,
     &             numint,expos,mepoc,errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF              
c
c --- WRITE OUTPUT FILE ---
c

      call ecd_wt(infile,outfile,copyall,copyprime,
     &           hduclas2,fchan,extnum,channel,counts,rcts,
     &           dtype,qerror,serr,qsys,syserr,qqual,
     &           qualty,grping,nchan,detchans,conv,
     &           phsize,instrum,tlscop,detnam,filter,object,chantype,
     &           texpos,ascale,bscale,cscale,
     &           backfil,corfil,rmffil,arffil,sttime,stptime,
     &           majorfrm,minorfrm,jcgparm,mean_ice,ra,dec,equinox,
     &           date_obs,time_obs,date_end,time_end,meanepoc,
     &           numint,expos,mepoc,killit,errflg,chatter)
      IF (errflg.NE.0) THEN
        call fcerr(context)
        call fcecho(termdesc)
        return
      ENDIF   
      IF (chatter.GE.1) THEN
        desc = ' ECD2PHA ver '//version//' completed'
        call fcecho(desc)
      ENDIF
      return
      end
c ----------------------------------------------------------------------
c     END OF MAIN ECD2PHA
c ----------------------------------------------------------------------
    
*+ECD_GP
c     -------------------------------------------------------
      subroutine ecd_gp(infile,outfile,killit,copyall,
     &                  copyprime,errflg,chatter)
c     -------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      character(80) filename
      character(70) ill_files(5)
      integer errflg,chatter,n_ill
      character(26) errstr, wrnstr
      integer status, extnum
      character(70) desc
      logical ext,valfil,killit,copyall,copyprime
      integer flen, fcstln
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c infile     char   : input file name
c outfile    char   : Output filename
c chatter    int    : Chattiness flag, >20 verbose
c clobber    logical: Overwrite existing file ?
c errflg     int    : Error flag
c
c --- CALLED ROUTINES -------------------------------------------------
c
c UCLGST     : (HOST) Get string input
c UCLGSI     : (HOST) Get integer input
c FCECHO     : (FTOOLS) Screen write
c
c --- COMPILATION/LINKING ---------------------------------------------
c
c CALTOOLS, FTOOLS
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1995 Jan 18) 1.0.0; 
      character(5) version 
      parameter (version = '1.0.0')
*-
c ---------------------------------------------------------------------
c
      errstr = ' ERROR : ECD_GP Ver '//version//':'
      wrnstr = ' WARNING : ECD_GP Ver '//version//':'

c GET INFILE

      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting infile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(infile)
      IF (infile.EQ.'  ') THEN
        errflg = 1
        desc = ' Input PHA file not entered !'
        call fcecho(desc)
        return
      ENDIF

C PDW 6/22/98: Leave this for ftopen to determine
C
C      call fcpars(infile,filename,extnum,status)
C      ext = .true.
C      flen = fcstln(filename)
C      INQUIRE(FILE=filename(:flen),EXIST=ext)
C      IF (.NOT.ext) THEN
C        errflg = 1
C        desc = errstr//' File does not EXIST :'//filename
C        call fcecho(desc)
C        return
C      ENDIF

c GET OUTFILE 

      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting outfile parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

c GET CLOBBER

      call uclgsb('clobber',killit,status)
      IF (status.NE.0) THEN
        killit = .false.
        desc = errstr//' ... getting clobber value '
        call fcecho(desc)
      ENDIF


c OUTFILE VALIDATION 

      call crmvlbk(outfile)
      IF (outfile.EQ.'  ') THEN
        desc = errstr//' outfile must be entered !!'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF

      n_ill = 1
C PDW 6/22/98: Must strip off extended syntax for ck_file comparison
C      ill_files(1) = infile
      call ftrtnm(infile,ill_files(1),status)
      call ck_file(outfile,ill_files,n_ill,valfil,
     &             killit,chatter)
      IF (.NOT.valfil) THEN
        errflg = 2
        return
      ENDIF

c GET CHATTER 

      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        desc = errstr//' .. getting chatter parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF        

c GET COPYALL
    
      status = 0
      call uclgsb('copyall',copyall,status)
      IF (status.NE.0) THEN
        copyall = .false.
        desc = errstr//' ... getting copyall value '
        call fcecho(desc)
      ENDIF

c GET COPYPRIME

      status = 0
      call uclgsb('copyprime',copyprime,status)
      IF (status.NE.0) THEN
        copyprime = .false.
        desc = errstr//' ... getting copyprime value '
        call fcecho(desc)
      ENDIF
      return
      end
c ---------------------------------------------------------------------
c     END OF ECD_GP 
c ---------------------------------------------------------------------


*+ECD_RDAT
c     -----------------------------------------------------
      subroutine ecd_rdat(infile,hduclas2,object,fchan,
     &         extnum,channel,counts,rcts,
     &         dtype,qerror,serr,qsys,syserr,qqual,
     &         qualty,nchan,detchans,conv,
     &         phsize,instrum,tlscop,detnam,filter,chantyp,
     &         texpos,ascale,bscale,cscale,
     &         backfil,corfil,rmffil,arffil,sttime,stptime,
     &         majorfrm,minorfrm,jcgparm,mean_ice,ra,dec,equinox,
     &         date_obs,time_obs,date_end,time_end,meanepoc,
     &         numint,expos,mepoc,errflg,chatter)    
c     -----------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c 
c This routine reads the Einstein SPECTRUM PHA extension, 
c by calling RDSSS1
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      integer phsize
      character*(*) infile,hduclas2,object
      character*(*) instrum, tlscop,detnam,chantyp
      integer chatter, nchan, errflg, dtype,fchan,extnum
      integer channel(phsize), counts(phsize),numint
      integer qualty(phsize),detchans
      integer*2 conv(phsize)
      real rcts(phsize), syserr(phsize), serr(phsize)
      real texpos,ascale,bscale,cscale,expos(*),mepoc(*)
      real sttime,stptime,jcgparm,mean_ice
      real ra,dec,equinox,meanepoc
      integer majorfrm,minorfrm
      character*(*) date_obs,time_obs,date_end,time_end
      logical qerror, qsys, qqual,pois
c
c --- LOCALS ---------------------------------------------------------
c
      character*(*) filter, backfil,corfil,arffil,rmffil
      character(70) desc,errinfo
      character(80) filename
      character(28) errstr,wrnstr,comm
      integer  status,block,iunit,imove
      integer  ninstr,nsearch,nfound,next(50),htype,i,j
      character(20) extname(50),outhdu(9,50),instr(50),outver(9,50)
      character(8) phaversn
      double precision mjdobs
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1995 Jan 18) 1.0.0;
      character(5) version
      parameter (version = '1.0.0')
*-
c -------------------------------------------------------------------
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        desc = ' ... using ECD_RDAT Ver '//version//':'
        call fcecho(desc)
      ENDIF
      errstr = ' ERROR : ECD_RDAT Ver '//version//':'   
      wrnstr = ' WARNING : ECD_RDAT Ver '//version//':'
c
c --- OPEN PHAFILE ---
c
      status = 0
      block = 0
      call fcpars(infile,filename,extnum,status)
      status = 0
      call ftgiou(iunit,status)
      call ftopen(iunit,filename,0,block,status)
      errinfo = errstr//' opening PHA file'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        errflg = 1
        return
      ENDIF

c
c --- FIND EXTNUM if EXTNUM IF NOT SPECIFIED AT THE END OF INFILE ---
c
      IF (extnum.LE.0) THEN
        ninstr = 1
        instr(1) = 'SPECTRUM'
        nsearch = 50
        call fndhdu(chatter,iunit,ninstr,instr,nsearch,nfound,next,
     &              outhdu,outver,extname,status)

c --- CHECK FOR (old-style) EXTNAME if HDUCLASS not found ---

        IF (nfound.LE.0) THEN
          IF (chatter.GE.20) THEN
            errinfo = wrnstr//
     &      'Ext with allowed HDUCLASn keywords not found'
            call fcecho(errinfo)
            errinfo = ' ... searching for EXTNAME= SPECTRUM'
          ENDIF
          call fndext(chatter,iunit,'SPECTRUM',
     &        nsearch,nfound,next,outhdu,outver,extname,status)
        ENDIF

        IF (nfound.GT.1) THEN
          errinfo = errstr//
     &            ' Input file contains >1 SPECTRUM datasets'
          call fcecho(errinfo)
          write(errinfo,'(a,i12,a)')'... ',nfound,' extensions found'
          call fcecho(errinfo)
          do i=1,nfound
            write(errinfo,'(a,i12,a)')'... Ext ',next(i),':'
            call fcecho(errinfo)
            write(errinfo,'(4X,a,a)')'EXTNAME = ',extname(i)
            call fcecho(errinfo)
            do j=1,4
              write(errinfo,'(4X,a,i2,2a)')
     &        'HDUCLAS',j,' = ',outhdu(j,i)
              call fcecho(errinfo)
            enddo
          enddo
          errinfo =
     & '... Extension number must be specified via infile parameter'
          call fcecho(errinfo)
          errinfo = ' ... for example INPUT.PHA[1]'
          call fcecho(errinfo)
          errflg = 1
          return
        ENDIF
      ELSEIF (nfound.LE.0) THEN
        errinfo = ' SPECTRUM extension not found !'
        call fcecho(errinfo)
        errflg = 1
        return
      ENDIF

c --- MOVE TO APPROPRIATE PLACE IN FILE ---

      IF (extnum.LE.0) THEN
        IF (next(1).GT.0) THEN
           imove = next(1)
           status = 0
           call ftmrhd(iunit,imove,htype,status)
           errinfo = wrnstr // ' Problem moving to SPECTRUM xtens'
           IF (status.NE.0) THEN
             call wt_ferrmsg(status, errinfo)
             errflg = 1
             return
           ENDIF
        ENDIF
        extnum = next(1)
      ELSE
        status = 0
        call ftmahd(iunit,extnum+1,htype,status)
        errinfo = errstr//' Problem moving to SPECTRUM extension'
        IF (status.NE.0) THEN
          errflg = 1
          call wt_ferrmsg(status,errinfo)
          return
        ENDIF
      ENDIF
      infile = filename

c READ PHA EXTENSION ...

      call ftgkys(iunit,'PHAVERSN',phaversn,comm,status)
      call crmvlbk(phaversn)
      IF (phaversn.EQ.'1992a') THEN
         errinfo = errstr//
     &'The PHA extension is in OGIP phaversn 1992a format'
         call fcecho(errinfo)
         errinfo = ' Is a conversion needed ?!'
         call fcecho(errinfo)
         errflg = 1
         return
      ENDIF
      call rdsss1(iunit,phsize,nchan,tlscop,instrum,detnam,
     &            filter,object,detchans,texpos,ascale,bscale,
     &            cscale,backfil,corfil,rmffil,arffil,
     &            chantyp,hduclas2,
     &            fchan,channel,dtype,
     &            counts,rcts,qerror,serr,qsys,syserr,qqual,
     &            qualty,pois,numint,expos,mepoc,errflg,chatter)
      IF (errflg.NE.0) THEN
        desc = errstr//' reading PHA file'
        call fcecho(desc)
      ENDIF

c Read details of the time of the observation

      call rdtobs(chatter, iunit, 
     &  date_obs, time_obs, date_end, time_end,
     &  mjdobs,errflg)
      IF (errflg.NE.0) THEN
        desc = errstr//' reading PHA TIME info'
        call fcecho(desc)
      ENDIF

c Read additional header info

      call rdsss_info(iunit,sttime,stptime,majorfrm,minorfrm,
     &                jcgparm,mean_ice,ra,dec,equinox,meanepoc,
     &                chatter,errflg)
      IF (errflg.NE.0) THEN
        desc = errstr//' reading PHA info'
        call fcecho(desc)
      ENDIF

c CLOSE FILE ...

      status = 0 
      call ftclos(iunit,status)
      desc = errstr//' closing PHA file'
      call wt_ferrmsg(status,desc) 
      status = 0
      call ftfiou(iunit,status)
      return
      end
c ----------------------------------------------------------------------
c     END OF ECD_RDAT
c ----------------------------------------------------------------------

          
*+ECD_WT
c     -----------------------------------------------------
      subroutine ecd_wt(infile,outfile,copyall,copyprime,
     &         hduclas2,fchan,
     &         extnum,channel,counts,rcts,
     &         dtype,qerror,serr,qsys,syserr,qqual,
     &         qualty,grping,nchan,detchans,conv,
     &         phsize,instrum,tlscop,detnam,
     &         filter,object,chantyp,
     &         texpos,ascale,bscale,cscale,
     &         backfil,corfil,rmffil,arffil,sttime,stptime,
     &         majorfrm,minorfrm,jcgparm,mean_ice,ra,dec,equinox,
     &         date_obs,time_obs,date_end,time_end,meanepoc,
     &         numint,expos,mepoc,killit,errflg,chatter)    
c     -----------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c 
c This routine writes an OGIP standard SPECTRUM Einstein PHA extension, 
c by calling WTPHA1
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      integer phsize
      character*(*) infile,hduclas2,chantyp,object
      character*(*) outfile,detnam,filter,backfil
      character*(*) corfil,rmffil,arffil
      real texpos,ascale,bscale,cscale
      character(20) instrum, tlscop
      integer chatter, nchan, errflg, dtype,fchan,extnum
      integer channel(phsize), counts(phsize)
      integer qualty(phsize),detchans,grping(phsize)
      integer*2 conv(phsize)
      real rcts(phsize), syserr(phsize), serr(phsize)
      real sttime,stptime,jcgparm,mean_ice
      real ra,dec,equinox,meanepoc,expos(*),mepoc(*)
      character*(*) date_obs,time_obs,date_end,time_end
      integer majorfrm,minorfrm,numint
      logical qerror, qsys, qqual,qgroup
      logical killit,copyall,copyprime
c
c --- LOCALS ---------------------------------------------------------
c
      character(80) desc,errinfo,hist(2),comment(99),record(2)
      character(28) errstr,wrnstr
      integer  status,block,iunit,ounit,nk_hist,nk_comm,j
      integer htype,i,index
      logical history
      double precision tstart,tstop,mjdref,tzero
      real tierrela, tierabso, ontime, deadcorr, vigncorr
      real telapse, livetime,ss
      double precision dss
      integer dd,mon,yy,hh,mm
      character(20) timesys, timeunit, timeref
      character(20) tassign
      logical deadapp,vignapp,clockapp
      double precision mjdobs
      
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Rehana Yusaf (1995 Jan 18) 1.0.0; 
      character(5) version
      parameter (version = '1.0.0')
*-
c -------------------------------------------------------------------
c
c COMMONS
      character(40) taskname
      COMMON/task/taskname

c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        desc = ' ... using ECD_WT Ver '//version//':'
        call fcecho(desc)
      ENDIF
      errstr = ' ERROR :  ECD_WT Ver '//version//':'   
      wrnstr = ' WARNING : ECD_WT Ver '//version//':'

c
c --- OPEN INFILE ---
c
      status = 0
      call ftgiou(iunit,status)
      call ftopen(iunit,infile,0,block,status)
      errinfo = errstr//' opening PHA file'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        errflg = 1
        return
      ENDIF

c
c --- OPEN OUTFILE ---
c
      status = 0
      call ftgiou(ounit,status)
      status = 0
      call opfits(ounit,outfile,killit,chatter,status)
      errinfo = errstr//' opening outfile'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
         errflg = 1
         goto 999 
      ENDIF

c
c --- CREATE/COPY PRIMARY ARRAY ---
c
      status = 0
      history = .false.
      nk_hist = 0
      call copyfirst(iunit,ounit,extnum,copyprime,copyall,
     &               history,nk_hist,record,status)
      IF (status.NE.0) THEN
         errflg = 1
         goto 999
      ENDIF

c
c --- CONVERT QUALITY FLAG TO OGIP STANDARD ---
c
      IF (qqual) THEN
        do i=1,nchan
           IF (qualty(i).EQ.1) THEN
              qualty(i) = 0
           ELSE
              qualty(i) = 5
           ENDIF
        enddo
      ELSE
        do i=1,nchan
           qualty(i) = 0
        enddo
      ENDIF
c
c --- WRITE SPECTRUM EXTENSION ---
c 
      j = 0
      IF (hduclas2(1:4).EQ.'    ') THEN
        call ftupch(object)
        j = index(object,'BCKGRD')
        IF (j.NE.0) THEN
          hduclas2 = 'BKG'
        ELSE
          j = index(object,'BACKGROUND')
          IF (j.NE.0) THEN
            hduclas2 = 'BKG'
          ELSE
            j = index(object,'CORRECTION')
            IF (j.NE.0) THEN
              hduclas2 = 'NET'
            ELSE
              hduclas2 = 'TOTAL'
            ENDIF
          ENDIF
        ENDIF
      ENDIF  
      qgroup = .false.
      nk_hist = 0
      nk_comm = numint * 2
      do i=1,numint
        IF (i.le.9) THEN
          write(comment(i),1000) expos(i),i
        ELSE
          write(comment(i),1050) expos(i),i
        ENDIF
      enddo
      do i=numint+1,numint*2
        IF ((i-numint).LE.9) THEN
          write(comment(i),2000) mepoc(i-numint),i-numint
        ELSE
          write(comment(i),2050) mepoc(i-numint),i-numint
        ENDIF
      enddo

      call wtpha1(ounit,chatter,nk_hist,hist,nk_comm,
     &            comment,tlscop,instrum,detnam,filter,
     &            '1.0.0',hduclas2,fchan,
     &            texpos,ascale,backfil,bscale,corfil,
     &            cscale,rmffil,arffil,detchans,chantyp,
     &            channel,rcts,dtype,
     &            qerror,serr,qsys,
     &            syserr,qqual,qualty,qgroup,
     &            grping,nchan,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr//' problem writing PHA extension'
        call fcecho(errinfo)
        goto 999
      ENDIF

c Insert the source/observation timing info

      call ftpkyj(ounit,'NUMINT',numint,
     &            'Total Number of exposures',errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr//' writing NUMINT keyword'
        IF (chatter.GE.5) THEN
          call fcecho(errinfo)
        ENDIF
        errflg = 0
      ENDIF

      call wttobs(chatter, ounit,
     &      date_obs, time_obs, date_end, time_end,
     &      mjdobs, errflg)

c Insert the serious time info

      mjdref = 43508
      tassign = 'SATELLITE'
      timesys = '1978 1 0 00:00:00'
      timeunit = 'd'
      timeref = 'LOCAL' 

c Calculate tstart in mjd

      call fiptim(time_obs,hh,mm,ss,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr//' parsing time_obs string'
        call fcecho(errinfo)
      ELSE
c        call fidate(date_obs,dd,mon,yy,errflg)
        call fts2dt(date_obs,yy,mon,dd,errflg)
        IF (errflg.NE.0) THEN
          errinfo = errstr//' parsing date_obs string'
          call fcecho(errinfo)
        ELSE
          dss = ss
          call int2mjd(yy,mon,dd,hh,mm,dss,tstart,errflg)
          IF (errflg.NE.0) THEN
           errinfo = errstr//'calculating starting MJD time'
           call fcecho(errinfo)
          ELSE
           tstart = tstart - 40587 
          ENDIF 
        ENDIF 
      ENDIF

c Calculate tstop

      errflg = 0
      call fiptim(time_end,hh,mm,ss,errflg)
      IF (errflg.NE.0) THEN
        errinfo = errstr//' parsing time_end string'
        call fcecho(errinfo)
      ELSE
c        call fidate(date_end,dd,mon,yy,errflg)
        call fts2dt(date_end,yy,mon,dd,errflg)
        IF (errflg.NE.0) THEN
          errinfo = errstr//' parsing date_end string'
          call fcecho(errinfo)     
        ELSE
          dss = ss
          call int2mjd(yy,mon,dd,hh,mm,dss,tstop,errflg)
          IF (errflg.NE.0) THEN
           errinfo = errstr//' calculating stopping MJD time'
           call fcecho(errinfo)
          ELSE
           tstop = tstop - 40587 
          ENDIF
        ENDIF   
      ENDIF
        
      telapse = (tstop - tstart) * 86400

      call wtftim(chatter, ounit,
     &     mjdref, tstart, tstop, tzero,
     &     timesys,timeunit,clockapp, timeref,tassign,
     &     tierrela, tierabso, telapse, ontime, deadapp, deadcorr,
     &     livetime, vignapp, vigncorr, errflg)

      status = 0
      call ftpkys(ounit,'CREATOR',taskname,
     &            's/w task which wrote this dataset',status)
      
c --- MOVE TO APPROPRIATE PLACE IN FILE ---

      status = 0
      call ftmahd(iunit,extnum+1,htype,status)
      errinfo = errstr//' Problem moving to SPECTRUM extension'
      IF (status.NE.0) THEN
        errflg = 1
        call wt_ferrmsg(status,errinfo)
        return
      ENDIF

      IF (copyall) THEN
        call copylast(iunit,ounit,status)
      ENDIF

c CLOSE FILES ...

 999  status = 0 
      call ftclos(iunit,status)
      desc = errstr//' closing infile'
      call wt_ferrmsg(status,desc) 
      status = 0
      call ftclos(ounit,status)
      desc = errstr//' closing outfile'
      status = 0
      call ftfiou(iunit,status)
 1000 FORMAT(F25.9,2X,'EXPOS',I1,' value,length of exposure (sec)')  
 1050 FORMAT(F25.9,2X,'EXPOS',I2,' value,length of exposure (sec)')
 2000 FORMAT(F25.9,2X,'MEPOC',I1,' value,mean epoch of exposure') 
 2050 FORMAT(F25.9,2X,'MEPOC',I2,'value,mean epoch of exposure')
      return
      end
c ----------------------------------------------------------------------
c     END OF ECD_WT
c ----------------------------------------------------------------------

