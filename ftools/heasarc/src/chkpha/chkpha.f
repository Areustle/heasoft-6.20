
*+Phacheck

      subroutine chkpha 

c ---------------------------------------------------------------
c this subroutine checks the input FITS file for XSPEC
c    1. whether the mandatory keywords are present
c    2. also checks for optional keywords which should be
c       useful
c    3. writes to an output log file
c
c input  : input file for XSPEC (e.g. input.pha)
c output : log file (pha.log) containing warnings etc., if any 
c ---------------------------------------------------------------
      implicit none
      character(180) infile,outfile
      integer chatter,errflg

c ------------ author/modifications ----------------------------
c Banashree Mitra Seifert (1997, Feb.) 1.0.0
c Ning Gan (1998 Jun) 1.0.1: 
C       Modified the program to accept the new fits date format. 
c Peter D Wilson (1998 Jul) 1.0.2:
c       Fixed clobber handling
c James Peachey (August 1998)
c       changed name of top-level routine to conform to Ftools standard
c Keith Arnaud (Nov 1998)
c       Fixed a couple of typos in the output
c Alex  Muslimov (Dec 1998)
c       implemented a capability to handle both Type I and Type II 
c       data formats
c ----------------------------------------------------------------
      character(9) taskname
      parameter (taskname='chkpha')
      character(5) version
      parameter (version='1.0.3')
c --------------------- internal variables ------------------------

      character(100) subinfo
      logical killit

c ----------------- variable definitions -----------------
c infile     char  i/p  name of input FITS file
c outfile    char  o/p  output log file
c -------------------------------------------------------------------

      subinfo='using '//taskname//' Ver :'//version
      call wtinfo(chatter,10,2,subinfo)

c ----------------- get parameter file ---------------------------

      call phacheck_gp(infile,outfile,chatter,killit,errflg)

      if (errflg .ne. 0) then
          subinfo = 'getting parameter file '
          call wterrm(taskname,version,subinfo)
          goto 100
      endif

c ---------------- do the checking -------------------------------
     
      call wtbegm(taskname,version,chatter)

      call do_phacheck(infile,outfile,chatter,errflg)
      if (errflg .ne. 0) then
          subinfo = 'returning from do_pha'
          call wterrm(taskname,version,subinfo)
          goto 100
      endif

 100  call wtendm(taskname,version,errflg,chatter)

      end

c -----------------------------------------------------------------
c                   END OF MAIN PHACHECK
c -----------------------------------------------------------------

*+PHACHECK_GP

      subroutine phacheck_gp(infile,outfile,chatter,killit,errflg)

c -----------------------------------------------------------------
c this subroutine gets the input/output file parameters
c -----------------------------------------------------------------

      implicit none
      character*(*) infile,outfile
      integer chatter,errflg
      logical killit

c ----------- authors/modifications ---------------------------------
c Banashree M Seifert (1997, Feb, 1:0:0:)
c Peter D Wilson (1998, Jul, 1.0.1): Add clobber INQUIRE test for log file
c -------------------------------------------------------------------
      character(5)  version
      parameter (version = '1.0.1')
      character(12) subname
*-

c ------------- internals ------------------------------------------
      character(100) subinfo
      integer status
      logical ext 

c ------------------------------------------------------------------
      subname ='phacheck_gp'
      subinfo  = 'using '//subname//' Ver :'//version
      call wtinfo(chatter,10,2,subinfo)

c ----------------- chatter parameter ------------------------------

      status = 0
      call uclgsi('chatter',chatter,status)
      if (status .ne. 0) then
          subinfo = 'getting chatter parameter'
          call wterrm(subname,version,subinfo)
          chatter = 9
          status = 0
      endif

c ----------------- read in clobber -----------------------

      status = 0
      call uclgsb('clobber', killit, status)
      if (status .ne. 0) then
          subinfo = 'getting clobber parameter'
          call wterrm(subname,version,subinfo)
          killit =.false.
          status = 0
      endif

c ----------------- get input PHA file --------------------------

      call uclgst('infile', infile, status)
      if (status .ne. 0) then
          subinfo = 'getting input file parameters !'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      call crmvlbk(infile)
      if (infile .eq. ' ') then
          subinfo = 'input file has to be entered!'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

      ext = .true.
      INQUIRE(FILE=infile, EXIST=ext)
      if ((.NOT. ext) .OR. (infile  .eq.  '  ')) then
          subinfo= 'input file does not exist!'
          call wterrm(subname,version,subinfo)
          subinfo = 'filename : '//infile
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
      endif

c ---------------- outfile name (hidden)----------------------------------------

      call uclgst('outfile', outfile, status)
      if (status .ne. 0) then
          subinfo = 'getting outfile parameter'
          call wterrm(subname,version,subinfo)
      endif
      call crmvlbk(outfile)
      if (outfile(1:1).eq.'!') then
         killit = .true.
         outfile = outfile(2:)
      endif
      INQUIRE(FILE=outfile, EXIST=ext)
      if ( .not.killit .and. ext ) then
         subinfo = 'output file exists but clobber not set'
         call wterrm(subname,version,subinfo)
         errflg = 1
         return
      endif

      errflg = 0
      return
      end

c -----------------------------------------------------------------
c                   END OF GETPAR_PHA
c -----------------------------------------------------------------

*+DO_PHA

      subroutine do_phacheck(infile,outfile,chatter,errflg)

c ------------------------------------------------------------------
c this subroutine do the checking of the mandatory keywords and
c some other optional keywords
c ------------------------------------------------------------------

      implicit none
      character*(*) infile,outfile
      integer chatter,errflg,col_num

c ----------- authors/modifications ---------------------------------
c Banashree M Seifert (1997, Feb 1:0:0:)
c -------------------------------------------------------------------
      character(5)  version
      parameter (version = '1.0.0')
      character(12) subname
*-
c ------------------------- internal variables ---------------------
      character(80) subinfo,subinfo1,subinfo2
      character(180) backfile,corrfile,respfile,ancrfile
      character(80) keyinfo,keyinfo1,keyinfo2,keyinfo3,keyinfo4,keyinfo5
      character(20) comm,object,radecsys
      character(68) date_obs,time_obs
      character(68) date_end,time_end
      character(10) extension,extname,telescop,instrume,filter
      character(8) ttype,ttype1
      character(6) hduclass,hduclas1,hduvers1,chantype
      integer iunit,ounit,block,hdutype,detchans
      integer tlmin,tlmax
      integer nok, ok, ok_op, nok_op, blank
      real exposure,areascal,backscal,corrscal,stat_err,equinox
      logical poisserr,qstat_err
      integer yr, mo, day, istatus

c -------------------------------------------------------------------

      subname ='do_phacheck'
      subinfo  = 'using '//subname//' Ver :'//version
      call wtinfo(chatter,10,2,subinfo)

c --------------- open input(FITS)/output(ascii)  file ----------
      errflg = 0
      call ftgiou(iunit,errflg)
      call ftopen(iunit,infile,0,block,errflg)
      call ftmahd(iunit,2, hdutype,errflg)

      call ftgiou(ounit,errflg)
      open(ounit,file=outfile,status='unknown')
      write(ounit,'(/,1x,a,a)') 
     >     'Log file containing information on: ',infile
      subinfo='               *** Mandatory keywords ***'
      write(ounit,'(1x,a70,/)') subinfo 
      call wtinfo(chatter,10,0,subinfo)

c ===================== MANDATORY KEYWORDS ==============================

      ok = 0
      nok = 0

c --------------------- extension name

      call ftgkys(iunit,'XTENSION',extension,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='XTENSION keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         nok=nok+1
      else
         subinfo='XTENSION keyword is ok'
         call wtinfo(chatter,10,1,subinfo)
         ok=ok+1
      endif

      errflg = 0
      call ftgkys(iunit,'EXTNAME',extname,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='EXTNAME keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         nok=nok+1
      else
         call crmvlbk(extname)
         if(extname(1:8) .eq. 'SPECTRUM') then
            subinfo='EXTNAME  keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='EXTNAME keyword should be SPECTRUM'
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            nok=nok+1
         endif
      endif

c -------------------- telescope name 

      errflg = 0
      call ftgkys(iunit,'TELESCOP',telescop,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='TELESCOP keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         nok=nok+1
      else
         call crmvlbk(telescop)
         if(telescop(1:1) .ne. ' ') then
            subinfo='TELESCOP keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='TELESCOP name is not present '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            nok=nok+1
         endif
      endif

c ---------------- instrument name

      errflg = 0
      call ftgkys(iunit,'INSTRUME',instrume,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='INSTRUME keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         nok=nok+1
      else
         call crmvlbk(instrume)
         if(instrume(1:1) .ne. ' ') then
            subinfo='INSTRUME keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='INSTRUME name is not present '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            nok=nok+1
         endif
      endif

c ---------------------- FILTER 

      keyinfo1=
     > 'FILTER is the instrument filter in use, and if no filter used'
      keyinfo2=
     > 'then it should be FILTER = NONE'
 
      errflg = 0
      call ftgkys(iunit,'FILTER',filter,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='FILTER keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70)') keyinfo1
         write(ounit,'(5x,a70,/)') keyinfo2
         nok=nok+1
      else
         call crmvlbk(filter)
         if(filter(1:1) .ne. ' ') then
            subinfo='FILTER   keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='FILTER name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70)') keyinfo1
            write(ounit,'(5x,a70,/)') keyinfo2
            nok=nok+1
         endif
      endif
c ----------------- exposure time

      keyinfo1=
     > 'EXPOSURE is integration time in seconds for the PHA data,'
      keyinfo2=
     > 'assumed to be corrected for deadtime, data dropouts, etc. '

      errflg = 0
      call ftgkye(iunit,'EXPOSURE',exposure,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='EXPOSURE keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70)') keyinfo1
         write(ounit,'(5x,a70,/)') keyinfo2
         nok=nok+1
      else
         if(exposure .gt. 0.0) then
            subinfo='EXPOSURE keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='EXPOSURE keyword needs to be > 0.0'
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70)') keyinfo1
            write(ounit,'(5x,a70,/)') keyinfo2
            nok=nok+1
         endif 
      endif

c ------------------- areascal

      keyinfo1=
     > 'AREASCAL gives the scaling factor of the PHA data. In most'
      keyinfo2=
     > 'cases AREASCAL=1.0 since the instrumental effective area'
      keyinfo3=
     > 'is provided within the spectral calibration files.'

      errflg = 0
      call ftgkye(iunit,'AREASCAL',areascal,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='AREASCAL keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70)') keyinfo1
         write(ounit,'(5x,a70)') keyinfo2
         write(ounit,'(5x,a70)') keyinfo3
         write(ounit,'(5x,a70,/)') keyinfo4
         nok=nok+1
      else
         if(areascal .gt. 0.0) then
            subinfo='AREASCAL keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='AREASCAL keyword needs to be > 0.0'
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a70)') 'COMMENT :'
            write(ounit,'(5x,a70)') keyinfo1
            write(ounit,'(5x,a70)') keyinfo2
            write(ounit,'(5x,a70)') keyinfo3
            write(ounit,'(5x,a70,/)') keyinfo4
            nok=nok+1
         endif
      endif

c ------------------- backfile

      keyinfo1=
     > 'BACKFILE is the name of the background, if any.' 

      errflg = 0
      call ftgkys(iunit,'BACKFILE',backfile,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='BACKFILE keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70,/)') keyinfo1
         nok=nok+1
      else
         call crmvlbk(backfile)
         if(backfile(1:1) .ne. ' ') then
            subinfo='BACKFILE keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='BACKFILE name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70,/)') keyinfo1
            nok=nok+1
         endif
      endif

c ------------------- backscal

      keyinfo1=
     > 'BACKSCAL is the background scaling factor'

      errflg = 0
      call ftgkye(iunit,'BACKSCAL',backscal,comm,errflg) 
      if(errflg .ne. 0) then
         subinfo='BACKSCAL keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70,/)') keyinfo1
         nok=nok+1
      else
         if(backscal .gt. 0.0) then
            subinfo='BACKSCAL keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='BACKSCAL keyword needs to be > 0.0'
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70,/)') keyinfo1
            nok=nok+1
         endif
      endif
   
c ------------------- corrscal

      keyinfo1=
     > 'CORRSCAL is the correction scaling factor'

      errflg = 0
      call ftgkye(iunit,'CORRSCAL',corrscal,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='CORRSCAL keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70,/)') keyinfo1
         nok=nok+1
      else
         subinfo='CORRSCAL keyword is ok'
         call wtinfo(chatter,10,1,subinfo)
         ok=ok+1
      endif

c ------------------- corrfile

      keyinfo1=
     > 'CORRFILE is the name of the correction file' 

      errflg = 0
      call ftgkys(iunit,'CORRFILE',corrfile,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='CORRFILE keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70,/)') keyinfo1
         nok=nok+1
      else
         call crmvlbk(corrfile)
         if(corrfile(1:1) .ne. ' ') then
            subinfo='CORRFILE keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='CORRFILE name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70,/)') keyinfo1
            nok=nok+1
         endif
      endif

   
c ------------------- respfile

      keyinfo1=
     > 'RESPFILE is the name of the redistribution matrix file (RMF)'

      errflg = 0
      call ftgkys(iunit,'RESPFILE',respfile,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='RESPFILE keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70,/)') keyinfo1
         nok=nok+1
      else
         call crmvlbk(respfile)
         if(respfile(1:1) .ne. ' ') then
            subinfo='RESPFILE keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='RESPFILE name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70,/)') keyinfo1
            nok=nok+1
         endif
      endif

c ------------------- ancrfile

      keyinfo1=
     > 'ANCRFILE is the name of the ancillary response file '
     >         //'(ARF)'

      errflg = 0
      call ftgkys(iunit,'ANCRFILE',ancrfile,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='ANCRFILE keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70,/)') keyinfo1
         nok=nok+1
      else
         call crmvlbk(ancrfile)
         if(ancrfile(1:1) .ne. ' ') then
            subinfo='ANCRFILE keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='ANCRFILE name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70,/)') keyinfo1
            nok=nok+1
         endif
      endif
   
c ------------------- hduclass

      keyinfo1=
     > 'HDUCLASS should contain the string OGIP to indicate that'
      keyinfo2=
     > 'this is an OGIP style file'

      errflg = 0
      call ftgkys(iunit,'HDUCLASS',hduclass,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='HDUCLASS keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70)') keyinfo1
         write(ounit,'(5x,a70,/)') keyinfo2
         nok=nok+1
      else
         call crmvlbk(hduclass)
         if(hduclass(1:1) .ne. ' ') then
            subinfo='HDUCLASS keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='HDUCLASS name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70)') keyinfo1
            write(ounit,'(5x,a70,/)') keyinfo2
            nok=nok+1
         endif
      endif
   
c ------------------- hduclas1

      keyinfo1=
     > 'HDUCLAS1 should contain the string SPECTRUM to indicate'
      keyinfo2=
     > 'that this file has spectrum data'

      errflg = 0
      call ftgkys(iunit,'HDUCLAS1',hduclas1,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='HDUCLAS1 keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70)') keyinfo1
         write(ounit,'(5x,a70,/)') keyinfo2
         nok=nok+1
      else
         call crmvlbk(hduclas1)
         if(hduclas1(1:1) .ne. ' ') then
            subinfo='HDUCLAS1 keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='HDUCLAS1 name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70)') keyinfo1
            write(ounit,'(5x,a70,/)') keyinfo2
            nok=nok+1
         endif
      endif
   
c ------------------- hduvers1 

      keyinfo1=
     > 'HDUVERS1 is the version no. of the format'

      errflg = 0
      call ftgkys(iunit,'HDUVERS1',hduvers1,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='HDUVERS1 keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70,/)') keyinfo1
         nok=nok+1
      else
         call crmvlbk(hduvers1)
         if(hduvers1(1:1) .ne. ' ') then
            subinfo='HDUVERS1 keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='HDUVERS1 name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70,/)') keyinfo1
            nok=nok+1
         endif
      endif

c ------------------- chantype 

      keyinfo1=
     > 'CHANTYPE keyword specifies whether the channels referred to'
      keyinfo2=
     > 'are assigned by the detector electronics at the time of data'
      keyinfo3=
     > 'collection (CHANTYPE=PHA), Or, '
      keyinfo4=
     > 'whether any corrections have been applied. Such as, re-mapped'
      keyinfo5=
     > 'onto a standard pulse-invariant channel grid (CHANTYPE=PI)' 

      errflg = 0
      call ftgkys(iunit,'CHANTYPE',chantype,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='CHANTYPE keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70)') keyinfo1
         write(ounit,'(5x,a70)') keyinfo2
         write(ounit,'(5x,a70)') keyinfo3
         write(ounit,'(5x,a70)') keyinfo4
         write(ounit,'(5x,a70,/)') keyinfo5
         nok=nok+1
      else
         call crmvlbk(chantype)
         if(chantype(1:1) .ne. ' ') then
            subinfo='CHANTYPE keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='CHANTYPE name is blank '
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70)') keyinfo1
            write(ounit,'(5x,a70)') keyinfo2
            write(ounit,'(5x,a70)') keyinfo3
            write(ounit,'(5x,a70)') keyinfo4
            write(ounit,'(5x,a70,/)') keyinfo5
            nok=nok+1
         endif
      endif

c ------------------- poisserr & stat_err

      keyinfo1=
     > 'POISSERR keyword defines if Poissonian errors are appropriate' 
      keyinfo2=
     > 'to the data. If POISSERR = .false., then STAT_ERR keyword or'
      keyinfo3=
     > 'column should be present. If both POISSERR & STAT_ERR present,' 
      keyinfo4=
     > 'then to be warned that STAT_ERR will be ignored by XSPEC.'
       

      errflg = 0
      poisserr=.false.
      call ftgkyl(iunit,'POISSERR',poisserr,comm,errflg)
      if(errflg .ne. 0) then
         poisserr=.false.
      endif

      errflg=0
      stat_err=0.0
      call ftgkye(iunit,'STAT_ERR',stat_err,comm,errflg)

      if(errflg .ne. 0) then
         errflg=0

         call ftgcno(iunit,.false.,'STAT_ERR',col_num,errflg)
        
          if(errflg .ne. 0) then
             qstat_err=.false.
          else 
             qstat_err=.true.
          endif

         else   
           if(stat_err .eq. 0.) then
              qstat_err=.false.
           else
              qstat_err=.true.  
           endif
      endif

      if(poisserr) then
         if(qstat_err) then

            subinfo='Warning for error calculation: '
            call wtinfo(chatter,10,0,subinfo)
            write(ounit,'(1x,a70)') subinfo

            subinfo=' >>  Both POISSERR & STAT_ERR are present'
            call wtinfo(chatter,10,0,subinfo)
            write(ounit,'(1x,a70)') subinfo

            subinfo=' >>  XSPEC will calculate the POISSERR and '
     >              //'STAT_ERR will be ignored'
            call wtinfo(chatter,10,0,subinfo)
            write(ounit,'(1x,a70)') subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70)') keyinfo1
            write(ounit,'(5x,a70)') keyinfo2
            write(ounit,'(5x,a70)') keyinfo3
            write(ounit,'(5x,a70,/)') keyinfo4
            ok=ok+1 
         else
            subinfo='POISSERR & STAT_ERR keywords ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1 
         endif
      else

         if(qstat_err) then
            subinfo='POISSERR & STAT_ERR keywords OK'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1 
         else
            subinfo='ERROR: Both POISSERR & STAT_ERR absent'
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70)') keyinfo1
            write(ounit,'(5x,a70)') keyinfo2
            write(ounit,'(5x,a70)') keyinfo3
            write(ounit,'(5x,a70,/)') keyinfo4
            nok=nok+1 
         endif

      endif

c ------------------- counts or rate

      errflg = 0

      call ftgkys(iunit,'TTYPE1',ttype1,comm,errflg)

      if (ttype1(1:8).eq.'SPEC_NUM') then

         call ftgkys(iunit,'TTYPE3',ttype,comm,errflg)
         keyinfo='TTYPE3 defines either counts or counts/s'
         subinfo1='TTYPE3 & POISSERR ok'
         subinfo2='TTYPE3 & STAT_ERR ok'
             if(errflg.ne.0) then
                subinfo='TTYPE3 keyword is absent'
                call wtinfo(chatter,10,1,subinfo)
                write(ounit,'(1x,a,a70)') '... ',subinfo
                write(ounit,'(5x,a)') 'COMMENT :'
                write(ounit,'(5x,a70,/)') keyinfo
                nok=nok+1
              endif

      else

         call ftgkys(iunit,'TTYPE2',ttype,comm,errflg)
         keyinfo='TTYPE2 defines either counts or counts/s'
         subinfo1='TTYPE2 & POISSERR ok'
         subinfo2='TTYPE2 & STAT_ERR ok'
              if(errflg.ne.0) then
                 subinfo='TTYPE2 keyword is absent'
                 call wtinfo(chatter,10,1,subinfo)
                 write(ounit,'(1x,a,a70)') '... ',subinfo
                 write(ounit,'(5x,a)') 'COMMENT :'
                 write(ounit,'(5x,a70,/)') keyinfo
                 nok=nok+1
               endif
      endif
     

      if(errflg .eq. 0) then

         if(ttype(1:6) .eq. 'COUNTS') then
            if(poisserr) then
               subinfo=subinfo1
               call wtinfo(chatter,10,1,subinfo)
               ok=ok+1
            else
               if(qstat_err) then
                  subinfo=subinfo2
                  call wtinfo(chatter,10,1,subinfo)
                  ok=ok+1
               else
                  subinfo='Error: COUNTS & POISSERR'
                  call wtinfo(chatter,10,1,subinfo)
                  write(ounit,'(1x,a,a70)') '... ',subinfo
                  subinfo='In case of COUNTS, and POISSERR=false'
     >                    //' STAT_ERR should be present'
                  call wtinfo(chatter,10,1,subinfo)
                  write(ounit,'(1x,a,a70)') '... ',subinfo
                  nok=nok+1
               endif
            endif                  

         elseif (ttype(1:4) .eq. 'RATE') then
                 if(qstat_err) then
                    subinfo=subinfo2
                    call wtinfo(chatter,10,1,subinfo)
                    ok=ok+1
                 else
                    subinfo='Error: datatype & stat_err:'
                    call wtinfo(chatter,10,1,subinfo)
                    write(ounit,'(1x,a,a70)') '... ',subinfo

                    subinfo='For datatype= RATE, STAT_ERR is mandatory'
                    call wtinfo(chatter,10,1,subinfo)
                    write(ounit,'(1x,a,a70)') '... ',subinfo
                    write(ounit,'(5x,a)') 'COMMENT :'
                    write(ounit,'(5x,a70,/)') keyinfo
                    nok=nok+1
                 endif
                
                 if(poisserr) then
                    subinfo='Error: datatype & poisserr:'
                    call wtinfo(chatter,10,1,subinfo)
                    write(ounit,'(1x,a,a70)') '... ',subinfo

                    subinfo='For datatype= RATE, POISSERR should be'
     >                      //' false'
                    call wtinfo(chatter,10,1,subinfo)
                    write(ounit,'(1x,a,a70)') '... ',subinfo
                    write(ounit,'(5x,a)') 'COMMENT :'
                    write(ounit,'(5x,a70,/)') keyinfo
                    nok=nok+1
                 endif

         else
             subinfo='unrecognised data type: '//ttype
             call wtinfo(chatter,10,1,subinfo)
             write(ounit,'(1x,a,a70)') '... ',subinfo
             write(ounit,'(5x,a)') 'COMMENT :'
             write(ounit,'(5x,a70,/)') keyinfo
             nok=nok+1
         endif
      endif


c ------------------- detchans 

      keyinfo1=
     > 'DETCHANS gives the total number of possible detector channels'

      errflg = 0

      call ftgkyj(iunit,'DETCHANS',detchans,comm,errflg)

      if(errflg .ne. 0) then
         subinfo='DETCHANS keyword is not present'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         write(ounit,'(5x,a)') 'COMMENT :'
         write(ounit,'(5x,a70,/)') keyinfo1
         nok=nok+1
      else
         if(detchans .gt. 0) then
            subinfo='DETCHANS keyword is ok'
            call wtinfo(chatter,10,1,subinfo)
            ok=ok+1
         else
            subinfo='DETCHANS keyword needs to be > 0'
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            write(ounit,'(5x,a)') 'COMMENT :'
            write(ounit,'(5x,a70,/)') keyinfo1
            nok=nok+1
         endif
      endif

      if(detchans .gt. 0) then
         errflg = 0
         
         if(ttype1(1:8).eq.'SPEC_NUM') then 
            call ftgkyj(iunit,'TLMIN2',tlmin,comm,errflg)
            subinfo1='WARNING: TLMIN2 keyword is absent'
            subinfo2='DETCHANS & TLMIN2 keywords ok'
         else
            call ftgkyj(iunit,'TLMIN1',tlmin,comm,errflg)
            subinfo1='WARNING: TLMIN1 keyword is absent'
            subinfo2='DETCHANS & TLMIN1 keywords ok'
         endif   


         if(errflg .ne. 0) then
            subinfo=subinfo1
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            ok = ok+1
         else
            subinfo=subinfo2
            call wtinfo(chatter,10,1,subinfo)
            ok = ok+1
         endif

         errflg = 0

         
         if(ttype1(1:8).eq.'SPEC_NUM') then 
            call ftgkyj(iunit,'TLMAX2',tlmax,comm,errflg)
            subinfo1='WARNING: TLMAX2 keyword is absent'
            subinfo2='DETCHANS & TLMAX2 keywords ok'
         else
            call ftgkyj(iunit,'TLMAX1',tlmax,comm,errflg)
            subinfo1='WARNING: TLMAX1 keyword is absent'
            subinfo2='DETCHANS & TLMAX1 keywords ok'
         endif   


         if(errflg .ne. 0) then
            subinfo=subinfo1
            call wtinfo(chatter,10,1,subinfo)
            write(ounit,'(1x,a,a70)') '... ',subinfo
            ok = ok+1
         else
            subinfo=subinfo2
            call wtinfo(chatter,10,1,subinfo)
            ok = ok+1
         endif
      endif
           
c ----------------- statistics of OK ness 
      subinfo='Mandatory keyword checked are:'
      call wtinfo(chatter,0,0,subinfo)
      write(ounit,'(/,1x,a70)') subinfo
      subinfo='EXTNAME, TELESCOP, INSTRUME, FILTER, EXPOSURE, AREASCAL,'
      keyinfo1='BACKFILE, BACKSCAL, CORRFILE, CORRSCAL, RESPFILE, '
      keyinfo2='ANCRFILE, HDUCLASS, HDUCLAS1, HDUVERS1, CHANTYPE, '
      keyinfo3='DETCHANS, ERROR (POISSERR/STAT_ERR)'
      call wtinfo(chatter,0,0,subinfo)
      call wtinfo(chatter,0,0,keyinfo1)
      call wtinfo(chatter,0,0,keyinfo2)
      call wtinfo(chatter,0,0,keyinfo3)
      write(ounit,'(1x,a70)') subinfo
      write(ounit,'(1x,a70)') keyinfo1
      write(ounit,'(1x,a70)') keyinfo2
      write(ounit,'(1x,a70)') keyinfo3

      if(nok .eq. 0) then
         subinfo='All Mandatory keywords are ok'
         call wtinfo(chatter,0,0,subinfo)
         write(ounit,'(/,1x,a70)') subinfo
      
      else
         write(subinfo,'(a,i3)')
     >        'Number of Mandatory keywords present = ', ok
         call wtinfo(chatter,0,0,subinfo)
         write(ounit,'(1x,a70)') subinfo

         write(subinfo,'(a,i3)') 
     >        'Number of Mandatory keywords missing = ', nok
         call wtinfo(chatter,0,0,subinfo)
         write(ounit,'(/,1x,a70)') subinfo

      endif
 
c ===================== OPTIONAL KEYWORDS ==============================
c Optional keywords are suggested, as they supply further detailed 
c informations regarding data sets
c ---------------------------------------------------------------------

      subinfo='               *** Optional keywords ***'
      write(ounit,'(/,1x,a70)') subinfo 
      call wtinfo(chatter,10,0,subinfo)

      subinfo='----------------------------------------------------'
      write(ounit,'(/,6x,a70)') subinfo 
      subinfo='Optional keywords are suggested, as they supply'
      write(ounit,'(8x,a70)') subinfo 
      subinfo='further detailed informations regarding data sets'
      write(ounit,'(8x,a70)') subinfo 
      subinfo='----------------------------------------------------'
      write(ounit,'(6x,a70,/)') subinfo 

c ------------------- object name

      ok_op=0
      nok_op=0

      errflg=0
      call ftgkys(iunit,'OBJECT',object,comm,errflg)
      if(errflg .ne. 0) then
         subinfo='OBJECT name is not available'
         call wtinfo(chatter,10,1,subinfo)
         write(ounit,'(1x,a,a70)') '... ',subinfo
         nok_op = nok_op+1
      else
         if(object(1:2) .ne. '  ') then
            subinfo='OBJECT name = '//object
            call wtinfo(chatter,10,1,subinfo)
            ok_op = ok_op+1
         else
            subinfo='OBJECT name is blank'
            call wtinfo(chatter,10,1,subinfo)
            nok_op = nok_op+1
         endif
       endif

c ------------------- equinox

       errflg=0
       call ftgkye(iunit,'EQUINOX',equinox,comm,errflg)
       if(errflg .ne. 0) then
          subinfo='EQUINOX of the celestial coordinate is not specified'
          call wtinfo(chatter,10,1,subinfo)
          write(ounit,'(1x,a,a70)') '... ',subinfo
          nok_op = nok_op+1
       else
          ok_op = ok_op+1
       endif

c ------------------- radecsys

       errflg=0
       call ftgkys(iunit,'RADECSYS',radecsys,comm,errflg)
       if(errflg .ne. 0) then
          subinfo='RADECSYS (the coordinate frame used for EQUINOX)'
     >             //' is not specified'
          call wtinfo(chatter,10,1,subinfo)
          write(ounit,'(1x,a,a70)') '... ',subinfo
          nok_op = nok_op+1
       else
          ok_op = ok_op+1
       endif
 
c ------------------- date-obs

       errflg=0
       call ftgkys(iunit,'DATE-OBS',date_obs,comm,errflg)
       if(errflg .ne. 0) then
          subinfo='DATE-OBS, U.T. date observations started is not'
     >             //' available'
          call wtinfo(chatter,10,1,subinfo)
          write(ounit,'(1x,a,a70)') '... ',subinfo
          nok_op = nok_op+1

       else
	  day = 0
	  istatus = 0
	  call fts2dt(date_obs,yr,mo,day,istatus)
	  if(day.gt.0.and. day.le.31.and.istatus.eq.0) then
C          if((date_obs(:1) .eq. '0') .or. (date_obs(:1) .eq. '1') .or.
C     >       (date_obs(:1) .eq. '2') .or. (date_obs(:1) .eq. '3')) then
C
               subinfo= 'DATE-OBS is ok'
               call wtinfo(chatter,10,1,subinfo)
               ok_op = ok_op+1
          else
	      istatus = 0
              blank=index(date_obs,' ')
              subinfo= 'DATE-OBS '//''''//date_obs(1:blank-1)//''''
     >                //' is not a proper date'
       
              call wtinfo(chatter,10,1,subinfo)
              write(ounit,'(1x,a,a70)') '... ',subinfo
              nok_op = nok_op+1
          endif
     
       endif

c ------------------- time-obs

       errflg=0
       call ftgkys(iunit,'TIME-OBS',time_obs,comm,errflg)
       if(errflg .ne. 0) then
          subinfo='TIME-OBS, U.T. time observations started is not'
     >             //' available'
          call wtinfo(chatter,10,1,subinfo)
          write(ounit,'(1x,a,a70)') '... ',subinfo
          nok_op = nok_op+1

       else

          if((time_obs(:1) .eq. '0') .or. (time_obs(:1) .eq. '1') .or.
     >       (time_obs(:1) .eq. '2') .or. (time_obs(:1) .eq. '3') .or.
     >       (time_obs(:1) .eq. '4') .or. (time_obs(:1) .eq. '5')) then

             subinfo= 'TIME-OBS is ok'
             call wtinfo(chatter,10,1,subinfo)
             ok_op = ok_op+1
          else
             blank=index(time_obs,' ')
             subinfo= 'TIME-OBS '//''''//time_obs(1:blank-1)//''''
     >                //' is not a proper time'
             call wtinfo(chatter,10,1,subinfo)
             write(ounit,'(1x,a,a70)') '... ',subinfo
             nok_op = nok_op+1
          endif

       endif
   
c ------------------- date-end

       errflg=0
       call ftgkys(iunit,'DATE-END',date_end,comm,errflg)
       if(errflg .ne. 0) then
          subinfo='DATE-END, U.T. date observations ended is not'
     >             //' available'
          call wtinfo(chatter,10,1,subinfo)
          write(ounit,'(1x,a,a70)') '... ',subinfo
          nok_op = nok_op+1

       else
	  day = 0
	  istatus = 0
	  call fts2dt(date_obs,yr,mo,day,istatus)
	  if(day.gt.0.and. day.le.31.and.istatus.eq.0) then

C         if((date_end(:1) .eq. '0') .or. (date_end(:1) .eq. '1') .or.
C     >       (date_end(:1) .eq. '2') .or. (date_end(:1) .eq. '3')) then
C
              subinfo= 'DATE-END is ok'
              call wtinfo(chatter,10,1,subinfo)
             ok_op = ok_op+1
          else
	     istatus = 0
             blank=index(date_end,' ')
             subinfo= 'DATE-END '//''''//date_end(1:blank-1)//''''
     >                //' is not a proper date'
             call wtinfo(chatter,10,1,subinfo)
             write(ounit,'(1x,a,a70)') '... ',subinfo
             nok_op = nok_op+1
          endif

       endif
c ------------------- time-end

       errflg=0
       call ftgkys(iunit,'TIME-END',time_end,comm,errflg)
       if(errflg .ne. 0) then
          subinfo='TIME-END, U.T. time observations ended is not'
     >             //' available'
          call wtinfo(chatter,10,1,subinfo)
          write(ounit,'(1x,a,a70)') '... ',subinfo
          nok_op = nok_op+1

       else

          if((time_end(:1) .eq. '0') .or. (time_end(:1) .eq. '1') .or.
     >       (time_end(:1) .eq. '2') .or. (time_end(:1) .eq. '3') .or.
     >       (time_end(:1) .eq. '4') .or. (time_end(:1) .eq. '5')) then

             subinfo= 'TIME-END is ok'
             call wtinfo(chatter,10,1,subinfo)
             ok_op = ok_op+1
          else
             blank=index(time_end,' ')
             subinfo= 'TIME-END '//''''//time_end(1:blank-1)//''''
     >                //' is not a proper time'
             call wtinfo(chatter,10,1,subinfo)
             write(ounit,'(1x,a,a70)') '... ',subinfo
             nok_op = nok_op+1
          endif
    
       endif
   
c ----------------- statistics of OK ness 
      subinfo='Optional keyword checked are:'
      call wtinfo(chatter,0,0,subinfo)
      write(ounit,'(/,1x,a70)') subinfo
      subinfo='OBJECT, EQUINOX, RADECSYS, DATE-OBS, TIME-OBS'
      keyinfo1='DATE-END, TIME-END '
      call wtinfo(chatter,0,0,subinfo)
      call wtinfo(chatter,0,0,keyinfo1)
      write(ounit,'(1x,a70)') subinfo
      write(ounit,'(1x,a70)') keyinfo1

      if(nok_op .eq. 0) then
         subinfo='All Optional  keywords are ok'
         call wtinfo(chatter,0,0,subinfo)
         write(ounit,'(/,1x,a70)') subinfo
      
      else
         write(subinfo,'(a,i3)')
     >        'Number of Optional  keywords present = ', ok_op
         call wtinfo(chatter,0,0,subinfo)
         write(ounit,'(1x,a70)') subinfo

         write(subinfo,'(a,i3)') 
     >        'Number of Optional  keywords missing = ', nok_op
         call wtinfo(chatter,0,0,subinfo)
         write(ounit,'(/,1x,a70)') subinfo

      endif
 
c --------------------------------------------------------------  
c leave an extra blank line at the end of output file

      write(ounit,*) 
      close(ounit)

      errflg=0
      call ftclos(iunit,errflg)
      call ftfiou(iunit,errflg)
      call ftfiou(ounit,errflg)

      errflg=0
      return
      end

c -------------------------------------------------------------------
c               END OF DO_PHA
c -------------------------------------------------------------------
 


