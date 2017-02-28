*+GCORRMF
c     -----------------
      subroutine gcorrf
c     -----------------
c --- DESCRIPTION ------------------------------------------------------
c This task shifts the channels of an OGIP standard RMF file, 
c by a specified amount.
c ----------------------------------------------------------------------
c --- VARIABLES --------------------------------------------------------
c  
      IMPLICIT NONE
      character(180) infile,outfile,ebdfile,shftfile
      integer errflg, chatter
      logical killit
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c infile     char   : input RMF filename
c ebdfile    char   : input EBD filename - usually same as RMF filename
c outfile    char   : output filename
c shftfile   char   : input shift filename 
c
c --- AUTHORS/MODIFICATION HISTORY -------------------------------------
c
c Rehana Yusaf (1.0.0: August 1995)
c
c Banashree M Seifert (1.1.0: July 1996)
c           . bug fixed -- DMA allocation for N_chan was not done 
c                          but was declared as integer p_N_chan
c 
c Banashree M Seifert (1.2.0: Aug, 1996)
c           . fractional shift introduced
c           . replaced by screen display routines
c Banashree M Seifert (1.3.0: Oct 1996)
c           . replaced by rdebd3,rdrmf3 in place of rdebd1,rdrmf1
c           . filename character(180)
c
c Banashree M Seifert (1.4.0: Nov21 1996)
c           . replaced by call to grprmf in place of call to grp_rmf 
c              change in grprmf contains passing parameter first channel
c              no. (0 or 1)
c
c Banashree M Seifert (1.4.1: Mar 25, 1997)
c           . clobber parameter was not active
c
c Peter D Wilson (1.4.2: Jul 01, 1998)
c           . Updated for new FCPARS behavior
c Ning Gan (1.4.3: Nov 01, 1999)
c           . Fixed bugs in gcorr_rmf. Now the output file will 
c             have the same group structure as the input file. 
c ------------------------------------------------------------------------
      character(5) version
      parameter (version = '1.4.3')
      character(8) taskname
      taskname ='gcorrmf'
*-
c ----------------------------------------------------------------------
c
c --- GET PARAMETERS ---
c
      errflg = 0
      call gcorr_gp(infile,ebdfile,outfile,shftfile,
     &             killit,chatter,errflg)
      IF (errflg.NE.0) THEN
          call wterrm(taskname,version,'returning from gcorr_gp')
          goto 100
      ENDIF

      call wtbegm(taskname,version,chatter)
      
c --- COMPRESS RMF DATA ---

      call gcorr_shft(infile,ebdfile,outfile,shftfile,
     &               version,killit,errflg,chatter)
      IF (errflg.NE.0) THEN
          call wterrm(taskname,version,'returning from gcorr_shft')
      ENDIF

 100  call wtendm(taskname,version,errflg,chatter)
      return
      end
c ----------------------------------------------------------------
c     END OF GCORRMF
c ----------------------------------------------------------------


*+GCORR_GP
c     ---------------------------------------------------------
      subroutine gcorr_gp(infile,ebdfile,outfile,shftfile,
     &                   killit,chatter,errflg)
c     ---------------------------------------------------------
c --- DESCRIPTION ------------------------------------------------------
c     Gets parameters.
c --- VARIABLES --------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile, shftfile,ebdfile
      character(80) ebdfilename, rmffilename
      character(70) desc,defval
      integer errflg,chatter
      integer status,rmfextno,ebdextno
      integer flen, fcstln
      logical killit, ext
c
c --- VARIABLE DIRECTORY -----------------------------------------------
c
c Arguments ...
c
c infile     char   : input file name
c ebdfile    char   : ebdfile input file, usually same as infile
c outfile    char   : Output filename
c nchan      int    : final number of channels, entered directly/from pha
c chatter    int    : Chattiness flag, >20 verbose
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
c Rehana Yusaf (1995 August 9) 1.0.0;
c
c Banashree M Seifert (1996 Nov20) 1.1.0:
c        . substituted screen display routine, e.g., wterrm etc
c Banashree M Seifert (1997 Mar25) 1.2.0:
c        . clobber parameter was not checking if clobber=yes is given 
c          on command line. 
c Peter D Wilson (1998 Jul 01) 1.2.1:
c        . Drop INQUIRE tests for input files. Let FTOPEN check
c -------------------------------------------------------------

      character(9) subname
      parameter (subname='gcorr_gp')
      character(5) version 
      parameter (version = '1.2.1')
c -------------------------------------------------------------

c GET CLOBBER

      status = 0
      call uclgsb('clobber',killit,status)
      IF (status.NE.0) THEN
        desc = 'getting clobber parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

c GET CHATTER 

      status = 0
      call uclgsi('chatter',chatter,status)
      IF (status.NE.0) THEN
        desc = 'getting chatter parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF        

c GET INFILE

      status = 0
      call uclgst('infile',infile,status)
      IF (status.NE.0) THEN
        desc = 'getting infile parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(infile)
      IF (infile(1:2) .EQ.'  ') THEN
         desc = 'input file has to be entered !'
         call wterrm(subname,version,desc)
         errflg = 1
         return
      ENDIF
C PDW 7/1/98: Don't bother! Let FTOPEN determine if file exists
C             Strip off extension with ftrtnm, for later
      status = 0
      call ftrtnm( infile, rmffilename, status )
C      call fcpars(infile,rmffilename,rmfextno,status)
C      ext = .true.
C      flen = fcstln(rmffilename)
C      INQUIRE(FILE=rmffilename(:flen),EXIST=ext)
C      IF (.NOT.ext) THEN
C        errflg = 1
C        desc = 'File does not exist :'//rmffilename
C        call wterrm(subname,version,desc)
C        return
C      ENDIF

c GET EBDFILE 

      status = 0
      call uclgst('ebdfile',ebdfile,status)
      IF (status.NE.0) THEN
        desc = 'getting ebdfile parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF

      call crmvlbk(ebdfile)
      IF (ebdfile.EQ.'%') THEN
        ebdfile = rmffilename
C PDW 7/1/98: Don't bother! Let FTOPEN determine if file exists
C      ELSE
C        call fcpars(ebdfile,ebdfilename,ebdextno,status)
C        IF (ebdfilename.NE.rmffilename) THEN
C          ext = .true.
C          flen = fcstln(ebdfilename)
C          INQUIRE(FILE=ebdfilename(:flen),EXIST=ext)
C          IF (.NOT.ext) THEN
C            errflg = 1
C            desc = 'File does not exist :'//ebdfilename  
C            call wterrm(subname,version,desc)
C            return
C          ENDIF
C        ENDIF 
      ENDIF

c PUT DEFAULT EBDFILE 

      defval = '%'
      status = 0
      call uclpst('ebdfile',defval,status)
      IF (status.NE.0) THEN
        desc = 'putting default ebdfile parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF           

c GET SHFTFILE

      status = 0
      call uclgst('shftfile',shftfile,status)
      IF (status.NE.0) THEN
        desc = 'getting shftfile parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF
      ext=.true.
      call crmvlbk(shftfile)
      INQUIRE(FILE=shftfile,EXIST=ext)
      IF (.NOT.ext) THEN
        errflg = 1
        desc = 'File does not exist :'//shftfile
        call wterrm(subname,version,desc)
        return
      ENDIF
           
c GET OUTFILE 

      status = 0
      call uclgst('outfile',outfile,status)
      IF (status.NE.0) THEN
        desc = 'getting outfile parameter !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF
      call crmvlbk(outfile)
      IF (outfile.EQ.' ') THEN
        desc='outfile must be entered !'
        call wterrm(subname,version,desc)
        errflg = 1
        return
      ENDIF
      ext=.false.
      INQUIRE(FILE=outfile,EXIST=ext)
      IF (ext) THEN
        if(.not. killit ) then

           errflg = 1
           desc = 'File exists :'//outfile
           call wterrm(subname,version,desc)
           desc = 'try with !'//outfile
           call wtinfo(chatter,0,2,desc)
           return
        endif

      ENDIF
           

      return
      end
c ---------------------------------------------------------------------
c     END OF GCORR_GP
c ---------------------------------------------------------------------


*+GCORR_SHFT
c     -----------------------------------------------------------------
      subroutine gcorr_shft(infile,ebdfile,outfile,shftfile,
     &                     mnver,killit,errflg,chatter)
c     -----------------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c This routine shifts the channels, in the EBOUNDS, and SPECRESP
c extensions of a user defined RMF file, and writes the rebinned
c data to an output file.
c 
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile, mnver,shftfile,ebdfile
      integer errflg, chatter
      logical killit
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c Arguments ...
c
c infile     char   : input filename
c outfile    char   : output filename
c phatlscop  char   : Telescope name, read from Phafile
c phainstrum char   : Instrument name, read from phafile
c phafile    char   : phafile name
c phachan    int    : Array of pha channels
c mnver      char   : Main task version number
c errflg     int    : error flag, 0 is ok
c chatter    int    : Chattines flag
c
c --- COMPILATION/LINKING ------------------------------------------
c
c FTOOLS,CALLIB,FITSIO
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1995 August 9) 1.0.0;
c
c Banashree Mitra Seifert (Aug, 1996) 1.1.0:
c        . replaced screen display routines
c
c Banashree Mitra Seifert (Oct, 1996) 1.2.0:
c        . replaced rdebd1 by rdebd3
c        . replaced rcrmf1 by rdrmf3
c        . replaced wtrmf1 by wtrmf3
c        . replaced wtebd1 by wtebd3
c        . filename character(180)
c        . initialised status=0 (LINUX problem)
c        . formatting i8 instead of i for print out
c
c Peter D Wilsom (Jul 01, 1998) 1.2.1:
c        . Update filename handling for new FCPARS/FTOPEN behavior
c
c Alex M. ( Feb 16, 1999) 1.2.2:
c        . Replaced RDRMF3/WTRMF3 by RDRMF4/WTRMF4. Added 'call RMFSZ' 
c -------------------------------------------------------------------
      character(11) subname
      parameter (subname='gcorr_shft')
      character(5) version
      parameter (version = '1.2.2')
c ------------------------------------------------------------------
C **** DYNAMIC MEMORY ALLOCATION ****
C  the following MEM common block definition is in the system iraf77.inc 
C  file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ************************************
c MAX ARRAY SIZES ( Alex -- added maxne, maxelt, and numelt )
c increased size of keys array (MJT 17Feb2010)
      integer maxne, maxchan, maxgrp, maxelt, numelt
c INTERNALS
c ... parameters & static arrays
      integer maxshft
      parameter (maxshft = 256)
      integer nshfts
      real sfacts(maxshft)
      integer stbins(maxshft), endbins(maxshft)
      character(16) ebdtlscop, ebdinstrum, ebdfilt,ebddet
      character(16) rmftlscop, rmfinstrum, rmffilt,rmfdet        
      real ebdarea
      integer iunit, ounit, n_ebd, ebderr, cerr,lchan
      integer  nkeys, nmore, nk_comm, nk_hist
      integer imove,next(50),ninstr,nfound,nsearch
      character(20) instr(50),outhdu(9,50),extname(50),outver(9,50)
      integer imaxgrp,rmfchan,ienerg,detchans,fchan
      INTEGER numelt_cor,imaxgrp_cor
      INTEGER imaxchan,imaxelt,imaxne
      integer keyspres,j,i,rmfextno,ebdextno,iunit2
      real lo_thresh, rmfarea
      integer block, rw,status,htype
      character(4) cstart, cend
      character(6) compchar
      character(70) bincomm, exhist,errinfo,desc,message
      character(80) rec, hist(5), comm(5)
      logical man_key, qorder, isorder
      character(5) rsp_rmfversn,ebd_rmfversn
      character(20) hduclas3,taskname
      character(180) ebdfilename,rmffilename
      character(8) task,dmode,keys(100),matext
      character(80) rmf_extname,rmf_extcomm
      character(80) ebd_extname,ebd_extcomm

      character(8) ebdchantype,rmfchantype
      real febdchan,frmfchan

c ... pointers to "arrays" to be dynamically allocated

        integer p_ngrp,p_fmatrix,p_F_chan,p_N_chan,p_order
        integer p_energ_lo,p_energ_hi,p_e_min,p_e_max
	integer p_ebdchan,p_en_slice,p_buffer,p_fmatrix_cor
        integer p_ngrp_cor, p_F_ch,p_N_ch

c ... "arrays" to be dynamically allocated
c       integer ngrp(maxne)             real fmatrix(maxelt)
c       integer F_chan(maxgrp)          integer N_chan(maxgrp)
c       real energ_lo(maxne)            real energ_hi(maxne)
c       real e_min(maxchan)             real e_max(maxchan)
c	integer ebdchan(maxchan)        real en_slice(maxchan)
c       real buffer(maxchan)            integer N_ch(maxgrp)
c       real p_fmatrix_cor(2*maxelt)      integer F_ch(maxgrp)
c       integer p_ngrp_cor(maxne)
c       

c
c --- USER INFO ---
c
  
      desc = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,desc) 


c --- Alex -- initialize parameters

      isorder = .false.
      qorder  = .false.
      maxne   = 0
      maxelt  = 0
      maxchan = 0
      maxgrp  = 0

c --- FIND EBOUNDS EXTENSION ---

      status=0
      call fcpars(ebdfile,ebdfilename,ebdextno,status)
C PDW 7/1/98: Call ftrtnm to really strip off extension #
      call ftrtnm(ebdfile,ebdfilename,status)
      call crmvlbk(ebdfilename)
      status = 0
      block = 2880
      rw = 0
      call cgetlun(iunit)
C PDW 7/1/98: Call ftopen with original filename
C      call ftopen(iunit,ebdfilename,rw,block,status)
      call ftopen(iunit,ebdfile,rw,block,status)
      IF (status.NE.0) THEN
          errinfo = 'opening EBOUNDS file !'
          call wterrm(subname,version,errinfo)
          errflg = 1
          goto 482
      ENDIF        
c
c --- FIND EXTNUM if EXTNUM IS NOT SPECIFIED AT THE END OF EBDFILE ---
c
      IF (ebdextno.LE.0) THEN
        ninstr = 2 
        instr(1) ='RESPONSE'
        instr(2) = 'EBOUNDS' 
        nsearch = 50 
        call fndhdu(chatter,iunit,ninstr,instr,nsearch,nfound,next,
     &              outhdu,outver,extname,status)

c --- CHECK FOR (old-style) EXTNAME if HDUCLASS not found ---

        IF (nfound.LE.0) THEN
            errinfo = 'Ext with allowed HDUCLASn keywords not found'
            call wtwarm(subname,version,chatter,20,errinfo)
            errinfo = 'searching for EXTNAME= EBOUNDS'
            call wtwarm(subname,version,chatter,20,errinfo)
          call fndext(chatter,iunit,'EBOUNDS',
     &           nsearch,nfound,next,outhdu,outver,extname,status)
        ENDIF

        IF (nfound.LE.0) THEN
           errinfo = 'EBOUNDS extension found '
           call wterrm(subname,version,errinfo)
           errflg = 1
           goto 482
        ENDIF         

        IF (nfound.GT.1) THEN
            errinfo='Input file contains >1 EBOUNDS datasets'
            call wterrm(subname,version,errinfo)
            write(errinfo,'(a,i12,a)')'... ',nfound,' extensions found'
            call wterrm(subname,version,errinfo)
          do i=1,nfound
            write(errinfo,'(a,i12,a)')'Ext ',next(i),':'
            call wterrm(subname,version,errinfo)
            write(errinfo,'(4X,a,a)')'EXTNAME = ',extname(i)
            call wterrm(subname,version,errinfo)
            do j=1,4
              write(errinfo,'(4X,a,i2,2a)')
     &        'HDUCLAS',j,' = ',outhdu(j,i)
              call wterrm(subname,version,errinfo)
            enddo
          enddo
          errinfo =
     & 'Extension number must be specified via ebdfile parameter'
          call wterrm(subname,version,errinfo)
          errinfo = 'for example INPUT.EBD[1]'
          call wterrm(subname,version,errinfo)
          errinfo = 'ebdfile is a hidden parameter'
          call wterrm(subname,version,errinfo)
          errflg = 1
          goto 482
        ENDIF
      ENDIF

c --- MOVE TO APPROPRIATE PLACE IN FILE ---

      IF (ebdextno.LE.0) THEN
        IF (next(1).GT.0) THEN
           imove = next(1)
           status = 0
           call ftmrhd(iunit,imove,htype,status)
           errinfo = ' Problem moving to EBOUNDS xtens'
           IF (status.NE.0) THEN
             call wtferr(subname,version,status, errinfo)
             errflg = 1
             goto 482
           ENDIF
        ENDIF
        ebdextno = next(1)
      ELSE
        status = 0
        call ftmahd(iunit,ebdextno+1,htype,status)
        errinfo = 'Problem moving to EBOUNDS extension'
        IF (status.NE.0) THEN
          errflg = 1
          call wterrm(subname,version,errinfo)
          goto 482
        ENDIF
      ENDIF


c Alex --- Get the maxchan and maxne  array sizes

      status = 0

      call ftgkyj(iunit,'DETCHANS',maxchan,errinfo,status)
      errinfo = 'reading RMF DETCHANS value'
      call wtferr(subname,version,status,errinfo)
      maxchan = maxchan + 2
      write(errinfo,'(a,i8)') ' Maxchan array :',maxchan
      call wtinfo(chatter,30,2,errinfo)
      IF (status.NE.0) THEN
        errflg = 1
        goto 482
      ENDIF

      call ftgkyj(iunit,'NAXIS2',maxne,errinfo,status)
      errinfo = 'reading RMF NAXIS2 value'
      call wtferr(subname,version,status,errinfo)
      write(errinfo,'(a,i8)') ' Maxne array :',maxne
      call wtinfo(chatter,30,2,errinfo)
      IF (status.NE.0) THEN
        errflg = 1
        goto 482
      ENDIF

c ALLOCATE MEMORY

      p_e_min = 0
      p_e_max = 0
      p_ebdchan = 0

      status = 0
      call udmget(maxne, 6, p_e_min, status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(maxne, 6, p_e_max, status)
      IF (status.NE.0) THEN
        goto 50
      ENDIF
      call udmget(maxne, 4, p_ebdchan, status)
 50   if(status.NE.0) then
           message = 
     >     'Failed to allocate Dynamic Memory for EBOUNDS data'
           call wterrm(subname,version,message)
           errflg = -1
            goto 482
      endif
c
c --- READ EBOUNDS EXTENSION ---
c
      ebderr = 0
      detchans = 0
      febdchan=0.
      ebd_extname =' '
      ebd_extcomm =' '
      call rdebd4(iunit,chatter,ebd_extname,ebd_extcomm,
     >                 maxchan,ebdtlscop,ebdinstrum,
     >                 ebddet,ebdfilt,ebdarea,ebdchantype,febdchan,
     >                 n_ebd,MEMI(p_ebdchan),
     >		       MEMR(p_e_min),MEMR(p_e_max),
     >                 ebd_rmfversn,ebderr)
      IF (ebderr.NE.0) THEN
        errflg = 1
        errinfo = 'reading EBOUNDS extension'
        call wterrm(subname,version,errinfo)
        goto 482
      ENDIF  
c
c --- GET SHIFT FACTOR AND CHANNELS ---
c
      call gt_fchan(MEMI(p_ebdchan),fchan,
     &     lchan,n_ebd,errflg,chatter)
      task = 'GCORRMF'
      dmode = '   '
      IF (detchans.EQ.0) THEN
        detchans = n_ebd
      ENDIF
      call shft_fact(shftfile,fchan,lchan,nshfts,
     &              stbins,endbins,sfacts,maxshft,errflg,chatter)
      IF (errflg.NE.0) THEN
        errinfo = '..determining shift factor'
        call wterrm(subname,version,errinfo)
        goto 482
      ENDIF 
c
c --- SHIFT EBOUNDS DATA ---
c
c      ebdcerr = 0
c      call gcorr_ebd(n_ebd,MEMI(p_ebdchan),MEMR(p_e_min),MEMR(p_e_max),
c     &		    nshfts,stbins,endbins,sfacts,ebdcerr,chatter)
c
c --- READ RMF DATA ---
c
c --- FIND RMF EXTENSION ---

      call fcpars(infile,rmffilename,rmfextno,status)
C PDW 7/1/98: Call ftrtnm to really strip off extension #
      call ftrtnm(infile,rmffilename,status)
      call crmvlbk(rmffilename)
      IF (rmffilename.EQ.ebdfilename) THEN
        iunit2=iunit
        call ftmahd(iunit2,1,htype,status)
      ELSE
        status = 0
        call ftclos(iunit,status)
        status = 0
        block = 2880
        rw = 0
        call cgetlun(iunit2)
C PDW 7/1/98: Call ftopen with original filename
        call ftopen(iunit2,infile,rw,block,status)
        IF (status.NE.0) THEN
          errinfo = 'opening RMF file !'
          call wterrm(subname,version,errinfo)
          errflg = 1
          goto 482
        ENDIF        
      ENDIF

c
c --- FIND EXTNUM if EXTNUM IS NOT SPECIFIED AT THE END OF INFILE ---
c
      IF (rmfextno.LE.0) THEN
        ninstr = 2 
        instr(1) = 'RESPONSE'
        instr(2) = 'RSP_MATRIX'
        nsearch = 50 
        call fndhdu(chatter,iunit2,ninstr,instr,nsearch,nfound,next,
     &              outhdu,outver,extname,status)

c --- CHECK FOR (old-style) EXTNAME if HDUCLASS not found ---

        IF (nfound.LE.0) THEN
            errinfo = 
     &      'Ext with allowed HDUCLASn keywords not found'
            call wtinfo(chatter,20,1,errinfo)
            errinfo = 'searching for EXTNAME= MATRIX'
            call wtinfo(chatter,20,1,errinfo)
          call fndext(chatter,iunit2,'MATRIX',
     &           nsearch,nfound,next,outhdu,outver,extname,status)
        ENDIF

        IF (nfound.LE.0) THEN
            errinfo = ' ... searching for EXTNAME=SPECRESP'
            call wtinfo(chatter,20,1,errinfo)
            call fndext(chatter,iunit2,'SPECRESP MATRIX',
     &           nsearch,nfound,next,outhdu,outver,extname,status)
        ENDIF

        IF (nfound.LE.0) THEN
           errinfo = 'RSP_MATRIX extension not found '
           call wtinfo(chatter,20,1,errinfo)
           errflg = 1
           goto 482
        ENDIF

        IF (nfound.GT.1) THEN
          errinfo = 'Input file contains >1 MATRIX datasets'
          call wterrm(subname,version,errinfo)
          write(errinfo,'(a,i12,a)')' ',nfound,' extensions found'
          call wterrm(subname,version,errinfo)
          do i=1,nfound
            write(errinfo,'(a,i12,a)')'Ext ',next(i),':'
            call wterrm(subname,version,errinfo)
            write(errinfo,'(4X,a,a)')'EXTNAME = ',extname(i)
            call wterrm(subname,version,errinfo)
            do j=1,4
              write(errinfo,'(4X,a,i2,2a)')
     &        'HDUCLAS',j,' = ',outhdu(j,i)
              call wterrm(subname,version,errinfo)
            enddo
          enddo
          errinfo =
     & '... Extension number must be specified via infile parameter'
          call wterrm(subname,version,errinfo)
          errinfo = 'for example INPUT.RMF[1]'
          call wterrm(subname,version,errinfo)
          errflg = 1
          goto 482
        ENDIF
      ENDIF

c --- MOVE TO APPROPRIATE PLACE IN FILE ---

      IF (rmfextno.LE.0) THEN
        IF (next(1).GT.0) THEN
           imove = next(1)
           status = 0
           call ftmrhd(iunit2,imove,htype,status)
           errinfo = 'Problem moving to EBOUNDS xtens'
           IF (status.NE.0) THEN
             call wtferr(subname,version,status, errinfo)
             errflg = 1
             goto 482
           ENDIF
        ENDIF
        rmfextno = next(1)
      ELSE
        status = 0
        call ftmahd(iunit2,rmfextno+1,htype,status)
        errinfo = 'Problem moving to MATRIX extension'
        IF (status.NE.0) THEN
          errflg = 1
          call wtferr(subname,version,status,errinfo)
          goto 482
        ENDIF
      ENDIF


c Alex --- Get the array sizes maxne, maxgrp, maxelt
      
      status = 0
      call rmfsz( iunit2, chatter, maxne, maxgrp, maxelt, status )
      errinfo = 'reading RMF array sizes maxne, maxgrp, and maxelt'
      call wtferr(subname,version,status,errinfo)
      IF (status.NE.0) THEN
        errflg = 1
        goto 482
      ENDIF

c --------------------------------------------------------------------

c ALLOCATE DMA

      p_energ_lo = 0
      p_energ_hi = 0
      p_ngrp = 0
      p_fmatrix = 0
      p_F_chan = 0
      p_N_chan = 0
      p_ngrp_cor = 0
      p_fmatrix_cor = 0
      p_F_ch = 0
      p_N_ch = 0
      p_order = 0
      p_en_slice = 0
      p_buffer = 0

      status = 0
      call udmget(maxne, 6, p_energ_lo, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(maxne, 6, p_energ_hi, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      status = 0
      call udmget(maxne, 4, p_ngrp, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(maxelt, 6, p_fmatrix, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(maxgrp, 4, p_F_chan, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(maxgrp, 4, p_N_chan, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(maxne, 4, p_ngrp_cor, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(2*maxelt, 6, p_fmatrix_cor, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(2*maxgrp, 4, p_F_ch, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(2*maxgrp, 4, p_N_ch, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(2*maxgrp, 4, p_order, status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(maxchan,6,p_en_slice,status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
      call udmget(maxchan,6,p_buffer,status)
      IF (status.NE.0) THEN
        goto 70
      ENDIF
 70   if(status.NE.0) then
         message = 'Failed to allocate dynamic Memory for RMF data'
         call wterrm(subname,version,message)
         errflg = -1
         goto 482
       endif

c --- Alex --- Replaced RDRMF3 by RDRMF4
c --- Ziqin --- Replaced RDRMF4 by RDRMF5
     
      rmfchan = 0
      ienerg = 0
      imaxgrp = 0
      numelt = 0

      rmf_extname = ' '
      rmf_extcomm = ' '
      call rdrmf5(iunit2,chatter,rmf_extname,rmf_extcomm,
     & qorder,maxne,maxgrp,maxelt,
     & rsp_rmfversn,hduclas3,rmftlscop,rmfinstrum,rmfdet,rmffilt,
     & rmfarea,rmfchantype,frmfchan,rmfchan,ienerg,imaxgrp,numelt,
     & MEMR(p_energ_lo),MEMR(p_energ_hi),MEMI(p_ngrp),MEMI(p_F_chan),
     & MEMI(p_N_chan),isorder,MEMI(p_order),MEMR(p_fmatrix),
     & lo_thresh,errflg)
      IF (errflg.NE.0) THEN
        errinfo = 'reading RMF extension '
        call wterrm(subname,version,errinfo)
        goto 482
      ENDIF
      status = 0
c
c --- SHIFT RMF DATA ---
c

c --- Alex: add MEMI(p_ngrp), MEMI(p_F_chan), and MEMI(p_N_chan) arrays
c --- GCORR_RMF returns the matrix with the shifted channels, 
c --- FMATRIX_COR, and new arrays NGRP_COR(), F_CH(), and N_CH() 

      numelt_cor = 0
      imaxgrp_cor = 0

      call gcorr_rmf(maxchan,maxne,maxelt,maxgrp,rmfchan,ienerg,
     & MEMR(p_fmatrix),MEMI(p_ngrp),MEMI(p_F_chan),MEMI(p_N_chan),
     & numelt_cor,imaxgrp_cor,MEMR(p_fmatrix_cor),
     & MEMI(p_ngrp_cor),MEMI(p_F_ch),MEMI(p_N_ch),MEMR(p_en_slice),
     & MEMR(p_buffer),lo_thresh,nshfts,stbins,endbins,sfacts,
     & fchan,lchan,chatter,errflg)

c maxchan              i
c maxne                i
c maxelt               i
c maxgrp               i
c rmfchan              i
c ienerg               i
c MEMR(p_fmatrix)      i
c MEMI(p_ngrp)         i
c MEMI(p_F_chan)       i
c MEMI(p_N_chan)       i
c numelt_cor           o  <=== number of output matrix elements
c imaxgrp_cor          o  <=== total number of groups in the output matrix
c MEMR(p_fmatrix_cor)  o  <=== output RMF matrix
c MEMR(p_ngrp_cor)     o  <=== number of channel groups output array
c MEMI(p_F_ch)         o  <=== first channels output array
c MEMI(p_N_ch)         o  <=== number of channels output array
c MEMR(P_en_slice)     i
c MEMR(p_buffer)       i
c lo_thresh            i
c nshfts               i
c stbins               i
c endbins              i
c sfacts               i
c fchan                i
c lchan                i
c chatter              i
c errflg               o


      IF (errflg.NE.0) THEN
        errinfo = 'rebinning RMF data'
        call wterrm(subname,version,errinfo)
        goto 482
      ENDIF
      call ftclos(iunit2,status)

c
c --- WRITE DATA ---
c
      call copyphd(rmffilename,outfile,killit,errflg,chatter)
c Ziqin --- copy the extension before rmfextno & eboundextno
      IF (ebdfilename.EQ.rmffilename) THEN
        call gcorr_copyrem(rmffilename,outfile,ebdextno,rmfextno,
     &                    -1,errflg,chatter)
      ENDIF

      call cgetlun(iunit)
      block = 2280
C PDW 7/1/98: Use original filename instead of rmffilename:
C      call ftopen(iunit,rmffilename,0,block,status)
      call ftopen(iunit,infile,0,block,status)
      status = 0
      block = 2280
      call cgetlun(ounit)
      call ftopen(ounit,outfile,1,block,status)
      nk_hist = 0 
      nk_comm = 0 
      ebderr = 0
c
c --- WRITE RMF EXTENSION ( Alex --- Replaced WTRMF3 by WTRMF4 )
c
c --- WRITE RMF EXTENSION ( Ziqin --- Replaced WTRMF4 by WTRMF5 )

      IF (ebdfilename.EQ.rmffilename) THEN
      IF( rmfextno.gt. ebdextno) THEN
          goto 504
      ENDIF
      ENDIF

502   CONTINUE

      errflg = 0
      call wtrmf5(ounit,chatter,rmf_extname,rmf_extcomm,
     & nk_hist,hist,nk_comm,
     & comm,rsp_rmfversn,hduclas3,rmftlscop,rmfinstrum,rmfdet,
     & rmffilt,rmfarea,rmfchantype,frmfchan,numelt_cor,rmfchan,
     & ienerg,imaxgrp_cor,MEMR(p_energ_lo),MEMR(p_energ_hi),
     & MEMI(p_ngrp_cor),MEMI(p_F_ch),MEMI(p_N_ch),qorder,
     & MEMI(p_order),MEMR(p_fmatrix_cor),lo_thresh,errflg)

      IF (errflg.NE.0) THEN
        errinfo = 'writing RMF extension !'
        call wterrm(subname,version,errinfo)
        goto 482
      ENDIF

c
c --- WRITE REMAINING KEYWORDS IN RMF EXTENSION ---
c
      call ftmahd(iunit,rmfextno+1,htype,status)
      call getkeys(ounit,keyspres,keys,chatter,status)
      call ftghsp(iunit,nkeys,nmore,status)
      do i=1,nkeys
        status = 0
        call ftgrec(iunit,i,rec,status)
        errinfo='reading RMF record'
        call wtferr(subname,version,status,errinfo)
        man_key = .false.
        do j=1,keyspres
          IF (rec(1:8).EQ.keys(j)) THEN
            man_key = .true.
          ENDIF
        enddo
        IF (.NOT.man_key) THEN
          call ftprec(ounit,rec,status)
        ENDIF
        errinfo = 'writing non-mandatory record '
        call wtferr(subname,version,status,errinfo)
      enddo      
c
c --- WRITE EXTRA HISTORY ---
c
      status = 0
      call ftpdat(ounit,status)
      taskname = task//mnver
      status = 0
      call ftpkys(ounit,'CREATOR',taskname,
     & ' s/w task which wrote this dataset',status)
      exhist = ' The original file was '//infile
      status = 0
      call ftphis(ounit,exhist,status)
c
c --- WRITE REBIN COMMENT ---
c
      do i=1,nshfts
        write(cstart,100,IOSTAT = cerr) stbins(i)
        write(cend,100,IOSTAT = cerr) endbins(i)
        write(compchar,110,IOSTAT = cerr) sfacts(i)
        bincomm = ' data shifted from'//cstart//' TO '
     &//cend//' with a shift factor : '//compchar
        status = 0
        call rmvexsp(bincomm,exhist)
        call ftpcom(ounit,exhist,status)
      enddo       

      IF (ebdfilename.EQ.rmffilename) THEN
      IF( rmfextno.gt. ebdextno) THEN
          goto 505
      ENDIF
      ENDIF
503   CONTINUE
      IF (ebdfilename.EQ.rmffilename) THEN
        call gcorr_copyrem(rmffilename,outfile,ebdextno,rmfextno,
     &                    0,errflg,chatter)

      ENDIF
      IF (ebdfilename.EQ.rmffilename) THEN
      IF( rmfextno.gt. ebdextno) THEN
          goto 502
      ENDIF
      ENDIF

504   CONTINUE
c
c --- WRITE EBOUNDS EXTENSION ---
c


      call wtebd4(ounit,chatter,ebd_extname,ebd_extcomm,
     & nk_hist,hist,nk_comm,
     & comm,ebd_rmfversn,ebdtlscop,ebdinstrum,ebddet,
     & ebdfilt,ebdarea,ebdchantype,febdchan,n_ebd,
     > MEMR(p_e_min), MEMR(p_e_max),ebderr)
      IF (ebderr.NE.0) THEN
        errflg = 2
        errinfo = 'writing EBOUNDS extension'
        call wterrm(subname,version,errinfo)
        goto 482
      ENDIf
c
c --- WRITE REMAINING KEYWORDS IN EBOUNDS EXTENSION ---
c
      IF (ebdfilename.EQ.rmffilename) THEN
        call ftmahd(iunit,ebdextno+1,htype,status)
        iunit2=iunit
      ELSE
        status = 0
        call ftclos(iunit,status)
        status = 0
        call cgetlun(iunit2)
C PDW 7/1/98: Use original filename instead of ebdfilename:
C        call ftopen(iunit2,ebdfilename,0,block,status)
        call ftopen(iunit2,ebdfile,0,block,status)
        status = 0
        call ftmahd(iunit2,ebdextno+1,htype,status)
      ENDIF
      status = 0
      call getkeys(ounit,keyspres,keys,chatter,status)
      call ftghsp(iunit2,nkeys,nmore,status)     
      do i=1,nkeys
        status = 0
        call ftgrec(iunit2,i,rec,status)
        errinfo = 'readind record from EBOUNDS ext'
        call wtferr(subname,version,status,errinfo)
        man_key = .false.
        do j=1,keyspres
          IF (rec(1:8).EQ.keys(j)) THEN
            man_key = .true.
          ENDIF
        enddo
        IF (.NOT.man_key) THEN
          call ftprec(ounit,rec,status)
        ENDIF
        errinfo = 'writing non-mandatory record '
        call wtferr(subname,version,status,errinfo)
      enddo
c      
c --- WRITE EXTRA HISTORY ---
c  
      call ftpdat(ounit,status) 
      taskname = task//mnver 
      status = 0 
      call ftpkys(ounit,'CREATOR',taskname,
     &' s/w task which wrote this dataset',status) 
      exhist = ' The original file was '//ebdfile
      call ftphis(ounit,exhist,status)  
c
c --- WRITE shift COMMENT ---
c
c      do i=1,nshfts
c        write(cstart,100,IOSTAT = cerr) stbins(i)
c        write(cend,100,IOSTAT = cerr) endbins(i)
c        write(compchar,100,IOSTAT = cerr) sfacts(i)
c        bincomm = ' data shifted from'//cstart//' TO '
c     &//cend//' with a shift factor: '//compchar
c        call rmvexsp(bincomm,exhist)
c        status = 0
c        call ftpcom(ounit,exhist,status)
c      enddo
      status = 0
      call ftclos(iunit2,status)
      status = 0
      call ftclos(ounit,status)

      IF (ebdfilename.EQ.rmffilename) THEN
      IF( rmfextno.gt. ebdextno) THEN
          goto 503
      ENDIF
      ENDIF

505   CONTINUE
c
c --- MAKE COMPLETE COPIES OF ANY OTHER EXTENSIONS IN INFILE ---
c
      IF (ebdfilename.EQ.rmffilename) THEN
        call gcorr_copyrem(rmffilename,outfile,ebdextno,rmfextno,
     &    1,errflg,chatter)
      ENDIF
  100 FORMAT(I4)
 110  format (f6.2)


c Check for errors
482     if(errflg.ne.0) then
        errflg = 1
                message =  ' Fatal'
                call wterrm(subname,version,message)
        endif

c Delete the Allocated Memory

        call udmfre(p_e_min, 6, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_e_max, 6, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
	call udmfre(p_ebdchan, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_energ_lo, 6, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_energ_hi, 6, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_ngrp,4,status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_fmatrix,6,status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_F_chan, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_N_chan, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_ngrp_cor,4,status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_fmatrix_cor,6,status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_F_ch, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_N_ch, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_order, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_en_slice, 6, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_buffer, 6, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
 485    if(status.NE.0) then
           message = 'Failed to deallocate Dynamic Memory'
           call wterrm(subname,version,message)
           errflg = 99
        endif

c ----------------------------------------------------------------------
      return
      end
c ----------------------------------------------------------------------
c     END OF GCORR_SHFT
c ----------------------------------------------------------------------


*+GCORR_RMF
c     -------------------------------------------------------
      subroutine gcorr_rmf(maxchan,maxne,maxelt,maxgrp,
     & ichan,ienerg,fmatrix,NGRP,F_CHAN,N_CHAN,
     & numelt_corr,imaxgrp_corr,fmatrix_corr,
     & NGRP_CORR,F_CH,N_CH,en_slice,buffer,thresh,nshfts,
     & stbins,endbins,sfacts,fchan,lchan,chatter,err)
c     -------------------------------------------------------


c --- DESCRIPTION --------------------------------------------------
c
c This routine shifts RMF channels, and the energy arrays are
c changed accordingly.
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE

      integer ichan, chatter, err, nshfts, maxelt, maxgrp
      integer fchan,lchan,ienerg,numelt,imaxgrp_corr
      integer stbins(*),endbins(*),maxchan,maxne,numelt_corr

      real sfacts(*),en_slice(maxchan)
      REAL BUFFER(maxchan),THRESH
      REAL fmatrix(maxelt),fmatrix_corr(2*maxelt)

      INTEGER NGRP(maxne), NGRP_CORR(ienerg),F_CHAN(maxgrp)
      INTEGER N_CHAN(maxgrp),F_CH(2*maxgrp),N_CH(2*maxgrp)
c
c --- LOCALS ---
c
      integer newch,ch,stbin,endbin,ie,i,ch_mv
      real s_shft,frac1
      integer shft1
      character(100) desc
      logical curshift

      INTEGER Igr,Irmf,Imtrx,INDEX,J,Jmax,K,Kmax
      LOGICAL  COUNT

c
c --- MAIN VARIABLES  ----- -----------------------------------------
c
c     MAXCHAN             i  maximum number of channels for each energy row
c     MAXNE               i  maximum number of energy rows
c     MAXELT              i  maximum number of matrix elements
c     MAXGRP              i  maximum number of channel groups
c     ICHAN               i  actual number of channels for each energy row in 
c                              rmf matrix
c     IENERG              i  actual number of energy rows in rmf matrix
c     FMATRIX             i  input rmf matrix array
c     NGRP                i  input array of number of groups
c     F_CHAN              i  input array of first channels
c     N_CHAN              i  input array of number of channels
c     EN_SLICE            i  input array of size = maximum number of channels
c     BUFFER              i  input array of size = maximum number of channels
c     THRESH              i  input value of threshold
c ------------------------------------------------------------------
c     NUMELT_CORR         o  size of the output corrected matrix array 
c     IMAXGRP_CORR        o  total number of groups in the output matrix 
c     FMATRIX_CORR        o  output corrected matrix array
c     NGRP_CORR           o  number of groups as a function of energy in 
c                            the output matrix
c     F_CH                o  output array of first channels
c     N_CH                o  ouput array of number of channels
c-------------------------------------------------------------------
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1995 Aug 11)
c
c Banashree Mitra Seifert (Aug, 1996) 1.1.0:
c        . replaced with screen display routines
c        . fractional shift introduced instead of integer
c
c AM (March 1999) 1.1.1: 
c        . modified to take into account the fact that the arrays 
c          are now 1-dimensional. Added a block that generates 
c          the output corrected matrix array, calculates the number 
c
c Ning Gan (1.1.2: Nov 01, 1999)
c           . Fixed the bugs in gcorr_rmf. Now the output file will 
c             have the same group structure as the input file. 
c ------------------------------------------------------------------

      character(10) subname
      parameter (subname='gcorr_rmf')
      character(5) version 
      parameter (version = '1.1.2')
      integer lch, fch,igr2 
      logical found_fch, found_lch

c ------------------------------------------------------------------

      desc = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,1,desc)


      IF (fchan.EQ.0) THEN
        ch_mv = 1
      ELSE
        ch_mv = 0
      ENDIF

       INDEX = 0 
       Irmf  = 0
       Imtrx = 0
       Igr   = 0
       igr2 = 0

       numelt_corr  = 0
       imaxgrp_corr = 0

C Alex ---- Initialize the output F_CH() and N_CH() arrays

       DO J = 1,2*MAXGRP 
         F_CH(J) = 0
         N_CH(J) = 0
       ENDDO

c Alex ---- Initialize the output FMATRIX_CORR() array

       DO J = 1, MAXELT
         FMATRIX_CORR(J) = 0.E0
       ENDDO

c --------------------------------------------------------

       
       DO IE = 1,IENERG

       COUNT = .FALSE.


c Alex ---- Initialize the BUFFER and EN_SLICE arrays

	DO CH = 1,ICHAN
	  EN_SLICE(CH) = 0.E0
          BUFFER(CH)   = 0.E0
        ENDDO

c Alex ---- "Uncompress" matrix array and copy its elements for each energy 
c            row to the EN_SLICE() array

	    Jmax = NGRP(IE)

	      DO J = 1,Jmax
                 Igr = Igr + 1
	         Kmax = N_CHAN(Igr)

		    DO K = 1, Kmax
                      Imtrx = Imtrx + 1
                      CH = F_CHAN(Igr) + K - 1 + ch_mv
                      EN_SLICE(CH) = EN_SLICE(CH) + FMATRIX(Imtrx)
                    ENDDO

              ENDDO	

c Alex ---  BEGIN LOOPING THROUGH THE CHANNEL SHIFTS  

        do i=1,nshfts

c Alex --- SHIFT THE CHANNELS

          stbin = stbins(i)
          endbin = endbins(i)
          s_shft = abs(sfacts(i))
          shft1=int(s_shft +1)
          frac1=s_shft - int(s_shft)
          curshift = .true.
          ch = stbin

c -----------  case when shift is +ve ---------------------------------

        IF(sfacts(i).ge.0) then

          do WHILE(curshift)
            newch = ch + shft1
            IF ((newch.GE.fchan).AND.(newch.LE.lchan)) THEN
              BUFFER(newch+ch_mv ) = BUFFER(newch+ch_mv)+
     &            frac1* en_slice(ch+ch_mv) 
            ENDIF
            IF ((newch-1 .GE.fchan).AND.(newch-1 .LE.lchan)) THEN
              BUFFER(newch+ch_mv-1) = BUFFER(newch+ch_mv-1)+
     &            (1-frac1)*en_slice(ch+ch_mv)
            ENDIF

            ch = ch + 1
            IF (ch.GT.endbin) THEN
              curshift = .false.
            ENDIF
          enddo

c --------------  case when shift < 0  ------------------------------------

        else

          shft1=-shft1

          DO WHILE(curshift)
            newch = ch + shft1
            IF ((newch.GE.fchan).AND.(newch.LE.lchan)) THEN
              BUFFER(newch+ch_mv ) = BUFFER(newch+ch_mv)+
     &            frac1*en_slice(ch+ch_mv)
            ENDIF
            IF ((newch+1 .GE.fchan).AND.(newch+1 .LE.lchan)) THEN
              BUFFER(newch+ch_mv+1) = BUFFER(newch+ch_mv+1)+
     &            (1-frac1)*en_slice(ch+ch_mv)
            ENDIF
            ch = ch + 1
            IF (ch.GT.endbin) THEN
              curshift = .false.

            ENDIF
          ENDDO

        ENDIF

c ---------------------------------------------------------------------------

      ENDDO

C -------------------  THE CHANNELS ARE SHIFTED -----------------------------


c Alex *****  CREATE CORRECTED MATRIX ARRAY, FMATRIX_CORR(), ***** 
c       ****   FIND NGRP_CORR(), N_CH(), and F_CH() ARRAYS   ****


c Alex ---- Find the channel groups, number of channels, and the first channel 
c                       in each group of the output RMF matrix

        NGRP_CORR(IE) = 0
        COUNT    = .FALSE.

c Alex ---- COMPRESS THE BUFFER ARRAY AND COPY IT TO THE CORRESPONDING 
c               ELEMENTS OF THE MATRIX FMATRIX_CORR()


c Ning Gan ---- Find the corrected first channel and last channel in each 
c               group.
        DO J = 1,Jmax
            igr2 = igr2 + 1
	    Kmax = N_CHAN(igr2)
            fch = f_chan(igr2)
            lch = f_chan(igr2)+Kmax-1
            found_fch = .false.
            found_lch = .false.
            do i=1,nshfts
               shft1 = nint(sfacts(i))
               if(.not.found_fch .and. 
     &              fch .ge. stbins(i) . and.
     &              fch .le. endbins(i)) then 
                    fch = fch + shft1
c                   check the previous channel
                    IF ( fch-1.ge.fchan ) then 
                       if(BUFFER(fch-1+ch_mv) .GT.THRESH ) fch = fch - 1 
                    endif
                    if(fch .lt. fchan) fch = fchan
                    found_fch = .true.
               endif
               if(.not.found_lch .and. 
     &              lch .ge. stbins(i) . and.
     &              lch .le. endbins(i)) then 
                    lch = lch + shft1
c                   check the next channel
                    IF ( lch+1.le.lchan) then 
                       if(BUFFER(lch +1+ch_mv) .GT. THRESH ) lch = lch+1 
                    endif
                    if(lch .gt. lchan) lch = Lchan
                    found_lch = .true.
               endif 
               if(found_lch.and.found_fch) goto 213
            enddo
213         if(fch .gt. lchan) fch = lchan
            if(lch .lt. fch) lch = fch
c Alex ---- Save the channel group and first channel for each group 
c           in the corresponding arrays 
 
            INDEX = INDEX + 1
            IF ( INDEX.GT.(2*MAXGRP) ) THEN 
                DESC = 'GCORR_RMF: INSUFFICIENT MEMORY FOR F_CH, N_CH'
                CALL FCECHO(DESC)
                ERR = 1
                RETURN
            ENDIF

            NGRP_CORR(IE)  = NGRP_CORR(IE) + 1


c           THE BUFFER IS COMPRESSED AND COPIED
c The first channels array:
             F_CH(INDEX) = fch 
c --------------------------------------------
c The number of channels array:
             N_CH(INDEX) = lch - fch +1
c --------------------------------------------------------

            do i = fch, lch 
                Irmf = Irmf + 1

c Alex --- Copy the elements (with the values exceeding the threshold) 
c            of the BUFFER array to the "compressed" FMATRIX array

                FMATRIX_CORR(Irmf) = BUFFER(i+ch_mv)
            ENDDO
       enddo

c The number of channel groups array:

      IMAXGRP_CORR = IMAXGRP_CORR + NGRP_CORR(IE)
c -----------------------------------------------
      ENDDO

c Alex ----     THE CHANNEL SHIFT IS DONE FOR ALL ENERGIES

c  Save the number of matrix elements
    
           NUMELT_CORR  = Irmf

c -----------------------------------

        desc = subname//' : matrix is shifted'
        call wtinfo(chatter,20,1,desc)
      return
      end

c -----------------------------------------------------------------------
c                         << END OF GCORR_RMF >>
c -----------------------------------------------------------------------



*+GCORR_EBD
c     -------------------------------------------------------
      subroutine gcorr_ebd(n_ebd,ebdchan,e_min,e_max,nshfts,
     &                    stbins,endbins,sfacts,err,chatter)
c     -------------------------------------------------------
c --- DESCRIPTION --------------------------------------------------
c
c This routine rebins EBOUNDS channels, and the energy arrays are
c changed accordingly.
c
c --- VARIABLES ----------------------------------------------------
c
      IMPLICIT NONE
      integer n_ebd, chatter, err, nshfts
      integer stbins(*), endbins(*)
      real sfacts(*)
      integer ebdchan(*)
      real e_min(*), e_max(*)
c
c --- LOCALS ---
c
      integer new, i, stbin, stnew, endbin, nbin, endnew
      logical curbin
      character(70) desc
c
c --- VARIABLE DIRECTORY -------------------------------------------
c
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c
c Rehana Yusaf (1993 July 16)
c
      character(5) version 
      parameter (version = '1.0.0')
*-
c ------------------------------------------------------------------
c
c --- USER INFO ---
c
      IF (chatter.GE.10) THEN
        desc = ' ... using GCORR_EBD Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- COMPRESS --- 
c
      new = 1
      do i=1,nshfts
        stbin = stbins(i)
        endbin = endbins(i)
        stnew = stbin
        nbin = sfacts(i)
        curbin = .true.
        do WHILE(curbin)
          endnew = stnew + nbin - 1
          ebdchan(new) = new
          e_min(new) = e_min(stnew)
          e_max(new) = e_max(endnew)
          new = new + 1
          stnew = endnew + 1
          IF (stnew.GE.endbin) THEN
            curbin = .false.
          ENDIF
        enddo
      enddo
      n_ebd = new - 1
      return
      end
c -----------------------------------------------------------------------
c     END OF GCORR_EBD
c -----------------------------------------------------------------------
   


*+GCORR_COPYREM
c     ------------------------------------------------------
      subroutine gcorr_copyrem(infile,outfile,ebdextno,specextno,
     &                        flg,errflg,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c This routine copies any extensions, other than the EBOUNDS, and
c SPECRESP from infile to outfile.
c --------------------------------------------------------------------
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile, outfile
      integer chatter, errflg,ebdextno,specextno,flg
c
c --- INTERNALS ------------------------------------------------------
c
      integer ignore1,ignore2,iunit,block,i
      logical endfile,pkey,hist
      character(70) ap_in
      character(4) cnum
      integer htype, nhdu,status,tothd,iend,end_num,ierr
      character(32) errstr,wrnstr
      character(70) desc,errinfo
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c infile     char     : input filename
c outfile    char     : output filename
c errflg     int      : error flag
c chatter    int      : Chattiness flag
c
c --- AUTHORS/MODIFICATION -------------------------------------------
c
c Rehana Yusaf (1993 August 1)
c Rehana Yusaf (1993 Nov 22) 1.0.1; spexextno and ebdextno now passed
      character(5) version
      parameter (version = '1.0.1')
*-
c --------------------------------------------------------------------
c
c --- USER INFO ---
c
      errstr = ' ERROR : GCORR_COPYREM Ver '//version//':'
      wrnstr = ' WARNING : GCORR_COPYREM Ver '//version//':'       
      IF (chatter.GE.10) THEN
        desc = ' ... using GCORR_COPYREM Ver '//version
        call fcecho(desc)
      ENDIF
c
c --- DETERMINE SPECRESP AND EBOUNDS EXT Nos ---
c
      status = 0
      call cgetlun(iunit)
      call ftopen(iunit,infile,0,block,status)
      errinfo = errstr//' opening infile !'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
       errflg = 1
       return
      ENDIF
      endfile = .false.
      tothd = 0
      nhdu = 1
      do WHILE (.NOT.endfile)
        status = 0
        call ftmahd(iunit,nhdu,htype,status)
        IF ((status.EQ.107).OR.(status.EQ.207)) THEN
           endfile = .true.
           tothd = nhdu - 2
        ENDIF
        nhdu = nhdu + 1
      enddo
      
c
c --- COPY EXTENSIONS, OTHER THAN EBOUNDS OR SPECRESP ---
c
      IF (specextno.LT.ebdextno) THEN
        ignore1 = specextno
        ignore2 = ebdextno
      ELSE
        ignore1 = ebdextno
        ignore2 = specextno
      ENDIF

c ... COPY EXTENSIONS BEFORE IGNORE1

      IF (flg.eq.-1) THEN

      iend = index(infile(1:),' ') - 1
      pkey = .false.
      hist = .false.
      do i=1,(ignore1-1)
        write(cnum,100,IOSTAT=ierr) i       
        call crmvlbk(cnum)
        end_num = index(cnum(1:),' ') - 1
        IF (end_num.EQ.0) THEN
          end_num = 4
        ENDIF
        ap_in = infile(1:iend)//'['//cnum(1:end_num)//']'
        status = 0
        call cfappend(ap_in,outfile,pkey,hist)
      enddo

      ENDIF

c ... COPY EXTENSIONS INBETWEEN IGNORE1 AND IGNORE2

      IF(flg.eq. 0) THEN
      IF ((ignore1+1).NE.ignore2) THEN
       do i=(ignore1+1),ignore2-1
         write(cnum,100,IOSTAT=ierr) i
         call crmvlbk(cnum)
         end_num = index(cnum(1:),' ') - 1
         IF (end_num.EQ.0) THEN
           end_num = 4
         ENDIF
         ap_in = infile(1:iend)//'['//cnum(1:end_num)//']'
         status = 0
         call cfappend(ap_in,outfile,pkey,hist)          
       enddo
      ENDIF
      ENDIF



c ... COPY EXTENSIONS AFTER IGNORE2 

      IF(flg.eq.1) THEN
      IF (ignore2.NE.tothd) THEN
       do i=1,(ignore2+1),tothd
        write(cnum,100,IOSTAT=ierr) i
        call crmvlbk(cnum)
        end_num = index(cnum(1:),' ') - 1
        IF (end_num.EQ.0) THEN
          end_num = 4
        ENDIF
        ap_in = infile(1:iend)//'['//cnum(1:end_num)//']'
        status = 0
        call cfappend(ap_in,outfile,pkey,hist)    
       enddo
      ENDIF
      ENDIF
  100 FORMAT(I4)
      return
      end
c ---------------------------------------------------------------
c     END OF GCORR_COPYREM
c --------------------------------------------------------------- 

*+SHFT_FACT
c     ------------------------------------------------------ 
      subroutine shft_fact(shftfile,
     &       fchan,lchan,nshfts,stbins,endbins,sfacts,
     &       maxshft,errflg,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION -------------------------------------------------
c This subroutine reads binning info from an ascii file
c --- VARIABLES ---------------------------------------------------
c
      IMPLICIT NONE
      character*(*) shftfile
      integer fchan,maxshft,nshfts,lchan
      integer stbins(maxshft),endbins(maxshft)
      real sfacts(maxshft)
      integer errflg,chatter
c
c --- VARIABLE DIRECTORY ------------------------------------------
c
c fchan    int  : starting channel 
c shftfile char : shift filename
c nshfts   int  : Number of compression factors
c sfacts   int  : Array of shift factors
c stbins   int  : Array of shift starting points for each sfact
c endbins  int  : Array of shift ending points for each sfact
c errflg   int  : Error flag
c chatter  int  : chatter flag
c
c --- AUTHORS/MODIFICATION HISTORY ---------------------------------
c  Rehana Yusaf (1995 August 10) 1.0.0;
c
c Banashree Mitra Seifert (Aug, 1996)1.1.0:
c       . replaced by screen display routines
c       . accomodated for shft, so that it can be fractional shift
c ------------------------------------------------------------------
      character(10) subname
      parameter (subname='shft_fact')
      character(5) version
      parameter (version = '1.0.0')
c 
*-
c -------------------------------------------------------------------
c --- INTERNALS ---

      character(70) subinfo,errinfo
      character(5) chanchar,chanchar2
      character(6) comp
      real s_fact
      integer st_bin,end_bin,i,iunit,status
      integer prev_stbin,prev_endbin
c
c --- USER INFO ---
c
      subinfo = 'using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,subinfo)
c
c --- OPEN BINFILE ---
c
      call ftgiou(iunit,errflg)
      IF (errflg.NE.0) THEN
        errinfo = 'problem getting free lun'
        call wterrm(subname,version,errinfo)
        return
      ENDIF
      open(unit=iunit,file=shftfile,status='old')

c --- READ COMPRESSION DATA FROM BINFILE ---

      nshfts = 0
      status = 0
      prev_stbin = 0
      prev_endbin = 0
      do i=1,maxshft + 1
        read(iunit,*,IOSTAT=status,end=100)st_bin,end_bin,s_fact
        IF (status.NE.0) THEN
          subinfo = 'invalid number in file'
          call wterrm(subname,version,subinfo)
          errflg = 1
          return
        ENDIF
        IF ((nshfts + 1).GT.maxshft) THEN
          subinfo = ' The shift array sizes have been exceeded'
          call wterrm(subname,version,subinfo)
          write(subinfo,'(a,i12)')' The max array dimension is ',maxshft
          call wterrm(subname,version,subinfo)
          errflg = 3
          goto 200
        ENDIF
        IF (nshfts.GE.1) THEN
          IF ((endbins(nshfts) +1).LT.st_bin) THEN
             nshfts = nshfts + 1
             stbins(nshfts) = endbins(nshfts - 1) + 1
             endbins(nshfts) = st_bin - 1
             sfacts(nshfts) = 0.
          ENDIF
        ENDIF
        IF ((nshfts.EQ.0).AND.(st_bin.GT.fchan)) THEN
          nshfts = nshfts + 1
          stbins(nshfts) = fchan
          endbins(nshfts) = st_bin - 1
          sfacts(nshfts) = 0.
        ENDIF
        nshfts = nshfts + 1
        stbins(nshfts) = st_bin
        endbins(nshfts) = end_bin
        sfacts(nshfts) = s_fact
        prev_stbin = stbins(nshfts)
        prev_endbin = endbins(nshfts)
      enddo

 100  IF (endbins(nshfts).LT.lchan) THEN
        IF ((nshfts + 1).GT.maxshft) THEN
          subinfo = ' The shift array sizes have been exceeded'
          call wtinfo(chatter,9,1,subinfo)
          write(subinfo,'(a,i12)')' The max array dimension is ',maxshft
          call wtinfo(chatter,9,1,subinfo)
          errflg = 3
          goto 200
        ENDIF
        nshfts = nshfts + 1
        stbins(nshfts) = endbins(nshfts - 1) + 1
        endbins(nshfts)= lchan
        sfacts(nshfts) = 0.
      ENDIF

c USER INFO 

      IF (chatter.GE.20) THEN
        do i=1,nshfts
          write(chanchar,300,IOSTAT=status) stbins(i)
          write(chanchar2,300,IOSTAT=status) endbins(i)
          write(comp,310,IOSTAT=status) sfacts(i)
          errinfo = chanchar//' - '//chanchar2
     &//' are shifted with shift factor '//comp
          call rmvexsp(errinfo,subinfo)
          call wtinfo(chatter,20,2,subinfo)
        enddo
      ENDIF
  200 close(unit=iunit)
      status = 0
      call ftfiou(iunit,status)
  300 FORMAT(i4)
  310 format (f6.2)
  350 FORMAT(' starting channel:',I4,' ending channel:',I4)
      return
      end
c     -------------------------------------------------------------
c     END OF SHFT_FACT
c     ------------------------------------------------------------- 

























