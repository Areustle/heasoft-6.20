*+PCPARPHA
      SUBROUTINE PCPARA
      IMPLICIT NONE
c 
c Description:
c  Program to generate a PHA spectrum of the Particle background in the 
c  ROSAT PSPC.
c
c Passed Parameters
c  None
c
c User i/ps required (prompted for):
c  None here, isolated in GP_PCPARPHA (see below)
c
c Called routines
c  subroutine FCECHO           : (FTOOLS) write to standard i/o
c  subroutine GP_PCPARPHA     : (below) Gets parameters from par file
c  subroutine DO_PCPARPHA     : (below) Does the rest
c  
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1993 Oct 18), original
c  Ian M George     (1.1.0:1994 Mar 20), Beta-test version
c  Ian M George     (2.0.0:1994 Apr 06), Renamed from PSPCBKGD
c  Ian M George     (2.0.1:1994 Aug 15), MVEXT changes in subroutines
c  Lawrence E Brown (2.1.0:1994 Sept), Added WMAP functionality
c                                      . RY added clobber read (Sept 14 1994)
c  Banashree M Seifert (Oct 9, 1996) 3.0.0:
c            . modifications are made to subroutines to be 
c              compatible with LINUX (e.g. initialisations)
c  Peter D Wilson (3.0.1:1998 Feb 09):
c            . In GTEVRDF, call ftgcvd with dnull, not inull    
c  Peter D Wilson (3.0.2:1998 June 30)
c            . Updated for new FCPARS behavior
c  Ning Gan(3.0.3:1998 July 2)
c            .Modified the date/time string length to 68. 
c  toliver (August 3, 1999) 3.1.0:
c            . increased local array sizes in subroutine datain
c              (were too small for some data files)
c -------------------------------------------------------------------------
      character(7) version
      parameter (version = '3.1.0')
*- 
c Internals 
      integer chatter, ierr, npixsou, evrbin
           real usr_thet, pixsize
      character(5) phaversn
      character(25) context
      character(40) taskname
      character(80) message
      character(80) evrfil, gtifil, oahfil, outfil
      logical qint, qal,qext, qarea,killit
c Initialize
        COMMON/task/taskname
        taskname ='PCPARPHA '//version
 
        message = '** PCPARPHA '//version
        call fcecho(message)

c Get Parameters from the par file
      call gp_PCPARPHA(chatter, 
     &             qint, qal,qext,
     &            evrfil, gtifil, oahfil,
     &            usr_thet,npixsou, pixsize, qarea,
     &            outfil, phaversn, evrbin, killit,ierr)
      if(ierr.NE.0.0) goto 567

c Do the nasty deed      
      call do_PCPARPHA(chatter, 
     &             qint, qal,qext,
     &            evrfil, gtifil, oahfil,
     &            usr_thet, npixsou, pixsize, qarea,
     &            outfil, phaversn, evrbin, killit,ierr)

c Inform user of failure, if necessary
567     if(ierr.NE.0) then
          context = 'Incomplete Execution'
          message = '** PCPARPHA '//version//' ERROR : '// context
          call fcecho(message)
          call fcerr(context)
        else
          message = '** PCPARPHA '//version//' Finished'
          call fcecho(message)
        endif

      return
      end

c -------------------------------------------------------------------------
*+GP_PCPARPHA
      subroutine gp_PCPARPHA(chatter, 
     &             qint, qal,qext,
     &            evrfil, gtifil, oahfil,
     &            usr_thet, npixsou, pixsize, qarea,
     &            outfil, phaversn, evrbin, killit,ierr)

      IMPLICIT NONE
      integer chatter, ierr, npixsou, evrbin, iwibble
           real usr_thet, pixsize
      character*(*) phaversn
      character*(*) evrfil, gtifil, oahfil, outfil
      logical qint, qal,qext, qarea,killit
c 
c Description:
c  Gets the parameters required by PCPARPHA from the parameter file
c  NOTE - The XPI par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  EVRFIL      - name of i/p EVR file
c  GTIFIL      - name of i/p GTI file
c  OAHFIL      - if(QINT) name of i/p Off-axis histogram file 
c  QINT             - Flag whether Internal bkgd to be included in spectrum
c  QAL             - Flag whether Alumin line to be included in spectrum
c  QEXT             - Flag whether External bkgd to be included in spectrum
c  OUTFIL      - name of o/p PHA file in OGIP-standard format
c  PHAVERSN    - OGIP version of PHA file required
c  IERR        - Error Flag (0=OK)
c
c Origin:
c  Original
c
c Called Routines
c  subroutine FCECHO           : (FTOOLS) writes to standard o/p device
c  subroutine WT_FERRMSG       : (CALLIB) Writes standard FITSIO message etc
c
c Compilation:
c  requires XPI/Host interface etc and CALLIB
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1993 Oct 18), Original
      character(7) version
      parameter (version = '1.0.0')
*- 
c Internals
      character(40)  errstr, wrnstr, npixstr
      character(80)  message
c Initialize
      errstr = '** GP_PCPARPHA '//version//' ERROR: '
      wrnstr = '** GP_PCPARPHA '//version//' WARNING: '
      ierr = 0

c Get the name of the o/p FITS file in OGIP-standard format
      call uclgst('outfil',outfil, ierr)
      if(ierr.NE.0) then
        message = errstr // 'Getting OUTFIL parameter'
        call fcecho(message)
        return
      endif

c Get the components that are required to be present in the output spectrum
c ... The Internal background spectrum
        call uclgsb('qintbgd',qint, ierr)
      if(ierr.NE.0) then
          message = errstr // 'Getting QINTBGD parameter'
        call fcecho(message)
        return
      endif
c ... The Al line spectrum
        call uclgsb('qalbgd',qal, ierr)
      if(ierr.NE.0) then
          message = errstr // 'Getting QALBGD parameter'
        call fcecho(message)
        return
      endif
c ... The external background
        call uclgsb('qextbgd',qext, ierr)
      if(ierr.NE.0) then
          message = errstr // 'Getting QEXTBGD parameter'
        call fcecho(message)
        return
      endif

c Check for sillies
        if((.NOT.qint).AND.(.NOT.qal).AND.(.NOT.qext)) then
          message = wrnstr // ' No Bkgd component requested'
          call fcecho(message)
          ierr = 1
          goto 876
        endif
 
c Get the name of the i/p EVR file 
      call uclgst('evrfil',evrfil, ierr)
      if(ierr.NE.0) then
          message = errstr // 'Getting EVRFIL parameter'
        call fcecho(message)
        return
      endif

c Get the rebinning factor
      call uclgsi('evrbin',evrbin, ierr)
      if(ierr.NE.0) then
            message = errstr // 'Getting EVRBIN parameter'
            call fcecho(message)
            ierr = 0 
            message = '... Setting EVRBIN = 30'
            call fcecho(message)
            evrbin = 30
      endif      
      if(evrbin.LT.1) then
                message = wrnstr // 'Illegal value of EVRBIN'
                call fcecho(message)
                message = ' ... Setting EVRBIN = 30'
                call fcecho(message)
                evrbin = 30
      endif

c Get the name of the i/p GTI file 
      call uclgst('gtifil',gtifil, ierr)
      if(ierr.NE.0) then
          message = errstr // 'Getting GTIFIL parameter'
        call fcecho(message)
        return
      endif
      call crmvblk(gtifil)
      if((gtifil.EQ.'NONE').or.(gtifil.EQ.'none').or.
     &            (gtifil.EQ.' '))then
      gtifil = 'NONE'
                endif

      oahfil = 'NONE'
      iwibble = 0
c Get the name of the i/p Off-axis histogram file 
c      (only required if Internal bkgd to be calculated)
827      if(qint) then
        call uclgst('oahfil',oahfil, ierr)
        if(ierr.NE.0) then
            message = errstr // 'Getting OAHFIL parameter'
          call fcecho(message)
          return
        endif
        if((oahfil.EQ.'NONE').or.(oahfil.EQ.'none').or.
     &            (oahfil.EQ.' '))then
          oahfil = 'NONE'
          call uclgsr('theta',usr_thet, ierr)
          if(ierr.NE.0) then
              message = errstr // 'Getting THETA parameter'
            call fcecho(message)
            return
          endif
        endif
      endif

c Decide whether user wishes to override area from off-axis histo
828      call uclgst('npixsou',npixstr, ierr)
      if(ierr.NE.0) then
            message = errstr // 'Getting NPIXSOU parameter'
          call fcecho(message)
          return
      endif
      call crmvlbk(npixstr)
      if(npixstr.EQ.'%') then   
        qarea = .false.
        if(oahfil.EQ.'NONE') then
          message = errstr // ' No WMAP/Off-axis file specified'
          call fcecho(message)
          iwibble=iwibble+1
          if(iwibble.lt.4) then
             goto 827
          else
             message = errstr // 'Aborting run, try specifying an'
             call fcecho(message)
             message = 'integer for npixsou next time.'
             call fcecho(message)
             ierr=88
             goto 876
          endif
        endif
      elseif(npixstr.EQ.'?') then
          message = ' ... This parameter is the number of pixels '// 
     &            'for which the '
          call fcecho(message)
            message = '     particle background spectrum is to be '// 
     &            'calculated.'
          call fcecho(message)
      if(oahfil.EQ.'NONE') then
            message = '     Normally this information is in the '// 
     &            'Off-axis histogram '
          call fcecho(message)
            message = '     dataset, but since no WMAP/OAH file was '// 
     &             'given you must '
          call fcecho(message)
            message = '     specify an integer (you will be '// 
     &            'prompted for '
          call fcecho(message)
            message = '     the pixel size next)'
          call fcecho(message)
      else
            message = '     This information should also be in the '// 
     &            'Off-axis histogram '
          call fcecho(message)
            message = '     dataset. Enter "%" to use the stored '// 
     &            'values or enter '
          call fcecho(message)
            message = '     an integer to override the stored '//
     &             'values. In the '
          call fcecho(message)
            message = '     latter case, you will be prompted for '//
     &             'the pixel size next.'
          call fcecho(message)
      endif
          goto 828
      else
        qarea = .true.
        read(npixstr,'(BN,i40)',err=828) npixsou
        call uclgsr('pixsize',pixsize, ierr)
        if(ierr.NE.0) then
            message = errstr // 'Getting PIXSIZE parameter'
          call fcecho(message)
          return
        endif
        if((npixsou.LE.0).OR.(pixsize.LE.0.0)) then
           message = errstr // 
     &            'Unphysical NPIXSOU/PIXSIZE combo'
           call fcecho(message)
           goto 828
        endif
      endif

c Get the OGIP version number of the PHA file format to be created
      call uclgst('phaversn',phaversn, ierr)
      if(ierr.NE.0) then      
        message = errstr // 'Getting PHAVERSN parameter'
        call fcecho(message)
        phaversn = '1.1.0'
        message = ' ...... Setting format (HDUVERS1) to '//phaversn
          call fcecho(message)                                   
      endif
        if(phaversn.EQ.'1992a') then
                phaversn='1.1.0'
        elseif(phaversn.EQ.'1.0.0') then
           phaversn = '1.1.0'
           message = wrnstr // ' Task no longer supports this format'
           call fcecho(message)
           message =
     &        ' ...... Resetting format (HDUVERS1) to '//phaversn
           call fcecho(message)
        endif

c Get the chattiness flag
      call uclgsi('chatter',chatter, ierr)
      if(ierr.NE.0) then
            message = errstr // 'Getting CHATTER parameter'
            call fcecho(message)
            ierr = 0 
            message = errstr // 'Setting CHATTER = 10'
            call fcecho(message)
            chatter = 10
      endif      

c get clobber

      call uclgsb('clobber',killit,ierr)
      if(ierr.NE.0) then
            message = errstr // 'Getting CLOBBER parameter'
            call fcecho(message)
            killit = .false.
      endif

c Give user info if requested
      if(chatter.GE.20) then      
       message = ' ... using GP_PCPARPHA ' // version
       call fcecho(message)
      endif


876     if(ierr.NE.0) then
                message = errstr // ' Incomplete Parameter Set'
                call fcecho(message)
        endif

      return
      end
c -------------------------------------------------------------------------
*+DO_PCPARPHA
      subroutine do_PCPARPHA(chatter, 
     &             qint, qal,qext,
     &            evrfil, gtifil, oahfil,
     &            usr_thet, npixsou, pixsize, qarea,
     &            outfil, phaversn, evrbin, killit, ierr)

      IMPLICIT NONE
      integer chatter, ierr, npixsou, evrbin
      character*(*) phaversn
      character*(*) evrfil, gtifil, oahfil, outfil
      real usr_thet, pixsize
      logical qint, qal,qext, qarea,killit
c 
c Description:
c  Program to generate a PHA file containing a theoretical Background 
c  spectrum for the ROSAT PSPC.
c
c User i/ps required (prompted for):
c  None
c
c Passed parameters
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  EVRFIL      - name of i/p EVR file
c  GTIFIL      - name of i/p GTI file
c  OAHFIL      - if(QINT) name of i/p Off-axis histogram file 
c  QINT             - Flag whether Internal bkgd to be included in spectrum
c  QAL             - Flag whether Alumin line to be included in spectrum
c  QEXT             - Flag whether External bkgd to be included in spectrum
c  OUTFIL      - name of o/p PHA file in OGIP-standard format
c  PHAVERSN    - OGIP version of PHA file required
c  IERR        - Error Flag (0=OK)
c
c Origin:
c  Original
c
c Called Routines
c  subroutine OP_NPA     : (CALLIB) Opens & writes a null P.Header 
c  subroutine WTPHA1   : (CALLIB) Writes the PHA Xtensn 
c
c Compilation & Linking
c  link with CALLIB
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1993 Oct 18), original
c  Ian M George     (1.1.0:1994 Feb 22), revised & expanded
c
c Banashree M Seifert (1.2.0:1996 Oct9)
c          . format for writing i8 or i5 etc instead of i 
c            in various places (LINUX problem)
c          . initialisation
c Peter D Wilson (1.2.1:1998 June 30)
c          . Replace calls to fcpars with ftrtnm
c ----------------------------------------------------------------------
        character(7) version
        parameter (version = '1.2.1')
*- 
C **** DYNAMIC MEMORY ALLOCATION ****
C  the following MEM common block definition is in the system iraf77.inc file
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
      character(40) taskname
        COMMON/task/taskname
c Max Array sizes
      integer maxchan, maxtheta
      integer mx_history, mx_comment
            integer max_evr, max_gti
C
C       WARNING: THE VALUE OF MAX_EVR MUST MATCH THE VALUE OF
C                MAX_CUREVR IN THE DATAIN SUBROUTINE!!!
C
        parameter (max_evr=80000, max_gti=500)
      parameter (maxchan=256, maxtheta=20)
      parameter (mx_history = 50, mx_comment=200)
c Internals
c ... parameters & static arrays
      integer ntheta, clenact
      integer status, detchans
      integer fchan, nchans
      integer ounit, extn
        integer nk_hist, nk_comm, dtype
      logical qgroup, qqual, qsys, qerror 
      character(30) backfil, corrfil, respfil, ancrfil
      character(30) chantyp, instrume, filter
      real corrscal, backscal
      real ftsver
      real thet_min(maxtheta), thet_max(maxtheta)
      real oahist(maxtheta)
      character(40) errstr, wrnstr
      character(70) hist(mx_history), comment(mx_comment)
      character(80) message, badname(3)
      logical qokfil
      real live_time, area
      character(20) hduclas2, ftype
c ...... VARIABLES RELATED TO EVENT RATE DATA ...
            integer n_evr, n_evr_rbn
c ...... VARIABLES RELATED TO GTI DATA ...
            integer n_gti
            double precision start(max_gti),stop(max_gti)       
c ... pointers to "arrays" to be dynamically allocated
        integer p_channel, p_qualty, p_grping
            integer p_tim_evr, p_iac_evr
c      integer p_iqe_evr
c            integer p_ia1_evr, p_iax_evr
      integer p_bspec, p_berr, p_syserr
c ... "arrays" to be dynamically allocated
c       integer channel(maxchan)        integer qualty(maxchan)
c       integer grping(maxchan)         
c            real*8 tim_evr(max_evr)            integer iac_evr(max_evr)
c      integer iqe_evr(max_evr)      integer ia1_evr(max_evr)
c      integer iax_evr(max_evr)                  
c      real bspec(maxchan)            real berr(maxchan)
c      real syserr(maxchan)
C WMAP variables
        integer wmap,wsize
        double precision opticx,opticy,deltx,delty
C The PHA common block variables
      character(8) pha_telescop,pha_instrume,pha_detnam,pha_filter
      integer pha_unit,phatyp,nx,ny
      character(68) obs_date, obs_time
      common /pha_block/ pha_unit,pha_telescop, pha_instrume,
     $     pha_detnam, pha_filter, phatyp, obs_date, obs_time,
     $     nx,ny


c Initialize
      ierr = 0
      errstr = '** DO_PCPARPHA '//version//' ERROR:'      
      wrnstr = '** DO_PCPARPHA '//version//' WARNING:'      


c Give user info if requested
        if(chatter.GE.20) then
           message = ' ... using DO_PCPARPHA '// version
         call fcecho(message)
           call ftvers(ftsver)
           write(message,'(a,f6.3)')
     &          ' ... using FITSIO Version ', ftsver
           call fcecho(message)
        endif


        status = 0

c Check that the o/p file doesn't already exist or is illegal
c PDW 6/30/98: Change fcpars to ftrtnm
c        call fcpars(evrfil,badname(1),extn,status)
c        call fcpars(gtifil,badname(2),extn,status)
c        call fcpars(oahfil,badname(3),extn,status)
        call ftrtnm(evrfil,badname(1),status)
        call ftrtnm(gtifil,badname(2),status)
        call ftrtnm(oahfil,badname(3),status)
        call ck_file(outfil,badname, 3, qokfil, killit,chatter)
        if(.NOT.qokfil) then
                message = ' ... Offending file is OUTFIL: '// outfil
                call fcecho(message)
                ierr = -1
                goto 482
        endif

c *****
c Allocate dynamic memory
        p_channel = 0
        p_qualty = 0
        p_grping = 0
        p_tim_evr = 0
        p_iac_evr = 0
        p_bspec = 0
        p_berr = 0
        p_syserr = 0
        status = 0
        call udmget(maxchan, 4, p_channel, status)
        call udmget(maxchan, 4, p_qualty, status)
        call udmget(maxchan, 4, p_grping, status)
        call udmget(max_evr, 7, p_tim_evr, status)
        call udmget(max_evr, 4, p_iac_evr, status)
        call udmget(maxchan, 6, p_bspec, status)
        call udmget(maxchan, 6, p_berr, status)
        call udmget(maxchan, 6, p_syserr, status)
        if(status.NE.0) then
                message = errstr// ' Failed to allocate Dynamic Memory'
                call fcecho(message)
                ierr = -1
                goto 482
        endif
c *****

c Fill in a few history & comment records
        hist(1) = ' ROSAT PSPC PHA Dataset constructed using'
     &          // ' PCPARPHA '//version
        nk_hist = 1
        comment(1) = 'PCPARPHA '//version//'Summary:'
        comment(2) = ' ... I/p files:'
        comment(3) = '  EVR file:      '//
     &          evrfil(:MIN(60,clenact(evrfil)))
        comment(4) = '  GTI file:      '//
     &          gtifil(:MIN(60,clenact(gtifil)))
        comment(5) = '  WMAP/OAH file: '//
     &          oahfil(:MIN(60,clenact(oahfil)))
      write(comment(6),'(a,i8)') 
     &            '  EVR file rebining (seconds): ', evrbin
      nk_comm = 6
      if(qint.AND.(oahfil.NE.'NONE')) then
         nk_comm = nk_comm + 1
         comment(nk_comm) = '  Area calculated from: '//
     &          oahfil(:MIN(60,clenact(oahfil)))
      else
         nk_comm = nk_comm + 1
         write(comment(nk_comm),'(a,f10.5)') 
     &      '  User-defined off-axis angle (arcmin): ', usr_thet
      endif
      if(qarea) then
         comment(nk_comm+1) = '  Area calculated assuming: '
         write(comment(nk_comm+2),'(a,i8)')
     &      '  No. pixels         : ', npixsou
         write(comment(nk_comm+3),'(a,f10.5)')
     &      '  Pixel size (arcmin): ', pixsize
         nk_comm = nk_comm + 3
      endif
      comment(nk_comm+1) = ' ... Following bkgd components included:'
      if(qint) then
        comment(nk_comm+2) = ' Internal:  YES'
      else
        comment(nk_comm+2) = ' Internal:  NO'
      endif
      if(qal) then
        comment(nk_comm+3) = ' Aluminium: YES'
      else
        comment(nk_comm+3) = ' Aluminium: NO'
      endif
      if(qext) then
        comment(nk_comm+4) = ' External:  YES'
      else
        comment(nk_comm+4) = ' External:  NO'
      endif
      nk_comm = nk_comm + 4

c Read in the EVR & GTI data
      call datain(chatter,
     &              evrfil,max_evr,n_evr,MEMD(p_tim_evr),
     &                MEMI(p_iac_evr),
     &                ftype, instrume, filter,
     &              gtifil,max_gti,n_gti,start,stop,
     &                ierr)
      if(ierr.NE.0) goto 482
      if(filter(:5).EQ.'BORON') then
            continue
c fix up a few checks regarding which components are necessary
      elseif(filter(:7).EQ.'UNKNOWN') then
            comment(nk_comm + 1)= wrnstr //' FILTER = UNKNOWN'
            call fcecho(comment(nk_comm + 1))
        comment(nk_comm + 2)= ' ...... continuing, assuming BORON '//
     &                  'filter NOT in focal plane.'
              call fcecho(comment(nk_comm + 2))
            nk_comm = nk_comm + 2
      endif

      nk_comm = nk_comm + 1
      write(comment(nk_comm),'(a,i8)')
     &  ' ... No. EVR records read from EVRFIL:    ',n_evr
      if(chatter.GE.9) call fcecho(comment(nk_comm))

c Filter the EVR array using GTIs
      call filtevr(chatter, n_evr, MEMD(p_tim_evr),
     &              MEMI(p_iac_evr), n_gti,start, stop, ierr)
      if(ierr.NE.0) goto 482

      nk_comm = nk_comm + 1
      write(comment(nk_comm),'(a,i8)')
     &      ' ... No. EVR records after GTI filtering: ',n_evr
      if(chatter.GE.9) call fcecho(comment(nk_comm))

c Calc the mean MV rates using user-defined bin sizes
      if(chatter.gt.5) then
           write(message,'(a,i8)') 
     &       ' ... Using EVR file rebining factor: ', evrbin
           call fcecho(message)
      endif
      call rbnevr(chatter, n_evr, evrbin, MEMD(p_tim_evr),
     &              MEMI(p_iac_evr), n_gti, start, stop,
     &                  n_evr_rbn, ierr)

      nk_comm = nk_comm + 1
      write(comment(nk_comm),'(a,i8)')
     &      ' ... No. EVR records after GTI rebinning: ',n_evr_rbn
      if(chatter.GE.15) call fcecho(comment(nk_comm))

      n_evr = n_evr_rbn


      if(qint.AND.(oahfil.NE.'NONE')) then
C     Check out the pha file (fills pha_block COMMON)
           call ck_pha(chatter,oahfil,ierr)
           if(phatyp.eq.1) then
C     get WMAP and create an OAH
              wsize=nx*ny
              call aloc1a(wsize,wmap,ierr)
              call gt_wmap(chatter,pha_unit,nx,ny,MEMD(wmap),
     $             opticx,opticy,
     $             deltx,delty,ierr)
              call wmap2oah(pha_unit,chatter,MEMD(wmap),nx,ny,
     $             opticx,opticy,deltx,delty,
     $             ntheta,thet_min,thet_max,oahist,area,ierr)
              instrume=pha_instrume
              filter=pha_filter
           else
C     Close OAH file since gt_oahist expects this
              call ftclos(pha_unit,ierr)
              call gt_oahist(chatter, oahfil, maxtheta,
     &             ntheta, thet_min, thet_max,
     &             oahist, area, 
     &             instrume, filter, ierr)
              if(ierr.NE.0) goto 482
           endif
        else
           ntheta = 1
           oahist(1) = 1.0
           thet_min(1) = usr_thet
           thet_max(1) = usr_thet
           area = 1.0
      endif

c Make sure will figure out the area (in square degrees)
      if(qarea) then
        area = npixsou * (pixsize*pixsize)
      else
        if(area.LE.0.0) then
          message = errstr// ' Zero or -ve area'
          call fcecho(message)
            message = ' ... probably because following keywords '//
     &            'not in Off-axis histogram:'
          call fcecho(message)
          message = ' ...... NPIXSOU and/or PIXSIZE '
          call fcecho(message)
          message = ' ...... these could be added to the file '//
     &            'using FMODHEAD '
          call fcecho(message)
          message = ' ...... or PCPARPHA run again specifying the '//
     &            'parameter values on the command line '
          call fcecho(message)
          ierr = 1
          goto 482
        endif
      endif

c Do the job
      call do_pluc(chatter, nk_hist,hist,nk_comm,comment, 
     &            qint, qal, qext, ntheta,
     &            thet_min, thet_max, oahist,area, 
     &          n_evr, MEMD(p_tim_evr), MEMI(p_iac_evr),
     &          n_gti,start,stop,instrume,live_time,evrbin,
     &            nchans, fchan, detchans, MEMI(p_channel),
     &          MEMR(p_bspec),MEMR(p_berr), MEMR(p_syserr),
     &            ierr)



c Open & Write the FITS file
      status = 0
c ------------------------ PRIMARY HEADER ---------------------------
      if(phaversn(1:1).EQ.'1') then

c             ... Open the FITS file and write a null primary header
            call opnpa(outfil, chatter, ounit, killit,status)
            if(status.NE.0) then
                  ierr = status
                  goto 482
            endif

c             ... Add additional keywords to Primary Header
              call FTPKYS(ounit,'CONTENT',
     &                'PHA SPECTRUM',
     &             'SPECTRUM xtens (at least) present',
     &                status)

                call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)
       endif
c ------------------------ finished PRIMARY ---------------------------

      hduclas2 = 'BGD'
      qgroup = .false.
      qqual = .false.
      qsys = .false.
      backfil = 'NONE'
      corrfil = 'NONE'
      respfil = 'NONE'
      ancrfil = 'NONE'
      chantyp = 'PI'
      corrscal = 1.0
      backscal = 1.0
      dtype = 2


c ------------------------ SPECTRUM EXTENSION ----------------------------
      status=0
      if(phaversn(1:1).EQ.'1') then
                  call wtpha1(ounit,chatter,nk_hist,hist,nk_comm,
     &                  comment,'ROSAT',instrume,'NONE',filter,
     &                  phaversn,hduclas2,fchan,
     &                  live_time,area,backfil,backscal,corrfil,
     &                  corrscal,respfil,ancrfil,detchans,chantyp,
     &                  MEMI(p_channel),MEMR(p_bspec),dtype,qerror,
     &                  MEMR(p_berr), qsys,
     &                  MEMR(p_syserr),qqual,MEMI(p_qualty),qgroup,
     &                  MEMI(p_grping),nchans,
     &                  status)
            if(status.NE.0) then
                  ierr = 2
                  goto 482
            endif
                call FTPKYS(ounit,'CREATOR',
     &                  taskname,
     &             's/w task which wrote this dataset',
     &                  status)
      else
            message = errstr // 'Unknown format: '// phaversn
            call fcecho(message)
            ierr = 1
            goto 482
      endif
c ------------------------ finished SPECTRUM EXTENSION -------------------

c Close the FITS file
        call ftclos(ounit, status) 
      if(status.ne.0) then
            message = wrnstr // ' Problem closing file'
            call wt_ferrmsg(status, message)
      endif

c Check of errors
482      if(ierr.ne.0) then
            message = errstr // ' Fatal'
            call fcecho(message)
      endif

c *****
c Free the dynamic Memory
        call udmfre(p_channel, 4, status)
        call udmfre(p_qualty, 4, status)
        call udmfre(p_grping, 4, status)
        call udmfre(p_tim_evr, 7, status)
        call udmfre(p_iac_evr, 4, status)
      call udmfre(p_bspec, 6, status)
      call udmfre(p_berr, 6, status)
        call udmfre(p_syserr, 6, status)
        if(status.NE.0) then
                message = errstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
                ierr = 99
        endif

c *****

      return
      end
c -----------------------------------------------------------------
*+ DATAIN
      subroutine datain(chatter,
     &              evrexp,max_evr,n_evr,tim_evr,iac_evr,
     &                ftype, instrume, filter,
     &              gtiexp,max_gti,n_gti,start,stop,
     &                errflg)

      IMPLICIT NONE
      integer chatter, errflg
            character*(*) evrexp
      character*(*) instrume, filter
      character*(*) ftype
            integer max_evr,n_evr
            double precision tim_evr(max_evr)
            integer iac_evr(max_evr)
            character*(*) gtiexp
            integer*4 max_gti,n_gti
            real*8 start(max_gti),stop(max_gti)       

c DESCRIPTION 
c    This subroutine reads the EVR & GTI data inputs required for the 
c PCPARPHA task. 
c
c PASSED PARAMETERS
c
c
c CALLED ROUTINES 
c  RDGTI1                  : (callib) Reads GTI file
c  FTOPEN                  : (FITSIO) Opens FITS file
c  FTCLOS                  : (FITSIO) Closes FITS file
c
c AUTHORS/MODIFICATION HISTORY 
c  Ian M George     (1.0.1: 93 Oct 20), original
c  Ian M George     (2.0.0:1994 Aug 15), MVEXT change
c
c Banashree M Seifert (Oct 9, 1996) 2.1.0:
c         . changes made for LINUX
c           nsearch =maxextn instead of 999
c         . replaced by screen display routines wt*
c toliver (August 3, 1999) 2.2.0:
c         . increased max_curevr to match value of max_evr,
c           eliminating the posibilty of writing outside of
c           bounds of the tim_curevr and iac_curevr arrays 
c -----------------------------------------------------------------------

            character(7) subname
            parameter (subname='datain')
            character(5) version
            parameter (version='2.2.0')
*-
c Max array sizes etc
      integer maxextn
      parameter (maxextn = 99)
c INTERNALS ...
      integer i, extn, clenact, ninstr, nsearch
      integer next(maxextn)
        character(20) instr(9)
      character(10) extname
            character(80) message
        character(20) outhdu(9,maxextn), outver(9,maxextn)
      character(20) extnam(maxextn)
            character(25) errstr,wrnstr
      character(80) evrfil
      character(30) telescop, detnam
            integer status,iunit
c       ...LOCAL VARIABLES FOR READING EVENT RATE DATA
            integer max_curevr,n_curevr
C
C           WARNING: THIS NEEDS TO EQUAL THE VALUE OF MAX_EVR!!!!!!
C
            parameter (max_curevr = 80000)
            double precision tim_curevr(max_curevr)
            integer iac_curevr(max_curevr)
      logical end_evr
c Initialize
      errflg = 0 
      end_evr = .false.
            errstr = '** DATAIN '//version//' ERROR: '
            wrnstr = '** DATAIN '//version//' WARNING: '

c Lob out some info to the user, if required
            message = 'using '//subname//' Ver '//version
            call wtinfo(chatter,10,2,message)               


c ---------------------------------------------------------------------
c                        EVENT RATE FILE 
c ---------------------------------------------------------------------
         message = 'processing the EVR file'
         call wtinfo(chatter,5,2,message)               
c ----------------------------- RDF format ----------------------------
c ... attempt to read as RDF format
         message = 'checking for RDF format'
         call wtinfo(chatter,15,2,message)               

          errflg=0
          extname='EVRAT'
          ninstr = 2
          instr(1) = 'TEMPORALDATA'
          instr(2) = 'EVRATE'
          nsearch = maxextn 
        call mvext(0,evrexp,iunit, ninstr,instr,nsearch,
     &            next,outhdu, extnam, outver, instr(2), 
     &            errflg, chatter)

        if(errflg.NE.0) then
          if(errflg.EQ.3) then
            message = 'EVRFIL not RDF format'
            call wtwarm(subname,version,chatter,15,message)
            goto 111
            else
            goto 113
          endif
        endif

c Read in the data
c          call gtevrdf(iunit,n_curevr,max_curevr,tim_curevr,
c     &                iac_curevr,
c     &                  telescop,instrume,detnam,filter,
c     &                chatter,errflg)
C Changed L.Brown 8/29/94
          call gtevrdf(iunit,n_evr,max_curevr,tim_evr,
     &         iac_evr,
     &         telescop,instrume,detnam,filter,
     &         chatter,errflg)
          
          if(errflg.NE.0) then
             message = 'Problem reading EVRFIL as RDF format'      
             call wtinfo(chatter,20,2,message)               
          goto 111
      endif
      ftype = 'RDF'
      if(telescop.NE.'ROSAT'.OR.instrume(:4).NE.'PSPC')then
        message = 'EVR dataset from Unexpected instrument'
        call wtwarm(subname,version,chatter,0,message)
        message = 'TELESCOP keyword : '//
     &                  telescop(:MIN(40,clenact(telescop)))
        call wtinfo(chatter,0,1,message)
        message = 'INSTRUME keyword : '//
     &                  instrume(:MIN(40,clenact(instrume)))
        call wtinfo(chatter,0,1,message)
        message = 'Expected         : ROSAT'
        call wtinfo(chatter,0,1,message)
        message = 'Expected         : PSPCB or PSPCC'
        call wtinfo(chatter,0,1,message)
        message = 'continuing, assuming you know '//
     &            'what you are doing ..'
        call wtinfo(chatter,0,2,message)
      endif
      if(detnam.NE.'NONE') then
         message = 'EVR dataset from Unknown subinstr'
         call wtwarm(subname,version,chatter,0,message)
         message = 'DETNAM keyword   : '//
     &                  detnam(:MIN(40,clenact(detnam)))
         call wtinfo(chatter,0,2,message)
         message = 'which will be completely ignored'
         call wtinfo(chatter,0,2,message)
      endif
      goto 113

c ----------------------------- US Rev 0 format ----------------------------
c ... attempt to read as US Rev0 format
111      message = 'checking for US Rev0 format'      
         call wtinfo(chatter,20,2,message)

      call fcpars(evrexp, evrfil, extn,status)
      if(status.NE.0) then
        message = 'Problem parsing expression:'
        call wtwarm(subname,version,chatter,0,message)
        message = ' '//evrexp(:MIN(50,clenact(evrexp)))
        call wtinfo(chatter,0,2,message)
        message = 'will search all extensions'
        call wtinfo(chatter,0,2,message)
        extn = -99
      endif
      if(extn.GE.0) then
         message =  'Ignoring Extn no. provided'
         call wtwarm(subname,version,chatter,0,message)
      endif
        end_evr = .false.
        do WHILE (.NOT.end_evr)             
          call gtevru0(iunit,n_curevr,max_curevr,tim_curevr,
     &                iac_curevr,
     &                chatter,errflg)
          IF ((errflg.EQ.107).OR.(errflg.EQ.207)) THEN
            end_evr = .true.
            errflg = 0
          ELSEIF (errflg.NE.0) THEN
             message = 'EVRFIL not US Rev0 format'      
             call wtinfo(chatter,20,2,message)
          goto 112
          ENDIF
      
          IF (.NOT.end_evr) THEN
           do i=1,n_curevr
            tim_evr(i+n_evr) = tim_curevr(i)
            iac_evr(i+n_evr) = iac_curevr(i)
           enddo
           n_evr = n_evr + n_curevr
          ENDIF
        enddo
      if(n_evr.LE.0) then
             message = 'No EVR events found'
             call wtinfo(chatter,20,2,message)
             message = 'assuming EVRFIL not US Rev0 format'      
             call wtinfo(chatter,20,2,message)
          goto 112
      endif
      ftype = 'US Rev0'
      message = 'EVR dataset in US Rev 0 format'
      call wtwarm(subname,version,chatter,0,message)
      message = 'No info available regarding instrument/filter in use'
      call wtinfo(chatter,0,0,message)
      message = 'continuing, assuming you know what you are doing ..'
      call wtinfo(chatter,0,3,message)
      instrume = 'UNKNOWN'
      filter = 'UNKNOWN'          
      goto 113
c ----------------------------- German Rev 0 format ----------------------------
c ... attempt to read as German Rev0 format
112   message = 'checking for German Rev0 format'      
      call wtinfo(chatter,20,3,message)

      if(extn.GE.0) then
         message = 'Ignoring Extn no. provided'
         call wtwarm(subname,version,chatter,0,message)
      endif
        end_evr = .false.
        do WHILE (.NOT.end_evr)             
          call gtevrd0(iunit,n_curevr,max_curevr,tim_curevr,
     &                iac_curevr,
     &                chatter,errflg)
          IF ((errflg.EQ.107).OR.(errflg.EQ.207)) THEN
            end_evr = .true.
            errflg = 0
          ELSEIF (errflg.NE.0) THEN
             message = 'EVRFIL not German Rev0 format'      
             call wtinfo(chatter,20,3,message)
          goto 114
          ENDIF
          IF (.NOT.end_evr) THEN
           do i=1,n_curevr
            tim_evr(i+n_evr) = tim_curevr(i)
            iac_evr(i+n_evr) = iac_curevr(i)
           enddo
           n_evr = n_evr + n_curevr
          ENDIF
          if(n_evr.gt.max_evr) then
             message='MAX_EVR size too small, change program'
             call wtwarm(subname,version,chatter,0,message)
          endif
        enddo
      if(n_evr.LE.0) then
             message = 'No EVR events found'
             call wtinfo(chatter,20,2,message)
             message = 'assuming EVRFIL not German Rev0 format'      
             call wtinfo(chatter,20,2,message)
          goto 114
      endif
      ftype = 'German Rev0'
      message = 'EVR dataset in German Rev 0 format'
      call wtwarm(subname,version,chatter,0,message)
      message = 'No info available regarding instrument/filter in use'
      call wtinfo(chatter,0,1,message)
      message = 'continuing, assuming you know what you are doing ..'
      call wtinfo(chatter,0,2,message)
      instrume = 'UNKNOWN'
      filter = 'UNKNOWN'          
      goto 113
c ---------------------------------------------------------------------

114    message = 'Unable to identify EVR format'
      call wterrm(subname,version,message)
      errflg = 99


c CLOSE THE EVENT RATE FILE
113     if(errflg.EQ.0) then
            message = 'Successfully read the Events data'
            call wtinfo(chatter,5,1,message)
        else
            message = 'Unable to read the Events data'
            call wtinfo(chatter,5,1,message)
        endif

      status = 0                       
            call ftclos(iunit,status)
            message = 'closing Event Rate file'
            call wtferr(subname,version,status,message)       

c Go home if there is a problem
      if(errflg.NE.0) then
            return
      endif

c ---------------------------------------------------------------------
c                        GTI FILE 
c ---------------------------------------------------------------------
c Fix up the arrays if no GTI file specified
      if(gtiexp.EQ.'NONE') then
        n_gti = 1
        start(1) = tim_evr(1)
        stop(1) = tim_evr(n_evr)+1
        goto 765
      endif

c OK, so we think we have a GTI file
         message = 'processing the GTI file'
         call wtinfo(chatter,5,1,message)

c ----------------------------- RDF format ----------------------------
c ... attempt to read as RDF format
            message = 'checking for RDF format'
            call wtinfo(chatter,15,2,message)
            call fcecho(message)
          ninstr = 1
          instr(1) = 'GTI'
          nsearch = 99
        call mvext(0,gtiexp,iunit, ninstr,instr,nsearch,
     &            next,outhdu, extnam, outver, 'STDGTI', 
     &            errflg, chatter)
        if(errflg.NE.0) then
          if(errflg.EQ.3) then
             message = 'GTIFIL not RDF format'
             call wtinfo(chatter,15,2,message)
            goto 211
            else
            goto 213
          endif
        endif

c Read in the data
      call rdgti1(iunit,n_gti,max_gti,start,stop,chatter,errflg)
      if(errflg.NE.0) then
         message = 'Problem reading GTIFIL as RDF format'      
         call wtinfo(chatter,15,2,message)
         goto 211
      endif
      goto 213
c ----------------------------- US Rev 0 format ----------------------------
c ... attempt to read as US Rev0 format
211      message = 'checking for US Rev0 format'      
         call wtinfo(chatter,15,2,message)
           ninstr = 1
           instr(1) = '*'
           nsearch = 1
          nsearch = 99
        call mvext(0,gtiexp,iunit, ninstr,instr,nsearch,
     &            next,outhdu, extnam, outver, 'GTI', 
     &            errflg, chatter)
        if(errflg.NE.0) then
          if(errflg.EQ.3) then
             message = 'GTIFIL not US Rev0 format'      
             call wtinfo(chatter,15,2,message)
            goto 214
            else
            goto 213
          endif
        endif
c Read in the data
        call rdgti1(iunit,n_gti,max_gti,start,stop,chatter,errflg)
      if(errflg.NE.0) then
         message =  'Problem reading GTIFIL as US Rev0 format'      
         call wtinfo(chatter,15,2,message)
         call fcecho(message)
         goto 214
      endif
      goto 213
c -------------------------------------------------------------------

214    message = 'Unable to identify GTI format'
      call wterrm(subname,version,message)
      errflg = 99


c CLOSE THE GTI FILE
213     if(errflg.EQ.0) then
            message = 'Successfully read the GTI data'
            call wtinfo(chatter,5,1,message)
        else
            message = 'Unable to read the GTI data'
            call wtinfo(chatter,5,1,message)
        endif

        status = 0
             call ftclos(iunit,status)
             message = 'closing GTI file'
             call wtferr(subname,version,status,message)

c Go home if there is a problem
      if(errflg.NE.0) then
            return
      endif

765      continue     
          
         message = 'DATAIN completed successfully'
         call wtinfo(chatter,20,1,message)

482        return
            end
c ----------------------------------------------------------------
*+ GT_OAHIST
         subroutine gt_oahist(chatter, oahexp, maxradii, 
     &            nrad, rad_lo, rad_hi, 
     &            oahist, area, instrume, filter, ierr)

      IMPLICIT NONE 
      integer chatter, nrad, ierr, maxradii
      real rad_lo(*), rad_hi(*), oahist(*)
      real area
      character*(*) oahexp, instrume, filter
c Description
c  This guy reads in the Off-axis histogram from the DETECTOR extension 
c of a ROSAT PSPC PHA file.
c
c Author/Modification History
c  Ian M George (1.0.0: 1994 Mar 15), original
c
c Banashree M Seifert (Oct 9, 1996) 1.1.0:
c      . nsearch= maxextn instead of 999
c --------------------------------------------------------------------
      character(7) version
      parameter (version = '1.1.0')
*-
c Max array sizes etc
        integer maxextn
        parameter (maxextn = 99)
c Internals 
      integer iunit, status, block, extn, nfound, htype
      integer detchans, npixsou, ninstr, nsearch
      integer next(maxextn), imove, jj, i, clenact
        character(20) outhdu(9,maxextn), outver(9,maxextn)
        character(20) extnam(maxextn)
      real dmyhist(100), dmyreal, texpos, pixsize
      character(30) errstr, wrnstr, comm
      character(20) instr(9)
      character(30) dmystr, telescop
      character(80) message, oahfil
c Initialize
      ierr = 0
      extn = -99
      errstr = '** GT_OAHIST '//version//' ERROR:'
      wrnstr = '** GT_OAHIST '//version//' WARNING:'

c Chuck out some user info, if requested
      if(chatter.GE.15) then
        message = ' ... using GT_OAHIST Version '//version
        call fcecho(message)
      endif

c Parse the supplied filenames, stripping off incld extension numbers
        status=0
        call fcpars(oahexp,oahfil,extn,status)
        if(status.NE.0) then
          message = wrnstr // ' Problem parsing the expression:'
          call fcecho(message)
          message = ' ......    '//oahexp(:MIN(50,clenact(oahexp)))
          call fcecho(message)
          message = ' ...... will search all extensions'
          call fcecho(message)
          extn = -99
        endif

c Open the file
      call cgetlun(iunit)
      status = 0
      call ftopen(iunit,oahfil,0,block,status)
      message = errstr//' Opening Off-axis histogram file'
      call wt_ferrmsg(status,message)
      IF (status.NE.0) THEN
        ierr = 1
        return
      ENDIF

c FIND & READ THE EXTENSION
c       ... Extension number NOT given as part of gtiexp 
        if(extn.LT.0) then
          ninstr = 2
          instr(1) = 'RESPONSE'
          instr(2) = 'DETECTOR'
          nsearch = maxextn 
          call fndhdu(chatter, iunit, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam,ierr)
c         ... check for old-style EXTNAME values if no OK HDUCLASn values found
          if(nfound.LE.0) then
         if(chatter.GT.20) then
            message = wrnstr//
     &          ' Ext w/ allowed HDUCLASn keywrds not found'
            call fcecho(message)
            message = ' ... offending file: '//oahfil
            call fcecho(message)
            message = 
     &       ' ... searching for extnsion with EXTNAME = DETECTOR'
            call fcecho(message)
         endif
            call fndext(chatter, iunit, 'DETECTOR',
     &          nsearch, nfound, next, outhdu, outver, extnam,ierr)
          endif

c       ... Extension number IS given as part of oahfil
        else
           call ftmahd(iunit,extn+1,htype,status)
           message = wrnstr // ' Problem moving to specified xtens'
           call wt_ferrmsg(status, message)
c          ... grab the HDUCLAS values for reference
           ninstr = 1
           instr(1) = '*'
           nsearch = 1
           call fndhdu(chatter, iunit, ninstr, instr,
     &          nsearch, nfound, next, outhdu, outver, extnam, ierr)
           nfound = 1
           next(1) = 0
        endif

c       ... sort out what we've got
        if(nfound.GT.1) then
            message = errstr//' OAHFIL contains >1 DETECTOR datasets'
            call fcecho(message)
            write(message,'(a,i2,a)') ' ',nfound,' extensions found:'
            call fcecho(message)
            do i = 1, nfound
                write(message,'(a,i2,a)') '...... Ext ',next(i),':'
                call fcecho(message)
                write(message,'(6X,a,a)') 'EXTNAME = ', extnam(i)
                do jj = 1, 4
                  write(message,'(6X,a,i2,2a)')
     &                  'HDUCLAS',jj,' = ',outhdu(i,jj)
                  call fcecho(message)
                enddo
            enddo
            message = ' ... Extension number must be specified' //   
     &  'via oahfil param'
            call fcecho(message)
            ierr = 2
          return
        endif

c       ... Move to the Extension if not already there
        if(next(1).GT.0) then
           imove = next(1)
           status = 0
           call ftmrhd(iunit,imove,htype,status)
         if(status.NE.0) then
            message = wrnstr // ' Problem moving to DETECTOR xtens'
            call wt_ferrmsg(status, message)
         elseif(chatter.gt.25) then
            message = ' ...... Moved to DETECTOR xtens'
            call fcecho(message)
         endif
        endif

c Read in the data
      call rdoht1(iunit,telescop,instrume,filter,
     &                  texpos, dmyreal, detchans,dmystr,nrad, 
     &                  rad_lo, rad_hi,oahist, dmyhist, ierr, 
     &                  chatter)
      if(ierr.NE.0) goto 998

c Read in the other keywords necessary
      status = 0
        call FTGKYJ(iunit,'NPIXSOU ',
     &          npixsou,
     &          comm,
     &          status)
        if(status.NE.0) then
        npixsou = 0
          status = 0
        endif
        call FTGKYE(iunit,'PIXSIZE ',
     &          pixsize,
     &          comm,
     &          status)
        if(status.NE.0) then
        pixsize = 0
          status = 0
        endif
      area = npixsou * (pixsize*pixsize)


      call ftclos(iunit, status)
      if(status.NE.0) then
        message = errstr//' Closing Off-axis Histogram file'
        call wt_ferrmsg(status, message)
        ierr = 2
      endif
      
998      if(ierr.NE.0)then
        message = errstr//' Incomplete execution'
        call fcecho(message)
      endif
        
      return
      end
c ----------------------------------------------------------------
*+GTEVRDF
      subroutine gtevrdf(iunit,n_evr,max_evr,tim_evr,iac_evr,
     &                telescop, instrume,detnam,filter,
     &                        chatter,ierr)

            IMPLICIT NONE
            integer iunit,max_evr,n_evr,ierr,chatter
      double precision tim_evr(max_evr)
            integer iac_evr(max_evr)
      character*(*) telescop, instrume, detnam, filter

c Description
c  This subroutine reads a FITS RDF format Events extension
c  NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c  The following columns (only) are read & returned:
c       TIME      : Time of event rate
c       IAC_EVR   : MV anticoincident rate
c  The following header keywords are read & returned:
c       {none}
c
c Passed Parameters
c  max_evr    int    : Array dimensions
c  iunit      int    : Fortran unit number for file
c  chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c  n_evr      int    : Counter event rate data 
c  ierr       int    : Error flag, ierr = 0 okay
c
c Called Routines 
c subroutine FTMAHD      : FITSIO routine to move to extension header
c subroutine FTGKYj      : FITSIO routine to read extension header keyword,
c subroutine FTGKNS      : FITSIO routine to read extension header keyword,
c subroutine FCECHO      : FTOOLS routine to write to screen
c subroutine WT_FERRMSG  : Writes FITSIO error text if required
c
c Compilation and Linking 
c  Link with FTOOLS - FITSIO, CALLIB
c
c Authors/Modification History 
c  Ian M George (1.0.0:1994 Mar 16) 
c  Peter D Wilson (1.0.1:1998 Feb 09): Call ftgcvd with dnull, not inull    
       character(5) version
       parameter (version = '1.0.1' )
*-
c Internals
            character(30) errstr,wrnstr
            character(80) message
            character(40) comm
            integer status,colnum
            integer felem,inull,frow
            logical anyflg,foundcol
            double precision dnull
c Initialize
       ierr = 0
       errstr =' ** GTEVRDF '// version // ' ERROR:'
       wrnstr =' ** GTEVRDF '// version // ' WARNING:'

c User info
       IF (chatter.GE.15) THEN
         message =' ... using GTEVRDF Ver '//version
         call fcecho(message)
       ENDIF 

c Read Keywords
c ... NAXIS2 
       status = 0
       call ftgkyj(iunit,'NAXIS2',n_evr,comm,status)
       IF (status.NE.0) THEN
              message = errstr//' reading NAXIS2'
              call wt_ferrmsg(status,message)
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
         write(message,'(A,i8)')
     &  '   ... Number of records found = ',n_evr
         call fcecho(message)
       ENDIF

c check that array dimensions are large enough 
       IF (n_evr.GT.max_evr) THEN
         message = errstr//' array dimensions are too small !'
         call fcecho(message)
         ierr = 5
         return
       ENDIF

c ... TELESCOP
       status = 0
       call ftgkys(iunit,'TELESCOP',telescop,comm,status)
       IF (status.NE.0) THEN
         message = errstr//' reading TELESCOP keyword'
         call wt_ferrmsg(status,message)
       message = ' ...... Setting TELESCOP to UNKNOWN'
       call fcecho(message)
       telescop = 'UNKNOWN'
       status = 0
       ENDIF
c ... INSTRUME
       status = 0
       call ftgkys(iunit,'INSTRUME',instrume,comm,status)
       IF (status.NE.0) THEN
         message = errstr//' reading INSTRUME keyword'
         call wt_ferrmsg(status,message)
       message = ' ...... Setting INSTRUME to UNKNOWN'
       call fcecho(message)
       instrume = 'UNKNOWN'
       status = 0
       ENDIF
c ... DETNAM
       status = 0
       call ftgkys(iunit,'DETNAM',detnam,comm,status)
       IF (status.NE.0) THEN
       detnam = 'NONE'
       status = 0
       ENDIF
c ... FILTER
       status = 0
       call ftgkys(iunit,'FILTER',filter,comm,status)
       IF (status.NE.0) THEN
         message = errstr//' reading FILTER keyword'
         call wt_ferrmsg(status,message)
       message = ' ...... Setting FILTER to UNKNOWN'
       call fcecho(message)
       filter = 'UNKNOWN'
       status = 0
       ENDIF


c Find/Read Data
c ... find TIME column 
       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TIME',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          message =errstr//'TIME column not present'
          call fcecho(message)
          ierr = 2
          return
       ENDIF
c ... read TIME column 
       frow=1
       felem=1
       dnull=0.D0
       status=0
           call ftgcvd(iunit,colnum,frow,felem,n_evr,dnull,tim_evr,
     &             anyflg,status)
           IF (status.NE.0) THEN
             message = errstr//' reading TIME column '
             call wt_ferrmsg(status,message)
             ierr = 3
             return
           ENDIF

c ... find MV_ACO column 
       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'MV_ACO',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          message=errstr//'MV_ACO column not present'
          call fcecho(message)
          ierr = 2
          return
       ENDIF      
c ... read MV_ACO column 
       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,iac_evr,
     &             anyflg,status)
       message = errstr//' reading MV_ACO column '
       call wt_ferrmsg(status,message)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c User info, if requested
       IF (chatter.GE.20) THEN
         message = '      ... EVR data read successfully'
         call fcecho(message)
       ENDIF

       return
       end

c ------------------------------------------------------------------------
c     END OF SUBROUTINE GTEVRDF
c ------------------------------------------------------------------------

*+GTEVRU0
      subroutine gtevru0(iunit,n_evr,max_evr,tim_evr,iac_evr,
     &                        chatter,ierr)

      IMPLICIT NONE
      integer iunit,max_evr,n_evr,ierr,chatter
      double precision tim_evr(max_evr)
      integer iac_evr(max_evr)

c Description
c  This subroutine reads a FITS US REV0 format Events extension
c  NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c  The following columns (only) are read & returned:
c       ITI_EVR   : Time of event rate
c       IAC_EVR   : MV anticoincident rate
c  The following header keywords are read & returned:
c       {none}
c
c Passed Parameters
c  max_evr    int    : Array dimensions
c  iunit      int    : Fortran unit number for file
c  chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c  n_evr      int    : Counter event rate data 
c                     (IF NE 0 on entry then that many evr entries already
c                      aaumed to have been read in (fron prevoius ext') 
c 
c  ierr       int    : Error flag, ierr = 0 okay
c
c Called Routines 
c subroutine FTMAHD      : FITSIO routine to move to extension header
c subroutine FTGKYj      : FITSIO routine to read extension header keyword,
c subroutine FTGKNS      : FITSIO routine to read extension header keyword,
c subroutine FCECHO      : FTOOLS routine to write to screen
c subroutine WT_FERRMSG  : Writes FITSIO error text if required
c
c Compilation and Linking 
c  Link with FTOOLS - FITSIO, CALLIB
c
c Authors/Modification History 
c  Rehana Yusaf (1.0.0:1993 Oct 01) original rd_evrusrev0 version
c  Ian M George (2.0.0:1994 Mar 16) renamed to gtevru0
c
c Banashree M Seifert (Oct 9, 1996) 2.1.0:
c           . format replaced insteda of i by i5 or i8 etc
c            
       character(5) version
       parameter (version = '2.1.0' )
      
*-
c Internals
      character(30) errstr,wrnstr
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      character(80) message
      integer iti_evr, i
      integer status,htype,colnum
      integer felem,inull,frow
      logical anyflg,foundcol,extfind,qfirst
c Initialize
       ierr = 0

c User info
       IF (chatter.GE.15) THEN
         subinfo =' ... using GTEVRU0 Ver '//version
         call fcecho(subinfo)
       ENDIF 
       IF (n_evr.LE.0) THEN
         qfirst = .true.
       ELSE
         qfirst = .false.
       ENDIF

c Move to the data extension
       errstr =' ** GTEVRU0 '// version // ' ERROR:'
       wrnstr =' ** GTEVRU0 '// version // ' WARNING:'
       extfind = .false.
       status=0
       do WHILE(.NOT.extfind)
         call ftmrhd(iunit,1,htype,status)
         extname = '  '
         call ftgkys(iunit,'EXTNAME',extname,comm,status)
         IF (extname.EQ.'EVRAT') THEN 
            extfind = .true.
         ELSE
            IF ((status.EQ.107).OR.(status.EQ.207)) THEN
              IF (qfirst) THEN
                errinfo = errstr//' "EVRAT" EXTENSION NOT FOUND '
                call fcecho(errinfo)
              ELSE
                IF (chatter.GE.20) THEN
                  subinfo = ' ... end of file encoutered'
                  call fcecho(subinfo)
                ENDIF
              ENDIF
              ierr=status
              return
            ENDIF
         ENDIF
      enddo  
      IF (chatter.GE.20) THEN
        IF (qfirst) THEN
          subinfo = '   ... moved to '//extname//'extension'
        ELSE
          subinfo = '   ... found another '//extname//'extension'
        ENDIF
        call fcecho(subinfo)
      ENDIF

c Read Keywords
c ... NAXIS2 
       status = 0
       call ftgkyj(iunit,'NAXIS2',n_evr,comm,status)
       errinfo = errstr//' reading NAXIS2'
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
         write(subinfo,'(A,i8)')
     &  '   ... Number of records found = ',n_evr
         call fcecho(subinfo)
       ENDIF

c check that array dimensions are large enough 
       IF (n_evr.GT.max_evr) THEN
         errinfo = errstr//' array dimensions are too small !'
         call fcecho(errinfo)
         ierr = 5
         return
       ENDIF

c Find/Read Data
c ... find ITI_EVR column 
       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'ITI_EVR',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'ITI_EVR column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF
c ... read ITI_EVR column 
       felem=1
       inull=0
       status=0
      do i = 1, n_evr
           call ftgcvj(iunit,colnum,i,felem,1,inull,iti_evr,
     &             anyflg,status)
           IF (status.NE.0) THEN
             message = errstr//' reading ITI_EVR column '
             call wt_ferrmsg(status,message)
             ierr = 3
             return
           ENDIF
         tim_evr(i) = dble(iti_evr)
      enddo

c ... find IAC_EVR column 
       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IAC_EVR',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'IAC_EVR column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF      
c ... read IAC_EVR column 
       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,iac_evr,
     &             anyflg,status)
       errinfo = errstr//' reading IAC_EVR column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c User info, if requested
       IF (chatter.GE.20) THEN
         subinfo = '      ... EVR data has been read'
         call fcecho(subinfo)
       ENDIF

       return
       end

c ------------------------------------------------------------------------
c     END OF SUBROUTINE GTEVRU0
c ------------------------------------------------------------------------

*+GTEVRD0
      subroutine gtevrd0(iunit,n_evr,max_evr,tim_evr,iac_evr,
     &                        chatter,ierr)

      IMPLICIT NONE
      integer iunit,max_evr,n_evr,ierr,chatter
      double precision tim_evr(max_evr)
      integer iac_evr(max_evr)

c Description
c  This subroutine reads a FITS GERMAN REV0 format Events extension
c  NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c  The following columns (only) are read & returned:
c       TIME      : Time of event rate
c       EE_MV     : MV anticoincident rate
c  The following header keywords are read & returned:
c       {none}
c
c Passed Parameters
c  max_evr    int    : Array dimensions
c  iunit      int    : Fortran unit number for file
c  chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c  n_evr      int    : Counter event rate data 
c                     (IF NE 0 on entry then that many evr entries already
c                      aaumed to have been read in (fron prevoius ext') 
c 
c  ierr       int    : Error flag, ierr = 0 okay
c
c Called Routines 
c subroutine FTMAHD      : FITSIO routine to move to extension header
c subroutine FTGKYj      : FITSIO routine to read extension header keyword,
c subroutine FTGKNS      : FITSIO routine to read extension header keyword,
c subroutine FCECHO      : FTOOLS routine to write to screen
c subroutine WT_FERRMSG  : Writes FITSIO error text if required
c
c Compilation and Linking 
c  Link with FTOOLS - FITSIO, CALLIB
c
c Authors/Modification History 
c  Ian M George (1.0.0:1994 Mar 20), original
c
c Banashree M Seifert (Oct 9, 1996) 1.1.0:
c           . format statement replaced by i8 instead of i

       character(5) version
       parameter (version = '1.1.0' )
      
*-
c Internals
      character(30) errstr,wrnstr
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      character(80) message
      integer iti_evr, i
      integer status,htype,colnum
      integer felem,inull
      logical anyflg,foundcol,extfind,qfirst
c Initialize
       ierr = 0
       errstr =' ** GTEVRD0 '// version // ' ERROR:'
       wrnstr =' ** GTEVRD0 '// version // ' WARNING:'

c User info
       IF (chatter.GE.15) THEN
         subinfo =' ... using GTEVRD0 Ver '//version
         call fcecho(subinfo)
       ENDIF 
       IF (n_evr.LE.0) THEN
         qfirst = .true.
       ELSE
         qfirst = .false.
       ENDIF

c Move to the data extension
       extfind = .false.
       do WHILE(.NOT.extfind)
         call ftmrhd(iunit,1,htype,status)
         extname = '  '
         call ftgkys(iunit,'EXTNAME',extname,comm,status)
         IF (extname.EQ.'EVRAT') THEN 
            extfind = .true.
         ELSE
            IF ((status.EQ.107).OR.(status.EQ.207)) THEN
              IF (qfirst) THEN
                errinfo = errstr//' "EVRAT" EXTENSION NOT FOUND '
                call fcecho(errinfo)
              ELSE
                IF (chatter.GE.20) THEN
                  subinfo = ' ... end of file encoutered'
                  call fcecho(subinfo)
                ENDIF
              ENDIF
              ierr=status
              return
            ENDIF
         ENDIF
      enddo  
      IF (chatter.GE.20) THEN
        IF (qfirst) THEN
          subinfo = '   ... moved to '//extname//'extension'
        ELSE
          subinfo = '   ... found another '//extname//'extension'
        ENDIF
        call fcecho(subinfo)
      ENDIF

c Read Keywords
c ... NAXIS2 
       status = 0
       call ftgkyj(iunit,'NAXIS2',n_evr,comm,status)
       errinfo = errstr//' reading NAXIS2'
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
         write(subinfo,'(A,i8)')
     &  '   ... Number of records found = ',n_evr
         call fcecho(subinfo)
       ENDIF

c check that array dimensions are large enough 
       IF (n_evr.GT.max_evr) THEN
         errinfo = errstr//' array dimensions are too small !'
         call fcecho(errinfo)
         ierr = 5
         return
       ENDIF

c Find/Read Data
c ... find TIME column 
       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TIME',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TIME column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF
c ... read TIME column 
       felem=1
       inull=0
       status=0
      do i = 1, n_evr
           call ftgcvj(iunit,colnum,i,felem,1,inull,iti_evr,
     &             anyflg,status)
           IF (status.NE.0) THEN
             message = errstr//' reading TIME column '
             call wt_ferrmsg(status,message)
             ierr = 3
             return
           ENDIF
         tim_evr(i) = dble(iti_evr)
      enddo


c ... find EE_MV column 
       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'EE_MV',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'EE_MV column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF      

c User info, if requested
       IF (chatter.GE.20) THEN
         subinfo = '      ... EVR data has been read'
         call fcecho(subinfo)
       ENDIF

       return
       end

c ------------------------------------------------------------------------
c     END OF SUBROUTINE GTEVRD0
c ------------------------------------------------------------------------

*+ DO_PLUC
      subroutine do_pluc(chatter, nk_hist,hist,nk_comm,comment,
     &            qint, qal, qext, ntheta,
     &            theta_min, theta_max, oahist, area, 
     &          n_evr, tim_evr, iac_evr,
     &          n_gti,start,stop,instrume, live_time,evrbin,
     &            nchan, fchan, detchan, chan,
     &          bspec,berr, syserr, ierr)
      IMPLICIT NONE
      integer nk_comm, nk_hist, evrbin
      integer chatter,ntheta, n_evr, n_gti, ierr
      integer nchan, fchan, detchan, chan(*)
      real theta_min(*), theta_max(*), oahist(*), area
      real  bspec(*), berr(*)
      integer iac_evr(*)
      real*8 tim_evr(*)
      real syserr(*), live_time
      character*(*) comment(*), hist(*), instrume
      double precision start(*), stop(*)
      logical qint, qal, qext
c Description
c  Subroutine to loop through off-axis angle, calculating the 
c ROSAT PSPC background spectrum according to the prescription 
c of Plucinsky etal. The off-axis histogram is used to weigth the 
c o/p
c
c Author/Modification History
c  Ian M George (1.0.0: 1994 Mar 15), original
      character(7) version
      parameter (version = '1.0.0')
*-
c Internals
      integer i, k
      real theta, talive
      real bcalc(256)
      character(30) errstr, wrnstr
      character(80) message
c Initialize
      ierr = 0
      nchan = 256
      detchan = 256
      fchan = 1
      errstr = ' ** DO_PLUC '// version// ' ERROR:'
      wrnstr = ' ** DO_PLUC '// version// ' WARNING:'

      write(message,'(a,i3)') 
     &            ' ... No. theta bins to process: ', ntheta
      call fcecho(message)
      write(message,'(a,i8)') 
     &            ' ... No. EVR events to process: ', n_evr
      call fcecho(message)

      nk_hist = nk_hist + 1
      hist(nk_hist) = ' BKGD SPECTRUM generated using '
     &            //'algorithm of Plucinsky etal'
      nk_comm = nk_comm+1
      comment(nk_comm) = ' Processing history'

      do i=1,256
         bspec(i) = 0.
         bcalc(i)=0.
      enddo
c LOOP over off-axis angles
      do i = 1, ntheta
          theta = (theta_max(i)+theta_min(i))/2.0
          if(oahist(i).EQ.0) then
            goto 849
          elseif(oahist(i).LT.0) then
            message = wrnstr//' Weighting < 0 {skipping}'
            call fcecho(message)
            goto 849
          endif
c ...      Perform the Particle Bkgd calculations
              call pluc1_bgd(chatter, qint, qal, qext, theta,
     &                n_evr, tim_evr, iac_evr,
     &                  n_gti,start,stop,instrume, talive,
     &                bcalc,ierr)
              if(ierr.NE.0) return

c ...   Correct for the fact that we fooled pluc1_bgd by rebining EVR
            talive = talive*evrbin

c ...       Sum, renormalizing by livetime, area & weighting factor
            do k = 1, 256
                  bspec(k) = bspec(k) 
     &                     + bcalc(k)*area*talive*oahist(i)
            enddo
            live_time = live_time + talive*oahist(i)            
c ...      ... user info
849            if(chatter.GE.5) then
             if(i.EQ.1) then
              write(message,'(31X,a)') 
     &                  ' theta      weight (%)'
              call fcecho(message)
             endif
             write(message,'(a,i3,a,g10.5,a,f10.6,a)') 
     &                  ' ...... finished theta bin ', i,
     &                  ' (',theta,',',oahist(i)*100,')'
             call fcecho(message)
            endif
         if(i.EQ.1) then
           nk_comm = nk_comm+1
           write(comment(nk_comm),'(a)') 
     &                  '   bin    theta      weight (%)'
         endif
         nk_comm = nk_comm+1
         write(comment(nk_comm),'(a,i3,6X,g10.5,f10.6)') 
     &                  ' ', i, theta, oahist(i)*100
      enddo

c Check for sillies
      if(live_time.LE.0) then
         write(message,'(a,a,G10.4)') 
     &            errstr,' calcd livetime: ',live_time
         call fcecho(message)
         ierr = 1
         return
      endif

c Divide through be exposure, fill background array etc 
      do k = 1, nchan
         bspec(k) = bspec(k)/live_time
         berr(k) = 0.0
         syserr(k) = 0.0
         chan(k) = k
      enddo

      write(comment(nk_comm+1),'(a,i3)')
     &            ' ... No. theta bins processed: ', ntheta
      write(comment(nk_comm+2),'(a,i8)')
     &            ' ... No. EVR events processed: ', n_evr
      nk_comm = nk_comm + 2

      return
      end
c -----------------------------------------------------------------
*+ FILTEVR
      subroutine filtevr(chatter, n_evr, tim_evr,
     &              iac_evr, n_gti,start, stop, ierr)

      IMPLICIT NONE
      integer chatter, n_evr, n_gti, ierr
      double precision tim_evr(*)
      integer iac_evr(*)
      double precision start(*),stop(*)

c Description
c  Filters the EVR dataset based upon the GTIs, returning the new 
c arrays of ITI_EVR & IAC_EVR, along with the new n_evr
c
c Passed Parameters
c
c Author/Modification History
c  Ian M George (1.0.0:1994 Apr 27) 
      character(7) version
      parameter(version ='1.0.0')
*-
c Internals 
      integer inside, istart
        integer i, j
      double precision curevr, curgti
      character(30) errstr
      character(80) message

c Initialize
      errstr = ' ** FILTEVR '// version// ' ERROR:'
      inside = 0
      ierr = 0


c Cop out if single GTI covering entire EVR dataset
      if((n_gti.EQ.1).AND.(start(1).LE.tim_evr(1))
     &            .AND.(stop(1).GE.tim_evr(n_evr)))
     &            return


c Main filtering
      curevr = 0
      curgti = 0
      istart = 1
      do j = 1, n_gti
         if(start(j).LT.curevr) then
            message = errstr//' GTI dataset not in time order'
            call fcecho(message)
            ierr = 1 
            return
         else
              curgti = start(j)
         endif
         do i = istart, n_evr
            if((inside.GE.1).AND.(tim_evr(inside).LT.curevr)) then
               message = errstr//' EVR dataset not in time order'
               call fcecho(message)
               write(message,'(a,i8)')
     &            ' ...... 1st element out of order: ', i
               call fcecho(message)
               write(message,'(a,f15.10)')
     &            ' ...... Time for this element:     ', tim_evr(inside)
               call fcecho(message)
               write(message,'(a,f15.10)')
     &            ' ...... Time for previous element: ', curevr
               call fcecho(message)
               ierr = 1 
               return
            elseif((tim_evr(i).GE.start(j)).AND.
     &                  (tim_evr(i).LE.stop(j))) then
                curevr = tim_evr(inside)
                inside = inside + 1
                tim_evr(inside) = tim_evr(i)
                iac_evr(inside) = iac_evr(i)
            elseif(tim_evr(i).GT.stop(j)) then
                istart = i
                goto 125
              endif            
         enddo
125      enddo

c Fix up the new counter
      n_evr = inside

      return
      end

c -----------------------------------------------------------------
*+ RBNEVR
        subroutine rbnevr(chatter, n_evr, evrbin, tim_evr,
     &              iac_evr, n_gti, start, stop, n_evr_rbn, ierr)

      IMPLICIT NONE
      integer chatter, n_evr, ierr, evrbin, n_evr_rbn
      integer n_gti
      double precision tim_evr(*), start(*), stop(*)
      integer iac_evr(*)

c Description
c  Calculates and returns mean MV rate & time using bins of size EVRBIN 
c SECONDS. Each GTI is done individually.
c
c Passed Parameters
c
c Author/Modification History
c  Ian M George (1.0.0:1994 Mar 20) 
c
c Banashree M Seifert (Oct 10 1996) 1.1.0:
c        . double and single precision was mixed
c          corrected  
c            end = beg + dble(evrbin)  
c -------------------------------------------------------------------
      character(7) version
      parameter(version ='1.1.0')
*-
c Internals 
      integer i, j, k, istart, inew
      integer inside, nbins
      real mv
      double precision time, beg, end
c Initialize
      ierr = 0
      inew=0
      inside=0
      time=0.d0
      mv = 0.0

c Get out, if silly
      if(evrbin.LE.1) return

c GTI DO-LOOP
      istart = 1
      do j = 1, n_gti
         nbins = INT((stop(j) - start(j))/REAL(evrbin)) + 1
         do k = 1, nbins
            beg = start(j) + (k-1)*evrbin 
            end = beg + dble(evrbin)  
            do i = istart, n_evr
            if((tim_evr(i).GE.beg).AND.
     &                  (tim_evr(i).LE.end)) then
                inside = inside + 1
                time = time + tim_evr(i)
                mv = mv + iac_evr(i)
            elseif(tim_evr(i).GT.end) then
                if((inside.GT.0).AND.(mv.GT.0)) then
                   inew = inew + 1
                   tim_evr(inew) = (beg+end)/2
                   iac_evr(inew) = mv/REAL(inside)
                endif
                inside = 0
                time = 0.d0
                  mv = 0.
                istart = i
                goto 125
              endif            
            enddo
125         enddo
      enddo

c Fix up the new counter
      n_evr_rbn = inew

      return
      end
c ----------------------------------------------------------------
c ----------------------------------------------------------------
*+PLUC1_BGD
            subroutine pluc1_bgd(chatter, qint, qal, qext, theta, 
     &            n_evr, tim_evr, iac_evr, 
     &            n_gti,start,stop,instrume, talive, 
     &            spec,ierr)

      IMPLICIT NONE
      integer chatter, ierr
      integer n_evr, n_gti
      integer iac_evr(*)
      real theta, talive
      double precision start(*), stop(*)
      double precision tim_evr(*)
      real spec(*)
c      real tspec(*)
      character*(*) instrume
      logical qint, qal, qext

c Description
c  Calculation of ROSAT PSPC background spectrum a'la Plucinsky 
c  (1993 Ap J. 418, 519)
c
c Passed Parameters
c  CHATTER      i   : Chatter flag
c  QINT            i   : Logical flag whether to include Internal component 
c  QAL            i   : Logical flag whether to include AL component 
c  QEXT            i   : Logical flag whether to include External component 
c  THETA        i   : Off-axis angle (in arcmins)
c  N_EVR        i   : No. of MV anticoincidence values
c  IAC_EVR      i   : Array of MV anticoincidence values
c  SPEC           r : Particle bkgd Spectrum (per arcmin**2 per second)
c  IERR           r : Error flag on return (zero = OK)
c
c Origin
c  IMG hack of PRTBKG.FOR code distributed by PROSCON, which was originally
c  written by dsdavis - dec 1992
c
c Authors/Modification History
c  dsdavis            (1.0.0; 1992 Dec), original PROSCON version
c  Ian M George            (1.1.0; 1993 Oct 18) FTOOLized hack
c  Ian M George            (2.0.0; 1993 Oct 19) Complete re-write
c  Ian M George            (2.1.0; 1994 Mar 15) Added GTI checking
c  Jane Turner          (2.2.0 ; 1994 Apr 29) Time dependant coefficients
      character(7) version
      parameter (version = '2.2.0')
*-
c Internals 
      integer i,k
      real F_i, F_ex, F_al
      real S_i, S_ex, S_al
      real phi_i, phi_e, temp, arg, L
      real a_i, b_i, a_e, b_e, a_al, b_al
      real c_1, c_2, c_3, c_4, d_3, d_4
      character(70) message
      character(30) errstr, wrnstr
c Initialize      
      ierr = 0
      talive = 0.
      errstr = '** PLUC1_BGD '//version//' ERROR: '
      wrnstr = '** PLUC1_BGD '//version//' WARNING: '
      a_i =0.0
      b_i =0.0
      a_e =0.0
      b_e =0.0
      a_al =0.0
      b_al =0.0
      c_1 =0.0
      c_2 =0.0
      c_3 =0.0
      c_4 =0.0
      d_3 =0.0
      d_4 =0.0



c Check for sillies
      if((.NOT.qint).AND.(.NOT.qal).AND.(.NOT.qext)) then
        message = wrnstr // ' No Bkgd component requested'
        call fcecho(message)
        ierr = 1
        goto 876
      endif

c Calculate the Phi-factors 
      Phi_i = 9.22E-5 + 2.99E-7*theta
      Phi_e = 1.08E-4

      instrume = 'PSPCB'
c Set up the time-dependent coeffs
c ......Before 1991 Jan 25
      if(tim_evr(1).LE.20530391) then
            if(tim_evr(n_evr).LE.20530391) then
                  instrume = 'PSPCC'
                     a_i = 2.1e-2
                     b_i = 8.64e-4
                     a_e = 1.3e-2
                     b_e = 2.86e-4
                     a_al = -0.6e-2
                     b_al = 2.44e-4
                     c_1 = 1.36
                     c_2 = -1.97
                     c_3 = 0
                     c_4 = 3.40e-3
                     d_3 =  0
                     d_4 = 4.13e-3
            else
                  ierr = 1
                  goto 546
            endif
c ......1991 Jan 25 -> 1991 May 31
      elseif((tim_evr(1).GT.20530391) 
     &         .AND. (tim_evr(1).LE.31416791)) then
            if((tim_evr(n_evr).GT.20530391) 
     &                  .AND. (tim_evr(n_evr).LE.31416791)) then     
                     a_i = 1.8e-2
                     b_i = 8.64e-4
                     a_e = 1.3e-2
                     b_e = 2.86e-4
                     a_al = -0.6e-2
                     b_al = 2.44e-4
                     c_1 = 1.40
                     c_2 = -1.97
                     c_3 = 0
                     c_4 = 3.38e-3
                     d_3 =  0
                     d_4 = 4.13e-3
            else
                  ierr = 2
                  goto 546
            endif
c ......1991 May 31 -> 1991 Oct 11
      elseif((tim_evr(1).GT.31416791) 
     &         .AND. (tim_evr(1).lE.42907991)) then
            if((tim_evr(n_evr).GT.31416791) 
     &                     .AND. (tim_evr(n_evr).lE.42907991)) then
                           a_i = 1.6e-2
                     b_i = 7.07e-4
                     a_e = 1.0e-2
                     b_e = 2.44e-4
                     a_al = -0.6e-2
                     b_al = 2.44e-4
                     c_1 = 1.61
                     c_2 = -1.97
                     c_3 = 0
                     c_4 = 3.93e-3
                     d_3 =  0
                     d_4 = 4.33e-3
            else
                  ierr = 3
                  goto 546
            endif
c ......After 1991 Oct 11 (Plucinsky only goes as far as 1992 Feb 15)
      elseif(tim_evr(1).GT.42907991) then  
            if(tim_evr(n_evr).GT.42907991) then 
                     a_i = 1.8e-2
                     b_i = 7.37e-4
                     a_e = 0.7e-2
                     b_e = 2.21e-4
                     a_al = -0.4e-2
                     b_al = 2.29e-4
                     c_1 = 39.66
                     c_2 = -2.91
                     c_3 = -3.96e-6
                     c_4 = 4.50e-3
                     d_3 =  -4.47e-6
                     d_4 = 4.93e-3
            else
                  ierr = 4
                  goto 546
            endif
       endif


546      if(ierr.NE.0) then
        message = errstr//' Dataset overlaps T.periods'
        call fcecho(message)
        write(message,'(a,f15.5)') ' ... Start: ', tim_evr(1)
        call fcecho(message)
        write(message,'(a,f15.5)') ' ... Stop:  ', tim_evr(n_evr)
        call fcecho(message)
        write(message,'(a,f15.5)') ' ... Error Code: ', ierr
        return
      endif

c Main DO LOOP
      do i = 1, n_evr
456         if(iac_evr(i).LE.0) then
            goto 124
         endif
c       .....Differential Livetime
         L = 1. - 2.3E-4 * iac_evr(i)
         talive = talive + L

c       .....Internal/Particle Background
         F_i = a_i + (b_i)*iac_evr(i)
         
c      .....Aluminium Line
         F_al = a_al + (b_al)*iac_evr(i)

c        .....External Bkgd
         F_ex = a_e + (b_e)*iac_evr(i)

c       .... Looping over energies
         do k = 1, 256
            temp = 0.0
            S_i  = c_1*(k+0.5)**(c_2) + c_3*(k+0.5)**(c_4)
            arg  = -0.716*(12.25-sqrt(k+0.5))**2.0
            if(arg.lt.-60) arg = -60
            S_al = 0.835*(k+0.5)**(-0.75) *exp(arg)
            S_ex = d_3*(k+0.5) + d_4
            if(qint) temp = (F_i*phi_i*S_i)
            if(qal)  temp = temp + (F_al*phi_e*S_al)
            if(qext) temp = temp + (F_ex*phi_e*S_ex)
c            tspec(k) = tspec(k) + (1./L)*temp
            spec(k)= spec(k) + (1./L)*temp 
          enddo
124      enddo
c            spec(i)= spec(i) + tspec(i)

123      continue

876      if(ierr.NE.0) then
            message = errstr // ' Incomplete Execution'
            call fcecho(message)
      endif

        return
        end
c ----------------------------------------------------------------

      subroutine aloc1a(wsize,wmap,status)
      implicit none
      integer wsize,wmap,status
C     Local variables
      integer m
      m=max(wsize,100)
      wmap = 0
      call udmget(m,7,wmap,status)
      return
      end
