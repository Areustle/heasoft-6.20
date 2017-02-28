*+PCARF
C
C Description:
C  Program to generate a Ancillary Response File (ARF) for the ROSAT PSPC
C instrument. The ARF consists of a simple 1-d list of the corrections
C required to be applied to the user-defined detector response matrix
C during the spectral analysis of a user-defined PHA dataset.
C  NOTE - The par file is assumed to have been opened.
C
C Passed Parameters
C  None
C
C User i/ps required (prompted for):
C  None here, isolated in GPCARF (see below)
C
C Include/Common blocks files
C  common TASK                   : (FTOOLS) standard fatal error message thingy
C
C Called routines
C  subroutine gpcarf       : (below) Gets parameters from XPI par file
C  subroutine dopcarf       : (below) Performs the calculations
C
C Compilation:
C  subroutines require CALLIB, FTOOLS, FITSIO
C
C Principal Local Variables:
C  phafil - Name of input PHA file
C  rmffil  - Name of input RMF file
C  crffil - Name of input EFFAREA or SPECRESP file
C  outfil - Name of output ARF file
C  gefil - Name of input gas efficiency file (used if crffil is EFFAREA)
C  wtfil - Name of input window trasmission file (used if crffil is EFFAREA)
C  fltfil - Name of input filter resp. file (used if necessary)
C  earflag - .TRUE. if EFFAREA crffil
C  fltflag - .TRUE. if FILTER used in observation (according to PHA file)
C  mincnts - minimum  counts per WMAP bin for that bin to be processed
C  chatter- How much to tell the user
C  arfversn- Version number of the ARF file to be written
C
C
C Origin:
C  Original
C
C Authors/Modification History:
C  Ian M George     (1.0.0:1993 Sept 15), test version
C  Lawrence E Brown (2.0.0:1994 Sept), real version (nearly complete rewrite)
C  Lawrence E Brown (2.0.1:1995 Oct), fixed uninitialized variable problems
c
c Banashree M. Seifert (1996 Sept) 2.1.0:
c           . replaced by gtcalf inplace of gtcal
C  Peter D Wilson    (1998 Feb 09) 2.1.1:
C           . Replaced string constants in calls to gtcalf with variables
C             (causing problems with C wrappers under OSF 4.0)
C           . Changed calls to ftgcvd to send 0.D0 as null value
C  Ning Gan    (1998 Jul) 2.1.2:
C	    . Modified the date/time string length to 68
C  Jeff Guerber (1999 March 16) 2.1.3:
C           . Abort if the WMAP has no values >= MINCNTS (xv_wm) or
C             wasn't read (do_pcarf).  Also removed a few unused vars.
C
*-
      subroutine pcarf
      implicit none

      character(7) version
      parameter (version = '2.1.3')
C     Internals
      character(40) taskname
      integer chatter, ierr, mincnts
      character(5) arfversn
      character(80) context
      character(160) phafil, rmffil, outfil
      character(160) message
      character(160) crffil,gefil,wtfil,fltfil
      logical earflag,fltflag
C     Initialize
      COMMON/task/taskname
      taskname ='PCARF '//version
      ierr = 0



C     Get Parameters from the par file
      call gpcarf(phafil, rmffil, crffil, outfil,
     $     gefil,wtfil,fltfil,earflag,fltflag,
     &     chatter, arfversn, mincnts, ierr)
      if(ierr.NE.0) goto 987

C     Do the nasty deed
      call do_pcarf(chatter, phafil, rmffil, crffil, outfil,
     $     gefil,wtfil,fltfil,earflag,fltflag,
     &     arfversn, mincnts, ierr)


987      if(ierr.NE.0) then
        context = 'Incomplete Execution'
        message = '** PCARF '//version//' ERROR : '// context
        call fcecho(message)
          call fcerr(context)
      else
         if(chatter.gt.5) then
        message = '** PCARF '//version//' Finished'
        call fcecho(message)
        endif
      endif


      return
      end

C -------------------------------------------------------------------------
*+GPCARF
      subroutine gpcarf(phafil, rmffil, crffil, outfil,
     $     gefil,wtfil,fltfil,earflag,fltflag,
     &            chatter, arfversn, mincnts, ierr)

      IMPLICIT NONE
      integer chatter, ierr, mincnts
      character*(*) arfversn
      character*(*) phafil, rmffil, outfil, crffil
      character*(*) gefil,wtfil,fltfil
      logical earflag,fltflag


C
C Description:
C  Gets the parameters required by PCARF from the parameter file
C  NOTE - The par file is assumed to have been opened.
C
C User i/ps required (prompted for):
C
C Origin:
C  Original
C
C Passed parameters
C  CHATTER     - chattiness flag for o/p
C              - (0 dead silent: only fatal errors reported, 5 quiet,
C              -  10 normal,15 high,>20 silly)
C  PHAFIL      - the spectrum file
C  RMFFIL      - the response matrix file
C  CRFFIL      - either the EAR file or the SRF
C  OUTFIL      - name of o/p ARF to be created
C  GEFIL       - the gas efficiency file (used if EARFLAG)
C  WTFIL       - the window transmission file (used if EARFLAG)
C  FLTFIL      - boron filter CRF file (used if FLTFLAG)
C  EARFLAG     - true if EAR file input, false if SRF
C  FLTFLAG     - true if FILTER specified in PHA file
C  ARFVERSN    - version style of output ARF file
C  MINCNTS     - minimum counts per WMAP bin necessary for inclusion
C              - in eff. area calculation
C  IERR            o : Error Flag (zero if all OK)
C
C Called Routines
C  subroutine FCECHO           : (FTOOLS) writes to standard o/p device
C  subroutine FCERRM       : (FTOOLS) Writes standard FITSIO message,
C                            dumps error stack
C  subroutine FTCMSG       : (FITSIO) clears the FITSIO error stack
C  subroutine FTMRHD       : (FITSIO) moves between adjacent HDUs in fits file
C  subroutine FTOPEN       : (FITSIO) opens a fits file
C  subroutine FTGKY_       : (FITSIO) gets a keyword from a fits header
C  subroutine FTUPCH       : (FITSIO) promotes all members of a string to
C                          : upper case
C  subroutine FCPARSE      : (FTOOLS) parses an input filename
C  subroutine UCLG_        : (XPI) Get parameter values from par file
C  subroutine GTCAL        : (FTOOLS) gets calibration dataset pathnames
C                            from the CALDB
C  subroutine CK_PHA       : (CALLIB) Checks out the PHA file and
C                            fills the COMMON block pha_block
C
C Compilation:
C  requires XPI/Host interface etc
C
C Authors/Modification History:
C  Ian M George     (0.9.0: 1993 Sep 16) test
C  Ian M George     (1.0.0: 1993 Dec 19) expanded & debugged
C  Lawrence E Brown (2.0.0: 1994 August) Added lots of new functionality
c
c Banashree M Seifert (Sept 1996)2.1.0:
c          . replaced call gtcal by call gtcalf
c
c Banashree M Seifert (May, 1997) 2.2.0:
c          . screen display the filename from CALDB
c
c Peter D Wilson    (1998 Feb 09) 2.2.1:
c          . Use variables instead of constant strings in gtcalf calls
c -------------------------------------------------------------------
      character(7) version
      parameter (version = '2.2.1')
*-
C Internals
      integer extnum,block,crfunit,extno,nfound,nret
      character(30)  errstr, wrnstr
      character(80) online
      character(160) filnm,keystr
      character(80) comment
      character(200) message
C The PHA common block variables
      character(8) pha_telescop,pha_instrume,pha_detnam,pha_filter
      integer pha_unit,typflag,nx_or_ntheta,ny
C      character(16) obs_date, obs_time, codename, dash, instr
      character(16) codename, dash, instr
      character(68) obs_date, obs_time
      common /pha_block/ pha_unit,pha_telescop, pha_instrume,
     $     pha_detnam, pha_filter, typflag, obs_date, obs_time,
     $     nx_or_ntheta,ny
      logical quiet
      character(40) taskname
      COMMON/task/taskname

C Initialize
      dash = '-'
      quiet=.true.
      errstr = '** GPCARF '//version//' ERROR: '
      wrnstr = '** GPCARF '//version//' WARNING: '
C Get the chattiness flag
      call uclgsi('chatter',chatter, ierr)
      if(ierr.NE.0) then
             message = wrnstr // 'Couldn''t get CHATTER parameter'
              call fcecho(message)
            ierr = 0
            message =wrnstr // 'Setting CHATTER = 10'
            call fcecho(message)
            chatter = 10
      endif
      if(chatter.ge.10) then
         quiet=.false.
      endif
      if (chatter.gt.5) then
         message = '**  '//taskname
         call fcecho(message)
      endif


C Get the name of the i/p PHA file
      call uclgst('phafil',phafil, ierr)
      if(ierr.NE.0) then
        message = errstr // 'Getting PHAFIL parameter'
        call fcerr(message)
        call fcerrm(ierr)
        return
      endif


C     Check out the PHA file
      call CK_PHA(chatter, phafil, ierr)
      if(ierr.NE.0)  then
         message = errstr // 'Checking out  PHAFIL '
         call fcerr(message)
         call fcerrm(ierr)
         return
      endif

C Do we need a filter...
      if(pha_filter(1:4).eq.'NONE'.or.pha_filter(1:7).eq.'UNKNOWN'
     $     .or.pha_filter(1:8).eq.'        ')then
         fltflag=.false.
      else
         fltflag=.true.
      endif


C Get the name of the i/p RMF file
      call uclgst('rmffil',rmffil, ierr)
      if(ierr.NE.0) then
        message = errstr // 'Getting RMFFIL parameter'
        call fcerr(message)
        return
      endif
      if((rmffil(1:5).eq.'CALDB').or.(rmffil(1:5).eq.'caldb')) then
C Note: don't look for Filter here
         codename = 'MATRIX'
         call gtcalf(30,pha_telescop,pha_instrume,dash,dash,
     >              codename,obs_date,obs_time,obs_date,obs_time,
     >              dash,1,rmffil,extno,online,nret,nfound,ierr)
         if(chatter .ge. 10) then
             message = 'RMF file='//rmffil
             call fcecho(message)
         endif

      endif

C Get the name of the o/p file
      call uclgst('outfil',outfil, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting OUTFIL parameter'
         call fcerr(message)
         return
      endif


C Get the name of the Effective Area File or Spectral Response file
      call uclgst('crffil',crffil, ierr)
      if(ierr.NE.0) then
         message = errstr // 'Getting CRFFIL parameter'
         call fcerr(message)
         return
      endif

      if((crffil(1:5).eq.'CALDB').or.(crffil(1:5).eq.'caldb')) then
C      Note: don't look for Filter here
         nfound=0
         codename = 'SPECRESP'
         call gtcalf(30,pha_telescop,pha_instrume,dash,dash,
     >               codename,obs_date,obs_time,obs_date,obs_time,
     >               dash,1,crffil,extno,online,nret,nfound,ierr)

         if(ierr.ne.0) then
            if(chatter.ge.10) then
               message = wrnstr //'Trouble getting SPECRESP file from'
     $              //' CALDB.  Trying EFFAREA.'
               call fcecho(message)
            endif
            if(chatter.gt.20) then
               call fcerrm(ierr)
            else
               call ftcmsg()
            endif
            ierr=0
C           Note: don't look for Filter here
C           Note: INSTRUME name for EFFAREA is XRT (not PSPC-?)
            codename = 'EFFAREA'
            instr = 'XRT'
            call gtcalf(chatter,pha_telescop,instr,dash,dash,
     >                  codename,obs_date,obs_time,obs_date,obs_time,
     >                  dash,1,crffil,extno,online,nret,nfound,ierr)

         endif
         if(ierr.ne.0) then
            message = errstr // 'Getting CRF file from CALDB'
            call fcerr(message)
            return
         endif
         if(chatter .ge. 10) then
             message = 'CRF file='//crffil
             call fcecho(message)
         endif
      endif

C     What is it?
      crfunit=98
      call fcpars(crffil,filnm,extnum,ierr)
      call ftopen(crfunit,filnm,0,block,ierr)
      if(ierr.ne.0)then
         message = errstr // 'Opening CRF file for initial checkout'
         call fcerr(message)
         call fcerrm(ierr)
         return
      endif
      call ftgkys(crfunit,'CONTENT',keystr,comment,ierr)
      if(ierr.ne.0)then
         message=errstr // 'Getting CONTENT keyword from ' // filnm
         call fcerr(message)
         call fcerrm(ierr)
         return
      endif
      if(keystr(1:17).eq.'SPECTRAL RESPONSE')then
         earflag=.false.
      else if (keystr(1:14).eq.'EFFECTIVE AREA') then
         earflag=.true.
      else
         message=errstr//'Unknown primary CRF file type'
         call fcerr(message)
         message=errstr//'Should be SPECTRAL RESPONSE'
         call fcerr(message)
         message=errstr//'or EFFECTIVE AREA file'
      endif
      if(fltflag.and.(.not.earflag))then
C     If the SPECRESP file already has a filter response in
C     it, set fltflag=.false. or we'll get the effect of 2 filters
         call srchkey(crfunit,chatter,'FILTER',keystr,ierr)
         call ftupch(keystr)
         if(keystr(1:4).ne.'NONE'.and.keystr(1:7).ne.'UNKNOWN'
     $        .and.keystr(1:8).ne.'        ')then
C     The SPECRESP file was apparently created with a filter, so
C     don't use an additional filter file
            fltflag=.false.
            ierr=0
            call ftcmsg()
         endif
         call ftclos(crfunit,ierr)
      endif

      if(earflag)then
C     Get the name of the Gas Efficiency file
         call uclgst('gefil',gefil, ierr)
         if(ierr.NE.0) then
            message = errstr // 'Getting GEFIL parameter'
            call fcerr(message)
            return
         endif
         if((gefil(1:5).eq.'CALDB').or.(gefil(1:5).eq.'caldb')) then
C Note: don't look for Filter here
            codename = 'DET_EFF'
            call gtcalf(chatter,pha_telescop,pha_instrume,dash,dash,
     >                codename,obs_date,obs_time,obs_date,obs_time,
     >                dash,1,gefil,extno,online,nret,nfound,ierr)

            if(ierr.ne.0) then
               message = errstr // 'Getting Gas Efficiency'//
     $              'file from CALDB'
               call fcerr(message)
               return
            endif
            if(chatter .ge. 10) then
                message = 'GE file='//gefil
                call fcecho(message)
            endif
         endif


C     Get the name of the Window Transmission file
         call uclgst('wtfil',wtfil,ierr)
         if(ierr.NE.0) then
            message = errstr // 'Getting WTFIL parameter'
            call fcerr(message)
            return
         endif
         if((wtfil(1:5).eq.'CALDB').or.(wtfil(1:5).eq.'caldb')) then
C Note: don't look for Filter here
            codename = 'WTRANS'
            call gtcalf(chatter,pha_telescop,pha_instrume,dash,dash,
     >                 codename,obs_date,obs_time,obs_date,obs_time,
     >                 dash,1,wtfil,extno,online,nret,nfound,ierr)

            if(ierr.ne.0) then
               message = errstr // 'Getting Window Transmission'//
     $              'file from CALDB'
               call fcerr(message)
               return
            endif
            if(chatter .ge. 10) then
                message = 'WTRANS file='//wtfil
                call fcecho(message)
            endif
         endif
      endif


C     Get the name of the Filter file
      if(fltflag) then
         call uclgst('fltfil',fltfil,ierr)

         if(ierr.NE.0) then
            message = errstr // 'Getting FLTFIL parameter'
            call fcerr(message)
            return
         endif
      endif


      if(fltflag.and.
     $     (fltfil(1:5).eq.'CALDB'.or.fltfil(1:5).eq.'caldb')) then
         codename = 'FTRANS'
         call gtcalf(chatter,pha_telescop,pha_instrume,dash,pha_filter,
     >              codename,obs_date,obs_time,obs_date,obs_time,
     >              dash,1,fltfil,extno,online,nret,nfound,ierr)

            if(ierr.ne.0) then
               message = errstr // 'Getting Filter'//
     $              'file from CALDB'
               call fcerr(message)
               return
            endif
            if(chatter .ge. 10) then
                message = 'FILTER file='//fltfil
                call fcecho(message)
            endif
      endif


C     Get the minimum counts flag
      call uclgsi('mincnts',mincnts, ierr)
      if(ierr.NE.0) then
         if(chatter.ge.5)then
            message = wrnstr // 'Getting MINCNTS parameter'
            call fcecho(message)
            message = 'Setting MINCNTS = 1'
            call fcecho(message)
            if(chatter.ge.20) call fcerrm(ierr)
            call ftcmsg()
         endif
         ierr = 0
         mincnts=1
      endif

C     Get the arfversn to be used
      call uclgst('arfversn',arfversn, ierr)
      if(ierr.NE.0) then
         if(chatter.ge.5)then
            message = errstr // 'Getting ARFVERSN parameter'
            call fcecho(message)
         endif
         ierr = 0
         arfversn = '1.1.0'
         if(chatter.ge.5)then
            message = wrnstr // 'Setting ARFVERSN to '// arfversn
            call fcecho(message)
         endif
      endif



C Give user info if requested
      if(chatter.GE.20) then
       message = ' ... using GPCARF ' // version
       call fcecho(message)
      endif

      return
      end

C -------------------------------------------------------------------------
*+DO_PCARF
C
C Description:
C  Opens input PHA file, RMF file, filter file (if appropriate), and either a
C  spectral response file (SRF) or a set of effective area (EAR) gas
C  effciency (GE) and window transmission (WT) files.  Creates a simple
C  one-d list of weights (units: area???) to be multiplied with the response
C  matrix in the RMF file to give the total response of the instrument.
C  It weights the response according to a WMAP (or if not present, an Off
C  Axis Histogram) from the PHA file.
C  It writes the weights to the output ARF file.
C
C User i/ps required (prompted for):
C  None
C
C Passed parameters
C  CHATTER     - chattiness flag for o/p
C              - (0 dead silent: only fatal errors reported, 5 quiet,
C              -  10 normal,15 high,>20 silly)
C  PHAFIL      - the spectrum file
C  RMFFIL      - the response matrix file
C  CRFFIL      - either the EAR file or the SRF
C  OUTFIL      - name of o/p ARF to be created
C  GEFIL       - the gas efficiency file (used if EARFLAG)
C  WTFIL       - the window transmission file (used if EARFLAG)
C  FLTFIL      - boron filter CRF file (used if FLTFLAG)
C  EARFLAG     - true if EAR file input, false if SRF
C  FLTFLAG     - true if FILTER specified in PHA file
C  ARFVERSN    - version style of output ARF file
C  MINCNTS     - minimum counts per WMAP bin necessary for inclusion
C              - in eff. area calculation
C  IERR            o : Error Flag (zero if all OK)
C
C Called Routines:
C  subroutine FCECHO           : (FTOOLS) writes to standard o/p device
C  subroutine FCERRM       : (FTOOLS) Writes standard FITSIO message,
C                            dumps error stack
C  subroutine FTVERS       : (FITSIO) Returns the FITSIO version number
C  subroutine CK_PHA       : (CALLIB) Checks out the PHA file and gets its
C                            characteristics
C  subroutine CK_RMF       : (CALLIB) Checks out the RMF file and gets its
C                            characteristics
C  subroutine FTGKY_       : (FITSIO) gets a keyword from a fits header
C  subroutine UDMGET       : (XPI) Dynamic memory allocation
C  subroutine UDMFRE       : (XPI) Dynamic memory de-allocation
C  subroutine GT_WMAP      : (CALLIB) Get WMAP and related things from PHA file
C  subroutine GT_OAH       : Get OAH and related things from PHA file
C  subroutine GT_RMF_GRIDS : (CALLIB) Get RMF energy grids from RMF file
C  subroutine CONST        : Assigns a constant to all the cells of an array
C  subroutine MULT         : Multiplies two vectors
C  subroutine RMVLBK       : Removes leading blanks from a string
C  subroutine CK_CRF       : (CALLIB) Checks out an CRF file and gets its
C                            characteristics
C  subroutine GT_CRF_GRIDS : (CALLIB) Gets the energy and angular grids and
C                             the column numbers for an CRF file ("parses" it)
C  subroutine XV_ONE_D : Gets the "arf" output vector from a 1-d CRF file
C  subroutine XV_WM : Gets the "arf" output vector from an CRF file using a
C                     WMAP (only does 2d in this release, no phi dependance)
C  subroutine XV_OAH : Gets the "arf" output vector from an CRF file using an
C                     OAH.
C  subroutine REGRID : Interpolates input vector from one grid to another
C  subroutine OP_NPA : (CALLIB) opens an output file with null PHDU
C  subroutine WTARF1 : (CALLIB) writes an ARF file
C  subroutine FTCLOS : (FITSIO) closes a fits file
C  subtoutine ALLOC_ : dynamically allocates some arrays
C  subtoutine DEALL_ : dynamically de-allocates some arrays
C
C Origin:
C  Original
C
C Authors/Modification History:
C  Ian M George     (1.0.0:1993 Oct 20), original
C  Ian M George     (1.1.0:1993 Dec 19), revised & globally updated
C  Ian M George     (1.2.0:1994 Apr 29), added Dynamic Memory Allocation
C                                            ^some?
C  Lawrence E Brown (2.0.0:1994 August), added data fetching routines
C  Jeff Guerber (2.0.1: 1999 March), Be sure to abort if WMAP unreadable.

      subroutine do_pcarf(chatter, phafil, rmffil, crffil, outfil,
     $     gefil,wtfil,fltfil,earflag,fltflag,
     &             arfversn, mincnts, ierr)

      IMPLICIT NONE
      integer chatter, ierr, mincnts
      character*(*) arfversn
      character*(*) phafil, rmffil, outfil, crffil
      character*(*) gefil,wtfil,fltfil
      logical earflag,fltflag

        character(7) version
        parameter (version = '2.0.1')
*-
C Commons
        character(40) taskname
        COMMON/task/taskname
C Max array sizes
      integer  maxhist, maxcomm
      parameter (maxhist=10, maxcomm = 20)
C Internals
      integer mintheta,maxtheta
      integer sfrac
      integer crf_lo_energ,crf_hi_energ
      integer cr_vector
      integer w1_cr_vector,w2_cr_vector
      integer lo_energ, hi_energ
      integer o_vector,work_vector
      integer theta_crf,phi_crf
      integer n_cr_vector,n_theta_crf,n_phi_crf,crf_col
      integer tdim(3),ndims,thind,phind
      integer  ounit
      integer  pha_unit
      integer ienerg
      integer nk_hist, nk_comm
      integer typflag
      double precision ftsver,opticx,opticy,deltx,delty
      character(16) telescop, instrume, detnam, filter
      character(20) rmfclas3
        character(20) hduclas(9),extnsn,datnam
        character(70) hist(maxhist), comment(maxcomm)
        character(160) filfil
        character(160) message
        character(30)  errstr, wrnstr
C      character(16) obs_date, obs_time
      character(68) obs_date, obs_time
      character(8) pha_telescop,pha_instrume,pha_detnam,pha_filter
      character(8) rmf_telescop,rmf_instrume,rmf_detnam,rmf_filter
      character(8) crf_telescop,crf_instrume,crf_detnam,crf_filter
      integer iunit2,iunit3,nx_or_ntheta,ny,wsize,wmap
      integer  lecol,hecol,thcol,phcol
      integer itrip,ifs,ife

C   COMMON variables
      common /pha_block/ pha_unit,pha_telescop, pha_instrume,
     $     pha_detnam, pha_filter, typflag, obs_date, obs_time,
     $     nx_or_ntheta,ny

C******************************************************************************
C   the following MEM common block definition is in the system iraf77.inc file
C   and is used for dynamic memory allocation
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD

C     note:
C      datatype      value
C      logical        1
C      integer*2      3
C      Integer        4
C      Long Integer   5
C      Real           6
C      Double         7
C      Complex        8
C******************************************************************************
      ierr=0
      nk_hist=0
      nk_comm=0
      n_theta_crf = 0
      n_phi_crf = 0

C Initialize
        errstr = '** DO_PCARF '//version//' ERROR: '
        wrnstr = '** DO_PCARF '//version//' WARNING: '

C Give user info if requested
        if(chatter.GE.20) then
           message = ' ... using DO_PCARF '// version
           call fcecho(message)
           call ftvers(ftsver)
           write(message,'(a,f6.3)')
     &          ' ... using FITSIO Version ', ftsver
           call fcecho(message)
      endif

C ------------------ START CHECK-OUT THE INPUT FILES --------------------
C     use telescop,instrume,detnam,filter keywords
C     from PHA file as the authoritative ones
         telescop=pha_telescop
         instrume=pha_instrume
         detnam=pha_detnam
         filter=pha_filter


C Check out the RMF file,
      call CK_RMF(chatter, rmffil, iunit2, rmf_telescop,
     &            rmf_instrume, rmf_detnam, rmf_filter,
     &            ienerg, rmfclas3, ierr)
      if(ierr.NE.0) goto 998

      call grab_meaningful_name(rmffil,ifs,ife)


      if(rmf_telescop.ne.telescop)then
         message=errstr//rmffil(ifs:ife)//' telescope: '
         call fcerr(message)
         message= rmf_telescop//
     $        'doesn''t match PHA telescope: '//telescop
         call fcerr(message)
         goto 998
      endif
      if(rmf_instrume.ne.instrume.and.chatter.ge.5)then
         message=wrnstr//rmffil(ifs:ife)//' instrument: '
     $        //rmf_instrume//
     $        'doesn''t match PHA instrument: '//instrume
         call fcecho(message)
      endif
      if(rmf_detnam.ne.detnam.and.chatter.ge.5)then
         message=wrnstr//rmffil(ifs:ife)//' detector: '//rmf_detnam//
     $        'doesn''t match PHA detector: '//detnam
         call fcecho(message)
      endif



C ------------------ END CHECK-OUT THE INPUT FILES --------------------
C ------------------(Except CRF files) --------------------------
C

      if (typflag.eq.1) then
         wsize=nx_or_ntheta*ny
         call alloc1(wsize,wmap,ierr)
C  ....Get the WMAP
         call gt_wmap(chatter,pha_unit,nx_or_ntheta,ny,MEMD(wmap),
     $        opticx,opticy,deltx,delty,ierr)
         call ftclos(pha_unit,ierr)
         if (ierr .ne. 0) then
             call fcerr(errstr//'Couldn''t get WMAP from PHA file')
             goto 998
         endif
      else
         if(ierr.ne.0) then
            call fcerrm(ierr)
            goto 998
         endif
         call alloc2(nx_or_ntheta,mintheta,maxtheta,sfrac,ierr)
C  ....Get the OAH
         call gt_oah(pha_unit,nx_or_ntheta,MEMD(mintheta),
     $        MEMD(maxtheta),MEMD(sfrac),ierr)
      endif
      call alloc3(ienerg,lo_energ,hi_energ,work_vector,o_vector,ierr)
      call const(MEMR(o_vector),ienerg,1.0)




C Read the RMF file, and get the energy grid
      call GT_RMF_GRIDS(chatter, iunit2,
     &            ienerg, MEMR(lo_energ), MEMR(hi_energ), ierr)
      if(ierr.NE.0) goto 998

C-------BEGIN MAIN PROCESSING LOOP----If input file is SPECRESP----
C-------This will execute once--------3 times otherwise but-------
C-------4 if Boron filter was in use--the value of FILFIL---------
C-------changes each trip----------------------------
      filfil=crffil
      itrip=1
      hduclas(1)='RESPONSE'
      if(earflag)then
         hduclas(2)='EFF_AREA'
         extnsn='EFFECTIVE AREA'
         datnam='EFFAREA'
      else
         hduclas(2)='SPECRESP'
         extnsn='SPECTRAL RESPONSE'
         datnam='SPECRESP'
      endif
C     ACTUAL LOOP STARTS HERE
 10   continue

C     .... main CRF check-out
      call CK_CRF(chatter, filfil, hduclas, extnsn,
     $     iunit3, crf_telescop, crf_instrume,
     $     crf_detnam, crf_filter, n_cr_vector,
     $     n_theta_crf,n_phi_crf, lecol, hecol, thcol,
     $     phcol, ierr)

      call grab_meaningful_name(filfil,ifs,ife)

      if(crf_telescop.ne.telescop)then
         message=errstr//filfil(ifs:ife)//' telescope: '
         call fcerr(message)
         message=crf_telescop//
     $        'doesn''t match PHA telescope: '//telescop
         call fcerr(message)
         goto 998
      endif
      if(crf_instrume.ne.instrume.and.chatter.ge.5.and.
     $     crf_instrume(5:5).ne.' ')then
         message=wrnstr//filfil(ifs:ife)//' instrument: '
     $        //crf_instrume//
     $        'doesn''t match PHA instrument: '//instrume
         call fcecho(message)
      endif
      if(crf_detnam.ne.detnam.and.chatter.ge.5)then
         message=wrnstr//filfil(ifs:ife)//' detector: '//crf_detnam//
     $        'doesn''t match PHA detector: '//detnam
         call fcecho(message)
      endif

      call alloc4(n_cr_vector,n_theta_crf,n_phi_crf,crf_lo_energ,
     $     crf_hi_energ,w1_cr_vector,w2_cr_vector,cr_vector,
     $     theta_crf,phi_crf,ierr)

C     .... get the energy and theta grids from the crf file

      call gt_crf_grids(iunit3,n_cr_vector,n_theta_crf,n_phi_crf,
     $     lecol,hecol,thcol,phcol,datnam,
     $     crf_col, MEMD(crf_lo_energ),MEMD(crf_hi_energ),
     $     MEMD(theta_crf),MEMD(phi_crf),tdim,ndims,
     $     thind,phind,ierr)

C     check the crf_lo_energ and crf_hi_energ grids to see if they're
C     equal.  If not, make them so.
      if(MEMD(crf_lo_energ).ne.MEMD(crf_hi_energ)) then
C     average the lo and hi grids, the regridder will then interpolate
C     between the midpoints.  This seems the most robust way to do this.
         call avg(MEMD(crf_lo_energ),MEMD(crf_hi_energ),n_cr_vector)
      endif

      if(phind.eq.0.and.thind.eq.0) then
C     ....get vector
         call xv_one_d(iunit3,crf_col,n_cr_vector,
     $        tdim,ndims,MEMD(cr_vector),ierr)
      else
         if(typflag.eq.1) then
C     ....get vector
            call xv_wm(iunit3,crf_col,tdim,ndims,mincnts,thind,phind,
     $           n_cr_vector,n_theta_crf,n_phi_crf,
     $           MEMD(theta_crf),MEMD(phi_crf),
     $           MEMD(wmap),nx_or_ntheta,ny,opticx,opticy,deltx,delty,
     $           MEMD(w1_cr_vector),MEMD(w2_cr_vector),
     $           MEMD(cr_vector),ierr)
         else
C     ....get vector
            call xv_oah(iunit3,crf_col,tdim,ndims,thind,
     $         n_cr_vector,n_theta_crf,nx_or_ntheta,
     $         MEMD(theta_crf),MEMD(sfrac),
     $         MEMD(maxtheta),MEMD(mintheta),MEMD(w1_cr_vector),
     $         MEMD(w2_cr_vector),MEMD(cr_vector),ierr)
         endif
      endif
      if (ierr .ne. 0) goto 998

      call regrid(n_cr_vector,MEMD(crf_lo_energ),MEMD(cr_vector),
     $     ienerg,MEMR(lo_energ),MEMR(hi_energ),MEMR(work_vector))
C     Multiply current vector into output vector
      call mult(MEMR(o_vector),MEMR(work_vector),MEMR(o_vector),ienerg)

      call deall1(crf_lo_energ,crf_hi_energ,w1_cr_vector,
     $     w2_cr_vector,cr_vector,theta_crf,phi_crf,ierr)

C     Setup for next loop

C     do FILTER file if necessary on second trip
      if(itrip.eq.1)then
         if(fltflag) then
            filfil=fltfil
            hduclas(2)='TRANSMISSION'
            extnsn='TRANSMISSION'
            datnam='TRANSMIS'
            itrip=itrip+1
            phind=0
            thind=0
            goto 10
         else
            itrip=itrip+1
         endif
      endif

      if(earflag) then
         if(itrip.eq.2)then
            filfil=gefil
            hduclas(2)='DET_EFF'
            extnsn='DETECTOR EFFICIENCY'
            datnam='DET_EFF'
         endif
         if(itrip.eq.3)then
            filfil=wtfil
            hduclas(2)='TRANSMISSION'
            extnsn='TRANSMISSION'
            datnam='TRANSMIS'
         endif
         itrip=itrip+1
         phind=0
         thind=0
         if(itrip.lt.5) goto 10
      endif

C--------END OF MAIN PROCESSING LOOP-----------------------------------


C ============================= OUTPUT ================================

C Open & Write the FITS file
C ------------------------ PRIMARY HEADER ---------------------------
      if(arfversn(1:1).EQ.'1')then
C             ... Open the FITS file and write a null primary header
            call op_nPA(outfil, chatter, ounit, ierr)
            if(ierr.ne.0) then
                  ierr = 5
                  goto 998
            endif
C             ... Add additional keywords to Primary Header
              call FTPKYS(ounit,'CREATOR',
     &                taskname,
     &             's/w task which wrote this dataset',
     &                ierr)

              call FTPKYS(ounit,'CONTENT',
     &                'ANCILLARY RESP',
     &             'SPECRESP xtension',
     &                ierr)
        else
              message = errstr // 'Unknown format: '// arfversn
              call fcecho(message)
              ierr = 1
              goto 998
        endif
C ------------------------ finished PRIMARY ---------------------------

C ------------------------ SPECRESP EXTENSION ----------------------------
      if(arfversn(1:1).EQ.'1')then
C     ... Write the SPECRESP extension within ARF file
         call wtarf1(ounit, chatter,
     &        nk_hist, hist,
     &        nk_comm, comment,arfversn,phafil,
     &        telescop, instrume, detnam, filter,
     &        ienerg, ienerg, MEMR(lo_energ),MEMR(hi_energ),
     &        MEMR(o_vector), ierr)
         if(ierr.NE.0) goto 876

         call FTPKYS(ounit,'CREATOR',
     &        taskname,
     &        's/w task which wrote this dataset',
     &        ierr)
         call FTPKYS(ounit,'RESPFILE',
     &        rmffil,
     &        'RMF file',
     &        ierr)
      else
         message = errstr // 'Unknown format: '// arfversn
         call fcecho(message)
         ierr = 1
         goto 998
      endif

C ----------------- finished SPECRESP EXTENSION ----------------------------

876            continue

C Close the FITS file
        call ftclos(ounit, ierr)
      if(ierr.ne.0) then
            ierr = 6
            goto 998
      endif
C ============================ finished OUTPUT ==============================

C Check for errors
 998  if(ierr.ne.0) then
         message = errstr // ' Fatal'
         call fcecho(message)
         return
      endif

      call deall2(typflag,work_vector,lo_energ,hi_energ,
     $     o_vector,wmap,mintheta,maxtheta,sfrac,ierr)
      return
      end



*+
      subroutine gt_oah(iunit,ntheta,thmin,thmax,sfrac,status)
      implicit none
      integer iunit,ntheta,status,col
      double precision thmin(ntheta),thmax(ntheta),sfrac(ntheta)
C     This extracts an off axis histogram from the PHA file.
C  !!! NOTE !!! The FITS PHA file is assumed to be open and scrolled to
C                 apprproiate extension.
C               File will be closed on successful execution
C Passed Parameters
C  IUNIT        i    : Where's the file?
C  NTHETA       i    : Size of theta vectors
C  THMIN,THMAX     o : The theta grids
C  SFRAC           o : Weight for a given theta bin
C  STATUS          o : Error Flag (zero if all OK)
C Author/Modification History
C  Lawrence E Brown (1.0.0:94 August), added MVEXT calls
C  Peter D Wilson   (1.0.1:98 Feb 09), call ftgcvd with 0.D0 for null values
*-

C     LOCAL variables
      character(7) version
      parameter (version='1.0.1')
      character(80) context
      logical anyf

      character(30) errstr, wrnstr
      errstr = '** GT_OAH '//version//' ERROR:'
      wrnstr = '** GT_OAH '//version//' WARNING:'

      if(status.ne.0) return

      call ftgcno(iunit,.false.,'THET_MIN',col,status)
      call ftgcvd(iunit,col,1,1,ntheta,0.D0,thmin,anyf,status)
      if (status .ne. 0) then
         context = errstr//
     $        ' getting theta_min column'
         call fcerr (context)
         goto 999
      endif
      call ftgcno(iunit,.false.,'THET_MAX',col,status)
      call ftgcvd(iunit,col,1,1,ntheta,0.D0,thmax,anyf,status)
      if (status .ne. 0) then
         context = errstr//
     $        ' getting theta_max column'
         call fcerr (context)
         goto 999
      endif
      call ftgcno(iunit,.false.,'SOU_FRAC',col,status)
      call ftgcvd(iunit,col,1,1,ntheta,0.D0,sfrac,anyf,status)
      if (status .ne. 0) then
         context = errstr//
     $        ' getting OAH column'
         call fcerr (context)
         goto 999
      endif
 999  call ftclos(iunit,status)
      if(status.ne.0) then
         context = errstr//
     $        ' processing PHA file'
         call fcerr (context)
         call fcerrm(status)
      endif
      return
      end







*+
      subroutine xv_oah(iunit,crf_col,tdim,ndims,thind,
     $ n_cr_vector, n_theta_crf,ntheta,
     $ theta_crf,sfrac,maxtheta,mintheta,w1_cr_vector,w2_cr_vector,
     $ cr_vector,status)
      implicit none
      integer n_cr_vector,n_theta_crf,ntheta,iunit,crf_col
      integer status
      integer tdim(3),ndims,thind
      double precision cr_vector(n_cr_vector)
      double precision w1_cr_vector(n_cr_vector)
      double precision w2_cr_vector(n_cr_vector)
      double precision maxtheta(ntheta),mintheta(ntheta)
      double precision sfrac(ntheta)
      double precision theta_crf(n_theta_crf)
C  Description
C     Extracts an apropriate ARF vector from the CRF array in the
C     FITS file connected to iunit using an Off Axis Histogram.
C
C  Passed Parameters
C     IUNIT              i : The Unit number of the CRF file
C     CRF_COL            i : Column number of CRF array
C     TDIM               i : Dimensions of the CRF array
C     NDIMS              i : Number of Dimensions in the CRF array
C     THIND              i : Theta dimension index
C     N_CR_VECTOOR       i : Size of CRF Energy vectors
C     N_THETA_CRF        i : Size of CRF Theta vector
C     NTHETA             i : Size of OAH Theta vector
C     THETA_CRF          i : CRF theta grid
C     SFRAC              i : OAH values
C     MAXTHETA,MINTHETA  i : OAH theta grids
C     CR_VECTOR          o : Output ARF vector
C     W1_CR_VECTOR,W2_CR_VECTOR : Work arrays, dynamically allocated elsewhere
C     STATUS             o : Error status
C
C  Called Subroutines
C     XFVV       : Extracts a vector along the fastest varying axis
C                  of a fits table array element
C
C  Author/Modification History
C     Lawrence E Brown (1.0.0:94 August) Original
      character(7) version
      parameter (version='1.0.0')

*-



C LOCAL variables
      character(80) context
      integer itheta,i
      double precision thetabar,lin_int_fac,om_lin_int_fac
      double precision val
      integer itargtheta,row,nelem(3)
      integer locat
      external locat
      character(30) errstr, wrnstr

      errstr = '** XV_OAH '//version//' ERROR:'
      wrnstr = '** XV_OAH '//version//' WARNING:'


      do i=1,n_cr_vector
         cr_vector(i)=0.0
      enddo




      do itheta=1,ntheta
         thetabar=0.5*(mintheta(itheta)+maxtheta(itheta))
         itargtheta=locat(thetabar,theta_crf,n_theta_crf)
         if(itargtheta.eq.0.or.itargtheta.eq.n_theta_crf) then
            context=errstr//'OAH theta out of range for CRF theta'
            call fcerr(context)
            status=33
            goto 998
         endif

C Assume only one row to the table
         row=1
         nelem(1)=1
         nelem(thind)=itargtheta
         call xfvv(iunit,crf_col,row,nelem,tdim,ndims,w1_cr_vector,
     $ status)
         nelem(thind)=itargtheta+1
         call xfvv(iunit,crf_col,row,nelem,tdim,ndims,w2_cr_vector,
     $ status)
         lin_int_fac=(thetabar-theta_crf(itargtheta))/
     $ (theta_crf(itargtheta+1)-theta_crf(itargtheta))
         om_lin_int_fac=1.0-lin_int_fac
         do i=1,n_cr_vector
C interpolate
            val=lin_int_fac*w2_cr_vector(i)+
     $ w1_cr_vector(i)*om_lin_int_fac
            cr_vector(i)=cr_vector(i)+val*sfrac(itheta)
         enddo
      enddo
 998  continue
      call ftclos(iunit,status)
      if(status.ne.0) then
         context = errstr//'getting vector from filtering file.'
         call fcerr (context)
         call fcerrm(status)
      endif
      return
      end

*+
      subroutine xv_one_d(iunit,crf_col,n_cr_vector,
     $ tdim,ndims,cr_vector,status)
      implicit none
      integer n_cr_vector,iunit,crf_col
      integer status
      integer tdim(3),ndims
      double precision cr_vector(n_cr_vector)
C  Description
C     Extracts an apropriate ARF vector from the CRF array in the
C     FITS file connected to iunit.  The CRF array should be
C     one-dimensional.
C
C  Passed Parameters
C     IUNIT              i : The Unit number of the CRF file
C     CRF_COL            i : Column number of CRF array
C     N_CR_VECTOOR       i : Size of CRF Energy vectors
C     TDIM               i : Dimensions of the CRF array
C     NDIMS              i : Number of Dimensions in the CRF array
C     CR_VECTOR          o : Output ARF vector
C     STATUS             o : Error status
C
C  Called Subroutines
C     XFVV       : Extracts a vector along the fastest varying axis
C                  of a fits table array element
C
C  Author/Modification History
C     Lawrence E Brown (1.0.0:94 August) Original
      character(7) version
      parameter (version='1.0.0')

*-

C LOCAL variables
      character(80) context
      integer row,nelem(3),i
      character(30) errstr, wrnstr

      errstr = '** XV_ONE_D '//version//' ERROR:'
      wrnstr = '** XV_ONE_D '//version//' WARNING:'

      do i=1,n_cr_vector
         cr_vector(i)=0.0
      enddo
      row=1
      nelem(1)=1

      call xfvv(iunit,crf_col,row,nelem,tdim,ndims,
     $     cr_vector,status)
      if(status.ne.0) then
         context = errstr//'Error getting vector from CRF file.'
         call fcerr (context)
         call fcerrm(status)
      endif
      return
      end

      subroutine xv_wm(iunit,crf_col,tdim,ndims,mincnts,
     $     thind,phind,n_cr_vector,n_theta_crf,
     $     n_phi_crf,theta_crf,phi_crf,wmap,nx,ny,opticx,
     $     opticy,deltx,delty,
     $     w1_cr_vector,w2_cr_vector,cr_vector,status)
      implicit none
      integer n_cr_vector,n_theta_crf,n_phi_crf,iunit,crf_col
      integer status,mincnts
      integer tdim(3),ndims,nx,ny
      double precision opticx,opticy,deltx,delty
      double precision cr_vector(n_cr_vector)
      double precision w1_cr_vector(n_cr_vector)
      double precision w2_cr_vector(n_cr_vector)
      double precision wmap(nx,ny)
      double precision theta_crf(*),phi_crf(*)
C  Description
C     Extracts an apropriate ARF vector from the CRF array in the
C     FITS file connected to iunit using a WMAP.
C
C  Passed Parameters
C     IUNIT              i : The Unit number of the CRF file
C     CRF_COL            i : Column number of CRF array
C     TDIM               i : Dimensions of the CRF array
C     NDIMS              i : Number of Dimensions in the CRF array
C     MINCNTS            i : Minimum counts per WMAP bin necessary for
C                            inclusion in eff. area calculation
C     THIND              i : Theta dimension index
C     PHIND              i : Phi dimension index
C     N_CR_VECTOOR       i : Size of CRF Energy vectors
C     N_THETA_CRF        i : Size of CRF Theta vector
C     N_PHI_CRF          i : Size of CRF Phi vector
C     THETA_CRF          i : CRF theta grid
C     PHI_CRF            i : CRF phi grid
C     WMAP               i : Weighted map showing where the telescope was
C                            pointed
C     NX,NY              i : Dimensions of the WMAP
C     OPTICX,OPTIXY      i : Optical axis of the telescope in WMAP coord system
C     DELTX,DELTY        i : Size of WMAP bins in arcmin.
C     W1_CR_VECTOR,W2_CR_VECTOR : Work arrays, dynamically allocated elsewhere
C     CR_VECTOR          o : Output ARF vector
C     STATUS             o : Error status

      character(7) version
      parameter (version='1.0.0')

C     LOCAL variables
      character(80) context
      integer thind,phind,i,ix,iy
      double precision thetabar,lin_int_fac,om_lin_int_fac
      double precision val,dx,dx2,dy,dy2
      integer itargtheta,row,nelem(3),totcnts
      integer locat
      external locat

      do i=1,n_cr_vector
         cr_vector(i)=0.0
      enddo
      totcnts=0
      if(thind.ne.0) then
         do 10 ix=1,nx
            dx=(real(ix)-opticx)*deltx
            dx2=dx*dx
            do 20 iy=1,ny
               if(wmap(ix,iy).lt.mincnts) goto 20
               dy=(real(iy)-opticy)*delty
               dy2=dy*dy
               thetabar=sqrt(dx2+dy2)
               if(phind.ne.0) then
                  context='Sorry phi dependance not supported yet'
                  call fcerr(context)
                  status=99
                  goto 998
               else
                  itargtheta=locat(thetabar,theta_crf,n_theta_crf)
                  if(itargtheta.eq.0.or.itargtheta.gt.n_theta_crf) then
                     context='WMAP theta out of range for CRF theta'
                     call fcerr(context)
                     status=33
                     goto 998
                  endif
C     Assume only one row to the table
                  row=1
                  nelem(1)=1
                  nelem(thind)=itargtheta
                  call xfvv(iunit,crf_col,row,nelem,tdim,ndims,
     $                 w1_cr_vector,status)
                  if(itargtheta.ne.n_theta_crf) then
                     nelem(thind)=itargtheta+1
                     call xfvv(iunit,crf_col,row,nelem,tdim,ndims,
     $                    w2_cr_vector,status)
                     lin_int_fac=(thetabar-theta_crf(itargtheta))/
     $                    (theta_crf(itargtheta+1)-theta_crf(
     $                    itargtheta))
                     om_lin_int_fac=1.0-lin_int_fac
                     do i=1,n_cr_vector
C     interpolate
                        val=lin_int_fac*w2_cr_vector(i)+
     $                       w1_cr_vector(i)*om_lin_int_fac
                        cr_vector(i)=cr_vector(i)+val*wmap(ix,iy)
                     enddo
                     totcnts=totcnts+nint(wmap(ix,iy))
                  else
                     do i=1,n_cr_vector
                        cr_vector(i)=cr_vector(i)+w1_cr_vector(i)
     $                       *wmap(ix,iy)
                     enddo
                     totcnts=totcnts+nint(wmap(ix,iy))
                  endif
               endif
 20         continue
 10      continue
         if (totcnts .ge. 1) then
             do 30 i=1,n_cr_vector
                 cr_vector(i)=cr_vector(i)/dble(totcnts)
   30        continue
         else
             write (context,'(a,i4,a)') 'ERROR: WMAP in input PHA '
     &           // 'file has no values >= MINCNTS (==',mincnts,')'
             call fcerr(context)
             status = 66
             goto 998
         endif
      endif

 998  continue
      call ftclos(iunit,status)
      if(status.ne.0) then
         context = 'Error getting vector from CRF file.'
         call fcerr (context)
         call fcerrm(status)
      endif
      return
      end

      subroutine const(a,n,c)
C     fills array A (size N) with constant C
      implicit none
      integer n
      real a(n),c
      integer i
      do i=1,n
         a(i)=c
      enddo
      return
      end

      subroutine mult(in1,in2,out,n)
C     multiplies array IN1 by array IN2, result in OUT, all arrays of size N
      integer n
      real in1(n),in2(n),out(n)
      integer i
      do i=1,n
         out(i)=in1(i)*in2(i)
      enddo
      return
      end

      subroutine avg(a,b,n)
C     Refills A and B (size N) with (A+B)/2
      integer n,i
      double precision a(n),b(n),avgg
      do i=1,n
         avgg=a(i)+b(i)
         avgg=avgg/2.0
         a(i)=avgg
         b(i)=avgg
      enddo
      return
      end


      subroutine alloc1(wsize,wmap,status)
      implicit none
      integer wsize,wmap,status
C     Local variables
      integer m
      m=max(wsize,100)
      wmap = 0 
      call udmget(m,7,wmap,status)
      return
      end


      subroutine alloc2(ntheta,mintheta,maxtheta,sfrac,status)
      implicit none
      integer ntheta,mintheta,maxtheta,sfrac,status
C     Local variables
      integer m
C     Workaround for strange bug in dynamic memory allocation
      m=max(ntheta,100)
      mintheta = 0
      maxtheta = 0
      sfrac = 0
      call udmget(m,7,mintheta,status)
      call udmget(m,7,maxtheta,status)
      call udmget(m,7,sfrac,status)
      return
      end


      subroutine alloc3(ienerg,lo_energ,hi_energ,work_vector,o_vector,
     $     status)
      implicit none
      integer ienerg,lo_energ,hi_energ,work_vector,o_vector,status
C     Local variables
      integer m
C     Workaround for strange bug in dynamic memory allocation
      m=max(ienerg,100)
      lo_energ = 0
      hi_energ = 0
      work_vector = 0
      o_vector = 0
      call udmget(m,6,lo_energ,status)
      call udmget(m,6,hi_energ,status)
      call udmget(m,6,work_vector,status)
      call udmget(m,6,o_vector,status)
      return
      end

      subroutine alloc4(n_cr_vector,n_theta_crf,n_phi_crf,crf_lo_energ,
     $     crf_hi_energ,w1_cr_vector,w2_cr_vector,cr_vector,
     $     theta_crf,phi_crf,status)
      implicit none
      integer n_cr_vector,n_theta_crf,n_phi_crf,crf_lo_energ
      integer crf_hi_energ,w1_cr_vector,w2_cr_vector,cr_vector
      integer theta_crf,phi_crf,status
C     Local variables
      integer m
C     Workaround for strange bug in dynamic memory allocation
         m=max(n_cr_vector,100)
      crf_lo_energ = 0
      crf_hi_energ = 0
      w1_cr_vector = 0
      w2_cr_vector = 0
      cr_vector = 0
      call udmget(m,7,crf_lo_energ,status)
      call udmget(m,7,crf_hi_energ,status)
      call udmget(m,7,w1_cr_vector,status)
      call udmget(m,7,w2_cr_vector,status)
      call udmget(m,7,cr_vector,status)
CALLOCATE theta_crf need n_theta_crf
C     Workaround for strange bug in dynamic memory allocation
      m=max(n_theta_crf,100)
      theta_crf = 0
      call udmget(m,7,theta_crf,status)
CALLOCATE phi_crf need n_phi_crf
C     Workaround for strange bug in dynamic memory allocation
         m=max(n_phi_crf,100)
      phi_crf = 0
      call udmget(m,7,phi_crf,status)
      return
      end

      subroutine deall1(crf_lo_energ,crf_hi_energ,w1_cr_vector,
     $     w2_cr_vector,cr_vector,theta_crf,phi_crf,status)
      implicit none
      integer crf_lo_energ,crf_hi_energ,w1_cr_vector
      integer w2_cr_vector,cr_vector,theta_crf,phi_crf,status
      call udmfre(crf_lo_energ,7,status)
      call udmfre(crf_hi_energ,7,status)
      call udmfre(w1_cr_vector,7,status)
      call udmfre(w2_cr_vector,7,status)
      call udmfre(cr_vector,7,status)
      call udmfre(theta_crf,7,status)
      call udmfre(phi_crf,7,status)
      return
      end

      subroutine deall2(typflag,work_vector,lo_energ,hi_energ,
     $     o_vector,wmap,mintheta,maxtheta,sfrac,status)
      implicit none
      integer typflag,work_vector,lo_energ,hi_energ
      integer o_vector,wmap,mintheta,maxtheta,sfrac,status
      call udmfre(work_vector,7,status)
      call udmfre(lo_energ,6,status)
      call udmfre(hi_energ,6,status)
      call udmfre(o_vector,6,status)
      if(typflag.eq.1) then
         call udmfre(wmap,7,status)
      else if(typflag.eq.2) then
         call udmfre(mintheta,7,status)
         call udmfre(maxtheta,7,status)
         call udmfre(sfrac,7,status)
      endif
      return
      end


      subroutine regrid( nin, xin, yin,
     & nout, xout_lo, xout_hi, yout)
C     Takes an input grid (XIN) sized (NIN) with values (YIN) and
C     interpolates an output vector (YOUT) sized (NOUT) at the midpoints
C     between corresponding entries of the two output grids (XOUT_LO)
C     and (XOUT_HI) .
      IMPLICIT NONE
      integer nin, nout
      double precision xin(nin), yin(nin)
      real xout_lo(nout), xout_hi(nout),yout(nout)
C     LOCAL variables
      integer i
      double precision avg
      do 10 i=1,nout
         avg=xout_lo(i)+xout_hi(i)
         avg=avg/2.0
         call interpo(xin,yin,nin,avg,yout(i))
 10   continue
      return
      end


      subroutine interpo(xin,yin,n,xout,yout)
C     Input: vectors xin and yin of length n and an xout
C     Output: yout interpolated between values of yin on the grid xin
      implicit none
      integer n
      real yout
      double precision xout
      double precision xin(n),yin(n)
C LOCAL variables
      integer jlo
      save jlo
C
C find next point to xout
C
      call dhunt(xin,n,xout,jlo)
      if(jlo.eq.0) then
C     don't extrapolate
         xout=xin(1)
      else if(jlo.eq.n) then
         xout=xin(n)
      else
         yout=(yin(jlo+1)-yin(jlo))*(xout-xin(jlo))/
     $        (xin(jlo+1)-xin(jlo))+yin(jlo)
      endif
      return
      end


      SUBROUTINE DHUNT(XX,N,X,JLO)
C     Taken from Numerical Recipes
      implicit none
      integer n,jlo,jhi,inc,jm
      double precision XX(N),X
      logical ascnd

      ASCND=XX(N).GT.XX(1)
      IF(JLO.LE.0.OR.JLO.GT.N)THEN
         JLO=0
         JHI=N+1
         GO TO 3
      ENDIF
      INC=1
      IF(X.GE.XX(JLO).EQV.ASCND)THEN
 1       JHI=JLO+INC
         IF(JHI.GT.N)THEN
            JHI=N+1
         ELSE IF(X.GE.XX(JHI).EQV.ASCND)THEN
            JLO=JHI
            INC=INC+INC
            GO TO 1
         ENDIF
      ELSE
         JHI=JLO
 2       JLO=JHI-INC
         IF(JLO.LT.1)THEN
            JLO=0
         ELSE IF(X.LT.XX(JLO).EQV.ASCND)THEN
            JHI=JLO
            INC=INC+INC
            GO TO 2
         ENDIF
      ENDIF
 3    IF(JHI-JLO.EQ.1)RETURN
      JM=(JHI+JLO)/2
      IF(X.GT.XX(JM).EQV.ASCND)THEN
         JLO=JM
      ELSE
         JHI=JM
      ENDIF
      GO TO 3
      END


      integer function locat(x,xx,n)
C     lifted (basically) from Numerical Recipes.  Returns the index of
C     the member of sorted vector XX (length N)  which is just below X.
      implicit none
      integer n
      double precision XX(N),x
      integer jl,ju,jm
      JL=0
      JU=N+1
 10   IF(JU-JL.GT.1)THEN
         JM=(JU+JL)/2
         IF((XX(N).GT.XX(1)).EQV.(X.GT.XX(JM)))THEN
            JL=JM
         ELSE
            JU=JM
         ENDIF
         GO TO 10
      ENDIF
      locat=JL
      RETURN
      END


      subroutine srchkey(iunit,chatter,keywrd,keystr,ierr)
      implicit none
      integer iunit,ierr,chatter
      character*(*) keywrd,keystr
C     This subroutine searches through all extensions of a FITS file
C     for a given keyword and returns its value. If it can't find the
C     keyword, it returns 'NONE' and sets ierr=1.  Subroutine assumes
C     file is open and at the primary HDU.  Upon completion, routine
C     returns the file to this position.
C     Author
C     Lawrence E Brown (August 1994)
      character(160) message,comment
      integer htyp
      character(30) errstr, wrnstr
      character(7) version
      parameter (version='1.0.0')

C     LOCAL variables


      errstr = '** SRCHKEY '//version//' ERROR:'
      wrnstr = '** SRCHKEY '//version//' WARNING:'

 10   call ftgkys(iunit,keywrd,keystr,comment,ierr)
      if(ierr.ne.0)then
         if(ierr.eq.202)then
C     keywrd not found move ahead one HDU and try again
            ierr=0
            call ftcmsg()
            call ftmrhd(iunit,1,htyp,ierr)
            goto 10
         endif
C     if you're here, either keywrd wasn't present or something unknown
C     went wrong
         if(chatter.ge.10)then
            message=wrnstr // 'Couldn''t get'//keywrd//' keyword'
            call fcecho(message)
            message=wrnstr // 'Setting value to NONE'
            call fcecho(message)
            if(chatter.gt.20) call fcerrm(ierr)
            call ftcmsg()
         endif
         ierr=1
         keystr='NONE'
      endif
      call ftmahd(iunit,1,htyp,ierr)
      return
      end


      subroutine grab_meaningful_name(filfil,ifs,ife)
      implicit none
      character*(*) filfil
      integer ifs,ife,i
      do i=160,1,-1
         if(filfil(i:i).ne.' ') goto 10
      enddo
 10   ife=i
      do i=ife,1,-1
         if(filfil(i:i).eq.'/') goto 20
      enddo
      ifs=1
      return
 20   ifs=i+1
      return
      end
