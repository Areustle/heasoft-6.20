C -------------------------------------------------------------------------
*+ CK_PHA
       subroutine CK_PHA(chatter, phafil, ierr)


      IMPLICIT NONE
      integer chatter, ierr, typflag, iunit
      character*(*) phafil
      character(8) instrume, telescop, detnam, filter
c      character(16) obs_date, obs_tim
      character(68) obs_date, obs_tim
      integer nx_or_ntheta,ny
      common /pha_block/ iunit, telescop, instrume, detnam, filter,
     &             typflag, obs_date, obs_tim, nx_or_ntheta,ny

C Description
C   Subroutine to perform an initial check out the input PHA file presented 
C to pcarf, determine whether there is (first) a WMAP or (if not, then 
C secondly) an OAHIST dataset present. If an acceptable extension is found
C then a bunch of observation-related keywords are read from its header and
C placed in the COMMON BLOCK /phablock/.
C   The file is left open and scrolled to the extension ifan acceptable
C extension is found, otherwise the file is CLOSED
C
C Passed Parameters
C  CHATTER          i   : Chattiness flag (<5 quiet, >15 noisey)
C  PHAFIL           i   : PHA filename (optionally incl extension #)
C  IERR               o : Subroutine error flag (zero = OK)
C  IUNIT             co : The Unit number of the PHA file
C  TELESCOP          co : Value of the TELESCOP keyword from located extn
C  INSTRUME          co : Value of the INSTRUME keyword from located extn
C  DETNAM            co : Value of the DETNAM keyword (or 'NONE')
C  FILTER            co : Value of the FILTER keyword (or 'NONE')
C  TYPFLAG           co : Flag to indicating whether WMAP (typflag=1) 
C                              or OAHIST (typflag=2) located
C  OBS_DATE          co : Value of the DATE_OBS keyword from located extn
C  OBS_TIM           co : Value of the OBS_TIME keyword from located extn
C  NX_OR_NTHETA      co : The X dimension of the WMAP or the number of theta 
C                         values in the OAHIST
C  NY                co : The Y dimension of the WMAP
C  *co means output in COMMON
C Called Routines
C
C Author/Modification History
C  Ian M George (1.0.0:93 Dec 18) Original
C  Ian M George (2.0.0:94 Apr 29), added MVEXT calls
C  Lawrence E Brown (2.1.0:94 Apr 29), fixed up extension search keywords
c
C Banashree Mitra Seifert (3.0.0: May 1996) 
C           . modified the search for keyword FILTER.
C                if it doesn't find FILTER keyword, it looks for
C                XS-FILTER keyword.  If that is not there, then
C                FILTER is set to NONE.  Note that XS-FILTER is an
C                integer.  Now if XS-FILTER=1, then FILTER=BORON
C                if XS-FILTER=0, then FILTER=NONE.
C           . introduced screen display routines instead of calling for
C             FCERR etc.
c
c Banashree Mitra Seifert (3.1.0: Oct9, 1996)
c           . initialisation (LINUX problem)
c  Banashree M Seifert (3.2.0: Nov6, 1996)
c           . problem with mver is fixed by extname=-1
c  Ning Gan (3.2.1: Jul2, 1998)
c           .Modified the length of time/date string to 68. 
C --------------------------------------------------------------------
                   
*-
C Max arrays
      integer maxextn
      parameter (maxextn=99)
C Internals
      integer  status
      integer ninstr, nsearch, extnum
        integer next(maxextn)
        character(20) instr(9)
        character(20) outhdu(9,maxextn), outver(9,maxextn)
        character(20) extnam(maxextn)
      character(70) comm  
      character(80) message
      character(20) extn
      integer errflg
c -----------------------------------------------------------------------
c these parameters are added by Banashree M Seifert in the version 3.0.0
c -----------------------------------------------------------------------
      integer ifilter
      character(7) subname
      parameter (subname='ck_pha')
      character(5) version
      parameter (version='3.2.0')

c ------------------------ addition completed ---------------------------

C Initialize
      errflg=0
      typflag = 0
      ierr=0
      status=0
      
C     Main
C     ... give user info if requested
      message = 'using '//subname//' Ver '// version
      call wtinfo(chatter,30,3,message)
      
C     Look for the WMAP extension in the PHA file 
      ninstr = 2
      extn=' '
      instr(1) = 'IMAGE'
      instr(2) = 'WMAP'
      nsearch = maxextn
      call mvext(0,phafil, iunit, ninstr, instr, nsearch,
     &     next, outhdu, extnam, outver,
     &     extn, errflg, chatter)
      if(errflg.EQ.0) typflag = 1      
C     if errflg.eq.3, look for DETECTOR extension
      if(errflg.EQ.3) then
         errflg=0
         message = 'No WMAP found, looking for Off-axis '//
     &             'histogram'
         call wtinfo(chatter,10,2,message)
         
         ninstr = 2
         instr(1) = 'SPECTRUM'
         instr(2) = 'DETECTOR'

ccc         extnum=0
         extnum=-1
         call mver(iunit,extnum,ninstr, instr, nsearch,
     &        next, outhdu, extnam, outver, 
     &        'DETECTOR', errflg, chatter)
         if(errflg.EQ.0) typflag = 2
      endif
      if(typflag.ne.1.and.typflag.ne.2) goto 482
      if(typflag.eq.1) then
C     get WMAP sizes
         ierr=0
         call ftgkyj(iunit,'NAXIS1',nx_or_ntheta,comm,ierr)
         call ftgkyj(iunit,'NAXIS2',ny,comm,ierr)
      else
C     get OAH size
         call ftgkyj(iunit,'NAXIS2',nx_or_ntheta,comm,ierr)
      endif

C --------- Read the necessary keywords -----------------
      
C TELESCOP ...
      status = 0
      telescop = 'UNKNOWN'
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call ftupch(telescop)
      IF (status.EQ.202) THEN
         telescop = 'UNKNOWN'
         status=0
      else if (status.ne.0) then
         message = ' reading TELESCOP'
         call wterrm(subname,version,message)
      ENDIF

C INSTRUME ...
      status = 0
      instrume = 'UNKNOWN'
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call ftupch(instrume)
      IF (status.EQ.202) THEN
         instrume = 'UNKNOWN'
         status=0
      else if (status.ne.0) then
         message = ' reading INSTRUME'
         call wterrm(subname,version,message)
      ENDIF

C DETNAM ...
      status = 0
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call ftupch(detnam)         
      IF (status.EQ.202) THEN
         detnam = 'UNKNOWN'
         status=0
      else if (status.ne.0) then
         message = ' reading DETNAM'
         call wterrm(subname,version,message)
      ENDIF

C FILTER ...
      status = 0
      call ftgkys(iunit,'FILTER',filter,comm,status)
      call ftupch(filter)         
      IF (status.EQ.202) THEN
         call ftgkyj(iunit,'XS-FILTR',ifilter,comm,status)
         if(status .ne. 0) then
            filter = 'UNKNOWN'
            status=0
         else
            if(ifilter .eq. 0) then
               filter='NONE'
            else
               filter='BORON'
            endif
         endif    
         status=0
      else if (status.ne.0) then
            filter='NONE'
            status=0
      ENDIF

C OBS-DATE ...
      status = 0
      obs_date = 'UNKNOWN'
      call ftgkys(iunit,'DATE-OBS',obs_date,comm,status)
      IF (status.EQ.202) THEN
         obs_date = 'UNKNOWN'
         status=0
      else if (status.ne.0) then
         message = ' reading OBS-DATE'
         call wterrm(subname,version,message)
      ENDIF


C OBS-TIME ...
      status = 0
      obs_tim = 'UNKNOWN'
      call ftgkys(iunit,'TIME-OBS',obs_tim,comm,status)
      IF (status.EQ.202) THEN
         obs_tim = 'UNKNOWN'
         status=0
      else if (status.ne.0) then
         message = ' reading OBS-DATE'
         call wterrm(subname,version,message)
      ENDIF

C -------------- End of keyword reading -----------------

C  Resolve PSPC naming irregularities
      call pspcres(instrume,obs_date,errflg) 
      if(errflg.ne.0) then
         message=' Unable to resolve instrument name: '//
     $        instrume
         call wterrm(subname,version,message)
      endif
      
C Check for errors
482     if(errflg.ne.0) then
                message = ' Unable to recover'
                call wterrm(subname,version,message)
              call ftclos(iunit, status) 
              ierr=errflg
            return
        endif

      return
      end

c -----------------------------------------------------------------
c                    end of ck_pha 
c -----------------------------------------------------------------

