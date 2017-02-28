*+ADDARF
        SUBROUTINE ADDARF
        IMPLICIT NONE
c
c Description:
c  Adds ARF files with specified weights, and writes result
c  to a single ARF file. The maximum number of input ARFs is defined
c  by MXARF in addarf.inc.
c  NOTE - The par file is assumed to have been opened.
c
c Passed Paremeters
c  None
c
c User i/ps required (prompted for):
c  None here, isolated in GP_CNVOIRSP (see below)
c
c Include/Common blocks files
c  common TASK                 : (FTOOLS) standard fatal error message thingy
c
c Called routines
c  subroutine GP_ADDARF       : (below) Gets parameters from XPI par file
c  subroutine CAL_ADDARF      : (below) Does the job
c
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ken Ebisawa      (1.0.0;1995 Oct 16) original
c  Ian M George     (1.0.1:1995 Oct 27) minor (mainly cosmetic) changes
c
c Banashree M Seifert (1.1.0:1996 Nov26)
c      . changes made by Keith A and Ken Ebisawa is put here
c        they are as below:
c Banashree M Seifert (1.2.0:1997 March 11)
c      . added option for input file list, as below:
c        Extra option added so that instead of giving file names,
c        user can also input '@filename', where filename is an ascii file
c        containing files to be added and factor(weight)
c        e.g., if file1, file2 to be added as file1*1 with file2*4, then
c        input file should have two lines as:
c         file1 1.0
c         file2 4.0
c
c      . for this a new subroutine added to reda the input ascii file
c        and parse them (rdlistfile)
c Ning Gan (1.2.1:1998 Aug. 28)
c      . changed the dimension of comment array from 10 to 800.
c Ning Gan (1.2.2:1999 Oct 25)
c        . Added dynamically allocated arrays of sum_arf, e_high, e_low. 
c        . Added a routine process_add_arf
c Ning Gan (1.2.3:1999 Nov 27)
c        . Changed  the read format from '(f)' to '(f22.0)' 
c Ning Gan (1.2.4:2000 Mar 01)
c        . Find ARF extensions automatically 
c Ning Gan (1.2.5:2000 Mar 19)
c        . Increase the size of filenames to 255.  
c        . Increase the total number of arf files from 100 to 2500.
c Ning Gan (1.2.6:2000 May 11)
c        . Fixed the write statements for output of error and warning
C          strings.
c     
c------------------------------------------------------------------------------

      character(7) version
      parameter (version = '1.2.7')
*-
c Internals
      character(511) message, context
      integer nfile, ierr, chatter
      include 'addarf.inc'
      character(255) arfnames(MXARFS), outarf
      real weights(MXARFS)
      logical clobber
      character(20) taskname
      integer clenact

c Initization
      COMMON/task/taskname
      taskname ='addarf '//version
      ierr = 0

c User Info
      message = '** addarf '//version
      call fcecho(message)

C Open and read parameter file
      call gp_addarf(nfile, arfnames, weights, outarf,
     $     clobber, chatter, ierr)

      if (ierr .ne. 0) go to 999

C The main part
      call cal_addarf(nfile, arfnames, weights, outarf, clobber,
     $     chatter,  ierr)

C Error handling
 999  continue
      if(ierr.NE.0) then
         context = 'Incomplete Execution'
         message = '** addarf '//version//' ERROR : '
     $         // context(:clenact(context))
         call fcecho(message)
         call fcerr(context)
      else 
         message = '** addarf '//version//' Finished'
         call fcecho(message)
      endif

      return
      end
c -----------------------------------------------------------------

*+ GP_ADDARF
      subroutine gp_addarf(nfile, arfnames, weights, outarf,
     $     clobber, chatter, ierr)

      integer nfile, ierr,  chatter
      character*(*)  arfnames(*), outarf
      real weights(*)
      logical clobber
c
c Description:
c  Gets the parameters required by ADDARF from the parameter file
c  NOTE - The XPI par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  IN_ARFS     - string containing list of arf filenames to be added
c  WEIGHTS     - string containing weighting factors of arfs
c  OUT_ARF     - O/p filename to be created
c  CLOBBER     - Falg as to whether overwriting of o/p file allowed
c  CHATTER     - chattiness flag for o/p (5 quite,9 normal,15 high,>20 silly)
c
c Origin:
c  Original
c
c Called Routines
c  subroutine FCECHO           : (FTOOLS) writes to standard o/p device
c
c Compilation:
c  requires XPI/Host interface etc and CALLIB
c
c Authors/Modification History:
c  Ken Ebisawa          (1.0.0;1995 Oct 16) original
c  Ian M George     (1.0.1:1995 Oct 27) minor (mainly cosmetic) changes
c
c Banashree M Seifert (1.1.0:1996 Nov 26)
c          . message was character(80). it is made character(180)
c Banashree M Seifert (1.2.0:1997 March 11)
c      . added option for input file list, as below:
c        Extra option added so that instead of giving file names,
c        user can also input '@filename', where filename is an ascii file
c        containing files to be added and factor(weight)
c        e.g., if file1, file2 to be added as file1*1 with file2*4, then
c        input file should have two lines as:
c         file1 1.0
c         file2 4.0
c------------------------------------------------------------------------------

      character(7) version
      parameter (version = '1.2.1')
*-
        
c Internals
      include 'addarf.inc'
      character(255) context
      character(1000) wtstring
      character(20)   st_weights(MXARFS)
      integer i, status, nweights
      logical flag
      character(255) errstr, wrnstr,outstr
      character(511) message
      character(5000) list
      character(5000) lstfil
      integer lstu
      integer clenact

c Initialize variables
      status = 0
      errstr = '** GP_ADDARF '//version//' ERROR: '
      wrnstr = '** GP_ADDARF '//version//' WARNING: '


C Get the input list parameter which could be either
C string for the ARF names or @fielname

      call uclgst('list', list, status)
      if (status .ne. 0) then
         context = 'Cannot get the listfile parameter'
         goto 999
      endif

      call crmvlbk(list)

* if list has '@' as first character, then
* >>1. read list file to load the names of RMF files, their weighting factors
* if not,
* >>1. Parse the string and get RMF names and number of files(fcgcls)
* >>2. Get the weights string(uclgst)
* >>3. Parse the string and get the weights and number of weights(fcgcls)
                                     
      lstu=8
      status=0
      if(list(1:1) .eq. '@') then
         lstfil=list(2:)
         CALL rdlistfile(lstfil, lstu, MXARFS, nfile, arfnames,
     &        weights, message, status)
         IF ( status .NE. 0 ) GOTO 999
      else

C Parse the string and get the ARF names and number of files
         flag = .false.
         call fcgcls(list, arfnames, nfile, flag)

C Get the weights
         call uclgst('weights',wtstring,status)
         if (status .ne. 0) then
            message = errstr(:clenact(errstr))// 
     &           'Getting WEIGHTS parameter'
            call fcecho(message)
            goto 999
         endif

C Parse the string and get the weights and number of weights
         flag = .false.
         call fcgcls(wtstring, st_weights, nweights, flag)
         do i = 1, nweights
            read(st_weights(i), *) weights(i)
         enddo
         
         if (nweights .ne. nfile) then
            ierr=999
            message = errstr(:clenact(errstr))//
     &           'Incompatible files/weights'
            call fcecho(message)
            write(context,'(a,i4,a,i4)') 
     $           ' ... Number of files:', nfile,
     $           ', Number of weights:', nweights
            call fcecho(message)
            go to 999
         endif

         CALL fcecho('Summing ...')

         do i=1,nfile
            WRITE(outstr(1:13),'(1pe10.3, a3)') weights(i), ' * '
            outstr(14:) = arfnames(i)
            CALL fcecho(outstr)
         enddo

      endif


C Get the output arf name
      call uclgst('out_ARF', outarf, status)
      if(status.ne.0) then
         message =errstr(:clenact(errstr))// 
     &        'Getting OUT_ARF parameter'
         call fcecho(message)
         goto 999
      endif

C  Get the cloober flag
      call uclgsb('clobber', clobber, status)
      if(status.ne.0) then
         message =errstr(:clenact(errstr))// 
     &        'Getting CLOBBER parameter'
         call fcecho(message)
         message = ' ... setting CLOBBER = .false.'
         clobber = .false.
         status = 0
      endif

C Get the chatter flag
      call uclgsi('chatter', chatter, status)
      if (status .ne. 0) then
         message =errstr(:clenact(errstr))// 
     &        'Getting CHATTER parameter'
         call fcecho(message)
         status = 0
         message = ' ... setting CHATTER = 9'
         call fcecho(message)
         chatter = 9
      endif

 999  continue

      if ((status .ne. 0).OR.(ierr.ne.0))  then
         ierr = 999
         message = ' ... GP_ADDARF '//version//' aborting'
         call fcecho(message)
      elseif(chatter.GE.20) then
         message = ' ... using GP_ADDARF ' // version
         call fcecho(message)
      endif  

      return
      end

c -----------------------------------------------------------------
*+ CAL_ADDARF
      subroutine cal_addarf(nfile, arfnames, weights, outarf, clobber,
     $     chatter, ierr)

      implicit none
      integer nfile, ierr, chatter
      character*(*) arfnames(*), outarf
      real weights(*)
      logical clobber
c
c Description:
c  Subroutine adds passed list of ARF files with passed weights, and writes 
c result to the specified o/p ARF file. 
c NOTE: Maximum number of input ARFs is 99.
c       Max number of energy bins is currently 2048
c
c Passed Parameters
C  NFILE           (i)   : Number of ARF files to be added
C  ARFNAMES         (i)   : Names of input ARFs
C  WEIGHTS         (i)   : weights for adding ARFs
C  OUTARF         (i)   : output arfname
c  CLOBBER         (i)   : clobber flag
c  CHATTER         (i)   : chatiness flag (9=normal)
c  IERR               (r): error flag (zero = successful completion)
c
c User i/ps required (prompted for):
c  None 
c
c Include/Common blocks files
c  common TASK                 : (FTOOLS) standard fatal error message thingy
c
c Called routines
c  subroutine GP_ADDARF       : (below) Gets parameters from XPI par file
c  subroutine CAL_ADDARF      : (below) Does the job
c
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ken Ebisawa          (1.0.0;1995 Oct 16) original
c  Ian M George     (1.0.1:1995 Oct 27) minor (mainly cosmetic) changes
c 
c Banashree M Seifert (1.1.0:1996 Nov26)
c        . message was same as wrnstr so was error. It has been 
c          made character(180)
c Ning Gan (1.1.1:1999 Oct 25)
c        . Added dynamically allocated arrays of sum_arf, e_high, e_low. 


      character(7) version
      parameter (version = '1.1.1')
*-
      include 'addarf.inc'

c Internals

      integer i, ii, n_energy_bin, status, clenact
      character(120) context
C     real sum_arf(maxne), e_low(maxne), e_high(maxne)
      real ftsver
      integer nk_hist, nk_comm, ounit
      character(120) hist(10), comment(maxcomm)
      character(255) errstr, wrnstr
      character(500) message
      logical qok
      integer funit
      integer sstatus
c initialize
      character(20) telescop, instrume, detnam, filter, arfversn
      data telescop/' '/, instrume/' '/, detnam/' '/,
     $     filter/' '/, arfversn/' '/

      character(20) taskname
      COMMON/task/taskname

C
C Get IRAF MEM common into main program.
C
      LOGICAL          MEMB(100)
      INTEGER          MEMI(100)
      INTEGER          MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB,MEMI,MEML,MEMR,MEMD,MEMX)
      COMMON /MEM/ MEMD

C     datatype gives a symbolic code for the data type, e.g.,
C     4 is Integer*4
C     1 is boolean
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 is complex

* pointers for dynamic arrays
      integer psum_arf, pe_low, pe_high
      character(255) arf_name
      integer arf_extn, ninstr, nsearch,
     $     nfound, next(maxextn), htype 
      character(20)  instr(9), outhdu(9,maxextn), outver(9,maxextn),
     $     extnam(maxextn)


      errstr = '** CAL_ADDARF '//version//' ERROR:'
      wrnstr = '** CAL_ADDARF '//version//' WARNING:'
      ierr = 0

c Here we go ...

      if (chatter.ge.20) then
         message = ' ... using CAL_ADDARF '// version
         call fcecho(message)
         call ftvers(ftsver)
         write(message,'(a,f6.3)')
     &        ' ... using FITSIO Version ', ftsver
         call fcecho(message)
         message = ' ... I/p file summary:'
         call fcecho(message)
         do i = 1, nfile
            write(context,'(a,i4)') ' ...... File No =', i
            call fcecho(context)
            context = ' ...... Name    ='// arfnames(i) 
            call fcecho(context)
            write(context,'(a,f7.3)') ' ...... Weight  =',  weights(i)
            call fcecho(context)
         end do
      end if

c *** Check for an illegal o/p filename
      call ck_file(outarf,arfnames, nfile, qok, .false., chatter)
      if(.not.qok) then
         ierr = -1
         goto 999
      endif

c ***   parse the filename 
      status = 0
      call fcpars(arfnames(1),arf_name,arf_extn,status)
      if(arf_extn.ge.0) then
         if(chatter.ge.15) then
            message = ' ...... ARF name is '//arf_name
            call fcecho(message)
            write(message,'(a, i3)') ' ...... ARF extension is ',
     $           arf_extn
            call fcecho(message)
         endif
      else
         if (chatter.ge.15) then
            message = '...... ARF extension is not specified for '
     $           //arfnames(i)
            call fcecho(message)
            message = '...... ARF extension will be searched'
            call fcecho(message)
         end if
         arf_extn = -99
      endif      

c     *** open one input file and find out the n_energy_bin 
      status = 0
      call ftgiou(funit,status)
      call ftnopn(funit, arfnames(1), 0, status)
      if(status.ne.0) then 
         message = 'Error open file: '// 
     $        arfnames(1)(1:clenact(arfnames(1)))
         call wt_ferrmsg(status, message)
         goto 999
      endif   

C     Jump to the specified extension or find the ARF extension
c     Find the SPECRESP extension in the ARF file 
c     - Extension number NOT given as part of arexp (search for HDUCLAS/EXTNAM)
c     This part was taken from marfrmf by Ian Geroge.
      if(arf_extn.LT.0) then
         ninstr = 2
         instr(1) = 'RESPONSE'
         instr(2) = 'SPECRESP'
         nsearch = maxextn
         call fndhdu(chatter, funit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
         if(nfound.lt.0) then
c     - check for old-style EXTNAME values if no acceptable HDUCLASn values found
            message = wrnstr(1:clenact(wrnstr))//
     &           ' Ext w/ allowed HDUCLASn keywrds not found'
            call fcecho(message)
            message = ' ... offending file: '//
     &           arf_name(:clenact(arf_name))
            call fcecho(message)
            message =
     $           ' .. searching for extnsion with EXTNAME = SPECRESP'
            call fcecho(message)
            call fndext(chatter, funit, 'SPECRESP',
     &           nsearch, nfound, next, outhdu, outver, extnam,
     $           ierr)
         endif
         if(nfound.ge.1) then
C     ARF extension having correct HDUCLASS keywords are found
            if (nfound.ge.2) then
C     Warning if there are two or more ARF extensions
               if (chatter.gt.0) then
                  message = wrnstr(1:clenact(wrnstr))//
     &                 ' there are more than two ARF extensions'
                  call fcecho(message)
                  message ='File name: '
     &                 //arf_name(1:clenact(arf_name))
                  call fcecho(message)
                  do ii = 1, nfound 
                     write(message,'(a, i3)')
     $                    'ARF extension no :', next(ii)
                     call fcecho(message)
                  end do
                  message ='The first ARF extension will be used'
                  call fcecho(message)
               endif
            endif
C     Jump to the first ARF extension 
            if (chatter.ge.10) then
               message = 'ARF extension is found'
               call fcecho(message)
               write(message, '(a,i2)') 'ARF extension number : ',
     $              next(1)
               call fcecho(message)
            endif
            call ftmahd(funit,1+next(1),htype,status)
            message = wrnstr(1:clenact(wrnstr))// 
     $           ' Problem moving to specified xtens'
            call wt_ferrmsg(status,message)
         endif
      else
c     - Extension number IS given as part of arexp
         call ftmahd(funit,arf_extn+1,htype,status)
         message = wrnstr(1:clenact(wrnstr))// 
     $        ' Problem moving to specified xtens'
         call wt_ferrmsg(status,message)
      endif

C     If error, terminate
      if (status.ne.0) then
         ierr = 999
         message = errstr(:clenact(errstr))
     &        //' Problem moving to specified xtens'
         call fcecho(message)
         go to 999
      end if
 
C     Find the total number of bins. 
      status = 0
      call ftgkyj(funit,'NAXIS2',n_energy_bin,message,status)
      IF (status.NE.0) THEN
         message = arfnames(1)(1:clenact(arfnames(1)))
     $        //':    reading NAXIS2 value '
         call wt_ferrmsg(status,message)
         goto 999
      ENDIF
      call ftclos(funit, status)
      call ftfiou(funit, status)

      status = 0

c *** Dynamically allocate the sum_arf, e_low, e_high array. 

      psum_arf = 0
      CALL udmget(n_energy_bin,6,psum_arf,status)
      if( status. ne. 0) then 
         message = 'Insufficient memory for the SUM_ARF array'
         call wt_ferrmsg(status,message)
         goto 999
      endif

      pe_low = 0
      CALL udmget(n_energy_bin,6,pe_low,status)
      if( status. ne. 0) then 
         message = 'Insufficient memory for the E_LOW array'
         call wt_ferrmsg(status,message)
         goto 999
      endif

      pe_high = 0
      CALL udmget(n_energy_bin,6,pe_high,status)
      if( status. ne. 0) then 
         message = 'Insufficient memory for the E_HIGH array'
         call wt_ferrmsg(status,message)
         goto 999
      endif
         
C Open input ARFs, check formats, and sum ARFs if ARF extension format is fine.
      call open_addarf(nfile, arfnames, weights, 
     $     nk_comm, comment, telescop, instrume, detnam, filter,
     $     arfversn, n_energy_bin, MEMR(pe_low), MEMR(pe_high),
     $     MEMR(psum_arf), chatter, ierr)
      if(ierr .ne. 0) goto 999

C Output the summed ARF (wtarf1 is a CALLIB routine)
      call ftgiou(ounit,ierr)
c P.array
      call opnpa(outarf, chatter, ounit, clobber, ierr)
      if(ierr .ne. 0) goto 999
      
      call FTPKYS(ounit,'CREATOR', taskname,
     $     's/w task which wrote this dataset',      status)
      if(status .ne. 0) then
         message = wrnstr(1:clenact(wrnstr))//
     $        ' Writing CREATOR keyword'
         call fcecho(message)
         status = 0
      endif
      nk_hist = 1
      hist(1) = 'This file is made with the ftool '//taskname

c ARF extension
      call wtarf1(ounit, chatter,  nk_hist, hist,
     &     nk_comm, comment, arfversn(1:clenact(arfversn)), 'UNKNOWN',
     &     telescop, instrume, detnam, filter,
     &     n_energy_bin, n_energy_bin, MEMR(pe_low), MEMR(pe_high), 
     &     MEMR(psum_arf),ierr)
      if(ierr .ne. 0) goto 999

      call FTPKYS(ounit,'CREATOR', taskname,
     $     's/w task which wrote this dataset', status)
      if(status .ne. 0) then
         message = wrnstr(1:clenact(wrnstr))//
     $        ' Writing CREATOR keyword'
         call fcecho(message)
         status = 0
      endif
c Shut up
      call ftclos(ounit, status)
      if(status .ne. 0) then
         message = errstr(:clenact(errstr))//' Closing O/p file'
         call wt_ferrmsg(status, message)
         goto 999
      endif
      call ftfiou(ounit, status)

 999  continue
c Last check for errors
      if((status.ne.0).or.(ierr.ne.0)) then
         message = errstr(:clenact(errstr))// ' Fatal'
         call fcecho(message)
      endif
      sstatus = 0
      if(psum_arf.ne.0) call udmfre(psum_arf,6,sstatus)
      sstatus = 0
      if(pe_low.ne.0)  call udmfre(pe_low,6,sstatus)
      sstatus = 0
      if(pe_high.ne.0)  call udmfre(pe_high,6,sstatus)

      return
      end

c --------------------------------------------------------------

*+ OPEN_ADDARF
      subroutine open_addarf(nfile, arfnames, weights, 
     $     nk_comm, comment, telescop, instrume, detnam,
     $     filter, arfversn, n_energy_bin, energy_lo, energy_hi,
     $     sum_arf, chatter, ierr)
      
      implicit none
      integer nfile, nk_comm, n_energy_bin, chatter, ierr
      character(255) arfnames(nfile)
      character*(*) telescop, instrume, detnam, filter, arfversn
      character*(*) comment(*)
      real weights(nfile), energy_lo(*), energy_hi(*), sum_arf(*)
c
c Description:
c  Open input ARFs, check formats, and sum ARFs if ARF extension format is fine.
c
c Passed Parameters
C  NFILE           (i)   : Number of ARF files to be added
C  ARFNAMES         (i)   : Names of input ARFs
C  WEIGHTS         (i)   : weights for adding ARFs
c  NK_COMM            (r): No. comment cards
c  COMMENT            (r): Array of comment cards
c  TELESCOPE          (r): Name of TELESCOP from ARF files
c  INSTRUME           (r): Name of INSTRUME from ARF files
c  DETNAM             (r): Name of DETNAM from ARF files
c  FILTER             (r): Name of FILTER from ARF files
c  ARFVERSN           (r): ARFVERSN keyword value in use
c  N_ENERGY_BIN       (r): Number of energy bins in ARF array 
c  ENERG_LO           (r): Lower energy bound array
c  ENERG_HI           (r): Upper energy bound array
c  SUM_ARF            (r): Summed ARF array
c  CHATTER         (i)   : chatiness flag (9=normal)
c  IERR               (r): error flag (zero = successful completion)
c
c Called routines
c  {incomplete}
c
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ken Ebisawa          (1.0.0;1995 Oct 16) original
c  Ian M George     (1.0.1:1995 Oct 27) minor (mainly cosmetic) changes
c  Ian M George     (1.0.2:1995 Nov 21) fixed typo
c  Ning Gan (1.0.2:1999 Oct 25)
c        . Added dynamically allocated arrays of sum_arf, e_high, e_low. 

      character(7) version
      parameter (version = '1.0.2')
*     -
c Max Array sizes (disable the maxchan, maxne, maxgrp)
      include 'addarf.inc'

      character(255) arf_name
      character(512)  message
      integer i, ii, arf_extn, status, iunit, block, ninstr, nsearch,
     $     nfound, next(maxextn), htype, clenact
      character(40) errstr, wrnstr

      character(20)  instr(9), outhdu(9,maxextn), outver(9,maxextn),
     $     extnam(maxextn)


C     Variables  for the dynamical array.
      LOGICAL          MEMB(100)
      INTEGER          MEMI(100)
      INTEGER          MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)      
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB,MEMI,MEML,MEMR,MEMD,MEMX)
      COMMON /MEM/ MEMD
      integer e_lo, e_hi, sprsp
      integer sstatus 

C Initialization
      ierr = 0
      errstr = '** OPEN_ADDARF '//version//' ERROR: '
      wrnstr = '** OPEN_ADDARF '//version//' WARNING: '
      telescop = 'UNKNOWN'
      instrume = 'UNKNOWN'
      detnam = ' '
      filter = 'NONE'
      nk_comm = 0

c *** Dynamically allocate the sprsp, e_lo, e_hi array.

      sprsp = 0
      CALL udmget(n_energy_bin,6,sprsp,status)
      if( status. ne. 0) then
         message = 'Insufficient memory for the SPRSP array'
         call wt_ferrmsg(status,message)
         goto 666
      endif

      e_lo = 0
      CALL udmget(n_energy_bin,6,e_lo,status)
      if( status. ne. 0) then
         message = 'Insufficient memory for the E_LO array'
         call wt_ferrmsg(status,message)
         goto 666
      endif

      e_hi = 0
      CALL udmget(n_energy_bin,6,e_hi,status)
      if( status. ne. 0) then
         message = 'Insufficient memory for the E_HI array'
         call wt_ferrmsg(status,message)
         goto 666
      endif

c User Info
      if(chatter.GE.20) then
         message = ' ... using OPEN_ADDARF ' // version
         call fcecho(message)
      endif


      if(chatter.GE.9) then
         message = ' ... processing: '
         call fcecho(message)
      endif

      do i = 1, nfile
         nk_comm = nk_comm + 1
         write(comment(nk_comm),'(a,i2,a,a)') 'File',i,' : ',
     $        arfnames(i)(:MIN(clenact(arfnames(i)),
     $        len(comment(nk_comm))-9))
         nk_comm = nk_comm + 1
         write(comment(nk_comm),'(a,f9.4)') 'Weight: ',
     $        weights(i)
         if (chatter.ge.9) then
            write(message,'(a, i4)') '...... file number ', i
            call fcecho(message)
         endif
C   Open the each ARF file
         call fcpars(arfnames(i),arf_name,arf_extn,status)
         if(arf_extn.ge.0) then
            if(chatter.ge.15) then
               message = ' ...... ARF name is '
     &              //arf_name(1:clenact(arf_name))
               call fcecho(message)
               write(message,'(a, i3)') ' ...... ARF extension is ',
     $              arf_extn
               call fcecho(message)
            endif
         else
            if (chatter.ge.15) then
               message = '...... ARF extension is not specified for '
     $              //arfnames(i)(:clenact(arfnames(i)))
               call fcecho(message)
               message = '...... ARF extension will be searched'
               call fcecho(message)
            end if
            arf_extn = -99
         endif      
         
         call ftgiou(iunit,status)
         call ftopen(iunit, arf_name, 0, block, status)
         if(status.ne.0) then
            ierr = 999
            message = errstr(:clenact(errstr))// ' Opening ARF '
            call fcecho(message)
            message = '... Offending file: '//
     &           arf_name(:clenact(arf_name))
            call wt_ferrmsg(status,message)
            go to 666
         endif

C     Jump to the specified extension or find the ARF extension
c     Find the SPECRESP extension in the ARF file 
c     - Extension number NOT given as part of arexp (search for HDUCLAS/EXTNAM)
c     This part was taken from marfrmf by Ian Geroge.
         if(arf_extn.LT.0) then
            ninstr = 2
            instr(1) = 'RESPONSE'
            instr(2) = 'SPECRESP'
            nsearch = maxextn
            call fndhdu(chatter, iunit, ninstr, instr,
     &           nsearch, nfound, next, outhdu, outver, extnam, ierr)
            if(nfound.lt.0) then
c     - check for old-style EXTNAME values if no acceptable HDUCLASn values found
               message = wrnstr(1:clenact(wrnstr))//
     &              ' Ext w/ allowed HDUCLASn keywrds not found'
               call fcecho(message)
               message = ' ... offending file: '//
     &              arf_name(:clenact(arf_name))
               call fcecho(message)
               message =
     $              ' .. searching for extnsion with EXTNAME = SPECRESP'
               call fcecho(message)
               call fndext(chatter, iunit, 'SPECRESP',
     &              nsearch, nfound, next, outhdu, outver, extnam,
     $              ierr)
            endif
            if(nfound.ge.1) then
C     ARF extension having correct HDUCLASS keywords are found
               if (nfound.ge.2) then
C     Warning if there are two or more ARF extensions
                  if (chatter.gt.0) then
                     message = wrnstr(1:clenact(wrnstr))//
     &                    ' there are more than two ARF extensions'
                     call fcecho(message)
                     message ='File name: '//
     &                    arf_name(:clenact(arf_name))
                     call fcecho(message)
                     do ii = 1, nfound 
                        write(message,'(a, i3)')
     $                       'ARF extension no :', next(ii)
                        call fcecho(message)
                     end do
                     message ='The first ARF extension will be used'
                     call fcecho(message)
                  endif
               endif
C     Jump to the first ARF extension 
               if (chatter.ge.10) then
                  message = 'ARF extension is found'
                  call fcecho(message)
                  write(message, '(a,i2)') 'ARF extension number : ',
     $                 next(1)
                  call fcecho(message)
               endif
               call ftmahd(iunit,1+next(1),htype,status)
               message = wrnstr(1:clenact(wrnstr))// 
     $              ' Problem moving to specified xtens'
               call wt_ferrmsg(status,message)
            endif
         else
c     - Extension number IS given as part of arexp
            call ftmahd(iunit,arf_extn+1,htype,status)
            message = wrnstr(1:clenact(wrnstr))// 
     $           ' Problem moving to specified xtens'
            call wt_ferrmsg(status,message)
         endif

C     If error, terminate
         if (status.ne.0) then
            ierr = 999
            message = errstr(:clenact(errstr))//
     $           ' Problem moving to specified xtens'
            call fcecho(message)
            go to 666
         end if

         call process_add_arf(iunit, i, arfversn, telescop, 
     &        instrume, detnam, filter, 
     &        n_energy_bin, weights(i),  
     &        energy_lo, energy_hi, sum_arf, 
     &        MEMR(e_lo), MEMR(e_hi),MEMR(sprsp), 
     &        nk_comm, comment, chatter, status ) 
     
         if(status.ne.0) then
            ierr = 999
            goto 666
         end if

C     Close the fits file
         call ftclos(iunit, status)
         if(status.ne.0) then
            message = wrnstr(1:clenact(wrnstr))//
     &           ' Problem closing File'
            call wt_ferrmsg(status, message)
            status = 0
         endif
         call ftfiou(iunit, status)
      enddo
 666  continue

c Last check for errors
      if((status.ne.0).or.(ierr.ne.0)) then
         message = errstr(:clenact(errstr)) // 
     &        ' Unable to continue'
         call fcecho(message)
      endif
      sstatus = 0
      if(sprsp.ne.0) call udmfre(sprsp,6,sstatus)
      sstatus = 0
      if(e_lo.ne.0)  call udmfre(e_lo,6,sstatus)
      sstatus = 0
      if(e_hi.ne.0)  call udmfre(e_hi,6,sstatus)

      return
      end

C
C   Processing each arffile.
C
C   Most of the codes are from the OPEN_ADDARF.
C
      Subroutine process_add_arf(iunit, iarf, arfversn, telescop, 
     &     instrume, detnam, filter, 
     &     n_energy_bin, weight,  
     &     energy_lo, energy_hi, sum_arf, e_lo, e_hi,sprsp, 
     &     nk_comm, comment, chatter, status ) 

      IMPLICIT NONE
c  Ning Gan (1.0.0:1999 Oct 25)
c        . Added dynamically allocated arrays of sum_arf, e_high, e_low. 

C
C Arguments
C
      integer iunit, iarf 
      character*(*) telescop, instrume, detnam, filter,arfversn 
      integer n_energy_bin
      real energy_lo(*), energy_hi(*), sum_arf(*)
      real e_lo(*), e_hi(*), sprsp(*)
      real weight
      integer nk_comm
      CHARACTER*(*) Comment(*)
      integer status, chatter
      integer clenact

C
C Internals 
C
      character(20)  t_telescop, t_instrume
      integer ii, n_energ
      integer ierr
      character(255) errstr, wrnstr
      character(511) message
      character(7) version
      parameter (version = '1.0.0')

c Initialize variables
      status = 0
      errstr = '** PROCESS_ADD_ARF '//version//' ERROR: '
      wrnstr = '** PROCESS_ADD_ARF '//version//' WARNING: '
      
      ierr = 0
C     Read the ARF extension
      call rdarf1(iunit,chatter,t_telescop,t_instrume,detnam,filter,
     &     n_energ, e_lo,e_hi,sprsp, arfversn,ierr)
      if(ierr.ne.0) then 
         status = ierr
         message = errstr(1:clenact(errstr))//
     &        ' RDARF1, Problem reading arf file'
         call fcecho(message)
         go to 6666
      endif
C     Now, we are at the ARF extension of the i-th ARF file. 
C     Save the n_energy_bin, e_lo and e_hi for the first file,
C     and make sure the other files have the same values.  
C     If not, terminate.
C     Also, check the consistency of TELESCOP, INSTRUME
      nk_comm = nk_comm + 1
      write(comment(nk_comm),'(a,a)') 'TELESCOP: ', t_telescop
      nk_comm = nk_comm + 1
      write(comment(nk_comm),'(a,a)') 'INSTRUME: ', t_instrume
      if (iarf .eq. 1) then
         n_energy_bin = n_energ
         telescop = t_telescop
         instrume = t_instrume
         do ii = 1, n_energy_bin
            energy_lo(ii) = e_lo(ii)
            energy_hi(ii) = e_hi(ii)
            sum_arf(ii) = weight*sprsp(ii)
         end do
      else
         if (telescop .ne. t_telescop) then
            message = wrnstr(1:clenact(wrnstr))//' TELESCOP mismatch'
            nk_comm = nk_comm + 1
            write(comment(nk_comm),'(a)') message(:clenact(message))
            call fcecho(message)
            write(message,*) ' ... TELESCOP of the first file :',
     $           telescop(1:clenact(telescop))
            call fcecho(message)
            write(message,*) ' ... TELESCOP of this file :',
     $           t_telescop(1:clenact(t_telescop))
            call fcecho(message)
            write(message,'(a)')
     $           ' ... TELESCOP of the first file will be used for o/p.'
            call fcecho(message)
            nk_comm = nk_comm + 1
            write(comment(nk_comm),'(a)') message(:clenact(message))
         end if
         if (instrume .ne. t_instrume) then
            message = wrnstr(1:clenact(wrnstr))//' INSTRUME mismatch'
            nk_comm = nk_comm + 1
            write(comment(nk_comm),'(a)') message(:clenact(message))
            call fcecho(message)
            write(message,*) ' ... INSTRUME of the first file :',
     $           instrume(:clenact(instrume))
            call fcecho(message)
            write(message,*) ' ... INSTRUME of this file :',
     $           t_instrume(:clenact(t_instrume))
            call fcecho(message)
            write(message,'(a)')
     $           ' ... INSTRUME of the first file will be used for o/p.'
            call fcecho(message)
            nk_comm = nk_comm + 1
            write(comment(nk_comm),'(a)') message(:clenact(message))
         end if
         if (n_energy_bin .ne. n_energ) then
            ierr = 999
            status = ierr
            message = errstr(:clenact(errstr))//
     $           ' Energy bins mismatch'
            call fcecho(message)
            go to 6666
         endif
         do ii = 1, n_energy_bin
            if (energy_lo(ii) .ne. e_lo(ii) .or.
     $           energy_hi(ii) .ne. e_hi(ii)) then
               ierr = 999
               status = ierr
               message = errstr(:clenact(errstr))//
     $              ' Different E_LO or E_HI values'
               call fcecho(message)
               go to 6666
            else
               sum_arf(ii) = sum_arf(ii)+weight*sprsp(ii)
            endif
         end do
      endif
 6666 if((status.ne.0).or.(ierr.ne.0)) then
         message = errstr(:clenact(errstr)) // 
     $        ' Unable to continue'
         call fcecho(message)
      endif
      return
      end

c -----------------------------------------------------------
*+RDLISTFILE

      SUBROUTINE rdlistfile(lstfil, lstu, MXARFS, nfiles, 
     &     inarfs, infact, contxt, errstat)

      INTEGER MXARFS

      REAL    infact(*)

      INTEGER lstu, nfiles

      CHARACTER*(*) inarfs(*)
      CHARACTER*(*) lstfil, contxt
      integer errstat

c  Subroutine to read the list file and load the arrays of RMF names and 
c  weighting factors.

c  Arguments :
c     lstfil        c       i: name of file with list of RMFs
c     wgtfil        c       i: name of file with weightings
c     lstu          i       i: I/O unit for above
c     MXARFS        i       i: Max number of RMFs allowed (parameter)
c     nfiles        i       r: Number of RMFs in lstfil
c     inarfs        c       r: Array of RMF names
c     infact        r       r: Array of RMF weighting factors
c     contxt        c       r: error description
c     errstatus     i       r: Status  : 0 = OK, !0 = !OK

      INTEGER ierr, i

      character(511) instrg, outstr

      errstat = 0

c Open the list file

      OPEN(unit=lstu, file=lstfil, status='old')

c Read the lines of the list file, loading the arrays as we go

      nfiles = 0
      READ(lstu,'(a)',iostat=ierr) instrg

      CALL fcecho('Summing ...')

      DO WHILE ( ierr .EQ. 0 )

         IF ( nfiles .EQ. MXARFS ) THEN
            write(contxt,'(a,i4,a)') 'I can only deal with ', 
     *           MXARFS, ' files.'
            CALL fcecho(contxt)
            errstat = 1
            RETURN
         ENDIF

         nfiles = nfiles + 1

         CALL crmvlbk(instrg)
         i = 1
         DO WHILE ( instrg(i:i) .NE. ' ' )
            i = i + 1
         ENDDO
         inarfs(nfiles) = instrg(:i-1)

         READ(instrg(i:),'(f22.0)') infact(nfiles)

         outstr(14:) = inarfs(nfiles)
         WRITE(outstr(1:13),'(1pe10.3, a3)') infact(nfiles), ' * '
         CALL fcecho(outstr)

         READ(lstu,'(a)',iostat=ierr) instrg

      ENDDO

      CLOSE(lstu)

      IF ( nfiles .EQ. 0 ) THEN
         contxt = 'Failed to read any input ARFs'
         CALL fcecho(contxt)
         errstat = 1
         RETURN
      ENDIF

      RETURN
      END

