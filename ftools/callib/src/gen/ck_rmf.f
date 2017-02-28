C -------------------------------------------------------------------------
*+ CK_RMF
      subroutine CK_RMF(chatter, rmffil, iunit, telescop, instrume, 
     &            detnam, filter,
     &            ienerg, rmfclas3, ierr)

      IMPLICIT NONE
      integer chatter, ierr, iunit, ienerg
      character*(*) rmffil, instrume, telescop
      character*(*) detnam, filter
      character*(*) rmfclas3

C Description
C   Subroutine to perform an initial check out the inputRMF file presented 
C to pcarf, determine whether there is a RMF extension present. If an 
C acceptable extension is found a bunch of instrumet-related keywords are 
C read from its header.
C   The file is left open and scrolled to the extension ifan acceptable
C extension is found, otherwise the file is CLOSED
C
C Passed Parameters
C  CHATTER          i   : Chattiness flag (<5 quiet, >15 noisey)
C  PHAFIL           i   : PHA filename (optionally incl extension #)
C  IUNIT              o : The Unit number of the PHA file
C  TELESCOP           o : Value of the TELESCOP keyword from located extn
C  INSTRUME           o : Value of the INSTRUME keyword from located extn
C  DETNAM             o : Value of the DETNAM keyword (or 'NONE')
C  FILTER             o : Value of the FILTER keyword (or 'NONE')
C  IENERG             o : Number of incident energy bins in the matrix
C  RMFCLAS3           o : Character string giving type of matrix
C  IERR               o : Subroutine error flag (zero = OK)
C
C Called Routines
C
C Author/Modification History
C  Ian M George (1.0.0:94 Jan 24) Original
C  Ian M George (2.0.0:94 Apr 29), added MVEXT calls
c
c Banashree M Seifert (2.1.0, 1997 Mar 27)
c      . report the name of the failed file
c Jeff Guerber (2.1.1, 1999-05-05) set filter var, not detnam, if FILTER
c      kwd read fails
c -----------------------------------------------------------------
      character(7) version
      parameter (version='2.1.1')
*-
C Max arrays
      integer maxextn
      parameter (maxextn=99)
C Internals
      integer  status
      integer ninstr, nsearch
        integer next(maxextn)
        character(20) instr(9)
        character(20) outhdu(9,maxextn), outver(9,maxextn)
        character(20) extnam(maxextn)
      character(30) errstr, wrnstr, comm
      character(80) message
      integer errflg
C Initialize
      ierr = 0 
             errstr = '** CK_RMF '//version//' ERROR:'
             wrnstr = '** CK_RMF '//version//' WARNING:'

C Start 

C ... give user info if requested
        if(chatter.GT.40) then
           message = ' ... using CK_RMF '// version
           call fcecho(message)
        endif

C -------------------------- RSP_MATRIX extension --------------------
C Find the RSP_MATRIX extension in the RMF file 
        ninstr = 2
        instr(1) = 'RESPONSE'
        instr(2) = 'RSP_MATRIX'
        nsearch = maxextn
      call mvext(0,rmffil, iunit, ninstr, instr, nsearch,
     &            next, outhdu, extnam, outver, 
     &            'SPECRESP MATRIX', errflg, chatter)
      if(errflg.ne.0) then
         message = 'Unable to locate extension for '//rmffil
         call fcecho(message)
         call ftclos(iunit, status) 
         ierr=errflg
         return
      endif


C Set the RMF Class to the value of the relevant HDUCLASn keyword.
      rmfclas3 = outhdu(3,1)

C --------- Read the necessary keywords -----------------
      
      status = 0
      call ftcmsg()
      call ftgkyj(iunit,'NAXIS2',ienerg,comm,status)
      message = errstr//' reading NAXIS2 value '
      IF (status.NE.0) THEN
        ierr = 4
        call fcecho(message)
      goto 483
      ENDIF

C TELESCOP ...
      status = 0
      call ftcmsg()
      telescop = 'UNKNOWN'
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      IF (status.EQ.202) THEN
         telescop = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         message = 'Error reading TELESCOP from '//rmffil
         call fcerr(message)
         call fcerrm(status)
      ENDIF

C INSTRUME ...
      status = 0
      call ftcmsg()
      instrume = 'UNKNOWN'
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      IF (status.EQ.202) THEN
         instrume = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         message = 'Error reading INSTRUME from '//rmffil
         call fcerr(message)
         call fcerrm(status)
      ENDIF

C DETNAM ...
      status = 0
      call ftcmsg()
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      IF (status.EQ.202) THEN
         detnam = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         message = 'Error reading DETNAM from '//rmffil
         call fcerr(message)
         call fcerrm(status)
      ENDIF


C FILTER ...
      status = 0
      call ftcmsg()
      call ftgkys(iunit,'FILTER',filter,comm,status)
      IF (status.EQ.202) THEN
         filter = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         message = 'Error reading FILTER from '//rmffil
         call fcerr(message)
         call fcerrm(status)
      ENDIF

C Check for errors
483     if(ierr.ne.0) then
           message = 'Error while reading '//rmffil
           call fcecho(message)
           call ftclos(iunit, status) 
           return
        endif

      return
      end
