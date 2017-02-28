C -------------------------------------------------------------------------
*+ CK_XTELC
       subroutine CK_XTELC(chatter, lcfil, iunit, telescop,
     $     instrume, detnam, filter, obs_date, obs_tim, ierr)


      IMPLICIT NONE
      integer chatter, ierr, iunit
      character*(*) lcfil
      character(8) instrume, telescop, detnam, filter
C	the following change made by ZG.
C      character(16) obs_date, obs_tim
	character*(*) obs_date, obs_tim

C Description
C   Subroutine to perform an initial check out the input XTE light
c     curve files.  If an acceptable extension is found then a bunch of
c     observation-related keywords are read from its header and returned.
C     The file is left open and scrolled to the extension if an acceptable
C     extension is found, otherwise the file is CLOSED
C
C Passed Parameters
C  CHATTER          i   : Chattiness flag (<5 quiet, >15 noisey)
C  LCFIL           i   : PHA filename (optionally incl extension #)
C  IERR               o : Subroutine error flag (zero = OK)
C  IUNIT              o : The Unit number of the PHA file
C  TELESCOP           o : Value of the TELESCOP keyword from located extn
C  INSTRUME           o : Value of the INSTRUME keyword from located extn
C  DETNAM             o : Value of the DETNAM keyword (or 'NONE')
C  FILTER             o : Value of the FILTER keyword (or 'NONE')
C  OBS_DATE           o : Value of the DATE_OBS keyword from located extn
C  OBS_TIM            o : Value of the OBS_TIME keyword from located extn
C
C Called Routines
C     mvext - CALLIB routine to open file and move to desired extionsion
C
C Author/Modification History
C     Jim Lochner May 1995, based on CK_PHA (v2.1.0) from CALLIB

      character(7) version
      parameter (version='2.1.0')
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
      character(30) errstr, wrnstr
      character(70) comm  
      character(80) message, mssg
      character(20) extn
      integer errflg, typflag
C Initialize
      errflg=0
      typflag = 0
      errstr = '** CK_XTEPHA '//version//' ERROR:'
      wrnstr = '** CK_XTEPHA '//version//' WARNING:'
      
C     Main
C     ... give user info if requested
      if(chatter.GT.40) then
         message = ' ... using CK_XTEPHA '// version
         call fcecho(message)
      endif
      
C     Look for the LIGHTCURVE extension in the PHA file 
      ninstr = 1
      instr(1) = 'LIGHTCURVE'
      nsearch = maxextn
      call mvext(0,lcfil, iunit, ninstr, instr, nsearch,
     &     next, outhdu, extnam, outver,
     &     extn, errflg, chatter)

      if (errflg .ne. 0) then
         mssg = errstr//' Cannot find LIGHTCURVE extension'
         go to 482
      endif
      
C --------- Read the necessary keywords -----------------
      
C TELESCOP ...
      status = 0
      call ftcmsg()
      telescop = 'UNKNOWN'
      call ftgkys(iunit,'TELESCOP',telescop,comm,status)
      call ftupch(telescop)
      IF (status.EQ.202) THEN
         telescop = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         message = errstr//' reading TELESCOP'
         call fcerr(message)
         call fcerrm(status)
      ENDIF

C INSTRUME ...
      status = 0
      call ftcmsg()
      instrume = 'UNKNOWN'
      call ftgkys(iunit,'INSTRUME',instrume,comm,status)
      call ftupch(instrume)
      IF (status.EQ.202) THEN
         instrume = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         message = errstr//' reading INSTRUME'
         call fcerr(message)
         call fcerrm(status)
      ENDIF

C DETNAM ...
      status = 0
      call ftcmsg()
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call ftupch(detnam)         
      IF (status.EQ.202) THEN
         detnam = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         message = errstr//' reading DETNAM'
         call fcerr(message)
         call fcerrm(status)
      ENDIF

C FILTER ...
      status = 0
      call ftcmsg()
      call ftgkys(iunit,'FILTER',filter,comm,status)
      call ftupch(filter)         
      IF (status.EQ.202) THEN
         filter = 'UNKNOWN'
         status=0
         call ftcmsg()
      else if (status.ne.0) then
         message = errstr//' reading FILTER'
         call fcerr(message)
         call fcerrm(status)
      ENDIF

C OBS-DATE ...
      status = 0
      call ftcmsg()
      obs_date = 'UNKNOWN'
      call ftgkys(iunit,'DATE-OBS',obs_date,comm,status)
      IF (status.EQ.202) THEN
         obs_date = 'UNKNOWN'
         status=0
      else if (status.ne.0) then
         message = errstr//' reading OBS-DATE'
         call fcerr(message)
         call fcerrm(status)
      ENDIF


C OBS-TIME ...
      status = 0
      call ftcmsg()
      obs_tim = 'UNKNOWN'
      call ftgkys(iunit,'TIME-OBS',obs_tim,comm,status)
      IF (status.EQ.202) THEN
         obs_tim = 'UNKNOWN'
         status=0
      else if (status.ne.0) then
         message = errstr//' reading OBS-DATE'
         call fcerr(message)
         call fcerrm(status)
      ENDIF

C -------------- End of keyword reading -----------------


C Check for errors
482     if(errflg.ne.0) then
                message = errstr // ' Unable to recover'
                call fcecho(message)
              call ftclos(iunit, status) 
              ierr=errflg
            return
        endif

      return
      end
