C -------------------------------------------------------------------------
*+ CK_CRF
       subroutine CK_CRF(chatter, crffil, instr, extnsn, iunit, 
     $     telescop, instrume, detnam, filter, nenergy, ntheta, 
     $     nphi, lecol, hecol, thcol, phcol, ierr)

      IMPLICIT NONE
      integer chatter, ierr, iunit
      character*(*) crffil, instrume, telescop, detnam, filter
      character*(*) instr(9),extnsn
      integer  lecol,hecol,thcol,phcol,nenergy,ntheta,nphi
      

C Description
C   Subroutine to perform an initial check out of the input CRF file presented 
C to pcarf, determine whether everything is in order, and to get the sizes of 
C the various arrays for dynamic memory purposes.  
C   The file is left open and scrolled to the extension if an acceptable
C extension is found, otherwise the file is CLOSED
C
C Passed Parameters
C  CHATTER          i   : Chattiness flag (<5 quiet, >15 noisey)
C  CRFFIL           i   : CRF filename (optionally incl extension #)
C  INSTR            i   : Hduclas keyword value array to specify extension
C  EXTNSN           i   : Hduclas keyword value array to specify extension
C  IUNIT              o : The Unit number of the CRF file
C  TELESCOP           o : Value of the TELESCOP keyword from located extn
C  INSTRUME           o : Value of the INSTRUME keyword from located extn
C  DETNAM             o : Value of the DETNAM keyword (or 'NONE')
C  FILTER             o : Value of the FILTER keyword (or 'NONE')
C  NENERGY            o : Size of the energy grid column
C  NTHETA             o : Size of the theta grid column
C  NPHI               o : Size of the phi grid column
C  LECOL,HECOL        o : Low and High Energy column numbers
C  THCOL              o : Theta column number
C  PHCOL              o : Phi column number
C  IERR               o : Subroutine error flag (zero = OK)
C
C Called Routines
C
C Author/Modification History
C  Ian M George (1.0.0:93 Dec 18) Original
C  Lawrence E Brown (2.0.0:94 Aug) Added Column size fetching stuff

      character(7) version
      parameter (version='2.0.0')
*-
C Max arrays
      integer maxextn
      parameter (maxextn=99)
C Internals
      character(16) ttype
      character(25) tunit
      character(8) datatype,tdisp
      double precision tscale,tzero
      integer tnull
      integer status
      integer ninstr, nsearch
        integer next(maxextn)
        character(20) outhdu(9,maxextn), outver(9,maxextn)
        character(20) extnam(maxextn)
      character(30) errstr, wrnstr, comm
      character(80) message
C Initialize
      ierr = 0 
             errstr = '** CK_CRF '//version//' ERROR:'
             wrnstr = '** CK_CRF '//version//' WARNING:'

C Main
C ... give user info if requested
        if(chatter.GT.40) then
           message = ' ... using CK_CRF '// version
           call fcecho(message)
        endif

C Search
        ninstr = 2
        nsearch = maxextn
      call mvext(0,crffil, iunit, ninstr, instr, nsearch,
     &            next, outhdu, extnam, outver, 
     $        extnsn, status, chatter)
      if(status.ne.0) goto 482
678      continue


C --------- Read the necessary keywords -----------------
      
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
         message = errstr//' reading TELESCOP'
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
         message = errstr//' reading INSTRUME'
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
         message = errstr//' reading DETNAM'
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
         message = errstr//' reading FILTER'
         call fcerr(message)
         call fcerrm(status)
      ENDIF


C -------------- End of keyword reading -----------------
C --------------Get array sizes--------------------------
      call ftgcno(iunit,.false.,'ENERG_LO',lecol,status)
      call ftgbcl(iunit,lecol,ttype,tunit,datatype,nenergy,tscale,
     $     tzero,tnull,tdisp,status)
      if (status .ne. 0) then
         message = ' Error getting energ_lo column in CRF file'
         call fcerr (message)
         call fcerrm(status)
         goto 482
      endif
      call ftgcno(iunit,.false.,'ENERG_HI',hecol,status)
      call ftgbcl(iunit,hecol,ttype,tunit,datatype,nenergy,tscale,
     $     tzero,tnull,tdisp,status)
      if (status .ne. 0) then
         message = ' Error getting energ_hi column in CRF file'
         call fcerr (message)
         call fcerrm(status)
         goto 482
      endif
      call ftgcno(iunit,.false.,'THETA',thcol,status)
      call ftgbcl(iunit,thcol,ttype,tunit,datatype,ntheta,tscale,
     $     tzero,tnull,tdisp,status)
      if (status .ne. 0) then
         if(chatter.gt.10)then
            message = 'Can''t find THETA column in CRF file'
            call fcecho (message)
         endif
         status=0
      endif
      call ftgcno(iunit,.false.,'PHI',phcol,status)
      call ftgbcl(iunit,phcol,ttype,tunit,datatype,nphi,tscale,
     $     tzero,tnull,tdisp,status)
      if (status .ne. 0) then
         if(chatter.gt.10)then
            message = 'Can''t find PHI column in CRF file'
            call fcecho (message)
         endif
         status=0
      endif


C Check for errors
482     if(status.ne.0) then
                message = errstr // ' Unable to recover'
                call fcerr(message)
              call ftclos(iunit, ierr) 
              ierr=status
            return
        endif

      return
      end
C -------------------------------------------------------------------------
