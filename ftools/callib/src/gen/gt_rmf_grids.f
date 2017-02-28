C -------------------------------------------------------------------------
*+ GT_RMF_GRIDS
      subroutine GT_RMF_GRIDS(chatter,iunit,ienerg,en_lo,en_hi,ierr)

      IMPLICIT NONE
      integer chatter, ierr, iunit, ienerg
      real en_lo(ienerg), en_hi(ienerg)

C Description
C  This routine simply reads & returns the ENERG_LO & ENERG_HI columns 
C of the RSP_MATRIX.
C  !!! NOTE !!! The FITS RMF file is assumed to be open and scrolled to 
C                 apprproiate extension.
C               File will be closed on successful execution
C
C Passed Parameters
C  CHATTER      i    : How much info to report
C  IUNIT        i    : Where's the file?
C  IENERG       i    : How many values in the energy columns
C  EN_LO,EN_HI     o : The energy grid values
C  IERR            o : Error Flag (zero if all OK)
C Called Routines
C
C Author/Modification History
C  Ian M George (1.0.0:94 Jan 24) Original
C  Ian M George (2.0.0:94 Apr 29), added MVEXT calls
C  Lawrence E Brown  (2.0.1:94 Aug), Renamed routine
      character(7) version
      parameter (version='2.0.1')

*-
C Internals
      character(30) errstr, wrnstr
      character(80) message
      integer enull, felem, colnum, frow, status
      logical anyflg



C Initialize
      ierr = 0 
             errstr = '** GT_RMF_GRIDS '//version//' ERROR:'
             wrnstr = '** GT_RMF_GRIDS '//version//' WARNING:'

C Start 

C ... give user info if requested
        if(chatter.GT.40) then
           message = ' ... using GT_RMF_GRIDS '// version
           call fcecho(message)
        endif

C READ DATA

C     ENERG_LO ...

      frow = 1
      felem = 1
      status = 0
      call ftcmsg()
      call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
      If (status.NE.0) THEN
         message = errstr//' ENERG_LO column not present !'
         call fcecho(message)
         ierr = 4
      goto 483
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,en_lo,
     &            anyflg,status)
      IF (status.NE.0) THEN
        message = errstr//' reading ENERG_LO column'
        call fcecho(message)
        ierr = 1
      goto 483
      ENDIF

C     HI_ENERG ...

      status = 0
      call ftcmsg()
      call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
      If (status.NE.0) THEN
         message = errstr//' ENERG_HI column not present !'
         call fcecho(message)
         ierr = 4
      goto 483
      ENDIF
      enull = 0
      call ftgcve(iunit,colnum,frow,felem,ienerg,enull,en_hi,
     &            anyflg,status)
      IF (status.NE.0) THEN
        message = errstr//' reading ENERG_HI column'
        call fcecho(message)
        ierr = 1
      goto 483
      ENDIF


C Check for errors
483     if(ierr.ne.0) then
                message = errstr // ' Unable to recover'
                call fcecho(message)
            return
        endif
C Close the FITS file
        call ftclos(iunit, status) 

      return
      end
