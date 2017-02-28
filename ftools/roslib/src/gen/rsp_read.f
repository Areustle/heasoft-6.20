C******************************************************************************
C SUBROUTINE:
C      		rsp_read
C
C DESCRIPTION:
C      read a response matrix
C
c --- CALLED ROUTINES -------------------------------------------------
c
c RDRMF3   (CALLIB) : Reads OGIP format Response matrix extension
c
C
C******************************************************************************

      subroutine rsp_read(iunit2,maxen,maxchan, fmatrix
     &                   ,chatter, status)

c
      IMPLICIT NONE
      integer iunit2,chatter
      integer maxen,maxchan
c
c
      character(5) version
      parameter (version = '1.0.1')

c ---------------------------------------------------------------------
c
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
c 
c ---------------------------------------------------------------------
c
c
c ... pointers to "arrays" to be dynamically allocated
      integer p_ngrp,  p_F_chan, p_N_chan
      integer p_energ_lo, p_energ_hi

c ... "arrays" to be dynamically allocated
c     integer ngrp(maxen)             real fmatrix(maxchan,maxen)
c     integer F_chan(maxen,maxgrp)    integer N_chan(maxen,maxgrp
c     real energ_lo(maxen)            real energ_hi(maxen)

      character(30) errstr, chantype
      character(70) message
      character(16) rmftlscop, rmfinstrum, rmffilt,rmfdet
      character(8) matext
      character(5) rsp_rmfversn
      character(20) hduclas3
      real lo_thresh, rmfarea
      integer rmfchan,ienerg
      integer imaxgrp,status,maxgrp,flchan
      parameter (maxgrp = 3)
      real fmatrix(maxchan,maxen)

C Initialize
       chantype = 'UNKNOWN'


c --- DMA ---

c Allocate dynamic memory
        p_ngrp = 0
        p_F_chan = 0
        p_N_chan = 0
        p_energ_lo = 0
        p_energ_hi = 0
        status = 0
        call udmget(maxen, 4, p_ngrp, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
c       call udmget(maxen*maxchan, 6, p_fmatrix, status)
c       IF (status.NE.0) THEN
c         goto 50
c       ENDIF
        call udmget(maxen*maxgrp, 4, p_F_chan, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(maxen*maxgrp, 4, p_N_chan, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(maxen, 6, p_energ_lo, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
        call udmget(maxen, 6, p_energ_hi, status)
        IF (status.NE.0) THEN
          goto 50
        ENDIF
 50     IF (status.NE.0) then
              message = errstr// ' Failed to allocate Dynamic Memory'
              call fcecho(message)
              status = -1
              goto 100 
        ENDIF 
c
c --- READ RESPONSE MATRIX ---
c

      call rdrmf3(iunit2,chatter,matext,rmftlscop,rmfinstrum,
     &            rmfdet,rmffilt,rmfarea, chantype, flchan,
     &            rmfchan,ienerg,
     &            MEMR(p_energ_lo),MEMR(p_energ_hi),imaxgrp,
     &            MEMI(p_ngrp),MEMI(p_F_chan),
     &            MEMI(p_N_chan), fmatrix,lo_thresh,maxchan,
     &            maxen,rsp_rmfversn,hduclas3,status)

      
c *****
c Free the dynamic Memory
  100   call udmfre(p_ngrp,4,status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
c       call udmfre(p_fmatrix,6,status)
c       IF (status.NE.0) THEN
c         goto 485
c       ENDIF
        call udmfre(p_F_chan, 4, status)
        IF (status.NE.0) THEN
          goto 485
        ENDIF
        call udmfre(p_N_chan, 4, status)
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
 485    IF (status.NE.0) then
                message = errstr//
     &          ' Failed to deallocate Dynamic Memory'
                call fcecho(message)
                status= 99
        ENDIF
        return
        end
c -------------------------------------------------------------------
c     END OF RMF_IMG 
c -------------------------------------------------------------------  
