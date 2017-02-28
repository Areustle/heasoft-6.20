C***************************************************************
C
C subroutine:  xrrdwin
C              read window file
C
C written by:
C      Emily A. Greene
C      HEASARC/GSFC/NASA  Hughes STX
C      3/6/95
C
C modification history:
C
C notes:
C      see xronos.inc for the meaning and current size of all parameter
C      arrays
C
C      this routine is to replace xrrdwin.f calls
c
C      previously the windows of one series could be applied to a different
C      one using a "brother" window.  This has been removed.  The user
C      can accomplish the same thing by editting the input window file
C
C calling sequence:
C      call xrrdwin(nser,cfilwi, twia, twio, pwi,
C                        pwia, pwio, fwia, fwio, ewia, ewio, nwi, nwito,
C                        status)
C
C variables:
C
C      nser - integer - input - number of series
C      cfilwi - character - input - window file name
C      twia - double array - output - time window start times
C      twio - double array - output - time window stop times
C      pwi - double array - output - phase window epoch and period
C      pwia - real array - output - phase window start
C      pwio - real array - output - phase window stop
C      fwia - real array - output - flux window start
C      fwio - real array - output - flux window stop
C      ewia - real array - output - exposure window start
C      ewio - real array - output - exposure window stop
C      nwi - integer array - output - number of each type of windows
C      nwito - integer - output - total number of all windows = iflags(5)
C      status - integer - output - status of operation
C
C***************************************************************
c
      SUBROUTINE xrrdwin(nser, cfilwi, twia, twio, pwi,
     &     pwia, pwio, fwia, fwio, ewia, ewio, nwi, nwito,
     &     status)
c
      implicit none
      include '../include/io.inc'
      include '../include/xronos.inc'
C
      integer lup, nwito, iv, ix, i, j, ise, idum, ie
      character(160) cfilwi, filename
      include '../include/xronos_init.inc'
      parameter (subname = 'xrrdwin:')

      if (status .ne. 0) return

c set temporary intensity and exposure windows to d/f values
      DO iv=1,maxwintype
         nwi(iv)=0
      ENDDO
      DO iv=1,maxseries*maxbintype
         ewia(iv)=0.
         ewio(iv)=0.
         DO ix=1,maxfluxwin
            fwia(iv,ix)=0.
            fwio(iv,ix)=0.
         ENDDO
      ENDDO
      do iv=1,maxtimewin
         twia(iv)=0.
         twio(iv)=0.
      enddo
      do iv=1,maxphwin
         pwia(iv)=0.
         pwio(iv)=0.
      enddo
      pwi(1)=0.
      pwi(2)=0.
c
c       append d/f filename extension if there is not one
      filename = cfilwi
      CALL xrfilext(filename, '.wi ', 1)


      call getlun(lup)
      CALL openwr(lup, filename, 'old', ' ', ' ', 0, 1, status)
      IF (status.NE.0) THEN
         GOTO 99
      ENDIF

c read header line
      READ (lup, *, IOSTAT=status, ERR=99, END=99) nwito

c read time windows
      READ (lup, *, IOSTAT=status, ERR=99, END=99) nwi(1)
      IF (nwi(1).GT.0) THEN
         DO i = 1, nwi(1)
            READ (lup, *, IOSTAT=status, ERR=99, END=99) 
     $           twia(i), twio(i), idum
         ENDDO
      ENDIF
c read phase windows
 2    CONTINUE
      READ (lup, *, IOSTAT=status, ERR=99, END=99) nwi(2)
      IF (nwi(2).GT.0) THEN
         READ (lup, *, IOSTAT=status, ERR=99, END=99) pwi(1), pwi(2)
         DO i = 1, nwi(2)
            READ (lup, *, IOSTAT=status, ERR=99, END=99) 
     $           pwia(i), pwio(i), idum
         ENDDO
      ENDIF
 3    CONTINUE
c loop for series
      DO ise = 1, nser
c read intensity,  and exposure windows
c loop for original bin, new bin, interval
c read intensity windows
         DO j = 1, 3
            iv = 2 + 3*(ise-1) + j  
            READ (lup, *, IOSTAT=status, ERR=99, END=99) nwi(iv)  
            IF (nwi(iv).GT.0) THEN
               DO i = 1, nwi(iv)
                  READ (lup, *, IOSTAT=status, ERR=99, END=99) 
     $                 fwia(iv-2, i),  fwio(iv-2, i), idum 
               ENDDO
            ENDIF
         ENDDO
c  read exposure windows
 4       CONTINUE
c     loop for original bin, new bin, interval
         DO j = 1, 3
            ie = 14 + 3*(ise-1) + j    
            READ (lup, *, IOSTAT=status, ERR=99, END=99) nwi(ie)
            IF (nwi(ie).GT.0) THEN
               DO i = 1, nwi(ie)
                  READ (lup, *, IOSTAT=status, ERR=99, END=99)
     $                 ewia(ie-14), ewio(ie-14), idum
               ENDDO
            ENDIF
         ENDDO
      enddo
c     reset nwito to new total value 
      nwito=0
      DO iv=1,20
         nwito=nwito+nwi(iv)
      ENDDO
c     close window file
 5    CONTINUE
      CLOSE (lup)
      call frelun(lup)
      RETURN
c     open/write error
 99   continue
      errm = subname//' '//'Error opening/reading window file'
      call xaerror (errm, 5)
      close (lup)
      call frelun(lup)
 9    RETURN
      END
c
c
