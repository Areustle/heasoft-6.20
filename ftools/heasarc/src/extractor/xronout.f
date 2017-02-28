C write a xronos window file
      SUBROUTINE XRONOUT(Gti,Imaxgti,Maxgti)
 
      IMPLICIT NONE

      INTEGER Imaxgti , Maxgti
      DOUBLE PRECISION Gti(3,Maxgti)

      INCLUDE 'extractor.inc'
 
      INTEGER lus , ierr , i , ise , j
      CHARACTER(512) errormsg
      LOGICAL qexist

      INTEGER lenact
      EXTERNAL lenact

      lus = 0
      ierr = 0
      i = 0
      ise = 0
      j = 0
      errormsg = ' '
 
      CALL GETLUN(lus)
 
 
      ierr = 0
c open window file
      INQUIRE(file=Xronoutfile(:lenact(Xronoutfile)),exist=qexist)
      IF ( qexist .AND. clobber ) THEN
         CALL delfil(Xronoutfile(:lenact(Xronoutfile)))
      ENDIF

      CALL OPENWR(lus,Xronoutfile,'UNKNOWN',' ',' ',0,0,ierr)
      IF ( ierr.NE.0 ) THEN
         CALL fcerr(' Failed to open')
         CALL fcerr(Xronoutfile)
         GOTO 400
      ENDIF
c header line
      WRITE (lus,'(I6,A)',IOSTAT=ierr,ERR=400) Imaxgti, 
     &         'Windows in this < Xronos Window File > '
c write time windows
      WRITE (lus,'(I6,A,T70,A,I6)',IOSTAT=ierr,ERR=400) Imaxgti,
     &         ' Time Wind.: start       stop  (days)','max ', Maxgti
      IF ( Imaxgti.GT.0 ) THEN
         DO 50 i = 1 , Imaxgti
            WRITE (lus,*,IOSTAT=ierr,ERR=400) '                    ' , 
     &             Gti(1,i)/86400.D0 , Gti(2,i)/86400.D0 , i
 50      CONTINUE
      ENDIF
c write phase windows
      WRITE (lus,'(I6,A,A,T70,A,I6)',IOSTAT=ierr,ERR=400) 0,
     &        ' Phase Wind.: epoch  period  (days)',
     &        '/ start stop (0->1) phases','max ', 0
c write intensity windows
c loop for series
      DO 200 ise = 1 , 3
c loop for original bin, new bin, interval
         DO 100 j = 1 , 3
            WRITE (lus,'(I6,A,A,A,I2,A,T70,A,I6)',IOSTAT=ierr,ERR=400)
     &          0, ' Ints. Wind. for ', '0', ' in Series', 0, 
     &          ' : min  max (c/s)', 'max ', 0
 100     CONTINUE
 200  CONTINUE
c  write exposure windows
c loop for series
      DO 300 ise = 1 , 3
c loop for original bin, new bin, interval
         DO 250 j = 1 , 3
            WRITE (lus,'(I6,A,A,A,I2,A,T70,A,I6)',IOSTAT=ierr,ERR=400)
     &          0, ' Exps. Wind. for ', '0', ' in Series', 0,
     &          ' : min  max (0->50)', 'max ', 0
 250     CONTINUE
 300  CONTINUE
c close window file
      CLOSE (lus)
      RETURN

 400  errormsg = ' Unable to write output XRONOS window file: '//
     &           Xronoutfile(:lenact(Xronoutfile))
      CALL fcerr(errormsg)

      RETURN
      END

