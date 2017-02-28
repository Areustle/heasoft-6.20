c
c
c
      SUBROUTINE EXRDWIN(Lur,Lut,Lcd,Lul,Cfilwi,Twia,Twio,Pwi,Pwia,Pwio,
     &                   Fwia,Fwio,Ewia,Ewio,Nwi,Nwito,*)
      implicit none
c
c ls    12/4/88 to read windows from file
c Rev.1 11/9/91 to handle window files containing flag for using d/f expos.
c               windows of each program (i.e. no. of expos. winds =-1)
c Rev.2 13/9/91 to handle "brothers" in cfilwi filename (e.g. 'file c a b')
c               used to reassign intensity and exposure windows consistently
c
c     I   lur,lut,lul = lu of read, terminal, and log file
c     I   lcd = cd-xronos chatt.
c     I   cfilwi = window filename
c   for all series
c     I/R   twia,twio = time window(s) start/stop (max 100)
c     I/R   pwi,pwia,pwio = phase window(s) epoch, per., start, stop (max 10)
c   for each series separately:
c     I/R   fwia,fwio = flux window start, stop (in bin, rebin, intv) (max 10)
c     I/R   ewia,ewio = expos window start, stop (in bin, rebin, intv) (max 1)
c     I/R   nwi = no. of windows in each window type
c     I/R   nwito = total no. of windows (all types)
c
c     R   * = conditional return is for error in opening/reading window file
c
c   exinerr, xrbell  = subroutines used
c
      character(80) filename
      CHARACTER Cfilwi*(*)
c !Rev.2
      CHARACTER cfilwind*132 , cpro*1 , cque*1 , cbrot*9
c !Rev.2
      character(1) cbro(3)
      INTEGER*4 iv , Lur , Lut , Lcd , Lul , ierx , ierr , Nwi(*)
      INTEGER*4 i , j , ise , Nwito , lup , idum
c !Rev.2 brothers and temporary no. of windows
      INTEGER*4 nbro(3) , nwit(20) , ix , itemp
      REAL*4 rv , Pwia(*) , Pwio(*)
      REAL*4 Fwia(9,*) , Fwio(9,*) , Ewia(*) , Ewio(*)
c !Rev.2 Temporary windows
      REAL*4 fwiat(9,10) , fwiot(9,10) , ewiat(9) , ewiot(9)
      REAL*8 dv , Twia(*) , Twio(*) , Pwi(*)
      DATA lup/30/
c !Rev.2
      DATA cbrot/'1aA2bB3cC'/
c
      ierx = 0
      ierr = 0
c !Rev.2 start
c set temporary intensity and exposure windows to d/f values
      DO 100 iv = 1 , 20
         nwit(iv) = Nwi(iv)
 100  CONTINUE
      DO 200 iv = 1 , 9
         ewiat(iv) = Ewia(iv)
         ewiot(iv) = Ewio(iv)
         DO 150 ix = 1 , 10
            fwiat(iv,ix) = Fwia(iv,ix)
            fwiot(iv,ix) = Fwio(iv,ix)
 150     CONTINUE
 200  CONTINUE
c
c decode input filename and brothers
      CALL EXASK(cpro,cque,Lut,Lcd,Lut,Lul,0,Cfilwi,cfilwind,iv,rv,dv,1,
     &           -1,*800,*800)
      DO 300 ise = 1 , 3
         cbro(ise) = ' '
         CALL EXASK(cpro,cque,Lut,Lcd,Lut,Lul,0,Cfilwi,cbro(ise),iv,rv,
     &              dv,1,ise+1,*800,*800)
 300  CONTINUE
c       append d/f filename extension if there is not one
C      CALL EXFILEXT(cfilwind,'.wi ',1)
c      write (*,9002) cfilwind,(cbro(ise),ise=1,3)                  !!!!!!
c9002  format(' Window File :  ',a,/,' Brothers :',a,a,a)           !!!!!!
c       set nbro
      DO 400 ise = 1 , 3
c       set temporary default values
         nbro(ise) = 0
         DO 350 iv = 1 , 9
c         set nbro to new value if cbro matches cbrot
            IF ( cbro(ise).EQ.cbrot(iv:iv) ) nbro(ise) = (iv+2)/3
 350     CONTINUE
c       if no match set to default
         IF ( nbro(ise).EQ.0 ) THEN
            nbro(ise) = ise
c         if cbro is not a blank it is illegal and a warning is issued
            IF ( cbro(ise).NE.' ' ) THEN
               WRITE (*,99001) ise , cbrot(ise*3:ise*3)
               IF ( Lul.NE.0 ) WRITE (Lul,99001) ise , 
     &                                cbrot(ise*3:ise*3)
            ENDIF
         ENDIF
 400  CONTINUE
c      write(*,*)' nbro: ',(nbro(ise),ise=1,3)              !!!!!!
c open window file
c      filename=cfilwi
      filename = cfilwind
c !Rev.2 stop
      CALL OPENWR(lup,filename,'old',' ',' ',0,1,ierr)
      IF ( ierr.EQ.0 ) THEN
c read header line
         READ (lup,*,IOSTAT=ierr,ERR=800) Nwito
cc message
c      if(lut.ne.0)write(lut,1000) nwito, cfilwi
c      if(lul.ne.0) write(lul,1000)  nwito, cfilwi
c1000  format(' ',i4,' windows in file ',a)
c read time windows
         READ (lup,*,IOSTAT=ierr,ERR=800) Nwi(1)
         IF ( Nwi(1).GT.0 ) THEN
            DO 420 i = 1 , Nwi(1)
               READ (lup,*,IOSTAT=ierr,ERR=800) Twia(i) , Twio(i) , idum
 420        CONTINUE
         ENDIF
c read phase windows
         READ (lup,*,IOSTAT=ierr,ERR=800) Nwi(2)
         IF ( Nwi(2).GT.0 ) THEN
            READ (lup,*,IOSTAT=ierr,ERR=800) Pwi(1) , Pwi(2)
            DO 440 i = 1 , Nwi(2)
               READ (lup,*,IOSTAT=ierr,ERR=800) Pwia(i) , Pwio(i) , idum
 440        CONTINUE
         ENDIF
c read intensity windows
c loop for series
         DO 500 ise = 1 , 3
c loop for original bin, new bin, interval
            DO 460 j = 1 , 3
               iv = 2 + 3*(ise-1) + j
c !Rev.2 start: read in temporary no. of windows
c            READ (lup, *, IOSTAT=ierr, ERR=99) nwi(iv)
               READ (lup,*,IOSTAT=ierr,ERR=800) nwit(iv)
c            IF (nwi(iv).GT.0) THEN
               IF ( nwit(iv).GT.0 ) THEN
c               DO i = 1, nwi(iv)
                  DO 445 i = 1 , nwit(iv)
c                  READ (lup, *, IOSTAT=ierr, ERR=99) fwia(iv-2, i),
c     &                  fwio(iv-2, i), idum
                     READ (lup,*,IOSTAT=ierr,ERR=800) fwiat(iv-2,i) , 
     &                     fwiot(iv-2,i) , idum
c !Rev.2 stop
 445              CONTINUE
               ENDIF
 460        CONTINUE
 500     CONTINUE
c !Rev.2 start :              reassign ints windows according to nbro
c                             loop for series
         DO 550 ise = 1 , 3
c                             loop for original bin, new bin, interval
            DO 520 j = 1 , 3
               ix = 2 + 3*(nbro(ise)-1) + j
               iv = 2 + 3*(ise-1) + j
               Nwi(iv) = nwit(ix)
               IF ( Nwi(iv).GT.0 ) THEN
                  DO 505 i = 1 , Nwi(iv)
                     Fwia(iv-2,i) = fwiat(ix-2,i)
                     Fwio(iv-2,i) = fwiot(ix-2,i)
 505              CONTINUE
               ENDIF
 520        CONTINUE
 550     CONTINUE
c !Rev.2 stop
c
c  read exposure windows
c loop for series
         DO 600 ise = 1 , 3
c loop for original bin, new bin, interval
            DO 560 j = 1 , 3
               iv = 11 + 3*(ise-1) + j
c Rev.1 start
c            READ (lup, *, IOSTAT=ierr, ERR=99) nwi(iv)
c               read no. of windows in file in a temp. variable
               READ (lup,*,IOSTAT=ierr,ERR=800) itemp
c               if no d/f windows are specified then overwrite window
c               number and read in new windows (if ge.0)
               IF ( itemp.GE.0 ) THEN
c              nwi(iv)=itemp   !Rev.2
c !Rev.2 : store in temp. window no.
                  nwit(iv) = itemp
c              write(*,*)' nwit(iv),itemp', nwit(iv),itemp      !!!!!
c Rev.1 stop
c !Rev.2 start : use temp windows
c              IF (nwi(iv).GT.0) THEN
                  IF ( nwit(iv).GT.0 ) THEN
c                 DO i = 1, nwi(iv)
                     DO 552 i = 1 , nwit(iv)
c                    READ (lup, *, IOSTAT=ierr, ERR=99) ewia(iv-11),
c     &                    ewio(iv-11), idum
                        READ (lup,*,IOSTAT=ierr,ERR=800) ewiat(iv-11) , 
     &                        ewiot(iv-11) , idum
c !Rev.2 stop
 552                 CONTINUE
                  ENDIF
c Rev.1
               ENDIF
 560        CONTINUE
 600     CONTINUE
c !Rev.2 start :              reassign exps windows according to nbro
c                             loop for series
         DO 650 ise = 1 , 3
c                             loop for original bin, new bin, interval
            DO 620 j = 1 , 3
               ix = 11 + 3*(nbro(ise)-1) + j
               iv = 11 + 3*(ise-1) + j
               Nwi(iv) = nwit(ix)
               IF ( Nwi(iv).GT.0 ) THEN
                  Ewia(iv-11) = ewiat(ix-11)
                  Ewio(iv-11) = ewiot(ix-11)
               ENDIF
 620        CONTINUE
 650     CONTINUE
c reset nwito to new total value
         Nwito = 0
         DO 700 iv = 1 , 20
            Nwito = Nwito + Nwi(iv)
 700     CONTINUE
c !Rev.2 stop
c close window file
         CLOSE (lup)
         RETURN
      ENDIF
c open/write error
 800  ierx = 1008
      CALL XERROR(' Open/reading file',5)
*      CALL EXINERR(Lur,Lut,Lcd,Lul,ierx,ierr,*900)
      RETURN 1
99001 FORMAT (/,' **** Warning : Illegal assignement for Ints/Expos',
     &        ' Windows of Series',I2,/,'                Series ',A,
     &        ' Ints/Expos Windows from',' file will be used!')
      END
