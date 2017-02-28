       subroutine get_bs_chan(ch1,ch2,ch3,ch4,y,npt,npar,status,msg)
         integer PGOPEN, ID, ID1, ID2, NP,i,npt,npar
         integer status
         real    x(5000),y(5000)
         character*(*) msg
c         character device*10
         status = 0 
C
C Call PGOPEN to initiate PGPLOT and open the output device;

C

      ID1 = PGOPEN('/XWIN')  
      IF (ID1.LE.0) STOP
      CALL INIT
      CALL PGASK(.FALSE.)

c      status = 0
c      call uclgst('device',device,status)
c      IF (status.NE.0) THEN
c         desc = 'Error getting device parameter !'
c        call fcecho(desc)
c        errflg = 1
c        return
c      ENDIF
c      call crmvlbk(device)
      
      ID2 = PGOPEN('/XWIN')
      
      IF (ID2.LE.0) THEN
        ID2 = PGOPEN('/XWIN')  
      ENDIF
      IF (ID2.LE.0) STOP
      CALL PGASK(.FALSE.)
C
C Select a plot.
C
      NP = 1
 100  CALL PGSLCT(ID1)
      CALL MENU(NP, ID)
      CALL PGSLCT(ID2)
      CALL PGSAVE
      CALL PGBBUF
c     get channel number
      do i =1,npt   
         x(i) = i
      enddo

      IF (ID.EQ.1) THEN
         CALL curvePlot(ch1,ch2,ch3,ch4,npt,x,y,npar,msg)
      ELSE
         GOTO 200
      END IF
      CALL PGEBUF
      CALL PGUNSA
      GOTO 100
C
C Done: close devices.
C
 200  CALL PGEND      
C-----------------------------------------------------------------------
      END

      SUBROUTINE INIT
C
C Set up graphics device to display menu.
C-----------------------------------------------------------------------
      CALL PGPAP(2.5, 2.0)
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGSWIN(0.0,0.5,0.0,1.0)
      CALL PGSCR(0, 0.4, 0.4, 0.4)
      RETURN
C-----------------------------------------------------------------------
      END

      SUBROUTINE MENU(NP, ID)
      INTEGER NP, ID
C
C Display menu of plots.
C-----------------------------------------------------------------------
      INTEGER NBOX
      PARAMETER (NBOX=2)
      character(12) VALUE(NBOX)
      INTEGER I, JUNK
      REAL X1, X2, Y(NBOX), XX, YY, R
      CHARACTER CH
      INTEGER PGCURS
C
      DATA VALUE / 'Plot', 'Exit' /

      DATA XX/0.5/, YY/0.5/
C
      X1 = 0.1
      X2 = 0.2
      DO 5 I=1,NBOX
         Y(I) = 1.0 - REAL(I+1)/REAL(NBOX+2)
 5    CONTINUE
C
C Display buttons.
C
      CALL PGBBUF
      CALL PGSAVE
      CALL PGERAS
      CALL PGSCI(1)
      CALL PGSCH(2.5)
      CALL PGPTXT(X1, 1.0-1.0/REAL(NBOX+2), 0.0, 0.0, 'MENU')
      CALL PGSLW(1)
      CALL PGSCH(2.0)
      DO 10 I=1,NBOX
         CALL PGSCI(1)
         CALL PGSFS(1)
         CALL PGCIRC(X1, Y(I), 0.02)
         CALL PGSCI(2)
         CALL PGSFS(2)
         CALL PGCIRC(X1, Y(I), 0.02)
         CALL PGSCI(1)
         CALL PGPTXT(X2, Y(I), 0.0, 0.0, VALUE(I))
 10   CONTINUE
c      K = 14
c      IF (NP.EQ.4) K = 15
c      CALL PGSCI(2)
c      CALL PGSFS(1)
c      CALL PGCIRC(X1, Y(k), 0.02)
c      CALL PGUNSA
c      CALL PGEBUF
C
C Cursor input.
C
 20   JUNK = PGCURS(XX, YY, CH)
      IF (ICHAR(CH).EQ.0) GOTO 50
C
C Find which box and highlight it
C
      DO 30 I=1,NBOX
         R = (XX-X1)**2 +(YY-Y(I))**2
         IF (R.LT.(0.03**2)) THEN
            ID = I
            CALL PGSAVE
            CALL PGSCI(2)
            CALL PGSFS(1)
            CALL PGCIRC(X1, Y(I), 0.02)
            CALL PGUNSA
            RETURN
         END IF
 30   CONTINUE
      GOTO 20
 50   ID = 0
      RETURN
      END
