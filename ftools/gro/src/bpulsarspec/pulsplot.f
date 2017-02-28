      SUBROUTINE pulsplot(nptf,xf,yf,zf,nptp,yp,zp,phsta,phstp,decnum,
     &  chani, chanj, chanm, chann, tjdsta, tjdstp,yph,zph,
     &  chansta,chanstp, nphs)
        INTEGER PGOPEN, ID, ID1, ID2, NP, devflg,i, nptf
        integer         phsta, phstp, nptp, decnum, nphs
        integer         chani, chanj, chanm, chann
        double precision  xf(5000),yf(5000),yp(64),zf(5000),zp(64)
        double precision  yph(64),zph(64)
        double precision  tjdsta, tjdstp
        real              xp(64)
        character(20)      device*3,desc*100
        integer  status
C
C Call PGOPEN to initiate PGPLOT and open the output device; 
      
c      ID1 = PGOPEN('?Graphics device for menu (eg, /XWIN): ') 
      ID1 = PGOPEN('/XWIN') 
      IF (ID1.LE.0) STOP

      CALL PULSPLOT_INIT
      CALL PGASK(.FALSE.)
     
      status = 0
      call uclgst('device',device,status)
      IF (status.NE.0) THEN
        desc = 'problem getting device parameter !'
        call fcecho(desc)
        errflg = 1
        return
      ENDIF
      
      call crmvlbk(device)

      if (device .eq. ' ') then
        device = '/xwin'
      endif

      devflg = 1 
      if ((device(1:3) .eq. '/XW') .or. (device(1:3) .eq. '/xw')) then
         devflg = 0
      endif

      ID2 = PGOPEN(device)
      IF (ID2.LE.0) THEN
         ID2 = PGOPEN('/XWIN')  
      ENDIF  
      IF (ID2.LE.0) STOP
      CALL PGASK(.FALSE.)

c     get phase
      do i=1, phstp-phsta+1,1
         xp(i) = (i-1+phsta)*1.0/64.0+0.5*1.0/64.
      enddo
C
C Select a plot.
C 
 
      NP = 1
 100  CALL PGSLCT(ID1)
      CALL PULSPLOT_MENU(NP, ID)
      CALL PGSLCT(ID2)
      CALL PGSAVE
      CALL PGBBUF
      IF (ID.EQ.1) THEN
         CALL bp_onedplot(nptf,xf,yf,zf,phsta, phstp, decnum,
     & chani, chanj, chanm, chann, tjdsta, tjdstp) 
      ELSE IF (ID.EQ.2) THEN
         tjd1 = xf(1)
         tjd2 = xf(nptf)
         CALL hrphasplot(nptp,xp,yp,zp,phsta, phstp,tjd1,tjd2, decnum,
     & chani, chanj, chanm, chann,tjdsta, tjdstp) 
       ELSE IF (ID.EQ.3) THEN
         tjd1 = xf(1)
         tjd2 = xf(nptf)
         CALL fluxplot(64,yph,zph,phsta, phstp,tjd1,tjd2, decnum,
     & chansta, chanstp,tjdsta, tjdstp, nphs)  
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

      SUBROUTINE PULSPLOT_INIT
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

      SUBROUTINE PULSPLOT_MENU(NP, ID)
      INTEGER NP, ID
C
C Display menu of plots.
C-----------------------------------------------------------------------
      INTEGER NBOX
      PARAMETER (NBOX=4)
      character(12) VALUE(NBOX)
      INTEGER I, JUNK
      REAL X1, X2, Y(NBOX), XX, YY, R
      CHARACTER CH
      INTEGER PGCURS
C
      DATA VALUE /'HR vs. TDJ','HR vs. PH','Flux vs. PH','Exit'/

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
      K = 14
      IF (NP.EQ.4) K = 15
      CALL PGSCI(2)
      CALL PGSFS(1)
c     CALL PGCIRC(X1, Y(1), 0.02)
      CALL PGUNSA
      CALL PGEBUF
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


