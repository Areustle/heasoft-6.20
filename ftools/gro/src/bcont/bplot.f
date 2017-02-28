
      subroutine bplot(nptf,xf,yf,zf,det,ndet)
      implicit none
      INTEGER N,ndet,det(8), status
      PARAMETER (N=8)
      REAL X1(N), X2(N)
      character(1) BSL,si,st0*16
      DATA X1 /   2*0.0, -8000.0, 100.3, 205.3, -45000.0, 2*0.0/
      DATA X2 /2*8000.0,  8000.0, 101.3, 201.1, 3*-100000.0/
     
      integer i,j,nptf
      double precision xf(nptf),yf(nptf,8),zf(nptf,8)
      INTEGER PGOPEN, ID, ID1, ID2, NP, devflg
      character(20)      device*3,desc*100
      real     xt(3000),yt(3000),zt(3000)
      real     xmin, xmax, ymin, ymax, yhi, ylo
     
c note subscript order increases this way:
c   1,1 | 2,1 | 3,1 | 4,1 | 5,1 | 1,2 | 2,2 | ...

C Call PGOPEN to initiate PGPLOT and open the output device; 
      
c      ID1 = PGOPEN('?Graphics device for menu (eg, /XWIN): ') 
       ID1 = PGOPEN('/XWIN') 
       IF (ID1.LE.0) STOP
      
       CALL BPLOT_INIT
       CALL PGASK(.FALSE.)
     
c      status = 0
c      call uclgst('device',device,status)
c      IF (status.NE.0) THEN
c        desc = 'problem getting device parameter !'
c        call fcecho(desc)
c        return
c      ENDIF
      
c      call crmvlbk(device)

      device = '/xwin'
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
 
      NP = 1
 100  CALL PGSLCT(ID1)
      CALL BPLOT_MENU(NP, ID)
      CALL PGSLCT(ID2)
      CALL PGSAVE
      CALL PGBBUF
      IF (ID.EQ.1) THEN
      BSL = CHAR(92)
      CALL PGPAGE
      CALL PGSAVE
      CALL PGBBUF
      CALL PGSCH(0.7)

      do j=1,nptf
         xt(j)= xf(j) - xf(1) 
      enddo

      call getMaxMin(yf,nptf,8, ymax, ymin)
     
      xmin = xt(1)
      xmax = xt(nptf)
   
      write(st0,1) xf(1)
 1    format(f14.6)

       DO 101 I=1,N
       do j=1,nptf
         zt(j)= zf(8*(j-1)+I,1)
         yt(j)= yf(8*(j-1)+I,1)
       enddo

        write(si,2) I-1
 2      format(i1) 
     
        CALL PGSVP(0.10, 0.95, (0.7+REAL(N-I))/REAL(N)-0.07,
     :                         (0.7+REAL(N-I+1))/REAL(N)-0.11)
        CALL PGSWIN(0.0,1.0,0.0,1.0)    
        CALL PGSCH(0.7)                         
        CALL PGTEXT(-0.1,0.0, BSL//'fiDet'//si)
        CALL PGSCH(0.5)
        CALL PGSWIN(xmin,xmax,ymin/1.01,ymax*1.005)    
        CALL PGTBOX('ABCTS',0, 0, 'ABCTSV', 0.0, 0)
        
C Draw labels
C
      CALL PGSCI (1)
      CALL PGBOX ('N',0, 0, 'VN', 0.0, 0)
        IF (I.EQ.1) THEN
           CALL PGSCH(1.0)
           CALL PGMTXT('T', 0.4, 0.5, 0.5,
c     &  BSL//'fiFlux vs. '//char(7)//' TJD')
     &  BSL//'fiFlux vs. (TJD- '//st0//')')
           CALL PGSCH(0.8)
         END IF 
 
c     plot flux vs tjd
           CALL PGLINE(nptf, xt, yt)
           CALL PGPT(nptf, xt, yt, 18) 

c      plot error bar
       do j=1,nptf
          yhi = yt(j)+2.0*zt(j)
          ylo = yt(j)-2.0*zt(j)
c          call pgpoint(1,xt(j),yt(j),17)
c          call pgerry(1,xt(j), ylo, yhi, 1.0)
        enddo  

 101  CONTINUE      
      call getcurs(xmin,xmax,ymin,ymax)
      call bgetdet(ndet,det,xmin,xmax,ymin,ymax)
      CALL PGEBUF
      CALL PGUNSA
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

      SUBROUTINE BPLOT_INIT
C
C Set up graphics device to display menu.
C-----------------------------------------------------------------------
      CALL PGPAP(2.5, 2.0)
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGSWIN(0.0,0.5,0.0,1.0)
      CALL PGSCR(0, 0.4, 0.4, 0.4)
      CALL PGSLS (1)
      RETURN
C-----------------------------------------------------------------------
      END

      SUBROUTINE BPLOT_MENU(NP, ID)
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
      DATA VALUE /'Cts vs. Time','Exit'/

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

      return
      end
