      subroutine rd_evt_xim (Lun,Type,Map,Szx,Szy,Work,Wszx,Wszy,
     &                       Twork,Tszx,Tszy,Usrzm,
     &                       Xi,Yi,Zi,Ei,Ti,Emin,Emax,
     &                       Gtinum,Gtistart,Gtistop,Cenpix,Numreg,
     &                       Expr,Exposure,Ximscale,Ximzero,Unit,
     &                       Datamin,Datamax,Status)
      implicit none

      INCLUDE '../include/dynmem.inc'
      INCLUDE '../include/startup.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
c
c  Reads event list into image map
c
c  I  Lun        (c)  Logical unit of open event file
c  I  Type       (c)  Type of image
c  O  Map        (r)  Image map
c  I  Szx        (i)  Image size in X
c  I  Szy        (i)  Image size in Y
c  I  Work       (r)  Work map
c  I  Wszx       (i)  Work map size in X
c  I  Wszy       (i)  Work map size in Y
c  I  Twork      (r)  Time image work map
c  I  Tszx       (i)  Time image work map size in x
c  I  Tszy       (i)  Time image work map size in y
c  I  Usrzm(2)   (d)  Rebin factor/zoom
c  I  Xi         (c)  Image X column
c  I  Yi         (c)  Image Y column
c  I  Zi         (c)  Z column (Values in Zcol pile onto pixels)
c  I  Ei         (c)  Energy column (e.g. PHA or PI)
c  I  Ti         (c)  Time column
c  I  Emin       (i)  Minimum energy channel
c  I  Emax       (i)  Maximum energy channel
c  I  Gtinum     (i)  Number of values in GTI arrays
c  I  Gtistart   (d)  GTI start times
c  I  Gtistop    (d)  GTI stop times
c  I  Cenpix     (d)  Center of image in pixel coords
c  I  Numreg     (i)  Number of user-entered regions to filter events on
c  I  Expr       (c)  Boolean expression to filter columns on
c  I  Exposure   (d)  Exposure time
c  I  Ximscale   (d)  Scaling factor
c  I  Ximzero    (d)  Offset
c  I  Unit       (d)  Unit of values in image
c  O  Datamin    (d)  Minimum value in image
c  O  Datamax    (d)  Maximum value in image
c  O  Status     (i)  Error flag (0=OK)
c
      integer*4 Lun
      character*(*) Type
      integer*4 Szx, Szy, Wszx, Wszy, Tszx, Tszy
      real*4 Map(Szx,Szy), Work(Wszx,Wszy), Twork(Tszx,Tszy)
      integer*4 Xi, Yi, Zi, Ei, Ti, Emin, Emax
      integer*4 Gtinum
      real*8 Gtistart(Gtinum), Gtistop(Gtinum)
      real*8 Usrzm(2), Cenpix(2)
      character*(*) Expr, Unit
      real*8 Exposure, Ximscale, Ximzero, Datamin, Datamax
      integer*4 Numreg, Status
c
c  Local variables
c
c    Image construction
c
      integer*4 i, j, ii, jj
      INTEGER*4 MBUF
      PARAMETER (MBUF=500)
      REAL*8 x(MBUF), y(MBUF)
      INTEGER*4 z(MBUF)
      REAL*8 time(MBUF)
      INTEGER*4 engy(MBUF)
      INTEGER*4 nrows, ntexpr, rejexp
      INTEGER*4 nloop
      INTEGER*4 nget, ngot
      INTEGER*4 accepted, rejected
      INTEGER*4 rejout, rejeng, rejgti, rejreg, rejnull
      LOGICAL good
      real*8 xreal,yreal
      real*4 xoff , yoff
      INTEGER*4 xtmp, ytmp
      INTEGER*4 xind, yind
      LOGICAL anyf, flagvals(MBUF)
      LOGICAL xflagvals(MBUF), yflagvals(MBUF)
      integer ireg
      logical first_time, isinreg
      REAL*8 total_image , dist, dd
      REAL*8 image_distr
      REAL*8 previous_time
      real*4 temp  

c    Datamin/max
      real*4 largest, smallest

c    Other
      character(100) ds
      character(80) comment

      integer tchat, lchat, rejchat
      parameter ( rejchat = 40 )
      logical verbose

      if ( Status.ne.0 ) return
c
c  Chat values for diagnosing rejected events
c
      call xgtcht(tchat, lchat)
      verbose = MAX(tchat, lchat).ge.rejchat

      CALL XWRITE(' Reading an events file ',10)


c
c  Offset in detector coordinates

      xoff = Cenpix(1)
      yoff = Cenpix(2)
c
c initialize arrays
c
      DO i = 1 , Szx
         DO j = 1 , Szy
            Map(i,j) = 0.
         ENDDO
      ENDDO
      DO i = 1, Wszx
         DO j = 1, Wszy
            Work(i,j) = 0.
         ENDDO
      ENDDO
      DO i = 1, Tszx
         DO j = 1, Tszy
            Twork(i,j) = 0.
         ENDDO
      ENDDO
c
c  Set unit
c
      Unit = ' '
      if ( Type.eq.'EVT' ) then
         Unit = 'count'
      else if ( Type.eq.'TIME' ) then
         Unit = 'chi^2'
      else if ( Type.eq.'ZCOL' ) then
         CALL FTGKNS(Lun,'TUNIT',Zi,1,Unit,i,status)
         status = 0
      endif
c
c  Runs twice for time image
c
      first_time = .TRUE.
      total_image = 1.D0
c  Initialize to avoid warning
      image_distr = 0.D0
      previous_time = 0.D0
c  --
 100  IF ( Type.eq.'TIME' ) THEN
         previous_time = 0.
         IF ( first_time ) THEN
            CALL xwrite(' first pass ',10)
         ELSE
            CALL xwrite(' second pass ',10)
         ENDIF
      ENDIF
      IF ( .NOT.first_time ) total_image = image_distr
      image_distr = 0.D0
c
c  Get number of rows
c
         call FTGKYJ(Lun,'NAXIS2',nrows,comment,status)
         if ( status.eq.0 ) then
            WRITE (ZWRite,'('' File contains '',i10,'' events'')') nrows
            CALL XWRITE(zwrite,10)
         else
            call XWARN(' Warning: Failed to get event table',10)
            goto 300
         endif

         rejout = 0
         rejreg = 0
         rejeng = 0
         rejgti = 0
         rejexp = 0
         rejnull = 0
         accepted = 0
         rejected = 0
         ngot = 0
         nloop = nrows/MBUF + 1
         DO 150 i = 1 , nloop
            nget = MIN(MBUF,nrows-ngot)
            IF ( nget.GT.0 ) THEN

               ngot = ngot + nget
c
               ii = 1 + (i-1)*MBUF
               CALL FTGCFD(Lun,Xi,ii,1,nget,x,xflagvals,anyf,status)
               CALL FTGCFD(Lun,Yi,ii,1,nget,y,yflagvals,anyf,status)
               IF ( Zi.GT.0 ) CALL FTGCFJ(Lun,Zi,ii,1,nget,z,
     &              flagvals,anyf,status)
               IF ( Ei.GT.0 ) CALL FTGCFJ(Lun,Ei,ii,1,nget,engy,
     &              flagvals,anyf,status)
               IF ( Ti.GT.0 ) CALL FTGCFD(Lun,Ti,ii,1,nget,time,
     &              flagvals,anyf,status)
               IF ( Expr.ne.' ' ) call ftfrow(Lun, Expr, ii, nget,
     &              ntexpr, flagvals, status)
               IF ( status.NE.0 ) THEN
                  CALL XERROR(' Error reading event data',5)
                  GOTO 300
               ENDIF
c
               DO 2110 jj = 1 , nget
                IF ( Type.eq.'TIME' ) THEN
                 IF ( time(jj).LT.previous_time ) THEN
c                   write(*,*)previous_time,time(jj),ii,jj
                   CALL xerror(' File is not time ordered ',5)
                   status = -1
                   RETURN
                 ELSE
                   previous_time = time(jj)
                 ENDIF
                 image_distr = image_distr + 1.D0/total_image
                ENDIF
                  j = 1
                  good = .TRUE.
c
c  Filter out events with null coordinates.
c

                  if(xflagvals(jj).OR.yflagvals(jj)) then
                     good = .FALSE.
                     rejnull = rejnull + 1
                     goto 105
                  endif

c
c  Filter out events that don't match expression
c
                  if ( Expr.ne.' ' .and. .not.flagvals(jj) ) then
                     good = .FALSE.
                     rejexp = rejexp + 1
                     goto 105
                  endif
c
c use normal zoom
c
                  xreal = (x(jj)-Cenpix(1))/Usrzm(1) +
     &                 dfloat(Szx)/2. + 0.5
                  yreal = (y(jj)-Cenpix(2))/Usrzm(2) +
     &                 dfloat(Szy)/2. + 0.5
                  xtmp = nint(xreal)
                  ytmp = nint(yreal)
c
c is it inside the image? 
c
                  IF ( (xtmp.GT.Szx) .OR. (xtmp.LT.1) ) THEN
                     rejout = rejout + 1
                     if ( verbose ) then
                        write(ZWRite,*) ' Event x ', x(jj),
     &                    ' falls outside image: ', xtmp
                        call xwrite(ZWRite, rejchat)
                     endif
                     good = .FALSE.
                     GOTO 105
                  ENDIF
                  IF ( (ytmp.GT.Szy) .OR. (ytmp.LT.1) ) THEN
                     rejout = rejout + 1
                     if ( verbose ) then
                        write(ZWRite,*) ' Event y ', y(jj),
     &                    ' falls outside image: ', ytmp
                        call xwrite(ZWRite, rejchat)
                     endif
                     good = .FALSE.
                     GOTO 105
                  ENDIF
c
c is it in the region file
c 
                  IF ( Numreg.gt.0 ) THEN
                     if ( good ) then
                        good = .FALSE.
                        do ireg = 1, Numreg
                           good = good.or.isinreg(ireg, x(jj), y(jj))
                        enddo
                     endif
                     IF ( .NOT.good ) THEN
                        rejreg = rejreg + 1
                        if ( verbose ) then
                           write(ZWRite,*) ' Event ', x(jj),
     &                         ' ', y(jj), ' falls outside region'
                           call xwrite(ZWRite, rejchat)
                        endif
                        GOTO 105
                     ENDIF
                  ENDIF
c filter energy channel
                  IF ( Emin+Emax.ne.0 .and. good ) then
                     IF ( (Emin.LE.engy(jj)) .AND. 
     &                    (Emax.GE.engy(jj)) ) THEN
                        good = .TRUE.
                     ELSE
                        rejeng = rejeng + 1
                        if ( verbose ) then
                           write(ZWRite,*) ' Energy ', engy(jj),
     &                       ' falls outside min/max'
                           call xwrite(ZWRite, rejchat)
                        endif
                        good = .FALSE.
                        GOTO 105
                     ENDIF
                  ENDIF
c
c filter in gti
                  IF ( gtinum.EQ.0 ) THEN
                     good = .TRUE.
                  ELSE
                     good = .FALSE.
                     DO WHILE ( j.LE.Gtinum .AND. .NOT.good )
                        IF ( (time(jj).LE.Gtistop(j)) .AND. 
     &                       (time(jj).GE.Gtistart(j)) ) good = .TRUE.
                        j = j + 1
c                     write(*,*)'Time(jj),good ', time(jj),j, good
                     ENDDO
                     if (.not.good ) then
                        rejgti = rejgti + 1
                        if ( verbose ) then
                           write(ZWRite,*) ' Time ', time(jj),
     &                       ' falls outside GTIs'
                           call xwrite(ZWRite, rejchat)
                        endif
                     endif
                  ENDIF


 105              IF ( good ) THEN

               xind = xtmp 
               yind = ytmp

               IF ( Type.eq.'ZCOL' ) THEN

                  Map(xind,yind) = Map(xind,yind)+z(jj)

               ELSEIF ( Type.eq.'COLOR' ) THEN

                 Work(xind,yind) = Work(xind,yind) + 1.
                 Map(xind,yind) = 
     &              (Map(xind,yind)*(Work(xind,yind)-1)+
     &               real(engy(jj)-Emin)/real(Emax-Emin)*100.)
     &                 / Work(xind,yind)

               ELSEIF ( Type.eq.'TIME' ) THEN

                  IF ( first_time ) THEN
                     Work(xind,yind) = Work(xind,yind) + 1.
                  ELSE
                     Twork(xind,yind) = Twork(xind,yind) + 1.
                     if( Work(xind,yind).gt.0. )then
                        dist = ABS(image_distr-Twork(xind,yind)
     &                         /Work(xind,yind))*1000.
                     else
                        dist = 0.
                     endif
                     IF ( dist.GT.Map(xind,yind) ) Map(xind,yind) = dist
                  ENDIF

               ELSE

                  Map(xind,yind) = Map(xind,yind)+1.

               END IF
c
                     accepted = accepted + 1
                  ELSE
                     rejected = rejected + 1
                  ENDIF
 2110           CONTINUE
            ENDIF
 150     CONTINUE
c
         IF ( status.NE.0 ) THEN
            CALL XERROR(' status not 0 after array read',5)
            GOTO 300
         ENDIF
c
         WRITE (ZWRite,*) ' Accepted: ' , accepted , 
     &                           ' Rejected: ' , rejected
         call rmvxbk(ZWRite(2:))
         CALL XWRITE(ZWRite,5)

         if ( rejnull.gt.0 ) then
            WRITE (ZWRite,*) ' Null coordinates cut = ', 
     &                        rejnull, ' events'
            call rmvxbk(ZWRite(2:))
            CALL XWRITE(ZWRite,15)
         endif
         if ( rejexp.gt.0 ) then
            WRITE (ZWRite,*) ' Expression cut = ', rejexp, ' events'
            call rmvxbk(ZWRite(2:))
            CALL XWRITE(ZWRite,15)
         endif
         if ( rejout.gt.0 ) then
            WRITE (ZWRite,*) ' Image bounds cut = ', rejout,' events'
            call rmvxbk(ZWRite(2:))
            CALL XWRITE(ZWRite,15)
         endif
         if ( rejeng.gt.0 ) then
            WRITE (ZWRite,*) ' Energy cut = ', rejeng, ' events'
            call rmvxbk(ZWRite(2:))
            CALL XWRITE(ZWRite,15)
         endif
         if ( rejgti.gt.0 ) then
            WRITE (ZWRite,*) ' Time cut = ', rejgti, ' events'
            call rmvxbk(ZWRite(2:))
            CALL XWRITE(ZWRite,15)
         endif
         if ( rejreg.gt.0 ) then
            WRITE (ZWRite,*) ' Region cut = ', rejreg, ' events'
            call rmvxbk(ZWRite(2:))
            CALL XWRITE(ZWRite,15)
         endif

         IF ( Type.eq.'TIME' ) THEN
            DO 520 i = 1 , Szy
               DO 510 j = 1 , Szx
                  IF ( Work(j,i).GT.10 ) THEN
                     IF ( Work(j,i).GT.100 ) THEN
                        temp = 4.*(Map(j,i)/1000.)
     &                         **2.*100.*10.*(2.-
     &                         exp((100.-Work(j,i))/100.))

c
c      the above statement is to limit the number of photons in the
c      calculation of chi**2 to 200. This is to reduce the correlation
c      between test sensitivity and count rate.  Temp grows linearly, as
c      normal KS test requires, up to a value of 100 after that it deviates
c      from linear growth and is limited to 200.
c
c
                     ELSE
                        temp = 4.*(Map(j,i)/1000.)
     &                         **2.*Work(j,i)*10.
                     ENDIF
                     Map(j,i) = min(temp,200.)
                  ELSE
                     Map(j,i) = 0.
                  ENDIF
 510           CONTINUE
 520        CONTINUE
         ENDIF

      IF ( Type.eq.'TIME' .AND. first_time ) THEN
         first_time = .FALSE.
         GOTO 100
      ENDIF
c
c find min/max and fill exposure map
c
      largest = -1e30
      smallest = 1e30

      DO 200 i = 1 , Szx
         DO 160 j = 1 , Szy
            dd = Map(i,j)
            Map(i,j) = dd*Ximscale+Ximzero
            largest = MAX(Map(i,j),largest)
            smallest = MIN(Map(i,j),smallest)
 160     CONTINUE
 200  CONTINUE
c
      Datamin = smallest
      Datamax = largest
c
c  Cleanup
c
 300  CONTINUE
      IF ( status.EQ.0 ) RETURN
      WRITE (ZWRite,'(a,i4)') ' Error in event reader, status: ', 
     &                        status
      CALL XERROR(ZWRite,10)
      call ftgerr(status, ds)
      ZWRite = '   '//ds
      CALL XERROR(ZWRite,10)
      RETURN
      END
