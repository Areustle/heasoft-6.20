      SUBROUTINE LOC_EXCESS(Map, Szx, Szy, Mapid, Pex, Tr, Status)
      IMPLICIT NONE
c
c  Locate excesses
c
c  Note on NULLs: As long as special NULL value is such that
c                 NINT(rnull()) = 0 no change is needed
c
c  I  map    (r)  Image map
c  I  szx/y  (i)  Size of image map
c  I  mapid  (s)  Map id string
c  I  pex    (l)  Whether to plot excesses
c  I  tr     (i)  Threshold
c  O  status (i)  Error flag (0 = OK)
c
      INTEGER*4 Szx, Szy, Tr, Status
      LOGICAL Pex
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      INCLUDE '../include/io.inc'
      INCLUDE '../include/startup.inc'
      INCLUDE 'excess.inc'
c
c  Local variables
c
      character(80) filtmp
      REAL*4 distt, xpix, ypix, ximg, yimg, tre, sumbox
      INTEGER*4 max_distance , ipas, itel
      INTEGER*4 i , j , ij , iii, jjj , ji
      LOGICAL cma, f3lx
      REAL*4 c1, c2, cr
      INTEGER*4 color, di
      REAL*8 xcen, ycen, xoff, yoff, zmx, zmy

      Status = 0

      max_distance = 800
      NUMsou = 0
      ipas = BOXsiz/3
      IF ( ipas.EQ.0 ) ipas = 1

      call get_refram(mapid, di, di, zmx, zmy, xcen, ycen, status)
      call get_itel(mapid, itel)
      call get_optax(itel, xcen, ycen, xoff, yoff)
      cma = .FALSE.
      f3lx = .FALSE.
      if ( itel.gt.0 ) then
         if ( ZTElescop(itel).eq.'EXOSAT' .and.
     &        ZINstrume(itel)(1:3).eq.'CMA' ) then
            cma = .TRUE.
            call gheads(mapid, 'FILTER', filtmp, 0, status)
            call upc(filtmp)
            if ( ZINstrume(itel)(1:4).eq.'CMA1' .and.
     &           filtmp(1:3).eq.'3LX' ) f3lx = .TRUE.
         endif
      endif
 
      if ( Pex ) call pgbbuf
      DO 100 i = 2 , Szx , ipas
         DO 50 j = 2 , Szy , ipas
c
c  Distance calculation pointless for any mission other than EXOSAT
c  May be slight difference from previous version as distt is
c    measured to pixel's center rather than its upper right corner.
c
            ximg = i
            yimg = j
            call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,xpix,ypix,ximg,
     &                     yimg,1)

            if ( itel.gt.0 ) then
               distt = SQRT((xpix-xoff)**2 + (ypix-yoff)**2)
            else
               distt = SQRT(xpix*xpix+ypix*ypix)
            endif
c
c  For EXOSAT if distance > 800 ignore
c
C*          IF ( .NOT.(cma .AND. distt.GT.max_distance .AND. IHEad(20)
C*   &           .NE.-1100) ) THEN
c
            if ( cma .and. distt.gt.max_distance ) goto 400
c
c  If EXOSAT/CMA1/3Lx and xpix<-300 and ypix>0 and distt>600, ignore
c
C*          IF ( itel.eq.1 ) THEN
C*             IF ( filter.EQ.6 .AND. xpix.LT.-300. .AND. 
C*   &              ypix.GT.0. .AND. distt.GT.600. ) GOTO 50
C*          ENDIF
c
            if ( f3lx .and. xpix.lt.-300. .and. ypix.gt.0. .and.
     &           distt.gt.600. ) goto 400

            IF ( CMA .AND. distt.GT.700. ) THEN
               tre = 1.5*Tr
            ELSE
               tre = Tr
            ENDIF
            sumbox = 0.
            DO 10 ij = 1 , BOXsiz
               DO 5 ji = 1 , BOXsiz
                  iii = i + ij - 1
                  jjj = j + ji - 1
                  IF ( iii.LE.0 .OR. jjj.LE.0 ) THEN
                     WRITE (ZWRite,99001) iii , jjj
                     CALL XWRITE(ZWRite,10)
                  ENDIF
                  IF ( iii.LE.Szx .AND. jjj.LE.Szy )
     &                 sumbox = sumbox + nint(Map(iii,jjj))
 5             CONTINUE
 10         CONTINUE
            IF ( sumbox.GE.tre ) THEN
               IF ( NUMsou.GE.MAXsou ) THEN
                  WRITE (ZWRite,99002) NUMsou
                  CALL XWRITE(ZWRite,10)
                  Status = 1
                  if ( Pex ) call pgebuf
                  RETURN
               ENDIF
               NUMsou = NUMsou + 1
               SOUrx(NUMsou) = i + BOXsiz/2.
               SOUry(NUMsou) = j + BOXsiz/2.
               RBOx(NUMsou) = sumbox
               if ( Pex ) then
c                 call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,c1,c2,
c    &                           SOUrx(NUMsou)-1.,SOUry(NUMsou)-1.,1)
                  call calimgpix(Szx,Szy,zmx,zmy,xcen,ycen,c1,c2,
     &                         SOUrx(NUMsou)-0.5,SOUry(NUMsou)-0.5,1)
                  cr = float(BOXsiz)*zmx/2.
                  color = 7
                  call get_color(color)
                  call jrnbox(c1,c2,cr*2.,cr*2.,0.,color,-1,-1)
               endif
            ENDIF
 400        continue
 50      CONTINUE
 100  CONTINUE
      if ( Pex ) call pgebuf
      IF ( NUMsou.EQ.0 ) RETURN
      WRITE (ZWRite,99003) NUMsou
      CALL XWRITE(ZWRite,10)

      DO 200 i = 1 , NUMsou
         NBRos(i) = i
 200  CONTINUE
      CALL QSORT_DETECT(RBOx,NBRos,MAXsou,NUMsou,Status)

      RETURN
99001 FORMAT (2x,2I4)
99002 FORMAT ('  Maximum excesses exceeded ',I5)
99003 FORMAT (' ',i6,' excesses found')
      END
