      SUBROUTINE CONTIG_SRCH(Mapid,Bright,Pex,Tr)
      IMPLICIT NONE
c
c  Search for contiguous excesses
c
c  I  mapid  (s)  Map id string
c  I  bright (l)  Whether to use old contig search (rather than fast)
c  I  pex    (l)  Whether to plot boxes
c  I  tr     (i)  Threshold value
c
      character*(*) Mapid
      integer*4 Tr
      logical Bright, Pex

      include 'excess.inc'
c
c  Local variables
c
      integer*4 soux(MAXsou), souy(MAXsou)

      INTEGER*4 i, j, iibb, jj, status
      REAL*4 ewtre, ewtre2, dmini, dmin2, difx , dify , ddd
      REAL*4 c1 , c2 , cr , size
      INTEGER*4 color, szx, szy, itel
      REAL*8 xcen, ycen, zmx, zmy
      LOGICAL good, npsf1st
c
      ewtre = 3.*Tr
      ewtre2 = 3.*ewtre
      call get_refram(mapid, szx, szy, zmx, zmy, xcen, ycen, status)
      call get_itel(mapid, itel)
c
      NUMexs = 0
      do i = 1 , NUMsou
         BXN(i) = 0.
         soux(i) = 0
         souy(i) = 0
      enddo

      if ( Bright ) then
         CALL XWRITE(' Using slow contiguous search',10)
      else
         CALL XWRITE(' Using fast contiguous search',10)
      endif
c
c  Init npsf1st for NPSF routine called by resolution
c
      npsf1st = .TRUE.
 
      IF ( Pex ) call pgbbuf
      DO 150 i = NUMsou , 1 , -1

c        IF ( SOUrx(NBRos(i)).NE.0. ) THEN
         IF ( NBRos(i).NE.0 ) THEN

            NUMexs = NUMexs + 1
            IF ( NUMexs.GT.MAXsou ) THEN
               CALL XWRITE(' Maximum non-contiguous excesses exceeded',
     &                       10)
               NUMexs = NUMexs - 1
               IF ( Pex ) call pgebuf
               RETURN
            ENDIF

            IF ( Bright .AND. RBOx(i).GT.ewtre2 ) THEN
               call resolution(mapid, i, szx, szy, zmx, zmy, xcen, ycen, 
     &                         itel, dmini, dmin2, npsf1st)
            ELSE
               dmini = 0.0001
               dmin2 = 0.0001
            ENDIF

            INTm(NUMexs) = RBOx(i)
            jj = 0
            iibb = 0
            DO 130 j = 1 , NUMsou
c              IF ( SOUrx(NBRos(j)).NE.0. ) THEN
               IF ( NBRos(j).NE.0 ) THEN
                  difx = ABS(SOUrx(NBRos(i))-SOUrx(NBRos(j)))
                  dify = ABS(SOUry(NBRos(i))-SOUry(NBRos(j)))
                  iibb = RBOx(j)
                  ddd = SQRT(difx*difx+dify*dify)
                  IF ( Bright ) then
                     good = ddd.le.dmin2 .and. ((ddd.ge.dmini .and.
     &                      iibb.lt.ewtre) .or. ddd.lt.dmini)
                  ELSE
                     good = ddd.lt.BOXsiz
c                    good = difx.lt.BOXsiz .and. dify.lt.BOXsiz
                  ENDIF
                  IF ( good ) THEN
                     jj = jj + 1
                     soux(NUMexs) = soux(NUMexs) + SOUrx(NBRos(j))
                     souy(NUMexs) = souy(NUMexs) + SOUry(NBRos(j))
                     if ( Bright ) then
                        BXN(NUMexs) = BOXsiz
                     else
                        size = SQRT(MAX(difx,dify))
                        BXN(NUMexs) = MAX(BXN(NUMexs),(BOXsiz+size))
                     endif
c                    SOUrx(NBRos(j)) = 0.0
                     NBRos(j) = 0
                     INTm(NUMexs) = MAX(INTm(NUMexs),iibb)
                  ENDIF
               ENDIF
 130        CONTINUE
c           SOUrx(NBRos(i)) = 0.
            NBRos(i) = 0
            IF ( jj.NE.0 ) THEN
               ASOux(NUMexs) = soux(NUMexs)/FLOAT(jj)
               ASOuy(NUMexs) = souy(NUMexs)/FLOAT(jj)
               IF ( Pex ) THEN
c                 call calimgpix(szx,szy,zmx,zmy,xcen,ycen,c1,c2,
c    &                           ASOux(NUMexs)-1.,ASOuy(NUMexs)-1.,1)
                  call calimgpix(szx,szy,zmx,zmy,xcen,ycen,c1,c2,
     &                           ASOux(NUMexs)-0.5,ASOuy(NUMexs)-0.5,1)
                  cr = (BXN(NUMexs))/2.0*zmx
                  color = 5
                  call get_color(color)
                  call jrnbox(c1,c2,cr*2.0,cr*2.0,0.,color,-1,-1)
               ENDIF
            ELSE
               NUMexs = NUMexs - 1
            ENDIF
         ENDIF
 150  CONTINUE
      IF ( Pex ) call pgebuf
c
      RETURN
      END
