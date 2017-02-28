      subroutine maptrfm(Ra,Dec,Crot,Rao,Deco,Croto,Pixsize,Map,
     &                   Szx,Szy,Exmap,Exszx,Exszy,Maptype,Mapo,
     &                   Szxo,Szyo,Exmapo,Exszxo,Exszyo)
      implicit none
c
c  Routine to transform (i.e. rotate and translate) image
c
c  I  Ra      (d)  RA of start image's center
c  I  Dec     (d)  Dec of start image's center
c  I  Crot    (d)  Rotation of start image
c  I  Rao     (d)  RA of output image's center
c  I  Deco    (d)  Dec of output image's center
c  I  Croto   (d)  Rotation of output image
c  I  Pixsize (d)  Pixels size of images
c  I  Map     (r)  Start image map
c  I  Szx     (i)  Size of start map in x
c  I  Szy     (i)  Size of start map in y
c  I  Exmap   (r)  Exposure map
c  I  Exszx   (i)  Size of exposure map in x
c  I  Exszy   (i)  Size of exposure map in y
c  I  Maptype (c)  I=integer R=real
c  O  Mapo    (r)  Output map
c  I  Szxo    (i)  Size of output map in x
c  I  Szyo    (i)  Size of output map in y
c  O  Exmapo  (r)  Output exposure map
c  I  Exszxo  (i)  Size of output exposure map in x
c  I  Exszyo  (i)  Size of output exposure map in y
c
      real*8 Ra, Dec, Crot, Rao, Deco, Croto, Pixsize(2)
      integer*4 Szx, Szy, Szxo, Szyo, Exszx, Exszy, Exszxo, Exszyo
      real*4 Map(Szx,Szy), Exmap(Exszx,Exszy)
      real*4 Mapo(Szxo,Szyo), Exmapo(Exszxo,Exszyo)
      character(1) Maptype 

      include '../include/maxvals.inc'
      include '../include/pi.inc'
      include '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 ximh, yimh, ximho, yimho, seed
      INTEGER*4 i , j , iy , ix , iiy
      INTEGER*4 ixnew , iynew , iix
      REAL*4 xnew , ynew , fracx , fracy , delal
      REAL*4 pix(4), npix(4), rnull
      REAL*8 radian
      REAL*4 gamma, beta, delde, cosan
      LOGICAL doexpo, isrnull

      if ( Szx.eq.Exszx .and. Szy.eq.Exszy ) then
         doexpo = .TRUE.
      else
         doexpo = .FALSE.
      endif
      
      radian = 180.d0/PI

      CALL GETSEED(seed)
c
c calculate variables for the rota-traslation
c
      gamma = (Crot-Croto)/radian
      beta = -Croto/radian
      delde = (Dec-Deco)/Pixsize(2)
      cosan = DCOS(Dec/radian)
      delal = (Ra-Rao)/(-Pixsize(1))*cosan
c
c Initialize map(s)
c
      if ( doexpo ) then
         DO i = 1 , Szxo
            DO j = 1 , Szyo
               Mapo(i,j) = rnull()
               Exmapo(i,j) = rnull()
            ENDDO
         ENDDO
      else
         DO i = 1 , Szxo
            DO j = 1 , Szyo
               Mapo(i,j) = rnull()
            ENDDO
         ENDDO
      endif
c
c setting variable to calculate exposure map
c in the exposure map the value in each pixel
c
C process image and store photons in map
C
      ximh = Szx/2
      yimh = Szy/2
      ximho = Szxo/2
      yimho = Szyo/2

      DO 300 iy = 1 , Szy
         DO 250 ix = 1 , Szx
            iix = ix - ximh
            iiy = iy - yimh
            xnew = FLOAT(iix)*COS(gamma) - FLOAT(iiy)*SIN(gamma)
     &             + delde*SIN(beta) - delal*COS(beta) + FLOAT(ximho)
            ynew = FLOAT(iix)*SIN(gamma) + FLOAT(iiy)*COS(gamma)
     &             + delde*COS(beta) + delal*SIN(beta) + FLOAT(yimho)
            ixnew = xnew
            iynew = ynew
            IF ( ixnew.GT.0 .AND. ixnew.LT.Szxo ) THEN
               IF ( iynew.GT.0 .AND. iynew.LT.Szyo ) THEN
                  IF ( Maptype.eq.'I' ) THEN
                     ixnew = xnew + 0.5
                     iynew = ynew + 0.5
                     if ( isrnull(Mapo(ixnew,iynew)) ) then
                        Mapo(ixnew,iynew) = Map(ix,iy)
                     else
                        Mapo(ixnew,iynew) = Map(ix,iy)+Mapo(ixnew,iynew)
                     endif
                     if ( doexpo ) then
                        if ( isrnull(Exmapo(ixnew,iynew)) ) then
                             Exmapo(ixnew,iynew) = Exmap(ix,iy)
                        endif
                     endif
                  ELSEIF ( .not.isrnull(Map(ix,iy)) ) THEN
c                 IF ( .not.isrnull(Map(ix,iy)) ) THEN
                     fracx = xnew - ixnew
                     fracy = ynew - iynew
                     pix(1) = (1.-fracx)*(1.-fracy)
                     pix(2) = fracx*(1.-fracy)
                     pix(3) = fracx*fracy
                     pix(4) = 1. - pix(1) - pix(2) - pix(3)
                     CALL DISRO(pix,Map(ix,iy),4,seed,Maptype,npix)
                     if ( isrnull(Mapo(ixnew,iynew)) ) then
                        Mapo(ixnew,iynew) = npix(1)
                     else
                        Mapo(ixnew,iynew) = Mapo(ixnew,iynew) + npix(1)
                     endif
                     if ( doexpo ) then
                        if ( isrnull(Exmapo(ixnew,iynew)) ) 
     &                     Exmapo(ixnew,iynew) = Exmap(ix,iy)
                     endif
                     IF ( ixnew.LT.Szxo ) THEN
                        if ( isrnull(Mapo(ixnew+1,iynew)) ) then
                           Mapo(ixnew+1,iynew) = npix(2)
                        else
                           Mapo(ixnew+1,iynew) = Mapo(ixnew+1,iynew)
     &                                           + npix(2)
                        endif
                        if ( doexpo ) then
                           if ( isrnull(Exmapo(ixnew+1,iynew)) ) 
     &                        Exmapo(ixnew+1,iynew) = Exmap(ix,iy)
                        endif
c
                        IF ( iynew.GE.Szyo ) GOTO 250
                        if ( isrnull(Mapo(ixnew+1,iynew+1)) ) then
                           Mapo(ixnew+1,iynew+1) = npix(3)
                        else
                           Mapo(ixnew+1,iynew+1) = Mapo(ixnew+1,iynew+1)
     &                                             + npix(3)
                        endif
                        if ( doexpo ) then
                           if ( isrnull(Exmapo(ixnew+1,iynew+1)) ) 
     &                        Exmapo(ixnew+1,iynew+1) = Exmap(ix,iy)
                        endif
                     ENDIF
                     IF ( iynew.LT.Szyo ) THEN
                        if ( isrnull(Mapo(ixnew,iynew+1)) ) then
                           Mapo(ixnew,iynew+1) = npix(4)
                        else
                           Mapo(ixnew,iynew+1) = Mapo(ixnew,iynew+1)
     &                                           + npix(4)
                        endif
                        if ( doexpo ) then
                           if ( isrnull(Exmapo(ixnew,iynew+1)) ) 
     &                        Exmapo(ixnew,iynew+1) = Exmap(ix,iy)
                        endif
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
 250     CONTINUE
 300  CONTINUE

      RETURN
      END
