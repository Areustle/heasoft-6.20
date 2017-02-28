      SUBROUTINE DSCALE(Y, Iery, Mxrow, Mxwin, Mx2d, Ngroup, Mxpar,
     &    Igap, Rgap, Idoall, Iskip, Newmod, Iactw, Logx, Logy, Imaster,
     &    Igrpos, Ipwin, Icont, Image, Ipmod, Icomp, Pval, Nterm,
     &    Imnmx, Xymnmx, Ermnmx, Xyscal)
      INTEGER   Mxrow, Mxwin, Mx2d, Ngroup, Mxpar
      INTEGER   Igap, Idoall, Iskip, Newmod, Imnmx
      INTEGER   Iery(*), Ipwin(*), Iactw(*), Logx(*), Logy(*)
      INTEGER   Igrpos(3, *), Icont(*), Image(*)
      INTEGER   Ipmod(*), Icomp(2*Mxpar, *), Nterm(*), Imaster(2,*)
      REAL      Rgap
      REAL      Y(*), Pval(Mxpar, *)
      REAL      Xymnmx(4, *), Ermnmx(4, *), Xyscal(4, *)
C---
C Find the min and max data values, and set the default PLT scales.
C We must set the scale for each window.  The default is plot group N
C is plotted in window N.
C---
C Y       I
C Iery    I
C Mxrow   I
C Mxwin   I    Maximum number of windows
C Mx2d    I    Maximum number of 2D plots
C Ngroup  I
C Mxpar   I
C Igap    I    =0 ignore errors, <>0 include error bar in min/max.
C Rgap    I
C Idoall  I
C Iskip   I
C Newmod  I
C Iactw   I    per window
C Logx    I    Per window
C Logy    I    Per window
C Imaster I    per window
C Igrpos  I    Per group
C Ipwin   I    Per group
C Icont   I    Per group
C Image   I    per group
C Ipmod   I    per group
C Pval    I    per model
C Nterm   I    per model
C Imnmx   I/O
C Xymnmx  I/O  The data min/max values by group, xmin, ymin, xmax, ymax
C Ermnmx  I/O  The min/max values with errors,   xmin, xmax, ymin, ymax
C Xyscal  I/O  The window min/max values,        xmin, xmax, ymin, ymax
C---
C 1990-Feb-26 - Extracted from PLT [AFT]
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      REAL      RMAX
      PARAMETER (RMAX=1.E35)
      INTEGER   MXDIM
      PARAMETER (MXDIM=2)
      REAL      FNFIT
C
      REAL      xt(MXDIM)
      REAL      tmplo, tmphi, tmp1, tmp2, xcen, xm, xp, yerr, yt
      INTEGER   i, i2drow, igroup, itmp, iwnum, iy0, iyi, iyoff
      INTEGER   lery, ndim
C---
C If new data or SKIP has changed, then the plot groups may have
C changed.  For this case scan all the groups and reset the Data min
C and max values
      IF ( Iskip.NE.Imnmx .OR. Newmod.NE.0 ) THEN
         DO 130 igroup = 1, Ngroup
            IF ( igrpos(1,igroup).LT.0 ) GOTO 130
            IF ( Iskip.EQ.Imnmx .AND. Newmod.NE.ABS(ipmod(igroup)) )
     &            GOTO 130
            Xymnmx(1,igroup) = RMAX
            Xymnmx(3,igroup) = -RMAX
            Xymnmx(2,igroup) = RMAX
            Xymnmx(4,igroup) = -RMAX
C
            Ermnmx(1, igroup) = RMAX
            Ermnmx(2, igroup) = -RMAX
            Ermnmx(3, igroup) = RMAX
            Ermnmx(4, igroup) = -RMAX
            yerr = 0.0
C
            iy0 = Igrpos(1, igroup)
            lery = Iery(Igrpos(3, igroup))
            IF ( lery.GT.0 ) THEN
               i2drow = Mxrow*(lery+1)
            ELSE
               i2drow = Mxrow
            END IF
            DO 120 i = 1, Igrpos(2, igroup)
               CALL PLTXCC(Y, i, igroup, xt, ndim, iyoff)
               IF ( xt(1).EQ.NO ) GOTO 120
               xcen = xt(1)
               iyi = iy0 + iyoff
               yt = Y(iyi)
               IF ( ipmod(igroup).LT.0 .AND. yt.NE.NO ) THEN
C ipmod.LT.0 means plot residuals
                  itmp = ABS(ipmod(igroup))
                  yt = yt - FNFIT(xcen,ICOMP(1,itmp),
     &              PVAL(1,itmp),nterm(itmp))
               END IF
               Xymnmx(1,igroup) = MIN(Xymnmx(1,igroup), xcen)
               Xymnmx(3,igroup) = MAX(Xymnmx(3,igroup), xcen)
               CALL PLTXCE(Y, i, igroup, 1, xm, xp)
               Ermnmx(1, igroup) = MIN(Ermnmx(1, igroup), xcen+xm)
               Ermnmx(2, igroup) = MAX(Ermnmx(2, igroup), xcen+xp)
               IF ( yt.NE.NO ) THEN
C Min/Max of data
                  Xymnmx(2,igroup) = MIN(Xymnmx(2,igroup), yt)
                  Xymnmx(4,igroup) = MAX(Xymnmx(4,igroup), yt)
C Now calculate error
                  IF ( lery.NE.0 ) THEN
                     IF ( lery.GT.0 ) THEN
                        yerr = Y(iyi+Mxrow)
                     ELSE
                        IF ( Y(iyi).GT.0.0 ) THEN
                           yerr = SQRT(Y(iyi))
                        ELSE
                           yerr = 1.0
                        END IF
                     END IF
                  END IF
C Min/Max of data with error
                  Ermnmx(3, igroup) = MIN(Ermnmx(3, igroup), yt-yerr)
                  Ermnmx(4, igroup) = MAX(Ermnmx(4, igroup), yt+yerr)
               END IF
  120       CONTINUE
  130    CONTINUE
         Imnmx = Iskip
         Newmod = 0
      END IF
C---
C Now set default min and max values in each window.
      DO 290 iwnum = 1, Mxwin
         IF ( Iactw(iwnum).EQ.0 ) GOTO 290
         IF ( Xyscal(1, iwnum).EQ.NO .OR. Xyscal(3, iwnum).EQ.NO ) THEN
C Find min/max X values for all groups plotted in the current window.
            tmp1 = RMAX
            tmp2 = -RMAX
            DO igroup = 1, Ngroup
               IF ( igrpos(1,igroup).GE.0 ) THEN
                  IF ( Idoall.NE.0 ) THEN
                     IF ( Ipwin(igroup).GT.0 ) THEN
                        IF ( Igap.EQ.0 ) THEN
                           tmp1 = MIN(tmp1, Xymnmx(1,igroup))
                           tmp2 = MAX(tmp2, Xymnmx(3,igroup))
                        ELSE
                           tmp1 = MIN(tmp1, Ermnmx(1, igroup))
                           tmp2 = MAX(tmp2, Ermnmx(2, igroup))
                        END IF
                     END IF
                  ELSE
                     IF ( Ipwin(igroup).EQ.iwnum ) THEN
                        IF ( Igap.EQ.0 ) THEN
                           tmp1 = MIN(tmp1, Xymnmx(1,igroup))
                           tmp2 = MAX(tmp2, Xymnmx(3,igroup))
                        ELSE
                           tmp1 = MIN(tmp1, Ermnmx(1, igroup))
                           tmp2 = MAX(tmp2, Ermnmx(2, igroup))
                        END IF
                     END IF
                  END IF
               END IF
            END DO
            IF ( tmp1.NE.RMAX ) THEN
               CALL PLTGAP(tmp1, tmp2, Rgap, Logx(iwnum), tmplo, tmphi)
            ELSE
               CALL PLTGAP(Xymnmx(1,iwnum), Xymnmx(3,iwnum), Rgap,
     &            Logx(iwnum), tmplo, tmphi)
            END IF
            IF ( Xyscal(1, iwnum).EQ.NO ) Xyscal(1, iwnum) = tmplo
            IF ( Xyscal(3, iwnum).EQ.NO ) Xyscal(3, iwnum) = tmphi
         END IF
         IF ( Xyscal(2, iwnum).EQ.NO .OR. Xyscal(4, iwnum).EQ.NO ) THEN
C Now find min/max of all y coordinate values.  Since contours and
C images are handled differently do them first.  If no contour/image
C then set scale the old fashion way.
            tmp1 = RMAX
            tmp2 = -RMAX
            xm = 0.0
            xp = 0.0
            DO igroup = 1, mx2d
               IF ( Ipwin(igroup).EQ.iwnum ) THEN
                  IF ( Icont(igroup).GT.0 .OR.
     &                 Image(igroup).GT.0 ) THEN
C If a contour/image appears in the current window, then the default
C ymax is set by the corners of the 2D array being plotted
                     CALL PLTXCC(Y, 1, igroup, xt, ndim, iyoff)
                     IF ( ndim.NE.2 ) GOTO 180
                     IF ( igap.NE.0 ) THEN
                        CALL PLTXCE(Y, 1, igroup, 2, xm, xp)
                     END IF
                     tmp1 = MIN(tmp1, xt(2)+xm)
                     tmp2 = MAX(tmp2, xt(2)+xp)
                     CALL PLTXCC(Y,igrpos(2,igroup),igroup,
     &                     xt,ndim,iyoff)
                     IF ( igap.NE.0 ) THEN
                        CALL PLTXCE(Y,igrpos(2,igroup),igroup,2,xm,xp)
                     END IF
                     tmp1 = MIN(tmp1, xt(2)+xm)
                     tmp2 = MAX(tmp2, xt(2)+xp)
                  END IF
               END IF
            END DO
            IF ( tmp1.NE.RMAX ) GOTO 190
C
C No contour/images in the current window, use the data min/max of
C the plot groups that appear in the window.
  180       CONTINUE
            tmp1 = RMAX
            tmp2 = -RMAX
            DO igroup = 1, Ngroup
               IF ( Ipwin(igroup).EQ.iwnum ) THEN
                  IF ( Igap.EQ.0 ) THEN
                     tmp1 = MIN(tmp1, Xymnmx(2,igroup))
                     tmp2 = MAX(tmp2, Xymnmx(4,igroup))
                  ELSE
                     tmp1 = MIN(tmp1, Ermnmx(3, igroup))
                     tmp2 = MAX(tmp2, Ermnmx(4, igroup))
                  END IF
               END IF
            END DO
  190       CONTINUE
            IF ( tmp1.NE.RMAX ) THEN
               CALL PLTGAP(tmp1, tmp2, Rgap, Logy(iwnum), tmplo, tmphi)
               IF ( Xyscal(2, iwnum).EQ.NO ) Xyscal(2, iwnum) = tmplo
               IF ( Xyscal(4, iwnum).EQ.NO ) Xyscal(4, iwnum) = tmphi
            END IF
         END IF
C---
C Check for negative scales when using a LOG plot.
         IF ( Logy(iwnum).NE.0 .AND. Xyscal(2, iwnum).LT.0. ) THEN
            tmp1 = RMAX
            tmp2 = -RMAX
            DO igroup = 1, Ngroup
               IF ( Ipwin(igroup).EQ.iwnum ) THEN
                  iy0 = Igrpos(1, igroup)
                  lery = Iery(Igrpos(3, igroup))
                  IF ( lery.GT.0 ) THEN
                      i2drow = Mxrow*(lery+1)
                  ELSE
                     i2drow = Mxrow
                  END IF
                  DO i = 1, Igrpos(2, igroup)
                     CALL PLTXCC(Y, i, igroup, xt, ndim, iyoff)
                     iyi = iy0 + iyoff
                     IF ( xt(1).NE.NO .AND. Y(iyi).GT.0. ) THEN
                        tmp1 = MIN(tmp1, Y(iyi))
                        tmp2 = MAX(tmp2, Y(iyi))
                     END IF
                  END DO
               END IF
            END DO
            CALL PLTGAP(tmp1,tmp2,Rgap,Logy(iwnum),Xyscal(2,iwnum),yt)
            IF ( Xyscal(4, iwnum).LT.0. ) Xyscal(4, iwnum) = yt
         END IF
C
         IF ( Logx(iwnum).NE.0 .AND. Xyscal(1, iwnum).LT.0. ) THEN
            tmp1 = RMAX
            tmp2 = -RMAX
            DO igroup = 1, Ngroup
               IF ( Ipwin(igroup).EQ.iwnum ) THEN
                  DO i = 1, Igrpos(2, igroup)
                     CALL PLTXCC(Y, i, igroup, xt, ndim, iyoff)
                     IF ( xt(1).NE.NO .AND. xt(1).GT.0 ) THEN
                        tmp1 = MIN(tmp1, xt(1))
                        tmp2 = MAX(tmp2, xt(1))
                     END IF
                  END DO
               END IF
            END DO
            CALL PLTGAP(tmp1,tmp2,Rgap,Logx(iwnum),Xyscal(1,iwnum),yt)
            IF ( Xyscal(3, iwnum).LT.0. ) Xyscal(3,iwnum) = yt
         END IF
 290  CONTINUE
C---
C Now define scales for the slaves
      DO iwnum=1,MXWIN
         IF ( Imaster(1,iwnum).GT.0 ) THEN
C Have an X master
            Xyscal(1, iwnum) = Xyscal(1,imaster(1,iwnum))
            Xyscal(3, iwnum) = Xyscal(3,imaster(1,iwnum))
         END IF
         IF ( Imaster(2,iwnum).GT.0 ) THEN
C Have a  Y master
            Xyscal(2, iwnum) = Xyscal(2,imaster(2,iwnum))
            Xyscal(4, iwnum) = Xyscal(4,imaster(2,iwnum))
         END IF
      END DO
C---
      RETURN
      END
