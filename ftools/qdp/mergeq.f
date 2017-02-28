C Program MERGEQ
C Merge QDP files.
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXPTS, MXVEC, MXCMD
      PARAMETER (MXPTS=1048576)
      PARAMETER (MXVEC=64)
      PARAMETER (MXCMD=50)
      INTEGER   LENACT
C
      CHARACTER cmd(MXCMD)*64, cnam*64, ctmp*64, ctok*64
      REAL      yin(MXPTS), yout(MXPTS)
      REAL      tmp
      INTEGER   ieryi(MXVEC), ieryo(MXVEC), ntot(MXPTS)
      INTEGER   i, ians, ichat, icnt, idef, idin
      INTEGER   ier, ipin, ipout, ivec, ixvec
      INTEGER   j, k, lnam, ltmp, ltok, mode
      INTEGER   ncmd, ndig, ndim, ndin, ndout
      INTEGER   npts, nrin, nrout, nvec, nveco
C---
 11   FORMAT (A)
C---
      nveco = 0
      ipout = 0
      nrout = 0
      ndout = 0
 100  cnam = ' '
      CALL RDQDP(ichat,0,cnam,yin,MXPTS,ieryi,MXVEC,nrin,npts,nvec,cmd,
     &           MXCMD,ncmd,ier)
      IF ( ier.EQ.0 ) THEN
         IF ( nrout.EQ.0 ) nrout = npts
         CALL INITX(cmd,ncmd,nrin,mode)
         ixvec = 1
         IF ( mode.LT.0 .OR. nvec.EQ.1 ) ixvec = -1
         WRITE (*,*) 'MODE, IXVEC=', mode, ixvec
         ndin = 0
         DO ivec = 1, nvec
            idin = 1
            IF ( ieryi(ivec).GT.0 ) idin = idin + ieryi(ivec)
            IF ( nveco.EQ.0 .OR. ivec.NE.ixvec ) THEN
               IF ( nveco.GE.MXVEC ) THEN
                  WRITE(*,*) 'Too many vectors, quiting.'
                  GOTO 900
               END IF
               nveco = nveco + 1
               ieryo(nveco) = ieryi(ivec)
               DO j = 1, idin
                  ndin = ndin + 1
                  ndout = ndout + 1
                  ipin = (ndin-1)*nrin
                  ipout = (ndout-1)*nrout
                  DO k = 1, MIN(npts,nrout)
                     ipout = ipout + 1
                     ipin = ipin + 1
                     yout(ipout) = yin(ipin)
                  END DO
               END DO
            ELSE
               ndin = ndin + idin
            END IF
         END DO
         GOTO 100
      END IF
C---
 200  CONTINUE
      ndig = -3
      npts = MIN(npts,nrout)
      CALL GTBUF('Output file name, [ndig(-3)]:',ier)
      IF ( ier.LT.0 ) GOTO 900
      CALL GTCHAR(cnam,lnam)
      CALL GTINT(ndig, ier)
      CALL XTEND(cnam,'QDP')
      CALL OPENWR(4,cnam,'NEW',' ','L',0,0,ier)
      IF ( ier.NE.0 ) GOTO 200
      idef = -1
      CALL YORN('Do you wish to see the average',idef,ians)
C---
      IF ( ians.GT.0 ) THEN
         ndout = ndout + 1
         ipout = (ndout-1)*nrout
         DO k = 1, npts
            ipout = ipout + 1
            yout(ipout) = 0.
            yout(ipout+nrout) = 0.
         END DO
         idin = 1
         DO ivec = 1, nveco
            IF ( ivec.NE.ixvec ) THEN
               ipin = (idin-1)*nrout
               ipout = (ndout-1)*nrout
               DO k = 1, npts
                  ipout = ipout + 1
                  ipin = ipin + 1
                  tmp = yout(ipin)
                  IF ( tmp.NE.NO ) THEN
                     yout(ipout) = yout(ipout) + tmp
                     yout(ipout+nrout) = yout(ipout+nrout) + tmp*tmp
                     ntot(ipout) = ntot(ipout) + 1
                  END IF
               END DO
            END IF
            idin = idin + 1
            IF ( ieryo(ivec).GT.0 ) idin = idin + ieryo(ivec)
         END DO
         nveco = nveco + 1
         ieryo(nveco) = 1
         ipout = (ndout-1)*nrout
         DO k = 1, npts
            ipout = ipout + 1
            icnt = ntot(ipout)
            yout(ipout) = yout(ipout)/icnt
            yout(ipout+nrout) = SQRT((yout(ipout+nrout)-
     &                  icnt*yout(ipout)*yout(ipout))/(icnt-1.))/icnt
         END DO
         ndout = ndout + 1
      END IF
C---
      ctok = ' '
      ltok = 0
      ctmp = ' '
      ltmp = 0
      ndim = 0
      WRITE (*,*) nveco, npts
      DO ivec = 1, nveco
         ndim = ndim + 1
         IF ( ieryo(ivec).GT.0 ) THEN
            ndim = ndim + ieryo(ivec)
            IF ( ieryo(ivec).EQ.1 ) THEN
               IF ( ltok.EQ.0 ) THEN
                  ctok = 'READ SERR'
                  ltok = 9
               END IF
               ltok = ltok + 1
               CALL CRAMI(ivec,ctok,ltok)
            ELSE IF ( ieryo(ivec).EQ.2 ) THEN
               IF ( ltmp.EQ.0 ) THEN
                  ctmp = 'READ TERR'
                  ltmp = 9
               END IF
               ltmp = ltmp + 1
               CALL CRAMI(ivec,ctmp,ltmp)
            END IF
         END IF
      END DO
      IF ( ltok.GT.0 ) WRITE (4,11) ctok(:ltok)
      IF ( ltmp.GT.0 ) WRITE (4,11) ctmp(:ltmp)
C---
      DO i = 1, ncmd
         WRITE (4,11) cmd(i)(:LENACT(cmd(i)))
      END DO
      WRITE (4,11) '!'
C- Now write out the data.
      DO i=1,npts
         CALL WRQDAT(4, ndig, yout(i), ieryo, nrout, nveco)
      END DO
      CLOSE (UNIT=4)
C---
 900  CONTINUE
      CALL EDICOM('OFF',3)
      END
