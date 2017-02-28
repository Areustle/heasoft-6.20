      SUBROUTINE PLTSKP(Y, Iery, Mxrow, Npts, Nvec, Mxgrp,
     &   Iskip, Ixvec, Ngroup, Igrpos, Ipyer, Ipwin)
      INTEGER   Iery(*), Mxrow, Npts, Nvec, Mxgrp, Iskip
      INTEGER   Ixvec, Igrpos(3,*), Ipyer(*), Ipwin(*), Ngroup
      REAL      Y(*)
C---
C Initialize the Igrpos array when using SKIP.
C---
C Y         I    Main data array
C Iery      I
C Mxrow     I
C Npts      I
C Nvec      I
C Mxgrp     I
C Iskip     I
C Ixvec     I
C Ngroup    I/O  Total number of PLT groups
C Igrpos(1,*) O  The y-coordinate offset
C Igrpos(2,*) O  Number of points in current group
C Igrpos(3,*) O  Original vector number for the Y data
C Ipyer       O
C Ipwin       O
C---
C 1990-Feb-28 - Extracted from PLT - [AFT]
C---
      REAL       NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   IOFSET
C---
      REAL      tmp
      INTEGER   i, icnt, igrp, ioff, itmp, ivec, ixoff, iytmp
      INTEGER   lasgrp, nno
C---
C Mark any previously defined groups as undefined.
      DO igrp=1,Ngroup
         igrpos(1,igrp) = -1
         IF ( Ipwin(igrp).GT.0 ) THEN
            Ipwin(igrp)=-ABS(Ipwin(igrp))
         END IF
      END DO
C
      ixoff= IOFSET(Ixvec, Iery, Nvec, Mxrow)
      ioff = IOFSET( 1, Iery, Nvec, Mxrow)
      nno  = Iskip
      icnt = Iskip
      lasgrp= 1
      Ngroup= 0
      DO i=1,Npts
         icnt=icnt+1
         tmp = Y(ixoff+i)
         IF ( tmp.EQ.NO ) THEN
            nno=nno+1
         ELSE
            IF ( nno.GE.Iskip ) THEN
C Load number of points for previous set of groups.  Don't include the
C ending NO data flags counted Iskip times in the count.
               DO igrp=lasgrp,Ngroup
                  Igrpos(2,igrp)=icnt-nno-1
               END DO
               lasgrp = Ngroup+1
               icnt = 1
               iytmp = ioff+I-1
               DO ivec=1,Nvec
                  IF ( ivec.NE.Ixvec ) THEN
                     Ngroup = MIN(Ngroup+1,Mxgrp)
                     CALL PLTXCN(Ngroup, 1, 0)
                     itmp = iofset(ixvec, Iery, ixvec, Mxrow)+i-1
                     CALL PLTXCG(Ngroup,1,ixvec,itmp,iery)
                     Igrpos(1,Ngroup) = iytmp
                     Igrpos(3,Ngroup) = ivec
                     Ipyer(Ngroup) = MIN(Iery(ivec), 1)
                  END IF
                  iytmp = iytmp+Mxrow
                  IF(Iery(ivec).GT.0) iytmp = iytmp+Iery(ivec)*Mxrow
               END DO
            END IF
            nno = 0
         END IF
      END DO
C---
      DO igrp=lasgrp,Ngroup
         Igrpos(2,igrp) = icnt-nno
      END DO
C---
C Make sure any newly defined groups are assigned plot windows.
      DO igrp=1,Ngroup
         IF ( Igrpos(1,igrp).GE.0 ) THEN
            IF ( Ipwin(igrp).LT.0 ) THEN
               Ipwin(igrp)=ABS(Ipwin(igrp))
            ELSE
               Ipwin(igrp)=1
            END IF
         END IF
      END DO
C
      RETURN
      END
