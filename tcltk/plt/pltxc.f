      SUBROUTINE PLTXCC(Yray, Ipos, Igrp, Xc, Ndim, Iyoff)
      REAL      Yray(*), Xc(*)
      INTEGER   Ipos, Igrp, Ndim, Iyoff
C Entry PLTXCD
      INTEGER   Iold, Inew
C Entry PLTXCE
      REAL      Xm, Xp
C Entry PLTXCG
      INTEGER   Idim, Ixveci, Ixoff, Iery(*)
C Entry PLTXCI
      INTEGER   Mxrowi
C Entry PLTXCL
      REAL      Offi, Slopi
C Entry PLTXCN
      INTEGER   Ndimi, Npci
C Entry PLTXCO
      REAL      Xoffi
C Entry PLTXCP
      INTEGER   Ipxei
C---
C The X-coordinate "object".  This subroutine stores all the internal
C data needed to compute the x-coordinate of data point Ipos in plot
C group with Igrp.  To allow for 2D (or higher), Xc is an array,
C with Xc(1) the traditional X position, Xc(2) Y, etc. as needed.
C---
C Yray    I    The master data array
C Igrp    I    The plot group to compute the Xc array for
C Ipos    I    The index into the plt group
C Xc        O  The X coordinates
C---
C 2000-08-10 - all the pltxcp entry - [AFT]
C 1998-10-05 - created from FNX - AFT
C---
      REAL       NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXDIM, MXGRP
      PARAMETER (MXDIM=2, MXGRP=500)
C
      REAL      slop(MXDIM, MXGRP), off(MXDIM, MXGRP)
      SAVE      slop,               off
      REAL      slop0(MXDIM, MXGRP), off0(MXDIM, MXGRP)
      SAVE      slop0,               off0
      REAL      xoff(MXDIM, MXGRP)
      SAVE      xoff
      INTEGER   ierx(MXDIM, MXGRP), ipxer(MXDIM, MXGRP)
      SAVE      ierx,               ipxer
      INTEGER   ixbeg(MXDIM, MXGRP), ixvec(MXDIM, MXGRP)
      SAVE      ixbeg,               ixvec
      INTEGER   ndims(MXGRP), new(MXDIM, MXGRP), npc(MXGRP)
      SAVE      ndims,        new,               npc
      INTEGER   mxrow
      SAVE      mxrow
      INTEGER   icolm1, id, ig, irow
C---
      Ndim = Ndims(Igrp)
      IF ( Ndims(Igrp).GT.1 ) THEN
C icolm1 is the column number minus 1
         icolm1 = (Ipos - 1)/npc(Igrp)
         irow = Ipos - icolm1*npc(Igrp)
         Xc(2)=off(2,Igrp)+icolm1*slop(2,Igrp)+xoff(2,Igrp)
         iyoff = irow + mxrow*icolm1
      ELSE
         Xc(2) = 0.0
         irow = Ipos
         Iyoff = irow
      END IF
      IF ( ixbeg(1,Igrp).LT.0 ) THEN
         Xc(1)=off(1,Igrp)+(irow-1)*slop(1,Igrp)+xoff(1,Igrp)
      ELSE
         Xc(1)=Yray(ixbeg(1,Igrp)+irow)+xoff(1,Igrp)
      END IF
      RETURN
C***
      ENTRY PLTXCD(Iold, Inew)
C Duplicate the X coordinate of an existing group
         DO id=1,MXDIM
            slop(id, Inew) = slop(id, Iold)
            off(id, Inew)  = off(id, Iold)
            xoff(id, Inew) = xoff(id, Iold)
            ierx(id, Inew) = ierx(id, Iold)
            ipxer(id, Inew) = ipxer(id, Iold)
            ixbeg(id, Inew) = ixbeg(id, Iold)
            new(id, Inew) = new(id, Iold)
         END DO
         ndims(Inew) = ndims(Iold)
         npc(Inew) = npc(Iold)
      RETURN
C***
      ENTRY PLTXCE(Yray, Ipos, Igrp, Idim, Xm, Xp)
C Return the X error
      IF ( ipxer(Idim,Igrp).EQ.-2 ) THEN
         Xp = 1+SQRT(0.75+ABS(Yray(ixbeg(Idim,Igrp)+Ipos)))
         Xm = -Xp
      ELSE IF ( ipxer(Idim,Igrp).EQ.-1 ) THEN
         Xp = SQRT(ABS(Yray(ixbeg(Idim,Igrp)+Ipos)))
         Xm = -Xp
      ELSE IF ( ipxer(Idim,Igrp).EQ.0 ) THEN
         Xp = 0.0
         Xm = 0.0
      ELSE
         IF ( ixbeg(Idim,Igrp).LT.0 ) THEN
C XAx LIN is easy
            Xp = ABS(slop(Idim,Igrp))/2.
            Xm = -Xp
         ELSE
C This uses XAx #
C%%% fails for 2d arrays, i.e., YAx #
            IF ( Ierx(idim, Igrp).EQ.1 ) THEN
               Xp = Yray(Mxrow+ixbeg(Idim,Igrp)+Ipos)
               Xm = -Xp
            ELSE IF ( Ierx(idim, Igrp).EQ.2 ) THEN
               Xp = Yray(  Mxrow+ixbeg(Idim,Igrp)+Ipos)
               Xm = Yray(2*Mxrow+ixbeg(Idim,Igrp)+Ipos)
            ELSE
               Xp = 0.0
               Xm = 0.0
            END IF
         END IF
      END IF
      RETURN
C***
      ENTRY PLTXCG(Igrp, Idim, Ixveci, Ixoff, Iery)
C Implement XAX # .  The first X coordinate is at Yray(Ixbeg+1).
C---
C Trap coding errors....
      IF ( Ixveci.LE.0 ) THEN
         WRITE(*,*) 'PLTXCG--ixveci=',ixveci
         STOP
      END IF
      IF ( Idim.GT.MXDIM ) THEN
         WRITE(*,*) 'PLTXCG--Idim,MXDIM=',Idim,MXDIM
         STOP
      END IF
      IF ( Igrp.GT.MXGRP ) THEN
         WRITE(*,*) 'PLTXCG--Igrp,MXGRP=',ixveci,MXGRP
         STOP
      END IF
C
      ixvec(Idim, Igrp) = Ixveci
      ierx(Idim, Igrp) = Iery(Ixveci)
      ipxer(Idim, Igrp) = 1
      xoff(Idim, Igrp) = 0.
      ixbeg(Idim, Igrp) = Ixoff
      RETURN
C***
      ENTRY PLTXCI(Mxrowi)
C Initialize the "new" array.  This allows pltxc to remember the first
C XAX LIN numbers, hence the user does not need to remember this.  Why?
C Because, the first XAX LIN is often defined in the *.qdp, *.pco, or
C otherwise in the calling program.
      mxrow = Mxrowi
      DO ig=1, MXGRP
         DO id=1,MXDIM
            new(id, ig) = 1
C Default XAX LIN 1 1
            ixbeg(id, ig) = -1
            off(id, ig) = 1.0
            slop(id, ig) = 1.0
            off0(id, ig) = 1.0
            slop0(id, ig) = 1.0
C No "extra" offset
            xoff(id, ig) = 0.0
         END DO
      END DO
      RETURN
C***
      ENTRY PLTXCL(Igrp, Idim, Offi, Slopi)
C Implement XAX/YAX LIN
      ixbeg(Idim, Igrp) = -1
      IF ( Offi .EQ.NO .AND. Slopi.EQ.NO ) THEN
C Both values are NO, then restore the first set of numbers.
         off(Idim, Igrp) = Off0(Idim, Igrp)
         slop(Idim, Igrp) = Slop0(Idim, Igrp)
      ELSE
         IF ( Offi .NE.NO ) off(Idim, Igrp) = Offi
         IF ( Slopi.NE.NO ) slop(Idim, Igrp) = Slopi
C An increment of zero can cause problems later.  So fudge value here.
         IF ( slop(Idim, Igrp).EQ.0. ) slop(Idim, Igrp) = 1.
         IF ( new(Idim,Igrp).NE.0 ) THEN
            new(idim, Igrp) = 0
            off0(Idim, Igrp) = off(Idim, Igrp)
            slop0(Idim, Igrp) = slop(Idim, Igrp)
         END IF
      END IF
      xoff(Idim, Igrp) = 0.
      ipxer(Idim, Igrp) = 1
      RETURN
C***
      ENTRY PLTXCN(Igrp, Ndimi, Npci)
C Set number of dimensions
      IF ( Ndimi.GT.MXDIM ) THEN
         WRITE(*,*) 'PLTXCN--Attempt to create', Ndimi,
     &            ' dimensional array.'
         WRITE(*,*) 'PLTXCN--Maximum allowed is',MXDIM, '.'
         STOP
      ELSE IF ( Igrp.GT.MXGRP ) THEN
         WRITE(*,*) 'PLTXCN--Attempt to create group number', Igrp
         WRITE(*,*) 'PLTXCN--Maximum allowed group is',MXGRP, '.'
         STOP
      END IF
      ndims(Igrp) = Ndimi
      npc(Igrp)  = Npci
      IF ( ndims(Igrp).GT.1 .AND. npc(Igrp).EQ.0 ) THEN
         WRITE(*,*) 'PLTXCN--Npc must be non-zero for 2D data!'
         STOP
      END IF
      RETURN
C***
      ENTRY PLTXCO(Igrp, Idim, Xoffi)
C Implement XAX OFF command
      xoff(Idim, Igrp) = Xoffi
      RETURN
C***
      ENTRY PLTXCP(Igrp, Idim, Ipxei)
C Implement ERR X on/off/sqrt/ger
      ipxer(Idim, Igrp) = Ipxei
      RETURN
C***
      ENTRY PLTXCQ(Igrp, Idim, Ipxei)
C Query plot X error status.
      Ipxei = ipxer(Idim, Igrp)
      RETURN
      END
