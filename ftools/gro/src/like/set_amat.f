c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C        SUBROUTINE SET_AMAT(AMAT,PARAMJ,pposj,jflag)
C
C
C  $Id: set_amat.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C
c==========================================================================
C  effect: sets up the matrix for a 2-dim AMOEBA search (adapted from
c          Numerical Recipies).  NOTE:  This routine returns on errors
c          from FUNKJ with signal and SIGMSG set.  Parameter jflag is
c          passed to FUNKJ and the return value passed to the calling
c          function (jflag contains the verbose flag setting and is
c          left unchanged by FUNKJ).
C=============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C* AMAT(3,2)  real
C* PARAMJ(3)  real
C* PPOSJ(2)   real  inital position values: PPOSJ(1)=Long(RA),PPOSJ(2)=LAT(DEC)
c* JFLAG      logical: no longer in use
C*=============================================================================
C* Returned Values
C* ^^^^^^^^^^^^^^^
C* AMAT       contains three verticies around test position for input to AMOEBA
C* PARAMJ     contains three corresponding values of -TS
C==============================================================================
C  $Log: set_amat.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:10  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.3  1996/06/20  18:49:58  jae
c Added lines to deal with error output to
c log file.
c
c Revision 5.2  1996/04/08  16:15:20  jae
c AMOEBA array set to positive first coordinate
c
c Revision 5.1  1996/02/29  20:53:21  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:19  jae
c Subroutine Module for like V5.00
c
C Changes:
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE SET_AMAT(AMAT,PARAMJ,pposj,jflag)

c Commons
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'

      save

      character(80) id
      common /id/id
c
      logical jflag
c
      dimension AMAT(3,2),PARAMJ(3),pposj(2),ppostmp(2)
      dimension AMATFND(3,25)
      integer JFND(25)
      real FUNKJ

      id = '$Id: set_amat.f,v 1.3 2013/05/21 19:08:26 irby Exp $'


      do jj=1,3
	 do kk= 1,25
            AMATFND(jj,kk)=10
	 enddo
      enddo

      AMATMAX=10
      dt = CTLSCL/2.0
      LOC='set_amat'

      if (jae_set_amat)write(*,'(" In Routine: ",a)') LOC
      signal=' '
c
c
c---> Set up Three verticies at {(pposj(1),pposj(2)),(pposj(1)+n*dt,pposj(2))
c---> and (pposj(1),pposj(2)+n*dt)} and calculate corresponding values of -TS
c---> where n ={ -2,-1,0,1,2 }. vertices cannot be colinear
c
      j=0
      do kk = -2,2
         do ll = -2,2
            j=j+1
            ppostmp(1)= pposj(1) + float(kk)*dt
            ppostmp(2)= pposj(2) + float(ll)*dt

            if (ppostmp(1).lt.0) ppostmp(1)=ppostmp(1)+360.

            AMATFND(1,j)=ppostmp(1)
            AMATFND(2,j)=ppostmp(2)
            JFND(j)=j
            AMATFND(3,j) = FUNKJ(ppostmp,jflag)

            if (AMATFND(3,j).lt.AMATMAX) then
               AMATMAX=AMATFND(3,j)
            endif
c
c---> If error return from FUNKJ set SIGMSG and SIGNAL the RETURN to calling
c---> routine
c
            if (signal.ne.' ') then
               if (signal.eq.'f')
     &              SIGMSG = 'Error returned from FUNKJ'
               if (jae_error) call error(-2,LOC)
               return
            endif
C
c---> If FUNKJ >= 0 then initial location has a poor value of -TS and should 
c---> not be used.  If the three values of FUNKJ >= 0 then gradiant could be
c---> bad and an infinite loop can occur in AMOEBA.  Therefore, an error 
c---> condition is set, SIGNAL='Z' and the routine returns to calling routine.
c
         enddo
      enddo

      if (AMATMAX.gt.-0.005) then
         signal='Z'
         SIGMSG='set_amat error: vertices are all ~> 0'
         if (jae_error) call error(-2,LOC)
      endif

5	j=0
	do kk=1,24
           if (AMATFND(3,kk).gt.AMATFND(3,kk+1)) then
              do j=1,3
                 PARAMJ(j)=AMATFND(j,kk+1)
                 AMATFND(j,kk+1)=AMATFND(j,kk)
                 AMATFND(j,kk)=PARAMJ(j)
                 jj=JFND(kk+1)
                 JFND(kk+1)=JFND(kk)
                 JFND(kk)=jj
              enddo
           endif
	enddo

	if (j.ne.0) goto 5

        if (AMATFND(3,2).gt.-0.005) then
           signal='Z'
           SIGMSG='set_amat error: 2 vertices are ~> 0'
           if(jae_error)call error(-2,LOC)
        endif

	j=0
	iy1 = MOD(JFND(1)-1,5)-2
	ix1 = (JFND(1)-iy1)/5 -2
	iy2 = MOD(JFND(2)-1,5)-2
	ix2 = (JFND(2)-iy2)/5 -2
	ixdif=ix2-ix1
	iydif=iy2-iy1

	do jj =3,25
           iy3 = MOD(JFND(jj)-1,5)-2
           ix3 = (JFND(jj)-iy3)/5 -2
           
           if (ixdif.eq.0) then
              if (ix1.eq.ix3) goto 10
           elseif (iydif.eq.0) then
              if (iy1.eq.iy3) goto 10
           else
              slope = float(iydif)/float(ixdif)
              binter= float(iy1) - slope*float(ix1)
              ytest = slope*float(ix3)+binter
              if (abs(ytest-float(iy3)).lt.dt/2) goto 10
           endif

           j=jj
           goto15
 10        continue
	enddo

 15     JFND(1)=1
	JFND(2)=2
	JFND(3)=j

	do jj=1,3
           AMAT(jj,1)=AMATFND(1,JFND(jj))
           AMAT(jj,2)=AMATFND(2,JFND(jj))
           PARAMJ(jj)=AMATFND(3,JFND(jj))
	enddo			  

        return
        end
c
