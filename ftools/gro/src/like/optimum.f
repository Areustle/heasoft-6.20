C=======================================================================
C        SUBROUTINE OPTIMUM(CtoZ,GtoZ,BtoZ)
C
C
C  $Id: optimum.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++           effect: optimize parameters - low level routine
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: optimum.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:19  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:37  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE OPTIMUM(CtoZ,GtoZ,BtoZ)

C     Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/likrep.copy'
      INCLUDE '../COMMON/maprep.copy'
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/gasrep.copy'
      INCLUDE '../COMMON/emprep.copy'
      INCLUDE '../COMMON/bmprep.copy'

      save

C     ************ ARGUMENTS *****************
      character(80) id
      common /id/id
      REAL score(3),information(3,3),INDX_pivot(3)
      logical changed,maximum,signal_flag
      logical CtoZ,GtoZ,BtoZ
      data LOC/'OPTIMUM'/

      id = '$Id: optimum.f,v 1.2 2013/05/21 19:08:26 irby Exp $'


c     determine Nopt
      Nopt=0
      signal_flag=.false.

      if (OptC) Nopt=Nopt+1
      if (OptB) Nopt=Nopt+1
      if (OptG) Nopt=Nopt+1

      if (Nopt.lt.1) then
         signal='O'
         sigmsg='OPTIMUM: No opt flags set.'
         return
      endif

      nloop=0
      CtoZ=.false.
      GtoZ=.false.
      BtoZ=.false.
 12   nloop=nloop+1

      if (verbose) then
         WRITE(6,'("VERBOSE(OPTIMUM):ITER",i2,"  Cnts",e12.4,
     &        " Gmult",e13.5," Gbias",e13.5)')nloop,Counts,Gmult,Gbias
      endif

      if (nloop.gt.50) then
         signal='I'
         sigmsg='OPTIMUM: exceeded maximum iterations.'
         return
      endif

      call LHOOD(map,bmap,gasmap,emap,score,information
     &     ,CTLMSZ1,CTLMSZ2)
      
      maximum=.true.
      do l=1,Nopt
         if(information(l,l).le.0.)maximum=.false.
      enddo
      
c     solve for the Newton step 
c     decompose information matrix
      call LUDCMP(information,Nopt,3,INDX_pivot,D)

      if (signal.eq.'S') then
         WRITE(sigmsg,'("optimum: Estimate:C=",e9.2,"Gm=",e9.2,"Gb=",
     *        e9.2," causes sing. info. matrix")')Counts,Gmult,Gbias
         return
      endif

c     solve by substitution (information * dTHETA = score
      call LUBKSB(information,Nopt,3,INDX_pivot,score)
      
      if (Nopt.eq.3) then
         dCounts=score(1)
         dGmult=score(2)
         dGbias=score(3)

      elseif (Nopt.eq.2) then
         if (.not.OptC) then
            dGmult=score(1)
            dGbias=score(2)

         elseif (.not.OptB) then
            dCounts=score(1)
            dGmult=score(2)

         else
            dCounts=score(1)
            dGbias=score(2)
         endif

      else
         if (OptC) then
            dCounts=score(1)

         elseif (OptB) then
            dGbias=score(1)

         else
            dGmult=score(1)
         endif
      endif

      change=-1.
      step_max=1.
 
      if (OptB) then
         if (abs(dGbias/Gbias).gt.change) then
	    change=abs(dGbias/Gbias)
         endif

         Gbias=Gbias+dGbias

         if (Gbias.lt.0.) then
            if (BtoZ) then
               Gbias=0.
               if (verbose) WRITE(6,*)
     &              'VERBOSE(OPTIMUM):new Gbias<0, abort'
               goto 20

            else
               if ((dGbias-Gbias)/dGbias.lt.step_max) then
                  step_max=(dGbias-Gbias)/dGbias
               endif

               BtoZ=.true.
            endif

         else
	    BtoZ=.false.
         endif
      endif

      if (OptG) then
         if (abs(dGmult/Gmult).gt.change) then
	    change=abs(dGmult/Gmult)
         endif

         Gmult=Gmult+dGmult

         if (Gmult.lt.0.) then
            if (GtoZ) then
               Gmult=0.
               if (verbose) WRITE(6,*)
     &              'VERBOSE(OPTIMUM):new Gmult<0, abort'
               BtoZ=.false.
               goto 20

            else
               if ((dGmult-Gmult)/dGmult.lt.step_max) then
                  step_max=(dGmult-Gmult)/dGmult
                  BtoZ=.false.
                  GtoZ=.true.
               endif
            endif

         else
	    GtoZ=.false.
         endif
      endif

      if (OptC) then
         if (abs(dCounts/Counts).gt.change) then
	    change=abs(dCounts/Counts)
         endif

         Counts=Counts+dCounts

         if (Counts.lt.0.) then
            if (CtoZ) then
               Counts=0.
               if (verbose) WRITE(6,*)
     &              'VERBOSE(OPTIMUM):new Counts<0, abort'
               BtoZ=.false.
               GtoZ=.false.
               goto 20

            else
               if ((dCounts-Counts)/dCounts.lt.step_max) then
                  step_max=(dCounts-Counts)/dCounts
                  BtoZ=.false.
                  GtoZ=.false.
                  CtoZ=.true.
               endif
            endif

         else
	    CtoZ=.false.
         endif
      endif

      if (BtoZ.or.GtoZ.or.CtoZ) then
         if (Nopt.eq.3) then
            Counts=Counts+score(1)*(step_max-1.)
            Gmult=Gmult+score(2)*(step_max-1.)
            Gbias=Gbias+score(3)*(step_max-1.)

         elseif (Nopt.eq.2) then
            if (.not.OptC) then
               Gmult=Gmult+score(1)*(step_max-1.)
               Gbias=Gbias+score(2)*(step_max-1.)

            elseif (.not.OptB) then
               Counts=Counts+score(1)*(step_max-1.)
               Gmult=Gmult+score(2)*(step_max-1.)

            else
               Counts=Counts+score(1)*(step_max-1.)
               Gbias=Gbias+score(2)*(step_max-1.)
            endif

         else
            if (OptC) then
               Counts=0.1

            elseif (OptB) then
               Gbias=0.001

            else
               Gmult=0.001
            endif
         endif
      endif
      
      if (signal_flag) goto 322
      if (Gmult.gt.Gmult_nom) signal='D'
      if (Gbias.gt.Gbias_nom) signal='D'
      if (Counts.gt.1.e7) signal='D'

      if (signal.eq.'D')  then
         sigmsg='OPTIMUM:parameter divergence occuring'
         write(*,*)'********************************************'
         CALL ERROR(0,LOC)
         write(*,*)'********************************************'
         write(*,'("Gmult: ",f5.1)') Gmult
         write(*,'("Gbias: ",f5.1)') Gbias
         write(*,'("Counts:",f5.1)') Counts
         write(*,*)'********************************************'
         signal_flag=.true.
      endif
      
 322  changed=.false.
      if (change.gt.1.e-5) then
         changed=.true.
         if (change.lt.1.e-3.and.nloop.gt.30) changed=.false.
         if (change.lt.1.e-2.and.nloop.gt.40) changed=.false.
         if (change.lt.1.e-1.and.nloop.gt.47) changed=.false.
      endif

      if (changed) goto 12
      
      if (.not.maximum) then
         signal='M'
         sigmsg='OPTIMUM: stationary point is not a maximum.'
         return
      endif
      RETURN

 20   continue
      signal='-'
      sigmsg='OPTIMUM:parameter estimate < 0'

      return
      END
