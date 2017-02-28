C        SUBROUTINE OPTIMIZE
C
C
C  $Id: optimize.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++           effect: optimize parameters - high level routine
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: optimize.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:18  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:35  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE OPTIMIZE

C  Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/likrep.copy'
      INCLUDE '../COMMON/maprep.copy'
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/gasrep.copy'
      INCLUDE '../COMMON/emprep.copy'
      INCLUDE '../COMMON/bmprep.copy'

      save

C   ************ ARGUMENTS *****************
      character(80) id
      common /id/id
      logical CtoZ,GtoZ,BtoZ
      logical Opt_Corg,Opt_Gorg,Opt_Borg
      logical Opt_Corg2,Opt_Gorg2,Opt_Borg2
      logical OptC_done,OptG_done,OptB_done
      CHARACTER NOTE*70

      data NOTE/
     &'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'/
      id = '$Id: optimize.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='OPTIMIZE'


      if (CountT.lt.0.5) then
         Write(sigmsg,*)
     &        'OPTIMIZE:There are less than 0.5 counts within Ranal.'
         signal='0'
         return
      endif

c     initialize the model
      if (OptC) Counts=1.
      if (OptG) Gmult=Gmult_nom/100.
      if (OptB) Gbias=Gbias_nom/100.
      
 10   CALL OPTIMUM(CtoZ,GtoZ,BtoZ)

      if (signal.eq.' ') goto 100 !success
      if (signal.ne.'-') return

      if (Nopt.eq.1) then  
         signal='W'
         sigmsg='OPTIMIZE:Single parameter estimate <0'
         return
      endif

c     there is a problem which we can do something about
      signal=' '

c     save original opt
      Opt_Corg=OptC
      Opt_Gorg=OptG
      Opt_Borg=OptB
      Nopt_org=Nopt

c     reduce opt
      if (CtoZ) OptC=.false.
      if (GtoZ) OptG=.false.
      if (BtoZ) OptB=.false.

      if (verbose) then
         WRITE(6,'("VERBOSE(OPTIMIZE): Reduce opt to ",$)')
         if (OptC) WRITE(6,'("Counts ",$)')
         if (OptG) WRITE(6,'("Gmult ",$)')
         if (OptB) WRITE(6,'("Gbias ",$)')
         WRITE(6,'(".")')
      endif

c     set initial values
      if (OptC) then
         Counts=1.
      endif

      if (OptG) then
         Gmult=Gmult_nom/100.
      endif

      if (OptB) then
         Gbias=Gbias_nom/100.
      endif
      
      CALL OPTIMUM(CtoZ,GtoZ,BtoZ) !reduced once

 20   if (signal.eq.' ') then
         if (verbose) then
            WRITE(6,'("VERBOSE(OPTIMIZE): Reduced opt succeeded")')
         endif

c     restore original opt
         Counts_store=Counts
         Gmult_store=Gmult
         Gbias_store=Gbias
         OptC_done=OptC
         OptG_done=OptG
         OptB_done=OptB
         OptC=Opt_Corg
         OptG=Opt_Gorg
         OptB=Opt_Borg

         CALL OPTIMUM(CtoZ,GtoZ,BtoZ) !original opt
         if (signal.ne.' ') then
            if (verbose) then
               WRITE(6,'("VERBOSE(OPTIMIZE): Org opt failed again")')
            endif
c     restore clean values
            Counts=Counts_store
            Gmult=Gmult_store
            Gbias=Gbias_store
            signal=' '
            goto 80
         endif
         goto 100

      else
         if (verbose) then
            WRITE(6,'("VERBOSE(OPTIMIZE): Reduced opt failed")')
         endif

         if (signal.ne.'-') return
         if (Nopt.eq.1) then  
            signal='W'
            sigmsg='OPTIMIZE:Single parameter estimate <0'
            return
         endif

         signal=' '
c     reduce opt once more
         Opt_Corg2=OptC
         Opt_Gorg2=OptG
         Opt_Borg2=OptB
         if (CtoZ) OptC=.false.
         if (GtoZ) OptG=.false.
         if (BtoZ) OptB=.false.

         if (verbose) then
            WRITE(6,'("VERBOSE(OPTIMIZE): 2nd reduced opt: ",$)')
            if(OptC)WRITE(6,'("Counts ",$)')
            if(OptG)WRITE(6,'("Gmult ",$)')
            if(OptB)WRITE(6,'("Gbias ",$)')
            WRITE(6,'(".")')
         endif

c     set initial values
         if (OptC) then
            Counts=0.
         endif

         if (OptG) then
            Gmult=Gmult_nom/100.
         endif

         if (OptB) then
            Gbias=Gbias_nom/100.
         endif

         CALL OPTIMUM(CtoZ,GtoZ,BtoZ) !reduced twice

         if (signal.ne.' ') then
            if (verbose) then
               WRITE(6,'(
     &              "VERBOSE(OPTIMIZE): 2nd reduced opt failed")')
            endif

c     nothing else to do
            return

         else
            if (verbose) then
               WRITE(6,'(
     &              "VERBOSE(OPTIMIZE): 2nd reduced opt succeeded")')
            endif

c     restore once reduced opt
            Counts_store=Counts
            Gmult_store=Gmult
            Gbias_store=Gbias
            OptC_done=OptC
            OptG_done=OptG
            OptB_done=OptB
            OptC=Opt_Corg2
            OptG=Opt_Gorg2
            OptB=Opt_Borg2

            CALL OPTIMUM(CtoZ,GtoZ,BtoZ) !reduced once
            if (signal.eq.' ') then
               goto 20
            else

c     failed - restore clean values
               Counts=Counts_store
               Gmult=Gmult_store
               Gbias=Gbias_store
               signal=' '
            endif
         endif
      endif
      
 80   continue
      if (Opt_Gorg.and.Opt_Borg) then
         if (OptG_done.and.OptB_done) goto 90
         if ((.not.OptG_done).and.(.not.OptB_done)) goto 90

         if (verbose) then
            WRITE(6,'(
     &           "VERBOSE(OPTIMIZE): checking alternative opt")')
         endif

         CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         if (signal.ne.' ') return

         first_lnL=lnL
         OptC=OptC_done
         Counts_store=Counts
         Gmult_store=Gmult
         Gbias_store=Gbias

c     set up alternative opt
         if (OptG_done) then
            OptG=.false.
            Gmult=0.
            OptB=.true.
            Gbias=Gbias_nom/100.

         else
            OptG=.true.
            Gmult=Gmult_nom/100.
            OptB=.false.
            Gbias=0.
         endif

         CALL OPTIMUM(CtoZ,GtoZ,BtoZ)
         if (signal.ne.' ') then
c     failed - restore clean values
            signal=' '
            goto 85
         endif

         call HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
         if (signal.ne.' ') return

         if (lnL.lt.first_lnL) then
            if (verbose) then
               WRITE(6,'(
     &              "VERBOSE(OPTIMIZE): alternative opt is an ",
     *              "improvement by",e12.3," in lnL")')first_lnL-lnL
            endif

            if (first_lnL-lnL.gt.1.) then
               signal='S'
               sigmsg='OPTIMIZE:alternative opt is better'

               if (.not.report2) then
                  return

	       else
                  call error(0,loc)       
                  WRITE(6,*)NOTE
                  WRITE(6,*)
     &                 'NOTICE!!(OPTIMIZE): alternative opt is ',
     *                 'significantly improved !!!'
                  WRITE(6,*)
     &                 'first_lnL,lnL',first_lnL,lnL
                  WRITE(6,'("NOTICE(OPTIMIZE): alternative opt: ",$)')

                  if (OptC) WRITE(6,'("Counts ",$)')
                  if (OptG) WRITE(6,'("Gmult ",$)')
                  if (OptB) WRITE(6,'("Gbias ",$)')

                  WRITE(6,'(".")')
                  WRITE(6,*)
     &                 'THIS CONVERGENCE IS PROBABLY WRONG! ',
     *                 'Try modifying Gmult_nom'
                  WRITE(6,*)
     &                 '& Gbias_nom with the PMN command.',
     &                 ' If that does not prevent this from occurring,'
                  WRITE(6,*)
     &                 'please do PMV, then LL for this source, and ',
     *                 'mail the result to',
     &                 ' jae@egret.gsfc.nasa.gov.'
                  WRITE(6,*)NOTE
               endif

               OptG_done=OptG
               OptB_done=OptB
               goto 90

            else
               if (verbose) then
                  WRITE(6,'(
     &                 "VERBOSE(OPTIMIZE): alternative opt is worse by",
     &                 e12.3," in lnL")')lnL-first_lnL
               endif
            endif
         endif
      endif
      
 85   continue
      Counts=Counts_store
      Gmult=Gmult_store
      Gbias=Gbias_store
      lnL=first_lnL

 90   continue
      if (report) then
         write(6,'(
     &        "WARNING(optimize):",
     &        "Optimization requested for:",$)')

         if (Opt_Corg) WRITE(6,'(" Counts",$)')
         if (Opt_Gorg) WRITE(6,'(" Gmult",$)')
         if (Opt_Borg) WRITE(6,'(" Gbias",$)')

         WRITE(6,'(";")')
         write(6,'(
     &        "but acheived for reduced number of parameters:",$)')

         if (OptC_done) WRITE(6,'(" Counts",$)')
         if (OptG_done) WRITE(6,'(" Gmult",$)')
         if (OptB_done) WRITE(6,'(" Gbias",$)')

         WRITE(6,'(".")')
      endif

      OptC=Opt_Corg
      OptG=Opt_Gorg
      OptB=Opt_Borg
      Nopt=Nopt_org
 100  call HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)

      RETURN
      END
c
