c        subroutine derivative2
c
c
C  $Id: derivati.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++	effect: Use 2nd derivatives to obtain parameter uncertainties.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
C Installation of RCS lines: 23 AUG 1995 by JAE
C=======================================================================
C  $Log: derivati.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:29  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  20:08:04  jae
c added lines for LOC
c
c Revision 5.1  1996/02/29  20:47:23  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:40  jae
c Subroutine Module for like V5.00
c
C%   Changes:                                                           
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine derivative2

C     Common block included
      INCLUDE '../COMMON/likrep.copy'
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/emprep.copy'

      save

      character(80) id
      common /id/id
      REAL score(3),information(3,3)


c     determine Nopt
      id = '$Id: derivati.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='DERIVATIVE2'

      if (jae_derivative2)write(*,'("In routine ",a)') LOC

      Nopt=0
      if (OptC)Nopt=Nopt+1
      if (OptB)Nopt=Nopt+1
      if (OptG)Nopt=Nopt+1
      
      call LHOOD(map,bmap,gasmap,emap,score,information
     &     ,CTLMSZ1,CTLMSZ2)

      call invert(information)  ! to form formation
      if (signal.eq.'S') then
         WRITE(sigmsg,'("derivat: Estimate:C=",e9.2,"Gm=",e9.2,"Gb=",
     *        e9.2," causes sing. info. matrix")')Counts,Gmult,Gbias
         return
      endif

      if (verbose) then
         WRITE(6,'(
     &        "VERBOSE: Covariance matrix for",$)')
         if (OptC) WRITE(6,'(" Counts ",$)')
         if (OptG) WRITE(6,'(" Gmult ",$)')
         if (OptB) WRITE(6,'(" Gbias ",$)')
         WRITE(6,'(" is:")')
         do i=1,Nopt
            WRITE(6,'(3e13.4)')(information(i,j),j=1,Nopt)
         enddo
      endif

      if (Nopt.eq.3) then
         if (information(1,1).ge.0.) then
	    dCounts=sqrt(information(1,1))
         else
	    print *,'imaginary dCounts'
	    dCounts=0.
         endif
         if (information(2,2).gt.0.) then
	    dGmult=sqrt(information(2,2))
         else
	    print *,'imaginary dGmult'
	    dGmult=0.
         endif
         if (information(3,3).gt.0.) then
	    dGbias=sqrt(information(3,3))
         else
	    print *,'imaginary dGbias'
	    dGbias=0.
         endif
         if (report) then
            print *,'Correlation coefficient for Counts/Gmult:',
     &           information(1,2)/dGmult/dCounts
            print *,'Correlation coefficient for Counts/Gbias:',
     &           information(1,3)/dCounts/dGbias
            print *,'Correlation coefficient for Gmult/Gbias:',
     &           information(2,3)/dGmult/dGbias
         endif

      elseif (Nopt.eq.2) then
         if (.not.OptC) then
	    if (information(1,1).gt.0.) then
               dGmult=sqrt(information(1,1))
            else
               dGmult=0.
               print *,'imaginary dGmult'
	    endif
	    if (information(2,2).gt.0.) then
               dGbias=sqrt(information(2,2))
            else
               dGbias=0.
               print *,'imaginary dGbias'
	    endif
            if (report) then
               print *,'Correlation coefficient for Gmult/Gbias:',
     &              information(1,2)/dGmult/dGbias
            endif

         elseif (.not.OptB) then
	    if (information(1,1).gt.0.) then
               dCounts=sqrt(information(1,1))
            else
               print *,'imaginary dCounts'
               dCounts=0.
	    endif
	    if (information(2,2).gt.0.) then
               dGmult=sqrt(information(2,2))
            else
               print *,'imaginary dGmult'
               dGmult=0.
            endif
            if (report) then
               print *,'Correlation coefficient for Counts/Gmult:',
     &              information(1,2)/dGmult/dCounts
            endif

         else
	    if (information(1,1).gt.0.) then
               dCounts=sqrt(information(1,1))
            else
               print *,'imaginary dCounts'
               dCounts=0.
	    endif
	    if (information(2,2).gt.0.) then
               dGbias=sqrt(information(2,2))
            else
               dGbias=0.
               print *,'imaginary dGbias'
	    endif
            if (report) then
               print *,'Correlation coefficient for Counts/Gbias:',
     &              information(1,2)/dGbias/dCounts
            endif
         endif

      else
         if (OptC) then
	    if (information(1,1).gt.0.) then
               dCounts=sqrt(information(1,1))
            else
               dCounts=0.
               print *,'imaginary dCounts'
	    endif
         elseif (OptB) then
	    if (information(1,1).gt.0.) then
               dGbias=sqrt(information(1,1))
            else
               print *,'imaginary dGbias'
               dGbias=0.
	    endif

         else
	    if (information(1,1).gt.0.) then
               dGmult=sqrt(information(1,1))
            else
               print *,'imaginary dGmult'
               dGmult=0.
	    endif
         endif
      endif
      
      RETURN
      END
