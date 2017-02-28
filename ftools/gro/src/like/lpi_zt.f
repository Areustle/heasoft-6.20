C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE lpi_zt(ztinput)
C
C
C  $Id: lpi_zt.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C======================================================================
C*     Effect: Upon user request the aspect cone half angle, minimum
C*             allowed TS nad looking direction can be set to a new 
C*             value or, in the case of the looking direction, reset
C*             to the map center or pointing direction for sum maps
C*             or single viewing periods respectively.
C*             
c
c----------------------------------------------------------------------
c     	Subroutine Argument Desriptions
c   	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c 	character(50) 	INPUT      	'Z ', 'T ', 'ZP', 'ZR'
c
c-----------------------------------------------------------------------
C LIKE Version: 5.0 DELIVERED: November 1st 1994, Programmer J.A. ESPOSITO
C+             UPDATED:    by  JAE	
c
C=======================================================================
C  $Log: lpi_zt.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 1.3  1997/04/07  03:49:10  jae
c Renamed local variables ending in "input" with
c a leading "z" to offset possible problems with
c common block files
c
c Revision 1.2  1997/01/31  16:22:54  jae
c Fixed error in IF-THEN-ELSE-ENDIF at line 64.  Added
c an ENDIF to the statement, realigned the code which
c repaired cannot reach line 74.
c
c Revision 1.1  1996/11/13  21:18:10  jae
c Initial revision
c
c-------------------------------------------------------------------------
c
      SUBROUTINE LPI_ZT(ztinput)

c     Common blocks used:
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
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save
c
      character(80) id
      common /id/id
      character zsinput*1,zsinput2*2,ztinput*20,zainput*20
      logical FEXIST

      id = '$Id: lpi_zt.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='LPI_ZT'

      zsinput=ztinput(1:1)
      zsinput2=ztinput(1:2)
	 
	 
c     input='        '
      print *,' '
      if(zsinput2.eq.'ZC')then
         pL=(ROIEND(1)+ROIORG(1))/2.0
         pB=(ROIEND(2)+ROIORG(2))/2.0
         if(coord_sys.eq.'G')then
            SC_LJJ=pL
            SC_BJJ=pB		
            CALL CELGALD('GC',SC_RAJ,SC_DECJ,pL,pB,iiret)
         else
            SC_RAJ = pL
            SC_DECJ = pB
            CALL CELGALD('CG',pL,pB,SC_LJJ,SC_BJJ,iiret)
         endif
         return
      endif
      if(zsinput2.eq.'ZR')then
         if(MAPTYPE.eq.'SINGLE_POINTING')then
            if(coord_sys.eq.'G')then
               SC_LJJ = SC_LII
               SC_BJJ = SC_BII
               tmpL=SC_LII
               tmpB=SC_BII
               CALL CELGALD('GC',SC_RAJ,SC_DECJ,tmpL,tmpB,iiret)
            else
               SC_RAJ = SC_RA
               SC_DECJ = SC_DEC
               tmpL = SC_RA
               tmpB = SC_DEC
               CALL CELGALD('CG',tmpL,tmpB,SC_LJJ,SC_BJJ,iiret)
            endif
            return
c	   
         else
            pL=(CTLEND(1)+CTLORG(1))/2.0
            pB=(CTLEND(2)+CTLORG(2))/2.0
            if(coord_sys.eq.'G')then
               SC_LJJ=pL
               SC_BJJ=pB		
               CALL CELGALD('GC',SC_RAJ,SC_DECJ,pL,pB,iiret)
            else
               SC_RAJ = pL
               SC_DECJ = pB
               CALL CELGALD('CG',pL,pB,SC_LJJ,SC_BJJ,iiret)
            endif
            return
         endif
      endif
      if(zsinput2.eq.'ZP')then
         if(coord_sys.eq.'G')then
            pL=SC_LJJ
            pB=SC_BJJ
         else
            pL=SC_RAJ
            pB=SC_DECJ
         endif
 591     print *,' Present pointing direction:',pL,',',pB
         if(coord_sys.eq.'G')then
            write(*,
     &           '("Enter (L,B) or <cr> to accept present values:"$)')
         else
            write(*,
     &           '("Enter (RA,DEC) or <cr> to ",
     *           "accept present values:"$)')
         endif
         read(LU(12),'(a)') zainput
         numcar = index(zainput, ' ') - 1
         if(numcar.eq.0)return
         read(zainput,*,err=591,end=591)pL,pB
         if(coord_sys.eq.'G')then
            SC_LJJ=pL
            SC_BJJ=pB
            CALL CELGALD('GC',SC_RAJ,SC_DECJ,pL,pB,iiret)
         else
            SC_RAJ = pL
            SC_DECJ = pB
            CALL CELGALD('CG',pL,pB,SC_LJJ,SC_BJJ,iiret)
         endif
         return
      endif
 6    if(zsinput.eq.'Z')then 
         write(*,*)' '
         write(*,'("Enter the aspect cone half angle (cr for "$)')
         write(*,'(f6.2,", A to abort): "$)')aspj
         read(LU(12),'(a)') zainput
         numcar = index(zainput, ' ') - 1
         if(numcar.eq.0)return
         if(zainput(1:1).eq.'A'.or.zainput(1:1).eq.'a')return
         read(zainput,*,end=6,err=6)aspj
         svaspj1=aspj
         return
      else
         write(*,*)' '
         write(*,'("Enter the new minimum TS value (cr for "$)')
         write(*,'(f6.2,", A to abort): "$)')psfminj
         read(LU(12),'(a)') zainput
         numcar = index(zainput, ' ') - 1
         if(numcar.eq.0)return
         if(zainput(1:1).eq.'A'.or.zainput(1:1).eq.'a')return
         read(zainput,*,end=6,err=6)psfminj
         if(psfminj.lt.0.1)then
            if(psfminj.gt.0.001)then
               write(*,'("Are you SURE you want: ",F8.3)')psfminj
            else
               write(*,'("The value: ",f8.3," IS TOO SMALL",/)')
     &              psfminj
               goto 6
            endif
            call intent(FEXIST)
            if(.not.FEXIST)goto 6
         endif
         svpsfminj1=psfminj
      endif
c     input='        '
      return
      end
