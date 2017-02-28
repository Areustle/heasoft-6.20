c
c      SUBROUTINE JFIND_ILOC(MAP1,JsrcL,JsrcB,IRRL,IRRB,
c     & ntmpc,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: jfind_il.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++   Effect: 	Obtain Integer 4 point boundaries (verticies) of IsrcL 
c 		and IsrcB for use by speedmap option in MAPSRC(J)
c
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     Real	MAP	contains boundary values of TS
c     Integer   IsrcL	Integer associated with Long(RA)
c     Integer   IsrcB	Integer associated with Lat(Dec)
c     Integer   IRRL	2x2 array which receives integer 
c			Long(RA) boundary values
c     Integer   IRRB	2x2 array which receives integer 
c			Lat(Dec) boundary values
c     Integer	ntmpc	integer modulus for boundary calculations
c     Integer	CTLMSZ1	Number of axis1 bins in CTL fits header
c     Integer	CTLMSZ2	Number of axis2 bins in CTL fits header
c
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J. A. ESPOSITO
C+            Updated:    by  JAE
c
c
C=======================================================================
C  $Log: jfind_il.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:33  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  20:22:16  jae
c added lines for LOC
c
c Revision 5.1  1996/02/29  20:48:14  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:09  jae
c Subroutine Module for like V5.00
c
C%   Changes:        
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE JFIND_ILOC(MAP1,JsrcL,JsrcB,IRRL,IRRB,
     &     ntmpc,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'

      save

      character(80) id
      common /id/id
      REAL MAP1(IMAPWIDTH,IMAPHEIGHT)
      INTEGER IRRL(2,2),IRRB(2,2)
c
      id = '$Id: jfind_il.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='JFIND_ILOC'

      if (jae_jfind_iloc)write(*,'("In routine ",a)') LOC
c
c     Loop over range 2*ntmpc.  The positions MUST lie in this range.
c     we only need to get min and max (2) values for each axis
      JL = 0
      JB = 0
      do jj=1,2
	 IRRL(jj,jj)=-1
	 IRRB(jj,jj)=-1
      enddo
      do jj = -ntmpc,ntmpc
	 if (JL.ge.2.and.JB.ge.2)goto 21
	 Itmpl = JsrcL + jj
	 ItmpB = JsrcB + jj
	 if (JL.lt.2) then
            if (Itmpl.gt.ROIPND(1).or.ItmpL.lt.ROIPRG(1))goto 10
            if (MOD(ItmpL-ROIPRG(1),ntmpc).ne.0.and.
     &           JsrcL.ne.1.and.JsrcL.ne.ROIPND(1))goto 10
            JL=JL+1
            IRRL(JL,JL)=ItmpL
	 endif
 10      if (JB.lt.2) then
            if (ItmpB.gt.ROIPND(2).or.ItmpB.lt.ROIPRG(2))goto 20
            if (MOD(ItmpB-ROIPRG(2),ntmpc).ne.0.and.
     &           JsrcB.ne.1.and.JsrcB.ne.ROIPND(2))goto 20
            JB=JB+1
            IRRB(JB,JB)=ItmpB
	 endif
 20   enddo
 21   continue
      IRRL(1,2)=IRRL(1,1)
      IRRL(2,1)=IRRL(2,2)
      IRRB(1,2)=IRRB(2,2)
      IRRB(2,1)=IRRB(1,1)
c
c     Check for negative IRRL or IRRB -- AN ERROR CONDITION CHECK
c     
      do jjk=1,2
	 do jjl=1,2
            if (IRRL(jjk,jjl).lt.0.or.IRRB(jjk,jjl).lt.0) then
               if (jae_jfind_iloc) then
                  print *,' '
                  print *,'JSRCL: ',JsrcL,' JSRCB: ',JSRCB
                  print *,'IRRL: ',IRRL
                  print *,'IRRB: ',IRRB
                  CALL MAPCOR(JsrcL,JsrcB,srcLcnt,srcBcnt)
                  print *,' l,b: ',srcLcnt,',',srcBcnt
                  print *,' '
               endif
               signal='N'
               return
            endif
            vtmp = MAPVAL(MAP1,IRRL(jjk,jjl),IRRB(jjk,jjl),
     &           IMAPWIDTH,IMAPHEIGHT)
c     
c     Check for negative vtmp -- AN ERROR CONDITION CHECK
c     
            if (vtmp.lt.-0.5e-10) then
               if (jae_jfind_iloc) then
                  print *,' '
                  print *,'JSRCL: ',JsrcL,' JSRCB: ',JSRCB
                  print *,'IRRL: ',IRRL
                  print *,'IRRB: ',IRRB
                  CALL MAPCOR(JsrcL,JsrcB,srcLcnt,srcBcnt)
                  print *,' l,b: ',srcLcnt,',',srcBcnt
                  print *,'vtmp: ',vtmp
                  print *,' '
               endif
               signal='L'
               return
            endif
	 enddo
      enddo
      signal=' '

      return
      end
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
