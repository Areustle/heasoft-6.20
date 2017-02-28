C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE PSFADD(AMP,map,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: psfadd.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++            effect: An analytical PSF is added to a map,
C++                    centered at the coordinates srcL,srcB, with 
C++                    multiplier AMP. 
C***********************************************************************
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     REAL   AMP  number of counts
c     REAL   map  matrix to which PSF is to be added
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:   by  JRM
C=======================================================================
C  $Log: psfadd.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/06/05  03:39:02  jae
c Changed routine to return if abs(amp) < 1.e-4
c
c Revision 5.1  1996/02/29  20:52:27  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:44  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE PSFADD(AMP,map,IMAPWIDTH,IMAPHEIGHT)

C  Common blocks used:
      INCLUDE   '../COMMON/psfrep.copy'
      INCLUDE   '../COMMON/psmrep.copy'
      INCLUDE   '../COMMON/cnfrep.copy'
      INCLUDE   '../COMMON/ctlrep.copy'
      INCLUDE   '../COMMON/errrep.copy'
      INCLUDE   '../COMMON/likrep.copy'

      save

      character(80) id
      common /id/id
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
C
      REAL AMP
C     The source strength, multiplies the PSF.

C     Local Variables:
      REAL L,B
C     Current pixel coordinates

      id = '$Id: psfadd.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='PSFADD  '


C     Add  the PSF to the map:
      if (abs(amp).lt.1.e-4) return

      DO 10 J=1,PSFSZ
         DO 10 I=1,PSFSZ
C     Find relative coordinates at centre of pixel I,J:
            CALL PSMCOR(I,J,L,B)
c     map indecies here are:
            CALL MAPIXL(srcLpxcnt+L,srcBpxcnt+B,M,N)
            IF (SIGNAL.NE.' ') then
               IF (SIGNAL.EQ.'O') then
c     point OUTSIDE MAP
                  SIGNAL=' '
               else
                  CALL ERROR(1,LOC)
               ENDIF
            else
C     Add the signal to the MAP
               MAP(M,N) =  MAP(M,N) + PSF(I,J)*AMP
            ENDIF
 10      CONTINUE
C
C     Note the total added to the map, for information:
c     WRITE(LU(1),'('' PSFADD: '',E13.7,'' requested;'')') AMP
c     WRITE(LU(1),'(E13.7,'' actually added to map.'')') TOTAL
         
C     
         SIGNAL=' '

         RETURN
         END
c
