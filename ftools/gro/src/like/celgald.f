c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c
C        SUBROUTINE CELGALD(cg_conv_typ,tmpaL,tmpaB,tmpL,tmpB,iiret)
C
C
C  $Id: celgald.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=============================================================================
C*      effect: interface conversions from degrees to radians for call to
C*              subroutine CELGAL.  Then converts CELGAL returned values
C*              from radians to degrees.			
C*                      
C=============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+              UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C* cg_conv   Char*2: Galactic to celestial='GC'; Celestial to Galactic='CG'
C* tmpaL     real RA in degrees
C* tmpaB     real DEC in Degrees
C* tmpL      real Longitude in degrees
C* tmpB      real Latitude in degrees
c=============================================================================
C  $Log: celgald.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  18:59:47  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:47:06  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:12  jae
c Subroutine Module for like V5.00
c
c
c
c=============================================================================
      SUBROUTINE CELGALD(cg_conv_typ,tmpaL,tmpaB,tmpL,tmpB,iiret)

      character(80) id
      common /id/id

      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save
c
      character(2) cg_conv_typ
c
      id = '$Id: celgald.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='CELGALD'

      if (jae_celgald)write(*,'("In routine ",a)') LOC
      
      conv2deg = 45./atan(1.)
      conv2rad = 1/conv2deg
      tmpaL = tmpaL*conv2rad
      tmpaB = tmpaB*conv2rad
      tmpL =  tmpL*conv2rad
      tmpB =  tmpB*conv2rad
c
      CALL CELGAL(cg_conv_typ,tmpaL,tmpaB,tmpL,tmpB,iiret)
c
      tmpaL = tmpaL*conv2deg
      tmpaB = tmpaB*conv2deg
      tmpL =  tmpL*conv2deg
      tmpB =  tmpB*conv2deg

      return
      end
c
