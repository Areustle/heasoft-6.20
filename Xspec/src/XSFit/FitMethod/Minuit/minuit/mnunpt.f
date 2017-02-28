*
* $Id: mnunpt.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mnunpt.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:30  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:18  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:57  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
 
      LOGICAL FUNCTION MNUNPT(CFNAME)
C           is .TRUE. if CFNAME contains unprintable characters.
      CHARACTER CFNAME*(*)
      CHARACTER CPT*80, CP1*40,CP2*40
      PARAMETER (CP1=' ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm')
      PARAMETER (CP2='nopqrstuvwxyz1234567890./;:[]$%*_!@#&+()')
      CPT=CP1//CP2
      MNUNPT = .FALSE.
      L = LEN(CFNAME)
      DO 100 I= 1, L
         DO 50 IC= 1, 80
         IF (CFNAME(I:I) .EQ. CPT(IC:IC))  GO TO 100
   50    CONTINUE
      MNUNPT = .TRUE.
      GO TO 150
  100 CONTINUE
  150 CONTINUE
      RETURN
      END
