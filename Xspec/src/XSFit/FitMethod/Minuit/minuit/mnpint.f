*
* $Id: mnpint.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mnpint.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:26  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:14  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:56  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
 
      SUBROUTINE MNPINT(PEXTI,I,PINTI)
      INCLUDE "d506dp.inc"
CC        Calculates the internal parameter value PINTI corresponding
CC        to the external value PEXTI for parameter I.
CC
      INCLUDE "d506cm.inc"
      CHARACTER CHBUFI*4, CHBUF2*30
      PINTI = PEXTI
      IGO = NVARL(I)
      IF (IGO .EQ. 4)  THEN
C--                          there are two limits
        ALIMI = ALIM(I)
        BLIMI = BLIM(I)
        YY=2.0*(PEXTI-ALIMI)/(BLIMI-ALIMI) - 1.0
        YY2 = YY**2
        IF (YY2 .GE. (1.0- EPSMA2))  THEN
           IF (YY .LT. 0.) THEN
               A = VLIMLO
               CHBUF2 = ' IS AT ITS LOWER ALLOWED LIMIT.'
           ELSE
               A = VLIMHI
               CHBUF2 = ' IS AT ITS UPPER ALLOWED LIMIT.'
           ENDIF
           PINTI = A
           PEXTI = ALIMI + 0.5* (BLIMI-ALIMI) *(SIN(A) +1.0)
           LIMSET = .TRUE.
           WRITE (CHBUFI,'(I4)') I
           IF (YY2 .GT. 1.0) CHBUF2 = ' BROUGHT BACK INSIDE LIMITS.'
           CALL MNWARN('W',CFROM,'VARIABLE'//CHBUFI//CHBUF2)
         ELSE
           PINTI = ASIN(YY)
         ENDIF
      ENDIF
      RETURN
      END