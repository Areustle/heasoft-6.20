*
* $Id: mninpu.f,v 1.1.1.1 2003/11/25 22:45:45 dorman Exp $
*
* $Log: mninpu.f,v $
* Revision 1.1.1.1  2003/11/25 22:45:45  dorman
* Xspec12.0 Export Version 11-25-2003
*
* Revision 7.0  2002/07/12 16:03:22  dorman
*
* version number change
*
* Revision 1.1  2002/06/26 14:52:11  dorman
*
*
* minuit library
*
* Revision 1.1  2001/12/28 02:53:55  kaa
* Added MINUIT source code.
*
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
 
      SUBROUTINE MNINPU(IUNIT,IERR)
      INCLUDE "d506dp.inc"
CC      called by the user to SET INPUT to IUNIT,
CC      an alternative to MNSTIN where the user can specify just
CC      a logical unit number and he is not interrogated about
CC      open files and rewinding, all that is the responsibility
CC      of the user and cannot be fixed interactively.
      INCLUDE "d506cm.inc"
C
      IERR = 0
C                              IUNIT = 0, revert to previous input file
      IF (IUNIT .EQ. 0) THEN
        IF (NSTKRD .EQ. 0)  THEN
           WRITE (ISYSWR, '(A)') ' CALL TO MNINPU(0) IGNORED'
           WRITE (ISYSWR, '(A)') ' ALREADY READING FROM PRIMARY INPUT'
        ELSE
          ISYSRD = ISTKRD(NSTKRD)
          NSTKRD = NSTKRD - 1
        ENDIF
C
C                               new input file
      ELSE
          IF (NSTKRD .GE. MAXSTK)  THEN
          WRITE (ISYSWR, '(A)') ' INPUT FILE STACK SIZE EXCEEDED.'
          GO TO 800
          ENDIF
        NSTKRD = NSTKRD + 1
        ISTKRD(NSTKRD) = ISYSRD
        ISYSRD = IUNIT
      ENDIF
C
      RETURN
  800 IERR = 1
      RETURN
      END
