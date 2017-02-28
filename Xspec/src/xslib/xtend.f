      SUBROUTINE XTEND(CNAM, CEXT)
      CHARACTER CNAM*(*), CEXT*(*)
C---
C If CEXT contains a dot then this routine will force CEXT to be the
C extension.  If CEXT does not contain a dot then then CEXT will be
C appended only if CNAM does not already have an extension.  For
C example, if CNAM='FUN' and CEXT='DAT' then on output CNAM='FUN.DAT'.
C---
C CNAM    I/O  The file name.
C CEXT      O  The extension.
C---
C 1989-Feb-13 - Don't XTEND zero length file names [AFT]
C---
      INTEGER   LENACT
C
      CHARACTER(10) CTMP
      INTEGER   IDOT, IEND, ISTA, LEXT, LNAM
C---
      LNAM=LENACT(CNAM)
      IF(LNAM.EQ.0) RETURN
      LEXT=LENACT(CEXT)
      CTMP=CEXT
      CALL CONC(CTMP)
C---
C Find first dot after the directory
      CALL DIRPOS(CNAM, ISTA, IEND)
      IDOT=IEND+INDEX(CNAM(IEND+1:),'.')
C Exit if file name contains an extension, and requested extension is
C not forcing.
      IF(IDOT.GT.IEND .AND. CTMP(1:1).NE.'.') RETURN
      IF(IDOT.EQ.IEND) THEN
         IDOT=LNAM+1
         CNAM(IDOT:IDOT)='.'
      END IF
      IEND=LEN(CNAM)-LNAM
      IF(CTMP(1:1).EQ.'.') THEN
         CNAM(IDOT+1:)=CTMP(2:LEXT)
         IEND=IEND-LEXT+1
      ELSE IF(CTMP(1:1).NE.'.') THEN
         CNAM(IDOT+1:)=CTMP(:LEXT)
         IEND=IEND-LEXT
      END IF
      IF(IEND.LE.0) THEN
         WRITE(*,*) 'XTEND--Programmer error.  Filename of ',CNAM
         WRITE(*,*) 'XTEND--was not properly extended.'
      END IF
      RETURN
      END
