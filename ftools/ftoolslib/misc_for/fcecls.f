C******************************************************************************
C SUBROUTINE:
C      fcecls
C
C DESCRIPTION:
C      expand the list of column specifiers by translating column numbers
C      to their corresponding column names and finding all column names that
C      match any wilcard characters.  This routine takes no action when column
C      specifiers do not match any column names.
C
C AUTHOR/DATE:
C      toliver, 04/02/1999 - original version
C
C MODIFICATION HISTORY:
C      James Peachey, 09/11/2000
C      Argument list changed to use an input case sensitivity flag rather
C      than simply ".FALSE.", and an output array which contains
C      the number of each matching column. (=0 if no matching column).
C
C NOTES:
C
C USAGE:
C      CALL fcecls (iunit, casesen, incolist, outcolist,
C     &             outcolpos, numcols, maxcols)
C
C ARGUMENTS:
C      iunit - FITS file logical unit number
C      incolist  - array of column specifiers (names, numbers, wildcards)
C      casesen - boolean value to indicate whether or not to ignore case
C      outcolist - array of column specifiers translated into column names
C      outcolpos - array of column numbers
C      numcols - number of column names in list (on entry is number in input
C                list, on exit is number in output list)
C      maxcols - maximum number of items allowed in column lists
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      subroutine ftgcnn (cfitsio) - get column name(s) and number(s) which
C                                    match input template
C
C******************************************************************************

      SUBROUTINE fcecls (iunit, casesen, incolist, outcolist,
     &                   outcolpos, numcols, maxcols)

      INTEGER iunit, maxcols, numcols
      CHARACTER*(*) incolist (maxcols), outcolist (maxcols)
      INTEGER outcolpos(maxcols)
      LOGICAL casesen

      character(160) colname
      INTEGER revnumcols, colnum, status, SUCCESS, COL_NOT_UNIQUE
      PARAMETER (SUCCESS = 0)
      PARAMETER (COL_NOT_UNIQUE = 237)

      revnumcols = 0
      status = 0

C
C     Loop over all column specifiers in input list
C
      DO i = 1, numcols

C
C        Get the column name corresponding to the given column specifier
C
         CALL ftgcnn (iunit, casesen, incolist (i),
     &                colname, colnum, status)

C
C        If a match was found, add this name to the output list
C
         IF (status .EQ. SUCCESS) THEN
            revnumcols = revnumcols + 1
            outcolist (revnumcols) = colname
            outcolpos (revnumcols) = colnum

C
C        If the column specifier was not unique, then continue to call
C           the cfitsio routine, accumulating column names, until no
C           additional matches are found
C
         ELSE IF (status .EQ. COL_NOT_UNIQUE) THEN
            DO WHILE (status .EQ. COL_NOT_UNIQUE)
               revnumcols = revnumcols + 1
               outcolist (revnumcols) = colname
               outcolpos (revnumcols) = colnum
               CALL ftgcnn (iunit, casesen, incolist (i),
     &                      colname, colnum, status)
            ENDDO

C
C        Any other returned status implies that the column specifier does
C           not match any of the column names in the extension.  In this
C           case, simply transfer the input column specifier to the output
C           list.
C
         ELSE
            revnumcols = revnumcols + 1
            outcolist (revnumcols) = incolist (i)
            outcolpos (revnumcols) = 0
         ENDIF
      ENDDO

C
C     Update the count of column names
C
      numcols = revnumcols

      RETURN
      END
