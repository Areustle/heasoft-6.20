      SUBROUTINE xmatch(string,array,narray,iret,*)

      CHARACTER*(*) string, array(*)
      INTEGER*4 iret, narray

c            rashafer 17 oct 1985
c      XPARSE subroutine to test if a string matches one of the strings
c      contained in an array of strings
c      string      c*      I: test string
c      array      c*(narray) I: array of strins to be matched against
c      narray      i4      I: no of eelements in the array
c      iret      i4      r: if > 0 then a match was fournd with element iret
c                     if = 0 no match was found
c                     if < 0 then a non-unique match was found
c                     where -IRET is the first such none unique match AND
c                     the QXPFIR flag was not set
c      Alternate returns:
c            1 -      IRET <= 0

      INCLUDE 'xparinc.inc'
      INTEGER*4 lenact
      INTEGER*4 lenst, iarray
      LOGICAL*4 qpart, xqmtch

      iret=0
      lenst=lenact(string)

c  Loop round test strings

      DO iarray = 1, narray

c  Check for a match. If qxpart (see xparinc.inc) is set true
c  then a partial match is allowed and qpart may be returned
c  true

         IF (xqmtch(string(:lenst),array(iarray),qpart)) THEN

c  If no multiple matches or the current match is exact then
c  this is it so return

            IF(qxpfir .OR. (.NOT.qpart) ) THEN
               iret=iarray
               RETURN
            ENDIF

c  Otherwise if iret is zero then set it and condition

            IF (iret .EQ. 0) THEN
               iret=iarray

            ELSE

c  but if iret is non-zero then this is a multiple match
c  so set iret negative

               iret = -iret

            ENDIF
         ENDIF
      ENDDO

      IF (iret .LE. 0) RETURN 1

      END
