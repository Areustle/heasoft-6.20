      SUBROUTINE rst2sf(iunit,str,ierrsf)

      CHARACTER str*(*)
      INTEGER iunit,ierrsf

c      rst2sf            rashafer 16 March 85
c            SF subroutine to write an array of character strings as
c            auxilary records to the current SF package.  N.B.  If
c            the package header shows an indeterminate no. of records
c            then good practice is for the user to add a terminating record.
c            (one with length 0).
c      modified from WSTRSF - Now READs a set of character strings.
c      modified from RSTRSF - Reads only a single string.
c
c      iunit      i4            i: read unit
c      str      c*            r: String read from subsidary record.
c      ierrsf      i4            i/r: SF error flag
c                  17 -      io read error
c                  18 -      EOF condition before required records read.
c                  19 -      No record in subsidiary field.
c                  20 -       Unable to reposition after a badly terminated
c                        indefinite no. of auxilary records.
c                  21 -    Iinsufficient length to contain info

      LOGICAL qwerr
      INTEGER lens, ios, lc
      CHARACTER wrtstr*255

      qwerr=ierrsf.eq.0
      ierrsf=0
      lens=len(str)

      READ (iunit,iostat=ios) lc, str(:min(lc,lens))

      IF ( ios .NE. 0 ) THEN
         IF ( ios .LT. 0 ) THEN
c **EOF condition
            IF (qwerr) CALL xerror('RST2SF: EOF while reading record',2)
            ierrsf=18
         ELSE
            IF (qwerr) THEN
               WRITE(wrtstr,'(''RST2SF: Read i/o error '',i4)') ios
               CALL xerror(wrtstr, 2)
            ENDIF
            ierrsf=17
         ENDIF
      ENDIF

      IF ( lc .LT. 0 ) THEN

c ** badly terminated subsidiary info portion.  Set lc to
c ** zero allows the blanking out of str.

         lc = 0
         IF (qwerr) CALL xerror(
     & 'RST2SF: Badly terminated auxilary package', 2)
         ierrsf=19
         BACKSPACE(iunit, iostat=ios)
         IF ( ios .NE. 0 ) THEN
            IF (qwerr) CALL xerror(
     &  'RST2SF: Unable to reposition to package header', 2)
            ierrsf=20
         ENDIF
      ELSEIF ( lc .EQ. 0 ) THEN
         ierrsf=19
         IF (qwerr) CALL xerror(
     &  ' RST2SF: End of package while reading in string ', 2)
      ELSEIF ( lens .LT. lc ) THEN
         IF (qwerr) THEN
            WRITE(wrtstr, '(
     &  ''RST2SF: Buffer too small,'', i5,'' required '',i5,
     &  '' some information lost'')') lens, lc
            CALL xerror(wrtstr, 2)
         ENDIF
         ierrsf=21
      ENDIF

      IF ( lc+1 .LE. lens ) str(lc+1:)=' '

      RETURN
      END
