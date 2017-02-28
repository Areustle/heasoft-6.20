**==DECOMP.spg  processed by SPAG 3.09I  at 09:41 on 20 Aug 1992
*- decompose string into fields separated by some character
      SUBROUTINE DECOMP(String,Length,Separator,Nf,Field_start,
     &                  Field_length,Status)
 
      INCLUDE 'status.codes'
* Import :
*  string - input string
      CHARACTER*(*) String
*  length - string length
      INTEGER Length
*  separator - separator character
      character(1) Separator
* Export :
*  nf - number of fields in string
      INTEGER Nf
*  field_start(jf) - index of first character of field
      INTEGER Field_start(*)
*  field_end(jf) - index of final character of field not including separators
      INTEGER Field_length(*)
* Status :
      INTEGER Status
 
* Local constants :
      character(1) DOUBLE_QUOTES
      PARAMETER (DOUBLE_QUOTES='"')
      character(1) NULL
C      PARAMETER (NULL=CHAR(0))
 
* Local variables :
      character(256) mask
      INTEGER i , i1 , i2
*-
      NULL=CHAR(0)
      IF ( Status.NE.OK__ ) RETURN
 
      mask = ' '
      i2 = 0
      i1 = INDEX(String(i2+1:),DOUBLE_QUOTES) + i2
      i2 = INDEX(String(i1+1:),DOUBLE_QUOTES) + i1
      DO WHILE ( i2.GT.i1 )
         DO 50 i = i1 + 1 , i2 - 1
            IF ( String(i:i).EQ.Separator ) mask(i:i) = NULL
 50      CONTINUE
         i1 = INDEX(String(i2+1:),DOUBLE_QUOTES) + i2
         i2 = INDEX(String(i1+1:),DOUBLE_QUOTES) + i1
      ENDDO
 
      Nf = 0
      DO 100 i = 1 , Length
         IF ( (String(i:i).EQ.Separator) .AND. (mask(i:i).NE.NULL) )
     &        THEN
            Nf = Nf + 1
            IF ( Nf.EQ.1 ) THEN
               Field_length(Nf) = i - 1
            ELSE
               Field_length(Nf) = i - 1 - Field_start(Nf-1)
     &                            - Field_length(Nf-1)
            ENDIF
            Field_start(Nf) = i - Field_length(Nf)
         ELSEIF ( i.EQ.Length ) THEN
            Nf = Nf + 1
            IF ( Nf.GT.1 ) THEN
               Field_length(Nf) = i - Field_start(Nf-1)
     &                            - Field_length(Nf-1)
               Field_start(Nf) = i + 1 - Field_length(Nf)
            ELSE
               Field_length(Nf) = Length
               Field_start(Nf) = 1
            ENDIF
         ENDIF
 100  CONTINUE
 
      RETURN
 
      END
