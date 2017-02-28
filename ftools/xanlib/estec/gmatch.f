**==GMATCH.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- get_match - verify answers
      SUBROUTINE GMATCH(List,N,Value,Index,Status)
* Description :
*  checks if a string appears in a list
* History :
*  1 July 1988 : original
*  8 November 1988 : put index=0 for blank input value
*  17 November 1988 : remove lower case conversion
*  2 February 1989 : check for exact and abbreviated match
*  2 March 1989 : status=error__ changed to warning__ for ambiguity
*  25 June 1991 : renamed to gmatch
*  8  July 1991 : set index = 0 if no match found
* Author :
*  Andy Pollock (EXOSAT::ANDY)
 
      INCLUDE 'status.codes'
* Import :
      CHARACTER*(*) List(*)
      INTEGER N
      CHARACTER*(*) Value
* Export :
      INTEGER Index
* Status :
      INTEGER Status
* Local variable :
      character(80) x , y , NOSTAR
      INTEGER lx
      INTEGER nin
      INTEGER j
* External reference :
      INTEGER LENACT
*-
      Status = 0
      x = Value
* find the non-blank length of the string
      lx = LENACT(x)
      IF ( lx.EQ.0 ) THEN
         Index = 0
      ELSE
         nin = 0
* check first for a exact match or exact match up to '*'
         DO 50 j = 1 , N
            y = NOSTAR(List(j))
            IF ( (y.EQ.x) .OR. 
     &           ((List(j)(:lx).EQ.x(:lx)) .AND. List(j)(lx+1:lx+1)
     &           .EQ.'*') ) THEN
               nin = nin + 1
               x = y
               Index = j
            ENDIF
 50      CONTINUE
* if no precise match found check for abbreviated match
         IF ( nin.EQ.0 ) THEN
            DO 60 j = 1 , N
               y = NOSTAR(List(j))
               IF ( y(:lx).EQ.x(:lx) ) THEN
                  nin = nin + 1
                  x = y
                  Index = j
               ENDIF
 60         CONTINUE
         ENDIF
         IF ( (Status.EQ.OK__) .AND. (nin.EQ.0) ) THEN
            Status = ERROR__
            Index = 0
         ELSEIF ( (Status.EQ.OK__) .AND. (nin.EQ.1) ) THEN
            Value = x
         ELSEIF ( (Status.EQ.OK__) .AND. (nin.GT.1) ) THEN
            x = Value(1:lx) // ' is ambiguous'
            lx = LENACT(x)
            WRITE (*,*) x(1:lx)
            Status = WARNING__
         ENDIF
      ENDIF
 
      RETURN
 
      END
