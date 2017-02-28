**==QLMTCH.spg  processed by SPAG 3.09I  at 09:47 on 20 Aug 1992
*- qmatch - verify qualifiers
      SUBROUTINE QLMTCH(List,N,Value,Position,Status)
* Description :
*  checks if a string appears in a list
* History :
*  5 April 1989 : original adapted from get_match to deal with value=something
*  8 May 1990 : fix x//y to x(:lx)//y
*  25 Junae 1991: renamed to qmatch
*  12 May 1992 : rename qlmtch
* Author :
*  Andy Pollock (EXOSAT::ANDY)
 
      INCLUDE 'status.codes'
* Import :
*  list - list of answers to be checked
      CHARACTER*(*) List(*)
      INTEGER N
      CHARACTER*(*) Value
* Export :
*  position - position of the value in the list
      INTEGER Position
 
 
* Status :
      INTEGER Status
* Local variable :
      character(256) x , y , ctmp
      INTEGER lv , lx , ly
      INTEGER nin
      INTEGER j
* External reference :
      INTEGER LENACT
*-
      IF ( Status.NE.OK__ ) RETURN
 
      x = Value
      CALL UPC(x)
* find the non-blank length of the string
      lv = LENACT(x)
      IF ( lv.EQ.0 ) THEN
         Position = 0
      ELSE
* see if there's an equals sign
         lx = INDEX(x,'=') - 1
         IF ( lx.NE.-1 ) THEN
            y = x(lx+1:lv)
            ly = lv - lx
            x = x(:lx)
         ELSE
            lx = lv
            ly = 0
         ENDIF
         nin = 0
* check first for a exact match
         DO 50 j = 1 , N
            IF ( List(j).EQ.x ) THEN
               nin = nin + 1
               x = List(j)
               Position = j
            ENDIF
 50      CONTINUE
* if no precise match found check for abbreviated match
         IF ( nin.EQ.0 ) THEN
            DO 60 j = 1 , N
               IF ( List(j)(:lx).EQ.x(:lx) ) THEN
                  nin = nin + 1
                  x = List(j)
                  Position = j
               ENDIF
 60         CONTINUE
         ENDIF
         IF ( (Status.EQ.OK__) .AND. (nin.EQ.0) ) THEN
            Status = ERROR__
         ELSEIF ( (Status.EQ.OK__) .AND. (nin.EQ.1) ) THEN
            lx = LENACT(x)
            IF ( ly.EQ.0 ) THEN
               Value = x
            ELSE
               Value = x(:lx) // y
            ENDIF
         ELSEIF ( (Status.EQ.OK__) .AND. (nin.GT.1) ) THEN
            ctmp = Value(1:lx) // ' is ambiguous'
            lx = LENACT(ctmp)
            WRITE (*,*) ctmp(:lx)
            lx = LENACT(x)
            IF ( ly.EQ.0 ) THEN
               Value = x
            ELSE
               Value = x(:lx) // y
            ENDIF
            Status = WARNING__
         ENDIF
         IF ( Status.EQ.OK__ ) THEN
            IF ( nin.EQ.0 ) THEN
               Status = ERROR__
            ELSE
               IF ( nin.GT.1 ) THEN
                  ctmp = Value(1:lx) // ' is ambiguous'
                  lx = LENACT(ctmp)
                  WRITE (*,*) ctmp(:lx)
                  Status = WARNING__
               ENDIF
               lx = LENACT(x)
               IF ( ly.EQ.0 ) THEN
                  Value = x
               ELSE
                  Value = x(:lx) // y
               ENDIF
            ENDIF
         ENDIF
      ENDIF
 
      RETURN
 
      END
