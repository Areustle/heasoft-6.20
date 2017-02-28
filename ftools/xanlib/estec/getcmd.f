**==GETCMD.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- get_command - get command, parameter and value
      SUBROUTINE GETCMD(Line,Ll,Command,Lc,Param,Lp,Value,Lv,Status)
* Description :
*  extracts the first three non-blank fields and their lengths from
*  character*(ll) line
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  14 June 1988 : original
*  12 October 1988 : set parameter and value to null where necessary
*  21 November 1989 : ignore spaces between brackets and double quotes
*  7 February 1990 : discard double quotes at the ends of the output strings
* 25 June 1991: renamed to getcmd
* 28 January 1991 : don't discard double quotes
 
      INCLUDE 'status.codes'
* Import :
      CHARACTER*(*) Line
      INTEGER Ll
* Export :
      CHARACTER*(*) Command
      INTEGER Lc
      CHARACTER*(*) Param
      INTEGER Lp
      CHARACTER*(*) Value
      INTEGER Lv
* Status :
      INTEGER Status
* Local constants :
      character(1) SPACE
      PARAMETER (SPACE=' ')
      character(1) NONE
C      PARAMETER (NONE=CHAR(0))
      character(1) LEFT_BRACKET
      PARAMETER (LEFT_BRACKET='(')
      character(1) RIGHT_BRACKET
      PARAMETER (RIGHT_BRACKET=')')
      character(1) DOUBLE_QUOTES
      PARAMETER (DOUBLE_QUOTES='"')
      character(1) IGNORE
*  ignore - to replace ignored spaces
C      PARAMETER (IGNORE=CHAR(0))
      INTEGER MAXIG
      PARAMETER (MAXIG=80)
* Local variables :
      INTEGER il , ir , jl , jr
      INTEGER nig
      INTEGER jig
      INTEGER isp(MAXIG)
      INTEGER i1(3) , i2(3)
      INTEGER i , j
*-
      NONE=CHAR(0)
      IGNORE=CHAR(0)
      IF ( Status.NE.OK__ ) RETURN
 
      Command = NONE
      Lc = 0
      Param = NONE
      Lp = 0
      Value = NONE
      Lv = 0
      DO 100 i = 1 , 3
         i1(i) = 0
         i2(i) = 0
 100  CONTINUE
 
      IF ( (Line.EQ.' ') .OR. (Ll.EQ.0) ) RETURN
 
      nig = 0
* ignore any bracketed spaces
      ir = 0
      jl = INDEX(Line(ir+1:),LEFT_BRACKET)
      jr = INDEX(Line(ir+1:),RIGHT_BRACKET)
      DO WHILE ( (jl.NE.0) .AND. (jr.NE.0) )
         il = jl + ir
         ir = jr + ir
         DO 150 i = il , ir
            IF ( Line(i:i).EQ.SPACE ) THEN
               Line(i:i) = IGNORE
               nig = nig + 1
               isp(nig) = i
            ENDIF
 150     CONTINUE
         jl = INDEX(Line(ir+1:),LEFT_BRACKET)
         jr = INDEX(Line(ir+1:),RIGHT_BRACKET)
      ENDDO
 
* ignore any spaces between pairs of double quotes
      ir = 0
      jl = INDEX(Line(ir+1:),DOUBLE_QUOTES)
      jr = INDEX(Line(ir+jl+1:),DOUBLE_QUOTES)
      DO WHILE ( (jl.NE.0) .AND. (jr.NE.0) )
         il = ir + jl
         ir = il + jr
         DO 200 i = il , ir
            IF ( Line(i:i).EQ.SPACE ) THEN
               Line(i:i) = IGNORE
               nig = nig + 1
               isp(nig) = i
            ENDIF
 200     CONTINUE
         jl = INDEX(Line(ir+1:),DOUBLE_QUOTES)
         jr = INDEX(Line(ir+jl+1:),DOUBLE_QUOTES)
      ENDDO
 
      i = 1
      IF ( Line(1:1).NE.SPACE ) THEN
         j = 1
         i1(j) = 1
      ENDIF
      DO WHILE ( (i.LT.Ll) .AND. (i1(3).LE.i2(2)) )
         IF ( (Line(i:i).NE.SPACE) .AND. (Line(i+1:i+1).EQ.SPACE) ) THEN
            i2(j) = i
         ELSEIF ( (Line(i:i).EQ.SPACE) .AND. (Line(i+1:i+1).NE.SPACE) )
     &            THEN
            j = j + 1
            i1(j) = i + 1
         ENDIF
         i = i + 1
      ENDDO
      i2(j) = Ll
 
* restore any ignored spaces
      IF ( nig.GT.0 ) THEN
         DO 250 jig = 1 , nig
            i = isp(jig)
            Line(i:i) = SPACE
 250     CONTINUE
      ENDIF
 
      IF ( j.GE.1 ) THEN
         Command = Line(i1(1):i2(1))
         Lc = i2(1) - i1(1) + 1
      ENDIF
      IF ( j.GE.2 ) THEN
         Param = Line(i1(2):i2(2))
         Lp = i2(2) - i1(2) + 1
      ENDIF
      IF ( j.GE.3 ) THEN
         Value = Line(i1(3):i2(3))
         Lv = i2(3) - i1(3) + 1
      ENDIF
 
      RETURN
 
      END
