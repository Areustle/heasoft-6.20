**==LFTJST.spg  processed by SPAG 3.09I  at 09:44 on 20 Aug 1992
*- lftjst.for - left justified string
      CHARACTER*(*) FUNCTION LFTJST(Value,Type,Format)
* Description :
*  left justifies an output string as opposed to the default right justification
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  26 January 1989 : original
*  1 October 1991 : allow format='*'
 
      INCLUDE 'status.codes'
 
* Import :
      character(1) Value(*)
      character(3) Type
      CHARACTER*(*) Format
* Local variables :
      character(80) string
      character(3) t
      INTEGER*2 i2
      INTEGER*4 i4
      REAL*4 r4
      REAL*8 r8
      INTEGER i , imax
* External reference :
      INTEGER LENACT
* Status :
      INTEGER status
*-
      status = OK__
 
      string = ' '
      t = Type
      CALL LOCASE(t)
      IF ( t.EQ.'i*2' ) THEN
         CALL XMOVE(2,Value,i2)
         IF ( Format.EQ.'*' ) THEN
            WRITE (string,*,IOSTAT=status) i2
         ELSE
            WRITE (string,Format,IOSTAT=status) i2
         ENDIF
      ELSEIF ( t.EQ.'i*4' ) THEN
         CALL XMOVE(4,Value,i4)
         IF ( Format.EQ.'*' ) THEN
            WRITE (string,*,IOSTAT=status) i4
         ELSE
            WRITE (string,Format,IOSTAT=status) i4
         ENDIF
      ELSEIF ( t.EQ.'r*4' ) THEN
         CALL XMOVE(4,Value,r4)
         IF ( Format.EQ.'*' ) THEN
            WRITE (string,*,IOSTAT=status) r4
         ELSE
            WRITE (string,Format,IOSTAT=status) r4
         ENDIF
      ELSEIF ( t.EQ.'r*8' ) THEN
         CALL XMOVE(8,Value,r8)
         IF ( Format.EQ.'*' ) THEN
            WRITE (string,*,IOSTAT=status) r8
         ELSE
            WRITE (string,Format,IOSTAT=status) r8
         ENDIF
      ENDIF
 
      i = 1
      IF ( status.EQ.OK__ ) THEN
         imax = LENACT(string)
         DO WHILE ( (string(i:i).EQ.' ') .AND. (i.LT.imax) )
            i = i + 1
         ENDDO
      ENDIF
 
      LFTJST = string(i:)
 
      RETURN
 
      END
