**==yprint.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
 
* Print everything out
 
      SUBROUTINE YPRINT(tmpstr)
 
      INCLUDE 'yaccfor.inc'
      INTEGER i , LENACT , j
      character(255) line
      character*(*) tmpstr
 
      IF ( .NOT.DEBug ) RETURN
 
      line = 'yprint called with '//tmpstr
      CALL XWRITE(line(1:LENACT(line)),5)
      line = 'Line: ' // SLIne(1:LENACT(SLIne))
      CALL XWRITE(line(1:LENACT(line)),5)
      line = 'Command: ' // SCOm(1:LENACT(SCOm))
      CALL XWRITE(line(1:LENACT(line)),5)
      WRITE (line,*,IOSTAT=j) 'MaxPars: ' , NPArs
      CALL XWRITE(line(1:LENACT(line)),5)
      DO 100 i = 1 , NPArs
         WRITE (line,*,IOSTAT=j) i
         line (LENACT(line)+1:) = ' PAR: ' //
     &                            SPArs(i)(1:LENACT(SPArs(i))) //
     &                            ' VAL: ' // SVAl(i)(1:LENACT(SVAl(i)))
         CALL XWRITE(line(1:LENACT(line)),5)
 100  CONTINUE
 
      RETURN
      END
