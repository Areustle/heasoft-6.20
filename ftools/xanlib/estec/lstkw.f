**==LSTKW.spg  processed by SPAG 3.09I  at 16:26 on 18 Nov 1992
      SUBROUTINE LSTKW(Keyword,Match)
 
* Lists entries matching keyword, returns match true if it's done, else
* returns false.  If it returns true no more processing has to be done with
* this command
 
      LOGICAL Match
      CHARACTER*(*) Keyword
 
      INCLUDE 'estec_tbl.inc'
      INCLUDE 'commands.inc'
 
      character(100) str1 , str2 , current , outline
      INTEGER*4 LENACT , i , j , nnn
      LOGICAL found , finish
 
 
      Match = .FALSE.
 
      IF ( Tblwcnt.EQ.0 ) RETURN
 
 
      current = Keyword
      nnn = 0
      found = .FALSE.
 
      str1 = current
      CALL UPC(str1)
 
 
      DO 100 i = 1 , Tblwcnt
         str1 = Tblwname(i)
         CALL UPC(str1)
         str2 = current
         CALL UPC(str2)
         IF ( str1(:LENACT(str2)).EQ.str2 ) THEN
* we have a command to print
            DO 20 j = 1 , Zcom_no
               str1 = Zcom_list(j)
               str2 = Tblwcomm(i)
               CALL UPC(str1)
               CALL UPC(str2)
               IF ( INDEX(str1,'*').NE.0 ) THEN
                  str2 = str2(1:INDEX(str1,'*')-1)
                  str1 = str1(1:INDEX(str1,'*')-1)
               ENDIF
               IF ( str1.EQ.str2 ) THEN
                  found = .TRUE.
                  outline = ' ' // Tblwcomm(i)(1:20) // '-->  ' // 
     &                      Zcom_descrip_list(j)
                  CALL XWRITE(outline,5)
                  nnn = nnn + 1
                  IF ( nnn.EQ.18 ) THEN
                     CALL PAGBRK(finish)
                     Match = found
                     IF ( finish ) RETURN
                     nnn = 0
                  ENDIF
               ENDIF
 20         CONTINUE
         ENDIF
 100  CONTINUE
      Match = found
      RETURN
      END
