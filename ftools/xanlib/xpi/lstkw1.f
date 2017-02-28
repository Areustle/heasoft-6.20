**==lstkw1.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE LSTKW1(Keyword,Match)
 
* Lists entries matching keyword, returns match true if it's done, else
* returns false.  If it returns true no more processing has to be done with
* this command
 
      LOGICAL Match
      CHARACTER*(*) Keyword
 
      INCLUDE 'tbl.inc'
      INCLUDE 'commands.inc'
 
      character(100) str1 , str2 , current , outline
      INTEGER*4 LENACT , i , j , nnn , itmp
      LOGICAL found , finish
 
 
      Match = .FALSE.
 
      IF ( TBLwcnt.EQ.0 ) RETURN
 
 
      current = Keyword
      nnn = 0
      found = .FALSE.
 
      str1 = current
      CALL UPC(str1)
 
 
 
 
 
      DO 100 i = 1 , TBLwcnt
         str1 = TBLwname(i)
         CALL UPC(str1)
         str2 = current
         CALL UPC(str2)
         IF ( str1(:LENACT(str2)).EQ.str2 ) THEN
* we have a command to print
            DO 20 j = 1 , ZCOm_no
               str1 = ZCOm_list(j)
               str2 = TBLwcomm(i)
               CALL UPC(str1)
               CALL UPC(str2)
               itmp = INDEX(str1,'*')
               IF ( itmp.NE.0 ) str1 = str1(1:itmp-1)//str1(itmp+1:)
               itmp = INDEX(str2,'*')
               IF ( itmp.NE.0 ) str2 = str2(1:itmp-1)//str2(itmp+1:)
               IF ( str1.EQ.str2 ) THEN
                  found = .TRUE.
                  outline = ' '//TBLwcomm(i)(1:20)//'-->  '//
     &                      ZCOm_descrip_list(j)
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
