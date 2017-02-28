**==CMPSTR.spg  processed by SPAG 3.09I  at 09:41 on 20 Aug 1992
      SUBROUTINE CMPSTR(String_1,String_2,Found)
c      checks for partial matching of string_1 and string_2
c      string_1 can include one wildcard  (*)
 
      CHARACTER*(*) String_1 , String_2
      INTEGER LENACT , len , len_1 , len_2 , start , end
      INTEGER wild , offset_s , offset_e
      LOGICAL Found
      Found = .FALSE.
      wild = INDEX(String_1,'*')
      IF ( wild.EQ.0 ) THEN
         len_1 = INDEX(String_1,' ') - 1
         len_2 = INDEX(String_2,' ') - 1
         len = MIN(len_1,len_2)
         IF ( String_1(1:len).EQ.String_2(1:len) ) Found = .TRUE.
      ELSEIF ( wild.EQ.LENACT(String_1) ) THEN
         IF ( String_1(1:wild-1).EQ.String_2(1:wild-1) ) Found = .TRUE.
         IF ( wild.EQ.1 ) Found = .TRUE.
      ELSE
         len_1 = INDEX(String_1,' ') - 1
         IF ( len_1.NE.0 .AND. String_1(len_1:len_1).EQ.'*' ) THEN
            String_1(len_1:len_1) = ' '
            len_1 = len_1 - 1
         ENDIF
         len_2 = INDEX(String_2,' ') - 1
         len = MIN(len_1,len_2)
         IF ( wild.EQ.1 .OR. (String_1(1:wild-1).EQ.String_2(1:wild-1))
     &        ) THEN
            IF ( wild.EQ.1 ) THEN
               offset_s = 1
               offset_e = 2
            ELSE
               offset_s = 0
               offset_e = 0
            ENDIF
            start = 1
            end = 0
            DO WHILE ( start+wild-1.LE.len_2 )
               IF ( String_1(1+offset_s:wild+offset_e-1)
     &              .EQ.String_2(start:start+wild+offset_s-2) ) THEN
                  start = start + wild - 2
                  DO WHILE ( end.LE.len_2 )
                     start = start + 1
                     end = start + len - wild - 1
                     IF ( String_1(wild+1:len).EQ.String_2(start:end) )
     &                    THEN
                        Found = .TRUE.
                        RETURN
                     ENDIF
                  ENDDO
                  RETURN
               ELSE
                  start = start + 1
               ENDIF
            ENDDO
         ENDIF
         Found = .FALSE.
      ENDIF
      RETURN
      END
