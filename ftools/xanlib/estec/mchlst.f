**==MCHLST.spg  processed by SPAG 3.09I  at 09:44 on 20 Aug 1992
c
      SUBROUTINE MCHLST(Name,Descrip_list,Com_list,List_no,J,Pages)
c
      character(255) zwrite
      CHARACTER*(*) Name , Descrip_list(*) , Com_list(*)
      character(70) list , string
      INTEGER*4 List_no , i , J , LENACT , ilen
      LOGICAL*4 finish , Pages , ifound
c
      finish = .FALSE.
c
      CALL UPC(Name)
      CALL RMVLBK(Name)
      ilen = LENACT(Name)
c
      i = 0
      ifound = .FALSE.
      DO WHILE ( i.LT.List_no )
         i = i + 1
         list(1:ilen) = Descrip_list(i)(1:ilen)
         CALL UPC(list)
         IF ( Name(1:ilen).EQ.list(1:ilen) ) THEN
c
c  found match
c
            ifound = .TRUE.
            string = ' '
            string(25:35) = Com_list(i)
            string(37:41) = ' --> '
            string(43:70) = Descrip_list(i)
            WRITE (zwrite,99001) string
            CALL XWRITE(zwrite,10)
            J = J + 1
            IF ( J.EQ.18 .AND. Pages ) THEN
               CALL PAGBRK(finish)
               IF ( finish ) RETURN
               J = 0
            ENDIF
         ENDIF
      ENDDO
      IF ( .NOT.ifound ) THEN
         WRITE (zwrite,99002)
         CALL XWRITE(zwrite,10)
      ENDIF
      RETURN
99001 FORMAT (1X,A)
99002 FORMAT (' Command description not found')
      END
