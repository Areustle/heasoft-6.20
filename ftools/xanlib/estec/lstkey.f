	subroutine listkey(keyword,match)

* Lists entries matching keyword, returns match true if it's done, else
* returns false.  If it returns true no more processing has to be done with
* this command

	logical match
	CHARACTER*(*) keyword

	include 'estec_tbl.inc'

	character(100) str1, str2, current,outline
	integer*4 lenact                         ,i,j,nnn
	logical found,finish


	match = .false.

	if (tblwcnt .eq. 0) return


	current = keyword
	nnn = 0
         found = .FALSE.
 
         str1 = current
         CALL UPC(str1)
 
 
            DO 60 i = 1 , Tblwcnt
               str1 = Tblwname(i)
               CALL UPC(str1)
               str2 = current
               CALL UPC(str2)
               IF ( str1(:LENACT(str2)).EQ.str2 ) THEN
* we have a command to print
                  DO 55 j = 1 , Tblccnt
                     str1 = Tblcname(j)
                     str2 = Tblwcomm(i)
                     CALL UPC(str1)
                     CALL UPC(str2)
                     IF ( str1.EQ.str2 ) THEN
                        found = .TRUE.
                        outline = ' ' // Tblwcomm(i)(1:20) // '-->  ' // 
     &                            Tblcdesc(j)
                        CALL XWRITE(outline,5)
                        nnn = nnn + 1
                        IF ( nnn.EQ.18 ) THEN
                           CALL PAGBRK(finish)
				match = found
                           IF ( finish ) RETURN
                           nnn = 0
                        ENDIF
                     ENDIF
 55               CONTINUE
               ENDIF
 60         CONTINUE
	match = found
	return
	end
