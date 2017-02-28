**==dumpkw.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE DUMPKW
 
* This dumps the key file to stdout using xwrite, this is
* primarily for the benefit of the GUI
 
 
 
      INTEGER*4 i
      INTEGER*4 ierr
      INTEGER*4 LENACT
 
      character(100) outline
 
      INCLUDE 'tbl.inc'
 
      ierr = 0
 
      WRITE (outline,99001) TBLwcnt
      CALL XWRITE(outline,5)
 
 
      DO 100 i = 1 , TBLwcnt
         outline = ' '
         outline = TBLwname(i)(1:LENACT(TBLwname(i)))//','
         outline = outline(1:LENACT(outline))//TBLwcomm(i)
     &             (1:LENACT(TBLwcomm(i)))
         CALL XWRITE(' '//outline,5)
 100  CONTINUE
 
      RETURN
 
99001 FORMAT (' ',i5,' Lines in the keyword table')
 
      END
