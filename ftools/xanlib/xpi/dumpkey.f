**==dumpkey.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE DUMPKEY
 
* This dumps the keyword file to stdout using xwrite, this is
* primarily for the benefit of the GUI
 
 
 
      INTEGER*4 i
      INTEGER*4 ierr
      INTEGER*4 LENACT
 
      character(100) outline , tmpline
 
      INCLUDE 'tbl.inc'
 
      ierr = 0
 
      WRITE (outline,99001) TBLkcnt
      CALL XWRITE(outline,5)
 
 
      DO 100 i = 1 , TBLkcnt
         outline = ' '
C         CALL LOCASE(Tblkname(i))
         outline = TBLkname(i)(1:LENACT(TBLkname(i)))//','
         outline = outline(1:LENACT(outline))//TBLkparm(i)
     &             (1:LENACT(TBLkparm(i)))//','
         WRITE (tmpline,*) TBLkposo(i)
         outline = outline(1:LENACT(outline))
     &             //tmpline(1:LENACT(tmpline))//','
         outline = outline(1:LENACT(outline))//TBLkclus(i)
     &             (1:LENACT(TBLkclus(i)))
         WRITE (tmpline,*) TBLkexcl(i)
         outline = outline(1:LENACT(outline))
     &             //tmpline(1:LENACT(tmpline))
         CALL XWRITE(' '//outline,5)
 100  CONTINUE
 
      RETURN
 
99001 FORMAT (' ',i5,' Lines in the key table')
 
      END
