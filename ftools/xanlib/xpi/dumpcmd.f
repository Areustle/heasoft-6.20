**==dumpcmd.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE DUMPCMD
 
* This dumps the command file to stdout using xwrite, this is
* primarily for the benefit of the GUI
 
 
 
      INTEGER*4 i
      INTEGER*4 ierr
      INTEGER*4 LENACT
 
      character(100) outline
 
      INCLUDE 'tbl.inc'
 
      ierr = 0
 
      WRITE (outline,99001) TBLccnt
      CALL XWRITE(outline,5)
 
 
      DO 100 i = 1 , TBLccnt
         outline = ' '
C         CALL LOCASE(Tblcname(i))
         outline = TBLcname(i)(1:LENACT(TBLcname(i)))//','
         outline = outline(1:LENACT(outline))//TBLcdesc(i)
     &             (1:LENACT(TBLcdesc(i)))//','
         outline = outline(1:LENACT(outline))//TBLcacce(i)
     &             (1:LENACT(TBLcacce(i)))//','
         outline = outline(1:LENACT(outline))//TBLcwtyp(i)
     &             (1:LENACT(TBLcwtyp(i)))
         CALL XWRITE(' '//outline,5)
 100  CONTINUE
 
      RETURN
 
99001 FORMAT (' ',i5,' Lines in the command table')
 
      END
