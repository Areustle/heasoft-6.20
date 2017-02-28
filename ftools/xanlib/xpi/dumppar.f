**==dumppar.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE DUMPPAR
 
* This dumps the parameter file to stdout using xwrite, this is
* primarily for the benefit of the GUI
 
 
 
      INTEGER*4 i
      INTEGER*4 ierr
      INTEGER*4 LENACT
 
      character(100) outline
 
      INCLUDE 'tbl.inc'
 
      ierr = 0
 
      WRITE (outline,99001) TBLpcnt
      CALL XWRITE(outline,5)
 
 
      DO 100 i = 1 , TBLpcnt
         outline = ' '
C         CALL LOCASE(Tblpname(i))
         outline = TBLpname(i)(1:LENACT(TBLpname(i)))//'|'
         outline = outline(1:LENACT(outline))//TBLptype(i)(1:1)//'|'
         outline = outline(1:LENACT(outline))//TBLpupda(i)
     &             (1:LENACT(TBLpupda(i)))//'|'
         outline = outline(1:LENACT(outline))//TBLpupd(i)
     &             (1:LENACT(TBLpupd(i)))//'|'
         IF ( TBLpdefl(i).NE.' ' ) THEN
            IF ( TBLptype(i)(1:1).EQ.'s' )
     &           outline = outline(1:LENACT(outline))//'"'
            outline = outline(1:LENACT(outline))//TBLpdefl(i)
     &                (1:LENACT(TBLpdefl(i)))
            IF ( TBLptype(i)(1:1).EQ.'s' )
     &           outline = outline(1:LENACT(outline))//'"'
         ENDIF
         outline = outline(1:LENACT(outline))//'|'
         IF ( TBLpminp(i).NE.' ' ) THEN
            IF ( TBLptype(i)(1:1).EQ.'s' )
     &           outline = outline(1:LENACT(outline))//'"'
            outline = outline(1:LENACT(outline))//TBLpminp(i)
     &                (1:LENACT(TBLpminp(i)))
            IF ( TBLptype(i)(1:1).EQ.'s' )
     &           outline = outline(1:LENACT(outline))//'"'
         ENDIF
         outline = outline(1:LENACT(outline))//'|'
         IF ( TBLpmaxp(i).NE.' ' ) THEN
            IF ( TBLptype(i)(1:1).EQ.'s' )
     &           outline = outline(1:LENACT(outline))//'"'
            outline = outline(1:LENACT(outline))//TBLpmaxp(i)
     &                (1:LENACT(TBLpmaxp(i)))
            IF ( TBLptype(i)(1:1).EQ.'s' )
     &           outline = outline(1:LENACT(outline))//'"'
         ENDIF
         outline = outline(1:LENACT(outline))//'|"'
         outline = outline(1:LENACT(outline))//TBLpdesc(i)
     &             (1:LENACT(TBLpdesc(i)))//'"'
         CALL XWRITE(' '//outline,5)
 100  CONTINUE
 
      RETURN
 
99001 FORMAT (' ',i5,' Lines in the parameter table')
 
      END
