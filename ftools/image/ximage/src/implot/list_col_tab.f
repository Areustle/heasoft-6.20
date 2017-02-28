 
      SUBROUTINE LIST_COL_TAB
      implicit none
c
c list the available color table. the color table filename are
c in xanadu:[ximage.manager]color_table.dat (equivalent unix structure)
c file. The table file are instead in xanadu:[ximage.files]
c Any time a new table is added or name cahnged the color_table.dat file
c should also update
c
      INTEGER*4 i, idiv, ileft
      INCLUDE '../include/colordef.inc'
      INCLUDE '../include/io.inc'
c
c the name of the table are stored in common in the start_up
c
      idiv = int(ZTAbnum/3)*3
      ileft = ZTAbnum - idiv
      CALL XWRITE(' Available color tables are:',10)
      i = 1
      DO WHILE ( i.LE.idiv )
         WRITE (ZWRite,99001) ZTAb_name(i) , ZTAb_name(i+1) , 
     &                        ZTAb_name(i+2)
         CALL XWRITE(ZWRite,10)
         i = i + 3
      ENDDO
      if ( ileft.eq.1 ) then
         write (ZWRite,99001) ZTAb_name(idiv+1), ' ', ' '
         CALL XWRITE(ZWRite,10)
      elseif ( ileft.eq.2 ) then
         write (ZWRite,99001) ZTAb_name(idiv+1), ZTAb_name(idiv+2), ' '
         CALL XWRITE(ZWRite,10)
      endif
      RETURN
99001 FORMAT ('  ',a,'  ',a,'  ',a)
      END
