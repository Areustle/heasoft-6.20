      character * 64  text
C      call CLpath( 'DISPLAY_LIB' )
 10   continue
C      call txtrd('?CLI>',text)
      Call titrd('CLI>',text)
C      write(6,*) text
      L = Lenrd(text)
      write(*,*) '<',text(:L),'>'
C      Call CLmacS(text,L)
C      write(*,*) '<',text(:L),'>'
      goto 10
      end
