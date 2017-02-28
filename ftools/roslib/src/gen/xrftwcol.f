c
      subroutine xrftwcol(lui,ichat,icols,icmax,extn)

c Write chosen FiTs COLumn information for a given extension.

c This routine displays the chosen columns -- #'s listed in icols --
c on the screen and in the log file, using xwrite.  It displays
c the TTYPEnnn keyword values for each chosen column.

c  I  lui     (i)  Input file lu
c  I  ichat   (i)  Chattiness parameter
c  I  icols   (i)  Column numbers to display
c  I  icmax   (i)  Maximum number of column numbers to display
c  I  extn    (i)  Number of the extension to fetch TTYPEs from

c Author: eal  GSFC/HSTX  HEASARC  March, 1994

      integer cmax
      parameter (cmax = 100)
      character(16) ttype(cmax)
      character(25) cbuf
      character(80) card
      integer i,j,extn,ftstat,ichat,icols(*),idum,lui,icmax

      if(ichat.eq.0) return

      ftstat = 0

      CALL ftmahd(lui,extn,idum,ftstat)
      CALL ftgkns(lui,'TTYPE',1,cmax,ttype,idum,ftstat)

      cbuf = ' Selected columns:'
      CALL xwrite(cbuf,ichat)

      i = 1
      DO WHILE (i.le.icmax)
         card = ' '
         j = 0
         DO WHILE ((j.lt.3).and.(i.le.icmax))
            cbuf = ' '
            IF((icols(i).gt.0).and.(icols(i).lt.cmax)) THEN
               WRITE(cbuf,101) icols(i), ttype(icols(i))
101            FORMAT(i4,2x,a8,';')
               j = j + 1
            card((j - 1)*25 + 1 : j*25) = cbuf
            ENDIF
            i = i + 1
         ENDDO
         CALL xwrite(card,ichat)
      ENDDO

      CALL xwrite(' ',ichat)

      return
      end
