      subroutine xrftgdes(lui,keywords)
      implicit none

c Get DEScriptive keywords from a XRonos FiTs file.

c This routine is unfinished.  It retrieves various general keywords
c for general purposes.  It performs several multiple kaeyword searches
c to cover several possible cases.  For example, the maximum energy channel
c may be listed in the MAXCHAN, CHANMAX or EMAX keyword.

c Xronos uses this routine for output to the terminal in subroutine
c xrfrdhe.

c  I  lui       (i)     Lu of input fits file
c  I  keysize   (i)     Size of each keyword string
c  O  keywords  (c*20)  Character array containing keyword values

c Author:  eal  GSFC/HSTX  HEASARC  February, 1994

c Subroutines called:  ftgkys

c keywords(1) = MINCHAN
c keywords(5) = MAXCHAN
c keywords(9) = RA
c keywords(13) = DEC
c keywords(17) = EXTNAME
c keywords(18) = OBJECT
c keywords(19) = TELESCOP
c keywords(20) = INSTRUME
c keywords(21) = DETNAME
c keywords(22) = FILTER
c keywords(23) = CLOCKCOR
c keywords(24) = TIMESYS
c keywords(4) = TIMEUNIT
c keywords(16) = HDUCLAS3

      integer kmax
      parameter (kmax = 26)
      character(8) keynames(kmax)
      character(20) keywords(*)
      character(48) comm
      integer lui, ftstat, k

      data keynames / 'MINCHAN ', 'CHANMIN ', 'E_MIN   ', 'TIMEUNIT'
     &              , 'MAXCHAN ', 'CHANMAX ', 'E_MAX   ', 'HDUCLAS1'
     &              , 'RA      ', 'RA_OBJ  ', 'RA_SRC  ', 'HDUCLAS2'
     &              , 'DEC     ', 'DEC_OBJ ', 'DEC_SRC ', 'HDUCLAS3'
     &              , 'EXTNAME ', 'OBJECT  ', 'TELESCOP', 'INSTRUME'
     &              , 'DETNAME ', 'FILTER  ', 'CLOCKCOR', 'TIMESYS '
     &              , 'RA_NOM  ', 'DEC_NOM ' /

      do k = 1, kmax
         keywords(k) = ' '
         ftstat = 0
         CALL ftgkys(lui,keynames(k),keywords(k),comm,ftstat)
      enddo

c Keywords with multiple possibilites.

      if(keywords(16).eq.' ') keywords(16) = keywords(17)

      do k = 0, 3
         if(keywords(1 + k*4).eq.' ') then
            if(keywords(2 + k*4).eq.' ') then
               keywords(1 + k*4) = keywords(3 + k*4)
            else
               keywords(1 + k*4) = keywords(2 + k*4)
            endif
         endif
      enddo

c Fourth choice for RA and DEC.

      if(keywords( 9).eq.' ') keywords( 9) = keywords(25)
      if(keywords(13).eq.' ') keywords(13) = keywords(26)

c Tidy up.

      CALL rmvlbk(keywords( 1))
      CALL rmvlbk(keywords( 4))
      CALL rmvlbk(keywords(18))
      CALL rmvlbk(keywords(19))
      CALL rmvlbk(keywords(20))
   
      return
      end
