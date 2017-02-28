c
      subroutine xrfwrlhe(lui)
 
c WRite Xronos Fits HEader information (Long format).

c Subroutines called: ftghsp, ftgkyn

c Author: Eric Lufkin, HEASARC/GSFC, September, 1993
c Revised: eal  May, 1994 to call xwrite.

      integer lui,lt,k,keynum,kdum,kystat
      character(80) card

c Dump all keywords.

      kystat = 0
      CALL ftghsp(lui,keynum,kdum,kystat)

      do 10 k = 1, keynum
         kystat = 0
         CALL ftgrec(lui,k,card,kystat)
         CALL xwrite(card,15)
10    continue

      CALL xwrite(' ',20)
 
      return
      end
