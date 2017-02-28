**==XHELP.spg  processed by SPAG 3.09I  at 15:17 on 30 Sep 1992
      SUBROUTINE XHELPSUB(Cstr)
      CHARACTER Cstr*(*)

C---
C XPARSE subroutine to write an help string on the terminal and/or the log
C file, depending on the values of the chattyness flags
C---
C CSTR    I    String to be written. 
C 1991-Nov-21 - Andy Pollock's adaptation of xwrite
C 1992-Oct-21 - Bruce O'Neel modified xwrite
C---

      	integer lenact
	write (*,'(a)')cstr(1:lenact(cstr))
      END
