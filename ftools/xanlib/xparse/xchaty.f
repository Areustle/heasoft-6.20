      subroutine xchaty(inter,log)
c            rashafer 22 mar 1986
c      XPARSE subroutine to set the chattyness levels used by XWRITE (et al.)
c      inter      i4      i: the chattyness level for the interactive terminal
c      log      i4      i: the chattyness level for the log file writes
c            N.B. if inter or log are < 0 then the current values are NOT
c            modified
      integer inter,log
      include 'xparinc.inc'
      trmcht=10
      logcht=10
      if(inter.ge.0)trmcht=inter
      if(log.ge.0)logcht=log
      return
      end
