      SUBROUTINE xpstlg (clog, qswtch)

      CHARACTER*(*) clog
      LOGICAL qswtch

c  XPARSE routine to set one of the logicals in the XPARINC common
c  block

c  clog    c*(*)    i: name of logical
c  qswtch  l        l: value

      character(20) tlog

      INCLUDE 'xparinc.inc'

      tlog = clog

      CALL upc(tlog)

      IF (tlog .EQ. 'QXCASE') THEN
         qxcase = qswtch
      ELSE IF (tlog .EQ. 'QXPART') THEN
         qxpart = qswtch
      ELSE IF (tlog .EQ. 'QXPFIR') THEN
         qxpfir = qswtch
      ELSE IF (tlog .EQ. 'REQUIRE_INQUIRY') THEN
         require_inquiry = qswtch
      ELSE IF (tlog .EQ. 'RETURN_INQUIRY') THEN
         return_inquiry = qswtch
      END IF

      RETURN
      END
