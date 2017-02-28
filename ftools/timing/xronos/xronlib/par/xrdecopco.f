c
      SUBROUTINE xrdecopco(copt, iv, dv, ity,status)
c
c ls  6/7/88 to decode numerical constant in file option
c
c     I   copt = option to be decoded
c     O   iv = integer variable containing numerical constant
c     O   dv = real*8 variable     "         "          "
c     I   ity = 1 for integer, 2 for real*8 numerical constant
c     O   status = iostat error code
c
c  Options consist of 2 chars. and in some cases a numerical value up to
c  8 digits. The maximum no. of chars in one option is 10.
c
      CHARACTER copt*10, cform*12
      INTEGER*4 iv, status, ity, io, ia
      REAL*8 dv
c
c set fake default to avoid overwriting error message in xrdecopt
c
      if(status.ne.0) return

      iv = 1
      dv = 1.D0
c
c detrmine ia and io
c
      ia = 3
c condition for missing constant (0 chars)
      IF (copt(ia:ia).EQ.' ') THEN
         status = 1018
         RETURN
      ENDIF
      io = 10 + 1
 1    io = io - 1
      IF (copt(io:io).EQ.' ') THEN
         GOTO 1
      ENDIF
c
c decode last 8 chars of copt
c
      GOTO (10, 20), ity
 10   WRITE (cform, 100) ia, io - ia + 1
 100  FORMAT ('(t', I2.2, ',i', I2.2, ')   ')
      READ (copt, cform, IOSTAT=status, ERR=900) iv
      RETURN
 20   WRITE (cform, 200) ia, io - ia + 1
 200  FORMAT ('(t', I2.2, ',f', I2.2, '.0) ')
      READ (copt, cform, IOSTAT=status, ERR=900) dv
      RETURN
 900  status = 1017
      RETURN
      END
c
c
c
