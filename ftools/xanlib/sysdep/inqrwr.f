      SUBROUTINE inqrwr(filenm, ios)

      CHARACTER*(*) filenm
      INTEGER ios

c  Wrap-up for the inquire statement which allows machine-independent
c  filename spec.

c   filenm    C*(*)   i: filename
c   ios       I*4     r: inquire status

      INTEGER lenact, ltmp, kp, lfile
      character(256) ctmp, cdisk, cfile, cdir

      kp = 0
      lfile = LENACT(filenm)
      CALL alf(filenm, lfile, kp, ctmp, ltmp)
      IF (ltmp.EQ.lfile) THEN
         cdisk = ctmp
         CALL alf(filenm, lfile, kp, ctmp, ltmp)
         cdir = ctmp
         CALL alf(filenm, lfile, kp, ctmp, ltmp)
         cfile = ctmp
         CALL ptend(cdisk, cdir, cfile)
      ELSE
         cfile = filenm
      ENDIF

      INQUIRE(file=cfile, iostat=ios)

      RETURN
      END
