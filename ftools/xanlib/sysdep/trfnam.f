
      FUNCTION trfnam(inname)

      CHARACTER*(*) trfnam, inname

c  Function to translate a filename from the system-independent format 
c  to the system-dependent format. The system-dependency is wrapped-up
c  in the PTEND call.

c  kaa   1/27/93

      INTEGER lname, ltmp, kp
      INTEGER lenact
      CHARACTER ctmp*255, cdisk*255, cdir*255, cfile*255

      EXTERNAL lenact

      kp = 0
      lname = lenact(inname)
      CALL alf(inname, lname, kp, ctmp, ltmp)

      IF ( ltmp .LT. lname ) then
         cdisk = ctmp
         CALL alf(inname, lname, kp, ctmp, ltmp)
         cdir = ctmp
         CALL alf(inname, lname, kp, ctmp, ltmp)
         cfile = ctmp
         CALL ptend(cdisk, cdir, cfile)
         trfnam = cfile
      ELSE
         trfnam = inname
      ENDIF

      END
