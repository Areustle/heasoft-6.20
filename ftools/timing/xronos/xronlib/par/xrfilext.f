c
      SUBROUTINE xrfilext(cfile, cext, imode)
      implicit none
c
c ls 27/2/88 subroutine to handle extension of filenames (cext: e.g. ".rb")
c    25/11/88 Rev.1 : imode=4 added
c    29/8 /90 Rev.2 : imode=5 added
c
c      I    imode
c      I/R  cfile,cext
c
c           imode=1:   appends the extension "cext" to a filename "file"
c             if there is not one (i.e. there is no dot in the filename)
c           imode=2:    gets the extension "cext" of a filename "cfile"
c             if there is one (i.e. there are is a dot in the filename).
c             If there is not one, cext is returned full of blanks
c           imode=3:   removes the extension "cext" of a filename "cfile"
c             if there is one (note that cext may not be able to contain
c             it all). Otherwise "cfile" is unchanged and "cext" full
c             of blanks.
c           imode=4: remove the "directory" part of a filename (extension
c             is left unchanged)
c           imode=5: change the first char of filename after directory part
c             to 'B' (used for exposure files)
c
c  This version is for vms (changes for hp unix are indicated) c!
c
      CHARACTER*(*) cfile, cext
      INTEGER*4 islash, idot, idum, imode
c
      idum = 1
      islash = 0
      idot = 0
c
c  find beginning of filename (where the directory specif. end)
      CALL xrslash(cfile, islash)
c
c  find position of dot after beginning of filename
      idum = islash + 1
 2    IF (cfile(idum:idum).EQ.'.') idot = idum
      idum = idum + 1
      IF (cfile(idum:idum).NE.' ') THEN
         GOTO 2
      ENDIF
c      write(*,*) idot,idum     !!!!!
c
c  append extension if absent
      IF (imode.EQ.1) THEN
         IF (idot.EQ.0) cfile = cfile(:idum-1)//cext
         RETURN
      ENDIF
c
c  extract or remove extension if present
      IF (imode.EQ.2 .OR. imode.EQ.3) THEN
         cext = ' '
         IF (idot.NE.0) THEN
            cext = cfile(idot:)
            IF (imode.EQ.3) cfile(idot:) = ' '
         ENDIF
      ENDIF
c  Rev.1 start
c  remove "directory" part of the filename
      IF (imode.EQ.4) THEN
         cfile(1:idum-1-islash) = cfile(islash+1:idum-1)
         cfile(idum-islash:) = ' '
      ENDIF
c  Rev.1 stop
c  Rev.2 start
      IF (imode.EQ.5) THEN
         cfile(islash+1:islash+1) = 'B'
      ENDIF
c  Rev.2 stop
      RETURN
      END
