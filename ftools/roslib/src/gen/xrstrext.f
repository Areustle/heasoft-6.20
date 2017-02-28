      subroutine xrstrext(in_fil,fname,iext,ierr)

c STRip the EXTension number from the filename

c The routine considers two cases: filename[ext#] and filename+ext#.
c The filename can contain a system-dependent path.
c The input filename has the extension substring stripped in place.
c The extension number is incremented by 1 for fitsio.

c I/O in_fil  (c)  input filename, with or without an extension substring,
c  O  fname   (c)  input filename, returned without an extension substring.
c  O  iext    (i)  extension number stripped off, + 1
c  O  ierr    (i)  error status

c Authors:  la, el  Goddard Space Flight Center   June, 1994

      CHARACTER*(*) in_fil,fname
      character(16) cbuf
      INTEGER*4 ierr,in1,in2,last,first,iext,iplus,LENACT

      if(ierr.gt.0) return

c Default extension number.

      iext = -99

c Parse the filename string.

      CALL rmvlbk(in_fil)
      CALL dirpos(in_fil,in1,in2)
      last=lenact(in_fil)

c Look for a '+' in the filename.

      iplus = index(in_fil(in2+1:last),'+')

c Test first for a closing bracket at the end of the string.

      IF(in_fil(last:last).EQ.']') THEN
         first=index(in_fil(in2+1:last),'[')
         cbuf = in_fil(first + 1:last - 1)
         READ(cbuf,*,err=999) iext
         iext = iext + 1
         fname = in_fil(1:first - 1)

c Test second for whether a '+' was found.

      ELSEIF(iplus.gt.0) THEN
         cbuf = in_fil(iplus + 1:last)
         READ(cbuf,*,err=999) iext
         iext = iext + 1
         fname = in_fil(1:iplus - 1)
      ELSE
         fname = in_fil
      ENDIF

      RETURN

999   CONTINUE
      ierr = 1060
      CALL xaerror('Error interpreting filename string',1)

      RETURN
      END
