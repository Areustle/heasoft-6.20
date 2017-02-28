      subroutine xrftinit(luo,ou_fil,ftstat)

c Xanadu wrapper for FITSIO ftinit.  

c This routine allows you to overwrite an existing FITS file.
c It opens a new file, and if it is unsuccessful, (presumably because
c the file already exists), deletes it with the xanadu delfil
c routine, then opens a new file called ou_fil.

c Use with caution.

c  I  luo    (i)  lu of input file
c  I  ou_fil (c)  name of input file
c  O  ftstat (i)  FITSIO status

      LOGICAL*4 exists
      CHARACTER*(*) ou_fil
      INTEGER*4 luo,block,ftstat
      integer dlu

      data block /2880/

      if(ftstat.ne.0) return

c If the file already exists, delete it and open a new one.
      inquire(file=ou_fil,exist=exists)
      if(exists) then
         call getlun(dlu)
         open(dlu,file=ou_fil,status='old',err=990)
         close(dlu,status='delete',err=990)
         call frelun(dlu)
      endif
      CALL ftinit(luo,ou_fil,block,ftstat)

 990  return
      end
