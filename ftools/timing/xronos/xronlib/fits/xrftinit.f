      subroutine xrftinit(luo,ou_fil,ftstat)
      implicit none

c TIming wrapper for FITSIO ftinit.  This routine opens a new file,
c overwriting if ou_fil is already there, and putting in a null primary array.

c Use with caution.

c  I  luo    (i)  lu of input file
c  I  ou_fil (c)  name of input file
c  O  ftstat (i)  FITSIO status

      LOGICAL*4 simple,extend,exists
      include '../include/io.inc'
      CHARACTER*(*) ou_fil
      character(80) context
      INTEGER*4 luo,block,ftstat,pcount,bitpix,gcount,naxes(99),naxis
      integer dlu

      data block, pcount /2880, 0/
      data simple,bitpix,naxis,extend,gcount /.true., 8, 0, .true., 1/
      parameter (subname = 'xrftinit:')

      if(ftstat.ne.0) return


c     If file the already exists, delete it and open a new one.
      inquire(file=ou_fil,exist=exists)
      if(exists) then
         call getlun(dlu)
         open(dlu,file=ou_fil,status='old',err=990)
         close(dlu,status='delete',err=990)
         call frelun(dlu)
      endif
      CALL ftinit(luo,ou_fil,block,ftstat)
      

c Primary header: null primary array.

      CALL ftphpr(luo,simple,bitpix,naxis,naxes,pcount,gcount
     &           ,extend,ftstat)
      CALL ftpdef(luo,bitpix,naxis,naxes,pcount,gcount,ftstat)

      return

 990  context = 'Could not open output file' // ou_fil
      errm = subname//' '//context
      call xaerror(errm, 5)
      return

      end


