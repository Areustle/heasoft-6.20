      integer function f77demo()
      implicit none
      character(80) sval, fval, errm
      character(256) msg
      character(160) outfn
      integer status, ival, ounit
      logical bval, simple, extend
      real*8 rval
      integer bitpix, naxis, pcount, gcount
      integer PILGETSTRING, PILGETFNAME, PILGETINT
      integer PILGETREAL, PILGETBOOL
      integer hdpar_stamp

      f77demo=0

c  Register tool name and version. If this is not done
c  then a default value (= executable name) is used.

      call hdnameset('f77demo')
      call hdverset('1.2.3')
 
      status = PILGETSTRING('text', sval)
      if (status .ne. 0) then
         call hderr('Error in PILGetString()')
         f77demo=1
         return
      endif
      call hdecho('text parameter is: '//sval)
      
      status = PILGETFNAME('infile', fval)
      if (status .ne. 0) then
         call hderr('Error in PILGetFname()')
         f77demo=1
         return
      endif
      call hdecho('infile parameter is: '//fval)
      
      status = PILGETINT('mult', ival)
      if (status .ne. 0) then
         call hderr('Error in PILGetInt()')
         f77demo=1
         return
      endif
      write(msg,'(A19,i2)') 'mult parameter is: ',ival
      call hdecho(msg)

      status = PILGETREAL('scale', rval)
      if (status .ne. 0) then
         call hderr('Error in PILGetReal()')
         f77demo=1
         return
      endif
      write(msg,'(A20,F12.6)') 'scale parameter is: ',rval
      call hdecho(msg)

      status = PILGETBOOL('clobber', bval)
      if (status .ne. 0) then
         call hderr('Error in PILGetBool()')
         f77demo=1
         return
      endif
      write(msg,'(A22,L1)') 'clobber parameter is: ',bval
      call hdecho(msg)

      status = PILGETSTRING('outfile', outfn)
      if (status .ne. 0) then
         call hderr('Error in PILGetString()')
         f77demo=1
         return
      endif
      call hdecho('outfile parameter is: '//outfn)

      call hdchat(4, 'This should only appear if chatter >= 4')

      call hdchat(5, 'Creating new output file')

c  create a simple FITS file

      call ftgiou(ounit,status)
      if (status .ne. 0) then
        call hderr('Error getting output unit number')
        call ftgerr(status, errm)
        call hderr(errm)
        f77demo=1
        return
      endif

c  if "clobber" was yes then prepend a '!' so
c  that FITSIO will overwrite the file

      if (bval) then
        if (outfn(1:1) .ne. '!') then
          outfn='!'//outfn
        endif
      endif

      call ftinit(ounit,outfn,2880,status)
      if (status .ne. 0) then
        call hderr('Error opening output file')
        call ftgerr(status, errm)
        call hderr(errm)
        f77demo=1
        return
      endif

      simple = .TRUE.
      bitpix = -32
      naxis = 0
      pcount = 0
      gcount = 1
      extend = .TRUE.

      call ftphpr(ounit,simple,bitpix,naxis,0,pcount,
     &   gcount,extend,status)
      if(status.ne.0)then
         call hderr('Error writing keywords for outfile')
         call ftgerr(status, errm)
         call hderr(errm)
         f77demo=1
         return
      endif

c  hdparstamp() already knows the value of the history 
c  parameter so there's no need to read it at the task level

      status=hdpar_stamp(ounit, 1, status)
      if (status .ne. 0) then
        call hderr('Error writing history block')
        f77demo=1
        return
      endif

      call ftclos(ounit,status)
      if (status .ne. 0) then
        call hderr('Error closing output file')
        call ftgerr(status, errm)
        call hderr(errm)
        f77demo=1
        return
      endif

      call ftfiou(ounit,status)
      if(status.ne.0)then
        call hderr('Error freeing output unit number')
        call ftgerr(status, errm)
        call hderr(errm)
        f77demo=1
        return
      endif

      return
      end
