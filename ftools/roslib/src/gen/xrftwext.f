c
      subroutine xrftwext(lui,ichat,extmax,extns,ftstat)

c Write write information about chosen FiTs file EXTensions.

c This routine writes out the extension number, EXTNAME keyword value,
c number of rows and number of columns for each extension listed in
c extns, up to extns's extmax-th array element.  If extns(i) < 1, no
c information is displayed for the i-th element.

c  I  lui     (i)  Input file lu
c  I  ichat   (i)  Chattiness parameter
c  I  extmax  (i)  Total number extensions to display
c                  (dimension of extns)
c  O  extns   (i)  array of extensions to display
c  O  ftstat  (i)  fitsio error

c Subroutines called: ftmahd, ftghbn, ftgkys, xwrite

c Author: eal  GSFC/HSTX  HEASARC  February, 1994

      integer extmax
      integer i, maxfld, nfield, ftstat, ichat
     &   ,hdutype, lui, nrows, extns(extmax)
      character(16) extname,hdu(3),routine
      character(48) comm
      character(79) cbuf
      data hdu /'Primary Array','ASCII Table','Binary Table'/
      data maxfld /1/
      data routine /'xrftwext'/

      if(ichat.eq.0) return

      CALL xwrite(' ',ichat)
      cbuf = ' Extensions:'
      CALL xwrite(cbuf,ichat)
      CALL xwrite(' ',ichat)

      write(cbuf, 102)
102   format(10x,'HDU type',5x,'HDU name',15x,'Rows',2x
     &       ,'Columns')
      CALL xwrite(cbuf,ichat)

      write(cbuf,103)
103   format(60('_'))
      CALL xwrite(cbuf,ichat)

      ftstat = 0
      extname=' '
c Loop over extensions.

      do i = 1, extmax
         IF(extns(i).gt.0) THEN
            CALL ftmahd(lui,extns(i),hdutype,ftstat)
            CALL ftgkyj(lui,'NAXIS2',nrows,comm,ftstat)
            CALL ftgkyj(lui,'TFIELDS',nfield,comm,ftstat)
            CALL ftgkys(lui,'EXTNAME',extname,comm,ftstat)
            IF(extname.EQ.' ')THEN
               extname='UNKNOWN'
               ftstat=0
            ENDIF
            write(cbuf, 101) extns(i) - 1, hdu(hdutype + 1), extname
     &         , nrows, nfield
101         format(i4,2x,a16,2x,a16,2x,i8,2x,i5)
            CALL xwrite(cbuf,ichat)
         ENDIF
      enddo

      CALL xwrite(' ',ichat)

      IF(ftstat.ne.0) CALL xaerror('xrftwext: reading from FITS file',1)

      return
      end
