c
      subroutine xrftgext(lui,string,imax,iext,itype,ifound,ierr)
      implicit none

c XRonos routine to Get FiTs EXTension number(s) 

c This routine looks for FITS extensions based on matching either input
c or default strings with EXTNAME, HDUCLAS1, HDUCALS2 and HDUCLAS3 keywords.
c It operates in several modes.

c If string is nonblank, xrftgext searches for a match with EXTNAME or HDUCLASx
c keywords.  The routine does not try to figure out itype in this case.
c If ichat > 0 the routine will also try to write some information to
c screen and log about the extensions it finds.  See subroutine xrftwext.

c If string is blank, and iext(1) < or = 0,
c    xrftgext searches for the rate table and gti EXTensions.

c If string is blank, and iext(1) > 0,
c    the routine only tries to determine the type of data
c    in that extension (as well as checking that fitsio can move to it).
c    This option overrides a nonblank input string.
c    Itype = 1 (event list) if the string 'EVENT' shows up in one of
c    the EXTNAME or HDUCLAS.  Otherwise the routine assumes itype = 2
c    (binned light curve).

c Note that we count extensions starting from 2, as in fitsio.
c Hence iext(i) = 1 points to the primary array.

c If no extension is found, the routine defaults to the first extension
c in the file and issues a warning.

c Default: iext(1) = 2, iext(2) = 0, itype = 2.

c  I  lui     (i)  Input file lu
c  I  ichat   (i)  Chattiness parameter
c  I  string  (c)  string to match with EXTNAME and HDUCLASx keywords
c  I  imax    (i)  Total number extensions to return (<= dimension of
c                     iext in the calling program)
c  O  iext    (i)  Vector of found extensions  
c                     for automatic search:
c                     iext(1) = rate table extension 
c                     iext(2) = gti extension ( = 0 if not found)
c  O  itype   (i)  Type of data in found extensions:
c                     = 1 for event data
c                     = 2 for binned data (by default)
c  O  ifound  (i)  number of extensions found in the case of string matching
c  O  ierr    (i)  Error status

c Author: eal  GSFC/HSTX  HEASARC  April, 1994
c Change DO WHILE ((ftstat.eq.0).and.(iext(1)*iext(2).eq.0)) to
c DO WHILE ((ftstat.eq.0).and.((iext(1).le.0).OR.(iext(2).le.0)))

      include '../include/io.inc'
      logical found
      integer nhdu1,nhdu2,nhdu3,nextname,ngti
      parameter (nhdu1 = 3, nhdu2 = 3, nhdu3 = 4, nextname = 5
     &         , ngti = 3)
      character*(*) string
      character(16) hduclas1(nhdu1),hduclas2(nhdu2),hduclas3(nhdu3)
     &   ,extname(nextname),gtiname(ngti),cbuf
      character(80) comm
      integer ftstat,hdutype,lui,iext(*),n,i,ifound,ichat,extn,imax
     &   ,itype,ierr

      data hduclas1 /'EVENT','EVENTS','LIGHTCURVE'/
      data hduclas2 /'TOTAL','NET','BKG'/
      data hduclas3 /'COUNTS','COUNT','RATE','FLUX'/
      data extname  /'EVENT','EVENTS','COUNTS','COUNT','RATE'/
      data gtiname  /'GTI','ALLGTI','STDGTI'/
      parameter (subname = 'xrftgext:')

      IF(ierr.ne.0) RETURN
      ftstat = 0
      itype = 2

c  Initialize to avoid warning
      extn = 0
c  --

c Switch for whether to search or just figure out the data type.

      IF(iext(1).gt.0) THEN
         
c     Check that iext(1) is ok.
         
	 CALL ftmahd(lui,iext(1),hdutype,ierr)
         IF((ierr.ne.0).or.(hdutype.ne.2)) THEN
            write(errm,*) 'In specified FITS extension ',iext(1)
            errm = subname//' '//errm
            CALL xaerror(errm, 5)
            RETURN
         ENDIF
         
c     Determine data type.
c     File type = 2 unless we see the word 'EVENT' in HDUCLAS1 or EXTNAME.
         
         CALL ftgkys(lui,'HDUCLAS1',cbuf,comm,ftstat)
         CALL rmvlbk(cbuf)
         CALL upc(cbuf)
         IF(cbuf(:5).eq.'EVENT') THEN
            itype = 1
         ELSE
            ftstat = 0
            CALL ftgkys(lui,'EXTNAME',cbuf,comm,ftstat)
            CALL upc(cbuf)
            CALL rmvlbk(cbuf)
            IF(cbuf(:5).eq.'EVENT') itype = 1
         ENDIF
         
      ENDIF
      
c Check for search string.

      CALL rmvblk(string)
      IF(string.ne.' ') THEN

c Try to match with string.

c Loop over found extensions.  This loop stops when it fills the iext
c array with imax entries or when it reaches the end of the file.

         n = 1
         DO WHILE((ftstat.eq.0).and.(ifound.lt.imax))
            found = .false.
            cbuf = ' '

c Loop over extensions.  This loop stops when it finds a match or when
c it gets to the end of the file.

            DO WHILE((ftstat.eq.0).and.(.not.found))

c Move to the next extension.

               CALL ftmahd(lui, n + 1, hdutype, ftstat)

               IF(ftstat.eq.0) THEN
                  CALL ftgkys(lui,'HDUCLAS3',cbuf,comm,ftstat)
                  CALL rmvlbk(cbuf)
                  IF(cbuf.eq.string) found = .true.
                  ftstat = 0
                  CALL ftgkys(lui,'HDUCLAS2',cbuf,comm,ftstat)
                  CALL rmvlbk(cbuf)
                  IF(cbuf.eq.string) found = .true.
                  ftstat = 0
                  CALL ftgkys(lui,'HDUCLAS1',cbuf,comm,ftstat)
                  CALL rmvlbk(cbuf)
                  IF(cbuf.eq.string) found = .true.
                  ftstat = 0
                  CALL ftgkys(lui,'EXTNAME',cbuf,comm,ftstat)
                  CALL rmvlbk(cbuf)
                  IF(cbuf.eq.string) found = .true.
                  ftstat = 0
               ENDIF
               IF(found) extn = n + 1
               n = n + 1

            ENDDO

            IF(ftstat.eq.0) THEN
               ifound = ifound + 1
               iext(ifound) = extn
            ENDIF
         ENDDO

c Write information about the extensions to stdio and log.

c         CALL xrftwext(lui,ichat,ifound,iext,ierr)

      ELSE

c Automatic search for a rate table.

         n = 1
c#          iext(1) = 0
         iext(2) = 0
      
c Loop over extensions.  This loop stops when it finds a match or when
c it gets to the end of the file.

         DO WHILE((ftstat.eq.0).and.((iext(1).le.0).OR.(iext(2).le.0)))
            cbuf = ' '
   
c  Move to the next extension.

            CALL ftmahd(lui, n + 1, hdutype, ftstat)

c Test against the templates defined above
            IF(ftstat.eq.0) THEN
               
               CALL ftgkys(lui,'HDUCLAS1',cbuf,comm,ftstat)
               CALL rmvblk(cbuf)
               CALL upc(cbuf)
               IF(iext(1).le.0) THEN
                  DO i = 1, nhdu1
                     IF(cbuf.eq.hduclas1(i)) iext(1) = n + 1
                     IF(cbuf(:5).eq.'EVENT') itype = 1
                  ENDDO
               ENDIF
               IF(iext(2).le.0) THEN
                  DO i = 1, ngti
                     IF(cbuf.eq.gtiname(i)) iext(2) = n + 1
                  ENDDO
               ENDIF
      
               ftstat = 0
               CALL ftgkys(lui,'HDUCLAS2',cbuf,comm,ftstat)
               CALL rmvblk(cbuf)
               CALL upc(cbuf)
               IF(iext(1).le.0) THEN
                  DO i = 1, nhdu2
                     IF(cbuf.eq.hduclas2(i)) iext(1) = n + 1
                  ENDDO
               ENDIF

               ftstat = 0
               CALL ftgkys(lui,'HDUCLAS3',cbuf,comm,ftstat)
               CALL rmvblk(cbuf)
               CALL upc(cbuf)
               IF(iext(1).le.0) THEN
                  DO i = 1, nhdu3
                     IF(cbuf.eq.hduclas3(i)) iext(1) = n + 1
                  ENDDO
               ENDIF

               ftstat = 0
               CALL ftgkys(lui,'EXTNAME',cbuf,comm,ftstat)
               CALL upc(cbuf)
               CALL rmvblk(cbuf)
               IF(iext(1).le.0) THEN
                  DO i = 1, nextname
                     IF(cbuf.eq.extname(i)) iext(1) = n + 1
                     IF(cbuf(:5).eq.'EVENT') itype = 1
                  ENDDO
               ENDIF
               IF(iext(2).le.0) THEN
                  DO i = 1, ngti
                     IF(cbuf.eq.gtiname(i)) iext(2) = n + 1
                  ENDDO
               ENDIF
               ftstat = 0

            ENDIF
            n = n + 1
         ENDDO
      ENDIF

c Default to first extension:

      IF(iext(1).le.0) THEN
         CALL xwarn(' Defaulting to first FITS extension',2)
         iext(1) = 2
      ENDIF

      return
      end
