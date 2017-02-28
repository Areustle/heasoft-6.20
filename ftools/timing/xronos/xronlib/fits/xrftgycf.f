      subroutine xrftgycf(lui,colnum,crunit,dtp,naxis,naxes,eaxis,taxis
     &                   ,emax,tmax,ierr)
      implicit none
      
c XRonos FiTs routine to Get the Y-Column Format.

c This routine retrieves all information needed to calculate a count
c rate and its instrumental error.

c the routine first decodes the units.  If the substring '/s' or '/S' appears
c anywhere in the TUNITxxx keyword value for the column, the units
c counts/sec are assumed.  Otherwise counts are assumed.

c Next, the routine tries to determine time and energy axes.
c Failure to find a 1CTYPnnn keyword implies a scalar Y column.

c Failure to find a TDIM keyword implies a 1-D vector for the column, even
c if it has a length of 1.

c if mCTYPnnn = 'TIME' the n-th axis is taken as time.
c If mCTYPnnn = 'ENERGY', 'CHANNEL' or 'SPECTRUM' the n-th axis
c is taken as energy channels.
c Any other dimensions are ignored, and only their first element will be
c read by the reader (subroutine xrfrdyco).

c In addition, checks are performed on the units in the time and channel
c axes:

c   The Time axis must have a nCUNImmm keyword value readable by subroutine 
c   xrdectun.  It must also have an integration time listed in the nCDLTmmm
c   keyword.  If not the routine will issue a fatal error.

c   The energy channel axis must have nCUNImmm = 'chan' or 'CHAN'.  If not
c   the energy channel axis is ignored.

c  I  lui    (i) = unit number for the fits file
c  I  colnum (i) = Y column number
c  O  crunit (i) = 0 for counts, 1 for counts per second.
c  O  naxis  (i) = number of dimensions (returned > = 1)
c  O  naxes  (i) = array of dimensions
c  O  eaxis  (i) = subscript for energy channels
c  O  taxis  (i) = subscript for time
c  O  dtp    (d) = delta time between times in the packet (= -1 if not found)
c  O  tmax   (i) = Number of times stored in each packet
c  O  emax   (i) = Number of energy channels in each packet
c  O  ierr   (i) = error returned

c Author: Eric Lufkin, HEASARC/GSFC, March 1994

      include '../include/io.inc'
      integer maxdim
      parameter(maxdim=9)
      character(16) dltstr,unistr,cbuf,cval,tform, unit
      integer lui,colnum,tmax,ierr,naxis,naxes(maxdim),crunit
     &   ,taxis,ptunit,eaxis,emax,ftstat,idum1,idum2,i,kystat
      double precision dtp
      parameter (subname = 'xrftgycf:')

      if(ierr.ne.0) return
      ftstat=0


c Units: counts or rates (counts/sec).

      cbuf = ' '
      kystat = 0
      CALL ftgkns(lui,'TUNIT',colnum,1,cbuf,idum1,kystat)
      CALL upc(cbuf)
      CALL rmvblk(cbuf)
      IF(index(cbuf,'/S').gt.0) THEN
         crunit = 1
      ELSE
         crunit = 0
      ENDIF

c Default: scalar rate column.

      tmax = 1
      emax = 1
      dtp = -1.d0

      if(colnum.lt.1) then
c No rate or counts column means event data.
         return
      else
         eaxis=0
         taxis=0
         kystat = 0
         naxis = 0

c Search for a TDIMnnn keyword.

         CALL ftgtdm(lui,colnum,maxdim,naxis,naxes,kystat)

c If not found, the column is treated as a 1-D vector.  Find its length.

         IF(kystat.ne.0) THEN
            kystat= 0
            naxis = 1
            CALL ftgkns(lui,'TFORM',colnum,1,tform,idum1,kystat)
            ftstat=0
            CALL ftbnfm(tform,idum1,naxes(1),idum2,ftstat)
         ELSE
            naxis = min(naxis,maxdim)
         ENDIF

         DO i = 1, naxis

c Assemble the keyword name for mCTYPnnn.

            cbuf = ' '
            write(cbuf,101) i, 'CTYP'
101         format(i1,a4)

c Look for the keyword.

            kystat = 0
            CALL ftgkns(lui,cbuf,colnum,1,cval,idum1,kystat)
            CALL rmvlbk(cval)
            CALL upc(cval)

c Assign array coordinates for time and energy.

            IF(cval.eq.'TIME') taxis = i
            IF((cval.eq.'ENERGY').or.(cval.eq.'CHANNEL').or.
     &         (cval.eq.'SPECTRUM')) THEN
               write(cbuf,101) i, 'CUNI'
               CALL ftgkns(lui,cbuf,colnum,1,cval,idum1,kystat)
               CALL upc(cval)
               CALL rmvlbk(cval)
               IF(cval(:4).eq.'CHAN') eaxis = i
            ENDIF
         ENDDO

c Case of scalar values.

         IF((taxis.eq.0).and.(eaxis.eq.0)) eaxis = 1

c Error trap.

         IF(ftstat.ne.0) THEN
            write(errm,*) 'xrftgycf: fitsio error = ',ftstat
            errm = subname//' '//errm
            CALL xaerror(errm, 5)
            return
         ENDIF

         if(eaxis.gt.0) emax = naxes(eaxis)
         if(taxis.gt.0) then
            tmax = naxes(taxis)

            write(dltstr,101) taxis, 'CDLT'
            write(unistr,101) taxis, 'CUNI'

c Search for keywords denoting delta-time and its units.

            CALL ftgknd(lui,dltstr,colnum,1,dtp  ,idum1,ierr)
            CALL ftgkns(lui,unistr,colnum,1,unit,idum1,ierr)

c Interpret the time units in the packet.

            if(ierr.ne.0) then
               errm = 'Unable to decode units in packet data'
               errm = subname//' '//errm
               CALL xaerror(errm, 5)
               return
            else
               CALL xrdectun(unit,ptunit,1,dtp,ierr)
            endif
         endif
      endif

      return
      end
