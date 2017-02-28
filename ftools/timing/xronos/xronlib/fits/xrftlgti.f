c
      subroutine xrftlgti(lui,extns,newfile,htunit,dtoffset,gtimax
     &                   ,frow,start,stop,dtsta,dtsto,nrows)
      implicit none

c TIming FiTs routine to Load Good Time Intervals.

c This routine reads from a GTI extension and loads GTIMAX GTI's
c starting at the FROW-th row,
c into the START and STOP vectors.  The times are converted to
c whatever time system, in days, is provided by DTOFFSET.

c If NEWFILE = .true. the routine looks through the header to
c find the start and stop columns and figure out how to reconstruct the
c time values.  If the columns are not found, the routine just returns with
c NROWS = 0.

c  I  lui      (i) lu for input FITS file
c  I  extns    (i) HDU numbers for GTI's (2) and RATE TABLE (1)
c  I  newfile  (l) = .true. if reading from a new file.
c  I  htunit   (i) Flag for time units in rate table header.
c  I  dtoffset (d) Internal time offset
c  I  gtimax   (i) length of start and stop vectors in calling routine.
c  I  frow     (i) First row to read from if .not.newfile
c  O  start    (d) Start times (returned in days)
c  O  stop     (d) Stop times  (returned in days)
c  O  dtsta    (d) Start time of the extension
c  O  dtsto    (d) Stop time of the extension
c  O  nrows    (i) Number of Good Time Intervals 

c Author: Eric Lufkin, HEASARC/GSFC, October 1993

      integer cmax
      parameter(cmax=100)
      logical anynul,newfile
      character(16) ttype(cmax),tform(cmax),tunit(cmax),extname,cdum
      PARAMETER (cdum = ' ')
      integer lui,extns(*),gtimax,iopt(15),ierr
     &   ,j,ivect(10),nrows,ctunit,idum,ichat
     &   ,nelem,htunit,frow,xtend,nfield,pcount
      double precision start(*),stop(*),nulvd,dtoffset,dtzero
     &   ,ddum,dtsta,dtsto
      parameter (nulvd = 0.d0)
      data ichat /0/
      data ivect /10*0/
      save

c Move to the GTI extension header.

      ierr = 0
      CALL ftmahd(lui,extns(2),xtend,ierr)

c Block for set-ups to make on the first read.

      if(newfile) then

c Read essential keywords.

         CALL ftghbn(lui,cmax,nrows,nfield,ttype,tform,tunit
     &              ,extname,pcount,ierr)

c Set vector columns using TTYPEnnn keywords.

         CALL xrftgcol(nfield,ttype,tunit,ivect)
         if(ivect(7)*ivect(8).le.0) then
            nrows = 0
            return
         endif

c Get timing keywords.

         CALL xrftgtky(lui,iopt,nrows,4,ivect,htunit,ctunit
     &             ,dtzero,dtoffset,ddum,dtsta,dtsto,idum,ddum
     &             ,idum,ierr)

      endif

c Read the start and stop columns.

      nelem = min(nrows-frow+1, gtimax)
      if(nelem.gt.0) then
         ierr = 0
         CALL ftgcvd(lui,ivect(7),frow,1,nelem,nulvd,start,anynul
     &              ,ierr)
         CALL ftgcvd(lui,ivect(8),frow,1,nelem,nulvd,stop ,anynul
     &              ,ierr)
      endif

      if(.not.anynul) then
c        Scale appropriately.
         do j = 1, nelem
            CALL xrdectun(cdum,ctunit,2,start(j),ierr)
            CALL xrdectun(cdum,ctunit,2,stop (j),ierr)
            start(j) = start(j) + dtoffset + dtzero
            stop (j) = stop (j) + dtoffset + dtzero
         enddo
      else
c        There should not be any null values in a GTI table.
         ierr = 1
      endif

c Return to the Rate Table header.

      CALL ftmahd(lui,extns(1),xtend,ierr)

c Error trap: no GTIs if there are errors

      IF(ierr.ne.0) THEN
         CALL xwrite('Error reading GTIs -- GTIs will be ignored',1)
         nrows = 0
         ierr = 0
      ENDIF

      return
      end
