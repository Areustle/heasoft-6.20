      subroutine xrfrdpon(cfilin, extn, iopt, mopt, dopt, m, irec
     &                    , iyrsta, dtime, dtint, expos, y, sy, nbad
     &                    , nogap, lui, ierr)
      implicit none
c
c ReaD POiNts from XRonos Fits file.
c
c This version handles scalar or vector or 2-D array columns.
c
c   I  cfilin = xronos FITS filename
c# c   I  iext = table of xronos FITS extensions 
c   I  extn = extension number to read from
c           = 1 for Rate Table extension
c           = 2 for Good Time Intervals extension
c           = 3 for Exposure extension
c   I  iopt = flag options
c   I  mopt = mathematical options
c   I  dopt = constants for math opt
c   I   m = index of file to be handled (1-3 for infiles; 4-6 for expos files)
c  I/O irec = 0 to open the file, n when reading line n, -1 when last point was
c             read and file was closed
c  I/O iyrsta = flag indicates if start time is set (1) or not set (0)  
c               originally set to year of first infile
c   O  dtime = center time (days) of bin (or arrival time of photon)
c   O  dtint = bin integration time in file ( if <0 file is  arrival time file)
c   O  expos = bin fractional exposure ( set =1 in all cases)
c   O  y = cts/s in bin (or =1 for arrival time files)
c   O  sy = error on cts/s in bin (or =1 for arrival time files)
c  I/O nbad  = no. of ``bad photons'' which arrive between good time 
c              intervals, or outside pha range.
c  I/O nogap  = no. of gap bins
c   O  lui = logical unit numbers for current files (array) passed
C            back up to controlling routine in case it needs to close one
c   O  ierr = i/o error returned
c
c File options relevant to this subr.
c      iopt(1) = n to start loading y-vector at element n
c      iopt(4) = n to stop  loading y-vector at element n
c      iopt(2) = n to start reading at row n
c      iopt(3) = n to stop  reading at row n
c      iopt(5) = n to assign column n to X-axis (time)
c      iopt(6) = n to assign column n to Y-axis
c      iopt(7) = n to assign column n to Energy channel (pha)
c      iopt(8) = n to assign column n to Dead Time
c      iopt(9) = n to assign column n to Y-error

c Subroutines called: ftopen, ftmahd, ftghbn, ftgkys, ftgkyj, ftbnfm, xrftsetv
c                     xrapfopt, xrftgdtm, xrftgbdv, ftgcvd, ftgcve,
c                     ftclos, xrwrplust, xrwrplusl, upc, xrftmktp, xrftgref
c                     xrftgtm0, xrftgyri, tiftlgti
c
c itype =1 event, itype=2 rate , itype=3 exposure, itype=4 gti.
c extn =1 event or rate extns=2 gti
c iext(1) extension number for rate or event iext(2) gti.
c
c 'TIME   '         ivect(1,m)
c 'RATE'   'COUNT'  ivect(2,m)
c 'DEADC'           ivect(4,m)
c 'TIMEDEL'         ivect(5,m)
c 'FRACEXP'         ivect(6,m)
c 'PHA'             ivect(9,m)
c
c Author: Eric Lufkin, HEASARC/GSFC, August 1993
c Revised:                        -  November 1993 to handle GTI extensions
c                                    and calls from xrgetexp family.
c                                 -  November 1993 to handle 2-D arrays.
c Based on subroutines xrqdprdpon.f and xrrbrdpon.f by L.Stella and L. Angelini.

      include '../include/io.inc'
      integer nlui, cmax, imax, vmax, gtimax, maxdim,iv
      logical eofflag
      parameter (nlui=8, cmax=40, imax=10, vmax=1024, maxdim = 9)
c     One FITS block contains 180 8-byte pairs.
      parameter (gtimax = 180)
      logical anynul,deadapp,vignapp,newfile(nlui),inside,agreed
      character(160) cfilin(8)
      character(16) ttype(cmax),tform(cmax),tunit(cmax),extname,cdum
      integer lui(nlui),ierx,iopt(15,nlui),mopt(15,nlui),itype(nlui)
     &   ,iyrsta(nlui),ivect(imax,nlui),ngti(nlui),igti(nlui),tgti(nlui)
     &   ,nogap(nlui),nbad,irec(nlui),m,npts(nlui),ierr
     &   ,pcount,nrows(nlui),nfield,emax(nlui)
     &   ,frow(nlui),lrow(nlui),block,rwmode,xtend,htunit(nlui)
     &   ,ctunit(nlui),iext(imax,nlui),extn,inext(imax)
     &   ,i,tmax(nlui),crunit(nlui),gmax(nlui),lcount(nlui),ndum(nlui)
     &   ,trow(nlui),pha,phahi(nlui),phalo(nlui),nulvj,idum,ichat
     &   ,naxis(nlui),naxes(maxdim,nlui),eaxis(nlui),taxis(nlui)
      real y(nlui),sy(nlui),expos(nlui),nulve,exposure(nlui)
     &   ,deadc,deadcor,vignet,backv,rdty(nlui),rdtsy(nlui)
      double precision dtint(nlui),dtime(nlui),dfoffset(nlui)
     &   ,dopt(15,nlui),nulvd,dtoffset(nlui),gtia(gtimax,nlui)
     &   ,gtio(gtimax,nlui),dtzero(nlui),gtiolast,dtp(nlui)
     &   ,start(gtimax),stop(gtimax),dtsta(nlui),dtsto(nlui),ddum,ddum1
     &   ,gtista,gtisto, grsta, grsto, tart(5), top(5), photfst(nlui)
     &   ,dtrow(nlui)
      parameter (nulvj = -9999, nulve = -1.2e34, nulvd = -1.2d34)

      data pcount,block,rwmode /0,2880,0/
      data deadc, vignet, backv /3*1./
      data tmax /nlui*1/
      data newfile /nlui*.false./
      data dtoffset /nlui*0.d0/
      data ichat /0/
      
      save
      parameter (subname = 'xrfrdpon:')
c Selected internal variables (arguments supressed):

c emax    =  number of energy bins stored in each packet ( > or = 1)
c tmax    =  number of TIMEs stored in each packet ( > or = 1)
c dtint   =  time interval between packets.
c dtp     =  TIME increment in the packet (set to dtint if not found)
c frow    =  current row in the FITS file.
c trow    =  current TIME row in the current packet
c ctunit(m) = 1 if the column is measured in seconds, 2 if in days

c Miscellaneous initializations.

c     Check Error flag
      if(ierr.ne.0) return

c Initializations and Header Reading.

      if(irec(m).eq.0) then

c First read from file: read header and initialize internal parameters.

         newfile(m) = .true.
         do i = 1, imax
            ivect(i,m) = 0
         enddo
         IF(m.gt.4) dtoffset(m) = dtoffset(m-4)
         IF(m.gt.4) htunit(m) = htunit(m-4)
         dfoffset(m) = 0.d0
         dtp(m) = -1.0d0

c Open the file: rwmode=0 -- read only.

         CALL getlun(lui(m))
         cdum = ' '
         IF(iopt(10,m).gt.0) THEN
            iext(1,m) = iopt(10,m) + 1
         ELSE
            iext(1,m) = 0
         ENDIF
         
         call ftopen(lui(m),cfilin(m),0,block,ierr)
 
         if(ierr.ne.0) then
            errm = subname//' '//'Error opening file '//cfilin(m)
            call xaerror(errm, 5)
            go to 999
         endif

c Look for usable extensions in the FITS file.
       CALL xrftgext(lui(m),' ',2,iext(1,m),itype(m),idum,ierr)

c Set gti location if veN (i.e. opt(8)) defined
         if ( itype(m).eq.1 .and. iopt(8,m).gt.0 ) then
            iext(2,m) = iopt(8,m) + 1
         endif

c Set-ups for rate tables.

         IF(extn.eq.1) THEN

c Move to the rate table extension header.

            CALL ftmahd(lui(m),iext(1,m),xtend,ierr)

c Read mandatory binary header keywords from extension.

            CALL ftghbn(lui(m),cmax,nrows(m),nfield,ttype,tform,tunit
     &                  ,extname,pcount,ierr)

c Set vector columns using TTYPEnnn keywords.

            CALL xrftgcol(nfield,ttype,tunit,ivect(1,m))
            CALL xrcolopt(iopt(1,m),nfield,itype(m),ivect(1,m),ierr)
   
c Determine rate column format.

            CALL xrftgycf(lui(m),ivect(2,m),crunit(m),dtp(m),naxis(m)
     &                   ,naxes(1,m),eaxis(m),taxis(m),emax(m)
     &                   ,tmax(m),ierr)

 
c Get timing information.
 
            CALL xrftgtky(lui(m),iopt(1,m),nrows(m),itype(m)
     &                   ,ivect(1,m),htunit(m),ctunit(m),dtzero(m)
     &                   ,dtoffset(m),dtint(m),dtsta(m),dtsto(m),npts(m)
     &                   ,dfoffset(m),idum,ierr)
c
c dtint return the value from the file or -1 is not found in the header
c and itype not an event list
c 
            if(dtp(m).le.0.d0) THEN
              IF (dtint(m).eq.-1)THEN
                 dtp(m) = 0.0
              else
                 dtp(m) = dtint(m)
                 IF(itype(m).eq.1) dtint(m) = -1.d0
              endif
            else 
               dtint(m)=dtp(m)
            endif
c Check agreement with GTI times.

C     PROBABLY WANT TO LOSE THIS
            IF(iopt(8,m).eq.-99) THEN
               inext(1) = iext(1,m)
               inext(2) = iext(2,m)
               CALL xrfagree(lui(m),0,iopt(1,m),inext,itype(m),dtsta(m)
     &                      ,dtsto(m), ddum, agreed,ierr)
               IF(.not.agreed) iext(2,m) = 0
            ENDIF

c Energy bounds.
 
            CALL xrphaopt(iopt(1,m),itype(m),emax(m),phalo(m),phahi(m)
     &                   ,ivect(1,m),ierr)

c Get deadtime and collimator corrections.

            CALL xrftgbdv(lui(m),deadapp,deadc,vignapp,vignet,rdty(m)
     &                   ,rdtsy(m),exposure(m))
c             write(*,*)'rdty(m), exposure(m)',rdty(m), exposure(m)


c GTI set-ups.

         elseif(extn.eq.2) then

c If there isn't a GTI extension, punt.

            IF(iext(2,m).le.0) THEN
               errm = subname//' '//'No GTI extension in  '//cfilin(m)
               call xaerror(errm,5)
               ierr = -1012
               idum = 0
               eofflag=.true.
               goto 999
            ENDIF
            IF(iopt(8,m).eq.-99) THEN
               errm = '  GTI will not be used '
               call xwrite(errm,5)
               ierr = -1012
               idum = 0
               eofflag=.true.
               goto 999
            ENDIF

c Move to the GTI extension header.

            CALL ftmahd(lui(m),iext(2,m),xtend,ierr)

            itype(m) = 4

            inext(1) = iext(1,m)
            inext(2) = iext(2,m)
            CALL xrftlgti(lui(m),inext,.true.,htunit(m),dtoffset(m)
     &                   ,gtimax,1,start,stop,gtista,gtisto,ngti(m))

c Check agreement with rate table times.

            IF(iopt(8,m).eq.-99) THEN
               CALL xrfagree(lui(m),0,iopt(1,m-4),inext,4,gtista,gtisto
     &                      ,ddum, agreed,ierr)
               IF(.not.agreed) THEN
                  ierr = - 1012
                  idum = 0
                  CALL ftclos(lui(m),idum)
                  CALL frelun(lui(m))
                  return
               ENDIF
            ENDIF

c Store gtis in memory.

            gmax(m) = min(ngti(m),gtimax)
            DO i = 1, gmax(m)
               gtia(i,m) = start(i)
               gtio(i,m) = stop(i)
            ENDDO
            lcount(m) = 1
         endif

         

c Initialize FITS reading parameters and counters.


c        Pointer for packet time data (gets incremented by 1 on each call).
         trow(m) = 0

c        Current FITS row (gets incremented each time trow rolls over).
         if((iopt(2,m).gt.0).and.(extn.eq.1)) then
            frow(m) = iopt(2,m)
         else
            frow(m) = 1
         endif

c        Last FITS row to read from (stays fixed).
         if((iopt(3,m).gt.0).and.(extn.eq.1)) then
            lrow(m) = iopt(3,m)
         else
            lrow(m) = nrows(m)
         endif

         igti(m) = 1
         tgti(m) = 1

c If this is the first point of the first interval in an events file,
c return the start of the first GTI as dtime(m).  If no GTI's, return
c the first TIME from the file as the start time, saving the point to
c be processed on the next call.
c
c
         if((extn.eq.1).and.(irec(m).eq.0).and.itype(m).eq.1
     &      .and.iyrsta(m).eq.0 ) then
c Read the first photon
            CALL ftgcvd(lui(m),ivect(1,m),frow(m),1,1,nulvd,photfst(m)
     &                 ,anynul,ierr)
            IF(iext(2,m).ne.0.and.iopt(2,m).le.0) THEN
c read the first gti
               inext(1) = iext(1,m)
               inext(2) = iext(2,m)
               CALL xrftlgti(lui(m),inext,.true.,htunit(m),dtoffset(m)
     &                       ,2,1,tart,top,grsta,grsto,ndum(m))

c  Use Y as a flag to tell the calling routine what's going on.
               y(m) = -1.
               dtime(m) = grsta + dtzero(m)
             ELSE   
               cdum = ' ' 
               CALL xrdectun (cdum, ctunit(m), 2, photfst(m),ierr)
               dtime(m) = photfst(m) + dtzero(m) + dtoffset(m)
             ENDIF
             iyrsta(m) = 1
             irec(m) = 1
             return
         endif
      endif
c 
c Error return for the first call of the first point
      if(ierr.ne.0) then
         errm = subname//' '//'Error initializing file '//cfilin(m)
         call xaerror(errm, 5)
         eofflag=.true.
         goto 999
      endif


c End of first-call commands.
C Top of data fetching block (if point is rejected for some reason, we will
C return here and try for the next one.
 800  continue


c Read the current data point.

c     Case of first read in arrival time file with GTIs.

c     Advance the counters.
      eofflag=.false.
c      if(.not.(irec(m).eq.1).and.(trow(m).eq.0)) irec(m) = irec(m) + 1
c change as old
      if((irec(m).eq.1).and.(trow(m).eq.0)) goto 810
c
      irec(m) = irec(m) + 1
810   continue
c change end
      trow(m) = trow(m) + 1
      if(trow(m).gt.tmax(m)) then
         frow(m) = frow(m) + 1
         trow(m) = 1
      endif

c Section for GTIs.

      if(extn.eq.2) then
         if(tgti(m).gt.ngti(m)) then
c           GTIs all used up.  Set exposure to gap and close the file
            expos(m) = 0.
            y(m) = nulve
            sy(m) = nulve
            dtime(m) = dtime(m) + dtint(m)/86400.d0
            eofflag=.true.
            goto 999
         endif
         if((igti(m).gt.gtimax).and.(tgti(m).le.ngti(m))) then
c           Past last stored GTI, but more GTI's remain in the file:
c           Save latest GTI stop and do another read.
            gtiolast = gtio(gtimax,m)
            ierr = 0
            inext(1) = iext(1,m)
            inext(2) = iext(2,m)
            frow(m) = lcount(m)*gtimax + 1
            CALL xrftlgti(lui(m),inext,.false.,htunit(m),dtoffset(m)
     &                   ,gtimax,frow(m),start,stop,ddum,ddum1,ngti(m))
            if(ngti(m).eq.0) then
               eofflag=.true.
               goto 999
            endif
            gmax(m) = min(ngti(m) - frow(m) + 1,gtimax)
            DO i = 1, gmax(m)
               gtia(i,m) = start(i)
               gtio(i,m) = stop(i)
            ENDDO
            igti(m) = 1
            lcount(m) = lcount(m) + 1
         endif
         expos(m) = 1.
         y(m) = 1.
         sy(m) = 1.
         dtint(m)    = (gtio(igti(m),m) - gtia(igti(m)  ,m))*86400.d0
         dtime(m)    = (gtio(igti(m),m) + gtia(igti(m)  ,m))*0.5d0
         igti(m) = igti(m) + 1
         tgti(m) = tgti(m) + 1
         go to 999
      endif

c Section for RATE TABLEs and EXPOSURE.
 
c     Query for end-of-file.  If so, close the file.
      if(frow(m).gt.lrow(m)) then
         eofflag=.true.
         go to 999
      endif

c X value.
c
c Add dtrow because was failing for packet data when SD (shift days) option
c was applied. The current value need to be save for the next round when
c the column time for packet data is not read.
c 
      if(ivect(1,m).gt.0) then
         if(trow(m).eq.1) then
            CALL ftgcvd(lui(m),ivect(1,m),frow(m),1,1,nulvd,dtrow(m)
     &                 ,anynul,ierr)
            cdum = ' '
            CALL xrdectun(cdum,ctunit(m),2,dtrow(m),ierr)
            dtime(m) = dtrow(m) + dtzero(m) + dtoffset(m)
     &                          + dfoffset(m)*dtp(m)/86400.d0
            dtrow(m)=dtime(m)
         else
            dtime(m) = dtrow(m) + dtp(m)/86400.d0
            dtrow(m)=dtime(m)
         endif
      else
         dtime(m) = dtoffset(m) + dtzero(m) 
     &                          + dble(frow(m)-1)*dtint(m)/86400.d0
      endif

c Flag for bad photons (i.e., those outside Good Time Intervals).
c If the point is in the current GTI, nothing happens.


c     Make this test only if data are events.
      if((extn.eq.1).and.(itype(m).eq.1).and.(ivect(9,m).gt.0)) then
c     Read PHA
         CALL ftgcvj(lui(m),ivect(9,m),frow(m),       1,1
     &              ,nulvj,  pha   ,anynul,ierr)
c        Increment the bad photon counter if pha is outside the
c        specified values.  Then pretend this photon never happened and
c        go back up for the next one.
         if((pha.gt.phahi(m)).or.(pha.lt.phalo(m))) then
            nbad = nbad + 1
C     go get the next data point
            go to 800
         endif
      endif

c Make this test only if there is a GTI extension and data are events.

cc      if((extn.eq.1).and.(iext(2,m).gt.0).and.(itype(m).eq.1)) then
cc
c Check photon against GTIs.
cc
cc         CALL xrchkgti(lui(m),m,iext(1,m),newfile(m),.true.,htunit(m)
cc     &                ,dtoffset(m),dtime(m),inside)
cc         IF(.not.inside) nbad = nbad + 1
cc      endif

c     If this is an events file, return with dummy values.
      if(itype(m).eq.1) then
         y(m) = 1.
         sy(m) = 1.
         expos(m) = 1.
         go to 999
      endif
      
c Y value & Y-error.

      if(ivect(2,m).gt.0) then
c        Y is returned in counts per second.
         CALL xrfrdyco(lui(m),dtp(m),irec(m),iopt(1,m),ivect(1,m)
     &                ,frow(m),trow(m),crunit(m),naxis(m)
     &                ,naxes(1,m),eaxis(m),taxis(m)
     &                ,y(m),sy(m),anynul,ierr)
c        If any values were undefined in the file, the current point will
c        be taken as a gap.
         if(anynul) then
            nogap(m) = nogap(m) + 1
C     go get the next data point
            go to 800
         endif
         y (m) = y (m)*rdty (m)
         sy(m) = sy(m)*rdtsy(m)
      endif

c Exposure.

      if(ivect(6,m).gt.0) then

c Read exposure from the file column.

         CALL ftgcve(lui(m),ivect(6,m),frow(m),       1,1
     &              ,nulve,expos(m),anynul,ierr)
c         write(*,*)'ivect(6,m) expos(m)', ivect(6,m),expos(m)
      elseif(ivect(4,m).gt.0) then

c Read the Dead time column and construct the exposure from that.

         CALL ftgcve(lui(m),ivect(4,m),frow(m),       1,1
     &              ,nulve,deadcor ,anynul,ierr)
         expos(m) = vignet*deadcor
c         write(*,*)'ivect(4,m) expos(m)', expos(m)
      else

c Exposure gets set from header information.

         expos(m) = exposure(m)
c         write(*,*)'from header expos(m)',ivect(6,m), expos(m)
      endif

c     Delta time.
c     Convert to seconds
c
      if(ivect(5,m).gt.0) then
         cdum = ' '
         CALL ftgkns(lui(m),'TUNIT',ivect(5,m),1,cdum,idum,ierr)
         CALL ftgcvd(lui(m),ivect(5,m),frow(m),       1,1
     &              ,nulvd,dtint(m),anynul,ierr)
         idum = 1
         CALL xrdectun(cdum,idum,1,dtint(m),ierr)
      endif

c Done reading the current data point.

c End of section for RATE TABLEs and EXPOSURE.

999   continue

      if(eofflag) then
c Done reading all data from this file.

c     Close the file and set the flag.
         idum = 0
         CALL ftclos(lui(m),idum)
         CALL frelun(lui(m))
         irec(m) = -1
         newfile(m) = .false.
      endif

      return
      end

