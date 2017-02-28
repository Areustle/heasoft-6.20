* $Id: extgetev.f,v 3.74 2016/02/22 17:34:38 kaa Exp $

      SUBROUTINE GETEVENTS(Infile1,Phasze,Pha,Imaxtime,Status,
     &                     Maxgtis1,Maxccd1,Gtis,Imaxgtis,Imaxccd,
     &                     Qinreg,Imszf1,Imszf2,Imagef,
     &                     Imagez,Imszh1,Imszh2,Imageh,Tmplun,Totpixh,
     &                     Totpixf,Mintime1,Imaxlc,Lc,Lcsize,Evlun,
     &                     Gtotals,Wmsums)

      IMPLICIT NONE

      INTEGER Imszf1, Imszf2, Imszh1, Imszh2, Imaxlc
      INTEGER Phasze, Maxgtis1, Maxccd1

      DOUBLE PRECISION Gtis(3,Maxgtis1,Maxccd1), Mintime1, Wmsums(9)

      REAL Imageh(Imszh1,Imszh2), Imagef(Imszf1,Imszf2)
      REAL Imagez(Imszf1,Imszf2)
      REAL Pha(Phasze)

      INTEGER Lc(Imaxlc), Gtotals(7)

      INTEGER Imaxtime, Imaxgtis(Maxccd1),Imaxccd, Lcsize
      INTEGER Status, Evlun, Tmplun, Totpixf, Totpixh

      LOGICAL Qinreg(0:Maxccd1-1)

      CHARACTER*(*) Infile1

c Getevents subroutine. This actually reads the events file, selects
c events and accumulates arrays.
c  Arguments :
c      Infile1        c           i: Filename
c      Phasze         i           i: Size of the spectrum array
c      Pha            r         i/r: Spectrum
c      Imaxtime       i         i/r: Number of output events
c      Status         i           r: Status   0==OK
c      Maxgti1        i           i: Size of the GTI array
c      Gtis           d           i: The GTIs
c      Imaxgti        i           i: The number of GTIs for each CCD
c      Maxccd1        i           i: The size of the Imaxgti array
c      Qinreg         l         i/r: True if a photon has been selected from
c                                    this CCD.
c      Imszf1         i           i: X-axis size of the image array
c      Imszf2         i           i: Y-axis size of the image array
c      Imagef         r           r: The image array
c      Imagez         r           r: The z-image array
c      Imszh1         i           i: X-axis size of the WMAP array
c      Imszh2         i           i: Y-axis size of the WMAP array
c      Imageh         r           r: The WMAP array
c      Tmplun         i           i: I/O unit for temporary output event file
c      Totpixh        i         i/r: Total number of counts in WMAP
c      Totpixf        i         i/r: Total number of counts in image
c      Mintime1       d           i: Start time for lightcurve
c      Imaxlc         i           i: Size of light curve array
c      Lc             i         i/r: Light curve array
c      Lcsize         i         i/r: Last non-zero bin in light curve array
c      Evlun          i           i: I/O unit for output event file
c      Gtotals        i         i/r: current totals of events processed
c      Wmsums         d         i/r: Sums accumulated to calculate relation
c                                    of image coordinates to detector 
c                                    coordinates

 
      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
      INCLUDE 'keys.inc'
      INCLUDE 'subkeys.inc'

C Arrays for reading in blocks of data

      DOUBLE PRECISION dtime(STRIDE), tpha(STRIDE), xf(STRIDE)
      DOUBLE PRECISION yf(STRIDE), xh(STRIDE), yh(STRIDE)
      DOUBLE PRECISION kval(STRIDE,MAXCOLS), zf(STRIDE), xff, yff
      DOUBLE PRECISION xf0, yf0, xh0, yh0, xhh, yhh, time0
      DOUBLE PRECISION crpix1(2), crval1(2), crdelt1(2), crota1
      DOUBLE PRECISION xsky, ysky

      INTEGER tgrade(STRIDE)
      INTEGER unit,i,j,k, blocksize, ibin, hdutype, nrows, tfields
      INTEGER ixh, iyh, ixf, iyf, xif, yif, zif, phai, gradei, timei
      INTEGER ntime ,stride1, xih , yih
      INTEGER totread , totgood , totreg , tottime , totphas, totgrade
      INTEGER fp, toget, rowsize1, totcut, plun
      INTEGER varidat , ntmp , ipha, nbin
      INTEGER kcol(MAXCOLS)
      INTEGER gticcd(0:MAXCCD-1)

      INTEGER ccdid, ccd(STRIDE)
 
      CHARACTER(20) extname, comment
      CHARACTER(255) wrtstr, contxt, filenm
      character(20) ttype(MAXCOLS), tunit(MAXCOLS)
      character(10) tform(MAXCOLS)
      character(10) ctype1(2), cname1(2), mform1(2), mtype1(2)
      character(4) coordt(2)
 
      LOGICAL anyf, doregion, wcsgood, qgood, dowmapsums

      INTEGER lenact
      LOGICAL select_region, select_phase, select_time
      LOGICAL is_good_grade
      EXTERNAL lenact, select_region, select_phase, select_time
      EXTERNAL is_good_grade

      character(40) taskname
      COMMON / task / taskname

* init the local variables.
 
      xf0=0.d0
      yf0=0.d0
      xh0=0.d0
      yh0=0.d0
      anyf = .FALSE.

      doregion = Regionfile.NE.' '

c Set the filename 

      filenm = Infile1(:lenact(infile1))

c Open the current event file

      Status = 0
      blocksize = 0
      CALL GETLUN(unit)
      CALL FTOPEN(unit,filenm,0,blocksize,Status)
      IF ( Status.NE.0 ) THEN
         wrtstr = ' File '//filenm(1:LENACT(filenm))//' not found'
         CALL fcerr(wrtstr)
         CALL fcerrm(status)
         CALL FRELUN(unit)
         RETURN
      ENDIF

      CALL FTMAHD(unit,1,hdutype,Status)
      wrtstr = 'Failed to go to first extension'
      IF ( Status .NE. 0 ) GOTO 150

*     Find the events table
 
      extname = ' '
      varidat = 0
      i = 2
      CALL UPC(Eventname)
      DO WHILE ( Status.EQ.0 .AND. extname.NE.Eventname )
         CALL FTMAHD(unit,i,hdutype,Status)
         IF ( hdutype.EQ.2 ) THEN
            CALL FTGHBN(unit,MAXCOLS,nrows,tfields,ttype,tform,tunit,
     &                  extname,varidat,Status)
            CALL UPC(extname)
         ENDIF
         i = i + 1
      ENDDO
 
      wrtstr = ' EVENTS table named '//Eventname//' not found'
      IF ( Status.NE.0 ) GOTO 150
 
      CALL FTGKYJ(unit,'naxis1',rowsize1,comment,Status)
      wrtstr = ' Failed to get NAXIS1 keyword'
      IF ( Status.NE.0 ) GOTO 150

c Find the column numbers for the event attributes that we need

      IF ( needener ) THEN
 
         CALL FTGCNO(unit,.FALSE.,Ecol,phai,Status)
         wrtstr = ' Column '//Ecol(1:LENACT(Ecol))//' Not found'
         IF ( Status.NE.0 ) GOTO 150

      ENDIF
 
      CALL FTGCNO(unit,.FALSE.,Tcol,timei,Status)
      wrtstr = ' Column '//Tcol(1:LENACT(Tcol))//' Not found'
      IF ( Status.NE.0 ) GOTO 150

      IF ( haveimco ) THEN

         CALL FTGCNO(unit,.FALSE.,Xcolf,xif,Status)
         wrtstr = ' Column '//Xcolf(1:LENACT(Xcolf))//' Not found'
         IF ( Status.NE.0 ) GOTO 150

         CALL FTGCNO(unit,.FALSE.,Ycolf,yif,Status)
         wrtstr = ' Column '//Ycolf(1:LENACT(Ycolf))//' Not found'
         IF ( Status.NE.0 ) GOTO 150

      ENDIF

      IF ( zimage ) THEN

         CALL FTGCNO(unit,.FALSE.,Zcolf,zif,Status)
         wrtstr = ' Column '//Zcolf(1:LENACT(Zcolf))//' Not found'
         IF ( Status.NE.0 ) GOTO 150

      ENDIF

      IF ( needwmco ) THEN

         CALL FTGCNO(unit,.FALSE.,Xcolh,xih,Status)
         wrtstr = ' Column '//Xcolh(1:LENACT(Xcolh))//' Not found'
         IF ( Status.NE.0 ) GOTO 150

         CALL FTGCNO(unit,.FALSE.,Ycolh,yih,Status)
         wrtstr = ' Column '//Ycolh(1:LENACT(Ycolh))//' Not found'
         IF ( Status.NE.0 ) GOTO 150

      ENDIF

      IF ( needgrade ) THEN
 
         CALL FTGCNO(unit,.FALSE.,Gcol,gradei,Status)
         wrtstr = ' Column '//Gcol(1:LENACT(Gcol))//' Not found'
         IF ( Status.NE.0 ) GOTO 150

      ENDIF
 
c Read the data subspace keywords and find the column containing the CCD IDs

      CALL GETSUBKEYS(unit,Status)

      CALL FTGCNO(unit,.FALSE.,ccol,ccdid,Status)
      wrtstr = ' Column '//ccol(1:LENACT(ccol))//' Not found'
      IF ( Status.NE.0 ) THEN
         ccdid = -1
         Status = 0
      ENDIF

      IF ( haveimco ) THEN

c Get the WCS info for this file

         CALL extwcs(unit, xif, yif, .TRUE., 'image', crpix1, crval1, 
     &        crdelt1, crota1, ctype1, cname1, mtype1, mform1,
     &        wcsgood, Status)
         wrtstr = 'Failed to read WCS data from current file'
         IF ( Status.NE.0 ) GOTO 150

c Set the coordtype used by the coordinate conversion routines

         coordt(1) = ctype1(1)(lenact(ctype1)-3:lenact(ctype1))
         coordt(2) = fctype(1)(lenact(fctype)-3:lenact(fctype))

c Assume it is a tangent plane if not set

         IF ( coordt(1) .NE. '-SIN' .AND. coordt(1) .NE. '-TAN' .AND.
     &        coordt(1) .NE. '-ARC' .AND. coordt(1) .NE. '-NCP' .AND.
     &        coordt(1) .NE. '-GLS' .AND. coordt(1) .NE. '-MER' .AND.
     &        coordt(1) .NE. '-AIT' ) coordt(1) = '-TAN'

         IF ( coordt(2) .NE. '-SIN' .AND. coordt(2) .NE. '-TAN' .AND.
     &        coordt(2) .NE. '-ARC' .AND. coordt(2) .NE. '-NCP' .AND.
     &        coordt(2) .NE. '-GLS' .AND. coordt(2) .NE. '-MER' .AND.
     &        coordt(2) .NE. '-AIT' ) coordt(2) = '-TAN'

      ENDIF

c Find the column numbers for any keys given as arguments to the event
c file name.

      DO i = 1, nkeys
         CALL FTGCNO(unit,.FALSE.,key(i),kcol(i),Status)
         wrtstr = ' Column '//key(i)(1:LENACT(key(i)))//' Not found'
         IF ( Status.NE.0 ) GOTO 150
      ENDDO

c modify the data subspace keywords by any keys given as input.

      CALL FINSUBKEYS(Status)
      wrtstr = ' Get subspace keywords error'
      IF ( Status .NE.0 )  GOTO 150

c Check for an error accessing the input FITS event file. If
c so write an error message and return

 150  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(wrtstr)
         Status = 0
         CALL FTCLOS(unit,Status)
         CALL frelun(unit)
         Status = -1
         RETURN
      ENDIF
 
*     Init the different selects
 
      totread = 0
      totgood = 0
      totreg = 0
      tottime = 0
      totphas = 0
      totgrade = 0
      totcut = 0
      ntime = 0
 
c If we are doing the WMAP fix then will need to know the centers of
c the images and WMAPS

      dowmapsums = buildwmap .AND. Wtmapfix .AND.
     &     .NOT.( (Xcolf.EQ.Xcolh).AND.(Ycolf.EQ.Ycolh) )

      IF ( dowmapsums ) THEN
         xf0 = (fbound(3) + fbound(1))/2.d0
         yf0 = (fbound(4) + fbound(2))/2.d0
         xh0 = (hbound(3) + hbound(1))/2.d0
         yh0 = (hbound(4) + hbound(2))/2.d0
      ENDIF

c If we are writing an events file then open the file used to store
c the pointers to events to be written out.

      plun = -1
      IF ( Evlun.NE.0 ) THEN
         CALL exdeltmp('extprt')
         CALL GETLUN(plun)
         wrtstr = 'extprt.tmp'
         CALL MAKEFNAME(wrtstr)
         CALL OPENWR(plun,wrtstr,'new','U',' ',0,0,status)
         IF ( status .NE. 0 ) THEN
            CALL fcerr(
     &       ' Problem opening extprt.tmp, events pointer file')
            CALL fcerrm(status)
            RETURN
         ENDIF
         ntmp = 0
      ENDIF

      wrtstr = ' Doing file: '//
     &           infile1(1:MIN(LENACT(infile1),len(wrtstr)-13))
      CALL fcecho(wrtstr)

* Calculate the blocksize for reading. 2880 is the size of a fits block.
 
      stride1 = 2880/rowsize1
      toget = MIN(stride1,nrows)

* Get any timezero keyword

      time0 = 0.d0
      CALL FTGKYD(unit, 'timezero', time0, wrtstr, Status)
      IF ( Status .NE. 0 ) THEN
         Status = 0
         time0 = 0.d0
      ENDIF

c convert time to be middle of frame (ie TIMEPIXR=0.5)

      IF ( timedel .GT. 0.0 ) time0 = time0 - (timepixr-0.5)*timedel

c Set up an array of pointers into the gti and imaxgti arrays for the CCD
c number. This avoids a search inside the main event loop

      IF ( imaxccd .GT. 1 ) THEN
         DO i = 0, MAXCCD-1
            gticcd(i) = 0
            DO j = 1, imaxccd
               IF ( abs(i-Gtis(3,1,j)) .LT. 0.05 ) gticcd(i) = j
            ENDDO
         ENDDO
      ELSE
         DO i = 0, MAXCCD-1
            gticcd(i) = 1
         ENDDO
      ENDIF

* Loop round the events in the input file. Read in blocks of size toget.

      fp = 0
      DO WHILE ( fp.LT.nrows )

c read the basic attributes - note that we read them most of them into 
c double arrays although in many cases these will be integers

         IF ( needener ) THEN 
            CALL FTGCVD(unit,phai,fp+1,1,toget,0.d0,tpha,anyf,Status)
         ENDIF
         IF ( haveimco ) THEN
            CALL FTGCVD(unit,xif,fp+1,1,toget,0.d0,xf,anyf,Status)
            CALL FTGCVD(unit,yif,fp+1,1,toget,0.d0,yf,anyf,Status)
         ENDIF
         IF ( zimage ) THEN
            CALL FTGCVD(unit,zif,fp+1,1,toget,0.d0,zf,anyf,Status)
         ENDIF
         IF ( needwmco ) THEN
            CALL FTGCVD(unit,xih,fp+1,1,toget,0.d0,xh,anyf,Status)
            CALL FTGCVD(unit,yih,fp+1,1,toget,0.d0,yh,anyf,Status)
         ENDIF
         CALL FTGCVD(unit,timei,fp+1,1,toget,Mintime1-999.0D0,dtime,
     &               anyf,Status)
         IF ( ccdid .GT. 0 ) THEN
            CALL FTGCVJ(unit,ccdid,fp+1,1,toget,1,ccd,anyf,Status)
         ELSE
            DO i = 1, toget
               ccd(i) = 1
            ENDDO
         ENDIF
         IF ( needgrade ) THEN 
            CALL FTGCVJ(unit,gradei,fp+1,1,toget,0,tgrade,anyf,Status)
         ENDIF
    
c now any specified as keys (this may be inefficient since some have
c just been read above - improve this code later)

         DO j = 1, nkeys
            CALL FTGCVD(unit,kcol(j),fp+1,1,toget,0.d0,kval(1,j),anyf,
     &                  Status)
         ENDDO

c loop round events in this block
 
         DO 300 i = 1 , toget
 
            CALL XCLOCK1(fp+i,nrows,10)
 
            totread = totread + 1

            dtime(i) = dtime(i) + time0

c Map image coordinates from WCS for this file to the WCS being used for
c the output (which may be different and is read from the first event file
c in the list). Map from pixels to sky then back to pixels

            IF ( haveimco ) THEN

               xff = xf(i)
               yff = yf(i)
               CALL FTWLDP(xf(i),yf(i),crval1(1),crval1(2),crpix1(1),
     &              crpix1(2),crdelt1(1),crdelt1(2),crota1,
     &              coordt(1),xsky,ysky,status)

               CALL FTXYPX(xsky,ysky,fcrval(1),fcrval(2),fcrpix(1),
     &              fcrpix(2),fcrdelt(1),fcrdelt(2),fcrota,
     &              coordt(2),xf(i),yf(i),status)

c If an error was found then just use the original position

               IF ( status .NE. 0 ) THEN
                  xf(i) = xff
                  yf(i) = yff
                  status = 0
               ENDIF

            ENDIF

c If we are building a WMAP in image coordinates then ensure the same
c transformation is used for the WMAP

            IF ( buildwmap .AND. xih .EQ. xif 
     &           .AND. yih .EQ. yif ) THEN
               xh(i) = xf(i)
               yh(i) = yf(i)
            ENDIF

c If we are going to have to fix up the WMAP then store the information
c to relate the image and WMAP coordinates.

            IF ( dowmapsums ) THEN
               xff = xf(i) - xf0
               yff = yf(i) - yf0
               xhh = xh(i) - xh0
               yhh = yh(i) - yh0
               Wmsums(1) = Wmsums(1) + xff
               Wmsums(2) = Wmsums(2) + yff
               Wmsums(3) = Wmsums(3) + xhh
               Wmsums(4) = Wmsums(4) + yhh
               Wmsums(5) = Wmsums(5) + xff*xhh
               Wmsums(6) = Wmsums(6) + xff*yhh
               Wmsums(7) = Wmsums(7) + yff*xhh
               Wmsums(8) = Wmsums(8) + yff*yhh
               Wmsums(9) = Wmsums(9) + 1.0
            ENDIF

c Test for in a valid region (in image coordinates !!)
c This is now performed using cfitsio filtering

cc            IF ( doregion ) THEN
cc               IF ( .NOT.select_region(
cc     &              NINT((xf(i)/xint+(binf-1)/2.-(fbound(1)-1)
cc     &                                  -(foffset(1)-1))/binf),
cc     &              NINT((yf(i)/yint+(binf-1)/2.-(fbound(2)-1)
cc     &                                  -(foffset(2)-1))/binf),1)
cc     &            ) THEN
cc                  totreg = totreg + 1
cc                 GOTO 300
cc               ENDIF
cc            ENDIF

c Test for a valid grade

            IF ( needgrade ) THEN 
               IF ( .NOT. is_good_grade(tgrade(i)) ) THEN
                  totgrade = totgrade + 1
                  GOTO 300
               ENDIF
            ENDIF
 
c Test for in a valid time interval - first find which set of GTIs
c to use for this event.

            k = gticcd(ccd(i))
            IF ( .NOT. select_time(dtime(i),timedel,Gtis(1,1,k),
     &           Maxgtis1,Imaxgtis(k),nbin,adjustgti) ) THEN
               tottime = tottime + 1
               GOTO 300
            ENDIF
 
c Test for in a valid phase bin

            IF ( dophase ) THEN 
               IF ( .NOT. select_phase(dtime(i)) ) THEN
                  totphas = totphas + 1
                  GOTO 300
               ENDIF
            ENDIF
 
c Test for one of the cuts based on event file name arguments. Slightly
c mess because we need to do an OR between all keys with the same name
c and AND between keys with different names.

            qgood = .FALSE.
            DO j = 1, nkeys

c If this a new key then set the qgood flag to false.

               IF ( j .GT. 1 ) THEN
                  IF ( key(j) .NE. key(j-1) ) qgood = .FALSE.
               ENDIF

c If the current value doesn't fall in the requested range

               IF ( (keyval(1,j) .NE. -99999.d0 .AND.
     &               kval(i,j) .LT. keyval(1,j)) .OR.
     &              (keyval(2,j) .NE. -99999.d0 .AND.
     &               kval(i,j) .GT. keyval(2,j)) ) THEN

c If this is the last key or this key is different from the next one and
c the value didn't fall in a previous range for this key then we can
c reject the event

                  IF ( ( j .EQ. nkeys .OR. 
     &                   key(j) .NE. key(j+1) ) .AND. 
     &                  .NOT.qgood ) THEN
                     totcut = totcut + 1
                     GOTO 300
                  ENDIF

c If the current value does fall in the requested range for this key then
c set the qgood flag to true so that we know not to reject the event if
c it does not fall in any remaining requested ranges for this key

               ELSE

                  qgood = .TRUE.

               ENDIF

            ENDDO

c The event has survived the selection
 
            totgood = totgood + 1

c Set the qinreg flag to true for the CCD from which this photon was detected.
c This tracks which CCDs were really included in the selected region.

            Qinreg(ccd(i)) = .TRUE.

* if we're writing an output events file get the event and write to
* the pointer file
 
            IF ( Evlun.NE.0 ) THEN
               WRITE (plun) fp + i
               ntmp = ntmp + 1
            ENDIF
 
c accumulate the spectrum. The check for array bounds is in case the
c user got the TLMIN and TLMAX wrong.

            IF ( buildspec ) THEN
               ipha = NINT(tpha(i)/specbin - ebound(1)/specbin) + 1
               IF ( ipha .GE. 1 .AND. ipha .LE. Phasze ) THEN
                  Pha(ipha) = Pha(ipha) + 1
               ENDIF
            ENDIF

* accumulate the light curve - note that dtime is defined to be the center time
* of the readout containing the event (ie TIMEPIXR=0.5).

            IF ( buildlc ) THEN

               ibin = INT((dtime(i) - Mintime1)/Binlc) + 1

               IF ( ibin .GT. 0 .AND. ibin.LE.Imaxlc ) THEN
                  Lc(ibin) = Lc(ibin) + 1
                  Lcsize = MAX(Lcsize,ibin)
               ENDIF

            ENDIF
 
c If we are writing a photon list then write to the file
 
            IF ( Tmplun.GT.1 ) THEN
 
               WRITE (Tmplun,'(1x,F30.16,2x,i5)') dtime(i) , 
     &                                            NINT(tpha(i))
               ntime = ntime + 1
 
            ENDIF
 
c Accumulate the histogram image. Again note the protection against
c incorrect TLMIN or TLMAX. If required then accumulate the sums used
c to determine the relation between the image and WMAP coordinates.

            IF ( buildwmap ) THEN 
               ixh = (NINT(xh(i))-(hbbox(1)-1)+binh-1)/binh
               iyh = (NINT(yh(i))-(hbbox(2)-1)+binh-1)/binh
               IF ( ixh .GE. 1 .AND. ixh .LE. Imszh1 .AND.
     &              iyh .GE. 1 .AND. iyh .LE. Imszh2 ) THEN
                  Imageh(ixh,iyh) = Imageh(ixh,iyh) + 1
               ENDIF
               Totpixh = Totpixh + 1
            ENDIF
 
c Accumulate the full image. Yet again protect against incorrect TLMIN
c or TLMAX.

            IF ( buildimage ) THEN 
               ixf = (NINT(xf(i)/xint)-(fbbox(1)-1)+binf-1)/binf
               iyf = (NINT(yf(i)/yint)-(fbbox(2)-1)+binf-1)/binf

               IF ( ixf .GE. 1 .AND. ixf .LE. Imszf1 .AND.
     &              iyf .GE. 1 .AND. iyf .LE. Imszf2 ) THEN
                  Imagef(ixf,iyf) = Imagef(ixf,iyf) + 1
                  IF ( zimage ) THEN
                     Imagez(ixf,iyf) = Imagez(ixf,iyf) 
     &                                 + SNGL(zf(i))
                  ENDIF
               ENDIF
               Totpixf = Totpixf + 1
            ENDIF
 
 300     CONTINUE
 
         fp = fp + toget
         toget = MIN(stride1,nrows-fp)
      ENDDO
 
      Imaxtime = ntime + Imaxtime - 1
 
      wrtstr = '          Total      Good    Bad: '//
     &         'Time     Phase     Grade       Cut'
      CALL fcecho(wrtstr)
      WRITE (wrtstr,'(7x,2(i8,2x),3x,4(i8,2x))') totread , totgood , 
     &       tottime , totphas , totgrade, totcut
      CALL fcecho(wrtstr)
 
      Gtotals(1) = totread + Gtotals(1)
      Gtotals(2) = totgood + Gtotals(2)
      Gtotals(3) = totreg + Gtotals(3)
      Gtotals(4) = tottime + Gtotals(4)
      Gtotals(5) = totphas + Gtotals(5)
      Gtotals(6) = totgrade + Gtotals(6)
      Gtotals(7) = totcut + Gtotals(7)

c Update the tstart and tstop values in case the GTIs have been adjusted

      IF ( adjustgti ) THEN
         DO i = 1, imaxccd
            IF ( Gtis(1,1,i) .LT. tstart ) tstart = Gtis(1,1,i)
            IF ( Gtis(2,Imaxgtis(i),i) .GT. tstop ) 
     &           tstop = Gtis(2,Imaxgtis(i),i)
         ENDDO
      ENDIF

c If required write the events into the output file
 
      IF ( Evlun .NE. 0 ) THEN
         CALL w_events(unit, Evlun, plun, status)
         contxt = 'GETEVENTS: failure in w_events'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

c Close the input file
 
      CALL FTCLOS(unit,Status)
      contxt = 'GETEVENTS: failed to close input file'
      IF ( status .NE. 0 ) GOTO 999
      CALL FRELUN(unit)
      IF ( plun.GT.0 ) THEN 
         CLOSE(plun)
         CALL FRELUN(plun)
         CALL exdeltmp('extprt')
      ENDIF

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF
 
      RETURN
      END


