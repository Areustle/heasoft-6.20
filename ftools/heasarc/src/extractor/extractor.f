c Routine to extract events/images/spectra/light curves from multiple
c event files with filtering on regions, times, pulse phases, values in
c columns in the event file.


      SUBROUTINE EXTRCT

      INTEGER ierr
      character(100) contxt

      character(40) taskname
      COMMON /task/ taskname
      taskname = 'extractor v5.31'
      contxt = taskname(1:15)//'     7 Sep 2016'
      CALL fcecho(contxt)

      CALL XCHATY(10,10)

      CALL GEXPAR(ierr)
      contxt = 'Failure in GEXPAR'
      IF ( ierr.NE.0 ) GOTO 999

      CALL EXTRACT_SUB(ierr)
      contxt = 'Failure in EXTRACT_SUB'
      IF ( ierr.NE.0 ) GOTO 999

      CALL CLIEEE
      CALL XWARN1PRINT

 999  CONTINUE
      IF ( ierr .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(ierr)
      ENDIF

      END
**==do_extract.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995
      SUBROUTINE DO_EXTRACT(Gtis,MAXGTIS1,Imaxgtis,MAXCCD1, Imaxccd,
     &                      gtihdunms,exposs,
     &                      Phasze, Pha, Chan, Imaxtime,
     &                      Status, Imszf1, Imszf2, Imagef, Imagez,
     &                      Imszh1, Imszh2, Imageh, Totpixh, Totpixf,
     &                      Imaxlc, Lc, Lcsize, Mintime1, Onefile, Area,
     &                      Qinreg)

      IMPLICIT NONE

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'

c Passed variables

      INTEGER Imszf1, Imszf2, Imszh1, Imszh2, Phasze, Imaxlc
      INTEGER MAXGTIS1,MAXCCD1

      
      DOUBLE PRECISION Gtis(3,MAXGTIS1,MAXCCD1), exposs(MAXCCD1)
      DOUBLE PRECISION Gti(3,MAXGTI)
      DOUBLE PRECISION Mintime1
      DOUBLE PRECISION Area

      REAL Imagef(Imszf1,Imszf2) , Imageh(Imszh1,Imszh2)
      REAL Imagez(Imszf1,Imszf2)
      REAL Pha(Phasze)

      INTEGER Chan(Phasze), Lc(Imaxlc)
      INTEGER Imaxgtis(MAXCCD1),Imaxccd  
      INTEGER Imaxtime , Totpixh , Totpixf , Lcsize, Status

      CHARACTER*(*) Onefile
      character(30) gtihdunms(MAXCCD)

      LOGICAL Qinreg(0:MAXCCD-1)

c Local variables

      DOUBLE PRECISION wmsums(9)

      INTEGER gtotals(7)
      INTEGER tmplun
      INTEGER i , j , j1, ier , evlun

      LOGICAL qexist

      character(255) infile1 , str, tmpstr
      character(80) outstr

      INTEGER lenact, exnevf
      LOGICAL select_region
      character(255) exevfl
      EXTERNAL lenact, exevfl, exnevf, select_region

c If an unbinned light curve is required then open the temporary file
c required to hold good events

      CALL EXDELTMP('extract')
      tmplun = -1
      IF ( Wunbinlc ) THEN
         CALL GETLUN(tmplun)
         str = 'extract.tmp'
         CALL MAKEFNAME(str)
         tmpstr = char(92)//str(:254)
         CALL OPENWR(tmplun,tmpstr,'new','U',' ',0,0,ier)
         IF ( ier.NE.0 ) THEN
            CALL XWARN1(' Temporary file not created, '//
     &                  'no light curve info',5)
            CLOSE (tmplun)
            CALL FRELUN(tmplun)
            tmplun = -1
         ENDIF
      ENDIF

c If an output events file is required then open it

      evlun = 0
      IF ( Eventsfile .NE. ' ' ) THEN
         CALL GETLUN(evlun)
         INQUIRE(file=Eventsfile(:lenact(Eventsfile)),exist=qexist)
         IF ( qexist .AND. clobber )
     &        CALL delfil(Eventsfile(:lenact(Eventsfile)))
         CALL FTINIT(evlun,Eventsfile,1,Status)
         IF ( Status.NE.0 ) THEN
            outstr = ' Cannot open events output file: '
     &               //Eventsfile(:lenact(Eventsfile))
            CALL fcerr(outstr)
            CALL fcerrm(Status)
            CALL frelun(evlun)
            evlun = 0
            Status = 0
         ENDIF
      ENDIF

c Initialize the output spectrum

      IF ( Buildspec ) THEN
         DO i = 1 , Phasze
            Pha(i) = 0.0
            Chan(i) = ebound(1)/specbin + i - 1
         ENDDO
      ENDIF

c Initialize the output image

      IF ( Buildimage ) THEN
         DO j = 1 , Imszf2
            DO i = 1 , Imszf1
               Imagef(i,j) = 0
            ENDDO
         ENDDO
      ENDIF

      IF ( zimage ) THEN
         DO j = 1 , Imszf2
            DO i = 1 , Imszf1
               Imagez(i,j) = 0
            ENDDO
         ENDDO
      ENDIF

c Initialize the output WMAP. If there is no region filter defined
c then we set wtmapfix to false since we don't have to worry about that.

      IF ( Buildwmap ) THEN

         DO i = 1 , Imszh2
            DO j = 1 , Imszh1
               Imageh(j,i) = 0
            ENDDO
         ENDDO

         IF ( regionfile .EQ. ' ' ) Wtmapfix = .FALSE.

      ENDIF

c Initialize the output light curve

      IF ( buildlc ) THEN
         DO 500 i = 1 , Imaxlc
            Lc(i) = 0
 500     CONTINUE
      ENDIF
      Lcsize = 0
      Imaxtime = 1

c Initialize the accumulators and flags

      Totpixh = 0
      Totpixf = 0

      DO i = 1 , 7
         gtotals(i) = 0
      ENDDO

      DO i = 1, 9
         wmsums(i) = 0.d0
      ENDDO

      DO i = 0, MAXCCD-1
         Qinreg(i) = .FALSE.
      ENDDO

c loop round the input event files extracting events

      DO i = 1, exnevf()

         infile1 = exevfl(i, .TRUE., ier)

         IF ( ier .EQ. 0 ) THEN
            Status = 0
            Onefile = exevfl(i, .FALSE., ier)
            CALL GETEVENTS(infile1,Phasze,Pha,Imaxtime,Status,
     &                     MAXGTIS,MAXCCD,Gtis,Imaxgtis,imaxccd,
     &                     Qinreg,Imszf1,Imszf2,Imagef,Imagez,
     &                     Imszh1,Imszh2,Imageh,tmplun,Totpixh,Totpixf,
     &                     Mintime1,Imaxlc,Lc,Lcsize,evlun,gtotals,
     &                     wmsums)
            WRITE(outstr,'(a,a)') 'Failure in GETEVENTS on file ',
     &              infile1(:MIN(lenact(infile1),len(outstr)-30))
            IF ( status .NE. 0 ) CALL fcecho(outstr)
         ENDIF

      ENDDO

c If there are multiple chips then set up a list of GTIs within the selected 
c chips and calculate the net exposure from that list. Also save individual
c exposures in the exposs array.

      IF ( imaxccd .GT. 1 ) THEN
         j1 = 0
         DO i = 1, imaxccd
            IF ( Qinreg(NINT(Gtis(3,1,i))) ) THEN
               CALL calc_exp(Gtis(1,1,i), Imaxgtis(i), MAXGTIS,
     &                       exposs(i))
               DO j = 1, Imaxgtis(i)
                  j1 = j1 + 1
                  Gti(1,j1) = Gtis(1,j,i)
                  Gti(2,j1) = Gtis(2,j,i)
                  Gti(3,j1) = Gtis(3,j,i)
               ENDDO
            ENDIF
         ENDDO
         CALL EXTMGTI(Gti, MAXGTI, j1, Status)
         CALL calc_exp(Gti, j1, MAXGTI, expos)
      ELSE
         CALL calc_exp(Gtis(1,1,1), Imaxgtis(1), MAXGTIS, expos)
         exposs(1) = expos
      ENDIF

c Write a summary

      CALL fcecho('============================================='//
     &            '==================================')
      CALL fcecho('    Grand Total      Good    Bad: '//
     &            'Time     Phase     Grade       Cut')
      WRITE (outstr,'(7x,2(i8,2x),3x,4(i8,2x))') gtotals(1), 
     &        gtotals(2), (gtotals(i), i=4,7)
      CALL fcecho(outstr)
      WRITE (outstr,'(a,1pg11.5,a)')
     &            '   in ', expos, ' seconds'
      CALL fcecho(outstr)

c Correct the data subspace keywords for the CCDs from which photons
c have actually been selected

      CALL corsubkeys(Qinreg, Status)
      IF ( Status .NE. 0 ) THEN
         CALL fcerr('Failure in CORSUBKEYS')
         WRITE(outstr, '(a,i4)') 'Error Status Returned : ', Status
         CALL fcerr(outstr)
         RETURN
      ENDIF

c If necessary, correct the wmap for the correct region selection

      IF ( buildwmap .AND. wtmapfix ) THEN
         CALL fixwmp(wmsums, Imszh1, Imszh2, Imageh, Status) 
         IF ( Status .NE. 0 ) THEN
            CALL fcerr('Failure in FIXWMP')
            WRITE(outstr, '(a,i4)') 'Error Status Returned : ', Status
            CALL fcerr(outstr)
            RETURN
         ENDIF
      ENDIF

c If we are building a map with a z-axis then calculate that and place
c the contents in imagef.

      IF ( zimage ) THEN
         DO j = 1, Imszf1
            DO i = 1, Imszf2
               IF ( Imagef(i,j) .GT. 0. ) THEN
                  Imagef(i,j) = Imagez(i,j) / Imagef(i,j)
               ENDIF
            ENDDO
         ENDDO
      ENDIF

c If we are writing out an event file then close out

      IF ( evlun .NE. 0 ) THEN

         CALL w_events_end(evlun, Imaxgtis, Imaxccd, Gtis, Gtihdunms, 
     &                     Exposs, Qinreg, Area, Status)
         IF ( Status .NE. 0 ) THEN
            CALL fcerr('Failure in W_EVENTS_END')
            CALL fcerrm(Status)
            RETURN
         ENDIF

      ENDIF

      IF ( tmplun.GT.0 ) THEN
         CLOSE (tmplun)
         CALL FRELUN(tmplun)
      ENDIF

      RETURN

      END
**==extconvdt.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995



      SUBROUTINE EXTCONVDT(Cdate,Ctime,Time)

      IMPLICIT NONE

*     convert from from cdate/ctime in character to time in 8byte binary
*     cdate is of the form dd/mm/yy or yyyy-mm-dd.
*     ctime is of the form hh:mm:ss

      CHARACTER*(*) Cdate , Ctime
      DOUBLE PRECISION Time
      character(80) errormsg
      integer iy, im, id, ih, imn, istat
      integer i1, i2, i3
      double precision ss

      INTEGER lenact
      EXTERNAL lenact

      istat = 0
      errormsg = ' '


      call fts2tm(Cdate,iy,im,id,ih,imn,ss,istat)
      IF ( im .LT. 1 .OR. im .GT. 12 .OR. id .LT. 1 .OR. 
     &     id .GT. 31 .OR. istat .NE. 0 ) GOTO 100
      

      IF ( lenact(Ctime) .NE. 0 ) THEN
         call fts2tm(Ctime,i1,i2,i3,ih,imn,ss,istat)
         IF ( istat .NE. 0 ) GOTO 200
      ENDIF

      IF ( ih .LT. 0 .OR. ih .GT. 23 .OR. im .LT. 0 .OR. 
     &     im .GT. 60 .OR. ss .LT. 0.d0 .OR. ss .GT. 60.d0 ) GOTO 200

      CALL CCALDJ(iy, im, id, Time, istat)
      IF ( istat .NE. 0 ) GOTO 100

      Time = Time + DBLE(ih/24.) + DBLE(im/24./60.) + ss/86400d0

      RETURN

 100  errormsg = ' Problem with date '//Cdate
      CALL XWARN1(errormsg,5)
      Time = 0
      RETURN
 200  errormsg = ' Problem with time '//Ctime
      CALL XWARN1(errormsg,5)
      Time = 0
      RETURN
      END
**==extract_sub.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995

      SUBROUTINE EXTRACT_SUB(status)

      IMPLICIT NONE

      INTEGER status

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
      INCLUDE 'keys.inc'

c Pointers to dynamically-allocated arrays

      INTEGER iimagef, iimagez, iimageh, iwmap, ipha, ichan, ilc

* gti is the TIME SELECTION gti intervals. gtic is a temporary array used
c to hold the gtis read in from the time selection file

      DOUBLE PRECISION gti(3,MAXGTI), gtic(3,MAXGTI)
      DOUBLE PRECISION gtiw(3,MAXGTI)
      DOUBLE PRECISION mintime1
      DOUBLE PRECISION gtis(3,MAXGTIS,MAXCCD),exposs(MAXCCD)
      DOUBLE PRECISION area

      INTEGER imaxlc, lcsize
      INTEGER bbox(4), fsize(2), hsize(2), phasze
      INTEGER imaxgti , imaxtime, imaxgtic
      INTEGER imaxgtis(MAXCCD),imaxccd,ccd(MAXCCD)
      INTEGER totpixh , totpixf
      INTEGER i , j , ier , tmplun

      CHARACTER(80) contxt
      CHARACTER(255) onefile, infile1, tmpstr
      character(30) gtihdunms(MAXCCD)

      LOGICAL gtiread, qexist, qinreg(0:MAXCCD-1)

      INTEGER lenact, exnevf, select_region_setup
      CHARACTER(255) exevfl
      LOGICAL select_phase_setup
      EXTERNAL exevfl, exnevf, lenact, select_phase_setup
      EXTERNAL select_region_setup

* Dynamic memory allocation stuff
C  the following MEM common block definition is in the system iraf77.inc file

      INCLUDE 'memblock.inc'

C     datatype gives a symbolic code for the data type, e.g.,  4 = Integer*4
C     1 is boolean
C     3 is short integer
C     4 is integer
C     5 is long integer
C     6 is single precision real
C     7 is double precision real
C     8 complex

c initialize pointers for dynamic arrays

      DATA iimagef, iimagez, iimageh, iwmap, ipha, ichan, ilc / 7*0 /

* init everything
* the big image arrays are inited later

      onefile = ' '
      status = 0
      ier = 0
      tmplun = 0
      lcsize = 0
      imaxtime = 0
      totpixh = 0
      totpixf = 0

      i = 0
      do j=1,2
         hsize(j)=0
      enddo

* done init
      CALL XCHATY(10,10)
      status = 0

c Read the event file name and split off any arguments specifying valid
c ranges of event attributes.

      CALL evprse(status)
      contxt = 'Failure in EVPRSE'
      IF ( status .NE. 0 ) GOTO 999

c Read the event file headers and set up the global variables

      CALL extsev(status)
      contxt = 'Failure in EXTSEV'
      IF ( status .NE. 0 ) GOTO 999

c make sure that area is set to some value because it will be written to
c NPIXSOU in output FITS files. If we have no image coordinates then
c presumably NPIXSOU is meaningless.

      area = 0.0
      IF ( haveimco ) THEN

c Set the bounding box for the output image (stored in extractor.inc)
c This is in unbinned image coordinates

         DO i = 1, 4
            fbbox(i) = fbound(i)
         ENDDO

c Set up the selection regions and modify the bounding box appropriately.

         area = (fbbox(3)-fbbox(1)+1)*(fbbox(4)-fbbox(2)+1)
         IF ( regionfile .NE. ' ' ) THEN
            
            ier = select_region_setup(regionfile, 1, fcrval, fcrpix, 
     &           fcrdelt, fcrota, fctype(1), area, 
     &           fbbox)

            IF ( ier .NE. 0 ) THEN
               CALL fcecho("Ignoring region filtering...")
               regionfile = ' '
            ENDIF

         ENDIF


         area = area/binf/binf

c Check whether any of the event filename arguments are Xcolf or Ycolf
c and if so modify the bounding box. Note that this does not change the
c area variable - should it ? Note that we modify keyval to take into
c account the case where X and Y are real coordinates (the modification
c shouldn't make any difference for integer coordinates).

         foffset(1) = 1
         foffset(2) = 1
         DO i = 1, nkeys
            IF ( key(i) .EQ. Xcolf ) THEN
               foffset(1) = INT(keyval(1,i))
               fbbox(1) = MAX(fbbox(1), INT(keyval(1,i)))
               IF ( keyval(2,i) .GE. 0. ) THEN
                  fbbox(3) = MIN(fbbox(3), INT(keyval(2,i)+0.999999))
               ELSE
                  fbbox(3) = MIN(fbbox(3), INT(keyval(2,i)-0.999999))
               ENDIF
               keyval(1,i) = keyval(1,i) - 0.5
               keyval(2,i) = keyval(2,i) + 0.5
            ELSEIF ( key(i) .EQ. Ycolf ) THEN
               foffset(2) = INT(keyval(1,i))
               fbbox(2) = MAX(fbbox(2), INT(keyval(1,i)))
               IF ( keyval(2,i) .GE. 0. ) THEN
                  fbbox(4) = MIN(fbbox(4), INT(keyval(2,i)+0.999999))
               ELSE
                  fbbox(4) = MIN(fbbox(4), INT(keyval(2,i)-0.999999))
               ENDIF
               keyval(1,i) = keyval(1,i) - 0.5
               keyval(2,i) = keyval(2,i) + 0.5
            ENDIF
         ENDDO

      ENDIF

c If the WMAP coordinates are the same as those used for the region
c selection then use that to set the WMAP bounding box (if required)

      IF ( buildwmap ) THEN

cc         IF ( (Xcolf.EQ.Xcolh) .AND. (Ycolf.EQ.Ycolh) ) THEN
cc
cc            DO i = 1, 4
cc               hbbox(i) = fbbox(i)
cc            ENDDO
cc
cc         ELSE

            DO i = 1, 4
               hbbox(i) = hbound(i)
            ENDDO

cc         ENDIF

         IF ( area .EQ. 0. ) THEN
            area = (hbbox(3)-hbbox(1)+1)*(hbbox(4)-hbbox(2)+1)/binh/binh
         ENDIF

c Check whether any of the event filename arguments are Xcolh or Ycolh
c and if so modify the bounding box. Modify keyval as per Xcolf and Ycolf
c unless Xcolf=Xcolh, Ycolf=Ycolh in which case we have already made the
c modification

         DO i = 1, nkeys
            IF ( key(i) .EQ. Xcolh ) THEN
               hbbox(1) = MAX(hbbox(1), INT(keyval(1,i)))
               IF ( keyval(2,i) .GE. 0. ) THEN
                  hbbox(3) = MIN(hbbox(3), INT(keyval(2,i)+0.999999))
               ELSE
                  hbbox(3) = MIN(hbbox(3), INT(keyval(2,i)-0.999999))
               ENDIF
               IF ( Xcolh .NE. Xcolf ) THEN
                  keyval(1,i) = keyval(1,i) - 0.5
                  keyval(2,i) = keyval(2,i) + 0.5
               ENDIF
            ELSEIF ( key(i) .EQ. Ycolh ) THEN
               hbbox(2) = MAX(hbbox(2), INT(keyval(1,i)))
               IF ( keyval(2,i) .GE. 0. ) THEN
                  hbbox(4) = MIN(hbbox(4), INT(keyval(2,i)+0.999999))
               ELSE
                  hbbox(4) = MIN(hbbox(4), INT(keyval(2,i)-0.999999))
               ENDIF
               IF ( Ycolh .NE. Ycolf ) THEN
                  keyval(1,i) = keyval(1,i) - 0.5
                  keyval(2,i) = keyval(2,i) + 0.5
               ENDIF
            ENDIF
         ENDDO

         hsize(1) = ( hbbox(3) - hbbox(1) + 1 ) / binh
         hsize(2) = ( hbbox(4) - hbbox(2) + 1 ) / binh

      ENDIF

c If the fullimage flag has been set then we write out the entire image
c so reset fbbox...

      IF ( fullimage ) THEN
         fbbox(1) = INT(fbound(1)/xint)
         fbbox(2) = INT(fbound(2)/yint)
         fbbox(3) = INT(fbound(3)/xint)
         fbbox(4) = INT(fbound(4)/yint)
      ENDIF         

      fsize(1) = ( fbbox(3) - fbbox(1) + 1 ) / binf
      fsize(2) = ( fbbox(4) - fbbox(2) + 1 ) / binf

c check for a null fbbox. This can occur if the user has passed in cuts on
c X and Y which do not intersect with the selected regions.

      IF ( fsize(1) .LT. 0 .OR. fsize(2) .LT. 0 ) THEN
         fsize(1) = 0
         fsize(2) = 0
         IF ( buildimage ) THEN
            CALL fcecho(
     & "Warning : no image will be created since the selected regions")
            CALL fcecho(
     & "lie outside the image cuts defined")
         ENDIF
      ENDIF

c set up the time selections

      Mintime1 = 9999999999.D0

c first get the gtis from all the input event files

      Imaxgti = 0
      DO i = 1 , MAXGTI
         gti(1,i) = 0.0
         gti(2,i) = 0.0
         gti(3,i) = 1.0
      ENDDO

      DO i = 1, exnevf()

         infile1 = exevfl(i, .FALSE., ier)

         IF ( ier .EQ. 0 ) THEN

            Status = 0
            CALL GETGTI(infile1,Gti,Maxgti,Imaxgti,
     &                  evgtinm,eventname,
     &                  5,Mintime1,MAXCCD,gtihdunms,gtihdunm,Status)
            contxt = 'Failure while getting GTIs from event files'
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

      ENDDO

c split Gti into sub gtis

      CALL SPLITGTI(Gti,Maxgti,Imaxgti,Gtis,Maxgtis,Imaxgtis,
     &              Ccd,Maxccd,imaxccd,Status)
      contxt = 'Failure while splitting gtis into sub gtis'
      IF ( Status .NE. 0 ) GOTO 999

c sort and merge the gtis from the event files

      DO i= 1, imaxccd
         CALL EXTMGTI(Gtis(1,1,i),Maxgtis,Imaxgtis(i),status)
      ENDDO
      contxt = 'Failure while sorting and merging gtis'
      IF ( Status .NE. 0 ) GOTO 999

c now add together the gtis from all the input time selection files

      Imaxgtic = 0
      DO i = 1, MAXGTI
         gtic(1,i) = 0.0d0
         gtic(2,i) = 0.0d0
      ENDDO
      Status = 0
      CALL ADDGTI(Gtic, Imaxgtic, Gtiw, tfilgtinm, gtiread, Status)
      contxt = 'Failure while adding gtis'
      IF ( Status .NE. 0 ) GOTO 999

c add in any event filename arguments involving the time

      DO i = 1, nkeys
         IF ( key(i) .EQ. Tcol ) THEN
            Imaxgtic = Imaxgtic + 1
            gtic(1,Imaxgtic) = keyval(1,i)
            gtic(2,Imaxgtic) = keyval(2,i)
         ENDIF
      ENDDO

c combine the gtis from the event file(s) with those from the
c time selection files

      DO i =1, imaxccd
         CALL FINGTI(Gtis(1,1,i),Imaxgtis(i),
     &               Gtic,Imaxgtic,Gtiw,Maxgtis,status)
         contxt =
     &  'Failure while merging event file GTIs and time selections'
         IF ( status .NE. 0 ) GOTO 999
      ENDDO

      imaxgti=0
      DO i = 1, Imaxccd
         DO j = 1, imaxgtis(i)
            IF ( Gtis(2,j,i) .GT. Gtis(1,j,i) ) THEN
               imaxgti=imaxgti+1
               Gti(1,imaxgti)=Gtis(1,j,i)
               Gti(2,imaxgti)=Gtis(2,j,i)
               Gti(3,imaxgti)=Gtis(3,j,i)
            ENDIF
         ENDDO
      ENDDO

      CALL EXTMGTI(Gti, Maxgti, Imaxgti, status)
      contxt =
     &  'Failure while merging event file GTIs and time selections'
      IF ( status .NE. 0 ) GOTO 999

c      DO i =1, imaxccd
c      Mintime1=min(Mintime1,Gtis(1,1,i))
c      ENDDO

c If a TIMEPIXR value has been set from the parameters then use that. If no value
c set from the parameter then use the value in file. If neither then use 0.0.

      IF ( tpixrpar .GT. -900.0 ) THEN
         timepixr = tpixrpar
         WRITE(contxt, *) 'Using TIMEPIXR value set from parameter of ', 
     &                    timepixr
         CALL fcecho(contxt)
      ELSE
         IF ( timepixr .LT. -900.0 ) timepixr = 0.0
      ENDIF

c if lcstart >= 0 then use this as the starting time

      IF ( lcstart .GE. 0.0 ) Mintime1 = lcstart

c if we did read in some files from addgti and they anded each other away,
c then we don't have any gti's

      IF ( gtiread  .AND. Imaxgtic .EQ. 0 ) THEN
        DO i = 1, Imaxccd
           Imaxgtis(i)=0
        ENDDO
      ENDIF
      Status = 0

c Initialize any phase selection

      dophase = select_phase_setup(mjdref, emjdrff, emjdrfi, status)
      contxt = 'Failure in select_phase_setup'
      IF ( status .NE. 0 ) GOTO 999

c Initialize any grade selection

      IF ( needgrade ) THEN
         CALL set_garray(gstring, gbound)
      ENDIF

c Set the start and stop time of the output data

      IF ( Imaxgti .GT. 0 ) THEN
         tstart = Gti(1,1)
         tstop = Gti(2,Imaxgti)
      ELSE
         tstart = 0.d0
         tstop = 0.d0
      ENDIF

c get the memory for the image and WMAP arrays if required (if arrays are
c not required then set them to size 1 to avoid undefined pointer problems).


      IF ( buildimage ) THEN
         CALL udmget(fsize(1)*fsize(2), 6, iimagef, status)
      ELSE
         CALL udmget(1, 6, iimagef, status)
      ENDIF
      contxt = 'Failed to get memory for imagef'
      IF ( status .NE. 0 ) GOTO 999

      IF ( zimage ) THEN
         CALL udmget(fsize(1)*fsize(2), 6, iimagez, status)
      ELSE
         CALL udmget(1, 6, iimagez, status)
      ENDIF
      contxt = 'Failed to get memory for imagez'
      IF ( status .NE. 0 ) GOTO 999

      IF ( buildwmap ) THEN
         CALL udmget(hsize(1)*hsize(2), 6, iimageh, status)
      ELSE
         CALL udmget(1, 6, iimageh, status)
      ENDIF
      contxt = 'Failed to get memory for imageh'
      IF ( status .NE. 0 ) GOTO 999

c if restrictions on the energy range were specified as arguments on the
c event file name then modify the ebounds

      DO i = 1, nkeys
         IF ( key(i) .EQ. Ecol ) THEN
            ebound(1) = MAX(ebound(1),NINT(keyval(1,i)))
            ebound(2) = MIN(ebound(2),NINT(keyval(2,i)))
         ENDIF
      ENDDO

c if required get the memory for the spectrum

      IF ( buildspec ) THEN
         phasze = INT((ebound(2)-ebound(1)+1)/specbin+0.99)
      ELSE
         phasze = 1
      ENDIF
      CALL udmget(phasze, 6, ipha, status)
      contxt = 'Failed to get memory for pha'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(phasze, 4, ichan, status)
      contxt = 'Failed to get memory for chan'
      IF ( status .NE. 0 ) GOTO 999

c if required get the memory for the lightcurve (this may be more than is
c required).


      IF ( buildlc .AND. imaxgti .GT. 0 ) THEN
         imaxlc = INT((gti(2,imaxgti)-mintime1)/binlc)+1
      ELSE
         imaxlc = 1
      ENDIF
      CALL udmget(imaxlc, 4, ilc, status)
      contxt = 'Failed to get memory for lc'
      IF ( status .NE. 0 ) GOTO 999

c do the extraction

      lcsize = 0
      CALL DO_EXTRACT(gtis, Maxgtis,imaxgtis,Maxccd,imaxccd,
     &                gtihdunms,exposs,phasze, 
     &                MEMR(ipha), MEMI(ichan),
     &                imaxtime, status, fsize(1), fsize(2),
     &                MEMR(iimagef), MEMR(iimagez), hsize(1), hsize(2),
     &                MEMR(iimageh), totpixh, totpixf, imaxlc,
     &                MEMI(ilc), lcsize, mintime1, onefile, area,
     &                qinreg)
      contxt = 'Failure in DO_EXTRACT'
      IF ( status .NE. 0 ) GOTO 999

      IF ( wunbinlc .AND. imaxtime .LE. 0 )
     &      CALL fcecho('Warning: No light curve data found')

      IF ( phafile .NE. ' ' .AND. ebound(2) .LT. 0 )
     &      CALL fcecho('Warning: No spectral data found')

      DO i=1,Imaxccd
         IF ( imaxgtis(i).LE.0 ) THEN
            imaxgtis(i) = 1
            gtis(1,1,i) = 0
            gtis(2,1,i) = 0
         ENDIF
      ENDDO

      IF ( imaxgti.LE.0 ) THEN
         imaxgti = 1
         gti(1,1) = 0
         gti(2,1) = 0
      ENDIF


c If required write the FITS format light curve

      IF ( fitsbinlc.NE.' ' ) THEN
         CALL W_FBLC(gti,imaxgti,gtis,imaxgtis,imaxccd,gtihdunms,
     &               exposs,qinreg,status,imaxlc,MEMI(ilc),lcsize,
     &               mintime1,onefile,area)
         contxt = 'Failure while writing FITS light curve file'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

c If required write the QDP format binned and/or unbinned light curves

      IF (  ( qdpfile.NE.' ' .OR. unbinlc.NE.' ' ) .AND.
     &      ( imaxtime.GT.0 .OR. imaxlc.GT.0 ) ) THEN
         CALL WRITE_QDP(imaxtime,gti,imaxgti,status,imaxlc,
     &                  MEMI(ilc),lcsize,mintime1)

         contxt = 'Failure while writing QDP light curve file'
         IF ( status .NE. 0 ) GOTO 999

      ENDIF

c and the spectrum file

      IF ( buildspec .AND. ebound(2).GT.0 ) THEN

c Get the actual size of the WMAP bounding box (if region filter coordinates
c were not the same as the WMAP coordinates we were not able to set this
c correctly before the extraction.)

         CALL bboxsz(hsize(1), hsize(2), MEMR(iimageh), wtmapfix, bbox)

c Get the memory for the WMAP output array

         CALL udmget((bbox(3)-bbox(1)+1)*(bbox(4)-bbox(2)+1), 6, iwmap,
     &               status)
         contxt = 'Failed to get memory for wmap array'
         IF ( status .NE. 0 ) GOTO 999

c Write out the PHA file

         CALL W_PHA(phasze, MEMR(ipha), MEMI(ichan), gtis, imaxgtis,
     &              imaxccd, gtihdunms, exposs, area, qinreg, hsize(1), 
     &              hsize(2), MEMR(iimageh),
     &              (bbox(3)-bbox(1)+1), (bbox(4)-bbox(2)+1),
     &              MEMR(iwmap), bbox, totpixh, onefile)

      ENDIF

c the image file ...

      IF ( buildimage ) THEN
         CALL W_IMAGE(gtis, imaxgtis, imaxccd, gtihdunms, exposs, 
     &                qinreg, fsize(1), fsize(2), MEMR(iimagef), 
     &                totpixf, onefile)
      ENDIF

c a text file giving the gtis used

      IF ( gtitxt.NE.' ' ) THEN
         CALL GETLUN(tmplun)
         INQUIRE(file=gtitxt(:lenact(gtitxt)),exist=qexist)
         IF ( qexist .AND. clobber )
     &         CALL delfil(gtitxt(:lenact(gtitxt)))
         tmpstr = char(92)//gtitxt(1:254)
         CALL OPENWR(tmplun,tmpstr,'new',' ','L',0,0,ier)
         IF ( ier.EQ.0 ) THEN
            DO i=1,Imaxccd
            DO 420 j = 1 , imaxgtis(i)
               WRITE (tmplun,'(1x,g23.16,2x,g23.16,2x,g23.16)'
     &                ,IOSTAT=ier) gtis(1,j,i)
     &                , gtis(2,j,i),gtis(3,j,i)
 420        CONTINUE
            ENDDO
            CLOSE (tmplun)
         ELSE
            CALL fcerr(' Could not open '//gtitxt//' for write')
            status = 1
            RETURN
         ENDIF
         CALL FRELUN(tmplun)
      ENDIF

c finally a xronos window file giving the gtis used

      IF ( xronoutfile.NE.' ' ) THEN
         DO i = 1, Imaxccd
            IF ( Qinreg(NINT(gtis(3,1,i))) ) THEN
               CALL XRONOUT(gtis(1,1,i), imaxgtis(i), MAXGTIS)
            ENDIF
         ENDDO
      ENDIF

      IF ( status.NE.0 ) THEN
         CALL fcerr('Output failed in EXTRACT_SUB')
         CALL fcerrm(status)
         GOTO 999
      ENDIF



      CALL EXDELTMP('extract')

 999  IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF

      RETURN

      END
**==stripfn.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995



      SUBROUTINE STRIPFN(Fname)

      IMPLICIT NONE

*
* removes directory from file fname
*
      CHARACTER*(*) Fname
      INTEGER i , LENACT

      i = LENACT(Fname)
      DO WHILE ( i.GT.1 .AND. Fname(i:i).NE.'/' .AND. Fname(i:i)
     &           .NE.']' )
         i = i - 1
      ENDDO
      IF ( i .EQ. 1 .AND. Fname(i:i).NE.'/' .AND. 
     &     Fname(i:i).NE.']' ) i = 0

      Fname = Fname(i+1:)

      RETURN
      END
**==xwarn1.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995



      SUBROUTINE XWARN1(Message,Prio)

      IMPLICIT NONE

      CHARACTER*(*) Message
      INTEGER Prio

      INTEGER maxwarn
      INTEGER i
      INTEGER LENACT

      INTEGER Lun , status

      INTEGER MAXMSG
      PARAMETER (MAXMSG=100)

      character(200) warnings(MAXMSG)

      character(40) taskname
      COMMON /task / taskname

      DATA maxwarn/0/

      SAVE maxwarn, warnings

      IF ( Prio .LE. 0 ) RETURN

      CALL XWARN(Message,Prio)

      maxwarn = MIN(maxwarn+1,MAXMSG)

      warnings(maxwarn) = Message

      RETURN

      ENTRY XWARN1PRINT


      IF ( maxwarn.EQ.0 ) RETURN
      CALL XWRITE(' ', 5)
      CALL XWRITE(
     &   'Warnings which occurred during this extractor run : ',5)
      DO 200 i = 1 , maxwarn
         CALL XWRITE(warnings(i)(1:LENACT(warnings(i))),5)
 200  CONTINUE
      CALL XWRITE(' ', 5)

      RETURN

      ENTRY XWARN1FITS(Lun)


      status = 0
      CALL FTPHIS(Lun,taskname,status)
      IF ( maxwarn.EQ.0 ) RETURN
      CALL FTPHIS(Lun,'Warnings which occured during this extractor run'
     &            ,status)
      DO 300 i = 1 , maxwarn
         CALL FTPHIS(Lun,warnings(i)(1:LENACT(warnings(i))),status)
 300  CONTINUE

      RETURN
      END
**==exterrstack.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995

      SUBROUTINE EXTERRSTACK

      IMPLICIT NONE

*
* prints out the fits error stack
*
      character(80) msg

      msg = 'a'

      CALL FTGMSG(msg)
      DO WHILE ( msg.NE.' ' )
         CALL XWRITE(msg,5)
         CALL FTGMSG(msg)
      ENDDO

      RETURN
      END
