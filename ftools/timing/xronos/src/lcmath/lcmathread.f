      subroutine lcmathread(lui,lub,ichat,mi,ai,mb,ab,as,iopt,ivect,
     &          ivectb,nrows,dtp,dtint,dtzero,dtoffset,ctunit,crunit
     &          ,naxis,naxes,eaxis,taxis ,docor,err_mode,frow
     &                 ,trow,ynet,synet,anynul,finished,ierr)

c READ the current rate (or count) from an input fits file for program LCMATH.

c This routine reads one bin from the input file, applies any user-specified
c scaling and offset to the count rate, subtracts (or adds) the rate from
c the bg file, and by default applies any deadtime or vignetting correction
c specified in the input file.

c On the first call, (thus assuming only a single file to be processed),
c this routine checks to see if deadtime and vignetting have been applied,
c and what the correction values are, if present in the input files.

c    If corrections have been applied, it issues a warning to that effect.

c    If corrections have not been applied but their values are given, it
c    issues a warning that the net light curve will be corrected.

c    If the user requests that corrections not be applied, the net lightcurve
c    will be uncorrected.  If the input lightcurve was corrected
c    it will issue a warning that the net lightcurve is uncorrected.

c The general algorithm is as follows:

c    The routine retrieves the count rate (in counts/sec) for
c    the current row in the input file (using subroutine xrfrdyco, as
c    in xronos).  The error on the count rate is read if present or else
c    calculated as a poisson error.

c    The routine then calls subroutine lcmathrdbkg to get the mean 
c    uncorrected count rate, and correction to apply during the 
c    current integration time.

c    Next, it converts the "source" rate to counts and un-applies any 
c    corrections for deadtime or vignetting.

c    It then applies user-specified scaling and offsets to both input and
c    and background.

c    If docor is set, then the full corrections are applied to the data before
c    either adding or subtracting the light curves. 

c    If docor is not set, then the corrections that were previously applied
c    to the input light curves are re-applied before either adding or
c    subtracting the light curves.

c    Finally it converts back to counts/sec.

c This routine allows for a TIME axis in the RATE packet, although the
c current version of lcmath does not.

c Note also that this routine does not care about the bin size in the input
c file relative to that in the bg file.

c  I  lui      (i)  Lu of input file.
c  I  lub      (i)  Lu of background file.
c  I  ichat    (i)  chattiness
c  I  mi       (r)  scaling factor for input
c  I  ai       (r)  offset for input
c  I  mb       (r)  scaling factor for background
c  I  ab       (r)  offset for background
c  I  as       (l)  if .true. means add rather than subtract
c  I  iopt     (i)  Array of option flags
c  I  ivect    (i)  FITS column numbers for TIME, Y and SY in infile
c  I  ivectb   (i)  FITS column numbers for TIME, Y and SY in background file
c  I  nrows    (i)  Number of rows in each file
c  I  dtp      (d)  Delta-time in packets (seconds)
c  I  dtint    (d)  File integration times (seconds)
c  I  dtzero   (d)  Zero-point offset for TIME columns (days)
c  I  dtoffset (d)  Internal time offsets (days)
c  I  ctunit   (i)  Units on TIME columns
c  I  crunit   (i)  units on RATE column
c  I  naxis    (i)  Number of dimension in RATE column
c  I  naxes    (i)  Length of each axis in RATE column
c  I  eaxis    (i)  Energy axis in RATE column
c  I  taxis    (i)  Time axis in RATE column
c  I  docor    (l)  = TRUE if deadtime/vignet corrections will be applied
c  I  err_mode (i)  method for the error  
c  O  frow     (i)  Current row in the table
c  O  trow     (i)  Current row in the packet (=1 if no time dimension)
c  O  ynet     (r)  Net count rate (counts/sec)
c  O  synet    (r)  Net Error (counts/sec)
c  O  anynul   (l)  = .true. if null values were found in the file on the
c                   current time interval
c  O  finished (l)  = .true. when the last point has been read
c  O  ierr     (i)  error status

c Author:  eal  GSFC/HSTX,  March, 1994

      INTEGER     maxdim
      PARAMETER ( maxdim = 9 )
      
      LOGICAL finished, anynul, deadapp, vignapp, first, as, docor,
     &        deadappb, vignappb
      
      character(80) comm, cdum
      
      INTEGER*4 lui, lub, frow, trow, iopt(*), tmax,
     &          nrows(*), ivect(*), crunit(*), ctunit(*), ivectb(*),
     &          naxis(*), naxes(maxdim,*), eaxis(*), taxis(*),
     &          ierr, ichat, err_mode 
      
      DOUBLE PRECISION yi, syi, yb, syb, deadc, vignet,
     &                 expos, deadcor, deadcb, vignetb,
     &                 expoc, rdty, rdtsy, rdtyap, rdtsyap

      REAL ynet, synet, mi, ai, mb, ab
      DOUBLE PRECISION addi, addb, addie, addbe, ddum1, ddum2

      REAL rd1, rd2, rd3, rd4, rd5
      
c These are the applied corrections for source/error (corapi/coraperi), 
c bkg/error (corapb/coraperb), which will get un-applied before calcs
c are done. And the full corrections to apply if docor is set for 
c source/error (corri/correri), and bkg/error (corrb/correrb).
      DOUBLE PRECISION corapi, coraperi, corri, correri, 
     &                 corapb, coraperb, corrb, correrb, expob 
      
      DOUBLE PRECISION dtime,dtint(*),dtp(*),dtzero(*),dtoffset(*),
     &                 nulvd
      
      data first,cdum /.true.,' '/
      
      data expoc, deadcor, nulvd / 1., 1., -1.2d34 /
      data expob / 1. /
      
      data corrb, correrb, corapb, coraperb / 1., 1., 1., 1. /

      data rd1, rd2, rd3, rd4, rd5 / 1., 1., 1., 1., 1. /

      data ddum1, ddum2 / 0.0d0, 0.0d0 /
      
      SAVE


      IF(ierr.ne.0) RETURN

      IF(first) THEN
         first = .false.
         finished = .false.

c Number of times stored in each packet.

         IF(taxis(1).ge.1) THEN
            tmax = naxes(taxis(1),1)
         ELSE
            tmax = 1
         ENDIF

         IF(dtp(1).le.0.d0) dtp(1) = dtint(1)

c Get deadtime and collimator corrections.
c Convert corrections to double (even though they are read as REALs)
         CALL xrftgbdv( lub, deadappb, rd1, vignappb, rd2,
     &                  rd3, rd4, rd5 )
         deadcb  = DBLE( rd1 )
         vignetb = DBLE( rd2 )
         rdty    = DBLE( rd3 )
         rdtsy   = DBLE( rd4 )
         expos   = DBLE( rd5 )
         CALL xrftgbdv( lui, deadapp, rd1, vignapp, rd2,
     &                  rd3, rd4, rd5 )
         deadc  = DBLE( rd1 )
         vignet = DBLE( rd2 )
         rdty   = DBLE( rd3 )
         rdtsy  = DBLE( rd4 )
         expos  = DBLE( rd5 )

c Get APPLIED corrections, so we can (re)unapply them 
c Convert corrections to double (even though they are read as REALs)
         CALL lcmathgabdv( lui, deadapp, vignapp, rd1, rd2 )
         rdtyap  = DBLE( rd1 )
         rdtsyap = DBLE( rd2 )

c Speak a little about what's happnin'
         IF (docor) THEN
            comm = 'Net or Sum light curve will be corrected'
            CALL xwrite(comm,ichat)
         ELSEIF(deadappb.and..not.deadapp.and.deadc.ne.1.) THEN
            comm = 'Dead time corrections applied '
     &          // 'to background, not source!'
            CALL xwarn(comm,ichat)
         ELSEIF(deadapp.and..not.deadappb.and.deadcb.ne.1.) THEN
            comm = 'Dead time corrections applied '
     &          // 'to source, not background!'
            CALL xwarn(comm,ichat)
         ELSEIF(vignapp.and..not.vignappb.and.vignetb.ne.1.) THEN
            comm = 'Vignetting corrections applied '
     &          // 'to source, not background!'
            CALL xwarn(comm,ichat)
         ELSEIF(vignappb.and..not.vignapp.and.vignet.ne.1.) THEN
            comm = 'Vignetting corrections applied '
     &          // 'to background, not source!'
            CALL xwarn(comm,ichat)
         ELSEIF(deadc.ne.1..or.vignet.ne.1..or.
     &          deadcb.ne.1..or.vignetb.ne.1.) THEN
            comm = 'Net or Sum light curve will be uncorrected'
            CALL xwrite(comm,ichat)
         ENDIF

c Initialize counters.

         trow = 0
         frow = 1

      endif

c Read the current data point.

c Advance the counters.
 
      trow = trow + 1
      if(trow.gt.tmax) then
          frow = frow + 1
          trow = 1
      endif
      if(ichat.ne.0)call xclock(frow,nrows,5)
      if(frow.eq.nrows(1)) finished = .true.
 
c Read in time (returned in MJD).

      IF(ivect(1).gt.0) THEN
         IF(trow.eq.1) THEN
            CALL ftgcvd(lui,ivect(1),frow,1,1,nulvd,dtime,
     &                  anynul,ierr)
            CALL xrdectun(cdum,ctunit,2,dtime,ierr)
c >>> Need to offset for centering in packet? <<<
            dtime = dtime + dtzero(1) + dtoffset(1)
         ELSE
            dtime = dtime + dtp(1)/86400.d0
         ENDIF
         IF(ierr.ne.0) RETURN
      ELSE
         dtime = dtoffset(1) + dtzero(1) 
     &                       + dble(frow-1)*dtint(1)/86400.d0
      ENDIF

c Read in Y value & Y-error (returned in counts/sec).
c Convert rate and error to double (even though they are read as REALs)

      anynul = .false.
      CALL xrfrdyco(lui,dtp(1),frow,iopt,ivect,frow,trow,crunit(1),
     &              naxis(1),naxes(1,1),eaxis(1),taxis(1),rd1,rd2,
     &              anynul,ierr)
      yi  = DBLE( rd1 )
      syi = DBLE( rd2 )
c
c      write(*,*)'yi,syi,dtime',yi,syi,dtime
c

c Corrections up to this point
      corri    = rdty  / rdtyap
      correri  = rdtsy / rdtsyap
      corapi   = rdtyap
      coraperi = rdtsyap
      
c Dead time - if we have a DEADC column, then read it
      if ( deadapp .and. ivect( 4 ) .gt. 0 ) then
          
          CALL ftgcvd( lui, ivect( 4 ), frow, 1, 1, 
     &                 nulvd, deadcor, anynul, ierr )
          
c If the DEADC column has been applied, then include this when 
c we unapply the corrections, otherwise apply it when we apply them
          if ( deadapp ) then
              corapi   = corapi   * deadcor
              coraperi = coraperi * deadcor
          endif
          corri   = corri   / deadcor
          correri = correri / deadcor
      endif
          
c
c Exposure
      if((ivect(6).gt.0)) 
     &   CALL ftgcvd(lui,ivect(6),frow,1,1,nulvd,expoc,anynul,ierr)
c
c If any values were undefined in the file, the current point will
c be taken as a gap.

      if(ierr.ne.0) return
      if(anynul) return

      CALL lcmathrdbkg(lub,ivectb,iopt,dtime,dtint,dtp(2),
     &             dtzero(2),dtoffset(2),ctunit(2),crunit(2),nrows(2),
     &             naxis(2),naxes(1,2),eaxis(2),taxis(2),
     &             yb,syb,corapb,coraperb,corrb,correrb,expob,anynul,
     &             ierr,ichat)
      if(ierr.ne.0) return
c      write(*,*)'lc frow, anynul',frow,anynul
      if(anynul) return

c
c
c Case of rates:
c Feb 7  the above routines xrfrdyco and lcmathrdbkg always convert and return 
c the data in c/s either if the column contains count or count/s
c the calculation here has always to go back at count to properly make errors.
c 
c
         yi = yi*dtp(1)*expoc
         syi = syi*dtp(1)*expoc
c         write(*,*)'before de-corr',yi,syi,yb,syb,expoc,corapi,coraperi,
c     &   corri,correri,corapb,coraperb,corrb,correrb
c
c Unapply corrections.
c
         yi  = yi  * corapi
         syi = syi * coraperi
         yb  = yb  * dtp(1)
         syb = syb * dtp(1)
c         yb = yb*dtp(1)*expoc
c         syb = syb*dtp(1)*expoc
c
c         write(*,*)'after de-corr',yi,syi,yb,syb,expoc,corapi,coraperi,
c     &   corri,correri,corapb,coraperb,corrb,correrb
c
c
c Go the data from files restore in counts
c 4 modes for error should be available 
c 1- additive and multiplicative as photons
c error than are:
c y=mx + b       
c c=mx   err_c=sqrt(c)  err_b=sqrt(b)   
c err_y=sqrt(err_c*err_c + err_b*err_b)
c
c 2- additive and multiplicative as constant
c y=mx + b       
c err_y= m*err_x
c
c 3- additive as constant and multiplicative as photons
c y=mx + b    c=mx err_c=sqrt(c)   
c err_y= sqrt(c)
c
c 4- multiplicative as constant additive as photons
c y=mx + b    c=mx err_c=m*sqrt(x)  err_b=sqrt(b)     
c err_y= sqrt(err_c**2 + err_b**2)
c
c 5- calculate the error directly from the root of the final value
c this ignores completly the original (if any) error column 
c
c y=mx + b    
c err_y= sqrt(y)
c
c 6- The error on the subtracted lightcurve is equal to the error column
c of the first input file. The count rate is the difference 
c between the first and second 
c  
c yi and yb output ==> y= yi-yb
c err_y=(syi) 
c
c 7- calculate the error using only the error columns of the
c input files and maintain the count rate of the first input file
c non done yet (I am not sure if usefull) !
c   
c yi and yb output ==> y= yi
c err_y=sqrt((syi)**2 + (syb)**2) 
c
c This is needed if there is not an error column in the file 
c
c      write(*,*)'ivect(3)',ivect(3)  
      IF (ivect(3).EQ.0) THEN
         syi = sqrt(yi)
c         syb = sqrt(yb)
c         if(yi.eq.0)syi=1.
c         if(yb.eq.0)syb=1.
      ENDIF
c
c Rescaling and additive.
c Note the rescaling is in count but need to be corrected
c for the exposure in each bin 
c Here turn the additive in counts and calculate the sqrt 
c to be used later if the constant is mimic 'photons'
c
c Do the calculation as double precision.
      addi=DBLE( ai )*expoc
      addb=DBLE( ab )*expob
      addie=sqrt(abs(addi))
      addbe=sqrt(abs(addb))
c
c values for the two file with rescaling and additive
      yi = DBLE( mi )*yi + DBLE( ai )*expoc
      yb = DBLE( mb )*yb + DBLE( ab )*expob
c
c
c work out error for different mode

      IF (err_mode.eq.1) THEN 
          syi=sqrt(DBLE( mi )*syi*syi)  
          syb=sqrt(DBLE( mb )*syb*syb)  
      ELSEIF(err_mode.eq.2) THEN  
          syi=DBLE( mi )*syi
          syb=DBLE( mb )*syb
          addie=0.0
          addbe=0.0 
      ELSEIF(err_mode.eq.3) THEN  
          syi=sqrt(DBLE( mi )*syi*syi)  
          syb=sqrt(DBLE( mb )*syb*syb)  
          addie=0.0
          addbe=0.0 
      ELSEIF(err_mode.eq.4) THEN  
          syi=DBLE( mi )*syi
          syb=DBLE( mb )*syb
      ELSEIF(err_mode.eq.5)THEN
          syi=sqrt(abs(yi))
          syb=sqrt(abs(yb))
          addie=0.0
          addbe=0.0 
      ELSEIF(err_mode.eq.6)THEN
          syi=syi
          syb=0.0
          addie=0.0
          addbe=0.0 
      ENDIF    
c
c      write(*,*)'syi,syb,addie,addbe,synet',syi,syb,addie,addbe,synet 
c

c Apply the full corrections to the errors before quadraturing them
c only if docor. if !docor, reapply the original corrections before
c quadrature. Do the calculation as double precision.
      if ( docor ) then
          ddum2 = ( syi * correri / expoc )**2.0 +
     &            ( syb * correrb / expob )**2.0 +
     &            ( addie * correri / expoc )**2.0 + 
     &            ( addbe * correrb / expob )**2.0
      else
          ddum2 = ( syi / expoc / coraperi )**2.0 + 
     &            ( syb / expob / coraperb )**2.0 +
     &            ( addie / expoc / coraperi )**2.0 + 
     &            ( addbe / expob / coraperb )**2.0
      endif
      ddum2 = sqrt( ddum2 )
c
c      write(*,*)'syi,syb,addie,addbe,synet,ceri,cerb,caperi,caperb,'
c     &        //'corri,corrb,corapi,corapb', 
c     & syi,syb,addie,addbe,synet,correri,correrb,coraperi,coraperb,
c     & corri,corrb,corapi,corapb
c      
c Subtract (or add) background from (to) input to get the net counts.
c Do the calculation as double precision.
c 
      if ( as ) then
          if ( docor ) then
              ddum1 = yi * corri / expoc + yb * corrb / expob
          else
              ddum1 = yi / corapi / expoc + yb / corapb / expob
          endif
      else
          if ( docor ) then
              ddum1 = yi * corri / expoc - yb * corrb / expob
          else 
              ddum1 = yi / corapi / expoc - yb / corapb / expob
          endif
      endif

c
c (Re)apply corrections.
c
c      IF(docor) THEN
c         ynet  = ynet *rdty/deadcor
c         synet = synet*rdtsy/deadcor
c      ENDIF
c
c Convert (back) to counts/sec. and convert back to single precision
      ynet  = SNGL( ddum1 / dtp(1) )
      synet = SNGL( ddum2 / dtp(1) )
c      write(*,*)'ynet,synet,dtp(1)',ynet,synet,dtp(1)
      return
      end
