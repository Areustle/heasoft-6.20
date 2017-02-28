      subroutine lcmathrdbkg(lub,ivect,iopt,dtime_in,dtint,dtp,
     &                   dtzero,dtoffset,ctunit,crunit,nrows,
     &                   naxis,naxes,eaxis,taxis,y,sy,
     &                   corapb,coraperb,corrb,correrb,
     &                   expob,allnul,ierr,ichat)

c ReaD BacKGround rates for program LCMATH.

c This routine supplies count rate information from the background file
c the time at the current bin in the input file.  It gets called by
c lcmathread.  This routine is used in much the same way as the routine
c xrgetexp to assemble the exposure in xronos.  the main difference is
c that whereas xronos assembles an interval of newbins in memory, lcmath
c takes bins straight from the input file, calling this routine once for
c original bin.

c All information regarding the structure of the background file is passed
c from the calling routine.

c The algorthm runs as follows:

c    First, it finds where the current time in the bg file is with respect to
c    the current bin in the input file.

c    IF the bg bin < the beginning of the input bin, the routine reads from
c    the bg file until the inequality is reversed, returning to the above test
c    on each read.

c    If the bg bin > the end of the input bin, the input bin is taken to
c    be in a gap in the background data.  The net rate will then be set to the 
c    gap value (undefined in the FITS file).

c    If any part of the bg bin overlaps with the input bin, the 
c    current background rate is included in the returned value.

c    If more than one background bin overlaps with the input bin, they are
c    averaged according to the amount of coverage in the returned value.

c The returned count rate is given in uncorrected counts/sec regardless 
c of how they are given in the input and bg files. The corrections that
c should be reapplied to recover original rate are returned, and the
c corrections that should be applied to fully correct the rates are
c also.

c This routine allows for a TIME axis in the RATE packet, although the
c current version of lcmath does not.

c Note also that this routine does not care about the bin size in the input
c file relative to that in the bg file.

c  I  lub      (i)  Lu of input file.
c  I  ivect    (i)  Vector of column numbers in background file
c  I  iopt     (i)  Array of option flags
c  I  dtime_in (d)  Current TIME value in the input file (days)
c  I  dtint    (d)  File integration time (seconds)
c  I  dtp      (d)  Delta-time in packets (seconds)
c  I  dtzero   (d)  Zero-point offset for TIME column (days)
c  I  dtoffset (d)  Internal time offset (days)
c  I  ctunit   (i)  Units on TIME column
c  I  crunit   (i)  units on RATE column
c  I  nrows    (i)  Number of rows in the file
c  I  naxis    (i)  Number of dimension in RATE column
c  I  naxes    (i)  Length of each axis in RATE column
c  I  eaxis    (i)  Energy axis in RATE column
c  I  taxis    (i)  Time axis in RATE column
c  O  y        (r)  Rate from input FITS file (counts/sec)
c  O  sy       (r)  Error in rate from input FITS file (counts/sec)
c  O  corapb   (r)  Weighted correction UN-applied to data
c  O  coraperb (r)  Weighted correction UN-applied to data errors
c  O  corrb    (r)  Weighted correction TO APPLY to data
c  O  correrb  (r)  Weighted correction TO APPLY to data errors
c  O  allnul   (l)  = .true. if the whole input bin fell in a background gap
c  O  ierr     (i)  error status

c Author:  eal  GSFC/HSTX,  March, 1994

      LOGICAL anynul,finished,first,syncwrnd,
     &        allnul,newrow,deadapp,vignapp
      
      character(1) cdum
      character(80) comm
      
      INTEGER nrows,ivect(10),crunit,ctunit,lub,iopt(*),
     &        ierr,frow,trow,tmax,naxis,naxes(*),eaxis,taxis
      
      DOUBLE PRECISION y,sy,yb,syb,rdty,rdtsy,rdtyap,rdtsyap,
     &                 deadc,vignet,expos,expoc,expob,deadcor
      
      DOUBLE PRECISION corapb,coraperb,corrb,correrb,
     &                 corap,coraper,corr,correr
      
      DOUBLE PRECISION dtime_in, dtime, dtzero, dtoffset, dtint( * ),
     &                 dtp, tr, tl, tbr, tbl, ontime, nulvd, tdiff,
     &                 ttol, ddum1

      REAL rd1, rd2, rd3, rd4, rd5
      
      data dtime, nulvd / -1.2d34, -1.2d34 /
      data first, syncwrnd, cdum / .true., .false., ' ' /

      data rd1, rd2, rd3, rd4, rd5 / 1.0, 1.0, 1.0, 1.0, 1.0 /

c Time tolerance - we seem to have round off errors around 1e-12 days
      data ttol / 5.0d-12 /

      data expoc / 1.0 /

      SAVE

      if(ierr.ne.0) return

      if(first) THEN
         first = .false.
         finished = .false.

c Number of times stored in each packet.

         IF(taxis.ge.1) THEN
            tmax = naxes(taxis)
         ELSE
            tmax = 1
         ENDIF

         IF(dtp.le.0.d0) dtp = dtint(2)

c Initialize counters.

         trow = 0
         frow = 1

c Initialize background bin time boundaries to very large and negative.

         tbl = -1.2d34
         tbr = -1.2d34

c Get deadtime and collimator corrections.
         CALL xrftgbdv( lub, deadapp, rd1, vignapp, rd2,
     &                  rd3, rd4, rd5 )
         deadc  = DBLE( rd1 )
         vignet = DBLE( rd2 )
         rdty   = DBLE( rd3 )
         rdtsy  = DBLE( rd4 )
         expos  = DBLE( rd5 )

c Get APPLIED corrections, so we can (re)unapply them 
         CALL lcmathgabdv( lub, deadapp, vignapp, rd1, rd2 )
         rdtyap  = DBLE( rd1 )
         rdtsyap = DBLE( rd2 )

      endif

      newrow = .false.
      allnul = .true.
      y = 0.
      sy = 0.
      ontime = 0.d0
      expob  = 0.d0
      corapb   = 0.d0
      coraperb = 0.d0
      corrb    = 0.d0
      correrb  = 0.d0
      
c            write(*,*)'dtime_in',dtime_in
      ddum1 = ( 0.5d0 * dtint( 1 ) ) / 86400.d0
      tl    = dtime_in - ddum1
      tr    = dtime_in + ddum1


100   CONTINUE
c >>> This bit assumes only one file, or that the last file has been read.<<<
      IF(finished) THEN
         anynul = .true.
         return
      ENDIF

c Locate the background bin time with respect to the input bin time.

c                   tl                  tr
c  _________________|_________x_________|___________________   Input
c     |             |     dtime_in  |   |
c     |                             |    
c    tbl                           tbr   
c  ___|______________x______________|______________x________   Background
c     |            dtime            |

 
c If input is ahead of the current bin, or if a new row is due to be
c read, read the next bin time and count rate.
c 
c      write(*,*)'1 tl, tr', tl, tr
c      write(*,*)'tl,tr,tbl,tbr',tl,tr,tbl,tbr
c
      IF((tbr.le.tl).or.newrow) THEN
c         write(*,*)'tbr.lt.tl',tbr,tl 
         trow = trow + 1 
         if(trow.gt.tmax) then
            frow = frow + 1
            trow = 1
         endif
         IF(frow.gt.nrows) THEN
            finished = .true.
            IF(allnul) THEN
               RETURN
            ELSE
               GOTO 200
            ENDIF
         ELSE
            IF(ivect(1).gt.0) THEN
               IF(trow.eq.1) THEN
                  CALL ftgcvd(lub,ivect(1),frow,1,1,nulvd,dtime
     &                       ,anynul,ierr)
                  CALL xrdectun(cdum,ctunit,2,dtime,ierr) 
c >>> Need to offset for centering in packet? <<<
                  dtime = dtime + dtzero + dtoffset
               ELSE
                  dtime = dtime + dtp/86400.d0 
               ENDIF 
               IF(ierr.ne.0) RETURN
            ELSE
               dtime = dtoffset + dtzero 
     &                          + dble(frow-1)*dtint(2)/86400.d0
            ENDIF
c            write(*,*)'dtime back',dtime,dtint(1),dtint(2)
            ddum1 = ( 0.5d0 * dtint( 2 ) ) / 86400.d0
            tbl   = dtime - ddum1
            tbr   = dtime + ddum1
         ENDIF

c Read in Y value for the current row (returned in counts/sec).

         anynul = .false.
         CALL xrfrdyco(lub,dtp,frow,iopt,ivect,frow,trow,crunit
     &                ,naxis,naxes,eaxis,taxis,rd1,rd2,anynul,ierr)
         yb  = DBLE( rd1 )
         syb = DBLE( rd2 )
         newrow = anynul

c Corrections up to this point
          corr    = rdty  / rdtyap
          correr  = rdtsy / rdtsyap
          corap   = rdtyap
          coraper = rdtsyap
          
c Dead time - if we have a DEADC column, then read it
          if ( ivect( 4 ) .gt. 0 ) then
              
              CALL ftgcvd( lub, ivect( 4 ), frow, 1, 1, 
     &                     nulvd, deadcor, anynul, ierr )
              
c If the DEADC column has been applied, then include this when 
c we unapply the corrections, and when we apply the total correction
              if ( deadapp ) then
                  corap   = corap   * deadcor
                  coraper = coraper * deadcor
              endif
              corr   = corr   / deadcor
              correr = correr / deadcor

          endif

c If no ERROR column, then use sqrt(y), where y is uncorrected counts
c          if ( ivect( 3 ) .eq. 0 ) then
c              syb = sqrt( yb * corap )
c          endif

c Read exposure column, if extant
          if ( ivect( 6 ) .gt. 0 ) then
              CALL ftgcvd( lub, ivect( 6 ), frow, 1, 1, 
     &                     nulvd, expoc, anynul, ierr )
              yb  = yb  * expoc
              syb = syb * expoc
c If no ERROR column, then use sqrt(y), where y is uncorrected counts
c apply any correction to this
              if ( ivect( 3 ) .eq. 0 ) then
                  syb = sqrt( yb * corap ) / (coraper*sqrt(dtint(2)))
              endif

           elseif ( ivect( 3 ) .eq. 0 ) then
              syb = sqrt( yb * corap ) / (coraper*sqrt(dtint(2)))
           endif
              
c 
c
c         write(*,*)'newrow',newrow
c
c If background is ahead of the current input bin, and no background bins have
c been processed on this call, this is a gap.  Otherwise finish
c processing the current input bin.
 
      ELSEIF(tbl.ge.tr) THEN
c         write(*,*)'tbl.ge.tr',tbl,tr
c >>> This would be the place to test for exposure.<<<
 
         IF(allnul) THEN
c            write(*,*)'here'
            RETURN
         ELSE
c            write(*,*)'here goto'
            GOTO 200
         ENDIF
 
c If any part of the background bin is inside the current input bin, 
c add on the current count rate  and erro (note that these values may 
c have been saved from a previous call).

      ELSE

c         write(*,*)'this time is ok'
          allnul = .false.
          if ( tbl .eq. tl ) then
              tbl = tl
          endif
                  
          if ( tbr .eq. tr ) then
              tbr = tr
          endif

c Warn about asynchronous light curves
          tdiff = max( abs( tbl - tl ), abs( tbr - tr ) )
          if ( .not. syncwrnd .and. tdiff .gt. ttol ) then
              comm = 'Light curves possibly asynchronous! '
     &            // 'Check output.'
              call xwarn( comm, ichat )
              syncwrnd = .true.
          endif

c Convert to counts and sum, unapplying any corrections applied.
          ddum1 = min(tr,tbr)-max(tl,tbl)
          expob = expob + expoc * ddum1
          y  =  y +   yb*ddum1*corap
          sy = sy + (syb*ddum1*coraper)**2
          ontime = ontime + ddum1
          
c get the total applied corrections (for re-applying later)
          corapb   = corapb   + yb*ddum1
          coraperb = coraperb + (syb*ddum1)**2

c get the total correction to apply (if docor)
          corrb   = corrb   + yb*ddum1*corap*corr
          correrb = correrb + (syb*ddum1*coraper*correr)**2
          
c          write(*,*)'ontime,ddum1,yb,syb',ontime,ddum1,y,sy
          IF(tbr.ge.tr) THEN
              GOTO 200      
          ELSE
              newrow = .true.
          ENDIF

      ENDIF

c Compare the new time to the time in the input file again.

      GOTO 100

200   CONTINUE

c Finish the quadrature on sy, and return to counts/sec.

      IF(ontime.ne.0.)  THEN
         if ( y .ne. 0. .and. corapb .ne. 0. ) then
             corapb   = y / corapb
             corrb    = corrb  / y
         else
             corapb   = 1.
             corrb    = 1.
         endif
         if ( sy .ne. 0. .and. coraperb .ne. 0. ) then
             coraperb = sqrt( sy / coraperb )
             correrb  = sqrt( correrb  / sy )
         else
             coraperb = 1.
             correrb  = 1.
         endif
         y  = y / ontime
         sy = sqrt( sy ) / ontime
         expob = expob / ontime
c         write(*,*)'yb,syb,corapb,corrb,coraperb,correrb',
c     &              y,sy,corapb,corrb,coraperb,correrb
      ELSE
         y        = 0.
         sy       = 0.
         corapb   = 1.
         coraperb = 1.
         corrb    = 1.
         correrb  = 1.
         expob    = 1.
         allnul   = .true.
      ENDIF

      RETURN
      END
