C This subroutine takes the list of boxes, reads in the data from the FITS
C  files, and places them into a data array
C
C Author: Jesse S. Allen
C History:
C  Vers. 0.0   5 Dec 1994  First draft
C        0.9  22 Dec 1994  Handles channels individually (halves memory demand)
C        1.0   6 Jan 1995  Added error processing
C        1.1   5 Feb 1996  Modified to reject bad NOS side data 

       subroutine rdvbx(boxlist, numofboxes, chnum, mjdtime, 
     +            colcnt, bkgcolerr, long_lc, lat_lc, imax, nx, status)

C Common block declarations

       common /SOURCE/ begintime, endtime, long_src, lat_src, 
     +        searchrad, minflux, maxflux, maxerr, stimbin, 
     +        backopt, binopt, collim, spincheck, pointcheck, weight
       common /NAME/ sourcename
       
       logical collim, spincheck, pointcheck, weight
       integer backopt, binopt
       real long_src, lat_src, searchrad, minflux, maxflux, maxerr
       real stimbin
       double precision begintime, endtime
       character(16) sourcename

C Local variables

       logical anyf

       integer inunit, rwmode, blocksize, nhdu, hdutype, status
       integer stabflag, pntflag, ipos, chnum
       integer i, imax, row, numofrows, boxlist(144), numofboxes, nx
      
       real r
       real l_exp, b_exp, cnts, spinper, eradd, rawcnts
       real l_sc, b_sc, bkg(3), bkgvar(3)
       real colcnt(imax), bkgcolerr(imax), long_lc(imax), lat_lc(imax)

       double precision obstime, mjdtime(imax)

       character(20) fitsname
       character(30) errtext
       character(16) obs_id, telescop
       character(70) comment
       character(80) message


C Initialize variables which require it

       nx = 0
       inunit = 1
       rwmode = 0

C Loop through the boxes, terminating if too many points are read in

       do 1000 i = 1, numofboxes
          write(fitsname,'(''b'',I5.5,''.raw'')') boxlist(i)
          if (numofboxes .ge. 100) then
             write(message,'('' Reading box '',i3,'' of '', i3, 
     +            '' from '', a20)') i, numofboxes, fitsname
          else if (numofboxes .ge. 10) then 
             write(message,'('' Reading box '',i2,'' of '', i2, 
     +            '' from '', a20)') i, numofboxes, fitsname
          else
             write(message,'('' Reading box '',i1,'' of '', i1, 
     +            '' from '', a20)') i, numofboxes, fitsname
          endif
          call fcecho(message)
          call ftopen(inunit, fitsname, rwmode, blocksize, status)
          if (status .ne. 0) then
             write(message, '('' Error opening '', A20)') fitsname
             call fcecho(message)
             call ftgerr(status, errtext)
             write(message, '('' FITSIO reports: '', A30, 
     +            '' FITSIO status = '', i3)') errtext, status
             call fcecho(message)
             go to 999
          endif

C Check that the data file is a Vela 5B raw data file

          call ftgkys(inunit, 'OBS_ID', obs_id, comment, status)
          call ftgkys(inunit, 'TELESCOP', telescop, comment, status)
          if ((obs_id .ne. fitsname(2:6)) .or. 
     +        (telescop .ne. 'Vela 5B ')) then
             message = ' Attempted to read in an inappropriate file...'
             call fcecho(message)
             if (status .eq. 0) status = -100
             go to 999
          endif
          nhdu = 1
          call ftmrhd(inunit, nhdu, hdutype, status)
          if (hdutype .ne. 2) then
             message = ' File does not have a binary table...'
             call fcecho(message)
             if (status .eq. 0) status = -100
             go to 999
          endif

          call ftgkyj(inunit, 'NAXIS2', numofrows, comment, status)
          do 200 row = 1, numofrows
             call ftgcvd(inunit, 4,row,1,1,0.D0, obstime, anyf, status)
             call ftgcve(inunit, 5,row,1,1,0, l_exp, anyf, status)
             call ftgcve(inunit, 6,row,1,1,0, b_exp, anyf, status)
             call ftgcvj(inunit,10,row,1,1,0, ipos, anyf, status)
             call ftgcve(inunit,17,row,1,1,0, l_sc, anyf, status)
             call ftgcve(inunit,18,row,1,1,0, b_sc, anyf, status)
             call ftgcvj(inunit,19,row,1,1,0, stabflag, anyf, status)
             call ftgcvj(inunit,20,row,1,1,0, pntflag, anyf, status)
             call ftgcve(inunit,21,row,1,1,0, spinper, anyf, status)
             if (chnum .eq. 1) then
                call ftgcve(inunit, 7,row,1,1,0, cnts, anyf, status)
                call ftgcve(inunit, 8,row,1,3,0, bkg, anyf, status)
                call ftgcve(inunit, 9,row,1,3,0, bkgvar, anyf, status)
             else if (chnum .eq. 2) then
                call ftgcve(inunit,12,row,1,1,0, cnts, anyf, status)
                call ftgcve(inunit,13,row,1,3,0, bkg, anyf, status)
                call ftgcve(inunit,14,row,1,3,0, bkgvar, anyf, status)
             endif
             if (status .ne. 0) then
                call ftgmsg(message)
                call fcecho(message)
                write(message, '('' FITSIO status = '', i3)') status
                call fcerr(message)
                go to 999
             endif

             if ((spincheck) .and. (stabflag .ne. 0)) go to 200
             if ((pointcheck) .and. (pntflag .ne. 0)) go to 200
             if ((obstime .lt. begintime) .or. (obstime .gt. endtime))
     +          go to 200

C Process the counter, background, and variance data
C Reject points with zero counts and points with invalid background
C values (absolute value is 32.75 for sin and cos terms, 
C 327.5 for linear, or less than 0 for linear).  These values were
C entered for bad data in the NOS side of the original Vela 5B data
C processing.

 100         if ((cnts .eq. 0) .or. 
     +           (abs(bkg(1)) .ge. 32.74) .or. 
     +           (abs(bkg(2)) .ge. 32.74) .or. 
     +           (bkg(3) .ge. 327.5) .or. 
     +           (bkg(3) .lt. 0.0))
     +          go to 200

             nx = nx + 1
             if (nx .gt. imax) then
                nx = nx - 1
                write(message,'('' Data from detector channel '',
     +               i1, '' exceeds maximum array size '', i8, 
     +               ''; not all data processed'')') chnum, imax
                call fcecho(message)
                call ftclos(inunit, status)
                go to 2000
             endif
             mjdtime(nx) = obstime
             if ((nx .gt. 1) .and. (mjdtime(nx) .eq. mjdtime(nx-1)))
     +          nx = nx - 1
            if (collim) then
                call colresp(obstime, searchrad, r, l_exp, b_exp, 
     +               long_src, lat_src, spinper)
                if (r .eq. 0.0) then
                   nx = nx - 1
                   go to 200
                endif
             else              
                r = 1.0
             endif
             eradd = 0.0
             rawcnts = cnts
             call background(cnts, bkg, bkgvar, eradd, backopt,
     +            ipos, spinper)
             colcnt(nx) = cnts / r
             bkgcolerr(nx) = (rawcnts + eradd) / (r**2)
             long_lc(nx) = l_sc
             lat_lc(nx) = b_sc
             if ((colcnt(nx) .lt. minflux) .or. 
     +           (colcnt(nx) .gt. maxflux) .or. 
     +           (bkgcolerr(nx) .gt. maxerr))
     +          nx = nx - 1

 200      continue

 500      call ftclos(inunit, status)

 1000  continue
 2000  write(message,'('' Number of points in channel '', i1, 
     +                 '':'', i8)') chnum, nx
       call fcecho(message)

C Sort the data into time ordered arrays

       message = ' Sorting data into time-ordered arrays'
       call fcecho(message)
       call sortdata(mjdtime,colcnt,bkgcolerr,long_lc,lat_lc,nx)

C Perform barycenteric corrections to the data

       message = ' Calculating barycentric corrections'
       call fcecho(message)
       call barycenter(mjdtime, long_lc, lat_lc, nx)

C Notify the user if the start and/or stop times will not match the user
C given guidelines (VERY LIKELY!).

       if (mjdtime(1) .gt. begintime) then
          write(message,'('' First available data point at MJD '',
     +          F12.6, '', modifying TIME-OBS to match'')')
     +          mjdtime(1)
          call fcecho(message)
       endif
       if (mjdtime(nx) .lt. endtime) then
          write(message,'('' Last available data point at MJD '',
     +          F12.6, '', modifying TIME-END to match'')')
     +          mjdtime(nx)
          call fcecho(message)
       endif

 999   return

       end
