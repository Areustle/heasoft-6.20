C  This is a subroutine to read HEAO-1 A-2 raw data into FITS format
C
C Author: Jesse Allen
C History:
C  Version 0.0  23 May 1997
C          0.9  17 Jun 1997  Returns status = -99 at the end of data
C          0.91 16 Feb 1998  Handles HED only and MED only requests

      subroutine readrawfits(rawunit, medflag, hedflag, row, iday,imsec,
     +           elcomflag, allflag, clnflag, spinflag, eang, rang, 
     +           spra, spdec, yra, ydec, hdsc, status)

      implicit none

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables

      logical clnflag(4), elcomflag(4), anyf, medflag, hedflag

      integer*2 allflag, spinflag, hdsc(5,8,8,6)
      integer*2 ds5(8)
      integer commonunit, rawunit, medunit, hedunit
      integer iday, imsec, year, doy
      integer row, maxrow, status, i, j, k,column

      real eang(2), mcilwainl, mcilwainb(3), ertokm
      real spra(32), spdec(32), yra(32), ydec(32), rang(12)

      character(70) comment
      character(80) message

      parameter (ertokm = 6371.23)


      commonunit = rawunit
      if (medflag) then
         medunit = rawunit
c         if (hedflag) hedunit = rawunit(2)
      else if (hedflag) then
         hedunit = rawunit
      else
         message = 'No MED or HED raw files requested... ERROR ERROR!!'
         call xaerror(message,1)
         message = 'Prior error trapping should not have permitted the '
         call xaerror(message,1)
         message = 'user to request illegal detector numbers.  Please '
         call xaerror(message,1)
         message = 'contact the FTOOLS team at ftoolhelp@heasarc and '
         call xaerror(message,1)
         message = 'report this error message.'
         call xaerror(message,1)
         status = -50
         go to 999
      endif

      status = 0
      call ftgkyj(commonunit, 'NAXIS2', maxrow, comment, status)
      if (row .gt. maxrow) then
         status = -99
         return
      endif
      call ftgcvj(commonunit, 1, row, 1, 1, 0, year, anyf, status)
      call ftgcvj(commonunit, 2, row, 1, 1, 0, doy, anyf, status)
      if (year .eq. 1977) then
         iday = doy
      else if (year .eq. 1978) then
         iday = doy + 365
      else
        iday = doy + 730
      endif
      call ftgcvj(commonunit, 3, row, 1, 1, 0, imsec, anyf, status)
      call ftgcve(commonunit, 5, row, 1, 1, 0.0, mcilwainl, anyf,status)
      rang(5) = mcilwainl / ertokm
      call ftgcve(commonunit, 6, row, 1, 3, 0.0, mcilwainb, anyf,status)
      rang(6) = mcilwainb(1)
      rang(7) = mcilwainb(2)
      rang(8) = mcilwainb(3)
      call ftgcvi(commonunit, 7, row, 1, 1, 0, allflag, anyf, status)
      call ftgcvi(commonunit, 8, row, 1, 1, 0, spinflag, anyf,status)
      if (medflag) then 
         call ftgcl(medunit, 9, row, 1, 1, clnflag(1), status)
         call ftgcl(medunit, 10, row, 1, 1, elcomflag(1), status)
      else
         clnflag(1) = .true.
         elcomflag(1) = .false.
      endif
      call ftgcve(commonunit, 11, row, 1, 1, 0.0, rang(4), anyf, status)
      call ftgcve(commonunit, 12, row, 1, 2, 0.0, eang, anyf, status)
      call ftgcve(commonunit, 13, row, 1, 32, 0.0, spra, anyf, status)
      call ftgcve(commonunit, 14, row, 1, 32, 0.0, spdec, anyf, status)
      call ftgcve(commonunit, 15, row, 1, 32, 0.0, yra, anyf, status)
      call ftgcve(commonunit, 16, row, 1, 32, 0.0, ydec, anyf, status)
      do 40 j = 1,8
         column = j + 16
         if (medflag) call ftgcvi(medunit, column, row, 1, 8, 0, ds5, 
     +      anyf, status)
         do 30 k = 1,8
            if (medflag) then 
               hdsc(5,k,j,1) = ds5(k)
            else
               hdsc(5,k,j,1) = 0
            endif
 30      continue
 40   continue
      
      do 45 i = 2,4
         if (hedflag) then
            call ftgcl(hedunit, 9, row, i-1, 1, clnflag(i), status)
            call ftgcl(hedunit, 10, row, i-1, 1, elcomflag(i), status)
         else
            clnflag(i) = .true.
            elcomflag(i) = .false.
         endif
 45   continue
      do 100 i = 2,4
         do 90 j = 1,8
            column = j + (8*(i-2)) + 16
            if (hedflag) call ftgcvi(hedunit, column, row, 1, 8, 0, ds5, 
     +         anyf, status)
            do 80 k = 1,8
               if (hedflag) then
                  hdsc(5,k,j,i) = ds5(k)
               else
                  hdsc(5,k,j,i) = 0
               endif
 80         continue
 90      continue
 100  continue

 999  return

      end
