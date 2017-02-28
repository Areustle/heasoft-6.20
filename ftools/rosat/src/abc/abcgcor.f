      subroutine abcgcor(lub,extmax,bcext,timuncor,dtoffset,outside
     &                  ,timcor,ierr)

c Get a barycentrically CORrect time for program ABC.

c This routine makes an interpolation on the barycenter correction table
c that closely parallels the algorithm used in IRAF/PROS.

c On the first call the routine loads all rows from all extensions in the
c table into memory.  The current limit to the number of rows is 10000.

c >>> This routine should be rewritten to have a location in dynamic memory
c     passed to it, with the vectors hcor and huncor passed as dummy
c     arguments.  <<<

c On all calls, the routine makes a linear interpolation on the barycenter
c table.  Normally, ROSAT orbit files have their rows spaced 1 minute
c apart.  If an input time falls in a space between successive rows 
c in the table that are more than 2 miniues apart, or if the time falls 
c completely outside the range of the table, the logical variable outside is
c returned .true.

c The routine assumes the table is written in four column format, with
c the uncorrected time in JD -- Integer part in column 1 and
c fractional part in column 2; and the full corrected time in columns 3 and 4.

c  I  lub      (i)  Lu of correction table file.
c  I  extmax   (i)  Dimension of bcext
c  I  bcext    (i)  Array of FITS extension numbers for TIMEREF extensions
c  I  timuncor (d)  Input uncorrected time in JD - dtoffset
c  I  dtoffset (d)  Internal offset to retain maximum accuracy
c  O  outside  (l)  = .true. of input time fell in a gap in the table.
c  O  timcor   (d)  Output corrected time in JD - dtoffset
c  O  ierr     (i)  error status

c subroutines called:  ftmahd, ftghbn, ftgkyd, ftgcvj, rmvlbk, ftgcvd

c Author:  eal  GSFC/HSTX,  February, 1994
c Revised: eal              April 1994 to do linear instead of spline
c                                      interpolation and to flag for gaps
c Revised: ng               Feb.  1998 Add the arrays to hold the linear
c                                 interpolation table. The table will be
c                                 calculated only once when this routine
c                                 is invoked at first time.
c                                 Fixed a bug in calculating the
c                                 intercept of the linear interpolation 
c                                 relation. This bug will cause loss
C                                 of the precision.
C                                 
c                                 
c                                  

      implicit none
      Integer imax, extmax, cmax
      parameter (imax = 10000, cmax = 100)
      logical first,anynul,outside
      character(16) ttype(cmax),tform(cmax),tunit(cmax),extname,routine
      character(80) errm
      INTEGER*4 ierr,lub,nrows,bcext(*),frow,j,i,xtend
     &   ,ctunit,iopt(10),ivect(10),idum,itype,htunit,nulvj
     &   ,jdi,jdo,jmax,pcount,nfield,ftstat,idum1
c 
c     cor_a: slope of the linear interpolation  
c     cor_b: intercept of the linear interpolation. 
c 
      double precision  cor_a(imax),cor_b(imax)
      integer*4   tcor_int(2),tuncor_int(2)
      double precision tcor_frac(2),tuncor_frac(2)
      double precision off_frac 
      integer*4  off_int
      DOUBLE PRECISION hcor(imax),huncor(imax),ddum
     &   ,timuncor,timcor,dtzero,dtoffset,start,stop,onemin
c     &   ,frci,frco,nulvd,a,b,twomin,ddum1,ddum2,ddum3,ddum4
     &   ,frci,frco,nulvd,twomin,ddum1,ddum2,ddum3,ddum4
      data nulvj,nulvd /0,0.d0/
      data routine /'abcgcor'/
      data first /.true./

      SAVE

      if(ierr.ne.0) return
      outside = .false.

c First-call set-ups.

      IF(first) THEN

         first = .false.
         twomin = 120.d0/86400.d0
         onemin =  60.d0/86400.d0

	 off_int = dtoffset/1.0 
	 off_frac = dtoffset - dble(off_int)
c Read in every row in the file.

         i = 0

c Loop over extensions.

         do j = 1, extmax

c Read header information.

            if(bcext(j).gt.0) then
               CALL ftmahd(lub,bcext(j),xtend,ftstat)

c Read essential keywords.

               CALL ftghbn(lub,cmax,nrows,nfield,ttype,tform,tunit
     &                    ,extname,pcount,ftstat)

c File type is timeref

               itype = 6

c Get timing keywords.

               CALL xrftgtky(lub,0,iopt,nrows,itype,ivect,htunit,ctunit
     &                      ,dtzero,ddum,ddum1,ddum2,ddum3,idum,ddum4
     &                      ,idum1,ierr)

c Loop over rows.

               frow = 1
               do while (frow.le.nrows)
                  i = i + 1

c Read in a pair of times.

                  CALL ftgcvj(lub,1,frow,1,1,nulvj,jdi ,anynul,ftstat)
                  CALL ftgcvd(lub,2,frow,1,1,nulvd,frci,anynul,ftstat)
                  CALL ftgcvj(lub,3,frow,1,1,nulvj,jdo ,anynul,ftstat)
                  CALL ftgcvd(lub,4,frow,1,1,nulvd,frco,anynul,ftstat)

c# c Fix the offset to be the first integer part in the file.
c# 
c#                   if(i.eq.1) dtoffset = dble(jdi)
                  huncor(i) = frci + (dble(jdi) - dtoffset)
                  hcor  (i) = frco + (dble(jdo) - dtoffset)
c
c Calculate the linear interpolation coefficients table. the tuncor_int,
c tuncor_frac, tcor_int,tcor_frac are the working arrays.
c 
                  if( i.eq.1) then 
		      tuncor_int(1) =   jdi - off_int
		      tuncor_frac(1) =  frci - off_frac
		      if(tuncor_frac(1).ge.1.0) then 
			  tuncor_frac(1) = tuncor_frac(1) - 1.0
			  tuncor_int(1) = tuncor_int(1) + 1
                      endif
		      if(tuncor_frac(1).le.-1.0) then 
			  tuncor_frac(1) = tuncor_frac(1) + 1.0
			  tuncor_int(1) = tuncor_int(1) - 1
                      endif
		      tcor_int(1) =   jdo - off_int
		      tcor_frac(1) =  frco - off_frac
		      if(tcor_frac(1).ge.1.0) then 
			  tcor_frac(1) = tcor_frac(1) - 1.0
			  tcor_int(1)  = tcor_int(1) + 1
                      endif
		      if(tcor_frac(1).le.-1.0) then 
			  tcor_frac(1) = tcor_frac(1) + 1.0
			  tcor_int(1) = tcor_int(1) - 1
                      endif
                  else
		      tuncor_int(2) =   jdi - off_int
		      tuncor_frac(2) =  frci - off_frac
		      if(tuncor_frac(2).ge.1.0) then 
			  tuncor_frac(2) = tuncor_frac(2) - 1.0
			  tuncor_int(2) = tuncor_int(2) + 1
                      endif
		      if(tuncor_frac(2).le.-1.0) then 
			  tuncor_frac(2) = tuncor_frac(2) + 1.0
			  tuncor_int(2) = tuncor_int(2) - 1
                      endif
		      tcor_int(2) =   jdo - off_int
		      tcor_frac(2) =  frco - off_frac
		      if(tcor_frac(2).ge.1.0) then 
			  tcor_frac(2) = tcor_frac(2) - 1.0
			  tcor_int(2)  = tcor_int(2) + 1
                      endif
		      if(tcor_frac(2).le.-1.0) then 
			  tcor_frac(2) = tcor_frac(2) + 1.0
			  tcor_int(2) = tcor_int(2) - 1
                      endif
		      call linear_inter
     *                   (tcor_int,tcor_frac,
     *                    tuncor_int,tuncor_frac,
     *                    cor_a(i-1),cor_b(i-1),ierr)
		      if(ierr.gt.0)  then 
                         errm = 'abcgcor: '
			 errm = errm // 'In bary table,'
			 errm = errm // ' there are inconsistencies.'
                         CALL xaerror(errm,1)
			 errm = ' Two entries having same '
			 errm = errm // ' uncorrected time  '
			 errm = errm // ' but different corrected '
			 errm = errm // ' times.'
                         CALL xaerror(errm,1)
			 ierr = 0
                      endif 
                      tcor_int(1)    =   tcor_int(2)
                      tcor_frac(1)   =   tcor_frac(2)
                      tuncor_int(1)  =   tuncor_int(2)
                      tuncor_frac(1) =   tuncor_frac(2)
                  endif

                  if(i.gt.imax) go to 920
                  frow = frow + 1
                  if(i.eq.1) start = huncor(i)
                             stop  = huncor(i)
               enddo
            endif
         enddo

         jmax = i

c# c Set up the spline interpolation table
c# 
c#          CALL splin(huncor, hcor, jmax , 1.1d30, 1.1d30, dinterp)

c End of first-call set-ups.

      endif

c Flag for input time not in the table.

c     IRAF/PROS makes this test.
      if(timuncor.lt.start) then
         j = 1
c        IRAF/PROS does NOT make this test.
         if(start - timuncor .gt. onemin) outside = .true.
         goto 10
c     IRAF/PROS makes this test.
      elseif(timuncor.gt.stop) then
         j = jmax - 1
c        IRAF/PROS does NOT make this test.
         if(timuncor - stop  .gt. onemin) outside = .true.
         goto 10
      endif

      CALL hunt(huncor,jmax,timuncor,j)

c Flag for input times falling in gaps in the table.
c IRAF PROS does not make these tests.

c     More than a two-minute gap:
      IF(huncor(j+1)-huncor(j).gt.twomin) THEN
c        More than one minute away from a table entry in that gap:
         IF((dabs(timuncor - huncor(j  )).gt.onemin) .and.
     &      (dabs(timuncor - huncor(j+1)).gt.onemin)) THEN
            outside = .true.
            IF(huncor(j+1) - timuncor .gt. timuncor - huncor(j)) THEN
               j = j - 1
            ELSE
               j = j + 1
            ENDIF
         ENDIF
      ENDIF

10    continue

c Calculate the linear interpolation coefficients.

c      a = huncor(j) - huncor(j+1)
c      b = (huncor(j)*hcor(j+1) - huncor(j+1)*hcor(j))/a
c      a = (hcor  (j) - hcor  (j+1))/a

c Do the correction.

c      timcor = a*timuncor + b
      timcor = cor_a(j)*timuncor + cor_b(j)

c#       CALL splint(huncor, hcor, dinterp, jmax, timuncor, timcor)

      if(ftstat.ne.0) THEN
         ierr = ftstat
         write(errm,*) 'abcgcor: fitsio error ',ftstat
         CALL xaerror(errm,1)
      endif

c Normal return

      return

920   continue
      ierr = -1055
      errm = 'abcgcor: Not enough memory allocated for barycenter table'
      CALL xaerror(errm,1)
      return
      end


      subroutine linear_inter(y_int,y_frac, 
     *                        x_int,x_frac,
     *                        a,b,ierr)
c
c     Given two points (x, y ), find the slope a and intercept b.
C     It tries to preserve the accuracy of the a and b. 
c
      double precision x_frac(2), y_frac(2)
      integer*4   x_int(2), y_int(2)
      double precision a,b
      double precision a1,a2,b1,b2,b3
      integer*4 x1,x2
      double precision v2,w1,w2

      ierr = 0

C
C     for y = a * x + b between (x1, y1) and (x2,y2)
C            
C     a = (y1 - y2) / (x1 - x2)
C     b =  (x1 * y2 - x2 * y1)/(x2-x2)

C     where x = x_int + x_frac
C           y = y_int + x_frac
C
      x1 = x_int(1) - x_int(2)
      a1 = x_frac(1) - x_frac(2) + dble(x1) 
      x2 = y_int(1) - y_int(2)
      a2 = y_frac(1) - y_frac(2) + dble(x2) 
      if(a1.eq.0) then
	  if (a2.eq.0) then 
	     a = 0
	     b = y_int(1) + y_frac(1)
          else
c
c     inconsistancy: x1 = x2 but y1 != y2. 
c
	     a = 0  
	     b = 0
	     ierr = 1
	     return
          endif
      endif
      a = a2/a1

      x1 = x_int(1) - x_int(2)
      x2 = y_int(1) - y_int(2)
      b1 = dble(x1*y_int(2) 
     *      -  x_int(2)*x2)
      if(x_int(1).ne.0) then 
          v2 = dble(x_int(2))/dble(x_int(1))
          w1 = dble(y_int(1))/dble(x_int(1))
          w2 = dble(y_int(2))/dble(x_int(1))
          b2 = x_frac(1)*w2 
     *       + y_frac(2)
     *       - x_frac(2)*w1
     *       - v2*y_frac(1)
          b2 = b2 * dble(x_int(1))
      else if(x_int(2).ne.0) then 
          w1 = dble(y_int(1))/dble(x_int(2))
          w2 = dble(y_int(2))/dble(x_int(2))
          b2 = x_frac(1)*w2 
     *       - x_frac(2)*w1
     *       - y_frac(1)
	  b2 = b2 * dble(x_int(2))
      else if(y_int(1).ne.0) then 
          w2 = dble(y_int(2))/dble(y_int(1))
          b2 =  y_frac(1)*w2 
     *       - x_frac(2)
	  b2 = b2 * dble(x_int(1))
      else if(y_int(2).ne.0) then 
	  b2 = x_frac(1) * dble(y_int(2))
      else
	  b2 = 0.0
      endif
      b3 = x_frac(1)*y_frac(2) 
     *   - x_frac(2)*y_frac(1)
      b = (b1 + b2 + b3)/a1
      return
      end
