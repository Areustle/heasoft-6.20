      subroutine xrchkgti(lui,m,extns,newfile,ordered,htunit,dtoffset
     &                   ,dtime,inside)

c xanadu TIming routine to CHecK input times against Good Time Intervals.

c This general routine can be called even if the are no GTI's in the
c file -- it just returns indise = .true. in that case.

c If there is a GTI extension, (extns[2] > 1), and if newfile = .true.,
c the routine loads up the GTIs from the file (subroutine xrftlgti).

c The times can be passed to this routine in either ordered or not
c ordered.  If the times are ordered, the routine will load up blocks
c of GTIs sequentially, and proceed straight trought the file.  If the
c times are not ordered, the routine will load up GTIs from the file
c and check against each one until the time is found to lie inside
c a GTI (inside = .true.) or untill all the GTI's are exhausted (inside =
c .false.).  The latter case is clearly slower.

c  I  lui      (i)  Lu of input file
c  I  m        (i)  Series number (1 by default)
c  I  extns    (i)  Vector of ectension numbers (rate table (1) and GTI (2))
c  I  newfile  (l)  = TRUE if reading from a new file
c  I  ordered  (l)  = TRUE if input dtime's arrive in acsending order.
c  I  htunit   (i)  Flag for time units in the rate table header
c  I  dtoffset (d)  Internal time offset
c  I  dtime    (d)  Input time value
c  O  inside   (l)  = TRUE if and only if dtime is inside a GTI

c Author:  EAL  GSFC/HSTX  March, 1994

      INTEGER nlui,gtimax
      parameter (nlui=6, gtimax = 2880)
      logical inside,newfile,ordered
      integer lui,ngti(nlui),igti(nlui),tgti(nlui),gmax(nlui)
     &   ,m,extns(*),htunit(*),i,j,nsets(nlui),frow
      double precision dtime,dtoffset(*),gtia(gtimax,nlui)
     &   ,gtio(gtimax,nlui),start(gtimax),stop(gtimax)
     &   ,first(nlui),last(nlui)

      save

c If there is no GTI extension, the test passes automatically.

      if(extns(2).le.0) then
         inside = .true.
         return
      endif

      if((m.lt.0).or.(m.gt.nlui)) m = 1

      if(newfile) then

c Read in the first set of GTIs.

         CALL xrftlgti(lui,extns,newfile,htunit(m),dtoffset(m)
     &                ,gtimax,1,start,stop,first(m),last(m),ngti(m))

c Calculate number of sets in the file, and number of GTIs in the first set.

         nsets(m) = ngti(m)/gtimax + 1
         gmax(m) = min(ngti(m),gtimax)

c Load up GTI storage vectors.

         DO i = 1, gmax(m)
            gtia(i,m) = start(i)
            gtio(i,m) = stop(i)
         ENDDO
c Initialize counters.

         igti(m) = 1
         tgti(m) = 1

         newfile = .false.

      endif

c End of first-call commands.

      inside = .false.

c Special case: photon completely outside the full range of GTIs.

      IF((dtime.lt.first(m)).or.(dtime.gt.last(m))) RETURN

      IF(.not.ordered) THEN 

c Case where dtime's CAN'T be assumed to be ordered.

         DO i = 1, gmax(m)
            IF((dtime.ge.gtia(i,m)).and.(dtime.le.gtio(i,m))) THEN
               inside = .true.
               RETURN
            ENDIF
         ENDDO

c Case where there are more GTIs in the file than in memory.  (Very expensive)

         DO j = 1, nsets(m) - 1
            frow = j*gtimax + 1
            CALL xrftlgti(lui,extns,.false.,htunit(m),dtoffset(m)
     &                   ,gtimax,frow,start,stop,first(m),last(m)
     &                   ,ngti(m))
            gmax(m) = min(ngti(m)-frow+1,gtimax)
            DO i = 1, gmax(m)
               IF((dtime.ge.start(i)).and.(dtime.le.stop(i))) THEN
                  inside = .true.
                  RETURN
               ENDIF
            ENDDO
         ENDDO

      ELSE

c Case where dtime's CAN be assumed to be ordered. (as in calls from xronos)
c This case is much more efficient, even if all GTI's do fit into memory.

10       continue

         if((igti(m).gt.gtimax).and.(tgti(m).le.ngti(m))) then

c Past last stored GTI, but more GTI's remain in the file:
c Load up more GTIs and pass through again.

            CALL xrftlgti(lui,extns,.false.,htunit(m),dtoffset(m)
     &             ,gtimax,frow,start,stop,first(m),last(m),ngti(m))
            gmax(m) = min(ngti(m)-frow+1,gtimax)
            DO i = 1, gmax(m)
               gtia(i,m) = start(i)
               gtio(i,m) = stop(i)
            ENDDO
            igti(m) = 1
            go to 10

         elseif((dtime.lt.gtia(igti(m),m)).or.(tgti(m).gt.ngti(m)))
     &      then

c In a GTI gap or before first GTI or after last GTI: Set flag.

            inside = .false.

         elseif(dtime.gt.gtio(igti(m),m)) then

c Past most recent GTI: advance GTI counters and pass through again.

            tgti(m) = tgti(m) + 1
            igti(m) = igti(m) + 1
            go to 10
         else

c        Only case left: inside a GTI.

            inside = .true.
         endif

      ENDIF

      return
      end
