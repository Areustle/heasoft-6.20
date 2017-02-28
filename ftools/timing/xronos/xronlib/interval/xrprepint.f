      subroutine xrprepint(iflags,dtint,dtnb,nbint,nkount,yi,expi,syi,
     $     iopt,mopt,dopt,fwia,fwio,ewia,ewio,nwi,intsta,rpf)
      implicit none
      integer nkount
      integer iflags(*),nbint,iopt(15,*),mopt(15,*),nwi(*),
     $     intsta(20,*)
      real yi(nkount,*),expi(nkount,*),
     $     syi(nkount,*),fwia(12,*),fwio(12,*)
      real ewia(*),ewio(*),rpf(*)
      double precision dtint(*),dtnb,dopt(15,*)

C     LOCAL
      character(80) context
      integer m,k,iv
      double precision dv
      real rv,pdum
      integer ibf,iaf,idum
      double precision tdum
      double precision ddum

c     loop for interval
c
      DO m = 1, iflags(10)
c
c       renormalise non-empty newbins
c
c for normal infiles
c
c         IF (dtint(m).GT.0.D0) THEN   !Rev.11
         rv = dtnb
         DO k = 1, nkount
            IF (yi(k,m).GT.-1.1E34) THEN
c back to cts/s
               yi(k, m) = yi(k, m)/expi(k, m)
c back to cts/s
               syi(k, m) = sqrt(syi(k,m))/expi(k, m)
c back to fraction (0->1)
               expi(k, m) = expi(k, m)*real(nkount)/real(nbint)/rv
            ENDIF
         ENDDO
c
c   If arriv. time file, apply math file options to all (new/phase)bins except 
c   for SS and ST which are applied above.  
c
         IF (dtint(m).LT.0.D0.and.mopt(1,m).NE.0) THEN 
            DO k=1,nkount
c               (note dv=1 used in place of dtime and dtint in order to fake
c                a binned file in which all math options are applied and times 
c                are not shifted)   
               dv=1.D0 
               CALL xrapplopt(iopt(1,m), mopt(1,m), dopt(1,m),
     &                        dv, dv, expi(k,m), yi(k,m), syi(k,m))
            ENDDO
         ENDIF
c
c for arrival time files??? (Threw away a bunch of junk after this comment LEB)
c
         iv=0
         ibf = 0
         iaf = 0
         DO k = 1, nkount
            if(rpf(1).gt.0..or.rpf(2).gt.0.) then
c     calculate how many newbins before (ibf) and after (iaf) 
C     should be excluded if special windows requested
               rv = rpf(1)/dtnb
               ibf = int(rv)
C     don't use address less than k=1
               ibf= min(k-1,ibf)
               ibf=-ibf
               rv = rpf(2)/dtnb
               iaf = int(rv)
C     don't use address gt. than k=nkount
               iaf = min(nkount-k,iaf)
            endif
            call xrappwins(m,2,.false.,.false.,.true.,.true.,.true.,
     $           ddum,dtint(m),expi(k,m),yi(k,m),syi(k,m),ibf,iaf,
     $           nkount,nwi,tdum,tdum,pdum,pdum,pdum,fwia,fwio,ewia,
     $           ewio,idum,idum,intsta(14,m),intsta(17,m),
     $           iv,idum)
         ENDDO
         
         if(iv.gt.0) then
            call xwrite(' ',5)
            WRITE (context, 1101) iv
            call xwrite(context,5)
 1101       FORMAT (1X, 9X, '   Warning: ', I7,
     &           ' newbins excluded by special',
     &           ' windows as requested !')
         endif

      ENDDO
      return
      end
