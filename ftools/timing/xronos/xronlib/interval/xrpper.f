      subroutine xrpper(nper,dper,dpdot,dres,progtype,dpera,dpdota)
      implicit none
      integer nper,k,i
      character(10) progtype
      double precision dper,dpdot,dpera(nper),dpdota(nper),dper0,dres
      double precision dv, dvpre
C     XRonos Prepare PERiod arrays
      if(progtype(1:10).ne.'FOLDSEARCH') then
         dpera(1)=dper
         dpdota(1)=dpdot
         return
      else
C     fill period and dp/dt arrays with test periods and period derivatives
C     each test consists of a P and a Pdot and occupies one element in the
C     dpera and dpdota arrrays.  Suppose you have 3 periods (p1,p2,p3) you
C     want to search and 2 period derivatives (dp1,dp2) your arrays should
C     look like:
C       dpera               dpdota
C        p1                   dp1
C        p2                   dp1
C        p3                   dp1
C        p1                   dp2
C        p2                   dp2
C        p3                   dp2
C     at the moment, no searching is done in the period derivative dimension
c     work out smallest period in search
      dper0 = dper - dble(nper/2)*dres
C     put in a safety catch for people who input odd values.
      dvpre=0.0
      if(dper0.le.0) THEN 
         DO i= 1, nper/2
            dv=dper-dble(i)*dres
            if(dv.le.0.and.dvpre.gt.0)dper0 = dvpre
            dvpre=dv
         ENDDO 
      endif
      do k=1,nper
         dpera(k) = dper0 + dble(k-1)*dres
         dpdota(k)= dpdot
      enddo
      endif
      return
      end
      
