      subroutine xrfold(nphas,dtime,depoch,dper,dpdot,dphase)
      implicit none
      double precision dtime,depoch,dper,dpdot,dphase
      double precision dtim
      integer nphas


c     calculate phase index
c     
c     time difference (half bin shift done below
      dtim = (dtime-depoch)*86400.D0
c     if constant Pdot given to account in the phase calculation
c     INTegrale (0-t) of 1/(P + t*Pdot) where P is the test period
c     t is the difference in time of the arrival time and the epoch
c     the solution of the integral
c     
c     1/(Pdot){ ln(1 + Pdot/P *t)}
c     
      IF (dpdot.eq.0.d0) THEN  
         dphase = dmod(dtim+dper/dble(nphas)/2.D0, dper)
c     phase
         dphase = dphase/(dper)
      ELSE 
         dphase = dlog(1.d0+dpdot*(dtim+dper/dble(nphas)/2.D0)/dper)
     &        /dpdot 
         dphase = dmod(dphase,1.D0) 
      ENDIF
c     if dtime is earlier than depoch
      IF (dphase.LT.0.D0) dphase = dphase + 1.D0
      return
      end
      


