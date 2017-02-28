      SUBROUTINE WEIGHT(G,H,ENERGY,NBRENR,GAMMA,FLUX,IENRGY,NCAL)
C=======================================================================
C*
C* This program calculates the weighting matrices (G,H) corresponding
C* to the probabilty that a photon will be measured in a given energy
C* range (ENERGY).
C*
C* Input:
C*   ENERGY(2,NBRENR) - real*4; array containing the lower and upper
C*                      limits of the desired energy intervals.
C*   NBRENR           - integer*4; number of energy intervals (max=10).
C*   GAMMA(NBRENR)    - real*4; array containing the spectral indices
C*                      for each energy interval.
C*   IENRGY(NCAL)     - real*4; calibration energies
C*   NCAL             - integer*4; number of calibration energies
C*
C* Output:
C*   G(10,NCAL,76)    - real*4; weights measuring the direct coupling 
C*                      of area and dispersion results
C*   H(10,NCAL,76)    - real*4; weights measuring the coupling of cross 
C*                      terms for area and dispersion results
C*   FLUX(NBRENR)     - real*4; contains the flux of the given spectrum
C*
C* Calls:
C*   EXTRP  - (function) numerically integrates .
C*
C* Commons:   none
C*
C=======================================================================
C+ $Log: weight.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:14  irby
C+ Additions to libgro - previously these codes existed in the following
C+ libraries:
C+
C+   libsenstv
C+   libsysutil
C+   libutil
C+   libftio
C+
c Revision 1.1  1996/08/15  17:27:23  programs
c Initial revision
c
c Revision 2.1  1991/09/09  17:42:41  nancy
c First controlled version on the Sun.
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      integer ncal,nmax,nbrenr
      real*4 e0
      parameter(nmax=10, e0=36000.0)
      real*4 energy(2,nbrenr),gamma(nbrenr),flux(nbrenr)
      real*4 ienrgy(ncal+1),ebrk(nmax),spcidx(nmax),c(nmax)
      real*4 g(nmax,ncal,76),h(nmax,ncal,76)
      real*4 e1,e2,de,emax
      real*4 en1,en2,en1nxt,en2nxt,norm,idx
      real*4 p11,s11,p12,s12,p22,s22
      real*4 a11,b11,a12,b12,a22,b22
      real*4 en1int(4),en2int(4),int(4),numint
      real*4 intgrl,enxt,extrp
      integer i,j,i0,n0,n1,n2,iovlap,node
      integer ical,ide

      character(80)	id

      save

      common	/id/	id
      id = '$Id: weight.f,v 3.2 2013/05/21 19:08:27 irby Exp $'


C             CHECK FOR OVERLAPPING ENERGY INTERVALS AND SPECTRAL BREAKS
      emax=ienrgy(ncal+1)
      iovlap=0
      node=1
      c(1)=1.0
      spcidx(1)=-gamma(1)
      do i=1,nbrenr-1
         if (gamma(i).ne.gamma(i+1)) then
            ebrk(node)=energy(1,i+1)
            c(node+1)=c(node)*ebrk(node)**(gamma(i+1)-gamma(i))
            node=node+1
            spcidx(node)=-gamma(i+1)
         endif
         do j=i+1,nbrenr
            if(energy(1,j).lt.energy(2,i) .and. 
     &         energy(2,j).gt.energy(1,i)) iovlap=1
         enddo
      enddo
      if (node.eq.1) iovlap=0
      if (iovlap.eq.1) node=1
      ebrk(node)=emax
      

C           CALCULATE FLUX OF SOURCE COUNTS FOR ENTERED ENERGY INTERVALS
      node=1
      do ide=1,nbrenr
         if (energy(1,ide).eq.ebrk(node)) node=node+1
         flux(ide)=c(node)*(intgrl(energy(2,ide),-gamma(ide)+1)-
     &    intgrl(energy(1,ide),-gamma(ide)+1))
      enddo

      do ide=1,nbrenr
         e1=50.0*energy(1,ide)
         e2=50.0*energy(2,ide)
         if (iovlap.eq.1) spcidx(1)=-gamma(ide)

         i0=2
         do while (1.5*ienrgy(i0).le.energy(1,ide))
            i0=i0+1
         enddo
         i0=i0-1
         en1=ienrgy(i0)
         p22=en1*en1
         s22=en1+en1

C                    FIND STARTING AND ENDING BINS FOR THIS ENERGY RANGE
         n1=1+(e1/en1)
         if (n1.ge.76) then
            n1=75
            en1=energy(1,ide)/1.5
         endif
         en1nxt=enxt(n1,e1,emax)

         n2=1+(e2/en1)
         if (n2.gt.76) n2=76
         en2nxt=enxt(n2,e2,emax)

         n0=1+(e1/ienrgy(i0+1))
         do i=n0,n2
            g(ide,i0,i)=0.0
         enddo

         node=1
         do while (en1.ge.ebrk(node))
            node=node+1
         enddo
         idx=spcidx(node)-1
         do i=1,4
            en2int(i)=intgrl(en1,idx+i)
         enddo

         do ical=i0,ncal-1
C                      INITIALIZE QUANTITIES FOR THIS CALIBRATION ENERGY
            en2=ienrgy(ical+1)
            p11=p22
            p12=ienrgy(ical)*en2
            p22=en2*en2
            s11=s22
            s12=ienrgy(ical)+en2
            s22=en2+en2
            de=en2-ienrgy(ical)

            n0=1+(e1/ienrgy(ical+2))
            do i=n0,n2
               g(ide,ical+1,i)=0.0
               h(ide,ical,i)=0.0
            enddo

            norm=c(node)/(de*de)
            en2=0.0
            do while (en2.lt.ienrgy(ical+1))
               en2=min(en1nxt,en2nxt,ebrk(node),ienrgy(ical+1))
               idx=spcidx(node)-1
               do i=1,4
                  en1int(i)=en2int(i)
                  en2int(i)=intgrl(en2,idx+i)
                  int(i)=norm*(en2int(i)-en1int(i))
               enddo

               a11=p22*int(1)-s22*int(2)+int(3)
               b11=p22*int(2)-s22*int(3)+int(4)
               a12=p12*int(1)-s12*int(2)+int(3)
               b12=p12*int(2)-s12*int(3)+int(4)
               a22=p11*int(1)-s11*int(2)+int(3)
               b22=p11*int(2)-s11*int(3)+int(4)

               g(ide,ical,n1)=g(ide,ical,n1)-e1*a11+n1*b11
               h(ide,ical,n1)=h(ide,ical,n1)+e1*a12-n1*b12
               g(ide,ical+1,n1)=g(ide,ical+1,n1)-e1*a22+n1*b22
               do i=n1+1,n2
                  g(ide,ical,i)=g(ide,ical,i)+b11
                  h(ide,ical,i)=h(ide,ical,i)-b12
                  g(ide,ical+1,i)=g(ide,ical+1,i)+b22
               enddo
               g(ide,ical,n2)=g(ide,ical,n2)+e2*a11-n2*b11
               h(ide,ical,n2)=h(ide,ical,n2)-e2*a12+n2*b12
               g(ide,ical+1,n2)=g(ide,ical+1,n2)+e2*a22-n2*b22

C       ADJUST STARTING AND ENDING BINS, AND SPECTRAL INDEX IF NECESSARY
               if (en1nxt.eq.en2) then
                  n1=n1-1
                  en1nxt=enxt(n1,e1,emax)
               endif
               if (en2nxt.eq.en2) then
                  n2=n2-1
                  en2nxt=enxt(n2,e2,emax)
               endif
               if (ebrk(node).eq.en2) then
                  node=node+1
                  norm=c(node)/(de*de)
                  idx=spcidx(node)-1
                  do i=1,4
                     en2int(i)=intgrl(en1,idx+i)
                  enddo
               endif
               en1=en2
            enddo
         enddo

C                                           CALCULATE HIGH ENERY WEIGHTS
         norm=1.32*c(node)
         en2=0.0
         do while (en2.lt.emax)
            en2=min(en1nxt,en2nxt,ebrk(node))
            numint=extrp(en1,en2,spcidx(node)-1)
            int(1)=norm*numint
            int(2)=norm*e0*(exp(-en1/e0)*en1**spcidx(node)-
     &       exp(-en2/e0)*en2**spcidx(node)+spcidx(node)*numint)

            g(ide,ncal,n1)=g(ide,ncal,n1)-e1*int(1)+n1*int(2)
            do i=n1+1,n2
               g(ide,ncal,i)=g(ide,ncal,i)+int(2)
            enddo
            g(ide,ncal,n2)=g(ide,ncal,n2)+e2*int(1)-n2*int(2)

            if (en1nxt.eq.en2) then
               n1=n1-1
               en1nxt=enxt(n1,e1,emax)
            endif
            if (en2nxt.eq.en2) then
               n2=n2-1
               en2nxt=enxt(n2,e2,emax)
            endif
            if (ebrk(node).eq.en2) then
               if (node.lt.nmax) node=node+1
               norm=1.32*c(node)
            endif
            en1=en2
         enddo
      enddo

c-----------------------------------------------------------------------
      return
      end




C=======================================================================
      REAL FUNCTION INTGRL(E,GAMMA)
C=======================================================================
C*
C* Returns integral of E^(GAMMA-1).
C*
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      real*4 e,gamma

      if (gamma.ne.0.0) then
         intgrl=(e**gamma)/gamma
      else
         intgrl=alog(e)
      endif

c-----------------------------------------------------------------------
      return
      end



C=======================================================================
      REAL FUNCTION ENXT(N,E,EMAX)
C=======================================================================
C*
C* Returns next energy at which n1 or n2 will change.
C*
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      real*4 e,emax
      integer n

      if (n.gt.1) then
         enxt=e/(n-1)
      else
         enxt=emax
      endif

c-----------------------------------------------------------------------
      return
      end
