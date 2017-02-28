C  $Header: /headas/headas/ftools/ascalib/src/general/trns.f,v 3.7 2001/12/28 16:23:48 irby Exp $
C          
      function trns(E,idev)
      real*8  trns,E
      integer idev

c     E : energy (keV)   0.150-10.000
c     idev : sensor ID ( 0 : SIS0, 1: SIS1, 2: GIS2, 3:GIS3 )
c
c     orginaly coded by Y. Tawara
c              moddified by H.Awaki  ( Apr.19.'93 )

      real*8 myler, almnm, work
c     thickness of myler and alminuum (unit : cm)
      real*8 mylth(0:3)
      data mylth/0.22d-4,0.22d-4,0.54d-4,0.54d-4/
      real*8 alth (0:3)
      data alth/0.28d-5,0.28d-5,0.37d-5,0.37d-5/
      real*8 mesh (0:3)
      data mesh/0.942,0.942,0.942,0.942/

      work = e*1.0d3
      if (work.gt.0.0) then
        trns = mesh(idev)*exp(-mylth(idev)*1.41d0*myler(work)
     &                  -alth (idev)*2.69d0*almnm(work))
	else
        trns = 1.0d-20
      endif

      return
      end


      function myler(E)
      real*8 myler,E

      real*8 norm, alpha

      if (E.le.283.84d0) then
         norm = 1.8871D+9
         alpha=-2.3434
        elseif (E.le.531.7d0) then
         norm = 3.5631D+10
         alpha=-2.4406
        elseif (E.le.1560.0d0) then
         norm = 2.6550d+11
         alpha=-2.6547
        else
         norm = 5.2462d+12
         alpha=-3.05
      endif
      myler = norm*E**alpha
      return
      end

      function almnm(E)
      real*8 almnm,E

      real*8 norm,alpha,e1

c . . > e1 is the cross point of two functions,
c         norm1*E**alpha1 and norm2*E**alpha2

      e1=243.534d0
      if (E.le.E1) then
         norm = 2.2377d+7
         alpha=-1.1203
        elseif (E.le.1559.9d0) then
         norm = 7.6486d10
         alpha=-2.601
        else
         norm = 2.6897d12
         alpha=-2.7469
      endif
      almnm = norm*E**alpha
      return
      end


