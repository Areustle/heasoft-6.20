      real*8 function ff2(x,lpri,lun11)
c
c     expression usind in collisional ionization rate coefficient following
c     arnaud and raymond (1992)
c     author:  T. Kallman
c
      implicit none
c
      real*8 x
      real*8 q(15),p(15)
      real*8 pp,qq,ptst,xprod
      integer j,lun11,lpri
c
      data q/1.,2.1958e+2,2.0984e+4,1.1517e+6,4.0349e+7,
     $      9.4900e+8,1.5345e+10,1.7182e+11,1.3249e+12,
     $      6.9071e+12,2.3531e+13,4.9432e+13,5.7760e+13,
     $      3.0225e+13,3.3641e+12/
      data p/1.,2.1658e+2,2.0336e+4,1.0911e+6,3.7114e+7,
     $       8.3963e+8,1.2889e+10,1.3449e+11,9.4002e+11,
     $       4.2571e+12,1.1743e+13,1.7549e+13,1.0806e+13,
     $       4.9776e+11,0./
c
      xprod=1.
      if (lpri.ne.0)
     $ write (lun11,*)'in ff2:',x
      pp=0.
      qq=0.
      do j=1,15
        ptst=1./xprod
        if ((ptst.lt.1.e+20).and.(xprod.lt.1.e+24/x)) then
          pp=pp+p(j)/xprod
          qq=qq+q(j)/xprod
          xprod=xprod*x
          if (lpri.ne.0)
     $     write (lun11,*)j,xprod,pp,qq,ptst
          endif
        enddo
      ff2=pp/(1.e-20+qq)/x/x
c
      return
      end
