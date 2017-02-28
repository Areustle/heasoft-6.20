c_______________________________________________________________
      real*8 function exint_n(x,E1x_in,n)
      real*8 x
      real*8 E1x_in
      integer n

c/* Calculates E_n(x), for n = 1,2,3,4. If n.gt.1, and E1x_in .ge. 0, then 
c   routine uses E1x for E_1(x) in calculating recurrance.
c   Returns a real*8 precision floating pointeger value. */
c
      real*8 E1x
      real*8 x2, x3, x4

      real*8 a(10)
      data 
     $a/7.122452e-07,1.766345e-06,2.928433e-05,.0002335379,.001664156,
     $.01041576, .05555682, .2500001, .9999999, .57721566490153/
      real*8 b(8)
      data
     $b/8.5733287401,18.059016973,8.6347608925,.2677737343,9.5733223454,
     $25.6329561486, 21.0996530827, 3.9584969228/
c
      E1x = E1x_in
      if ((n.eq.1).or.(E1x .lt. 0)) then
        if (x.eq.0.) then
           exint_n=0.
           return
        else 
          if (x .le. 1.0) then
            E1x = ((((((((a(1)*x-a(2))*x+a(3))*x-a(4))*x
     $         +a(5))*x-a(6))*x+a(7))*x
     $         -a(8))*x+a(9))*x - log(x) - a(10)
          else 
             x2 = x*x
             x3 = x2*x
             x4 = x2*x2
             E1x = (x4+b(1)*x3+b(2)*x2+b(3)*x+b(4))/
     $         (x4+b(5)*x3+b(6)*x2+b(7)*x+b(8))
             E1x = E1x / (x * exp(x))
          endif
        endif
      endif
c     
      go to (1,2,3,4)n
 1      continue
c       case (1):
        retval= (E1x)
        go to 9000
 2      continue
c       case (2):
        retval=(exp(-x) - x*E1x)
        go to 9000
 3      continue
c       case (3):
        retval= (x*x*E1x + exp(-x)*(1-x))/2.
        go to 9000
 4      continue
c       case (4):
        retval= (exp(-x)*(2-x+x*x) - x*x*x*E1x)/6.
        go to 9000
        call errmess(6,27,"exint_n. Bad value for n = ")
 9000   continue
        exint_n=retval
        return
        end
