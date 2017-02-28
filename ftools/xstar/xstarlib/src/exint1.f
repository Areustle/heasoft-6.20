c  /* Subroutine exint1(x,jump),
c   translated from Fortran.
c   This subroutine can be called by an external main program, such
c   as call_exint1.c.
c
c   jump = 1: exint1 = E1(x);
c   jump = 2: exint1 = exp(x) * E1(x);
c   jump = 3: exint1 = x * exp(x) * E1(x);
c   Returns a real*8 precision floating pointeger value.
c*/

      real*8 function exint1(x,jump)
      real*8 x
      integer jump
      real*8 EI_1
      real*8 EI_2
      real*8 EI_3
      real*8 x2
      real*8 x3
      real*8 x4,retval,pow
      real*8 a(10)
      data
     $a/7.122452e-07,1.766345e-06,2.928433e-05,.0002335379,.001664156,
     $.01041576, .05555682, .2500001, .9999999, .57721566490153/
      real*8 b(8)
      data
     $b/8.5733287401,18.059016973,8.6347608925,.2677737343,9.5733223454,
     $     25.6329561486, 21.0996530827, 3.9584969228/
c
      if (x.eq.0.d0) then
         exint1=0.
         return
      else 
        if (x .le. 1.0d0) then
          EI_1 = ((((((((a(1) * x - a(2)) * x + a(3)) * x
     $       - a(4)) * x + a(5)) * x - a(6)) * x
     $       + a(7)) * x - a(8)) * x + a(9)) * x
     $       - log(x) - a(10)
          EI_2 = exp(x) * EI_1
          EI_3 = x * EI_2
        else 
          x2 = pow(x,2.d0)
          x3 = pow(x,3.d0)
          x4 = pow(x,4.d0)
          EI_3 = (x4 + b(1) * x3 + b(2) * x2 
     $     + b(3) * x + b(4)) / 
     $      (x4 + b(5) * x3 + b(6) *x2
     $       + b(7) * x + b(8))
          EI_1 = EI_3 / (x * exp(x))
          EI_2 = EI_3 / x
        endif
      endif
c  /* In K&R C, the switch argument has to be of type
c     int.Can terminate with return or break.
c     */
      go to (1,2,3)jump
 1    continue
c     case 1:
      retval=EI_1
      go to 9000
 2    continue
c     case 2:
      retval=EI_2
      go to 9000
 3    continue
c     case 3:
      retval=EI_3
 9000 continue
c      default:
      exint1=retval
      return
      end
