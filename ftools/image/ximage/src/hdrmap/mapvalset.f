      subroutine mapvalset(Map, Szx, Szy, I, J, Value)
      implicit none
c
c  Simple routine to set value in Map 
c  Dealing directly with udmget pointers can get ugly in 2-D case
c
c I/O Map    (r)  Map
c  I  Szx/y  (i)  Size of map
c  I  I/J    (i)  Location to set
c  I  Value  (r)  Value to set
c
      integer Szx, Szy, I, J
      real*4 Map(Szx,Szy), Value

      Map(I,J) = Value

      return
      end
