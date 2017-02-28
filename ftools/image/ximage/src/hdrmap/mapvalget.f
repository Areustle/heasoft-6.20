      subroutine mapvalget(Map, Szx, Szy, I, J, Value)
      implicit none
c
c  Simple routine to get value from Map 
c  Dealing directly with udmget pointers can get ugly in 2-D case
c
c  I  Map    (r)  Map
c  I  Szx/y  (i)  Size of map
c  I  I/J    (i)  Location to get from
c  O  Value  (r)  Value 
c
      integer Szx, Szy, I, J
      real*4 Map(Szx,Szy), Value

      Value = Map(I,J)

      return
      end
