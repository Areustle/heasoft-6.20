      subroutine set_vpvals(vport, code)

      implicit none
c
c
c  Centralized routine for setting viewport variables
c
c  O  vport  (r)  Viewport specs
c  I  code   (i)  Code for setting viewport
c                   -1=reset
c                    0=center
c                    1=left side
c                    2=right side
c                    3=top
c                    4=bottom
c                    5=center
c
c  If number out of range, leave viewport alone.  Two centers,
c  to remain backwards compatible with display_area
c
       real*4 vport(4)
       integer code
c
       if ( code.eq.-1 ) then
          Vport(1) = -1.0
          Vport(2) = -1.0
          Vport(3) = -1.0
          Vport(4) = -1.0
       else if ( code.eq.0 ) then
          Vport(1) = 0.2
          Vport(2) = 0.9
          Vport(3) = 0.2
          Vport(4) = 0.9
       else if ( code.eq.1 ) then
          Vport(1) = 0.05
          Vport(2) = 0.5
          Vport(3) = 0.45
          Vport(4) = 0.9
       else if ( code.eq.2 ) then
          Vport(1) = 0.55
          Vport(2) = 0.9
          Vport(3) = 0.45
          Vport(4) = 0.9
       else if ( code.eq.3 ) then
          Vport(1) = 0.2
          Vport(2) = 0.9
          Vport(3) = 0.525
          Vport(4) = 0.95
       else if ( code.eq.4 ) then
          Vport(1) = 0.2
          Vport(2) = 0.9
          Vport(3) = 0.05
          Vport(4) = 0.475
       else if ( code.eq.5 ) then
          Vport(1) = 0.2
          Vport(2) = 0.9
          Vport(3) = 0.2
          Vport(4) = 0.9
       endif

       return
       end
