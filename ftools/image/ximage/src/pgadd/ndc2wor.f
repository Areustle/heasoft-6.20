      SUBROUTINE NDC2WOR (XNORMDEV, YNORMDEV, XWORLD, YWORLD)
      implicit none

c     IMPORT: X/YNORMDEV -  Point in normalized device coordinates
c     EXPORT: X/YWORLD   -  Point in world coordinates

      REAL XNORMDEV, YNORMDEV, XWORLD, YWORLD
c      
c  Translates point in normalized device coordinates to point in world 
c     coordinates
c  
c  Depends on current viewport and window
       
      real vport(4),win(4)
      real vpxwidth, vpxcenter, vpywidth, vpycenter
      real winxwidth, winxcenter, winywidth, winycenter
      real xscale, xzero, yscale, yzero

      CALL PGQVP(0, vport(1), vport(2), vport(3), vport(4))
      CALL PGQWIN (win(1), win(2), win(3), win(4))

c win* and vp* variables are superfluous, but make it easier to follow

      vpxwidth = vport(2) - vport(1)
      vpywidth = vport(4) - vport(3)
      vpxcenter = vport(1) + vpxwidth/2. 
      vpycenter = vport(3) + vpywidth/2. 
      
      winxwidth = win(2) - win(1)
      winywidth = win(4) - win(3)
      winxcenter = win(1) + winxwidth/2.
      winycenter = win(3) + winywidth/2.
    
      xscale = winxwidth/vpxwidth
      yscale = winywidth/vpywidth
      xzero = winxcenter - xscale*vpxcenter
      yzero = winycenter - yscale*vpycenter

      XWORLD = XNORMDEV*xscale + xzero
      YWORLD = YNORMDEV*yscale + yzero
      
      return
      end
