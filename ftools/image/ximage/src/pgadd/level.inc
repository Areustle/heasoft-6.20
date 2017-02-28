c *******************************************************
c  Include file to provide access to levels array for
c  GRIMG1_H and GRIMG1_H
c *******************************************************

      integer numlevs
      real*4 levels(256)

      common /level/ numlevs,levels
