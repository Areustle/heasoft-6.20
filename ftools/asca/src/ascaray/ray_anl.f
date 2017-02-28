
C--------1---------2---------3---------4---------5---------6---------7--
C
C ASCA XRT raytracing
C
C 94/10/27 Y.Ishisaki version 1.0
C     modified from NAGOYA version for SimASCA
C 94/03/27 A.Furuzawa version 2.0
C     introduced so called horn (tsuno)
C 95/10/20 M.Watanabe version 2.1
C     Floating error at gold edge is fixed
C 95/07/03 version 3.0
C     Crab tune sono2
C 95/09/30 version 4.0
C     Image tuning by GRO*** 
C       nvfluct, etc
C 95/10/27 Y.Ishisaki version 4.0a
C     for Infinit Loop
C       write(*,*) variables in Raytraceraycha() after theeta  = - dasin(st)
C       theeta = -theeta in Raytracescat() if ( theeta .lt. 0 )
C 95/10/27 M.Watanabe version 5.0
C     Crab tune sono3
C
C=======================================================================
      Subroutine Raytracedatred(iunt, iflct)
C     read astro-d xrt data
C  input   nothing
C  output  : ** in common /struct/**
C     double precision r(120,1)    :   radius of primary top
C     double precision r(120,2)    :   radius of primary bottom
C     double precision r(120,3)    :   radius of secondary top
C     double precision r(120,4)    :   radius of secondary bottom
C     double precision beta(120,1) :   angle of primary mirror
C     double precision beta(120,2) :   angle of secondary mirror
C     double precision z(4)        :   distance from focal plane
C=======================================================================
      Implicit None
C input
      Integer iunt, iflct
C common
      double precision r, beta, z
      Common /Raytracestruct/ r(120,4), beta(120,2), z(4)
C local
      Integer i, j, k
C function
      double precision Raytraceran
C
      Do i=1, 120
         Read(iunt,*,end=999) (r(i,j),j=1,4),(beta(i,k),k=1,2)
         If (iflct.eq.1) then
            Do j=1, 4
               r(i,j) = r(i,j)+((Raytraceran()-0.5d0)*0.035d0)
            End do
         End if
         beta(i,1) = (r(i,1)-r(i,2))/101.596
         beta(i,2) = (r(i,3)-r(i,4))/101.596
      End do
 999  Continue
C
      z(1) = 3605.3423525d0
      z(2) = 3503.7463525d0
      z(3) = 3496.2536475d0
      z(4) = 3394.6576475d0
C
      Return
      End

C=====================================================================
      Subroutine Raytraceraytra(eng,angin,angphi,xp,yp,fai,rad,
     &                           wei,xf,yf,icond,ihitp,ihits)
C
C     << ray tracing routine in astro-d mirror >>
C     input  : eng      / energy of incident photon (keV)
C              angin,angphi / direction of incident photon (degree)
C              xp,yp    / position of incident photon at mirror top (mm)
C              fai,rad  / position of incident photon at mirror top (radian)
C     input/output:
C              wei      / photon weight, 0 for discard mode /
C     output : xf,yf    / position of reflected photon at focal plane
C              icond    / condition code
C              icond =   0 : normal end
C                    = 910 : hidden by primary housing
C                    = 911 : hidden by primary housing side wall
C                    = 915 : hidden by secondary housing
C                    = 916 : hidden by secondary housing side wall
C                    = 920 : hidden by 1st alignment bar
C                    = 921 : hidden by 2nd alignment bar
C                    = 923 : hidden by 3rd alignment bar
C                    = 924 : hidden by 4th alignment bar
C                    = 925 : hidden by 1&14 sector
C                    = 930 : hidden by primary foil
C                    = 935 : hidden by secondary foil
C                    = 940 : absorbed by primary inner mirror
C                    = 945 : absorbed by secondarr inner mirror
C                    = 950 : absorbed on primary foil
C                    = 955 : absorbed on secondary foil
C                    = 960 : only primary reflect
C                    = 970 : only secondary reflect
C                    = 980 : no reflect
C                    = 990 : abnormal reflected
C
C=====================================================================
      Implicit None
C input
      double precision eng, angin, angphi, xp, yp, fai, rad
C input/output
      double precision wei
C output
      double precision xf, yf
      Integer icond, ihitp, ihits
C common
      double precision r, beta, z
      Common /Raytracestruct/ r(120,4),beta(120,2),z(4)
C constant
      Integer mskhs
      Parameter (mskhs=1)
      double precision MPI, M90DEG, DEG2RAD, WALBAR, SECTOR
      Parameter (MPI = 3.1415926535897932385d0)
      Parameter (M90DEG = MPI/2d0)
      Parameter (DEG2RAD = 0.017453292519943295769d0)
      Parameter (WALBAR = 1.574738d0)
      Parameter (SECTOR = 6.428d0*DEG2RAD)
C local
      Integer ihit, ihitm, km, knest, inhc, irc
      double precision theeta, phi
      double precision ray0(3), rayn(3), ray0s(2)
      double precision t, sint, widfai, diffai, secfai, rs
      double precision acref, aref, zcref, zref
C function
      double precision Raytraceran, Raytracecrefrd, Raytracerefrd

C-------------------------- initializasiot -----------------------------
      icond    = 0
      ray0(1)  = xp
      ray0(2)  = yp
      ray0(3)  = z(1)
      ihitp = 0
      ihits = 0

      theeta   = angin  * DEG2RAD
      phi      = angphi * DEG2RAD

      sint = dsin(theeta)
      rayn(1)  = sint * dcos(phi)
      rayn(2)  = sint * dsin(phi)
      rayn(3)  = - dcos(theeta)

      Do km=1, 2
        If (km.eq.2) then
          t = -( ray0(3) - z(3) ) / rayn(3)
          ray0(1) = ray0(1) + t * rayn(1)
          ray0(2) = ray0(2) + t * rayn(2)
          ray0(3) = z(3)
          rad     = ray0(1)*ray0(1)+ray0(2)*ray0(2)
          rad     = dsqrt(rad)
          fai     = datan(ray0(2)/ray0(1))
          If (ray0(1).lt.0.0d0) then
            fai   = fai + MPI
          Else if (ray0(2).lt.0.0d0) then
            fai   = fai + 2.0d0 * MPI
          End if
        End if
        ray0s(1) = ray0(1)
        ray0s(2) = ray0(2)
C------------- finding the hiddeness by housing ------------------------
        If ( km.eq.1 ) then
           If ((dabs(ray0(1)).lt.5.0036d0)) goto 910
           If ((dabs(ray0(2)).lt.5.0036d0)) goto 910
        Else
           If ((dabs(ray0(1)).lt.5.0036d0)) goto 915
           If ((dabs(ray0(2)).lt.5.0036d0)) goto 915
        End if
C------------- finding the hiddeness by alignment bar ------------------
        widfai = WALBAR / rad
        diffai = dmod(fai+widfai/2.0d0, SECTOR)
        If ( diffai.lt.widfai ) then
           If ( km.eq.1 ) goto 920
           If ( km.eq.2 ) goto 923
        End if
C------------- finding the photon in 1&14 sector -----------------------
        If ((mskhs.eq.1).and.(km.eq.1)) then
          secfai = dmod(fai, M90DEG)
          if (secfai.lt. SECTOR) goto 925
          if (secfai.gt. M90DEG - SECTOR) goto 925
        End if
C------------------ finding the hiddeness by foils ---------------------
        If (km.eq.1) then
           rs = 58.9903745d0
        Else
           rs = 57.6391477d0
        End if
        Do knest=1, 120
           If ((rad.gt.rs).and.(rad.lt.r(knest,km*2-1))) goto 22
           rs = r(knest,km*2-1) + 0.15d0
        End do
        if (km.eq.1) goto 930
        if (km.eq.2) goto 935
 22     Continue
C------------------- ray tracing in knest'th space ---------------------
        ihitm = 1
        Do ihit=1, 100
          Call Raytracehinm(theeta,ray0,rayn,knest,km,inhc)
          If (inhc.eq.1) then
            zcref = Raytracecrefrd(dabs(theeta), eng)
            If ( wei.gt.0d0 ) then
               wei = wei * zcref
               If ( km.eq.1 ) then
                  ihitp = ihitp + ihitm
               Else
                  ihits = ihits + ihitm
               End if
            Else
               acref = Raytraceran()
               If ( km.eq.1 ) then
                  If ( zcref.lt.acref ) goto 940
                  ihitp = ihitp + ihitm
               Else
                  If ( zcref.lt.acref ) goto 945
                  ihits = ihits + ihitm
               End if
            End if
          Endif
          If ((inhc.eq.0).and.(ihit.ne.1)) goto 24
C          write(*,*) 'O.K.3'
          Call Raytraceraycha(eng,theeta,ray0,rayn,
     &                         rad,fai,knest,km,irc)
C          write(*,*) 'O.K.6'
          ihitm = ihitm * 2
          If (irc.eq.1) then
            zref = Raytracerefrd(dabs(theeta), eng)
            If ( wei.gt.0d0 ) then
               wei = wei * zref
               If ( km.eq.1 ) then
                  ihitp = ihitp + ihitm
               Else
                  ihits = ihits + ihitm
               End if
            Else
               aref = Raytraceran()
               If ( km.eq.1 ) then
                  If ( zref.lt.aref ) goto 950
                  ihitp = ihitp + ihitm
               Else
                  if ( zref.lt.aref ) goto 955
                  ihits = ihits + ihitm
               End if
            End if
          End if
          if (irc.eq.0) goto 24
          ihitm = ihitm * 2
        End do
 24     Continue
        t = -( ray0(3) - z(2*km) ) / rayn(3)
        ray0(1) = ray0(1) + t * rayn(1)
        ray0(2) = ray0(2) + t * rayn(2)
        ray0(3) = z(2*km)
        rad     = ray0(1)*ray0(1)+ray0(2)*ray0(2)
        rad     = dsqrt(rad)
        fai     = datan(ray0(2)/ray0(1))
        If (ray0(1).lt.0.0d0) then
          fai   = fai + MPI
        Else if (ray0(2).lt.0.0d0) then
          fai   = fai + 2.0d0 * MPI
        End if
C-------- finding the hiddeness by housing side wall -------------------
        If (km.eq.1) then
          if((dabs(ray0(1)).lt.5.0036d0).or.
     &       (dabs(ray0(2)).lt.5.0036d0).or.
     &       ((ray0(1)*ray0s(1)).lt.0.0d0).or.
     &       ((ray0(2)*ray0s(2)).lt.0.0d0)) goto 911
        Else
          if((dabs(ray0(1)).lt.5.0036d0).or.
     &       (dabs(ray0(2)).lt.5.0036d0).or.
     &       ((ray0(1)*ray0s(1)).lt.0.0d0).or.
     &       ((ray0(2)*ray0s(2)).lt.0.0d0)) goto 916
        Endif
C-------- finding the hiddeness by alignment bar -----------------------
        widfai = WALBAR / rad
        diffai = dmod(fai+widfai/2.0d0, SECTOR)
        If ( diffai.lt.widfai ) then
           If ( km.eq.1 ) goto 921
           if ( km.eq.2 ) goto 924
        End if
      End do
C---------------- calcuration of the focal plane position --------------
      t      = - ray0(3) / rayn(3)
      xf     = ray0(1) + t * rayn(1)
      yf     = ray0(2) + t * rayn(2)

      If (ihitp.eq.2) then
         If (ihits.eq.2) then
            icond = 0
            Return
         Else if (ihits.eq.0) then
            goto 960
         End if
      Else if (ihitp.eq.0) then
         If (ihits.eq.2) then
            goto 970
         Else if (ihits.eq.0) then
            goto 980
         End if
      End if

      goto 990
C----------------------- extraordinary end treatment -------------------
 910  icond   = 910
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 911  icond   = 911
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 915  icond   = 915
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 916  icond   = 916
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 920  icond   = 920
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 921  icond   = 921
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 923  icond   = 923
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 924  icond   = 924
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 925  icond   = 925
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 930  icond   = 930
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 935  icond   = 935
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 940  icond   = 940
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 945  icond   = 945
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 950  icond   = 950
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 955  icond   = 955
      wei     = 0d0
      xf      = 999d0
      yf      = 999d0
      return

 960  icond   = 960
      return

 970  icond   = 970
      return

 980  icond   = 980
      return

 990  icond   = 990
      return

      End

C=====================================================================
      Subroutine Raytracehinm(theeta,ray0,rayn,knest,km,icond)
C
C     << inner mirror hitting condition >>
C     input  : ray0   / start point of ray
C              rayn   / direction vector of ray
C              knest  / mirror nesting number
C              km     / mirror number
C     output : icond  / inner mirror hitting condition
C                       = 0 .not hit
C                       = 1 .hit
C
C====================================================================
      Implicit None
C input
      Integer knest, km
C input/output
      double precision theeta, ray0(3), rayn(3)
C output
      Integer icond
C common
      double precision r, beta, z
      Common /Raytracestruct/ r(120,4),beta(120,2),z(4)
C local
      double precision tb, tzb, a, b, c, d
      double precision w1,w2,ww1,ww2,zh1,zh2,rh,vn1,vn2,vn3,st,cosb
C
      if( knest.ne.1 ) then
        tb     = dtan(beta(knest-1,km))
Ccc        brod   = gus(sigma,amean)
Ccc        tb     = dtan(beta(knest-1,km)+brod)      

        tzb    = ( z(km*2-1) - ray0(3) )*tb - r(knest-1,km*2-1) - 0.15d0
        a      = rayn(1)**2 + rayn(2)**2 - rayn(3)**2 * tb ** 2
        b      = ray0(1)*rayn(1) + ray0(2)*rayn(2) + tb*tzb*rayn(3)
        c      = ray0(1)**2 + ray0(2)**2 - tzb**2
        d      = b**2 - a*c
        If ( d.ge.0.0 ) then
          w1     = -b/a
          w2     = dsqrt(d)/dabs(a)
          ww1    = w1 - w2
          ww2    = w1 + w2
          zh1    = ray0(3) + ww1*rayn(3)
          zh2    = ray0(3) + ww2*rayn(3)
          icond   = 0
          If ((zh1.ge.z(km*2)).and.(zh1.le.z(km*2-1)).and.
     &        (dabs(ww1).gt.0.001d0)) then
            ray0(1) = ray0(1) + (w1-w2)*rayn(1)
            ray0(2) = ray0(2) + (w1-w2)*rayn(2)
            ray0(3) = zh1
            icond   = 1
          Else if ((zh2.ge.z(km*2)).and.(zh2.le.z(km*2-1)).and.
     &          (dabs(ww2).gt.0.001d0)) then
            ray0(1) = ray0(1) + (w1+w2)*rayn(1)
            ray0(2) = ray0(2) + (w1+w2)*rayn(2)
            ray0(3) = zh2
            icond   = 1
          End if

          If   ( icond.eq.1 ) then
            rh     = ray0(1)**2 + ray0(2)**2
            rh     = dsqrt(rh)
            cosb   = dcos(beta(knest,km)) / rh
            vn1    = cosb * ray0(1)
            vn2    = cosb * ray0(2)
            vn3    = - dsin(beta(knest,km))
Ccc            vn1    = dcos(beta(knest,km)+brod) * ray0(1) / rh
Ccc            vn2    = dcos(beta(knest,km)+brod) * ray0(2) / rh
Ccc            vn3    = - dsin(beta(knest,km)+brod)

            st        = rayn(1)*vn1 + rayn(2)*vn2 + rayn(3)*vn3
            theeta    = - dasin(st)
            rayn(1)   = rayn(1) - 2d0*st*vn1
            rayn(2)   = rayn(2) - 2d0*st*vn2
            rayn(3)   = rayn(3) - 2d0*st*vn3
          End if
        Else
          icond   = 0
        End if
      Else
        icond   = 0
      End if

      Return
      End

C=====================================================================
      Subroutine Raytraceraycha(eng,theeta,ray0,rayn,
     &                           rad,fai,knest,km,icond)
C
C     << ray line changing for reflection >>
C     input  : theeta / incident angle for the mirror (radiun)
C              ray0   / start point of ray
C              rayn   / direction vector of ray
C              knest  / mirror nesting number
C              km     / mirror number
C     output : ray0   / start point of ray
C              rayn   / direction vector of ray
C              rad    / start point of ray at secondary top
C              fai    / start point of ray at secondary top
C              icond  / outer mirror hitting condition
C                       = 0 .not hit
C                       = 1 .hit
C
C====================================================================
      Implicit None
C input
      double precision eng
      Integer knest, km
C input/output
      double precision theeta, ray0(3), rayn(3)
C output
      double precision rad, fai
      Integer icond
C common
      double precision r, beta, z
      Common /Raytracestruct/ r(120,4),beta(120,2),z(4)
      double precision sigma1, sigma2, amean, consct
      Common /Raytracebroad/ sigma1, sigma2, amean, consct
C local
      double precision tb, tzb, a, b, c, d, brod, raythe
      double precision w1,w2,ww1,ww2,zh1,zh2,rh,vn1,vn2,vn3,st,cosb
      double precision rpph
C constants
      double precision DEG2RAD,SCT2EDGE1,SCT2EDGE2
      double precision SCT13EDGE1,SCT13EDGE2
      Parameter (DEG2RAD = 0.017453292519943295769d0)
      parameter (SCT2EDGE1 = 6.428d0)
      parameter (SCT2EDGE2 = 12.857d0)
      parameter (SCT13EDGE1 = 90.d0 - SCT2EDGE2)
      parameter (SCT13EDGE2 = 90.d0 - SCT2EDGE1)
C function
      double precision Raytracescat, Raytraceflor, Raytracenvfluct
C
      tb     = dtan(beta(knest,km))
Ccc      tb     = dtan(beta(knest,km)+brod)
      tzb    = ( z(km*2-1) - ray0(3) )*tb - r(knest,km*2-1)
      a      = rayn(1)**2 + rayn(2)**2 - rayn(3)**2 * tb ** 2
      b      = ray0(1)*rayn(1) + ray0(2)*rayn(2) + tb*tzb*rayn(3)
      c      = ray0(1)**2 + ray0(2)**2 - tzb**2
      d      = b**2 - a*c

      If ( d.ge.0.0 ) then
         w1     = -b/a
         w2     = dsqrt(d)/dabs(a)
         ww1    = w1-w2
         ww2    = w1+w2
         zh1    = ray0(3) + ww1*rayn(3)
         zh2    = ray0(3) + ww2*rayn(3)
         icond  = 0
         If ((zh1.ge.z(km*2)).and.(zh1.le.z(km*2-1)).and.
     &        (dabs(ww1).gt.0.01)) then
            ray0(1) = ray0(1) + (w1-w2)*rayn(1)
            ray0(2) = ray0(2) + (w1-w2)*rayn(2)
            ray0(3) = zh1
            icond   = 1
         Else if ((zh2.ge.z(km*2)).and.(zh2.le.z(km*2-1)).and.
     &           (dabs(ww2).gt.0.001)) then
            ray0(1) = ray0(1) + (w1+w2)*rayn(1)
            ray0(2) = ray0(2) + (w1+w2)*rayn(2)
            ray0(3) = zh2
            icond   = 1
         End if

         brod = 0.0
         If ( icond.ne.0 ) then
C            write(*,*) 'O.K.6'
            rh      = ray0(1)**2 + ray0(2)**2
            rh      = dsqrt(rh)
            cosb    = - dcos(beta(knest,km)) / rh
            vn1     = cosb * ray0(1)
            vn2     = cosb * ray0(2)
            vn3     = dsin(beta(knest,km))
            st      = rayn(1)*vn1 + rayn(2)*vn2 + rayn(3)*vn3
            theeta  = - dasin(st)
            
            rayn(1) = rayn(1) - 2d0*st*vn1
            rayn(2) = rayn(2) - 2d0*st*vn2
            rayn(3) = rayn(3) - 2d0*st*vn3

            If ((consct.ge.1.0).or.(sigma1.gt.0.0)) then
 10            If (consct.ge.1d0) then
                  brod = Raytracescat(theeta,eng)
               End if
               If (sigma1.gt.0d0) then
                  If (sigma2.gt.0.0) then
                     rpph=datan(dabs(ray0(2)/ray0(1)))/DEG2RAD
                     If ((rpph.gt.SCT2EDGE1.and.rpph.lt.SCT2EDGE2)
     & .or.(rpph.gt.SCT13EDGE1.and.rpph.lt.SCT13EDGE2)) then
                        brod = brod + Raytraceflor(sigma2)
                     Else
                        brod = brod + Raytracenvfluct()
                     End if
                  Else
                     brod = brod + Raytracenvfluct()
                  End if
               End if
               If ((theeta+brod).le.0d0) then
                  brod = 0d0
                  goto 10
               End if
               raythe  = dacos(-rayn(3))
               cosb = dsin(raythe+brod) / dsin(raythe)
               rayn(1) = rayn(1) * cosb
               rayn(2) = rayn(2) * cosb
               rayn(3) = -dcos(raythe+brod)
            End if
         End if
      Else
         icond = 0
      End if

      Return
      End

C=====================================================================
      double precision function Raytracerefrd (ang, eng)
C
C     << reflectivity table read >>
C     input  :  ang     / incident angle (radian)
C     output :  refrd   / reflectivity
C
C=====================================================================
      Implicit None
C input
      double precision ang, eng
C constant
      double precision MPI
      Parameter (MPI = 3.1415926535897932385d0)
C local
      double precision energy, delta, beta, rough
      Save energy, delta, beta, rough
      double precision fw1, fw2, b, w, w1, w2, a2, b2, w3, w4, sa
C function
      double precision Raytraceclcdel, Raytraceclcbet

      data energy /-999d0/
      data rough /0d0/
C
      If ( eng.ne.energy ) then
         energy = eng
         delta = Raytraceclcdel(79,eng)
         beta = Raytraceclcbet(79,eng)
      End if
C
      w = ( 1d0 - delta )**2
      b = beta**2
      fw1  = w - b
      fw2  = 4d0 * w * b
      w2    = fw1 - dcos(ang)**2
      w1    = w2*w2 + fw2
      w = dsqrt(w1)
      a2    = 0.5d0 * ( w + w2 )
      b2    = 0.5d0 * ( w - w2 )
      sa = dsin(ang)
      w = a2 + b2 + sa**2
      b = 2.0*dsqrt(a2)*sa
      w3    = w - b
      w4    = w + b

      Raytracerefrd = w3/w4
      If ( rough .gt. 0d0 ) then
        Raytracerefrd = Raytracerefrd * 
     &                   exp(-(4d0*MPI*sa*rough*eng/12.4d0)**2)
      End if
      
      return
      end

C=====================================================================
      double precision function Raytracecrefrd (ang, eng)
C
C     << reflectivity table read >>
C     input  :  ang     / incident angle (radian)
C     output :  crefrd  / reflectivity
C
C=====================================================================
      Implicit None
C input
      double precision ang, eng
C local
      double precision energy, delta, beta
      Save energy, delta, beta
      double precision fw1, fw2, b, w, w1, w2, a2, b2, w3, w4, sa
C function
      double precision Raytraceclcdel, Raytraceclcbet

      data energy /-999d0/
C
      If ( eng.ne.energy ) then
         energy = eng
         delta = Raytraceclcdel(6,eng)
         beta = Raytraceclcbet(6,eng)
      End if
C
      w = ( 1d0 - delta )**2
      b = beta**2
      fw1  = w - b
      fw2  = 4d0 * w * b
      w2    = fw1 - dcos(ang)**2
      w1    = w2*w2 + fw2
      w = dsqrt(w1)
      a2    = 0.5d0 * ( w + w2 )
      b2    = 0.5d0 * ( w - w2 )
      sa = dsin(ang)
      w = a2 + b2 + sa**2
      b = 2d0*dsqrt(a2)*sa
      w3    = w - b
      w4    = w + b
      
      Raytracecrefrd = w3/w4

      return
      end

C==============================================================
       double precision function Raytraceflor (gamma)
C                    input gamma(arcmin.)
C                     
C                    output flor(rad.)
C
C
C==============================================================
      Implicit None
C input
      double precision gamma
C constant
      double precision MPI
      Parameter (MPI = 3.1415926535897932385d0)
C function
      double precision Raytraceran
C local
      double precision gamma1, a, b, flor
C
      gamma1 = gamma / 60d0
      a = -2d0 / gamma1
      b =  2d0 / gamma1

      flor = dtan((datan(b) - datan(a)) * Raytraceran() + datan(a))
      flor = flor * gamma / 2d0

      flor = flor * MPI / 1.8d2
      
      Raytraceflor = flor

      Return
      End

C=====================================================================
      double precision function Raytracescat (theeta,eng)
C
C     << make composite distribution >>
C     input  :  theeta(rad.)
C               eng(kev)
C     output :  scat(rad.)
C
C=====================================================================
      Implicit None
C input
      double precision theeta, eng
C constant
      double precision DEG2RAD, MIN2RAD, RAD2MIN
      Parameter (DEG2RAD = 0.017453292519943295769d0)
      Parameter (MIN2RAD = DEG2RAD/60d0)
      Parameter (RAD2MIN = 1.d0/MIN2RAD)
C common
      double precision sigma1 ,sigma2, amean, consct
      Common /Raytracebroad/  sigma1, sigma2, amean, consct
      double precision s3, c3
      Common /Raytracecalsct/ s3, c3
C local
      Integer i
      double precision core, r1, r2, ar1, rthr, thm
      double precision pwave, apw, fscat, f, fc
C function
      double precision Raytraceran
C
      If ( theeta .lt. 0 ) then
         write(*,*) 'theeta=', theeta
         theeta = -theeta
      End if
      
      thm = theeta*RAD2MIN
C      athm = dabs(thm)
C---------- definition of core width ---
      core = 0.8840d0
C---------------------------------------
 20   continue
C---------------------------------------------------------------------
C     generation of random number
C--------------------------------------------------------------------
C      do 10 i=1,41
C          r1=i-21.
C Y.ISHISAKI 95/09/19
CYI      r1 = (SimASCADRNDTS()-0.5d0)*100d0
C      r1 = SimASCADRNDTS()*(50.d0+thm)-thm
C      r2 = SimASCADRNDTS()
      r1=(Raytraceran()-0.5)*100.
C      r1 = Raytraceran()*(50.d0+thm)-thm
      r2=Raytraceran()
      ar1 = dabs(r1)
      rthr = r1*MIN2RAD
C--------------------------------------------------------------
C      calculation of core distribution
C--------------------------------------------------------------
      If ((-rthr).gt.theeta) goto 20
      If (ar1.ge.2d0) then
         If (ar1.lt.10d0) then
            If (r2.gt.0.2d0) goto 20
         Else
            If (r2.gt.0.015d0) goto 20
         Endif
         f = dlog10(ar1)
         fc = 10**(-0.5456d0-1.71887d0*f-0.70664d0*f*f)
      Else
         f = r1/core
         fc=dexp(-f*f/2d0)
      End if
C--------------------------------------------------------------
C      calculation of scattering component
C--------------------------------------------------------------
      pwave = 0.50671d0*eng*dsin(theeta+rthr/2d0)*dsin(rthr)
      apw = dabs(pwave)
      fscat = 68.8797d0*(1.d0+s3)*eng**3*dsin(theeta)*dsin(theeta+rthr)
     &     * dsin(theeta+rthr)* dexp(-9329d0*(1.d0+c3)*apw)
      If ((consct.ge.2d0).and.(consct.lt.3d0)) then
         fscat = 0d0
      Else if (consct.ge.3d0) then
         fc    = 0d0
      End if
      f=(fc+fscat)*0.8d0
C--------------------------------------------------------------
C       test print of fscat
C--------------------------------------------------------------
C      write(*,1003)eng,thm,r1,r2,f
C 1003  format(1x,5f12.5)
C--------------------------------------------------------------
      if (r2.gt.f) goto 20
 10   continue

      Raytracescat = rthr

      Return
      End

C=======================================================================
      double precision function Raytraceclcdel(jz,er)
C
C
C***********************************************************************
      Implicit None
C input
      Integer jz
      double precision er
C constant
      double precision MPI
      Parameter (MPI = 3.1415926535897932385d0)
C local
      Integer i, j, k, l, n, kz, izh
      double precision sum, x, zh, wl, clcdel
C
      Integer iz(7)
      double precision a(7), rho(7), e(22,7)
      double precision c, h, rcl, avn
      
      data c, h /2.99793d+10, 4.1354d-18/
      data rcl, avn /2.81785d-13, 6.02486d+23/

C   carbon's constant
      data iz(7),a(7),rho(7) /6, 12.011d0, 1.54d0/

C   nickel's constant
      data iz(4),a(4),rho(4) /28, 58.71d0, 6.92d0/

C   gold's constant
      data iz(6),a(6),rho(6) /79, 197.0d0, 18.88d0/

C   energy level of k,l(i),l(ii),.....
C   carbon
      data e(1,7),e(2,7),e(3,7),e(4,7)
     &     /0.2842d0, 0.0d0, 0.0d0, 0.0d0/

C   nickel
      data e(1,4),e(2,4),e(3,4),e(4,4)
     &     /8.333d0, 1.008d0, 0.872d0, 0.855d0/
      data e(5,4),e(6,4),e(7,4),e(8,4),e(9,4)
     &     /0.112d0, 0.068d0, 0.068d0, 0.004d0, 0.004d0/

C   gold
      data e(1,6),e(2,6),e(3,6),e(4,6)
     &   /80.72d0, 14.35d0, 13.73d0, 11.919d0/
      data e(5,6),e(6,6),e(7,6),e(8,6),e(9,6)
     &   /3.425d0, 3.148d0, 2.743d0, 2.291d0, 2.206d0/
      data e(10,6),e(11,6),e(12,6),e(13,6),e(14,6),e(15,6),e(16,6)
     &   /0.759d0, 0.644d0, 0.545d0, 0.352d0, 0.334d0, 0.086d0, 0.083d0/
      data e(17,6),e(18,6),e(19,6),e(20,6),e(21,6),e(22,6)
     &   /0.108d0, 0.072d0, 0.054d0, 0.003d0, 0.003d0, 0.001d0/
C
      do 10 i=1,7
        if(jz.eq.iz(i)) goto 20
 10   continue
      Raytraceclcdel=0.0d0
      return
 20   continue
      sum=iz(i)
      kz=0
      do 40 n=1,7
        do 35 l=1,n
          do 30 j=l,l+1
            if(j.eq.1)goto 30
            k=(n-1)**2+l+j-2
            if(k.gt.22)goto 50
            if(e(k,i)) 40,40,25
 25         x=er/e(k,i)
            if(x.eq.1.d0) then
              sum = 0.d0
              goto 50
            else
              zh= dble(min(2*(j-1),iz(i)-kz))
              sum=sum+zh*dlog(dabs(x**2-1.0d0))/(x**2)
              izh=int(zh)
              kz=kz+izh
            endif
            if(kz.ge.iz(i)) goto 50
 30       continue
 35     continue
 40   continue
 50   continue

      wl=h*c/er
      clcdel=(rcl*wl*wl*avn*rho(i)/(2.0*MPI*a(i)))*sum
      if(clcdel.lt.0.0) clcdel=0.0
      if(jz.eq.79) then
         if(er.lt.0.152) then
           clcdel = 1.065040  * dexp(-24.29330 * er)
         else if((er.ge.0.152).and.(er.lt.0.199)) then
           clcdel = 0.418700  * dexp(-18.15499 * er)
         else if((er.ge.0.199).and.(er.lt.0.286)) then
           clcdel = 0.031777  * dexp( -5.19690 * er)
         else if((er.ge.0.286).and.(er.lt.0.980)) then
           clcdel = 0.0119859 * dexp( -1.78744 * er)
         else if((er.ge.0.980).and.(er.lt.2.145)) then
           clcdel = 0.008700  * dexp( -1.46040 * er)
         else if((er.ge.2.145).and.(er.lt.2.206)) then
           clcdel = -0.0133595  *(er-2.09461)*(er-2.09461) + 0.000412683
         else if((er.ge.2.206).and.(er.lt.2.291)) then
           clcdel = -0.0174020  *(er-2.23930)*(er-2.23930) + 0.000309502
         else if((er.ge.2.291).and.(er.lt.2.397)) then
           clcdel = -0.00349211 *(er-2.39680)*(er-2.39680) + 0.000327174
         else if((er.ge.2.397).and.(er.lt.2.743)) then
           clcdel = -0.000520486*(er-2.39680)*(er-2.39680) + 0.000327174
         else if((er.ge.2.743).and.(er.lt.2.800)) then
           clcdel = -0.00908291 *(er-2.78728)*(er-2.78728) + 0.000282599
         else if((er.ge.2.800).and.(er.lt.3.450)) then
           clcdel = -0.00012313 * er + 0.0006241
         endif
         if((er.ge.2.3).and.(er.lt.3.68)) then
            clcdel = clcdel*(1.+0.092*(er-1.9465)/1.7335)
         elseif((er.ge.3.68).and.(er.lt.5.32)) then
            clcdel = clcdel*(1.+0.092*(5.32-er)/1.64)
         else if(er.ge.5.32) then
            clcdel = clcdel*(1.+0.044422*(er-5.32)
     &                         -0.0016108*(er*er-5.32*5.32))
         endif
Cccccccccccccccccccccc  added at 1995.7.3 (crab tune sono2)
                 if((er.ge.2.7).and.(er.lt.4.5)) then
            clcdel = clcdel*(-0.01638*er+1.0437)
         elseif((er.ge.4.5).and.(er.lt.5.8)) then
            clcdel = clcdel*(0.02361*er+0.8656)
         elseif((er.ge.5.8).and.(er.lt.7.2)) then
            clcdel = clcdel*(-0.02071*er+1.1221)
         elseif((er.ge.7.2).and.(er.le.10.0)) then
            clcdel = clcdel*(0.005406*er+0.9345)
         endif
Ccccccccccccccccccccc  added by  1995.10.27 (crab tune sono3)
                 if((er.ge.5.58).and.(er.lt.6.40)) then
            clcdel = clcdel*(-0.02955*er+1.167)
         elseif((er.ge.6.40).and.(er.lt.6.89)) then
            clcdel = clcdel*(0.02655*er+0.8080)
         elseif((er.ge.6.89).and.(er.lt.7.94)) then
            clcdel = clcdel*(0.002545*er+0.9740)
         elseif((er.ge.7.94).and.(er.le.8.53)) then
            clcdel = clcdel*(-0.0225*er+1.173)
         elseif((er.ge.8.53).and.(er.le.10.0)) then
            clcdel = clcdel*(0.001786*er+0.9659)
         endif
Cccccccccccccccccccccc
C         if((er.ge.2.7).and.(er.lt.3.6)) then
C            clcdel = clcdel * (1 + (er-2.7)*0.04)
C         elseif((er.ge.3.6).and.(er.lt.4.5)) then
C            clcdel = clcdel * (1 + (4.5-er)*0.04)
C         else if(er.ge.4.500) then
C           clcdel = clcdel * ( 1.+ 6.*(er-4.5)/ 350.)
C         endif
      endif

      Raytraceclcdel = clcdel
      return
      end

C*******************************************************************
      double precision function Raytraceclcbet(jz,e)
C    clcbet:imaginary part of reflaction
C    jz:atomic number
C    e:energy (kev)
C*******************************************************************
      Implicit None
C input
      Integer jz
      double precision e
C local
      Integer i, j
      double precision xmu, xml, el2, el, xlogmu
C
      double precision d(3,60,13), factor(13)
      integer z(13)
      data z/1,2,4,5,6,8,9,13,22,24,28,47,79/
C     carbon
      data factor(5)/0.1513d-7/
      data d(1,1,5),d(2,1,5),d(3,1,5)/0.1085d0,2*1.92d4/
      data d(1,2,5),d(2,2,5),d(3,2,5)/0.2122d0,2*4.50d3/
      data d(1,3,5),d(2,3,5),d(3,3,5)/0.2833d0,2303.0d0,58392.0d0/
      data d(1,4,5),d(2,4,5),d(3,4,5)/0.5728d0,2*9.9033d0/
      data d(1,5,5),d(2,5,5),d(3,5,5)/1.000d0,2*2.18d3/
      data d(1,6,5),d(2,6,5),d(3,6,5)/1.012d0,2*2.15d3/
      data d(1,7,5),d(2,7,5),d(3,7,5)/5.000,2*18.6d0/
      data d(1,8,5),d(2,8,5),d(3,8,5)/10.00d0,2*1.98d0/
      data d(1,9,5),d(2,9,5),d(3,9,5)/20.00d0,2*0.2141d0/
      data d(1,10,5),d(2,10,5),d(3,10,5)/40.00d0,2*0.03284d0/
      data d(1,11,5),d(2,11,5),d(3,11,5)/80.00d0,2*0.02031d0/
      data d(1,12,5),d(2,12,5),d(3,12,5)/100.00d0,2*0.02146d0/
C     nickel
      data factor(11)/0.8694d-7/
      data d(1,1,11),d(2,1,11),d(3,1,11)/0.100d0,2*8.00d4/
      data d(1,2,11),d(2,2,11),d(3,2,11)/0.150d0,2*5.70d4/
      data d(1,3,11),d(2,3,11),d(3,3,11)/0.200d0,2*3.60d4/
      data d(1,4,11),d(2,4,11),d(3,4,11)/0.853d0,2160.0d0,7800.0d0/
      data d(1,5,11),d(2,5,11),d(3,5,11)/0.871d0,7400.0d0,9000.0d0/
      data d(1,6,11),d(2,6,11),d(3,6,11)/1.015d0,6400.0d0,9500.0d0/
      data d(1,7,11),d(2,7,11),d(3,7,11)/1.500d0,2*3.75d3/
      data d(1,8,11),d(2,8,11),d(3,8,11)/8.331d0,38.7d0,345.0d0/
      data d(1,9,11),d(2,9,11),d(3,9,11)/10.000d0,2*2.24d2/
      data d(1,10,11),d(2,10,11),d(3,10,11)/100.00d0,2*0.224d0/
      data d(1,11,11),d(2,11,11),d(3,11,11)/100.00d0,2*0.224d0/
      data d(1,12,11),d(2,12,11),d(3,12,11)/100.00d0,2*0.224d0/
C   gold
      data factor(13)/1.896d-7/
      data d(1,1,13),d(2,1,13),d(3,1,13)/0.0915d0,2*33400/
      data d(1,2,13),d(2,2,13),d(3,2,13)/0.1085d0,2*17100/
      data d(1,3,13),d(2,3,13),d(3,3,13)/0.1140d0,2*14000/
      data d(1,4,13),d(2,4,13),d(3,4,13)/0.1328d0,2*8670/
      data d(1,5,13),d(2,5,13),d(3,5,13)/0.1487d0,2*7980/
      data d(1,6,13),d(2,6,13),d(3,6,13)/0.1511d0,2*8020/
      data d(1,7,13),d(2,7,13),d(3,7,13)/0.1717d0,2*9370/
      data d(1,8,13),d(2,8,13),d(3,8,13)/0.1833d0,2*10400/
      data d(1,9,13),d(2,9,13),d(3,9,13)/0.1926d0,2*11300/
      data d(1,10,13),d(2,10,13),d(3,10,13)/0.2122d0,2*13000/
      data d(1,11,13),d(2,11,13),d(3,11,13)/0.2770d0,2*15200/
      data d(1,12,13),d(2,12,13),d(3,12,13)/0.3177d0,2*15300/
      data d(1,13,13),d(2,13,13),d(3,13,13)/0.3924d0,2*15400/
      data d(1,14,13),d(2,14,13),d(3,14,13)/0.3953d0,2*15400/
      data d(1,15,13),d(2,15,13),d(3,15,13)/0.4522d0,2*13700/
      data d(1,16,13),d(2,16,13),d(3,16,13)/0.5113d0,2*12100/
      data d(1,17,13),d(2,17,13),d(3,17,13)/0.5249d0,2*11800/
      data d(1,18,13),d(2,18,13),d(3,18,13)/0.5563d0,2*10900/
      data d(1,19,13),d(2,19,13),d(3,19,13)/0.5728d0,2*10500/
      data d(1,20,13),d(2,20,13),d(3,20,13)/0.6374d0,2*10200/
      data d(1,21,13),d(2,21,13),d(3,21,13)/0.6768d0,2*9290/
      data d(1,22,13),d(2,22,13),d(3,22,13)/0.7050d0,2*8790/
      data d(1,23,13),d(2,23,13),d(3,23,13)/0.7762d0,2*7950/
      data d(1,24,13),d(2,24,13),d(3,24,13)/0.8515d0,2*6760/
      data d(1,25,13),d(2,25,13),d(3,25,13)/0.9297d0,2*5790/
      data d(1,26,13),d(2,26,13),d(3,26,13)/1.0117d0,2*4970/
      data d(1,27,13),d(2,27,13),d(3,27,13)/1.0410d0,2*4700/
      data d(1,28,13),d(2,28,13),d(3,28,13)/1.1880d0,2*3620/
      data d(1,29,13),d(2,29,13),d(3,29,13)/1.2536d0,2*3250/
      data d(1,30,13),d(2,30,13),d(3,30,13)/1.4867d0,2*2310/
      data d(1,31,13),d(2,31,13),d(3,31,13)/1.7400d0,2*1660/
      data d(1,32,13),d(2,32,13),d(3,32,13)/2.0424d0,2*1180/
      data d(1,33,13),d(2,33,13),d(3,33,13)/2.1659d0,2*1040/
      data d(1,34,13),d(2,34,13),d(3,34,13)/2.206d0,914.34,2489.2/
      data d(1,35,13),d(2,35,13),d(3,35,13)/2.291d0,2284.3,3333.2/
      data d(1,36,13),d(2,36,13),d(3,36,13)/2.743d0,2189.5,2522.9/
      data d(1,37,13),d(2,37,13),d(3,37,13)/3.150d0,1816.5,1929.6/
      data d(1,38,13),d(2,38,13),d(3,38,13)/3.425d0,1577.9,1648.3/
      data d(1,39,13),d(2,39,13),d(3,39,13)/3.6917d0,2*1510/
      data d(1,40,13),d(2,40,13),d(3,40,13)/4.4663d0,2*953/
      data d(1,41,13),d(2,41,13),d(3,41,13)/4.5108d0,2*930/
      data d(1,42,13),d(2,42,13),d(3,42,13)/4.9522d0,2*737/
      data d(1,43,13),d(2,43,13),d(3,43,13)/5.4147d0,2*589/
      data d(1,44,13),d(2,44,13),d(3,44,13)/5.8988d0,2*474/
      data d(1,45,13),d(2,45,13),d(3,45,13)/6.9303d0,2*314/
      data d(1,46,13),d(2,46,13),d(3,46,13)/7.4782d0,2*258/
      data d(1,47,13),d(2,47,13),d(3,47,13)/8.0478d0,2*214/
      data d(1,48,13),d(2,48,13),d(3,48,13)/8.6389d0,2*178/
      data d(1,49,13),d(2,49,13),d(3,49,13)/9.8864d0,2*126/
      data d(1,50,13),d(2,50,13),d(3,50,13)/11.92d0,75.2d0,187.1d0/
      data d(1,51,13),d(2,51,13),d(3,51,13)/13.73d0,128.1d0,177.36d0/

      Raytraceclcbet=0.0
      do 10 i=1,13
        if(jz.eq.z(i)) goto 20
 10   continue

      xmu=310415926d+31
      return

 20   continue
      if(jz.eq.79) goto26
      do 25 j=2,12
        if(e.lt.d(1,j,i)) goto 30
 25   continue
      j=12
      goto 30
 26   continue
      do 27 j=2,51
        if(e.lt.d(1,j,i)) goto 30
 27   continue
      

 30   xml=dlog(d(2,j,i)/d(3,j-1,i))
      el2=dlog(d(1,j,i)/d(1,j-1,i))
      el=dlog(e/d(1,j-1,i))

      xlogmu=dlog(d(3,j-1,i))+el*xml/el2
      xmu=exp(xlogmu)
C     Raytraceclcbet=1.896d-7*xmu/e
      Raytraceclcbet=factor(i)*xmu/e
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------1---------2---------3---------4---------5---------6---------7--
C SUBROUTINE : STPRNDINIT
C     step function initialization for stprnd     94.09.22 A. Furuzawa
C
      subroutine Raytracestprndinit()
      Implicit None
C
C     Output
C       stpx1 :R8  :left edge position of mid step
C       stpx2 :R8  :left edge position of low step
C       stpy0 :R8  :hight of top step
C       stpy1 :R8  :hight of mid step
C       stpy2 :R8  :hight of low step
C       stps0 :R8  :area of top step
C       stps1 :R8  :area of mid step
C       stps2 :R8  :area of low step
C
C       * : not available now
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      common /Raytracevstprnd/ stpx1,stpx2,stpy0,stpy1,stpy2,stps0,
     &     stps1,stps2,stpc
      common /Raytracevfluct/ ard,brd,crd,xmaxrd

C constant
      double precision MPI
      Parameter (MPI = 3.1415926535897932385d0)
C     For output
      double precision stpx1,stpx2,stpy0,stpy1,stpy2
      double precision stps0,stps1,stps2,stpc
C     LOCAL
      double precision y0,y1,y2,s0,s1,s2,x1,x2,c,ard,brd,crd,xmaxrd
C     EXTERNAL
      double precision Raytraceffluct

      data x1/5.d-1/,x2/5.d0/

      write(6,*) 'stprnd init'
      stpx1=x1/6.d1/1.8d2*MPI
      stpx2=x2/6.d1/1.8d2*MPI
C      write(6,*) stpx1,stpx2
      y0=1.1d0*Raytraceffluct(0.d0)
      s0=y0*stpx1
C      write(6,*) y0,s0
      y1=1.1d0*Raytraceffluct(stpx1)
      s1=y1*(stpx2-stpx1)
C      write(6,*) y1,s1
      y2=1.1d0*Raytraceffluct(stpx2)
      s2=y2*(xmaxrd-stpx2)
C      write(6,*) y2,s2
      stpc=2.d0*(s0+s1+s2)

      stpy0=y0/stpc
      stpy1=y1/stpc
      stpy2=y2/stpc
      stps0=s0/stpc
      stps1=s1/stpc
      stps2=s2/stpc

C      write(6,*) stps0+stps1+stps2

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------1---------2---------3---------4---------5---------6---------7--
C SUBROUTINE : STPRND
C     generate random number of step-like probability function
C                                                 94.09.22 A. Furuzawa
C
      subroutine Raytracestprnd (rx,y)
      Implicit None
C
C     Input
C       stpx1 :R8  :left edge position of mid step
C       stpx2 :R8  :left edge position of low step
C       stpy0 :R8  :hight of top step
C       stpy1 :R8  :hight of mid step
C       stpy2 :R8  :hight of low step
C       stps0 :R8  :area of top step
C       stps1 :R8  :area of mid step
C       stps2 :R8  :area of low step
C       stpc  :R8  :factor
C     Output
C       rx    :R8  :generated random number(rad.)
C       y     :R8  :height of step function * stpc
C       * : not available now
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      common /Raytracevstprnd/ stpx1,stpx2,stpy0,stpy1,stpy2,stps0,
     &     stps1,stps2,stpc

C     For INPUT
      double precision stpx1,stpx2,stpy0,stpy1
      double precision stpy2,stps0,stps1,stps2,stpc
C     For OUTPUT
      double precision rx,y
C     For LOCAL
      double precision ars, rs
C function
      double precision Raytraceran
C
      rs=Raytraceran()-5.d-1
      ars=dabs(rs)

      if (ars.le.stps0) then
         rx=ars/stpy0
         y=stpy0*stpc
      else if (ars.gt.stps0.and.ars.le.(stps0+stps1)) then
         rx=(ars-stps0)/stpy1+stpx1
         y=stpy1*stpc
      else if (ars.gt.(stps0+stps1).and.ars.le.5.d-1) then
         rx=(ars-(stps0+stps1))/stpy2+stpx2
         y=stpy2*stpc
      else
         call fcerr('ERROR : RX outside of region')
         stop
      endif

      if (rs.le.0.0d0) rx = -1.d0 * rx

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------1---------2---------3---------4---------5---------6---------7--
C SUBROUTINE : NVFLUCT
C     generate random number of user-defined probability function
C                                                 94.09.22 A. Furuzawa
C
      double precision function Raytracenvfluct()
      Implicit None
C
C     Input
C       gamma :R8  :lorenzian's gamma(arcmin.)
C       a     :R8  :inclination of linear function(/arcmin.)
C
C     Output
C       nvfluct:R8 :user-defined distribution random number(rad.)
C       * : not available now
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     For OUTPUT
C      double precision nvfluct

C     LOCAL
      double precision ry,fx

C     For STPRND
      double precision rx,y

C     EXTERNAL
      double precision Raytraceffluct, Raytraceran

 10   call Raytracestprnd(rx,y)
      ry=Raytraceran()*y
      fx=Raytraceffluct(rx)

      if (ry.gt.fx) goto 10

      Raytracenvfluct=rx

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------1---------2---------3---------4---------5---------6---------7--
C SUBROUTINE : FFLUCTINIT
C     initialize user-defined probability function
C                                               94.09.22 A. Furuzawa
C
      subroutine Raytraceffluctinit(gamma,a)
      Implicit None
C
C     Input
C       gamma :R8  :lorenzian's gamma(arcmin.)
C       a     :R8  :inclination of linear function(/arcmin.)
C
C       * : not available now
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      common /Raytracevfluct/ ard,brd,crd,xmaxrd

C constant
      double precision MPI
      Parameter (MPI = 3.1415926535897932385d0)
C     For INPUT
      double precision gamma,a
C     For OUTPUT
      double precision ard,brd,crd,xmaxrd
C     For LOCAL
      double precision b,xmax

      data xmax/1.2d2/

      write(6,*) 'ffunc init'
      ard = a*6.d1*1.8d2/MPI
      b=gamma/2.d0
      brd = b/6.d1/1.8d2*MPI
      xmaxrd=xmax/6.d1/1.8d2*MPI
C      write(6,*) a,b,xmax
C      write(6,*) ard,brd,crd,xmaxrd
      crd = 1.d0/(ard*dlog(xmax*xmax/b/b+1.d0)+2.d0/brd*datan(xmax/b))

      return
      end
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------1---------2---------3---------4---------5---------6---------7--
C FUNCTION : FFLUCT
C     user-defined probability function
C                                               94.09.22 A. Furuzawa
C
      double precision function Raytraceffluct(x)
      Implicit None
C
C     Input
C       x     :R8 :x(rad.)
C     Output
C       nvfluct:R8 :user-defined distribution random number(rad.)
C       * : not available now
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      common /Raytracevfluct/ ard,brd,crd,xmaxrd

C     For INPUT
      double precision x,ard,brd,crd,xmaxrd
C     For OUTPUT
C      double precision ffluct

      Raytraceffluct=crd*(ard*dabs(x)+1.d0)/(x*x+brd*brd)

      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------1---------2---------3---------4---------5---------6---------7--
C SUBROUTINE : RAYTRACERANINIT
C   initialize the random number generation routine 95.12.20 A. Furuzawa
C             (Original  :  RNDLCINI from "Keisan butsuri")
      subroutine Raytraceraninit(irseed)
C
C     Input                            
C       irseed:I4  : initial seed for random number generation
C
C     Output (Common)
C       iseed  :I4  : seed for random number generation(Common/rndsd/)
C
C       * : not available now
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     For INPUT
      integer irseed

C     For Common
      common /rndsd/ iseed
      integer iseed

      iseed = irseed

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------1---------2---------3---------4---------5---------6---------7--
C DOUBLE PRECISION FUNCTION : RAYTRACERAN
C     double precision random number(0-1) generation               
C                                             95.12.20 A. Furuzawa
C                    (Original : DRNDLC from "Keisan Butsuri")
      double precision function Raytraceran()
C
C     Input   (Common)
C       iseed :I4  :seed for random number generation(Common/rndsd/)
C
C     Output                           
C       Raytraceran:R8  : double precision random number
C       iseed :I4  :seed for random number generation(Common/rndsd/)
C
C       * : not available now
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     For LOCAL
      double precision dconst
      parameter (dconst=1.d0/2147483648.d0)

C     For Common
      common /rndsd/ iseed
      integer iseed

      iseed = iseed * 48828125
      if (iseed.le.0) iseed = (iseed + 2147483647) + 1
      Raytraceran = dble(iseed) * dconst

      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------1---------2---------3---------4---------5---------6---------7--
C SUBROUTINE : RAYTRACEPHOTONGEN
C     photon generator                96.04.13 A. Furuzawa
C
      subroutine raytracephotongen(phth,phphi)
C
C     Input(Common)                            
C       diftyp:R8  :source type(<0:point, =1:beta model, =0:flat diffuse
C       angin :R8  :source position(off-axis angle;deg)
C       angphi:R8  :source position(  phase angle ;deg)
C       difrad:R8  :source radius
C                     (flat diffuse:radius, core radius:beta model)
C       difbet:R8  :beta(beta model only)
C     Output
C       phth  :R8  :direction of incident photon(radial;deg)
C       phphi :R8  :direction of incident photon(phase;deg)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none

C     For PARAMETER
      double precision MPI, M2PI, M90DEG, DEG2RAD
      Parameter (MPI = 3.1415926535897932385d0)
      Parameter (M2PI = 2.d0*MPI)
      Parameter (M90DEG = MPI/2d0)
      Parameter (DEG2RAD = 0.017453292519943295769d0)

C     For OUTPUT
      double precision phth,phphi

C     For LOCAL
      double precision theeta,phi
      double precision bb,bc,tta1,tta2,tta,theed,phid,drayn(3),rayn(3)

C     For COMMON
C      integer diftyp
      double precision angin,angphi,diftyp,difrad,difbet
      common /ascaray_angle/ angin,angphi,diftyp,difrad,difbet

C     For EXTERNAL
      double precision Raytraceran

C=========
C---------
      theeta   = DEG2RAD * angin
      phi      = DEG2RAD * angphi

      if(diftyp.eq.0.0.or.diftyp.eq.1.0) then
         if(diftyp.eq.0.0) then
            bb       = Raytraceran()
            bc       = Raytraceran() 
            tta1     = 0.0D0  * DEG2RAD
            tta2     = difrad * DEG2RAD / 60.0d0
            tta      = -dcos(tta1)+(-dcos(tta2)-(-dcos(tta1)))*bb
            theed   = dacos(-tta)
            phid      = 360.00D0*DEG2RAD*bc
         else
            call raytracebetamdl(theed,phid)
         endif
         drayn(1)  = dsin(theed)*dcos(phid)
         drayn(2)  = dsin(theed)*dsin(phid)
         drayn(3)  = -dcos(theed)
         call raytracerotate(drayn,theeta,phi,rayn)
C
         phth = dacos(-rayn(3))
         if(rayn(1).gt.0.d0) then
            phphi=datan(rayn(2)/rayn(1))+M2PI
            if(phphi.ge.M2PI) then
               phphi=phphi-M2PI
            endif
         else if(rayn(1).lt.0.d0) then
            phphi=datan(rayn(2)/rayn(1))+MPI
         else if(rayn(2).ge.0.d0) then
            phphi=M90DEG
         else
            phphi=M90DEG*3.d0
         endif
         phth  = phth/DEG2RAD
         phphi = phphi/DEG2RAD
      else
         phth  = angin
         phphi = angphi
      endif

      return
      end

C
C     subroutine raytracebetamdl(theeta,phi)
C
C     input     difrad : core radius of beta model
C               difbet : beta of beta model
C
C     output    theeta : off-set angle of incident photon
C               phi    : rooll angle of incident photon
C
C
C                          '94.1.25
C                          coded by Fumie Akimoto

      subroutine raytracebetamdl(theeta,phi)

      implicit double precision (a-h,o-z) 
      double precision Raytraceran
      common /ascaray_angle/  angin,angphi,diftyp,difrad,difbet
      data deg /1.745329252d-2/

 222  continue
      bb     = Raytraceran()
      if (bb.le.0.0d0) goto 222
      bc     =Raytraceran()
      theeta =difrad*dsqrt(bb**(1/(1.5D0-3.0D0*difbet))-1)
      theeta =theeta/60.0d0 * deg
      phi    =360.00D0*deg*bc

      if(theeta.gt.0.04) goto 222

      return
      end

C
C     subroutine raytracerotate(drayn,theeta,phi,rayn)
C
C     input     drayn  : unit vector of incident photon
C                        (source direction is on-axis)
C               theeta : off-set angle of incident photon
C               phi    : roll angle of incident photon
C
C     output    rayn   : unit vector of incident photon
C                        (source direction is off-axis)
C
C                          '94.2.8
C                          coded by A. Furuzawa

      subroutine raytracerotate(drayn,theeta,phi,rayn)
      implicit double precision (a-h,o-z)
      dimension drayn(3),rayn(3)

      phid=phi+3.141592654d0
C      phid=phi

      rayn(1)=dcos(theeta)*dcos(phid)*drayn(1)-dsin(phid)*drayn(2)
     & +dsin(theeta)*dcos(phid)*drayn(3)
      rayn(2)=dcos(theeta)*dsin(phid)*drayn(1)+dcos(phid)*drayn(2)
     & +dsin(theeta)*dsin(phid)*drayn(3)
      rayn(3)=-dsin(theeta)*drayn(1)+dcos(theeta)*drayn(3)

      return
      end

