
      SUBROUTINE dokdisk(ear, ne, param, nenrgy, nrad, nteta, efil, 
     $     ebin, rad, incl, trs, bin, start, end, fstart, 
     $     fend, photloc, photlocinner, inner, outer, photar)

      implicit none
      INTEGER ne, nenrgy, nrad, nteta

      REAL param(7), ear(0:ne), photar(ne)
      REAL efil(0:*), ebin(*), rad(*), incl(*), trs(*), bin(*)
      REAL start(*), end(*), fstart(*), fend(*)
      REAL photloc(*), photlocinner(*), inner(*), outer(*)
      
C Subroutine to do the actual calculation of the LAOR model
C Parameters :
C       ear      r           i: energies for output
C       ne       i           i: number of energies
C       param    r           i: model parameters
c     1. - distance (kpc)
c     2. - spectral hardening factor (Tcol/Teff)
c     3. - mass of the central object (solar mass unit)
c     4. - mass accretion rate in 1e18 erg/s
c     5. - disk inclinatino angle (degree, 0 for face-on)
c     6. - disk inner radius in unit of GM/c^2 (>1.235)
c     7. - disk outer radius 
C       nenrgy   i           i: number of tabulated energies
C       nrad     i           i: number of tabulated radii
C       nteta    i           i: number of tabulated inclination angles
C       efil     r           i: energies on which model is calculated
C       ebin     r           i: tabulated energies (relative to line center)
C       rad      r           i: tabulated radii
C       incl     r           i: tabulated inclination angles
C       trs      r           i: tabulated transfer function
C       bin      r           w: workspace for accumulating model spectrum
C       start    r           w: workspace array used for rebinning
C       end      r           w: workspace array used for rebinning
C       fstart   r           w: workspace array used for rebinning
C       fend     r           w: workspace array used for rebinning
C       photar   r           r: final model spectrum

      REAL inclin, r, wlow, whgh, ems, area

      INTEGER i, j, ioff, inclow, iro, iri

      real pi
      parameter(pi=3.141592654)
      integer ie,nene,nradd,i1
      real    fcol,cnorm,ene1,ene2,dene,rmax,ena,enb,enn,kT,blackbody,
     >     r1,r2,ra,rb,tfun,dr
      real d, m, mdot, Rin, Rout

      d = param(1)
      fcol = param(2)
      m = param(3)
      mdot = param(4)
      inclin = max(0.0,cos(param(5)*pi/180.))
      Rin = param(6)
      Rout = param(7)

      cnorm = fcol**4

c$$$      write(*,*) '! distance=',d
c$$$      write(*,*) '! m       =',m
c$$$      write(*,*) '! mdot    =',mdot
c$$$      write(*,*) '! inclin  =',inclin, 'theta=',param(5)
c$$$      write(*,*) '! fcol    =',fcol
c$$$      write(*,*) '! Rin     =',Rin
c$$$      write(*,*) '! Rout    =',Rout

c Initialize the flux array.
      do i=1,ne
         photar(i) = 0
      end do

c     find the tabulated inclinations above and below that requested
c     and the associated weights
      i = 1
      DO WHILE( inclin.GT.incl(i) .AND. i.LT.nteta )
         i = i + 1
      ENDDO
      inclow = i - 1
      wlow = (incl(inclow+1)-inclin)/(incl(inclow+1)-incl(inclow))
      whgh = (inclin-incl(inclow))/(incl(inclow+1)-incl(inclow))
      
C     internal energy range and bin
      ene1 = 0.01
      ene2 = 20.
      nene = 300
      dene = log(ene2/ene1)/nene

      i1 = 0
 10   i1 = i1 + 1
      if (ene1*exp(dene* i1    ) .lt. 0.1) go to 10

C      print *,'i 1keV: ',i1,ene1*exp(dene* i1)

      if(Rin.lt.rad(1)) then
c     max radius to take into account the Kerr transfer function
         rmax = min(Rout,rad(1))
c     selecting the tabulated values of the radii which are slightly smaller than the inner 
C     or slightly larger than outer disk boundary.
C     Tabulatd radii =  rad(1) = 400, rad(2)=177.79,,,, rad(33)=1.38, rad(34)=1.306, rad(35)=1.235
         iro = nrad
         iri = 1
         DO i=1,nrad-1
            IF(Rmax .LE. rad(i)) iro=i
            IF(Rin  .LT. rad(i)) iri=i+1
         ENDDO
c$$$         write(*,'(a,f7.3,a,i2)')
c$$$     $'! Tabulated outermost radius to consider the Kerr transfer=',
c$$$     $        rad(iro), ' at suffix ', iro
c$$$         write(*,'(a,f7.3,a,i2)')
c$$$     $'! Tabulated innermost radius to consider the Kerr transfer=',
c$$$     $        rad(iri), ' at suffix ', iri

c     outer loop over energy in the emitted spectrum
         do ie = 1,nene
            ena = ene1*exp(dene*(ie-1.))
            enb = ene1*exp(dene* ie    )
            enn = sqrt(ena*enb)

            efil(0) = 0.
            efil(1) = (3*ebin(1)-ebin(2)) * enn / 2.
            DO i = 2, nenrgy
               efil(i) = (ebin(i-1)+ebin(i)) * enn / 2.
            ENDDO
            efil(nenrgy+1) = (3*ebin(nenrgy)-ebin(nenrgy-1)) * enn / 2.
            efil(nenrgy+2) = 1.e6
            DO i = 1, nenrgy+2
               bin(i) = 0.
               inner(i)=0.
            ENDDO

c     Summing the profiles of individual rings (from outer to inner rings)
            DO j=iro,iri
               r=rad(j)
               IF(j.EQ.iro) THEN
                  r=(rad(iro)+rad(iro+1))/2.
                  area=rmax**2-r**2
               ELSE
                  IF(j.EQ.iri) THEN
                     r=(rad(iri-1)+rad(iri))/2.
                     area=r**2-Rin**2
                  ELSE
                     area=((rad(j-1)+rad(j))/2.)**2-
     &                    ((rad(j)+rad(j+1))/2.)**2
                  ENDIF
               ENDIF

               area = area * pi

               kT = fcol*tfun(r,m,mdot)
               ems = 0.5*(blackbody(kT,ena)+blackbody(kT,enb))*(enb-ena)
C     d is distance in kpc, 1.48*m is the gravitational radius in
C     km. Note, inclination factor is included in the Laor's transfer
C     function, so it should not appear here.
               ems = ems*area/cnorm/d**2*(1.48*m)**2

c     sum in the required transfer functions
c     note average of two tabulated inclinations surrounding the input one 
               ioff = ((j-1)*nteta + inclow - 1)*nenrgy
               DO i=1,nenrgy
                  bin(i+1) = bin(i+1) + wlow*trs(ioff+i)*ems
                  if(r.lt.7.) then
                     inner(i+1) = inner(i+1) + wlow*trs(ioff+i)*ems
                  endif
               ENDDO
               ioff = ioff + nenrgy
               DO i=1,nenrgy
                  bin(i+1) = bin(i+1) + whgh*trs(ioff+i)*ems
                  if(r.lt.7.) then
                     inner(i+1) = inner(i+1) + whgh*trs(ioff+i)*ems
                  endif
               ENDDO
            ENDDO

c     Rebin onto passed energies
            do i=1,ne
               photloc(i) = 0.
               photlocinner(i) = 0.
            end do

c     divide by d mu:
C     It is needed to divide by 2* d\mu, where \mu is cos(i), where
C     i is the inclination. d\mu is 0.333 in Laor's file.
c     This procedure is needed, since Laor's transfer function is so defined that
C     adding up all the component (instead of 'integrating' over energy and
C     solid angl) will be one.  See the note attached in the end.
            do i=1,nenrgy
               bin(i+1) = bin(i+1)/3.3300001E-02/2 
               inner(i+1)=inner(i+1)/3.3300001E-02/2 
            end do

            CALL inibin((nenrgy+2), efil, ne, ear, start, end,
     &           fstart, fend, 0)
            CALL erebin((nenrgy+2), bin, ne, start, end, fstart, fend, 
     >           photloc)
            CALL inibin((nenrgy+2), efil, ne, ear, start, end,
     &           fstart, fend, 0)
            CALL erebin((nenrgy+2), inner, ne, start, end, fstart, fend, 
     >           photlocinner)
            do i=1,ne
               photar(i) = photar(i) + photloc(i)
            end do
c     end of energy loop
         end do
      endif

c     emission from r>400 Rg, if needed
      if (Rout .gt. rad(1).and.inclin.gt.0.0) then
         nradd = 20
         r1 = max(rad(1),Rin)
         r2 = Rout
         dr = log(r2/r1)/nradd
         do ie=1,ne
            ena = ear(ie-1)
            enb = ear(ie  )
            outer(ie)=0.0
            do i=1, nradd
               r = r1*exp(dr*(i-0.5))
               ra = r1*exp(dr*(i-1. ))
               rb = r1*exp(dr*(i    ))
               area = pi*(rb**2-ra**2)
               kT = fcol*tfun(r,m,mdot) 
               ems = 0.5*(blackbody(kT,ena)+blackbody(kT,enb))*(enb-ena)
C     d is distance in kpc, 1.48*m is the gravitational radius in
C     km.
               ems = ems*area/cnorm/d**2*inclin*(1.48*m)**2
               outer(ie)=outer(ie)+ems
               photar(ie) = photar(ie) + ems
            end do
         end do
      end if

      RETURN
      END

      function blackbody(kt,en)
      implicit none
      real blackbody,en,kt
c     function for bbody PHOTON intensity (photons/keV)
c     en, kt: in keV
      real fac,f
      real norm
c     corresponding to photons / sec / km^2 / keV / sr / (1 kpc)^2
C     
C     Planck function is 3.14e31* E**2 / ( exp(E/kT)-1 ) [photons/cm^2/s/str],
C     where E and kT are in keV.
C
c     Note, 3.3e-2 = 3.141e31 * (1e5)^2 / (1e3*3.08e18)^2
C
C     So, observed flux will be
C     3.3e-2 * [E**2 / ( exp(E/kT)-1 )] dE dS/d**2 [photons/s/cm^2],
C     = blackody(kT,En) * dE dS/d**2 [photons/s/cm^2],
C     where E, kT and dE are in keV, S is the projected emission area in
C     km**2 and d is the distance in kpc.

      parameter (norm=3.30e-02)

      fac  = en/kt
      if (fac .lt. 1e-3) then
         f = kt*en*en
      else
         if (fac .lt. 70.) then
            f = en*en*en/(exp(fac)-1.)
         else
            f = 0
         end  if
      end if

      blackbody = norm*f/en
      return
      end


      function tfun(x,m,mdot)
      implicit none
      real tfun,x,m,mdot
c     x - radius  in Rg
c     m - mass in solar mass unit
c     mdot - mass accretion rate in 1e18 g/s  

      real a,y1,y2,y3,xms,yms,y,cfac,bfac

c     this is what Laor's model assumes
      a = 0.998
c     'special' numbers
      y1 = -1.99956
      y2 = 0.963259
      y3 = 1.0363
      
c     marginally stable orbit
      xms = 1.235
      yms = sqrt(xms)

c     Krolik p. 152
      y = sqrt(x)
      cfac = 1 - yms/y - 1.5*a/y*log(y/yms) -
     >     3*(y1-a)*(y1-a)/(y*y1*(y1-y2)*(y1-y3))*log((y-y1)/(yms-y1)) - 
     >     3*(y2-a)*(y2-a)/(y*y2*(y2-y1)*(y2-y3))*log((y-y2)/(yms-y2)) - 
     >     3*(y3-a)*(y3-a)/(y*y3*(y3-y1)*(y3-y2))*log((y-y3)/(yms-y3)) 
      bfac = 1 - 3/x + 2*a*x**(-1.5)

c     radiation temp at this radius in keV
      tfun  = 8.24*m**(-0.5)*mdot**(0.25)*(x**(-3.0)*cfac/bfac)**(0.25)

      return
      end

c$$$    \documentstyle[psfig]{article}
c$$$    \begin{document}
c$$$    \centerline{Informal note  by K.E. on 2000-4-3}
c$$$
c$$$    The transfer function in the ari.mod file is so normalized that
c$$$    for large enough radius the summation over $g$ and $\mu$ (= $\cos \theta$)
c$$$    will be
c$$$    unity (figure 1).  Namely,
c$$$    \begin{equation}
c$$$    \sum_\mu \sum_g T(g,\mu,r) = 1 \;\;{\rm (for ~ large~enough~ r)}. 
c$$$    \end{equation}
c$$$    This may be rewritten as 
c$$$    \begin{equation}
c$$$    2\pi\sum_\mu \sum_g  \frac{T(g,\mu,r)}{2\Delta \mu} \Delta \mu = \pi \;\;{\rm (for ~ large~enough~ r)}. \label{2}
c$$$    \end{equation}
c$$$
c$$$
c$$$    In figure 1, reduction of the value from 1 as the radius decreases
c$$$    is the effect that the photons are not escaped due to strong gravity.
c$$$    The figure reproduces the values shown in Laor ApJ 376, 90, 1991;
c$$$    0.82 at r=6, 0.5 at r=2.17 and 0.1 at r=1.33.
c$$$
c$$$    On the other hand, integral of a unit specific intensity 
c$$$    ($I  \equiv 1$) over  hemisphere
c$$$    will be 
c$$$    \begin{equation}
c$$$    \int I \cos \theta d\Omega = 2 \pi \int \cos \theta \sin \theta d \theta
c$$$    = 2 \pi \int_0^1 \mu d\mu = \pi
c$$$    \end{equation}
c$$$    \begin{equation}
c$$$    =2 \pi \sum_\mu \mu \Delta\mu.\label{4}
c$$$    \end{equation}
c$$$    This is the total flux from a disk-like emission in the Newtonian case.
c$$$
c$$$    Comparing equations \ref{2} and \ref{4}, we can see 
c$$$    $\sum_g T(g,\mu,r)/2\Delta \mu $ is what tells the polar-angular dependence of 
c$$$    the disk emission, which corresponds to $\mu$ in the Newtonian
c$$$    case.  Figure 2 plots this function for various radii
c$$$    ($r$=400 to $r$=1.235 from top to down at large $\mu$). $\Delta \mu$ is 1/30 in ari.mod
c$$$    file.  We can see that for large radius this function is close to $\mu$,
c$$$    which is expected.  As the  radius gets smaller, the emission will 
c$$$    eventually be isotropic, then reversed (more flux for edge-on disk), 
c$$$    as the Laor 1991 Fig 1 caption says.
c$$$
c$$$    Let's assume $I(r,E)$ is the local specific intensity from the disk.
c$$$    In the Newtonian case, the disk spectrum will be, 
c$$$    \begin{equation}
c$$$    f(E,\theta) = \frac{\mu}{d^2} \times \int_{r_{in}}^{r_{out}}  2 \pi r  I(r,E)  dr 
c$$$    \end{equation}
c$$$    Replacing $\mu$ by $\sum_g T(g,\mu,r)/2\Delta \mu $,
c$$$    we may calculate the observed disk flux as follows.
c$$$    \begin{equation}
c$$$    f(E,\theta) = \frac{1}{d^2} \times \int_{r_{in}}^{r_{out}}  2 \pi r dr \sum_g  I(r,E) T(g,\mu,r)/2\Delta \mu 
c$$$    \end{equation}
c$$$
c$$$    \begin{figure}[b]
c$$$    \centerline{
c$$$    \psfig{file=flux2.ps,angle=270,height=8cm}
c$$$    }
c$$$    \caption{}
c$$$    \end{figure}
c$$$
c$$$    \begin{figure}
c$$$    \centerline{
c$$$    \psfig{file=flux.ps,angle=270,height=8cm}
c$$$    }
c$$$    \caption{}
c$$$    \end{figure}
c$$$    \end{document}
c$$$
c$$$      begin 644 flux2.ps.gz
c$$$      M'XL("*S*Z#@``V9L=7@R+G!S`,5;27,<MQ6^]Z]`#JY*#FUA7W2S:#$7NLPJ
c$$$      MJ2KGL3BFF5`<UG!D)Z7*?\_;@`:&PV4L2BDM'+Q^`+[WO05+#[_[R_F[^8>+
c$$$      MS2_KV7VOU=OS=Z?X8?KNN]/-]K5:_W)UM_IC!<WW5[OK]6MU_O?SLY_?J_/-
c$$$      MW>[=A^W5[4[=7F]V\/QDNU[ML`MK5,G5YN;'U0XZ*C?_<+N=K=9:&?O:!]!X
c$$$      ML_ET<W%U<_EF\^_7ZJ^@=G/Q-Q#_N/GPZ>/Z9G>ZN=G=]0_.5C>7GU:7Z[/U
c$$$      M[^OKU\J`Z.?M%6C2/*\5/+^X^["Z7<.#<]`;.K^]N3C9?,1Q[W#J]>75S?EV
c$$$      M<[VYG%Z=J<\?-[^O=QNUO;ZZP9\?/FVWH'F[N;K9J;O==O.OM6*5_ZI?KFXN
c$$$      MU,7ZU^G5B?I\7(<?VT0:_AS7]]W9/]3GH#Y^NE9WZQUV_>/J8O?;H')RJE[=
c$$$      M;F[5]68EHC?GZC-HWJYVOQT8\^Q<O:HHECYOSSN[KC=W:^J]WOQZ=7W=]_[I
c$$$      MC?I\>;?Z?:UVV]7-W350K7XZ_>%$77RZ5>"'Z[4R`U9E:Q.<!`1(XY]@N!*0
c$$$      MP_AOU:O+[?H.PFK=P3L!VK5R4:O5]H/PU7<['9[O@:8X$+\K"1.()(HEC@D0
c$$$      MO%OO/MU.KR36R4+LJ[_7R2K^G\R;L@E:6?BW$%#`KQL(R/5D!OM,;[I1Z$WX
c$$$      M_^14$+59&=+!S("9-9%VN5W]1\;(KA1@TF@`D;)69]"(UA5UHF9Y=@+_9I%I
c$$$      MY8,RQ?FF#&TP(`WM8ES?=M9F;D_<]CD,SY,>]7.Q?=OKD%L;^/%VG"^8<;R0
c$$$      M.GUH1Q-]/W_TJ0S/DXM#NX3!OF3\@"<Y77H\R8<!3_;C>$7KP?[BS3`^\1E+
c$$$      M\@.A@P`9[05$Z2!`3@<!DLJ":6&UUR!:FZ#R&HLM`[%]%V*V%Q"U@P"Y[:<E
c$$$      M<@<-9+<7$+V#`/GM@1'!/3!BN.]"%/?3$L>B86!XY8T%/GS&=L:_%-;&1HQT
c$$$      M:,Z@8J,!O!94X`%)3U`^.TD"$M1VT^#.)`#Y!$UIB;KTKJ-1<ZZ/9U&?N3=C
c$$$      M3<X\A14H>1CK]$RLZLNP5B@AW8,R'4O;%T(!CTL1,]8F"(*BFR`'C)M%P_IB
c$$$      M0+!H.&/]H.%2B(.&=SD/&D';<8P0@X-I%XUH(7=ZC5A,&Z-X[04I:Q0'('ND
c$$$      MI-$C)8T>*6DPTFG1Z)&21H^4-!AII]$C)0U!&HGEE),R*3J.1>3[!'W!KLM$
c$$$      M/C0C!SX]C35BJ37'Q6,L8'42:&G/+;)KGWF)>@Z>"`!-I2CR".T']L>0TRH;
c$$$      M@(O5DH-Q5*.Q,``8)8\<:]8MH6B;>3(](N1PS-RW,N.TS1TSTT%FU+=@AD-C
c$$$      M.L0,@D;\!-@Z6\LJ6HJ<=?+%,F^+?=+GARV;7M8R">F#ELWLJZQA.0L&<Z..
c$$$      M,W7CM'HS6.(T80'::N$9+*DQDVO=L7W=B7T%%!R#'9/,TJ:O"K7+0G4((?U)
c$$$      MJA\,HNE/42VUX2#5'5Y8GWA9G<>LB2VO(`YKEN6.W)IG><BS.#(%J*AO@[GG
c$$$      M["Z,:H8W]_!\&(!U"'';HJB'D0Y,"L^R@14M%XJGN@)SKIRH<1<]&^,AN[QL
c$$$      M.#(LA6?3G*$`S8&&:_0'$T3F'7PR)'0IP<3L-TA`^`\_V@*N\@3'Q@*]6.J1
c$$$      M!]J96\`R9U:`O=)L-%M5<&"H@.1^##A'.@90S<9SG$3+_^'G@)\3RSWVS32\
c$$$      M<1D'9AWP]6PKBP'GEHQ`N>//:!H>8^@SRF&]0`P:8-I$1AO8CLXVD]5PK("P
c$$$      M)''!?0%S42R3@!^!-.=)"O$VNT!F8!ESL.W!CZ1+'V&_.>,*MN>9B?V%1QA@
c$$$      MP5#-`TN'-C#B0VMW';RG508+A(=GQA=)T"7"NZU)JPP4>X!2>@5:-:$=(PNB
c$$$      M#BR8FR353()_L)CC`F?Z6@!!.18%Z_NB8+VJ1:4F&*6?T;;:DDLYA)U_P,Z;
c$$$      M<.1L&)E`+S%([`/0E!9!EV@=":VC-1".D`!B$;++(LXTJX8-884\%L;[&T+[
c$$$      M".ORN$.`Q702=WB`S9)]R+$11[SHOBX0;$=%;JB5%7:MY]W6H6TG:)\J1;`]
c$$$      MVZMVPX!+F:L:7);1>=;K(JORP)"V<BX7_UD7)<(MN<MZ5]UPK]RULO?5G+&D
c$$$      M@`W!#7'4!(?CJ+.<MYQ/6YY,Z%/5)@SWZ5FIV@5-CJ&/?UMWD$L0B>3I(!J"
c$$$      M'TZR[NL'_Y+MSF`INV_^D.T.C\*\RE,?JR72]Y;[V%/D\'*@.H0%J2RUZ[%C
c$$$      M\#,L>.!H>6]CU7.X0'.QC-YBR71DRKOHRA>F_/),<_X_,^6;+V(*S_!%',NU
c$$$      M2W:?`)(\DX"N5PG/K)2\6-+D1><EI8E'D4@5W!O&#L,M\5ZI77;M=I^R&C(U
c$$$      M=^IZ[;6US(?C=96NMLXFVC"(Q#B\;VO+Z:&)NCVFIZ,TC?K_6`%:.?,N^N?N
c$$$      M/)9X\!Z-G;IVRF-\5,EQ\>&#]\\L@LO<,>S/+9(CYT[VN7.W9=!G5V^EOMTR
c$$$      M."UEN05HT&:Y'N,DJ:*O"V$IU\'0A?/++#A/E^OI4+F67`R6`J#+SN"<[O<O
c$$$      M`=^^?(GCII?93`:?ZAF0]R]-<'#_,I5:%$,RDK?[)]Q'BQH%1EKN,8X/C.EH
c$$$      M$XOU^Z%9[+=%('?'+=>KY-GK$.98Q-/L>/1@T5???>%A7`(Y.G2>7`.B*7#^
c$$$      MEPM"W8VHQ^A9PJF>Y[QMB1L=A1RN7!U$WB#H_?Z\WH@+\#P9O8GR&@$5ZOC+
c$$$      M\5$.D_5LN?`Q,B3O'P*>I;Q!DP+V[=ICK$\]Y\/V;;G-J34C,`JT.>(4N`^B
c$$$      M"H%O2Z%:)%A3/=>'D;;8BM#^K=`<NXWXX1NOMH8W0\>KJ3X"N!`98[!"^7J"
c$$$      MN7>#A/OPD\F3KHX6=#4?RX@[8WRLW?E"V9CBJ]*]7<4(9'D-E-&%>)LK_?@F
c$$$      M"+8W=;YI;K*8%PP`P!-^PYM_X:';P4@T&*L>*EJC(\WRWJE>D%-2<_&'&0*^
c$$$      M!S56CK"QS3?7Z&5>8H@5ID0RBTP>KO'WW*L>=N]TI'LKD)0&_^)?$AT-Y/[E
c$$$      M\W%`LFF.$QPD81SFX"5JFV$?PKB[7#SWZ$6J`'$%')I\P1*2,A7:1>`[@8M+
c$$$      MG'N-K^])9^*X\,;32UQ_9)Q[JV/K%T4$TXJHQKG'HEMEE*G>YUPA/)RIRK/N
c$$$      M`DZF#=8O<'%-8@&7N8-7V?78$&WLD71;H2^H/@_=^(]%OSE3C$BF0IED966)
c$$$      MTP_&4(<SZN8-."K8X_I1W?8E1R%M.JYP/Y+91U(W,1?!Q,$M\%=$%+=?#\?4
c$$$      M^PAGQ3N3>[.F%Y@U/6`]+=K.FIJT[%*1'.'2.A1LS>I0U286'6)R>GF/\D(2
c$$$      M\*9&8!Q\%RQ^C_AMDEHL"&P*;@&;]]R>NT*QG)X61.W%7$>25,N^W]0=AY:A
c$$$      M8Y<`E4V\;ZELRAH:<FB8<4<$Y2YD4VKY$<-*<4.%$@$EV^$*Q1VC<6DO$T3T
c$$$      M8IDP/>X_,3W:8/8"241?"F0Z)I"B)=X>#*1*FRVE6R/`3='YI`XO:=-C2QKL
c$$$      MQ]T8DC%8\^(A.>V%I'HR)&'7HU7.^-4I>96`#SW_<(I>P"5%+R*-X1'PS1S^
c$$$      MS/B1M/`168JO(&M3'B<<#8!RYT0C)AH>]:/*>,..;Q[/2,Q-3?=Z-*WA`U_F
c$$$      M21,/.DZY0)CY^838^!6H7"CE:@FJTXO;7&!GP_.*F8[-]#P3#!V`F0*+H#%T
c$$$      MG`0'JJ)A,\_P<,:!%WP#Z@2-$)$7Y$8@"7OR4`]=>`R_-ZB`2PU4P9>M`@HM
c$$$      MXK:6=H=I:KX2WM"XGKC:3$_YBEYBX\PNUIEA\\4B..ZRJ"E5R;0HP8F'E<0$
c$$$      MBR]@.Q.HS29D,:&Y;C1AS_=N-$%8-G(=V9G@Z8M.'K:6['5\_S[;TMM+7<7_
c$$$      M>TPUYAJ.1USE6@Y-D;]OE?"KB31IIJ@O(5<<`&%/XO!NHP0XQ>7$C&'WZ)PP
c$$$      M1H11\XD\F9[($_5XGHBKDJXI@`"X3<6"M8;IV=P$JU?A[V0Z!2<%++V:^G/0
c$$$      M^19=)O34N^:!-#*?!#.#=WSQ<@*[N27SO(*UT%;?BNY$;JNKA1&W9I[1:/:]
c$$$      MI*6TN$'/?-N`&\;!&_`D[>KOU(,P6N-W_!#%M"#^5BBF!46*>UQ\`Q23<(&^
c$$$      M-1J_EL*O'*E<J5KQ\2?%+J8\Q86_'Q=^+R6?CHNIBPN)9DIEHYVK%:H-Y/FQ
c$$$      MV].B%XH'E7#L@%76XR^BN'831DLN<MT]F"F!09)*E7CYC@\/*BN'%`XCBT!^
c$$$      M8FVKZRWJ=X5G:B%0H5+AD=]"J+\'<??;YH_;U67]%9?WV]75]7I[Z-<&/%YI
c$$$      DXGY4I>3IEQT6Y4<4[_W^S?++-/A+$F]_/IW^!V>$L0HU-```
c$$$      `
c$$$      end
c$$$
c$$$      begin 644 flux.ps.gz
c$$$      M'XL("%?9Z#@``V9L=7@N<',`Q5U);QS'DK[7KZ@Y&)@YE)7[HIM-6W/A@P78
c$$$      MP)QIJ9_,&9I-D)0]@#'_?7*++S*KJ[M)D]*#+;(JUX@OEHR,JBQ^\V_O?UZ^
c$$$      M^[C_=;?H;\7\X_N?W^6+Z9MOWNWOW\Z[7Z\?KOZ\2K>_7#_>[-[.[__S_>5/
c$$$      MO\SO]P^//W^XO[Y[G.]N]H^I_N)^=_68N]065'*]O_WAZC%UG/7RW=W]HH00
c$$$      ML]1O34@MOM]_OOUX??OI^_W_OIW_/36[_?@?J?B'_8?/O^]N']_M;Q\?^HK+
c$$$      MJ]M/GZ\^[2YW?^QNWLXR%?UT?YU:EGG>SJG^X\.'J[M=JGB?V@V=?[S]>+'_
c$$$      M/8_[D*?>?;J^?7^_O]E_FMY<SG_]OO]C][B?[V^N;_/O#Y_O[U/+N_WU[>/\
c$$$      M\'B__Y_=7)O\W_SK]>W'^>/NG].;B_FOYW7X`1.)]-_S^OY\^5_S7W;^_?/-
c$$$      M_+![S%W_O/[X^-O0Y.+=_.9N?S??[*]:T??OY[]2R[NKQ]\VQKQ\/[\A*KC/
c$$$      MC^\[OF[V#[O2>[?_Y_7-3=_[']_/?WUZN/IC-S_>7]T^W"2HYW^\^^YB_OCY
c$$$      M;DYRN-G-<J!U5G2;A)0`:#?_G1B?&Y'#^#_.;S[=[QZ26NTZ\BX2[&+63LQ7
c$$$      M]Q\:7GVW=T/]BNBB!TWN<U.3I$E%EZI.I(*?=X^?[Z8W3=<+A[FO^%9X-=>?
c$$$      MA;TI2"MFE?XQ`#')=9\4<C?)@3_9LR[G+,WT\^)=HPBS5I(V+2/-G(RG_YF&
c$$$      MO/_TZX?$S7T;,^@8$[)2)*)\$/-ENG%*Q_EB7EK=1?JWM#*1Z96J:VSLK/K.
c$$$      MJ5X%NI]*O79CO3%C?ZM,[.N=Z.Y3O0MCO7=C?3!C?<1X$]/KHHH#P2YZ,U#<
c$$$      MMR@D]RT*S7V+0G3?HE!=6TQ,=M^BT-V/40CO6T3%+189$NR)>&-FES0HEZA2
c$$$      M>I'+%]TD4PKHGEI,M7,I*.5A;G>M>>M-HY7;A:J7UGQIO1LIRMNGDS*AQ>N2
c$$$      MX@HE65[6AD2)JX.[VJK\2O"G)E$D8HTJ35)G:I(N\R]E9L*K=J=?I:EHOQ<J
c$$$      MZ/D5Y=]%DEX/CM$$SO0,.;TJ.%,%IZCJ47`J`XEVDYM4:49!9BZ5ZRL:;S9:
c$$$      M?5;PTQ?EK0F^&-EQP1=[F9WTR?JL:0QL:/+%$3[*[W2?M*/QH3H^W)J-JE5E
c$$$      M%('Y`W&A>B[<UO10L<!UWCX!ZZ-Z-+TBUL5=G="C1J]7GNAMUC*1^9#UD)4E
c$$$      M6*M"L&*,#47'+]!Q`W"*K6Y0ZKD-T%21YB4!MND[#\`]5#<`QF4'X9);5DFA
c$$$      M3$8B53=Y%UNYJ+93EL^H4H,IN?5V+[7VJ4=7X(T;"I0L2MH56*?)'FM!+,AR
c$$$      M"ZW+.M<5^!"':8V,86AAG'##H%9(.[2P1HUTV*#':9TR<BS("^3E5`:-VL;*
c$$$      M?6T037*]/?>UH..^%/3<UX*.^S)FSWUI4;F?NH+"?=>BY[X6=-R707ON2XN>
c$$$      M^UK0<5\**O?=M)7[N9JN#SYK2%6.JK9:0-%%O6UWH\N`YI$1UH+6?&F]ES;:
c$$$      MM&!\.(W>TBHE*GCWI4B9B)3Y-"G)8H0DZ:T6G]J_KJ&97BUB;F6;:^CL.;#'
c$$$      MJG?3RI[9UQUZ$Z(H]"2R-U&8+5T&*6<3HVSK1?6GJGCL2;E$D*\AI`PJMU'I
c$$$      M?@G)ZUV42U]<2VH65"L)H5W$V*JD$*U(2FHEE:#:Y%U:F=9T972;75KTL)IZ
c$$$      MV$AEWM!5I/%4XH?*J%8)B5JB12F%*V)"@0(E7:$@UQK4.M32R!I4+<E-YFMY
c$$$      M<M^16T95$4WS"D)4IJ".(-4=I(6$0/-&!4@!E8@;D*)6`PQ#3$H#2`VQ)AW:
c$$$      M>?2-$$)D2-$W^D-(`8N2@6I!2P^X15^B7FD%2"5#:DI$>Q+26B)/`!X(<%^<
c$$$      M7P7<@S``[DEU`ZEWUJ`"003*@NJD!'H*:JC75ZXO,S`)!Y-@%0[H$5`;2`U)
c$$$      MF1-F@I44R(L(1"UJ_0;>Z&&@PL8RWFG9>AK>/>IKO'W#6V=E:G@G@Z()'=$:
c$$$      MY`'>ZA!ONX$W=)215W`/!CV,!][08`?Y>5B3YS(/O$D+9<1XH$H)("H8;XDK
c$$$      MEA5D`*>EX6X2*'GL9^CW-M[6-;Q=6<LKWA9&"[S!9B0MR,YZ6OEH`4@E()4.
c$$$      M,*.=AKO1\1!P]M$.+L.AAPL`/!#@'L:!)42"4!*"&X0`AR(VC,,3_UIT@&<^
c$$$      MS^6&SBIXYK,@;J+#HF@$7)@C8D%#,-U%U7`BE-5+\(H95V6\.F:\><4$W@82
c$$$      M@F%W>%N4>5I4I&.\V=U`5@$C=ZNH/9!&I^!!`V\#O(523\3[E`-/,!:XK2W)
c$$$      MH0JWY2"$.#%Q#7>QK^)K6+]YZ00^')?`:4HXS6X1U7`9&NTL1C8<H7!MA'ZS
c$$$      M]K.[8;FP?K,,]($TND74K9'/@5ZTYQT*(6V/XITYKH!''PAP*PQ63#(HAQ*$
c$$$      M?Q0W,I(NKL`=`!?L6J"074@(+ZP99O;O@!22[P"WB'AXM>T<#T8&[20$AGX$
c$$$      MG.,<.)2T:#]EQ;1G%'S1S:&D?3:63*OA"+%40>BP5&_@PAE3>!MHE>0PD;0Y
c$$$      MLKI"!H(5'$@9J"N[(`.3R-':U!O"X&YX9`O9L])SN\`.!>X&X;9$W)O"MN)`
c$$$      MGQ,1VBV\,QX5\.#AP:W'Y!P3(BP7':8-<"!IQHL!;US%#7S$5EGLL5U+0R,B
c$$$      MU+"7;I/$KBJ@EF,?(.HW8LUN816,=Y;Y$R-"UO(#AY)3(,6A*(T0W/`"CK4(
c$$$      MJR*V0Y'AAAX">"KKW4V_9*I#<6`[)[%<]_LE]%"#UD_K)0&H<3N.0)_JJ]@+
c$$$      MA@[QS-V3/,JI$$4908B7%$-%7",HA`)$^)AABU-C%'>(.'/9[80X'H=.2H[,
c$$$      MQ2&F:AU0NLXQ28F@D&L[^8:^W<'(HF>CR8-F@U91APQXSI$])R;<=BDYKJB`
c$$$      M&\$QH<;&`QXNPH7+7@\JX'+%&D,_+(L,AF3X6"710ZZA&J-ZCCSA5!"#QG`X
c$$$      M&5:'R$7BH-*$`UF8`+1-/!^!BV^U%L//`[0#H:T%AX0:D5H@8@)3"H^B#T(4
c$$$      MWFXJ=AE0ZBU5/J@=R^P*']<%.LVC,4F]3N!"KQG`L@G/#O-4[/^!LRY+V$F<
c$$$      MDQ2<S^7\\]"-4"28\V]PW'"G`:H&7\[[&<F5:^WS=4^Y=NSL.`5'+FZC#++L
c$$$      M,P=M+JKCM!G1X:D$892G<-52KRXOTGIA>[M@A[]T0*L2'YU6Z!KW-)75GD..
c$$$      M%%2:`VW8]JRLF.PD3`_FO-XZ\D8'SI[Q);T,-#_VJMBH8!<#B]8T#:="+2"A
c$$$      M;AXEI!(+`L<%@<R"-.1B2!/2&(7U\SE415M";366-V745F#`ULO`L28AX=/Y
c$$$      M5)CE%I;$RN%2BHT$G#[\GN%=!`U$CE:3`!9D2A3<);1QP>9K0=9V022Q6-1:
c$$$      M]`TA/L$#G-M[JT!(:\W9)<X*=(JIUD'"N$3S4P"^PN8%99P7(7W"_I$5DR8'
c$$$      M*(@[V!42T`N6PX4SL4`KDNHA@;)@0[]@S[+`%!9L>>DJU?IB.R](FT[5UUK*
c$$$      MFUI^,!`[5#F@X="5`RF.$SB;P;X3BP;PA;:2$.`5D<:#M3(1'#>I%4QYXT"5
c$$$      MCLV<<87/A/R*RC?EAY0<7(ICU!W7VB=B?2IE"O^A8M*ARTJ"=!R2\>.2;B/`
c$$$      M:0E>\WFU.XR$D'#%!32*-]`4^[&*L5GS)@Z5>#ZS8/^WN+7&LC/)M68#3]9=
c$$$      M`0FL<2^2.AM%/"%?JBF?I/(.FS3;,'::G_?Q,\"-Q:V+J^`4"`4X7:"!+1>,
c$$$      M9-'000C30`5YT5AXS6>_Z]4AWAVVK*'0_8,KM]&C2:-YH;)BO#1?FM_&JGB;
c$$$      MR$^\NJ2-9Z\\[!#J2FAX=<1#"PZCB4]61#""O/>"87O$P2]R*8MA/:2HN/._
c$$$      M'O+HL+(;\EC["9;,@+B-JZM"U?GM]OF,J1&!'NI&:'C@19(3<:`!X:E$$`H^
c$$$      M$+,@A:\Y6CKDP?!Z!BTT[#'8KN&?$:H4S9QZSS)X8&\V\.3Q]"$M%I)'2+T@
c$$$      M,YCT(KQ"NM3HME*J['4I>]?EOC9"$HZ:!<$=(`HBE%<W=AF&&0($S!HR5YUC
c$$$      M9E@"+VJ^AW3JS6!T&1L.A9VZW:"%5PNS=EKU$?HKY$LM/4*7000.`CFM3KAQ
c$$$      M6IH0TH0VK)JW#)P6@.QZ[4(M^PO/<*M56;X"W-[U,$XK474>(:RN7!]+=X#"
c$$$      MBW%$SFN$YJ#(/R6[<2Y=:IT@N&WD]9*3%'#?R,OAS95%<DC+;*H>CB.@#OK:
c$$$      M@0KF`D.^):2-H(Y!=>R_PHHJ-^P%]2&H6*H7]H((^)>"QHOSI5ZW)3-)D"`G
c$$$      M0J$X3`<C2\%S9\-XCV1!&FI!YG$)7&L/D>5$,./9.6M>$#GD@P/G%V?8G#ID
c$$$      M,3(6\X6U1K.+1BU>KEB0]%WB^<<!YW.E^=6W!C;>R&&'R\$G:RBN`D(E[+\7
c$$$      M;&D6O)34E076;OA,EI#?"MMX66,?S$Z+,@8=HHPR!SF,J&;7Q_K+"21.+VVL
c$$$      M0^XI[OM<MC1(1WCSXQ>%>=DQ(C^T<#YOH>RP:Y=4"D0BK#RR'G,(P2Y[[>3'
c$$$      M<('=+@<QZ*'IL>Z"Q7U!MK=;K1E7+/T]KOS@%KK-^UCD*XOG/(OZN=RI]X%0
c$$$      MMX@)D1U=9$<,VUQ)\K?MB&0?UV4>63#\6#2PKV`OS`O:.DWDL*#Y`6,.U>'!
c$$$      M.9#O7EL2&\ARAM^M\&2,'1EW)AYT8MCS?KSNDVEQ=)93?X+GXBA`\FHBU0&"
c$$$      M(VZ\9G$HN]ZFC!B)#61X$K9J#@ZV=`Y>`F[\$!F.14$&Y#B6Y!$Q77D_^"R>
c$$$      M^7%5Q=/FV*Z)G:U;\C+-3Y$Z2^=M`^L9;SXX2;%IK=V#?9X/WD:O4`(XO2)Q
c$$$      M<`PZ#N#B%Y(.X%('51(&V`_XTI=0<R!>8>Z>6$EV?YS^[W8(W:JC3^+(CX_9
c$$$      M^C;\6AA!ZU7+=<A,1U2+/>^H;#U6ZJ"*>HF3G<X%%>65]'PF4SKCRY'!9+SU
c$$$      M7N0UK1Q(,_EQE9=T@C`IM_=<0,>2QN,RW#&4$QAT.LDD=:8B2G=V)Y!PMF<X
c$$$      M)75X`JD=-.`)N],VM;JC(,KN/7R35DDJ&4=PPTD`)V8ZT)0/P.6C(R)8(CNL
c$$$      MR`XS'TFJAX'HP%`[&$6#TT$AT758#]B=WU(XVY`NLUR4+-BY-4)"M?-V*6C*
c$$$      M3*N<U<NG1I,+SN)2^<7S*@8QC\<5\/O5A3%!.R`,I84?]`@%FWK4<VZE?Q+G
c$$$      MQN732T5^Y=Z69ZPDUB-*T\YH,)VNJ'=W[_RH1%1R0HFF7HF:\JM@_)=7_HI*
c$$$      M@2V49.4!^VK`+'C?SM64/OF%Q\OU`9OVBR&IK^E6PG.!%KH@W3@X<6#X/`?S
c$$$      MF6.6:DMG09K.S\HNITY:M>1))D\H:*W\>12T+DQW]T&NIBXE3YJ:Q:9M>=_E
c$$$      MM-AT?:\11JZMDVSDT]<Q<A99=(2!+X?;^)#KP4GH\41F9QZUZRM1.AVCE*55
c$$$      M#_NQ.T+!MCOBCE&KU;)&13WIOAYB_GL@^RW2.Q6)P9U7D>A"OPX848Y,_JO6
c$$$      M`0XX3#X@-@)(15^6!/971I>G"J^C_.?]U;3EK[2KI)ARL#$5+%1BC>EMVUAO
c$$$      M7B2XE[EC*+YQ(0P6@X+M0!!.P=3CO'_'*;2N?U,QIB>RF%^(7O(C?>WM;/-+
c$$$      M[="-4J+-<"`WGSPO%W2`M1WO;N=;R\9DQK'8J8,G&V=,XP42:).?&]`K>4($
c$$$      MD1Q8@CD*-XD@?$5"U),:^4,QY:&.M>48N<[?.DAXZMF:<J)9UXQG*<&A_#1<
c$$$      M-D_KM'CM,[_3^OCQ?.[,KTV;5>N+5]@\\]O1&^1Y>J?GT3L_A]Y\?KU2$LOI
c$$$      M<!-:VG)8[R='N(UJ^G=/&V-6)]3Q6>=^UNGELU9=SD;A9'E]B8`CI6<;$*.!
c$$$      ML"<:#3<37;Y^8)7*00"M`SJ_:ITB-UV24D09VWMC10ZN+/THQE/H349K\_-1
c$$$      M'K,^N^O5K+>IWE-49ROSXU"O#+:Q;E[)OZ2^3&DKG$IM2^S6ON<@\ULOM?M4
c$$$      M+53F][Q:HX/-8R!"R+[:)"%#FP^XMWXUSR:UP'P+RI+K0EG1;B/S<:_R'2%8
c$$$      M3F=.1*G5#ATK%+5(!MU[*K(*8"&'B'QEQ8/$IB-\*O)ZE9#R=2=&//]?BEY,
c$$$      MR-2KSE,(B8"2Z,@EE0X6TN"/M^RIIVGJR&3*F17BI+8PH9*2-E<)`!/)RM=E
c$$$      M90>A<X(G1=#-6')\D1<T;W*J?*K:U$JJ>]_DH".\K2=+S.]*!!KIA&M8,4)A
c$$$      M5^NLW2OIT6GQ34U\:6OF")^R&NH4WF=\0L$'SE]U*\M74W*=/R?4B".4:M%+
c$$$      M47JJDE=(7'2O#LGT/'D!DKR(-Q5KB-22@LCSS&UB'7V&N55(HGRQEDPO5.%F
c$$$      MON7X,R%2,PJUY+SY3IWYEF]P*:E6^M:*OI153EOZ9E31\2]L@D3)=!+<LE`.
c$$$      MX-:29_A&0M*:8_YM>@4D-RW7V/`T)*<OYVEI2!,D+PX'@T]_<_!C87HW]UI1
c$$$      M\M<#S.!7BQH<"==,*,[/FA:NI1HK`LQD([,OJ*L5SO1=<^PJBQ)$!WUJ)5F?
c$$$      M8`RG]:E(UBIK_^4^N:)I<QYRA>:QX-?FG>0*$JM]Z][\4=IH6FKTG.#7.BO1
c$$$      MS[6BJ#DB:67>QEX1\VE[K8F$LQP$)JY-&UUD<K-(:X$,6*-Z@CM9..'E@4F\
c$$$      M>%EXIDE4)ES^4LL0!+:29SBZUD\I_[Q^1:&=T;*!-GUYIW\\*,Q8.#F()3>J
c$$$      M15\X.)UZ&>4BK^S7#XF==W[T4:WD>?N!#&3T?K",5G#<,IH$?'YK9)1`*_I:
c$$$      M$FA8^'QL?HR(6M'7BHC*[LDK[WG%Z;;EM+EJL.4ODK!ORJD`K>6S7"D)0$?/
c$$$      M?K.P;47'=EBQ38ELUS'9\X9<6Z<O#>:^W]2EQ'EHU_F"M.$7<PC&SK$F4,H3
c$$$      M$%-_Z;GD=/RL2T]91V@O4BRAOGV1;NJ+&:+>TVVMGMI+*ZVS+R/Z,KPH+RV&
c$$$      M*'7YR,-E*:ZWHF3FR[2R/J8+=5(_SB%6)"RM/M-6TU&BQH.!.,FU)E>&_$68
c$$$      M.F]C4U<V39U)E->F0LPOC,ORD"FIPASSLYQ*7IYQP"7]KL!H$!E`>4G`5=Y+
c$$$      MHU8IQBYE#+,:M!'G053,;]$UHC)']5ZT^TU955RFS%P/'-TVV([+2MHVLW8T
c$$$      MLS2M*+_=48K0""7<*)I:-#46\F?$>A;*?64A-!;:@_PPL'!4W5IU0QE:`Q8F
c$$$      M4[X=:H1K4L_O;-5W1\%OZ=KDOT(*R(&.$Z+2;$.N?FW4V_Q)E10-J5"T/MI`
c$$$      M="025B4Z/Z^(-NV+0GU!*+^F'EV*J2IB!;!R^SP[F<[9R3S:21.5%V0"F8!Z
c$$$      M7YQ%;35,7]GU*02/U6GJ.0:376^.QW4S/P/MDK:'7D,"?D3>-YHK\3EQ73+N
c$$$      MMK,\,\?\,+C)%FU-MUKD]^'HGM[%(RGZN=W5FU)G$/BUH6K@Y]L]R=OW1$@A
c$$$      MRF=S-ZF8&A7S5Z#"NQ45TQJ++T=%EJT4^>-J]160XJXJ^^4U=5,_8JR<?IHM
c$$$      M'G7WM`0E$K=MT<P:WG.JKET*K<D_&9JF;]RU<O%8HZR#-GM=DU^7+$Q80_%_
c$$$      MQIXK)@@EK_^-X5''35T-FK?.?K0YL7(?:JU&EV8&;!_D\QL(I@=A&D`@">IF
c$$$      MNZ'.TZ:M`J_NKXJ[#P<(U#K^U/Y0!OVICH??]G_>77VBO\+RR_W5]<WN?NLO
c$$$      I6YC\N'TNFU)ORM_CX,8G&A[\B1C^>R_Y[WC\^-.[Z?\!N@!R(MAF``#N
c$$$      `
c$$$      end

