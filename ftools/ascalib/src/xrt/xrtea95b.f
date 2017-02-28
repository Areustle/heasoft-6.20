      function XRTEA95b(E,theta,phi)
      real*8   xrtea95b,e,theta,phi

c********************************************************************
c*                                                                  *
c*    function : reading the XRT_Response file, named fname,        *
c*               and then calculate the XRT effective are in given  *
c*               energy;E, off-axis angle;theta, and                *
c*               azimuthal angle;phi                                *
c*                                                                  *
c*    input    : E       -- energy (keV)                            *
c*               theta   -- off-axis angle (mm)                     *
c*               phi     -- azimuthal angle (degree)                *
c*                                                                  *
c*    output   : EA      -- XRT effective area within the circle    *
c*                          of 12mm diameter                        *
c*                                                                  *
c*                              coded by H. Awaki                   *
c*                            awaki@cr.scphys.kyoto-u.ac.jp         *
c*                                                                  *
c*                                                                  *
c*    called libraries                                              *
c*      libfitsio.a, libftools.a, libhost.a                         *
c*                                                                  *
c*                                                                  *
c*    ver 1.0   first release                                       *
c*        1.1   fitsio routine is modified                          *
c*        1.2   a bug about interporation was fixed  (Nov.16,'93)   *
c*        1.2a  fitsio bug (ftclos)                                 *
c*        1.3   can read new format in xrt_ea.fits                  *
c*        95a   a small bug fixed and real*8                        *
c*        95b   modify a method of extrapolation theta > 25.0min    *
c*                                                                  *
c********************************************************************

      common /xrt_e_order/ eorder
      integer eorder (1200)

      integer iw,it,ip,ie,i,j

      real*8 rspt(2),rspp(2),wghtt(2),wghtp(2),t(2)
      character cmess*80

c . . > 
      it = theta+1
      ip = 4-abs(mod(phi+360.0d0,90.0d0)/15.0d0-3)
      if (ip.ge.4) ip = ip-1
      iw = max(1,idint(E*100.0d0+0.001d0))
      iw = min(1199,iw)
      ie = eorder(iw)

      call xrt_gettheta(it,t)

c      write (6,'(3(f8.2,i4),2f10.3)') e,ie,theta,it,phi,ip,t

      do 10 j=1,2
         do 20 i=1,2
            call xrt_getsmrsp(ie,e,it-1+i,ip-1+j,rspt(i))
c            call xrt_getwghtt(theta,it-1+i,wghtt(i))
 20      continue
         if (theta.gt.20.0d0) then
            rspt(1)=log(rspt(1))
            rspt(2)=log(rspt(2))
         endif
         call xrt_interp(t(1),t(2),rspt(1),rspt(2),theta,rspp(j),ICN)
c         call xrt_wghave(rspt,wghtt,2,rspp(j))
         call xrt_getwghtp(phi,ip-1+j,wghtp(j))
 10   continue

      call xrt_wghave(rspp,wghtp,2,xrtea95b)
      if (theta.gt.20.0) xrtea95b=exp(xrtea95b)

      return
      end


      subroutine xrtea95b_init(fname,icon)
      character fname*(*)
      integer   icon

      common /xrt_e_order/ eorder
      integer eorder (1200)

      character cmess*80

      icon = 0
      write (cmess,'(a)') 'read error occured in xrtea95b'
      call xrt_rdresp(20,fname,icon)
      if (icon.ne.0) then
         call fcecho(cmess)
         return
      endif
      call xrt_order(eorder)

      return
      end

            
      subroutine xrt_getsmrsp(ie,e,it,ip,smrsp)
      integer ie,it,ip
      real*8    e,smrsp

      common /xrt_xrtrsp/ rsp, engy, theta,phi,nengy,ntheta,nphi
     &                rspver
      real*8 rsp(500,30,5),engy(500),theta(30),phi(5)
      integer nengy,ntheta,nphi
      character(10) rspver

      real*8 xwrk(20),ywrk(20)
      integer nm, imt
      data nm/3/, imt/1/
      real*8    ea_crt
      data ea_crt /130.0/
      integer iemin,iemax

      if ( (rsp(ie,it,ip).le.ea_crt) .and. 
     &               (e .gt. 2.4d0 .or. e.lt.2.1d0) ) then
         iemin = max(1,ie-nm)
         if (iemin.eq.1) then
           iemax = 2*nm+1
          else
           iemax = min(nengy,ie+nm)
         endif
         if (iemax.eq.nengy) iemin = nengy -2*nm-1
         do 10 i=iemin,iemax
           xwrk(i-iemin+1)=engy(i)
           ywrk(i-iemin+1)=rsp(i,it,ip)
 10      continue

         call xrt_smooth(xwrk,ywrk,iemax-iemin+1,e,smrsp,imt)
         
        else
         ie = max (1,ie)
c         write (6,*) e, engy(ie), engy(ie+1)
         call xrt_interp(engy(ie),engy(ie+1),
     &               rsp(ie,it,ip),rsp(ie+1,it,ip),e,smrsp,icon)
      endif
      return
      end

      subroutine xrt_gettheta(it,t)
      real*8 t(2)
      integer it

      common /xrt_xrtrsp/ rsp, engy, theta,phi,nengy,ntheta,nphi
     &                rspver
      real*8 rsp(500,30,5),engy(500),theta(30),phi(5)
      integer nengy,ntheta,nphi
      character(10) rspver

      it = min(ntheta-1,it) 
      t(1)=theta(it)
      t(2)=theta(it+1)
      return
      end

      subroutine xrt_getwghtt(th,it,wght)
      real*8 th,wght
      integer it

      common /xrt_xrtrsp/ rsp, engy, theta,phi,nengy,ntheta,nphi
     &                rspver
      real*8 rsp(500,30,5),engy(500),theta(30),phi(5)
      integer nengy,ntheta,nphi
      character(10) rspver

      if (theta(it).eq.th) then
         wght = 1.0d6
        else
         wght = 1.0d0/abs(theta(it)-th)
      endif
      return
      end

      subroutine xrt_getwghtp(ph,ip,wght)
      real*8 ph,wght
      integer ip

      common /xrt_xrtrsp/ rsp, engy, theta,phi,nengy,ntheta,nphi
     &                rspver
      real*8 rsp(500,30,5),engy(500),theta(30),phi(5)
      integer nengy,ntheta,nphi
      character(10) rspver

      real*8 wph

      wph = 45.0d0-abs(mod(ph+360.0d0, 90.0d0) -45.0d0 )
      if (phi(ip).eq.wph) then
         wght = 1.0d6
        else
         wght = 1.0d0/abs(phi(ip)-wph)
      endif
      return
      end


      subroutine xrt_rdresp (unt,fname,icon)
      integer unt,icon
      character fname*(*)

c**************************************************************
c*                                                            *
c*    subroutine xrt_rdresp                                       *
c*   read XRT response matrix named 'fname' on the unit,'unt' *
c*                                                            *
c*    UNT (IN): unit number                                   *
c*    FNAME(IN): filne name of the RSP matrix                 *
c*    ICON(OUT): 0  normal end                                *
c*               else return code from ftool libraly          *
c*                                                            *
c*    Oct 16,'93    removed mid      H.Awaki                  *
c**************************************************************

      common /xrt_xrtrsp/ rsp, engy, theta,phi,nengy,ntheta,nphi
     &                rspver
      real*8 rsp(500,30,5),engy(500),theta(30),phi(5)
      integer nengy,ntheta,nphi
      character(10) rspver


c . . > ftools
      integer block,extnum,htype
      integer maxcl
      parameter (maxcl=512)
      integer twidth,nrows,tfields,varidat
      character(70) tform(maxcl),ttype(maxcl),tunit(maxcl)
      character(70) extname,comment

      character  array(80)*1, a4*4, a2*2
      integer    dtype(maxcl),repeat(maxcl),width  
      logical   anyf

      character cmess*80
      integer   icon1

      real*4  e4(500), th4(30),ph4(5),rsp4

      icon = 0

c . . > step 1 open the FITS file, read only(0)
      call ftopen(unt,fname,0,block,icon)

      if (icon.ne.0) then
         cmess = 'unnable to open the XRT rsp file :'//fname(1:46)
         goto 999
      endif

c . . > move to the extension number 
      extnum = 2
      call ftmahd(unt,extnum,htype,icon)
      if (icon.ne.0) then
       write (cmess,'(a,i4)') 'rd_resp: error moving to ext # :',extnum
       goto 999
      endif

      if (htype.eq.2) then
c . . . . > read data format . .
         call ftghbn(unt,maxcl,nrows,tfields,ttype,tform,
     &               tunit,extname,varidat,icon)
         if (icon. ne. 0 ) then 
           write (cmess,'(a17,i4)') 'FTGHBN : status =',icon
           goto 999
         endif
         
         do 5 i=1,tfields
            call ftbnfm(tform(i),dtype(i),repeat(i),width,icon)
 5       continue

c . . . . FILE FOMAT
c        300E : real*4   : ENERG_LO (keV)
c        300E : real*4   : ENERG_HI (keV)
c         21E : real*4   : THETA    (arcmin)
c          4E : real*4   : PHI      (degree)
c      25200E : real*4   : EFFAREA  (cm2)

c . . . . old version
c         1E  : real*4   : energy (keV)
c         1I  : integer*2: off-axis angle (aarcmin)
c         1I  : integer*2: azimuthal angle (degree)
c         1E  : real*4   : effective area (cm2)

         nn = 0
c . . . . . > read one line
          call ftgcve(unt, 1, 1, 1, repeat(1), 0.0, e4,anyf,icon)
          call ftgcve(unt, 3, 1, 1, repeat(3), 0.0,th4,anyf,icon)
          call ftgcve(unt, 4, 1, 1, repeat(4), 0.0,ph4,anyf,icon)
c . . . . . > change real*4--> real*8
          do 10 i=1, repeat(4)
             phi(i)=ph4(i)
 10       continue
          do 11 j=1, repeat(3)
             theta(j)=th4(j)
 11       continue
          do 12 k=1, repeat(1)
             engy(k)=e4(k)
 12       continue

          do 20 i=1,repeat(4)
           do 21 j=1, repeat(3)
            do 22 k=1, repeat(1)
             nn = nn + 1
             call ftgcve(unt, 5, 1, nn, 1, 0.0,rsp4,anyf,icon)
             rsp(k,j,i)=rsp4
 22         continue
 21        continue
           nengy = repeat(1)
           ntheta= repeat(3)
           nphi  = repeat(4)
 20       continue
      endif
      
 999  continue

      if (icon.ne.0) then
        call  fcecho(cmess)
      endif
      icon1 = 0
      call ftclos (unt,icon1)
      if ( icon .eq. 0 ) icon = icon1
      return
      end

      subroutine xrt_order(eorder)
      integer eorder(*)

      common /xrt_xrtrsp/ rsp, engy, theta,phi,nengy,ntheta,nphi
     &                rspver
      real*8 rsp(500,30,5),engy(500),theta(30),phi(5)
      integer nengy,ntheta,nphi
      character(10) rspver


      real*8 en
      integer i,j

      j =1
      do 10 i=1,1199
         en = i* 0.01d0+0.0001d0
         j= max (1,j-10)
         do 20 while (engy(j).lt.en)
            j = j + 1
 20      continue
         eorder(i)=max(1,j-1)
c         write (6,*) i, eorder(i)
 10   continue
c      eorder(i+1)=eorder(i)

      return
      end

      
      subroutine xrt_interp(X1,X2,Y1,Y2,X,Y,ICN)
      real*8 X1,X2,Y1,Y2,X,Y
      integer ICN

      ICN = -1
      if (X1.ne.X2) then
         ICN = 0
         Y = (Y2-Y1)*(X-X1)/(X2-X1)+Y1
      endif
      return
      end

      subroutine xrt_smooth(X,Y,NY,XX,SMY,IMT)
      real*8 X(*),Y(*),SMY,XX
      integer NY,NM,IMT

      integer I
      real*8 sumy,sumx2,sumx,sumxy,wghty,sumx4,sumx3,sumx2y
      real*8 deta,detb,detc,det,a,b,c

      sumy = 0.0d0
      sumx2= 0.0d0
      sumx = 0.0d0
      sumxy= 0.0d0
      wghty= 0.0d0
      sumx4= 0.0d0
      sumx3= 0.0d0
      sumx2y=0.0d0

      if (imt.eq.0) then
         do 10 i=1,NY
            sumy = sumy + Y(i)
            wghty= wghty+ 1.0d0
 10      continue
         SMY = sumy/wghty
        elseif (imt.eq.1) then
          do 20 i=1,ny
             sumy = sumy + y(i)
             sumx = sumx + x(i)
             sumxy= sumxy+ x(i)*y(i)
             sumx2= sumx2+ x(i)*x(i)
 20       continue
          DETA = ny*sumxy-sumx*sumy
          DETB = sumy*sumx2-sumx*sumxy
          DET  =  ny*sumx2-sumx*sumx
          A    = DETA/DET
          B    = DETB/DET
          smy  = A*XX+B
        elseif (imt.eq.2) then
           do 30 i=1, ny
              sumy  = sumy  + y(i)
              sumx  = sumx  + x(i)
              sumxy = sumxy + x(i)*y(i)
              sumx2 = sumx2 + x(i)*x(i)
              sumx2y= sumx2y+ x(i)*x(i)*y(i)
              sumx3 = sumx3 + x(i)*x(i)*x(i)
              sumx4 = sumx4 + x(i)*x(i)*x(i)*x(i)
 30         continue
c            write (6,*) sumx,sumx2,sumx3,sumx4,sumy,sumxy,sumx2y
            DET = sumx4*sumx2*ny + 2* sumx3*sumx*sumx2 
     &           -sumx2*sumx2*sumx2-sumx4*sumx*sumx-ny*sumx3*sumx3
            DETA= sumx2y*sumx2*ny+sumx3*sumx*sumy+sumx2*sumxy*sumx
     &           -sumx2*sumx2*sumy-sumx*sumx*sumx2y-sumx3*sumxy*ny
            DETB= sumx4*sumxy*ny+sumx2y*sumx*sumx2+sumx2*sumx3*sumy
     &           -sumx2*sumxy*sumx2-sumx*sumy*sumx4-ny*sumx2y*sumx3
            DETC= sumx4*sumx2*sumy+sumx3*sumxy*sumx2+sumx2y*sumx3*sumx
     &           -sumx2y*sumx2*sumx2-sumx3*sumx3*sumy-sumx4*sumxy*sumx
c            write (6,*) DET,DETA,DETB,DETC
            A = DETA/DET
            B = DETB/DET
            C = DETC/DET
            smy = A * XX*XX + B*XX + C
      endif
                      
      return
      end

      subroutine xrt_wghave(x,w,n,ave)
      real*8 x(*),w(*),ave
      integer n

      real*8 sum,wsum

      sum = 0.0d0
      wsum= 0.0d0

      do 10 i=1,n
         sum = sum + x(i)*w(i)
         wsum= wsum+ w(i)
 10   continue
      ave = sum/wsum
      return
      end






