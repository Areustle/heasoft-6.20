*- aitoff - deal with Aitoff projections
      subroutine aitoff_origin(phi0,theta0)
* Description :
*  Via the entry point AITOFF_XY this subroutine calculates the rectangular
*  Aitoff coordinates (x,y) where
*
*        -2 <= x <= +2 , -1 <= y <= +1
*
*  for the azimuth-elevation coordinates (phi,theta) where usually
*
*        0 <= phi <= 360 DEGREES, -90 <= theta <= +90 DEGREES
*
*  although the usual rules of trigonometry apply for angles specified
*  outside this range. North, defined by theta=+90, is up.
*  By default the (x,y) = (0,0) corresponds to (phi,theta) = (0,0).
*  The origin may be put elsewhere via the main entry point
*
*        call AITOFF_ORIGIN(phi0,theta0)
*
*  For the the routines to be used for small parts of the sky as well as
*  the current all-sky plots it may prove necessary in the future to make
*  all the arguments DOUBLE PRECISION real*8, but we'll cross that bridge
*  when we come to it. Also included below is an independent subroutine
*  called aitoff_frame which works out a series of points for a grid of
*  coordinates to overlay an Aitoff plot. It only works for the whole sky
*  centered on (0,0) for the moment, otherwise returning npts=0.
*
* Author :
*  Andy Pollock (HEASRC::POLLOCK)
* History :
*  2 May 1989 : original
*  17 February 1992 : generalised version collecting all Aitoff-related code
*  23 June 1993 : replaced occurrences of the backslash character '\' with
*                 ascii equivalent for correct handling by pgplot - Song Yom

      include 'dpi.inc'

* Import :
      real*4 phi0
      real*4 theta0

* Import at entry point aitoff_xy :
      real*4 phi
      real*4 theta
* Export at entry point aitoff_xy :
      real*4 x
      real*4 y

* Local variables :
      logical*4 transform
      real*8 ct,st
      real*8 cf,sf
c     real*8 ct0,st0
c     real*8 cf0,sf0
      real*8 cf2,sf2
      real*8 a11,a12,a13
      real*8 a21,a22,a23
      real*8 a31,a32,a33
      real*8 f,f2,t,z
      real*8 l,m,n
      real*8 u,v,w
* Local data :
      data transform /.false./
      data a11 /1d0/
      data a12 /0d0/
      data a13 /0d0/
      data a21 /0d0/
      data a22 /1d0/
      data a23 /0d0/
      data a31 /0d0/
      data a32 /0d0/
      data a33 /1d0/
* Saved variables :
C     save transform,a11,a12,a13,a21,a22,a23,a31,a32,a33
*-
      transform=((phi0.ne.0d0).or.(theta0.ne.0d0))
      if(transform)then
* define elements aij of the matrix to transform to a coordinate system
* centred on (phi0,theta0) with north up
         f=phi0*qi
         cf=cos(f)
         sf=sin(f)
         t=theta0*qi
         ct=cos(t)
         st=sin(t)
         a11= cf*ct
         a12= sf*ct
         a13=    st
         a21=-sf
         a22= cf
         a23=  0d0
         a31=-cf*st
         a32=-sf*st
         a33=    ct
      else
         a11=1d0
         a12=0d0
         a13=0d0
         a21=0d0
         a22=1d0
         a23=0d0
         a31=0d0
         a32=0d0
         a33=1d0
      endif

      return

***************************************************************************
***************************************************************************

      entry aitoff_xy(phi,theta,x,y)

      t=theta*qi
      ct=cos(t)
      st=sin(t)
      if(transform)then
         f=phi*qi
         cf=cos(f)
         sf=sin(f)
         l=cf*ct
         m=sf*ct
         n=st
         u=a11*l+a12*m+a13*n
         v=a21*l+a22*m+a23*n
         w=a31*l+a32*m+a33*n
         if(abs(w).lt.1d0)then
            ct=sqrt(1d0-w*w)
            st=w
            cf=u/ct
            sf=v/ct
            if(abs(cf).lt.1d0)then
               cf2=sqrt((1d0+cf)/2d0)
               sf2=sign(sqrt((1d0-cf)/2d0),sf)
            else if(cf.lt.1d0)then
               cf2=0d0
               sf2=sign(1d0,sf)
            else
               cf2=1d0
               sf2=0d0
            endif
         else
            ct=0d0
            st=sign(1d0,w)
            cf2=1d0
            sf2=0d0
         endif
      else
         f=phi
         do while(f.gt.180.)
            f=f-360.
         end do
         do while(f.lt.-180.)
            f=f+360.
         end do
         if(((f.ge.0.).and.(f.le.180.)).or.
     &      ((f.lt.0.).and.(f.ge.-180.)))then
            f2=dble(f*qi/2d0)
         else if((f.gt.180.).and.(f.le.360.))then
            f2=dble(f*qi/2d0-pi)
         endif
         cf2=cos(f2)
         sf2=sin(f2)
      endif

      z=sqrt(1d0+ct*cf2)
      x=sngl(-2d0*ct*sf2/z)
      y=sngl(st/z)

      return

      end

***************************************************************************
***************************************************************************

*- aitoff_grid - coordinate grid to overlay Aitoff plot
      subroutine aitoff_grid(phi0,theta0,npts,x,y)
* Description :
*  Only works for the whole sky centred on (0.,0.) for the moment. The
*  series of points (x,y) are intended to be joined by a line.
* History :
*  17 February 1992 : moved from [lib.util] within [lib.coords]aitoff.for

* Import :
      real*4 phi0
      real*4 theta0
* Export :
      integer*4 npts
      real*4 x(*)
      real*4 y(*)

* Import at entry point AITOFF_LABELS :
      character*(*) system
* Export at entry point AITOFF_LABELS :
      integer*4 nl
      character*(*) text(*)
      character*(*) just(*)

* Local constants :
      integer*4 nf
      parameter (nf=11)
      integer*4 nt
      parameter (nt=5)
      character(1) bslash
C      parameter (bslash = char('5c'x))
      character(5) degrees
C      parameter (degrees=bslash//'uo'//bslash//'d')
      character(5) hours
C      parameter (hours=bslash//'uh'//bslash//'d')
* Local variables :
      real*4 fg(nf)
      real*4 tg(nt)
      real*4 f,df,t,dt
      real*4 x1,x2,y1,y2
      real*4 f1,f2
      real*4 t1,t2
      integer*4 n
      integer*4 jt,jtmax
      integer*4 jf,jfmax
      integer*4 sign
      character(10) s
      character(10) ft,ff
      character(30) label
      integer*4 ll
* External references :
      character(80) lftjst
      integer*4 lenact
* Local data :
      data fg /0.,30.,60.,90.,120.,150.,210.,240.,270.,300.,330./
      data tg /-60.,-30.,0.,30.,60./
*-
      bslash = char(92)
      degrees=bslash//'uo'//bslash//'d'
      hours=bslash//'uh'//bslash//'d'

      if((phi0.ne.0d0).or.(theta0.ne.0d0))then
         npts=0
         return
      endif

      call aitoff_origin(phi0,theta0)

      sign=1
      n=0

* the azimuth grid
      do jf=1,nf
         sign=-sign
         f=fg(jf)
         t=90.*real(sign)
         if(f.eq.0)then
            n=n+1
            call aitoff_xy(f,t,x(n),y(n))
            t=-t
            n=n+1
            call aitoff_xy(f,t,x(n),y(n))
         else
            dt=-0.5*real(sign)
            jtmax=2*nint(90/0.5)+1
            do jt=1,jtmax
               n=n+1
               call aitoff_xy(f,t,x(n),y(n))
               t=t+dt
            end do
         endif
      end do

* the theta grid goes over some of the frame to make the line continuous
      t=90.
      dt=-0.5
      do jt=nt,1,-1
         sign=-sign
         f=180.*real(sign)
         do while(t.gt.tg(jt))
            n=n+1
            call aitoff_xy(f,t,x(n),y(n))
            t=t+dt
         end do
         t=tg(jt)
         if(t.eq.0)then
            n=n+1
            call aitoff_xy(f,t,x(n),y(n))
            f=-180.
            n=n+1
            call aitoff_xy(f,t,x(n),y(n))
         else
            df=-0.5*real(sign)
            jfmax=2*nint(180/0.5)+1
            do jf=1,jfmax
               n=n+1
               call aitoff_xy(f,t,x(n),y(n))
               f=f+df
             end do
         endif
      end do

      npts=n

      return

***************************************************************************
*** aitoff_frame **********************************************************
***************************************************************************
      entry aitoff_frame(phi0,theta0,npts,x,y)

      if((phi0.ne.0d0).or.(theta0.ne.0d0))then
         npts=0
         return
      endif

      call aitoff_origin(phi0,theta0)

      n=0
      do sign=-1,1,2
         f= 180.*real(sign)
         t=  90.*real(sign)
         dt=-0.5*real(sign)
         jtmax=2*nint(90/0.5)+1
         do jt=1,jtmax
            n=n+1
            call aitoff_xy(f,t,x(n),y(n))
            t=t+dt
         end do
      end do

      npts=n

      return

***************************************************************************
*** aitoff_labels *********************************************************
***************************************************************************
      entry aitoff_labels(system,nl,x,y,text,just)

      s=system
      call upc(s)

      ft='(sp,i3)'
      ff='(i3)'

      nl=1
      f=0.
      t=90.
      t1=t-5.
      t2=t
      call aitoff_xy(f,t1,x1,y1)
      call aitoff_xy(f,t2,x2,y2)
      x(nl)=x2+(x2-x1)
      y(nl)=y2+(y2-y1)
      label=lftjst(nint(t),'i*4',ft)
      ll=lenact(label)
      label(ll+1:)=degrees
      text(nl)=label
      just(nl)='CENTER'

      nl=2
      f=0.
      t=-90.
      t1=t+5.
      t2=t
      call aitoff_xy(f,t1,x1,y1)
      call aitoff_xy(f,t2,x2,y2)
      x(nl)=x2+(x2-x1)
      y(nl)=y2+(y2-y1)
      label=lftjst(nint(t),'i*4',ft)
      ll=lenact(label)
      label(ll+1:)=degrees
      text(nl)=label
      just(nl)='CENTER'

      f1=-175.
      f2=-180.
      do jt=1,nt
         nl=nl+1
         t=tg(jt)
         call aitoff_xy(f1,t,x1,y1)
         call aitoff_xy(f2,t,x2,y2)
         x(nl)=x2+(x2-x1)
         y(nl)=y2+(y2-y1)
         label=lftjst(nint(t),'i*4',ft)
         ll=lenact(label)
         label(ll+1:)=degrees
         text(nl)=label
         just(nl)='LEFT'
      end do

      t=-2.5
      f=180.
      do sign=-1,1,2
         nl=nl+1
         call aitoff_xy(f*real(sign),t,x(nl),y(nl))
         if(s.eq.'CELESTIAL')then
            label=lftjst(nint(f/15.),'i*4',ff)
            ll=lenact(label)
            label(ll+1:)=hours
         else
            label=lftjst(nint(f),'i*4',ff)
            ll=lenact(label)
            label(ll+1:)=degrees
         endif
         text(nl)=label
         just(nl)='CENTER'
      end do

      do jf=1,nf
         nl=nl+1
         f=fg(jf)
         call aitoff_xy(f,t,x(nl),y(nl))
         if(s.eq.'CELESTIAL')then
            label=lftjst(nint(f/15.),'i*4',ff)
            ll=lenact(label)
            label(ll+1:)=hours
         else
            label=lftjst(nint(f),'i*4',ff)
            ll=lenact(label)
            label(ll+1:)=degrees
         endif
         text(nl)=label
         just(nl)='CENTER'
      end do

      return

      end


