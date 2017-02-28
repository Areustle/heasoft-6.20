*- COCO - various coordinate convertions
      subroutine COCO (transform,phi1,theta1,
     *                               phi2,theta2,status)
* Description :
*  Transforms coordinates from one reference frame to another according to the
*  value of transform as follows :
*      transform='GALACTIC TO 1950'
*      transform='1950 TO GALACTIC'
*  For transform='1982.2 TO GALACTIC', for example, the precession is calculated
*      transform='1950 TO 2000', for example, for precession
*      transform='1976.6 TO ECLIPTIC' for the ecliptic coordinates at that epoch
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  26 August 1988 : original
*  31 July 1989 : addition of (ra,dec) to (l,b) transformation
*  3 August 1990 : change integer code to c*(*) transform
*  26 Sept  1990 : fixed error in ecliptic conversion
*  29 May 1992: moved the routine from XANADU:[LIB.UTIL] and replaced
*               references to UTIL_PRECESS with SLPREC.
       
      include 'status.codes'
      include 'dpi.inc'
* Import :
*  transform - transform required
      character*(*) transform
*  phi1,theta1 - azimuth,elevation coordinates in degrees
      real*8 phi1,theta1

* Export :
*  phi2,theta2 - azimuth,elevation coordinates in degrees
      real*8 phi2,theta2

* Status :
      integer status
* Local variables :
*  l,b - galactic coordinates
      real*8 l,b
*  ra,dec - celestial coordinates
      real*8 ra,dec                    
*  cl,sl,cb,sb - cosines and sines
      real*8 cl,sl,cb,sb               
*  ca,sa,cd,sd - cosines and sines
      real*8 ca,sa,cd,sd               
*  u,v,w - direction cosines
      real*8 u,v,w                     
*  cx,sx - cosine and sine
      real*8 cx,sx                     
*  e - obliquity of the eliptic
      real*8 e                         
*  ce,se - cosine and sine of e
      real*8 ce,se                     
*  t - time since 1950.0
      real*8 t                         
      real*8 epoch,epoch1,epoch2
      character(80) o
      integer i
* External reference :
      integer lenact
*-
      if(status.ne.ok__)return

      cx=cos(62.6d0*qi)
      sx=sin(62.6d0*qi)
      call upc(transform)
      if(transform(1:11).eq.'GALACTIC TO')then
         cl=cos((phi1-33d0)*qi)
         sl=sin((phi1-33d0)*qi)
         cb=cos(theta1*qi)
         sb=sin(theta1*qi)
         u=cl*cb
         v=sl*cb*cx-sb*sx
         w=sl*cb*sx+sb*cx
         dec=asin(w)/qi
         ra=atan2(v,u)/qi+282.25d0
         if(ra.lt.0d0)ra=ra+360d0
         if(ra.gt.360d0)ra=ra-360d0
         phi2=ra
         theta2=dec
         if(transform(13:16).ne.'1950')then
            read(transform(12:),*,iostat=status)epoch
            call SLPREC (ra,dec,1950d0,epoch)
         endif
      else if(index(transform,'TO GALACTIC').ne.0)then
         ra=phi1
         dec=theta1
         if(transform(1:4).ne.'1950')then
            read(transform,*,iostat=status)epoch
            call SLPREC (ra,dec,epoch,1950d0)
         endif
         ca=cos((ra-282.25d0)*qi)
         sa=sin((ra-282.25d0)*qi)
         cd=cos(dec*qi)
         sd=sin(dec*qi)
         u= ca*cd
         v= sa*cd*cx+sd*sx
         w=-sa*cd*sx+sd*cx
         b=asin(w)/qi
         l=atan2(v,u)/qi+33d0
         if(l.lt.0d0)l=l+360d0
         if(l.gt.360d0)l=l-360d0
         phi2=l
         theta2=b
      else if(index(transform,'TO ECLIPTIC').ne.0)then
         ra=phi1
         dec=theta1
         read(transform,*,iostat=status)epoch
* calculate the obliquity of the eliptic
         t=(epoch-1950d0)/100d0
         e=23.445788-1.30139d-2*t-9.1d-7*t*t+5.06d-7*t*t*t
         ce=cos(e*qi)
         se=sin(e*qi)
         ca=cos(ra*qi)
         sa=sin(ra*qi)
         cd=cos(dec*qi)
         sd=sin(dec*qi)
         u= ca*cd
         v= sa*cd*ce+sd*se
         w=-sa*cd*se+sd*ce
         b=asin(w)/qi
         l=atan2(v,u)/qi
         if(l.lt.0d0)l=l+360d0
         if(l.gt.360d0)l=l-360d0
         phi2=l
         theta2=b
      else if(transform.eq.' ')then
         phi2=phi1
         theta2=theta1
      else
         i=index(transform,'TO')
         if(i.gt.0)then
            read(transform(:i-1),*,iostat=status)epoch1
            if(status.eq.ok__)then
               read(transform(i+2:),*,iostat=status)epoch2
               if(status.eq.ok__)then
                  phi2=phi1
                  theta2=theta1
                  call SLPREC (phi2,phi1,epoch1,epoch2)
               endif
            endif
         else
            status=error__
         endif
      endif

      if(status.ne.ok__)then
         o=' coco unable to do '//transform(:lenact(transform))
     &                             //' transformations'
         write(*,*)o(:lenact(o))
      endif

      return

      end
