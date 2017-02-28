     	subroutine rdrpf1w
     *  (iunit,nrad,rad_lo,rad_hi, ntheta, theta_lo, theta_hi,
     *       nenerg, energ_lo, energ_hi, rpsf, qerror,
     *       rpsf_err, qarea, area_wgt,  maxrad, maxtheta,
     *       info, chatter, ierr)
      implicit none
      character(68) info(7) 
      integer iunit,nrad,ntheta,nenerg,chatter
      integer ierr,maxrad,maxtheta
      real rad_lo(maxrad),rad_hi(maxrad)
      real energ_lo(*),energ_hi(*)
      real theta_lo(maxtheta),theta_hi(maxtheta)
      real rpsf(maxrad,maxtheta,*),rpsf_err(maxrad,maxtheta,*)
      real area_wgt(maxrad,maxtheta,*)
      logical qarea,qerror
	call  rdrpf1(iunit, info(1), nrad, rad_lo, rad_hi,
     *  info(2),ntheta,theta_lo,theta_hi, info(3), 
     *  nenerg,energ_lo,energ_hi, info(4), rpsf,qerror,rpsf_err,
     *  info(5),qarea,area_wgt, info(6), info(7),maxrad, 
     *  maxtheta, ierr, chatter) 
      end
        
      subroutine wtrpf1w(ounit,rad_lo,rad_hi,
     &                   theta_lo,theta_hi,
     &                   energ_lo,energ_hi,
     &                   rpsf,rpsf_err,
     &                   area_wgt,hist,nk_hist,comms,
     &                   nk_comm, maxrad, maxtheta,info, num,qlog
     &                   ) 
      IMPLICIT NONE 
      integer num(5)
      integer ounit,maxrad, maxtheta 
C     integer nrad,nenerg,ntheta,ounit,ierr,maxrad,maxtheta 
      character*(68) info(8)
C      character*(*) extname, radunit,thetaunit,energunit,rpsfunit
C      character*(*) telescop,instrume,hduclas3
      real rad_lo(maxrad),rad_hi(maxrad),area_wgt(maxrad,maxtheta,*)
      real theta_lo(maxtheta), theta_hi(maxtheta)
      real energ_lo(*),energ_hi(*)
      real rpsf(maxrad,maxtheta,*),rpsf_err(maxrad,maxtheta,*)
      integer nk_hist,chatter,nk_comm
      character(80) hist(*),comms(*)
      logical qlog(2)
C     logical qarea,qerror
      call wtrpf1(ounit,info(1),info(2),num(1),rad_lo,rad_hi,
     &                        info(3),num(2),theta_lo,theta_hi,
     &                        info(4),num(3),energ_lo,energ_hi,
     &                        info(5),rpsf,qlog(1),rpsf_err,
     &                        info(6),qlog(2),area_wgt,info(7),
     &                        info(8),hist,nk_hist,comms, nk_comm,
     &                        maxrad,maxtheta,num(4),num(5))
      end


      subroutine rdrmf4w(iunit, chatter, Qorder,Maxen,Maxgrp,Maxelt,
     &          areascal, energ_lo, energ_hi,
     &          ngrp, F_chan, N_chan, Isorder,
     &          Order, Fmatrix, lo_thresh, 
     &          info, num, ierr)

      IMPLICIT NONE
      INTEGER Maxelt , Maxen , Maxgrp
      INTEGER Chatter , Ierr
      INTEGER Iunit 
      INTEGER Ngrp(Maxen) , F_chan(Maxgrp)
      INTEGER N_chan(Maxgrp), Order(Maxgrp)
      REAL Areascal , Lo_thresh
      REAL Energ_lo(Maxen) , Energ_hi(Maxen)
      REAL Fmatrix(Maxelt)
      LOGICAL Qorder, Isorder
      character(68) info(7)
      integer num(5)

       call RDRMF4(Iunit,Chatter,Qorder,Maxen,Maxgrp,Maxelt,
     &                  info(1),info(2),info(3),info(4),info(5),
     &                  info(6),Areascal,info(7),num(1),num(2),
     &                  num(3),num(4),num(5),Energ_lo,Energ_hi,Ngrp,
     &                  F_chan,N_chan,Isorder,Order,Fmatrix,Lo_thresh,
     &                  Ierr)

       end

      SUBROUTINE WTRMF4W(Ounit,Nk_hist,Hist,Nk_comm,Comment,
     &                  Areascal,Numelt,
     &                  Nenerg,Numgrp,Energ_lo,Energ_hi,
     &                  Ngrp,F_chan,N_chan,Qorder,Order,Fmatrix,
     &                  Lo_thresh,info, num)


      IMPLICIT NONE
      INTEGER Nenerg , Numelt, Numgrp
      INTEGER Ounit , Nk_hist , Nk_comm
      INTEGER Ngrp(Nenerg) , F_chan(Numgrp)
      INTEGER N_chan(Numgrp) , Order(Numgrp)
      REAL Areascal , Lo_thresh
      REAL Energ_lo(Nenerg) , Energ_hi(Nenerg)
      REAL Fmatrix(Numelt)
      character(68) Hist(*) , Comment(*)
      character(68) info(7)
      integer num(4)
      LOGICAL Qorder

       Call WTRMF4(Ounit,num(1),Nk_hist,Hist,Nk_comm,Comment,
     &             info(1),info(2),info(3),info(4),info(5),info(6),
     &             Areascal,info(7),num(2),Numelt,
     &             num(3),Nenerg,Numgrp,Energ_lo,Energ_hi,
     &             Ngrp,F_chan,N_chan,Qorder,Order,Fmatrix,
     &             Lo_thresh,num(4))



        end 

      subroutine rdeef1w(iunit,rad_lo,rad_hi,
     >                        theta_lo,theta_hi,
     >                        energ_lo,energ_hi,
     >                        reef,reef_err,
     >                        area_wgt,
     >                        maxrad,maxtheta,info,num,
     >                        qlog,ierr,chatter)

      IMPLICIT NONE
      integer iunit,chatter
      integer num(3)
c     integer nrad,ntheta,nenerg
      integer ierr,maxrad,maxtheta
      real rad_lo(maxrad),rad_hi(maxrad)
      real energ_lo(*),energ_hi(*)
      real theta_lo(maxtheta),theta_hi(maxtheta)
      real reef(maxrad,maxtheta,*),reef_err(maxrad,maxtheta,*)
      real area_wgt(maxrad,maxtheta,*)
      character(68) info(7)
c     character*(*) radunit,thetaunit,energunit,reefunit,hduclas3
c     character*(*) instrume,telescop
      logical qlog(2)
C     logical qarea,qerror
      
       call rdeef1(iunit,info(1),num(1),rad_lo,rad_hi,
     >                        info(2),num(2),theta_lo,theta_hi,
     >                        info(3),num(3),energ_lo,energ_hi,
     >                        info(4),reef,qlog(1),reef_err,
     >                        info(5),qlog(2),area_wgt,info(6),
     >                        info(7),maxrad,maxtheta,ierr,chatter)
      end

      subroutine wteef1w(ounit, rad_lo,
     >                  rad_hi, theta_lo, theta_hi,
     >                  energ_lo, energ_hi,
     >                  reef,reef_err,area_wgt,
     >                  hist,nk_hist,comms,nk_comm, 
     >                  maxrad, maxtheta, info, num, qlog,
     >                  ierr)
      implicit none
      integer num(4)
c      integer nrad,nenerg,ntheta,chatter
      integer ounit,ierr,maxrad,maxtheta
      character(68) info(9)
c      character*(*) radunit,thetaunit,energunit,reefunit
c      character*(*) telescop,instrume,extname,hduclas3, hduclas4
      real rad_lo(maxrad),rad_hi(maxrad),area_wgt(maxrad,maxtheta,*)
      real theta_lo(maxtheta), theta_hi(maxtheta)
      real energ_lo(*),energ_hi(*)
      real reef(maxrad,maxtheta,*),reef_err(maxrad,maxtheta,*)
      integer nk_hist,nk_comm
      character(80) hist(*),comms(*)
      logical qlog(2)
c      logical qarea,qerror
      call wteef1(ounit, info(1),info(2), info(3),num(1),rad_lo,
     >                  rad_hi, info(4),num(2),theta_lo, theta_hi,
     >                  info(5),num(3), energ_lo, energ_hi,info(6),
     >                  reef,qlog(1),reef_err,info(7),qlog(2),area_wgt,
     >                  hist,nk_hist,comms,nk_comm, info(8), info(9),
     >                  maxrad, maxtheta, ierr, num(4))
      end
