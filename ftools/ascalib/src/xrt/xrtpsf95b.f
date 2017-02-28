
      FUNCTION xrtpsf95b(e,theta,phi,ox,oy)
      DOUBLE PRECISION e, theta, phi, ox, oy, xrtpsf95b

c     arguments
c     e     (in)  : energy (keV)
c     theta, phi(in) : offset angle(arcmin), azimuthal angle(deg)
c     ox,oy (in)  : offset position from source position   (mm)
c     xrtpsf (out): point spread function normalized at 12 mm (1/mm2)
c
c                  /\  Y
c                   |
c                   |
c      +--------------------------+
c      |                          |
c      |              source pos  |
c      |                  *       |
c      |            (theta,phi)   |
c      |                          |
c      |           #              |----------> X
c      |        (ox, oy)          |
c      |                          |
c      |                          |
c      |                          |
c      |                          |
c      +--------------------------+
c         DETECTOR PLANE
c
c
c     HISTORY:   Ver 1.0   
c                Ver 2.0
c                Ver 2.1   xrtpsf95a 
c                Ver 2.2   xrtpsf95b       95.06.08
c

      DOUBLE PRECISION theta_mm, r, xrtpsf
      INTEGER icon

c ...>  arcmin-->mm  ( 1arcmin=1.0181088arcmin )
      theta_mm = theta*1.0181088d0

c . . .> ox,oy--> r
      r = SQRT (ox*ox+oy*oy)

      CALL xrt_psf2(e,theta_mm,phi,r,xrtpsf)

      xrtpsf95b = EXP(xrtpsf)

      RETURN
      END

c ************************************************************************

      SUBROUTINE xrtpsf95b_init(filename,icon)
      INTEGER icon
      CHARACTER*(*) filename

      character(80) cmess

      icon = 0
      WRITE (cmess,'(a)') 'read error occured in xrtpsf.fits, and exit!'
      CALL xrt_rdpsf(21,filename,icon)
      IF ( icon .NE. 0 ) THEN
c         call fcecho(cmess)
          icon = -99
          RETURN
      ENDIF
      CALL xrt_lin2log(icon)
      CALL xrt_mkidev(icon)

      RETURN
      END

c ************************************************************************

      SUBROUTINE xrt_psf2(e,theta,phi,r,xrtpsf)
      DOUBLE PRECISION e,theta,phi,r,xrtpsf

c Return the XRT PSF for energy e, off-axis theta, azimuthal angle
c phi, and distance r from the source position.

      INCLUDE 'xrtpsf.inc'

      INTEGER ie,it,ip,ir,icn
      DOUBLE PRECISION smpsf(2,2,2),smpsf1(2,2),smpsf2(2)
      DOUBLE PRECISION phi_wrk

c Find the tabulated energy, theta, phi, and radius immediately less than
c the values input.

      phi_wrk = MOD(phi,90.0d0)
      IF ( phi_wrk .GT. 45.0d0 ) phi_wrk = 90.0d0-phi_wrk   
      ip = 1
      IF ( phi_wrk .GE. 30.0d0 ) ip = 2
   
      ie = 1
      DO WHILE ( E .GT. emtx(ie) )
         ie = ie +1
      ENDDO
      ie = MAX(ie-1,1)
      IF (ie .GE. nne ) ie = nne-1

      it = theta*idevth(1)+1
      IF ( it .GE. nnth ) it = nnth-1

      ir = r*idevr(1)+0.5d0
      ir = max(ir,1)
      ir = min(ir,nnir)

c Interpolate in 4-D to get the PSF

c      write (6,*) ir,ie,it,ip

      CALL xrt_getsmpsf(ir,r,ie,  it,  ip,  smpsf(1,1,1))
      CALL xrt_getsmpsf(ir,r,ie+1,it,  ip,  smpsf(2,1,1))
      CALL xrt_getsmpsf(ir,r,ie  ,it+1,ip,  smpsf(1,2,1))
      CALL xrt_getsmpsf(ir,r,ie+1,it+1,ip,  smpsf(2,2,1))
      CALL xrt_getsmpsf(ir,r,ie  ,it  ,ip+1,smpsf(1,1,2))
      CALL xrt_getsmpsf(ir,r,ie+1,it,  ip+1,smpsf(2,1,2))
      CALL xrt_getsmpsf(ir,r,ie  ,it+1,ip+1,smpsf(1,2,2))
      CALL xrt_getsmpsf(ir,r,ie+1,it+1,ip+1,smpsf(2,2,2))

c      write (6,*) " 1 1 1 , 2 1 1  ",smpsf(1,1,1),smpsf(2,1,1)
c      write (6,*) " 1 2 1 , 2 2 1  ",smpsf(1,2,1),smpsf(2,2,1)
c      write (6,*) " 1 1 2 , 2 1 2  ",smpsf(1,1,2),smpsf(2,1,2)
c      write (6,*) " 1 2 2 , 2 2 2  ",smpsf(1,2,2),smpsf(2,2,2)


      CALL xrt_interp(emtx(ie),emtx(ie+1),
     &   smpsf(1,1,1),smpsf(2,1,1), E, smpsf1(1,1),icn)
      CALL xrt_interp(emtx(ie),emtx(ie+1),
     &   smpsf(1,2,1),smpsf(2,2,1), E, smpsf1(2,1),icn)
      CALL xrt_interp(thmtx(it),thmtx(it+1),
     &   smpsf1(1,1),smpsf1(2,1), theta, smpsf2(1),icn)

c      write (6,*) smpsf2(1)
      CALL xrt_interp(emtx(ie),emtx(ie+1),
     &   smpsf(1,1,2),smpsf(2,1,2),E, smpsf1(1,2),icn)
      CALL xrt_interp(emtx(ie),emtx(ie+1),
     &   smpsf(1,2,2),smpsf(2,2,2),E, smpsf1(2,2),icn)
      CALL xrt_interp(thmtx(it),thmtx(it+1),
     &   smpsf1(1,2),smpsf1(2,2),theta, smpsf2(2),icn)

      CALL xrt_interp(phimtx(ip),phimtx(ip+1), 
     &   smpsf2(1),smpsf2(2),phi_wrk,  xrtpsf,icn)

c      write (6,*) 'xrtpsf =',xrtpsf

      RETURN
      END

c ************************************************************************

      SUBROUTINE xrt_getsmpsf(ir,r,ie,it,ip,smpsf)
      INTEGER ie,it,ip,ir
      DOUBLE PRECISION r,smpsf

      INCLUDE 'xrtpsf.inc'


      IF ( r .LT. rmtx(1) ) THEN
         smpsf = psfmtx(1,it,ip,ie)
      ELSE
         smpsf = (psfmtx(ir+1,it,ip,ie)-psfmtx(ir,it,ip,ie))*
     &             idevr(ir)*(r-rmtx(ir))+psfmtx(ir,it,ip,ie)
      ENDIF

c      write (6,*) rmtx(ir), r, exp(psfmtx(ir,it,ip,ie)),
c     &                         exp(psfmtx(ir+1,it,ip,ie))

      RETURN
      END

c ************************************************************************
    
      SUBROUTINE xrt_rdpsf (unt,fname,icon)
      INTEGER unt, icon
      CHARACTER*(*) fname

c**************************************************************
c*                                                            *
c*    subroutine rdresp                                       *
c*   read XRT PSF matrix named 'fname' on the unit,'unt'      *
c*                                                            *
c*    UNT (IN): unit number                                   *
c*    FNAME(IN): filne name of the RSP matrix                 *
c*    ICON(OUT): 0  normal end                                *
c*               else return code from ftool libraly          *
c*                                                            *
c*    Dec. 15,'94    original      H.Awaki                    *
c**************************************************************

      INCLUDE 'xrtpsf.inc'

c . . > ftools

      INTEGER MAXCL
      PARAMETER (MAXCL=10)

      INTEGER block,extnum,htype
      INTEGER twidth,nrows,tfields,varidat
      character(70) tform(MAXCL),ttype(MAXCL),tunit(MAXCL)
      character(70) extname,comment

      REAL      rval(20), pixsiz
      INTEGER   dtype(maxcl), repeat(maxcl)
      INTEGER   width, icon1, i, j, k, l, nn
      CHARACTER cmess*80
      LOGICAL   anyf


      icon = 0

c open the FITS file, read only(0)

      CALL ftopen(unt,fname,0,block,icon)

      IF ( icon .NE. 0 ) THEN
         cmess = 'unable to open the XRT psf file :'//fname(1:46)
         GOTO 999
      ENDIF

c move to the RPSF extension

      CALL ftmrhd(unt, 1, htype, icon)
      IF ( icon .NE. 0 ) THEN
         WRITE (cmess,'(a)') 'xrt_rdpsf: error moving to RPSF extension'
         GOTO 999
      ENDIF

c . . . . > read data format . .

      IF ( htype .EQ. 2 ) THEN

         CALL ftghbn(unt,maxcl,nrows,tfields,ttype,tform,
     &               tunit,extname,varidat,icon)

         IF ( icon .NE. 0 ) THEN
            WRITE (cmess,'(a17,i4)') 'FTGHBN : status =',icon
            GOTO 999
         ENDIF
         
         DO i = 1, tfields
            CALL ftbnfm(tform(i),dtype(i),repeat(i),width,icon)
         ENDDO

c . . . . FILE FORMAT
c          502E : real*4   : RAD_LO    (arcmin)
c          502E : real*4   : RAD_HI    (arcmin)
c           11E : real*4   : THETA     (arcmin)
c            3E : real*4   : PHI       (degree)
c           11E : real*4   : ENERG_LO   (keV)
c           11E : real*4   : ENERG_HI   (keV)
c       182226E : real*4   : RPSF       (cm2)

         nne = repeat(5)
         nnth = repeat(3)
         nnph = repeat(4)
         nnir = repeat(1)

c Read the PIXSIZ keyword

         CALL ftgkye(unt, 'PIXSIZE', pixsiz, cmess, icon)

c Read the energies, theta, and phi

         CALL ftgcvd(unt, 3, 1, 1, nnth, 0.0d0, thmtx, anyf, icon)
         CALL ftgcvd(unt, 4, 1, 1, nnph, 0.0d0, phimtx, anyf, icon)
         CALL ftgcve(unt, 5, 1, 1, nne, 0.0, rval, anyf, icon)
         DO i = 1, nne
            emtx(i) = DBLE(rval(i)/2.)
         ENDDO
         CALL ftgcve(unt, 6, 1, 1, nne, 0.0, rval, anyf, icon)
         DO i = 1, nne
            emtx(i) = emtx(i) + DBLE(rval(i)/2.)
         ENDDO

c Read the radial PSF values and correct back to a differential PSF
c by dividing by the pixel size

         nn = 0
         DO i = 1, nne
            DO j = 1, nnph
               DO k = 1, nnth
                  DO l = 1, nnir
                     nn = nn + 1
                     CALL ftgcvd(unt, 7, 1, nn, 1, 0.0d0, 
     &                           psfmtx(l,k,j,i), anyf, icon)
                     psfmtx(l,k,j,i) = psfmtx(l,k,j,i)/DBLE(pixsiz)
                  ENDDO
                  psfmtx(nnir,k,j,i) = psfmtx(nnir-1,k,j,i)*1.0d-40
               ENDDO
            ENDDO
         ENDDO
         nnir = nnir - 1

         DO i = 1, nnir
            rmtx(i) = (i-0.5)*0.05d0
         ENDDO
         rmtx(nnir+1) = rmtx(nnir) + 1000.0d0

      ENDIF
      
 999  CONTINUE
      IF ( icon .NE. 0 ) THEN
c        call  fcecho(cmess)
      ENDIF
      icon1 = 0
      CALL FTCLOS (unt,icon1)
      IF ( icon .EQ. 0 ) icon = icon1

      RETURN
      END

c ************************************************************************

      SUBROUTINE xrt_lin2log(icon)
      INTEGER icon

      INCLUDE 'xrtpsf.inc'

      INTEGER i, j, k, l

c      write (6,*) nne, nnth, nnph
      DO i = 1, nne
         DO j = 1, nnph
            DO k = 1, nnth
               DO l = 1, nnir+1
                  IF ( psfmtx(l,k,j,i) .NE. 0.0 ) 
     &                 psfmtx(l,k,j,i) = LOG(psfmtx(l,k,j,i))
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END

      SUBROUTINE xrt_mkidev(icon)
      INTEGER icon

      INCLUDE 'xrtpsf.inc'

      INTEGER i

      icon = 0
      DO i = 1, nne-1
         ideve(i) = 1.0d0/(emtx(i+1)-emtx(i))
      ENDDO

      DO i = 1, nnth-1
         idevth(i) = 1.0d0/(thmtx(i+1)-thmtx(i))
      ENDDO

      DO i = 1, nnph-1
         idevphi(i) = 1.0d0/(phimtx(i+1)-phimtx(i))
      ENDDO

      DO i = 1, nnir
         idevr(i) = 1.0d0/(rmtx(i+1)-rmtx(i))
      ENDDO

c      write (6,*) nnir, idevr(nnir)
c      write (6,*) psfmtx(nnir,1,1,1),psfmtx(nnir+1,1,1,1)
c      write (6,*) rmtx(nnir), rmtx(nnir+1)

      RETURN
      END


