
      subroutine eqread(t,nionp,feq,f,ionel,ionstage)

      INTEGER nionp

      REAL t, feq(2,nionp), f(nionp)
      INTEGER ionel(nionp), ionstage(nionp)

*        Equilibrium ionization ionic concentrations for solar
*        abundances
*
*        Input:  t   -   temperature (K)
*        Output: f   -   ionic concentrations
*

c
c      the lower and upper limiting values of log temperature,
c            the increment in log temp., and the number of 
c            temperature values in the ionization tables.
c
      real tll,tlu,dtl
      integer nt
      parameter (tll=4.0e0,tlu=9.0e0,dtl=4.0e-3,nt=1251)
c      index for temperature value in ionization files
      integer it
      real rit, p, q

      INTEGER NELEM0
      PARAMETER (NELEM0=10)
      INTEGER nzz0(NELEM0)
      INTEGER NELEM1
      PARAMETER (NELEM1=11)
      INTEGER nzz1(NELEM1)

      integer ion, lun, block, ierr, hdutyp
      logical qanyf, qincar
      character(255) pathd, filenm, contxt
      character(128) pname, pvalue, ovalue
      integer lenn

      character(255) fgmodf
      character(128) fgmstr
      integer lenact
      external lenact, fgmodf, fgmstr

      SAVE ovalue, nzz0, nzz1

      DATA nzz0 /6,7,8,10,12,14,16,20,26,28/
      DATA nzz1 /6,7,8,10,12,14,16,18,20,26,28/
      DATA ovalue / '1.1' /

c get the NEI version in use. If not 1.0 then we need to include Ar.

      pname = 'NEIVERS'
      pvalue = fgmstr(pname)
      IF ( pvalue .NE. ' ' ) ovalue = pvalue
      qincar = .TRUE.
      IF ( ovalue(1:3) .EQ. '1.0' ) qincar = .FALSE.

      pathd = fgmodf()
      lenn = lenact(pathd)
      IF ( qincar ) THEN
         filenm = pathd(:lenn) // 'ioneq0502.fits'
      ELSE
         filenm = pathd(:lenn) // 'ioneq.fits'
      ENDIF
      ierr = 0

c  Calculate the temperature index and linear interpolation coefficients

      rit=(alog10(t)-tll)/dtl+1.00001
      it=int(rit)
      p=rit-float(it)
      IF ( it .LE. 0 ) THEN
         it = 1
         p = 0.0
      ELSEIF ( it .GE. nt ) THEN
         it = nt - 1
         p = 1.0
      ENDIF
      q=1.-p

      WRITE(contxt,'(a,1pg10.3,1x,i5,1x,1pg13.6,1x,1pg13.6)') 
     &              'EQREAD Interpolating ', t, it, rit, p
      CALL xwrite(contxt, 20)

c Read the data for the two temperatures to be interpolated

      call getlun(lun)
      call ftopen(lun, filenm, 0, block, ierr)
      call ftmrhd(lun, 1, hdutyp, ierr)
      DO ion = 1, nionp
         CALL ftgcve(lun, 1, ion, it, 2, 0.0, feq(1,ion), qanyf, 
     &                  ierr)
      ENDDO
      CALL ftclos(lun, ierr)
      CALL frelun(lun)

c do the interpolation

      DO ion=1,nionp
         f(ion)=q*feq(1,ion)+p*feq(2,ion)
      ENDDO

c set the ion element and ion stage arrays - include fully-ionized H and He.

      ionel(1) = 1
      ionstage(1) = 2
      ionel(2) = 2
      ionstage(2) = 3

      ion = 2
      IF ( qincar ) THEN
         DO ielem = 1, NELEM1
            DO i = 1, nzz1(ielem)+1
               ion = ion + 1
               ionel(ion) = nzz1(ielem)
               ionstage(ion) = i
            ENDDO
         ENDDO
      ELSE
         DO ielem = 1, NELEM0
            DO i = 1, nzz0(ielem)+1
               ion = ion + 1
               ionel(ion) = nzz0(ielem)
               ionstage(ion) = i
            ENDDO
         ENDDO
      ENDIF

      return
      end
