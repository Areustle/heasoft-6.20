
      SUBROUTINE mlvmkl(ear, ne, param, ifl, photar, photer)

      INTEGER ne, ifl
      REAL ear(0:ne), param(18), photar(ne), photer(ne)

c
c XSPEC model subroutine to calculate a MEKAL model with multiple temperatures
c according to Mike L's models for A496.
c
c This program calls 'vmekal' thus allowing one to vary the elemental
c abundances.
c 
c Parameters:
c    param(1) = option (1,2,3)
c    param(2) = nH (cm^-3)  Fixed at 1 for most applications
c    param(3) = He abundance
c    param(4) = C   "
c    param(5) = N   "
c    param(6) = O   "
c    param(7) = Ne  "
c    param(8) = Na  "
c    param(9) = Mg  "
c    param(10)= Al  "
c    param(11)= Si  "
c    param(12)= S   "
c    param(13)= Ar  "
c    param(14)= Ca  "
c    param(15)= Fe  "
c    param(16)= Ni  " 
c    param(17) = redshift used in the Mewe-Kaastra plasma model
c    param(18) = switch(0=calculate MEKAL model, 1=interpolate MEKAL model)


      REAL tarr(5), dem(5)


      INTEGER switch, i, nt, ierr, iopt

      CHARACTER contxt*127

      SAVE tarr, dem

      ierr = 0

      iopt = NINT(param(1))

      IF ( iopt .EQ. 1 .OR. iopt .EQ. 2 ) THEN
         nt = 4
      ELSEIF ( iopt .EQ. 3 ) THEN
         nt = 5
      ELSE
         CALL xwrite('Illegal value of option', 10)
         RETURN
      ENDIF

c Set the temperature and relative emissivity arrays

      IF ( iopt .EQ. 1 ) THEN
         tarr(1) = 5.6
         tarr(2) = 4.7
         tarr(3) = 3.7
         tarr(4) = 3.0
         dem(1)  = 0.18
         dem(2)  = 0.27
         dem(3)  = 0.42
         dem(4)  = 0.13
      ELSEIF ( iopt .EQ. 2 ) THEN
         tarr(1) = 5.6
         tarr(2) = 4.7
         tarr(3) = 3.7
         tarr(4) = 3.0
         dem(1)  = 0.10
         dem(2)  = 0.18
         dem(3)  = 0.22
         dem(4)  = 0.50
      ELSEIF ( iopt .EQ. 3 ) THEN
         tarr(1) = 5.6
         tarr(2) = 4.7
         tarr(3) = 3.7
         tarr(4) = 2.7
         tarr(5) = 2.0
         dem(1)  = 0.10
         dem(2)  = 0.18
         dem(3)  = 0.22
         dem(4)  = 0.30
         dem(5)  = 0.20
      ENDIF

      switch = NINT(param(18))

      CALL sumdem(2, switch, ear, ne, param(4), param(5), param(17),
     &            nt, tarr, dem, ifl, photar, ierr)
      contxt = 'MLVMKL: failure in SUMDEM'

 999  CONTINUE
      IF ( ierr .NE. 0 ) THEN
         CALL xwrite(contxt, 10)
         WRITE(contxt, '(a,i5)') 'Error number = ', ierr
         CALL xwrite(contxt, 10)
      ENDIF

      return
      end 












