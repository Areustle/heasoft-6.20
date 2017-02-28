
      SUBROUTINE rbenrg(npha, nbin, cgroup,
     &                  lower, hiher, middl, siseff, status)

      INTEGER npha, nbin, status
      character(1) cgroup(npha)
      REAL lower(npha), hiher(npha), middl(npha), siseff(npha)

c Routine to group the PHA bins

c Arguments :
c     npha    i      r: Actual size of PHA array
c     nbin    i      r: grouped size of PHA array
c     cgroup  i      r: grouping cards
c     lower   r      r: Lower energies
c     hiher   r      r: Upper energies
c     middl   r      r: Middle energies
c     siseff  r      r: SIS efficiency
c     status  i      r: Status  -   0 = OK

      INTEGER i, n, bin

      n = 0
      bin = 0
      status = 0

      DO i = 1, npha
         IF ( '-' .ne. cgroup(i) ) THEN
            IF ( bin .GT. 0 ) THEN
               siseff(n) = siseff(n) / bin
            ENDIF
            n = n + 1
            lower(n) = lower(i)
            bin = 1
            siseff(n) = siseff(i)
         ELSE
            bin = bin + 1
            siseff(n) = siseff(n) + siseff(i)
         ENDIF
         IF ( n .gt. 0 ) THEN
            hiher(n) = hiher(i)
            middl(n) = ( lower(n) + hiher(n) ) / 2.0
         ENDIF
      enddo

      IF ( n .NE. nbin ) status = 1

      RETURN
      END
