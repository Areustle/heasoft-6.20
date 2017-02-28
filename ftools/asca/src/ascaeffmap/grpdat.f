
      SUBROUTINE grpdat (data, nchan, cgroup, nchu)

      INTEGER nchan, nchu
      REAL data(nchan)
      character(1) cgroup(nchu)

c  Subroutine to group up the array data according to the grouping
c  card cgroup
c  Arguments :
c	data	R	i/r: data
c	nchan	I	   i: number of original data channels
c	cgroup	C	   i: target grouping card
c	nchu	I	   i: length of grouping card

      INTEGER ichu, ibin

c  If the number of original data channels is not the same as the
c  grouping card length then something is horribly wrong.

      IF ( nchan .NE. nchu ) THEN
         call fcecho (' GRPDAT: incompatible data and grouping card')
      ENDIF

      ibin = 0
      DO ichu = 1, nchu
         IF ( (cgroup(ichu) .EQ. '+') .OR.
     &        (cgroup(ichu) .EQ. '*') ) THEN
            ibin = ibin + 1
            data(ibin) = data(ichu)
         ELSE IF (cgroup(ichu) .EQ. '-') THEN
            data(ibin) = data(ibin) + data(ichu)
         ENDIF
      ENDDO

      END
