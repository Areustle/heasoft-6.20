
      logical function GOODWCS(record)
C
C  copy image World Coordinate System keywords
C       1/23/95 EAG 1.0
C
C     CRVAL
C     CRPIX
C     CDELT
C     CTYPE
C     CUNIT
C     OPTIC
C     CROTA
C
      character(80) record
      logical copy

      copy = (index(record(1:5),'CRVAL') .gt. 0)
      copy = ((copy) .or. (index(record(1:5),'CRPIX') .gt. 0))
      copy = ((copy) .or. (index(record(1:5),'CDELT') .gt. 0))
      copy = ((copy) .or. (index(record(1:5),'CTYPE') .gt. 0))
      copy = ((copy) .or. (index(record(1:5),'CUNIT') .gt. 0))
      copy = ((copy) .or. (index(record(1:5),'CROTA') .gt. 0))
      copy = ((copy) .or. (index(record(1:5),'OPTIC') .gt. 0))

      GOODWCS = copy
      return
      end
