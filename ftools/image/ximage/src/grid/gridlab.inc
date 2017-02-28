c
c  Common for properties of current grid line
c
c   labval   (d)  Number to use in label
c   axisnum  (i)  Axis number (e.g. 1 corresponds to CRVAL1)
c   coorsys  (s)  Coordinant system (GALactic, CELestial)
c
      real*8 labval
      integer axisnum
      character(3) coorsys

      common /labcom/ labval, axisnum, coorsys
