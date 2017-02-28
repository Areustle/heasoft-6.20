
      SUBROUTINE w_refkeys(Lun, Status)

      IMPLICIT NONE

      INTEGER Lun, Status

c Routine to write out the REF* keywords which store the WCS information for the
c image coordinates. This is an invention of the XMM folks but seems like a useful
c innovation if we are writing an image in detector coordinates. Note that these
c values are all for the original image - no cropping or binning.

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'

      CALL FTUKYS(lun,'refxctyp',fctype(1),'projection',status)
      CALL FTUKYD(lun,'refxcrpx',fcrpix(1),15,
     &            'Original X axis reference pixel', status)
      CALL FTUKYD(lun,'refxcrvl',fcrval(1),15,
     &            'Original Coord of X ref pixel',status)
      CALL FTUKYD(lun,'refxcdlt',fcrdelt(1),15,
     &            'Original X axis increment',status)

      CALL FTUKYS(lun,'refyctyp',fctype(2),'projection',status)
      CALL FTUKYD(lun,'refycrpx',fcrpix(2),15,
     &            'Original Y axis reference pixel', status)
      CALL FTUKYD(lun,'refycrvl',fcrval(2),15,
     &            'Original Coord of Y ref pixel',status)
      CALL FTUKYD(lun,'refycdlt',fcrdelt(2),15,
     &            'Original Y axis increment',status)

      IF ( Status .NE. 0 ) CALL EXTERRSTACK

      RETURN
      END

