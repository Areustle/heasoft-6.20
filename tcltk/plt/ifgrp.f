      INTEGER FUNCTION IFGRP(IWIN, MXGRP, IWNUM)
      INTEGER   IWIN(*), MXGRP, IWNUM
C---
C Find the first group to be plotted in the current window
C---
      INTEGER   IGROUP
C---
      DO 190 IGROUP=1,MXGRP
         IF(IWIN(IGROUP).EQ.IWNUM) THEN
            IFGRP=IGROUP
            RETURN
         END IF
  190 CONTINUE
      IFGRP=0
      RETURN
      END
