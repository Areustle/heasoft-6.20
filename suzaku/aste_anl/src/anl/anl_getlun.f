C $Id: anl_getlun.f,v 1.2 2005/02/23 06:59:49 ishisaki Exp $
C File: dis45_getlun.f
C Descripton: get/free logical unit number, use DIS45_xx for dis45 user command
C  Mon Jul 18 Y.Ishisaki
C
C History:
C     18-Jul-1998 Y.ISHISAKI, call CLgetlun(), CLfreelun() in DIS45_xx
C
      Subroutine ANL_getlun(lun)
      Implicit None
c output
      Integer lun
c begin
      Call DIS45_getlun(lun)
c
      Return
      End
C
C
      Subroutine ANL_freelun(lun)
      Implicit None
c output
      Integer lun
c begin
      Call DIS45_freelun(lun)
c
      Return
      End
