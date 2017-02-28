C $Id: dis45_getlun.f,v 1.2 2005/02/23 06:59:49 ishisaki Exp $
C File: dis45_getlun.f
C Descripton: get/free logical unit number
C  Mon Jul 18 Y.Ishisaki
C
C History:
C     18-Jul-1998 Y.ISHISAKI, imported from dis45unix.f
C     19-Feb-2005 Y.ISHISAKI, call CLgetlun(), CLfreelun()
C
      Subroutine DIS45_getlun( lun )
      Implicit None
C output
      Integer  lun
C begin
      Call CLgetlun(lun)
C
      Return
      End
C
C
      Subroutine DIS45_freelun( lun )
      Implicit None
C input
      Integer  lun
C begin
      Call CLfreelun(lun)
C
      Return
      End
