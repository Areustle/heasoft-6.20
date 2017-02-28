C
C File: version.f
C Description: Write version message of CLI
C Author: Y. ISHISAKI
C
C History:
C     22-Oct-1997 Y.ISHISAKI, created
C     27-Sep-2004 Y.ISHISAKI, version 1.91
C     23-Feb-2005 Y.ISHISAKI, version 2.00
C     05-Mar-2005 Y.ISHISAKI, version 2.01
C     28-May-2005 Y.ISHISAKI, version 2.02
C     21-Oct-2005 Y.ISHISAKI, version 2.03
C     11-Nov-2005 Y.ISHISAKI, version 2.04
C     04-Jun-2006 Y.ISHISAKI, version 2.05
C     01-Feb-2007 Y.ISHISAKI, version 2.06
C     27-Jul-2011 Y.ISHISAKI, version 2.07
C     16-May-2012 Y.ISHISAKI, version 2.08
C
C ----------
C   CLvers   ... Write version message
C ----------
      Subroutine CLvers
      Implicit NONE
c local
      Integer length
c data
      Character * 80  version
c function
      Integer Lenrd
c data
      Data version / 'CLI version 2.08 (last update: 16-May-2012)' /
c begin
      length = Lenrd(version)
      Write (*,*) version(:length)
c
      Return
      End
