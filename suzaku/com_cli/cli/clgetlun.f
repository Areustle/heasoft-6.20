C
C File: clgetlun.f
C Description: Get/Free/Mark logical unit number
C Author: Y. ISHISAKI
C
C History:
C     19-Feb-2005 Y.ISHISAKI, imported from dis45_getlun.f
C     20-Feb-2005 Y.ISHISAKI, include 'unitrd.inc' for LUNBASE
C     23-Feb-2005 Y.ISHISAKI, add CLmarklun()
C
C -----------
C   CLgetlun   ... get vacant logical unit number
C -----------
      Subroutine CLgetlun(lun)
      Implicit None
C output
      Integer lun
C common
      include 'clidef.inc'
      Include 'clunit.inc'
C const
      Integer LunS, LunE
C CLI itself use 81 - 88, see clunit.inc & clgetl.f
C               --> changed to use CLgetlun() in CLI itself
      Parameter ( LunS = LUNMIN, LunE = LUNBASE )
C local
      Integer Int_Lun
      Character*(LunE) Lun_Table
      Save Int_Lun, Lun_Table
      Data Int_Lun / LunE /
      Data Lun_Table / ' ' /
C begin
      lun = Int_Lun
      Do While ( Lun_Table(lun:lun) .ne. ' ' )
         lun = lun - 1
         If ( lun .lt. LunS ) lun = LunE
         If ( lun .eq. Int_Lun ) Then
            Call CLIerr(2, 'logical unit number table full')
            lun = -1
            Return
         End if
      End do
      Lun_Table(lun:lun) = 'x'
      Int_Lun = lun
C
ccc      Write (*,*) 'CLgetlun: lun=', lun
C
      Return
C
C ------------
C   CLfreelun   ... free logical unit number
C ------------
      Entry CLfreelun(lun)
C
C begin
      If ( (LunS .le. lun) .and. (lun .le. LunE) ) Then
         Lun_Table(lun:lun) = ' '
         If ( lun.gt.Int_Lun ) Then
            Int_Lun = lun
         End If
      End if
C
ccc      Write (*,*) 'CLfreelun: lun=', lun
C
      Return
C
C ------------
C   CLmarklun   ... mark logical unit number as being used
C ------------
      Entry CLmarklun(lun)
C
C begin
      If ( (LunS .le. lun) .and. (lun .le. LunE) ) Then
         Lun_Table(lun:lun) = 'x'
      End if
C
ccc      Write (*,*) 'CLmarklun: lun=', lun
C
      Return
      End
