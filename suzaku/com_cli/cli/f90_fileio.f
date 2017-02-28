C
C File: f90_fileio.f
C Description: File I/O routines for f90/f95
C
C History:
C     19-Jun-2006 Y.ISHISAKI,
C       Access -> Position='APPEND' in Clsyso()
C     02-Feb-2007 Y.ISHISAKI, add CLfilno, CLflush
C
C Public:
C     CLsyso, CLfgetc, CLfputc, CLfseek, CLftell, CLfnum, CLflush
C
      Subroutine Clsyso( Lun, File, Stat, status )
      Implicit None
C ... open stdin/stdout for redirection, or open a formatted file
C common
      include 'clidef.inc'
C input
      Integer  Lun
      Character * (*)  File, Stat
C output
      Integer status
C function
      Integer  cli__Fredirect, Lenrd
C local
      Integer  fh, fhin, fhout, Lfile
      Character * (LINSIZ)  refile
      Save  fhin, fhout, refile
      Data refile / ' ' /
C begin
      If( Lun.eq.5 .and.
     &    (File.eq.'SYS$INPUT' .or. File.eq.'/dev/stdin') ) Then
        Call cli__Frestore(0,fhin)
      Else If( Lun.eq.6 .and.
     &         (File.eq.'SYS$OUTPUT' .or. File.eq.'/dev/stdout') ) Then
        If( fhout.gt.0 ) Then
          Call cli__Fflush(1)
          Call cli__Frestore(1,fhout)
          fhout = 0
        End If
        If( Stat(1:1).ne.' ' ) Then
          Refile = ' '
        End If
      Else If( Lun.eq.6 .and.
     &         (File.eq.'NLA0:' .or. File.eq.'/dev/null') ) Then
        fhout = cli__Fredirect(1,'/dev/null','w')
      Else If( Lun.eq.6 ) Then
        If( File(1:1).ne.' ' ) Then
          Call cli__Fflush(1)
          refile = File
          Lfile = Lenrd(refile)
          If( Stat(1:1).eq.'A' .or. Stat(1:1).eq.'a' ) Then
            fh = cli__Fredirect(1,refile(:Lfile),'a')
          Else
            fh = cli__Fredirect(1,refile(:Lfile),'w')
          End If
          If( fh.gt.0 ) fhout = fh
        Else If( Refile(1:1).ne.' ' ) Then
          Call cli__Fflush(1)
          Lfile = Lenrd(refile)
          fh = cli__Fredirect(1,refile(:Lfile),'a')
          If( fh.gt.0 ) fhout = fh
        End If
      Else If( Lun.ne.6 ) Then
        If( stat(1:1).eq.'A' .or. stat.eq.'a' ) Then
          Open( Unit=Lun,File=File,Status='OLD',
     &          Position='APPEND',Err=900 )
        Else If( stat(1:1).eq.'W' .or. stat(1:1).eq.'w' ) Then
          Open( Unit=Lun,File=File,Status='NEW',
     &          Form='FORMATTED',Err=900 )
        Else
          Write(*,*) 'CLsyso: unknown mode: ', stat
          Goto 900
        End If
      Else
        Write(*,*) 'CLsyso: unknown device: ', File, ' for LUN ', Lun
        Goto 900
      End If
C
      status = 0
      Return
C
 900  Continue
      status = -1
      Return
      End
C
C
      Integer Function CLfgetc( Lun, C )
      Implicit None
C ... equivalent to Fgetc(), get one character from file, 0:success, -1:error
C input
      Integer  Lun
C output
      Character * 1  C
C function
      Integer  Fgetc
C begin
      CLfgetc = Fgetc( Lun, C )
C
      Return
      End
C
C
      Integer Function CLfputc( Lun, C )
      Implicit None
C ... equivalent to Fputc(), put one character from file, 0:success, -1:error
C input
      Integer  Lun
C output
      Character * 1  C
C function
      Integer  Fputc
C begin
      CLfputc = Fputc( Lun, C )
C
      Return
      End
C
C
      Subroutine CLfseek( Lun, Offset, Whence )
      Implicit None
C ... equivalent to Fseek(), set file position
C input
      Integer  Lun, Offset, Whence
C begin
      Call Fseek(Lun, Offset, Whence)
C
      Return
      End
C
C
      Integer Function CLftell( Lun )
      Implicit None
C ... equivalent to Ftell(), query file position
C input
      Integer  Lun
C function
      Integer  Ftell
C begin
      CLftell = Ftell( Lun )
C
      Return
      End
C
C
      Integer Function CLfnum( Lun )
      Implicit None
C ... equivalent to Fnum(), query file handle number
C input
      Integer  Lun
C function
ccc      Integer  Fnum
      Integer  Getfd
C begin
ccc   Fnum() not existent on Solaris f90 and Digital UNIX f77/f90
ccc      CLfnum = Fnum( Lun )
ccc      CLfnum = -1
      CLfnum = Getfd( Lun )
C
      Return
      End
C
C
      Subroutine CLflush( Lun )
      Implicit None
C ... equivalent to Flush(), built-in gfortran function/subroutine
C input
      Integer  Lun
C begin
      Call Flush( Lun )
C
      Return
      End