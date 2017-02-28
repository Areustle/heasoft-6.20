      SUBROUTINE SHOW (Cmdid,Mapid,Version,Vpfile,
     &                 Vpnum,Vpset,Status)
      IMPLICIT NONE
c
c List information on the screen from the db mission file 
c display set-up
c
c  I  cmdid          (c)  Command id number
c  I  mapid          (c)  Map id string
c  I  version        (c)  Program version
c  I  vpfile         (c)  Viewport configuration file
c  I  vpnum          (i)  Number of viewport in configuration
c  I  vpset          (r)  Viewport specifications
c  O  status         (i)  Error flag (0=OK)
c
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/startup.inc'
      INCLUDE '../include/colordef.inc'

      CHARACTER*(*) Mapid, Version
      INTEGER*4 Vpnum
      CHARACTER*(*) Vpfile
      REAL*4 Vpset(4)
      INTEGER Cmdid, Status
c
c  Local variables
      INTEGER*4 iet,ist,iest,lenact,curvpnum,slen
      INTEGER*4 equinox, numlevs, maxlevs, icmin, icmax
      REAL*4 contrast, bright
      character(80) field , table_col
      character(80) file
      character(300) ds
      CHARACTER*(MAX_FILELEN) device
      LOGICAL there, isdisplay, isloaded
c
      INTEGER*4 argc
c
c decode the string
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)
      if ( status.ne.0 ) return

c get default values
      call tclresi("set default(equinox)", equinox, status)
      call tclress("set default(device)", device, MAX_FILELEN, status)
      call tclresi("set default(numlevs)", numlevs, status)
c
c read and write general information from internal header and from
c variables set in commons
c
      there = isloaded(mapid)
c
      if ( there ) then
         call gheads(mapid, 'OBJECT', field, 0, status)
         call gheads(mapid, 'FILE', file, 0, status)
      ELSE
         field = 'none'
         file = 'none'
      ENDIF
      WRITE (ZWRite,'(''  Version               : '',a)') 
     &                     Version(:LENACT(Version))
      CALL XWRITE(ZWRite,10)
      call WCSVERSION(ds)
      WRITE (ZWRite,'(''  WCS AST Version       : '',a)') 
     &                     ds(1:LENACT(ds))
      CALL XWRITE(ZWRite,15)
      WRITE (ZWRite,'(''  Plot device           : '',a)') 
     &                     device(:LENACT(device))
      CALL XWRITE(ZWRite,10)
      WRITE (ZWRite,'(''  Equinox               : '',i4)') equinox
      CALL XWRITE(ZWRite,10)
      WRITE (ZWRite,'(''  Current image         : '',a)') field
      CALL XWRITE(ZWRite,10)
      WRITE (ZWRite,'(''  Current file          : '',a)') file
      CALL XWRITE(ZWRite,10)
c
      call xistr(numlevs,ds,slen)
      WRITE (ZWRite,'(2a)')'  Number of levels      : ',
     &                          ds(:slen)
      CALL XWRITE(ZWRite,10)
      if ( isdisplay() ) then
         call pgqcol(icmin, icmax)
         maxlevs = icmax - icmin - 16
         call xistr(maxlevs,ds,slen)
         WRITE (ZWRite,'(2a)')'  Max. number of levels : ',
     &                             ds(:slen)
         CALL XWRITE(ZWRite,10)
      endif
      call info_coltab(CTAB_CURRENT,ZWRite,contrast,bright)
      CALL DIRPOS(ZWRite,ist,iest)
      IF ( iest.EQ.0 ) THEN
         table_col = ZWRite(ist+1:LENACT(ZWRite))
      ELSE
         table_col = ZWRite(iest+1:LENACT(ZWRite))
      ENDIF
      iet = INDEX(table_col,'.')
      IF ( iet.NE.0 ) table_col = table_col(:iet-1)
      if ( table_col.eq.' ' ) then
         ZWRite = ' '
      else
         WRITE (ds,99004) table_col(:LENACT(table_col)), 
     &                    bright, contrast
         CALL RMVXBK(ds)
      endif
      if ( isdisplay() ) then
         WRITE (ZWRite,'(2a)')'  Displayed color table : ',
     &                        ds(:LENACT(ds))
         CALL XWRITE(ZWRite,10)
      endif
      call info_coltab(CTAB_DEFAULT,ZWRite,contrast,bright)
      CALL DIRPOS(ZWRite,ist,iest)
      IF ( iest.EQ.0 ) THEN
         table_col = ZWRite(ist+1:LENACT(ZWRite))
      ELSE
         table_col = ZWRite(iest+1:LENACT(ZWRite))
      ENDIF
      iet = INDEX(table_col,'.')
      IF ( iet.NE.0 ) table_col = table_col(:iet-1)
      WRITE (ds, 99004) table_col(:LENACT(table_col)), bright, contrast
      CALL RMVXBK(ds)
      WRITE (ZWRite,'(2a)')'  Default color table   : ',
     &                     ds(:LENACT(ds))
      CALL XWRITE(ZWRite,10)
      call xistr(NULlcol,ds,slen)
      WRITE (ZWRite,'(''  Null pixel color      : '',a)') ds(:slen)
      CALL XWRITE(ZWRite,10)
      if ( vpfile.ne.' ' ) then
         CALL DIRPOS(vpfile,ist,iest)
         IF ( iest.EQ.0 ) THEN
            ZWRite = vpfile(ist+1:LENACT(vpfile))
         ELSE
            ZWRite = vpfile(iest+1:LENACT(vpfile))
         ENDIF
         iet = INDEX(ZWRite,'.')
         IF ( iet.NE.0 ) ZWRite = ZWRite(:iet-1)
         if ( Vpnum.gt.0 ) then
            curvpnum = 0
            call tclresi('pgtk::curvp', curvpnum, status)
            status = 0
            if ( curvpnum.gt.0 ) then
               write(ds,'(a,1x,a,i2,a,i2)') ZWRite(:LENACT(ZWRite)),
     &                    ' - Next: ', Vpnum,' Current: ', curvpnum
            else
               write(ds,'(a,1x,a,i2)') ZWRite(:LENACT(ZWRite)),
     &                    ' - Next: ', Vpnum
            endif
         endif
      elseif ( vpset(1).lt.0.0 ) then
         ds = 'Default'
      else
         write(ds,99005) Vpset(1),Vpset(2),Vpset(3),Vpset(4)
      endif
      call RMVXBK(ds)
      WRITE (ZWRite,'(''  Viewport              : '',a)')
     &              ds(:LENACT(ds))
      CALL XWRITE(ZWRite,10)
      IF ( isdisplay() ) THEN
         CALL XWRITE('  Display               : on',10)
      ELSE
         CALL XWRITE('  Display               : off',10)
      ENDIF
      status = 0
c
      RETURN
c
99004 FORMAT (a,'( Brightness = ',f6.1,', Contrast = ',f6.1,' )')
99005 FORMAT (f7.2,', ',f7.2,', ',f7.2,', ',f7.2)
      END
