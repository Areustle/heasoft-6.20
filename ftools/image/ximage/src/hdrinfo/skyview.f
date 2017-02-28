      SUBROUTINE SKYview(Cmdid, Status)
      IMPLICIT NONE 
c
c  SKYVIEW interface. Get images from SKYVIEW for a given sky survey
c  using the position and size of the current image read in ximage. 
c  It also lists the available surveys 
c
c  I   cmdid    (i)   Command id
c  O   status   (i)   Error flag (0=OK)
c
      INTEGER*4 Cmdid, Status

      include '../include/maxvals.inc'
      include '../include/io.inc'
c
c Local variables
c
      INTEGER*4 szx, szy, imgeqx
      INTEGER*4 lun, block
      LOGICAL isloaded
      REAL*8 xcen, ycen, ra, dec, pixel_size(2), dsize, dequinox
      CHARACTER*(MAX_IDSTR) mapid, wcsid
      CHARACTER*(MAX_FILELEN) fileout
      character(40) system, xlab, ylab
      character(80) eqxstr, szstr
      character(80) insurvey,survey,ds
      character(200) cmd, string
      LOGICAL list
c
      INTEGER*4 argc, lenact 
c
      mapid = 'CUR'
      fileout='skyview.fits'
      insurvey = 'Digitized Sky Survey'
      list = .false.

      status = 0
c
c Retrieve argument as output file
c 
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)
      if ( status.ne.0 ) return
c 
      CALL GPARS(Cmdid,'SURVEY_NAME',insurvey,Status)
      CALL GPARS(Cmdid,'FILE',fileout,Status)
      CALL GPARL(Cmdid,'LIST_SURVEYS',list,Status)
      if ( status.ne.0 ) return

      call qustrip(fileout)
c
c Give list spawing to webquery 
      if (list) then
         cmd = 'xwebquery.pl'
         string = 'host=skys.gsfc.nasa.gov url=/cgi-bin/showsurveys.pl' 
         cmd = cmd(:lenact(cmd))//' '//string(:lenact(string)) 
         call xwrite(string(:lenact(cmd)), 25)
         zwrite=' Skyview surveys list will appear on the screen' 
         call xwrite(zwrite, 10)
         call spawn(cmd,LENACT(cmd),status)
         If (status.eq.0) then 
             zwrite='  Spawning Webquery return not error'
             call xwrite(zwrite, 20)
         endif  
         status=0 
         return
      endif

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 5)
         status = -1
         return
      endif
c
c Build the string to retrieve the image
      call rmvchr(insurvey,'"')
      if ( insurvey.eq.' ' ) then
         call XWRITE(' No survey specified', 10)
         status = -1
         return
      endif
      zwrite='Input survey : '//insurvey(:lenact(insurvey))
      call xwrite (zwrite,20)
      survey = 'SURVEY="'//insurvey(:lenact(insurvey))//'"'
c
c set variable from header image
      call gheads(mapid, 'WCSID', wcsid, 0, status)
      if ( wcsid.eq.' ' ) then
         call xwarn(' Current map has no WCSID. ', 5)
         call xwrite(' Run "wcs upwcs" and retry', 5)
         status = -1
         return
      endif
      call wcsskyinfo(wcsid, system, xlab, ylab, dequinox)
      if ( xlab.ne.'RA' ) then
         call xwrite(' Skyview only available for RA/Dec images', 10)
         status = -1
         return
      endif
c
c  Determine image center
c
      call gheadi(mapid, 'SZX', szx, 0, status)
      call gheadi(mapid, 'SZY', szy, 0, status)
      xcen = dble(szx)/2.0 + 0.5
      ycen = dble(szy)/2.0 + 0.5
      imgeqx = dequinox
      call wcsimgsky(wcsid, xcen, ycen, ra, dec, imgeqx, 1, status)
c
c  Determine image size
c
      call gheadd(mapid, 'CDELT1', pixel_size(1), 0, status)
      call gheadd(mapid, 'CDELT2', pixel_size(2), 0, status)
      if (szx.gt.szy) then
         dsize = abs(pixel_size(1))*dble(szx)
      else
         dsize = abs(pixel_size(2))*dble(szy)
      endif
c 
c build the string      
      cmd = 'skvbatch.pl'
      string = ' '
c
c write RA and Dec in VCOORD
      write(string,2000)ra,dec
 2000 format ('VCOORD="',f9.5,',',f9.5,'"')
      call rmvchr(string,' ')
c
c write image Equinox in EQUINX
      write(eqxstr,'(i4)') imgeqx
      call rmvlbk(eqxstr)
      eqxstr = 'EQUINX='//eqxstr
c
c write size in SFACTR
      write(szstr,'(f9.5)')dsize
      call rmvlbk(szstr)
      szstr='SFACTR='//szstr
c
c complete the string for skyview script
      string = string(:lenact(string))//' '//
     &  survey(:lenact(survey))//' '//
     &  eqxstr(:lenact(eqxstr))//' '//
     &  szstr(:lenact(szstr))//
     &  ' file='//fileout(:lenact(fileout))
         cmd = cmd(:lenact(cmd))//' '//string(:lenact(string)) 
         call xwrite(cmd,20)
         zwrite=' Image from the survey: '//insurvey(:lenact(insurvey))
         call xwrite(zwrite, 10)
         zwrite=' '
         call xwrite(zwrite, 10)
         zwrite=' Querying Skyview server...'
         call xwrite(zwrite, 10)
         call spawn(cmd,LENACT(cmd),status)
         call getlun(lun)
         status = 0
         call ftopen(lun,fileout,0,block,status)
         if ( status.ne.0 ) then
            call XWRITE(
     &        ' ERROR: SKYVIEW failed to create valid FITS file',10)
            call XWRITE(
     &        '        Server may be down or survey name is invalid',10)
            call XWRITE(
     &        '        Check survey name against SKYVIEW/LIST',10)
         else
            call ftclos(lun,status)
            call frelun(lun)
            zwrite=' Skyview image written to file: '//
     &             fileout(:lenact(fileout))
            call xwrite(zwrite, 10)
         endif
      END
