       subroutine nhinit(equinox, rastr, decstr, size, disio, map,
     &                   altmap, usemap, tchat, lchat, ierr)
c
c get parmater values for NH program 
c  O  equinox   (d)  equinox
c  O  rastr     (c)  R.A. string
c  O  decstr    (c)  Declination string
c  O  size      (d)  size of the submap
c  O  disio     (d)  distance in degree
c  O  map       (c)  input file map
c  O  altmap    (c)  alternate input map
c  O  usemap    (i)  which map to use
c  O  tchat     (i)  terminal chatness
c  O  lchat     (i)  log chatness
c  O  ierr      (i)  error status

       INTEGER*4 usemap, tchat, lchat, ierr
       DOUBLE PRECISION equinox, disio, size
       CHARACTER*(*) rastr, decstr, map, altmap
c
c Local     
       character(255) context
c
c Prompt user for EQUINOX 
       CALL uclgsd('equinox',equinox,ierr)
       IF(ierr.NE.0) THEN
         context = 'Error reading EQUINOX'
         GOTO 999
       ENDIF 
c
c Prompt user for RA 
       CALL uclgst('ra',rastr,ierr)
       IF(ierr.NE.0) THEN
         context = 'Error reading RA'
         GOTO 999
       ENDIF 
c
c Prompt user for DEC
       CALL uclgst('dec',decstr,ierr)
       IF(ierr.NE.0) THEN
         context = 'Error reading DEC' 
         GOTO 999
       ENDIF 
c
c Size of the submap in deg 
       CALL uclgsd('size',size,ierr)
       IF(ierr.NE.0) THEN
         context = 'Error reading SIZE'
         GOTO 999
       ENDIF 
c
c Distance in deg 
       CALL uclgsd('disio',disio,ierr)
       IF(ierr.NE.0) THEN
         context = 'Error reading DISIO'
         GOTO 999
       ENDIF 
c
c HI map location
       CALL uclgst('map',map,ierr)
       IF(ierr.NE.0) THEN
         context = 'Error reading input MAP'
         GOTO 999
       ENDIF 
c
c Alternate HI map location
       CALL uclgst('altmap',altmap,ierr)
       IF(ierr.NE.0) THEN
         context = 'Error reading input ALTMAP'
         GOTO 999
       ENDIF 
c
c Which HI map to use
       CALL uclgsi('usemap',usemap,ierr)
       IF(ierr.NE.0) THEN
         context = 'Could not get USEMAP parameter'
         GOTO 999
       ENDIF 
c
c terminal chatteness
       CALL uclgsi ('tchat', tchat, ierr)
       IF (ierr .NE. 0) THEN
           context = 'Could not get TCHAT parameter'
           GOTO 999
       ENDIF
c
c  log file chatteness
       CALL uclgsi ('lchat', lchat, ierr)
       IF (ierr .NE. 0) THEN
           context = 'Could not get LCHAT parameter'
           GOTO 999
       ENDIF

999    CONTINUE
       IF(ierr.NE.0)CALL xaerror(context,1)

       RETURN
       END
