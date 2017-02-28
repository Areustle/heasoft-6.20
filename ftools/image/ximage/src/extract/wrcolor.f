      subroutine wrcolor(filecolor, device, telstr,
     &                   field, chan, ratio, chi, mode, 
     &                   coldet, numsrc, status)
      implicit none
c
c Write color-color diagram
c
c  I  filecolor   (s)  Output file
c  I  device      (s)  PGPLOT device
c  I  telstr      (s)  Telescope id string
c  I  field       (s)  Observation description
c  I  chan        (i)  Energy channels
c  I  ratio       (i)  Channel ratios
c  I  chi         (r)  Chi-square
c  I  mode        (l)  0=Standard format 1=Extended format(SAX)
c  I  coldet      (l)  Detect sources if true, region if false
c  I  numsrc      (i)  Number of sources
c  O  status      (i)  Error flag (0=OK)
c
      include '../include/io.inc'
      include '../stat/detect.inc'

      character*(*) filecolor, device, telstr, field
      integer*4 chan(4), ratio(MAXDET,6), mode, numsrc, status
      real*4 chi(MAXDET)
      logical coldet
c
c  Local variables
c
      integer*4 ij, ii, lun, di, LENACT
      real*4 ratlow , rathigh , ratlerr , ratherr , totcerr , totcnt
      logical good
      character(100) ds
      
      ij = 1
      ii = 1

      CALL GETLUN(lun)
      CALL OPENWR(lun,filecolor,'unknown',' ',' ',0,0,status)
      IF ( status.NE.0 ) THEN
         WRITE (ZWRite,99005) status
         CALL XWRITE(ZWRite,5)
         CALL FRELUN(lun)
         RETURN
      ENDIF
c
c header output qdp file
c
      WRITE (lun,99006) field(:LENACT(field))
      WRITE (lun,99007) telstr(:LENACT(telstr)) 
      WRITE (lun,99018)
      WRITE (lun,99019)
      WRITE (lun,99020)
      WRITE (lun,99021)
      WRITE (lun,99022)
      WRITE (lun,99023)
      WRITE (lun,99024)
      WRITE (lun,99025)
      WRITE (lun,99026)
      WRITE (lun,99027)
c write qdp commands
      WRITE (lun,99030)
      WRITE (lun,99031)
      WRITE (lun,99032)
      WRITE (lun,99033) telstr(:LENACT(telstr))
      WRITE (lun,99034) field(:LENACT(field))
      WRITE (ZWRite,99002) chan(3) , chan(4) , chan(2) , chan(3)
      CALL RMVBLK(ZWRite)
      WRITE (lun,99003) ZWRite(:LENACT(ZWRite))
      WRITE (ZWRite,99002) chan(1) , chan(2) , chan(2) , chan(3)
      CALL RMVBLK(ZWRite)
      WRITE (lun,99004) ZWRite(:LENACT(ZWRite))
c
c  QDP doesn't support /xtk device, use /xw
c
      di = LENACT(Device)
      ds = Device(di-3:di)
      call upc(ds)
      if ( ds(1:4).eq.'/XTK' ) then
         WRITE (lun,99035) '/XW'
      else
         WRITE (lun,99035) Device(:LENACT(Device))
      endif
 
      WRITE (lun,99008)
      WRITE (lun,99009)
      WRITE (lun,99010)
      WRITE (lun,99011)
      WRITE (lun,99012)
      WRITE (lun,99013)
      WRITE (lun,99014)
      WRITE (lun,99015)
      WRITE (lun,99016)
      WRITE (lun,99017)
      WRITE (lun,99028)
      WRITE (lun,99029)
      WRITE (lun,99050)
      if ( mode.eq.1 ) then
         WRITE (lun,99051)
         WRITE (lun,99052)
         WRITE (lun,99053)
      endif
      
      if ( coldet ) then
         call xwrite('      i ratio:soft     err        hard      err  '
     &               //'   xpix     ypix     chi ', 10)
      else
         call xwrite('      i ratio:soft     err        hard      err  '
     &               //'   chi ', 10)
      endif
      DO WHILE ( ij.LE.Numsrc )
         good = .FALSE.
         if ( coldet .and. HOT(ij).EQ.1 ) good = .TRUE.
         if ( .not.coldet ) good = .TRUE.
         IF ( good ) THEN
            IF ( ratio(ij,2).GT.0.0 .AND. ratio(ij,1).GT.0.0 ) THEN
               ratlow = FLOAT(ratio(ij,1))/FLOAT(ratio(ij,2))
               ratlerr = ratlow*SQRT(1.0/ratio(ij,1)+1.0/ratio(ij,2))
            ELSE
               ratlow = 0.0
               ratlerr = 0.0
            ENDIF
            IF ( ratio(ij,3).GT.0 .AND. ratio(ij,2).GT.0 ) THEN
               rathigh = FLOAT(ratio(ij,3))/FLOAT(ratio(ij,2))
               ratherr = rathigh*SQRT(1.0/ratio(ij,2)+1.0/ratio(ij,3))
            ELSE
               rathigh = 0.0
               ratherr = 0.0
            ENDIF
            totcnt = ratio(ij,1) + ratio(ij,2) + ratio(ij,3)
            totcerr = SQRT(totcnt)
            if ( coldet ) then
               WRITE (ZWRite,99036) ii , ratlow , ratlerr , rathigh , 
     &                              ratherr , DTSox(ij) , DTSoy(ij), 
     &                              chi(ij)
            else
               WRITE (ZWRite,99046) ii , ratlow , ratlerr , rathigh , 
     &                              ratherr ,  chi(ij)
            endif
            CALL XWRITE(ZWRite,10)
            if ( mode.eq.0 ) then
               if ( coldet ) then
               WRITE (lun,99037) ii , ratlow , ratlerr , rathigh , 
     &                           ratherr , totcnt , totcerr , 
     &                           ratio(ij,1) , ratio(ij,2) , 
     &                           ratio(ij,3) , DTSox(ij) , DTSoy(ij),
     &                           chi(ij)
               else
               WRITE (lun,99047) ii , ratlow , ratlerr , rathigh , 
     &                           ratherr , totcnt , totcerr , 
     &                           ratio(ij,1) , ratio(ij,2) , 
     &                           ratio(ij,3) ,
     &                           chi(ij)
               endif
            else
               if ( coldet ) then
               WRITE (lun,99038) ii , ratlow , ratlerr , rathigh , 
     &                           ratherr , totcnt , totcerr , 
     &                           ratio(ij,1) , ratio(ij,2) , 
     &                           ratio(ij,3) , DTSox(ij) , DTSoy(ij),
     &                           chi(ij),  ratio(ij,4) , ratio(ij,5) , 
     &                           ratio(ij,6)
               else
               WRITE (lun,99048) ii , ratlow , ratlerr , rathigh , 
     &                           ratherr , totcnt , totcerr , 
     &                           ratio(ij,1) , ratio(ij,2) , 
     &                           ratio(ij,3) ,
     &                           chi(ij),  ratio(ij,4) , ratio(ij,5) , 
     &                           ratio(ij,6)
               endif
            endif
            ii = ii + 1
         ENDIF
         ij = ij + 1
      ENDDO
      CLOSE (lun)
      CALL FRELUN(lun)

      RETURN

99002 FORMAT (' (',i6,'-',i6,'/',i6,'-',i6,')')
99003 FORMAT ('la x Hardness Ratio ',a)
99004 FORMAT ('la y Softness Ratio ',a)
99005 FORMAT (' Error opening output color file',i3)
99006 FORMAT ('! Field Name :',1x,a)
99007 FORMAT ('! Instrument :',1x,a)
99008 FORMAT ('color off 1')
99009 FORMAT ('color off 4')
99010 FORMAT ('color off 5')
99011 FORMAT ('color off 6')
99012 FORMAT ('color off 7')
99013 FORMAT ('color off 8')
99014 FORMAT ('color off 9')
99015 FORMAT ('color off 10')
99016 FORMAT ('color off 11')
99017 FORMAT ('color off 12')
99018 FORMAT ('! column 1: detection id number')
99019 FORMAT ('! column 2: softness ratio 1 band1/band2')
99020 FORMAT ('! column 3: Error on ratio 1')
99021 FORMAT ('! column 4: hardness ratio 2 band3/band2')
99022 FORMAT ('! column 5: Error on ratio 2')
99023 FORMAT ('! column 6: Total counts')
99024 FORMAT ('! column 7: Error on Total counts')
99025 FORMAT ('! column 8: counts band1')
99026 FORMAT ('! column 9: counts band2')
99027 FORMAT ('! column 10: counts band3')
99028 FORMAT ('! column 11: x position')
99029 FORMAT ('! column 12: y position')
99050 FORMAT ('! column 13: chi')
99051 FORMAT ('! column 14: mean on channel for band1')
99052 FORMAT ('! column 15: mean on channel for band2')
99053 FORMAT ('! column 16: mean on channel for band3')
99030 FORMAT ('read serr 2 3 4')
99031 FORMAT ('xaxis 3')
99032 FORMAT ('yplot 2')
99033 FORMAT ('la ot ',a)
99034 FORMAT ('la t ',a)
99035 FORMAT ('dev ',a)
99036 FORMAT (' ',i6,2(1x,f10.3,f10.5),2(1x,f8.1),1x,f6.2)
99037 FORMAT (i6,6(1x,e10.4),3(1x,i9),2(1x,f10.2),1x,f7.3)
99038 FORMAT (i6,6(1x,e10.4),3(1x,i9),2(1x,f10.2),1x,f7.3,3(1x,i9))
99046 FORMAT (' ',i6,2(1x,f10.3,f10.5),1x,f6.2)
99047 FORMAT (i6,6(1x,e10.4),3(1x,i9),1x,f7.3)
99048 FORMAT (i6,6(1x,e10.4),3(1x,i9),1x,f7.3,3(1x,i9))
      END
