C----------------------------------------------------------------------------
C This subroutine processes the input coords etc
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Oct 26 1998 (taken from code written by Jesse Allen)

      subroutine process_coords(equinox,rastr,decstr,rtd,ichat,
     &     ounit1,ounit2,start1,stop1,start2,stop2,dayfile,errstat)

      implicit none
      character*(*) rastr,decstr
      double precision equinox,rtd
      integer errstat,ichat,start1,stop1,start2,stop2
      integer ounit1,ounit2
      character*(*) dayfile        

C Local variables
      INTEGER bday , prevday , startfile , stopfile,equi
      INTEGER idmsflt(4) , ihmsf(4) , i , j , jtd , jtmax
      REAL rlong , rlat
      DOUBLE PRECISION td(5) , docsav , scana , doca , doc
      DOUBLE PRECISION rardb50 , decrdb50 , rardb78 , decrdb78
      DOUBLE PRECISION rardj20 , decrdj20 , liird , biird
      DOUBLE PRECISION liidg , biidg
      DOUBLE PRECISION elgrd78 , eltrd78 , elgdg78 , eltdg78
      DOUBLE PRECISION mjdref , sunlon , radeg , decdeg
      character(160) message
      character(1) sign

   
C Initialise variables

      errstat=0
C MJDREF for 1978.001
      mjdref=43509.0d0
C MJDREF for 2000.001
c      mjdref=51544.0d0
 
 
c NOTE equinox is real *8 from the par file but parsera, prec,tranc
c /xanlib/coords ask for an int*4, instead cgt want a real *8
c
c transform RA/DEC string => numerical values
 
      equi = INT(equinox)
      CALL PARSERA(rastr,equi,radeg,errstat)
      IF ( errstat.NE.0 ) THEN
         message = '  parsera: Error traslating RA string'
         CALL XAERROR(message,1)
         return

      ENDIF
      CALL PARSEDEC(decstr,equi,decdeg,errstat)
      IF ( errstat.NE.0 ) THEN
         message = '  parsdec: Error traslate DEC string'
         CALL XAERROR(message,1)
         return

      ENDIF
 
C Note the code is hardwired to assume input from parameters was in
C FK4.  However, the difference between FK4 and FK5 systems is well
C below the resolving power of the A2 instrument.
 
      rardb50 = radeg/rtd
      decrdb50 = decdeg/rtd
      IF ( equinox.NE.1950.0D0 ) CALL SLA_PRECES('FK4',equinox,1950.0D0,
     &     rardb50,decrdb50)
 
C Convert provided B1950.0 positions into a variety of coordinate systems,
C including ecliptic coordinates (needed for actual calculations).
 
      CALL SLA_EG50(rardb50,decrdb50,liird,biird)
      liidg = liird*rtd
      biidg = biird*rtd
      rardb78 = rardb50
      decrdb78 = decrdb50
      CALL SLA_PRECES('FK4',1950.0D0,1978.0D0,rardb78,decrdb78)
      CALL SLA_FK45Z(rardb50,decrdb50,1950.0D0,rardj20,decrdj20)
      CALL SLA_EQECL(rardj20,decrdj20,mjdref,elgrd78,eltrd78)
      elgdg78 = elgrd78*rtd
      eltdg78 = eltrd78*rtd
      rlong = REAL(rardb50)
      rlat = REAL(decrdb50)
      CALL SLA_CR2TF(1,rlong,sign,ihmsf)
      IF ( ihmsf(1).EQ.24 ) THEN
         ihmsf(1) = 0
         ihmsf(2) = 0
         ihmsf(3) = 0
         ihmsf(4) = 0
      ENDIF
      CALL SLA_CR2AF(1,rlat,sign,idmsflt)
      IF ( sign.EQ.'+' ) THEN
         WRITE (message,99001) 'RA      DEC    B1950  ' , ihmsf(1) , 
     &                         ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                         idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                         idmsflt(4)
         WRITE (ounit1,99002) '# RA      DEC    B1950  ' , ihmsf(1) , 
     &                        ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                        idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                        idmsflt(4)
         WRITE (ounit2,99002) '# RA      DEC    B1950  ' , ihmsf(1) , 
     &                        ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                        idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                        idmsflt(4)
 
      ELSE
         WRITE (message,99003) 'RA      DEC    B1950  ' , ihmsf(1) , 
     &                         ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                         idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                         idmsflt(4)
         WRITE (ounit1,99004) '# RA      DEC    B1950  ' , ihmsf(1) , 
     &                        ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                        idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                        idmsflt(4)
         WRITE (ounit2,99004) '# RA      DEC    B1950  ' , ihmsf(1) , 
     &                        ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                        idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                        idmsflt(4)
 
 
      ENDIF
      CALL XWRITE(message,ichat)
      rlong = REAL(rardb78)
      rlat = REAL(decrdb78)
      CALL SLA_CR2TF(1,rlong,sign,ihmsf)
      IF ( ihmsf(1).EQ.24 ) THEN
         ihmsf(1) = 0
         ihmsf(2) = 0
         ihmsf(3) = 0
         ihmsf(4) = 0
      ENDIF
      CALL SLA_CR2AF(1,rlat,sign,idmsflt)
      IF ( sign.EQ.'+' ) THEN
         WRITE (message,99001) 'RA      DEC    B1978.0' , ihmsf(1) , 
     &                         ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                         idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                         idmsflt(4)
 
 
      ELSE
         WRITE (message,99003) 'RA      DEC    B1978.0' , ihmsf(1) , 
     &                         ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                         idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                         idmsflt(4)
 
 
      ENDIF
      CALL XWRITE(message,ichat)
      rlong = REAL(rardj20)
      rlat = REAL(decrdj20)
      CALL SLA_CR2TF(1,rlong,sign,ihmsf)
      IF ( ihmsf(1).EQ.24 ) THEN
         ihmsf(1) = 0
         ihmsf(2) = 0
         ihmsf(3) = 0
         ihmsf(4) = 0
      ENDIF
      CALL SLA_CR2AF(1,rlat,sign,idmsflt)
      IF ( idmsflt(1).EQ.360 ) THEN
         idmsflt(1) = 0
         idmsflt(2) = 0
         idmsflt(3) = 0
         idmsflt(4) = 0
      ENDIF
      IF ( sign.EQ.'+' ) THEN
         WRITE (message,99001) 'RA      DEC    J2000  ' , ihmsf(1) , 
     &                         ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                         idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                         idmsflt(4)
      ELSE
         WRITE (message,99003) 'RA      DEC    J2000  ' , ihmsf(1) , 
     &                         ihmsf(2) , ihmsf(3) , ihmsf(4) , 
     &                         idmsflt(1) , idmsflt(2) , idmsflt(3) , 
     &                         idmsflt(4)
      ENDIF
      CALL XWRITE(message,ichat)
      WRITE (message,99005) 'L(II)   B(II)         ' , liidg , biidg
      CALL XWRITE(message,ichat)
      WRITE (message,99005) 'EC.LNG  EL.LAT  1978.0' , elgdg78 , eltdg78
      CALL XWRITE(message,ichat)
 
      td(1) = 1977.0
      td(2) = 230.0
      td(3) = 0.0
      td(4) = 0.0
      td(5) = 0.0
      prevday = 0
      DO 100 j = 1 , 509
         docsav = 3.0
         td(2) = td(2) + 1.
         jtd = INT(td(2))
         CALL EPHEM2(td,sunlon)
         CALL DEGOFF(sunlon,elgdg78,eltdg78,scana,doc)
         doca = ABS(doc)
         IF ( doca.LE.docsav ) THEN
            docsav = doca
            jtmax = jtd
            IF ( prevday.EQ.0 ) THEN
               bday = jtmax
               prevday = jtmax - 1
            ENDIF
            IF ( (jtmax-prevday).NE.1 ) THEN
               CALL DAYTOFILE(bday,prevday,startfile,stopfile,dayfile)
               DO 10 i = bday , prevday
                  IF ( i.GT.730 ) THEN
                     WRITE (message,'(''Year: 1979  Day: '', i3)')
     &                      i - 730
                  ELSEIF ( i.GT.365 ) THEN
                     WRITE (message,'(''Year: 1978  Day: '', i3)')
     &                      i - 365
                  ELSE
                     WRITE (message,'(''Year: 1977  Day: '', i3)') i
                  ENDIF
                  CALL XWRITE(message,12)
 10            CONTINUE
               message = 
     &                  '       MED  1.28s          MED  5.12s         '
     &                  //'HEDs  1.28s         HEDs  5.12s'
               CALL XWRITE(message,15)
 
               DO 20 i = startfile , stopfile
                  WRITE (message,
     &'(''a2_xrate'',i4.4,''med_128 a2_xrate'',             i4.4,''med_5
     &12 a2_xrate'',i4.4,''hed_128 a2_xrate'',              i4.4,''hed_5
     &12'')') i , i , i , i
                  CALL XWRITE(message,15)
 20            CONTINUE
 
               start1 = startfile
               stop1 = stopfile
 
 
 
 
               bday = jtmax
            ENDIF
            prevday = jtmax
         ENDIF
 100  CONTINUE
      DO 200 i = bday , prevday
         IF ( i.GT.730 ) THEN
            WRITE (message,'(''Year: 1979  Day: '', i3)') i - 730
         ELSEIF ( i.GT.365 ) THEN
            WRITE (message,'(''Year: 1978  Day: '', i3)') i - 365
         ELSE
            WRITE (message,'(''Year: 1977  Day: '', i3)') i
         ENDIF
         CALL XWRITE(message,15)
 200  CONTINUE
      message = '       MED  1.28s          MED  5.12s         '//
     &          'HEDs  1.28s         HEDs  5.12s'
      CALL XWRITE(message,15)
      CALL DAYTOFILE(bday,prevday,startfile,stopfile,dayfile)
      DO 300 i = startfile , stopfile
         WRITE (message,
     &'(''a2_xrate'',i4.4,''med_128 a2_xrate'',             i4.4,''med_5
     &12 a2_xrate'',i4.4,''hed_128 a2_xrate'',              i4.4,''hed_5
     &12'')') i , i , i , i
          CALL XWRITE(message,15)
 300  CONTINUE
 
      start2 = startfile
      stop2 = stopfile




99001 FORMAT (10X,A22,5X,2(I2.2,1X),I2.2,'.',I1,3X,2(I2.2,1X),I2.2,'.',
     &        I1)
99002 FORMAT (A24,5X,2(I2.2,1X),I2.2,'.',I1,3X,2(I2.2,1X),I2.2,'.',I1)
99003 FORMAT (10X,A22,5X,2(I2.2,1X),I2.2,'.',I1,2X,'-',2(I2.2,1X),I2.2,
     &        '.',I1)
99004 FORMAT (A24,5X,2(I2.2,1X),I2.2,'.',I1,2X,'-',2(I2.2,1X),I2.2,'.',
     &        I1)
99005 FORMAT (10X,A22,5X,2F10.3)
 
 
      return

      end


