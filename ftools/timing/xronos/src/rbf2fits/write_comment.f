      SUBROUTINE write_comment(fits_unit, newrec1, newrec2, ftstat)
      include 'xrrbstr.h'
C
      character(3) itype(10)
      character(4) cyn(3), cwhat(3), cbg(4), cdt(3)
      character(12) inst(10)
      character(70) comm
      integer fits_unit, ftstat, iyr, iday, ihr, imn, isec
      REAL roll, savg, ssdev, rexp, norm
      REAL*8 scorr, bar1, bar2, reftfi, ra, dec, sra, sdec, tint

      DATA itype/' AR', ' XE', ' BO', ' GS', ' LE', 'SSS', 'MPC', '   ',
     &     '   ', ' A1'/
      DATA inst/'   EXOSAT L1', '   EXOSAT L2', '   EXOSAT ME',
     &          ' EXOSAT GSPC', 'Einstein SSS', 'Einstein MPC',
     &          '            ', '            ', '            ',
     &          '   HEAO-1 A1'/
c
      DATA cdt/'  no', ' yes', '  rc'/
      DATA cbg/'none', 'auto', 'file','yes'/
      DATA cwhat/'data', 'wind', 'expo'/
      DATA cyn/'  no', ' yes', 'head'/
c
      RECORD /rnewbuf_rec_1/newrec1
      RECORD /rnewbuf_rec_2/newrec2
      RECORD /rnewbuf_le/newle
      RECORD /rnewbuf_me/newme
      RECORD /rnewbuf_gs/newgs
c
      newme=newrec2.newme
      newgs=newrec2.newgs
      newle=newrec2.newle
c
c* Output infos in first record
c
c calculate start year before corrections of any kind
      CALL xrshfcon(newrec1.STARTSHF, iyr, iday, ihr, imn, isec)

      WRITE(comm,'(''Source ............'', A    
     &        ,t33,''Start Time ........'', 2I4,x,I2.2,2('':'',I2.2))')
     &        newrec1.SOURCE_NAME, iyr, iday, ihr, imn, isec
      CALL FTPCOM (fits_unit, comm, ftstat) 

c calculate stop year before corrections of any kind
      CALL xrshfcon(newrec1.ENDSHF, iyr, iday, ihr, imn, isec)
c start (tint in 2**-14, microsec or sec depending on value
c                of newrec1.flagunit)
      IF (newrec1.FLAGUNIT.EQ.0) tint = dble(newrec1.INTTIME)/16384.D0
      IF (newrec1.FLAGUNIT.EQ.1) tint = dble(newrec1.INTTIME)*1.D-6
      IF (newrec1.FLAGUNIT.EQ.2) tint = dble(newrec1.INTTIME)

      WRITE(comm,'(''Bin Time (s) ......'', G12.4
     &        ,t33,''Stop  Time ........'', 2I4,x,I2.2,2('':'',I2.2))')
     &        tint, iyr, iday, ihr, imn, isec
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''No. of Points .....'', I12  
     &        ,t33,''Experiment ........'', A)') 
     &        newrec1.NPT, inst(newrec1.EXPT)
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''No. of Recs .......'', I12
     &        ,t33,''Baryc. Times ......'', 8X, A)') 
     &        newrec1.NREC, cyn(newrec1.CORRECT+1)
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''Rate Buffer Type ..'', I12
     &        ,t33,''No. of (msec)Gaps .'', I12)') 
     &        newrec1.TYPE, newrec1.NGAP
      CALL FTPCOM (fits_unit, comm, ftstat) 

      savg = float(newrec1.AVERAGE)/1000.
      ssdev = float(newrec1.SDEV)/1000.

      WRITE(comm,'(''Average (c/s) .....'', F12.4
     &        ,t33,''Stand. Dev.(c/s) ..'', F12.4)') 
     &        savg,ssdev
      CALL FTPCOM (fits_unit, comm, ftstat) 

      sra = dble(newrec1.SOURCE_RA)/1.D9*360.D0
      sdec = dble(newrec1.SOURCE_DEC)/1.D9*90.D0
      ra = dble(newrec1.IRA)/1.D9*360.D0
      dec = dble(newrec1.IDEC)/1.D9*90.D0
      roll = float(newrec1.ROLL)/10.
c      write(*,*)newrec1.SHFCREATE
      CALL xrshfcon(newrec1.SHFCREATE, iyr, iday, ihr, imn, isec)

      WRITE(comm,'(''Source R.A.(deg) ..'', F12.6
     &        ,t33,''Source Dec.(deg) ..'', F12.6)') 
     &        sra, sdec
      CALL FTPCOM (fits_unit, comm, ftstat) 
      WRITE(comm,'(''Point. R.A.(deg) ..'', F12.6
     &        ,t33,''Point. Dec.(deg) ..'', F12.6)') 
     &        ra, dec
      CALL FTPCOM (fits_unit, comm, ftstat) 
      WRITE(comm,'(''Roll  Angle(deg) ..'', F12.2
     &        ,t33,''Creation Time .....'', 2I4,x,I2.2,2('':'',I2.2))')
     &        roll, iyr, iday, ihr, imn, isec
      CALL FTPCOM (fits_unit, comm, ftstat) 

      IF (newrec1.S_C_CORR.eq.0) THEN
         scorr = 0.D0
      ELSE
         scorr = 0.999999D0 + dble(newrec1.S_C_CORR)*1.D-10
      ENDIF

      WRITE(comm,'(''Slewing ...........'', 8X, A
     &        ,t33,''Concatenated ......'', 8X, A)') 
     &        cyn(newrec1.SLEW_FLAG+1), cyn(newrec1.FLAG_CON+1)
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''S/C Clock Corr.....'', F12.10
     &        ,t33,''Avg.Baryc.Corr.(s).'', F12.3 )') 
     &        scorr, float(newrec1.AVG_BARY)/10.
      CALL FTPCOM (fits_unit, comm, ftstat) 

      IF (newrec1.TYPE.eq.2) THEN
         WRITE(comm,'(''Point Rescal ......'', 6X, ''2**'', I3
     &           ,t33,''Error Rescal ......'', 6X, ''2**'', I3)') 
     &           newrec1.POWER1, newrec1.POWER2
         CALL FTPCOM (fits_unit, comm, ftstat) 
      ENDIF

      IF (newrec1.CORRECT.eq.2) THEN

         bar1 = dble(newrec1.MS_RFTM1)/1000.D0 + dble(newrec1.MMS_RFTM1)
     &       /1000.D3
         bar2 = dble(newrec1.MS_RFTM2)/1000.D0 + dble(newrec1.MMS_RFTM2)
     &       /1000.D3

         WRITE(comm,'(''Baryc.Corr.Start(s)'', F12.4)') bar1
         CALL FTPCOM (fits_unit, comm, ftstat) 
         WRITE(comm,'(''Baryc.Corr.Stop (s)'', F12.4)') bar2
         CALL FTPCOM (fits_unit, comm, ftstat) 

      ENDIF

      CALL xrshfcon(newrec1.UTCTIME1, iyr, iday, ihr, imn, isec)

      WRITE(comm,'(''S/C Clock Start ...'', I12
     &        ,t33,''H/K Time Start ....'', 2I4,x,I2.2,2('':'',I2.2))')
     &        newrec1.REFTIME1, iyr, iday, ihr, imn, isec
      CALL FTPCOM (fits_unit, comm, ftstat) 

      CALL xrshfcon(newrec1.UTCTIME2, iyr, iday, ihr, imn, isec)

      WRITE(comm,'(''S/C Clock Stop ....'', I12
     &        ,t33,''H/K Time Stop .....'', 2I4,x,I2.2,2('':'',I2.2))')
     &        newrec1.REFTIME2, iyr, iday, ihr, imn, isec
      CALL FTPCOM (fits_unit, comm, ftstat) 

c     Reftfi in 2**-14, microsec or sec depending on value
c     of newrec1.flagunit)(like tint)
      IF (newrec1.FLAGUNIT.EQ.0) reftfi = dble(newrec1.TIMEBIN)/16384.D0
      IF (newrec1.FLAGUNIT.EQ.1) reftfi = dble(newrec1.TIMEBIN)*1.D-6
      IF (newrec1.FLAGUNIT.EQ.2) reftfi = dble(newrec1.TIMEBIN)

      WRITE(comm,'(''1st Bin Shift(s) ..'', F12.5
     &        ,t33,''H/K micros Start ..'', I12)') 
     &        reftfi, newrec1.MICRO1
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''IEEE stand. r*4 ...'', 8X, A
     &        ,t33,''H/K micros Stop ...'', I12)') 
     &        cyn(newrec1.R4HP9000+1), newrec1.MICRO2
      CALL FTPCOM (fits_unit, comm, ftstat) 

c Second record: Branch for specific instruments.
      if   ((newrec1.EXPT + 1 .eq. 2).or.(newrec1.EXPT + 1 .eq. 3))then
         go to 100
      elseif(newrec1.EXPT + 1 .eq. 4) then
         go to 200
      elseif(newrec1.EXPT + 1 .eq. 5) then
         go to 300
      else
         go to 1000
      endif

100   continue
 
c LE specific comments.

      newle = newrec2.NEWLE

      WRITE (comm,'(''Instrument ........'', 9X, A
     &         ,t33,''Filter No. ........'', I12)') 
     &         itype(newle.INSTRUMENT), newle.NFW
      CALL FTPCOM (fits_unit, comm, ftstat) 

      norm = float(newle.NORM2)/3000.0*10.0**newle.NORM1
      rexp = rexp/norm

      WRITE (comm,'(''Min Bin Exposure ..'', F12.3
     &         ,t33,''Normalisation .....'', F12.3)') 
     &         float(newle.MIN_EXPO)/10000., norm
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''Avg Dead Time corr.'', F12.3
     &         ,t33,''Source X,Y ........'', 2I6)') 
     &         float(newle.DTFACT)/1000., newle.XSOURCE, newle.YSOURCE
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''Sample Dead Time ..'', 8X, A
     &         ,t33,''Source Radius .....'', I12)') 
     &         cdt(newle.DTFLAG+1),newle.RSOURCE
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''Telem. Dead Time ..'', 8X, A
     &         ,t33,''Bkgd In,Out Radii..'', 2I6)') 
     &         cyn(newle.TELFLAG+1),newle.RIBKGD,newle.ROBKGD
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''Bkgd Subtraction ..'', 8X, A
     &         ,t33,''Source/Bkgd area ..'', F12.3)') 
     &         cyn(newle.BGFLAG+1), float(newle.SB_RAT)/10000.
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''OBC Prog No. ......'', I12  
     &         ,t33,''Min Wait Time .....'', I12)') 
     &         newle.NPROG, newle.MINWAIT
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''Lcurv Prog Vers. ..'', I12  
     &         ,t33,''Bkgd Only .........'', 8X, A)') 
     &         newle.LCURVER, cyn(abs(newle.SBFLAG-2))
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''No. Source Boxes ..'', I12  
     &         ,t33,''Points Content ....'', 8X, A)') 
     &         newle.NBOX, cwhat(newle.PROFLAG+1)
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''No. Bkgd   Boxes ..'', I12  
     &         ,t33,''Bkgd Non-unif Corr.'', F12.3)') 
     &         newle.BGBOX, float(newle.NU_BKGD)/10000.
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''Srce Box 1 Low X,Y '', 2I6  
     &         ,t33,''High X,Y ..........'', 2I6)') 
     &          newle.X1LOW,newle.Y1LOW, newle.X1HIGH,newle.Y1HIGH
      CALL FTPCOM (fits_unit, comm, ftstat) 

      IF (newle.BGBOX.ge.1 ) THEN
         WRITE (comm,'(''Bkgd Box 1 Low X,Y '', 2I6  
     &            ,t33,''High X,Y ..........'', 2I6)') 
     &            newle.X1BLOW, newle.Y1BLOW, 
     &            newle.X1BHIGH, newle.Y1BHIGH
         CALL FTPCOM (fits_unit, comm, ftstat) 
      ENDIF

      IF (newle.NBOX.ge.2 ) THEN
         WRITE (comm,'(''Srce Box 2 Low X,Y '', 2I6  
     &            ,t33,''High X,Y ..........'', 2I6)') 
     &            newle.X2LOW, newle.Y2LOW,
     &            newle.X2HIGH,newle.Y2HIGH
         CALL FTPCOM (fits_unit, comm, ftstat) 
      ENDIF

      IF (newle.BGBOX.ge.2) THEN
         WRITE (comm,'(''Bkgd Box 2 Low X,Y '', 2I6
     &            ,t33,''High X,Y ..........'', 2I6)') 
     &            newle.X2BLOW, newle.Y2BLOW,
     &            newle.X2BHIGH,newle.Y2BHIGH
         CALL FTPCOM (fits_unit, comm, ftstat) 
      ENDIF

      IF (newle.NBOX.ge.3 ) THEN
         WRITE (comm,'(''Srce Box 3 Low X,Y '', 2I6
     &            ,t33,''High X,Y ..........'', 2I6)') 
     &            newle.X3LOW, newle.Y3LOW,
     &            newle.X3HIGH,newle.Y3HIGH
         CALL FTPCOM (fits_unit, comm, ftstat) 
      ENDIF

      IF (newle.BGBOX.ge.3) THEN
         WRITE (comm,'(''Bkgd Box 3 Low X,Y '', 2I6
     &            ,t33,''High X,Y ..........'', 2I6)') 
     &            newle.X3BLOW, newle.Y3BLOW,
     &            newle.X3BHIGH,newle.Y3BHIGH
         CALL FTPCOM (fits_unit, comm, ftstat) 
      ENDIF

      GOTO 1000

 200  CONTINUE

c ME specific comments.

      newme = newrec2.NEWME
c
      WRITE(comm,'(''Instrum., OBC rec..'', 4X, A, '' , '', A
     &        ,t33,''Detectors on Source'', 5X, I3, ''-'', I3)')
     &        itype(newme.INSTRUMENT), newme.REC_TYPE,
     &        newme.STARTDET, newme.STOPDET
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''PHA Bin range .....'', 5X, I3, ''-'', I3
     &        ,t33,''Detectors on Bkgd..'', 5X, I3, ''-'', I3)')
     &        newme.STARTBIN, newme.STOPBIN,
     &        newme.STARTBG, newme.STOPBG
      CALL FTPCOM (fits_unit, comm, ftstat) 

      rexp = rexp*float(newme.COLL_EFF)/10000.

      WRITE(comm,'(''Dead Time Corr.....'', 8X, A
     &        ,t33,''Avg Dead Time Corr.'', F12.3)') 
     &        cdt(newme.DTFLAG+1), float(newme.DTFACT)/1000.
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''Bkgd Subtraction ..'', 8X, A
     &        ,t33,''Additive Corr (c/s)'', F12.3)') 
     &        cbg(newme.BGFLAG+1), float(newme.LCFACT)/1000.
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''H/K Counts/s ......'', F12.2
     &        ,t33,''Collim. Efficiency.'', F12.3)') 
     &        float(newme.HKCOUNT)/100., float(newme.COLL_EFF)/10000.
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''Min Wait Time .....'', I12
     &        ,t33,''No. Source Detects.'', I12)') 
     &        newme.MINWAIT, newme.NDET
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE(comm,'(''PHA Channel Range .'', 5X, I3, ''-'', I3
     &        ,t33,''No. Bkgd   Detects.'', I12)') 
     &        newme.STARTCHAN, newme.ENDCHAN, newme.NBGDET
      CALL FTPCOM (fits_unit, comm, ftstat) 

      GOTO 1000

 300  CONTINUE

c GS specific comments.

      newgs = newrec2.NEWGS
c
      WRITE (comm,'(''Instrument ........'', 9X, A
     &         ,t33,''Channel range .....'', 5X, I3, ''-'', I3)') 
     &         itype(newgs.INSTRUMENT), newgs.STARTCHAN, newgs.STOPCHAN
      CALL FTPCOM (fits_unit, comm, ftstat) 

      rexp = rexp*float(newgs.COLL_EFF)/10000.

      WRITE (comm,'(''OBC Program No. ...'', I12
     &         ,t33,''Collim. Efficiency.'', F12.3)') 
     &         newgs.NPROG, float(newgs.COLL_EFF)/10000.
      CALL FTPCOM (fits_unit, comm, ftstat) 

      WRITE (comm,'(''Burst Length Min...'', I12
     &         ,t33,''Max................'', I12)') 
     &         newgs.BLMIN, newgs.BLMAX
      CALL FTPCOM (fits_unit, comm, ftstat) 

      GOTO 1000

1000  continue
      RETURN

      END
