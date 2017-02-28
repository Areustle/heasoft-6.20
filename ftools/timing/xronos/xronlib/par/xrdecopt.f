c
      SUBROUTINE xrdecopt( copt, iopt, mopt, dopt, status)

c
c        ls  5/7/88  to decode file options
c Rev.1  ls 14/10/91 "VEn" option added 
c Rev.2  ls 21/11/91 "VEn" with n<0 added to indicate that no exposure vector 
c                     is specified even if there are more than 2 vectors in 
c                     infile
c Rev.3  el 10/06/94 "VE0" is now used to turn off GTIs for FITS event lists
c
c     I   lut,lul = lu of terminal and log
c     I   lcd = cd_xronos chattiness
c     I   lch = terminal chattiness
c     I   copt = array of options to be decoded
c     O   iopt = array of flag options
c     O   mopt = array of math options
c     O   dopt = array of constants for math options
c     O   status = xronos error code
c     O   * = conditional return if error is found
c
c  Options consist of 2 chars. and in some cases a numerical value up to
c  8 digits. The maximum no. of chars in one option is 10.
c
c  Note that the array of options copt*10(10) is decoded in
c  a) flag options iopt(10) (0= no= df, 1=yes)
c  b) matemathical options mopt(10) and constants dopt(10) to be executed
c     In the same order as the input string
c
c  List of supported options:
c
c  BT    => iopt(1) = 1    ; apply Barycentric Time correction (rate buff)
c  FEn   => iopt(1) = n    ; start loading counts at energy channel n in
c                            the FITS y-vector.
c  FRn   => iopt(2) = n    ; start reading infile from record n (First Rec)(rb)
c                          ;                     (from row n for FITS)
c  LRn   => iopt(3) = n    ; stop reading infile at record n (Last Rec)(rb)
c                          ;                     (at row n for FITS)
c  DN    => iopt(4) = 1    ; Not to do what is described below (which is done
c                            by d/f): (ME rate buff)
c                            apply Dead Time correction to counts and variance
c                            (to variance only if it is already applied to the
c                             counts) (for ME only) (this if all the relevant
c                            info is in the header otherwise see 'DV')
c                            (option 'MA' can be used to apply a dead time
c                            correction factor to the counts)
c  LEn   => iopt(4) = n    ; stop loading counts at energy channel n in
c                            the FITS y-vector.
c  VXn   => iopt(5) = n    ; use vector n as X-axis (time) (for qdp or FITS)
c                            (a neg. value means stepped X-axis according to
c                             file header line "!!H")
c  VYn   => iopt(6) = n    ; use vector n as Y-axis (for qdp and FITS infiles)
c  FSn   => iopt(7) = n    ; Find Subfile n (for z-files)
c
c  VCn   => iopt(7) = n    ; use column n for Energy Channel in FITS infiles
c
c  VEn   => iopt(8) = n    ; use vector n as Exposure-axis (for qdp)
c                                       or as Dead Time (FITS)
c  VE0   => iopt(8) = -99  ; turn off GTIs (FITS).
c  VSn   => iopt(9) = n    ; use vector n as Y-error-axis (for qdp and FITS)
c  RTn   => iopt(10)= n    ; use extension n for the rate table.
c                            (where the first extension is
c                            n = 1 and the primary array is irrelevant)
c  OF    => iopt(11) =1    ; set the doffset =0 no MJDREF 
c
c  MUx   => mopt(nma) = 10 ; multiply data and errors by x (MUltiply)
c  MDx   => mopt(nma) = 11 ; multiply data by x (Multiply Data)
c  MEx   => mopt(nma) = 12 ; multiply errors by x (Multiply Errors)
c  MAx   => mopt(nma) = 13 ; as MU but divide exposure by x (Multiply All)
c                            (used to have possib. to reconstruct photon stat)
c  DIx   => mopt(nma) = 20 ; divide data and errors by x (DIvide)
c  DDx   => mopt(nma) = 21 ; divide data by x (Divide Data)
c  DEx   => mopt(nma) = 22 ; divide  errors by x (Divide  Errors)
c  DAx   => mopt(nma) = 23 ; as DI but multiply exposure by x (Divide All)
c                            (used to have possib. to reconstruct photon stat)
c  AAx   => mopt(nma) = 30 ; add data and errors with x (Add All)
c  ADx   => mopt(nma) = 31 ; add data with x (Add Data)
c  AEx   => mopt(nma) = 32 ; add errors with x (Add Errors)
c  SAx   => mopt(nma) = 40 ; subtract data and errors with x (Subtract All)
c  SDx   => mopt(nma) = 41 ; subtract  data with x (Subtract Data)
c  SEx   => mopt(nma) = 42 ; subtract  errors with x (Subtract Errors)
c  QAx   => mopt(nma) = 33 ; add to data the square of data multiplied by x
c                            and sum to errors the product of data and error
c                            multiplied by x. This is used to multiply data
c                            and errors by a linear interpolation constant
c                            y'=y(a+by)=ya+byaya/aa=ya+cyaya with c=b/aa
c                            sy'=sy(a+by)=sya+byasya/aa=sya+cyasya
c                            (sy = error). This can be done by using options:
c                            MUa QAc (where a and c are those used above).
c  QDx   => mopt(nma) = 34 ; as above but for data only
c  QEx   => mopt(nma) = 35 ; as above but for errors only
c
c  STx   => mopt(nma) = 50 ; shift times by x days (Shift Times)
c  SSx   => mopt(nma) = 51 ; shift times by x seconds (Shift Seconds)
c
c  DVx   => mopt(nma) = 60 ; apply Dead Time correction to variance
c                            by providing avg. count rate BEFORE
c                            dead time correction x, or AFTER dead time
c                            correction -x (i.e. negative) (for ME rate buff)
c
c  xrdecopco = subroutine used
c
      implicit none
      include '../include/io.inc'
      include '../include/xronos.inc'

      INTEGER iv, nopt, 
     &           k, nma
      include '../include/xronos_init.inc'
      parameter (subname = 'xrdecopt:')

c
c set default values
      
      if(status.ne.0) return

      DO k = 1, 15
         iopt(k) = 0
         mopt(k) = 0
         dopt(k) = 0.D0
      ENDDO
      nma = 0
c
c loop for 10 options
c
      do 10 nopt=1,maxopts
c return condition
      IF (copt(nopt).EQ.' ') THEN
         GOTO 100
      ENDIF
         write(context,'(''Processing: '',1x,a)')copt(nopt)
         call xwrite(context,20)
c barycentric time correction option   'BT'
      IF (copt(nopt)(1:3).EQ.'BT ' .OR. copt(nopt)(1:3).EQ.'bt ') THEN
         iopt(1) = 1
c FITS option: start reading from energy bin n option 'FE'
      ELSEIF (copt(nopt)(1:2).EQ.'FE' .OR. copt(nopt)(1:2).EQ.'fe') THEN
         CALL xrdecopco(copt(nopt), iopt(1), dv, 1,
     &                  status)
         IF (iopt(1).LT.0) status = 1017
c FITS option: stop reading at energy bin n option 'LE'
      ELSEIF (copt(nopt)(1:2).EQ.'LE' .OR. copt(nopt)(1:2).EQ.'le') THEN
         CALL xrdecopco( copt(nopt), iopt(4), dv, 1,
     &                  status)
         IF (iopt(4).LE.0) status = 1017
c start reading from rec n option  'FR'
      ELSEIF (copt(nopt)(1:2).EQ.'FR' .OR. copt(nopt)(1:2).EQ.'fr') THEN
         CALL xrdecopco( copt(nopt), iopt(2), dv, 1,
     &                  status)
         IF (iopt(2).LE.0) status = 1017
c stop reading at rec n option 'LR'
      ELSEIF (copt(nopt)(1:2).EQ.'LR' .OR. copt(nopt)(1:2).EQ.'lr') THEN
         CALL xrdecopco( copt(nopt), iopt(3), dv, 1,
     &                  status)
         IF (iopt(3).LE.0) status = 1017
         IF (iopt(3).LT.iopt(2)) status = 1017
c no dead time correction option 'DN'
      ELSEIF (copt(nopt)(1:3).EQ.'DN ' .OR. copt(nopt)(1:3).EQ.'dn ')
     &         THEN
         iopt(4) = 1
c use vector n as x-axis (for qdp infiles): option  'VX'
      ELSEIF (copt(nopt)(1:2).EQ.'VX' .OR. copt(nopt)(1:2).EQ.'vx') THEN
         CALL xrdecopco( copt(nopt), iopt(5), dv, 1,
     &                  status)
         IF (iopt(5).EQ.0) status = 1017
c use vector n as y-axis (for qdp infiles): option  'VY'
      ELSEIF (copt(nopt)(1:2).EQ.'VY' .OR. copt(nopt)(1:2).EQ.'vy') THEN
         CALL xrdecopco( copt(nopt), iopt(6), dv, 1,
     &                  status)
         IF (iopt(6).LE.0) status = 1017
c use vector n as y-error (for qdp infiles): option  'VS'
      ELSEIF (copt(nopt)(1:2).EQ.'VS' .OR. copt(nopt)(1:2).EQ.'vs') THEN
         CALL xrdecopco( copt(nopt), iopt(9), dv, 1,
     &                  status)
         IF (iopt(9).LE.0) status = 1017
c find subfile n option (for Z-files): option  'FS'
      ELSEIF (copt(nopt)(1:2).EQ.'FS' .OR. copt(nopt)(1:2).EQ.'fs') THEN
         CALL xrdecopco( copt(nopt), iopt(7), dv, 1,
     &                  status)
         IF (iopt(7).LE.0) status = 1017
c Use column n for energy channel in FITS infiles: option  'VC'
      ELSEIF (copt(nopt)(1:2).EQ.'VC' .OR. copt(nopt)(1:2).EQ.'vc') THEN
         CALL xrdecopco( copt(nopt), iopt(7), dv, 1,
     &                  status)
         IF (iopt(7).LT.0) status = 1017
         IF (iopt(7).EQ.0) iopt(7)=-99
c use vector n as exposure-axis (for qdp infiles): option  'VE'   !Rev.1 start
      ELSEIF (copt(nopt)(1:2).EQ.'VE' .OR. copt(nopt)(1:2).EQ.'ve') THEN
         CALL xrdecopco( copt(nopt), iopt(8), dv, 1,
     &                  status)
c         IF (iopt(8).LE.0) status = 1017      !Rev.2 
c  Rev.2 : accept also negative values
c  Rev.3 : to turn off gtis
         IF (iopt(8).EQ.0) iopt(8) = -99
c FITS option: use data extension # n for the rate table
      ELSEIF (copt(nopt)(1:2).EQ.'RT' .OR. copt(nopt)(1:2).EQ.'rt') THEN
         CALL xrdecopco( copt(nopt), iopt(10), dv, 1,
     &                  status)
         IF (iopt(10).EQ.0) status = 1017
c 
c No MJDREF included in the offset  
      ELSEIF (copt(nopt)(1:3).EQ.'OF ' .OR. copt(nopt)(1:3).EQ.'of ')
     &         THEN
         iopt(11) = 1
c
c multiply data and error option 'MU'
      ELSEIF (copt(nopt)(1:2).EQ.'MU' .OR. copt(nopt)(1:2).EQ.'mu') THEN
         nma = nma + 1
         mopt(nma) = 10
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c multiply data option 'MD'
      ELSEIF (copt(nopt)(1:2).EQ.'MD' .OR. copt(nopt)(1:2).EQ.'md') THEN
         nma = nma + 1
         mopt(nma) = 11
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                   2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c multiply error option 'ME'
      ELSEIF (copt(nopt)(1:2).EQ.'ME' .OR. copt(nopt)(1:2).EQ.'me') THEN
         nma = nma + 1
         mopt(nma) = 12
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c multiply all option 'MA'
      ELSEIF (copt(nopt)(1:2).EQ.'MA' .OR. copt(nopt)(1:2).EQ.'ma') THEN
         nma = nma + 1
         mopt(nma) = 13
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c
c
c divide data and error option 'DI'
      ELSEIF (copt(nopt)(1:2).EQ.'DI' .OR. copt(nopt)(1:2).EQ.'di') THEN
         nma = nma + 1
         mopt(nma) = 20
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c divide data option 'DD'
      ELSEIF (copt(nopt)(1:2).EQ.'DD' .OR. copt(nopt)(1:2).EQ.'dd') THEN
         nma = nma + 1
         mopt(nma) = 21
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c divide error option 'DE'
      ELSEIF (copt(nopt)(1:2).EQ.'DE' .OR. copt(nopt)(1:2).EQ.'de') THEN
         nma = nma + 1
         mopt(nma) = 22
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c divide all option 'DA'
      ELSEIF (copt(nopt)(1:2).EQ.'DA' .OR. copt(nopt)(1:2).EQ.'da') THEN
         nma = nma + 1
         mopt(nma) = 23
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c
c add data and error option 'AA'
      ELSEIF (copt(nopt)(1:2).EQ.'AA' .OR. copt(nopt)(1:2).EQ.'aa') THEN
         nma = nma + 1
         mopt(nma) = 30
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c add data option 'AD'
      ELSEIF (copt(nopt)(1:2).EQ.'AD' .OR. copt(nopt)(1:2).EQ.'ad') THEN
         nma = nma + 1
         mopt(nma) = 31
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c add error option 'AE'
      ELSEIF (copt(nopt)(1:2).EQ.'AE' .OR. copt(nopt)(1:2).EQ.'ae') THEN
         nma = nma + 1
         mopt(nma) = 32
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c
c
c subtract data and error option 'SA'
      ELSEIF (copt(nopt)(1:2).EQ.'SA' .OR. copt(nopt)(1:2).EQ.'sa') THEN
         nma = nma + 1
         mopt(nma) = 40
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c subtract  data option 'SD'
      ELSEIF (copt(nopt)(1:2).EQ.'SD' .OR. copt(nopt)(1:2).EQ.'sd') THEN
         nma = nma + 1
         mopt(nma) = 41
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c subtract  error option 'SE'
      ELSEIF (copt(nopt)(1:2).EQ.'SE' .OR. copt(nopt)(1:2).EQ.'se') THEN
         nma = nma + 1
         mopt(nma) = 42
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c
c add to data the square od data multiplied by x and sum to errors the product
c  of data and error multiplied by x (QA =quadratic all)
      ELSEIF (copt(nopt)(1:2).EQ.'QA' .OR. copt(nopt)(1:2).EQ.'qa') THEN
         nma = nma + 1
         mopt(nma) = 33
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c as above but for data only (QD= quadratic data)
      ELSEIF (copt(nopt)(1:2).EQ.'QD' .OR. copt(nopt)(1:2).EQ.'qd') THEN
         nma = nma + 1
         mopt(nma) = 34
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c as above but for errors only (QE= quadratic errors)
      ELSEIF (copt(nopt)(1:2).EQ.'QE' .OR. copt(nopt)(1:2).EQ.'qe') THEN
         nma = nma + 1
         mopt(nma) = 35
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c
c
c shift time option 'ST' (in days)
      ELSEIF (copt(nopt)(1:2).EQ.'ST' .OR. copt(nopt)(1:2).EQ.'st') THEN
         nma = nma + 1
         mopt(nma) = 50
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c shift time option 'SS' (in secs)
      ELSEIF (copt(nopt)(1:2).EQ.'SS' .OR. copt(nopt)(1:2).EQ.'ss') THEN
         nma = nma + 1
         mopt(nma) = 51
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c dead time corr. for variance by providing the avg. count rate: 'DV'
      ELSEIF (copt(nopt)(1:2).EQ.'DV' .OR. copt(nopt)(1:2).EQ.'dv') THEN
         nma = nma + 1
         mopt(nma) = 60
         CALL xrdecopco( copt(nopt), iv, dopt(nma),
     &                  2, status)
         IF (dopt(nma).EQ.0.0D0) status = 1017
c add here new options
c
c illegal file option condition
      ELSE
         status = 1016
      ENDIF
      IF (status.NE.0) THEN
         WRITE (context, '('' xrdecopt> Cannot decode option : '',a)')
     &           copt(nopt)
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      ENDIF
 10   continue
 100  CONTINUE

      if(nopt.ne.1) then
         call xwrite(
     $        'Decoded options:     Flags     Math   Constants',20)

         do k=1,maxopts
            write(context,101) iopt(k), mopt(k), dopt(k)
            call xwrite(context,20)
         enddo
 101     FORMAT (16X,I10,I10,G12.4)
      endif
      
 999  RETURN
      END
c
c
c
