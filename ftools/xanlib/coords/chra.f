      SUBROUTINE CHRA(Ra,Irh,Irm,Rsec,Iflag)
C
C CHANGE ra hr, min and sec to decimal degrees (iflag .ne. 1)
C          or decimal degrees to hr, mn, sec (iflag = 1 )
C 
c
      REAL*8 Ra , rra , ram , rasec
      REAL*4 Rsec
      INTEGER*4 Irh , Irm , Iflag
c
      IF ( Iflag.EQ.1 ) THEN
         rra = Ra/15.
         Irh = rra
         ram = rra - Irh
         ram = ram*60.
         Irm = ram
         rasec = ram - Irm
         Rsec = rasec*60.
         IF ( Rsec.EQ.60.0 ) THEN
            Rsec = 0.0
            Irm = Irm + 1
         ENDIF
         IF ( Irm.EQ.60.0 ) THEN
            Irm = 0.0
            Irh = Irh + 1
         ENDIF
         IF ( Irh.GE.24.0 ) Irh = Irh - 24.0
      ELSE
         Ra = (DFLOAT(Irh)+(DFLOAT(Irm)+DBLE(Rsec)/60.)/60.)*15.D0
      ENDIF
      RETURN
      END
