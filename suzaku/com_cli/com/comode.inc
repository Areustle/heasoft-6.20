C+++++ THIS IS COMODE.INC
C
      Integer MAXENT                    ! Y.Ishisaki 94/12/06 defined
C      Parameter( MAXENT = 64 )         !         32 -> 64 for minuit
      Parameter( MAXENT = 100 )         ! 64 -> 100 for minuit (97/01/23)
C
      COMMON /COMODE/ ICOMER, BATCH, RLTIME, LUNCOM,
     &                 BYTCOM(4)
C
      INTEGER*4 ICOMER, LUNCOM
      LOGICAL*4 RLTIME, BATCH
      LOGICAL * 1 BYTCOM, CTRLC
      CHARACTER*3 COMPRM
      EQUIVALENCE (COMPRM,BYTCOM(1))
      EQUIVALENCE (CTRLC,BYTCOM(4))
C
C----- END OF  COMODE.INC
