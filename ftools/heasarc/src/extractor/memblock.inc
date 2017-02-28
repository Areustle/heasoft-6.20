c Common block used in dynamic memory allocation

      DOUBLE PRECISION MEMD(100)
      REAL MEMR(100)
      INTEGER MEMI(100)
      LOGICAL MEML(100)

      EQUIVALENCE(MEMD, MEMR, MEMI, MEML)

      COMMON /MEM/ MEMD
