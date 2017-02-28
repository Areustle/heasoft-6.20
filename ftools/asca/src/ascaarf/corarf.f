
      SUBROUTINE corarf(instrum, qnorti, qarffl, nenerg, middl, arf)

      INTEGER instrum, nenerg
      REAL    middl(nenerg), arf(nenerg)
      LOGICAL qnorti, qarffl

c This routine modifies the ARF using the arrfilter correction and if
c necessary the effects of no RTI rejection.

c Arguments :
c     instrum        i      i: Instrument
c     qnorti         l      i: If true then no RTI rejection used
c     qarffl         l      i: If true then arf filter is used
c     nenerg         i      i: Number of energies in ARF
c     middl          r      i: Central energies of ARF bins
c     arf            r    i/r: Effective areas

      INTEGER ie

      DOUBLE PRECISION get_gis_norti, get_arf_corr
      EXTERNAL get_gis_norti, get_arf_corr

c Loop over energies for RTI correction

      IF ( instrum .GE. 2 .AND. qnorti ) THEN

         DO ie = 1, nenerg

            arf(ie) = arf(ie) * SNGL(get_gis_norti(DBLE(middl(ie)), 
     &                                          instrum))

         ENDDO

      ENDIF

c Do the arffilter correction

      IF ( qarffl ) THEN

         DO ie = 1, nenerg

            arf(ie) = arf(ie) * SNGL(get_arf_corr(DBLE(middl(ie)), 
     &                                          instrum))

         ENDDO

      ENDIF


      RETURN
      END




