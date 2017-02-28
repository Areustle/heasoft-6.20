 
      SUBROUTINE FOLDIN(Max_elements,N_energies,N_a_energies,
     &                  Max_tot_groups,Eff_area,Resp_matrix,
     &                  Resp_energies,Ngroup,Ichanb,Ichane,
     &                  Num_elements,Num_tot_groups)

      IMPLICIT NONE

      INTEGER Max_elements, Max_tot_groups, N_a_energies, N_energies
 
      REAL Resp_matrix(Max_elements)
      REAL Resp_energies(0:N_energies)
      REAL Eff_area(2,N_a_energies)
 
      INTEGER Ngroup(N_energies)
      INTEGER Ichanb(Max_tot_groups)
      INTEGER Ichane(Max_tot_groups)
      INTEGER Num_elements, Num_tot_groups

c subroutine to fold the effective area dependence on energy into the 
c response matrix.
c Arguments :
c     Max_elements     i     i: Maximum number of response elements
c     N_energies       i     i: Number of response energy bins
c     N_a_energies     i     i: Number of effective area points
c     Max_tot_groups   i     i: The maximum number of total response groups
c     Eff_area         r     i: Energies and effective areas
c     Resp_matrix      r   i/r: Response matrix elements
c     Resp_energies    r     i: Response energy ranges
c     Ngroup           i     r: The number of response groups for each energy
c     Ichanb           i     r: Start channel for each response group
c     Ichane           i     r: End channel for each response group
c     Num_elements     i     i: Number of response matrix elements calculated
c     Num_tot_groups   i     i: Total number of response groups calculated
 
      REAL cx(0:1000) , cy(0:1000) , factor
 
      INTEGER ia , ib , ien , n , k , i
      INTEGER iresp , igroup
 
c  loop over energies
 
      iresp = 0
      igroup = 0
      DO ien = 1 , N_energies
 
         cx(0) = Resp_energies(ien-1)
         cx(1) = Resp_energies(ien)

c  if energy is above effective area data then put factor equal to zero
 
         IF ( cx(0).GE.Eff_area(1,N_a_energies) ) THEN
 
            factor = 0.

         ELSE
 
c  find first tabulated energy above bottom of bin and place in IB+1
 
            ib = 1
            DO WHILE ( Eff_area(1,ib+1).LE.cx(0) )
               ib = ib + 1
            ENDDO
 
c  find first tabulated energy above top of bin and place in IA+1
 
            ia = ib + 1
            IF ( ia.GT.N_a_energies ) ia = ia - 1
            DO WHILE ( Eff_area(1,ia).LT.cx(1) .AND. 
     &                 (ia).LT.N_a_energies )
               ia = ia + 1
            ENDDO
 
c  calculate number of tabulated values for this bin. if > 1 then there are
c  edges present.
 
            n = ia - ib
 
c  calculate interpolated areas at top and bottom of bin
 
            cy(0) = Eff_area(2,ib) + (Eff_area(2,ib+1)-Eff_area(2,ib))
     &              *(cx(0)-Eff_area(1,ib))
     &              /(Eff_area(1,ib+1)-Eff_area(1,ib))
            cy(n) = Eff_area(2,ia-1) + (Eff_area(2,ia)-Eff_area(2,ia-1))
     &              *(cx(1)-Eff_area(1,ia-1))
     &              /(Eff_area(1,ia)-Eff_area(1,ia-1))

c  if no edges present then factor is mean of these two areas
 
            IF ( n.LE.1 ) THEN
 
               factor = 0.5*(cy(0)+cy(1))
 
c  otherwise factor is energy-weighted mean of areas
 
            ELSE
 
               cx(n) = cx(1)
               DO k = 1 , n - 1
                  cx(k) = Eff_area(1,ib+k)
                  cy(k) = Eff_area(2,ib+k)
               ENDDO
               factor = 0.
               DO k = 1 , n
                  factor = factor + 0.5*(cy(k)+cy(k-1))*(cx(k)-cx(k-1))
               ENDDO
               factor = factor/(Resp_energies(ien)-Resp_energies(ien-1))
 
            ENDIF

         ENDIF

c  multiply matrix by factor

         DO k = 1 , Ngroup(ien)
            igroup = igroup + 1
            IF ( igroup.GT.Num_tot_groups ) THEN
               CALL fcecho('FOLDIN: too many response groups')
               CALL EXIT(1)
            ENDIF
            DO i = Ichanb(igroup) , Ichane(igroup)
               iresp = iresp + 1
               IF ( iresp.GT.Num_elements ) THEN
                  CALL fcecho('FOLDIN: too many response elements')
                  CALL EXIT(1)
               ENDIF
               Resp_matrix(iresp) = Resp_matrix(iresp)*factor
            ENDDO
         ENDDO
 
      ENDDO
 
      END
 
c ****************************************************************************

