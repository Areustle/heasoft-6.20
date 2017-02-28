**==clcrsp.spg  processed by SPAG 4.50J  at 17:44 on 20 Jan 1996
 
      SUBROUTINE CLCRSP(N_energies, Resp_energies, N_channels, 
     &                  Ch_bounds, Rsp_min, Ngroup, Max_tot_groups,
     &                  Max_elements, Disperse, Res_reln, Fwhm, 
     &                  N_r_energies, N_lines, LineEn, LineDt, Ichanb, 
     &                  Ichane, Resp_matrix, Resp_work, Num_elements, 
     &                  Num_tot_groups, Ierr)

      IMPLICIT NONE
 
      INTEGER N_energies , N_channels, N_r_energies, N_lines
      INTEGER Max_elements , Max_tot_groups
 
      REAL Resp_energies(0:N_energies) , Resp_matrix(Max_elements)
      REAL Ch_bounds(N_channels,2), LineEn(N_r_energies)
      REAL Resp_work(N_channels), LineDt(3,N_lines,N_r_energies)
      REAL Rsp_min, Fwhm
 
      INTEGER Ngroup(N_energies)
      INTEGER Ichanb(Max_tot_groups) , Ichane(Max_tot_groups)
      INTEGER Num_elements , Num_tot_groups , Ierr

      CHARACTER res_reln*(*)

      LOGICAL disperse

c This routine actually calculates the response matrix assuming a
c Gaussian response
c Arguments :
c     N_energies       i     i: Number of response energy bins
c     Resp_energies    r     i: Response energy ranges
c     N_channels       i     i: Number of channels
c     Ch_bounds        r     i: Channel energy boundaries
c     Rsp_min          r     i: The minimum response value that will be stored
c     Ngroup           i     r: The number of response groups for each energy
c     Max_tot_groups   i     i: The maximum number of total response groups
c     Ichanb           i     r: Start channel for each response group
c     Ichane           i     r: End channel for each response group
c     Max_elements     i     i: Maximum number of response elements
c     Disperse         c*(*) i: Whether dispersive or non-dispersive
c     Fwhm             r     i: FWHM for fiducial spectral resolution
c     N_r_energies     i     i: Number of resolution points if tabulated
c     N_lines          i     i: Number of lines (photopeak + escape)
c     LineEn           i     i: Energies for tabulated response data
c     LineDt           i     i: Tabulated line (delta energy, 
c                               resolution, norm) data
c     Resp_matrix      r     r: Response matrix elements
c     Resp_work        r     w: Work array for response for current energy
c     Num_elements     i     r: Number of response matrix elements calculated
c     Num_tot_groups   i     r: Total number of response groups calculated
c     Ierr             i     r: Error status    0 == OK

 
      REAL centroid, sigma , norm, energy, effic, gfact
 
      INTEGER igroup, iresp, i, j, k, icen

      CHARACTER(255) wrtstr
 
      LOGICAL counting
 
      INTEGER bisearch
      REAL     grspvl
      EXTERNAL grspvl, bisearch

      Ierr = 0

c Initialize the Ichanb and Ichane arrays

      DO i = 1, Max_tot_groups
         Ichanb(i) = 0
         Ichane(i) = 0
      ENDDO

c Loop round the response energies
 
      igroup = 0
      iresp = 0
 
      DO i = 1 , N_energies
 
         energy = (Resp_energies(i-1)+Resp_energies(i))/2

c Initialize the resp_work array

         DO j = 1, N_channels
            resp_work(j) = 0.
         ENDDO

c Loop over the lines (photopeak + escapes)

         DO k = 1, N_lines

c Get the centroid, sigma, and norm for this line

            CALL GETSIG(energy, k, Disperse, Res_reln, Fwhm, 
     &                  N_r_energies, N_lines, lineEn, lineDt,
     &                  centroid, sigma, norm)
            IF ( sigma .LT. 0 ) THEN
               Ierr = 3
               RETURN
            ENDIF

            IF ( i/100. .EQ. FLOAT(i/100) ) THEN
               WRITE(wrtstr, '(2(2x,i8), 4(2x,1pe12.5))') i, k, energy, 
     &            centroid, sigma, norm
               CALL fcecho(wrtstr)
            ENDIF

c Find the channel containing centroid

            icen = bisearch(N_channels, Ch_bounds, centroid)

c Load the line into the resp_work array

            j = icen
            gfact = 1.
            DO WHILE ( gfact .GT. 0. .AND. j .GE. 1 )
               gfact = 0.
               IF ( sigma .GT. 0. ) THEN 
                  gfact = GRSPVL(centroid,Ch_bounds(j,1),
     &                           Ch_bounds(j,2),sigma)
               ELSE
                  IF ( centroid .GT. Ch_bounds(j,1) .AND. 
     &                 centroid .LE. Ch_bounds(j,2) ) THEN
                     gfact = 1.
                  ENDIF
               ENDIF
               Resp_work(j) = Resp_work(j) + norm*gfact
               j = j - 1
            ENDDO

            j = icen + 1
            gfact = 1.
            DO WHILE ( gfact .GT. 0. .AND. j .LE. N_channels )
               gfact = 0.
               IF ( sigma .GT. 0. ) THEN 
                  gfact = GRSPVL(centroid,Ch_bounds(j,1),
     &                           Ch_bounds(j,2),sigma)
               ELSE
                  IF ( centroid .GT. Ch_bounds(j,1) .AND. 
     &                 centroid .LE. Ch_bounds(j,2) ) THEN
                     gfact = 1.
                  ENDIF
               ENDIF
               Resp_work(j) = Resp_work(j) + norm*gfact
               j = j + 1
            ENDDO

c End loop over lines

         ENDDO

c Transfer the work array into the standard compressed response structure

         Ngroup(i) = 0
         counting = .FALSE.
         effic = 0.
         DO j = 1 , N_channels

c  if response greater than minimum acceptable then include
 
            IF ( Resp_work(j).GT.Rsp_min ) THEN
 
               iresp = iresp + 1
               IF ( iresp.GT.Max_elements ) THEN
                  wrtstr = 'Too many response elements'//
     &                     ' - increase max_elements'
                  CALL fcecho(wrtstr)
                  Ierr = 1
                  RETURN
               ENDIF
               Resp_matrix(iresp) = Resp_work(j)
               effic = effic + Resp_work(j)
 
c  if not currently in a group then start one
 
               IF ( .NOT.counting ) THEN
                  igroup = igroup + 1
                  IF ( igroup.GT.Max_tot_groups ) THEN
                     wrtstr = 'Too many response groups'//
     &                        ' - increase max_tot_groups'
                     CALL fcecho(wrtstr)
                     Ierr = 2
                     RETURN
                  ENDIF
                  Ichanb(igroup) = j
                  counting = .TRUE.
                  Ngroup(i) = Ngroup(i) + 1
               ENDIF
 
               Ichane(igroup) = j
 
c  if response not greater than acceptable minimum and in a group 
c  then end that group
 
 
            ELSEIF ( counting ) THEN
               counting = .FALSE.
 
            ENDIF
 
         ENDDO

c  end loop over energies

      ENDDO
 
      Num_elements = iresp
      Num_tot_groups = igroup

      WRITE(wrtstr, '(a,i7,a)') '...', Num_tot_groups, 
     &                             ' groups in response'
      CALL xwrite(wrtstr, 10)
      WRITE(wrtstr, '(a,i7,a)') '...', Num_elements, 
     &                             ' elements in response'
      CALL xwrite(wrtstr, 10)
 
      RETURN
      END
 
 
c ******************************************************************

      FUNCTION bisearch(N_channels, Ch_bounds, centroid)


      INTEGER bisearch, N_channels
      REAL Ch_bounds(N_channels,2)
      REAL centroid

c Function to do a bisection search for which element of the Ch_bounds array
c the input centroid lies in.

      INTEGER low, high

      LOGICAL increase

      IF ( Ch_bounds(1,1) .LT. Ch_bounds(N_channels, 2) ) THEN
         increase = .TRUE.
      ELSE
         increase = .FALSE.
      ENDIF

      IF ( increase ) THEN
         IF ( centroid .LT. Ch_bounds(1,1) ) THEN
            bisearch = 1
            RETURN
         ENDIF 
         IF ( centroid .GT. Ch_bounds(N_channels,2) ) THEN
            bisearch = N_channels
            RETURN
         ENDIF
      ELSE
         IF ( centroid .GT. Ch_bounds(1,1) ) THEN
            bisearch = 1
            RETURN
         ENDIF
         IF ( centroid .LT. Ch_bounds(N_channels,2) ) THEN
            bisearch = N_channels
            RETURN
         ENDIF
      ENDIF

      low = 0
      high = N_channels

      DO WHILE ( (high-low) .GT. 1 )

         bisearch = (low + high) / 2
         IF ( (increase .AND. centroid .GT. Ch_bounds(bisearch,1)) .OR.
     &        (.NOT.increase .AND. centroid .LT. Ch_bounds(bisearch,1))
     &           ) THEN
            low = bisearch
         ELSE
            high = bisearch
         ENDIF

      ENDDO

      bisearch = low

      RETURN
      END

