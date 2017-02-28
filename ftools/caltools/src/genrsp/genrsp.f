
c  Program to fake up a response matrix. Assumes that the response is 
c  Gaussian and that the efficiency is given by a combination of data 
c  read in from effective area, detector efficiency, and filter transmission 
c  files. The last two of these can be ignored by setting the filename to 
c  'none'. If the spectrometer is defined as dispersive then parameters are 
c  assumed to be given in Angstroms and if non-dispersive then in keV. The 
c  response bins and channels are linear with hidden parametes allowing a 
c  single break to a different linear relation. There are a number of options 
c  for the change in resolution with energy or wavelength : constant is 
c  constant in either energy or wavelength; sqroot is square root in energy;
c  czt is the relation for CdZnTe detectors; and file reads the relation 
c  from a file.

c  v1.00   kaa   1/22/96
c  v1.01   kaa   4/17/98  bug fix in stbins. was not setting the number of
c                         channels and energies correctly when there was no
c                         break in the channel/energy binsize.
c  v1.10   kaa   2/16/98  Added the option to give an input RMF file instead
c                         of calculating it from the resolution information
c  v1.11   ngan  9/21/00  Initialized the variable ierr to 0.
c  v1.12   kaa   7/17/02  Fixed unsaved variables and incorrect initialization in getsig
c  v1.13   kaa   7/30/04  Improved error if user gives an invalid value for
c                         the res_reln parameter.
c  v1.14   kaa   2/02/05  Corrected bug when reading from a file resolution 
c                         information for a dispersive instrument. This option 
c                         has probably never been used.
c  v1.15   kaa   5/17/05  Added the option to include escape peaks.
c  v2.00   kaa   7/25/05  Rewrite to allow more general gain relations
c  v2.01   kaa   7/12/06  Fixed bug (introduced in v2.00) in generating gaussian 
c                         response shape
c  v2.02   kaa   4/02/08  Added LINEAR option for resolution.
c  v2.03   kaa   7/09/08  Fixed bug in generating gaussian response shape when channel
c                         boundaries are in decreasing energy order (viz gratings)
c  v2.04   kaa   8/05/08  Write LO_THRES keyword as well as LO_THRESH since the
c                         former actually conforms to the FITS standard.
c  v2.05   kaa  11/16/10  Improved diagnostics for user error.
c  v2.06   kaa   4/26/11  Fixed a bug when reading energies from resp_file.
c  v2.07   kaa   5/2/14   Ensure that E_MIN is always <= E_MAX.
c  v2.08   kaa   5/15/15  Fixed bug when the response energies extend beyond the
c                         channel energies - had to fix bisearch in clcrsp.f


      SUBROUTINE genrsp

      IMPLICIT NONE
 
c Pointers for dynamic arrays
c      resp_matrix(max_elements)      the response matrix elements
c      resp_energies(0:n_energies)    the response matrix energy ranges
c      eff_area(2,n_a_energies)       the energies and effective areas
c      filt_eff(2,n_f_energies)       the energies and filter transmissions
c      det_eff(2,n_d_energies)        the energies and detector efficiencies
c      lineEn(n_r_energies)           the energies for tabulated response data
c      lineDt(3,n_lines,n_r_energies) the centroids (relative to energy), 
c                                     resolutions, and normalizations 
c                                     of lines
c      resp_data(2,n_e_energies)      definition of the response energies
c      chan_data(2,n_c_energies)      definition of the channels
c      ch_bounds(n_channels,2)        the channel boundary energies
c      ngroup(n_energies)             the number of response groups at 
c                                       each energy
c      ichanb(max_tot_groups)         start channel for response groups
c      ichane(max_tot_groups)         end channel for response groups
 
      INTEGER resp_matrix, resp_energies, eff_area, filt_eff, det_eff
      INTEGER resp_work, lineEn, lineDt, resp_data, chan_data
      INTEGER ch_bounds, ngroup, ichanb, ichane

c Other variables 
 
      REAL fwhm, rsp_min, resp_low, resp_high, resp_break 
      REAL chan_low, chan_high, chan_break
 
      INTEGER resp_number, resp_bnumber, chan_number, chan_bnumber
      INTEGER num_elements, max_elements, max_tot_groups
      INTEGER num_tot_groups, n_a_energies, n_channels, n_d_energies
      INTEGER n_energies, n_f_energies, n_r_energies, n_lines
      INTEGER n_e_energies, n_c_energies, ierr

      CHARACTER(20) resp_reln, chan_reln, resol_reln
      CHARACTER(72) resp_file, chan_file, resol_file
      CHARACTER(20) tlscpe, instrm
      CHARACTER(72) contxt, rmffil, efffil, detfil, filfil, inrfil
 
      LOGICAL disperse, clobber

      INTEGER lenact
      EXTERNAL lenact
 
c  common block for dynamic memory
 
      INTEGER          MEMI(100)
      REAL             MEMR(100)
      EQUIVALENCE (MEMI, MEMR)
      COMMON /MEM/ MEMR

      CHARACTER(40) taskname
      COMMON /task/ taskname

      ierr = 0
      CALL fcecho('GENRSP vers 2.08    5/15/15.')
      taskname = 'genrsp v2.08'
       
c  get the parameters
 
      CALL RSPPAR(inrfil, rmffil, disperse, tlscpe, instrm, 
     &            resp_reln, resp_file, resp_low, resp_high, 
     &            resp_number, resp_break, resp_bnumber, chan_reln, 
     &            chan_file, chan_low, chan_high, chan_number, 
     &            chan_break, chan_bnumber, efffil, detfil, filfil, 
     &            resol_reln, resol_file, fwhm, max_elements, rsp_min, 
     &            clobber, ierr)
      contxt = 'Failed to read parameters'
      IF ( ierr.NE.0 ) GOTO 999

      CALL fcecho(' ')

c  get the sizes of the input data (effective area, detector efficiency,
c  filter transmission, and if requested resolution, response energy, and 
c  channel data)
 
      CALL GTDTSZ(efffil, detfil, filfil, resol_file, resp_file, 
     &            chan_file, n_a_energies, n_d_energies, n_f_energies, 
     &            n_r_energies, n_e_energies, n_c_energies, n_lines, 
     &            ierr)
      contxt = 'Failed to get sizes of input files'
      IF ( ierr.NE.0 ) GOTO 999

c  grab the memory for the input data

      eff_area = 0
      filt_eff = 0
      det_eff = 0
      lineEn = 0
      lineDt = 0
      resp_data = 0
      chan_data = 0

      IF ( n_a_energies .GT. 0 ) THEN 
         CALL UDMGET(2*n_a_energies,6,eff_area,ierr)
         contxt = ' Not enough memory for eff_area array'
         IF ( ierr.NE.0 ) GOTO 999
      ENDIF

      IF ( n_f_energies .GT. 0 ) THEN 
         CALL UDMGET(2*n_f_energies,6,filt_eff,ierr)
         contxt = ' Not enough memory for filt_eff array'
         IF ( ierr.NE.0 ) GOTO 999
      ENDIF

      IF ( n_d_energies .GT. 0 ) THEN  
         CALL UDMGET(2*n_d_energies,6,det_eff,ierr)
         contxt = ' Not enough memory for det_eff array'
         IF ( ierr.NE.0 ) GOTO 999
      ENDIF
 
      IF ( n_r_energies .GT. 0 ) THEN  
         CALL UDMGET(n_r_energies,6,lineEn,ierr)
         contxt = ' Not enough memory for lineEn array'
         IF ( ierr.NE.0 ) GOTO 999
         CALL UDMGET(3*n_lines*n_r_energies,6,lineDt,ierr)
         contxt = ' Not enough memory for lineDt array'
         IF ( ierr.NE.0 ) GOTO 999
      ENDIF

      IF ( n_e_energies .GT. 0 ) THEN 
         CALL UDMGET(2*n_e_energies,6,resp_data,ierr)
         contxt = ' Not enough memory for resp_data array'
         IF ( ierr.NE.0 ) GOTO 999
      ENDIF

      IF ( n_c_energies .GT. 0 ) THEN 
         CALL UDMGET(2*n_c_energies,6,chan_data,ierr)
         contxt = ' Not enough memory for chan_data array'
         IF ( ierr.NE.0 ) GOTO 999
      ENDIF

c  read in the effective areas, detector efficiencies, filter
c  transmissions, lines (energies, resolutions, norms), and
c  gain relation

      CALL RDINPD(n_a_energies, efffil, MEMR(eff_area), n_d_energies, 
     &            detfil, MEMR(det_eff), n_f_energies, filfil, 
     &            MEMR(filt_eff), n_r_energies, n_lines, resol_file, 
     &            MEMR(lineEn), MEMR(lineDt), n_e_energies, resp_file, 
     &            MEMR(resp_data), n_c_energies, chan_file, 
     &            MEMR(chan_data), ierr)
      contxt = 'Failed to read input data'
      IF ( ierr.NE.0 ) GOTO 999
 
c If no input RMF file was given then we need to make one so first
c calculate or estimate the sizes of the arrays that will be required.

      IF ( inrfil .EQ. 'none' .OR. inrfil .EQ. 'NONE' ) THEN
 
c  calculate the number of response energy bins and channels

c  if the response energy definition file was given then use that
c  number

         IF ( n_e_energies .GT. 0 ) THEN

            n_energies = n_e_energies

c  otherwise calculate from the other parameters

         ELSE

            n_energies = Resp_number
            IF ( Resp_high .GT. Resp_break .AND. 
     &           Resp_break .GT. 0. ) THEN
               n_energies = n_energies + Resp_bnumber
            ENDIF

         ENDIF
c  if the response energy definition file was given then use that
c  number

         IF ( n_e_energies .GT. 0 ) THEN

            n_energies = n_e_energies

c  otherwise calculate from the other parameters

         ELSE

            n_energies = Resp_number
            IF ( Resp_high .GT. Resp_break .AND. 
     &           Resp_break .GT. 0. ) THEN
               n_energies = n_energies + Resp_bnumber
            ENDIF

         ENDIF

c  if the channel definition file was given then use that
c  number

         IF ( n_c_energies .GT. 0 ) THEN

            n_channels = n_c_energies

c  otherwise calculate from the other parameters

         ELSE

            n_channels = Chan_number
            IF ( Chan_high .GT. Chan_break .AND. 
     &           Chan_break .GT. 0. ) THEN
               n_channels = n_channels + Chan_bnumber
            ENDIF

         ENDIF

         CALL xwrite(' ', 10)
         WRITE(contxt, '(a,i7,a)') '...', n_channels, 
     &                             ' channels in spectrum'
         CALL xwrite(contxt, 10)
         WRITE(contxt, '(a,i7,a)') '...', n_energies, 
     &                             ' energies in response'
         CALL xwrite(contxt, 10)

c  estimate the total number of response groups

         max_tot_groups = n_energies * 10

c  in this case when an input RMF is given then we can read that
c  to get the array sizes

      ELSE

         CALL GTRSSZ(inrfil, max_elements, n_energies, n_channels,
     &               max_tot_groups, ierr)
         contxt = 'Failed to get sizes of input rmf data'
         IF ( ierr.NE.0 ) GOTO 999

      ENDIF
 
c  now grab the memory for the response matrix arrays

      resp_matrix = 0
      resp_energies = 0
      resp_work = 0
      ch_bounds = 0
      ngroup = 0
      ichanb = 0
      ichane = 0
 
      CALL UDMGET(max_elements,6,resp_matrix,ierr)
      contxt = ' Not enough memory for resp_matrix array'
      IF ( ierr.NE.0 ) GOTO 999
 
      CALL UDMGET(n_energies+1,6,resp_energies,ierr)
      contxt = ' Not enough memory for resp_energies array'
      IF ( ierr.NE.0 ) GOTO 999
 
      CALL UDMGET(n_channels,6,resp_work,ierr)
      contxt = ' Not enough memory for resp_work array'
      IF ( ierr.NE.0 ) GOTO 999
 
      CALL UDMGET(2*n_channels,6,ch_bounds,ierr)
      contxt = ' Not enough memory for ch_bounds array'
      IF ( ierr.NE.0 ) GOTO 999
 
      CALL UDMGET(n_energies,4,ngroup,ierr)
      contxt = ' Not enough memory for ngroup array'
      IF ( ierr.NE.0 ) GOTO 999
 
      CALL UDMGET(max_tot_groups,4,ichanb,ierr)
      contxt = ' Not enough memory for ichanb array'
      IF ( ierr.NE.0 ) GOTO 999
 
      CALL UDMGET(max_tot_groups,4,ichane,ierr)
      contxt = ' Not enough memory for ichane array'
      IF ( ierr.NE.0 ) GOTO 999

c Again if there was no input RMF then calculate it

      IF ( inrfil .EQ. 'none' .OR. inrfil .EQ. 'NONE' ) THEN
 
c  set up channel boundaries and response energies

         CALL STBINS(n_channels, chan_low, chan_high, chan_break, 
     &               chan_number, chan_bnumber, n_energies, resp_low,
     &               resp_high, resp_break, resp_number, resp_bnumber, 
     &               disperse, resp_reln, n_e_energies, MEMR(resp_data),
     &               chan_reln, n_c_energies, MEMR(chan_data),
     &               MEMR(ch_bounds),MEMR(resp_energies))
 
c  calculate the response matrix
 
         CALL CLCRSP(n_energies, MEMR(resp_energies), n_channels,
     &               MEMR(ch_bounds), rsp_min, MEMI(ngroup),
     &               max_tot_groups, max_elements, disperse, resol_reln,
     &               fwhm, n_r_energies, n_lines, MEMR(lineEn), 
     &               MEMR(lineDt), MEMI(ichanb), MEMI(ichane), 
     &               MEMR(resp_matrix), MEMR(resp_work), num_elements, 
     &               num_tot_groups, ierr)
         contxt = 'Failed to calculate response matrix'
         IF ( ierr.NE.0 ) GOTO 999

         IF ( num_elements .EQ. 0 ) THEN
            contxt = 
     & 'The calculated response has no elements greater than rsp_min'
            CALL fcecho(contxt)
            CALL exit(1)
         ENDIF

c or if there was an input RMF then read the information from that

      ELSE

         CALL INPRMF(inrfil, n_energies, MEMR(resp_energies), 
     &               n_channels, MEMR(ch_bounds), rsp_min, 
     &               MEMI(ngroup), max_tot_groups, max_elements, 
     &               MEMI(ichanb), MEMI(ichane), MEMR(resp_matrix),
     &               num_elements, num_tot_groups, ierr)
         contxt = 'Failed to input response matrix'
         IF ( ierr.NE.0 ) GOTO 999

      ENDIF

c  fold effective areas into response matrix
 
      IF ( n_a_energies .GT. 0 ) THEN 
         CALL FOLDIN(max_elements,n_energies,n_a_energies,
     &               max_tot_groups,MEMR(eff_area),MEMR(resp_matrix),
     &               MEMR(resp_energies),MEMI(ngroup),MEMI(ichanb),
     &               MEMI(ichane),num_elements,num_tot_groups)
      ENDIF
 
c  fold detector efficiencies into response matrix

      IF ( n_d_energies .GT. 0 ) THEN 
         CALL FOLDIN(max_elements,n_energies,n_d_energies,
     &               max_tot_groups,MEMR(det_eff),MEMR(resp_matrix),
     &               MEMR(resp_energies),MEMI(ngroup),MEMI(ichanb),
     &               MEMI(ichane),num_elements,num_tot_groups)
      ENDIF

c  fold filter transmissions into response matrix
 
      IF ( n_f_energies .GT. 0 ) THEN
         CALL FOLDIN(max_elements,n_energies,n_f_energies,
     &               max_tot_groups,MEMR(filt_eff),MEMR(resp_matrix),
     &               MEMR(resp_energies),MEMI(ngroup),MEMI(ichanb),
     &               MEMI(ichane),num_elements,num_tot_groups)
      ENDIF

c  write response matrix

      CALL WRTRSP(max_elements,n_energies,n_channels,max_tot_groups,
     &            MEMR(resp_matrix),MEMR(resp_energies),
     &            MEMI(ngroup),MEMI(ichanb),MEMI(ichane),
     &            MEMR(ch_bounds),rmffil,tlscpe,instrm,rsp_min,
     &            clobber,ierr)
 
      contxt = ' Failed to write response matrix'
      IF ( ierr .NE. 0 ) GOTO 999

 999  CONTINUE
      IF ( ierr.NE.0 ) THEN
         CALL xwrite(contxt, 10)
         WRITE(contxt, '(a,i4)') ' error = ', ierr
         CALL xwrite(contxt, 10)
      ENDIF
 
      CALL EXIT(0)
      END
 








