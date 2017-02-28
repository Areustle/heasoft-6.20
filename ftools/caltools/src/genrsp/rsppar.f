 
      SUBROUTINE RSPPAR(inrfil, rmffil, disperse, tlscpe, instrm, 
     &                  resp_reln, resp_file, resp_low, resp_high, 
     &                  resp_number, resp_break, resp_bnumber, 
     &                  chan_reln, chan_file, chan_low, chan_high, 
     &                  chan_number, chan_break, chan_bnumber, efffil, 
     &                  detfil, filfil, resol_reln, resol_file, fwhm, 
     &                  max_elements, rsp_min, clobber, ierr)

      IMPLICIT NONE
 
      REAL Fwhm, Rsp_min, Resp_Low, Resp_High, Resp_break
      REAL Chan_low, Chan_high, Chan_break
 
      INTEGER Max_elements, Resp_Number, Resp_Bnumber, Ierr
      INTEGER Chan_number, Chan_bnumber
 
      CHARACTER*(*) Inrfil, Rmffil, Tlscpe, Instrm, Efffil, Detfil
      CHARACTER*(*) Filfil, resol_reln, resol_file
      CHARACTER*(*) resp_reln, resp_file, Chan_reln, Chan_file

      LOGICAL disperse, clobber
 
 
c Arguments :
c    inrfil         c*(*)      r: Input RMF filename
c    rmffil         c*(*)      r: Output RMF filename
c    disperse       l          r: Whether dispersive or non-dispersive
c    tlscpe         c*(*)      r: Telescope name
c    instrm         c*(*)      r: Instrument name
c    resp_reln      c*(*)      r: Response energy relation (LINEAR, LOG, FILE)
c    resp_file      c*(*)      r: File for response energies
c    resp_low       r          r: Low energy/wavelength for response energies
c    resp_high      r          r: High energy/wavelength for response energies
c    resp_number    i          r: Number of response bins
c    resp_break     r          r: Break energy/wavelength for response
c    resp_bnumber   i          r: Number of response bins above break
c    chan_reln      c(*)       r: Channel energy relation (LINEAR, LOG, FILE)
c    chan_file      c*(*)      r: File for channel specifications
c    chan_low       r          r: Lowest energy/wavelength of channels
c    chan_high      r          r: Highest energy/wavelength of channels
c    chan_number    i          r: Number of channels
c    chan_break     r          r: Break energy/wavelength for channel size
c    chan_bnumber   i          r: Number of channels above break
c    efffil         c*(*)      r: Name of the file with effective areas
c    detfil         c*(*)      r: Name of the file with detector efficiencies
c    filfil         c*(*)      r: Name of the file with filter transmissions
c    resol_reln       c*(*)      r: Relation for resolution dependence
c    resol_file       c*(*)      r: Filename for resolution dependence
c    fwhm           r          r: Fiducial FWHM of spectral resolution
c    Max_elements   i          r: Maximum number of non-zero response elements
c    rsp_min        r          r: Minimum response value stored
c    clobber        l          r: Whether to delete an existing output file

 
      CHARACTER(72) contxt

      INTEGER lenact
      EXTERNAL lenact

c Get the name of any input RMF file

      CALL UCLGST('inrfil',Inrfil,Ierr)
      contxt = 'Failed to get RMF filename'
      IF ( Ierr.NE.0 ) GOTO 999

c Get the name of the RMF file
 
      CALL UCLGST('rmffil',Rmffil,Ierr)
      contxt = 'Failed to get RMF filename'
      IF ( Ierr.NE.0 ) GOTO 999

c If no input RMF file was given then get the information required
c to calculate the file

      IF ( Inrfil .EQ. 'none' .OR. Inrfil .EQ. 'NONE' ) THEN
 
c  get the relation for the resolution
 
         CALL UCLGST('resol_reln',resol_reln,Ierr)
         contxt = 'Failed to get resol_reln parameter'
         IF ( Ierr.NE.0 ) GOTO 999
         CALL upc(resol_reln)
 
c  if required get the filename with the resolution relation

         IF ( resol_reln .EQ. 'FILE' ) THEN
            CALL UCLGST('resol_file',resol_file,Ierr)
            contxt = 'Failed to get resol_file parameter'
            IF ( Ierr.NE.0 ) GOTO 999
         ELSE
            resol_file = 'none'
         ENDIF

c Get the disperse parameter

         IF ( resol_reln .EQ. 'CONSTANT' .OR. 
     &        resol_reln .EQ. 'LINEAR' .OR. 
     &        resol_reln .EQ. 'FILE' ) THEN
            CALL UCLGSB('disperse',disperse,Ierr)
            contxt = 'Failed to get disperse'
            IF ( Ierr.NE.0 ) GOTO 999
         ELSE
            disperse = .FALSE.
         ENDIF

c Get the fiducial spectral resolution if the resol_reln is not
c file or czt.
 
         IF ( resol_reln .NE. 'FILE' .AND. resol_reln .NE. 'CZT' ) THEN
            CALL UCLGSR('fwhm',Fwhm,Ierr)
            contxt = 'Failed to get fwhm'
            IF ( Ierr.NE.0 ) GOTO 999
         ENDIF

c  get the relation for the response energy definitions
 
         CALL UCLGST('resp_reln',resp_reln,Ierr)
         contxt = 'Failed to get resp_reln parameter'
         IF ( Ierr.NE.0 ) GOTO 999
         CALL upc(resp_reln)

c  if required get the filename with the gain relation

         IF ( resp_reln .EQ. 'FILE' ) THEN
            CALL UCLGST('resp_file',resp_file,Ierr)
            contxt = 'Failed to get resp_file parameter'
            IF ( Ierr.NE.0 ) GOTO 999
         ELSE
            resp_file = 'none'
         ENDIF

c If a response energy definition file is not in use then get all the 
c information required to set up the response energies.

         IF ( resp_reln .NE. 'FILE' ) THEN

c Get the response low energy/wavelength
 
            CALL UCLGSR('resp_low',resp_low,Ierr)
            contxt = 'Failed to get resp_low'
            IF ( Ierr.NE.0 ) GOTO 999
 
c Get the response high energy/wavelength
 
            CALL UCLGSR('resp_high',resp_high,Ierr)
            contxt = 'Failed to get resp_high'
            IF ( Ierr.NE.0 ) GOTO 999

c Get the number of response energies
 
            CALL UCLGSI('resp_number',Resp_Number,Ierr)
            contxt = 'Failed to get resp_number'
            IF ( Ierr.NE.0 ) GOTO 999
 
c Get the response break energy/wavelength
 
            CALL UCLGSR('resp_break',Resp_Break,Ierr)
            contxt = 'Failed to get resp_break'
            IF ( Ierr.NE.0 ) GOTO 999
 
c Get the number of response energies above the break
 
            CALL UCLGSI('resp_bnumber',Resp_Bnumber,Ierr)
            contxt = 'Failed to get resp_bnumber'
            IF ( Ierr.NE.0 ) GOTO 999

         ENDIF

c  get the relation for the channel definitions
 
         CALL UCLGST('chan_reln',Chan_reln,Ierr)
         contxt = 'Failed to get chan_reln parameter'
         IF ( Ierr.NE.0 ) GOTO 999
         CALL upc(Chan_reln)

c  if required get the filename with the gain relation

         IF ( Chan_reln .EQ. 'FILE' ) THEN
            CALL UCLGST('chan_file',Chan_file,Ierr)
            contxt = 'Failed to get Chan_file parameter'
            IF ( Ierr.NE.0 ) GOTO 999
         ELSE
            Chan_file = 'none'
         ENDIF

c If a channel definition file is not in use then get all the 
c information required to set up the response energies.

         IF ( Chan_reln .NE. 'FILE' ) THEN

c Get the channel low energy/wavelength
 
            CALL UCLGSR('chan_low',Chan_low,Ierr)
            contxt = 'Failed to get chan_low'
            IF ( Ierr.NE.0 ) GOTO 999
 
c Get the channel high energy/wavelength
 
            CALL UCLGSR('chan_high',Chan_high,Ierr)
            contxt = 'Failed to get chan_high'
            IF ( Ierr.NE.0 ) GOTO 999
 
c Get the number of channels
 
            CALL UCLGSI('chan_number',chan_number,Ierr)
            contxt = 'Failed to get chan_number'
            IF ( Ierr.NE.0 ) GOTO 999
 
c Get the channel break energy

            CALL UCLGSR('chan_break',Chan_break,Ierr)
            contxt = 'Failed to get chan_break'
            IF ( Ierr.NE.0 ) GOTO 999
 
c Get the channel bin size above the break
 
            CALL UCLGSI('chan_bnumber',chan_bnumber,Ierr)
            contxt = 'Failed to get chan_bnumber'
            IF ( Ierr.NE.0 ) GOTO 999

         ENDIF

c Get the maximum number of response elements
 
         CALL UCLGSI('max_elements',Max_elements,Ierr)
         contxt = 'Failed to get max_elements'
         IF ( Ierr.NE.0 ) GOTO 999

c End of parameters required to calculate the RMF - if RMF is being
c read then make sure that the resol_file is set to none

      ELSE

         resol_file = 'none'
         resp_file = 'none'
         Chan_file = 'none'

      ENDIF

c Get the telescope name
 
      CALL UCLGST('tlscpe',Tlscpe,Ierr)
      contxt = 'Failed to get tlscpe'
      IF ( Ierr.NE.0 ) GOTO 999
 
c Get the instrument name
 
      CALL UCLGST('instrm',Instrm,Ierr)
      contxt = 'Failed to get instrm'
      IF ( Ierr.NE.0 ) GOTO 999

c  get the name of the effective area file
 
      CALL UCLGST('efffil',Efffil,Ierr)
      contxt = 'Failed to get efffil parameter'
      IF ( Ierr.NE.0 ) GOTO 999

c  get the name of the detector efficiency file

      CALL UCLGST('detfil',Detfil,Ierr)
      contxt = 'Failed to get detfil parameter'
      IF ( Ierr.NE.0 ) GOTO 999

c  get the name of the filter transmission file
 
      CALL UCLGST('filfil',Filfil,Ierr)
      contxt = 'Failed to get filfil parameter'
      IF ( Ierr.NE.0 ) GOTO 999
 
c Get the response minimum
 
      CALL UCLGSR('rsp_min',Rsp_min,Ierr)
      contxt = 'Failed to get rsp_min'
      IF ( Ierr.NE.0 ) GOTO 999

c Get the clobber parameter
 
      CALL UCLGSB('clobber',clobber,Ierr)
      contxt = 'Failed to get clobber'
      IF ( Ierr.NE.0 ) GOTO 999

 999  CONTINUE
      IF ( Ierr.NE.0 ) THEN
         CALL XWRITE(contxt,10)
         WRITE (contxt,'(a,i4)') ' error = ' , Ierr
         CALL XWRITE(contxt,10)
      ENDIF
 
      RETURN
      END
