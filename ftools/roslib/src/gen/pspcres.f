C -------------------------------------------------------------------------
*+PSPCRES
      subroutine pspcres(instrume,obs_date,status)
      implicit none
      character*(*) instrume,obs_date
      integer status
C Description
C     Checks to make sure the instrument string for a pspc observation 
C     is standard. Fixes it if it's not. Exits if first four characters
C     of instrument string are not PSPC (case insensitive).
C Parameters
C     instrume    i   : the instrument name 
C     obs_date    i   : the observation date string
C     status       o  : status flag
C Authors/Modification History:
C     Ning Gan (1.0.2:1998 Jul) Using fts2dt to parse the date.
C     Lawrence E Brown (1.0.1:1994 Oct), fixed bug in obs_date if structure
C     Lawrence E Brown (1.0.0:1994 Sept), first release
*-
C     
C  Local Variables
      character(160) message
      integer month,year,day
      call ftupch(instrume)
      if(instrume(1:4).ne.'PSPC') return
      if(instrume(1:5).eq.'PSPCB'.or.instrume(1:5).eq.'PSPCC'.or.
     $     instrume(1:5).eq.'PSPCA'.or.instrume(1:5).eq.'PSPCD')return
      if(instrume(1:6).eq.'PSPC-B'.or.instrume(1:6).eq.'PSPC-C') then
         instrume(5:5)=instrume(6:6)
         instrume(6:6)=' '
         return
      endif
      if(instrume(5:5).eq.' ') then
C     decide between PSPCB and PSPCC by date
C         read(obs_date(4:5),'(i2)') month
C         read(obs_date(7:8),'(i2)') year
	 status = 0
	 call fts2dt(obs_date,year,month,day,status)
	 status = 0 
         if(year.gt.1991) then
            instrume='PSPCB'
         else if (year.lt.1991) then
            instrume='PSPCC'
         else if (month.ge.2) then                                      
            instrume='PSPCB'
         else                                                           
            instrume='PSPCC'
         endif
         return
      endif
C     if you made it to here, you're uncouth
         status=1
       message='PSPCRES cannot resolve'//instrume//'into a legal name.'
         call fcerr(message)
      return 
      end
      

