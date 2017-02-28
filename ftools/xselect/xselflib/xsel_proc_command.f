c Common routine to process the input command

      SUBROUTINE xsel_proc_command(commnd, idone, isTcl)

      IMPLICIT NONE

      CHARACTER commnd*(*)
      INTEGER idone
      LOGICAL isTcl

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

c ---------------------------------------------
c To test for presence of saved session
      logical SAVED
c ---------------------------------------------
      CHARACTER(255) str1
      INTEGER LENACT

c Go through the command options...

      IF ( commnd.EQ.'QUIT' .OR. commnd.eq.'STOP' .OR.
     &     (commnd.eq.'EXIT' .AND. idone.NE.-1) ) THEN
         status = 0
         call UCLGSB('save_session',SAVED,status)
c Next save the par file
         IF(SAVED) THEN
            call XSL_SAVE(1)
         ELSE
            CALL XSL_CLEAR(1)
            call XSL_RMFILE(cmdfil)
            CALL XSL_CLEANPAR()
            ierr = 0
            call XPISAVEPAR(ierr)
            call XSL_RMFILE(cmdfil)
            call XSL_EXIT(status)
         ENDIF

c GTCOM2 sends this signal if it gets an EOF from the input stream

      ELSE IF(commnd.eq.'EXIT' .AND. idone.EQ.-1) THEN

c Use ierr here and not status,
c since we want to report status in the exit:
         ierr = 0 
c Next save the par file
         call XSL_SAVE(1)     
      ELSE IF(idone.eq.1) THEN
c This is if the command has already been done (by XPI):       
c Reset the status flag, so it doesn't keep reporting the error:
         status = 0

c End of general XANADU stuff. Into individual XSELECT commands.

c ---------------------------------------------
      ELSE IF(commnd.eq.'BIN'.or.commnd.eq.'EXTRACT')then
c ---------------------------------------------
c New BINning, using EXTRACTor **

         call XSL_BIN(0)
            
c ---------------------------------------------
      ELSE IF(commnd.eq.'CHOOSE')then
c ---------------------------------------------

         call XSL_CHOOSE()

c ---------------------------------------------
      ELSE IF(commnd.eq.'CLEAR')then
c ---------------------------------------------
c CLEAR **
c XSL_CLEAR() combines the functions of the old _CLEAR and
c _EXIT; it removes all the temporary files that have been
c lying around. and resets logicals and filenames.  

         call XSL_CLEAR(0)
c ---------------------------------------------
      ELSE IF(commnd.eq.'CPD') THEN            
c ---------------------------------------------
c CPD device = set dev:
         call XSL_CPD()

c ---------------------------------------------
      ELSE IF(commnd.eq.'ECHO')then
c ---------------------------------------------
c ECHO **
c Toggle Echoing command files to the screen

         call XSL_ECHO(ECHO)
            
c ---------------------------------------------
      ELSE IF (commnd.eq.'FILTER') then
c ---------------------------------------------
c FILTER **
c Enter filter regions
         call XSL_ENTER_FILTER() 

c ---------------------------------------------
      ELSE IF(commnd.eq.'FAINT')then
c ---------------------------------------------
c FAINT **
         
         call XSL_FAINT()
c ---------------------------------------------
      ELSE IF(commnd.eq.'FAST')then
c ---------------------------------------------
c FAST **

         call XSL_FAST()

c ---------------------------------------------
      ELSE IF(commnd.eq.'GISCLEAN') THEN
c ---------------------------------------------
c GISCLEAN **

         call XSL_GISCLEAN()

c ---------------------------------------------
      ELSEIF( (commnd.eq.'LIST'.and..not.isTcl) .OR.
     &        (commnd.eq.'XLIST'.and.isTcl) )then
c ---------------------------------------------
c LIST **
         call XSL_LISTSTUFF()
            
c ---------------------------------------------
      ELSEIF( (commnd.eq.'LOAD'.and..not.isTcl) .OR.
     &        (commnd.eq.'XLOAD'.and.isTcl) )then
c ---------------------------------------------
c LIST **
         call XSL_LOAD()
            

c ---------------------------------------------
      ELSEIF(commnd.eq.'MKFBIN')then
c ---------------------------------------------
c MKFBIN **

         call XSL_MKFBIN()

c ---------------------------------------------
      ELSEIF(commnd.eq.'HKBIN')then
c ---------------------------------------------
c HKBIN **

         call XSL_HKBIN()

c ---------------------------------------------
      ELSEIF(commnd.eq.'MAKE')then
c ---------------------------------------------
c MAKE **

         call XSL_MAKEO(0)


c ---------------------------------------------
      ELSEIF(commnd.eq.'PLOT')then
c ---------------------------------------------
c PLOT **

         call XSL_PLOT()

c ---------------------------------------------
      ELSEIF( (commnd.eq.'READ'.and..not.isTcl) .OR.
     &        (commnd.eq.'XREAD'.and.isTcl) )then
c ---------------------------------------------
c READ **
             
c Set up default keywords for the mission.  
c For now we do this at the very beginning.
c           IF(.not.READ.and. .not.HKREAD) then
c              call XSL_MISSION()
c           ENDIF
c Do the read
         call XSL_READ()

c ---------------------------------------------
      ELSEIF(commnd.eq.'SAOIMAGE')then
c ---------------------------------------------
c IMAGE **

         call XSL_SAOIMAGE(0)

c ---------------------------------------------
      ELSEIF(commnd.eq.'SAVE')then
c ---------------------------------------------
c SAVE **

         call XSL_SAVE(0)

c ---------------------------------------------
      ELSEIF(commnd.eq.'SELECT')then
c ---------------------------------------------
c SELECT **
         call XSL_SELECT()

c ---------------------------------------------
      ELSEIF( (commnd.eq.'SET'.and..not.isTcl) .OR.
     &        (commnd.eq.'XSET'.and.isTcl) )then
c ---------------------------------------------
c SET **
         call XSL_SET(0)

c ---------------------------------------------
      ELSEIF(commnd.eq.'SHOW')then
c ---------------------------------------------
c SHOW **

         call XSL_SHOW()

c ---------------------------------------------
      ELSEIF(commnd.eq.'SISPI')then
c ---------------------------------------------
c SISPI **

         call XSL_SISPI()

c ---------------------------------------------
      ELSEIF(commnd.eq.'SMOOTH')then
c ---------------------------------------------
c SMOOTH **

         call XSL_SMOOTH()

c ---------------------------------------------
      ELSEIF(commnd.eq.'SISCLEAN')then
c ---------------------------------------------
c SISCLEAN **

         call XSL_SISCLEAN()

c ---------------------------------------------
      ELSEIF(commnd.eq.'UNSELECT')then
c ---------------------------------------------
c UNSELECT **
         call XWRITE(' UNSELECT is not available yet.',5)

c ---------------------------------------------
c We're done.
c ---------------------------------------------
      ELSE
         str1 = 'Unknown Command: '//commnd(:LENACT(commnd))
         call XWRITE(str1, 5)

      ENDIF

      RETURN
      END
