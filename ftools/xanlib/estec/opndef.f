**==OPNDEF.spg  processed by SPAG 3.09I  at 09:45 on 20 Aug 1992
      SUBROUTINE OPNDEF(Ierr)
c        open defaults file, or create one if its not there.
c   		taken from defaults by nick 10/2/90
c        n = parameter number to retrieve or store
c        defaults file contains :
c        parameter no.  1 -> database name
c        parameter no.  2 -> sample file name
c        parameter no.  3 -> sort file name
c        parameter no.  4 -> right ascension (1950 independently of line 9)
c        parameter no.  5 -> declination     (1950 independently of line 9)
c        parameter no.  6 -> search radius in arcmin
c        parameter no.  7 -> plot device
c        parameter no.  8 -> source name
c        parameter no.  9 -> equinox year used to precess coordinates
c        parameter is returned in a string of characters
c        parameter no. 10 -> sequence number
c        parameter no. 15 -> default file for user-defined parameters
c        parameter no. 16 -> x pixels
c        parameter no. 17 -> y pixels
c        parameter no. 18 -> previous ra defaults
c        parameter no. 19 -> previous de default
c        parameter no. 20 -> previous rad
c        parameter no. 21 -> ra entered at sc
c        parameter no. 22 -> dec entered at sc
c        parameter no. 22 -39 -> spare
c        parameter no. 40 -50 -> pe command
c        parameter no. 51   -> default ascii table for database 1
c        parameter no. 50+n -> default ascii table for database n
c        parameter no. 200  -> reserved for internal use
c
      INCLUDE 'estec.inc'
      character(100) filename
c
      character(40) string , zero*2 , blank*2
      INTEGER*4 LENACT
      LOGICAL there , open
      INTEGER*2 numeric(200)
      INTEGER*4 Ierr , i , ii
c
      DATA numeric/0 , 0 , 0 , 1 , 1 , 1 , 0 , 0 , 1 , 1 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0/
c
      Ierr = 0
      ii = 0
c
cc get root dir
c
      CALL GETROT(Zrootname)
      filename = Zrootname(:LENACT(Zrootname)) // 'defaults.def'
c
c open lu and check if file is there
c
      INQUIRE (FILE=filename,EXIST=there,OPENED=open)
c
c prepend a backslash "\" (char(92)) to the filename to prevent OPENWR
c from lowercasing the entire path
c
      filename=char(92)//Zrootname(:LENACT(Zrootname))//'defaults.def'
c
      IF ( open ) THEN
c
c
c default file already opened
c
         CALL XWRITE(' Default file was already opened ',5)
c
      ELSEIF ( there ) THEN
c
         CALL GETLUN(Zlun_def)
         CALL OPENWR(Zlun_def,filename,'old','df',' ',40,0,Ierr)
c
cc  open error
c
         IF ( Ierr.NE.0 ) THEN
            WRITE (Zwrite,99001) filename(:LENACT(filename)) , Ierr
            CALL XWRITE(Zwrite,5)
            CALL FRELUN(Zlun_def)
         ENDIF
c
c
      ELSE
c        OPEN (zlun_def,FILE='sys$login:defaults.def',STATUS='new',
c     &        ACCESS='direct',
c     &        shared,RECL=40,FORM='formatted',MAXREC=200)
c
         CALL GETLUN(Zlun_def)
         CALL OPENWR(Zlun_def,filename,'new','df',' ',40,0,Ierr)
c
c  open error
c
         IF ( Ierr.NE.0 ) THEN
            WRITE (Zwrite,99002) filename(:LENACT(filename)) , Ierr
            CALL XWRITE(Zwrite,5)
            CALL FRELUN(Zlun_def)
c
            RETURN
         ENDIF
c
c fill up new default file with zeros
c
         zero = '0'
         blank = ' '
         DO 50 i = 1 , 200
            IF ( numeric(i).EQ.1 ) THEN
               string = zero
            ELSE
               string = blank
               IF ( i.EQ.7 ) string = ' '
            ENDIF
            WRITE (Zlun_def,'(a)',REC=i) string
 50      CONTINUE
         Ierr = 0
         CALL UNLOCK(Zlun_def)
c
      ENDIF
c
c get current directory
c
      CALL GETDIR(Zcurrent_directory)
      CALL LOCASE(Zcurrent_directory)
c
      RETURN
99001 FORMAT (' Old default file ',A,' open error ',I4)
99002 FORMAT (' New default file ',A,' open/create error ',I4)
      END
