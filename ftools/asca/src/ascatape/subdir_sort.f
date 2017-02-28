CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Module subdir_sort: determine the proper subdirectory for ASCA data
C                         files using the ASCA Data Center file organization
C                         scheme.
C
C     Developer:          Don Jennings, HSTX/ADF/GSFC   
C     Version:            1.0
C     Date:               08/17/94
C
C     Modifications:
C     
C     10/25/94 D. Jennings -- added code to distinguish between the (h)igh,
C                             (m)edium, (l)ow and "other" modes for the 
C                             screened and unscreened data catagories
C     01/06/95 D. Jennings -- added code to work with the the new modal
C                             configuration file.
C     01/06/95 D. Jennings -- added code to work with possible GIF file
C                             replacements; ie., jpeg, pds and "pic".
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This subroutine takes as input the name of an ASCA data file and gives
C     back a number code that determines the subdirectory into which the
C     file should go. The subdirectory code is kept in a common block named
C     subdircodes -- any program calling this routine should include this
C     common block in its variable list.
C
C     There are 7 subdirectory classes in the ASCA Data Center organiation:
C
C     RAW        uncleaned and un-modally merged event files from FRFREAD
C     UNSCREENED unfiltered (unscreened) event files
C     SCREENED   time screened (filtered) event files
C     AUX        files necessary to reduce and/or analyze the event files
C     PRODUCT    science products generated from the event files
C     TELEM      original spacecraft telemetry -- used to generate event files
C     WORK       directory where user is assumed to to his/her work
C
C     By default, any file that does not get assigned to a specific class will
C     be placed into the AUX subdirectory.
C
C     Upon sucessful completion subdir_sort returns a 0 in the status argument
C     and upon unsucessful completion it returns a negative number. 
C    
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      subroutine subdir_sort(filename,bincode,status)
C
      implicit      none
C
      character*(*) filename
      integer*4     bincode,status
      integer*4     RAW,UNSCREENED,SCREENED,PROD,AUX,WORK,TELEM
C
      common        /subdircodes/RAW,UNSCREENED,SCREENED,PROD,
     *                           AUX,WORK,TELEM
C
C     initialize variables
C
      status      = 0
      RAW         = 1
      UNSCREENED  = 2
      SCREENED    = 3
      PROD        = 4
      AUX         = 5
      WORK        = 6
      TELEM       = 7
C
C     A file is put in the AUX directory by default
C
      bincode = AUX
C
C     compare the filename against all of the following sort keys. Once
C     a sort match is found then stop looking and goto the end of the
C     program. The sort key comparisions are arranged in a specific order. 
C

C     sort on the catalog file keys
      if(index(filename,'.cat')       .ne. 0)then
         bincode = RAW
         if(index(filename,'frf.cat') .ne. 0) bincode = RAW
         if(index(filename,'raw.cat') .ne. 0) bincode = RAW
         if(index(filename,'unf.cat') .ne. 0) bincode = UNSCREENED
         if(index(filename,'evt.cat') .ne. 0) bincode = SCREENED
         if(index(filename,'tape.cat').ne. 0) bincode = PROD
         goto 1000
      endif
C     sort on the diff file keys
      if(index(filename,'.dif')       .ne. 0)then
         bincode = RAW
         if(index(filename,'frf.dif') .ne. 0) bincode = RAW
         if(index(filename,'unf.dif') .ne. 0) bincode = UNSCREENED
         if(index(filename,'evt.dif') .ne. 0) bincode = SCREENED
         goto 1000
      endif
C     sort on the unscreened event list key
      if(index(filename,'.unf')       .ne. 0)then
         if(index(filename,'h.')      .ne. 0 .or. 
     *      index(filename,'m.')      .ne. 0 .or. 
     *      index(filename,'l.')      .ne. 0 .or.
     *      index(filename,'H.')      .ne. 0 .or. 
     *      index(filename,'M.')      .ne. 0 .or. 
     *      index(filename,'L.')      .ne. 0     )then
            bincode = UNSCREENED
         else
            bincode = PROD
         endif
         goto 1000
      endif
C     sort on the screened event list key
      if(index(filename,'.evt')       .ne. 0)then
         if(index(filename,'h.')      .ne. 0 .or. 
     *      index(filename,'m.')      .ne. 0 .or. 
     *      index(filename,'l.')      .ne. 0 .or.
     *      index(filename,'H.')      .ne. 0 .or. 
     *      index(filename,'M.')      .ne. 0 .or. 
     *      index(filename,'L.')      .ne. 0     )then
            bincode = SCREENED
         else
            bincode = PROD
         endif
         goto 1000
      endif
C     sort on the image file key
      if(index(filename,'.img')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the detector image file key
      if(index(filename,'.detimg')    .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the gif file key
      if(index(filename,'.gif')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the jpeg file key
      if(index(filename,'.jpeg')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the pds file key
      if(index(filename,'.pds')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the pic file key
      if(index(filename,'.pic')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the pixel file key
      if(index(filename,'.pxl')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the config file key
      if(index(filename,'config.')    .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the attitude file key
      if(index(filename,'fa')         .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the orbit file key
      if(index(filename,'.orbit')     .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the mkfilter file key
      if(index(filename,'.mkf')       .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the imkfilter file key
      if(index(filename,'.imkf')      .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the housekeeping file key
      if(index(filename,'HK.')        .ne. 0 .or. 
     *     index(filename,'hk.')      .ne. 0    )then
         bincode = AUX
         goto 1000
      endif
C     sort on the gain history file key
      if(index(filename,'.ghf')       .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the dfe file key
      if(index(filename,'dfe')        .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the leaptable file key
      if(index(filename,'leaptable')  .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the ascalin file key
      if(index(filename,'ascalin')    .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the flf file key
      if(index(filename,'flf.')       .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the ano file key
      if(index(filename,'ano')        .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the rigid file key
      if(index(filename,'rigid')      .ne. 0)then
         bincode = AUX
         goto 1000
      endif
C     sort on the .par file key
      if(index(filename,'.par')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the tape file key
      if(index(filename,'tape')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the text file key
      if(index(filename,'.txt')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the postsript file key
      if(index(filename,'.ps')        .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the output file key
      if(index(filename,'.out')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the log file key
      if(index(filename,'.log')       .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the fits wrapped file key
      if(index(filename,'.wrap')      .ne. 0)then
         bincode = PROD
         goto 1000
      endif
C     sort on the ft file key
      if(index(filename,'ft')         .ne. 0)then
         if(index(filename,'.fits')   .ne. 0)then
            bincode = RAW
         else
            bincode = TELEM
         endif
         goto 1000
      endif
C
C     return to the calling program
C
 1000 return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC





