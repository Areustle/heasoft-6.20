*+RDREG1

      subroutine rdreg1(iunit,char_no,maxpoints,line_no,comm_line,
     >                  shape_no,shape,npoints,points,sign,
     >                  chatter,errflg)

c ------------------------- description ------------------------------
c READ_REGION reads the input region file, extracts the information
c and passed to the called routine.
c
c ------------- variable definitions ---------------------------
       implicit none
       character*(*) comm_line(*)
       character*(*) shape(*)
       character*(*) sign(*)
       integer maxpoints
       real points(maxpoints,maxpoints)
       integer iunit,line_no,shape_no
       integer npoints(*),char_no(*)
       integer chatter,errflg

c ----------------------- internal variables ---------------------
       character(200) line,temp
       character(255) subinfo
       character(20) char1,char_points(100,100)
       integer i,count,end,start

c ----------------------- variable directory ---------------------
c iunit     int  i/p  unit no. of input file
c char_no   int  o/p  no. of letters in the particular shape
c maxpoints int  o/p  maximum number of shapes and maximum number of
c                     points for a particular shape
c line_no   int  o/p  no. of comment lines
c comm_line char o/p  content of comment lines
c shape_no  int  o/p  no. of shapes in the input region file
c shape     char o/p  the shape of a particular region
c                     six shapes are supported at present:
c                     1. CIRCLE
c                     2. BOX
c                     3. POLYGON
c                     4. POINT
c                     5. ELLIPSE
c                     6. ANNULUS
c npoints   int  o/p  no. of parameters inside shape
c points    real o/p  parameters for defining shape
c                     points(1,1),points(1,2) are the x and y coordinate
c                     of shape_no=1
c sign      char o/p  sign preceeding the shape -> " " , "!", or " "
c errflg    int  o/p  error flag defining error status
c
c ----------------- internal variable directory --------------------
c
c line        char  content of the line read
c temp        char  assign read character to temp
c char1       char  temporary character save
c char_points char  the parameter values read in character
c i           int   count for do loop
c count       int   count of the lines read
c end         int   end point of a character in the line read
c start       int start point of a character in the line read
c
c ----------------------- called routines -----------------------
c    none
c --------- author/modifications --------------------------------
c
c Banashree Mitra Seifert (Feb 1996) 1:0:0
c
c ---------------------------------------------------------------
       character(5) version
       parameter (version='1.0.0')
       character(7) subname
       integer len_trim
*_

       subname = 'rdreg1'
       subinfo='using '//subname//version
       call wtinfo(chatter,10,2,subinfo)

c ------------------------- initialisation -----------------------------
      line_no=0
      count = 0
      shape_no=0
 1    count=count+1
      read(iunit,'(a)',end=200) line
           call ftupch(line)
           if (line(1:1) .eq. '#') then
               line_no=line_no+1
               comm_line(line_no)= line
               goto 1
           elseif((line(1:1) .eq. '-') .or.
     >            (line(1:1) .eq. '!')) then
                  shape_no=shape_no+1
                  sign(shape_no) = line(1:1)
           elseif((line(1:1) .eq. ' ') .or.
     >            (line(1:1) .eq. '+')) then
                  shape_no=shape_no+1
                  sign(shape_no) = line(1:1)
           else
                  subinfo ='Input region file "' //
     >               line(1:len_trim(line)) //
     >               '" must begin with "-", "!", or " ": Aborting'
                  call wterrm(subname,version,subinfo)
                  errflg = 1
                  return
           endif

c -----------------------------------------------------------------
c excluded or included region is found by line(1:1)
c Now to find out the first starting point after '('
c -----------------------------------------------------------------

      do i=1,20
         char1 = line(i:i)
         if (char1 .eq. '(' ) then
             goto 2
         endif
      enddo

c now we have region,ie, BOX,CIRCLE,POLYGON,POINTS,ELLIPSE, or ANNULUS

 2    end = i-1
      char_no(shape_no) = end-1
      if (line(2:end) .eq. 'CIRCLE') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'BOX') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'ELLIPSE') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'POINT') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'POLYGON') then
               shape(shape_no)=line(2:end)

       elseif (line(2:end) .eq. 'ANNULUS') then
               shape(shape_no)=line(2:end)

       else
               subinfo='Unknown region: '//line(1:end)
               call wterrm(subname,version,subinfo)
               errflg=1
               return
       endif


c Now get all the points

c move past '('

      start = end+1
      npoints(shape_no)=0
20    do i=1,10
         temp = line(start+i:start+i)
         if (temp .eq. ',') then
             npoints(shape_no)=npoints(shape_no)+1
             end=start+i-1
             char_points(shape_no,npoints(shape_no)) = line(start+1:end)
             start = end+1
             goto 20
         elseif (temp .eq. ')') then
                 npoints(shape_no)=npoints(shape_no)+1
                 end=start+i-1
                 char_points(shape_no,npoints(shape_no)) =
     >                                             line(start+1:end)
                 goto 6
         endif
      enddo


 6    do i=1,npoints(shape_no)
         read (char_points(shape_no,i),*,iostat=errflg)
     >                                          points(shape_no,i)
      enddo

      goto 1

 200  return
      end
c --------------------------------------------------------------
c             end of read_region subroutine
c --------------------------------------------------------------

