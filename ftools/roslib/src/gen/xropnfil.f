c
      subroutine xropnfil(infile, outfile, ichat, lui, luo,
     &                     iext,itype,ierr)

c OPeN a fits FILe

c This routine is used for tasks that open existing files to read timing
c data.

c The input file is opened read-only.

c If outfile is nonblank, the routine also copies the input file
c to file and output file with write access.  The output file name
c also must not be 'null', 'NULL', or the same as the input file name.
c The latter case will result in a fatal error; the others are ignored.

c The logical unit obtained with getlun is returned in lui.  If there
c is an output file, the same is true for luo.

c If iext > 0, the routine will assume that iext is the extension number
c of a rate table.

c If iext < or = 0, the routine will search automatically for a rate table
c and a GTI extension (by calling subroutine xrgetext).

c The input filename can have an extension number as well, in the form
c filename[ext#] or filename+ext#.  The same rules apply as if iext > 0,
c but iext takes precedence.

c
c I infile      C input file name plus [ext]
c I outfile     C output file 
c I ichat       I chat for xwrite
c O lui         I unit input file 
c O luo         I unit output file 
c O iext(2)     I iext(1)= EVENT or RATE iext(2)=GTI
c O itype       I = 1 for event lists, 2 (df) for binned data
c O ierr        I error return

c Author: la/el  NASA/GSFC  July 1994
c
      IMPLICIT NONE
c Input variable 
      CHARACTER*(*) infile, outfile
      INTEGER*4 ichat
c
c Output variable
      INTEGER*4 lui, luo, iext(*),ierr,itype
c
c Local variable
      INTEGER*4 next, block, ftstat, idum,i,htype
      character(160) inputf
      character(80) errm,string
      DATA string /' '/
c
c  
c Search for extention specification in input file 
      next=-99
      CALL xrstrext(infile,inputf,next,ierr)
      if(ierr.ne.0) RETURN
c
c open input file 
      CALL getlun(lui)
      block=0
      ftstat=0
      CALL ftopen(lui, inputf, 0, block, ftstat)
      IF (ftstat.NE.0) THEN
         errm= 'ERROR opening input file' // inputf
         ierr=ftstat 
         goto 999
      ENDIF

c >>> Here an input iext(1) (as from the RT option in xronos) can override
c     next as stripped from the filename.<<<
      IF(iext(1).le.0) iext(1) = next
      CALL xrftgext(lui,ichat,string,2,iext,itype,idum,ierr)
      if(ierr.ne.0) RETURN

c Open an output file if requested.
      IF((outfile.ne.' ').and.(outfile.ne.'null').and.
     &   (outfile.ne.'NULL')) THEN
c check if outfile = infile fatal error   
         IF(inputf.EQ.outfile) THEN 
            errm = 'Output filename must be different from input'
            ierr=2
            goto 999
         ENDIF  
c copy input into output
         CALL xwrite('Copying to output file...',ichat)
c         CALL xcopy(inputf,outfile)
c         CALL xwrite('...done',ichat)
c open output file 
         CALL getlun(luo)
         CALL ffinit(luo,outfile,ftstat)
         IF (ftstat.NE.0) THEN
            errm='ERROR creating output file, the file may exist'
            ierr=ftstat 
            goto 999
         ENDIF

         i=1
         htype=0
         CALL ftmahd(lui,i,htype,ftstat)
         DO WHILE(ftstat.EQ.0)
         CALL ftcopy(lui,luo,0,ftstat)
         IF (ftstat.NE.0) THEN
            errm='ERROR copying output file'
            ierr=ftstat 
            goto 999
         ENDIF
         i=i+1
         CALL ftmahd(lui,i,htype,ftstat)
	 ENDDO
     
         IF(ftstat.eq.107) THEN
            ftstat=0
         ELSE
            errm='ERROR opening output file'
            ierr=ftstat 
            goto 999
         ENDIF
        
         CALL ftmahd(luo,1,htype,ftstat)

         CALL xwrite('...done',ichat)
c         CALL ftopen(luo, outfile, 1, block, ftstat)
         IF (ftstat.NE.0) THEN
            errm='ERROR opening output file'
            ierr=ftstat 
            goto 999
         ENDIF
      ENDIF
c
      goto 1000
999   CONTINUE
      CALL xaerror(errm,5)
1000  CONTINUE

      RETURN
      END
