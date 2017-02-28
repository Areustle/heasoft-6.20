      subroutine exomergemake(evtfile,outfile,
     &     ichat, clobber,ierr)
c
c
c INITialize files and parameters for program exomerge
c read parameters in the parameters file.
c
c  I  evtfile   (d)  list of event files to merge
c  I  outfile   (d)  merged output file
c  I  tchat     (i)  Chattiness terminal
c  I  lchat     (i)  Chattiness log 
c  I  ierr      (i)  Error return
c  I  clobber   (l)  Overwrite output file if it exists?
c
      IMPLICIT NONE
C Input 
      INTEGER*4 ichat, ierr
      CHARACTER*(*) outfile, evtfile
      LOGICAL clobber
c 
c Local variable 
c dynamic memory
      INTEGER*4 shftime,x,y,pha,exptime,frac,dead,obctime,obcshftime,
     &     prtime,bctime,bad,samples

      INTEGER*4 nfiles,status,npoint,i,tstart,tstop
      INTEGER*4 tlist(256,2),first_obs,last_obs
      INTEGER*4 fracexpcnt,fracnt,npointsum,nevents
      INTEGER*4 nexp,nobc,tmid,telapse,gtilist(256,2),ngti
      INTEGER*4 ontime,hkstart(99)
      
      character(160) subname,cfile(256),subprog
      character(255)  errm ,context,extname,string
      character(80) star,stimekeys(4)
      PARAMETER (subname = 'exomergemake:')
      REAL fracexpsum,fracsum,tsort(256),tshuf(256),deadc,minwait
      REAL*8 expt,inttime,ra_nom,dec_nom,roll90
      LOGICAL image,first
      include 'dmcommon.inc'
c      DATA disk /' '/
c
      extname="OBC_PACKET"
      errm = ' '
      fracexpsum=0.0
      fracsum=0.0
      fracexpcnt=0
      fracnt=0
      expt=0.0
      inttime=0.0
      image=.false.
      first=.true.
      status=0
      nfiles=0
      npoint=0
      npointsum=0


c
c Open the @ file. Get the names of all event files from evtfile and store
c in cfile(nfiles)
c
      subprog="GETFILES"
      CALL xwrite(subprog,15) 
      CALL getfiles(evtfile, cfile, nfiles, status)

      IF (status.ne.0) Then 
         context=' Problem in opening the filelist'
         errm= subname//' '//context
         GOTO 999
      ENDIF
      
C Checks that all files have the same filter,instrument,ST mode, etc.
      subprog="EXSTARMODE"
      CALL xwrite(subprog,15) 

      CALL exstarmode(cfile,nfiles,star,tlist,nevents,nexp,nobc,ontime,
     &     deadc,minwait,clobber,outfile,status)
      IF (status.ne.0) Then 
         context=' Problem in EXSTARMODE'
         errm= subname//' '//context
         GOTO 999
      ENDIF

      write(string,5) nfiles
 5    format("Number of input event files     ",I3)
      CALL xwrite(string,10)
      do i=1,nfiles
         string=cfile(i)
         CALL RMVXBK(string)
         CALL xwrite(string,10)
      enddo
      string="ST Mode= "//star
      CALL RMVXBK(string)
      CALL xwrite(string,10)

      do i=1,nfiles
         tshuf(i)=i
         tsort(i)=tlist(i,1)
      enddo
C
C Find the the tstart and tstop values just in case they are not in
C chronological order
C SORT2 is a XANLIB routine
      if(nfiles.gt.1) then
         subprog="SORT2"
         CALL xwrite(subprog,15) 
         CALL sort2(nfiles,tsort,tshuf)
C Re-order all the filenames and times based on tshuf

         subprog="SHUFFLE"
         CALL xwrite(subprog,15) 
         CALL exshuffle(nfiles,tshuf,cfile,tlist)
      endif
      tstop=tlist(nfiles,2)
      last_obs=nfiles
      tstart=tlist(1,1)
      first_obs=1
      telapse=tstop-tstart
      tmid=(tstop+tstart)/2
      write(string,40) tstart,tstop,tmid
 40   format("SHF: Start time= ",I9," Stop time= ",I9," Midpoint= ",I9)
      CALL xwrite(string,10)
c
c     Use the first_obs,last_obs to get the information for the DATE-
c     and TIME- keywords

      subprog="EXTIMEKEY"
      CALL xwrite(subprog,15) 
      CALL extimekey(cfile,first_obs,last_obs,stimekeys,status)
C
C     
C If the mode is STR1 then the RA, DEC keywords need to be modified and
c  one of the HK files need to be read. 
C 
      if(star.eq."STR1") then
         subprog="EXFSSCO"
         CALL xwrite(subprog,15) 
         CALL exfssco(tmid,tlist,cfile,nfiles,ra_nom,dec_nom,roll90,
     &        status)
      endif

C Read the GTI table.
C

      subprog="EXOGTI"
      CALL xwrite(subprog,15) 

      CALL exogti(cfile,nfiles,gtilist,ngti)

C
C Merge the event, exposure and obc_packet tables
C   First allocate sufficent dynamic memory to store all of the data
C
      subprog="MALL - allocate"
      CALL xwrite(subprog,15) 
      call mall(nevents,shftime,x,y,pha,nexp,exptime,frac,dead,nobc,
     &     obctime,obcshftime,prtime,bctime,bad,samples,1,status)
      IF (status.ne.0) Then 
         context=' Problem allocating memory'
         errm= subname//' '//context
         GOTO 999
      ENDIF

C Read and merge all the event files
      subprog="EXMERGE"
      CALL xwrite(subprog,15) 
      CALL exmerge(cfile,nfiles,nevents,memd(shftime),memi(x),
     &     memi(y),memi(pha),nexp,memd(exptime),memr(frac),
     &     memr(dead),nobc,memd(obctime),memi(obcshftime),memd(prtime),
     &     memr(bctime),mems(bad),mems(samples),hkstart,status)

      write(string,10) nevents
 10   format("Total number of events          ",I9)
      CALL xwrite(string,10)
      write(string,20) nexp
 20   format("Total number of exposure rows   ",I9)
      CALL xwrite(string,10)
      write(string,30) nobc
 30   format("Total number of obc_packet rows ",I9)
      CALL xwrite(string,10)

C Write the merged arrays to the output FITS files
      subprog="EXOWRITE"
      CALL xwrite(subprog,15) 
      CALL exowrite(star,outfile,cfile,nfiles,tlist,stimekeys,ra_nom,
     &     dec_nom,ontime,deadc,minwait,
     &     roll90,ngti,gtilist,nevents,memd(shftime),memi(x),
     &     memi(y),memi(pha),nexp,memd(exptime),memr(frac),
     &     memr(dead),nobc,memd(obctime),memi(obcshftime),memd(prtime),
     &     memr(bctime),mems(bad),mems(samples),hkstart,clobber,status)

C Deallocate memory
      subprog="MALL - deallocate"
      CALL xwrite(subprog,15) 
      call mall(nevents,shftime,x,y,pha,nexp,exptime,frac,dead,nobc,
     &     obctime,obcshftime,prtime,bctime,bad,samples,2,status)
      IF (status.ne.0) Then 
         context=' Problem deallocating memory'
         errm= subname//' '//context
         GOTO 999
      ENDIF
      GOTO 1000

999   CONTINUE
      CALL RMVXBK(errm)
      call xerror(errm,5)
1000  CONTINUE
      RETURN
      END
