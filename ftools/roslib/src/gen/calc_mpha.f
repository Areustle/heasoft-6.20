
*+
c     ------------------------------------------------------ 
      subroutine calc_mpha(alklun,strt,stp,mean_pha,n_alk,
     &                 iscc_col,alk_col,errflg,chatter)
c     ------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------
c This routine calculates the mean pha channel using the ALK HIST
c FITS extension.
c --- VARIABLES -------------------------------------------------
c
      IMPLICIT NONE
      integer errflg,chatter,n_alk
      real mean_pha
      real*8 strt,stp
      integer alk_col, iscc_col,alklun
c
c --- VARIABLE DIRECTORY -----------------------------------------
c
c errflg      int   : error status flag
c chatter     int   : verbose/quietness flag
c strt        int   : gti start time
c stp         int   : gti stop time
c n_alk       int   : Number of values in alkfile
c mean_pha    real  : mean pha channel (o)
c alk_col     int   : ALK_BIN column number
c iscc_col    int   : ISCC column number
c alklun      int   : file unit number
c
c --- AUTHORS/MODIFICATION HISTORY ---
c Rehana Yusaf (1995 OCT ) 1.0.0;
c Rehana Yusaf (1995 Oct 19) 1.0.1; use dummy value for first
c                                   before
c Rehana Yusaf (1996 Feb 23) 1.0.2; add wtinfo et al
c
c Banashree M Seifert (1996 Sept) 1.1.0:
c         . formatting error in write corrected
c -----------------------------------------------------------------
      character(5) version
      parameter (version = '1.1.0')
      character(9) subname
      parameter (subname = 'calc_mpha')
*-
c ---------------------------------------------------------------
c
      character(80) desc
      logical time_match,anyf
      real*8 mean_time
      integer i,inull,before,after,status,alkpos
      real enull,alk_before,alk_after

      DATA alkpos /0/

      desc = ' using '//subname//' '//version
      call wtinfo(chatter,10,3,desc)

      mean_time = (strt+stp)/2
      IF (alkpos.GT.3) THEN
        i = alkpos - 2
        status = 0
        call ftgcvj(alklun,iscc_col,i,1,1,inull,before,
     &  anyf,status)
        desc = ' reading ISCC column'
        call wtferr(subname,version,status,desc)
        IF (status.NE.0) THEN
          errflg = 1
          return
        ENDIF
      ELSE
        i = 1
        before = 0
      ENDIF
      time_match = .false.
      do WHILE((.NOT.time_match).AND.(i.LT.n_alk))
        i = i + 1
        status = 0
        call ftgcvj(alklun,iscc_col,i,1,1,inull,after,anyf,status)
        desc = ' reading ISCC column'
        call wtferr(subname,version,status,desc)
        IF (status.NE.0) THEN
          errflg = 1
          return
        ENDIF
        IF ((before.LE.mean_time).AND.(after.GE.mean_time)) THEN
          status = 0
          call ftgcve(alklun,alk_col,i-1,1,1,enull,alk_before,
     &    anyf,status)
          status = 0
          call ftgcve(alklun,alk_col,i,1,1,enull,alk_after,
     &    anyf,status)
          desc = ' reading ALK column'
          call wtferr(subname,version,status,desc)
          IF (status.NE.0) THEN
            errflg = 1
            return
          ENDIF
          mean_pha = alk_before + ((alk_after - alk_before) * 
     &               ((mean_time - before)/(after - before)))
          time_match = .true.
        ENDIF
        before = after 
       enddo
       alkpos = i

       write(desc,'(a,f10.2)') ' Calculated mean_pha :', mean_pha
       call wtinfo(chatter,20,3,desc)
       return
       end
c --------------------------------------------------------------------
c      END OF CALC_MPHA
c -------------------------------------------------------------------- 
        
