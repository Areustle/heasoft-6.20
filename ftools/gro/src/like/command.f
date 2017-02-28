C       SUBROUTINE COMMAND(input)
C
C  $Id: command.f,v 1.6 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++     effect: Interpret commands for LIKE program, call appropriate
c               subroutines.
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     character input*50   Command text typed by user on console

C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
c
C=======================================================================
C  $Log: command.f,v $
C  Revision 1.6  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.5  2007/01/31 21:04:41  irby
C  Fix lines that were too long (>72 chars).
C
C  Revision 1.4  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.3  2002/12/26 17:42:53  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
C
C  Revision 1.2  2002/04/18 19:34:08  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:29  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.10  1997/02/03  14:49:53  jae
c Removed local variables which were not in use
c
c Revision 5.9  1996/10/14  16:22:26  jae
c fixed typo due to previous edit on
c line 984.
c
c Revision 5.8  1996/10/14  16:19:34  jae
c Added option to output PSF map summed into
c the model for flux or counts maps.
c
c Revision 5.7  1996/07/18  19:47:01  jae
c Repaired typo's on line 863 and 874
c
c Revision 5.6  1996/07/02  21:02:49  jae
c changed command 'CR' to 'COR' to avoid
c possible typos (caused by 'RC' command
c mis-typing).
c
c Revision 5.5  1996/06/25  17:33:57  jae
c Changed "CR" command to set the test source
c (PSF) map to null, reset the sv_dstsv array
c and set all test source (PSF) counts to 0.25
c
c Revision 5.4  1996/06/09  00:18:58  jae
c Added rebuild of PSF map in new command 'CR'
c
c Revision 5.3  1996/06/09  00:15:30  jae
c Added command 'CR' which resets all PSF
c array counts to 0.5
c
c Revision 5.2  1996/02/29  21:05:27  jae
c Added call to routine ERROR(0,LOC) after return from
c subroutine MAPFINE.  This is a check against returns
c on error from MAPFINE.  This should be added to many
c of the single line function calls within this program
c at some future date.
c
c Revision 5.1  1996/02/29  20:47:16  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/14  22:55:06  jae
c Subroutine Module for like V5.00
c
C%   Changes:
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE LIKECOMMAND(input)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/nmprep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
      character input*50,moreinput*70
      logical determined,FEXIST
      real PSFARRTMP(100)


      id = '$Id: command.f,v 1.6 2013/05/21 19:08:25 irby Exp $'
      LOC='COMMAND'

      IF (input(1:1).EQ.'P'.or.input(1:1).EQ.'p') THEN
c
c     if 2:2 == 'A'
c
         if (input(2:2).EQ.'A'.or.input(2:2).EQ.'a') then
            call psfadj
c
c     elseif 2:2 == 'B'
c
         elseif (input(2:2).EQ.'B'.or.input(2:2).EQ.'b') then
            write(lu(1),'(
     &           "Input . to reset diffuse  model to null; ",/,
     &           "Gbias to be added to diffuse  model (cr for",
     &           f12.6,"):",$)')Gbias
            if (Restrict_Gbias)write(lu(1),'("Gbias is restricted ",
     &           "and the value above will be added to the ",
     *           "diffuse model")')
            READ(LU(12),'(A)')input
c     
c     if-then-else-endif for '.' reset
c     
            if (input.eq.'.') then
               call MAPRST(GASMAP,CTLMSZ1,CTLMSZ2)
               LikTotaled=.false.
               write(lu(1),*)'Setting GASMAP to null'
               GASDOC(1)='Diffuse  model set to null.'
               do n=1,9
                  GASDOC(n)=' '
               enddo
               gmap_null=.true.
            else
               if (input.ne.' '.and..not.Restrict_Gbias)
     &              read(input,*,end=1111)Gbias
               CALL MAPRST(TMPMAP,CTLMSZ1,CTLMSZ2)
               CALL MAPADD(Gbias*1.e-5,TMPMAP,CTLMSZ1,CTLMSZ2)
               call error(1,loc)
               CALL MAPMLT(TMPMAP,EMAP,CTLMSZ1,CTLMSZ2)
               call error(1,loc)
               CALL MAPSUM(gasmap,TMPMAP,CTLMSZ1,CTLMSZ2)
               call error(1,loc)
               LikTotaled=.false.
               write(input,'("Gbias=",f12.6," *1e-5 added. ")')Gbias
               nchar=32
               call DOCADD(nchar,GASDOC,GASTYP,input)
               write(lu(1),*)input
            endif
c
c     end '.' reset
c
c
c     elseif 2:2 == 'G'
c

         elseif (input(2:2).EQ.'G'.or.input(2:2).EQ.'g') then
            write(lu(1),'(
     &           "Input desired spectral index for PSF (cr for ",
     &           f4.2,":",$)') gamma
            READ(LU(12),'(A)')input
            if (input.ne.' ') then
               read(input,*,end=1111)gamma
               CALL PSMGET
               CALL ERROR(1,LOC)
               LikTotaled=.false.
            endif
c
c     elseif 2:2 == 'M'
c
         elseif (input(2:2).EQ.'M'.or.input(2:2).EQ.'m') then
c
c     if 3:3 == 'V'
c 
            if (input(3:3).eq.'v'.or.input(3:3).eq.'V') then
               if (verbose) then
                  verbose=.false.
                  write(lu(1),*)'Turning VERBOSE flag off.'
               else
                  verbose=.true.
                  write(lu(1),*)'Turning VERBOSE flag on.'
               endif
c
c     elseif 3:3 == 'N'
c
            elseif (input(3:3).eq.'n'.or.input(3:3).eq.'N') then
 105           write(LU(1),*)' '
               if (Restrict_Gbias)WRITE(LU(1),*)'Gbias is restricted'
               if (Restrict_Gmult)WRITE(LU(1),*)'Gmult is restricted'
               write(lu(1),'(
     &              "Enter Gmult_nom,Gbias_nom ",
     *              "(cr for ",2f10.2,"):",$)')
     &              Gmult_nom/100.,Gbias_nom/100.
               READ(LU(12),'(a)') input
               numcar = index(input, ' ') - 1
               
               if (numcar.eq.0) return
c
c     ->	if input !=  ' '
c
               if (numcar.ne.0) then
                  read(input,*,end=105,err=105)
     &                 Gmult_nom_tmp,Gbias_nom_tmp
		  Gmult_nom=Gmult_nom_tmp
		  Gbias_nom=Gbias_nom_tmp
		  Gmult_nom=Gmult_nom*100.
		  Gbias_nom=Gbias_nom*100.
c
c     ->	->	if Restrict_Gmult
c
		  if (Restrict_Gmult) then
                     write(LU(1),
     &                    '("Gmult is restricted.  Set this as new ",
     *                    "value of Gmult ? <cr> for Yes: "$)')
                     read(LU(12),'(A)')input
                     if (input.eq.' '.or.input(1:1).eq.'Y'.or.
     &                    input(1:1).eq.'y') then
                        Gmult_nomj=Gmult_nom
                        Gmult=Gmult_nom/100.
                     endif
                  endif
c
c     ->	->	endif Gmult
c
c     
c     ->	->	If Restrict_Gbias
c
		  if (Restrict_Gbias) then
                     write(LU(1),
     &                    '("Gbias is restricted.  Set this as new ",
     *                    "value of Gbias ? <cr> for Yes: "$)')
                     read(LU(12),'(A)')input
                     if (input.eq.' '.or.input(1:1).eq.'Y'.or.
     &                    input(1:1).eq.'y') then
                        Gbias_nomj=Gbias_nom
                        Gbias = Gbias_nom/100.
                     endif
		  endif
c
c     ->	->	endif Gbias
c
               endif
c     
c     ->	endif input != ' '
c
c     elseif 3:3 == 'R'
c
            elseif (input(3:3).eq.'r'.or.input(3:3).eq.'R') then
               write(LU(1),*)' '
               print *,' '
               write(LU(1),'(" Restrict Gmult: ",l2," Restrict Gbias: ",
     &              l1)') Restrict_Gmult, Restrict_Gbias
               write(LU(1),'(" Gmult notification range: ",f5.2,
     &              " to ",f5.2)')Gmult_min,Gmult_max
               write(LU(1),'(" Gbias notification range: ",f5.2,
     &              " to ",f5.2)')Gbias_min,Gbias_max
               if (.not.Restrict_notice) then
                  write(LU(1),'(" User is notified if range ",
     *                 "is violated")')
               else
                  write(LU(1),'(" User is NOT notified if range ",
     *                 "is violated")')
               endif
               write(LU(1),*)' '
 107           write(LU(1),*)' '
               write(lu(1),'(
     &              "Parameter restriction adjustment menu:",/,/,
     &              "Enter: 1 to adjust Gmult; 2 to adjust Gbias;")')
c     &"3 to adjust Counts; ")')
c     
c     
               moreinput(1:5)='     '
               if (Restrict_Gmult)
     &              write(lu(1),'("Enter: M to change Restricted ",
     *              "Gmult value;")')
               write(LU(1),'("Enter: MR to change notification ",
     *              "range of Gmult")')
               if (Restrict_Gbias)
     &              write(lu(1),'("Enter: B to change Restricted ",
     *              "Gbias value;")')
               write(LU(1),'("Enter: BR to change notification range ",
     &              "of Gbias")')
               print *,'Enter: N to toggle the value of the',
     &              ' notification flag. Present value: ',
     *              (.not.Restrict_notice)
               write(LU(1),'(" cr to return: ",$)')
               READ(LU(12),'(A)')moreinput
               if (moreinput(1:1).eq.'n')moreinput(1:1)='N'
               if (moreinput(1:1).eq.'b')moreinput(1:1)='B'
               if (moreinput(1:1).eq.'m')moreinput(1:1)='M'
               if (moreinput(2:2).eq.'r')moreinput(2:2)='R'
               if (moreinput(1:1).eq.' ') then
                  write(LU(1),*)' '
                  print *,' '
                  write(LU(1),'(" Restrict Gmult: ",l2,
     &                 " Restrict Gbias: ",l2)') 
     &                 Restrict_Gmult, Restrict_Gbias
                  write(LU(1),'(" Gmult notification range: ",f5.2,
     &                 " to ",f5.2)')Gmult_min,Gmult_max
                  write(LU(1),'(" Gbias notification range: ",f5.2,
     &                 " to ",f5.2)')Gbias_min,Gbias_max
                  if (.not.Restrict_notice) then
                     write(LU(1),'(" User is notified ",
     *                    "if range is violated")')
                  else
                     write(LU(1),'(" User is NOT notified if range is ",
     &                    "violated")')
                  endif
                  write(LU(1),*)' '
                  return
c     
c     toggle notice restriction flag
c     
               elseif (moreinput(1:1).eq.'N') then
                  if (Restrict_notice) then
                     Restrict_notice=.false.
                  else
                     Restrict_notice=.true.
                  endif
                  if (.not.Restrict_notice) then
                     write(LU(1),'(" User is notified if range ",
     *                    "is violated")')
                  else
                     write(LU(1),'(" User is NOT notified if range ",
     *                    "is violated")')
                  endif
c     
c     
c     restrict Gmult block
c     
               elseif (moreinput.eq.'1' .or. moreinput(1:1).eq.'M') then
                  if (Restrict_Gmult .and. moreinput(1:1).ne.'M') then
                     write(LU(1),*)' '
                     write(Lu(1),'("You are changing the restriction ",
     &                    "value of Gmult from true to false.")')
                     determined=.false.
                     call intent(determined)
                     if (.not.determined) goto 107
                     Restrict_Gmult=.false.
                     Gmult_nom=Gmult_nom_sav
                     Gmult=Gmult_nom/100.
c     
                     write(LU(1),*)' '
                     write(LU(1),'(" Restrict Gmult: ",l2,
     &                    "   Restrict Gbias: ",l2)') 
     &                    Restrict_Gmult,Restrict_Gbias
                     write(LU(1),'(" Gmult notification range: ",f5.2,
     &                    " to ",f5.2)') Gmult_min,Gmult_max
                     write(LU(1),'(" Gbias notification range: ",f5.2,
     &                    " to ",f5.2)') Gbias_min,Gbias_max
                     if (.not.Restrict_notice) then
                        write(LU(1),'(" User is notified if range ",
     *                       "is violated")')
                     else
                        write(LU(1),'(" User is NOT notified if range ",
     *                       "is violated")')
                     endif
                     write(LU(1),*)' '
c     
c
                  else
                     if (moreinput(1:2) .eq. 'MR') then
                        write(LU(1),*)' '
                        write(LU(1),'("Enter Gmult min and Gmult max")')
                        write(LU(1),
     *                       '("<cr> to accept ",f5.2," < Gmult < ",
     &                       f5.2," : "$)') Gmult_min,Gmult_max
                        read(LU(12),'(a)') input
                        numcar = index(input, ' ') - 1
                        if (input.eq.' ') goto 109
                        Gmult_min_sav=Gmult_min
                        Gmult_max_sav=Gmult_max
                        read(input,*,end=110)
     *                       Gmult_min,Gmult_max
 109                    write(LU(1),*)' '
                        write(LU(1),'(" Restrict Gmult: ",l2,
     &                       "   Restrict Gbias: ",
     *                       l2)') Restrict_Gmult, Restrict_Gbias
                        write(LU(1),'(" Gmult notification range: ",f5.2
     &                       ," to ",f5.2)') Gmult_min,Gmult_max
                        write(LU(1),'(" Gbias notification range: ",f5.2
     &                       ," to ",f5.2)') Gbias_min,Gbias_max
                        if (.not.Restrict_notice) then
                           write(LU(1),'(" User is notified if range ",
     *                          "is violated")')
                        else
                           write(LU(1),'(" User is NOT notified if ",
     *                          "range is violated")')
                        endif
                        write(LU(1),*)' '
c     
                        goto 107
 110                    write(LU(1),*)' INPUT ERROR '
                        write(LU(1),'("Your input: ",a)')
     *                       input(1:numcar)
                        write(LU(1),*)'Restoring previous values'
                        Gmult_min=Gmult_min_sav
                        Gmult_max=Gmult_max_sav
                        goto 109
                     endif
                     if (.not.Restrict_Gmult) then
                        write(LU(1),*)' '
                        write(Lu(1),'("Do you wish to change the ",
     *                       "restriction value of Gmult from false ",
     *                       "to true?")')
                        call intent(Restrict_Gmult)
                        if (.not.Restrict_Gmult) goto 107
                     endif
                     write(LU(1),*)' '
                     write(lu(1),'("Enter new value of Gmult (or ",
     &                    " <cr> for ",f6.3,") : "$)') Gmult
                     READ(LU(12),'(A)')input
                     if (input.ne.' ') then
                        read(input,*,end=1111)Gmult
                     endif
                     if (Restrict_Gmult) then
                        Gmult_nomj=Gmult*100.
                        Gmult_nom=Gmult*100.
                     endif
                     write(LU(1),*)' '
                     write(LU(1),'(" Restrict Gmult: ",l2,
     &                    "   Restrict Gbias: ",l2)') 
     &                    Restrict_Gmult,Restrict_Gbias
                     write(LU(1),'(" Gmult notification range: ",f5.2,
     &                    " to ",f5.2)') Gmult_min,Gmult_max
                     write(LU(1),'(" Gbias notification range: ",f5.2,
     &                    " to ",f5.2)') Gbias_min,Gbias_max
                     if (.not.Restrict_notice) then
                        write(LU(1),'(" User is notified if range is ",
     *                       "violated")')
                     else
                        write(LU(1),'(" User is NOT notified if range ",
     *                       "is violated")')
                     endif
                     write(LU(1),*)' '
                  endif
c     
c     restrict Gbias block
c
               elseif (moreinput.eq.'2' .or. moreinput(1:1).eq.'B') then
                  if (Restrict_Gbias.and.
     *                 moreinput(1:1).ne.'B') then
                     write(LU(1),*)' '
                     write(Lu(1),'("You are changing the restriction ",
     &                    "value of Gbias from true to false")')
                     determined=.false.
                     call intent(determined)
                     if (.not.determined) goto 107
                     Restrict_Gbias=.false.
                     Gbias_nom=Gbias_nom_sav
                     write(LU(1),*)' '
                     write(LU(1),'(" Restrict Gmult: ",l2,
     &                    "   Restrict Gbias: ",l2)') 
     *                    Restrict_Gmult,Restrict_Gbias
                     write(LU(1),'(" Gmult notification range: ",f5.2,
     &                    " to ",f5.2)') Gmult_min,Gmult_max
                     write(LU(1),'(" Gbias notification range: ",f5.2,
     &                    " to ",f5.2)') Gbias_min,Gbias_max
                     if (.not.Restrict_notice) then
                        write(LU(1),'(" User is notified if range ",
     *                       "is violated")')
                     else
                        write(LU(1),'(" User is NOT notified if range ",
     *                       "is violated")')
                     endif
                     write(LU(1),*)' '
                  else
                     if (moreinput(1:2).eq.'BR') then
                        write(LU(1),*)' '
                        write(LU(1),'("Enter Gbias min and Gbias max")')
                        write(LU(1),
     *                       '("<cr> to accept ",f5.2," < Gbias < ",
     &                       f5.2," : "$)')
     *                       Gbias_min,Gbias_max
                        read(LU(12),'(a)')input
                        numcar = index(input, ' ') - 1
                        if (input.eq.' ') goto 119
                        Gbias_min_sav=Gbias_min
                        Gbias_max_sav=Gbias_max
                        read(input,*,end=120)
     *                       Gbias_min,Gbias_max
 119                    write(LU(1),*)' '
                        write(LU(1),'(" Restrict Gmult: ",l2,
     &                       "   Restrict Gbias: ",
     *                       l2)') 
     *                       Restrict_Gmult,Restrict_Gbias
                        write(LU(1),'(" Gmult notification range: ",f5.2
     &                       ," to ",f5.2)')
     *                       Gmult_min,Gmult_max
                        write(LU(1),'(" Gbias notification range: ",f5.2
     &                       ," to ",f5.2)')
     *                       Gbias_min,Gbias_max
                        if (.not.Restrict_notice) then
                           write(LU(1),'(" User is notified if range ",
     *                          "is violated")')
                        else
                           write(LU(1),'(" User is NOT notified if ",
     *                          "range is violated")')
                        endif
                        write(LU(1),*)' '
                        goto 107
 120                    write(LU(1),*)' INPUT ERROR '
                        write(LU(1),'("Your input: ",a)') 
     *                       input(1:numcar)
                        write(LU(1),*)'Restoring previous values'
                        Gbias_min=Gbias_min_sav
                        Gbias_max=Gbias_max_sav
                        goto 119
                     endif
                     if (.not.Restrict_Gbias) then
                        write(LU(1),*)' '
                        write(Lu(1),'("Do you wish to change the ",
     *                       "restriction value of Gbias from ",
     *                       "false to true? ")')
                        call intent(Restrict_Gbias)
                        if (.not.Restrict_Gbias) goto 107
                     endif
                     write(LU(1),*)' '
                     write(lu(1),'("Enter new value of Gbias (or ",
     &                    " <cr> for ",f6.3,") : "$)') Gbias
                     READ(LU(12),'(A)')input
                     if (input.ne.' ') then
                        read(input,*,end=1111)Gbias
                     endif
                     if (Restrict_Gbias) then
                        Gbias_nomj=Gbias*100.
                        Gbias_nom=Gbias*100.
                     endif
                     write(LU(1),*)' '
                     write(LU(1),'("Restrict Gmult: ",l2,
     &                    " Restrict Gbias: ",a)') 
     *                    Restrict_Gmult,Restrict_Gbias
                     write(LU(1),'(" Gmult notification range: ",f5.2,
     &                    " to ",f5.2)')
     *                    Gmult_min,Gmult_max
                     write(LU(1),'(" Gbias notification range: ",f5.2,
     &                    " to ",f5.2)')
     *                    Gbias_min,Gbias_max
                     if (.not.Restrict_notice) then
                        write(LU(1),'(" User is notified if range ",
     *                       "is violated")')
                     else
                        write(LU(1),'(" User is NOT notified if range ",
     *                       "is violated")')
                     endif
                     write(LU(1),*)' '
                  endif
               elseif ('2'.eq.'1') then
                  if (Restrict_counts) then
                     Restrict_counts=.false.
                     write(lu(1),'("Restrict_counts ",l2)') 
     *                    Restrict_counts
                  else
                     Restrict_counts=.true.
                     write(lu(1),'(
     *                    "Restrict_counts ",l2)') 
     *                    Restrict_counts
                     write(lu(1),'(
     &                    "Enter Counts_min,Counts_max ",
     *                    "(cr for ",2f13.1,"):",$)')
     &                    Counts_min,Counts_max
                     READ(LU(12),'(A)')input
                     if (input.ne.' ') then
                        read(input,*,end=1111)
     *                       Counts_min,Counts_max
                     endif
                  endif
               endif
               goto 107
c     
c     elseif 3:3 == 'L'
c
            elseif (input(3:3).eq.'l'.or.input(3:3).eq.'L') then
               write(lu(1),'("Input delta_lnL,",
     *              "conf_pr,(cr for",2f7.2,")")')
     &              delta_lnL,conf_precent
               READ(LU(12),'(A)')input
               if (input.ne.' ') then
                  read(input,*,end=1111)
     &                 delta_lnL,conf_precent
               endif
c     
c     elseif 3:3 == 'T'
c
            elseif (input(3:3).eq.'t'.or.input(3:3).eq.'T') then
               write(lu(1),*)' '
               write(lu(1),'("Input TS_max & TS_min,",
     *              "(cr for",2f7.2,")")')
     &              TS_max,TS_min
               READ(LU(12),'(A)')input
               if (input.ne.' ') then
                  read(input,*,end=1111)
     &                 TS_max,TS_min
               endif
c     
c     elseif 3:3 == 'Y'
c
            elseif (input(3:3).eq.'y'.or.input(3:3).eq.'Y') then
               if (report) then
                  report=.false.
                  write(lu(1),*)'Turning REPORT flag off.'
               else
                  report=.true.
                  write(lu(1),*)'Turning REPORT flag on.'
               endif
c     
c     elseif 3:3 == 'S'
c
            elseif (input(3:3).eq.'s'.or.input(3:3).eq.'S') then
               if (spectral) then
                  close(99)
                  spectral=.false.
                  write(lu(1),*)'Turning SPECTRAL flag off.'
                  delta_lnL=delta_lnL_old
                  conf_precent=conf_precent_old
                  write(6,'(" Setting upper limit conf. percent to ",
     &                 "the old value: ",f5.1)') 
     *                 conf_percent
               else
                  open(99,file='spectral-output')
                  spectral=.true.
                  write(lu(1),*)'Turning SPECTRAL flag on.'
                  delta_lnL_old=delta_lnL
                  delta_lnL=1.0
                  conf_precent_old=conf_precent
                  conf_precent=84.
                  write(6,'("Setting upper limit conf. percent ",
     &                 "to appropriate value:",f5.1)') 
     *                 conf_precent
               endif
c
c     elseif 3:3 == 'Z'
c
            elseif (input(3:3).eq.'z'.or.input(3:3).eq.'Z') then
               if (debug) then
                  debug=.false.
                  write(lu(1),*)'Turning DEBUG  flag off.'
               else
                  debug=.true.
                  write(lu(1),*)'Turning DEBUG flag on.'
               endif
c     
c     elseif 3:3 == 'P'
c     
            elseif (input(3:3).eq.'P'.or.input(3:3).eq.'p') then
               if (publish) then
                  publish=.false.
               else
                  publish=.true.
               endif
               write(lu(1),'("publish flag set to ",l2)') publish
c     
c     elseif 3:3 == 'Q'
c     
            elseif (input(3:3).eq.'Q'.or.input(3:3).eq.'Q') then
               if (catalog) then
                  catalog=.false.
               else
                  catalog=.true.
               endif
               write(lu(1),'("publish flag set to ",l2)') catalog
c     
c     elseif 3:3 == 'F'
c
            elseif (input(3:3).eq.'F'.or.input(3:3).eq.'f') then
 96            write(lu(1),*)
     &              'Enter name of file to read into the PSF array'
               write(lu(1),*)
     &              '(this map must have 100 elements ",
     *              "with 0.2 degree spacing.'
               write(lu(1),*)
     &              " Thus, it must extend 20 degrees ",
     *              "from 0,0.) "
               write(lu(1),*)
     &              ' or cr to abort.'
 960           READ(LU(12),'(A)')MAPFILE
               if (MAPFILE.eq.' ') return
               INQUIRE(FILE=MAPFILE,EXIST=FEXIST)
               if (.not.FEXIST) then
                  numcar = len_trim(MAPFILE)
                  write(*,'("The file ",a," does not exist!" )') 
     *                 MAPFILE(1:numcar)
                  write(*,'("Please re-enter the filename or ",
     &                 "enter <cr> to abort:",$)')
                  MAPFILE='                    '
                  goto 960 
               else
                  open(43,FILE=MAPFILE,err=962)
                  do j=1,100
                     read(43,*,err=964)PSFARRTMP(j)
                  enddo
                  close(unit=43)
                  do j=1,100
                     PSFARR(j)=PSFARRTMP(j)
                  enddo
                  LSHIFT=0.
                  BSHIFT=0.
                  egret_psf=.false.
                  CALL PSMGET
               endif
               return
 962           write(*,*)' '
               write(*,'("Could not open file ",a)') 
     *              mapfile(1:numcar)
               write(*,*)'The file DOES exist !!!'
               write(*,*)'Check your file for access ",
     *              "problems'
               write(*,*)' '
               close(unit=43)
               return
 964           write(*,*)' '
               write(*,'("READ ERROR in file ",a)') 
     *              mapfile(1:numcar)
               write(*,'("RECORD number:",i5)') J
               write(*,*)'Check your file for problems'
               write(*,*)' '
               close(unit=43)
               return
c
c
c     elseif 3:3 == 'X'
c
            elseif (input(3:3).eq.'X'.or.input(3:3).eq.'x') then
 97            write(lu(1),'(
     &              "Enter: 1 to adjust Counts; ",
     *              "2 to adjust Gmult;",/,
     &              "3 to adjust Gbias; or ",
     *              "4 to adjust Position;",
     &              " cr to return",$)')
               READ(LU(12),'(A)')moreinput
               if (moreinput.eq.'1') then
                  write(lu(1),'("Enter Counts (cr for ",
     *                 f16.2,"):",$)')
     &                 Counts
                  READ(LU(12),'(A)')moreinput
                  if (moreinput.ne.' ')READ(moreinput,*)
     *                 Counts
                  goto 97
               elseif (moreinput.eq.'2') then
                  if (Restrict_Gmult) then
                     write(lu(1),*)'Gmult is resticted.  Use PMR ',
     *                    'command'
                     goto 97
                  endif
                  write(lu(1),'("Enter Gmult (cr for ",f16.2,"):",$)')
     &                 Gmult
                  READ(LU(12),'(A)')moreinput
                  if (moreinput.ne.' ')READ(moreinput,*)
     *                 Gmult
                  goto 97
               elseif (moreinput.eq.'3') then
                  if (Restrict_Gbias) then
                     write(lu(1),*)'Gbias is resticted.  Use PMR ',
     *                    'command'
                     goto 97
                  endif
                  write(lu(1),'("Enter Gbias (cr for ",f16.2,"):",$)')
     &                 Gbias
                  READ(LU(12),'(A)')moreinput
                  if (moreinput.ne.' ')READ(moreinput,*)
     *                 Gbias
                  goto 97
               elseif (moreinput.eq.'4') then
                  call pixel_select(.true.,
     *                 '       test point:  ')
                  CALL ERROR(0,LOC)
                  goto 97
               elseif (moreinput.eq.' ') then
                  return
               else
                  write(lu(1),'("This parameter adjustment option ",
     &                 "is not supported.")')
                  goto 97
               endif

c
c     elseif 3:3 == 'C'
c     
            elseif (input(3:3).eq.'C'.or.input(3:3).eq.'c') then
               write(lu(1),'("Coordinate system label is:",a)') 
     &              coord_sys
               if (coord_sys.eq.'C'.or.coord_sys.eq.'G') then
                  write(lu(1),'(
     &                 "Coordinate system label change not allowed.")')
               else
                  write(lu(1),'(
     &                 "Input new label (G,C, or O):",$)')
                  READ(LU(12),'(A1)')coord_sys
               endif
c     
c     elseif 3:3 == 'M'
c
            elseif (input(3:3).eq.'M'.or.input(3:3).eq.'m') then
 98            write(lu(1),'(
     &              "Enter: 1 to adjust NEWMAPFILE; 2 ",
     *              "to adjust LMAPFILE;",/,
     &              "3 to adjust GBIASFILE;",$)')
               READ(LU(12),'(A)')moreinput
               if (moreinput.eq.' '.or.moreinput.eq.'q'.or.
     *              moreinput.eq.'Q') then
                  return
               elseif (moreinput.eq.'1') then
                  write(lu(1),'("Enter NEWMAPFILE (cr for ",A,"):",$)')
     &                 NEWMAPFILE(1:40)
                  READ(LU(12),'(A)')moreinput
                  if (moreinput.ne.' ')
     *                 READ(moreinput,'(A)')NEWMAPFILE
               elseif (moreinput.eq.'2') then
                  write(lu(1),'("Enter LMAPFILE (cr for ",A,"):",$)')
     &                 LMAPFILE(1:40)
                  READ(LU(12),'(A)')moreinput
                  if (moreinput.ne.' ')
     *                 READ(moreinput,'(A)')LMAPFILE
               elseif (moreinput.eq.'3') then
                  write(lu(1),'("Enter GBIASFILE (cr for ",A,"):",$)')
     &                 GBIASFILE(1:40)
                  READ(LU(12),'(A)')moreinput
                  if (moreinput.ne.' ')
     *                 READ(moreinput,'(A)')GBIASFILE
               else
                  write(lu(1),'("This parameter adjustment option ",
     &                 "is not supported.")')
               endif
               goto 98
            else
               write(lu(1),'("This parameter adjustment option ",
     &              "is not supported.")')
            endif

c
c     elseif 2:2 == 'R'
c
         elseif (input(2:2).EQ.'R' .or. input(2:2).EQ.'r') then
            write(lu(1),*)
     &           'Re-specifying the radius for ",
     *           "likelihood calculation (Ranal).'
            write(lu(1),'("Input R (cr for",f7.2," degrees):",$)')Ranal
            READ(LU(12),'(A)')input
            if (input.ne.' ') then
              read(input,*,end=1111)Ranal
              if (Ranal.gt.180.) then
                 write(lu(1),*)
     &                'Setting Ranal to 180 degrees ",
     *                "(maximum meaningful value)'
                 Ranal=180.
              endif
              CTLNBC=Ranal/CTLSCL
              Ranal=float(CTLNBC)*CTLSCL
              write(lu(1),'("Setting Ranal to",f7.2," degrees):")')Ranal
              LikTotaled=.false.
            endif
c     
c     elseif 2:2 default output
c
         else
            write(lu(1),'("The parameter option ",a,
     &           " is not supported. See ?P.")') 
     *           input(2:2)
         endif
c
c     ENDIF 1:1 == 'P'
c
c
c     IF 1:1 == 'M'
c
      ELSEIF (input(1:1).EQ.'M'.or.input(1:1).EQ.'m') THEN
c     
c     if 2:2 == 'F'
c
         if (input(2:2).EQ.'F'.or.input(2:2).EQ.'f') then
            call MAPFINE
            CALL ERROR(0,LOC)
c
c     if 2:2 == 'S'
c
         elseif (input(2:2).EQ.'S'.or.input(2:2).EQ.'s') then
            if (gmap_null) then
               print *,'The gasmap is null - ',
     *              'this command makes no sense.'
               RETURN
            endif
            tttflg=.false.
            call MAPSRCJ(.true.)
c
c     elseif 2:2 == 'G'
c
         elseif (input(2:2).EQ.'G' .or. input(2:2).EQ.'g') then
            if (gmap_null) then
               print *,'The gasmap is null - ',
     *              'this command makes no sense.'
               RETURN
            endif
            call MAPMULT
c
c     elseif 2:2 == 'H'
c
         elseif (input(2:2).EQ.'H' .or. input(2:2).EQ.'h') then
            call MAPHIGH
         else
c     
c     else 2:2 default output
c
            write(lu(1),'("The map option ",a,
     &           " is not supported. See ?M.")') 
     *           input(2:2)
         endif
c     
c     ENDIF 2:2
c
         CALL ERROR(0,LOC)
c
c     end 1:1 == 'M' 
c
c
c     elseif 1:1 == 'G'  
c
c
      ELSEIF (input(1:1).EQ.'G'.or.input(1:1).EQ.'g') THEN
c     test GAS Gmult
         if (gmap_null) then
            print *,'The gasmap is null - this command makes no sense.'
            RETURN
         endif
         report=.true.
         calc_uncert=.true.
         if (input(2:3).EQ.'  ' .or. input(2:3).EQ.'  ') then
            call gastest(.true.) !with parameter adjustment
         elseif (input(2:3).EQ.'G ' .or. input(2:3).EQ.'g ') then
            call gastest(.false.) !without parameter adjustment
         elseif (input(2:3).EQ.'B ' .or. input(2:3).EQ.'b ') then
            call gasbias(.true.)
         elseif (input(2:3).EQ.'BB' .or. input(2:3).EQ.'bb') then
            call gasbias(.false.)
         elseif (input(2:3).EQ.'BC' .or. input(2:3).EQ.'bc') then
            call allfit(.true.)
         elseif (input(2:3).EQ.'CB' .or. input(2:3).EQ.'cb') then
            call allfit(.true.)
         elseif (input(2:2).EQ.'C' .or. input(2:2).EQ.'c') then
            call gascnts(.true.)
         else
            write(lu(1),'("The G option ",a,
     &           " is not supported. See ?G.")') input(2:3)
         endif
         CALL ERROR(0,LOC)
c
c     eslseif 1:1 == 'C'
c
      ELSEIF (input(1:1).EQ.'C'.or.input(1:1).EQ.'c') THEN
c     evaluate expected number of counts
         report=.true.
         calc_uncert=.true.
         if (input(2:3).EQ.'  ' .or. input(2:3).EQ.'  ') then
            call cnttest(.true.)
         elseif (input(2:3).EQ.'B ' .or. input(2:3).EQ.'b ') then
            call cntsbias(.true.) !with parameter adjustment
         elseif (input(2:3).EQ.'BB' .or. input(2:3).EQ.'bb') then
            call cntsbias(.false.)
         elseif (input(2:3).EQ.'C ' .or. input(2:3).EQ.'c ') then
            call cnttest(.false.) !without parameter adjustment
         elseif (input(2:3).EQ.'G ' .or. input(2:3).EQ.'g ') then
            call gascnts(.true.)
         elseif (input(2:3).EQ.'GB' .or. input(2:3).EQ.'gb') then
            call allfit(.true.)
         elseif (input(2:3).EQ.'BG' .or. input(2:3).EQ.'bg') then
            call allfit(.true.)
         elseif (input(1:3).eq.'COR' .or. input(1:3).eq.'cor ') then
            if (NSOURCE.lt.1)
     *           write(*,*)
     *           'There are no sources in the map'
            if (NSOURCE.lt.1) return
            write(*,*)' '
            write(*,'("Reseting counts to 0.25 and resetting",
     &           "the PSF map to null.")')
            write(*,*)' '
            do kj=1,NSOURCE
               SRC_PARMS(kj,3)=0.25
               do kjl=1,10
                  sv_dstsv(kjl,kj)=.false.
               enddo
               write(*,'(I3,3x,A,2x,"done")') kj,SRC_NAMES(kj)
            enddo
            call MAPRST(bmap,CTLMSZ1,CTLMSZ2)
            write(*,*)' '
            write(*,*)' '
         else
            write(lu(1),'("The C option ",a,
     &           " is not supported. See ?C.")') 
     *           input(2:3)
         endif
         CALL ERROR(0,LOC)
c     
c     elseif 1:1 == 'B'
c
      ELSEIF (input(1:1).EQ.'B'.or.input(1:1).EQ.'b') THEN
c     test Gbias
         report=.true.
         calc_uncert=.true.
         if (input(2:3).EQ.'  ' .or. input(2:3).EQ.'  ') then
            call biastest(.true.) !with parameter adjustment
         elseif (input(2:3).EQ.'B ' .or. input(2:2).EQ.'b ') then
            call biastest(.false.) !without parameter adjustment
         elseif (input(2:3).EQ.'C ' .or. input(2:3).EQ.'c ') then
            call cntsbias(.true.) 
         elseif (input(2:3).EQ.'CG' .or. input(2:3).EQ.'cg') then
            call allfit(.true.)
         elseif (input(2:3).EQ.'G ' .or. input(2:3).EQ.'g ') then
            call gasbias(.true.)
         elseif (input(2:3).EQ.'GC' .or. input(2:3).EQ.'gc') then
            call allfit(.true.)
         else
            write(lu(1),'("The B option ",a,
     &           " is not supported. See ?B.")') 
     *           input(2:3)
         endif
         CALL ERROR(0,LOC)
c
c     elseif 1:1 == 'L'
c
      ELSEIF (input(1:1).EQ.'L'.or.input(1:1).EQ.'l') THEN
c     
c     IF 2:2 == 'C'
c     
         if (input(2:2).EQ.'C'.or.input(2:2).EQ.'c') then
            call listproc
c
c     elseif 2:2 == 'M'
c
         elseif (input(2:2).EQ.'M' .or. input(2:2).EQ.'m') then
            call multproc(.true.)
c
c     elseif 2:2 == 'E'
c 
         elseif (input(2:2).EQ.'E' .or. input(2:2).EQ.'e') then
c     do position analysis
            call catproc(.true.,.false.)
            call catproc(.false.,.false.)
c
c     elseif 2:2 == 'E'  !!!! Why is this here twice ????
c     
         elseif (input(2:2).EQ.'E' .or. input(2:2).EQ.'e') then
            call catproc(.true.,.false.)
            call catproc(.false.,.false.)
c
c     elseif 2:2 == 'N'
c 
         elseif (input(2:2).EQ.'N' .or. input(2:2).EQ.'n') then
            call next_src
c
c     elseif 2:2 == 'L'
c     
         elseif (input(2:2).EQ.'L' .or. input(2:2).EQ.'l') then
            report=.true.
            calc_uncert=.true.
            call srctest(.false.) !without parameter adjustment
c
c     elseif 2:2 == 'P'
c     
         elseif (input(2:2).eq.'P' .or. input(2:2).eq.'p') then
            call jloc_pos(input)
         else
            report=.true.
            calc_uncert=.true.
            call srctest(.true.)
         endif
         CALL ERROR(0,LOC)
c
c     elseif 1:1 == 'O'
c
      ELSEIF (input(1:1).EQ.'O'.or.input(1:1).EQ.'o') THEN
c     output
c     
c     elseif 2:3 == 'MR'
c 
         if (input(2:3).EQ.'MR' .or. input(2:3).EQ.'mr') then
c     write  data-model residual
            call residue(input,.true.)
c
c     elseif 2:3 == 'MG'
c 
         elseif (input(2:3).EQ.'MG' .or. input(2:3).EQ.'mg') then
c     write diffuse model
            write(lu(1),*)
     &           'Will write a map of Gbias + ',
     *           'Gmult*diffuse model only'
            write(*,'("Include other PSF matrix ? (N/y):"$)')
            read(*,'(a)')input(5:5)
            if (input(5:5).eq.' ')input(5:5)='N'
            call to_upper(input(5:5))
            if (.not.Restrict_Gmult) then
               write(lu(1),'("Input Gmult (cr for",f12.6,"):",$)')Gmult
               READ(LU(12),'(A)')moreinput
               if (moreinput.ne.' ') 
     *              read(moreinput,*,end=1111)Gmult
            else
               write(LU(1),'(
     *              " Gmult is restricted to: ",f5.1)') 
     *              Gmult
            endif
            if (.not.Restrict_Gbias) then
               write(lu(1),'("Input Gbias (cr for",f12.6,"):",$)')Gbias
               READ(LU(12),'(A)')moreinput
               if (moreinput.ne.' ') 
     *              read(moreinput,*,end=1111)Gbias
            else
               write(LU(1),'(" Gbias is restricted to: ",f5.1)') Gbias
            endif
            call MAPRST(tmpmap,CTLMSZ1,CTLMSZ2)
            CALL MAPADD(Gmult,tmpmap,CTLMSZ1,CTLMSZ2)
            CALL MAPMLT(tmpmap,GASMAP,CTLMSZ1,CTLMSZ2)
            if (input(5:5).eq.'Y')CALL MAPSUM(tmpmap,bmap,
     &           CTLMSZ1,CTLMSZ2)
            if (input(4:4).EQ.'C' .or.
     *           input(4:4).EQ.'c') then
               write(6,*)
     &              'Writing a counts map corresponding to the current ",
     *              "diffuse model.'
               call MAPRST(xrrmap,CTLMSZ1,CTLMSZ2)
               CALL MAPADD(Gbias*1.e-5,xrrmap,CTLMSZ1,
     *              CTLMSZ2)
               CALL MAPMLT(xrrmap,EMAP,CTLMSZ1,CTLMSZ2)
               CALL MAPSUM(tmpmap,xrrmap,CTLMSZ1,CTLMSZ2)
               TMPTYP='GCMP'
            else
               write(6,*)
     &              'Writing a flux map corresponding ',
     *              'to the current diffuse model.'
               CALL MAPDIV(TMPMAP,EMAP,CTLMSZ1,CTLMSZ2)
               CALL MAPADD(Gbias*1.e-5,tmpmap,CTLMSZ1,
     *              CTLMSZ2)
               TMPTYP='GMAP'
            endif
            do i=1,10
               TMPDOC(i)=GASDOC(i)
            enddo
            write(input,'("Gmult=",f12.6," applied")')Gmult
            nchar=26
            call DOCADD(nchar,TMPDOC,TMPTYP,input)
            write(input,'("Gbias=",f12.6,
     *           " *1e-5 added. ")')Gbias
            nchar=32
            call DOCADD(nchar,TMPDOC,TMPTYP,input)
            
            MAPFILE=NEWMAPFILE
            write(lu(1),'("Writing ",a,
     &           " to ",a)') TMPTYP,MAPFILE(1:50)
            CALL MAPRITROI(TMPMAP,TMPTYP,TMPDOC,nmap)
            CALL ERROR(0,LOC)
c
c     elseif 2:3 == 'MC'
c 
         elseif (input(2:3).EQ.'MC' .or. input(2:3).EQ.'mc') then
C     Write cnts map
            MAPTYP='CMAP'
            MAPFILE=NEWMAPFILE
            write(lu(1),'("Writing ",a," to ",a)') MAPTYP, 
     &           MAPFILE(1:50)
            CALL MAPRITROI(MAP,MAPTYP,MAPDOC,nmap)
            CALL ERROR(0,LOC)
c
c     elseif 2:3 == 'ME'
c 
         elseif (input(2:3).EQ.'ME' .or. input(2:3).EQ.'me') then
C     Write exposure map
            EMPTYP='EMAP'
            MAPFILE=NEWMAPFILE
            write(lu(1),'("Writing ",a," to ",a)')EMPTYP,
     &           MAPFILE(1:50)
            CALL MAPRITROI(EMAP,EMPTYP,EMPDOC,nmap)
            CALL ERROR(0,LOC)
c
c     elseif 2:2 == 'P'
c 
         elseif (input(2:2).EQ.'P' .or. input(2:2).EQ.'p') then
C     Profile plot
            CALL PROFILE
            CALL ERROR(0,LOC)
         else
c
c     elseif 2:2 default output for 1:1 'O'
c 
            
            write(lu(1),'("The output option ",a,
     &           " is not supported. See ?O.")') 
     *           input(2:3)
         endif
      ELSE
         WRITE(LU(1),'('' Invalid command, try again.'')')
      END IF
      
      RETURN
      
 1111 write(6,*)'Invalid input, try again.'
      RETURN
      END
