
      SUBROUTINE gpaddr(MXRMFS,infact,inrmfs,nfiles, rmffil, clobber, 
     >                  status)

      CHARACTER*(*) rmffil,inrmfs(*)
      real  infact(*)
      integer MXRMFS,nfiles
      INTEGER status
      LOGICAL clobber

*------------------------------------------------------------------------------
* Description: Gets the parameters from the addrmf par file.
*
* Arguments:   lstfil  (r) : The name of the text file containing the list of
*                            RMFs to be added and their weighting factors
*              rmffil  (r) : The name of the output rmf file
*              clobber (r) : If clobber = .true. and outfile exists, then
*                            delete outfile before writing it.  Otherwise,
*                            exit with error.
*              status  (r) : The success status for this routine
*                            0 = OK
*                            else = NOT OK
*              list    (s) : list of input filenames
*              lstfil        File containing list of RMFs to be added.
*
* Origin:      Written for ASCA mission data analysis
*
* Authors/Modification History:
*              Keith Arnaud  Jun  9, 1994
*              Banashree M Seifert (March 12, 1997)
*                  . added option for input file list, as below: 
*                    Extra option added so that instead of ascii filename
*                    user can also input filenames, and if this is the case, 
*                    then user is asked for weight factors
*
* $Id: gpaddr.f,v 3.12 2013/05/21 19:08:11 irby Exp $
*------------------------------------------------------------------------------

        integer errstat,i,ierr,nweights
        character(255) context,outstr
        character(400) list,st_weights(100)
        character(255) lstfil,string,message
        
        integer lstu
        logical flag

*the parameter file has been opened by the c-wrapper (haddrmf.c). that's 
*how gpaddr knows what the parameter file is.

*       Set status = OK
        status = 0
* MJT 17Sept96 - initializing errstat 
        errstat = 0

* Get the list parameter
        call uclgst('list', list, errstat)
        if (errstat .ne. 0) then
            context = 'Cannot get the listfile parameter'
            goto 999
        endif

        call crmvlbk(list)

* if list has '@' as first character, then
* >>1. read list file to load the names of RMF files, their weighting factors
* if not,
* >>1. Parse the string and get RMF names and number of files(fcgcls)
* >>2. Get the weights string(uclgst)
* >>3. Parse the string and get the weights and number of weights(fcgcls)

        lstu=8
        errstat=0
        if(list(1:1) .eq. '@') then
           lstfil=list(2:)
           CALL rdlstf(lstfil, lstu, MXRMFS, nfiles, inrmfs,
     &               infact, context, errstat)
           IF ( errstat .NE. 0 ) GOTO 999
        else
           flag=.false.
           call fcgcls(list, inrmfs, nfiles, flag)
           if(nfiles .gt. MXRMFS) then
              context = 'I can only deal with 100 RMFs'
              errstat = 1
              return
            endif

           errstat=0
           call uclgst('weights',string,errstat)
           if (errstat .ne. 0) then
               context =  'Error getting WEIGHTS parameter'
               call fcecho(message)
               goto 999
           endif
           flag=.false.
           call fcgcls(string, st_weights, nweights, flag)
           do i = 1, nweights
              read(st_weights(i), *) infact(i)
           enddo

           if (nweights .ne. nfiles) then
               ierr = 999
               context =  'Error: Incompatible files/weights'
               call fcecho(context)
               write(context,'(a,i3,a,i3)')
     $                 ' ... Number of files:', nfiles,
     $                 ', Number of weights:', nweights
               errstat=1
               go to 999
           endif

           CALL fcecho('Summing ...')

           do i=1,nfiles
              WRITE(outstr(1:13),'(1pe10.3, a3)') infact(i), ' * '
              outstr(14:) = inrmfs(i)
              CALL fcecho(outstr)
           enddo


        endif


*       Get the rmffile parameter
        errstat=0
        call uclgst('rmffile', rmffil, errstat)
        if (errstat .ne. 0) then
                context = 'Cannot get the rmffile parameter'
                goto 999
        endif
        
*       Get the clobber parameter
        call uclgsb('clobber', clobber, errstat)
        if (errstat .ne. 0) then
                context = 'Cannot get the clobber parameter'
                goto 999
        endif

        return

999     continue
        call fcerr(context)
        status = errstat
        return

        end


