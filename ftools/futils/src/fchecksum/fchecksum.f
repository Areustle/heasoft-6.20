C******************************************************************************
C TASK:
C       fchecksum
C
C DESCRIPTION:
C       Program to verify or update the checksum keywords in a FITS file.
C
C AUTHOR/DATE:
C       William D Pence 23 May 1995
C
C MODIFICATION HISTORY:
C       PDW 03/08/99: v1.5  Accept '@files' syntax; work around CFITSIO's
C                           ffchdu behavior of fixing/modifying some keywords
C       NG  12/23/99: v1.6  Added a parameter OK to return the status of
C                           the verification.
******************************************************************************
C       Program to verify or update the checksum keywords in a FITS file.
C       These keywords provide a way to verify that the file has not
C       been modified since the keywords were written.

        subroutine fchecm

C       taskname is required in the FTOOLS environment
        character(40) taskname
        common /task/ taskname

        character fname*160,filename*160,errtxt*80
        character contxt*70,errst*3
        integer iunit,funit,status,i,ios,nfiles
        logical update,dodata,filelist
   
        taskname = 'fchecksum 1.6'
        status=0

        call gcheck(fname,update,dodata,status)
        if (status .gt. 0)go to 900

C       get logical unit number and open the file
        call ftgiou(iunit,status)

        if (fname(1:1).eq.'@') then
           call ftgiou(funit,status)
           open(unit=funit,file=fname(2:),iostat=ios,status='old')
           if (ios.ne.0) then
              contxt = 'Unable to find list file: '//fname(2:)
              call fcecho(contxt)
              goto 900
           endif
           filelist = .true.
           nfiles = 999999
        else
           filelist = .false.
           nfiles = 1
        end if
        
C     Loop over all the files
        do 100 i=1,nfiles
           if( filelist ) then
              read(funit,10,end=900) filename
 10           format(A)
           else
              filename = fname
           endif
           call dochck(iunit,filename,update,dodata,filelist,status)
           if( status.ne.0 ) goto 900
 100    continue

 900    continue

        if( filelist ) close(funit)

        if (status .gt. 0) then
            write(errst,910)status
 910        format(i3)
            contxt='Error Status Code = '//errst
            call fcerr(contxt)
            call ftgerr(status,errtxt)
            call fcerr(errtxt)

C           print out the FITSIO error message stack
 990        call ftgmsg(errtxt)
            if (errtxt .ne. ' ')then
                call fcerr(errtxt)
                go to 990
            end if
        end if

        call ftfiou(iunit,status)
        end

C******************************************************************************
C Routine:
C       dochck
C
C DESCRIPTION:
C       Verifies or updates the checksum keywords in one FITS file.
C
C AUTHOR/DATE:
C       William D Pence 23 May 1995
C
C MODIFICATION HISTORY:
C       PDW 03/08/99: Make this a subroutine
******************************************************************************
        subroutine dochck(iunit,fname,update,dodata,list,status)

        integer iunit,status
        character*(*) fname
        logical update,dodata,list

        character comm*40,prefix*40
        character(16) oldcks,newcks,olddat,newdat
        character contxt*80,id*14
        integer iomode,block,hdutyp,i,dataok,hduok,plen,fcstln
        logical allok
        logical ok
        integer istatus

        if( status.ne.0 ) goto 999

        allok=.true.
        ok = .TRUE.

        if( list ) then
           plen = fcstln(fname)+2
           if (plen.gt.40) plen=40
           prefix = fname(1:plen-2)//': '
        else
           prefix = ' '
           plen = 0
        endif

        if (update) then
            iomode=1
        else
            iomode=0
        end if
        call ftopen(iunit,fname,iomode,block,status)
        if (status .gt. 0) go to 900

        if (update) then
C          Hack to have CFITSIO open/close every HDU which potentially
C          modifies the contents of the HDU... eg, TFORM=PE -> PE(n)
           call ftmahd(iunit,999,hdutyp,status)
           if( status.eq.107 ) then
              status=0
              call ftcmsg
           else
              goto 900
           endif
        end if

C       process each HDU in turn,  until we reach the end of file
        do 100 i=1,10000
            call ftmahd(iunit,i,hdutyp,status)
            if (status .gt. 0)go to 900

            if (update .and. dodata) then
C               Update both the CHECKSUM and DATASUM keywords.
C               Get the current value of keywords
                call ftgkys(iunit,'CHECKSUM',oldcks,comm,status)
                if (status .eq. 202)then
                    oldcks=' '
                    status=0
                end if
                call ftgkys(iunit,'DATASUM',olddat,comm,status)
                if (status .eq. 202) then
                    olddat=' '
                    status=0
                end if

C               update the CHECKSUM and DATASUM keywords
                call ftpcks(iunit,status)

C               check if keywords were modified
                call ftgkys(iunit,'CHECKSUM',newcks,comm,status)
                call ftgkys(iunit,'DATASUM',newdat,comm,status)
                if (newcks .ne. oldcks .or. newdat .ne. olddat)then
                    allok=.false.
                end if

            else if (update)then
C               Update only the CHECKSUM keyword. Assume DATASUM is correct
C               Get the current value of CHECKSUM
                call ftgkys(iunit,'CHECKSUM',oldcks,comm,status)
                if (status .eq. 202)then
                    oldcks=' '
                    status=0
                end if

C               update CHECKSUM
                call ftucks(iunit,status)

C               check if keyword was modified
                call ftgkys(iunit,'CHECKSUM',newcks,comm,status)
                if (newcks .ne. oldcks)then
                    allok=.false.
                end if

            else
C               just verify if the checksum keywords are correct
                call ftvcks(iunit,dataok,hduok,status)
 
C               construct ID string
                if (i .eq. 1)then
                    id='Primary Array: '
                else 
                    id='Extension    :'
                    write(id(11:13),2000)i-1
2000                format(i3)
                end if

C               write status report, if there is a problem
                if (hduok .eq. 0)then
                    contxt=id//'  CHECKSUM keyword not found.'
                else if (hduok .eq. -1 .and. dataok .eq. 1)then
                    contxt=id//'  CHECKSUM keyword incorrect;'//
     &                     ' DATASUM keyword OK.'
                else if (hduok .eq. -1 .and. dataok .eq. 0)then
                    contxt=id//'  CHECKSUM keyword incorrect;'//
     &                     ' DATASUM keyword not found.'
                else if (hduok .eq. -1 .and. dataok .eq. -1)then
                    contxt=id//'  CHECKSUM and DATASUM '//
     &                     'keywords incorrect.'
                else if (hduok .eq. 1 .and. dataok .eq. -1)then
                    contxt=id//'  CHECKSUM keyword OK;'//
     &                     ' DATASUM keyword incorrect.'
                else
                    go to 20
                end if

                if (list .and. allok) call fcecho(prefix)
                call fcecho(contxt)
                allok=.false.
20              continue
            end if
100     continue

900     continue
C       check for normal end of file error code
        if (status .eq. 107) then
           status=0
           call ftcmsg
        endif

C       close the FITS file
        call ftclos(iunit,status) 

        if (status .eq. 0) then
            if (allok .and. update) then
                contxt=prefix(1:plen)//
     &              'Checksum keywords were correct.'//
     &              '  File not updated.'
                call fcecho(contxt)
            else if (update) then
                contxt=prefix(1:plen)//
     &              'Checksum keywords updated successfully.'
                call fcecho(contxt)
            else if (allok) then
                contxt=prefix(1:plen)//
     &              'All checksum values are correct.'
                call fcecho(contxt)
            else
                contxt='Problems found while verifying checksums.'
                call fcerr(contxt)
                ok = .FALSE.
            end if
        end if
        if (status .ne. 0) ok = .FALSE.
        istatus = 0
        call uclpsb('ok', ok, istatus)
        if(istatus.ne.0)   
     &     call fcerr("Error to update the OK keyword.")

 999    continue
        end
C---------------------------------------------------------------------------
        subroutine gcheck(infile,update,datasum,status)

C       get the input parameters for this task

        character*(*) infile
        logical update,datasum
        character(80) context
        integer status

C       Get the name of the input FITS file
        call uclgst('infile',infile,status)
        if ( status .ne. 0 ) then
            context = 'Could not get INFILE parameter'
            call fcerr(context)
            goto 999
        endif

C       Update the checksum keywords if they are out of date?
        call uclgsb('update',update,status)
        if (status .ne. 0) then
            context = 'could not get UPDATE flag'
            call fcerr(context)
            goto 999
        endif

        if (update)then
C           verify (and update) the DATASUM keyword?
            call uclgsb('datasum',datasum,status)
            if (status .ne. 0) then
                context = 'could not get DATASUM flag'
                call fcerr(context)
            endif
        end if

 999    continue
        end
