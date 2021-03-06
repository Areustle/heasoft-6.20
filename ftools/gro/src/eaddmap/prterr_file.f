C      Name: prterr_file.f 
C  Purpose: This program will write an error meaasage caused by
C   fitsio to a file. The routine is based on printerror.f from the 
C   fitsio cookbook.
C    See:
C   http://heasarc.gsfc.nasa.gov/docs/software/fitsio/cookbook/cookbook.html
C
C   lunout: the name of the logical number of the output file.
C   status:  an error code generated by fitsio.
C
C    Called by eaddmap
C
C
C   J Silvis
C   6 August 1997
C*************************************************************************** 
       subroutine prterr_file(lunout,status)
      implicit none 
       integer lunout,status
        character errtext*30,errmessage*80

C      check if status is OK (no error); if so, simply return
       if (status .le. 0)return

C      get the text string which describes the error
       call ftgerr(status,errtext)
       print *,'FITSIO Error Status =',status,': ',errtext
       write(lunout,*) 'FITSIO Error Status =',status,': ',errtext  
C      read and print out all the error messages on the FITSIO stack
       call ftgmsg(errmessage)
       do while (errmessage .ne. ' ')
C***************************************************************************
C          print *,errmessage
C
C      Ftool change
C      
C    Write the error message to a file instad of the screen.
C
C          
C***************************************************************************
          call ftgmsg(errmessage)
          write(lunout,*) errmessage
        end do
       end
