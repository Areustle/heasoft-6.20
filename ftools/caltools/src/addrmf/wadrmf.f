
        SUBROUTINE wadrmf(nfiles,inrmfs,infact,iunit,nphbns, 
     &  maxne,maxgrp,maxelt,thresh,flchan,rspval,rngrp, 
     &  rfchan,rnchan,rorder,renlo,renhi,rspval1,rspval2,rngrp1,
     &  rngrp2,rfchan1,rfchan2,rnchan1,rnchan2,buffer,numgrp,numelt,
     &  contxt,extname,extcomm,status)

      INTEGER nfiles,iunit,flchan,status
      INTEGER nphbns,maxne,maxgrp,maxelt
      INTEGER numgrp,numelt
      INTEGER rngrp(maxne),rfchan(maxgrp)
      INTEGER rorder(maxgrp),rnchan(maxgrp)
      INTEGER rngrp1(maxne),rngrp2(maxne)
      INTEGER rfchan1(2*maxgrp),rfchan2(2*maxgrp)
      INTEGER rnchan1(2*maxgrp),rnchan2(2*maxgrp)

      REAL    infact(nfiles), rspval(maxelt)
      REAL    rspval1(2*maxelt), rspval2(2*maxelt)
      REAL    buffer(2,nphbns)
      REAL    renlo(maxne),renhi(maxne),thresh

      CHARACTER*(*) inrmfs(nfiles)
      character(511) contxt
      character(160) infil
      character(80)  extname
      character(80)  extcomm

c Subroutine to construct the redistribution array from a weighted sum of
c the arrays from the input RMFs. The output RMF is opened and positioned
c at the end of the file.

c History :
c Banashree M Seifert (Mar 11, 1997)
c          . weights were not used at all. So I modified it
c
c AM (February 23, 1999)
c          . Incorporated the call to RDRMF(). Added a proper memory 
c            management for the local arrays. Modified a block that 
c            adds two matrices and finds the channel groups, number 
c            of channels for each group, and the first channel in each
c            group. Corrected many minor things. 
c
c
c Arguments :
c     nfiles        i       i: number of input files
c     inrmfs        c       i: names of the input files
c     infact        r       i: weightings for the input files
c     iunit         i       i: I/O unit for the input files
c     nphbns        i       i: Number of PHA bins
c     maxne         i       i: Size of energy array
c     maxgrp        i       i: Size of response group array
c     maxelt        i       i: Size of Matrix array
c     thresh        r       i: Threshold value for response
c     flchan        i       i: Zero point for channels in RESPONSE (0 or 1)
c     rspval        r       r: Full matrix
c     rngrp         i       r: Number of groups per energy
c     rfchan        i       r: Start channel for each group
c     rnchan        i       r: Number of channels for each group
c     renlo         r       r: Low bounds for energy bins
c     renhi         r       r: High bounds for energy bins
c     rspval1       r       r: Work array for full matrix
c     rngrp1        i       r: Work array for number of groups per energy
c     rfchan1       i       r: Work array for start channel for each group
c     rnchan1       i       r: Work array for number of channels for each group
c     rspval2       r       r: Work array for full matrix
c     rngrp2        i       r: Work array for number of groups per energy
c     rfchan2       i       r: Work array for start channel for each group
c     rnchan2       i       r: Work array for number of channels for each group
c     numgrp        i       r: Number of groups in output response
c     numrsp        i       r: Number of response elements in output response
c     contxt        c       r: string describing error
c     status        i       r: Status    0 = OK, !0 = !OK

      REAL areascal,lothresh

      INTEGER ifile,nfound,htype,rmfsiz,chatter,ich
      INTEGER i,j,next(10),ichan,ienerg,imaxgrp
      INTEGER irsp,irsp1,irsp2,igroup,igroup1,igroup2
      INTEGER nelem,flchan1

      character(80) telescop,instrume,detnam,filter
      character(80) chantype,rmfversn,hduclas3
      character(20) instr(2),outhdu(9,10)
      character(20) extnam(10),outver(9,10)

      LOGICAL qorder, isorder, inopen, count

c Initialize the output matrix array RSPVAL(). The size of array is 
c as big as twice of the size of input array.

      DO i = 1, maxelt
         rspval(i) = 0.
      ENDDO

      igroup = 0
      irsp = 0
      igroup1 = 0
      irsp1 = 0
      igroup2 = 0
      irsp2 = 0

c ----------------------------------------------------------

c Loop round the input files


      DO ifile = 1,nfiles       

         qorder  = .false.
         isorder = .false.
         chatter = 50       

c First open the input RMF and position it at the redistribution
c extensions.

         rmfsiz = 2880
         infil  = inrmfs(ifile)
       
         INOPEN = .false.
         STATUS = 0
          CALL ftopen(iunit,infil,0,rmfsiz,status)
          WRITE(contxt,'(a,i4)') 'WADRMF: Failed to open RMF No.',
     &                           ifile
         IF ( status .NE. 0 ) RETURN
         INOPEN = .true.
      
c Find the extension with the redistribution matrix.

         instr(1) = 'RESPONSE'
         instr(2) = 'RSP_MATRIX'
         CALL fndhdu(5,iunit,2,instr,10,nfound,next,outhdu,
     &               outver,extnam,status)
         WRITE(contxt,'(a,i4)') 
     &    'WADRMF:Failed to find matrix extn. in file',ifile
         IF (nfound .EQ. 0) status = 1
         IF (status .NE. 0) RETURN

c Go to the extension
       
         status = 0
         CALL ftmrhd(iunit, next(1), htype, status)
         WRITE(contxt,'(a,i4)') 
     &     'WADRMF:Failed to go to matrix extn. in file',ifile
         IF ( status .NE. 0 ) RETURN

c Read the information for the response matrix into the first work
c array

         status = 0

c         CALL rdrmf4(iunit,chatter,qorder,maxne,maxgrp,maxelt,
c     &               rmfversn,hduclas3,telescop,instrume,detnam,
c     &               filter,areascal,chantype,flchan1,ichan,ienerg,
c     &               imaxgrp,nelem,renlo,renhi,rngrp1,rfchan1,
c     &               rnchan1,isorder,rorder,rspval1,lothresh,status)
c         WRITE(contxt,'(a,i4)')  
c     &    'WADRMF:Failed to go to read matrix in file',ifile

         CALL rdrmf5(iunit,chatter,extname,extcomm,qorder,maxne,maxgrp,
     &               maxelt,
     &               rmfversn,hduclas3,telescop,instrume,detnam,
     &               filter,areascal,chantype,flchan1,ichan,ienerg,
     &               imaxgrp,nelem,renlo,renhi,rngrp1,rfchan1,
     &               rnchan1,isorder,rorder,rspval1,lothresh,status)
         WRITE(contxt,'(a,i4)')  
     &    'WADRMF:Failed to go to read matrix in file',ifile

         IF ( status .NE. 0 ) RETURN

c copy the elements of the first matrix to accumulation arrays

         IF ( ifile .EQ. 1 ) THEN

            DO i = 1, nelem
               rspval(i) = infact(ifile)*rspval1(i)
            ENDDO

c Save the number of channel groups, first channels, and number 
c of channels in each group

            flchan = flchan1

            DO i = 1, ienerg
               rngrp(i) = rngrp1(i)
            ENDDO

            DO i = 1, imaxgrp
               rfchan(i) = rfchan1(i)
               rnchan(i) = rnchan1(i)
            ENDDO

         ENDIF

c If this not the first file then sum the new file with the current
c accumulation. 

         IF ( ifile .GE. 2 ) THEN

c Start by copying the current accumulation into the second work array.

            DO i = 1, maxelt
               rspval2(i) = rspval(i)
            ENDDO
            DO i = 1, maxne
               rngrp2(i) = rngrp(i)
            ENDDO
            DO i = 1, maxgrp
               rfchan2(i) = rfchan(i)
               rnchan2(i) = rnchan(i)
            ENDDO            

c Initialize the group and element pointers

            igroup = 0
            irsp = 0
            igroup1 = 0
            irsp1 = 0
            igroup2 = 0
            irsp2 = 0

c Loop round the response energies

            DO i = 1, ienerg

               DO j = 1, ichan
                  buffer(1,j) = 0.
                  buffer(2,j) = 0.
               ENDDO         

c unpack the response for this energy for the new file and
c place in buffer(1,*).

               DO j = 1, rngrp1(i)
                  igroup1 = igroup1 + 1
                  DO ich = rfchan1(igroup1), 
     &                     rfchan1(igroup1)+rnchan1(igroup1)-1

                     irsp1 = irsp1 + 1
                     buffer(1,ich+1-flchan1) = infact(ifile)
     &                                          *rspval1(irsp1)

                  ENDDO
               ENDDO

c then unpack the response for this energy for the current accumulation and
c place in buffer(2,*).

               DO j = 1, rngrp2(i)
                  igroup2 = igroup2 + 1
                  DO ich = rfchan2(igroup2), 
     &                     rfchan2(igroup2)+rnchan2(igroup2)-1

                     irsp2 = irsp2 + 1

                     buffer(2,ich+1-flchan) = rspval2(irsp2)

                  ENDDO
               ENDDO

c Process this energy range and set the new accumulated response

               rngrp(i) = 0
               count = .FALSE.

               DO j = 1, ichan

c Add two arrays of equal sizes BUFFER1 and BUFFER2 and save the result
c  in BUFFER2 array

                  buffer(2,j) = buffer(1,j) + buffer(2,j)

c If the result is above the threshold then add to the response.

                  IF ( buffer(2,j) .GT. thresh ) THEN  

                     irsp = irsp + 1

c Copy the elements (with the values exceeding the threshold) of BUFFER2 
c  array to the RSPVAL array

                     rspval(irsp) = buffer(2,j)
                     if (irsp .ge. maxelt) then 
                        status = 1101
                        return  
                     endif
                     IF ( .NOT. count ) THEN
                        rngrp(i)  = rngrp(i) + 1

c Save the found channel group and first channel for each group 
c in the corresponding arrays for the first matrix of a sum
 
                        igroup = igroup + 1
                        if (igroup .ge. maxgrp) then 
                            status = 1102
                            return 
                        endif
                        rfchan(igroup) = j + (flchan-1)
                        count = .TRUE.
                     ENDIF

                     rnchan(igroup) = j-rfchan(igroup)+1+(flchan-1)

                  ELSE
        
                     count = .FALSE.

                  ENDIF

               ENDDO

            ENDDO

         ENDIF

c Close the input FITS file

         status = 0
         IF ( inopen ) THEN
            CALL ftclos(iunit,status)
            status = 0
         ENDIF

      ENDDO

c Return the number of groups and elements 

      numgrp = igroup
      numelt = irsp

      RETURN
      END








