C******************************************************************************
C SUBROUTINE: 
C 
C asp_record 
C 
C DESCRIPTION: 
C 
C get asp record from open fits file using buffer I/O 
C 
C AUTHOR/DATE: 
C 
C Eric Gotthelf, Aug 1993 C ASCA GOF, NASA/GSFC.  
C 
C MODIFICATION HISTORY: 
C 
C NOTES:
C 
C USAGE: 
C 
C call asp_record (iunit, first, row, event_time, event_record, status) 
C 
C ARGUMENTS: 
C 
C iunit - I/O unit for opened data file 
C verbose - whether to print out diagnostic info 
C status -FITSIO status 
C 
C PRIMARY LOCAL VARIABLES: 
C 
C fitsio variables - hdtype, rows, filenm, xtensn, rwmode, block, rowlen 
C vardat, fcstln, crpix1, crpix2, tfield, exact 
C tbcol, ttype, tform, tunit, extnam, comment, keywd 
C 
C CALLED ROUTINES: 
C 
C subroutine fcecho - echo message to terminal 
C subroutine ftxxxx - FITSIO calls 
C
C******************************************************************************

        subroutine asp_record (iunit, first, row, aspect_time,
     &     q_param, quality, status)

        implicit none
        
        include 'asca_defs.inc'

        integer iunit, first, row, quality, status
        double precision q_param(4), aspect_time
	integer pcount
        
c     Local:
        
        integer block_size, record_size, buffered_recs, records
        parameter(block_size     = 2880)
        parameter(record_size    = 67)
        parameter(buffered_recs  = 96)
        parameter(records = buffered_recs)
        integer asp_qual(3,records)
        double precision dnull, asp_time(records)
        double precision asp_q(4, records)
        
        integer count, total, index, inull
        integer nrecords, narows
        logical anyf, flgval
        save count, total, nrecords,pcount,asp_time,asp_q,asp_qual
        
        include 'asca_common.inc'
        
c     Start:
        
c     for the first call, set the pointer for the file:
        
        if (first .eq. 0) then
           first = 1
           count = records
           total = records
           nrecords = records
	pcount=0
        end if

        
c     at the end of buffer, read next one:
        
        if (count .eq. total) then
           
c     partial buffer means the end of file:
        
           if (total .lt. records .or. row .gt. nattitude) then
              first = -1
              return
           end if
           
           if (row+records-1 .gt. nattitude) 
     &          nrecords = nattitude - row + 1
           
           narows = nrecords
           call ftgcvd(iunit, atime_col, row, 1, narows, dnull,
     &          asp_time, anyf, status)

           
           narows = nrecords
           call ftgcvd (iunit, q_col, row, 1, narows*4, dnull, asp_q,
     &          anyf, status)

           narows = nrecords
           call ftgcvj (iunit, qstat_col, row, 1, narows*3, inull, 
     &          asp_qual, anyf, status)
                      
c     reset COUNT, and read the buffer, total is the number of words read:
           
           count = 0
           total = nrecords

        end if
        
c     if there is something left in the buffer, fill the record:
        
        count = count + 1
        row = row + 1
        
        do index = 1, 4
           q_param(index) = asp_q(index, count)
        end do
        
        aspect_time = asp_time(count)
        quality = asp_qual(1,count)+asp_qual(2,count)*256 +
     &       asp_qual(3,count)*256*256

        return
        
        end
      


