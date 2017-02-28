
       subroutine tim_point(rbf_unit,newrec1, timezero, status)
c
       include 'xrrbstr.h'
       INTEGER*2    nday(num_rdata_0),  dt(num_rdata_0)
       REAL count(num_rdata_0), err(num_rdata_0)
       REAL*8 dyear, shfdyr
       INTEGER  rbf_unit, irec, year, itime(5), key(5)
       INTEGER  i,status  ,time(num_rdata_0),shfkey
       REAL*8 dtime, tsec, timezero
       character(80) context
       RECORD /rnewbuf_rec_1/newrec1       

         irec=5
         READ(rbf_unit,REC=irec, ERR=990)
     &                 (nday(i), time(i), dt(i), count(i), err(i),
     &                  i=1,num_rdata_0)
c
c first value in the array in days
c      
         dtime = dble(dfloat(nday(1))) + dble(time(1))/86400000.d0
         write(context,'('' dtime first value in file'', f17.11)')dtime
         call xwrite(context,15)
c
c convert shf into decimal year
         dyear=shfdyr(newrec1.utctime1, status)
         key(1)=dyear
c convert time in days into d h m s ms
         call xrdhms(dtime,tsec,itime,1)
         write(context,100)(itime(i),i=1,5)
100      format (' Itime', 5(1X,i4))
         call xwrite (context, 15)
         do i=1,5
           key(i+1)=itime(i)
         enddo
         write(context,200)(key(i),i=1,5)
200      format (' Key', 5(1X,i4))
         call xwrite (context, 15)
c convert y day hh min sec into shf
         call timak(key, shfkey)
         write(context, '('' shfkey'', i10)')shfkey         
         timezero = dfloat(shfkey) 
         return
990      call xwrite(' Error reading in tim_point', 10)
         return
         end
       
