*+MUDCIF
      subroutine mudcif
      implicit none

C Description: Many UpDates to a CIF.
C              Enter many cal filenames (+ extns) via ASCII file.


C              (1) Reads info from 'data/input.ASCII'.
C              This file contains 4 lines :  name of mission, name of
C              instrument, name of instrument alias
C              and name of a 'special' ASCII file.
C              The 'special' ASCII file contains a list
C              of 'good' quality calibration files appropriate
C              for the given mission and instrument; together with
C              'last modification date' info.
C              The cal filenames[+ extns] + dates are put into memory.
C
C              (2) cal filenames[+extns] + other info from the
C              CIF (appropriate for the given mission, instrument)
C              are put into memory.
C
C              (3) For any file extn in the ASCII list which is also
C              in the CIF whose 'last modification date' is later than
C              its CIF installation date ---> OVERWRITE the row in the
C              CIF for this file extn to describe the more recently changed
C              dataset
C
C              (4) For any file extn in the ASCII list which is also
C              in the CIF whose 'last modification date' is earlier
C              or equal to its CIF installation date ---> DO NOTHING !
C
C              (5) For any file extn in the ASCII list which is NOT
C              listed in the CIF ---> ADD a new row (1 for each INSTRUME
C              alias) to the end of the CIF. These rows contain the keyword
C              values from the header of the new cal file extn.
C
C              (6) For any file extn in the CIF NOT listed in the ASCII
C              list ----> switch the quality flags in the CIF for these
C              files to 'bad' (qualval=5).
C
C
C
C Passed parameters : none
C
C User i/ps required (prompted for): none
C
C called routines :
C  subroutine GPARAMS      : (CALLIB) gets parameters from mudcif.par file
C  subroutine WT_FERRMSG   : (CALLIB) dumps error messages
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine CFRELUN      : (CALLIB) releases FORTRAN logical unit no
C  subroutine CALDB_INFO   : (CALLIB) checks local CALDB available to user
C                                     for required mission/instrument
C  subroutine RDCNFG       : (CALLIB) gets the CALDB config file for the
C                                     given mission and instrument
C  subroutine FCECHO       : (FITSIO) write to standard o/p
C  subroutine CIFINFO      : (CALLIB) obtains necessary info from CIF and
C                                     writes to memory
C  subroutine MOD_CIF      : (CALLIB) does the business
C
C compilation & linking :
C  link with CALLIB & FITSIO
C
C Origin: Written for the Calibration Database.
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:96 Oct 15) original version
C  Lorraine Breedon (1.1.0:97 Jun 11) accounts for extra CIF column CAL_ORIGIN
C  Lorraine Breedon (1.2.0:97 Aug 5) now flags files NOT INCLUDED in
C                                    '_special.ASCII' file as 'bad'
C Lorraine Breedon (1.3.0:98 Aug 20) y2k compliance changes
c Jeff Guerber (1.3.1 1999-02-23) mod_cif: can't call ftgstm with const arg
c
	character(7) version
        parameter (version = '1.3.1')
*-

C internals
        integer arrsz
        parameter (arrsz=3000)
	character(20) mission, instru,alias,chextno,cif_inst(arrsz),
     &               cifinstval,cal_origin(arrsz),wherefrom
	character(160) message,ciftmp
        character(160) contxt,inpfil,lclf, compf(arrsz),inp_info(50),
     &            special,info,calfiles(arrsz),infile
        character(160) name(arrsz), lclfile(arrsz),compfile, filename
        character(160) path(arrsz), finalf3, localf, finalf, finalf2,
     &                fext,fileandext(arrsz),calfexp,caldbpth,cif

        character(160) junkdir,fileval,dirval
        character(10) date,dat(arrsz),dates,udmode,trans(10)
        character(30) errstr,warnstr
	integer errstat,kunit,status,chatter,nospace,caldblen
	integer fcstln,nfiles,ncomm,lastslash,dirstr, ncalfiles,
     &          flen, flen1, flen2, n,m,fl1,fl2,len,len1,length,lunit
	integer iy,id,imon,cifqual(arrsz), newcif_unit,oldcif_unit,
     &          ext(arrsz),extno,extnum,qualval,cifrow,ntrans,
     &          alias_unit,qualcoll
     	double precision cifinstdat(arrsz),mjd,lmoddate(arrsz)
        logical there,agree,qual_switch,old_open,new_open,alias_open

C initialise
	character(40) taskname
	common /task/ taskname

	taskname = 'MUDCIF '//version
	errstat = 0
	nfiles  = 0
	ncalfiles = 0
        ncomm   = 0
        ntrans = 0
	iy = 0
	id = 0
	imon = 0
        infile=' '
        fileval=' '
        dirval=' '
        ciftmp=' '
        do 5 n=1,arrsz
              cif_inst(n)=' '
              compf(n)=' '
              calfiles(n)=' '
              name(n)=' '
              lclfile(n)=' '
              path(n)=' '
              fileandext(n)=' '
              dat(n)=' '
              ext(n)=0
              cifinstdat(n)=0.0
              lmoddate(n)=0.0
              cal_origin(n)=' '
5       continue
	do 6 n=1,10
               trans(n)=' '
6	continue

	message = '** MUDCIF '//version
        call fcecho(message)
        errstr = '** MUDCIF '//version//' ERROR: '
        warnstr = '** MUDCIF '//version//' WARNING: '

C Get the parameters from the par file
	call gparams(inpfil,calfexp,qual_switch,chatter,errstat)


C If there's an error getting parameters then return
	if (errstat .ne. 0) return

C see if the user has set the environment variable specified in the
C calfexp arguement
	call ctrlog(calfexp,fcstln(calfexp),caldbpth,caldblen)
	if (caldblen .eq. 0) then
	   contxt = 'Environment variable not set'
    	   message = '**MUDCIF '//version//' ERROR : '//contxt
	   call fcecho(message)
    	   message = 'The environment variable or logical "'
     &     //calfexp(:fcstln(calfexp))//'"'
	   call fcecho(message)
           message= 'must be set to point at the top of the Caldb'
           call fcecho(message)
           message= 'see the Caldb Users Guide for details'
           call fcecho(message)
	   errstat=2
	   return
	endif


C Open data/input.ASCII file
	     call cgetlun(lunit)
	     open (lunit,file=inpfil,iostat=errstat,status='OLD')
    	     if (errstat .ne. 0) then
                inquire (file=inpfil,exist=there)
                   if (.not.there) then
                     contxt = 'cant find '//inpfil
                     message = '**MUDCIF '//version//'ERROR : '//contxt
                     goto 10
		   endif
		contxt = ' can find but cant open '//inpfil
    	        message = '**MUDCIF '//version//' ERROR : '//contxt
10             continue
	        call fcecho(message)
		status=20
		return
	     endif

20	continue

C read the 4 lines of the input ASCII file containing names of mission,
C instrument, alias and 'special' ASCII file

	     read (lunit,'(A)',iostat=errstat,end=50) info
	     if (errstat .ne. 0 ) then
	     	contxt = 'error reading '//inpfil
	        message = '**MUDCIF '//version//' ERROR : '//contxt
	        call fcecho(message)
		status=errstat
	     	return
	     endif

C Skip any blank lines or lines beginning with '#' characters to obtain
C name of 'special' ASCII file

	     if ((info(1:1).ne.'#').and.(fcstln(info).ne.0)) then
	          nfiles = nfiles + 1
                  inp_info(nfiles) = info
             else
                  goto 20
             endif
             goto 20
50	continue

        if (nfiles .le. 3) then
            contxt = 'input ASCII file: '//inpfil//' empty !! '
	    message = '**MUDCIF '//version//' ERROR : '//contxt
	    call fcecho(message)
            status=-1
	    return
	endif

C Now get names of mission, instrument, alias  and actual name of the 'special' file

	do 55 m=1,nfiles
                special=inp_info(m)
         	len=fcstln(special)
                len1=len-1
                length=0
                nospace=0
                do 60 n=0,len1
                 if (special(len-n:len-n) .ne.'  ') then
                     nospace=nospace+1
                     if (nospace .eq. 1) length=len-n
                     inp_info(m)=special(len-n:length)
                 endif
60              continue
55	continue


C Open 'special' ASCII file
	     inpfil=inp_info(nfiles)
	     call cgetlun(kunit)
	     open (kunit,file=inpfil,iostat=errstat,status='OLD')
    	     if (errstat .ne. 0) then
                inquire (file=inpfil,exist=there)
                   if (.not.there) then
                     contxt = 'cant find special ASCII file '//inpfil
                     message = '**MUDCIF '//version//'ERROR : '//contxt
                     goto 110
		   endif
		contxt = 'can find but cant open special ASCII file '//inpfil
    	        message = '**MUDCIF '//version//' ERROR : '//contxt
110             continue
	        call fcecho(message)
		status=20
		return
	     endif

C reset nfiles parameter
	nfiles=0

100	continue

C get calibration files from 'special' ASCII file

	     read (kunit,'(A)',iostat=errstat,end=1000) lclf
	     if (errstat .ne. 0 ) then
	     	contxt = 'error reading special ASCII file'
	        message = '**MUDCIF '//version//' ERROR : '//contxt
	        call fcecho(message)
		status=errstat
	     	return
	     endif

C Skip blank lines or lines beginning with '#' characters to obtain cal
C filenames[#ext] and cal date info (= last modification date)
	     if ((lclf(1:1).ne.'#').and.(fcstln(lclf).ne.0)) then
	          nfiles = nfiles + 1
                  filename = lclf(1:75)
                  date = lclf(99:108)
       	          lclfile(nfiles) = filename
                  dat(nfiles) = date
              else
                  goto 100
              endif
              goto 100


1000	continue

        if (nfiles .eq. 0) then
            contxt = 'special ASCII file: '//inpfil//' empty !! '
	    message = '**MUDCIF '//version//' ERROR : '//contxt
	    call fcecho(message)
            status=-1
	    return
	endif

C close the ASCII files

	close (kunit)
	call cfrelun(kunit)
        close (lunit)
	call cfrelun(lunit)



C Get calibration filenames from special ASCII file
C (**NB** all filenames contain '$CALDB' to represent the top of the
C caldb directory tree)

	do 1100 m=1,nfiles
                localf=lclfile(m)
                dates=dat(m)
C strip off [#ext], write filename (including path) to 'compfile'.
C get actual ext no and convert to integer
                flen=fcstln(localf)
                compfile=' '
                flen1=flen-1
                flen2=0
                fl1=0
                fl2=0
                do 1150 n=0,flen1
                          if (localf(flen-n:flen-n).eq.']') fl1=flen-n
                          if (localf(flen-n:flen-n).eq.'[') fl2=flen-n-1
1150            continue
	        compfile=localf(1:fl2)
	        chextno=localf(fl2+2:fl1-1)
                if (fcstln(chextno) .eq. 1) then
                    read (chextno(1:1),fmt='(I1)') extno
	        else
                    read (chextno(1:2),fmt='(I2)') extno
	        endif
	        ext(m)=extno


c create COMPLETE filename using 'compfile' info and the environment variable
C specified in the calfexp arguement (NB strip off $CALDB characters first)

 		compf(m)=caldbpth(:fcstln(caldbpth))//localf(7:fl2)

C now strip filenames of their paths, write the actual filename
C to 'finalf2' and the path to 'finalf3' (ignoring the initial $CALDB/
C characters).
C Then concatenate [#extn] onto filename
                finalf2=' '
                finalf3= ' '
                flen=fcstln(compfile)
                lastslash=0
                dirstr=0
                flen1=flen-1
                do 1200 n=0,flen1
			finalf=compfile(flen-n:flen)
                        if (dirstr .eq. 1) then
                           finalf3=compfile(8:flen2-1)
                        endif
                        if (compfile(flen-n:flen-n).eq.'/') then
                           lastslash=lastslash+1
                           dirstr=1
                           if (lastslash .eq. 1) then
                              flen2=flen-n
                              finalf2=compfile(flen-n+1:flen)
                              fext=localf(flen-n+1:fl1)
                           endif
                        endif
1200            continue
	        name(m)=finalf2
	        path(m)=finalf3
	        fileandext(m)=fext
c
c convert last modification date for each cal file into MJD -
C Parse date into day month and year
               call fts2dt( dates, iy, imon, id, errstat )
               if (errstat .ne. 0) then
                  contxt = 'Unable to parse date argument: '//
     &            date(:fcstln(date))
               endif

C Convert id, im, iy to modified julian date.  New fitsio date functions
C guarantee 4-digit years, so call ccldj instead of ccaldj.



               call ccldj(iy,imon,id,mjd,errstat)
	       if (errstat .ne. 0) then
	           message=errstr//
     &                     'ccldj: converting last mod date to MJD'
	           call fcecho(message)
                   return
	       endif

	       lmoddate(m) = mjd


1100	continue

c now check that the calibration files exist.

	do 1300 m=1,nfiles
	          inpfil=compf(m)
                  inquire (file=inpfil,exist=there)
                  if (.not.there) then
                     contxt = 'cant find '//inpfil
                     message = '**MUDCIF '//version//'WARNING:'
     & //contxt
	             call fcecho(message)
                  endif
1300	continue


C get and read the caldb.config file

	mission=inp_info(1)
        instru=inp_info(3)
        alias=inp_info(3)
C NOTE : caldb_info calls the routine ..rdcnfg...where the instru argument
C MUST be the 'instrument alias' rather then the 'instrument'
	if ((calfexp(1:5).eq.'caldb').or.(calfexp(1:5).eq.'CALDB')) then
	    call caldb_info(chatter,'INST',mission,instru,errstat)
	    if (errstat.ne.0) then
               message='CALDB not defined/available'
               call fcecho(message)
	       message='task requires CALDB to be both defined '//
     &             '& available in order to run'
               call wt_ferrmsg(errstat,message)
	       goto 999
	    endif
	    errstat=0
	    call rdcnfg(mission,alias,.false.,calfexp,junkdir,errstat)

            if (errstat .ne. 0) return
        endif




C get the relevant CIF info and place in memory
	errstat=0
	call cifinfo(calfexp,arrsz,cif_inst,calfiles,cifinstdat,
     &           cifqual,cal_origin,ncalfiles,chatter,errstat)
        if (errstat .ne. 0) return




C now do any relevant OVERWRITING to the CIF :
C
C First have to open the relevant CIF ('old') and a copy ('new') and also the
C alias_config.fits files.
C NOTE : opcifs calls the routine ...rdcaf1....where the instru argument
C MUST be the 'instrument' rather then the 'instrument alias'!!


        errstat=0
	instru=inp_info(2)
        call ftupch(instru)

        newcif_unit=0
        oldcif_unit=0
        alias_unit=0
        old_open=.false.
        new_open=.false.
        alias_open=.false.
	call opcifs(calfexp,old_open,new_open,alias_open,oldcif_unit,
     &              newcif_unit,alias_unit,ciftmp,
     &              mission,instru,trans,ntrans,chatter,errstat)

        if (errstat.ne. 0) return


C ok...here goes
        errstat=0
        m=0
        n=0
        do 1600 m=1,ncalfiles
                  wherefrom=cal_origin(m)
                  if ((wherefrom(1:7) .eq. 'HEASARC') .or.
     &                 (wherefrom(1:7) .eq. ' ')) then
                      agree=.false.
                      do 1700 n=1,nfiles


C for each row in the CIF compare the cal filename[#ext]
C with the cal filenames[#ext] (given in the ASCII file list)
                         if (fileandext(n).eq.calfiles(m)) then
	                     agree=.true.

C now compare the last modification date of the cal file (given in the
C ASCII list) with its installation date in the CIF...

                              if (lmoddate(n).gt.cifinstdat(m)) then

C in this case :
C the last modification date for the cal file in the ASCII file list
C is LATER than its installation date in the CIF, therefore must
C OVERWRITE the current line in the CIF to describe the more recently
C 'changed' dataset.
C if 'qualswitch' flag = .true. then it is assumed that this file is now
C `good' quality so must switch CAL_QUAL flag in CIF to 0 (good).
C if 'qualswitch' flag = .false. then do not assume the file is good and hence
C keep the quality status already set in CIF

	                       udmode='over'
			       infile=compf(n)
                               cif=calfexp
                               extnum=ext(n)
                               if (qual_switch .eqv. .true.) then
                                  qualval=0
                               else
                                  qualval=cifqual(m)
                               endif
                               fileval=name(n)
	                       dirval=path(n)
                               cifrow=m
                               cifinstval=cif_inst(m)
                               call mod_cif(chatter,infile,cif,
     &                            newcif_unit,udmode,trans,ntrans,
     &                            extnum,qualval,fileval,dirval,
     &                             cifrow,cifinstval,errstat)
                               if (errstat .ne. 0) return

                            endif

C in this case :
C the last modification date is EARLIER or the SAME as the installation date
C in the CIF....therefore do nothing

                        endif

1700		      continue
                       if (agree .eqv. .false.) then
c there is a cal file present in the CIF which is NOT present in the ASCII file.
C if 'qualswitch' flag = .true. then it is assumed that this file is now 'bad'
C and so must change current line in CIF with CAL_QUAL switched to 5 (bad).
C if 'qualswitch' flag = .false. then do not assume file is bad and do nothing
C to CIF.
                          if (qual_switch .eqv. .true.) then
                            errstat=0
                            call ftgcno(newcif_unit,.true.,
     &                           'CAL_QUAL',qualcoll,errstat)
	                    if (errstat .ne. 0) then
	                       message=errstr//'Cannot find index file
     & column CAL_QUAL'
		               call fcecho(message)
                               return
	                    endif

                            qualval=5
                            cifrow=m

	                    call ftpclj(newcif_unit,qualcoll,
     &                            cifrow,1,1,qualval,errstat)
                            if (errstat .ne. 0) return

                          endif
                       endif
                    endif
1600	continue

C Close the relevant CIF ('old') and the copy ('new') and also the
C alias_config.fits file.

  	call clcifs(calfexp,old_open,new_open,alias_open,oldcif_unit,
     &               newcif_unit,alias_unit,ciftmp,chatter,errstat)
        if (errstat .ne. 0) return


C now do any necessary ADDITIONS to the CIF

C First have to open the relevant CIF ('old') and a copy ('new') and also the
C alias_config.fits files.

        errstat=0
        newcif_unit=0
        oldcif_unit=0
        alias_unit=0
        old_open=.false.
        new_open=.false.
        alias_open=.false.
	call opcifs(calfexp,old_open,new_open,alias_open,oldcif_unit,
     &              newcif_unit,alias_unit,ciftmp,
     &              mission,instru,trans,ntrans,chatter,errstat)
        if (errstat.ne. 0) return

C ok...here goes
        errstat=0
        do 1800 m=1,nfiles
	          agree=.false.
                  do 1900 n=1,ncalfiles
C for each cal filename[#ext] (given in the ASCII file list) , compare
C this to the cal filenames[#ext] in the rows of the CIF.
                         if (fileandext(m).eq.calfiles(n))  then
                            agree = .true.
                         endif
1900	          continue
	          if (agree .eqv. .false.) then

C then there is a cal file present in the ASCII file and NOT in the CIF.
C this file must be a completely new good quality file and so must ADD
C this file to the CIF..


	              udmode='add'
	              infile=compf(m)
                      cif=calfexp
                      extnum=ext(m)
                      qualval=0
                      fileval=name(m)
	              dirval=path(m)
                      cifrow=0
                      cifinstval=cif_inst(n)
                      call mod_cif(chatter,infile,cif,newcif_unit,
     &                             udmode,trans,ntrans,
     &                             extnum,qualval,fileval,dirval,
     &                             cifrow,cifinstval,errstat)
                      if (errstat .ne. 0) return


		  endif
1800	continue

C Close the relevant CIF ('old') and the copy ('new') and also the
C alias_config.fits file.

	call clcifs(calfexp,old_open,new_open,alias_open,oldcif_unit,
     &               newcif_unit,alias_unit,ciftmp,chatter,errstat)
        if (errstat .ne. 0) return


	message=' '
        call fcecho(message)
	message = '** MUDCIF '//version//' finished **'
        call fcecho(message)

	return
999	continue

	end

C----------End of MUDCIF subroutine-----------------------------------

C-----------------------------------------------------------------------

*+GPARAMS
	subroutine gparams(inpfil,calfexp,qual_switch,chatter,
     &               status)

	implicit none
        character*(*) inpfil,calfexp
     	integer status,chatter
        logical qual_switch


C Description:
C  Gets the parameters for MUDCIF from the parameter
C  file.

C
C passed parameters:
C  CALFEXP     :    value of environment variable
C  STATUS      :    error flg (0=OK)
C  CHATTER     :    chattiness flag for o/p (5 low,10 normal,15 high)
C
C user i/ps (prompted for):
C  INPFIL      :   the name of the input ASCII file (=input.ASCII).
C                  This file contains 4 lines :  name of mission, name of
C                  instrument, name of instrument alias
C                  and name of a 'special' ASCII file.
C                  The 'special' ASCII file contains a list
C                  of 'good' quality calibration files appropriate
C                  for the given mission and instrument.

C
C Called routines :
C  subroutine UGLGST       : (XPI) gets parameter values from mudcif.par
C  subroutine FCECHO       : (FITSIO) write to standard o/p
C
C compilation & linking :
C  link with XPI and FITSIO
C
C Origin: Written for the Calibration Database.
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:96 Aug 12) original version
C  Lorraine Breedon (1.1.0:97 Jun 11) accounts for extra CIF column CAL_ORIGIN
        character(7) version
        parameter (version='1.1.0')
*-
C Internals

        character(50) contxt
        character(80) message
        integer errstat

C initialise
C Set Status flg to 'no problem!'
        status = 0
        errstat = 0



C Get inpfile parameter
        call uclgst('inpfile', inpfil, errstat)
C If there's an error getting inpfile, return
        if(errstat.ne.0)then
           contxt = 'cant get inpfile parameter'
	   message = '**UCLGST '//version//' ERROR : '//contxt
	   call fcecho(message)
           status = 1
           return
        endif

C Get calfexp parameter
        call uclgst('calfexp', calfexp, errstat)
C If there's an error getting calfexp, return
	if(errstat.ne.0) then
             contxt = 'cant get calfexp parameter'
             message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
             status = 1
             return
        endif

C Get qualswitch parameter
        call uclgsb('qualswitch',qual_switch, errstat)
C If there's an error getting qualswitch, return
	if(errstat.ne.0) then
             contxt = 'cant get qual_switch parameter'
             message = '**UCLGST '//version//' ERROR : '//contxt
	     call fcecho(message)
             status = 1
             return
        endif


C Get the chatter parameter
        call uclgsi('chatter', chatter, errstat)
	 if(errstat.ne.0)then
             contxt = 'cant get chatter parameter'
	     message = '**UCLGST '//version//' ERROR : '//contxt
             call fcecho(message)
	     chatter=9
	     message = ' setting chatter=9'
	     call fcecho(message)
             status = 0
        endif




        return
        end

C-----------------------end of GPARAMS subroutine----------------------

C---------------------------------------------------------------------

*+CIFINFO
        subroutine cifinfo(calf,size,cif_instval,namesext,inst_date,
     &               cif_quality,origin,nrows,chatter,status)

        implicit none
        integer size
        character*(*) calf,namesext(size),cif_instval(size),origin(size)
       	integer status,chatter,nrows,cif_quality(size)
	double precision inst_date(size)

C Description:
C  opens a CIF (appropriate for a given mission and instrument).
C  reads appropriate column info and places in memory


C passed parameters:
C  CALFEXP             :    value of environment variable
C  STATUS              :    error flg (0=OK)
C  CHATTER             :    chattiness flag for o/p (5 low,10 normal,15 high)
C  NROWS(o/p)          :    the number of rows in the CIF
C  CIF_INSTVAL(o/p)    :    array containing info from CIF column INSTRUME
C  NAMESEXT(o/p)       :    array containing info from CIF columns CAL_FILE and
C                           CAL_XNO i.e. cal filename with extn number
C                           concatenated
C  INST_DATE(o/p)      :    array containing info from CIF column CAL_DATE
C  CIF_QUALITY(o/p)    :    array containing info from CIF column CAL_QUAL
C  ORIGIN(o/p)         :    array containing info from CIF column CAL_ORIG
C
C user i/ps (prompted for):
C none
C
C Called routines :
C  subroutine FCECHO       : (FITSIO) write to STDOUT
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine WT_FERRMSG   : (CALLIB) dumps error messages if necessary
C  subroutine FTOPEN       : (FITSIO) opens CIF
C  subroutine FTMAHD       : (FITSIO) moves to 1st extension in FITS file
C  subroutine FTGKYJ       : (FITSIO) determines no. rows in CIF
C  subroutine FTGCNO       : (FITSIO) gets column nos of all columns to be read
C  subroutine FTGCVS       : (FITSIO) gets cal file name, instrument values from C                                     CIF
C  subroutine FTGCVJ       : (FITSIO) gets extn numbers from CIF

C compilation & linking :
C  link with XPI, FITSIO & CALLIB
C
C Origin: Written for the Calibration Database.
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:96 Oct 11) original version
C  Lorraine Breedon (1.1.0:97 Jun 11) accounts for extra CIF column CAL_ORIGIN
	character(7) version
        parameter (version = '1.1.0')


*-
C Internals
	character(1) chextno1,bracket1,bracket2
        character(2) chextno2
	character(10) instdate
        character(20) cal_orig
	character(30) sjunk,errstr,wrnstr,instrument
        character(160) file,calfile,calfile1,calfile2,
     &                  calfile3,message
        integer iunit, ijunk, nax2val,fcstln,errstat,nn,i,extno
	integer filcol,datcol,xnocol,len,len1,id,iy,imon,flen2
	integer lencal,instcol,qualcol, cal_qual,origcol
        double precision mjd

	logical ljunk,origin_flag

C initialise and set up defaults
        origin_flag=.true.
        file=' '
        calfile=' '
        calfile1=' '
        calfile2=' '
        calfile3=' '
       	errstat = 0
        status=0
	len=0
        len1=0
        flen2=0
	i=0
        id=0
        imon=0
        iy=0
        do 25 i=1,size
                inst_date(i)=0.0
                namesext(i)=' '
	        cif_instval(i)=' '
                cif_quality(i)=0
                origin(i)=' '
25      continue


        errstr = '** CIFINFO '//version//'ERROR: '
        wrnstr = '** CIFINFO '//version//'WARNING: '


C Open the CIF
        call cgetlun(iunit)
        call ftopen(iunit, calf, 1, ijunk, errstat)
        if(errstat.ne.0)then
           message=errstr// ' opening the CIF'
	   call wt_ferrmsg(errstat,message)
           status = errstat
           return
        endif

C Move to the first extension
        call ftmahd(iunit, 2, ijunk, errstat)
        if(errstat.ne.0)then
            message=errstr// ' moving to 1st extension'
	    call wt_ferrmsg(errstat,message)
	    status = errstat
	    return
	endif

C Find out how many rows the CIF contains
        call ftgkyj(iunit, 'NAXIS2', nax2val, sjunk, errstat)
        if(nax2val.eq.0)then
             status = errstat
             return
        endif

C Get the column numbers of all the columns to be read

	call ftgcno(iunit,.true.,'INSTRUME',instcol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting INSTRUME column number '
	   call wt_ferrmsg(errstat,message)
          status = errstat
	  return
	endif


	call ftgcno(iunit,.true.,'CAL_FILE',filcol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting CAL_FILE column number '
	   call wt_ferrmsg(errstat,message)
          status = errstat
	  return
	endif

        call ftgcno(iunit,.true.,'CAL_XNO',xnocol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting CAL_XNO column number '
	   call wt_ferrmsg(errstat,message)
          status = errstat
	  return
	endif

        call ftgcno(iunit,.true.,'CAL_DATE',datcol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting CAL_DATE column number '
	   call wt_ferrmsg(errstat,message)
          status = errstat
	  return
	endif

        call ftgcno(iunit,.true.,'CAL_QUAL',qualcol,errstat)
        if(errstat.ne.0)then
           message=errstr// ' getting CAL_QUAL column number'
	   call wt_ferrmsg(errstat,message)
	   status = errstat
	   return
	 endif

        call ftgcno(iunit,.true.,'CAL_ORIG',origcol,errstat)
        if(errstat.ne.0)then
           message=wrnstr// ' getting CAL_ORIG column number'
           call fcecho(message)
           origin_flag=.false.
	endif


C Read required info from each row
	nrows=nax2val
	do 100 i=1,nax2val
	     errstat = 0

C get instrument value
	     call ftgcvs(iunit,instcol,i,1,1,' ',instrument,ljunk,
     &                   errstat)
	     if (errstat .ne. 0) then
	       	 message=errstr// ' getting instrument value '
	         call wt_ferrmsg(errstat,message)
                 status=errstat
                 return
	     endif

C Get filename value
	     call ftgcvs(iunit,filcol,i,1,1,' ',file,ljunk,errstat)
	     if (errstat .ne. 0) then
	       	 message=errstr// ' getting cal file name '
	         call wt_ferrmsg(errstat,message)
                 status=errstat
                 return
	     endif


C Get cal filename in the CIF independant of path
             len=fcstln(file)
             len1=len-1
    	     do 120 nn=0,len1
	            if (file(len-nn:len-nn).eq.'/') goto 150
                    calfile=file(len-nn:len)
120          continue

150          continue



C Get ext number

	     call ftgcvj(iunit,xnocol,i,1,1,0,extno,ljunk,errstat)
	     if (errstat .ne. 0) then
	       	 message=errstr// ' getting extension number '
	         call wt_ferrmsg(errstat,message)
                 status=errstat
                 return
	     endif

C Concatenate [#ext] onto cal filename
	     lencal=fcstln(calfile)
	     bracket1='['
             bracket2=']'
	     if (extno .lt. 10) then
                write (chextno1,fmt='(I1)') extno
                calfile1=calfile(1:lencal)//bracket1
                calfile2=calfile1(1:lencal+1)//chextno1
                calfile3=calfile2(1:lencal+2)//bracket2
             else
                write (chextno2,fmt='(I2)') extno
                if (extno .le. 99) then
                   calfile1=calfile(1:lencal)//bracket1
                   calfile2=calfile1(1:lencal+1)//chextno2
                   calfile3=calfile2(1:lencal+3)//bracket2
                else
                   message=errstr//calfile
	           call fcecho(message)
                   message='extension number greater than 99!'
	           call fcecho(message)
                   status=1
                   return
                endif
	     endif


C Get installation date

	     call ftgcvs(iunit,datcol,i,1,1,' ',instdate,ljunk,errstat)
	     if (errstat .ne. 0) then
	       	 message=errstr// ' getting installation date '
	         call wt_ferrmsg(errstat,message)
                 status=errstat
                 return
	     endif

C convert installation date into MJD-
C Parse date into day month and year
               call fts2dt(instdate, iy, imon, id, errstat )
               if (errstat .ne. 0) then
                  message = 'Unable to parse date argument: '//
     &            instdate(:fcstln(instdate))
                  call fcecho(message)
               endif

C Convert id, im, iy to modified julian date.  New fitsio date functions
C guarantee 4-digit years, so call ccldj instead of ccaldj.



               call ccldj(iy,imon,id,mjd,errstat)
	       if (errstat .ne. 0) then
	           message=errstr//
     &                     'ccldj: converting last mod date to MJD'
	           call fcecho(message)
                   return
	       endif




C get quality value

	     call ftgcvj(iunit,qualcol,i,1,1,0,cal_qual,ljunk,errstat)
	     if (errstat .ne. 0) then
	       	 message=errstr// ' getting quality value '
	         call wt_ferrmsg(errstat,message)
                 status=errstat
                 return
	     endif

             if (origin_flag .eqv. .true.) then
C get origin value

	        call ftgcvs(iunit,origcol,i,1,1,' ',cal_orig,ljunk,
     &                      errstat)
	        if (errstat .ne. 0) then
	       	   message=errstr// ' getting CAL_ORIGIN value '
	           call wt_ferrmsg(errstat,message)
                    status=errstat
                    return
                 else
                    origin(i)=cal_orig
	        endif
              endif

C write info read from CIF into memory
	     cif_instval(i) = instrument
	     namesext(i) = calfile3
             inst_date(i) = mjd
             cif_quality(i) = cal_qual

100	     continue



C close the CIF
	call ftclos(iunit,errstat)
	if (errstat .ne. 0) then
	     status = errstat
	else
	     call cfrelun(iunit)
	endif
	return
999	continue
	end


C--------------------End of CIFINFO subroutine--------------------------

C---------------------------------------------------------------------
*+MOD_CIF
        subroutine mod_cif(chatter, infile, cif, ounit,udmode, trans,
     &                     ntrans, extnum,qualval,
     &                    fileval, dirval, cifrow, cifinstval, ierr)

        implicit none
        character*(*) infile, cif,udmode, fileval, dirval,
     &                cifinstval,trans(10)
	integer ierr, chatter, cifrow,qualval,extnum,ounit,ntrans

C-----------------------------------------------------------------------
C Description: Accesses the cal file, infile, and searches for OGIP Caldb
C              cal datasets in the appropriate CIF.
C              ** NB **
C              udmode ='over' option is for a cal file already listed in CIF.
C              udmode ='add' option is for a cal file NOT listed in the CIF.
C              *********
C              If udmode='over' the row in the CIF containing infile
C              is OVERWRITTEN with the keyword values from the header of the
C              latest version of infile.
C              If udmode='add' then a new row (1 for each INSTRUME alias) is
C              ADDED to the end of the CIF. These rows contain the keyword
C              values from the header of the new cal file, infile.
C
C              *** This is a variant of the routine UPDCIF *****


C Arguments:   infile  (i): the full name (including path up to the top
C                           of the CALDB directory tree) of the FITS file
C                           containing a Calibration dataset.
C              cif     (i): the name of the Calibration Index File
C              ounit   (i): logical unit no for CIF
C              udmode  (i): update mode required
C              trans   (i): array containing the 'alias' values for instrument
C              ntrans  (i): number of 'alias' values

C              fileval (i): the file name (only) of the FITS file
C              dirval  (i): the directory path (data/{mission}/{instrument}/..)
C                           of the FITS file.
C              extnum  (i): the extension (data set) of the FITS file.
C              qualval (i): the quality value of the data set (default=0;good)
C              cifrow  (i): the row in the CIF to be OVERWRITTEN if
C                           udmode option ='over'. If udmode option = 'add'
C                           then cifrow is set to 0 since the new row number(s)
C                           at the end of the CIF will be determined later.
C
C Origin:      Written for the OGIP Calibration Database
C
C Authors/Modification History:
C   Ron Zellar   (1.0.0: 1993 Jun 09) Original version
C   Ron Zellar   (2.0.0: 1994 Aug 08) Added envar variable for cgdr
c   Ian M George  (3.0.0: 1996 Feb 06) Redesign to include udmode options
C   Lorraine Breedon (3.0.1: 1996 Nov 01) Redesigned to parse more parameters:
C                                         infile (the FULL name); fileval;
C                                         dirval;extnum;qualval;cifrow;ounit;
C                                         trans; ntrans.

C                                         Since extnum parsed directly,cal file
C                                         'extension search' commented out.

C                                         Udmode options written.

C                                         Code dealing with the opening and
C                                         closing of CIFS (`old' and 'new'
C                                         cif.tmp ) and the alias_config.fits
C                                         ripped out and placed into 2 new
C                                         subroutines (OPCIFS.f & CLCIFS.f)
C

C                                         ** Renamed from UPDCIF to MOD_DIF **

C                                         OPCIFS.f & CLCIFS.f are called before
C                                         and after MOD_CIF respectively.
C   Lorraine Breedon (3.0.2: 1997 Jun 11) now accounts for extra CIF column
C                                         CAL_ORIGIN
C   Lorraine Breedon (3.1.2: 1998 Aug 20) y2k compliance changes
c   Jeff Guerber (3.1.3 1999-02-23) can't call ftgstm with const arg
c


        character(7) version
        parameter (version = '3.1.3')
C-----------------------------------------------------------------------
*-
c Internals
	character(6) subname
	parameter (subname = 'mod_cif')
	character(80) comment,cclsval
	character(160) context
        integer fillen, fcstln,errstat,hdutype,iunit
	integer telecol,instcol,filtcol,devcol,dircol,detcol
	integer filecol,cbdcol,xnocol,bunit
	integer refcol,qualcol,datecol,cdescol
	integer oldrows,newrows
	integer blcksz,extval,i
	integer cclscol,cdtpcol,ccnmcol,cvsdcol,cvstcol,row,origcol
c	width,size
	character(10) televal,instval,cvsdatval,cvsdval,date
	character(4)  cdtpval,cval
	character(20) ccnmval,filtval,detval,devval
	character(8)  cvstval
	character(70) cdesval, cbdval(9)
	double precision reftime,second
	logical exact,iopen,result
	logical modcif
c	character(10)  envar
	integer match
c	character(80) dmystr
        integer iyear,imon,iday,ihour,imin,decimals, utmode
        character(30) timestr
        character(160) message



C Initialize the variables
C L.B. (Nov 01 1996) ...comment out extnum
c	extnum = 0

	iopen = .false.
	modcif = .false.
	oldrows = 0
	newrows = 0
        iunit=0
        bunit=0
        iyear=0
        imon=0
        iday=0
        ihour=0
        imin=0
        second=0.0
        decimals=0
        utmode = 0

c Get the units
	call cgetlun(iunit)


C Get length of the cal filename
        fillen = fcstln(infile)
c        type *, ' ** in mod_cif.f **'
c        type *, 'infile=',infile
C Open the calibration file
	call ftopen(iunit,infile(:fillen),0,blcksz,errstat)
	If (errstat .ne. 0) then
	     context='Cannot open the input file: '//
     &	        infile(:fillen)
	     call wtferr(subname,version,errstat,context)
	     ierr = 1
	     goto 999
	else
	     iopen = .true.
	endif

C ------ cal file extension search-----
C L.B. (Nov 01 1996) ...comment out loop (via 1000 continuation statement)

c1000    continue

	errstat = 0

C	keep track of the extension number
	extnum=extnum + 1
	extval = extnum - 1

C	move to the extension given by extnum
	call ftmahd(iunit,extnum,hdutype,errstat)

C	If errstat does not = 0, then end of file
	if ( errstat .ne. 0) then
	     if (modcif) goto 9000
	     context='No datasets indexed'
	     call wtwarm(subname,version,chatter,1,context)
	     goto 999
	endif

C	Look in the cal file for the CCLS0001 keyword
	call ftgkys(iunit,'CCLS0001',cclsval,comment,errstat)
c	if (errstat .ne. 0) goto 1000

C	Get the required calibration keywords

	call ftgkys(iunit,'CCNM0001',ccnmval,comment,errstat)
	if (errstat .ne. 0) then
	   write(context,'(a,i4)')
     &		'Problem reading CCNM0001 keyword in ext: ',
     &		extnum
	   call wtwarm(subname,version,chatter,1,context)
	   call wtinfo(chatter,1,2,'skipping extension')
c	     goto 1000
	else
	   context = 'Dataset Code  :'//ccnmval
	   call wtinfo(chatter,1,1,context)
	endif


	call ftgkys(iunit,'TELESCOP',televal,comment,errstat)
	if (errstat .ne. 0) then
	   write(context,'(a,i4)')
     &		'Problem reading TELESCOP keyword in ext: ',
     &		extnum
	   call wtwarm(subname,version,chatter,1,context)
	   call wtinfo(chatter,1,2,'skipping extension')
c	     goto 1000
	endif

	call ftgkys(iunit,'INSTRUME',instval,comment,errstat)
	if (errstat .ne. 0) then
	   write(context,'(a,i4)')
     &		'Problem reading INSTRUME keyword in ext: ',
     &		extnum
	   call wtwarm(subname,version,chatter,1,context)
	   call wtinfo(chatter,1,2,'skipping extension')
c	     goto 1000
	endif

	call ftgkys(iunit,'CDTP0001',cdtpval,comment,errstat)
	if (errstat .ne. 0) then
	   write(context,'(a,i4)')
     &		'Problem reading CDTP0001 keyword in ext: ',
     &		extnum
	   call wtwarm(subname,version,chatter,1,context)
	   call wtinfo(chatter,1,2,'skipping extension')
c	     goto 1000
	endif

	call ftgkys(iunit,'CVSD0001',cvsdatval,comment,errstat)
	if (errstat .ne. 0) then
	   write(context,'(a,i4)')
     &		'Problem reading CVSD0001 keyword in ext: ',
     &		extnum
	   call wtwarm(subname,version,chatter,1,context)
	   call wtinfo(chatter,1,2,'skipping extension')
c	     goto 1000
	endif

        if (fcstln(cvsdatval) .ne. 10) then

           message='WARNING: CVSD0001 keyword in calibration'//
     &                 ' file is in dd/mm/yy format'
           call fcecho(message)
           message='WARNING: this date will be written in'//
     &                ' yyyy-mm-dd format in the caldb.indx file '

           call fcecho(message)
           call fts2dt(cvsdatval,iyear,imon,iday,errstat)
           if (errstat .ne. 0) then
              message='problem in fts2dt '
              call fcecho(message)
           endif

           call fttm2s(iyear,imon,iday,ihour,
     &                 imin,second,decimals,cvsdatval,errstat)
           if (errstat .ne. 0) then
              message='problem in fttm2s '
              call fcecho(message)
           endif

        endif

        cvsdval=cvsdatval(1:10)



	call ftgkys(iunit,'CVST0001',cvstval,comment,errstat)
	if (errstat .ne. 0) then
	   write(context,'(a,i4)')
     &		'Problem reading CVST0001 keyword in ext: ',
     &		extnum
	   call wtwarm(subname,version,chatter,1,context)
	   call wtinfo(chatter,1,2,'skipping extension')
c	     goto 1000
	endif

C	Calculate the reference time for input into the
C       REF_TIME column
	call calcrt(cvsdatval,cvstval,reftime,errstat)
	if (errstat .ne. 0) then
	     write(cval,'(I4)')extval
	     context='Error calculating the REF_TIME value'//
     &               ' in extension:'//cval
	     call wterrm(subname,version,context)
c	     goto 1000
	endif

	call ftgkys(iunit,'CDES0001',cdesval,comment,errstat)
	if (errstat .ne. 0) then
	   write(context,'(a,i4)')
     &		'Problem reading CDES0001 keyword in ext: ',
     &		extnum
	   call wtwarm(subname,version,chatter,1,context)
	   call wtinfo(chatter,1,2,'skipping extension')
c            goto 1000
	endif

C	Get the non-required keywords
	call ftgkys(iunit,'FILTER',filtval,comment,errstat)
	if (errstat .ne. 0) filtval = 'NONE'
	errstat = 0

	call ftgkys(iunit,'DETNAM',detval,comment,errstat)
	if (errstat .ne. 0) detval = 'NONE'
	errstat = 0

C	Get all the CBDnxxxx keywords
	call ftgkys(iunit,'CBD10001',cbdval(1),comment,errstat)
	if (errstat .ne. 0) cbdval(1)='NONE'
	call ftgkys(iunit,'CBD20001',cbdval(2),comment,errstat)
	if (errstat .ne. 0) cbdval(2)='NONE'
	call ftgkys(iunit,'CBD30001',cbdval(3),comment,errstat)
	if (errstat .ne. 0) cbdval(3)='NONE'
	call ftgkys(iunit,'CBD40001',cbdval(4),comment,errstat)
	if (errstat .ne. 0) cbdval(4)='NONE'
	call ftgkys(iunit,'CBD50001',cbdval(5),comment,errstat)
	if (errstat .ne. 0) cbdval(5)='NONE'
	call ftgkys(iunit,'CBD60001',cbdval(6),comment,errstat)
	if (errstat .ne. 0) cbdval(6)='NONE'
	call ftgkys(iunit,'CBD70001',cbdval(7),comment,errstat)
	if (errstat .ne. 0) cbdval(7)='NONE'
	call ftgkys(iunit,'CBD80001',cbdval(8),comment,errstat)
	if (errstat .ne. 0) cbdval(8)='NONE'
	call ftgkys(iunit,'CBD90001',cbdval(9),comment,errstat)
	if (errstat .ne. 0) cbdval(9)='NONE'

	errstat = 0
C L.B. remove any `blanks' in CBD keyword values so as not to cock-uo
C ckcbd routine.
	do i=1,9
             call crmvblk(cbdval(i))
	end do

C	Check the CBD values to see if the format is readable
	call ckcbd(cbdval,result)
	if (.not. result) then
	     context='Cannot read the CBD values for this dataset'
	     call wterrm(subname, version, context)
c	     goto 1000
	endif

C L.B. (Nov 01 1996) ...comment out any previous UPDCIF code for fileval;dirval
C                       qualval.

C	Assign input file name to variable written to CIF
c	fileval = infile

C	Get dir part of system independent specification for the
C	current working directory
c	call cgdr(envar,dirval,errstat)
c	if (errstat .ne. 0) then
c	     context='Cannot get directory from CGDR'
c	     call wterrm(subname, version, context)
c	     ierr = 1
c	     goto 999
c	endif

C	Use the CAL_DEV column to store the online/offline info
C	Since this dataset must be online for me to access it
C	write 'ONLINE' to the CAL_DEV column.
	devval = 'ONLINE'

C	Get the system date

c	call gtdats(dd,mm,yy,date)
c new FITS standard date
        call ftgstm(timestr,utmode,errstat)
        date=timestr(1:10)


C	Get the quality value for this dataset
c	call uclgsi('quality',qualval,errstat)
c	if (errstat .ne. 0) then
c	     context = 'Cannot get quality parameter'
c	     call wterrm(subname,version,context)
c	     goto 1000
c	endif

C .......... Get the column numbers from the index file
	     exact = .true.
	     call ftgcno(ounit,exact,'TELESCOP',telecol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column TELESCOP'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'INSTRUME',instcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column INSTRUME'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'DETNAM',detcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column DETNAM'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'FILTER',filtcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column FILTER'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_DEV',devcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_DEV'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_DIR',dircol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_DIR'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_FILE',filecol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_FILE'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_CLAS',cclscol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_CLAS'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_DTYP',cdtpcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_DTYP'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_CNAM',ccnmcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_CNAM'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_CBD',cbdcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_CBD'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_XNO',xnocol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_XNO'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_VSD',cvsdcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_VSD'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_VST',cvstcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_VST'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'REF_TIME',refcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column REF_TIME'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_QUAL',qualcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_QUAL'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_DATE',datecol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_DATE'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_DESC',cdescol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_DESC'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

	     call ftgcno(ounit,exact,'CAL_ORIG',origcol,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot find index file column CAL_ORIG'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif

C .......... Get the number of rows in the cif
	     call ftgkyj(ounit,'NAXIS2',oldrows,comment,errstat)
	     if (errstat .ne. 0) then
	          context='Cannot get NAXIS2 from back-up CIF'
		  call wterrm(subname,version,context)
		  ierr = 1
	          goto 999
	     endif
c	endif

C L.B. (Nov 01 1996) ...udmode options written

C UDMODE = ADD
	if ((udmode.eq.'ADD').OR.(udmode.eq.'add')) then
C Make a new entry in the "back-up" index file containing the keyword
C values gotten above - one entry for each alias (if any) of the
C instval.

          match=0
          do i=1,ntrans
C make 1 entry for a cal file with INSTRUME value = a particular alias
             if (trans(i).eq.instval(:fcstln(instval))) then
c             instval = trans(i)
              newrows = newrows + 1
              row = newrows + oldrows

              call wtcif2(chatter,ounit,row,udmode,telecol,televal,
     &           instcol,
     &          instval,detcol,detval,filtcol,filtval,devcol,devval,
     &          dircol,dirval,filecol,fileval, cclscol,cclsval,cdtpcol,
     &          cdtpval,ccnmcol,ccnmval,cbdcol,cbdval,xnocol,extval,
     &          cvsdcol,cvsdval,cvstcol,cvstval,refcol,reftime,qualcol,
     &          qualval,datecol,date,cdescol,cdesval,origcol,errstat)
              if(errstat .ne. 0) then
c                context='punting for '//instval
c                call wterrm(subname,version,context)
                errstat = 0
                newrows = newrows -1
                row = row - 1
              else
                modcif = .true.
                match=1
              endif
             endif
           enddo
C make one entry for each alias if cal file INSTRUME value is the instrument

          do i=1,ntrans
             if (match .eq. 0) then
              instval = trans(i)
              newrows = newrows + 1
              row = newrows + oldrows
              call wtcif2(chatter,ounit,row,udmode,telecol,televal,
     &           instcol,
     &          instval,detcol,detval,filtcol,filtval,devcol,devval,
     &          dircol,dirval,filecol,fileval, cclscol,cclsval,cdtpcol,
     &          cdtpval,ccnmcol,ccnmval,cbdcol,cbdval,xnocol,extval,
     &          cvsdcol,cvsdval,cvstcol,cvstval,refcol,reftime,qualcol,
     &          qualval,datecol,date,cdescol,cdesval,origcol,errstat)
              if(errstat .ne. 0) then
c                context='punting for '//instval
c                call wterrm(subname,version,context)
                errstat = 0
                newrows = newrows -1
                row = row - 1
              else
                modcif = .true.
              endif
             endif
           enddo

C UDMODE = OVERWRITE
C overwrite the row in the 'back-up' CIF
	elseif ((udmode.eq.'OVER').OR.(udmode.eq.'over')) then
              row=cifrow
              match=0
              do i=1,ntrans

                 if (trans(i).eq.cifinstval(:fcstln(cifinstval))) then
 	            instval = trans(i)
                    match=1
c	            type *, 'OK OVERWRITING NOW *...entering wtcif2**'
                    call wtcif2(chatter,ounit,row,udmode,telecol,
     &                   televal,instcol,instval,detcol,
     &		         detval,filtcol,filtval,devcol,devval,
     &		         dircol,dirval,filecol,fileval, cclscol,
     &                   cclsval,cdtpcol,cdtpval,ccnmcol,ccnmval,
     &                   cbdcol,cbdval,xnocol,extval,
     &		         cvsdcol,cvsdval,cvstcol,cvstval,refcol,
     &                   reftime,qualcol, qualval,datecol,date,cdescol,
     &                   cdesval,origcol,errstat)
	            if(errstat .ne. 0) then
	               errstat = 0
 	            else
	               modcif = .true.
	            endif
                 endif
               enddo
               if (match .eq. 0) then
                  context='no match found in CIF for '//instval
                  call wterrm(subname,version,context)
                  ierr=1
                  goto 999
               endif
C UDMODE = DELETE
c ........ Delete
	else
	   call wterrm(subname,version,'Unsupported UDMODE')
	   ierr = 1
	   goto 999
	endif

C Search for the next dataset
C L.B. (Nov 01 1996) ...comment out loop (via 1000 continuation statement)
c	goto 1000

C ------ Bottom of extension searching loop -------
9000	continue

C close the calibration file
	errstat = 0
	call ftclos(iunit, errstat)
	if ( errstat .ne. 0 ) then
	     context = 'Cannot close calibration file: '
     &	        //infile(:fillen)
	     call wtferr(subname,version,errstat,context)
	else
             call cfrelun(iunit)
	     iopen = .false.
	endif


999	if(ierr.ne.0) then
	  context = 'Unable to continue '//
     &		' -- attempting to shut down & clean up'
	  call wtinfo(chatter,1,1,context)
	  errstat = 0
	endif




C	close the calibration file
	if (iopen) then
	     call ftclos(iunit, errstat)
	     if (errstat .ne. 0) then
	          context='Cannot close calibration file: '//
     &	             infile(:fillen)
	          call wtferr(subname,version, errstat,context)
	          errstat = 0

	     endif
             call cfrelun(iunit)
             iopen=.false.
	endif



        return
        end

C-------End of subroutine UPDCIF----------------------------------------

*+WTCIF2
	subroutine wtcif2(chatter,ounit,row,udmode,telecol,televal,
     &  instcol,instval,detcol,detval,
     &  filtcol,filtval,devcol,devval,dircol,dirval,filecol,fileval,
     &  cclscol,cclsval,cdtpcol,cdtpval,ccnmcol,ccnmval,cbdcol,cbdval,
     &  xnocol,extval,cvsdcol,cvsdval,cvstcol,cvstval,refcol,reftime,
     &  qualcol,qualval,datecol,date,cdescol,cdesval,origcol,status)

	implicit none
     	integer ounit,row,status
	integer extval,qualval, chatter
	double precision reftime
     	character*(*)televal,instval,detval,filtval,devval,dirval,
     &  fileval,cclsval,cdtpval,ccnmval,cbdval(9),cvsdval,cvstval,date,
     &  cdesval,udmode
	integer telecol,instcol,detcol,filtcol,devcol,dircol,filecol,
     &  cclscol,cdtpcol,ccnmcol,cbdcol,xnocol,cvsdcol,cvstcol,refcol,
     &  qualcol,datecol,cdescol,origcol
        logical check

C Description:
C  Writes Calibration Index File row to the file opened unit ounit to the
C row given by 'row'.
C
C Arguments:   ounit (i) : the logical unit number for the cif
C              row   (i) : the row number where data will be written
C              errstat (r) : the success status of this routine
C		... fill in rest, Ron ....
C
C Origin:      written for the caldb
C
C Authors/Modification History
C   Ron Zellar 		(1.0.0: 93 Oct 01) Original version
C   Ron Zellar 		(2.0.0: 94 May 24) Modified ckcif call to include
C                                          cbd values
C   Ron Zellar 		(2.1.0: 94 May 25) Added error checking
C   Ian M George        (3.0.0: 96 Feb 12) added chatter + cosmetics
C   Lorraine Breedon    (1.0.0: 96 Nov 01) extra parameter 'udmode' parsed
C                                          + 'check' option added.
C                                          ** Renamed from WTCIFR to WTCIF2 **
C   Lorraine Breedon    (1.1.0: 97 Jun 11) now accounts for extra CIF column
C                                          CAL_ORIG
	character(7) version
	parameter(version = '1.1.0')
*-
c Internals
	character(6) subname
	parameter (subname = 'wtcif2')
	integer nrows,errstat, width, size
	character(80) context, comment
        character(20) origval

C Initialize
	errstat = 0
	status = 0
	check=.false.

c Give user info if requested

         context = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,context)

	write(context, '(a,i12)') 'attempting to write CIF row ', row
	call wtinfo(chatter,20,3,context)

C If quality = 0, check the other entries to make sure that
C there are no conflicting datasets
	if ((udmode .eq. 'ADD') .or. (udmode .eq. 'add')) then
          if (qualval .eq. 0) then
             if (check .eqv. .true.) then
	        nrows = row -1
	        call ckcif2(chatter, ounit,nrows,telecol,televal,instcol,
     &		    instval,refcol,reftime,ccnmcol,
     &           ccnmval,qualcol,qualval,filtcol,
     &            filtval,detcol,detval,cbdcol,cbdval,filecol,cdescol,
     &		  errstat)
	        if (errstat .ne. 0) then
	           status = 1
	           context='Current dataset not written to CIF'
	           call wterrm(subname, version, context)
	           goto 888
	       endif
             endif
          endif
	endif

C	make necessary fitsio calls to add the data
	errstat = 0
        origval='HEASARC'
	call ftpcls(ounit,telecol,row,1,1,televal,errstat)
	call ftpcls(ounit,instcol,row,1,1,instval,errstat)
	call ftpcls(ounit,detcol,row,1,1,detval,errstat)
	call ftpcls(ounit,filtcol,row,1,1,filtval,errstat)
	call ftpcls(ounit,devcol,row,1,1,devval,errstat)
	call ftpcls(ounit,dircol,row,1,1,dirval,errstat)
	call ftpcls(ounit,filecol,row,1,1,fileval,errstat)
	call ftpcls(ounit,cclscol,row,1,1,cclsval,errstat)
	call ftpcls(ounit,cdtpcol,row,1,1,cdtpval,errstat)
	call ftpcls(ounit,ccnmcol,row,1,1,ccnmval,errstat)
	call ftpcls(ounit,cbdcol,row,1,9,cbdval,errstat)
	call ftpclj(ounit,xnocol,row,1,1,extval,errstat)
	call ftpcls(ounit,cvsdcol,row,1,1,cvsdval,errstat)
	call ftpcls(ounit,cvstcol,row,1,1,cvstval,errstat)
	call ftpcld(ounit,refcol,row,1,1,reftime,errstat)
	call ftpclj(ounit,qualcol,row,1,1,qualval,errstat)
	call ftpcls(ounit,datecol,row,1,1,date,errstat)
	call ftpcls(ounit,cdescol,row,1,1,cdesval,errstat)
	call ftpcls(ounit,origcol,row,1,1,origval,errstat)

	if (errstat .ne. 0) then
	     call wtferr(subname,version,errstat,
     &		'Error writing CIF entry')
	     status = 1
	     goto 888
	else
	     if ((udmode .eq. 'ADD') .or. (udmode .eq. 'add')) then
		call ftmkyj(ounit,'NAXIS2',row,'&',errstat)
		call ftgkyj(ounit,'NAXIS1',width,comment,errstat)
		size = width * row
		call ftddef(ounit,size,errstat)
             endif
	endif



888	if(status.NE.0) then
	   call wterrm(subname,version,'Aborting')
	else
	 context = subname//' '//version//' successful'
         call wtinfo(chatter,20,3,context)
	endif

	return
	end

c -----------------------------------------------------
*+CKCIF2
	subroutine ckcif2(chatter, lun,nrows,telecol,televal,instcol,
     &  instval,
     &  refcol,reftime,ccnmcol,ccnmval,qualcol,qualval,filtcol,filtval,
     &	detcol,detval,cbdcol,cbdval,filecol,descol,status)

	implicit none
	integer lun,nrows,telecol,instcol,refcol,ccnmcol,qualcol,qualval
	integer filtcol,detcol,filecol,descol,cbdcol,status, chatter
	character*(*) televal,instval,filtval
	character*(*) ccnmval,detval
	character*(*) cbdval(9)
	double precision reftime

C-----------------------------------------------------------------------
C Description: Examines the first nrows of a CIF for rows which have the
C              quality value qualval and which contain the values
C              televal, instval, ccnmval, reftime, filtval, detval and
C              the values cbdval.  If a row which
C              meets these criteria is found then the user is asked to
C              input a different quality value for the row which is
C              found or for the row being added.  If he or she chooses
C              to change the quality value of the row being added, this
C              new quality value is returned to qualval provided it is
C              not the same as the original value.  If he or she
C              chooses to change the quality value in the row which was
C              found, this subroutine will overwrite that row's quality
C              value provided the new value is different from the old
C              value.  If the quality value being edited is not
C              different from it's original value, the user will be
C              reprompted.
C
C Arguments:   lun       (i) : The logical unit number for the CIF
C              nrows     (i) : The number of rows in the CIF excluding
C                              the row about to be added
C              telecol   (i) : the col number of the TELESCOP col
C              televal   (i) : the TELESCOP col value
C              instcol   (i) : the col number of the INSTRUME col
C              instval   (i) : the INSTRUME col value
C              refcol    (i) : the col number of the REF_TIME col
C              reftime   (i) : the REF_TIME col value
C              ccnmcol   (i) : the col number of the CAL_CNAM col
C              ccnmval   (i) : the CAL_CNAM col value
C              qualcol   (i) : the col number of the CAL_QUAL col
C              qualval   (r) : the quality value
C              filtcol   (i) : the col number of the FILTER col
C              filtval   (i) : the FILTER col value
C              detcol    (i) : the col number of the DETNAM col
C              detval    (i) : the DETNAM col value
C              cbdcol    (i) : the col number of the CAL_CBD col
C              cbdval    (i) : the CAL_CBD col values
C              filecol   (i) : the col number of the CAL_FILE col
C              descol    (i) : the col number of the CAL_DESC col
C
C Origin:      Written for the Caldb
C
C Authors/Modification History:
C  Ron Zellar 	(1.0.0: 93 Oct 02) Original Version
C  Ron Zellar 	(1.1.0: 94 May 23) Added FILTER and DETNAM search
C  Ron Zellar 	(1.2.0: 94 May 24) Added CBD search, cmpcbd call
C  Ron Zellar 	(1.3.0: 94 May 25) Added error checking and improved comments
C  Ron Zellar 	(2.0.0: 94 Jun 13) Modified fcecho text and if blocks so that
C				single good quality entry is no longer required
C  Ian M George (3.0.0: 96 Feb 12) Added chatter, plus cosmetics
C  Ian M George (3.0.1: 96 Sep 12) made following parameters character*(*) -
C				   televal,instval,filtval, ccnmval,detval
C  Lorraine Breedon (1.0.0: 96 Nov 01) commented out XPI calls when there is
C                                      a 'conflicting' dataset.
C                                      ** renamed from CKCIF to CKCIF2 **
C

	character(7) version
	parameter (version='3.0.1')
C-----------------------------------------------------------------------
*-
c Internals
	character(5) subname
	parameter (subname='ckcif2')
	integer qval,errstat,i
	character(10) tval,ival,fval
	character(20) cval,dval
	character(40) file
	character(80) desc,bval(9),context
	character(256) text
	double precision rval
	logical anyf,result
        integer pmatch(1)
        logical valtest(1), unitstest(1)

c Initialize:
        pmatch(1) = 0
        valtest(1) = .false.
        unitstest(1) = .false.

c Give user info if requested
         context = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,context)


C ------------------------ Start Main Loop ------------------------
	Do 5000 i=1,nrows
C .....	Get quality value from CIF and compare to qual value of dataset
	call ftgcvj(lun,qualcol,i,1,1,0,qval,anyf,errstat)
	if (errstat .ne. 0) then
	     write(context,'(a,I4)')
     &		'Unable to get QUALITY info from CIF row ', i
	     call wtferr(subname,version,errstat,context)
	     status = 1
	     goto 777
	endif
	if (qval .eq. qualval) then

C ....... Get codename value from CIF and compare to codename value of dataset
	  call ftgcvs(lun,ccnmcol,i,1,1,' ',cval,anyf,errstat)
	  if (errstat .ne. 0) then
	     write(context,'(a,I4)')
     &		'Unable to get CODENAME info from CIF row ', i
	     call wtferr(subname,version,errstat,context)
	     status = 1
	     goto 777
	  endif
	  if (cval .eq. ccnmval) then

C ......... Get instrument value from CIF and compare to that in dataset
	    call ftgcvs(lun,instcol,i,1,1,' ',ival,anyf,errstat)
	    if (errstat .ne. 0) then
	     write(context,'(a,I4)')
     &		'Unable to get INSTRUME info from CIF row ', i
	     call wtferr(subname,version,errstat,context)
	     status = 1
	     goto 777
	    endif
	    if (ival .eq. instval) then

C ........... Get telescope value from CIF and compare to that in dataset
	      call ftgcvs(lun,telecol,i,1,1,' ',tval,anyf,errstat)
	      if (errstat .ne. 0) then
	     	write(context,'(a,I4)')
     &		  'Unable to get TELESCOP info from CIF row ', i
	        call wtferr(subname,version,errstat,context)
	        status = 1
	        goto 777
	      endif
	      if (tval .eq. televal) then

C ............. Get reftime value from CIF and compare to that in dataset
	        call ftgcvd(lun,refcol,i,1,1,0.0d0,rval,anyf,errstat)
	        if (errstat .ne. 0) then
	     	  write(context,'(a,I4)')
     &		    'Unable to get REFTIME info from CIF row ', i
	          call wtferr(subname,version,errstat,context)
	          status = 1
	          goto 777
	        endif
	        if (rval .eq. reftime) then

C ............... Get filter value from CIF and compare to that in dataset
	          call ftgcvs(lun,filtcol,i,1,1,' ',fval,anyf,errstat)
	          if (errstat .ne. 0) then
	     	    write(context,'(a,I4)')
     &		      'Unable to get FILTER info from CIF row ', i
	            call wtferr(subname,version,errstat,context)
	            status = 1
	            goto 777
	          endif
	          if (fval .eq. filtval) then

C ................. Get detnam value from CIF and compare to that in dataset
	            call ftgcvs(lun,detcol,i,1,1,' ',dval,anyf,errstat)
	            if (errstat .ne. 0) then
	     	      write(context,'(a,I4)')
     &		       'Unable to get DETNAM info from CIF row ', i
	              call wtferr(subname,version,errstat,context)
	              status = 1
	              goto 777
	            endif
	            if (dval .eq. detval) then

C ................... Get boundary values from CIF and compare
	              call ftgcvs(lun,cbdcol,i,1,9,' ',bval,anyf,errstat)
	              if (errstat .ne. 0) then
	     	  	write(context,'(a,I4)')
     &		         'Unable to get CBD info from CIF row ', i
	                call wtferr(subname,version,errstat,context)
	                status = 1
	                goto 777
	              endif
	              call cbdcma(bval,9,cbdval,9,chatter,result,
     &                            pmatch,valtest,unitstest,errstat)
	              if (errstat .ne. 0) then
	     	  	write(context,'(a,I4)')
     &		    'Unable to compare boundary info from CIF row', i
	          	call wterrm(subname,version,context)
	          	status = 1
	          	goto 777
	              endif
	              if (result) then

C ..................... If all the comparisons above are true then there is a
C                       conflict which the user needs to know about
			call ftgcvs(lun,filecol,i,1,1,' ',file,anyf,
     &                       errstat)
			call ftgcvs(lun,descol,i,1,1,' ',desc,anyf,
     &                       errstat)
			text = 'Another dataset has been found which '//
     &			'is valid for the same conditions as the dataset '//
     &			'being indexed. This conflicting dataset has:'
			call wtinfo(10,1,1,text)
			text='Instrument : '//ival
		 	call wtinfo(10,1,2,text)
			text='Code Name  : '//ccnmval
		 	call wtinfo(10,1,2,text)
			text='File       : '//file
		 	call wtinfo(10,1,2,text)
			text='Description: '//desc
			call wtinfo(10,1,2,text)

C L.B. (Nov 01 1996) ..XPI calls commented out and status set to 1
c			call uclgsb('editc',ans,errstat)
c			if (ans) then
c 			   call uclgsi('newquality',newqual,errstat)
c			   call ftpclj(lun,qualcol,i,1,1,newqual,
c     &                          errstat)
c                       else
c			   call uclgsb('editi',ans,errstat)
c			   if (ans) then
c			      call uclgsi('quality',qval,errstat)
c			      qualval = qval
c			   endif
c                       endif
                        status=1
                        goto 777
	              endif
	            endif
		  endif
                endif
              endif
            endif
	  endif
	endif
5000 	continue


777	if(status.NE.0) then
	   call wterrm(subname,version,'Aborting')
	else
         context = subname//' '//version//' successful'
         call wtinfo(chatter,25,3,context)
	endif

	return
	end

c -------------------------------------------------------------------
*+RDCAF1
	subroutine rdcaf1(chatter, iunit, telescop,
     &		instval,ntrans,trans,ierr)

	implicit none
	character*(*)instval, telescop
	character(10) trans(10)
	integer ntrans, iunit, ierr, chatter

C Description:
C Checks the alias config file in the current HDU for an alias value matching
C the instval.  If one is found, trans is returned with the translations for
C instval and ntrans is the number of translations for instval.
C If instval does not have any translations, ntrans is returned as 0.
C
C Arguments:
C  chatter (i) : chattiness flag (zero for silent running)
C  iunit   (i) : the logical unit number for the previously
C                            opened configuration file
C  telescop(i) : name of the mission
C  instval (i) : name of the instrument for which translations are searched for
C  ntrans  (r) : the number of translations for instval
C  trans   (r) : the translations for instval
C  ierr    (r) : error flag (zero = OK)
C
C Origin:      Written for the Caldb
C
C Authors/Modifiction History:
C   Ron Zellar     (1.0.0: 93 Oct 01) Original Version
C   Ian M George   (2.0.0: 96 Feb 06) Renamed from ckinst & extracted from
C				udcif code, also new params+functionality
C			        error checking etc added
	character(7) version
	parameter (version = '2.0.0')
C-----------------------------------------------------------------------
*-
c Internals
	character(6) subname
	parameter (subname = 'rdcaf1')
	character(10) alias
	character(80) comment, contxt, value
	logical anyf
	integer errstat,i,nalias,fcstln, hdutype
c Initialize
	ntrans = 0
	ierr = 0

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,contxt)

c Read the current EXTNAME to see where we are in the file
c if we get an error, then assume we're at the top of the file
100     call ftgkys(iunit,'EXTNAME',value,comment,errstat)
	if(errstat.ne.0) then
	    call wtinfo(chatter,30,3,
     &		'No EXTNAME in current extn of CAF - searching')
            errstat = 0
        elseif ((errstat.eq.0).and.
     &  (value(:fcstln(value)).eq.telescop(:fcstln(telescop))))then
             goto 200
	endif

c ... not in the right place, so start scrolling down the file
        call ftmrhd(iunit,1,hdutype,errstat)
	if(errstat.eq.107) then
	     call wtinfo(chatter,20,2,
     &	  	'EOF encountered in CAF before desired EXTNAME')
	     goto 999
        elseif (errstat .ne. 0) then
	     call wtferr(subname,version,errstat,'Unable to move')
             ierr = -1
             goto 999
	else
	     goto 100
        endif

200	continue
C ... we've found the right extension
        call wtinfo(chatter,30,3,'Located correct extn of CAF')
	call ftgkyj(iunit,'NAXIS2',nalias,comment,errstat)
        if (errstat .ne. 0) then
             call wtferr(subname,version,errstat,
     &		'Problem reading NAXIS2 keyword in CAF')
             ierr = -1
             goto 999
        endif

	Do 1000 i=1,nalias
	   call ftgcvs(iunit,1,i,1,1,' ',alias,anyf,errstat)
	   if(alias(:fcstln(alias)).eq.instval(:fcstln(instval)))then
	      call ftgcvj(iunit,2,i,1,1,0,ntrans,anyf,errstat)
	      call ftgcvs(iunit,3,i,1,ntrans,' ',trans,anyf,errstat)
	   endif
	   if(errstat.ne.0) then
             call wtferr(subname,version,errstat,
     &		'Problem reading CAF')
             ierr = -1
             goto 999
           endif

1000	continue

999     if(ierr.ne.0) then
          call wterrm(subname, version, 'Incomplete Execution')
        endif

	return
	end

C-----------------------end of RDCAF1subroutine----------------------
C ------------------------------------------------------------------------------

*+OPCIFS
        subroutine opcifs(calfexp,copen,oopen,aopen,cunit,
     &            ounit,aunit,ciftmp,televal,instval,trans,
     &            ntrans,chatter,ierr)

        implicit none
        character*(*) calfexp,trans(10),televal,instval,ciftmp
        logical copen,aopen,oopen
       	integer ierr,chatter,ounit,cunit,aunit,ntrans
C Description:
C  Opens a CIF (appropriate for a given mission and instrument).
C  Creates a new CIF called cif.tmp and copies all data from old CIF (above)
C  to new.
C  Opens the alias_config.fits file for the mission/instrument


C passed parameters:
C  STATUS              :    error flg (0=OK)
C  CHATTER             :    chattiness flag for o/p (5 low,10 normal,15 high)
C  COPEN               :    logical flag indicating wether 'old' CIF open
C  OOPEN               :    logical flag indicating wether 'new' CIF open
C  AOPEN               :    logical flag indicating wether alias_config.fits
C                           file open
C  CUNIT               :    logical unit no for 'old' CIF
C  OUNIT               :    logical unit no for 'new' CIF
C  AUNIT               :    logical unit no for alias_config.fits file
C  CALFEXP             :    name (+path) of `old' CIF
C  CIFTMP              :    name (+path) of 'new' CIF
C  TELEVAL             :    name of mission
C  INSTVAL             :    name of instrument
C  TRANS()             :    array containing the 'alias' values for instrument
C  NTRANS              :    number of 'alias' values
C
C user i/ps (prompted for):
C none
C
C Called routines :
C  subroutine FCECHO       : (FITSIO) write to STDOUT
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine CFRELUN      : (CALLIB) free FORTRAN logical unit no
C  subroutine WT_FERRMSG   : (CALLIB) dumps error messages if necessary
C  subroutine WTINFO       : (CALLIB) dumps error messages if necessary
C  subroutine FTOPEN       : (FITSIO) opens FITS file
C  subroutine FTCLOS       : (FITSIO) closes FITS file
C  subroutine FTMAHD       : (FITSIO) moves to 1st extension in FITS file
C  subroutine RDCNFG       : (CALLIB) reads the caldb.config file for
C                                     given mission/instrument

C compilation & linking :
C  link with XPI, FITSIO & CALLIB
C
C Origin: Written for the Calibration Database.
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:97 Jan 15) original version
C  Lorraine Breedon (1.1.0:98 Apr 23) removed crtcif routine due to
C                                     problems with ftcopy not
C                                     clobbering 1st extn data in
C                                     cif.tmp
	character(7) version
        parameter (version = '1.1.0')


*-
C Internals
  	character(6) subname
	parameter (subname = 'opcifs')
        character(10) aliasvar
	character(160) alsfil,context,cif
        integer fcstln,errstat,hdutype,aplen
	integer blcksz,j,i
        logical simple,extend
        integer bitpix,naxis,naxes(2),pcount,gcount

C initialise and set up defaults
        ierr = 0
	aliasvar = 'CALDBALIAS'
        cif=calfexp
        ounit=0
        cunit=0
        aunit=0

   	call cgetlun(cunit)
        call cgetlun(ounit)
        call cgetlun(aunit)



c	type *, '*** IN OPCIFS ***'
c        type *, 'cunit,ounit,aunit=',cunit,ounit,aunit

c Give user info if requested
         context = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,context)

c Translate the ALIASVAR environment/logicals


	call ctrlog(aliasvar,fcstln(aliasvar),alsfil,aplen)

C Open the configuration file containing the telescop alias values
	call ftopen(aunit,alsfil,0,blcksz,errstat)
	if (errstat .ne. 0) then
		     context = 'Cannot open the CAF'
	     call wtferr(subname,version,errstat,context)
	     context = 'Offending file: '//
     &          alsfil(:fcstln(alsfil))
	     call wtinfo(chatter,1,2,context)
	     ierr = 1
	     goto 999
	endif
	aopen = .true.

c See whether we have the CIF open. If not, then open it, and make
c a "back-up" copy (which is actually the guys we'll be working in)
c ------------------------------------------------------------
	if (.not. oopen) then

C ........ Open the old calibration index file and move to the first extension.
	   call ftopen(cunit,cif(:fcstln(cif)),1,blcksz,errstat)
	   if (errstat .eq. 0) then
		copen = .true.
	   else
		context='Cannot open the CIF'
		call wtferr(subname,version,errstat,context)
     		context = 'offending file: '//cif
		call wtinfo(chatter,1,1,context)
	        ierr = 1
	        goto 999
	   endif
	   call ftmahd(cunit,2,hdutype,errstat)
	   If (errstat .ne. 0) then
	        context='Cannot move within the CIF'
	        call wtferr(subname,version,errstat,context)
     		context = 'offending file: '//cif
		call wtinfo(chatter,1,1,context)
	        ierr = 1
	        goto 999
	   endif



C .........For safety, create a new cif called 'cif.tmp' which will contain
C          all the entries -- old and new.
	   call wtinfo(chatter,10,1,'Making back-up copy of CIF')
	   j = index(cif,'caldb.indx')
	   ciftmp = cif(:j-1)//'cif.tmp   '

             simple = .true.
             blcksz = 0
             bitpix = 8
             naxis = 0
             naxes(1) = 0
             naxes(2) = 0
             pcount = 0
             gcount = 0
             extend = .true.

             call ftinit(ounit,ciftmp,blcksz,errstat)
             if (errstat .eq. 0) oopen = .true.
             if (errstat .eq. 105) then
                  context='Please remove the file cif.tmp'
                  goto 999
             endif

             call ftphpr(ounit,simple,bitpix,naxis,naxes,
     &          pcount,gcount,extend,errstat)
             call ftpdef(ounit,bitpix,naxis,naxes,pcount,
     &          gcount,errstat)







C ......... Copy all the data from the old cif to the new cif
	     call ftcrhd(ounit,errstat)
	     if (errstat .ne. 0) then
		context='Cannot create new FITS header'//
     &			' in back-up copy'
		call wtferr(subname,version,errstat,context)
     		context = 'offending file: '//ciftmp
		call wtinfo(chatter,1,1,context)
	        ierr = 1
	        goto 999
	     endif
	     call ftcopy(cunit,ounit,0,errstat)
	     if (errstat .ne. 0) then
		errstat = 0
		if (oopen) then
		  call ftclos(ounit,errstat)
		  oopen = .false.
                  call cfrelun(ounit)
	          call delfil(ciftmp)

	        endif
	        context='Cannot copy data from CIF to back-up copy'
		call wterrm(subname,version,context)
		ierr = 1
	        goto 999
	     endif
	     call ftclos(cunit,errstat)
	     if (errstat .ne. 0) then
		context='Cannot close index file: caldb.indx'
		call wtferr(subname,version, errstat,context)
		errstat = 0
		copen = .true.
	     else
                call cfrelun(cunit)
		copen = .false.
	     endif
	endif
c ------------------------------------------------------------

C........ read the alias file
	  call rdcaf1(chatter,aunit,televal,instval,ntrans,trans,ierr)
	  if(ierr.ne.0) then
		context='Problem reading CAF'
		call wterrm(subname,version,context)
	  	goto 999
	  endif

c ....... no alias found, so put in the defaults
          if (ntrans.eq. 0) then
	   ntrans = 1
	   trans(1) = instval
	   context='No alias found for '//televal(:fcstln(televal))//
     &		' '// instval(:fcstln(instval))
           call wtinfo(chatter,20,2,context)
	  else
	   context='Following aliases found for '//
     &		televal(:fcstln(televal))//
     &		' '// instval(:fcstln(instval))
           call wtinfo(chatter,20,2,context)
	   do i = 1, ntrans
		context=televal(:fcstln(televal))//
     &		' '//trans(i)(:fcstln(trans(i)))
		call wtinfo(chatter,20,3,context)

	   enddo
	  endif

999	if(ierr.ne.0) then
	  context = 'Unable to continue '//
     &		' -- attempting to shut down & clean up'
	  call wtinfo(chatter,1,1,context)


C	Close the instrument alias configuration file
	  if (aopen) then
	     call ftclos(aunit,errstat)
	     if (errstat .ne. 0) then
		  context='Cannot close config file'
		  call wtferr(subname,version,errstat,context)
		  errstat = 0

	     endif
             call cfrelun(aunit)
	  endif

C	close the new calibration index file
	  if (oopen) then
	     call ftclos(ounit, errstat)
	     if (errstat .ne. 0) then
	          context='Cannot close index file: cif.tmp'
		  call wtferr(subname,version, errstat,context)
	          errstat = 0
	     endif
             call cfrelun(ounit)
	  endif

C	Close the old calibration index file
	  if (copen) then
	     call ftclos(cunit,errstat)
	     if (errstat .ne. 0) then
		  context='Cannot close index file: caldb.indx'
		  call wtferr(subname,version, errstat,context)
		  errstat = 0
	     endif
             call cfrelun(cunit)
	  endif
	endif
C        type *, 'cunit,ounit,aunit=',cunit,ounit,aunit

        return
        end

C-------End of subroutine OPCIFS----------------------------------------

*+CLCIFS
        subroutine clcifs(calfexp,copen,oopen,aopen,cunit,ounit,aunit,
     &                    ciftmp,chatter,status)

        implicit none
        character *(*) calfexp,ciftmp
        logical copen,aopen,oopen
       	integer status,chatter,cunit,ounit,aunit
C Description:
C  closes 2 CIFS ('old' original and 'new' modified copy).
C  Moves 'new' CIF onto 'old' one.
C  closes the alias_config.fits file for the mission/instrument

C  ** note ***
C  it is assumed that the subroutine OPCIFS (Open CIFS) has been
C  called prior to this routine


C passed parameters:
C  STATUS              :    error flg (0=OK)
C  CHATTER             :    chattiness flag for o/p (5 low,10 normal,15 high)
C  COPEN               :    logical flag indicating wether 'old' CIF open
C  OOPEN               :    logical flag indicating wether 'new' CIF open
C  AOPEN               :    logical flag indicating wether alias_config.fits
C                           file open
C  CUNIT               :    logical unit no for 'old' CIF
C  OUNIT               :    logical unit no for 'new' CIF
C  AUNIT               :    logical unit no for alias_config.fits file
C  CALFEXP             :    name (+path) of `old' CIF
C  CIFTMP              :    name (+path) of 'new' CIF


C
C user i/ps (prompted for):
C none
C
C Called routines :
C  subroutine FCECHO       : (FITSIO) write to STDOUT
C  subroutine CGETLUN      : (CALLIB) get free FORTRAN logical unit no
C  subroutine CFRELUN      : (CALLIB) free FORTRAN logical unit no
C  subroutine WT_FERRMSG   : (CALLIB) dumps error messages if necessary

C compilation & linking :
C  link with XPI, FITSIO & CALLIB
C
C Origin: Written for the Calibration Database.
C          Based on code written by Ron Zellar for routine UPDCIF
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:97 Jan 15) original version
	character(7) version
        parameter (version = '1.0.0')


*-
C Internals
  	character(6) subname
	parameter (subname = 'clcifs')
	character(160) alsfil,context,cif
        integer fcstln,errstat

C initialise and set up defaults
        errstat=0
        cif=calfexp


c Give user info if requested
         context = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,context)

C Close the alias_config.fits file
	errstat = 0
	call ftclos(aunit, errstat)
	if ( errstat .ne. 0 ) then
	     context = 'Cannot close alias_config.fits file '
     &	        //alsfil(:fcstln(alsfil))
	     call wtferr(subname,version,errstat,context)
             status=errstat
             return

	else
             call cfrelun(aunit)
	     aopen = .false.
	endif

C close the new calibration index file
	call ftclos(ounit, errstat)
	if (errstat .ne. 0) then
	     context='Cannot close index file: cif.tmp'
	     call wtferr(subname,version,errstat,context)
	     status=errstat
             return
	else
              call cfrelun(ounit)
	     oopen = .false.
	endif

C Move the new cal index file onto the old one
 	call mvfile(ciftmp,cif)

C Close the old calibration index file
	if (copen) then
	     call ftclos(cunit,errstat)
	     if (errstat .ne. 0) then
		  context='Cannot close index file: caldb.indx'
		  call wtferr(subname,version, errstat,context)
		  status=errstat
                  return
	     endif
             call cfrelun(cunit)
	endif


        return
        end

C-------End of subroutine CLCIFS----------------------------------------
