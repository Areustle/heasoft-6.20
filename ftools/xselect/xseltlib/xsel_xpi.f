c
c Subroutine for loading the positional parameters of a command  
c 
      Subroutine ldpospar(commnd)

      implicit none
      integer lenact
C       character array for holding the position parameters
      character(30) pospar(100) 
C       The parameter is loaded in the xpi? (T = yes)
      logical par_state(100) 
C       total number of the position parameters
      integer npospar
      common /cmdpar/pospar,par_state,npospar 
        
      character(16) commnd
      integer i
      character(30) name
      character(30) param 
      character(30) clus 
      integer pos, excl
        
c       reset the cmdpar
       
      do i = 1,100
         pospar(i) = " "
         par_state(i) = .FALSE.
      enddo 
      npospar = 0

c       init the yacc
      call yinit()
        
C       dummy command for initialization.
      if(commnd(:lenact(commnd)).eq."XSELECT_INIT")return

c       find the position parameters for this command. look through the
C       the key table (starting from 1)
      i = 1
 10   call tbgtky(i,name,param,pos,clus,excl) 
      call upc(name)
      if(name(:lenact(name)).eq.commnd(:lenact(commnd))
     *     .and.pos.ne.0) then
         pospar(pos)=param(:lenact(param))
         npospar = npospar + 1
      endif 
C       if tbgtky return i < 0, it is already at the bottom of the key
C       table
      if(i .lt. 0)  goto 11    
      goto 10  
c  
c       capture the scom of the yaccfor.inc
c
 11   call ycapcmd(Commnd)
      end

c
c Subroutine for finding a positional parameter for a given position. 
c 
      Subroutine fnpospar(ipos ,parnam)
      implicit none

C       character array for holding the position parameters
      character(30) pospar(100) 
C       The parameter is loaded in the xpi? (T = yes)
      logical par_state(100) 
C       total number of the position parameters
      integer npospar
      common /cmdpar/pospar,par_state,npospar 
      character(30) parnam
      integer ipos


      if(ipos .gt.npospar.or.ipos.lt.0) then 
         parnam = " "
         return
      endif 
      parnam = pospar(ipos)
      return 
      end


C
C Load the ipos'th(given in key table) command argument into the xpi interface 
C
      Subroutine ldcmdpar(ipos, cmdarg)

      implicit none
      integer lenact
      integer ipos
      character(200)  cmdarg   

      character(30) parnam
      character(200) parval
      integer delim

C       character array for holding the position parameters
      character(30) pospar(100) 
C       The parameter is loaded in the xpi? (T = yes)
      logical par_state(100) 
C       total number of the position parameters
      integer npospar
      common /cmdpar/pospar,par_state,npospar 

C
C       if argument have = in it, it is the option. parse it and ignore 
C       its position 
C        
      delim = index(cmdarg,'=')      
      if(delim.ne.0) then  
         parnam = cmdarg(:delim-1)
         parval = cmdarg(delim+1:lenact(cmdarg))
      else
         call fnpospar(ipos ,parnam)
         parval = cmdarg(:lenact(cmdarg)) 
      endif 

      if(parnam(1:1).eq." ") return
C
C    The order of calling is important. The ycappar has to call first 
C    otherwise, the parameter will be double counted.
C
      call ycappar(parnam)
      call ycapval(parval)

      par_state(ipos) = .TRUE. 
      cmdarg = " "
      return
      end


C
C   Find the parameter from par file. 
C   if the Update = 'h', then the parval will be default one and the 
C   Descript will be an empty string. Otherwise, it is the other way. 
C   flag = 0: position parameter
C   flag = 1: hidden parameter
C   flag = -1: not found
C    
      Subroutine fndfilpar(parnam,Desc,Type,Minp,Maxp,parval,flag)  


      implicit none
      integer lenact
      character*(*) parnam
      character*(*) desc
      character*(*) type
      character*(*) minp 
      character*(*) maxp 
      character*(*) parval   
      integer flag

      character(4) mode
      character(30) pnam

      integer id

      id = 1 
      desc = " "
      type = " "        
      minp = " "        
      maxp = " "   
      parval = " "      

      id = 1 
      flag = 0
 20   call tbgtpr(id,pnam,desc,type,minp,maxp,parval,mode)
      if(pnam(:lenact(pnam)).eq.parnam(:lenact(parnam))) then  
         if(mode(1:1).eq."h") then 
            flag = 1 
         else 
            flag = 0
         endif 
         id = id - 1
         return
      endif
      if(id.lt.0) then 
         flag = -1
         return  
      endif 
      goto 20 
      end 

C
C     query and read the xpi parameter buffer.      
C

      integer function read_xpi_buffer(parname)
      implicit none

C     character array for holding the position parameters
      character(30) pospar(100)
C     The parameter is loaded in the xpi? (T = yes)
      logical par_state(100)
C     total number of the position parameters
      integer npospar
      common /cmdpar/pospar,par_state,npospar

      CHARACTER*(*) Parname
      character(80) desc
      character(2) type
      character(60) minp
      character(60) maxp
      character(200) parval
      integer flag
      character(100) str
      integer status
      integer lenact 
      integer get_val
      integer xselqinter
      integer i

C
C     check to see whether this parameter has been in the yaccfor.inc
C
      read_xpi_buffer = 0
      status = 0
      do i = 1, npospar
         if(parname(:lenact(parname)).eq.
     &        pospar(i)(:lenact(pospar(i)))
     &        .and.par_state(i)) then 
            return 
         endif
      enddo

      call fndfilpar(parname,Desc,Type,Minp,Maxp,parval,flag)
C
C     flag = 0: ql
C     flag = 1: hidden
C     flag = -1: not found
C
      if(flag.eq.-1) then
         str = "Error in reading parameter "//parname(:lenact(parname))
         call xwrite(str,5) 
         read_xpi_buffer = flag
         return
      endif

      if(flag.eq.0) then
C          desc=desc(1:lenact(desc))//
C     &          '['//parval(1:lenact(parval))//']'
         status = get_val(desc,type,parval)
C         try to catch the interupt.
         if(status.eq.0) status = xselqinter()
      endif
c
c     fix the string for xpi, add " if it is not there 
c
c      if(type.eq.'s' .and. parval(1:1).ne.'\"') then 
c         parval = '\"'//parval(:lenact(parval))//'\"'
c      endif 

      if(status.ne.0) then 
         read_xpi_buffer = status
      else
         call ycappar(parname)
         call ycapval(parval)
      endif
      return 
      end




C
C       Print out all the parameters for a command. 
C
      Subroutine prparm(commnd)  

      character(16) commnd

      character(30) parnam(100)
      character(30) unfound(100)
      integer nunfound
      integer pos(100)
      integer npar
      integer max_pos
      integer i

      character(16) name
      character(30) parname
      character(30) clus 
      integer position, excl
      integer flag

      character(80) desc
      character(2) type
      character(60) minp 
      character(60) maxp 
      character(200) parval   

      character(100) str

C       Find all the parameters of the command
      call upc(commnd)
      i = 1 
      npar = 0 
      max_pos = 0 
 10   call tbgtky(i,name,parname,position,clus,excl) 
      if(i .lt. 0)  goto 11    
      call upc(name)
      if(name(:lenact(name)).ne.commnd(:lenact(commnd)))goto 10         
      npar = npar + 1
      parnam(npar) = parname 
      pos(npar) = position
      if(position .gt. max_pos) max_pos = position
      goto 10   
 11   continue
      if(npar.eq.0) return

      nunfound = 0
      if(max_pos .gt. 0) then 
         ipos = 1
 12      do i = 1, npar 
            if(pos(i).eq.ipos.and.ipos.le.max_pos) then 
               call fndfilpar(parnam(i),Desc,Type,Minp,
     &              Maxp,parval,flag)  
               if(flag.eq.-1) then 
                  nunfound = nunfound + 1 
                  unfound(nunfound) = parnam(i) 
               else  
                  parname = parnam(i)(:lenact(parnam(i)))//'['
     &                 //type(:lenact(type))//']'
                  write(str,89)parname(:lenact(parname)), parval(:16),
     &                 desc(:32)
                  call xwrite(str,5)
               endif 
               ipos = ipos + 1
               goto 12  
            endif 
         enddo
      endif  
        
      do i = 1, npar 
         if(pos(i).eq.0) then  
            call fndfilpar(parnam(i),Desc,Type,Minp,
     &           Maxp,parval,flag)  
            if(flag.eq.-1) then 
               nunfound = nunfound + 1 
               unfound(nunfound) = parnam(i) 
            else   
               parname = '('//parnam(i)(:lenact(parnam(i)))//'['
     &              //type(:lenact(type))//']' 
               parval = parval(:lenact(parval))//")"
               write(str,89)parname(:lenact(parname)),parval(:16),
     &              desc(:32)
               call xwrite(str,5)
            endif 
         endif
      enddo 
           
      do i = 1, nunfound
         write(str,79)unfound(i)(:lenact(unfound(i)))
         call xwrite(str,5) 
      enddo
 79   format(1x,"Warning: ",a20,"is not found.") 
 89   format(1x,a20," = ",a16,4x,a32) 
      return
      end

C    Subrouties for simulating behaviours of XSL_UCLGxx routines.
C    It will give a prompt and get a value and then pass
C    to the real UCLGxx.
C 

      SUBROUTINE XSL_UCLGSB(Parname,Buffer,Status)

C     return an boolean value for parname

      CHARACTER*(*) Parname
      LOGICAL Buffer
      INTEGER Status
      integer read_xpi_buffer

      status = read_xpi_buffer(parname)
      if(status.ne.0) return
      call uclgsb(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLGSI(Parname,Buffer,Status)

C     return an INTEGER value for parname

      CHARACTER*(*) Parname
      INTEGER Buffer
      INTEGER Status
      integer read_xpi_buffer

      status = read_xpi_buffer(parname)
      if(status.ne.0) return
      call uclgsi(Parname,Buffer,Status)
      return
      end
 

      SUBROUTINE XSL_UCLGSR(Parname,Buffer,Status)

*
* return a REAL value related to parname
*
      CHARACTER*(*) Parname
      REAL Buffer
      INTEGER Status
      integer read_xpi_buffer

      status = read_xpi_buffer(parname)
      if(status.ne.0) return
      call uclgsr(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLGSD(Parname,Buffer,Status)
*
* return a DOUBLE PRECISION value related to parname
*
      CHARACTER*(*) Parname
      DOUBLE PRECISION Buffer
      INTEGER Status
      integer read_xpi_buffer

      status = read_xpi_buffer(parname)
      if(status.ne.0) return
      call uclgsd(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLGST(Parname,Buffer,Status)

*
* return a string value related to parname
*
      CHARACTER*(*) Parname
      CHARACTER*(*) Buffer
      INTEGER Status
      integer read_xpi_buffer

      status = read_xpi_buffer(parname)
      if(status.ne.0) return
      call uclgst(Parname,Buffer,Status)

      return
      end

      SUBROUTINE XSL_UCLGSTD(Parname,Buffer,Status)

*
* return the default string value related to parname
*
      CHARACTER*(*) Parname
      CHARACTER*(*) Buffer
      INTEGER Status
      integer read_xpi_buffer

      status = read_xpi_buffer(parname)
      if(status.ne.0) return
      call uclgstd(Parname,Buffer,Status)
      return
      end

      SUBROUTINE XSL_UCLGSG(Parname,Buffer,N,Rmin,Rmax,Nr,Status)

*
* parname is the parameter name (c*(*))
* buffer1 is a 2xn real*4 array to hold the resulting ranges
* n is the size of buffer
* rmin and rmax are the min and max for the range
* nr is the number ranges in buffer which are good
* status is the return status
*
* return a range value related to parname
*
*
      CHARACTER*(*) Parname
      INTEGER N
      REAL Buffer(2,N) , Rmin , Rmax
      INTEGER Nr
      INTEGER Status
      integer read_xpi_buffer

      status = read_xpi_buffer(parname)
      if(status.ne.0) return
      call UCLGSG(Parname,Buffer,N,Rmin,Rmax,Nr,Status)
      return
      end 


      SUBROUTINE XSL_UCLGSGPARSE(Buffer,Buffer1,N,Rmin,Rmax,Nr,Status)

* parse a string into a buffer 
 
      IMPLICIT NONE
      INTEGER N
      REAL Buffer1(2,N) , Rmin , Rmax
      CHARACTER*(*) Buffer
      INTEGER Nr
      INTEGER Status

      CALL UCLGSGPARSE(Buffer,Buffer1,N,Rmin,Rmax,Nr,Status)
      RETURN
      END


      SUBROUTINE XSL_UCLGOT(Parname, Status)

* check if parameter has been entered on command line

      IMPLICIT NONE
      CHARACTER*(*) Parname
      INTEGER Status

      CALL uclgot(Parname, Status)
      RETURN
      END

      SUBROUTINE XSL_UCLPSB(Parname,Buffer,Status)

C     put a boolean value into parname

      CHARACTER*(*) Parname
      LOGICAL Buffer
      INTEGER Status

      call uclpsb(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLPSI(Parname,Buffer,Status)

C     put an integer*4 value into parname

      CHARACTER*(*) Parname
      INTEGER Buffer
      INTEGER Status

      call uclpsi(Parname,Buffer,Status)
      return
      end
 

      SUBROUTINE XSL_UCLPSR(Parname,Buffer,Status)

*
* put a REAL*4 value into parname
*
      CHARACTER*(*) Parname
      REAL Buffer
      INTEGER Status

      call uclpsr(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLPSD(Parname,Buffer,Status)
*
* put a REAL*8 value into parname
*
      CHARACTER*(*) Parname
      DOUBLE PRECISION Buffer
      INTEGER Status

      if(status.ne.0) return
      call uclpsd(Parname,Buffer,Status)
      return
      end


      SUBROUTINE XSL_UCLPST(Parname,Buffer,Status)

*
* return a string value related to parname
*
      CHARACTER*(*) Parname
      CHARACTER*(*) Buffer
      INTEGER Status

      call uclpst(Parname,Buffer,Status)
      return
      end





C
C  Take care of the fplot x-window and fv
C
      subroutine xset_fplotwin(fwin,fbkg,ffv)
C
C     ffv = 1: use fv
C     ffv = 0: use fdump
C
      implicit none
      CHARACTER*(*) fwin
      CHARACTER*(*) fbkg
      integer  ffv
      include 'xselplt.inc' 
      fpltwin = fwin
      fpltbkg = fbkg
      flagfv  = ffv
      return
      end


C  Ning Gan:   April, 1999:
C  Took out from xsel_unix.f and modified for tcl-tk input/output. 
c ---------------------------------------------
      subroutine XSL_RUNCF(fname,ECHO,ierr)
c ---------------------------------------------
c Makes a file executable and runs it as a command file
c     Alan Smale, June 1992
      implicit none

      character*(*) fname
      logical ECHO
      integer len1, ierr
      integer LENACT

      ierr = 0
      len1 = LENACT( fname )
C  Cat it back, if ECHO
C      call XSL_MORE(fname,ECHO)

c Now close the log file, so that the ftools can open it:
      call xpicloselog()

c Now run the command file

C     use the ierr to pass the echo option. Its return value is the 
C     status of the xsl_runcc.
      if(ECHO)ierr = 1
      call xsl_runcc(fname,ierr)
c Now reopen the log file
      call xpiopenlog()
      return
      end
      
