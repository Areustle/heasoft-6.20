* Common include file for all of the command interface tables
*
* $Id: tbl.inc,v 3.9 2015/06/11 20:02:35 kaa Exp $
*



* parameter table

* max number of parameters
      integer tblpmax
      parameter (tblpmax=500)
* max number of parameter files
      integer tblpfmax
      parameter (tblpfmax=20)
* count of parameters
      integer tblpcnt
      integer tblpcntorig
* extra parameters for command driven tasks (gtcom2)
      integer tblpextra
      parameter (tblpextra=14)
* Table holding status of paramter
* .true. = it's still good, .false. = it's been read
      logical tblpstat(tblpmax)
      common /tblpi/tblpcnt,tblpcntorig,tblpstat
* parameter name
      character(40) tblpname(tblpmax)
* parameter description
      character(120) tblpdesc(tblpmax)
* parameter type
      character(5)  tblptype(tblpmax)
* parameter minimum
      character(120) tblpminp(tblpmax)
* parameter maximum
      character(120) tblpmaxp(tblpmax)
* parameter default
      character(700) tblpdefl(tblpmax)
* parameter update
      character(10) tblpupd(tblpmax)
* parameter actual update
      character(10) tblpupda(tblpmax)
* parameter file number
      integer tblppfnum(tblpmax)
* parameter filename
      character(100) tblpfname(tblpfmax)
* current parameter file number
      integer tblpcpf

      common /tblpc /tblpname,tblpdesc,tblptype,tblpminp,tblpmaxp,
     &               tblpdefl,tblpupd,tblpupda,tblpfname,tblppfnum,
     &               tblpcpf


      

* command table

* max number of commands
      integer tblcmax
      parameter (tblcmax=200)
* count of commands
      integer tblccnt

      common /tblci/tblccnt
* command name
      character(20) tblcname(tblcmax)
* command description
      character(80) tblcdesc(tblcmax)
* command access
      character(1) tblcacce(tblcmax)
* command window type
      character(10) tblcwtyp(tblcmax)

      common /tblcc/tblcname,tblcdesc,tblcacce,tblcwtyp

* keywords table

* max number of keywords
      integer tblwmax
      parameter (tblwmax=400)
* count of keywords
      integer tblwcnt

      common /tblwi/ tblwcnt
* keyword name
      character(20) tblwname(tblwmax)
* command name
      character(20) tblwcomm(tblwmax)
      common /tblwc/ tblwname, tblwcomm

* aliases table

* max number of aliases
      integer tblamax
      parameter (tblamax=120)
* count of keywords
      integer tblacnt

      common /tblai/ tblacnt
* keyword name
      character(20) tblaname(tblamax)
* command name
      character(120) tblacomm(tblamax)

      common /tblac/ tblaname, tblacomm

* key table

* max number of keys
      integer tblkmax
      parameter (tblkmax=400)
* count of keys
      integer tblkcnt
* positional order
      integer tblkposo(tblkmax)
* exclusive flag
      integer tblkexcl(tblkmax)

      common /tblki/ tblkcnt, tblkposo, tblkexcl

* command name
      character(20) tblkname(tblkmax)
* parameter
      character(40) tblkparm(tblkmax)
* cluster
      character(20) tblkclus(tblkmax)

      common /tblkc/ tblkname, tblkparm,tblkclus

* standalone??
      logical    tblstandalone
      character(1000) tblstandalonecmd
      logical           tblextrapar

* good command
      logical tblgood

      common /tblmisci/ tblstandalone, tblgood, tblextrapar
      common /tblmiscc/ tblstandalonecmd

C      common /tblcomc/ tblpname,tblpdesc,tblptype,
C     1            tblpminp,tblpmaxp,tblpdefl,tblpupd,tblpupda,
C     2            tblcname,tblcdesc,tblcacce,tblcwtyp,
CC     3            tblwname,tblwcomm,
C     4              tblkname,tblkparm,tblkclus,
C     5            tblaname,tblacomm,tblpfname,
C     6            tblstandalonecmd

C      common /tblcomi/ tblpcnt,tblccnt,
C     3            tblwcnt,
C     4            tblkcnt,tblkposo,tblkexcl,
C     5            tblacnt,tblstandalone,
C     6            tblpstat,tblgood,tblpcntorig

