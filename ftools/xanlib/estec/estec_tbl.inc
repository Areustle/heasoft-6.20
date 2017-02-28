* Common include file for all of the command interface tables
*
*



* parameter table

* max number of parameters
	integer*4 tblpmax
	parameter (tblpmax=300)
* count of parameters
	integer*4 tblpcnt
* parameter name
	character(20) tblpname(tblpmax)
* parameter description
	character(80) tblpdesc(tblpmax)
* parameter type
	character(5)  tblptype(tblpmax)
* parameter minimum
	character(10) tblpminp(tblpmax)
* parameter maximum
	character(10) tblpmaxp(tblpmax)
* parameter default
	character(50) tblpdefl(tblpmax)
* parameter update
	character(10) tblpupd(tblpmax)
* parameter actual update
	character(10) tblpupda(tblpmax)

* parameter filename
	character(100) tblpfname

* command table

* max number of commands
	integer*4 tblcmax
	parameter (tblcmax=200)
* count of commands
	integer*4 tblccnt
* command name
	character(20) tblcname(tblcmax)
* command description
	character(80) tblcdesc(tblcmax)
* command access
	character(1) tblcacce(tblcmax)
* command window type
	character(10) tblcwtyp(tblcmax)

* keywords table

* max number of keywords
	integer*4 tblwmax
	parameter (tblwmax=400)
* count of keywords
	integer*4 tblwcnt
* keyword name
	character(20) tblwname(tblwmax)
* command name
	character(20) tblwcomm(tblwmax)

* aliases table

* max number of aliases
	integer*4 tblamax
	parameter (tblamax=120)
* count of keywords
	integer*4 tblacnt
* keyword name
	character(20) tblaname(tblamax)
* command name
	character(120) tblacomm(tblamax)

* key table

* max number of keys
	integer*4 tblkmax
	parameter (tblkmax=400)
* count of keys
	integer*4 tblkcnt
* command name
	character(20) tblkname(tblkmax)
* parameter
	character(40) tblkparm(tblkmax)
* positional order
	integer*4 tblkposo(tblkmax)
* cluster
	character(20) tblkclus(tblkmax)
* exclusive flag
	integer*4    tblkexcl(tblkmax)

* standalone??
	logical*4    tblstandalone
	character(100) tblstandalonecmd

	common /tblcom/ tblpcnt,tblpname,tblpdesc,tblptype,
     1		tblpminp,tblpmaxp,tblpdefl,tblpupd,tblpupda,tblccnt,
     2		tblcname,tblcdesc,tblcacce,tblcwtyp,
     3		tblwcnt,tblwname,tblwcomm,
     4		tblkcnt,tblkname,tblkparm,tblkposo,tblkclus,tblkexcl,
     5		tblacnt,tblaname,tblacomm,tblpfname,tblstandalone,
     6		tblstandalonecmd

