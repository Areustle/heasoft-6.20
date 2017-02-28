itcl::class RemoteClass {

    constructor {args} {}
    destructor {}

    private variable address ""
    private variable userName ""
    private variable passwd  ""
    private variable isOpen  0 
    private variable pwd ""
    private variable beginPwd ""

    public method openConn {add user pass}
    public method list {pwd}
    public method cd {dir}
    public method get {rFile lFile}
    public method pwd { {full 1} }
    private method closeConn {}
} 

itcl::body RemoteClass::constructor {args} {
    
}
    
itcl::body RemoteClass::destructor {} {
    closeConn
}

itcl::body RemoteClass::openConn {add user pass} {
    if {$isOpen == 1} {
	FTP::Close
	set address ""
	set passwd "" 
	set userName ""
	set isOpen 0
    } 
    set address $add
    set passwd $pass
    set userName $user

    FTP::Open $add $user $pass
    if { ! [FTP::isConn] } {
	error "Cannot open connection to $add, please check your user name and password"
	return
    }
    set isOpen 1
    set beginPwd [FTP::Pwd]
    if { $beginPwd == "/" } { set beginPwd "" }
}

itcl::body RemoteClass::list {pwd} {
    if { $isOpen == 0 } return
    
    set contentList [FTP::List $pwd]
    set dln ""
    set fln ""
    set dls ""
    set fls ""
    set dld ""
    set fld ""
    foreach i $contentList {
        set name [lindex $i end]
	if { "d" == [string range [lindex $i 0] 0 0] } {
	    if { [lindex $i end] == "." || [lindex $i end] == ".."} {
# get rid of the . and ..
		; 
	    } else {
		lappend dln $name/
                lappend dls "(dir)"
                lappend dld [join [lrange $i 5 7]]
	    }
	} elseif { "l" == [string range [lindex $i 0] 0 0] } {
           # Should we treat this as a directory or file?
           set link [lindex [split $name "/"] end]
           set name [lindex $i 8]
           set extLen [string length [file extension $name]]
           set lnkLen [string length [file extension $link]]
           if {  $extLen<2 || $extLen>6 || $extLen==[string length $name] || \
                 $lnkLen<2 || $lnkLen>6 || $lnkLen==[string length $link] \
              } {
              # Directory
              lappend dln $name/
              lappend dls "(sym)"
              lappend dld [join [lrange $i 5 7]]
           } else {
              # File
              lappend fln $name
              lappend fls "(sym)"
              lappend fld [join [lrange $i 5 7]]
           }
	} else {
	    lappend fln $name
            lappend fls [calcSizeStr [lindex $i 4]]
            lappend fld [join [lrange $i 5 7]]
	} 
    }
    return [::list $dln $dls $dld $fln $fls $fld]
}

itcl::body RemoteClass::cd {dir} {
    if { $isOpen == 0 } return
    
    if { $dir == "" } return
    FTP::Cd $dir
    set pwd $dir
}
    
itcl::body RemoteClass::get {rFile lFile} {
   if {$isOpen == 0} return 

   FTP::Type binary
   FTP::Get $rFile $lFile
}

itcl::body RemoteClass::pwd { {full 1} } {

    if {$isOpen == 0} return 

    set dir [FTP::Pwd]
    if { $full } {
       return "ftp://${address}${dir}"
    }

    set crrntParts [split $dir '/']
    set beginParts [split $beginPwd '/']
    set same 1
    set common ""
    set backup ""
    for { set i 1 } { $i < [llength $beginParts] } { incr i } {
	if { $same && [lindex $beginParts $i]==[lindex $crrntParts $i] } {
	    set common "$common/[lindex $beginParts $i]"
	} else {
	    set backup "/..$backup"
	    set same 0
	}
    }
    set dir "$backup/.[string range $dir [string length $common] end]"

    set netdir "ftp://${address}${dir}"
    return $netdir
}

itcl::body RemoteClass::closeConn {} {
    if {$isOpen == 0} return 
    FTP::Close
}
