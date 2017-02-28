# modified by Jianjun Xu 

#
#                          FitsFileselectionbox
# ------------------------------------------------------------------
itcl::class FitsFileselectionbox {
    inherit itk::Toplevel

    constructor {args} {}
    destructor {}

    private common _ffbFilter *
    private common _ffbFileName ""
    private common _displayFitsOnly 0
    private common _userName ""
    private common _passwd ""
    private common _ftpHost ""

    private variable _pwd .
    private variable _selected ""
    private variable _fileCmd Open
    private variable _isFtp 0
    private variable _lastLocalDir  "" 
    private variable _lastRemoteDir ""
    private variable _nameList {}
    private variable _sizeList {}
    private variable _dateList {}
    private variable _dirChar "/"
    private variable currentWndw ""
    
    private method _fillContent {cDir}
    private method _upDir {}
    private method _setDir { n }
    private method _openThisFile {}
    private method _openSelection {} 
    private method _completeFileName {}
    private method _selectThat { box }
    private method _listFits {}
    private method _setFilter {}
    private method _helpCmd {} 
    private method _initFtp {} 
    private method _isDir {}
    private method _pwd { {full 1} }
    private method _updateAll {}
    private method _switchRemoteLocal {}
    private method _closeFTP {}
    private method _showNamedFile {}
    private method _searchCharsForCompletion {root list} 
    private method _pickElemByName {name list} 
    private method _ftpOpen {}
    private method _buildMenu {}
    private method _postMenu {time x y}
    private method _buttonRelease {time}
    private method urlSplit { url_ }
    private method calcDateStr { time_ }
    private method setWatchCursor { win_ args }
    private method resolveSymLinks { filename_ }
    private method calcSizeStr { size_ }

    private method _scrollBoxes  { args }
    private method _scrollOthers { box args }
    private method _updateBoxes  { data }

    private method _cancelCmd   {}
    private method _openFileCmd {}
    private method _saveFileCmd {}
    private method _setCmd {cmd} 

    private variable _postTime
    private variable _pathElems
    private variable _fileCols "files sizes dates"
    private variable _fileLbls "Name Size {Mod Date}"
    private variable _driveList
    private variable _dsizeList
    private variable _ddateList
    private variable _callRoutine

    public method get {} 
    public method chgDir {dir}
    public method init {}
    public method activate { cmd {defaultName ""} }
    public method deactivate {}
    public method changeListFITSoption {}
}

# ------------------------------------------------------------------
#                        CONSTRUCTOR
# ------------------------------------------------------------------
itcl::body FitsFileselectionbox::constructor {args} {
    global g_fitsFileMode
    global isMac
    global isWin
    global env tcl_platform

    set _callRoutine [lindex $args 0]
    set args [lreplace $args 0 0]

    if { $tcl_platform(platform) == "windows" } {
       load fitstcl fits
    } else {
       load [glob $env(FITSVIEWER_LIBRARY)/../libfitstcl.{s\[ol\]*,dylib}]
    }

    if { $isWin } {
       set _driveList {}
       foreach drive { c d e f g h i j k l m n o p q r s } {
           catch { [glob ${drive}:/*] } returnCodeList
           catch { [glob ${drive}:/*.*] } returnCodeFile
           if { [string first "no files matched" $returnCodeList] < 0 ||
                [string first "no files matched" $returnCodeFile] < 0 } {
              lappend _driveList "$drive:/"
              lappend _dsizeList "(dir)"
              lappend _ddateList {}
           }
       }
    }
    
    wm withdraw $itk_component(hull)
    wm geometry $itk_component(hull) 350x450
    if { $isMac } { set _dirChar ":" }

    component hull configure -borderwidth 0

    #
    # Create an internal frame to contain the components.
    #
    itk_component add frame {
        frame $itk_interior.frame
    } 
    pack $itk_component(frame) -fill both -expand yes
#    pack propagate $itk_component(frame) no

    # Create the dir entry.
    #
    itk_component add dframe {
	frame $itk_component(frame).df 
    }
    pack $itk_component(dframe) -fill x -expand 0 -pady 4 -padx 4

    itk_component add dirBtn {
       menubutton $itk_component(dframe).dir -indicatoron 1 -relief raised \
             -menu $itk_component(dframe).dir.menu
    }
    pack $itk_component(dirBtn) -side left -fill x -expand 1 -padx 2

    itk_component add dirMenu {
       menu $itk_component(dirBtn).menu -tearoff 0
    }

    bind $itk_component(dirBtn) <ButtonPress-1> \
	    "[itcl::code $this _postMenu %t %X %Y]; break"
    bind $itk_component(dirMenu) <ButtonRelease-1> \
	    [itcl::code $this _buttonRelease %t]
    
    image create bitmap _upDirIcon -data {
	#define updir_width 28
	#define updir_height 16
	static char updir_bits[] = {
           0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20,
           0x00, 0x00, 0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01,
           0x10, 0x00, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x07,
           0x00, 0x01, 0x90, 0x0f, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
           0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0xfe,
           0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
           0xf0, 0xff, 0xff, 0x01};
    }
    
    itk_component add updir {
 	button $itk_component(dframe).up \
	    -image _upDirIcon \
	    -command  [itcl::code $this _upDir]
    }
    pack $itk_component(updir) -side left -expand 0 -padx 2


    ############################
    #
    #   Create the files list.
    #

    itk_component add fframe {
       frame $itk_component(frame).fframe -relief sunken
    }
    pack $itk_component(fframe) -side top -fill both -expand 1 -padx 4 -pady 6
    grid columnconfig $itk_component(fframe) 0 -weight 1
    grid rowconfig    $itk_component(fframe) 1 -weight 1

    # Create file columns

    set i 0
    foreach l $_fileLbls lb $_fileCols {
       itk_component add lbl$i {
          label $itk_component(fframe).lbl$i -text $l -relief raised
       }
       itk_component add $lb {
          listbox $itk_component(fframe).$lb -height 3 -relief flat \
                -exportselection 0 -bd 0 -highlightthickness 0 \
                -yscrollcommand [itcl::code $this _scrollOthers $lb] \
                -takefocus 0
       }
       grid $itk_component(lbl$i) -row 0 -column $i -sticky "news"
       grid $itk_component($lb)   -row 1 -column $i -sticky "news"

       bind $itk_component($lb) <ButtonRelease-1> \
             +[itcl::code $this _selectThat $lb]
       bind $itk_component($lb) <Double-1> \
             [itcl::code $this setWatchCursor $itk_component(hull) [itcl::code $this _openSelection]]
       bind $itk_component($lb) <Enter> {
            %W configure -cursor hand2
       }
       incr i
    }
    $itk_component(sizes) config -width 7
    $itk_component(dates) config -width 12

    # scrollbar

    itk_component add fscroll {
        scrollbar $itk_component(fframe).fscroll \
              -command [itcl::code $this _scrollBoxes]
    } 
    grid $itk_component(fscroll) -row 1 -column $i -sticky news

    #  
    #
    ############################

# the bottom portion using grid
    itk_component add bframe {
        frame $itk_component(frame).bframe
    } 
    pack $itk_component(bframe) -side top -fill x -expand 0   
# File name 
    itk_component add fnamel {
	label $itk_component(bframe).fnamel -text "File Name"
    }
    itk_component add fnamee {
	entry $itk_component(bframe).fnamee -textvariable [itcl::scope _ffbFileName]
    }
    bind $itk_component(fnamee) <Return> \
	  [itcl::code $this setWatchCursor $itk_component(hull) [itcl::code $this _openThisFile]]

# 
#    bind all <Tab> {}
    bind $itk_component(fnamee) <Tab> "[itcl::code $this _completeFileName]; break"
# Filter 
    itk_component add filterl {
	label $itk_component(bframe).filterl -text "File of type"
    }
    itk_component add filtere {
	entry $itk_component(bframe).filtere -textvariable [itcl::scope _ffbFilter]
    }
    bind $itk_component(filtere) <Return> [itcl::code $this _setFilter]
# fitsfile
    # set _displayFitsOnly $fguiPref::ifDispFitsOnly

    itk_component add fitsmode {
	checkbutton $itk_component(bframe).fitsmode \
	    -variable [itcl::scope _displayFitsOnly] \
	    -text "List FITS files only" \
	    -selectcolor $fguiPref::checkBBgColor \
	    -activeforeground black -activebackground $fguiPref::globalBgColor \
	    -command [itcl::code $this _listFits] 
    }
# buttons
    itk_component add openB {
	button $itk_component(bframe).openB -text Open \
	    -command [itcl::code $this setWatchCursor $itk_component(hull) [itcl::code $this _openThisFile]]
    }
    itk_component add cancelB {
	button $itk_component(bframe).cancelB -text Cancel \
	    -command [itcl::code $this _cancelCmd]
    }
    itk_component add helpB {
	button $itk_component(bframe).helpB -text Help \
	    -command [itcl::code $this _helpCmd]
    }
# geometry
    grid config $itk_component(fnamel) -column 0 -row 0 \
	-columnspan 1 -rowspan 1 -sticky "w" 
    grid config $itk_component(fnamee) -column 1 -row 0 \
	-columnspan 1 -rowspan 1 -sticky "snew" 
    grid config $itk_component(filterl) -column 0 -row 1 \
	-columnspan 1 -rowspan 1 -sticky "w"
    grid config $itk_component(filtere) -column 1 -row 1 \
	-columnspan 1 -rowspan 1 -sticky "snew" 
    grid config $itk_component(fitsmode) -column 1 -row 3 \
	-columnspan 1 -rowspan 1 -sticky "w"
    grid config $itk_component(openB) -column 2 -row 0 \
	-columnspan 1 -rowspan 1 -sticky "snew" -padx 4
    grid config $itk_component(cancelB) -column 2 -row 1 \
	-columnspan 1 -rowspan 1 -sticky "snew" -padx 4
    grid config $itk_component(helpB) -column 2 -row 2 \
	-columnspan 1 -rowspan 1 -sticky "snew" -padx 4

    grid columnconfigure $itk_component(bframe) 1 -weight 5

    wm protocol $itk_interior WM_DELETE_WINDOW [itcl::code $this _cancelCmd]

    eval itk_initialize $args

}

# ------------------------------------------------------------------
#                           DESTRUCTOR
# ------------------------------------------------------------------
itcl::body FitsFileselectionbox::destructor {} {
}

itcl::body FitsFileselectionbox::_fillContent {cDir} {
    global isMac isWin
    
    if { $cDir != "" } {
        chgDir $cDir
    }
    _buildMenu

    set fileNameList ""
    set fileSizeList ""
    set fileDateList ""
    set dirNameList  ""
    set dirSizeList  ""
    set dirDateList  ""
    set selCO ""

    if { $_isFtp } {
        _updateBoxes [list {\[Loading...\]} {} {}]
        set rlist [itcl::code $this [setWatchCursor $itk_component(hull) [itcl::code ftpClient list ""]]]
	set dirNameList  [lindex $rlist 0]
        set dirSizeList  [lindex $rlist 1]
        set dirDateList  [lindex $rlist 2]
	set fileNameList [lindex $rlist 3]	
        set fileSizeList [lindex $rlist 4]
        set fileDateList [lindex $rlist 5]
    } else {
	set _lastLocalDir $_pwd
	set _ffbFilter [string trim $_ffbFilter " "]
	if { $_ffbFilter  == "" } {
	    set _ffbFilter *
	}
	
	set tmpID [$itk_component(files) curselection]
	if { $tmpID != "" } {
	    $itk_component(files) see $tmpID
	} 
	
        if { $cDir == "" } {
            set curContent [file volumes]
        } else {
            #multi filter separated by comma
            set _ffbFilter [join [split $_ffbFilter ","]]
            set curContent [lsort -increasing [eval glob -nocomplain $_ffbFilter]]
        }

        foreach i $curContent {
	   if ![file readable $i ] continue
           if {  ( [string index $i 0]=="~" ) || \
                 ( ($isMac || $isWin) && [file attributes $i -hidden]) || \
                 ( $isMac && $i=="Trash" ) } {
              continue
           }
           if { [catch {set fType [file type [resolveSymLinks $i]]}] } {
              continue
           }
           switch $fType {
              file {
                 if { $_displayFitsOnly } {
                    if { [catch {set isfits [isFits $i]} err] } {
                        continue
                    }
                    if { $isfits == 0 } continue
                 }
                 lappend fileNameList $i
                 lappend fileSizeList [calcSizeStr [file size $i]]
                 lappend fileDateList [file mtime $i]
              }
              directory {
                 lappend dirNameList [string trimright $i $_dirChar]$_dirChar
                 lappend dirSizeList "(dir)"
                 lappend dirDateList [file mtime $i]
              }
              default {
                 ;
              }
           }
        }

    }
    if { $isMac } {
	set _nameList [eval list $dirNameList $fileNameList]
        set _sizeList [eval list $dirSizeList $fileSizeList]
        set _dateList [eval list $dirDateList $fileDateList]
    } else {
	set _nameList [eval list "../" $dirNameList $fileNameList]
        set _sizeList [eval list "(dir)" $dirSizeList $fileSizeList]
        set _dateList [eval list "-" $dirDateList $fileDateList]
    }

    set selID [$itk_component(files) curselection]
    if { $selID != "" } {
	set selCO [$itk_component(files) get $selID]
    }

    if { [llength $_nameList] == 0 } {
       _updateBoxes [list {\[  \]} {} {}]
    } elseif { $_isFtp } {
       _updateBoxes [list $_nameList $_sizeList $_dateList]
    } else {
       set _dateListFmt {}
       foreach d $_dateList {
          lappend _dateListFmt [calcDateStr $d]
       }
       _updateBoxes [list $_nameList $_sizeList $_dateListFmt]
    }

    set newID [lsearch $_nameList $selCO] 
    if { $newID != -1} {
	$itk_component(files) selection set $newID
	$itk_component(files) see $newID
        _selectThat files
    }

}

itcl::body FitsFileselectionbox::_upDir {} {
    global isMac
    global isWin
    
    set preUpDir $_pwd
    if { $_isFtp } {
	ftpClient cd ..
    } else {
        if { $isMac && [string first : $_pwd] == [expr [string length $_pwd]-1] } {
            set _pwd ""
            _fillContent ""
            return
        } elseif { $_pwd != "" } {
            cd ..
        }
    }
    set _pwd [_pwd]
    set postUpDir $_pwd
   
    if { $isWin && $preUpDir == $postUpDir } {
       _updateBoxes [list $_driveList $_dsizeList $_ddateList]
    } else {
       _fillContent $_pwd
    }
}

itcl::body FitsFileselectionbox::_selectThat { box } {
   set tmpIdx [$itk_component($box) curselection]
   if { $tmpIdx == "" } {
      set _selected ""
      return
   }
   foreach b $_fileCols {
      if { $box!=$b } {
         $itk_component($b) selection clear 0 end
         $itk_component($b) selection set $tmpIdx
      }
   }
   set _selected [$itk_component(files) get $tmpIdx]
   if { ![_isDir] } {
      set _ffbFileName $_selected
      $itk_component(fnamee) selection range 0 end
      focus $itk_component(fnamee)
      $itk_component(fnamee) icursor end
   }
}


itcl::body FitsFileselectionbox::_isDir {} {
    global isMac
    if { $_isFtp } {
	if { [string range $_selected end end] == "/" } {
	    return 1
	} elseif { [string trim $_selected .] == "" } {
	    return 1
        } elseif { [lsearch -exact $_nameList $_ffbFileName/] != -1 } {
	    return 1
        } elseif { [string first / $_ffbFileName] != -1 } {
            return 1
	} else {
	    return 0
	} 
    } else {
        if { $isMac && $_pwd=="" } {
            return 1
        } else {
            return [file isdirectory [string trimright $_selected $_dirChar]]
        }
    }
}

itcl::body FitsFileselectionbox::_openThisFile {} {
   global pWorkingDirectory

   set _ffbFileName [string trim $_ffbFileName " "]
   if { $_ffbFileName != "" } {
      set _selected $_ffbFileName
      set dirFlag [_isDir]
      _openSelection
      if { [winfo exists $currentWndw] && $currentWndw != "" } {
         $currentWndw delete 0 end
         $currentWndw insert end $_pwd/$_ffbFileName
         set pWorkingDirectory $_pwd
         .ftoolframe.tools.info.entry delete 0 end
         .ftoolframe.tools.info.entry insert end $pWorkingDirectory
      }
      if { $dirFlag } {
         set _ffbFileName ""
      }
   }
}


itcl::body FitsFileselectionbox::_openSelection {} {
    global isMac
    if { $_selected=="" } return
    if { [_isDir] } {
	# open contents of directory
	if { !$isMac && [file pathtype $_selected]=="absolute" } {
	    _fillContent $_selected
	} else {
	    _fillContent $_pwd$_selected
	}
    } else {
	# it's a file
	if { $_fileCmd == "Save" } {
            _saveFileCmd
	} elseif { $_fileCmd == "Open"} {
	    _openFileCmd
	} else {
	    puts "Un-supported file command"
	}
    }
}


itcl::body FitsFileselectionbox::_listFits {} {
    _fillContent $_pwd
    # set fguiPref::ifDispFitsOnly $_displayFitsOnly
    # ::fguiPref save
}

itcl::body FitsFileselectionbox::_setFilter {} {
    _fillContent $_pwd
}

itcl::body FitsFileselectionbox::_helpCmd {} {
    $_callRoutine displayHelpMessage fileSelection.html NONE local
}

itcl::body FitsFileselectionbox::get {} {
    if { $_isFtp } {
       # Need to call _pwd here to remove the starting directory from filename
       set r_string "[_pwd 0]$_ffbFileName"
       set r_string \
             "ftp://${_userName}:${_passwd}\@[string range $r_string 6 end]"
    } else {
       set r_string [string trim $_ffbFileName " "]

       if { [file pathtype $_ffbFileName]!="absolute" } {
          set r_string "$_pwd$_ffbFileName"
       }

    }
    return $r_string
}

itcl::body FitsFileselectionbox::_setCmd {cmd} {
    set _fileCmd $cmd
    if { $cmd == "Open" } {
	$itk_component(openB) configure -text Open 
    } elseif {$cmd == "Save" } { 
	$itk_component(openB) configure -text Save 
    } else {
	error "Not recognized command"
    }
}

itcl::body FitsFileselectionbox::init {} {
    $itk_component(files) select clear 0 end
    if { $_isFtp } {
        # Reinitialzing, so return to local directory
        _switchRemoteLocal
    } else {
        catch { _fillContent [_pwd] } err
    }
}

itcl::body FitsFileselectionbox::chgDir {dir} {
    if { $_isFtp } {
       set dir [string range $dir 6 end]
       set idx [string first / $dir]
       if { $idx==-1 } {
          set tmpdir "/"
       } else {
          set tmpdir [string range $dir $idx end]
          if { $tmpdir=="/." } {set tmpdir "."}
       }
       ftpClient cd $tmpdir
    } else {
       cd $dir
    }
    
    set _pwd [_pwd]
}

itcl::body FitsFileselectionbox::_initFtp {} {
     global g_titleFont

    _closeFTP

    RemoteClass ftpClient
# ask for user name and _passwd
    powToplevel .ftp .dummy

    iwidgets::entryfield .ftp.rhost -labeltext "Remote Host" \
	-labelpos nw -textvariable [itcl::scope _ftpHost] \
	-command  [itcl::code $this _ftpOpen] -textfont g_titleFont -labelfont g_titleFont
    pack .ftp.rhost -padx 4 -pady 4 -fill x

    iwidgets::entryfield .ftp.login -labeltext "Optional User Name (if not 'anonymous')" \
	-labelpos nw -textvariable [itcl::scope _userName] \
	-command  [itcl::code $this _ftpOpen] -textfont g_titleFont -labelfont g_titleFont
    pack .ftp.login -padx 4 -pady 4 -fill x

    iwidgets::entryfield .ftp._passwd -labeltext "Password" \
	-labelpos nw -show "\267" -textvariable [itcl::scope _passwd] \
	-command  [itcl::code $this _ftpOpen] -textfont g_titleFont -labelfont g_titleFont
    pack .ftp._passwd -padx 4 -pady 4 -fill x

    iwidgets::Buttonbox .ftp.butts -padx 4 -pady 4 -orient horizontal

    .ftp.butts add connect -text "Connect" \
          -font g_titleFont \
	  -command [itcl::code $this _ftpOpen]
    .ftp.butts add cancel -text "Cancel" \
          -font g_titleFont \
	  -command { destroy .ftp           }
    .ftp.butts default connect
    pack .ftp.butts 
    focus [.ftp.rhost component entry]

    tkwait window .ftp
}

itcl::body FitsFileselectionbox::_ftpOpen {} {
   if { $_userName=="" } {
      set _userName anonymous
   }
   if { $_passwd=="" && ($_userName=="anonymous" || $_userName=="ftp") } {
      set _passwd "fv@fv.gsfc.nasa.gov"
   }      
   setWatchCursor .ftp [itcl::code ftpClient openConn $_ftpHost $_userName $_passwd]
   set _isFtp 1
   destroy .ftp
}

itcl::body FitsFileselectionbox::_pwd { {full 1} } {
    # Always return the _dirChar at end of the _pwd string
    if { $_isFtp } {
	return [string trimright [ftpClient pwd $full] "/"]/
    } else {
	return [string trimright [pwd] $_dirChar]$_dirChar
    } 
}

itcl::body FitsFileselectionbox::_updateAll {} {
    if { $_isFtp } return
    _setFilter
}

itcl::body FitsFileselectionbox::_switchRemoteLocal {} {
    if { $_isFtp } {
	set ffbCurrentDir $_lastLocalDir
	set _isFtp 0
	$itk_component(ftpB) configure -text "FTP..."
	_closeFTP
    } else {
       _initFtp
       if { $_isFtp } {
	  set ffbCurrentDir "ftp://$_ftpHost/."
	  $itk_component(ftpB) configure -text "Local..."
       } else {
          set ffbCurrentDir $_lastLocalDir
       }
    }

    _fillContent $ffbCurrentDir
}

itcl::body FitsFileselectionbox::_closeFTP {} {
# if another ftp is in session, close it first
    if { [itcl::find objects -class RemoteClass] != "" } {
	itcl::delete object ftpClient
    } 
}

itcl::body FitsFileselectionbox::_completeFileName {} {
    if { $_ffbFileName == "" } return

    set tmpContent [lsort -increasing \
        [_pickElemByName ${_ffbFileName} $_nameList]]

    if { [llength $tmpContent] == 0 } return

    set tmpRootName [lindex $tmpContent 0]
    set tmpRootLength [string length $tmpRootName]

    set tmpList [lrange $tmpContent 1 end]
    if { [llength $tmpList ] == 0 } {
       set i [string length $tmpRootName]
    } else {
       for {set i 0} {$i < $tmpRootLength} {incr i} {
          set tmpRoot [string range $tmpRootName 0 $i]
          
          if { [_searchCharsForCompletion $tmpRoot $tmpList] == 1} {
             break
          }
       }
    }

    set _ffbFileName [string range $tmpRootName 0 [expr $i-1]]
    _showNamedFile
    $itk_component(fnamee) selection range $i end
}

itcl::body FitsFileselectionbox::_pickElemByName {name list} {
    set tmplist ""

    set last [expr [string length $name]-1]
    foreach i $list {
	if { $name==[string range $i 0 $last]} {
	    lappend tmplist $i
	}
    }
    return $tmplist
}

itcl::body FitsFileselectionbox::_searchCharsForCompletion {root list} {
    set last [expr [string length $root]-1]
    foreach i $list {
	if { $root!=[string range $i 0 $last] } {
	    return 1
	}
    }
    return 0
}


itcl::body FitsFileselectionbox::_showNamedFile {} {

    $itk_component(fnamee) icursor end
    
    set tmpIndex [lsearch -glob $_nameList ${_ffbFileName}*]
    if { $tmpIndex == -1} return
    $itk_component(files) see $tmpIndex
    $itk_component(files) select clear 0 end
    $itk_component(files) select set $tmpIndex
    _selectThat files
}

###################################################

itcl::body FitsFileselectionbox::activate { cmd {defaultName ""} } {
    init
    _setCmd $cmd

    if { $cmd == "Open" && $defaultName != "" } {
       set currentWndw $defaultName
       set defaultName ""
    }

    if { $defaultName != "" } {
       set _ffbFileName $defaultName
    }
    wm deiconify $itk_component(hull)
    raise $itk_component(hull)
    focus $itk_component(hull)
    $itk_component(fnamee) selection range 0 end
    $itk_component(fnamee) icursor end
}

itcl::body FitsFileselectionbox::deactivate {} {
     _cancelCmd
}

###################################################

itcl::body FitsFileselectionbox::_cancelCmd {} {
   global fileselect

   wm withdraw $itk_component(hull)
   set fileselect ""
}


itcl::body FitsFileselectionbox::_openFileCmd {} {
   global g_isScript fileselect
   set fileselect [get]
   if { $_isFtp } {
      itcl::delete object ftpClient
      if { [catch {openFitsFileWM $fileselect 1} err] } {
         # reopen ftpClient
         RemoteClass ftpClient
         setWatchCursor "" \
               [itcl::code ftpClient openConn $_ftpHost $_userName $_passwd]
         _fillContent $_pwd
         error $err
      }

   } else {

      if {$fileselect == "" } {
         error "Please select a file"
         return
      } 

      if { [file exist $fileselect] == 0 } {
         set oldFile $fileselect

         foreach ext { gz Z z zip } {
            if { [file exists $fileselect.$ext] } {
               set fileselect $fileselect.$ext
               break     
            }
         }
         if { $fileselect == $oldFile } {
            error "File $fileselect does not exist"
            return
         }
      }
      if { [file readable $fileselect] == 0 } {
         error "File $fileselect is not readable"
         return
      }
   }
   wm withdraw $itk_component(hull)
}


itcl::body FitsFileselectionbox::_saveFileCmd {} {
   global fileselect

   set fileselect [get]
   set saveToDir  [file dir $fileselect]
   if {$fileselect == "" } {
      error "No file name given"
   } elseif { ![file writable $saveToDir] } {
      error "$saveToDir is not writable"
   } else {
      wm withdraw $itk_component(hull)
   }
}


###################################################

itcl::body FitsFileselectionbox::_buildMenu {} {
   global isMac

   $itk_component(dirMenu) delete 0 end
   set _pathElems [urlSplit $_pwd]
   if { $_isFtp } {
      set newP "[lindex $_pathElems 0][lindex $_pathElems 1]"
      eval lappend newP [lrange $_pathElems 2 end]
      set _pathElems $newP
   } elseif { $isMac } {
      set _pathElems [linsert $_pathElems 0 Desktop]
   }

   if { !$isMac || $_isFtp } {
      set pathSym $_dirChar
   } else {
      set pathSym ""
   }

   set c 0
   foreach i $_pathElems {
      if { $i==$pathSym } { set i "" }
      $itk_component(dirMenu) add command -label $i$pathSym \
            -command [itcl::code $this _setDir $c]
      incr c
   }
   $itk_component(dirBtn) configure -text $i$pathSym
}

itcl::body FitsFileselectionbox::_setDir { n } {
   global isMac
   
   if { $_isFtp } {
      set newPath [join [lrange $_pathElems 0 $n] /]
   } elseif { $isMac } {
      if { $n==0 } {
          set newPath ""
          set _pwd ""
      } else {
          set newPath [eval file join [lrange $_pathElems 1 $n]]
      }
   } else {
      set newPath [eval file join [lrange $_pathElems 0 $n]]
   }
   _fillContent $newPath
}

itcl::body FitsFileselectionbox::_postMenu {time X Y} {

   set _postTime $time

   set x [expr $X - [winfo reqwidth $itk_component(dirMenu)]/2  ]
   set y [expr $Y - [$itk_component(dirMenu) yposition end] - 10]

   tk_popup $itk_component(dirMenu) $x $y
}


itcl::body FitsFileselectionbox::_buttonRelease {time} {
    if { [expr abs([expr $_postTime - $time])] <= 150 } {
        return -code break
    }
}

############################################################

itcl::body FitsFileselectionbox::_scrollBoxes { args } {
   foreach b $_fileCols {
      eval $itk_component($b) yview $args
   }
}

itcl::body FitsFileselectionbox::_scrollOthers { box args } {
   eval $itk_component(fscroll) set $args
   set view [$itk_component($box) yview]
   foreach b $_fileCols {
      if { $box!=$b } {
         $itk_component($b) yview moveto [lindex $view 0]
      }
   }
}

itcl::body FitsFileselectionbox::_updateBoxes { data } {
   foreach b $_fileCols d $data {
           $itk_component($b) delete 0 end
      eval $itk_component($b) insert 0 $d
   }
}

itcl::body FitsFileselectionbox::changeListFITSoption {} {
     # set _displayFitsOnly $fguiPref::ifDispFitsOnly
}

itcl::body FitsFileselectionbox::urlSplit { url_ } {
    global isMac

    set uPos [string first :// $url_]
    if { $uPos == -1 } {
        return [file split $url_]
    } else {
       set lst [string range $url_ 0 [expr $uPos+2]]
       eval lappend lst [split [string range $url_ [expr $uPos+3] end] /]
       if { [lindex $lst end]=="" } {
          set lst [lrange $lst 0 [expr [llength $lst]-2]]
       }
       return $lst
    }
}

itcl::body FitsFileselectionbox::calcDateStr { time_ } {
   if { $time_=="-" || [catch {expr $time_+1}] } {
      return "-"
   }
   if { $time_ > [clock scan "00:00 today"] } {
      set str [clock format $time_ -format "Today %H:%M"]
   } elseif { $time_ > [clock scan "00:00 6 days ago"] } {
      set str [clock format $time_ -format "%a  %H:%M"]
   } else {
      set str [clock format $time_ -format "%b %d, %Y"]
   }
   return $str
}

itcl::body FitsFileselectionbox::setWatchCursor { win_ args } {
#   puts "$win $args"
   if { $win_ != "" } {
      $win_ configure -cursor watch
   }
   update

   catch {set rslt [eval $args]} err

   if { $win_ != "" } {
      $win_ configure -cursor top_left_arrow
   }

   if { [info exists rslt] } {
      return $rslt
   } else {
      error $err
   }
}

itcl::body FitsFileselectionbox::resolveSymLinks { filename_ } {

   if { [file type $filename_] == "link" } {
      set lName [file readlink $filename_]
      if { [file pathtype $lName] == "relative" } {
         set root [file dirname $filename_]
         set lName [file join $root $lName]
      }
      return [resolveSymLinks $lName]
   }
   return $filename_
}

itcl::body FitsFileselectionbox::calcSizeStr { size_ } {
   if { [catch {expr $size_+1}] } {
      return "???"
   }
   set unit 0
   set units [list "" k M G T ? ? ? ? ? ?]
   while { $size_ > 5632 } {
      incr unit
      set size_ [expr round( $size_/1024.0 )]
   }
   return $size_[lindex $units $unit]
}
