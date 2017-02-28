# First draft 05/16/96    Jianjun

#constrct a FitsHeader object
# FitsHeader FitsHeaderObjName FitsFileObjName _currentHDU


itcl::class FitsHeader {

    constructor {args} {}
    destructor  {} 

# all public methods are only called by $_fatherFitsExtension
#   when changes at the father window need to propagate to its children
#   (of which this is one)

    public method refresh {}
    public method closeCmd {}
    public method bringToFront {}
    public method setFileName { fName_ }

# creates the window is called by the constructor
    private method _checkTDISP { card }
    private method _makeHeader {}

    private method _bind_show { wndw {mode "-verbose"} }
    private method _insertAbove {} 
    private method _deleteRec {} 

    private method _userInputCommand { selection }
    private method _UndoCmd  {}
    private method _CopyCmd  {}
    private method _CutCmd   {}
    private method _PasteCmd {}
    private method _ClearCmd {}

    private method _changeKey {} 
    private method _saveAsFile {}
    private method _saveHeaderToAscii {} 
    private method _clickSelectLine {} 
    private method _selectLine {line_} 
    private method _popTheMenu {x_ y_} 
    private method _setEditStatus {}
    private method _searchStr {} 
    private method _saveFile {}
    private method _closeFrWm {w_} 
    private method _caseSel {}
    private method _moveLine {mode_} 
    private method _cleanEmptyInsert {}
    private method _dispKeys {}
    private method _updateRestDisps {}
    private method _recAction {action}
    private method _insKey {pos_ rec_} 
    private method _delKey {pos_} 
    private method _createChecksum {}
    private method _verifyChecksum {}
    private method _postMenus {}
    private method _updateHL {}
    private method _highlightValue {}
    
    private variable _fFile
    private variable _tixSubEntry ""
    
    private variable preDefineHistorySize 6

    private variable _userInputCmdStr ""
    private variable _droot
    private variable _fileName
    private variable _mBar
# _allHeader is a string separated by newline characters
    private variable _allHeader
    private variable _ablehighLightColor yellow
    private variable _disablehighLightColor tan
    private variable _selLine 0
    private variable _editStatus "append"
    private variable _keyList 
    private variable _isFailedToCopy 0
    private variable _fatherFitsExtension
    private variable _caseOption "-nocase"
    private variable _isRequired 0
    private variable _insertDone 1
    private variable _currentHDU
    private variable _actionRec {} 
    private variable _backUpRec ""

    private variable _isBeingDestroyed 0

    private variable _modifiedKey
}

# FitsHeader FitsHeaderObjName FitsFileObjName _currentHDU
itcl::body FitsHeader::constructor {args} {

    #use the FitsFile class to handle the fits files.
    set _fFile FitsExtension::[lindex $args 0]
    set _fatherFitsExtension [lindex $args 1]

    set _currentHDU     [$_fFile cget -currentHDU]
    set _isFailedToCopy [$_fFile cget -isFailedToCopy]
    set _fileName       [$_fFile getOrigName]

# Header info
    set _allHeader  [$_fFile dumpHeader]
# _allHeader is a string separated by newline characters

    $_fatherFitsExtension addChild $this header

    _makeHeader
}

itcl::body  FitsHeader::destructor {} {

    set _isBeingDestroyed 1
    destroy $_droot

    $_fatherFitsExtension freeChild $this
}


itcl::body FitsHeader::refresh {} {
    set _allHeader [eval $_fFile dumpHeader]
# rewrite keys
    _dispKeys
# clean up the highlighted keys
    _selectLine 1
}

## sets the _fileName
##   then registers the window with the filename as window title
#
itcl::body FitsHeader::setFileName { fName_ } {
   if ![winfo exists $_droot] return
   set _fileName $fName_
   set rName [urlTail $fName_]
   set dName [getFullDirPath $fName_]
   .fvwinkeeper signoff  $_droot
   .fvwinkeeper register $_droot Header $rName $_currentHDU $this
   wm title $_droot "fv: Header of $rName\[[expr $_currentHDU-1]\] in $dName"
}

## this creates the FitsHeader edit window
#
itcl::body FitsHeader::_makeHeader {} {
    global isMac isWin
    global g_titleFont
    global g_entryFont

    if { $isMac } {
	set cmdkey "Cmd"
    } else {
	set cmdkey "Alt"
    }
    
    # Display parameter
    catch  {destroy $_droot}
    
    set _droot .[namespace tail $this]
    powToplevel  $_droot .dummy
    setFileName $_fileName

    bind $_droot <Double-1> {}
    bind $_droot <Triple-1> {}
    bind $_droot <B1-Motion> {}
# bind <Button-1> for the selection
    bind $_droot <Up> [itcl::code $this _moveLine -1]
    bind $_droot <Down> [itcl::code $this _moveLine 1]
    bind $_droot <Destroy> +[itcl::code $this _closeFrWm %W]
    
    # Bind window to menu events

    bind $_droot <<SaveFile>>    [itcl::code $this _saveFile]
    bind $_droot <<SaveFileAs>>  [itcl::code $this _saveAsFile]
    bind $_droot <<Export>>      [itcl::code $this _saveHeaderToAscii]
    bind $_droot <<CloseWindow>> [itcl::code $this closeCmd]

    bind $_droot <<Undo>>        [itcl::code $this _UndoCmd]
    bind $_droot <<Copy>>        [itcl::code $this _CopyCmd]
    bind $_droot <<Cut>>         [itcl::code $this _CutCmd]
    bind $_droot <<Paste>>       [itcl::code $this _PasteCmd]
    bind $_droot <<Clear>>       [itcl::code $this _ClearCmd]
    
    bind $_droot <<Delete>>      [itcl::code $this _deleteRec]
    bind $_droot <<Insert>>      [itcl::code $this _insertAbove]
    
    bind $_droot <<PostMenus>>   [itcl::code $this _postMenus]

    bind $_droot <<CreateChecksum>>  [itcl::code $this _createChecksum]
    bind $_droot <<VerifyChecksum>>  [itcl::code $this _verifyChecksum]
    
    # Create toplevel MenuBar

    if { $isMac } {
        set _mBar .mbar.header
        set evtWndw ""
    } else {
        set _mBar $_droot.mbar
        set evtWndw $_droot
    }
    $_droot config -menu $_mBar
    
    if { ![winfo exists $_mBar] } {
        menu $_mBar -font g_titleFont
        if { $isMac } {
            $_mBar add cascade -menu $_mBar.apple
            $_mBar add cascade -menu $_mBar.file  -label File
            $_mBar add cascade -menu $_mBar.edit  -label Edit
            $_mBar add cascade -menu $_mBar.tool  -label Tools
            $_mBar add cascade -menu .mbar.wind  -label Windows
            $_mBar add cascade -menu $_mBar.help  -label Help
            buildAppleStyleMenu $_mBar.apple
        } else {
            $_mBar add cascade -menu $_mBar.file -label File
            $_mBar add cascade -menu $_mBar.edit -label Edit
            $_mBar add cascade -menu $_mBar.tool -label Tools
            $_mBar add cascade -menu $_mBar.help -label Help
        }

        # FILE

        if { $isMac } {
            buildFileMenu $_mBar.file
            $_mBar.file entryconfig "Export" -label "Export as Text..." \
                                             -state normal -font g_titleFont
            $_mBar.file entryconfig "Save As..." -state normal -font g_titleFont
            $_mBar.file entryconfig "Close"      -state normal -font g_titleFont
        } else {
            menu $_mBar.file -tearoff False
            $_mBar.file add command -label "Save" -underline 0 \
                -command "doMenuEvent <<SaveFile>>" -accelerator "$cmdkey+S" -font g_titleFont
            $_mBar.file add command -label "Export as Text..." \
                -command "doMenuEvent <<Export>>" -font g_titleFont
            $_mBar.file add command -label "Close" \
                -command "doMenuEvent <<CloseWindow>>" -accelerator "$cmdkey+W" -font g_titleFont
        }
        
        # EDIT
            
        buildEditMenu $_mBar.edit
        
        $_mBar.edit insert "Prefer*" command -label "Insert Key" \
              -command "doMenuEvent <<Insert>>" -accelerator "$cmdkey+I" -font g_titleFont
        $_mBar.edit insert "Prefer*" command -label "Delete Key" \
              -command "doMenuEvent <<Delete>>" -accelerator "$cmdkey+D" -font g_titleFont
        $_mBar.edit insert "Prefer*" separator

        # TOOLS

        menu $_mBar.tool -tearoff False
        $_mBar.tool add command -label "Create Checksum" \
            -command "doMenuEvent <<CreateChecksum>>" -font g_titleFont
        $_mBar.tool add command -label "Verify Checksum" \
            -command "doMenuEvent <<VerifyChecksum>>" -font g_titleFont

        # HELP

        buildHelpMenu $_mBar.help headerDisplay "Header Display"
        
        # Configure the Post Commands

        if { $isMac || $isWin } {
            
            # The Mac and Windows post all menus at once, so we only need
            # to do the post on the top level
            
            $_mBar configure -postcommand "doMenuEvent <<PostMenus>> $evtWndw"

        } else {
            
            # Unix has to post each individual menu

            $_mBar.file configure -postcommand \
                    "doMenuEvent <<PostMenus>> $evtWndw"
            $_mBar.edit configure -postcommand \
                    "doMenuEvent <<PostMenus>> $evtWndw"
            $_mBar.tool configure -postcommand \
                    "doMenuEvent <<PostMenus>> $evtWndw"
        }
    }
    
    frame $_droot.toolbar -relief raised -bd 1
    pack  $_droot.toolbar -side top -fill x -expand 0
    label $_droot.toolbar.lfind -text "Search for:"  -font g_titleFont
    package require Tix
    tix configure -fontset 14Point
    tixComboBox $_droot.toolbar.efind -editable true \
                                      -historylimit 20 \
                                      -selectmode browse \
                                      -variable _userInputCmdStr \
                                      -options { \
                                         listbox.height 4 \
                                         label.font g_titleFont \
                                         listbox.font g_titleFont \
                                         entry.font g_titleFont \
                                         entry.background white \
                                         entry.ipady 5 \
                                       } \
                                       -command [itcl::code $this _userInputCommand]

    set _tixSubEntry  [$_droot.toolbar.efind subwidget entry]
    bind $_tixSubEntry <Return> [itcl::code $this _searchStr]

    button $_droot.toolbar.sfind -text "Find" \
        -command [itcl::code $this _searchStr]  -font g_titleFont

    label  $_droot.toolbar.casel -text "Case sensitive?" -font g_titleFont
    button $_droot.toolbar.case -text No -command [itcl::code $this _caseSel] -font g_titleFont
    pack $_droot.toolbar.lfind -side left 
    pack $_droot.toolbar.efind -side left
    pack $_droot.toolbar.sfind -side left 
    pack $_droot.toolbar.casel -side left
    pack $_droot.toolbar.case  -side left
#
    text $_droot.text -wrap none -width 80 -relief sunken -bd 1 \
	-xscrollcommand "$_droot.xscroll set" \
	-yscrollcommand "$_droot.scroll set"  -font g_entryFont

    if { ![winfo exist .popmenu] } {
#       buildEditMenu .popmenu
        menu .popmenu 
        .popmenu add command -label "Undo"   -command "doMenuEvent <<Undo>>"
        .popmenu add command -label "Insert" -command "doMenuEvent <<Insert>>"
        .popmenu add command -label "Delete" -command "doMenuEvent <<Delete>>"
    }

    bind $_droot.text <1> +[itcl::code $this _clickSelectLine]

# bin <Button-3> for the popup menu
    bind $_droot.text <3> [itcl::code $this _popTheMenu %X %Y]    
    bind $_droot.text <Control-ButtonPress-1> [itcl::code $this _popTheMenu %X %Y]    

    scrollbar $_droot.scroll -command "$_droot.text yview" 
    scrollbar $_droot.xscroll -orient horizontal -command "$_droot.text xview" 
    
    entry $_droot.text2 -width 80   -relief sunken -bd 1 -font g_entryFont \
	-textvariable [itcl::scope _modifiedKey]
    pack $_droot.text2  -side bottom -fill y -anchor w
    bind $_droot.text2 <Return> [itcl::code $this _changeKey]
    bind $_droot.text2 <<Clear>> "[itcl::code $this _ClearCmd];break"
    
    pack $_droot.scroll -side right -fill y -expand 0     
    pack $_droot.xscroll -side bottom -fill x -expand 0     
    pack $_droot.text  -side left -fill both -expand 1

    _dispKeys

}

itcl::body FitsHeader::_userInputCommand { selection } {
     set _userInputCmdStr $selection
}

itcl::body FitsHeader::_bind_show { wndw {mode "-verbose"} } {
     puts $wndw
     foreach tag [bindtags $wndw] {
         puts "\t$tag"
         foreach spec [bind $tag] {
             puts "\t\t$spec"
             if { $mode == "-verbose" } {
                set cmd [bind $tag $spec]
                set cmd [string trim $cmd "\n"]
                regsub -all "\n" $cmd "\n\t\t\t" cmd
                puts "\t\t\t$cmd"
             }
         }
     }
}


itcl::body FitsHeader::_postMenus {} {
   global isMac
   
   _setEditStatus
   if { [$_fFile isFileChanged] && ![$_fFile isReadOnly] } {
      $_mBar.file entryconfigure Save -state normal
   } else {
      $_mBar.file entryconfigure Save -state disabled
   }
   update idle
}


itcl::body FitsHeader::bringToFront {} {
   if { ![winfo ismapped $_droot] } {
       wm deiconify $_droot
   }
   raise $_droot
   focus $_droot
}


itcl::body FitsHeader::_dispKeys {} {
    $_droot.text configure -state normal
    $_droot.text delete 1.0 end 
    $_droot.text insert 0.0 $_allHeader
    set tmpNumKwds [$_droot.text index end]
    $_droot.text insert end "END"
    set _keyList {}
# there are two blank space needs to be  removed. 
    for {set i 1} { $i< [expr $tmpNumKwds-1]} {incr i} {
	lappend _keyList [$_droot.text get $i.0 $i.8]
    }
#    $_droot.text configure -state disabled
    $_droot.text configure -state disabled
# set up a mark for search
    $_droot.text mark set smark 1.0    
}

itcl::body FitsHeader::_caseSel {} {
    if { [$_droot.toolbar.case cget  -text] == "Yes"} {
	$_droot.toolbar.case configure -text No
	set _caseOption "-nocase"
    } else {
	$_droot.toolbar.case configure -text Yes
	set _caseOption ""

    }
}

itcl::body FitsHeader::_popTheMenu {x_ y_} {
    _setEditStatus
    tk_popup .popmenu $x_ $y_ 
}

itcl::body FitsHeader::_setEditStatus {} {

    #        _selLine    isFailed  _actionRec   clipboard
    # Undo                           Y
    # Copy     Y
    # Cut      Y           N
    # Paste    Y           N                      Y
    # Clear    Y           N
    # Insert   Y           N
    # Delete   Y           N

    if { [llength $_actionRec] == 0 } {
	$_mBar.edit entryconfigure Undo  -state disabled
	.popmenu entryconfigure Undo -state disabled
    } else { 
	$_mBar.edit entryconfigure Undo  -state normal
	.popmenu entryconfigure Undo -state normal
    }

    if { $_selLine == 0 } {
	$_mBar.edit entryconfigure "Copy"  -state disabled
    } else {
	$_mBar.edit entryconfigure "Copy"  -state normal
    }
    
    if { $_selLine != 0 && $_isFailedToCopy == 0 } {
	.popmenu entryconfigure Insert -state normal
	.popmenu entryconfigure Delete -state normal
	$_mBar.edit entryconfigure "Insert*"  -state normal
	$_mBar.edit entryconfigure "Delete*"  -state normal
	$_mBar.edit entryconfigure "Cut*"     -state normal
	$_mBar.edit entryconfigure "Clear"    -state normal
	if { [lindex [fvClipBoard report] 0] == "keyword" } {
	    $_mBar.edit entryconfigure "Paste"  -state normal
	} else {
	    $_mBar.edit entryconfigure "Paste"  -state disabled
	}
	
    } else {
	.popmenu entryconfigure Delete -state disabled 
	.popmenu entryconfigure Insert -state disabled  
	$_mBar.edit entryconfigure "Delete*" -state disabled
	$_mBar.edit entryconfigure "Insert*" -state disabled
	$_mBar.edit entryconfigure "Cut*"    -state disabled
	$_mBar.edit entryconfigure "Clear"   -state disabled
	$_mBar.edit entryconfigure "Paste"   -state disabled
    }
}



itcl::body FitsHeader::_searchStr {} {
    global env

# get the string from entry and find out the line and column number
    set fStr [$_tixSubEntry get]

    if { $fStr == ""} {
       return
    }

    $_droot.toolbar.efind addhistory $fStr

    set fPos [eval $_droot.text search $_caseOption -forward {$fStr} smark ]
    if { $fPos == ""} {
       tk_messageBox -icon warning -type ok -message "End of Search"
       return
    }

    scan $fPos "%d.%d" curLine curCol

    _selectLine $curLine
    _highlightValue

# if doesn't exist, beep; else highlight
    if { $fPos == "" } {
        bell
    } else {
	$_droot.text mark set smark $curLine.[expr $curCol+2]
	$_droot.text see smark
	$_droot.text tag delete foundStr
	$_droot.text tag add foundStr ${curLine}.${curCol} \
	    ${curLine}.[expr $curCol+[string length $fStr]]
	$_droot.text tag configure foundStr -background grey -borderwidth 2 \
	    -relief raised
    }

    focus $_tixSubEntry
}

itcl::body FitsHeader::_insertAbove {} {
# delete the previous imcomplete insertion
    _cleanEmptyInsert

    if { $_isRequired > 1} return

    $_droot.text configure -state normal
    $_droot.text insert $_selLine.0 "                                                                                \n"
    $_droot.text configure -state disabled
    _selectLine $_selLine
    focus $_droot.text2
# reset parameters
    set _modifiedKey ""
    set _insertDone 0
    set _editStatus "insert"
}

itcl::body FitsHeader::_clickSelectLine {} {
    _cleanEmptyInsert

    set pos [$_droot.text index current]
    set line [lindex [split $pos .] 0]
    _selectLine $line
    _highlightValue
}    
     
itcl::body FitsHeader::_cleanEmptyInsert {} {
# if one is inserting a new key, and he moves the highlight. Then the inserting 
# is canceled, clean the blank line
    if { ($_insertDone != 0) || ($_editStatus != "insert")} {
	return
    } 

    $_droot.text configure -state normal
    $_droot.text delete $_selLine.0 [expr 1+$_selLine].0
    $_droot.text configure -state disabled
    set _insertDone 1
}
   
itcl::body FitsHeader::_selectLine {line_} {

    set _selLine $line_
    $_droot.text mark set smark $line_.0
    $_droot.text see $line_.0
    set key [$_droot.text get $line_.0 $line_.80]
    set _backUpRec $key
    set keyRoot [string range $key 0 4]
    set keyName [string range $key 0 7]
    if { $keyName == "" } {
	$_droot.text configure -state normal
	$_droot.text insert $line_.0  "       "
	$_droot.text configure -state disabled
    }
    $_droot.text tag delete selectedLine
    $_droot.text tag add selectedLine $line_.0 $line_.80
    
#    $_droot.text2 delete 0 end 
    if { $fvPref::ifProtectedKeys && (
         ($keyName == "SIMPLE  ") ||
	 ($keyName == "BITPIX  ") ||
	 ($keyRoot == "NAXIS"   ) ||
	 ($keyName == "PCOUNT  ") ||
         ($keyName == "TFIELDS ") ||
	 ($keyName == "GCOUNT  ") 
                                  ) } {
	$_droot.text tag configure selectedLine \
	    -background $_disablehighLightColor \
	    -borderwidth 2 -relief raised
	set tmpRec [$_fFile getNthKey $line_]
	set _modifiedKey [lindex $tmpRec 2]
	set _isRequired 2
	
    } elseif { 	 $fvPref::ifProtectedKeys && (
		 ($keyName == "XTENSION") ||
		 ($keyRoot == "TBCOL"   ) ||
		 ($keyRoot == "THEAP"   ) ||
		 ($keyRoot == "TFORM"   )
                                          ) } {
	$_droot.text tag configure selectedLine \
	    -background $_disablehighLightColor \
	    -borderwidth 2 -relief raised
	set tmpRec [$_fFile getNthKey $line_]
	set _modifiedKey [lindex $tmpRec 2]
	set _isRequired 1
    } elseif { $keyName == "END" } {
	set _isRequired 0
	$_droot.text tag configure selectedLine \
	    -background $_disablehighLightColor \
	    -borderwidth 2 -relief raised	
	set _modifiedKey ""
    } else {
	$_droot.text tag configure selectedLine \
	    -background $_ablehighLightColor \
	    -borderwidth 2 -relief raised
	set _modifiedKey $key
	set _isRequired 0
    }
    focus $_droot.text2
}


itcl::body FitsHeader::_changeKey {} {

    set newRec $_modifiedKey

    if { ($newRec == "") && ($_editStatus  != "insert") } {
	return
    } 
    
    if { $_isFailedToCopy == 0 } {
	if { $_editStatus == "insert"} {
            # Pan Chai -
            # Add check to see if TDISPn has correct value

            if { [string range $newRec 0 4] == "TDISP" } {
               set result [ _checkTDISP $newRec ]

               if { $result == "FAILED" } {
	          error "illegal TDISP value\nExamples: 'I6', 'F10.6', 'E12.4', 'D20.14'\nCode letter must be uppercase"
               }
            }

	    _insKey $_selLine $newRec
	    _recAction insert
	} else {
	    if {$_isRequired >= 1} {
		set oldCard [$_fFile getNthKey $_selLine]
		if { $fvPref::ifAutoReformatKeys == 1} {
		    set newRec "[lindex $oldCard 0] [lindex $oldCard 1] $newRec"
		} else {
		    if { $newRec == "" } {
			set newRec { }
		    }
		    set newRec [format "%-8s= %s / %s" [lindex $oldCard 0] \
				    [lindex $oldCard 1] $newRec]
		}
	    } 
	    set card [$_fFile putNthKey $_selLine $newRec \
                  $fvPref::ifAutoReformatKeys]
	    set keyName [string range $card 0 7]

            # Pan Chai -
            # Add check to see if TDISPn has correct value

            if { [string range $keyName 0 4] == "TDISP" } {
               set result [ _checkTDISP $card ]

               if { $result == "FAILED" } {
	          error "illegal TDISP value\nExamples: 'I6', 'F10.6', 'E12.4', 'D20.14'\nCode letter must be uppercase"
               }
            }

	    set tmpPos [expr 1+ [lsearch $_keyList $keyName]]
	    
	    if { ($tmpPos == 0) && ($_selLine == 0)} {
		set ke_yPos [$_droot.text index end]
		scan $ke_yPos "%d.%d" tmpLine tmpCol
		$_droot.text configure -state normal
		$_droot.text insert [expr $tmpLine-1].0 "$card\n"
		$_droot.text configure -state disabled
		lappend _keyList $keyName
		_recAction append
		set _selLine $tmpLine
	    } else {	
		$_droot.text configure -state normal
		$_droot.text delete $_selLine.0 $_selLine.end
		$_droot.text insert $_selLine.0 $card
		$_droot.text configure -state disabled
		_recAction change
	    }
	    set _editStatus "append"
	}
	$_fFile changeFile
	_updateRestDisps
    }
    _selectLine $_selLine
}

itcl::body FitsHeader::_checkTDISP { card } {
     set valueStr [string range $card [expr [string first "'" $card] + 1] end]
     set valueStr [string range $valueStr 0 [expr [string first "'" $valueStr] - 1] ]

     set valueStr [string trim $valueStr]
     if { [string length $valueStr] == 0 } {
        return IGNORED
     }

     # check No.1 the TDIM keyword is a string 
     if { [string wordend $valueStr 0] != [string length $valueStr] } {
        # then we find out the character in the breaking point..
        set idx [string wordend $valueStr 0]
        # if the character in the breaking point is not a "." as like XX.X
        # then the test failed.
        if {[string range $valueStr $idx $idx] != "." } {
           return FAILED
        }
     }

     # check No.2 the TDIM keyword starts with A, L, I, B, O, Z, E, EN, ES, G or D 
     set firstChar [string range $valueStr 0 0]

     if { $firstChar == "A" || $firstChar == "L" || $firstChar == "I" ||
          $firstChar == "B" || $firstChar == "O" || $firstChar == "Z" ||
          $firstChar == "F" ||
          $firstChar == "E" || $firstChar == "G" || $firstChar == "D" } {

          set idx 1
          set twoChars [string range $valueStr 0 1]

          if { $twoChars == "EN" || $twoChars == "ES" } {
             set idx 2
          }

          set numChar [string range $valueStr $idx $idx]

          # check No.3 follow by a decimal digit 
          if { $numChar >= "0" && $numChar <= "9" } {
             return SUCCESS
          }
     }

     # illegal TDISP keyword
     return FAILED
}

itcl::body FitsHeader::_deleteRec {} {
    if { $_isRequired >= 1} return
    _delKey $_selLine 
# record the action
    _recAction delete
    $_fFile changeFile
# highlight the next line
    _selectLine $_selLine
    _highlightValue
    _updateRestDisps
}

itcl::body FitsHeader::_saveHeaderToAscii  {} {

   set t [urlTail $_fileName]
   set r [file root $t]
   set e [file ext $t]
   if { [lsearch -exact [list .gz .Z .z] $e] != -1 } {
      set e [file ext $r]
      set r [file root $r]
   }
   set sugName "${r}_h[expr $_currentHDU-1].txt"

# get the file name from the entry
    set asciiFileName [getSelectedFileName $sugName]
    if { $asciiFileName == "" } return
    if { [file exist $asciiFileName] == 1 } {
	set feedback [promptMsg "File $asciiFileName exist\n overwrite?" \
			  return Yes No]
	if { $feedback == "CANCEL"} return
    }
# open the file as write 
    set f [open $asciiFileName w] 
# dump all the headers to the file    
    puts $f [string trimright [$_fFile dumpHeader]]
    puts $f "END"
# flush it
    flush $f
# close it
    close $f
# destroy it    
}

itcl::body FitsHeader::_saveAsFile { } {
    $_fFile saveAs
}

itcl::body FitsHeader::_saveFile {} {
    $_fFile save
    _updateHL
}

itcl::body FitsHeader::_updateHL {} {
    if { [lsearch $_keyList "EXTNAME "]<0 } {
       # $_fatherFitsExtension updateHL EXTNAME NoName
    } else {
       set extVal [lindex [lindex [$_fFile getKeyword EXTNAME] 0] 1]
       $_fatherFitsExtension updateHL EXTNAME [string trim $extVal {' }]
    }
}

itcl::body FitsHeader::_closeFrWm {w_} {
    if { $w_ == $_droot && !$_isBeingDestroyed } {
	closeCmd
    }
}

itcl::body FitsHeader::closeCmd {} {

    .fvwinkeeper signoff $_droot
    _updateHL
    itcl::delete object $this
}


itcl::body FitsHeader::_moveLine {mode_} {
    if { $_selLine == 0 } return 
    
    _cleanEmptyInsert

    if { $mode_ == 1} {
       incr _selLine 
	set tmp  [expr 1+[llength $_keyList]]
	if {$_selLine > $tmp } {
	    set _selLine $tmp
	}
    } elseif {$mode_ == -1 } {
	incr _selLine -1
	if { $_selLine < 1 } {
	    set _selLine 1
	}
    } else {
	error "Unknown move mode (should be 1/-1)"
    }

    _selectLine $_selLine
    _highlightValue
}


itcl::body FitsHeader::_updateRestDisps {} {
   $_fatherFitsExtension updateDisps $this
}

itcl::body FitsHeader::_recAction {action_} {
# store actions and associated info
    if { $action_ == "append" } {
	lappend _actionRec [list $action_ [llength $_keyList] $_backUpRec]
    } else {
	lappend _actionRec [list $action_ $_selLine $_backUpRec]
    }
}

#####################
#
#  Edit Menu Bindings
#
#####################

itcl::body FitsHeader::_UndoCmd {} {

    set lastActionRec  [lindex $_actionRec end]

    if { $lastActionRec == "" } {
	return
    }
    set _actionRec [lreplace $_actionRec end end]

    set lastAction    [lindex $lastActionRec 0]
    set lastPosition  [lindex $lastActionRec 1]
    set lastRecord    [lindex $lastActionRec 2]
    switch $lastAction {
	insert {
	    _delKey $lastPosition
	}
	delete {
	    $_droot.text configure -state normal
	    $_droot.text insert $lastPosition.0 "\n"
	    $_droot.text configure -state disabled
	    _insKey $lastPosition $lastRecord
	}
	append {
	    _delKey $lastPosition
	}
	change {
	    _delKey $lastPosition
	    $_droot.text configure -state normal
	    $_droot.text insert $lastPosition.0 "\n"
	    $_droot.text configure -state disabled
	    _insKey $lastPosition $lastRecord
	}
	default {
	    error "No such action recorded"
	}
    }
    _selectLine $lastPosition
    _highlightValue
    _updateRestDisps
}

itcl::body FitsHeader::_CopyCmd {} {
    set key [$_droot.text get $_selLine.0 $_selLine.80]
    set keyName [string trimright [lindex $key 0] =]
    if { $keyName == "HIERARCH" } {
	set keyName [string trimright [lindex $key 1] =]
    }
    fvClipBoard register keyword $_fileName $keyName $key
}

itcl::body FitsHeader::_CutCmd {} {
    if {$_isRequired >= 1} return

    _CopyCmd
    _deleteRec
}

itcl::body FitsHeader::_PasteCmd {} {
    if {$_isRequired >= 1} return

    set clipRec [fvClipBoard report]
    if { [lindex $clipRec 0] != "keyword" } return
    _insertAbove
    set _modifiedKey [lindex $clipRec 2]
    _changeKey
}

itcl::body FitsHeader::_ClearCmd {} {
    if {$_isRequired >= 1} return

    _deleteRec
    _insertAbove
}


itcl::body FitsHeader::_insKey {pos_ rec_} {

    set card [$_fFile insertKey $pos_ $rec_ $fvPref::ifAutoReformatKeys]
    set newKey [string range $card 0 7]
    $_droot.text configure -state normal
    $_droot.text delete $pos_.0 $pos_.end
    $_droot.text insert $pos_.0 $card
    $_droot.text configure -state disabled
    set _keyList [linsert $_keyList $pos_ $newKey]
    set _editStatus "append"
    set _insertDone 1
}


itcl::body FitsHeader::_delKey {pos_} {
    $_droot.text configure -state normal
    $_droot.text delete $pos_.0 [expr 1+$pos_].0
    $_droot.text tag delete selectedLine
    $_droot.text configure -state disabled
# need to delete the rec from the file
    $_fFile delNthKey $pos_
#
    set _modifiedKey ""
    set tmp [expr $pos_ - 1]
    set _keyList [lreplace $_keyList $tmp $tmp] 
}

itcl::body FitsHeader::_createChecksum {} {
   $_fFile updateChecksum 1
   refresh
}

itcl::body FitsHeader::_verifyChecksum {} {
    set checksumStatus  [$_fFile verifyChecksum]
    switch -- $checksumStatus {
	1 {
	   tk_messageBox -icon info \
		 -message "The CHECKSUM keyword is valid"
	}
	0 {
	   set res [tk_messageBox -icon warning -message \
		 "The CHECKSUM keyword does not exist in this HDU.\n\
		 \nCreate keywords?" -type yesno -default "no"]
	   if { $res == "yes" } {
	      $_fFile updateChecksum 1
	      refresh
	   }
	}
	-1 {
	   set res [tk_messageBox -icon error \
		 -message "The CHECKSUM keyword is invalid!\n\
		 \nUpdate keywords?" -type yesno -default "no"]
	   if { $res == "yes" } {
	      $_fFile updateChecksum 1
	      refresh
	   }
	}
	default { error "Unknown error in checksum varification"}
    }

}

itcl::body FitsHeader::_highlightValue {} {

   set root [string range $_modifiedKey 0 7]
   $_droot.text2 selection clear
   if { $_isRequired } {

      # Only the comment is present

      $_droot.text2 selection range 0 end
      $_droot.text2 icursor end

   } elseif { $root == "COMMENT " || $root == "HISTORY " } {

      # Highlight entire field except keyword

      set value [string trimleft [string range $_modifiedKey 8 end] { }]
      set idx [string first $value $_modifiedKey]
      $_droot.text2 selection range $idx end
      $_droot.text2 icursor end

   } else {

      # Locate value to hightlight

      set k [$_fFile getNthKey $_selLine]
      set val [lindex $k 1]
      if { [string range $val 0 0] == "'" && [string range $val end end] == "'" } {
         set last [string length $val]
         set val [string range $val 1 [expr $last-2]]
      }
      set start [string first = $_modifiedKey]
      if { $start == -1 } {
         set start 9
      }
      set idx [string first $val $_modifiedKey $start]
      if { $idx == -1 } {
         $_droot.text2 icursor end
      } else {
         set last [expr $idx + [string length $val]]
         $_droot.text2 selection range $idx $last
         $_droot.text2 icursor $last
      }
   }
}
