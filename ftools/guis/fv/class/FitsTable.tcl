# First draft 05/01/96    Jianjun
# 01/01/2004 Ziqin add row selection function

#constrct a FitsTable object
# FitsTable FitsTableObjName FitsFileObjName currentHDU 

itcl::class FitsTable {
    inherit Table

    constructor {args} {}
    destructor         {}

### the following override public methods in Table parent class
    public method setFileName { fName_ }

### the following does not exist in the Table parent class
    public method dispTable   { extNum_ {cols_ {}} }

    #-------------------------------

### the following override protected methods in Table class
    private method _drawTable {}
    private method _buildNewMenus {}
    private method _postMenus {}
    private method _updateHL {}
    private method _updateNumCols {}

    private method _writeTableData {col_ row_ val_}
    private method _readTableData {fCol_ fRow_ nCols_ nRows_}
    private method _getFormattedData {col_ row_}
    private method _getRawData {col_ row_}
    private method _getRawDataBlock { fCol_ fRow_ lCol_ lRow_ }
    private method _putRawDataBlock { fCol_ fRow_ data_ }
    private method _putRawData {col_ row_ val_}

#    private method _createTable {}

### the following methods and variables do not exist in the parent
###  in fact, these names are procs are unique to this class
    protected method _editColParamsWindow { { colNum0_ 0 } }
    private method _tableHighLight {}
    private method _selAllCol {}
    private method _unselAllCol {}
    private method _makeColSelList {}
    private method _colInfoSelCmd {}
    private method _changeTDisp {}  
    private method _checkTDisp {tdisp_}  

# stores display data of the "Edit Column Parameters" window
    private variable _editColWin_name
    private variable _editColWin_tdisp
    private variable _editColWin_unit
    private variable _editColWin_newname
    private variable _editColWin_type
    private variable _editColWin_comment

    private variable _colSelState
    private variable _isTableDisplayed 0
}


itcl::body FitsTable::constructor {args} {

    global g_titleFont
    option add *FitsTable.font g_titleFont

    set fFile [lindex $args 0]
    set fFile FitsExtension::$fFile
    set _fatherFitsExtension [lindex $args 1]

    set currentHDU           [$fFile cget -currentHDU]
    set isFailedToCopy [$fFile cget -isFailedToCopy]
    set fileName       [$fFile getOrigName]

# Table info        
    set _tableType [$fFile getTableInfo hdutype]
    
    if { ($_tableType != "Binary Table") \
	     && ($_tableType != "ASCII Table") } {
	puts "It is not a table \n"
	return 
    }

    
    set _numCols  [$fFile getTableInfo ncols]       
    set _numRows  [$fFile getTableInfo nrows]
    set _listColNames [$fFile getTableInfo column]
    set _listPreSelectedColNames $_listColNames


    $_fatherFitsExtension addChild $this table
}

itcl::body FitsTable::destructor {} {

    set _isBeingDestroyed 1
    if { [winfo exists $_droot] } {
       destroy $_droot
    }

    $_fatherFitsExtension freeChild $this
}

#body FitsTable::_createTable {} {
#     puts "I am going to create a new Table"
#}


itcl::body FitsTable::setFileName { fName_ } {

   Table::setFileName $fName_
   .fvwinkeeper signoff  $_droot
   .fvwinkeeper register $_droot "Table" [urlTail $fName_] $currentHDU \
         $this
   if { [llength $_myChildren] } {
      foreach child $_myChildren {
         # Not all children contain setFileName
         catch {$child setFileName $fName_}
      }
   }
}


itcl::body FitsTable::_drawTable {} {

   Table::_drawTable

#   bind $_droot <<DeleteRows>>  [itcl::code $this _selRowsFrExpr 1]
   bind $_droot <<DeleteRows>>  [itcl::code $this _tryDelRows]
   bind $_droot <<DeleteCols>>  [itcl::code $this _tryDelCols]
   bind $_droot <<InsertRows>>  [itcl::code $this _addRows]
   bind $_droot <<InsertCols>>  [itcl::code $this _addCols]
   bind $_droot <<SelectRows>>  [itcl::code $this _selRowsFrExpr 0]
   bind $_droot <<DispFormat>>  [itcl::code $this _editColParamsWindow]

   bind $_droot <<Plot>>        [itcl::code $this _plotCmd]
   bind $_droot <<Sort>>        [itcl::code $this _sortCmd]
   bind $_droot <<Calc>>        [itcl::code $this _calculateCmd]
   bind $_droot <<Histogram>>   [itcl::code $this _histoCmd]
   bind $_droot <<Statistics>>  [itcl::code $this _statCmd]
}


itcl::body FitsTable::_buildNewMenus {} {
   global g_titleFont

   Table::_buildNewMenus

   # Add some items to the Edit menu

   #    Insert

   $_mBar.edit insert "Prefer*" cascade -label "Insert" -menu $_mBar.edit.insert -font g_titleFont
   menu $_mBar.edit.insert -tearoff false
   $_mBar.edit.insert add command -label "Row" \
         -command "doMenuEvent <<InsertRows>>" -font g_titleFont
   $_mBar.edit.insert add command -label "Column" \
         -command "doMenuEvent <<InsertCols>>" -font g_titleFont

   #    Delete

   $_mBar.edit insert "Prefer*" cascade -label "Delete" -menu $_mBar.edit.delete -font g_titleFont
   menu $_mBar.edit.delete -tearoff false
   $_mBar.edit.delete add command -label "Selected rows" \
         -command "doMenuEvent <<DeleteRows>>" -font g_titleFont
   $_mBar.edit.delete add command -label "Selected columns" \
         -command "doMenuEvent <<DeleteCols>>" -font g_titleFont

#Ziqin pan

   
   $_mBar.edit insert "Prefer*" command  -label "Select Rows From Expr" \
        -command  "doMenuEvent <<SelectRows>>" -font g_titleFont
   

   #    Display Format

   $_mBar.edit insert "Prefer*" command -label "Column Parameters" \
         -command "doMenuEvent <<DispFormat>>" -font g_titleFont

   $_mBar.edit insert "Prefer*" separator


   # Fill in the TOOLS menu

   $_mBar.tools add command -label "Plot..." \
         -command "doMenuEvent <<Plot>>" -font g_titleFont
   $_mBar.tools add command -label "Sort Rows..." \
         -command "doMenuEvent <<Sort>>" -font g_titleFont
   $_mBar.tools add command -label "Calculator..." \
         -command "doMenuEvent <<Calc>>" -font g_titleFont
   $_mBar.tools add command -label "Histogram..." \
         -command "doMenuEvent <<Histogram>>" -font g_titleFont
   $_mBar.tools add command -label "Statistics..." \
         -command "doMenuEvent <<Statistics>>" -font g_titleFont
}


itcl::body FitsTable::_postMenus {} {

   Table::_postMenus

   set _anyColSelected [sarray _colNotchedState 0 [expr $_dispCols-1] 1]
   if { $_anyColSelected } {
      $_mBar.edit.delete entryconfigure "*column*" -state normal
   } else {
      $_mBar.edit.delete entryconfigure "*column*" -state disabled
   }

   set _anyRowsSelected 0
   for {set i 1} { $i <= $_numRows } { incr i } {
        if {$_selectedRows($i) == 1} {
             set _anyRowsSelected 1
             break
        }
   }
   if { $_anyRowsSelected == 1} {
      $_mBar.edit.delete entryconfigure "*rows*" -state normal
   } else {
      $_mBar.edit.delete entryconfigure "*rows*" -state disabled
   }

   if { $isFailedToCopy } {
       $_mBar.tools entryconfigure "Sort Rows..." -state disabled
       $_mBar.tools entryconfigure "Calculator..." -state disabled
   } else {
       $_mBar.tools entryconfigure "Sort Rows..." -state normal
       $_mBar.tools entryconfigure "Calculator..." -state normal
   }
   update idle
}

itcl::body FitsTable::_updateNumCols {} {
   set _numCols  [$fFile getTableInfo ncols]
   set _listColNames [$fFile getTableInfo column]
}

itcl::body FitsTable::_editColParamsWindow { { colNum0_ 0 } } {
    global g_titleFont

    set fmtWin .fmtInfo
    catch {destroy $fmtWin}
    powToplevel $fmtWin .dummy
    wm title $fmtWin "fv: Edit Column Parameters"
    wm geometry $fmtWin +[winfo pointerx .]+[winfo pointery .]

    # start out displaying first (0th) column data
    set _editColWin_name    $_columnName($colNum0_)
    set _editColWin_tdisp   $_columnTDisp($colNum0_)
    set _editColWin_unit    $_columnUnit($colNum0_)
    set _editColWin_newname $_columnName($colNum0_)
    set _editColWin_type    $_columnType($colNum0_)
    set _editColWin_comment $_columnComment($colNum0_)

    iwidgets::combobox   $fmtWin.name  -labeltext "Column"  \
          -labelpos w \
          -labelfont g_titleFont \
          -textfont g_titleFont \
          -selectioncommand [itcl::code $this _colInfoSelCmd] \
          -textvariable [itcl::scope _editColWin_name]  -editable 0 
    eval $fmtWin.name insert list end $_listPreSelectedColNames

    iwidgets::entryfield $fmtWin.w -labeltext "Display format" \
          -labelpos w \
          -width 10 \
          -labelfont g_titleFont \
          -textfont g_titleFont \
          -textvariable [itcl::scope _editColWin_tdisp] \
          -command [itcl::code $this _changeTDisp]
    iwidgets::entryfield $fmtWin.w2 -labeltext "Column Units" \
          -labelpos w \
          -width 10 \
          -labelfont g_titleFont \
          -textfont g_titleFont \
          -textvariable [itcl::scope _editColWin_unit] \
          -command [itcl::code $this _changeTDisp]
    iwidgets::entryfield $fmtWin.w3 -labeltext "New Name" \
          -labelpos w \
          -width 10 \
          -labelfont g_titleFont \
          -textfont g_titleFont \
          -textvariable [itcl::scope _editColWin_newname] \
          -command [itcl::code $this _changeTDisp]
    iwidgets::entryfield $fmtWin.w4 -labeltext "Comments" \
          -labelpos w \
          -width 10 \
          -labelfont g_titleFont \
          -textfont g_titleFont \
          -textvariable [itcl::scope _editColWin_comment] \
          -command [itcl::code $this _changeTDisp]
    _colInfoSelCmd 

    grid $fmtWin.name -row 1 -column 1 -sticky ew
    grid $fmtWin.w    -row 2 -column 1 -sticky ew
    grid $fmtWin.w2   -row 3 -column 1 -sticky ew
    grid $fmtWin.w3   -row 4 -column 1 -sticky ew
    grid $fmtWin.w4   -row 5 -column 1 -sticky ew

    iwidgets::buttonbox $fmtWin.bbox 
    $fmtWin.bbox add apply -text Apply -command [itcl::code $this _changeTDisp] -font g_titleFont
    $fmtWin.bbox add help   -text Help -command "hhelp displayFormat" -font g_titleFont
    $fmtWin.bbox add cancel -text Close -command "destroy $fmtWin" -font g_titleFont

    grid $fmtWin.bbox  -row 6 -column 1

    iwidgets::Labeledwidget::alignlabels $fmtWin.name $fmtWin.w $fmtWin.w2 $fmtWin.w3 $fmtWin.w4
}


itcl::body FitsTable::_colInfoSelCmd {} {
## NOTE
# if something changes, make sure to save, else do nothing

    # from the chosen name, get the position and rest of data
    set i [lsearch $_listPreSelectedColNames $_editColWin_name]

    set _editColWin_newname $_editColWin_name
    set _editColWin_unit $_columnUnit($i)
    set _editColWin_type $_columnType($i)
    set _editColWin_tdisp [string trim $_columnTDisp($i) ']
    set _editColWin_type  [string trim $_columnType($i)  ']
    set _editColWin_comment [string trim $_columnComment($i)  ']

    _checkTDisp $_editColWin_tdisp 
}

itcl::body FitsTable::_changeTDisp {} {
    regsub -all " " $_editColWin_tdisp "" [itcl::scope _editColWin_tdisp]
    set tmpTdisp [_checkTDisp $_editColWin_tdisp ]
    if { $tmpTdisp  == "NO" } {
        error "illegal TDISP value\nExamples: 'I6', 'F10.6', 'E12.4', 'D20.14'\nCode letter must be uppercase"
	return
    } else {
	set _editColWin_tdisp $tmpTdisp
    }
    set _editColWin_newname [string trim $_editColWin_newname " "]
    if { $_editColWin_newname == "" } {
	error "New column name cannot be a blank string"
	return 
    }

    set tmpPos [lsearch -exact $_listColNames $_editColWin_name]
    set tmpPreSelectedPos [lsearch -exact $_listPreSelectedColNames $_editColWin_name]
    if { $tmpPos == -1} {
	error "No such column."
	return
    } else {
	# tmpPos is 0-based, n is 1-based (used for keywords)
	set n [expr $tmpPos+1]
	if { $tmpTdisp == "" } {
	    # if the key doesn't exist, we get an error; catch this error
	    catch { $fFile delNthKey  "TDISP$n" }
	} else {
	    $fFile putKwd "TDISP$n '$_editColWin_tdisp'" 1 
	}
	# change unit
	if { $_editColWin_unit != $_columnUnit($tmpPreSelectedPos) } {
	    $fFile putKwd "TUNIT$n '$_editColWin_unit'" 1 
	}

	if { $_editColWin_comment != $_columnComment($tmpPreSelectedPos) } {
	   $fFile putKwd "TTYPE$n '$_editColWin_newname' / $_editColWin_comment" 1 
	}

	# change name
	if { $_editColWin_newname != $_editColWin_name && \
		$_editColWin_newname != "" } {
	    $fFile putKwd "TTYPE$n '$_editColWin_newname' / $_editColWin_comment" 1 
	    # table refresh calls Table::_readInTable which requires
	    # correct _listPreSelectedColNames
	    set _listPreSelectedColNames [lreplace $_listPreSelectedColNames \
		    $tmpPreSelectedPos $tmpPreSelectedPos $_editColWin_newname]
	}
     }
     $fFile changeFile
# refresh 
     refresh
     _updateRestDisps
     _drawTableFrame
}

itcl::body FitsTable::_checkTDisp {tdisp} {

# illegal TDISP: w - width, m - minimum number of digits, d - number of didgets,
# e - number of digits in the exp.   
# Aw
# Lw
# Iw.m
# Bw.m (binary integers only)
# Ow.m (Octal integers only)
# Zw.m (Hexadecimal integer)
# Fw.d
# Ew.dEe    
# ENw.d
# ESw.d
# Gw.dEe
# Dw.dEe
# also allow lower case starting char

    if {($tdisp == "") || ($tdisp == " ")} return ""

    set typeIdx  [string range $tdisp 0 0]
    set type $typeIdx
    set tmp1 [string range $tdisp 0 1]
    switch $typeIdx {
	a -
	A { 
	    if { [regexp {[Aa]} $_editColWin_type] == 0 } {
		error "Cannot format column to character type"
		return
	    }
	    scan $tdisp "$typeIdx%d" w 
	}
	l -
	L {
	    if { [regexp {[Ll]} $_editColWin_type] == 0 } {
		error "Cannot format column to logical type"
		return
	    }
	    scan $tdisp "$typeIdx%d" w 
	}
	i -
	I {
	    if { [regexp {[IJBijb]} $_editColWin_type] == 0 } {
		error "Cannot format column to integer type"
		return
	    }
	    scan $tdisp "$typeIdx%d.%d" w m 
	}
	b -
	B {
	    if { [regexp {[IJBijb]} $_editColWin_type] == 0 } {
		error "Cannot format column to byte type"
		return
	    } 
	    scan $tdisp "$typeIdx%d.%d" w m 
	}
	o -
	O {
	    if { [regexp {[IJBijb]} $_editColWin_type] == 0 } {
		error "Cannot format column to octal integer type"
		return
	    } 
	    scan $tdisp "$typeIdx%d.%d" w m 
	}
	z -
	Z {
	    if { [regexp {[IJBijb]} $_editColWin_type] == 0 } {
		error "Cannot format column to hexadecimal integer"
		return
	    } 
	    scan $tdisp "$typeIdx%d.%d" w m 
	}
	f -
	F {
	    if { [regexp {[FEDGfedg]} $_editColWin_type] == 0 } {
		error "Cannot format column to float type"
		return
	    }  
	    scan $tdisp "$typeIdx%d.%d" w m }
	e -
	E {
	    if { [regexp {[FEDGfedg]} $_editColWin_type] == 0 } {
		error "Cannot format column to E type"
		return
	    }  	    
	    switch $tmp1 {
		EN {scan $tdisp "EN%d.%dE%d" w d e; set type EN }
		ES {scan $tdisp "ES%d.%dE%d" w d e; set type ES }
		default {scan $tdisp "$typeIdx%d.%d$typeIdx%d" w d e}
	    }
	}
	g -
	G {
	    if { [regexp {[FEDGfedg]} $_editColWin_type] == 0 } {
		error "Cannot format column to G type"
		return
	    }  	 

	    scan $tdisp "$typeIdx%d.%dE%d" w d e 
	}
	d -
	D {
	    if { [regexp {[FEDGfedg]} $_editColWin_type] == 0 } {
		error "Cannot format column to double type"
		return
	    }  	 
	    scan $tdisp "$typeIdx%d.%dE%d" w d e 
	}
	default {error "Unknown TDISP type"; return NO}
    }

    if { [info exist w] == 0 } {
	return NO
    } 

    set type [string toupper $type]
    if { [info exist m] == 1 } {
	return "${type}${w}.${m}"
    } else {
	if { [info exist d] == 1} {
	    if { [info exist e] == 1 } {
		return ${type}${w}.${d}E${e}
	    } else {
		return ${type}${w}.${d}
	    }
	} else {
	    return ${type}${w}
	}
    }
}

itcl::body FitsTable::_updateHL {} {
   $_fatherFitsExtension updateHL DIMENSION [list $_numCols $_numRows]
}


##############################################
#
#   Select the Table columns to be displayed
#

itcl::body FitsTable::dispTable { extNum_ {cols_ {}} } {
   if { [llength $cols_]==0 } {
      _tableHighLight
      tkwait window .thl
      if { !$_isTableDisplayed } {
         itcl::delete object $this
         return
      }
   } elseif { [lindex $cols_ 0]=="-" } {
      set _listPreSelectedColNames {}
      for {set i 0} {$i < $_numCols } {incr i} {
         lappend _listPreSelectedColNames [lindex $_listColNames $i]
      }
      set _isTableDisplayed 1
   } else {
      set _listPreSelectedColNames $cols_
      set _isTableDisplayed 1
   }

   makeTable $extNum_
}


itcl::body FitsTable::_tableHighLight { } {
    global g_titleFont

    if { [winfo exists .thl] } {
       destroy .thl
    }
    # table highlight
    powToplevel .thl .dummy

    set fName $fileName
    set rName [urlTail $fName]
    # set dName [getFullDirPath $fName]
    wm title .thl "fv: Table Info of $rName\[[expr $currentHDU-1]\]"

    wm geometry .thl 300x300
#
    frame .thl.finfo     -relief sunken  -borderwidth 2
    pack  .thl.finfo      -side top -fill both -expand 0

    #Table Infomation
    label .thl.finfo.col -text "Total Columns: $_numCols" -font g_titleFont
    label .thl.finfo.row -text "Total Rows   : $_numRows" -font g_titleFont
    pack  .thl.finfo.col -side top -fill both -expand 1
    pack  .thl.finfo.row -side top -fill both -expand 1

    #buttons
    iwidgets::buttonbox .thl.buttonbox -orient vertical
    pack  .thl.buttonbox  -side right -padx 0 -pady 0 -fill x -expand 0

    .thl.buttonbox add makeTable -text "Display Table" \
	-command  " [itcl::code $this _makeColSelList]
                    destroy .thl " -padx 0 -pady 0 -font g_titleFont
    .thl.buttonbox add selectAll -text "Select All"  -padx 0 -pady 0 \
	    -command [itcl::code $this _selAllCol] -font g_titleFont
    .thl.buttonbox add clearAll -text "Clear All"    -padx 0 -pady 0 \
	    -command [itcl::code $this _unselAllCol] -font g_titleFont
    .thl.buttonbox add cancel   -text "Cancel"       -padx 0 -pady 0 \
            -command "destroy .thl" -font g_titleFont
    .thl.buttonbox add help     -text "Help"         -padx 0 -pady 0 \
          -command {hhelp columnSelection} -font g_titleFont

    _selAllCol
    # column names         
    iwidgets::scrolledframe .thl.fcolList  \
	    -labeltext "Selected columns for display" -hscrollmode dynamic \
	    -vscrollmode dynamic  -labelfont g_titleFont
    pack  .thl.fcolList   -side left -fill both -expand 1

    set tmpSF [.thl.fcolList childsite]
    set j 0
    foreach i $_listPreSelectedColNames {
	pack [checkbutton $tmpSF.$j -text $i \
		  -variable [itcl::scope _colSelState($j)] \
		  -selectcolor $fvPref::checkBBgColor  \
		  -activeforeground black \
                  -font g_titleFont \
                  -activebackground $fvPref::globalBgColor ]\
	    -anchor w -pady 0
	incr j
    }
}

itcl::body FitsTable::_makeColSelList {} {
    set _listPreSelectedColNames {}
    for {set i 0} {$i < $_numCols } {incr i} {
	if { $_colSelState($i) == 1} {
	    lappend _listPreSelectedColNames [lindex $_listColNames $i]
	}
    }
    set _isTableDisplayed 1
}

itcl::body FitsTable::_selAllCol {} {
    for {set i 0} {$i < $_numCols } {incr i} {
	set _colSelState($i) 1
    }
}

itcl::body FitsTable::_unselAllCol {} {
    for {set i 0} {$i < $_numCols } {incr i} {
	set _colSelState($i) 0
    }
}

#
#  End column selection handlers
#
##############################################


##############################################
#
# Handle Reading/Writing/Formatting of Data
#

itcl::body FitsTable::_writeTableData {col_ row_ val_} {

   set tmpNull [string trim "$_columnNull($col_)" ']
   set tmpForm $_columnForm($col_)
   set tmpType $_columnType($col_)
   set tmpName $_columnName($col_)
    
   if { [regexp L $tmpType] == 1} {
      if { ([regexp -nocase {[ftu]} $val_] == 0) || \
            ([string length $val_] !=1)} {
         error "Logical column can only have value T, F or U"
         return
      } else {
         set val_ [string toupper $val_]
      }
   }	
   set tmpStr [string toupper [string trim $val_ " "]]

   if { $tmpStr == "NULL" } {

      if { $_tableType == "ASCII Table" && [regexp A $tmpType] } {
         set val_ "NULL"
      } else {

         # float and double do not need a TNULL key for binary table
         if { $_tableType == "ASCII Table" } {
            if { $tmpNull == "NULL" } {
               error "\nNo NULL value is defined. Please write a\
                     TNULLn keyword in the header first."
            }
         } else {
            if { $tmpNull == "NULL" && \
                  ![regexp A|D|E|F|C|M|d|e|f $tmpType] } {
               error "\nNo NULL value is defined. Please write a\
                     TNULLn keyword in the header first."
            }		
         }
         set val_ "NULL"
         
      }

   }

   _putRawData $col_ $row_ $val_
}

itcl::body FitsTable::_readTableData {fCol_ fRow_ nCols_ nRows_} {
    # col/row is 0-indexed

    set tmpColList [lrange $_listPreSelectedColNames $fCol_ [expr $fCol_+$nCols_-1]]
    incr fRow_
    incr fCol_

    # table block data will be loaded into a 2-d Tcl array _tableData
    # this cannot be done as a FitsFile method because Tcl does not
    #  allow arrays to be returned
    #  the array must be created locally
    
    set fitsfile [$fFile cget -fitsfile]
    $fitsfile load tblock -noformat _tableData $tmpColList $fRow_ $nRows_ $fCol_ 1
}

itcl::body FitsTable::_getFormattedData {col_ row_} {
   set val [_getRawData $col_ $row_]
   if { $val=="NULL" || $val==" " } {
      return $val
   }
   if { [regexp C|M $_columnType($col_)] } {
      foreach [list v1 v2] $val {}
      return [format "$_columnForm($col_), $_columnForm($col_)" $v1 $v2]
   } elseif {[regexp K $_columnType($col_)] } {
      # chai: return 64 bit integer without format
      return $val
   } else {
      regsub -all {%[0-9]*[iuld]*} $_columnForm($col_) "" result
      if { $result == "" } {
         # $_columnForm($col_) == "%i" || $_columnForm($col_) == "%d" ||
         # $_columnForm($col_) == "%u" || $_columnForm($col_) == "%ld"
         # $_columnForm($col_) == "%xxi" || $_columnForm($col_) == "%xxd" ||
         # $_columnForm($col_) == "%xxu" || $_columnForm($col_) == "%xxld"
         set token [split $val "."]
         set val [lindex $token 0]
      }
      return [format $_columnForm($col_) $val]
   }
}


itcl::body FitsTable::_getRawData {col_ row_} {

   set v $_tableData($col_,$row_)
   if { $v!="NULL" && $v!=" " } {
      if { [regexp E $_columnType($col_)] } {
         return [format "%.7G" $v]
      } elseif { [regexp D $_columnType($col_)] } {
         return [format "%.15G" $v]
      } elseif { [regexp C $_columnType($col_)] } {
         return [format "%.7G %.7G" [lindex $v 0] [lindex $v 1]]
      } elseif { [regexp M $_columnType($col_)] } {
         return [format "%.15G %.15G" [lindex $v 0] [lindex $v 1]]
      } elseif { [regexp K $_columnType($col_)] } {
         # chai: K for 64 bits
         return [lindex [split $v "."] 0]
      }
   }
   return $v
}

itcl::body FitsTable::_putRawData {col_ row_ val_} {
   $fFile putTable $_columnName($col_) 1 [expr $row_+1] [list $val_]
   _readTableData $col_ $row_ 1 1
}

itcl::body FitsTable::_getRawDataBlock { fCol_ fRow_ lCol_ lRow_ } {
   # col/row zero-indexed

   set clipCols [lrange $_listPreSelectedColNames $fCol_ $lCol_]
   incr fRow_
   incr lRow_
   return [$fFile getcolblock $clipCols ${fRow_}-${lRow_}]
}

itcl::body FitsTable::_putRawDataBlock { fCol_ fRow_ data_ } {
   # col/row zero-indexed

   set nCols  [llength $data_]
   set nRows  [llength [lindex $data_ 0]]

   set range "[expr $fRow_+1]-[expr $fRow_+$nRows]"
   foreach cData $data_ {
      $fFile putTable $_columnName($fCol_) 1 $range $cData
      incr fCol_
   }
}


#
#  End Data handlers
#
##############################################
