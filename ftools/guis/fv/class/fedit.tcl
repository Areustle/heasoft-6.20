proc fvInit { args } {
   global g_isExitting g_plugin
   global env

   #  Prevent doing this twice
   if { ![info exists g_isExitting] } {
      
      # Load Itcl/Itk packages

       package require Itcl
       package require Itk

       #namespace import -force itcl::*
   # Strange behaviour of mktclapp. For standalone, this statement
   # causes trouble
       if ![info exists env(FV_ISEXEC) ] { 
           package require Iwidgets
       }
       
      eval fvApp fv $args
   } elseif { [itcl::find object fv]!="" } {
      fv processOptions $args
   }
   after idle checkForExit
   return
}

## note .fD is hardcoded --Han
#
proc selFile {{dir_ {}} } {
   if { $dir_ != {} } {
      .fD chgDir $dir_
   }
   .fD activate Open
}


## why is it called defName_?
##  note .fD is hardcoded
#
proc getSelectedFileName { defName_ } {
   global fileselect

   .fD activate Save $defName_

   # wait till a file is selected  
   tkwait variable fileselect

   return $fileselect
}


proc createNewFITS { {file_ ""} } {
    if { [itcl::find objects .newfits] != ""} {
	itcl::delete object .newfits
    }

# NEWFITS cleans itself afterwards. No need to delete
    NewFITS .newfits $file_ -title "fv: Create Image"
    wm geometry .newfits +[winfo pointerx .]+[winfo pointery .]
    
    tkwait window .newfits 
}

proc openFile { filename_ } {
    global isWin g_isScript g_openDir

    if $isWin {
        set filename_ [file attributes $filename_ -longname]
    }

    if { ![file exist $filename_] } {
       set oldFile $filename_

       foreach ext { gz Z z zip } {
          if { [file exists $filename_.$ext] } {
             set filename_ $filename_.$ext
             break
          }
       }
       if { $filename_== $oldFile } {
          #  error "File $filename_ does not exist"
          tk_messageBox -type ok -icon warning \
                        -message "File $filename_ deoes not exist"
          .fD activate Open
          return
       }
    }

    if { [file extension $filename_] == ".fv" } {
        set g_isScript 1
        namespace eval ::fvCmds [list source $filename_]
        set g_isScript 0
    } else { 
        if { [file isdirectory $filename_] } {
            set g_openDir $filename_
        } else {
            if { [catch {openFitsFile $filename_} msg] } {
                tk_messageBox -type ok -icon warning \
                        -message "Cannot open file $filename_\n\n$msg"
            }
        }
    } 
}


proc openFitsFile {filename_} {
    global g_fitsFileMode
    return [openFitsFileWM $filename_ $g_fitsFileMode]
    return $result
}

proc openFitsFileWM {filename_ fitsFileMode_} {

    # if it's not an ftp or http file
    if { [string range $filename_ 0 5]!= "ftp://" && \
	    [string range $filename_ 0 6]!= "http://" } {
	
	# This is a local file so resolve any symlinks
	# and see if the file exists
	
	set filename_ [resolveSymLinks $filename_]

	if { ![file exists $filename_] } {
	    set resp [tk_messageBox -icon error -type yesno \
		    -message "$filename_ doesn't exists.\nCreate new file?" \
		    -title "File doesn't exist"]
	    if { $resp=="no" } {
		return ""
	    }
	    createNewFITS $filename_
	    return ""
	}
    }
    
    set isfits [isFits $filename_]
    if { $isfits == 0 } {
	error "$filename_ is not a FITS file"
	return ERROR
    } 
    
    #the fitsfilemode is fed from the file dialog. 1=readonly, 0=read/write
    
    if { $fitsFileMode_ == 0 } {
        if { ![file writable $filename_] } {
#puts "File is not writable, open as read only"
	    set fitsFileMode_ 1
	} elseif {$isfits == 2} {
#puts "File is compressed, open as read only"
	    set fitsFileMode_ 1
	}
    }
    
    # returns the FITS file
    return [FitsFile #auto $filename_ $fitsFileMode_]
}

set ::g_fvHelpFiles [list \
      "About fv"               aboutFv.html          \
      "Start fv"               startFv.html          \
      "Calculator"             calculator.html       \
      "Calculator Expressions" expressions.html      \
      "Column Selection"       columnSelection.html  \
      "Column Statistics"      columnStatistics.html \
      "Connect to Hera"        connectToHera.html \
      "Connect to Student Hera" connectToStudentHera.html \
      "Create New FITS File"   createNewFITS.html    \
      "Deleting Rows"          deleteRows.html       \
      "Desktop Manager"        deskTopManager.html   \
      "Display Device"         displayDevice.html   \
      "Column Parameters"      displayFormat.html    \
      "File Summary"           fileSummary.html      \
      "File Selection"         fileSelection.html    \
      "Header Display"         headerDisplay.html    \
      "Image Plots"            imagePlot.html        \
      "FV License"             license.html          \
      "Plot Dialog"            plotDialog.html       \
      "Preferences"            preferences.html      \
      "Sorting Columns"        sortColumn.html       \
      "Scripting"              fv_scripting.html     \
      "SkyView"                SkyView.html          \
      "Catalog Database"       catalog.html          \
      "VizieR"                 VizieR.html           \
      "FTOOL Execution"        ftool.html            \
      "Table Display"          tableDisplay.html     \
      "Image Tables"           imageDisplay.html     \
      "Histograms"             2D-Histogram.html     \
      "3D Image Tables"        3D-ImageTable.html    \
      "3D Image Display"       3D-ImageDisplay.html  \
      ]

proc hhelp {topic_} {
  global g_fvHelpFiles
  global isWin
  global env

  if { [string match "*.html" $topic_] } {
     set topic_ [string range $topic_ 0 end-5]
  }
   
  if { [winfo exist .hyperHelp] == 0} {
      if { $isWin } {
         set size large
      } else {
         set size medium
      }
      set allTopics {}
      foreach [list aTitle aTopic] $g_fvHelpFiles {
         lappend allTopics [list $aTitle $aTopic]
      }
      iwidgets::hyperhelp .hyperHelp -title "fv: Hyperhelp" \
            -topics $allTopics \
            -fontname courier -fontsize $size \
            -helpdir	 $env(FV_HELPDIR)
  } 
    .hyperHelp showtopic $topic_
    catch {.hyperHelp activate}
} 

proc promptMsg {text_ command_ okText_ cancelText_ {newCmdText ""} } {
    global isWin
    global g_titleFont
    global notSaveAll

#   For some reasons, we can not activate the messagedialog on windows,
#   We have use tk_messagebox instead. 

    if {[info exists notSaveAll] && $newCmdText == "No to All"} {
       return "CANCEL"
    }

    if {$isWin } { 
        if {$cancelText_ == "Cancel" } {
          set choice [tk_messageBox -type yesnocancel -default yes \
             -message $text_ -icon question ]
        } else {
          set choice [tk_messageBox -type yesno -default yes \
             -message $text_ -icon question ]
        }
        if { $choice == "yes"  } {
          $command_
	  return
        } elseif {$choice == "cancel"} {
	    return BREAK
        } else {
	    return CANCEL
        }
       
    }    

    # Make sure all windows get mapped first, then popup window
    update idletasks
    if { [winfo exist .md] == 0} {
# load  the message dialog if not exist
       iwidgets::messagedialog .md -title "fv: Message Dialog" \
            -font g_titleFont \
 	    -bitmap questhead \
	    -modality application
    } 

    wm geom .md +[expr [winfo screenwidth .] / 3]+[expr [winfo screenheight .] / 2]
    if { $newCmdText == "No to All" } {
       catch { 
           .md add NoToAll -text $newCmdText -font g_titleFont \
                           -command { \
                               .md deactivate NoToAll ; \
                           } 
       }
    }
    .md configure -text $text_
    .md buttonconfigure OK -text $okText_ -font g_titleFont
    .md buttonconfigure Cancel -text No -font g_titleFont     
    if { $newCmdText == "No to All" } {
       .md buttonconfigure NoToAll -text "No to All" -font g_titleFont     
    }
    if { $cancelText_ == "Cancel" } {
	.md show Help
	.md buttonconfigure Help -text Cancel -command ".md deactivate 2" -font g_titleFont
    } else {
        .md hide Help
    }
    set fb [.md activate]

    # fb: 0: No, 
    #     1: Yes, 
    #     2: Break (Cancel)
    #     val: other

    if { $fb == 1  } {
	$command_
	return
    } elseif {$fb == 0} {
	return CANCEL
    } elseif {$fb == 2} {
	return BREAK
    } elseif {$fb == "NoToAll" } {
        global notSaveAll

        if ![info exists notSaveAll] {
           set notSaveAll 1
        }
        return CANCEL
    }
    
}

proc cleanBackupDir {} {
    global g_backupDir

    if { [catch {set filelist [glob [file join $g_backupDir *]]}] == 0 } {
       foreach i $filelist {
           file delete -force $i
       }
    }

    file delete -force $g_backupDir
    return
}


proc checkForExit { } {
   global g_listObjs isMac g_isExitting g_isQuiet

   # If there is still a file or Manager open, or on Mac, continue
   if {     $g_isExitting \
         || $isMac || $g_isQuiet \
         || [llength $g_listObjs]>0 \
         || $fvPref::ifUseManager \
         || ([winfo exists .fD] && [winfo ismapped .fD]) } {
      return
   }
   exitCmd
}


proc exitCmd { {force_ 0} } {
    global g_listObjs g_isExitting g_isQuiet g_backupDir
    global env fvTmp
    
    if { $g_isExitting } return

    # Once we have reached this point, cease being a quiet application
    set g_isQuiet 0
    foreach i $g_listObjs {
       if { [$i closeCmd $force_] } return
    }
    set g_isExitting 1 

    # Pan Chai: clean up the backup directory
    if { [info exists fvTmp] && [string first $fvTmp $g_backupDir] > 0 } {
       set fileNameList [eval glob -directory {$g_backupDir} -nocomplain "*"]
       foreach name $fileNameList {
          if [file exist $name] {
             catch { file delete -force $name } err
          }
       }
       if [file exist $g_backupDir] {
          catch { file delete -force $g_backupDir } err
       }
    }
    # after idle {itcl::delete object fv}
    exit
}


# convert a decimal degree to HH MM SS.S
proc hourRA {deciValue_} {
    set hourValue [expr $deciValue_/15.0]
    set hour [expr int($hourValue)]
    set minuValue [expr ($hourValue - $hour)*60.0]
    set minu [expr int($minuValue)]
    set scndValue [expr ($minuValue - $minu)*60.0]
    set scnd $scndValue
    return [format "%d:%02d:%05.2f" $hour $minu $scnd]

}

# convert a decimal degree to DD MM SS.S
proc degDec {deciValue_} {
    if { $deciValue_ < 0} {
	set isNeg 1
    } else {
	set isNeg 0
    }
    set deciValue_ [expr abs($deciValue)]
    set deg [expr int($deciValue_)]
    set minuValue [expr ($deciValue_ - $deg)*60.0]
    set minu [expr int($minuValue)]
    set scndValue [expr ($minuValue - $minu)*60.0]
    set scnd $scndValue
    if { $isNeg == 1} {
	return [format "-%d:%02d:%05.2f" $deg $minu $scnd]
    } else {
	return [format "%d:%02d:%05.2f" $deg $minu $scnd]
    }
}

proc validColName {text_} {
    return [regexp -nocase {[0-9a-z_A-Z ]} $text_]
}


proc returnCurrentImageInfo {father_ extno_ img_ x_ y_} {
    global xCount yCount

    set gn [powWhereAmI [.pow.pow canvasx $x_] [.pow.pow canvasy $y_]]
    if { $gn == "NULL" } return

    set cx [.pow.pow canvasx $x_]
    set cy [.pow.pow canvasy $y_]
    set ccord  [powCanvasToPixel $gn $img_ $cx $cy]
    set pixelx [expr round([lindex $ccord 0]) + 1]
    set pixely [expr round([lindex $ccord 1]) + 1]

    set width  [image width  $gn]
    set height [image height $gn]

    # these are for flipping the image
    if { [info exists xCount($gn)] && [expr $xCount($gn) % 2] != 0 } {
       set pixelx [expr $width - $pixelx + 1]
    }
    if { [info exists yCount($gn)] && [expr $yCount($gn) % 2] != 0 } {
       set pixely [expr $height - $pixely + 1]
    }
    $father_ openTable $extno_ [list $pixelx $pixely]
}

# note this code will do nothing if the VectorTable is closed
# double-clicking on an image will *not* open a new VectorTable, whereas
#  it *will* for FitsImage tables
proc returnCurrentImageInfo_ForVectorTables {vectortable_ img_ x_ y_} {

    set gn [powWhereAmI [.pow.pow canvasx $x_] [.pow.pow canvasy $y_]]
    if { $gn == "NULL" } return
    
    set cx [.pow.pow canvasx $x_]
    set cy [.pow.pow canvasy $y_]
    set ccord  [powCanvasToPixel $gn $img_ $cx $cy]
    set pixelx [expr round([lindex $ccord 0]) + 1]
    set pixely [expr round([lindex $ccord 1]) + 1]

# if $vectortable_ no longer exists, nothing will happen
    catch { $vectortable_ bringToFront }
    catch { $vectortable_ showCell $pixelx $pixely }
}


proc getFullDirPath { file_ } {
   global isWin isMac

   if { [string range $file_ 0 5] == "ftp://" || \
        [string range $file_ 0 6] == "http://" } {

      set dName [string range $file_ 0 [string last "/" $file_]]

   } else {

       if { $isMac } {
	  set dirChar ":"
      } else {
	  set dirChar "/"
      }
      
      set dName [file dirname $file_]
      if { [string range $dName end end]!=$dirChar } {
         set dName $dName$dirChar
      }
      
      if {     (!$isWin && !$isMac && [string range $dName 0 0] != "/") \
	    || ($isWin && [string range $dName 1 2]!=":/" \
	        && [string range $dName 0 0]!="/") \
	    || ($isMac && [string range $dName 0 0]==":") } {
	 if { $dName == ".$dirChar"} {
	    set dName [pwd]$dirChar
	 } elseif { $isMac } {
	    set dName [file join [pwd] $dName]$dirChar
	 } else {
	    set dName [pwd]$dirChar$dName
	 }
      }

   }
   return $dName
}

proc setWatchCursor { win_ args } {
#   puts "$win $args"
   .fvwinkeeper setCursor watch
   if { $win_ != "" } {
      $win_ configure -cursor watch
   }
   update

   catch {set rslt [eval $args]} err

   if { $win_ != "" } {
      $win_ configure -cursor top_left_arrow
   }
   .fvwinkeeper setCursor top_left_arrow

   if { [info exists rslt] } {
      return $rslt
   } else {
      error $err
   }
}


proc doMenuEvent { evt_ {evtWndw_ ""} } {
   global lastEventWndw isMac isWin

# This evtWndw_ messiness is necessary due to LinuxPPC's (and others?) problems
# in tracking the focus when selecting menu items.  Can't tell if it is a
# Window Manager problem or Tk problem.

# set whn
   if { $evt_ == "<<PostMenus>>" } {
       set whn "now"
       set lastEventWndw $evtWndw_
   } else {
       set whn "tail"
   }

# set evtWndw_
   if { $evtWndw_=="" } {
      if { $lastEventWndw=="" } {
         set evtWndw_ [focus]
      } else {
         set evtWndw_ $lastEventWndw
      }
   }

#puts "Evt: $evt_ in $evtWndw_"
   if { $evtWndw_ != "" && [winfo exists $evtWndw_] } {
      if { $isMac && $evt_=="<<Quit>>" } {
          exitCmd
      }
      if { $isWin && $evt_=="<<Quit>>" } {
          exitCmd
      }

#  the main use of this proc
      event generate $evtWndw_ $evt_ -when $whn

   } else {
      event generate . $evt_ -when $whn
   }
}

proc BuildMacMenus { } {

   menu          .mbar       -postcommand "doMenuEvent <<PostMenus>>"
   bind all <<PostMenus>> postMacMenus
   buildAppleStyleMenu .mbar.apple
   buildFileMenu .mbar.file
   buildEditMenu .mbar.edit
   buildToolsMenu .mbar.tools
   buildWindMenu .mbar.wind
   buildHelpMenu .mbar.help aboutFv "About fv"
   .mbar.help delete 0 1

   . configure -menu .mbar
   .mbar add cascade -menu .mbar.apple
   .mbar add cascade -menu .mbar.file -label "File"
   .mbar add cascade -menu .mbar.edit -label "Edit"
   .mbar add cascade -menu .mbar.tools -label "Tools"
   .mbar add cascade -menu .mbar.wind -label "Windows"
   .mbar add cascade -menu .mbar.help -label "Help"
}


# this proc does nothing, why is this here at all? --Han
#
proc postMacMenus { } {
    #.fvwinkeeper updateMacWindows
}

# for Apples only (only called when g_isMac is true)
#
proc buildAppleStyleMenu { theMenu_ } {
    menu $theMenu_ -tearoff False
    $theMenu_ add command -label "About fv" -command "hhelp aboutFv"
}


proc buildFileMenu { theMenu_ } {
   global isMac
   global g_titleFont

   if { $isMac } {
      set cmdkey "Cmd"
   } else {
      set cmdkey "Alt"
   }
    
   menu $theMenu_ -tearoff False

   $theMenu_ add command -label "New File..." -underline 0 \
         -command createNewFITS -accelerator "$cmdkey+N" -font g_titleFont
   $theMenu_ add command -label "Open File..." -underline 0 \
         -command selFile -accelerator "$cmdkey+O"  -font g_titleFont

   $theMenu_ add command -label "Save" -underline 0 -state disabled \
         -command "doMenuEvent <<SaveFile>>" -accelerator "$cmdkey+S"  -font g_titleFont
   $theMenu_ add command -label "Save As..." -state disabled \
         -command "doMenuEvent <<SaveFileAs>>"  -font g_titleFont
   $theMenu_ add command -label "Export" -underline 7 -state disabled \
         -command "doMenuEvent <<Export>>" -accelerator "$cmdkey+H"  -font g_titleFont
   $theMenu_ add command -label "ExportAs" -state disabled \
         -command "doMenuEvent <<ExportAs>>" -font g_titleFont
   $theMenu_ add command -label "Revert" -underline 0 -state disabled \
         -command "doMenuEvent <<RevertFile>>" -accelerator "$cmdkey+R"  -font g_titleFont
   $theMenu_ add command -label "Close"  -state disabled \
         -command "doMenuEvent <<CloseWindow>>" -accelerator "$cmdkey+W"  -font g_titleFont
   
   $theMenu_ add command -label Quit -underline 0 \
         -command "doMenuEvent <<Quit>> ." -accelerator "$cmdkey+Q"  -font g_titleFont

}


proc buildEditMenu { theMenu_ } {
   global isMac
   global g_titleFont

   if { $isMac } {
      set cmdkey "Cmd"
   } else {
      set cmdkey "Alt"
   }
    
   menu $theMenu_ -tearoff False

   $theMenu_ add command -label Undo -state disabled \
         -command "doMenuEvent <<Undo>>"  -accelerator "$cmdkey+Z"  -font g_titleFont
   $theMenu_ add separator
   $theMenu_ add command -label Cut -state disabled \
         -command "doMenuEvent <<Cut>>"   -accelerator "$cmdkey+X"  -font g_titleFont
   $theMenu_ add command -label Copy -underline 0 -state disabled \
         -command "doMenuEvent <<Copy>>"  -accelerator "$cmdkey+C"  -font g_titleFont
   $theMenu_ add command -label Paste -state disabled \
         -command "doMenuEvent <<Paste>>" -accelerator "$cmdkey+V"  -font g_titleFont
   $theMenu_ add command -label Clear -state disabled \
         -command "doMenuEvent <<Clear>>"  -font g_titleFont
   $theMenu_ add separator
   $theMenu_ add command -label Clipboard \
         -command "fvClipBoard toggleView"  -font g_titleFont
   $theMenu_ add separator
   $theMenu_ add command -label "Preferences..." -command "fvPref edit"  -font g_titleFont
}

proc buildToolsMenu { theMenu_ } {
   global isMac
   global env

   if { $isMac } {
      set cmdkey "Cmd"
   } else {
      set cmdkey "Alt"
   }

   menu $theMenu_ -tearoff False
   $theMenu_ add command -label "Plot" -state disabled \
         -command "doMenuEvent <<PlotHDUs>>"   -font g_titleFont
   $theMenu_ add command -label Smooth -state disabled \
         -command "doMenuEvent <<Smooth>>"   -font g_titleFont
   $theMenu_ add command -label "Connect to SkyView" -state normal \
         -font g_titleFont \
         -command {
                global g_skyvflag
                if ![info exists g_skyvflag ] {
                      set g_skyvflag 1
                      FVSkyview skyv
                }
                skyv getskyvfile
          }
   $theMenu_ add command -label "Connect to Catalog" -state normal \
         -font g_titleFont \
         -command {
                global g_skyvflag
                if ![info exists g_skyvflag ] {
                      set g_skyvflag 1
                      FVSkyview skyv
                }
                skyv getskyvcat
          }
   $theMenu_ add command -label "Connect to VizieR" -state normal \
         -font g_titleFont \
         -command {
                global g_vizier
                if ![info exists g_vizier ] {
                     set g_vizier 1
                     FVVizier viz
                }
                viz selectVizier
          }

   if { [info exists env(FTOOLS)] || [info exists env(LHEASOFT)] } {
      $theMenu_ add command -label "Run Ftool" -state normal \
         -font g_titleFont \
         -command {
                global g_ftoolflag
                if ![info exists g_ftoolflag ] {
                      set g_ftoolflag 1
                      FtoolInstance fvFtool
                }
                fvFtool MainMenu
         }
   }

}

proc buildWindMenu { theMenu_ } {

   menu $theMenu_ -tearoff False

   $theMenu_ add command -label "Hide All Windows" \
         -font g_titleFont \
         -command ".fvwinkeeper actOn all"

   menu $theMenu_.sm  -tearoff false
   menu $theMenu_.hm  -tearoff false
   menu $theMenu_.tm  -tearoff false
   menu $theMenu_.itm -tearoff false
   menu $theMenu_.vtm -tearoff false

   $theMenu_ add cascade -menu $theMenu_.sm  -label "File Summary"  -font g_titleFont
   $theMenu_ add cascade -menu $theMenu_.hm  -label "Header"  -font g_titleFont
   $theMenu_ add cascade -menu $theMenu_.tm  -label "Table"  -font g_titleFont
   $theMenu_ add cascade -menu $theMenu_.itm -label "Image Table"  -font g_titleFont
   $theMenu_ add cascade -menu $theMenu_.vtm -label "Vector Table"  -font g_titleFont

}

## adds firstTitle_ and firstTopic_, then adds rest of titles/topics in
## g_fvHelpFiles
#
proc buildHelpMenu { theMenu_ firstTopic_ firstTitle_ } {
    global g_titleFont
    menu $theMenu_ -tearoff False
    $theMenu_ add command -label $firstTitle_ -command "hhelp $firstTopic_" \
                          -font g_titleFont
    $theMenu_ add separator
    foreach {title topic} $::g_fvHelpFiles {
        $theMenu_ add command -label $title -command "hhelp $topic" -font g_titleFont
    }
}


proc urlSplit { url_ } {
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

proc urlTail { url_ } {
    return [lindex [urlSplit $url_] end]
}

## what does this do?
#
proc calcSizeStr { size_ } {
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


proc calcDateStr { time_ } {
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


proc resolveSymLinks { filename_ } {

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


proc loadTclCode { hostPath_ sourceFile_ } {
   global serverIdx
#puts loadTclCode

   set returnStr "SUCCESSFUL"
   package require http
   set errorFlag [ catch {
       set urlToken [http::geturl "$hostPath_/$sourceFile_"] 
   } err ]

   if { $errorFlag } {

      set top .errorMsg
      toplevel .errorMsg
      set screenX [expr [winfo screenwidth .] / 3 - 100]
      set screenY [expr [winfo screenheight .] / 2 - 100]
      wm geometry $top +$screenX+$screenY
      wm title .errorMsg "Error Connecting"

      message $top.text -justify left -relief sunken \
                        -text "Can't access Hera Module.\n\nYou might not be able to connect to NASA web server to run HERA \"[file tail $hostPath_]\" module.\n\nPlease verify your connection to\n\n      $hostPath_\n\nby using your web browser and direct the page to above address\n\nIf you can't, contact your network administator to resolve the problem.\n\n\nContact HERA development team at\n\n     heradev@olegacy.gsfc.nasa.gov\n\nif problem presisted.\n\nThank you." \
                        -cursor watch -font g_titleFont -width 490

      button $top.done -text "EXIT" -command "destroy $top" -font g_titleFont

      grid $top.text -row 0 -column 0 -columnspan 5 -rowspan 20 -sticky news
      grid $top.done -row 20 -column 2 -sticky news
      update idletasks

      tkwait window .errorMsg

      return FAILED
   } else {
      set httpCode [http::code $urlToken]

      if { [string first "404 NOT FOUND" [string toupper $httpCode]] >= 0 } {
         return MODULE_NOT_FOUND
      }

   }

   set contents [http::data $urlToken]
   if { $sourceFile_ == "tclIndex" } {
      set targetList [split $contents \n]
      set targetLine ""
      foreach line $targetList {
           set idx [lsearch -glob $line "*(::*::init)*"]
           if { $idx > 0 } {
              set targetLine $line
              break
           }
      }
#puts "targetLine: $targetLine"
      if { $targetLine != "" } {
         regsub -all ":" $targetLine "" targetLine 
         set tokenList [split $targetLine "()"]     
         set targetModule [lindex $tokenList 1]
         set idx [string first "init" $targetModule]
         set returnStr [string range $targetModule 0 [expr $idx - 1]] 
      }
#puts "returnStr: $returnStr"
   }

   http::cleanup $urlToken
   if { $sourceFile_=="tclIndex" } {
      #  Munge standard tclIndex format into WWW format
      regsub -all {source \[file join \$dir} \
            $contents "loadTclCode $hostPath_" tmp
      regsub -all {\]\]} $tmp {]} contents
   }
   namespace eval :: $contents
#puts "done loadTclCode"
   return $returnStr
}

##### only for debugging purposes

proc okbox { message_ } {
   tk_messageBox -type ok -message $message_
}

