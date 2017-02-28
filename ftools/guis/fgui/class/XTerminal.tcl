proc gXTerminal { args } {
   return [uplevel #0 XTerminal #auto $args]
}

itcl::class XTerminal {
   constructor { wndw option server cSession tSession } {}
   destructor {}
   
   public {
       method setupRunningState { state }
       method shutdown {}

       # drag and drop
       method dropDraggedItem { args }
       method dragOver { args }

       # I/O
       method appendOutput { output fromWhere { option "NOT_USER_INPUT" } }
       method sendTextToTool { cmd }
       method _getPreviousHistory {} 
       method _displayHistory {} 
       method _setScriptRunning { state } { set scriptFlag $state }

       # for Print
       method printOutput {}
       method checkEditor { fileName }
       method setEditor {}
       
       # for Save
       method saveAsSelectDirectory { dir }
       method saveFile {}
       method saveAs   {}
       method Dump_Session  {}
       method Dump_Cmd  {}
       method setLocalHandler { obj }
       method changeToolServer { srvr }
       method getItsToolServer {} { return $toolServer }

       # interactive with Server
       method receiveOutput { args }
       method setToolName { name } { set theTool $name }

       # add to history
       method addToHistory { cmd } 

       # window manage
   }
   
   private {
       method processAnyKey { currentInput currentInputKeySym wndw }
       method processOutput { line lastLineFlag }
       method reverseModifyName { name }

       # binding
       method setup_XTerminalBinding { wndw }
       method bind_MouseEnterX { wndw }
       method bind_MouseEnter { wndw }
       method bind_ButtonPressOne { wndw }
       method bind_ButtonReleaseOne { wndw }
       method bind_ButtonOneMotion { wndw }

       method bind_KeyPressDeleteCheck { currentInputKeySym wndw }
       method bind_KeyPressLeft { wndw }
       method bind_KeyReleaseHome { wndw }

       method bind_KeyReleaseReconstructDelete { wndw }
       method bind_KeyReleaseReturn { wndw }
       method bind_KeyReleaseLeft { wndw }
       method bind_KeyReleaseRight { wndw }

       method bind_KeyReleaseUp { wndw }
       method bind_KeyReleaseDown { wndw }

       method bind_TripleButtonPressOne { wndw }
       method bind_TripleButtonReleaseOne { wndw }
       method bind_ButtonTwo { wndw { x {} } { y {} } }

       method bind_FocusIn { wndw }

       # interactive with Server
       method _sendPanicSignal { wndw }

       # prompt
       variable currentPrompt
       variable promptLine
       variable previous_promptStart "X.X"
       variable promptStart "X.X"
       variable promptMajor
       variable promptMinor
       variable expectingReturn false
       variable promptLineFound false
       variable promptLength 

       # button selection
       variable selectionStart
       variable selectionEnd
       variable appendOutputFlag false

       # buffer and history of input key
       variable previousInputBuffer ""
       variable inputBuffer
       variable inputHistory {}
       variable selectedHistoryIdx 0
       variable textBuffer {}
       variable viewer "notepad"
       variable selectDirectory
       variable savedFileName "saveFile.txt"
       variable currentChar 

       # current position of insertion
       variable currentPos
       variable triplePressSelection
       variable pIdx1

       # tool 
       variable theTool "Terminal"
       variable outputWndw
       variable toolServer "NONE"
       variable plistFlag "false"
       variable scriptFlag "false"
       variable callSession 
       variable topSession 

       # display History
       variable nextLineNumber 1
       variable maxMark 500
       variable moreFlag "false"

       # user info
       variable userName
       variable myWndw
       variable myWndwHeight
       variable myWndwWidth
       variable lastRowNumber
       variable wndwOption
       variable heraFS
       variable localFS
       variable runner 
       variable mainWndw
       variable path
   }
}

itcl::body XTerminal::constructor { wndw option server cSession tSession } {
     set outputWndw  $wndw
     set wndwOption $option
     set toolServer $server
     set callSession $cSession
     set topSession $tSession

     frame $outputWndw.opframe 
    
     menubutton $outputWndw.opframe.file -text "File" -menu $outputWndw.opframe.file.menu

     button $outputWndw.opframe.history -text "History" -command [itcl::code $this _displayHistory] \
                                        -relief flat

     grid $outputWndw.opframe.file -row 0 -column 0 -padx 10
     grid $outputWndw.opframe.history -row 0 -column 1 -padx 10

     set m [menu $outputWndw.opframe.file.menu -tearoff 0]

     $m add command -label "Dump Log" -command [itcl::code $this Dump_Session]
     $m add command -label "Dump Cmd" -command [itcl::code $this Dump_Cmd]
     $m add separator 
     $m add command -label "Print" -command [itcl::code $this printOutput]

     iwidgets::scrolledtext $outputWndw.txt \
                  -textfont [list Courier 12] \
                  -textbackground white \
                  -hscrollmode dynamic \
                  -vscrollmode dynamic \
                  -wrap char -visibleitems 80x15
 
      $outputWndw.txt tag configure inputTag -foreground blue
      $outputWndw.txt tag configure errorTag -foreground red
      $outputWndw.txt component text configure -insertwidth 3 -insertbackground red
      label $outputWndw.title
 
      set colNum [lindex [split [$outputWndw.txt cget -visibleitems] "x"] 0]
      set rowNum [lindex [split [$outputWndw.txt cget -visibleitems] "x"] 1]
 
      grid $outputWndw.opframe -row 0 -column 0 -sticky nws -columnspan 2
      grid $outputWndw.txt   -row 1 -column 0 -sticky news -rowspan $rowNum -columnspan $colNum
      grid $outputWndw.title -row [expr $rowNum + 1] -column 0 -sticky new -columnspan $colNum
 
      grid rowconfigure    $outputWndw 2 -weight 1
      grid columnconfigure $outputWndw 1 -weight 1
 
      appendOutput "cmd> " OutputChannel NEW_WINDOWS
 
      update idletasks
 
      set childsite [$outputWndw.txt childsite]
      setup_XTerminalBinding $childsite
      focus $childsite
}

itcl::body XTerminal::destructor {} { 
}  

itcl::body XTerminal::shutdown {} { 
   set windowList [winfo children $outputWndw]
   foreach wndw $windowList {
       destroy $wndw
   }
   destroy $outputWndw
   #itcl::delete object $this
}  

itcl::body XTerminal::processAnyKey { currentInput currentInputKeySym wndw } {

     # make sure that the insert is always at the end
     set lineMajor [lindex [split [$wndw index insert] "."] 0]
     set lineMinor [lindex [split [$wndw index insert] "."] 1]
     if { $lineMajor != [expr $nextLineNumber - 1] || $lineMinor < $promptLength  } {
        $wndw mark set insert end
     }

     if ![info exists inputBuffer] {
        set inputBuffer ""
     }

     if ![info exists inputHistory] {
        set inputHistory {}
        set selectedHistoryIdx 0
     }

     if { [string length $inputBuffer] == 0 } {
        set previous_promptStart $promptStart
        set promptStart [$wndw index insert]
        set currentPos [lindex [split $promptStart "."] 1]
        set token [split $promptStart "."]
        set promptMajor [lindex $token 0]
        set promptMinor [lindex $token 1]
     }


     # reset selectedHistoryIdx to top of history stack
     set previousSelectedHistoryIdx $selectedHistoryIdx
     set selectedHistoryIdx -1

     set idx2 [$wndw index "end - 1 char"]
     $wndw tag add inputTag [expr $nextLineNumber - 1].$promptLength $idx2

     scan $currentInput "%s" currentInput
     switch -exact $currentInputKeySym {
            "Return" {
                 $wndw mark set insert end
                 # append the string enter before <Return> to history, and clear the buffer
                 # only those string that has length greater than 0 will be appended
                 # list is in stack order for easy retrieval

                 set doneLine [lindex [split [$wndw index insert] "."] 0]
                 set doneChar [lindex [split [$wndw index insert] "."] 1]
                 if { $doneChar < $currentPos } {
                     $wndw mark set insert ${doneLine}.$currentPos
                 }
                 set inputBuffer [$wndw get $promptStart [$wndw index insert]]
                 if { [string length [string trim $inputBuffer]] > 0 } {
                    set inputOldHistory $inputHistory
                    unset inputHistory

                    # reverse order
                    lappend inputHistory $inputBuffer
                    foreach history $inputOldHistory {
                       if { [string trim $history] != "" } {
                         lappend inputHistory $history
                       }
                    }
                 }

                 set selectedHistoryIdx -1

                 set nextPromptStartMajor [lindex [split [$wndw index insert] "."] 0]
               
                 set promptStart [format "%s.0" [expr $nextPromptStartMajor + 1]]
                 $wndw mark set insert $promptStart
                 update idletasks
            }
            "BackSpace" - 
            "Delete" {
                 # These keys are bounded by other procedures, processAnyKey would ignore this key
            }
            "Up" -
            "Down" {
                 $wndw mark set insert end
                 set selectedHistoryIdx $previousSelectedHistoryIdx

                 if { [string length $inputBuffer] == 0 } {
                    set previous_promptStart $promptStart
                    set promptStart [$wndw index insert]
                    set currentPos [lindex [split $promptStart "."] 1]
                    set token [split $promptStart "."]
                    set promptMajor [lindex $token 0]
                    set promptMinor [lindex $token 1]
                 }

                 # Event generate this key, process should ignore this key and not put in the list
            }
            "Left" -
            "Right" {

                 # Event generate this key, process should ignore this key and not put in the list
            }
            default {
                 $wndw mark set insert [$wndw index insert]
                 if { [string length $inputBuffer] == 0 } {
                    set previous_promptStart $promptStart
                    set promptStart [$wndw index insert]
                    set currentPos [lindex [split $promptStart "."] 1]
                    set token [split $promptStart "."]
                    set promptMajor [lindex $token 0]
                    set promptMinor [lindex $token 1]
                 }

                 set minor [lindex [split [$wndw index insert] "."] 1]
                 $wndw mark set current ${promptMajor}.$minor

                 if { [string length $currentInput] > 0 } {
                    # append the current input character to buffer
                    # append inputBuffer $currentInput
                    $wndw tag remove currentInputTag $promptStart end
                    if { $inputBuffer == "" } {
                       set inputBuffer $currentInput
                    } else {
                       set inputBuffer [format "%s%s" $inputBuffer $currentInput]
                    }
                    $wndw tag add currentInputTag $promptStart end
                    incr currentPos
                 }
            }
     }
}
     
itcl::body XTerminal::setup_XTerminalBinding { wndw } { 

     bind $wndw <Any-Key>                [itcl::code $this processAnyKey %A %K %W]
     bind $wndw <KeyPress-Delete>        [itcl::code $this bind_KeyPressDeleteCheck %K %W]
     bind $wndw <KeyPress-BackSpace>     [itcl::code $this bind_KeyPressDeleteCheck %K %W]
     bind $wndw <KeyPress-Left>          [itcl::code $this bind_KeyPressLeft %W]
     bind $wndw <KeyRelease-Home>        [itcl::code $this bind_KeyReleaseHome %W]

     bind $wndw <KeyRelease-BackSpace>   [itcl::code $this bind_KeyReleaseReconstructDelete %W]
     bind $wndw <KeyRelease-Delete>      [itcl::code $this bind_KeyReleaseReconstructDelete %W]
     bind $wndw <KeyRelease-Right>       [itcl::code $this bind_KeyReleaseRight %W]
     bind $wndw <KeyRelease-Left>        [itcl::code $this bind_KeyReleaseLeft %W]
     bind $wndw <KeyRelease-Return>      [itcl::code $this bind_KeyReleaseReturn %W]
     bind $wndw <KeyRelease-Up>          [itcl::code $this bind_KeyReleaseUp %W]
     bind $wndw <KeyRelease-Down>        [itcl::code $this bind_KeyReleaseDown %W]

     bind $wndw <B1-Motion>              [itcl::code $this bind_ButtonOneMotion %W]
     bind $wndw <ButtonPress-1>          [itcl::code $this bind_ButtonPressOne %W]
     bind $wndw <ButtonRelease-1>        [itcl::code $this bind_ButtonReleaseOne %W]

     bind $wndw <Button-2>               [itcl::code $this bind_ButtonTwo $wndw %x %y]
     bind $wndw <<Paste>>                [itcl::code $this bind_ButtonTwo $wndw] 

     bind $wndw <Triple-ButtonPress-1>   [itcl::code $this bind_TripleButtonPressOne %W]
     bind $wndw <Triple-ButtonRelease-1> [itcl::code $this bind_TripleButtonReleaseOne %W]

     bind $wndw <<DragOver>>             [itcl::code $this dragOver %W]
     bind $wndw <<DragDrop>>             [itcl::code $this dropDraggedItem %W]

     # bind $wndw <Enter>                  [itcl::code $this bind_MouseEnter %W]
     bind $wndw <Control-c>              [itcl::code $this _sendPanicSignal NONE]

}

itcl::body XTerminal::bind_FocusIn { wndw } {
#puts "focus: wndw: $wndw"
     if { $wndwOption == "parameter" } {
        $callSession notifyHeraClient "setCurrentOpenedWndw $wndw"
     } else {
        $callSession setCurrentOpenedWndw $wndw
     }
}

itcl::body XTerminal::bind_MouseEnterX { wndw } {
     if { ![winfo exists .saveAsSetup] || ![winfo ismapped .saveAsSetup] } {
        focus $wndw
        event generate $wndw <Control-End>
        catch {
            unset selectionStart
            unset selectionEnd
            set inputBuffer ""
        }

        $wndw mark set insert end
        $wndw mark set current end

        $wndw see end
     }
}

itcl::body XTerminal::bind_MouseEnter { wndw } {
     if { ![winfo exists .saveAsSetup] || ![winfo ismapped .saveAsSetup] } {
        focus $wndw

        $wndw mark set current [$wndw index insert]
        $wndw mark set insert  [$wndw index insert]
     }
}

itcl::body XTerminal::bind_KeyPressDeleteCheck { currentInputKeySym wndw } {
     set selectionStart [$wndw index insert]
     set selectionMajor [lindex [split $selectionStart "."] 0]
     set selectionMinor [lindex [split $selectionStart "."] 1]

     if { $selectionMajor != $promptMajor } {
        event generate $wndw <Control-End>
        catch {
           unset selectionStart
           unset selectionEnd
        }
        $wndw see end
     } else {
        # blocking deletion (backspace or delete key) to remove prompt
        if { $selectionMinor <= $promptLength } {
           # pick up the one character is been deleted by "BackSpace or Delete"
           if { $currentInputKeySym == "BackSpace" } {
              set currentChar [$wndw get $selectionMajor.[expr $selectionMinor - 1]]
           } else {
              set currentChar [$wndw get $selectionMajor.$selectionMinor]
           }
        }
     }
}

itcl::body XTerminal::bind_KeyReleaseReconstructDelete { wndw } {
     set currentMajor [lindex [split [$wndw index insert] "."] 0]
     set currentMinor [lindex [split [$wndw index insert] "."] 1]
     if { $currentMinor > $promptLength } {
        if { [info exists inputBuffer] && [string length $inputBuffer] > 0 } {
           set inputBuffer [string range $inputBuffer 0 [expr [string length $inputBuffer] - 2]]
        }
        $wndw mark set current [$wndw index insert]
     } else {
        catch { $wndw insert [$wndw index insert] $currentChar }
        set currentChar ""
        $wndw mark set current ${currentMajor}.[expr $promptLength + 1]
        $wndw mark set insert ${currentMajor}.[expr $promptLength + 1]
     }
     update idletasks
}

itcl::body XTerminal::bind_ButtonPressOne { wndw } {
     if { $appendOutputFlag == "false" } {
        set selectionStart [$wndw index current]
     }
}

itcl::body XTerminal::bind_ButtonReleaseOne { wndw } {
     if { $appendOutputFlag == "false" } {
        set selectionEnd [$wndw index current]
     }
}

itcl::body XTerminal::bind_ButtonOneMotion { wndw } {
     if ![info exists selectionStart] {
        set selectionStart [$wndw index current]
     }
}

itcl::body XTerminal::bind_ButtonTwo { wndw { x {} } { y {} } } {
     set errorFlag [ catch {
         set result [selection get -selection PRIMARY]
     } err ]

     if { !$errorFlag } {
        set inputBuffer $result
     } else {
        set inputBuffer $previousInputBuffer
     }

     event generate $wndw <Control-End>
     catch {
         unset selectionStart
         unset selectionEnd
     }

     set currentPos [lindex [split [$wndw index insert] "."] 1]
     set currentMajor [lindex [split [$wndw index insert] "."] 0]

     $wndw insert end $inputBuffer
     set previousInputBuffer $inputBuffer

     set inputBuffer [$wndw get ${currentMajor}.$promptMinor end]
     set promptStart ${currentMajor}.$promptMinor
     set promptMajor $currentMajor

     bind $wndw <<PasteSelection>> { break }

     $wndw mark set current [$wndw index insert]
     update idletasks
}

itcl::body XTerminal::bind_KeyReleaseReturn { wndw } {
     global toolHasFinishFlag

     if { [info exists toolHasFinishFlag($toolServer)] } {
#puts "bind_KeyReleaseReturn toolHasFinishFlag($toolServer): $toolHasFinishFlag($toolServer)"
     }
     
     if { ([info exists toolHasFinishFlag($toolServer)] && $toolHasFinishFlag($toolServer) == "continue") || \
          [string length [string trim $inputBuffer]] > 0 } {
        sendTextToTool $inputBuffer
        set inputBuffer ""
     } else { 
        appendOutput "$theTool> " OutputChannel NEW_COMMANDS
     }

     update idletasks
}

itcl::body XTerminal::bind_KeyReleaseRight { wndw } {
    $wndw mark set current [$wndw index insert]
    update idletasks
}

itcl::body XTerminal::bind_KeyPressLeft { wndw } {
    set leftMajor [lindex [split [$wndw index current] "."] 0]
    set leftMinor [lindex [split [$wndw index current] "."] 1]
    set promptMajor [lindex [split $promptStart "."] 0]
    set promptMinor [lindex [split $promptStart "."] 1]
    if { $leftMajor == $promptMajor } {
       $wndw mark set insert ${leftMajor}.$leftMinor
       $wndw mark set current ${leftMajor}.$leftMinor
       if { $leftMinor < $promptMinor } {
          event generate $wndw <Right>
          $wndw mark set insert $promptStart
       }
    }
}

itcl::body XTerminal::bind_KeyReleaseLeft { wndw } {
    $wndw mark set current [$wndw index insert]
    update idletasks
}

itcl::body XTerminal::bind_KeyReleaseUp { wndw } {
     $wndw mark set insert $promptStart

     if ![info exists selectedHistoryIdx] {
        set selectedHistoryIdx -1
     }

     incr selectedHistoryIdx
     
     if { $selectedHistoryIdx > [expr [llength $inputHistory] - 1] } {
        set selectedHistoryIdx [expr [llength $inputHistory] - 1] 
        return
     }

     set currentSelection [string trim [lindex $inputHistory $selectedHistoryIdx]]
     set promptStart [$wndw index insert]
 
     $wndw delete $promptStart end
     $wndw insert $promptStart "$currentSelection"
     $wndw see $promptStart 
     set inputBuffer $currentSelection
}

itcl::body XTerminal::bind_KeyReleaseDown { wndw } {
     $wndw mark set insert $promptStart
     
     if ![info exists selectedHistoryIdx] {
        set selectedHistoryIdx [llength $inputHistory]
     }

     incr selectedHistoryIdx -1
    
     if { $selectedHistoryIdx < 0 } {
        set selectedHistoryIdx 0
        return
     }

     set currentSelection [lindex $inputHistory $selectedHistoryIdx]
     set promptStart [$wndw index insert]

     $wndw delete $promptStart end
     $wndw insert $promptStart "$currentSelection"
     $wndw see $promptStart 
     set inputBuffer $currentSelection
}

itcl::body XTerminal::bind_KeyReleaseHome { wndw } {
     set insertMajor [lindex [split [$wndw index insert] "."] 0]
     $wndw mark set insert $insertMajor.$promptLength
}

itcl::body XTerminal::bind_TripleButtonPressOne { wndw } {
     set triplePressSelection [$wndw index insert]
     set selectionStart [$wndw index insert]
}

itcl::body XTerminal::bind_TripleButtonReleaseOne { wndw } {
    set promptMajor [lindex [split $promptStart "."] 0]
    set promptMinor [lindex [split $promptStart "."] 1]
    set buttonMajor [lindex [split $triplePressSelection "."] 0]
    set buttonMinor [lindex [split $triplePressSelection "."] 1]
    set selectionEnd [$wndw index insert]
     
    if { $buttonMajor == $promptMajor } {
       selection clear
       $wndw tag add sel $promptStart end
    }
}

itcl::body XTerminal::dragOver { args } {
     set w [lindex $args 0]
     $w configure -cursor hand2
     #raise [winfo toplevel $w] [focus]
}

itcl::body XTerminal::dropDraggedItem { args } {
     global tcl_platform

     set w [lindex $args 0]

     set dragFromWndw [focus]
     $dragFromWndw configure -cursor {}

     set dragItem [[focus] item cget [[focus] info anchor] 0 -text]
     set dragItemParent [reverseModifyName [[focus] info parent [[focus] info anchor]]]

     event generate $w <Enter>

     if {[string first "local" $dragFromWndw] >= 0} {
        set rootDir [${mainWndw}.dirFrame.topLocalDirFrame.localLabelFrame.localDirEntry get] 
        set dragItemParent ""
        set localFS [gLocalFileSystem]
        set fileSystem [gLocalFileSystem]
     } else {
        set rootDir /heradata/users/$userName
        set dragItemParent [string range $dragItemParent 1 end]
     }

     set result [$fileSystem resolveSymLinks $rootDir$dragItemParent/$dragItem]

     if { ![info exists inputBuffer] || $inputBuffer == "" } {
        set inputBuffer $result
        set promptStart [$w index insert]
     } else {
        set inputBuffer [format "%s %s" $inputBuffer $result]
     }

     set currentPos [lindex [split [$w index insert] "."] 1]
     # event generate $outputWndw.txt <Control-End>
     $w insert [$w index insert] $result

     $w configure -cursor {}
     event generate $w <Enter>
     $w mark set current [$w index insert]
     update idletasks
     focus $w
}

itcl::body XTerminal::setupRunningState { state } {
     if { $state == "DONE" } {
        if { [string first "toolRunnerX" $callSession] < 0 } {
           $outputWndw.txt configure -cursor {}
           $outputWndw.title   configure -text "$theTool: Done" -fg black
           $outputWndw.txt configure -state disabled
        }

        if { $theTool == "POWplot" } {
           powResizeScope $powScopeWidth $powScopeHeight
        }
        return
     }

     if { $state == "WAITING_FOR_NEXT_CMD" } {
        if { [string first "toolRunnerX" $callSession] < 0 } {
           $outputWndw.txt configure -cursor {}
           $outputWndw.title   configure -text "$theTool: Done" -fg red
        }
        return
     }

     if { $theTool == "fdump" } {
        $outputWndw.txt configure -cursor watch
     }

     #update idletasks
     #scan [winfo geometry $outputWndw] "%dx%d+%d+%d" heraRw heraRh Rx Ry
     #set x [expr int(([winfo screenwidth .] - $heraRw )/ 2.0)]
     #catch { wm geometry $outputWndw +${x}-0 } err
     #update idletasks

     set childsite [$outputWndw.txt childsite]
     focus $childsite

     $outputWndw.title configure -text "$theTool: Running..." -fg red
}

itcl::body XTerminal::sendTextToTool { cmd } {
     global toolHasFinishFlag
     global signalToXterm
     global tkWaitAnswer
     global promptWaitAnswer
     global writeChannelCmd

     if { [info exists tkWaitAnswer] && [string tolower $tkWaitAnswer] == "wait" } {
        set tkWaitAnswer $cmd
        return
     }

     switch -exact [string trim [string tolower $cmd]] {
         "exit" -
         "quit" {
           if { $toolServer != "NONE" } {
              $toolServer writeChannel exit
              set writeChannelCmd exit
              $toolServer flushChannel 
              $toolServer shutdownToolServer
           }
           appendOutput "cmd> " OutputChannel
           set toolServer "NONE"
           return
         }
     }

     if { $toolServer == "NONE" } {
        # submit the first command
        # set path "/data"
        set tokenList [split $cmd " "]
        set tool [lindex $tokenList 0]
        set toolServer [$topSession FtoolRunInteractive $cmd]
        lappend textBuffer $cmd
     } else {
        $toolServer writeChannel $cmd
        set writeChannelCmd $cmd
        $toolServer flushChannel
     }
}

itcl::body XTerminal::processOutput { line lastLineFlag } {
        global promptWaitAnswer

        # very likely to be the prompt
        set promptLineFound false
        set token [split $line "\n"]

        set outputLine [string trim $line] 
        set lastChar [string range $outputLine end end]

        if { $lastLineFlag == "true" } {
           switch -exact $lastChar {
                ")" -
                ">" -
                "\]" -
                ":" {
                   set firstChar "NONE"
                   switch -exact $lastChar {
                       "\]" {
                         set firstChar "\["
                       }
                   }

                   # add a space between prompt and command
                   set currentPrompt [string range $outputLine 0 [expr [string length $outputLine] - 1]]

                   set promptLine [format "%s " $outputLine]
                   # set currentMajor [lindex [split [$outputWndw.txt index insert] "."] 0]
                   set promptMajor [lindex [split [$outputWndw.txt index insert] "."] 0]

                   # index of line position starts from 1
                   # set promptMajor [expr $currentMajor + 1]

                   # index of character position starts from 0
                   set promptMinor [string length $promptLine]
                   set promptStart [format "%s.%s" $promptMajor $promptMinor]
                   set promptLength [lindex [split $promptStart "."] 1]
   
                   # no end of line appended to return string since it represents command line
                   set promptLineFound true
                   if { $promptLine != "$theTool> " } {
                      if { $firstChar != "NONE" } {
                         set idxB [string last $firstChar $promptLine]
                         set idxE [string last $lastChar $promptLine]
                         set promptWaitAnswer [string range $promptLine [expr $idxB + 1] [expr $idxE - 1]]
                      } else {
                         set promptWaitAnswer "NONE"
                      }
                   } else {
                      catch { unset promptWaitAnswer }
                   }
                   return $promptLine
                }
                default {
                   catch { unset promptWaitAnswer }
                   # any other type should have end of line appended to it
                   set returnLine $line
                   if { [info exists promptStart] && $promptStart != "X.X" } {
                      set previous_promptStart $promptStart
                      set promptStart "X.X"
                   }
                   return $returnLine
                }
           }
        } else {
           # any other type should have end of line appended to it
           set returnLine [format "%s\n" $line]
           if { [info exists promptStart] && $promptStart != "X.X" } {
              set previous_promptStart $promptStart
              set promptStart "X.X"
           }
           return $returnLine
        }
}

itcl::body XTerminal::appendOutput { output fromWhere { option "NOT_USER_INPUT" } } {
   global toolHasFinishFlag
   global writeChannelCmd

   set checkToken [string trim [lindex [split $output " "] 0]]
   if [info exists writeChannelCmd] { 
      if { $checkToken == [string trim $writeChannelCmd] && \
           [string trim $writeChannelCmd] != "" } return 
   }
   if { [string tolower $checkToken] == "spawn" } {
      set idx [string first "\n" $output]
      set output [string range $output [expr $idx + 1] end]
   }

   focus $outputWndw.txt

   event generate $outputWndw.txt <Control-End>
   catch {
      unset selectionStart
      unset selectionEnd
   }

   set appendOutputFlag true

   # no need to append output if outputWndw not exist or tool has finished
   if { [info exists toolHasFinishFlag($toolServer)] && \
         $toolHasFinishFlag($toolServer) == "true" && [string trim $output] == "" } {
      return
   }

   set resultData {}
   if { $output != "" } {
      # split the output into lines
      set resultData [split $output \n]

      set moreFlag false
      set moreStrOutput ""

      # get rid of any occurance of line feed "\n" at the end of TOTAL data stream
      # the reason is to find out if last non-empty line contains prompt
      set stopFlag "false"
      while { $stopFlag == "false" } {
            set token [lindex $resultData end]
            if { [string length [string trim $token]] <= 0 } {
               set resultData [lreplace $resultData end end]
            } else {
               set stopFlag true
            }
      }
   } else {
      return
   }

   # begin process line by line
   for {set i 0} {$i < [llength $resultData]} {incr i} {
       # get current line
       set inputLine [lindex $resultData $i]
 
       if { $i == [expr [llength $resultData] - 1] } {
          # last line
          set outputLine [processOutput $inputLine true]
      } else {
          set outputLine [processOutput $inputLine false]
      }

# Debug
#set outputLine [format "%s. %s" $nextLineNumber $outputLine]
#if { $promptStart != "X.X" } {
#   set promptMajor [lindex [split $promptStart "."] 0]
#   set promptMinor [lindex [split $promptStart "."] 1]
#   set promptStart [format "%s.%s" $promptMajor [expr [string length $nextLineNumber] + 2 + $promptMinor]]
#   set promptMajor [lindex [split $promptStart "."] 0]
#   set promptMinor [lindex [split $promptStart "."] 1]
#}
      
      # to see if "more" is part of string
      set moreStr [string tolower [string range $outputLine 0 3]]

      if { $moreStr == "more" } {
         set moreStrOutput $outputLine
         set moreFlag true
      }

      set systemInsertMajor [lindex [split [$outputWndw.txt index insert] "."] 0]
      set systemInsertMinor [lindex [split [$outputWndw.txt index insert] "."] 1]

      if { $systemInsertMajor != $nextLineNumber } {
         # meaning system is try to insert the line at the position that is different
         # than the desired insert loction
         if { ($systemInsertMinor == 0 || $systemInsertMinor == $promptLength) && \
               $systemInsertMajor < $nextLineNumber } {
            # system has determined that insert the line into a right position than determined
            set nextLineNumber $systemInsertMajor
         } else {
            $outputWndw.txt insert end "\n"
            incr nextLineNumber
         }
      }

      if { $outputLine == "$theTool> " && $option != "NEW_WINDOWS" && $option != "NEW_COMMANDS"} {
         # set outputLine [format "%s" $outputLine]
      }
      set errorFlag [ catch {
         set idx1 [$outputWndw.txt index "end - 1 char"]
         $outputWndw.txt insert ${nextLineNumber}.$systemInsertMinor $outputLine
         set idx2 [$outputWndw.txt index "end - 1 char"]
         $outputWndw.txt see end

         if { $fromWhere == "ErrorChannel" } {
            $outputWndw.txt tag add errorTag $idx1 $idx2
         } else {
            if { $option == "USER_INPUT" } {
               $outputWndw.txt tag add inputTag $idx1 $idx2
            }
         }
      } err]
    
      lappend textBuffer $outputLine
      incr nextLineNumber
   }

   if { $promptStart == "X.X" } {
      # last line does not contain prompt
      set promptStart $previous_promptStart
      $outputWndw.txt insert ${nextLineNumber}.0 "\n"
      incr nextLineNumber
   }

   if { $moreFlag == "true" } {
      
      if { $nextLineNumber < $maxMark } {
         # this is for very large output, allow a break for user to determine if he/she want to continue
         sendTextToTool "y"
      } else {
         $outputWndw.txt see end
      }
   }
   set appendOutputFlag false

   # now determine where the insert cursor is
   set currentPosMinor [lindex [split [$outputWndw.txt index insert] "."] 1]
   if { $currentPosMinor == 0 } {
      # current pos is at 0
   }

   set childsite [$outputWndw.txt childsite]
   focus $childsite
   update idletasks
}

itcl::body XTerminal::reverseModifyName { name } {
     set newName ""
     for {set i 0} {$i < [string length $name]} {incr i} {
         if [string match "%" [string range $name $i $i]] {
            set newSubString " "
         } else {
            set newSubString [string range $name $i $i]
         }
         set newName [format "%s%s" $newName $newSubString]
     }
     return $newName
}

itcl::body XTerminal::_sendPanicSignal { wndw } {
     catch { $toolServer shutdownToolServer }
     if { $wndw != "NONE" } {
        $outputWndw.txt configure -cursor {}
        $outputWndw.title   configure -text "$theTool: Done" -fg black
        $outputWndw.txt configure -state disabled
        $wndw configure -state disabled
     } else {
        catch { itcl::delete object $toolServer }
        catch { itcl::delete object $runner }
        catch { unset runner }
        # append prompt and ready to accept new command
        appendOutput "cmd> " OutputChannel
        set toolServer "NONE"
     }
}

itcl::body XTerminal::setEditor {} {
     set viewer [.newTextEditor.entry get]
}

itcl::body XTerminal::checkEditor { fileName } {
     global outFlag

     set outFlag false
     while (1) {
        set errorFlag [ catch {
            exec $viewer $fileName
        } err ]

        if { !$errorFlag } {
           break
        }

        toplevel .newTextEditor
        set top .newTextEditor
        wm geometry $top 300x100
        wm title $top "New Text Editor"
        label $top.label -text "$viewer not found.\nPlease enter new text editor name on Windows"
        entry $top.entry -width 40 -bg white
        frame $top.frame
        button $top.frame.action -text "GO" -command "[itcl::code $this setEditor] ; destroy .newTextEditor"
        button $top.frame.cancel -text "Cancel" -command "destroy .newTextEditor ; set outFlag true"

        pack $top.frame.action $top.frame.cancel -side left -padx 5

        pack $top.label
        pack $top.entry
        pack $top.frame
        tkwait window .newTextEditor
        if { $outFlag == "true" } break

     }
}

itcl::body XTerminal::printOutput {} {

     set tmpFile [format "%s/%s" $::env(HOME) tmpfile_[clock seconds]]
     set f [open $tmpFile "w"]

     foreach line $textBuffer {
          puts $f $line
     }

     close $f

     if { $::isWin } {
        checkEditor $tmpFile
     } else {
        set errorFlag [ catch {
            exec lp $tmpFile
        } err ]

        if { $errorFlag } {
           tk_messageBox -icon error -type ok \
                         -message "Failed to print. Error: $err"
        } else {
           tk_messageBox -icon info -type ok \
                         -message "The file has been printed.. $err"
        }
     }

     file delete -force $tmpFile
}

itcl::body XTerminal::saveAs {} {
     global g_titleFont powbg

     if [winfo exists .saveAsSetup] {
        wm deiconify .saveAsSetup
        return
     }

     toplevel .saveAsSetup
     bind .saveAsSetup <<CloseWindow>> "destroy .saveAsSetup"

     wm title .saveAsSetup "Save File As"

     grid rowconfigure .saveAsSetup 2 -weight 1
     grid columnconfigure .saveAsSetup 0 -weight 1
     grid columnconfigure .saveAsSetup 1 -weight 1

     frame .saveAsSetup.directory             -bd 2 -relief ridge
     label .saveAsSetup.directory.dirLabel    -text "Directory: " -font g_titleFont
     entry .saveAsSetup.directory.saveInEntry -width 35 -bg white -font g_titleFont
   
     .saveAsSetup.directory.saveInEntry delete 0 end
     .saveAsSetup.directory.saveInEntry insert end [pwd]
     set directoryValue [pwd]

     tixDirTree .saveAsSetup.directory.directoryTree -value $directoryValue \
                -browsecmd [itcl::code $this saveAsSelectDirectory] \
                -command [itcl::code $this saveAsSelectDirectory] \
                -options { \
                     hlist.foreground black \
                     hlist.background white \
                     hlist.font g_titleFont \
                     hlist.width 40 \
                }

     set selectDirectory $directoryValue

     grid .saveAsSetup.directory               -row 2 -column 0 -sticky news -columnspan 5 -rowspan 10
     grid .saveAsSetup.directory.dirLabel      -row 2 -column 0 -sticky nw
     grid .saveAsSetup.directory.saveInEntry   -row 2 -column 1 -sticky new
     grid .saveAsSetup.directory.directoryTree -row 3 -column 0 -columnspan 5 -rowspan 10 -sticky news

     grid columnconfigure .saveAsSetup.directory 1 -weight 1
     grid rowconfigure .saveAsSetup.directory 3 -weight 1

     bind .saveAsSetup.directory.saveInEntry <Return> {
          [.saveAsSetup.directory.directoryTree subwidget hlist] delete all
          destroy .saveAsSetup.directory.directoryTree
          tixDirTree .saveAsSetup.directory.directoryTree \
                     -value [.saveAsSetup.directory.saveInEntry get] \
                     -browsecmd [itcl::code $this saveAsSelectDirectory] \
                     -command [itcl::code $this saveAsSelectDirectory] \
                     -options { \
                          hlist.foreground black \
                          hlist.background white \
                          hlist.font g_titleFont \
                          hlist.width 40 \
                     }
          grid .saveAsSetup.directory.directoryTree -row 3 -column 0 -columnspan 5 -rowspan 10 -sticky news
     }

     frame .saveAsSetup.file               
     label .saveAsSetup.file.fileNameLbl   -text "File name:" -font g_titleFont
     entry .saveAsSetup.file.fileNameEntry -width 35 -bg white -font g_titleFont

     .saveAsSetup.file.fileNameEntry insert end $savedFileName
     grid .saveAsSetup.file               -row 13 -column 0 -sticky news -columnspan 5 -rowspan 2
     grid .saveAsSetup.file.fileNameLbl   -row 0 -column 0 -sticky nw
     grid .saveAsSetup.file.fileNameEntry -row 0 -column 1 -sticky new

     grid columnconfigure .saveAsSetup.file 1 -weight 1

     frame .saveAsSetup.action 
     button .saveAsSetup.action.ok  -text "OK" -font g_titleFont -command [itcl::code $this saveFile]

     label .saveAsSetup.action.blanklabel -text " " -font g_titleFont
     button .saveAsSetup.action.cancel -text "Cancel" -font g_titleFont \
                                                  -command { wm withdraw .saveAsSetup }

     grid .saveAsSetup.action -row 18 -column 0 -columnspan 6 -sticky news
     grid .saveAsSetup.action.ok     -row 0 -column 1 -sticky w
     grid .saveAsSetup.action.blanklabel -row 0 -column 2 -columnspan 2 -sticky news
     grid .saveAsSetup.action.cancel -row 0 -column 4 -sticky e

     wm geometry .saveAsSetup +[expr [winfo screenwidth .] / 3]+[expr [winfo screenheight .] / 2]
}

itcl::body XTerminal::saveFile {} {
     set savedFileName [.saveAsSetup.file.fileNameEntry get]
     set selectDirectory [.saveAsSetup.directory.saveInEntry get] ; \
     if [file exists $selectDirectory/$savedFileName] {
        set ans [tk_messageBox -icon warning -type okcancel \
                               -message "File exists. Do you want to write over it?"]

        if { $ans == "cancel" } {
           return
        }
     }
     set fd [open $selectDirectory/$savedFileName "w+"]
     foreach line $textBuffer {
         puts $fd [string trim $line \n]
     }
     close $fd
     wm withdraw .saveAsSetup
}

itcl::body XTerminal::saveAsSelectDirectory { dir } {
     set selectDirectory $dir

     .saveAsSetup.directory.saveInEntry delete 0 end
     .saveAsSetup.directory.saveInEntry insert end $selectDirectory
}

itcl::body XTerminal::_displayHistory {} {
     if [winfo exists .history] {
        destroy .history
     }
     toplevel .history
     set top .history
     wm title $top "Command History"
     wm geometry $top +[winfo pointerx .]+[winfo pointery .]

     listbox $top.historyList -relief sunken -borderwidth 2 -bg white -fg black \
                              -yscrollcommand "$top.scroll set"
     pack $top.historyList -side left
     scrollbar $top.scroll -command "$top.historyList yview"
     pack $top.scroll -side right -fill y

     foreach dirName $inputHistory {
         $top.historyList insert end $dirName
     }

     bind $top <Leave> {
          destroy .history
     }

     bind $top <ButtonRelease-1> [itcl::code $this _getPreviousHistory]
}

itcl::body XTerminal::_getPreviousHistory {} {
     set chosedCmd [selection get]
     set wndw [$outputWndw.txt childsite]

     $wndw mark set insert $promptStart

     set selectedHistoryIdx [lsearch -exact $inputHistory $chosedCmd]

     set minor [lindex [split $promptStart "."] 1]
     set major [lindex [split [$wndw index insert] "."] 0]

     $wndw delete ${major}.${minor} end
     $wndw insert ${major}.${minor} "$chosedCmd"
     $wndw see ${major}.${minor}
     set promptStart ${major}.${minor}
     set inputBuffer $chosedCmd
     destroy .history
}

itcl::body XTerminal::setLocalHandler { obj } {
     set callSession $obj
}

itcl::body XTerminal::receiveOutput { args } {
     global toolHasFinishFlag

#puts "XTerminal receiveOutput: args: $args"
     set type  [lindex $args 0]
     set value [lindex $args 1]
     if { [llength $args] > 2 } {
        set package [lindex $args 2]
        set resetParam "[lindex $args 3] [lindex $args 4]"
        for { set i 5 } { $i < [llength $args] } {incr i} {
            if { [string first "mode" [lindex $args $i]] < 0 } {
               set resetParam [format "%s %s" $resetParam [lindex $args $i]]
            }
        }
     }

#puts "resetParam: $resetParam"
     switch -glob -- $type {
         "toolInXtermDone" {
             set toolHasFinishFlag($value) continue
#puts "scriptFlag: $scriptFlag"
             catch { $value shutdownToolServer
                     itcl::delete object $value
                     [gNotifications default] removeObserver $this $value "*"
                     itcl::delete object $runner
                     unset runner }

             # delete temporary directory
#puts "delete temp directory"
             set result [$mySession askDeleteTempDir $userName [file tail $path] $scriptFlag]

             # set parfile to what user input
             if { [llength $args] > 2 } {
                catch { unset toolHasFinishFlag($value) }
                set toolHasFinishFlag($toolServer) false
                set path "/data"
                set toolServer [$mySession tool $userName $path]
                [gNotifications default] addObserver $this notify $toolServer "*"

                catch { $toolServer runTool $package $resetParam } err
                #$toolServer shutdownToolServer
                #itcl::delete object $toolServer
                set toolHasFinishFlag($toolServer) continue
                set theOperation [lindex [split [lindex $args 4] "."] 0]
             }

             # append prompt and ready to accept new command
             appendOutput "$theTool> " OutputChannel
             set toolServer "NONE"

             if { $plistFlag == "true" } {
                sendTextToTool "plist $theOperation"
                set plistFlag false
             }
#puts "done delete temp directory"
         }
         "toolToXtermDone" {
             set toolHasFinishFlag($value) continue
             $value shutdownToolServer
             itcl::delete object $value

             # append prompt and ready to accept new command
             appendOutput "$theTool> " OutputChannel
             set toolServer "NONE"
         }
         "herahtmlbrowser" {
             if { $wndwOption == "parameter" } {
                $callSession notifyHeraClient "herahtmlbrowser $value"
             } else {
                $callSession receiveOutput herahtmlbrowser $value
             }
         }
     }
}

itcl::body XTerminal::addToHistory { cmd } {
     set inputOldHistory $inputHistory
     unset inputHistory

     # reverse order
     lappend inputHistory $cmd
     foreach history $inputOldHistory {
        lappend inputHistory $history
     }
     set selectedHistoryIdx -1
}

itcl::body XTerminal::changeToolServer { srvr } {
     global toolHasFinishFlag

     catch { unset toolHasFinishFlag($toolServer) }
     set toolServer $srvr

     if { $srvr != "NONE" } {
        set toolHasFinishFlag($toolServer) false
     }
}

itcl::body XTerminal::Dump_Session {} {

  set logfile [ tk_getSaveFile -initialfile "ftool.log"]
  if { $logfile == "" } {
     return
  }

  set channel [open $logfile a]
  set num [llength $textBuffer]
  for {set i 0} {$i < $num} {incr i} {
    puts $channel [lindex $textBuffer $i]
  }
  close $channel
}

itcl::body XTerminal::Dump_Cmd {} {

  set cmdlogfile [ tk_getSaveFile -initialfile "ftool_cmd.log"]
  if { $cmdlogfile == "" } {
     return
  }

  set channel [open $cmdlogfile a]
  set num [llength $inputHistory]
  for {set i 0} {$i < $num} {incr i} {
    puts $channel [lindex $inputHistory $i]
  }
  close $channel
}

