	proc XSgetbox  { {prompt "XSPEC>"} } {
            global xsaux xscon xsget
	    ## 'gets' - a replacement for [gets stdin]
	    ## This pops up a text widget to be used for stdin (local grabbed)
            set t [$xsaux childsite]
            set s $t.gets
            set xsget {}
            global b
            frame $s
            set et [::iwidgets::entryfield $s.entry -width 60 -textbackground white -labeltext $prompt -labelfont {helvetica 12 bold} ]
            focus $et.lwchildsite.entry
            grab  $et.lwchildsite.entry
            set bt [ button $s.ok -text "Set" -command {set b 1} ]
            set ct [ button $s.clear -text "Clear" -command {deletePromptText} ]
            set qt [ button $s.quit -text "Use Defaults" -command {set b 0} ]
            
            grid $et -row 0 -column 0 -sticky news -padx 10 -pady 10 -columnspan 3
            grid $bt -row 1 -column 0 -padx 5 -pady 5 
            grid $ct -row 1 -column 1 -padx 5 -pady 5
            grid $qt -row 1 -column 2 -padx 5 -pady 5
            pack $s
            
            bind $et.lwchildsite.entry <Return> {set b 1}
            
            
	    tkwait variable b
            if { $b == 1} {
               set result [$s.entry.lwchildsite.entry get]
            } else {
              set result {}
            }
            grab release $et.lwchildsite.entry
            destroyPromptBox
            focus -force $xscon
            return $result
	}


        proc destroyPromptBox {} {
                global xsaux
                destroy [$xsaux childsite].gets
                
        }
        
        proc deletePromptText {} {
                global xsaux
                set s [$xsaux childsite].gets
                $s.entry delete 0 end
        }

#        proc getEntry {} {
#                global xsaux
#                set s [$xsaux childsite].gets
#                $s.entry.lwchildsite.entry get
#        }
        
        
